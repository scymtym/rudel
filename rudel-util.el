;;; rudel-util.el --- Miscellaneous functions for Rudel
;;
;; Copyright (C) 2008, 2009 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, miscellaneous, util
;; X-RCS: $Id:$
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA, or see <http://www.gnu.org/licenses>.

;;; Commentary:
;;
;; This file contains miscellaneous functions for Rudel.

;;; History:
;;
;; 0.1 - Initial revision.

;;; Code:
;;

(eval-when-compile
  (require 'cl)) ; TODO check

(require 'eieio)


;;; Class rudel-socket-owner
;;

(defclass rudel-socket-owner ()
  ((socket :initarg :socket 
	   :type    process
	   :documentation
	   "The process object representing the socket through
which the communication happens."))
  "Class rudel-socket-owner ")

(defmethod initialize-instance :after ((this rudel-socket-owner)
				       &rest slots)
  "Attach THIS to as process object of our socket."
  ;; Attach to our socket.
  (with-slots (socket) this
    (rudel-set-process-object socket this))
  )

(defmethod rudel-disconnect ((this rudel-socket-owner))
  "Disconnect the network connection owned by THIS."
  (with-slots (socket) this
    (delete-process socket)))

(defmethod rudel-state-change ((this rudel-socket-owner) state message)
  "Called when the state of THIS changes to STATE.
MESSAGE is the message emitted when the state transition
occurred."
  (with-slots (socket) this
    (case state
      ;; Dispatch events which indicate the termination of the
      ;; connection to `rudel-close'.
      ((closed failed) (rudel-close this)))))

(defmethod rudel-close ((this rudel-socket-owner))
  "Called when the connection associated to THIS is closed.")


;;; Networking helper functions and macros
;;

(defun rudel-process-object (process &optional key)
  "Return the object attached to PROCESS using identifier KEY."
  (unless key
    (setq key :object))
  (get (intern (process-name process)) key))

(defun rudel-set-process-object (process object &optional key)
  "Set object attached to PROCESS using identifier KEY to OBJECT."
  (unless key
    (setq key :object))
  (put (intern (process-name process)) key object))

(defun rudel-filter-dispatch (process data)
  "Call `rudel-receive' method of object attached to PROCESS with DATA."
  (let ((object (rudel-process-object process)))
    (rudel-receive object data)))

(defun rudel-sentinel-dispatch (process message)
  "Call `rudel-state-change' method of the object attached to PROCESS with state and MESSAGE."
  (let ((object (rudel-process-object process))
	(state  (process-status process)))
    (rudel-state-change object state message)))


;;; Fragmentation and assembling functions.
;;

(defmacro rudel-assemble-line-fragments (data storage)
  "Find an return complete lines in DATA, store excess data in STORAGE.
If STORAGE is non-nil when calling, consider content as leftover
data from last and concatenate with DATA before processing."
  (let ((index (make-symbol "index")))
    `(progn
       ;; If there are stored fragments, append them to the new data.
       (when ,storage
	 (setq ,data    (concat ,storage ,data))
	 (setq ,storage nil))
       ;; Try to find a line break in the augmented data.
       (let ((,index (position ?\n ,data :from-end t)))
	 (unless (and ,index (eq ,index (- (length ,data) 1)))
	   (setq ,storage (if ,index 
			      (substring ,data (+ ,index 1))
			    ,data))
	   (setq ,data    (when ,index
			    (substring ,data 0 (+ ,index 1))))))
       ,data))
  )

(defmacro rudel-loop-lines (data var &rest forms)
  "Execute FROMS with VAR subsequently bound to all lines in DATA."
  (declare (indent 2))
  (let ((lines (make-symbol "lines")))
    `(when ,data
       (let ((,lines (split-string ,data "\n" t)))
	 (dolist (,var ,lines)
	   (progn ,@forms)))))
  )

(defmacro rudel-loop-chunks (data var size &rest forms)
  "Execute FROMS in a loop with VAR bound chunks of DATA of SIZE.
Unless (zerop (mod (length data) size) 0) the final chunk is
truncated. The expression SIZE is evaluated in each loop unless
it is a number."
  (declare (indent 3))
  ;; If we got a constant number as SIZE, we can check right away.
  (when (and (numberp size) (<= size 0))
    (error "Size should be positive"))

  (let ((rest   (make-symbol "rest"))
	(amount (make-symbol "amount"))
	;; If SIZE has to be evaluated, we have to check at runtime.
	(check  (unless (numberp size)
		  `((when (<= ,size 0)
		      (error "Size should be positive"))))))
    `(progn
       ,@check
       (let ((,rest ,data)
	     (,var)
	     (,amount))
	 (while (not (string= ,rest ""))
	   (setq ,amount (min (length ,rest) ,size)
		 ,var    (substring ,rest 0 ,amount)
		 ,rest   (substring ,rest ,amount))
	   (progn ,@forms)))))
  )

(provide 'rudel-util)
;;; rudel-util.el ends here
