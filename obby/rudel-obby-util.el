;;; rudel-obby-util.el --- Miscellaneous functions for the Rudel obby backend
;;
;; Copyright (C) 2008, 2009 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: Rudel, obby, backend, miscellaneous
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
;; along with this program; see the file COPYING. If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA, or see <http://www.gnu.org/licenses>.


;;; Commentary:
;;
;;


;;; History:
;;
;; 1.0 - Initial revision.


;;; Code:
;;

(require 'eieio)

(require 'rudel-util)


;;; Class rudel-obby-socket-owner
;;

(defclass rudel-obby-socket-owner (rudel-socket-owner)
  ((buffer :initarg  :buffer
	   :type     (or null string)
	   :initform nil
	   :documentation
	   "Stores message fragments until complete messages can
be assembled."))
  "This class adds functions for sending and receiving obby
messages to the base class rudel-socket-owner.")

(defmethod rudel-send ((this rudel-obby-socket-owner) 
		       name &rest arguments)
  "Send obby message NAME with arguments ARGUMENTS through the socket associated to THIS."
  (with-slots (socket) this
    (rudel-obby-send socket name arguments)))

(defmethod rudel-receive ((this rudel-obby-socket-owner) data)
  "Reassemble lines in DATA received on the socket associated with THIS and call message handler."
  ;; Assemble fragmented lines.
  (with-slots (buffer) this
    (rudel-assemble-line-fragments data buffer))

  ;; Process all available lines.
  (rudel-loop-lines data line
    ;; `rudel-message' has to dispatch message to an appropriate
    ;; handler.
    (let ((message (rudel-obby-parse-message line)))
      (rudel-message this message)))
  )

(defgeneric rudel-message ((this rudel-obby-socket-owner) message)
  "Called when a message arrives.
Should be implemented in derived classes.")


;;; Miscellaneous functions
;;

(defun rudel-obby-dispatch (object name arguments &optional prefix)
  "Call method starting with PREFIX and ending in NAME of OBJECT with ARGUMENTS.
When PREFIX is not specified, \"rudel-obby/\" is used."
  ;; Fallback prefix.
  (unless prefix
    (setq prefix "rudel-obby/"))

  ;; Construct a matching symbol.
  (let ((method (intern-soft (concat prefix name))))
    ;; If we found a suitable method, run it; Otherwise warn and do
    ;; nothing.
    (unless (and method
		 (condition-case error
		     ;; Try to call METHOD. If successful, always
		     ;; return t.
		     (progn
		       (apply method object arguments)
		       't)
		   ;; Warn only when the condition is
		   ;; 'no-method-definition' and refers to METHOD,
		   ;; otherwise continue unwinding.
		   (no-method-definition
		    (if (eq method (cadr error))
			nil
		      (signal (car error) (cdr error))))))
      (warn "%s: in context `%s': no method: `%s'; arguments:  %s" 
	    (object-name-string object) prefix name arguments)))
  )

(provide 'rudel-obby-util)
;;; rudel-obby-util.el ends here
