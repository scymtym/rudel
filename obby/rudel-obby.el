;;; rudel-obby.el --- An obby backend for Rudel
;;
;; Copyright (C) 2008 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;; Keywords: Rudel, obby, backend, implementation
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
;; This file contains a Rudel backend, which implements the obby
;; protocol (used by the Gobby collaborative editor).

;;; History:
;;
;; 0.1 - Initial version

;;; Code:
;;

(eval-when-compile
  (require 'cl))

(require 'eieio)

(require 'rudel)


;;; Class rudel-obby-backend
;;

(defclass rudel-obby-backend (rudel-backend)
  ((capabilities :initform '(join host
			     change-color
			     track-subscriptions)))
  "Class rudel-obby-backend ")

(defmethod rudel-ask-connect-info ((this rudel-obby-backend))
  ""
  ;; Read server host and port.
  (let ((host     (read-string "Server: "))
	(port     (read-number "Port: " 6522))
	;; Read desired username and color
	(username (read-string "Username: " user-login-name))
	(color    (read-color  "Color: " 't))) ; TODO default/customize color
    (list :host host :port port :username username :color color))
  )

(defmethod rudel-connect ((this rudel-obby-backend) info)
  ""
  ;; Before we start, load the client functionality.
  (require 'rudel-obby-client)
  (let* ((session    (plist-get info :session))
	 (host       (plist-get info :host))
	 (port       (plist-get info :port))
	 ;; Create the network process
	 (socket     (make-network-process
		      :name     host
		      :host     host
		      :service  port
		      ;; Install connection filter to redirect data to
		      ;; the connection object
		      :filter   'rudel-filter-dispatch
		      ;; Install connection sentinel to redirect state
		      ;; changes to the connection object
		      :sentinel 'rudel-sentinel-dispatch))
	 (connection (rudel-obby-connection host
                      :socket socket
		      :info   info)))
    connection)
  )

(defmethod rudel-make-document ((this rudel-obby-backend)
				name session)
  ""
  ;; Find an unused document id and create a document with that id.
  (let ((id (rudel-available-document-id this session)))
    (with-slots (user-id) (oref session :self)
      (rudel-obby-document name 
			   :session  session
			   :id       id
			   :owner-id user-id)))
  )

(defmethod rudel-available-document-id ((this rudel-obby-backend)
					session)
  "Return a document id, which is not in use in SESSION."
  ;; Look through some candidates until an unused id is hit.
  (let* ((used-ids (with-slots (documents) session
		     (mapcar 'rudel-id documents)))
	 (test-ids (number-sequence 0 (length used-ids))))
    (car (sort (set-difference test-ids used-ids) '<)))
  )


;;; Class rudel-obby-user
;;

(defclass rudel-obby-user (rudel-user)
  ((client-id  :initarg  :client-id
	       :type     integer
	       :accessor rudel-client-id
	       :documentation
	       "")
   (user-id    :initarg  :user-id
	       :type     integer
	       :accessor rudel-id
	       :documentation
	       "")
   (connected  :initarg  :connected
	       :type     boolean
	       :documentation
	       "")
   (encryption :initarg  :encryption
	       :type     boolean
	       :documentation
	       ""))
  "Class rudel-obby-user ")

(defmethod eieio-speedbar-description ((this rudel-obby-user))
  "Provide a speedbar description for THIS."
  (let ((connected  (oref this :connected))
	(encryption (if (slot-boundp this :encryption)
			(oref this :encryption)
		      nil)))
    (format "User %s (%s, %s)" (object-name-string this)
	    (if connected  "Online" "Offline")
	    (if encryption "Encryption" "Plain")))
  )

(defmethod eieio-speedbar-object-buttonname ((this rudel-obby-user))
  "Return a string to use as a speedbar button for THIS."
  (let ((connected  (oref this :connected))
	(encryption (if (slot-boundp this :encryption)
			(oref this :encryption)
		      nil)))
    (format "%-12s %s%s" (object-name-string this)
	    (if connected  "c" "-")
	    (if encryption "e" "-")))
  )


;;; Class rudel-obby-document
;;

(defclass rudel-obby-document (rudel-document)
  ((id         :initarg  :id
	       :type     integer
	       :accessor rudel-id
	       :documentation
	       "The id of this document.
The id has to be unique only with respect to the other documents
owned by the owner.")
   (owner-id   :initarg  :owner-id
	       :type     integer
	       :documentation
	       "")
   (revision   :initarg  :revision
	       :type     integer
	       :initform 0
	       :documentation
	       "")
   (subscribed :initarg  :subscribed
	       :type     boolean
	       :initform nil
	       :documentation
	       ""))
  "Class rudel-obby-document ")

(defmethod eieio-speedbar-description ((this rudel-obby-document))
  "Construct a description for from the name of document object THIS."
  (format "Document %s" (object-name-string this)))

(defmethod eieio-speedbar-object-buttonname ((this rudel-obby-document))
  "Return a string to use as a speedbar button for OBJECT."
  (with-slots (subscribed) this
    (format "%-12s %s" (object-name-string this)
	    (if subscribed "s" "-")))
  )


;;; Obby message functions
;;

(defun rudel-obby-replace-in-string (string replacements)
  "Replace elements of REPLACEMENTS in STRING.
REPLACEMENTS is a list of conses whose car is the pattern and
whose cdr is the replacement for the pattern."
  (let ((result string))
    (dolist (replacement replacements)
      (let ((from (car replacement))
	    (to   (cdr replacement)))
	(setq result (replace-regexp-in-string
		      from to result nil 't))))
    result)
  )

(defun rudel-obby-escape-string (string)
  "Replace meta characters in STRING with their escape sequences."
  (obby-replace-in-string 
   string 
   '(("\\\\" . "\\b") ("\n" . "\\n") (":" . "\\d")))
  )

(defun rudel-obby-unescape-string (string)
  "Replace escaped versions of obby meta characters in STRING with the actual meta characters."
  (obby-replace-in-string 
   string 
   '(("\\\\n" . "\n") ("\\\\d" . ":") ("\\\\b" . "\\")))
  )

(defun rudel-obby-parse-color (color)
  "Parse the obby color string COLOR into an Emacs color."
  (let* ((color-numeric (string-to-number color 16))
	 (color-string  (format "#%04X%04X%04X"
				(lsh (logand #xff0000 color-numeric) -08)
				(lsh (logand #x00ff00 color-numeric) -00)
				(lsh (logand #x0000ff color-numeric)  08))))
    color-string)
  )
  
(defun rudel-obby-format-color (color)
  "Format the Emacs color COLOR as obby color string."
  (let ((color-numeric-r (string-to-number (substring color 1 5) 16))
	(color-numeric-g (string-to-number (substring color 5 9) 16))
	(color-numeric-b (string-to-number (substring color 9 13) 16)))
    (format "%02x%02x%02x" (lsh color-numeric-r -8)
	    (lsh color-numeric-g -8)
	    (lsh color-numeric-b -8)))
  )

(defun rudel-obby-assemble-message (name &rest arguments)
  ""
  (concat (mapconcat
	   (lambda (part)
	     (if (and (not (null part)) (stringp part))
		 (rudel-obby-escape-string part)
	       part))
	   (cons name arguments) ":")
	  "\n")
  )

(defun rudel-obby-parse-message (message)
  "Split MESSAGE at `:' and unescape resulting parts.

The terminating `\n' should be removed from MESSAGE before
calling this function."
  (mapcar 'rudel-obby-unescape-string (split-string message ":")))


;;; Autoloading
;;

;;;###autoload
(add-to-list 'rudel-backends
	     (cons "obby" 'rudel-obby-backend))

(provide 'rudel-obby)
;;; rudel-obby.el ends here
