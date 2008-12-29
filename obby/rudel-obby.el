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
  ((capabilities :initform '(join host change-color)))
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


;;; Class rudel-obby-connection
;;

(defclass rudel-obby-connection (rudel-connection)
  ((socket :initarg :socket
	   :documentation
	   "")
   (info   :initarg :info
	   :type    list
	   :documentation
	   "Stores connection information for later use."))
  "Class rudel-obby-connection ")

(defmethod rudel-disconnect ((this rudel-obby-connection))
  ""
  (with-slots (socket) this
    (delete-process socket)))

(defmethod rudel-subscribe-to ((this rudel-obby-connection) document)
  ""
  (let* ((session (oref this :session))
	 (user-id (oref (oref session :self) :user-id)))
    (with-slots (id owner-id subscribed) document
      (rudel-send this "obby_document" 
		  (format "%x %x" owner-id id) 
		  "subscribe"
		  (format "%x" user-id))
      (setq subscribed 't)))
  )

(defmethod rudel-local-insert ((this rudel-obby-connection) document from to what)
  ""
  (with-slots (owner-id (doc-id :id) (local-revision :revision)) document
    (let ((remote-revision 0))
      (rudel-send this 
		  "obby_document" 
		  (format "%x %x" owner-id doc-id)
		  "record"
		  (format "%x" local-revision) ; user-id may be wrong
		  (format "%x" remote-revision)
		  "ins"
		  (format "%x" (- from 1))
		  what)) ; TODO escape
    (incf local-revision))
  )

(defmethod rudel-local-delete ((this rudel-obby-connection))
  ""
  )

(defmethod rudel-receive ((this rudel-obby-connection) data)
  ""
  (let* ((lines    (split-string data "\n" 't))
	 (messages (mapcar 'rudel-obby-parse-message lines))) ; TODO does not work when partial message arrives
    ;; Dispatch messages to respective handlers
    (dolist (message messages)
      (let* ((name      (car message))
	     (arguments (cdr message))
	     (method    (intern-soft (concat "rudel-obby/" name))))
	(if method
	    (apply method (cons this arguments))
	  (warn "Message not understood: `%s' with data %s" name arguments)))))
  )

(defmethod rudel-send ((this rudel-obby-connection) name &rest arguments)
  ""
  (let ((socket  (oref this :socket))
	(message (apply 'rudel-obby-assemble-message
			(cons name arguments))))
    (process-send-string socket message))
  )

(defmethod rudel-obby/net6_ping ((this rudel-obby-connection))
  ""
  (rudel-send this "net6_pong"))

(defmethod rudel-obby/net6_encryption ((this rudel-obby-connection) value)
  ""
  (let*  ((info     (oref this :info))
	  (username (plist-get info :username))
	  (color    (plist-get info :color)))
    (rudel-send this "net6_encryption_failed")
    (rudel-send this "net6_client_login" 
		username (rudel-obby-format-color color))))

(defmethod rudel-obby/net6_login_failed ((this rudel-obby-connection) reason)
  ""
  ) ; TODO

(defmethod rudel-obby/net6_client_join 
  ((this rudel-obby-connection) 
   client-id name encryption user-id color)
  ""
  (with-slots (session) this ; TODO the user can be in our list as offline user
    (let ((user (rudel-obby-user name
 	         :client-id  (string-to-number client-id 16)
		 :user-id    (string-to-number user-id 16)
		 :connected  't
		 :encryption (when (string= encryption "1") 't)
		 :color      (rudel-obby-parse-color color))))
      (rudel-add-user session user)
      (unless (slot-boundp session :self)
	(oset session :self user))))
  (message "Client joined: %s %s" name (rudel-obby-parse-color color))
  )

(defmethod rudel-obby/net6_client_part ((this rudel-obby-connection) client-id)
  ""
  (with-slots (session) this
    (let ((user (rudel-find-user 
		 session (string-to-number client-id 16)
		 'eq (lambda (user) (oref user :client-id)))))
      (oset user :connected nil)))
  )

(defmethod rudel-obby/obby_welcome ((this rudel-obby-connection) version)
  ""
  (let ((version-number (string-to-number version)))
    (message "Received Obby welcome message (version %d)" version-number))) ; TODO check version number

(defmethod rudel-obby/obby_sync_init ((this rudel-obby-connection) count) ; in hex
  ""
  )

(defmethod rudel-obby/obby_sync_final ((this rudel-obby-connection))
  ""
  )

(defmethod rudel-obby/obby_sync_usertable_user ((this rudel-obby-connection) user-id name color)
  ""
  (with-slots (session) this
    (rudel-add-user session (rudel-obby-user name
			     :user-id    (string-to-number user-id 16)
			     :connected  nil
			     :color      (rudel-obby-parse-color color))))
  )

(defmethod rudel-obby/obby_user_colour ((this rudel-obby-connection) user-id color)
    ""
    (with-slots (session) this
      (let ((user (rudel-find-user 
		   session (string-to-number user-id 16)
		   'eq (lambda (user) (oref user :user-id))))) ; TODO do we handle cases like user not found?
	(oset user :color (rudel-obby-parse-color color))))
    )

(defmethod rudel-obby/obby_sync_doclist_document 
  ((this rudel-obby-connection) 
   owner-id doc-id name suffix encoding &rest subscribed-users)
  ""
  (with-slots (session) this
    (rudel-add-document session (rudel-obby-document name 
				 :id       (string-to-number doc-id 16)
				 :owner-id (string-to-number owner-id 16))))
  (message "New document %s" name))

(defmethod rudel-obby/obby_document_create ((this rudel-obby-connection) 
					    owner-id doc-id name suffix encoding)
  ""
  (with-slots (session) this
    (rudel-add-document session (rudel-obby-document name 
				 :id       (string-to-number doc-id 16)
				 :owner-id (string-to-number owner-id 16)))) ; TODO same code in obby_sync_doclist_document
  (message "New document %s" name))

(defmethod rudel-obby/obby_document_remove ((this rudel-obby-connection) doc-id)
  ""
  (message "Document removed %d" (string-to-number doc-id)))

(defmethod rudel-obby/obby_document ((this rudel-obby-connection) 
				     owner-and-doc-id action &rest arguments)
  ""
  (with-slots (session) this
    (let* ((ids-numeric (mapcar 
			 (lambda (string) 
			   (string-to-number string 16))
			 (split-string owner-and-doc-id " " 't)))
	   (method      (intern-soft (concat "rudel-obby/obby_document/" action)))
	   ;; Locate the document
	   (document    (rudel-find-document 
			 session ids-numeric
			 (lambda (owner-and-doc-id document)
			   (equal owner-and-doc-id 
				  (list (oref document :owner-id)
					(oref document :id))))
			 'identity)))
      (if (and method document)
	  (apply method (cons this (cons document arguments)))
	(if (not method)
	    (warn "Document message not understood: `%s' with data %s" action arguments)
	  (warn "Document not found %s" owner-and-doc-id)))))
  )

(defmethod rudel-obby/obby_document/subscribe ((this rudel-obby-connection)
					       document user-id)
  ""
  )

(defmethod rudel-obby/obby_document/sync_init ((this rudel-obby-connection)
					       document size?)
  ""
  )

(defmethod rudel-obby/obby_document/sync_chunk ((this rudel-obby-connection)
						document data user-id)
  ""
  (with-slots (session) this
    (let ((user (rudel-find-user 
		 session (string-to-number user-id 16)
		 'eq (lambda (user) (oref user :user-id)))))
      (if user
	  (rudel-remote-insert document user -1 data)
	(warn "User not found %s" user-id))
      ))
  )

(defmethod rudel-obby/obby_document/record ((this rudel-obby-connection)
					    document
					    user-id revision unk3 action position data)
  ""
  (message "record %s %s" (object-name-string document) action)
  (with-slots (session) this
    (let ((user (rudel-find-user 
		 session (string-to-number user-id 16)
		 'eq (lambda (user) (oref user :user-id))))
	  (position-numeric (string-to-number position 16)))
      (if user
	  (cond
	   ((string= action "ins") 
	    (rudel-remote-insert document user (+ position-numeric 1) data))
	   ((string= action "del")
	    (rudel-remote-delete document user (+ position-numeric 1) (+ position-numeric 2) 1))
	   ((string= action "split")
	    (error "not implemented"))
	   ((string= action "noop"))
	   )
	(warn "User not found %s" user-id))
      ))
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
