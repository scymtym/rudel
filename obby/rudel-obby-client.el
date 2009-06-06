;;; rudel-obby-client.el --- Client functions of the Rudel obby backend
;;
;; Copyright (C) 2008, 2009 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: Rudel, obby, backend, client
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
;; This file contains the client part of the obby backend.


;;; History:
;;
;; 0.1 - Initial revision.


;;; Code:
;;

(require 'eieio)

(require 'jupiter)

(require 'rudel-operations)

(require 'rudel-obby)
(require 'rudel-obby-util)


;;; Class rudel-obby-connection
;;

(defclass rudel-obby-connection (rudel-obby-socket-owner
				 rudel-connection)
  ((info     :initarg :info
	     :type    list
	     :documentation
	     "Stores connection information for later use.")
   (contexts :initarg :contexts
	     :type    hash-table
	     :documentation
	     "Contains jupiter context objects for all
documents."))
  "Class rudel-obby-connection ")

(defmethod initialize-instance :after ((this rudel-obby-connection) slots)
  ;; Create a new hash-table object to hold jupiter contexts
  ;; associated to documents.
  (with-slots (contexts) this
    (setq contexts (make-hash-table :test 'equal))))

(defmethod rudel-disconnect ((this rudel-obby-connection))
  ""
  (call-next-method))

(defmethod rudel-close ((this rudel-obby-connection))
  ""
  (with-slots (session) this
    (rudel-end session)))

(defmethod rudel-find-context ((this rudel-obby-connection) document)
  "Return the jupiter context associated to DOCUMENT in THIS connection."
  (with-slots (contexts) this
    (gethash (oref document :id) contexts)))

(defmethod rudel-add-context ((this rudel-obby-connection) document)
  "Add a jupiter context for DOCUMENT to THIS connection."
  (with-slots (contexts) this
    (let ((doc-name (object-name-string document)))
      (puthash
       (oref document :id)
       (jupiter-context (format "%s" doc-name))
       contexts)))
  )

(defmethod rudel-remove-context ((this rudel-obby-connection) document)
  "Remove the jupiter context associated to DOCUMENT from THIS connection."
  (with-slots (contexts) this
    (remhash (oref document :id) contexts)))

(defmethod rudel-message ((this rudel-obby-connection) message)
  "Dispatch MESSAGE to appropriate handler method of THIS object.
If there is no suitable method, generate a warning, but do
nothing else."
  ;; Dispatch message to handler
  (let ((name      (car message))
	(arguments (cdr message)))
    (rudel-obby-dispatch this name arguments)))

(defmethod rudel-change-color- ((this rudel-obby-connection) color)
  ""
  (with-slots (socket) this
    (rudel-send this "obby_user_colour" 
		(rudel-obby-format-color color)))
  )

(defmethod rudel-publish ((this rudel-obby-connection) document)
  ""
  ;; Create a new jupiter context for DOCUMENT.
  (rudel-add-context this document)

  ;; Announce the new document to the server.
  (let ((name (object-name-string document)))
    (with-slots (id buffer) document
      (rudel-send this "obby_document_create"
		  (format "%x" id)
		  name
		  "UTF-8"
		  (with-current-buffer buffer
		    (buffer-string)))))
  )

(defmethod rudel-subscribe-to ((this rudel-obby-connection) document)
  ""
  ;; Create a new jupiter context for DOCUMENT.
  (rudel-add-context this document)

  ;; Announce the subscription to the server.
  (with-slots (session) this
    (with-slots ((user-id :user-id)) (oref session :self)
      (with-slots (id owner-id subscribed) document
	(rudel-send this "obby_document"
		    (format "%x %x" owner-id id)
		    "subscribe"
		    (format "%x" user-id)))))
  
  ;; We receive a notification of our own subscription from the
  ;; server. Consequently we do not add SELF to the list of subscribed
  ;; users of DOCUMENT.
  )

(defmethod rudel-unsubscribe-from ((this rudel-obby-connection) document)
  ""
  ;; Delete the jupiter context for DOCUMENT.
  (rudel-remove-context this document)

  ;; Announce the end of our subscription to the server.
  (with-slots (session) this
    (with-slots (user-id) (oref session :self)
      (with-slots (id owner-id subscribed) document
	(rudel-send this "obby_document"
		    (format "%x %x" owner-id id)
		    "unsubscribe"
		    (format "%x" user-id)))))
  
  ;; We receive a notification of the end of our own subscription from
  ;; the server. Consequently we do not remove SELF from the list of
  ;; subscribed users of DOCUMENT.
  )

(defmethod rudel-local-insert ((this rudel-obby-connection)
			       document position data)
  ""
  (rudel-local-operation 
   this 
   document 
   (jupiter-insert "insert" :from position :data data)))

(defmethod rudel-local-delete ((this rudel-obby-connection)
			       document position length)
  ""
  (rudel-local-operation 
   this 
   document 
   (jupiter-delete "delete" :from position :to (+ position length))))

(defmethod rudel-local-operation ((this rudel-obby-connection) 
				  document operation)
  "Handle OPERATION performed on DOCUMENT by sending a message through THIS connection."
  ;; Find jupiter context for DOCUMENT.
  (let ((context (rudel-find-context this document)))

    ;; Notify the server of the deletion
    (with-slots (owner-id (doc-id :id)) document
      (with-slots (local-revision remote-revision) context
	(apply 'rudel-send
	       this
	       "obby_document"
	       (format "%x %x" owner-id doc-id)
	       "record"
	       (format "%x" local-revision)
	       (format "%x" remote-revision)
	       (rudel-operation->message operation))))

      ;; Submit the operation to the jupiter context.
      (jupiter-local-operation context operation))
  )

(defmethod rudel-remote-operation ((this rudel-obby-connection)
				   document user
				   remote-revision local-revision 
				   operation)
  "Handle OPERATION received through THIS connection performed by USER on DOCUMENT."
  (let* (;; Find jupiter context for DOCUMENT.
	 (context     (rudel-find-context this document))
	 ;; And transform the operation.
	 (transformed (jupiter-remote-operation
		       context 
		       remote-revision local-revision
		       operation)))

    ;; Apply the transformed operation to the document.
    (rudel-remote-operation document user transformed))
  )

(defmethod rudel-obby/net6_ping ((this rudel-obby-connection))
  "Handle net6 'ping' message."
  (rudel-send this "net6_pong"))

(defmethod rudel-obby/net6_encryption ((this rudel-obby-connection) value)
  "Handle net6 'encryption' message."
  (rudel-send this "net6_encryption_ok"))

(defmethod rudel-obby/net6_encryption_begin ((this rudel-obby-connection))
  "Handle net6 'encryption_begin' message."
  ;; Start TLS encryption for the connection.
  (require 'rudel-tls)
  (with-slots (socket) this
    (rudel-tls-start-tls socket)
    (sit-for 1)) ;; TODO not too pretty

  ;; Send login request with username and color. This can easily fail
  ;; (resulting in response 'net6_login_failed') if the username or
  ;; color is already taken.
  (with-slots (info) this
    (let ((username (plist-get info :username))
	  (color    (plist-get info :color)))
      (rudel-send this 
		  "net6_client_login"
		  username (rudel-obby-format-color color))))
  )

(defmethod rudel-obby/net6_encryption_failed ((this rudel-obby-connection))
  "Handle net6 'encryption_failed' message."
  ;; We ignore encryption errors unless encryption has been requested
  ;; explicitly.
  (with-slots (info) this
    (if (plist-get info :encryption)
      (error "Enabling encryption failed")

      ;; If encryption has not been requested in the first place, send
      ;; login request with username and color. This can easily fail
      ;; (resulting in response 'net6_login_failed') if the username
      ;; or color is already taken.
      (let ((username (plist-get info :username))
	    (color    (plist-get info :color)))
	(rudel-send this 
		    "net6_client_login"
		    username (rudel-obby-format-color color)))))
  )

(defmethod rudel-obby/net6_login_failed ((this rudel-obby-connection) reason)
  "Handle net6 'encryption_failed' message."
  )

(defmethod rudel-obby/net6_client_join ((this rudel-obby-connection) 
					client-id name encryption user-id color)
  "Handle 'net6_client_join message."
  (with-slots (session) this ; TODO the user can be in our list as offline user
    (let ((user (rudel-obby-user name
 	         :client-id  (string-to-number client-id 16)
		 :user-id    (string-to-number user-id   16)
		 :connected  t
		 :encryption (string= encryption "1")
		 :color      (rudel-obby-parse-color color))))
      (rudel-add-user session user)
      (unless (slot-boundp session :self)
	(oset session :self user))))
  (message "Client joined: %s %s" name (rudel-obby-parse-color color))
  )

(defmethod rudel-obby/net6_client_part ((this rudel-obby-connection) client-id)
  "Handle net6 'client_part' message."
  ;; Find the user object, associated to the client id. Remove the
  ;; client id and change the user's state to disconnected.
  (with-slots (session) this
    (let* ((client-id-numeric (string-to-number client-id 16))
	   (user             (rudel-find-user session client-id-numeric
					      #'eql #'rudel-client-id)))
      (if user
	  (with-slots (client-id connected) user
	    (setq client-id nil
		  connected nil))
	(warn "Unknown user: %d" client-id-numeric))))
  )

(defmethod rudel-obby/obby_welcome ((this rudel-obby-connection) version)
  ""
  (let ((version-number (string-to-number version)))
    (message "Received Obby welcome message (version %d)" version-number))) ; TODO check version number

(defmethod rudel-obby/obby_sync_init ((this rudel-obby-connection) count)
  ""
  )

(defmethod rudel-obby/obby_sync_final ((this rudel-obby-connection))
  ""
  )

(defmethod rudel-obby/obby_sync_usertable_user ((this rudel-obby-connection)
						user-id name color)
  ""
  (with-slots (session) this
    (let ((user-id-numric (string-to-number user-id 16))
	  (color-parsed   (rudel-obby-parse-color color)))
      (rudel-add-user session (rudel-obby-user name
			      :user-id    user-id-numric
			      :connected  nil
			      :color      color-parsed))))
  )

(defmethod rudel-obby/obby_user_colour ((this rudel-obby-connection) 
					user-id color)
  ""
  (with-slots (session) this
    (let* ((user-id-numeric (string-to-number user-id 16))
	   (color-parsed    (rudel-obby-parse-color color))
	   (user            (rudel-find-user session user-id-numeric
					     '= 'rudel-id)))
      (oset user :color color-parsed)))
  )

(defmethod rudel-obby/obby_sync_doclist_document
  ((this rudel-obby-connection)
   owner-id doc-id name suffix encoding &rest subscribed-user-ids)
  ""
  (with-slots (session) this

    ;; Retrieve the subscribed users
    (let ((subscribed-users
	   (mapcar
	    (lambda (user-id)
	      (rudel-find-user 
	       session (string-to-number user-id 16)
	       '= 'rudel-id))
	    subscribed-user-ids))
	  (doc-id-numeric   (string-to-number doc-id 16))
	  (owner-id-numeric (string-to-number owner-id 16))
	  (suffix-numeric   (string-to-number suffix 16)))

      ;; Make a new document with the list of subscribed users.
      (rudel-add-document session (rudel-obby-document name
				   :subscribed subscribed-users
				   :id         doc-id-numeric
				   :owner-id   owner-id-numeric
				   :suffix     suffix-numeric)))
  (message "New document %s" name))
  )

(defmethod rudel-obby/obby_document_create ((this rudel-obby-connection)
					    owner-id doc-id name suffix encoding)
  ""
  (with-slots (session) this
    (let* ((owner-id-numeric (string-to-number owner-id 16))
	   (doc-id-numeric   (string-to-number doc-id 16))
	   (suffix-numeric   (string-to-number suffix 16))
	   (owner            (rudel-find-user session owner-id-numeric
					      '= 'rudel-id)))
      (rudel-add-document session (rudel-obby-document name
				   :subscribed (list owner)
				   :id         doc-id-numeric
				   :owner-id   owner-id-numeric
				   :suffix     suffix-numeric))))
  (message "New document %s" name)
  )

(defmethod rudel-obby/obby_document_remove ((this rudel-obby-connection) doc-id)
  ""
  (message "Document removed %d" (string-to-number doc-id)))

(defmethod rudel-obby/obby_document ((this rudel-obby-connection)
				     owner-and-doc-id action &rest arguments)
  ""
  (with-slots (session) this
    ;; First parse the owner and document id ...
    (let* ((ids-numeric (mapcar
			 (lambda (string)
			   (string-to-number string 16))
			 (split-string owner-and-doc-id " " t)))
	   ;; ... then locate the document based on owner id and
	   ;; document id
	   (document    (rudel-find-document session ids-numeric
					     'equal 'rudel-both-ids)))
      (if document
	  ;; Dispatch message to handler
	  (rudel-obby-dispatch
	   this action
	   (append (list document) arguments)
	   "rudel-obby/obby_document/")
	;; If we did not find the document, warn
	(warn "Document not found: %s" owner-and-doc-id))))
  )

(defmethod rudel-obby/obby_document/rename ((this rudel-obby-connection)
					    document 
					    user new-name new-suffix)
  "Handle obby 'rename' submessage of the 'obby_document' message."
  (let ((new-suffix-numeric (string-to-number new-suffix 16)))
    (with-slots (suffix) document
      (object-set-name-string document new-name)
      (setq suffix new-suffix-numeric)))
  )

(defmethod rudel-obby/obby_document/subscribe ((this rudel-obby-connection)
					       document user-id)
  ""
  (with-slots (session) this
    (let* ((user-id-numeric (string-to-number user-id 16))
	   (user            (rudel-find-user
			     session (string-to-number user-id 16)
			     '= 'rudel-id)))
      (object-add-to-list document :subscribed user)))
  )

(defmethod rudel-obby/obby_document/unsubscribe ((this rudel-obby-connection)
						 document user-id)
  ""
  (with-slots (session) this
    (let* ((user-id-numeric (string-to-number user-id 16))
	   (user            (rudel-find-user session user-id-numeric
					     '= 'rudel-id)))
      (object-remove-from-list document :subscribed user)))
  )

(defmethod rudel-obby/obby_document/sync_init ((this rudel-obby-connection)
					       document size?)
  ""
  )

(defmethod rudel-obby/obby_document/sync_chunk ((this rudel-obby-connection)
						document data user-id)
  ""
  (with-slots (session) this
    (let* ((user-id-numeric (string-to-number user-id 16))
	   (user            (unless (zerop user-id-numeric)
			      (rudel-find-user session user-id-numeric
					       '= 'rudel-id)))
	   (operation       (rudel-insert-op "bulk-insert"
					     :from nil
					     :data data)))
      (rudel-remote-operation document user operation)))
  )

(defmethod rudel-obby/obby_document/record ((this rudel-obby-connection)
					    document user-id 
					    local-revision remote-revision
					    action &rest arguments)
  ""
  (with-slots (session) this
    ;; Find the user
    (let* ((user-id-numeric         (string-to-number user-id 16))
	   (user                    (rudel-find-user session user-id-numeric
						     '= 'rudel-id))
	   (local-revision-numeric  (string-to-number local-revision 16))
	   (remote-revision-numeric (string-to-number remote-revision 16)))
      (if user
	  ;; Dispatch message to handler
	  (rudel-obby-dispatch
	   this action
	   (append 
	    (list document user
		  local-revision-numeric remote-revision-numeric)
	    arguments)
	   "rudel-obby/obby_document/record/")
	;; If we did not find the user, warn
	(warn "User not found: %s" user-id))))
  )

(defmethod rudel-obby/obby_document/record/ins ((this rudel-obby-connection)
						document user
						local-revision remote-revision
						position data)
  ""
  (let* ((position-numeric (string-to-number position 16))
	 (operation        (jupiter-insert
			    (format "insert-%d-%d"
				    remote-revision local-revision)
			    :from position-numeric
			    :data data)))
    (rudel-remote-operation this
			    document user
			    remote-revision local-revision
			    operation))
  )

(defmethod rudel-obby/obby_document/record/del ((this rudel-obby-connection)
						document user
						local-revision remote-revision
						position length)
  ""
  (let* ((position-numeric (string-to-number position 16))
	 (length-numeric   (string-to-number length   16))
	 (operation	   (jupiter-delete 
			    (format "delete-%d-%d" 
				    remote-revision local-revision)
			    :from position-numeric 
			    :to   (+ position-numeric length-numeric))))
    (rudel-remote-operation this
			    document user
			    remote-revision local-revision
			    operation))
  )

(defmethod rudel-obby/obby_document/record/split ((this rudel-obby-connection)
						  document user
						  local-revision remote-revision
						  &rest operations)
  ""
  (let ((operation (rudel-message->operation
		    (cons "split" operations)
		    local-revision remote-revision)))
    (rudel-remote-operation this
			    document user
			    remote-revision local-revision
			    operation))
  )

(defmethod rudel-obby/obby_document/record/noop ((this rudel-obby-connection)
						 document user
						 local-revision remote-revision)
  ""
  (let ((operation (jupiter-nop 
		    (format "nop-%d-%d" 
			    remote-revision local-revision))))
    (rudel-remote-operation this 
			    document user
			    remote-revision local-revision
			    operation))
  )

(provide 'rudel-obby-client)
;;; rudel-obby-client.el ends here
