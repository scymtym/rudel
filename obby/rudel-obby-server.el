;;; rudel-obby-server.el --- Server component of the Rudel obby backend
;;
;; Copyright (C) 2008, 2009 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;; Keywords: Rudel, obby, backend, server
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


;;; History:
;;
;; 1.0 - initial revision.


;;; Code:
;;

(require 'eieio)

(require 'rudel-util)


;;; Class rudel-obby-client
;;

(defclass rudel-obby-client (rudel-socket-owner)
  ((server     :initarg  :server
	       :type     rudel-obby-server
	       :documentation
	       "")
   (id         :initarg  :id
	       :type     integer
	       :accessor rudel-id
	       :documentation
	       "")
   (user       :initarg  :user
	       :type     (or rudel-obby-user null)
	       :initform nil
	       :documentation
	       "")
   (encryption :initarg  :encryption
	       :type     boolean
	       :documentation
	       ""))
  "Each object of this class represents one client, that is
connected to the server. This object handles all direct
communication with the client, while broadcast messages are
handled by the server.")

(defmethod initialize-instance :after ((this rudel-obby-client) &rest slots)
  "Sends welcome messages to the client and starts the session
timeout timer."
  ;; Send greeting sequence to the client.
  (with-slots (socket) this
    (rudel-send this 
		"obby_welcome"
		(number-to-string rudel-obby-protocol-version))
    (rudel-send this "net6_encryption" "0"))
  )

(defmethod rudel-end ((this rudel-obby-client))
  ""
  (rudel-disconnect this))

(defmethod rudel-close ((this rudel-obby-client))
  ""
  (with-slots (server) this
    (rudel-remove-client server this)))

(defmethod rudel-receive ((this rudel-obby-client) data)
  ""
  (let* ((lines    (split-string data "\n" 't))
	 (messages (mapcar 'rudel-obby-parse-message lines)))
    ;; Dispatch messages to respective handlers
    (dolist (message messages)
      (let* ((name      (car message))
	     (arguments (cdr message))
	     (method    (intern-soft (concat "rudel-obby/" name))))
	(if method
	    (apply method (cons this arguments))
	  (warn "Message not understood: `%s' with data %s" name arguments)))))
  )

(defmethod rudel-send ((this rudel-obby-client) name &rest arguments)
  ""
  (let ((socket  (oref this :socket))
	(message (apply 'rudel-obby-assemble-message
			(cons name arguments))))
    (process-send-string socket message))
  )

(defmethod rudel-broadcast ((this rudel-obby-client)
			    receivers name &rest arguments)
  "Broadcast message NAME with arguments ARGUMENTS to RECEIVERS."
  (with-slots (server) this
    (apply 'rudel-broadcast server receivers name arguments)))

(defmethod rudel-obby/net6_encryption_ok ((this rudel-obby-client))
  "Handle 'net6_encryption_ok' message.
Even if the client requests an encrypted connection, we cancel
the negotiation."
  (rudel-send this "net6_encryption_failed"))

(defmethod rudel-obby/net6_encryption_failed ((this rudel-obby-client))
  "Handle 'net6_encryption_failed' message.
No action has to be taken, since the client simply proceeds after
failed encryption negotiation.")

(defmethod rudel-obby/net6_client_login ((this rudel-obby-client) 
					 username color)
  "Handle 'net6_client_login' message."
  (with-slots (server (client-id :id) user encryption) this
    ;; Create a user object for this client and add it to the server.
    (let ((color-parsed (rudel-obby-parse-color color)))
      (setq user (rudel-make-user 
		  server 
		  username client-id color-parsed encryption)))

    (rudel-add-user server user)

    ;; Broadcast the join event to all clients (including the new
    ;; one).
    (let ((name (object-name-string user)))
      (with-slots (color (user-id :user-id)) user
	(rudel-broadcast this (list 'exclude this)
			 "net6_client_join"
			 (format "%x" client-id)
			 name
			 "0"
			 (format "%x" user-id)
			 (rudel-obby-format-color color)))))

  ;; Get the new client up to date:
  ;; - transmit user list
  ;;   - connected users
  ;;   - disconnected users
  ;; - transmit document list
  (rudel-send this 
	      "obby_sync_init"
	      (format "%x" 0))

  (with-slots (users clients documents) (oref this :server)

    ;; Transmit list of connected users.
    (dolist (client clients)
      (with-slots ((client-id :id) user) client
	(let ((name (object-name-string user)))
	  (with-slots (color (user-id :user-id)) user
	    (rudel-send this
			"net6_client_join"
			(format "%x" client-id)
			name
			"0"
			(format "%x" user-id)
			(rudel-obby-format-color color))))))

    ;; Transmit document list
    (dolist (document documents)
      (with-slots ((doc-id :id) owner-id subscribed) document
	(let ((name (object-name-string document)))
	  (apply 'rudel-send 
		 (append
		  (list this
			"obby_sync_doclist_document"
			(format "%x" owner-id)
			(format "%x" doc-id)
			name
			""
			"UTF-8")
		  (mapcar 
		   (lambda (user) 
		     (format "%x" (rudel-id user)))
		   subscribed)))))))
    
  (rudel-send this "obby_sync_final")
  )

(defmethod rudel-obby/obby_user_colour ((this rudel-obby-client)
					color-)
  "Handle 'obby_user_colour' message.
This method is called when the connected user requests a change
of his color to COLOR."
  (with-slots (color (user-id :user-id)) (oref this :user)
    (setq color (rudel-obby-parse-color color-))
    (rudel-broadcast this (list 'exclude this)
		     "obby_user_colour"
		     (format "%x" user-id)
		     (rudel-obby-format-color color)))
  )

(defmethod rudel-obby/obby_document_create ((this rudel-obby-client)
					    doc-id name encoding content)
  "Handle 'obby_document_create' message."
  (with-slots (user server) this
    (with-slots ((user-id :user-id)) user
      (let* ((doc-id-numeric (string-to-number doc-id 16))
	     ;; Create a buffer for the new document
	     (buffer         (get-buffer-create 
			      (concat 
			       " *" (generate-new-buffer-name name) "*")))
	     ;; Create the new document object
	     (document       (rudel-obby-document name
			      :buffer     buffer
			      :subscribed (list user)
                              :id         doc-id-numeric
			      :owner-id   user-id)))

	;; Add the document to the server's document list
	(rudel-add-document server document)

	;; Initialize the buffer's content
	(with-current-buffer buffer
	  (insert content))

	;; Notify other clients of the new document
	(rudel-broadcast this (list 'exclude this)
			 "obby_document_create"
			 (format "%x" user-id)
			 (format "%x" doc-id-numeric)
			 name
			 ""
			 encoding))))
  )

(defmethod rudel-obby/obby_document ((this rudel-obby-client) 
				     owner-and-doc-id action &rest arguments)
  "Handle 'obby_document' messages."
  (let* ((ids-numeric (mapcar
		       (lambda (string)
			 (string-to-number string 16))
		       (split-string owner-and-doc-id " " 't)))
	 ;; Locate the document based on owner id and document id
	 (document    (with-slots (server) this
			(rudel-find-document server ids-numeric
					     'equal 'rudel-both-ids)))
	 ;; Find the method symbol based on the action
	 (method      (intern-soft 
		       (concat "rudel-obby/obby_document/" action))))
    ;; Apply the method to the document and the remaining arguments
    (if method
	(apply method this document arguments)
      (warn "%s: `document' message not understood: `%s' with data %s" 
	    (object-name-string this) action arguments)))
  )

(defmethod rudel-obby/obby_document/subscribe ((this rudel-obby-client)
					       document user-id)
  "Handle 'subscribe' submessage of 'obby_document' message."
  (let* ((user-id-numeric (string-to-number user-id 16))
	 (user            (with-slots (server) this
			    (rudel-find-user server user-id-numeric
					     '= 'rudel-id))))
    (with-slots (owner-id (doc-id :id) subscribed buffer) document

      ;; Track subscription, handle duplicate subscription requests
      (when (memq user subscribed)
	(error "User `%s' already subscribed to document `%s'"
	       (object-name user) (object-name document)))
      (push user subscribed)

      ;; Synchronize the buffer content to the client.
      (with-current-buffer buffer
	;; Send overall buffer size
	(rudel-send this
		    "obby_document"
		    (format "%x %x" owner-id doc-id)
		    "sync_init"
		    (format "%x" (- (point-max) 1)))
	;; Send buffer content
	(rudel-send this
		    "obby_document"
		    (format "%x %x" owner-id doc-id)
		    "sync_chunk"
		    (buffer-string)
		    "0"))
    
      ;; Notify clients of the new subscription (including our own
      ;; client, who requested the subscription).
      (with-slots ((user-id :user-id)) user
	(rudel-broadcast this nil
			 "obby_document"
			 (format "%x %x" owner-id doc-id)
			 "subscribe"
			 (format "%x" user-id)))))
  )

(defmethod rudel-obby/obby_document/unsubscribe ((this rudel-obby-client)
						 document user-id)
  "Handle 'unsubscribe' submessage of 'obby_document' message."
  (let* ((user-id-numeric (string-to-number user-id 16))
	 (user            (with-slots (server) this
			    (rudel-find-user server user-id-numeric
					     '= 'rudel-id))))
    (with-slots (owner-id (doc-id :id) subscribed) document

      ;;
      (unless (memq user subscribed)
	(error "User `%s' not subscribed to document `%s'"
	       (object-name user) (object-name document)))
      (setq subscribed (delq user subscribed))

      ;; Notify clients of the canceled subscription (including our
      ;; own client, who requested being unsubscribed).
      (with-slots ((user-id :user-id)) user
	(rudel-broadcast this nil
			 "obby_document"
			 (format "%x %x" owner-id doc-id)
			 "unsubscribe"
			 (format "%x" user-id)))))
  )

(defmethod rudel-obby/obby_document/record ((this rudel-obby-client)
					    document
					    local-revision remote-revision
					    action &rest arguments)
  "Handle 'record' submessages of 'obby_document' message."
  (let ((method                  (intern-soft
				  (concat 
				   "rudel-obby/obby_document/record/" 
				   action)))
	(local-revision-numeric  (string-to-number local-revision 16))
	(remote-revision-numeric (string-to-number remote-revision 16)))
    (if method
	(apply method this 
	       document
	       local-revision-numeric remote-revision-numeric
	       arguments)
      (warn "%s: `document/record' message not understood: `%s' with data %s" 
	    (object-name-string this) action arguments)))
  )

(defmethod rudel-obby/obby_document/record/ins ((this rudel-obby-client)
						document
						local-revision remote-revision
						position data)
  ""
  (with-slots ((client-id :id) user) this
    (let ((position-numeric (string-to-number position 16)))
   
      ;; Incorporate change into DOCUMENT.
      (rudel-remote-insert document user position-numeric data)

      ;; Relay change notification to other clients.
      (with-slots (owner-id (doc-id :id)) document
	(rudel-broadcast this (list 'exclude this)
			 "obby_document"
			 (format "%x %x" owner-id doc-id)
			 "record"
			 (format "%x" client-id)
			 (format "%x" local-revision)
			 (format "%x" remote-revision)
			 "ins"
			 (format "%x" position-numeric)
			 data))))
  )

(defmethod rudel-obby/obby_document/record/del ((this rudel-obby-client)
						document
						local-revision remote-revision
						position length)
  ""
  (with-slots ((client-id :id) user) this
    (let ((position-numeric (string-to-number position 16))
	  (length-numeric   (string-to-number length   16)))

      ;; Incorporate change into DOCUMENT.
      (rudel-remote-delete document user position-numeric length-numeric)

      ;; Relay change notification to other clients.
      (with-slots (owner-id (doc-id :id)) document
	(rudel-broadcast this (list 'exclude this)
			 "obby_document"
			 (format "%x %x" owner-id doc-id)
			 "record"
			 (format "%x" client-id)
			 (format "%x" local-revision)
			 (format "%x" remote-revision)
			 "del"
			 (format "%x" position-numeric)
			 (format "%x" length-numeric)))))
  )


;;; Class rudel-obby-server
;;

(defclass rudel-obby-server (rudel-server-session rudel-socket-owner)
  ((clients        :initarg  :clients
		   :type     list
		   :initform nil
		   :documentation
		   "")
   (next-client-id :initarg  :next-client-id
		   :type     integer
		   :initform 0
		   :documentation
		   "")
   (next-user-id   :initarg  :next-user-id
		   :type     integer
		   :initform 0
		   :documentation
		   ""))
  "Class rudel-obby-server ")

(defmethod rudel-end ((this rudel-obby-server))
  ""
  (rudel-disconnect this))

(defmethod rudel-broadcast ((this rudel-obby-server)
			    receivers name &rest arguments)
  "Send a message of type NAME with arguments ARGUMENTS to RECEIVERS.

RECEIVERS can be a object derived from rudel-obby-client, a list
of such objects or a list with car 'exclude and cdr a list of
such objects derived from rudel-obby-client."
  ;; Construct list of receivers.
  (let ((receiver-list
	 (cond
	  ;; If RECEIVERS is nil, the message should be broadcast to
	  ;; all clients.
	  ((null receivers) (oref this :clients))
	  ;; If RECEIVERS is a (non-empty) list of rudel-obby-client
	  ;; (or derived) objects, treat it as a list of receivers.
	  ((and (listp receivers) 
		(rudel-obby-client-child-p (car receivers)))
	   receivers)
	  ;; If RECEIVERS is a (non-empty) list with cdr equal to
	  ;; 'exclude treat it as a list of receivers to exclude.
	  ((and (listp receivers)
		(eq (car receivers) 'exclude))
	   (with-slots (clients) this
	     (set-difference clients (cdr receivers)
			     :key 'rudel-id)))
	  ;; If RECEIVERS is a single rudel-obby-client (or derived)
	  ;; object, send the message to that client.
	  ((rudel-obby-client-child-p receivers)
	   (list receivers))
	  ;;
	  ('t (error "Wrong argument type")))))

    ;; Send message to receivers.
    (dolist (receiver receiver-list)
      (apply 'rudel-send receiver name arguments)))
  )

(defmethod rudel-make-user ((this rudel-obby-server)
			    name client-id color encryption)
  ""
  (with-slots (next-user-id) this
    (let ((user (rudel-obby-user name
                 :color      color
		 :client-id  client-id
		 :user-id    next-user-id
		 :connected  't
		 :encryption encryption)))
      (incf next-user-id)
      user))
  )

(defmethod rudel-add-client ((this rudel-obby-server) 
			     client-socket)
  ""
  (with-slots (next-client-id clients) this
    (let ((client (rudel-obby-client (process-name client-socket)
		   :server     this
		   :socket     client-socket
		   :id         next-client-id
		   :encryption nil)))
      (push client clients))
    (incf next-client-id))
  )

(defmethod rudel-remove-client ((this rudel-obby-server)
				client)
  ""
  (with-slots ((client-id :id) user) client
    ;; Broadcast the part event
    (rudel-broadcast this (list 'exclude client)
		     "net6_client_part"
		     (format "%x" client-id))

    ;; Set the user object to offline
    (with-slots (connected) user
      (setq connected nil)))

  (object-remove-from-list this :clients client)
  )

(provide 'rudel-obby-server)
;;; rudel-obby-server.el ends here
