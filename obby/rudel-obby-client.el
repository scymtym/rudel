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
;; 1.0 - Initial revision.


;;; Code:
;;

(require 'eieio)

(require 'rudel-obby-util)


;;; Class rudel-obby-connection
;;

(defclass rudel-obby-connection (rudel-obby-socket-owner
				 rudel-connection)
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
  (call-next-method))

(defmethod rudel-close ((this rudel-obby-connection))
  ""
  (with-slots (session) this
    (rudel-end session)))

(defmethod rudel-change-color- ((this rudel-obby-connection) color)
  ""
  (with-slots (socket) this
    (rudel-send this "obby_user_colour" 
		(rudel-obby-format-color color)))
  )

(defmethod rudel-publish ((this rudel-obby-connection) document)
  ""
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
  (let* ((session (oref this :session))
	 (user-id (oref (oref session :self) :user-id)))
    (with-slots (id owner-id subscribed) document
      (rudel-send this "obby_document" 
		  (format "%x %x" owner-id id) 
		  "subscribe"
		  (format "%x" user-id))))
  )

(defmethod rudel-unsubscribe-from ((this rudel-obby-connection) document)
  ""
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
  (with-slots (owner-id (doc-id :id) (local-revision :revision)) document
    (let ((remote-revision 0))
      (rudel-send this 
		  "obby_document" 
		  (format "%x %x" owner-id doc-id)
		  "record"
		  (format "%x" local-revision)
		  (format "%x" remote-revision)
		  "ins"
		  (format "%x" position)
		  data))
    (incf local-revision))
  )

(defmethod rudel-local-delete ((this rudel-obby-connection)
			       document position length)
  ""
  (with-slots (owner-id (doc-id :id) (local-revision :revision)) document
    (let ((remote-revision 0))
      (rudel-send this 
		  "obby_document" 
		  (format "%x %x" owner-id doc-id)
		  "record"
		  (format "%x" local-revision)
		  (format "%x" remote-revision)
		  "del"
		  (format "%x" position)
		  (format "%x" length)))
    (incf local-revision))
  )

(defmethod rudel-message ((this rudel-obby-connection) message)
  "Dispatch MESSAGE to appropriate handler method of THIS object.
If there is no suitable method, generate a warning, but do
nothing else."
  ;; Dispatch message to handler
  (let* ((name      (car message))
	 (arguments (cdr message))
	 (method    (intern-soft (concat "rudel-obby/" name))))
    ;; If we found a suitable method, run it; Otherwise warn and do
    ;; nothing.
    (unless (and method
		 (condition-case error
		     (progn
		       (apply method this arguments)
		       't)
		   (no-method-definition nil)))
      (warn "%s: message not understood: `%s' with data %s" 
	    (object-name-string this) name arguments)))
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
  )

(defmethod rudel-obby/net6_client_join ((this rudel-obby-connection) 
					client-id name encryption user-id color)
  "Handle 'net6_client_join message."
  (with-slots (session) this ; TODO the user can be in our list as offline user
    (let ((user (rudel-obby-user name
 	         :client-id  (string-to-number client-id 16)
		 :user-id    (string-to-number user-id   16)
		 :connected  't
		 :encryption (when (string= encryption "1") 't)
		 :color      (rudel-obby-parse-color color))))
      (rudel-add-user session user)
      (unless (slot-boundp session :self)
	(oset session :self user))))
  (message "Client joined: %s %s" name (rudel-obby-parse-color color))
  )

(defmethod rudel-obby/net6_client_part ((this rudel-obby-connection) client-id)
  "Handle 'net6_client_part' message."
  ;; Find the user object, associated to the client id. Remove the
  ;; client id and make the user to disconnected.
  (with-slots (session) this
    (let* ((client-id-numric (string-to-number client-id 16))
	   (user             (rudel-find-user session client-id-numric
					      'eq 'rudel-client-id)))
      (with-slots (client-id connected) user
	(setq client-id nil
	      connected nil))))
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
					     'eq 'rudel-id)))
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
	  (owner-id-numeric (string-to-number owner-id 16)))

      ;; Make a new document with the list of subscribed users.
      (rudel-add-document session (rudel-obby-document name
				   :subscribed subscribed-users
				   :id         doc-id-numeric
				   :owner-id   owner-id-numeric)))
  (message "New document %s" name))
  )

(defmethod rudel-obby/obby_document_create ((this rudel-obby-connection)
					    owner-id doc-id name suffix encoding)
  ""
  (with-slots (session) this
    (let* ((owner-id-numeric (string-to-number owner-id 16))
	   (doc-id-numeric   (string-to-number doc-id 16))
	   (owner            (rudel-find-user session owner-id-numeric
					      '= 'rudel-id)))
      (rudel-add-document session (rudel-obby-document name
				   :subscribed (list owner)
				   :id         doc-id-numeric
				   :owner-id   owner-id-numeric))))
  (message "New document %s" name)
  )

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
			      (rudel-find-user 
			       session user-id-numeric
			       'eq (lambda (user) (oref user :user-id))))))
      (rudel-remote-insert document user -1 data)))
  )

(defmethod rudel-obby/obby_document/record ((this rudel-obby-connection)
					    document user-id 
					    local-revision remote-revision
					    action &rest arguments)
  ""
  (with-slots (session) this
    (let ((user                    (rudel-find-user 
				    session (string-to-number user-id 16)
				    'eq (lambda (user)
					  (oref user :user-id))))
	  (method                  (intern-soft
				    (concat 
				     "rudel-obby/obby_document/record/" 
				     action)))
	  (local-revision-numeric  (string-to-number local-revision 16))
	  (remote-revision-numeric (string-to-number remote-revision 16)))
      (if user
	  (apply method
		 this document user 
		 local-revision-numeric remote-revision-numeric
		 arguments)
	(warn "User not found %s" user-id))))
  )

(defmethod rudel-obby/obby_document/record/ins ((this rudel-obby-connection)
						document user
						local-revision remote-revision
						position data)
  ""
  (let ((position-numeric (string-to-number position 16)))
    (rudel-remote-insert document user position-numeric data))
  )

(defmethod rudel-obby/obby_document/record/del ((this rudel-obby-connection)
						document user
						local-revision remote-revision
						position length)
  ""
  (let ((position-numeric (string-to-number position 16))
	(length-numeric   (string-to-number length   16)))
    (rudel-remote-delete document user 
			 position-numeric length-numeric))
  )

(provide 'rudel-obby-client)
;;; rudel-obby-client.el ends here
