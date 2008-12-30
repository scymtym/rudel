;;; rudel.el --- A collaborative editing framework for Emacs
;;
;; Copyright (C) 2008 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;; Keywords: rudel, collaboration
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
;; Rudel is a framework for collaborative editing in Emacs.  Its
;; architecture allows communication with arbitrary collaborative
;; editors.

;;; History:
;; 
;; 0.1 - Initial revision.

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'eieio)


;;; Global variables
;;

(defconst rudel-version 0.1
  "Version of the Rudel framework.")

;;;###autoload
(defvar rudel-backends nil
  "List of Rudel backends.")

(defvar rudel-current-session nil
  "Global object representing the current Rudel session.
nil if there is no active session.")

(defvar rudel-buffer-document nil
  "Buffer-local variable which holds the rudel document associated with the buffer.")
(make-variable-buffer-local 'rudel-buffer-document)


;;; Customization
;;

(defgroup rudel nil
  "Rudel collaborative editing framework."
  :group 'applications)


;;; Class rudel-backend
;;

(defclass rudel-backend ()
  ((capabilities :initarg  :capabilities
		 :type     list
		 :initform nil
		 :documentation
		 ""))
  "Class rudel-backend "
  :abstract 't)

(defmethod rudel-capable-of-p ((this rudel-backend) capability)
  ""
  (with-slots (capabilities) this
    (member capability capabilities)))

(defmethod rudel-ask-connect-info ((this rudel-backend))
  ""
  (error "Needs to be implemented in derived classes"))

(defmethod rudel-connect ((this rudel-backend) info)
  "Create a new connection according to the data in the property list INFO.
Implementations can rely on the fact that the property :session
contains the rudel-session object to which the new connection
will be associated."
  (error "Needs to be implemented in derived classes"))

(defmethod rudel-ask-host-info ((this rudel-backend))
  ""
  (error "Needs to be implemented in derived classes"))

(defmethod rudel-host ((this rudel-backend) info)
  ""
  (error "Needs to be implemented in derived classes"))

(defmethod rudel-make-document ((this rudel-backend) name session)
  ""
  (error "Needs to be implemented in derived classes"))


;;; Class rudel-session
;;

(defclass rudel-session ()
  ((connection :initarg :connection
	       :documentation
	       "")
   (users      :initarg  :users
	       :type     list
	       :initform nil
	       :documentation
	       "")
   (self       :initarg  :self
	       :type     rudel-user ; TODO initform?
	       :documentation
	       "Points into USERS to the user object representing the local user")
   (documents  :initarg  :documents
	       :type     list
	       :initform nil
	       :documentation
	       ""))
  "Class rudel-session ")

(defmethod rudel-end ((this rudel-session))
  "Terminate THIS session performing all necessary cleanup."
  ;; Clean everything up
  (with-slots (connection users documents) this
    (mapc 'rudel-detach-from-buffer documents)
    ;; Terminate the connection
    (rudel-disconnect connection))
  )

(defmethod rudel-add-user ((this rudel-session) user)
  "Add USER to the user list of THIS session."
  (with-slots (users) this
    (push user users)))

(defmethod rudel-remove-user ((this rudel-session) user)
  "Remove USER from the user list of THIS session."
  (with-slots (users) this
    (setq users (remove user users))))

(defmethod rudel-find-user ((this rudel-session)
			    which &optional test key)
  ""
  (unless test
    (setq test 'string=))
  (unless key
    (setq key 'object-name-string))
  (with-slots (users) this
    (find which users :key key :test test))
  )

(defmethod rudel-add-document ((this rudel-session) document)
  ""
  (unless (slot-boundp document :session)
    (oset document :session this)) ; TODO do we want to do this here?
  (with-slots (documents) this
    (push document documents))
  )

(defmethod rudel-remove-document ((this rudel-session) document)
  ""
  (with-slots (documents) this
    (setq documents (remove document documents))))

(defmethod rudel-find-document ((this rudel-session)
				which &optional test key)
  ""
  (unless test
    (setq test 'string=))
  (unless key
    (setq key 'object-name-string))
  (with-slots (documents) this
    (find which documents :key key :test test))
  )


;;; Class rudel-connection
;;

(defclass rudel-connection ()
  ((session :initarg :session
	    :type    rudel-session-child
	    :documentation
	    ""))
  "Class rudel-connection "
  :abstract 't)

(defmethod rudel-disconnect ((this rudel-connection))
  "Close the connection."
  (error "Needs to be implemented in derived classes"))

(defmethod rudel-change-color- ((this rudel-connection) color)
  ""
  (error "Needs to be implemented in derived classes"))

(defmethod rudel-publish ((this rudel-connection) document)
  ""
  (error "Needs to be implemented in derived classes"))

(defmethod rudel-subscribe-to ((this rudel-connection) document) ; TODO name should be rudel-subscribe
  ""
  (error "Needs to be implemented in derived classes"))

(defmethod rudel-unsubscribe-from ((this rudel-connection) document) ; TODO name should be rudel-unsubscribe
  (error "Needs to be implemented in derived classes"))

(defmethod rudel-local-insert ((this rudel-connection))
  ""
  (error "Needs to be implemented in derived classes"))
  
(defmethod rudel-local-delete ((this rudel-connection))
  ""
  (error "Needs to be implemented in derived classes"))

(defmethod rudel-remote-insert ((this rudel-connection))
  ""
  (error "Needs to be implemented in derived classes"))
  
(defmethod rudel-remote-delete ((this rudel-connection))
  ""
  (error "Needs to be implemented in derived classes"))


;;; Class rudel-user
;;

(defclass rudel-user (eieio-speedbar-file-button)
  ((color :initarg :color
	  :documentation
	  "Color used to indicate ownership or authorship by the
user. Examples includes text written by the user or the user name
itself."))
  "Objects of this class represent users participating in
collaborative editing session. Note that a participating user
does not have to be connected to the session at any given time.")
; abstract


;;; Class rudel-document
;;

(defclass rudel-document (eieio-speedbar-file-button)
  ((session    :initarg  :session
	       :type     rudel-session
	       :documentation
	       "")
   (buffer     :initarg  :buffer
	       :type     (or null buffer)
	       :initform nil
	       :documentation
	       "")
   (subscribed :initarg  :subscribed
	       :type     list ; TODO of rudel-user-child
	       :initform nil
	       :documentation
	       ""))
  "This class represents a document, which participants of a
collaborative editing session can subscribe to."
  :abstract 't)

(defmethod rudel-attach-to-buffer ((this rudel-document) the-buffer)
  ""
  (with-slots (buffer) this
    (setq buffer the-buffer)
    (with-current-buffer buffer
      (setq rudel-buffer-document this)

      ;; Add the handler function for buffer changes to the buffer's
      ;; change hook.
      (add-hook 'after-change-functions
		'rudel-handle-buffer-change
		nil 't)))
  )

(defmethod rudel-detach-from-buffer ((this rudel-document))
  ""
  (with-slots (buffer) this
    (with-current-buffer buffer
      (setq rudel-buffer-document nil)
      (remove-hook 'after-change-functions
		   'rudel-handle-buffer-change)
      (setq buffer nil)))
  )

(defmethod rudel-local-insert ((this rudel-document) from to what)
  ""
  (let ((connection (oref (oref this :session) :connection)))
    (rudel-local-insert connection this from to what)))

(defmethod rudel-local-delete ((this rudel-document) from to length)
  ""
  (let ((connection (oref (oref this :session) :connection)))
    (rudel-local-delete connection this from to length)))

(defmethod rudel-remote-insert ((this rudel-document) user from what)
  ""
  (with-slots (buffer) this
    ;; Perform insert operation
    (save-excursion
      (with-current-buffer buffer
	(when (< from 0)
	  (setq from (point-max)))
	(let ((inhibit-modification-hooks 't)
	      (to                         (+ from (length what))))
	  (goto-char from)
	  (insert what)))))
  )

(defmethod rudel-remote-delete ((this rudel-document) user from to length)
  ""
  (with-slots (buffer) this
    ;; Perform delete operation
    (save-excursion
      (with-current-buffer buffer
	(let ((inhibit-modification-hooks 't))
	  (delete-region from to)))))
  )


;;; 
;;

(defun rudel-handle-buffer-change (from to length)
  "Handle buffer change at range FROM to TO with length LENGTH by relaying them to the document object of the buffer.
See after-change-functions for more information."
  (when rudel-buffer-document
    (let ((document rudel-buffer-document)
	  (text)) ; TODO with-rudel-buffer-document?
      (if (zerop length)
	  ;; The change was an insert
	  (with-slots (buffer) document
	    (with-current-buffer buffer
	      (setq text (buffer-substring-no-properties from to)))
	    (rudel-local-insert document from to text))
	;; The change was a delete
	(rudel-local-delete document from to length))))
  )


;;; Backend functions
;;
(defun rudel-load-backends ()
  "Resolve and load backends in the `rudel-backends' list."
  (setq rudel-backends
	(mapcar
	 (lambda (name-and-class)
	   (let ((name  (car name-and-class)) ; TODO ugly
		 (class (cdr name-and-class)))
	     (if (eieio-object-p class)
		 name-and-class
	       (progn
		 (load (concat "rudel-" name))
		 (cons name (make-instance class name))))))
	 rudel-backends))
  )

(defun rudel-suitable-backends (predicate)
  "Return a list of backends which satisfy PREDICATE.
Backends are loaded, if necessary."
  (rudel-load-backends)
  (if predicate
      (remove-if-not
       (lambda (cell)
	 (funcall predicate (cdr cell)))
       rudel-backends)
    rudel-backends)
  )

(defun rudel-choose-backend (&optional predicate)
  "When possible, choose a backend satisfying PREDICATE automatically or by asking the user."
  (let ((backends (rudel-suitable-backends predicate)))
    (unless backends
      (error "No backends available"))

    (if (eq (length backends) 1)
	;; If there is only one backend, we can choose that one right
	;; away.
	(prog1
	  (cdar backends)
	  (when (interactive-p)
	    (message "Using backend `%s'" (object-name-string backend))
	    (sit-for 0 500)))

      ;; When we have more than one backend, we have to ask the user,
      ;; which one she wants.
      (require 'rudel-interactive)
      (rudel-read-backend backends nil 'object)))
  )


;;; Interactive functions
;;

;;;###autoload
(defun rudel-join-session ()
  "Join a collaborative editing session.
All data required to join a session will be prompted for interactively."
  (interactive)
  ;; First, we have to ask to user for the backend we should use
  (let ((backend (rudel-choose-backend
		  (lambda (backend)
		    (rudel-capable-of-p backend 'join))))
	(connect-info)
	(connection))
    (setq connect-info (rudel-ask-connect-info backend)) ; TODO use let
    (condition-case error-data
	(setq connection (rudel-connect backend connect-info))
      ('error (error "Could not connect using backend `%s' with %s: %s"
		     (object-name-string backend)
		     connect-info
		     (car error-data))))
    (setq rudel-current-session (rudel-session "bla" ; TODO
			         :connection connection))
    (oset connection :session rudel-current-session)))
    ;(add-to-list 'rudel-sessions session)) ; only one session for now

;;;###autoload
(defun rudel-host-session ()
  "Host a collaborative editing session.
All data required to host a session will be prompted for
interactively."
  (interactive)
  (let ((backend (rudel-choose-backend
		  (lambda (backend) (rudel-capable-of-p backend 'host)))))
    ))

;;;###autoload
(defun rudel-end-session ()
  "End the current collaborative editing session."
  (interactive)
  (unless rudel-current-session
    (error "No active Rudel session"))

  (rudel-end rudel-current-session)
  (setq rudel-current-session nil)
  )

;;;###autoload
(defun rudel-change-color ()
  "Change the color associated with the local user.
Not all backends support this operation."
  (interactive)
  ;; Make sure we have a session.
  (unless rudel-current-session
    (error "No active Rudel session"))

  (with-slots (backend connection self) rudel-current-session
    ;; Make sure the backend can change colors.
    (unless (rudel-capable-of-p backend 'change-color)
      (error "Backend `%s' cannot change colors" 
	     (object-name-string backend)))

    ;; Ask the user for a new color, tell the connection to announce
    ;; the change and change it in our user object.
    (with-slots (color) self
      (setq color (read-color "New Color: " 't))
      (rudel-change-color- connection color)))
  )

;;;###autoload
(defun rudel-subscribe (document)
  "Subscribe to DOCUMENT offered by a peer in a collaborative editing session.
When called interactively, DOCUMENT is prompted for interactively."
  (interactive
   (list (progn
	   ;; We have to retrieve the document list from an active
	   ;; session.
	   (unless rudel-current-session
	     (error "No active Rudel session"))
	   ;; Select unsubscribed documents.
	   (let ((documents (oref rudel-current-session :documents)))
	     ;; Already subscribed to all documents. This is an error.
	     (when (null documents)
	       (error "No unsubscribed documents"))
	     ;; Read an unsubscribed document.
	     (rudel-read-document documents nil 'object)))))

  ;; Make sure we have a session.
  (unless rudel-current-session
    (error "No active Rudel session"))

  ;; Create a new buffer and attach the document to it.
  (let* ((name   (object-name-string document))
	 (buffer (get-buffer-create name)))
    (rudel-attach-to-buffer document buffer)

    (let ((connection (oref (oref document :session) :connection)))
      (rudel-subscribe-to connection document))

    ;; Show the new buffer.
    (show-buffer nil buffer))
  )

;;;###autoload
(defun rudel-publish-buffer (&optional buffer)
  "Make the BUFFER available for subscription to peers in a collaborative editing session.
If BUFFER is nil, the current buffer is used."
  (interactive (list nil))

  ;; Make sure we have a session.
  (unless rudel-current-session
    (error "No active Rudel session"))

  (unless buffer
    (setq buffer (current-buffer)))

  (with-current-buffer buffer
    (when rudel-buffer-document
      (error "Buffer already published or subscribed"))) ; TODO keep this?

  ;;
  (with-slots (backend connection self) rudel-current-session
    (let ((document (rudel-make-document backend
					 (buffer-name buffer)
					 rudel-current-session)))
      (rudel-add-document rudel-current-session document)

      (rudel-attach-to-buffer document buffer)
      (object-add-to-list document :subscribed self)

      (rudel-publish connection document)))
  )

;;;###autoload
(defun rudel-unpublish-buffer (&optional buffer)
  "Deny peers access to BUFFER in a collaborative editing session.
If BUFFER is nil, the current is used."
  (interactive)

  ;; Make sure we have a session.
  (unless rudel-current-session
    (error "No active Rudel session"))

  (unless buffer
    (setq buffer (current-buffer)))

  (with-current-buffer buffer
    (unless rudel-buffer-document
      (error "Buffer is not published")))

  ;;
  (with-slots (connection) rudel-current-session
    (let ((document (with-current-buffer buffer
		      rudel-buffer-document)))
      (rudel-detach-from-buffer document)

      (rudel-unsubscribe-from connection document)))
  )

(provide 'rudel)
;;; rudel.el ends here
