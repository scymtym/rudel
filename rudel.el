;;; rudel.el --- A collaborative editing framework for Emacs
;;
;; Copyright (C) 2008, 2009 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
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
;;

(eval-when-compile
  (require 'cl))

(require 'eieio)

(require 'eieio-speedbar) ;; TODO required for now

(require 'rudel-operations)
(require 'rudel-operators)
(require 'rudel-overlay)


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

(defcustom rudel-allocate-buffer-function 
  'rudel-allocate-buffer-clear-existing
  "*"
  :group   'rudel
  :type    '(choice (const :tag "Clear content of existing buffer"
			   rudel-allocate-buffer-clear-existing )
		    (const :tag "Create a new uniquely named buffer"
			   rudel-allocate-buffer-make-unique )
		    (function :tag "Other function"))
  :require 'rudel-interactive)

(defcustom rudel-default-username (user-login-name)
  "*"
  :group 'rudel
  :type  '(string))


;;; Class rudel-backend
;;

(defclass rudel-backend ()
  ((capabilities :initarg  :capabilities
		 :type     list
		 :initform nil
		 :documentation
		 ""))
  "Class rudel-backend "
  :abstract t)

(defmethod rudel-capable-of-p ((this rudel-backend) capability)
  ""
  (with-slots (capabilities) this
    (member capability capabilities)))

(defgeneric rudel-ask-connect-info ((this rudel-backend))
  "")

(defgeneric rudel-connect ((this rudel-backend) info)
  "Create a new connection according to the data in the property list INFO.
Implementations can rely on the fact that the property :session
contains the rudel-session object to which the new connection
will be associated.")

(defgeneric rudel-ask-host-info ((this rudel-backend))
  "")

(defgeneric rudel-host ((this rudel-backend) info)
  "")

(defgeneric rudel-make-document ((this rudel-backend) name session)
  "")


;;; Class rudel-session
;;

(defclass rudel-session ()
  ((backend   :initarg  :backend
	      :type     rudel-backend-child
	      :documentation
	      "The backend used by this session.")
   (users     :initarg  :users
	      :type     list
	      :initform nil
	      :documentation
	      "The list of users participating in this session.")
   (documents :initarg  :documents
	      :type     list
	      :initform nil
	      :documentation
	      "This list of documents available in this session."))
  "This class serves as a base class for rudel-client-session and
rudel-server-session. Consequently, it consists of slots common
to client and server sessions."
  :abstract t)

(defmethod rudel-end ((this rudel-session))
  "Terminate THIS session performing all necessary cleanup.")

(defmethod rudel-add-user ((this rudel-session) user)
  "Add USER to the user list of THIS session."
  (object-add-to-list this :users user))

(defmethod rudel-remove-user ((this rudel-session) user)
  "Remove USER from the user list of THIS session."
  (object-remove-from-list this :users user))

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
    (oset document :session this))

  (object-add-to-list this :documents document))

(defmethod rudel-remove-document ((this rudel-session) document)
  ""
  (object-remove-from-list this :documents document))

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

(defmethod rudel-unsubscribed-documents ((this rudel-session))
  ""
  (unless (slot-boundp this :self)
    (error "Cannot find unsubscribed documents unless slot self is bound"))
  (with-slots (documents self) this
    (remove-if
     (lambda (document)
       (with-slots (subscribed) document
	 (memq self subscribed)))
       documents))
  )


;;; Class rudel-client-session
;;
(defclass rudel-client-session (rudel-session)
  ((connection :initarg  :connection
	       :type     rudel-connection-child
	       :documentation
	       "")
   (self       :initarg  :self
	       :type     rudel-user-child
	       :documentation
	       "Points into USERS to the user object representing
the local user"))
  "Objects represent a collaborative editing session from a
client perspective.")

(defmethod rudel-end ((this rudel-client-session))
  ;; Clean everything up
  (with-slots (connection users documents) this
    ;; Detach all documents from their buffers
    (mapc 'rudel-detach-from-buffer documents)

    ;; Terminate the connection
    (rudel-disconnect connection))

  ;; 
  (call-next-method)
  )


;;; Class rudel-server-session
;;

(defclass rudel-server-session (rudel-session)
  ()
  "Class rudel-server-session "
  :abstract t)


;;; Class rudel-connection
;;

(defclass rudel-connection ()
  ((session :initarg :session
	    :type    rudel-session-child
	    :documentation
	    ""))
  "This abstract class defines the interface implementations of
client protocols have to obey."
  :abstract t)

(defgeneric rudel-disconnect ((this rudel-connection))
  "Close the connection.")

(defgeneric rudel-change-color- ((this rudel-connection) color) ;; TODO name
  "")

(defgeneric rudel-publish ((this rudel-connection) document)
  "")

(defgeneric rudel-subscribe-to ((this rudel-connection) document)
  "")

(defgeneric rudel-unsubscribe-from ((this rudel-connection) document) ; TODO name should be rudel-unsubscribe
  "")

(defgeneric rudel-local-insert ((this rudel-connection))
  "")
  
(defgeneric rudel-local-delete ((this rudel-connection))
  "")

(defgeneric rudel-remote-insert ((this rudel-connection))
  "")
  
(defgeneric rudel-remote-delete ((this rudel-connection))
  "")


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
  :abstract t)

(defmethod rudel-attach-to-buffer ((this rudel-document) buffer)
  "Attach THIS document to BUFFER"
  (with-slots ((doc-buffer :buffer)) this
    (setq doc-buffer buffer)
    (with-current-buffer doc-buffer
      (setq rudel-buffer-document this)

      ;; Add the handler function for buffer changes to the buffer's
      ;; change hook.
      (add-hook 'after-change-functions
		'rudel-handle-buffer-change
		nil t)))
  )

(defmethod rudel-detach-from-buffer ((this rudel-document))
  "Detach document THIS from its buffer.
Do nothing, if THIS is not attached to any buffer."
  (with-slots (buffer) this
    ;; Only try to detach from BUFFER, if it is non-nil. BUFFER can be
    ;; nil, if the user did not subscribe to the document, or
    ;; unsubscribed after subscribing.
    (when buffer
      (with-current-buffer buffer
	(setq rudel-buffer-document nil)
	
	(remove-hook 'after-change-functions
		     'rudel-handle-buffer-change
		     t))
      
      (setq buffer nil)))
  )

(defmethod rudel-insert ((this rudel-document) position data)
  "Insert DATA at POSITION into the buffer attached to THIS.
When POSITION is nil `point-max' is used to determine the
insertion position.
Modification hooks are disabled during the insertion."
  (with-slots (buffer) this
    (save-excursion
      (set-buffer buffer)

      (unless position
	(setq position (- (point-max) 1)))

      (let ((inhibit-modification-hooks t))
	(goto-char (+ position 1))
	(insert data))))
  )

(defmethod rudel-delete ((this rudel-document) position length)
  "Delete a region of LENGTH character at POSITION from the buffer attached to THIS.
Modification hooks are disabled during the insertion."
  (with-slots (buffer) this
    (save-excursion
      (set-buffer buffer)

      (let ((inhibit-modification-hooks t))
	(delete-region (+ position 1) (+ position length 1)))))
  )

(defmethod rudel-local-operation ((this rudel-document) operation)
  "Apply the local operation OPERATION to THIS."
  (with-slots (session buffer) this
    (with-slots (connection (user :self)) session
      (dolist (operations (list

			   ;; Update overlays
			   (rudel-overlay-operators
			    "overlay-operators"
			    :document this
			    :user     user)

			   ;; Notify connection
			   (rudel-connection-operators
			    "connection-operators"
			    :connection connection
			    :document   this)))

	;; Apply the operation using each set of operators
	(rudel-apply operation operations))))
  )

(defmethod rudel-remote-operation ((this rudel-document) user operation)
  "Apply the remote operation OPERATION performed by USER to THIS."
  (dolist (operations (append

		       ;; Update buffer contents
		       (list (rudel-document-operators
			      "document-operators"
			      :document this))

		       ;; Update overlays
		       (when user
			 (list (rudel-overlay-operators
				"overlay-operators"
				:document this
				:user     user)))))

    ;; Apply the operation using each set of operators
    (rudel-apply operation operations))
  )

(defmethod rudel-chunks ((this rudel-document))
  "Return a list of text chunks of the associated buffer.
Each element in the chunk is a list structured like this (START
END AUTHOR). START and END are numbers, AUTHOR is of type (or
null rudel-user-child)."
  (with-slots (buffer) this
    ;; Extract buffer string and a list of chunks partitioning the
    ;; string according to the respective author (or nil).
    (with-current-buffer buffer
      (let ((string         (buffer-string)) ;; TODO no-properties?
	    (overlay-chunks (mapcar 
			     (lambda (overlay)
			       (list (- (overlay-start overlay) 1)
				     (- (overlay-end   overlay) 1)
				     (rudel-overlay-user overlay)))
			     (sort* (rudel-author-overlays) 
				    '< :key 'overlay-start)))
	    (last)
	    (augmented-chunks))

	;; Iterate through the list of chunks to find gaps between
	;; chunks (also before the first) and insert entries with
	;; author nil accordingly.
	(dolist (chunk overlay-chunks)
	  (when (or (and (not last)
			 (> (nth 0 chunk) 0))
		    (and last
			 (/= (nth 1 last)
			     (nth 0 chunk))))
	    (push (list (if last (nth 1 last) 0)
			(nth 0 chunk)
			nil)
		  augmented-chunks))
	  (push chunk augmented-chunks)
	  (setq last chunk))

	;; If there is text after the last chunk, create another one
	;; with author nil. If there were no chunks at all, this chunk
	;; can also cover the whole buffer string.
	(when (or (and (not last)
		       (/= (length string) 0))
		  (and last
		       (/= (nth 1 last) (length string))))
	  (push (list (if last (nth 1 last) 0)
		      (length string)
		      nil)
		augmented-chunks))

	;; Sort chunks according to the start position.
	(sort* augmented-chunks '< :key 'car))))
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
	    (rudel-local-operation document 
				   (rudel-insert-op
				    "insert"
				    :from (- from 1)
				    :data text)))
	;; The change was a delete
	(rudel-local-operation document
			       (rudel-delete-op 
				"delete"
				:from   (- from 1)
				:length length)))))
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

    (if (= (length backends) 1)
	;; If there is only one backend, we can choose that one right
	;; away.
	(prog1
	  (cdar backends)
	  (when (interactive-p)
	    (message "Using backend `%s'" (object-name-string 
					   (cdar backends)))
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
    (setq connect-info (rudel-ask-connect-info backend))
    ;; Try to connect
    (condition-case error-data
	(setq connection (rudel-connect backend connect-info))
      ('error (error "Could not connect using backend `%s' with %s: %s"
		     (object-name-string backend)
		     connect-info
		     (car error-data))))
    ;; Store the new session object globally.
    (setq rudel-current-session (rudel-client-session
				 (format "%s session" 
					 (object-name-string backend))
				 :backend    backend
			         :connection connection))
    (oset connection :session rudel-current-session))
  )

;;;###autoload
(defun rudel-host-session ()
  "Host a collaborative editing session.
All data required to host a session will be prompted for
interactively."
  (interactive)
  ;; If necessary, ask the user for the backend we should use.
  (let* ((backend (rudel-choose-backend
		   (lambda (backend) (rudel-capable-of-p backend 'host))))
	 (info    (rudel-ask-host-info backend))
	 (server))

    ;; Try to create the server
    (condition-case error-data
	(setq server (rudel-host backend info))
      ('error 
       (error "Could not host session using backend `%s' with %s: %s"
       	      (object-name-string backend)
       	      info
       	      (car error-data))))
    server))

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
      (setq color (read-color "New Color: " t))
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
	   (let ((documents (rudel-unsubscribed-documents
			     rudel-current-session)))
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
	 (buffer (funcall rudel-allocate-buffer-function name)))
    (rudel-attach-to-buffer document buffer)

    (let ((connection (oref (oref document :session) :connection)))
      (rudel-subscribe-to connection document))

    ;; Show the new buffer.
    (set-window-buffer nil buffer))
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
