;;; rudel.el --- A collaborative editing framework for Emacs
;;
;; Copyright (C) 2008 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING. If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA, or see <http://www.gnu.org/licenses>.

;;; Commentary:
;;
;; Rudel is a framework for collaborative editing in Emacs.  Its
;; architecture allows communication with arbitrary collaborative
;; editors.

;;; History:
;; 

;;; Code:

(require 'eieio)

(eval-when-compile
  (require 'cl))


;;; Global variables
;;

;;;###autoload
(defvar rudel-backends nil
  "List of rudel backends.")

;(defvar rudel-sessions nil
;  "") ; TODO do we allow multiple sessions?

(defvar rudel-current-session nil
  "Global object representing the current Rudel session.
nil if there is no active session.") ; only one session for now

(defvar rudel-buffer-document nil
  "Buffer-local variable which holds the rudel document associated with the buffer.")
(make-variable-buffer-local 'rudel-buffer-document)


;;; Class rudel-session
;;

(defclass rudel-session ()
  ((connection :initarg :connection
	       ;:type    rudel-connection-child
	       ;:type    (satifies rudel-connection-child-p)
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
  ""
  (with-slots (connection users documents) this
;;;     (dolist (document documents)
;;;       (rudel-detach-from-buffer document))
    (mapc 'rudel-detach-from-buffer documents)
;;;     (dolist (user users)
;;;       ?)
    (rudel-disconnect connection)))

(defmethod rudel-add-user ((this rudel-session) user)
  ""
  (with-slots (users) this
    (setq users (cons user users))))
    ;(add-to-list 'users user))) ; TODO

(defmethod rudel-remove-user ((this rudel-session) user)
  ""
  (with-slots (users) this
    (setq users (remove user users)))) ; TODO

(defmethod rudel-find-user ((this rudel-session)
			    which &optional test key)
  ""
  (unless test
    (setq test 'string=))
  (unless key
    (setq key 'object-name-string))
  (with-slots (users) this
    (find which users :key key :test test)))

(defmethod rudel-add-document ((this rudel-session) document)
  ""
  (unless (slot-boundp document :session)
    (oset document :session this)) ; TODO do we want to do this here?
  (with-slots (documents) this
    (setq documents (cons document documents))))
    ;(add-to-list 'documents document))) ; TODO

(defmethod rudel-find-document ((this rudel-session)
				which &optional test key)
  ""
  (unless test
    (setq test 'string=))
  (unless key
    (setq key 'object-name-string))
  (with-slots (documents) this
    (find which documents :key key :test test)))


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
  ""
  (error "Needs to be implemented in derived classes"))

(defmethod rudel-ask-listen-info ((this rudel-backend))
  ""
  (error "Needs to be implemented in derived classes"))

(defmethod rudel-listen ((this rudel-backend) info)
  ""
  (error "Needs to be implemented in derived classes"))


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

(defmethod rudel-subscribe-to ((this rudel-connection) document) ; TODO name should be rudel-subscribe
  ""
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
	  :documentation ;type color
	  ""))
  "Class rudel-user ")
; abstract


;;; Class rudel-document
;;

(defclass rudel-document (eieio-speedbar-file-button)
  ((buffer  :initarg :buffer ;type buffer
	    :documentation
	    "")
   (session :initarg :session
	    :type    rudel-session
	    :documentation
	    ""))
  "Class rudel-document ")
;abstract

(defmethod rudel-attach-to-buffer ((this rudel-document) the-buffer)
  ""
  (with-slots (buffer) this
    (setq buffer the-buffer)
    (with-current-buffer buffer
      (setq rudel-buffer-document this)
      (add-hook 'after-change-functions
		'rudel-handle-buffer-change))))

(defmethod rudel-detach-from-buffer ((this rudel-document))
  ""
  (with-slots (buffer) this
    (with-current-buffer buffer
      (setq rudel-buffer-document nil)
      (remove-hook 'after-change-functions
		   'rudel-handle-buffer-change)
      (setq buffer nil)))) ; TODO we could also use unbound

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
  (message "%s::remote-insert %s %s %s" (object-print this) (object-print user) from what)
  (with-slots (buffer) this
    (save-excursion
      (with-current-buffer buffer
	(when (< from 0)
	  (setq from (point-max)))
	(let ((inhibit-modification-hooks 't)
	      (to                         (+ from (length what))))
	  (goto-char from)
	  (insert what)
	  (put-text-property from to 'face (list :background (oref user :color))))))) ; TODO overlays, tooltips?
  )

(defmethod rudel-remote-delete ((this rudel-document) user from to length)
  ""
  (message "%s::remote-delete %s %d %d" (object-print this) (object-print user) from to)
  (with-slots (buffer) this
    (save-excursion
      (with-current-buffer buffer
	(let ((inhibit-modification-hooks 't))
	  (delete-region from (+ to length))))))
  )


;;; 
;;

(defun rudel-handle-buffer-change (from to length)
  "Handle buffer change at range FROM to TO with length LENGTH by relaying them to the document object of the buffer.
See after-change-functions for more information."
  (when rudel-buffer-document
    (let ((document rudel-buffer-document)
	  (text)) ; TODO with-rudel-buffer-document?
      (if (eq length 0)
	  (progn
	    (with-current-buffer (oref document :buffer)
	      (setq text (buffer-substring-no-properties from to)))
	    (rudel-local-insert document from to text))
	(rudel-local-delete document from to length))
  )))

;; (defun rudel-handle-remote-insert (user from what)
;;   ""
;;   (when rudel-buffer-document
;;     (let ((document rudel-buffer-document))
;;       (rudel-remote-insert document user from what))))
;; 
;; (defun rudel-handle-remote-delete (user from to length)
;;   ""
;;   (when rudel-buffer-document
;;     (let ((document rudel-buffer-document))
;;       (rudel-remote-delete document user from to length))))


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
	 rudel-backends)))

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
	(cdar backends)
      (rudel-read-backend backends nil 'object)))
  )
;(message "Using backend `%s'" (object-name-string backend))
;(sleep-for 0 500))


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
  "Host a collaborative editing session."
  (interactive)
  (let ((backend (rudel-choose-backend
		  (lambda (backend) (rudel-capable-of-p backend 'host)))))
    ))

;;;###autoload
(defun rudel-end-session ()
  "End the current collaborative editing session."
  (interactive)
  (rudel-end rudel-current-session)
  (setq rudel-current-session nil)) ; TODO cleanup

;;;###autoload
(defun rudel-change-color ()
  "Change the color associated with the local user.
Not all backends support this operation."
  (interactive)
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
	   (let ((documents (remove-if
			     (lambda (document)
			       (oref document :subscribed))
			     (oref rudel-current-session :documents))))
	     ;; Already subscribed to all documents. This is an error.
	     (when (null documents)
	       (error "No unsubscribed documents"))
	     ;; Read an unsubscribed document.
	     (rudel-read-document documents nil 'object)))))
  (let* ((name   (object-name-string document))
	 (buffer (get-buffer-create name)))
    (rudel-attach-to-buffer document buffer))
  (let ((connection (oref (oref document :session) :connection)))
    (rudel-subscribe-to connection document))) ; TODO we must not subscribe to our own buffers!

;;;###autoload
(defun rudel-publish-buffer (&optional buffer)
  "Make the BUFFER available for subscription to peers in a collaborative editing session.
If BUFFER is nil, the current buffer is used."
  (interactive (list nil))
  (unless buffer
    (setq buffer (current-buffer)))
  (unless rudel-current-session
    (error "No current session"))
  (let ((document (rudel-document (buffer-name buffer)
		   :session rudel-current-session)))
    (with-slots (connection) rudel-current-session
      (rudel-attach-to-buffer document buffer)
      (rudel-add-document rudel-current-session document)))
  )

;;;###autoload
(defun rudel-unpublish-buffer (&optional buffer)
  "Deny peers access to BUFFER in a collaborative editing session.
If BUFFER is nil, the current is used."
  (interactive)
  (unless buffer
    (setq buffer (current-buffer)))
  (with-current-buffer buffer
    (rudel-detach-from-buffer rudel-buffer-document)))

(provide 'rudel)
;;; rudel.el ends here


;;; Tests
;;

(when nil
  ;; Backends
  (progn
    (progn
      (require 'rudel-obby)
      (setq rudel-backends nil)
      (add-to-list 'rudel-backends
		   (cons "obby" (rudel-obby-backend "obby"
				 :capabilities '(join host)))))
    
    (progn
      (data-debug-new-buffer "BACKENDS")
      (data-debug-insert-stuff-list rudel-backends "* ")))
  ;;
  (setq john (rudel-user "john"
			 :name "John"
			 :color (read-color)))
  
  (setq jane (rudel-user "jane"
			 :name "Jane"
			 :color (read-color)))
  
  (with-current-buffer "BLUP"
    (setq rudel-buffer-document (rudel-document (buffer-name)
						:name (buffer-name)
						:buffer (current-buffer))))
  
  (rudel-handle-buffer-change (point) (+ (point) 2) 0)
  
  (let ((string (buffer-substring-no-properties 1 (point-max))))
    (with-current-buffer "BLUP"
      (erase-buffer)
      (insert string)))
  
  (with-current-buffer "BLUP"
    (rudel-handle-remote-insert john (point) "hallo")))

;;  (setq connection (rudel-obby-connection
;;  (rudel-connect )
