;;; rudel-interactive.el --- User interaction functions for Rudel.
;;
;; Copyright (C) 2008, 2009 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: Rudel, user, interface, interaction
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
;; Functions for user interactions commonly used in Rudel components.


;;; History:
;;
;; 0.1 - Initial revision.


;;; Code:
;;

(require 'rudel-compat) ;; for `read-color' replacement


;;; Function for reading Rudel objects from the user.
;;

(defun rudel-read-backend (&optional backends prompt return)
  "Read a backend name from BACKENDS and return that name or the actual backend depending on RETURN.
If RETURN. is 'object, return the backend object; Otherwise
return the name as string."
  (unless backends
    (setq backends rudel-backends))
  (unless prompt
    (setq prompt "Backend: "))
  (let* ((backend-names (mapcar 'car backends))
	 (backend-name  (completing-read prompt backend-names nil t)))
    (cond
     ((eq return 'object)
      (cdr (assoc backend-name backends)))
     (t backend-name)))
  )

(defun rudel-read-user-name ()
  "Read a username.
The default is taken from `rudel-default-username'."
  (read-string "Username: " rudel-default-username))

(defun rudel-read-user-color ()
  "Read a color."
  (read-color "Color: " t))

(defun rudel-read-user (&optional users prompt return)
  "Read a user name from USERS and return that name or the actual user depending on RETURN.
If USERS is nil, use the user list of `rudel-current-session'.
If RETURN. is 'object, return the user object; Otherwise return
the name as string."
  ;; If no user list is provided, the user list of the current session
  ;; is used.
  (unless users
    (if rudel-current-session
	(setq users (oref rudel-current-session :users))
      (error "No user list and no active Rudel session")))
  (unless prompt
    (setq prompt "User: "))
  ;; Construct a list of user name, read a name with completion and
  ;; return a user name of object.
  (let* ((user-names (mapcar 'object-name-string users))
	 (user-name  (completing-read prompt user-names nil t)))
    (cond 
     ((eq return 'object)
      (find user-name users
	    :test 'string= :key 'object-name-string))
     (t user-name)))
  )

(defun rudel-read-document (&optional documents prompt return)
  "Read a document name from DOCUMENTS and return that name or the actual document depending on RETURN.
If RETURN. is 'object, return the backend object; Otherwise
return the name as string."
  (unless documents
    (if rudel-current-session
	(setq documents (oref rudel-current-session :documents))
      (error "No document list and no active Rudel session")))
  (unless documents
    (error "No documents")) ; TODO error is a bit harsh
  (unless prompt
    (setq prompt "Document: "))
  (let* ((document-names (mapcar #'rudel-unique-name documents))
	 (document-name  (completing-read prompt document-names nil t)))
    (cond 
     ((eq return 'object) 
      (find document-name documents 
	    :test #'string= :key #'rudel-unique-name))
     (t document-name)))
  )


;;; Buffer allocation functions
;;

(defun rudel-allocate-buffer-clear-existing (name)
  "When the requested buffer NAME exists, clear its contents and use it."
  (let ((buffer (get-buffer name)))
    (if buffer
	(progn
	  (unless (yes-or-no-p (format 
				"Buffer `%s' already exists; Erase contents? "
				name))
	    (error "Buffer `%s' already exists" name)) ;; TODO throw or signal; not error
	  (with-current-buffer buffer
	    (erase-buffer)))
      (setq buffer (get-buffer-create name)))
    buffer)
  )

(defun rudel-allocate-buffer-make-unique (name)
  "When the requested buffer NAME exists, create another buffer."
  (get-buffer-create (generate-new-buffer-name name)))

(provide 'rudel-interactive)
;;; rudel-interactive.el ends here
