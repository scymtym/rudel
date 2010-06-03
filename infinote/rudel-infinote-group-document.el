;;; rudel-infinote-group-document.el --- Infinote document group
;;
;; Copyright (C) 2009, 2010 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, infinote, group, communication
;; X-RCS: $Id:$
;;
;; This file is part of Rudel.
;;
;; Rudel is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Rudel is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Rudel. If not, see <http://www.gnu.org/licenses>.


;;; Commentary:
;;
;; This file contains the implementation of the infinote communication
;; group used by document sessions.


;;; History:
;;
;; 0.1 - Initial version


;;; Code:
;;

(require 'warnings)

(require 'eieio)

(require 'rudel-xml)

(require 'rudel-infinote-group)


;;; Class rudel-infinote-group-document-state-idle
;;

(defclass rudel-infinote-group-document-state-idle
  (rudel-infinote-group-state)
  ()
  "")

(defmethod rudel-infinote/sync-begin
  ((this rudel-infinote-group-document-state-idle) xml)
  ""
  (with-tag-attrs ((num-messages num-messages number)) xml
    (list 'synchronizing num-messages)))

(defmethod rudel-infinote/user-join
  ((this rudel-infinote-group-document-state-idle) xml)
  "Handle 'user-join' message."
  (with-slots (document) this
    (with-tag-attrs ((id        id        number)
		     name
		     status
		     (caret     caret     number)
		     (selection selection number)
		     (hue       hue       number)) xml
      (let ((user (rudel-infinote-user
		   name
		   :color  (format "#%04x%04x%04x"
				   (* 65535 hue)
				   (* 65535 0.8)
				   (* 65535 0.8)) ;; TODO temp
		   :id     id
		   :status (intern-soft status))))
	;; Add user to session if necessary.
	;; (with-slots (session) (oref this :connection) ;; TODO
	;;   (rudel-add-user session user))

	;; Add USER to list of users subscribed to DOCUMENT.
	(rudel-add-user document user))))
  nil)

(defmethod rudel-infinote/user-rejoin
  ((this rudel-infinote-group-document-state-idle) xml)
  ""
  (with-slots (document) this
    (with-tag-attrs ((id        id        number)
		     name
		     status
		     (caret     caret     number)
		     (selection selection number)
		     (hue       hue       number)) xml
      (let ((user (rudel-find-user document id #'= #'rudel-id)))
	(if (not user)
	    ;; We did not find the user, display a warning and give
	    ;; up.
	    (display-warning
	     '(rudel infinote)
	     (format "Could not find user: %d" id)
	     :warning)

	  ;; If we found the user, change its status
	  (with-slots ((name    :object-name)
		       (color1  :color)
		       (id1     :id)
		       (status1 :status)) user
	    (setq name1   name
		  color1  (format "#%04x%04x%04x"
				  (* 65535 hue)
				  (* 65535 0.8)
				  (* 65535 0.8)) ;; TODO temp
		  id1     id
		  status1 (intern-soft status)))
	  (rudel-change-notify user)))))
  nil)

(defmethod rudel-infinote/user-status-change
  ((this rudel-infinote-group-document-state-idle) xml)
  ""
  (with-slots (document) this
    (with-tag-attrs ((id  id  number)
		     status)          xml
      (let ((user (rudel-find-user document id #'= #'rudel-id)))
	(if (not user)
	    ;; We did not find the user, display a warning and give
	    ;; up.
	    (display-warning
	     '(rudel infinote)
	     (format "Could not find user: %d" id)
	     :warning)

	  ;; If we found the user, change its status
	  (oset user :status (intern-soft status)) ;; TODO add type symbol to with-tag-attr?
	  (rudel-change-notify user)))))
  nil)

(defmethod rudel-infinote/user-color-change
  ((this rudel-infinote-group-document-state-idle) xml)
  ""
  (with-slots (document) this
    (with-tag-attrs ((id  id  number)
		     (hue hue number)) xml
      (let ((user (rudel-find-user document id #'= #'rudel-id)))
	(if (not user)
	    ;; We did not find the user, display a warning and give
	    ;; up.
	    (display-warning
	     '(rudel infinote)
	     (format "Could not find user: %d" id)
	     :warning)

	  ;; If we found the user, change its status
	  (oset user :color (format "#%04x%04x%04x"
				    (* 65535 hue)
				    (* 65535 0.8)
				    (* 65535 0.8))) ;; TODO temp
	  (rudel-change-notify user)))))
  nil)

;; TODO does this belong here or in derived classes?
(defmethod rudel-infinote/request
  ((this rudel-infinote-group-document-state-idle) xml)
  ""
  (with-tag-attrs (user user number) xml
    (let* ((operation (car (xml-node-children xml))) ;; TODO are multiple operations possible?
	   (type      (xml-node-name operation)))
      (rudel-dispatch this
		      "rudel-infinote/request/" (symbol-name type)
		      (list user operation))))
  nil)

(defmethod rudel-infinote/session-close
  ((this rudel-infinote-group-document-state-idle) xml)
  ""
  'closed)

;; we can receive
;; <request-failed domain="error_domain" code="error_code" seq="seq_id">
;;  <text>Human readable</text>
;; </request-failed>
;; or should the base class handle this?

;; TODO we can send
;; <user-join name="name" seq="seq_id" />
;; and
;; <session-unsubscribe />


;;; Class rudel-infinote-state-synchronizing
;;

(defclass rudel-infinote-group-document-state-synchronizing
  (rudel-infinote-group-state)
  ((all-items       :initarg :all-items
		    :type    (integer 0)
		    :documentation
		    "")
   (remaining-items :initarg :num-items
		    :type    (integer 0)
		    :documentation
		    ""))
  "")

(defmethod rudel-enter ((this rudel-infinote-group-document-state-synchronizing)
			num-items)
  ""
  (with-slots (all-items remaining-items) this
    (setq all-items       num-items
	  remaining-items num-items))
  nil)

(defmethod rudel-infinote/sync-user
  ((this rudel-infinote-group-document-state-synchronizing) xml)
  "Create a user object and add it to the document."
  ;; TODO send sync-error if remaining-items is already zero
  (with-slots (document remaining-items) this
    (with-tag-attrs ((id        id        number)
		     name
		     status
		     (caret     caret     number)
		     (selection selection number)
		     (hue       hue       number)) xml
      (let ((user (rudel-infinote-user
		   name
		   :color  (format "#%04x%04x%04x"
				   (* 65535 hue)
				   (* 65535 0.8)
				   (* 65535 0.8)) ;; TODO temp
		   :id     id
		   :status (intern-soft status))))

	;; TODO try to find the user in the session first?
	;; TODO add user to session?

	;; Add user to the list of subscribed users of the document.
	(rudel-add-user document user)))

    ;; Expect one less synchronization item.
    (decf remaining-items))
  nil)

(defmethod rudel-infinote/sync-request
  ((this rudel-infinote-group-document-state-synchronizing) xml)
  ""
  (with-slots (remaining-items) this
    (with-tag-attrs (user time) xml
      ) ;; TODO

    ;; Expect one less synchronization item.
    (decf remaining-items))
  nil)

(defmethod rudel-infinote/sync-segment ;; TODO text documents only?
  ((this rudel-infinote-group-document-state-synchronizing) xml)
  ""
  (with-slots (remaining-items) this
    (with-tag-attrs (author) xml
      ) ;; TODO

    ;; Expect one less synchronization item.
    (decf remaining-items))
  nil)

(defmethod rudel-infinote/sync-end
  ((this rudel-infinote-group-document-state-synchronizing) xml)
  "Handle 'sync-end' message."
  (with-slots (all-items remaining-items) this
    (if (= remaining-items 0)
	;; Everything is fine, we received the expected number of
	;; items.
	(rudel-send this '(sync-ack))
      ;; We did not received the correct number of items. Send an
      ;; error message and display a warning.
      (rudel-send
       this
       `(sync-error
	 ((domain . "INF_SESSION_SYNCHRONIZATION_ERROR")
	  (code   . "0"))
	 ,(format
	   "Received less synchronization items (%d) than previously announced (%d)"
	   (- all-items remaining-items)
	   all-items)))

      (display-warning
       '(rudel infinote)
       (format
	"Received less synchronization items (%d) than previously announced (%d)"
	(- all-items remaining-items)
	:warning))))
  'idle)

(defmethod rudel-infinote/sync-cancel
  ((this rudel-infinote-group-document-state-synchronizing) xml)
  "Handle 'sync-cancel' message."
  'idle)

;; In this state, we can send
;;<sync-error domain="domain" code="code">
;; <text>Human readable</text>
;;</sync-error>



;;; Class rudel-infinote-group-document-state-joining
;;

(defclass rudel-infinote-group-document-state-joining
  (rudel-infinote-group-state)
  ()
  "This state indicates that we are currently joining the session
associated to a document. After sending a 'user-join' message, we
expect a 'user-join' or 'user-rejoin' message in response.")

(defmethod rudel-enter
  ((this rudel-infinote-group-document-state-joining))
  ""
  ;;(with-slots ((name object-name)) user
  (rudel-send this
	      `(user-join
		((name   . "jan")
		 (status . "active")
		 (time   . "")
		 (caret  . ,(format "%d" 0))
		 (hue    . ,(format "%f" 0.67007399999999995)))))
  nil)

(defmethod rudel-infinote/user-join
  ((this rudel-infinote-group-document-state-joining) xml)
  ""
  (with-slots (document) this
    (with-tag-attrs ((id        id        number)
		     name
		     status
		     (caret     caret     number)
		     (selection selection number)
		     (hue       hue       number)) xml
      ;; In the joining state, the join message has to refer to our
      ;; own user. Therefore, we obtain the self user object from the
      ;; session, update its slots and add it to the document.
      (let ((self (rudel-self (oref document :session))))
	;; When we did not find the self user display a warning.
	(when (not self)
	  (display-warning
	   '(rudel infinote)
	   (format "Could not find self or document user: %d" id)
	   :warning))

	;; If we found the user object, update its slots.
	(when self
	  (with-slots ((name1   :object-name)
		       color
		       (id1     :id)
		       (status1 :status)) self
	    (setq name1   name
		  color   (format "#%04x%04x%04x"
				  (* 65535 hue)
				  (* 65535 0.8)
				  (* 65535 0.8)) ;; TODO temp
		  id1     id
		  status1 (intern-soft status)))
	  (rudel-change-notify self)

	  ;; Add self user to the list of subscribed users of the
	  ;; document.
	  (rudel-add-user document self)))))
  'idle)

(defmethod rudel-infinote/user-rejoin
  ((this rudel-infinote-group-document-state-joining) xml)
  ""
  (with-slots (document) this
    (with-tag-attrs ((id        id        number)
		     name
		     status
		     (caret     caret     number)
		     (selection selection number)
		     (hue       hue       number)) xml
      (let ((self (rudel-self (oref document :session)))
	    (user (rudel-find-user document id #'= #'rudel-id)))
	;; When we did not find the self user or the document user or
	;; they are not the same object, display a warning.
	(when (or (not self)
		  (not user)
		  (not (eq self user)))
	  (display-warning
	   '(rudel infinote)
	   (format "Could not find self or document user: %d" id)
	   :warning))

	;; If we found the user, update its slots.
	(when self
	  (with-slots ((name    :object-name)
		       (color1  :color)
		       (id1     :id)
		       (status1 :status))     self
	    (setq name1   name
		  color1  (format "#%04x%04x%04x"
				  (* 65535 hue)
				  (* 65535 0.8)
				  (* 65535 0.8)) ;; TODO temp
		  id1     id
		  status1 (intern-soft status)))
	  (rudel-change-notify self)))))
  'idle)


;;;
;;

(defvar rudel-infinote-group-document-states
  '((idle          . rudel-infinote-group-document-state-idle)
    (synchronizing . rudel-infinote-group-document-state-synchronizing)
    (joining       . rudel-infinote-group-document-state-joining)
    (closed        . rudel-infinote-group-state-closed))
  "TODO")


;;; Class rudel-infinote-group-document
;;

(defclass rudel-infinote-group-document (rudel-infinote-group)
  ((document :initarg :document
	     :type    rudel-infinote-document-child
	     :documentation
	     ""))
  "")

(defmethod initialize-instance ((this rudel-infinote-group-document)
				slots)
  ""
  ;; Initialize slots of THIS.
  (when (next-method-p)
    (call-next-method))

  ;; Register states.
  (rudel-register-states
   this rudel-infinote-group-document-states)
  )

(provide 'rudel-infinote-group-document)
;;; rudel-infinote-group-document.el ends here
