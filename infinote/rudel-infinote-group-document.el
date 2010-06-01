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
  ""
  ;;<user-join
  ;;  id="1"
  ;;  name="jan"
  ;;  status="active"
  ;;  caret="0"
  ;;  selection="0"
  ;;  hue="0.67007399999999995"/>
  (with-tag-attrs ((id        id        number)
		   name
		   status
		   (caret     caret     number)
		   (selection selection number)
		   (hue       hue       number)) xml
    ;; (with-slots (session) (oref this :connection) ;; TODO
    ;;   (rudel-add-user session (rudel-user name
    ;;					  :color "red")))

    ;; TODO add to documents subscribed users
    )
  nil)

(defmethod rudel-infinote/user-rejoin
  ((this rudel-infinote-group-document-state-idle) xml)
  ""
  ;; <user-rejoin
  ;;   id="1"
  ;;   name="jan"
  ;;   status="active"
  ;;   caret="0"
  ;;   selection="0"
  ;;   hue="0.67007399999999995"/>
  ;; TODO
  nil)

(defmethod rudel-infinote/user-status-change
  ((this rudel-infinote-group-document-state-idle) xml)
  ""
  ;;<user-status-change
  ;;  id="1"
  ;;  status="unavailable"/>
  (with-tag-attrs ((id id number)
		   status) xml
    )
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

(defmethod rudel-infinote/sync-user ;; TODO send sync-error if remaining-items is already zero
  ((this rudel-infinote-group-document-state-synchronizing) xml)
  ""
  (with-slots (remaining-items) this
    (with-tag-attrs (id name status caret selection hue) xml
      ;; TODO
      )

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
	(rudel-send this '(sync-ack))
      (rudel-send
       this
       `(sync-error
	 ((domain . "INF_SESSION_SYNCHRONIZATION_ERROR")
	  (code   . "0"))
	 ,(format
	   "Received less synchronization items (%d) than previously announced (%d)"
	   (- all-items remaining-items)
	   all-items)))))
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
  "")

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
  (with-tag-attrs ((id        id        number)
		   name
		   status
		   (caret     caret     number)
		   (selection selection number)
		   (hue       hue       number)) xml
    ) ;; TODO
  'idle)

(defmethod rudel-infinote/user-rejoin
  ((this rudel-infinote-group-document-state-joining) xml)
  ""
  (with-tag-attrs ((id        id        number)
		   name
		   status
		   (caret     caret     number)
		   (selection selection number)
		   (hue       hue       number)) xml
    ) ;; TODO
  'idle)


;;;
;;

(defvar rudel-infinote-group-document-states
  '((idle          . rudel-infinote-group-document-state-idle)
    (synchronizing . rudel-infinote-group-document-state-synchronizing)
    (joining       . rudel-infinote-group-document-state-joining))
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
