;;; rudel-infinote-group.el --- Common aspects of infinote communication groups
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
;; This file contains the classes `rudel-infinote-group' which is a
;; base class for other infinote group classes and
;; `rudel-infinote-group-state' which is a base class for infinote
;; group state classes.


;;; History:
;;
;; 0.1 - Initial version


;;; Code:
;;

(require 'warnings)

(require 'eieio)
(require 'eieio-base) ;; for `eieio-named'

(require 'rudel-util) ;; for `rudel-impersonator', `rudel-delegator'
(require 'rudel-state-machine)
(require 'rudel-infinote-state)


;;; Class rudel-infinote-group-state
;;

(defclass rudel-infinote-group-state (rudel-infinote-state
				      rudel-impersonator
				      rudel-delegator)
  ((impersonation-target-slot :initform 'group)
   (delegation-target-slot    :initform 'group)
   (group                     :initarg :group
			      :type    rudel-infinote-group-child
			      :documentation
			      ""))
  ""
  :abstract t)

(defmethod rudel-accept ((this rudel-infinote-group-state) xml) ;; TODO is xml a single element or a list of elements?
  ""
  (let ((type (xml-node-name xml)))
    (case type
      ;; Handle request-failed messages, which look like this:
      ;; <request-failed
      ;;     domain="error_domain"
      ;;     code="error_code"
      ;;     seq="seq_id">
      ;;   <text>Human-readable text</text>
      ;; </request-failed>
      ;; domain example: INF_DIRECTORY_ERROR
      (request-failed
       ;; TODO handle the problem
       (with-tag-attrs (domain
			(code            code number)
			(sequence-number seq  number)) xml
			)
       'idle)

      ;; Dispatch all normal message to appropriate methods
      ;; automatically.
      (t
       (let ((name (symbol-name type)))
	 (condition-case error
	     ;; Try to dispatch
	     (rudel-dispatch this
			     "rudel-infinote/" name
			     (list xml))
	   ;; Warn if we failed to locate or execute the method. Return
	   ;; nil in this case, so we remain in the current state.
	   (rudel-dispatch-error
	    (progn
	      (display-warning
	       '(rudel infinote)
	       (format "%s: no method (%s: %s): `%s/%s'; arguments: %s"
		       (object-print this) (car error) (cdr error)
		       "rudel-infinote" name arguments)
	       :debug)
	      nil)))))))
  )

;; TODO can all groups receive <session-close/> or just document groups?

(defmethod rudel-send ((this rudel-infinote-group-state) data)
  ""
  (with-slots (group) this
    ;;
    ;(with-slots (sequence-number) connection ;; TODO encapsualtion violation
    (rudel-send group data))
  )


;;; Class rudel-infinote-group
;;

(defclass rudel-infinote-group (eieio-named
				rudel-state-machine)
  ((connection :initarg :connection
	       ;:type    rudel-infinote-connection ;; TODO
	       :documentation
	       "")
   (publisher  :initarg  :publisher
	       :type     string
	       :documentation
	       "")
   (method     :initarg  :method
	       :type     symbol
	       :documentation
	       "")
   (members    :initarg  :members ;; TODO currently unused
	       :type     list
	       :initform nil
	       :documentation
	       ""))
  "")

;; TODO we could introduce rudel-message to pass data to rudel accept

(defmethod rudel-register-state ((this rudel-infinote-group) symbol state)
  "TODO"
  ;; Associate THIS connection to STATE.
  (oset state :group this)

  ;;
  (when (next-method-p)
    (call-next-method)))

(defmethod rudel-send ((this rudel-infinote-group) data)
  ""
  (with-slots (connection) this
    (rudel-send connection
		(rudel-infinote-embed-in-group this data))))


;;; Miscellaneous functions
;;

;; TODO move to util file
(defmacro rudel-infinote-embed-in-group (group &rest forms);; TODO bad name
  ""
  (declare (indent 1)
	   (debug (form &rest form)))
  (let ((group-var (make-symbol "group"))
	(name      (make-symbol "name"))
	(publisher (make-symbol "publisher")))
    `(let* ((,group-var ,group)
	    (,name      (object-name-string ,group-var))
	    (,publisher (oref ,group-var :publisher)))
       `(group
	 ((name      . ,,name)
	  (publisher . ,,publisher))
	 ,,@forms)))
  )

(provide 'rudel-infinote-group)
;;; rudel-infinote-group.el ends here
