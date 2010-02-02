;;; rudel-infinote-directory-document.el ---
;;
;; Copyright (C) 2009, 2010 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, infinote, document, directory
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


;;; History:
;;
;; 0.1 - Initial version


;;; Code:
;;

(require 'rudel-infinote-document)


;;; Class rudel-infinote-directory-document
;;

(defclass rudel-infinote-directory-document (rudel-infinote-document)
  ((child-cache :initarg :child-cache
		:type    list
		:documentation
		""))
  "")

(defmethod rudel-add-child ((this rudel-infinote-directory-document)
			    document)
  ""
  (with-slots (child-cache) this
    (push document child-cache))) ;; TODO object-add-to-list or add-to-list?

(defmethod slot-missing ((this rudel-infinote-directory-document)
			 slot-name operation &optional new-value) ;; TODO why not use slot-unbound?
  "Simulate slot :children. The value of the slot is fetched as
necessary."
  (cond
   ;; Slot :children
   ((and (or (eq slot-name :children)
	     (eq slot-name 'children))
	 (eq operation 'oref))
    ;; Retrieve children when the slot is accessed for the first time.
    (unless (slot-boundp this :child-cache)
      ;; Bind slot
      (oset this :child-cache nil)

      ;; Make group fetch children
      (with-slots (id group) this
	;;
	;; progress report string would be "Queued"
	(rudel-state-wait group '(idle) nil) ;; TODO can this really happen?

	;;
	(rudel-switch group 'exploring id)

	;; Busy wait for children to be retrieved
	;; progress report string would be "Exploring documents"
	(rudel-state-wait group '(idle) nil)))

    ;; Return children
    (oref this :child-cache))

   ;; Call next method
   (t
    (call-next-method)))
  )

(provide 'rudel-infinote-directory-document)
;;; rudel-infinote-directory-document.el ends here
