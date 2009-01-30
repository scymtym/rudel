;;; jupiter-delete.el --- Jupiter delete operation
;;
;; Copyright (C) 2009 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: Jupiter, operation, delete
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
;; Class jupiter-delete implements a delete operation for the Jupiter
;; algorithm.


;;; History:
;;
;; 0.1 - Initial revision.


;;; Code:
;;

(require 'eieio)

(require 'jupiter-operation)


;;; Class jupiter-delete
;;

(defclass jupiter-delete (jupiter-operation)
  ((from :initarg :from
	 :type    (integer 0)
	 :documentation
	 "Start of the region affected by this operation.")
   (to   :initarg :to
	 :type    (integer 0)
	 :documentation
	 "End of the region affected by this operation."))
  "Objects of this class represent deletions in buffers.")

(defmethod jupiter-apply ((this jupiter-delete) buffer)
  "Apply THIS to BUFFER by deleting the associated region."
  (with-slots (from to) this
    (save-excursion
      (with-current-buffer buffer
	(delete-region from to))))
  )

(defmethod jupiter-transform ((this jupiter-delete) other)
  "Transform other using THIS.
OTHER is destructively modified or replaced."
  (cond

   ;;
   ;; Transform an insert operation
   ;;
   ((jupiter-insert-p other)
    (with-slots ((this-from :from) (this-to :to) (this-length :length)) this
      (with-slots ((other-from :from) (other-to :to) (other-length :length)) other
	(cond
	 ;; 
	 ;; <other>
	 ;;         <this>
	 ;;
	 ((<= other-to this-from))

	 ;;        <other>
	 ;; <this>
	 ((> other-from this-to)
	  (decf other-from this-length))

	 ;;   <other>
	 ;; <  this  >
	 ((and (> other-from this-from) (< other-to this-to))
	  (setq other-from this-from))
	 )))
    )

   ;;
   ;; Transform a delete operation
   ;;
   ((jupiter-delete-p other)
    (with-slots ((this-from :from) (this-to :to) (this-length :length)) this
      (with-slots ((other-from :from) (other-to :to) (other-length :length)) other
	(cond

	 ;;        <other>
	 ;; <this>
	 ;; OTHER deleted a region after the region deleted by
	 ;; THIS. Therefore OTHER has to be shifted by the length of
	 ;; the deleted region.
	 ((> other-from this-to)
	  (decf other-from this-length)
	  (decf other-to   this-length))
	 
	 ;; <other>
	 ;;         <this>
	 ;; OTHER deleted a region before the region affected by
	 ;; THIS. That is not affected by THIS operation.
	 
	 ;; <  other  >
	 ;;   <this>
	 ((and (>= other-from this-from) (>= other-to this-to))
	  (decf other-to this-length))

	 ;; <other>
	 ;;    <this>
	 ((and (< other-from this-from) (< other-to this-to))
	  (decf other-to (- other-to this-to)))

	 ;;    <other>
	 ;; <this>
	 ;; The region deleted by OTHER overlaps with the region
	 ;; deleted by THIS, such that a part of the region of this is
	 ;; before the region of OTHER. The first part of the region
	 ;; deleted by OTHER has already been deleted. Therefore, the
	 ;; start of OTHER has to be shifted by the length of the
	 ;; overlap.
	 ((and (< other-from this-to) (> other-to this-to))
	  (setq other-from this-from)
	  (incf other-to   (+ other-from (- other-to this-to))))

	 ;;   <other>
	 ;; <  this   >
	 ;; The region deleted by OTHER is completely contained in
	 ;; the region affected by THIS. Therefore, OTHER must not
	 ;; be executed.
	 ((and (>= other-from this-from) (<= other-to this-to))
	  (setq other (jupiter-nop "nop")))
	 
	 (t (error "logic error in jupiter-delete::transform(jupiter-delete)"))
	 ))))

   ;;
   ;; Transform a compound operation
   ;;
   ((jupiter-compound-p other) ;; TODO encapsulation violation
    (with-slots (children) other
      (dolist (child children)
	(setf child (jupiter-transform this child)))))

   ;;
   (t (error "Cannot transform operation of type `%s'"
	     (object-class other))))
  other)

(defmethod slot-missing ((this jupiter-delete)
			 slot-name operation &optional new-value)
  "Simulate slot :length"
  (cond
   ;; Slot :length
   ((and (or (eq slot-name :length)
	     (eq slot-name 'length))
	 (eq operation 'oref))
    (with-slots (from to) this
      (- to from)))
   ;; Call next method
   (t (call-next-method)))
  )

(provide 'jupiter-delete)
;;; jupiter-delete.el ends here
