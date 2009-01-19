;;; jupiter-insert.el --- Jupiter insert operation
;;
;; Copyright (C) 2009 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;; Keywords: Jupiter, operation, insert
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
;; Class jupiter-insert implements an insert operation for the Jupiter
;; algorithm.


;;; History:
;;
;; 0.1 - Initial revision.


;;; Code:
;;

(require 'eieio)

(require 'jupiter-operation)


;;; Class jupiter-insert
;;

(defclass jupiter-insert (jupiter-operation)
  ((from :initarg :from
	 :type    (integer 0)
	 :documentation
	 "Start of the region affected by this operation.")
   (data :initarg :data
	 :type    string
	 :documentation
	 "The inserted string."))
  "Objects of this class represent insertions into buffers.")

(defmethod jupiter-apply ((this jupiter-insert) buffer)
  "Apply THIS to BUFFER by inserting the associated data."
  (with-slots (from data) this
    (save-excursion
      (with-current-buffer buffer
	(goto-char from)
	(insert data))))
  )

(defmethod jupiter-transform ((this jupiter-insert) other)
  "Transform OTHER using THIS."
  (cond

   ;;
   ;; Transform an insert operation
   ;;
   ((jupiter-insert-p other)
    (with-slots ((this-from :from) (this-to :to) (this-length :length) (this-data :data)) this
      (with-slots ((other-from :from) (other-to :to) (other-length :length) (other-data :data)) other
	(cond
	 ;;
	 ;; <other>
	 ;;         <this>
	 ;; No need to do anything in this case.
	 ;; ((< other-from this-from))

	 ;;
	 ;;        <other>
	 ;; <this>
	 ;;
	 ((> other-from this-from)
	  (incf other-from this-length))

	 ;;
	 ;; <other>
	 ;; <this>
	 ;; OTHER inserted at the same start position as we did. Since
	 ;; the situation is symmetric in the location properties of
	 ;; OTHER and THIS, we use the inserted data to construct an
	 ;; ordering.
	 ((= other-from this-from)
	  (when (string< this-data other-data)
	    (incf other-from this-length)))))))

   ;;
   ;; Transform a delete operation
   ;;
   ((jupiter-delete-p other)
    (with-slots ((this-from :from) (this-to :to) (this-length :length)) this
      (with-slots ((other-from :from) (other-to :to) (other-length :length)) other
	(cond
	   
	 ;;
	 ;; <other>
	 ;;         <this>
	 ;; no op

	 ;;
	 ;; <other> and   <other> and        <other>
	 ;; <this>      <this>        <this>
	 ((>= other-from this-from)
	  (incf other-to this-length)
	  (incf other-to this-length))

	 ;;
	 ;; <  other  >
	 ;;   <this>
	 ;; OTHER deleted a region that includes the point at which THIS
	 ;; inserted in its interior. OTHER has to be split into one
	 ;; deletion before and one delete after the inserted data.
	 ((and (< other-from this-from) (> other-to this-to))
	  (setq other
		(jupiter-compound "compound"
	         :children (list (jupiter-delete "delete-left"
				  :from other-from
				  :to   this-to)
				 (jupiter-delete "delete-right"
				  :from this-to
				  :to   (+ other-to this-length))))))
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

(defmethod slot-missing ((this jupiter-insert)
			 slot-name operation &optional new-value)
  "Simulate slots :length and :to"
  (cond
   ;; Slot :length
   ((and (or (eq slot-name :length)
	     (eq slot-name 'length))
	 (eq operation 'oref))
    (with-slots (data) this
      (length data)))
   ;; Slot :to
   ((and (or (eq slot-name :to)
	     (eq slot-name 'to))
	 (eq operation 'oref))
    (with-slots (from length) this
      (+ from length)))
   ;; Call next method
   (t (call-next-method)))
  )

(provide 'jupiter-insert)
;;; jupiter-insert.el ends here
