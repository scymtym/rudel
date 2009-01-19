;;; jupiter-compound.el --- Jupiter compound operation
;;
;; Copyright (C) 2009 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;; Keywords: Jupiter, operation, compound
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
;; Class jupiter-compound implements a compound operation comprised of
;; a number of child operations.


;;; History:
;;
;; 0.1 - Initial revision.


;;; Code:
;;

(require 'eieio)

(require 'jupiter-operation)


;;; Class jupiter-compound
;;

(defclass jupiter-compound ()
  ((children :initarg  :children
	     :type     list
	     :initform nil
	     :documentation
	     ""))
  "Objects of this class are operations, which are composed of a
number of child operation.")

;; TODO this has side effects. It can only be called once
(defmethod jupiter-apply ((this jupiter-compound) buffer)
  "Apply THIS to BUFFER by applying the child operation."
  (with-slots (children) this
    (let ((child (first children))
	  (rest  (rest  children)))
      ;; Apply all child operations
      (while child
	(rudel-apply child buffer)
	;; For each applied child operation, transform remaining
	;; operation with the applied operation.
	(dolist (next rest)
	  (setf next (jupiter-transform child next)))
	;; Advance to next child operation.
	(setq child (first rest)
	      rest  (rest rest)))))
  )

(defmethod jupiter-transform ((this jupiter-compound) other)
  "Transform OTHER using the child operations of THIS."
  (with-slots (children) this
    (dolist (child children) ;; TODO reverse children?
      (setq other (jupiter-transform child other)))
    other))

(provide 'jupiter-compound)
;;; jupiter-compound.el ends here
