;;; rudel-infinote-document.el --- Infinote document class
;;
;; Copyright (C) 2009, 2010 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, infinote, document
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
;; This file contains the `rudel-infinote-document' class which is the
;; base class for different document classes used in the infinote
;; backend.


;;; History:
;;
;; 0.1 - Initial version


;;; Code:
;;

(require 'eieio)


;;; Class rudel-infinote-document
;;

(defclass rudel-infinote-document (rudel-document)
  ((id       :initarg  :id
	     :type     (integer 0)
	     :accessor rudel-id
	     :documentation
	     "")
   (parent   :initarg  :parent
	     :type     (or null rudel-infinote-document)
	     :documentation
	     "")
   (group    :initarg  :group
	     ;:type    (or null rudel-infinote-group)
	     :documentation
	     ""))
  "")

(defmethod rudel-unique-name ((this rudel-infinote-document))
  "TODO"
  (with-slots (parent) this
    (concat
     (when parent
       (rudel-unique-name parent))
     "/"
     (object-name-string this)))
  )

(provide 'rudel-infinote-document)
;;; rudel-infinote-document.el ends here
