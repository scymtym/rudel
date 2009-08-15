;;; rudel-telepathy.el --- A telepathy backend for Rudel
;;
;; Copyright (C) 2008, 2009 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: Rudel, telepathy, backend
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
;; along with rudel. If not, see <http://www.gnu.org/licenses>.


;;; Commentary:
;;
;; This file contains a Rudel backend which realizes session
;; initiation and transport of Rudel data through freedesktop's
;; Telepathy framework.


;;; Code
;;

(require 'eieio)

(require 'rudel)


;;;  Class rudel-telepathy-backend
;;

(defclass rudel-telepathy-backend (rudel-backend)
  ((capabilities :initform '(join)))
  "Class rudel-telepathy-backend ")


;;; Autoloading
;;

;;;###autoload
(add-to-list 'rudel-backends
	     (cons "telepathy" 'rudel-telepathy-backend))

(provide 'rudel-telepathy)
;;; rudel-telepathy.el ends here
