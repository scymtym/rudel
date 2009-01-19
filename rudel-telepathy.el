;;; Copyright (C) 2008
;;   by Jan Moringen <scymtym@users.sourceforge.net>
;;
;; This file is part of Rudel.
;;
;; Rudel is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.
;;
;; Rudel is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Rudel; see the file COPYING. If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA, or see <http://www.gnu.org/licenses>.

;;; Commentary:
;;
;; 

;;; Code:

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
