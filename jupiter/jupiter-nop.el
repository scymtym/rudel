;;; jupiter-nop.el --- Jupiter no operation
;;
;; Copyright (C) 2009 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;; Keywords: Jupiter, operation, nop
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
;; Class jupiter-nop implements a no-operation for the Jupiter
;; algorithm.


;;; History:
;;
;; 0.1 - Initial revision.


;;; Code:
;;

(require 'eieio)

(require 'jupiter-operation)


;;; Class jupiter-nop
;;

(defclass jupiter-nop (jupiter-operation)
  ()
  "Operation, which does not change anything.")

(defmethod jupiter-apply ((this jupiter-nop) buffer)
  "Applying THIS does not change BUFFER.")

(defmethod jupiter-transform ((this jupiter-nop) other)
  "Transforming OTHER with THIS simply returns OTHER."
  other)

(provide 'jupiter-nop)
;;; jupiter-nop.el ends here
