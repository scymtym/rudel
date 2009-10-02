;;; rudel-compat.el --- Compatibility code for Rudel
;;
;; Copyright (C) 2009 Jan Moringen
;; Copyright (C) 2009 Phil Hagelberg
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;;         Phil Hagelberg <phil@enigma>
;; Keywords: rudel, compatibility
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
;; This file contains compatibility code required to make Rudel work
;; with different versions of Emacs.


;;; History:
;;
;; 0.1 - Initial revision


;;; Code:
;;

(unless (fboundp 'read-color)
  (defun read-color (prompt &rest ignored)
    "Poor man's read color without completion.
You have to take care to only enter valid color names."
    (read-string prompt)))


;;; Pulsing Progress Reporter
;;

(unless (functionp 'progress-reporter-pulse)
  (defvar progress-pulse-values ["-" "\\" "|" "/"])

  (defun progress-reporter-pulse (reporter &optional new-message)
  "Advance pulsing indicator of REPORTER. Display NEW-MESSAGE if given."
  (let* ((parameters (cdr reporter))
	 (message    (or new-message
			 (aref parameters 3)))
	 (index      (if (< (car reporter) 0)
                       (floor (- (car reporter)))
                       0))
	 (new-index  (mod (+ index 1) 4)))
    (setcar reporter (float (- new-index)))
    (aset parameters 0 (float-time))
    (aset parameters 3 message)
    (let ((message-log-max nil)) ;; No logging
      (message "%s %s"
	       (aref progress-pulse-values new-index)
	       message)))))

(provide 'rudel-compat)
;;; rudel-compat.el ends here
