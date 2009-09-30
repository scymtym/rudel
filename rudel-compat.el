;;; rudel-compat.el --- Compatibility code for Rudel
;;
;; Copyright (C) 2009 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: Rudel, compatibility
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

(defvar progress-pulse-values ["-" "\\" "|" "/"])

(defun make-progress-reporter-pulse (&optional message)
  "Return a pulsing progress reporter that display MESSAGE.
MESSAGE can contain all formatting characters accepted by
`format'. If message is nil, the string \"Working ...\" is
displayed.

Example:
(let ((rep (make-progress-reporter-pulse \"Connecting\")))
  (dotimes (n 3)
    (sleep-for 0.1)
    (progress-reporter-pulse rep \"Connecting [new]\"))
  (dotimes (n 3)
    (sleep-for 0.1)
    (progress-reporter-pulse rep \"Connecting [synching]\"))
  (dotimes (n 3)
    (sleep-for 0.1)
    (progress-reporter-pulse rep \"Connecting [idle]\"))
  (progress-reporter-pulse rep \"Connecting \")
  (progress-reporter-done rep))"
  (cons 'dummy 
	(vector 'dummy 0 'dummy (or message "Working ... "))))

(defun progress-reporter-pulse (reporter &optional new-message)
  "Advance indicator and display ARGS in message of REPORTER.
The number of arguments has to match the number of formatting
characters in the message of REPORTER."
  (let* ((parameters (cdr reporter))
	 (message    (or new-message
			 (aref parameters 3)))
	 (index      (aref parameters 1))
	 (new-index  (mod (+ index 1) 4)))
    (aset parameters 1 new-index)
    (aset parameters 3 message)
    (let ((message-log-max nil)) ;; No logging
      (message "%s %s"
	       (aref progress-pulse-values new-index)
	       message))))

(provide 'rudel-compat)
;;; rudel-compat.el ends here
