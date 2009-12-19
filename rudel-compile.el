;;; rudel-compile.el --- Byte-compile Rudel
;;
;; Copyright (C) 2009 Phil Hagelberg
;; Copyright (C) 2009 Jan Moringen
;;
;; Author: Phil Hagelberg <phil@enigma>
;;         Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, compile
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
;; This file contains some Emacs Lisp code, which can be used to
;; generate autoloads for Rudel and byte-compile Rudel. Autoloads or
;; written into a file named rudel-loaddefs.el. This file should be
;; loaded during the Emacs startup process.
;;
;; Press M-x eval-buffer to generate autoloads and byte-compile Rudel.


;;; History:
;;
;; 0.2 - Generation of autoloads
;;
;; 0.1 - Initial version


;;; Code:
;;

(require 'eieio)
(require 'bytecomp)

(let* ((rudel-dir (file-name-directory
		   (or (buffer-file-name) load-file-name)))
       (subdirs   (mapcar
		   (lambda (subdir)
		     (concat rudel-dir "/" subdir))
		   '("." "jupiter" "socket" "tls" "obby" "zeroconf")))
       (loaddefs  (concat rudel-dir "rudel-loaddefs.el")))

  (flet ((byte-compile-cl-warn (&rest) nil))

    ;; Adjust load path for compilation. We need to have all Rudel
    ;; subdirectories on the load path.
    (dolist (subdir subdirs)
      (add-to-list 'load-path subdir))

    ;; Byte compile everything.
    (byte-recompile-directory rudel-dir 0)

    ;; Update autoloads.
    (let ((generated-autoload-file loaddefs))
      (apply #'update-directory-autoloads subdirs))

    ;; This is for compatibility with older Emacs versions. Starting
    ;; from version 23.1 of GNU Emacs eieio should always be
    ;; (auto)loaded.
    (with-current-buffer (find-file-noselect loaddefs)
      (goto-char 1)
      (unless (looking-at "(require 'eieio)")
	(insert "(require 'eieio)\n\n")
	(save-buffer))
      (kill-buffer))))

;;; rudel-compile.el ends here
