;;; rudel-mode.el --- Read a document name
;;; Copyright (C) 2008
;;   by Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This file is part of .
;;
;;  is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.
;;
;;  is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with ; see the file COPYING. If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA, or see <http://www.gnu.org/licenses>.

;;; Commentary:
;;
;; 


;;; History:
;; 

;;; Code:

(require 'rudel)
;easy-menu-define?

(defun rudel-read-backend (backends &optional prompt return)
  ""
  (unless prompt
    (setq prompt "Backend: "))
  (let* ((backend-names (mapcar 'car backends))
	 (backend-name  (completing-read prompt backend-names)))
    (cond
     ((eq return 'object) (cdr (assoc backend-name backends)))
     (t                   backend-name)))
  )

(defun rudel-read-document (documents &optional prompt return)
  "Read a document name."
  (unless prompt
    (setq prompt "Document: "))
  (let* ((document-names (mapcar 'object-name-string documents))
	 (document-name  (completing-read prompt document-names)))
    (cond 
     ((eq return 'object) (rudel-find-document 
			   rudel-current-session document-name))
     (t                   document-name)))
  )


;;; Mode, Menu and Keymap
;;

;;;###autoload
(defvar global-rudel-minor-mode nil
  "Non-nil when Rudel is active globally.")
;(make-variable-buffer-local 'rudel-minor-mode)

(add-to-list 'minor-mode-alist '(global-rudel-minor-mode ""))

(defvar rudel-minor-keymap
  (let ((map  (make-sparse-keymap))
	(pmap (make-sparse-keymap)))
    (define-key pmap "j" 'rudel-join-session)
    (define-key pmap "h" 'rudel-host-session)
    (define-key pmap "e" 'rudel-end-session)

    (define-key pmap "p" 'rudel-publish-buffer)
    (define-key pmap "u" 'rudel-unpublish-buffer)
    (define-key pmap "s" 'rudel-subscribe)

    ;; bind our submap into map
    (define-key map "\C-cc" pmap)
    map)
  "Keymap used in Rudel minor mode.")

(when rudel-minor-keymap
  (easy-menu-define
    rudel-minor-menu rudel-minor-keymap "Rudel Minor Mode Menu"
    '("Rudel"
      [ "Join Session"             rudel-join-session (not rudel-current-session) ]
      [ "Host a Session"           rudel-host-session (not rudel-current-session) ]
      [ "End Session"              rudel-end-session  rudel-current-session ]
      "---"
      [ "Change color"             rudel-change-color rudel-current-session ]
      ; (and rudel-capable-of-p 'change-color)
      "---"
      [ "Publish current Buffer"   rudel-publish-buffer 
	                           (and rudel-current-session 
					(not rudel-buffer-document)) ]
      [ "Unpublish current Buffer" rudel-unpublish-buffer rudel-buffer-document ]
      [ "Subscribe to Document"    rudel-subscribe rudel-current-session ]
      "---"
      [ "Rudel Overview"           rudel-speedbar t ]
      ))
  )

;; Allow re-insertion of a new keymap
(add-to-list 'minor-mode-map-alist
	     (cons 'global-rudel-minor-mode rudel-minor-keymap))

;;;###autoload
(defun global-rudel-minor-mode (&optional arg)
  "Global Rudel minor mode.
TODO

With argument ARG positive, turn on the mode.  Negative, turn off the
mode.  nil means to toggle the mode."
  (interactive "P")
  (setq global-rudel-minor-mode
	(not (or (and (null arg) global-rudel-minor-mode)
		 (<= (prefix-numeric-value arg) 0)))))

(provide 'rudel-mode)
;;; rudel-mode.el ends here
