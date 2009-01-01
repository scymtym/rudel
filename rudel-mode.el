;;; rudel-mode.el --- Global and buffer-local Rudel minor modes
;;
;; Copyright (C) 2008, 2009 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;; Keywords: rudel, mode
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
;; This file contains the following global and buffer-local Rudel
;; minor modes:
;; - global-rudel-minor-mode: Installs a keymap and a `Rudel' menu

;;; History:
;;
;; 0.1 - Initial revision.

;;; Code:
;;

(require 'easymenu)

(require 'rudel)


;;; Global mode, menu and keymap
;;

;;;###autoload
(defvar global-rudel-minor-mode nil
  "Non-nil when Rudel is active globally.")

(add-to-list 'minor-mode-alist '(global-rudel-minor-mode ""))

(defvar rudel-minor-keymap
  (let ((map  (make-sparse-keymap))
	(pmap (make-sparse-keymap)))
    (define-key pmap "j" 'rudel-join-session)
    (define-key pmap "h" 'rudel-host-session)
    (define-key pmap "e" 'rudel-end-session)

    (define-key pmap "c" 'rudel-change-color)

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
      [ "Join Session"             rudel-join-session
	                           (not rudel-current-session) ]
      [ "Host a Session"           rudel-host-session
	                           (not rudel-current-session) ]
      [ "End Session"              rudel-end-session
	                           rudel-current-session ]
      "---"
      [ "Change Color"             rudel-change-color
	                           (and rudel-current-session
					(rudel-capable-of-p 
					 (oref rudel-current-session :backend)
					 'change-color)) ] ; TODO bulky
      "---"
      [ "Publish current Buffer"   rudel-publish-buffer 
	                           (and rudel-current-session 
					(not rudel-buffer-document)) ]
      [ "Unpublish current Buffer" rudel-unpublish-buffer
	                           rudel-buffer-document ]
      [ "Subscribe to Document"    rudel-subscribe
	                           rudel-current-session ]
      "---"
      [ "Rudel Overview"           rudel-speedbar t ]
      "---"
      ( "Options")
      ))
  )

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
		 (<= (prefix-numeric-value arg) 0))))
  )

(provide 'rudel-mode)
;;; rudel-mode.el ends here
