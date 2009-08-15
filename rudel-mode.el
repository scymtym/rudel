;;; rudel-mode.el --- Global and buffer-local Rudel minor modes
;;
;; Copyright (C) 2008, 2009 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, mode
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
;; This file contains the following global and buffer-local Rudel
;; minor modes:
;; - global-rudel-minor-mode: Installs a keymap and a `Rudel' menu


;;; History:
;;
;; 0.1 - Initial revision.
;;
;; 0.2 - Use define-minor-mode


;;; Code:
;;

(require 'easy-mmode)
(require 'easymenu)

(require 'rudel)


;;; Global mode, menu and keymap
;;

(defvar rudel-minor-keymap
  (let ((map     (make-sparse-keymap))
	(sub-map (make-sparse-keymap)))
    ;; Define sub keymap
    (define-key sub-map "j" #'rudel-join-session)
    (define-key sub-map "h" #'rudel-host-session)
    (define-key sub-map "e" #'rudel-end-session)

    (define-key sub-map "c" #'rudel-change-color)

    (define-key sub-map "p" #'rudel-publish-buffer)
    (define-key sub-map "u" #'rudel-unpublish-buffer)
    (define-key sub-map "s" #'rudel-subscribe)

    ;; Bind the sub keymap into map
    (define-key map "\C-cc" sub-map)
    map)
  "Keymap used in Rudel minor mode.")

(when rudel-minor-keymap
  (easy-menu-define
    rudel-minor-menu rudel-minor-keymap "Rudel Minor Mode Menu"
    '("Rudel"
      [ "Join Session"             rudel-join-session
	                           (not rudel-current-session) ]
      [ "Leave Session"            rudel-end-session
	                           rudel-current-session ]
      "---"
      [ "Host a Session"           rudel-host-session
	                           t ]
      "---"
      [ "Change Color"             rudel-change-color
	                           (and rudel-current-session
					(rudel-capable-of-p
					 (oref rudel-current-session :backend)
					 'change-color)) ] ; TODO bulky
      "---"
      [ "Publish current Buffer"   rudel-publish-buffer
	                           (and rudel-current-session
					(not (rudel-buffer-has-document-p))) ]
      [ "Unpublish current Buffer" rudel-unpublish-buffer
	                           (rudel-buffer-has-document-p) ]
      [ "Subscribe to Document"    rudel-subscribe
	                           rudel-current-session ]
      "---"
      [ "Rudel Overview"           rudel-speedbar
	                           t ]
      "---"
      ( "Options")
      ))
  )

;;;###autoload
(define-minor-mode global-rudel-minor-mode
  "Toggle global Rudel minor mode (No modeline indicator).

If ARG is null, toggle global Rudel mode.
If ARG is a number greater than zero, turn on global Rudel mode;
otherwise, turn it off."
  :init-value nil
  :keymap     rudel-minor-keymap
  :global     t
  :group      'rudel
  (cond
   ;; Emacs session is not interactive
   (noninteractive
    (setq global-rudel-minor-mode nil))

   ;; Mode is enabled
   (global-rudel-minor-mode
    )

   ;; Mode is disabled
   (t
    ))
  )

(provide 'rudel-mode)
;;; rudel-mode.el ends here
