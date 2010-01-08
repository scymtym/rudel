;;; rudel-irc-erc-commands.el --- ERC frontend integration for Rudel
;;
;; Copyright (C) 2010 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, irc, erc, integration
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
;; Provided commands
;; + /rudel host
;; + /rudel join NICK


;;; History:
;;
;; 0.1 - Initial version


;;; Code:
;;

(eval-when-compile
  (require 'cl)
  (require 'pcomplete))

(require 'erc)

(require 'rudel)


;;; Module
;;

;;;###autoload (autoload 'erc-rudel-mode "rudel-irc-erc-commands")
(define-erc-module rudel nil
  "Provide Rudel integration for ERC."
  ;; Enable
  (nil)
  ;; Disable
  (nil))


;;; Commands
;;

(defun erc-cmd-RUDEL (command &rest args)
  ""
  (let ((handler (cdr (assoc (downcase command)
			     '(("host" . rudel-irc-erc-host)
			       ("join" . rudel-irc-erc-join))))))
    (unless handler
      (erc-display-message
       nil 'notice 'active
       (format
	"RUDEL: %s undefined subcommand. Valid subcommands: HOST and JOIN."
	command)))
    (apply handler args))
  )

(defun rudel-irc-erc-host (&optional protocol-backend &rest extra-properties)
  ""
  (rudel-host-session
   (list :transport-backend (rudel-backend-get 'transport 'irc-erc)
	 :protocol-backend  (if protocol-backend
				(rudel-backend-get
				 'protocol
				 (intern-soft protocol-backend))
			      (car (rudel-irc-erc-host-capable-backends)))
	 :buffer            (current-buffer)
	 :channel           "#rudel-rudel"))
  )

;; (defun rudel-irc-erc-join (session-name)
;;   )

(defun rudel-irc-erc-join (nick &rest extra-properties)
  ""
  (rudel-join-session
   (append
    (mapcar
     (lambda (p)
       (or (and (= (aref p 0) ?:)
		(intern-soft p))
	   (read-from-string p)))
     extra-properties)
    (list :transport-backend (rudel-backend-get 'transport 'irc-erc)
	  :protocol-backend  (rudel-backend-get 'protocol  'obby)
	  :buffer            (current-buffer)
	  :peer-name         nick
	  :self-name         (erc-current-nick) ;; TODO can change
						;; later; handle in
						;; transport?
	  :encryption        nil
	  :username          (erc-current-nick)
	  :global-password   nil
	  :local-password    nil)))
  )


;;; Completion
;;

;;;###autoload
(defun pcomplete/erc-mode/RUDEL ()
  "Provides completion for the /RUDEL command."
  (pcomplete-here '("host" "join"))

  (pcomplete-here
   (case (intern (downcase (pcomplete-arg 1)))
     (host
      (mapcar
       #'symbol-name
       (mapcar #'car (rudel-irc-erc-host-capable-backends))))

     ;; TODO We could also have a session initiation backend that
     ;; knows announced sessions
     ;;(join
     ;;(mapcar (lambda (i) (plist-get :name i)) (rudel-discover 'erc))

     (join
      erc-channel-users)))
  )


;;; Utility functions
;;

(defun rudel-irc-erc-host-capable-backends ()
  ""
  (rudel-backend-suitable-backends
   'protocol (lambda (b) (rudel-capable-of-p b 'host))))

(provide 'rudel-irc-erc-commands)
;;; rudel-irc-erc-commands.el ends here
