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
;; This library provides the following ERC commands:
;;
;; + /rudel host NAME PROTOCOL (KEYWORD VALUE)*
;;   Keyword arguments are optional here and can be omitted entirely
;;
;; + /rudel join SESSION (KEYWORD VALUE)*
;;   Keyword arguments are optional here and can be omitted entirely
;;
;; + /rudel join-manual NICK PROTOCOL (KEYWORD VALUE)*
;;   keyword arguments that should always work are
;;   + :username
;;   + :color


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


;;; Constants
;;

(defconst rudel-irc-erc-commands-subcommands
  '(("host"        . rudel-irc-erc-commands-host)
    ("join"        . rudel-irc-erc-commands-join)
    ("join-manual" . rudel-irc-erc-commands-join-manual))
  "List of subcommands of the /rudel ERC command and
corresponding implementing functions.")


;;; Commands
;;

(defun erc-cmd-RUDEL (command &rest args)
  ""
  (let ((handler (cdr (assoc (downcase command)
			     rudel-irc-erc-commands-subcommands))))
    (if handler
	(apply handler args)
      (erc-display-message
       nil 'notice 'active
       (format
	"RUDEL: %s undefined subcommand. Valid subcommands: %s."
	command
	(mapconcat #'car rudel-irc-erc-commands-subcommands ", ")))))
  )

(defun rudel-irc-erc-commands-host (name protocol-backend
				    &rest extra-properties)
  ""
  (rudel-host-session
   (append
    (rudel-irc-erc-commands-parse-keyword-args extra-properties)
    (list :name              name
	  :transport-backend (rudel-backend-get 'transport 'irc-erc)
	  :protocol-backend  (rudel-backend-get
			      'protocol (intern-soft protocol-backend))
	  :buffer            (current-buffer))))
  )

(defun rudel-irc-erc-commands-join (session-name
				    &rest extra-properties)
  ""
  (let ((info (rudel-irc-erc-commands-named-session session-name)))
    (rudel-join-session
     (append
      (rudel-irc-erc-commands-parse-keyword-args extra-properties)
      info))))

(defun rudel-irc-erc-commands-join-manual (nick protocol-backend
					   &rest extra-properties)
  ""
  (rudel-join-session
   (append
    (rudel-irc-erc-commands-parse-keyword-args extra-properties)
    (list :transport-backend (rudel-backend-get 'transport 'irc-erc)
	  :protocol-backend  (rudel-backend-get
			      'protocol (intern-soft protocol-backend))
	  :buffer            (current-buffer)
	  :peer-name         nick
	  :self-name         (erc-current-nick)
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
  (pcomplete-here (mapcar #'car rudel-irc-erc-commands-subcommands))

  (case (intern (downcase (pcomplete-arg 1)))
     (host
      (rudel-irc-erc-commands-complete-host))

     (join
      (rudel-irc-erc-commands-complete-join))

     (join-manual
      (rudel-irc-erc-commands-complete-join-manual)))
  )

(defun rudel-irc-erc-commands-complete-host ()
  ""
  ;; Name of the session.
  (when (= pcomplete-index 2)
    (message "Enter session name (no spaces)"))
  (pcomplete-here '(""))

  ;; Name of the protocol backend to use.
  (pcomplete-here
   (mapcar
    #'symbol-name
    (mapcar #'car (rudel-irc-erc-commands-capable-backends 'host))))

  ;; Keyword arguments of the backend.
  (let ((backend (rudel-irc-erc-commands-named-backend
		  (pcomplete-arg 'first 3))))
    (rudel-irc-erc-commands-complete-keyword-args
     '(:global-password)))
  )

(defun rudel-irc-erc-commands-complete-join ()
  ""
  ;; Names of advertised sessions.
  (pcomplete-here
   (mapcar (lambda (i) (plist-get i :name))
	   (rudel-session-initiation-discover 'irc-erc)))

  ;;
  (let ((backend (plist-get
		  (rudel-irc-erc-commands-named-session
		   (pcomplete-arg 'first 2))
		  :protocol-backend)))
    (when backend
      (rudel-irc-erc-commands-complete-keyword-args
       '(:username :color))))
  )

(defun rudel-irc-erc-commands-complete-join-manual ()
  ""
  ;; Nick of hosting user.
  (pcomplete-here
   erc-channel-users)

  ;; Name of the protocol backend to use.
  (pcomplete-here
   (mapcar
    #'symbol-name
    (mapcar #'car (rudel-irc-erc-commands-capable-backends 'join))))

  ;; Keyword arguments for the protocol backend.
  (let ((backend (rudel-irc-erc-commands-named-backend
		  (pcomplete-arg 'first 3))))
    (rudel-irc-erc-commands-complete-keyword-args
     '(:username :color)))
  )

(defun rudel-irc-erc-commands-complete-keyword-args (keywords)
  (let ((seen-keywords))
    (while t
      (push (pcomplete-arg 0) seen-keywords)

      ;; Keyword
      (pcomplete-here
       (sort
	(set-difference
	 (mapcar #'symbol-name keywords)
	 seen-keywords
	 :test #'equal)
	#'string<))

      ;; Value
      (pcomplete-here))))


;;; Utility functions
;;

(defun rudel-irc-erc-commands-capable-backends (capability)
  ""
  (rudel-backend-suitable-backends
   'protocol (lambda (backend)
	       (rudel-capable-of-p backend capability))))

(defun rudel-irc-erc-commands-named-backend (name)
  (rudel-backend-get 'protocol (intern-soft name)))

(defun rudel-irc-erc-commands-named-session (name)
  ""
  (find name (rudel-session-initiation-discover 'irc-erc)
	:key  (lambda (i) (plist-get i :name))
	:test #'string=))

(defun rudel-irc-erc-commands-parse-keyword-args (args)
  ""
  (mapcar
   (lambda (p)
     (or (and (= (aref p 0) ?:)
	      (intern-soft p))
	 (car (read-from-string p))))
   args))

(provide 'rudel-irc-erc-commands)
;;; rudel-irc-erc-commands.el ends here
