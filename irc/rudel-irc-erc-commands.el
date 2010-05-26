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
;;
;; + /rudel sessions


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
    ("join-manual" . rudel-irc-erc-commands-join-manual)
    ("sessions"    . rudel-irc-erc-commands-sessions))
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
  "Host a Rudel session that uses an IRC transport.
NAME is the name of the new session and can be chosen
arbitrarily. PROTOCOL-BACKEND specifies which protocol should be
used in the new session. A list of backends can be obtained using
`rudel-backend-dump'. EXTRA-PROPERTIES contain additional
settings for the new session and depends on the chosen protocol
backend."
  ;; Build a session information list and call `rudel-host-session'.
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
  "Join an advertised Rudel session that uses an IRC transport.
SESSION-NAME is the name of a previously announced
session. EXTRA-PROPERTIES can be used to specify additional
settings and depends on the kind of session. To join sessions
that have not been announced or whose information is not
available for other reasons, the join-manual command has to be
used. "
  ;; Retrieve the specified session, merge EXTRA-PROPERTIES into its
  ;; information.
  (let ((info       (rudel-irc-erc-commands-named-session session-name))
	(extra-info (rudel-irc-erc-commands-parse-keyword-args
		     extra-properties)))
    (unless info
      (error "Could not find session `%s'" session-name))

    (rudel-join-session (append extra-info info))))

(defun rudel-irc-erc-commands-join-manual (nick protocol-backend
					   &rest extra-properties)
  "Join a Rudel session that uses an IRC transport.
NICK is the IRC nick name of the user hosting the
session. PROTOCOL-BACKEND specifies the protocol used in the
session. EXTRA-PROPERTIES can be used to specify additional
settings and depends on the kind of session."
  (let ((protocol-backend (rudel-backend-get
			   'protocol (intern-soft protocol-backend)))
	(self-nick        (erc-current-nick))
	(extra-info       (rudel-irc-erc-commands-parse-keyword-args
			   extra-properties)))
    (unless protocol-backend
      (error "No such protocol backend: `%s'" protocol-backend))
    (unless self-nick
      (error "Could not determine current nick name"))

    (rudel-join-session
     (append
      extra-info
      (list :transport-backend (rudel-backend-get 'transport 'irc-erc)
	    :protocol-backend  protocol-backend
	    :buffer            (current-buffer)
	    :peer-name         nick
	    :self-name         self-nick
	    :encryption        nil
	    :username          self-nick
	    :global-password   nil
	    :local-password    nil))))
  )

(defun rudel-irc-erc-commands-sessions ()
  ""
  (dolist (session (rudel-session-initiation-discover 'irc-erc))
    (erc-display-line (concat "- " (plist-get session :name)) 'active)))


;;; Completion
;;

;;;###autoload
(defun pcomplete/erc-mode/RUDEL ()
  "Provides completion for the /RUDEL command."
  ;; First position is the subcomand.
  (pcomplete-here (mapcar #'car rudel-irc-erc-commands-subcommands))

  ;; For other positions, dispatch to specialized completion
  ;; functions.
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
  (pcomplete-here '("NAME"))

  ;; Name of the protocol backend to use.
  (pcomplete-here
   (mapcar
    #'symbol-name
    (mapcar #'car (rudel-irc-erc-commands-capable-backends 'host))))

  ;; Keyword arguments of the backend.
  (let* ((backend  (rudel-irc-erc-commands-named-backend
		    (pcomplete-arg 'first 3)))
	 (keywords (when backend
		     (rudel-irc-erc-commands-keywords-for-backend
		      backend 'host))))
    (when keywords
      (rudel-irc-erc-commands-complete-keyword-args
       keywords)))
  )

(defun rudel-irc-erc-commands-complete-join ()
  ""
  ;; Names of advertised sessions.
  (pcomplete-here
   (mapcar (lambda (info)
	     (replace-regexp-in-string
	      "\s" "_" (plist-get info :name)))
	   (rudel-session-initiation-discover 'irc-erc)))

  ;; Additional keyword arguments.
  (let* ((backend  (plist-get
		    (rudel-irc-erc-commands-named-session
		     (pcomplete-arg 'first 2))
		    :protocol-backend))
	 (keywords (when backend
		    (rudel-irc-erc-commands-keywords-for-backend
		     backend 'join))))
    (when keywords
      (rudel-irc-erc-commands-complete-keyword-args
       keywords)))
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
    (mapcar
     #'car
     (rudel-irc-erc-commands-capable-backends 'join))))

  ;; Keyword arguments for the protocol backend.
  (let* ((backend  (rudel-irc-erc-commands-named-backend
		    (pcomplete-arg 'first 3)))
	 (keywords (when backend
		     (rudel-irc-erc-commands-keywords-for-backend
		      backend 'join))))
    (when keywords
      (rudel-irc-erc-commands-complete-keyword-args
       keywords)))
  )

(defun rudel-irc-erc-commands-complete-keyword-args (keywords)
  "Complete keyword arguments and their values from KEYWORDS."
  ;; Keep track of already used keywords.
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
  "Return protocol backends capable of CAPABILITY."
  (rudel-backend-suitable-backends
   'protocol (lambda (backend)
	       (rudel-capable-of-p backend capability))))

(defun rudel-irc-erc-commands-named-backend (name)
  "Return the protocol backend named NAME."
  (rudel-backend-get 'protocol (intern-soft name)))

(defun rudel-irc-erc-commands-keywords-for-backend (backend operation)
  "Return keywords accepted by BACKEND for OPERATION.
OPERATION has to be one of join and host."
  ;; TODO obtain these from the backend
  (case operation
    (join
     '(:username :color))
    (host
     '(:global-password))
    (t
     (signal 'wrong-type-argument nil))))

(defun rudel-irc-erc-commands-named-session (name)
  "Return a session named NAME advertised via the ERC backend or nil.
In NAME, whitespace characters have to be replace by \"_\"."
  (find name (rudel-session-initiation-discover 'irc-erc)
	:key  (lambda (info)
		(replace-regexp-in-string
		 "\s" "_" (plist-get info :name)))
	:test #'string=))

(defun rudel-irc-erc-commands-parse-keyword-args (args)
  ""
  (mapcar #'rudel-irc-erc-commands-parse-keyword-arg args))

(defun rudel-irc-erc-commands-parse-keyword-arg (arg) ;; TODO fails for empty
  ""
  (or
   ;; If it looks like a keyword, treat it like a symbol
   (and (= (aref arg 0) ?:)
	(car (read-from-string arg)))

   ;; If it looks like a quoted expression,
   (and (= (aref arg 0) ?')
	(car (read-from-string (substring arg 1))))

   ;; If it looks like a double quoted string,
   (and (= (aref arg 0) ?\")
	(= (aref arg (- (length arg) 1)) ?\")
	(car (read-from-string (substring arg 1 -1))))

   ;; Otherwise, read value as string.
   (car (read-from-string (concat "\"" arg "\"")))))

(provide 'rudel-irc-erc-commands)
;;; rudel-irc-erc-commands.el ends here
