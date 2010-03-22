;;; rudel-irc-erc-session-initiation.el --- Rudel session initiation over ERC
;;
;; Copyright (C) 2010 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, irc, erc, session initiation
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
;; This file contains an implementation of the
;; `rudel-session-initiation-backend' interface that operates on top
;; of erc connections, sending and receiving session advertise or
;; withdraw messages.


;;; History:
;;
;; 0.1 - Initial version


;;; Code:
;;

(require 'rudel-session-initiation)

(require 'rudel-irc-erc-util)


;;; Class rudel-irc-erc-session-initiation-handler
;;

(defclass rudel-irc-erc-session-initiation-handler
  (rudel-irc-erc-base)
  ((ctcp-type :initform "RUDEL-INIT")
   (master    :initarg :master
	      :type    object
	      :documentation
	      ""))
  "")

(defmethod initialize-instance
  ((this rudel-irc-erc-session-initiation-handler) slots)
  ""
  (when (next-method-p)
    (call-next-method))

  ;; Install CTCP handler for RUDEL-INIT messages.
  (with-slots (ctcp-type message-handler) this
    (lexical-let ((this1 this))
      (setq message-handler
	    (rudel-irc-erc-make-handler
		(data sender nil nil ctcp-type)
		;; Split data into command and argument parts.
		(let* ((index   (position ?\s  data))
		       (command (substring data 0 index))
		       (args    (substring data (+ index 1))))
		  (rudel-handle this1 sender command args))))))
  )

(defmethod rudel-advertise ((this rudel-irc-erc-session-initiation-handler)
			    info)
  ""
  (rudel-send
   this
   "ADVERTISE" (rudel-irc-erc-prin1-and-base64-encode-string info)))

(defmethod rudel-withdraw ((this rudel-irc-erc-session-initiation-handler)
			   info)
  ""
  (rudel-send
   this
   "WITHDRAW" (rudel-irc-erc-prin1-and-base64-encode-string info)))

(defmethod rudel-handle ((this rudel-irc-erc-session-initiation-handler)
			 sender type &rest args)
  ""
  (case (intern-soft (downcase type))
    ;; Handle ADVERTISE message.
    (advertise
     (rudel-handle-advertise this sender (car args))
     t)

    ;; Handle WITHDRAW message.
    (withdraw
     (rudel-handle-withdraw this sender (car args))
     t)

    ;; Do not handle other messages.
    (t
     nil))
  )

(defmethod rudel-handle-advertise
  ((this rudel-irc-erc-session-initiation-handler) sender data)
  "Add SENDER's session described by DATA to session list."
  (with-slots (buffer master) this
    (let ((info (condition-case error
		    (rudel-irc-erc-read-from-base64-encoded-string data)
		  (error
		   (display-warning
		    '(rudel irc)
		    (format
		     "Could not decode advertised session info: %s: %s "
		     (car error) (cdr error))
		    :warning)
		   nil))))
      (when info
	(rudel-add-session
	 master
	 (append (list
		  :name              (format
				      "IRC session \"%s\" by \"%s\""
				      (plist-get info :name)
				      sender)
		  :peer-name         sender
		  :buffer            buffer
		  :transport-backend 'irc-erc)
		 info))))) ;; TODO handle buffer properly
  )

(defmethod rudel-handle-withdraw
  ((this rudel-irc-erc-session-initiation-handler) sender data)
  "Remove SENDER's session described by DATA from session list."
  (with-slots (master) this
    (let ((info (condition-case error
		    (rudel-irc-erc-read-from-base64-encoded-string data)
		  (error
		   (display-warning
		    '(rudel irc)
		    (format
		     "Could not decode withdrawn session info: %s: %s "
		     (car error) (cdr error))
		    :warning)
		   nil))))
      (when info
	(rudel-remove-session master info))))
  )


;;; Class rudel-irc-erc-session-initiation
;;

;;;###autoload
(defclass rudel-irc-erc-session-initiation
  (rudel-session-initiation-backend)
  ((capabilities :initform (discover advertise))
   (priority     :initform primary)
   (handlers     :initarg  :handlers
		 :type     list
		 :initform nil
		 :documentation
		 "")
   (sessions     :initarg  :sessions
		 :type     list ;; of plists
		 :initform nil
		 :documentation
		 "TODO"))
  "TODO")

(defmethod initialize-instance ((this rudel-irc-erc-session-initiation) slots)
  ""
  ;; Initialize slots of THIS.
  (when (next-method-p)
    (call-next-method))

  ;; Set version slot.
  (oset this :version '(0 1)))

(defmethod initialize-instance :after
  ((this rudel-irc-erc-session-initiation) slots)
  "Install handlers which send notifications to THIS."
  ;; Add a handler to `erc-join-hook' that installs a
  ;; `rudel-irc-erc-session-initiation-handler' for the joined
  ;; channel.
  (lexical-let ((this1 this))
    (add-hook
     'erc-join-hook
     (lambda ()
       (rudel-add-handler
	this1 erc-session-server (erc-server-buffer) (buffer-name)))))
  )

(defmethod rudel-discover ((this rudel-irc-erc-session-initiation))
  ""
  (oref this :sessions))

(defmethod rudel-advertise ((this rudel-irc-erc-session-initiation)
			    info)
  ""

  (dolist (handler (oref this :handlers))
    (rudel-advertise handler info))

  (erc-send-action
   (erc-default-target)
   (format "announces Rudel session %s" (plist-get info :name))))

(defmethod rudel-withdraw ((this rudel-irc-erc-session-initiation)
			    info)
  ""
  (dolist (handler (oref this :handlers))
    (rudel-withdraw handler info)))

(defmethod rudel-add-handler ((this rudel-irc-erc-session-initiation)
			      server buffer target)
  "Add a handler whose message go to TARGET to BUFFER."
  (with-slots (handlers) this
    (push
     (rudel-irc-erc-session-initiation-handler
      (format "server %s" server)
      :buffer    buffer
      :peer-name target
      :master    this)
     handlers))
  )

(defmethod rudel-add-session ((this rudel-irc-erc-session-initiation)
			      info)
  "TODO
This function is called by
`rudel-irc-erc-session-initiation-handler' objects."
  (with-slots (sessions) this
    (let ((info (rudel-session-initiation-adjust-info info)))
      (push info sessions))))

(defmethod rudel-remove-session ((this rudel-irc-erc-session-initiation)
				 info)
  "TODO
This function is called by
`rudel-irc-erc-session-initiation-handler' objects."
  (with-slots (sessions) this
    (setq sessions (remove info sessions))))


;;; Autoloading
;;

;;;###autoload
(rudel-add-backend (rudel-backend-get-factory 'session-initiation)
		   'irc-erc 'rudel-irc-erc-session-initiation)

(provide 'rudel-irc-erc-session-initiation)
;;; rudel-irc-erc-session-initiation.el ends here
