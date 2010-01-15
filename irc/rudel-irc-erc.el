;;; rudel-irc-erc.el --- IRC transport backend based on ERC
;;
;; Copyright (C) 2010 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, irc, erc, transport, backend
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
;; This contains an IRC transport backend for Rudel that uses ERC to
;; do the heavy lifting.


;;; History:
;;
;; 0.1 - Initial version


;;; Code:
;;

(require 'erc)
(require 'erc-backend)

(require 'eieio)

(require 'rudel-transport)
(require 'rudel-transport-util)

(require 'rudel-irc-erc-util)


;;; Constants
;;

(defconst rudel-irc-erc-transport-version '(0 1)
  "Version of the ERC transport for Rudel.")


;;; Class rudel-irc-erc-transport
;;

(defclass rudel-irc-erc-transport (rudel-transport
				   rudel-irc-erc-base)
  ((filter   :initarg  :filter
	     :type     (or null function)
	     :initform nil
	     :reader   rudel-filter
	     :writer   rudel-set-filter
	     :documentation
	     "")
   (sentinel :initarg  :sentinel
	     :type     (or null function)
	     :initform nil
	     :reader   rudel-sentinel
	     :writer   rudel-set-sentinel
	     :documentation
	     ""))
  "")

(defmethod initialize-instance ((this rudel-irc-erc-transport) slots)
  ""
  ;;
  (when (next-method-p)
    (call-next-method))

  (with-slots (peer-name self-name ctcp-type message-handler part-handler) this
    ;; Construct handlers for PRIVMSG and PART messages. Store the
    ;; handler function for later removal.
    (lexical-let ((this1 this))
      (setq message-handler
	    (rudel-irc-erc-make-handler
		(data sender peer-name self-name ctcp-type)
		(rudel-handle this1 data)))))
  )

(defmethod rudel-send ((this rudel-irc-erc-transport) data)
  ""
  (when (next-method-p)
    (call-next-method this "DATA" (base64-encode-string data t)))
  )

(defmethod rudel-close ((this rudel-irc-erc-transport))
  "Nothing to do.")

(defmethod rudel-start ((this rudel-irc-erc-transport))
  "TODO.")

(defmethod rudel-handle ((this rudel-irc-erc-transport) data)
  ""
  (cond
   ;; Handle DATA messages.
   ((and (>= (length data) 5)
	 (string= (substring data 0 4) "DATA"))
    (with-slots (filter) this
      (when filter
	(let ((data (condition-case error
			(base64-decode-string (substring data 5))
		      (error nil))))
	  (if data
	      (funcall filter data)
	    (display-warning
	     '(rudel irc)
	     (format "Could not decode data: %s" data)
	     :warning))))))

   ;; Warn if we do not understand the message.
   (t
    (display-warning
     '(rudel irc)
     (format "Message not understood: %s" data)
     :warning)))
  )


;;; Class rudel-irc-erc-listener
;;

(defclass rudel-irc-erc-listener (rudel-listener
				  rudel-irc-erc-base)
  ((ctcp-type :initform "RUDEL-SETUP")
   (dispatch  :initarg :dispatch
	      :type    (or null function)
	      :reader  rudel-dispatcher
	      :writer  rudel-set-dispatcher
	      :documentation
	      ""))
  "")

(defmethod initialize-instance ((this rudel-irc-erc-listener) slots)
  ""
  ;;
  (when (next-method-p)
    (call-next-method))

  (with-slots (peer-name self-name ctcp-type message-handler part-handler) this
    ;; Construct a handler for CTCP messages. Store the handler
    ;; function for later removal.
    (lexical-let ((this1 this))
      (setq message-handler
	    (rudel-irc-erc-make-handler
		(data sender peer-name self-name ctcp-type)
	      (rudel-handle this1 sender data)))))
  )

(defmethod rudel-close ((this rudel-irc-erc-listener))
  "TODO")

(defmethod rudel-handle ((this rudel-irc-erc-listener) sender data)
  ""
  (cond
   ;; Handle CONNECT messages.
   ((string= data "CONNECT")
    ;; TODO (rudel-send this "OK" "RUDEL")
    (with-slots (buffer ctcp-type) this
      (with-current-buffer buffer
	(erc-cmd-CTCP sender ctcp-type "OK" "RUDEL")))

    (with-slots (buffer self-name dispatch) this
      (when dispatch
	(let ((transport (rudel-irc-erc-transport
			  "bla"
			  :buffer    buffer
			  :peer-name sender
			  :self-name self-name
			  :ctcp-type "RUDEL"))) ;; TODO

	  ;; TODO unify this
	  (setq transport (rudel-transport-make-filter-stack
			   transport
			   '(
			     (rudel-progress-reporting-transport-filter)
			     (rudel-collecting-transport-filter)
			     )))

	  (funcall dispatch transport)))))

   (t
    (display-warning
     '(rudel irc)
     (format "Message not understood %s" data)
     :warning)))
  )


;;; Class rudel-irc-erc-backend
;;

;;;###autoload
(defclass rudel-irc-erc-backend (rudel-transport-backend)
  ((capabilities :initform (listen connect)))
  "IRC-ERC transport backend.
The transport backend is a factory for IRC-ERC transport objects.")

(defmethod initialize-instance ((this rudel-irc-erc-backend) slots)
  "Initialize slots and set version of THIS."
  ;; Initialize slots of THIS.
  (when (next-method-p)
    (call-next-method))

  ;; Set version slot.
  (oset this :version rudel-irc-erc-transport-version))

(defmethod rudel-make-connection ((this rudel-irc-erc-backend)
				  info info-callback
				  &optional progress-callback)
  "Connect to a IRC-ERC server using the information in INFO.
INFO has to be a property list containing the keys :host
and :port."
  ;; Ensure that INFO contains all necessary information.
  (unless (every (lambda (keyword) (member keyword info))
		 '(:buffer :peer-name))
    (setq info (funcall info-callback this info)))

  ;; Extract information from INFO and create the socket.
  (let ((buffer    (plist-get info :buffer))
	(peer-name (plist-get info :peer-name))
	(transport))

    ;;
    (with-current-buffer buffer
      (erc-cmd-CTCP peer-name "RUDEL-SETUP" "CONNECT"))

    (setq transport (rudel-irc-erc-transport
		     "bla" ;; TODO
		     :buffer    buffer
		     :peer-name peer-name
		     :ctcp-type "RUDEL"))

    ;; Create the transport
    (rudel-transport-make-filter-stack
     transport
     '(
       (rudel-progress-reporting-transport-filter)
       (rudel-collecting-transport-filter)
       ))
    )
  )

(defmethod rudel-wait-for-connections ((this rudel-irc-erc-backend)
				       info info-callback)
  "Create IRC-ERC server according to INFO.
INFO has to be a property list containing the key :port."
  ;; Ensure that INFO contains all necessary information.
  (unless (every (lambda (keyword) (member keyword info))
		 '(:buffer))
    (setq info (funcall info-callback this info)))

  ;; Extract information from INFO and create the socket.
  (let* ((buffer   (plist-get info :buffer))
	 ;; Create the listener object; without process for now.
	 (listener (rudel-irc-erc-listener
		    "TODO"
		    :buffer buffer)))
    ;; Return the listener.
    listener)
  )


;;; Autoloading
;;

;;;###autoload
(rudel-add-backend (rudel-backend-get-factory 'transport)
		   'irc-erc 'rudel-irc-erc-backend)

(provide 'rudel-irc-erc)
;;; rudel-irc-erc.el ends here
