;;; rudel-irc-erc.el --- IRC transport backend based on ERC
;;
;; Copyright (C) 2010 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, irc, transport, backend
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



;;; Constants
;;

(defconst rudel-irc-erc-version '(0 1)
  "Version of the ERC transport for Rudel.")


;;; stuff for rudel-irc-erc-util
;;

(defclass rudel-irc-erc-base ()
  ((buffer    :initarg :buffer
	      :type    buffer
	      :documentation
	      "")
   (peer-name :initarg :peer-name
	      :type    string
	      :initform "" ;; TODO temp
	      :documentation
	      "")
   (self-name :initarg :self-name
	      :type    string
	      :documentation
	      ""))
  ""
  :abstract t)

(defmethod initialize-instance ((this rudel-irc-erc-base) slots)
  ""
  (when (next-method-p)
    (call-next-method))

  (with-slots (buffer peer-name self-name) this
    (lexical-let ((this1      this)
		  (peer-name1 peer-name)
		  (self-name1 self-name))
      (with-current-buffer buffer
	;; TODO
	(add-hook 'erc-server-PRIVMSG-functions
		  (lambda (server response)
		    (let ((args   (erc-response.command-args response))
			  (sender (nth 0 (erc-parse-user
					  (erc-response.sender response))))
			  (data   (erc-response.contents response)))
		      (message "sender: %s" sender)
		      (when (and (or (string= peer-name1 "")
				     (string= sender peer-name1))
				 (string= (car args) self-name1))
			(rudel-handle this1 data)))
		    ;; Prevent other handlers from running?
		    nil))

	;; TODO
	(add-hook 'erc-server-PART-functions
		  (lambda (server response)
		    nil)))))
  )

(defmethod rudel-send ((this rudel-irc-erc-base) data)
  "Send DATA through THIS."
  (with-slots (buffer peer-name) this
    (with-current-buffer buffer
      (erc-server-send (format "PRIVMSG %s :%s\n"
			       peer-name
			       (base64-encode-string data))))))


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

(defmethod rudel-handle ((this rudel-irc-erc-transport) data)
  ""
  (with-slots (filter) this
    (when filter
      (let ((data (condition-case error
		      (base64-decode-string data)
		    (error nil))))
	(when data
	  (funcall filter data))))))

(defmethod rudel-close ((this rudel-irc-erc-transport))
  "Nothing to do.")

(defmethod rudel-start ((this rudel-irc-erc-transport))
  "TODO.")


;;; Class rudel-irc-erc-listener
;;

(defclass rudel-irc-erc-listener (rudel-listener
				  rudel-irc-erc-base)
  ((dispatch :initarg :dispatch
	     :type    (or null function)
	     :reader  rudel-dispatcher
	     :writer  rudel-set-dispatcher
	     :documentation
	     ""))
  "")

(defmethod rudel-close ((this rudel-irc-erc-listener))
  "TODO")

(defmethod rudel-handle ((this rudel-irc-erc-listener) data)
  ""
  (with-slots (buffer dispatch) this
    (when dispatch
      (let ((transport (rudel-irc-erc-transport
			"bla"
			:buffer    buffer
			:peer-name data
			:self-name "scymtym"))) ;; TODO
	(funcall dispatch transport)))))


;;; Class rudel-irc-erc-backend
;;

;;;###autoload
(defclass rudel-irc-erc-backend (rudel-transport-backend)
  ((capabilities :initform (listen connect)))
  "IRC-ERC transport backend.
The transport backend is a factory for IRC-ERC transport objects.")

(defmethod initialize-instance ((this rudel-irc-erc-backend) slots)
  "Initialize slots and set version of THIS."
  (when (next-method-p)
    (call-next-method))

  (oset this :version rudel-irc-erc-version))

(defmethod rudel-make-connection ((this rudel-irc-erc-backend)
				  info info-callback
				  &optional progress-callback)
  "Connect to a IRC-ERC server using the information in INFO.
INFO has to be a property list containing the keys :host
and :port."
  ;; Ensure that INFO contains all necessary information.
  (unless (every (lambda (keyword) (member keyword info))
		 '(:buffer :peer-name :self-name))
    (setq info (funcall info-callback this info)))

  ;; Extract information from INFO and create the socket.
  (let* ((buffer    (plist-get info :buffer))
	 (peer-name (plist-get info :peer-name))
	 (self-name (plist-get info :self-name))
	 (transport (rudel-irc-erc-transport
		     "bla"
		     :buffer    buffer
		     :peer-name peer-name
		     :self-name self-name)))
    ;; Create the transport
    (rudel-transport-make-filter-stack
     transport
     '((rudel-progress-reporting-transport-filter)))
    )
  )

(defmethod rudel-wait-for-connections ((this rudel-irc-erc-backend)
				       info info-callback)
  "Create IRC-ERC server according to INFO.
INFO has to be a property list containing the key :port."
  ;; Ensure that INFO contains all necessary information.
  (unless (every (lambda (keyword) (member keyword info))
		 '(:buffer :channel))
    (setq info (funcall info-callback this info)))

  ;; Extract information from INFO and create the socket.
  (let* ((buffer    (plist-get info :buffer))
	 (channel   (plist-get info :channel))
	 ;; Create the listener object; without process for now.
	 (listener (rudel-irc-erc-listener
		    (format "on %s" channel)
		    :buffer    buffer
		    :self-name channel)))
    ;; Return the listener.
    listener))


;;; Autoloading
;;

;;;###autoload
(rudel-add-backend (rudel-backend-get-factory 'transport)
		   'irc-erc 'rudel-irc-erc-backend)

(provide 'rudel-irc-erc)
;;; rudel-irc-erc.el ends here
