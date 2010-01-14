;;; rudel-xmpp.el --- XMPP transport backend for Rudel
;;
;; Copyright (C) 2009, 2010 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, xmpp, transport, backend
;; X-RCS: $Id:$
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses>.


;;; Commentary:
;;


;;; History:
;;
;; 0.1 - initial version


;;; Code:
;;

(require 'rudel-backend)
(require 'rudel-transport)
(require 'rudel-transport-util)

(require 'rudel-util)

(require 'rudel-xmpp-state)
(require 'rudel-xmpp-util)


;;; Constants
;;

(defconst rudel-xmpp-transport-version '(0 1)
  "Version of the XMPP transport backend for Rudel.")

(defconst rudel-xmpp-protocol-version '(1 0)
  "Version of the XMPP protocol supported by this implementation.")


;;; Class rudel-xmpp-backend
;;

;;;###autoload
(defclass rudel-xmpp-backend (rudel-transport-backend)
  ()
  "")

(defmethod initialize-instance ((this rudel-xmpp-backend) &rest slots)
  ""
  (when (next-method-p)
    (call-next-method))

  (oset this :version rudel-xmpp-transport-version))

(defmethod rudel-ask-connect-info ((this rudel-xmpp-backend)
				   &optional info)
  "Augment INFO by read a hostname and a port number."
  ;; Read server host and port.
  (let ((host (or (plist-get info :host)
		  (read-string "Server: ")))
	(port (or (plist-get info :port)
		  (read-number "Port: "))))
    (append (list :host host
		  :port port)
	    info)))

(defmethod rudel-make-connection ((this rudel-xmpp-backend)
				  info info-callback
				  &optional progress-callback)
  "Connect to an XMPP server using the information in INFO.
INFO has to be a property list containing at least the keys :host
and :port."
  (let* ((host          (plist-get info :host))
	 (port          (plist-get info :port))
	 (tcp-transport (rudel-make-connection
			 (cdr (rudel-backend-get 'transport 'tcp))
			 info info-callback progress-callback))
	 (stack         (rudel-xmpp-make-transport-filter-stack
			 tcp-transport))
	 (transport     (rudel-xmpp-transport
			 host
			 :transport stack)))

    ;; Now start receiving and wait until the connection has been
    ;; established.
    (rudel-start tcp-transport)
    (rudel-state-wait transport
		      '(established) '(we-finalize they-finalize)
		      progress-callback)

    ;;
    transport))


;;; Class rudel-xmpp-state-new
;;

(defclass rudel-xmpp-state-new (rudel-xmpp-state)
  ()
  "Initial state of new XMPP connections.")

(defmethod rudel-enter ((this rudel-xmpp-state-new))
  ""
  '(negotiate-stream sasl-start))


;;; Class rudel-xmpp-state-negotiate-stream
;;

(defclass rudel-xmpp-state-negotiate-stream (rudel-xmpp-state)
  ((success-state :initarg :success-state
		  :type    symbol
		  :documentation
		  "State to switch to in case of successful
negotiation."))
  "Stream negotiation state.")

(defmethod rudel-enter ((this rudel-xmpp-state-negotiate-stream)
			success-state) ;; host)
  ""
  ;; Store the name of the successor state in case of successful
  ;; stream negotiation for later.
  (oset this :success-state success-state)

  ;; The first message we receive will be an incomplete <stream:stream
  ;; ... > XML tree.
  (with-slots (transport) this
    (rudel-set-assembly-function transport #'rudel-xmpp-assemble-stream)
    (rudel-set-generate-function transport #'identity))

  ;; We cannot generate this message by serializing an XML infoset
  ;; since the document is incomplete. We construct it as a string
  ;; instead.
  (rudel-send this
	      (format "<?xml version=\"1.0\" encoding=\"%s\"?><stream:stream xmlns:stream=\"http://etherx.jabber.org/streams\" xmlns=\"jabber:client\" version=\"%s\" to=\"%s\" id=\"%s\">"
		      "UTF-8"
		      (mapconcat #'identity
				 (mapcar #'number-to-string
					 rudel-xmpp-protocol-version)
				 ".")
		      "jabber.org"
		      "scymtym@jabber.org"))
  nil)

(defmethod rudel-leave ((this rudel-xmpp-state-negotiate-stream))
  ""
  ;; TODO explain
  (with-slots (transport) this
    (rudel-set-assembly-function transport #'rudel-xml-assemble-tags)
    (rudel-set-generate-function transport #'xml->string)))

(defmethod rudel-accept ((this rudel-xmpp-state-negotiate-stream) xml)
  ""
  (cond
   ;; Stream negotiation error.
   ;;((string= (xml-tag-name xml) "stream:stream")
   ;;nil) ;; TODO send error

   ;; Success
   (t
    (with-slots (success-state) this
      (let ((features (xml-node-children
		       (car (xml-get-children xml 'stream:features)))))
	(list success-state features)))))
  )


;;; Class rudel-xmpp-state-authenticated
;;

;; TODO similar to new state; could use common base class
(defclass rudel-xmpp-state-authenticated (rudel-xmpp-state)
  ()
  "")

(defmethod rudel-enter ((this rudel-xmpp-state-authenticated))
  ""
  (list 'negotiate-stream 'established))


;;; Class rudel-xmpp-state-authentication-failed
;;

(defclass rudel-xmpp-state-authentication-failed (rudel-xmpp-state)
  ()
  "")


;;; Class rudel-xmpp-state-established
;;

(defclass rudel-xmpp-state-established (rudel-xmpp-state)
  ()
  "")

(defmethod rudel-enter ((this rudel-xmpp-state-established) features)
  ""
  nil)

(defmethod rudel-accept ((this rudel-xmpp-state-established) xml)
  ""
  (with-slots (handler) this
    (when handler
      (funcall handler xml)))
  nil)


;;; Class rudel-xmpp-state-we-finalize
;;

(defclass rudel-xmpp-state-we-finalize (rudel-xmpp-state)
  ()
  "")

(defmethod rudel-enter ((this rudel-xmpp-state-we-finalize))
  ""
  (rudel-send this "</stream:stream>")

  ;; TODO (rudel-close connection))?
  'disconnected)


;;; Class rudel-xmpp-state-they-finalize
;;

(defclass rudel-xmpp-state-they-finalize (rudel-xmpp-state)
  ()
  "")

(defmethod rudel-enter ((this rudel-xmpp-state-they-finalize))
  ""
  (rudel-close this)
  nil)


;;; Class rudel-xmpp-state-disconnected
;;

(defclass rudel-xmpp-state-disconnected (rudel-xmpp-state)
  ()
  "")


;;; XMPP state list
;;

(defvar rudel-xmpp-states
  '(;; Basic XMPP states
    (new                   . rudel-xmpp-state-new)
    (negotiate-stream      . rudel-xmpp-state-negotiate-stream)
    (authenticated         . rudel-xmpp-state-authenticated)
    (authentication-failed . rudel-xmpp-state-authentication-failed)
    (established           . rudel-xmpp-state-established)
    (we-finalize           . rudel-xmpp-state-we-finalize)
    (they-finalize         . rudel-xmpp-state-they-finalize)
    (disconnected          . rudel-xmpp-state-disconnected))
  "")


;;; Class rudel-xmpp-transport
;;

(defclass rudel-xmpp-transport (rudel-state-machine
				rudel-transport-filter)
  ()
  "")

(defmethod initialize-instance ((this rudel-xmpp-transport)
				&rest slots)
  "Initialize THIS and register states."
  ;; Initialize slots of THIS.
  (when (next-method-p)
    (call-next-method))

  ;; Register states.
  (rudel-register-states this rudel-xmpp-states)

  ;; Install a handler that passes received data to `rudel-accept'.
  (with-slots (transport) this
    (lexical-let ((this1 this))
      (rudel-set-filter
       transport
       (lambda (data)
	 (rudel-accept this1 data)))))
  )

(defmethod rudel-register-state ((this rudel-xmpp-transport)
				 symbol state)
  "Associate THIS to STATE before registering STATE."
  ;; Associate THIS connection to STATE.
  (oset state :transport this)

  ;; Register the modified STATE.
  (call-next-method)
  )

(defmethod rudel-disconnect ((this rudel-xmpp-transport))
  ""
  (rudel-switch this 'we-finalize)

  (rudel-state-wait this '(disconnected) nil)

  (when (next-method-p)
    (call-next-method)) ;; TODO does this call rudel-close again?
  )


;;; Autoloading
;;

;;;###autoload
(rudel-add-backend (rudel-backend-get-factory 'transport)
		   'xmpp 'rudel-xmpp-backend)

(provide 'rudel-xmpp)
;;; rudel-xmpp.el ends here
