;;; rudel-tcp.el --- socket transport backend for Rudel
;;
;; Copyright (C) 2009 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, socket, transport, backend
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
;; Socket transport backend for Rudel.


;;; History:
;;
;; 0.2 - Use underlying socket directly
;;
;; 0.1 - Initial version


;;; Code:
;;

(require 'cl) ;; for `every'

(require 'rudel-backend)
(require 'rudel-transport)


;;; Constants
;;

(defconst rudel-tcp-version '(0 2)
  "Version of the TCP transport for Rudel.")


;;; Class rudel-socket-transport
;;

(defclass rudel-socket-transport (rudel-transport)
  ((socket :initarg :socket
	   :type    process
	   :documentation
	   "The socket represented by this transport object."))
  "Objects of this class use sockets to transport data.")

(defmethod rudel-filter ((this rudel-socket-transport))
  "Return filter function of THIS."
  (with-slots (socket) this
    (process-filter socket)))

(defmethod rudel-set-filter ((this rudel-socket-transport) filter)
  "Install FILTER as dispatcher for messages received by THIS."
  (with-slots (socket) this
    (lexical-let ((filter1 filter))
      (set-process-filter socket (lambda (process data)
				   (funcall filter1 data))))))

(defmethod rudel-sentinel ((this rudel-socket-transport))
  "Return sentinel function of THIS."
  (with-slots (socket) this
    (process-sentinel socket)))

(defmethod rudel-set-sentinel ((this rudel-socket-transport) sentinel)
  "Install SENTINEL as dispatcher for out-of-band events of THIS."
  (with-slots (socket) this
    (lexical-let ((sentinel1 sentinel))
      (set-process-sentinel socket (lambda (process message)
				     (funcall sentinel1 message))))))

(defmethod rudel-send ((this rudel-socket-transport) data)
  "Send DATA through THIS."
  (with-slots (socket) this
    (process-send-string socket data)))

(defmethod rudel-close ((this rudel-socket-transport))
  "Close THIS."
  (with-slots (socket) this
    (delete-process socket)))

(defmethod rudel-start ((this rudel-socket-transport))
  "Start THIS after it has been suspended."
  (with-slots (socket) this
    (continue-process socket)))


;;; Class rudel-tcp-backend
;;

;;;###autoload
(defclass rudel-tcp-backend (rudel-transport-backend)
  ((capabilities :initform (listen connect)))
  "TCP transport backend.
The transport backend is a factory for TCP transport objects.")

(defmethod initialize-instance ((this rudel-tcp-backend) slots)
  "Initialize slots and set version of THIS."
  (when (next-method-p)
    (call-next-method))

  (oset this :version rudel-tcp-version))

(defmethod rudel-make-connection
  ((this rudel-tcp-backend) info
   info-callback &optional progress-callback)
  "Connect to a TCP server using the information in INFO.
INFO has to be a property list containing the keys :host
and :port."
  ;; Ensure that INFO contains all necessary information.
  (unless (every (lambda (keyword) (member keyword info))
		 '(:host :port))
    (setq info (funcall info-callback this info)))

  ;; Extract information from INFO and create the socket.
  (let* ((host   (plist-get info :host))
	 (port   (plist-get info :port))
	 ;; Create the network process
	 (socket (make-network-process
		  :name     (format "TCP to %s" host)
		  :host     host
		  :service  port
		  :stop     t)))
    (rudel-socket-transport
     (format "to %s:%s" host port)
     :socket socket))
  )

(defmethod rudel-wait-for-connections ((this rudel-tcp-backend)
				       info dispatch-callback)
  "Create TCP server according to INFO, dispatch to DISPATCH-CALLBACK."
  ;; Extract information from INFO and create the socket.
  (let* ((address (plist-get info :address))
	 (port    (plist-get info :port)))
    ;; Create the network process
    (lexical-let ((dispatch-callback1 dispatch-callback))
      (apply
       #'make-network-process
       :name    (format "TCP on %s" port)
       :service port
       :server  t
       :log
       (lambda (server connection message)
	 (rudel-tcp-handle-connect connection dispatch-callback1))
       (when address
	 (list :host address)))))
  )

(defun rudel-tcp-handle-connect (connection dispatch-callback)
  "Handle incoming connection CONNECTION and call DISPATCH-CALLBACK."
  ;; Wrap CONNECTION in a transport object. Pass the constructed
  ;; object to DISPATCH-CALLBACK.
  (let ((transport (rudel-socket-transport
		    (format
		     "TCP from %s"
		     (format-network-address
		      (process-contact connection :remote)))
		    :socket connection)))
    (funcall dispatch-callback transport))
  )


;;; Autoloading
;;

;;;###autoload
(rudel-add-backend (rudel-backend-get-factory 'transport)
		   'tcp 'rudel-tcp-backend)

(provide 'rudel-tcp)
;;; rudel-tcp.el ends here
