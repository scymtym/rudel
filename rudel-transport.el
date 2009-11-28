;;; rudel-transport.el --- Rudel transport interface and backend
;;
;; Copyright (C) 2009 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, backend, transport
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
;; This file contains the interface definition for Rudel transport
;; objects and Rudel transport backends.


;;; History:
;;
;; 0.2 - Socket-like interface
;;
;; 0.1 - Initial version


;;; Code:
;;

(require 'eieio)

(require 'rudel-errors) ;; for `rudel-error'
(require 'rudel-backend)


;;; Error conditions
;;

;; rudel-malformed-message

(intern "rudel-malformed-message")

(put 'rudel-malformed-message 'error-conditions
     '(error
       rudel-error rudel-malformed-message))

(put 'rudel-malformed-message 'error-message
     "Received malformed message")


;;; Class rudel-transport
;;

(defclass rudel-transport ()
  ()
  "Interface for transport objects.")

(defgeneric rudel-set-filter ((this rudel-transport) handler)
  "Install HANDLER as dispatcher for messages received by THIS.")

(defgeneric rudel-set-sentinel ((this rudel-transport) handler)
  "Install HANDLER as dispatcher for state changes of THIS.")

(defgeneric rudel-send ((this rudel-transport) data)
  "Send DATA through THIS transport object.")

(defgeneric rudel-close ((this rudel-transport))
  "Close THIS.")

;; TODO we could get rid of this if we required implementations to
;; queue messages until a handler is installed
(defgeneric rudel-start ((this rudel-transport))
  "Start THIS.")


;;; Class rudel-transport-backend
;;

(defclass rudel-transport-backend (rudel-backend)
  ()
  "Interface implemented by transport backends."
  :abstract t)

(defgeneric rudel-make-connection ((this rudel-transport-backend) info
				   info-callback
				   &optional progress-callback)
  "Create a transport object according to INFO.

INFO-CALLBACK is called when the information provided in INFO is
not sufficient for establishing the requested
connection. INFO-CALLBACK has to accept the backend object and a
property list containing the current connection information and
return a property list containing the augmented connection
information.

When non-nil, PROGRESS-CALLBACK has to accept to arguments: a
state string and a float in the range [0, 1] indicating the
progress. PROGRESS-CALLBACK may be called repeatedly while the
connection is established.

The returned transport object has to be in a stopped state in the
sense that it does not attempt to dispatch any data to the filter
function before `rudel-start' has been called.")

(defgeneric rudel-wait-for-connections ((this rudel-transport-backend)
					info dispatch-callback)
  "Create transport objects according to INFO for incoming connections.

Call DISPATCH-CALLBACK each time a connection is
established. DISPATCH-CALLBACK has to accept the created
`rudel-transport' object as its only argument.")

(provide 'rudel-transport)
;;; rudel-transport.el ends here
