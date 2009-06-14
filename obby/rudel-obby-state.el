;;; rudel-obby-state.el --- Base class for states used in the obby backend
;;
;; Copyright (C) 2009 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;; Keywords: Rudel, obby, state machine
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
;; This file contains a base class for finite state machine states
;; used in the obby backend.


;;; History:
;;
;; 0.1 - Initial revision


;;; Code:
;;

(require 'eieio)

(require 'rudel-util)
(require 'rudel-state-machine)

(require 'rudel-obby-util)


;;; Class rudel-obby-state
;;

(defclass rudel-obby-state (rudel-state)
  ((connection :initarg :connection
	       :type    rudel-obby-socket-owner
	       :documentation
	       "Connection object that uses the state."))
  "Base class for state classes used in the obby backend."
  :abstract t)

(defmethod rudel-enter ((this rudel-obby-state))
  "Default behavior is doing nothing when entering a state."
  )

(defmethod rudel-leave ((this rudel-obby-state))
  "Default behavior is doing nothing when leaving a state."
  )

(defmethod rudel-accept ((this rudel-obby-state) message)
  "Dispatch to appropriate handler based on MESSAGE.
Display a warning if no such handler is found."
  ;; Try to dispatch to the correct message handler. If there is none,
  ;; warn.
  (let ((name      (car message))
	(arguments (cdr message)))
    (condition-case error
	;; Try to dispatch
	(rudel-dispatch this "rudel-obby/" name arguments)
      ;; Warn if we failed to locate or execute the method. Return nil
      ;; in this case, so we remain in the current state.
      (rudel-dispatch-error
       (progn
	 (warn "%s: no method (%s: %s): `%s:%s'; arguments: %s"
	       (object-print this) (car error) (cdr error)
	       "rudel-obby/" name arguments)
	 nil))))
  )

(defmethod rudel-send ((this rudel-obby-state) &rest args)
  "Send ARGS through the connection associated with THIS."
  (with-slots (connection) this
      (apply #'rudel-send connection args)))


;;; Class rudel-obby-client-connection-state
;;

(defclass rudel-obby-client-connection-state (rudel-obby-state)
  ()
  "Base class for state classes used by obby client connections."
  :abstract t)

(defmethod rudel-obby/net6_ping ((this rudel-obby-client-connection-state))
  "Handle net6 'ping' message."
  (rudel-send this "net6_pong"))

(provide 'rudel-obby-state)
;;; rudel-obby-state.el ends here
