;;; rudel-infinote.el --- Infinote backend for Rudel
;;
;; Copyright (C) 2009, 2010 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, gobby, infinote, protocol
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
;; This file contains a Rudel protocol backend, which implements the
;; infinote protocol (used by the Gobby family of collaborative
;; editors starting with version 0.5).



;;; History:
;;
;; 0.1 - Initial version


;;; Code:
;;

(require 'rudel-backend)
(require 'rudel-protocol)

(require 'rudel-interactive) ;; for read functions


;;; Constants
;;

(defconst rudel-infinote-version '(0 1)
  "Version of the infinote backend for Rudel.")


;;; Class rudel-infinote-backend
;;

;;;###autoload
(defclass rudel-infinote-backend (rudel-protocol-backend)
  ((capabilities :initform '(join
			     change-color
			     chat
			     track-subscriptions track-cursors
			     track-selections track-viewports)))
  "")

(defmethod initialize-instance ((this rudel-infinote-backend) slots)
  ""
  (when (next-method-p)
    (call-next-method))

  (oset this :version rudel-infinote-version))

(defmethod rudel-ask-connect-info ((this rudel-infinote-backend)
				   &optional info)
  ""
  ;; Read desired username and color
  (let ((username (or (plist-get info :username)
		      (rudel-read-user-name)))
	(color    (or (plist-get info :color)
		      (rudel-read-user-color))))
    (append
     (list :username username
	   :color    color)
     info))
  )

(defmethod rudel-connect ((this rudel-infinote-backend) transport
			  info info-callback
			  &optional progress-callback)
  "Connect to an infinote server using the information INFO.
Return the connection object."
  ;; Before we start, load the client functionality.
  (require 'rudel-infinote-client)

  ;; Create the connection object
  (let* ((session    (plist-get info :session))
	 (host       (plist-get info :host)) ;; Just as name
	 (connection (rudel-infinote-client-connection
		      host
		      :session   session
		      :transport transport)))

    ;; Start the transport and wait until the basic session setup is
    ;; complete.
    (rudel-start transport)

    (rudel-state-wait transport
		      '(established) '(we-finalize they-finalize)
		      progress-callback)

    ;; The connection is now ready for action; Return it.
    connection)
  )

(defmethod rudel-make-document ((this rudel-infinote-backend)
				name encoding session)
  ""
  (rudel-infinote-text-document name
				:session session))


;;; Autoloading
;;

;;;###autoload
(rudel-add-backend (rudel-backend-get-factory 'protocol)
		   'infinote 'rudel-infinote-backend)

;;;###autoload
(eval-after-load 'rudel-zeroconf
  '(rudel-zeroconf-register-service "_infinote._tcp"
				    'xmpp 'infinote))

(provide 'rudel-infinote)
;;; rudel-infinote.el ends here
