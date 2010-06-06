;;; rudel-infinote-user.el --- Infinote user class
;;
;; Copyright (C) 2009, 2010 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, infinote, user
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


;;; History:
;;
;; 0.1 - Initial version


;;; Code:
;;

(require 'rudel) ;; TODO organize this

(require 'rudel-icons) ;; TODO user interface stuff


;;; Class rudel-infinote-user
;;

(defclass rudel-infinote-user (rudel-user)
  ((id     :initarg  :id
	   :type     integer
	   :accessor rudel-id
	   :documentation
	   "")
   (status :initarg  :status
	   :type     symbol
	   ;;:accessor rudel-status
	   :documentation
	   "Status of the user. The following values are legal:
'active:
'inactive:
'unavailable: The host that joined the user to the session
unsubscribed from the session.")) ;; TODO in infinote the status has to be interpreted per session, not globally
  "Objects of this class represent participants of infinote
sessions.")

(defmethod rudel-display-string ((this rudel-infinote-user) ;; TODO move to -display.el
				 &optional use-images)
  ""
  (with-slots ((name :object-name) status) this
    (concat (call-next-method)
	    (case status
	      ('active
	       (propertize
		"a"
		'display   rudel-icon-connected ;; TODO typing?
		'help-echo (format "%s is connected"
				   name)))

	      ('inactive
	       (propertize
		"i"
		'display   rudel-icon-connected
		'help-echo (format "%s is connected, but inactive"
				   name))

	      ('unavailable
	       (propertize
		"-"
		'display   rudel-icon-disconnected
		'help-ehco (format "%s is not connected"
				   name)))

	      (t
	       "?")))))
  )

(provide 'rudel-infinote-user)
;;; rudel-infinote-user.el ends here
