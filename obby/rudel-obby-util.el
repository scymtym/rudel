;;; rudel-obby-util.el --- Miscellaneous functions for the Rudel obby backend
;;
;; Copyright (C) 2008, 2009 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: Rudel, obby, backend, miscellaneous
;; X-RCS: $Id:$
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING. If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA, or see <http://www.gnu.org/licenses>.


;;; Commentary:
;;
;;


;;; History:
;;
;; 1.0 - Initial revision.


;;; Code:
;;

(require 'eieio)

(require 'rudel-util)


;;; Class rudel-obby-socket-owner
;;

(defclass rudel-obby-socket-owner (rudel-socket-owner)
  ((buffer :initarg  :buffer
	   :type     (or null string)
	   :initform nil
	   :documentation
	   "Stores message fragments until complete messages can
be assembled."))
  "This class adds functions for sending and receiving obby
messages to the base class rudel-socket-owner.")

(defmethod rudel-send ((this rudel-obby-socket-owner) 
		       name &rest arguments)
  "Send obby message NAME with arguments ARGUMENTS through the socket associated to THIS."
  (with-slots (socket) this
    (rudel-obby-send socket name arguments)))

(defmethod rudel-receive ((this rudel-obby-socket-owner) data)
  "Reassemble lines in DATA received on the socket associated with THIS and call message handler."
  ;; Assemble fragmented lines.
  (with-slots (buffer) this
    (rudel-assemble-line-fragments data buffer))

  ;; Process all available lines.
  (rudel-loop-lines data line
    ;; `rudel-message' has to dispatch message to an appropriate
    ;; handler.
    (let ((message (rudel-obby-parse-message line)))
      (rudel-message this message)))
  )

(defmethod rudel-message ((this rudel-obby-socket-owner) message)
  "This method has to be implemented in derived classes."
  (error "Needs to be implemented in derived classes"))

(provide 'rudel-obby-util)
;;; rudel-obby-util.el ends here
