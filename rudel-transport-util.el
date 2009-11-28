;;; rudel-transport-util.el --- Utility functions for Rudel transport functionality
;;
;; Copyright (C) 2009 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, backend, transport, utility, miscellaneous
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
;; This file contains utility functions for implementing Rudel
;; transport functionality. In particular, several transport filter
;; classes for common task are available:
;;
;; + `rudel-transport-filter'
;;   + `rudel-assembling-transport-filter'
;;   + `rudel-parsing-transport-filter'


;;; History:
;;
;; 0.1 - Initial revision


;;; Code:
;;

(require 'rudel-transport)


;;; Class rudel-transport-filter
;;

(defclass rudel-transport-filter (rudel-transport)
  ((transport :initarg  :transport
	      :type     rudel-transport
	      :documentation
	      "Transport object that performs the send and
receive operations this filter builds upon.")  ;; TODO should be read only
   (filter    :initarg  :filter
	      :type     (or null function)
	      :initform nil
	      :accessor rudel-filter
	      :documentation
	      "Function that is called when data is received.")
   (sentinel  :initarg  :sentinel
	      :type     (or null function)
	      :initform nil
	      :accessor rudel-sentinel
	      :documentation
	      "Function that is called when the status of the
transport changes."))
  "This class is a base class for transport filters that
transform a bidirectional data stream as it passes through them."
  :abstract t)

(defmethod rudel-set-filter ((this rudel-transport-filter) filter)
  "Install FILTER as dispatcher for messages received by THIS."
  (oset this :filter filter))

(defmethod rudel-set-sentinel ((this rudel-transport-filter) sentinel)
  "Install SENTINEL as handler for state changes that occur in THIS."
  (oset this :sentinel sentinel))



;;; Class rudel-assembling-transport-filter
;;

(defclass rudel-assembling-transport-filter (rudel-transport-filter)
  ((buffer            :initarg  :buffer
		      :type     (or null string)
		      :initform nil
		      :documentation
		      "Stores message fragments until complete
messages can be assembled.")
   (assembly-function :initarg  :assembly-function
		      :type     function
		      :initform 'rudel-assemble-lines
		      :accessor rudel-assembly-function
		      :documentation
		      "Function that is called to assemble
message fragments into complete messages."))
  "Objects of this class assemble received message fragments into
complete messages by calling an assembly function.")

(defmethod initialize-instance ((this rudel-assembling-transport-filter)
				slots)
  "Initialize THIS using SLOTS and install suitable handlers."
  ;; Initialize slots.
  (when (next-method-p)
    (call-next-method))

  ;; Install a handler for received data that assembles messages and
  ;; passes them to the user-provided handler.
  (with-slots (transport) this
    (lexical-let ((this1 this))
      (rudel-set-filter
       transport
       (lambda (data)
	 ;; Assemble complete fragments from stored fragments and
	 ;; possibly incomplete messages in DATA.
	 (with-slots (buffer assembly-function) this1
	   (rudel-assemble-fragments data buffer assembly-function))

	 ;; Process all complete messages.
	 (with-slots (filter) this1
	   (when filter
	     (mapc filter data)))))))
  )

(defmethod rudel-send ((this rudel-assembling-transport-filter) data)
  "Send DATA using the transport of THIS."
  (with-slots (transport) this
    (rudel-send transport data)))



;;; Class rudel-parsing-transport-filter
;;

;;
(defclass rudel-parsing-transport-filter (rudel-transport-filter)
  ((parse-function    :initarg  :parse-function
		      :type     function
		      :initform 'identity
		      :documentation
		      "Function that is called on each received
piece of data to transform it into a suitable representation.")
   (generate-function :initarg  :generate-function
		      :type     function
		      :initform 'identity
		      :documentation
		      "Function that is called on each outgoing
object to transform it into a string representation."))
  "Objects of this class convert sent and received data between
string representations and structured representations by calling
a pair of one parse and one generate function.")

(defmethod initialize-instance ((this rudel-parsing-transport-filter) slots)
  "Initialize THIS using SLOTS and install suitable handlers."
  ;; Initialize slots.
  (when (next-method-p)
    (call-next-method))

  ;; Install a handler for received data that parses messages into
  ;; structured representations and passes those to the user-provided
  ;; handler.
  (with-slots (transport) this
    (lexical-let ((this1 this))
      (rudel-set-filter
       transport
       (lambda (data)
	 ;; Parse and process all complete messages.
	 (with-slots (parse-function filter) this1
	   (when filter
	     (rudel-loop-fragments data message-data
	       (let ((message (funcall parse-function message-data)))
		 (funcall filter message)))))))))
  )

(defmethod rudel-send ((this rudel-parsing-transport-filter) message)
  "Apply generate function to MESSAGE, pass result to transport of THIS."
  (with-slots (transport generate-function) this
    (rudel-send transport (funcall generate-function message))))

(provide 'rudel-transport-util)
;;; rudel-transport-util.el ends here
