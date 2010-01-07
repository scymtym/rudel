;;; rudel-irc-erc-util.el --- Utility functions used by the ERC IRC backend for Rudel
;;
;; Copyright (C) 2010 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, irc, erc, utility functions
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

(eval-when-compile
  (require 'cl))

(require 'erc)
(require 'erc-backend)

(require 'eieio)


;;; Class rudel-irc-erc-base
;;

(defclass rudel-irc-erc-base ()
  ((buffer          :initarg  :buffer
		    :type     buffer
		    :documentation
		    "")
   (peer-name       :initarg  :peer-name
		    :type     (or null string)
		    :initform nil
		    :documentation
		    "")
   (self-name       :initarg  :self-name
		    :type     (or null string)
		    :initform nil
		    :documentation
		    "")
   (message-handler :initarg  :message-handler
		    :type     function
		    :documentation
		    "")
   (part-handler    :initarg  :part-handler
		    :type     function
		    :documentation
		    ""))
  ""
  :abstract t)

(defmethod initialize-instance ((this rudel-irc-erc-base) slots)
  "Initialize slots of THIS and install handlers."
  ;; Initialize slots.
  (when (next-method-p)
    (call-next-method))

  (with-slots (buffer peer-name self-name message-handler part-handler) this
    ;; Construct handlers for PRIVMSG and PART messages. Store the
    ;; handler function for later removal.
    (lexical-let ((this1 this))
      (setq message-handler
	    (rudel-irc-erc-make-handler (data peer-name self-name)
	      (rudel-handle this1 data))
	    part-handler
	    (rudel-irc-erc-make-handler (data peer-name self-name)
	      (rudel-handle this1 data))))

    ;; Install the handlers.
    (with-current-buffer buffer
      (add-hook 'erc-server-PRIVMSG-functions message-handler)
      (add-hook 'erc-server-PART-functions    part-handler)))
  )

(defmethod rudel-send ((this rudel-irc-erc-base) data)
  "Send DATA through THIS."
  (with-slots (buffer peer-name) this
    (with-current-buffer buffer
      (erc-server-send
       (format "PRIVMSG %s :%s\n"
	       peer-name (base64-encode-string data))))))

(defmethod rudel-close ((this rudel-irc-erc-base))
  ""
  ;; Remove handlers.
  (with-slots (buffer message-handler part-handler) this
    (with-current-buffer buffer
      (remove-hook 'erc-server-PRIVMSG-functions message-handler)
      (remove-hook 'erc-server-PART-functions    part-handler))))


;;; Utility functions
;;

(defun rudel-irc-erc-parse-response (response)
  ""
  (list
   (erc-response.command-args response)
   (nth 0 (erc-parse-user (erc-response.sender response)))
   (erc-response.contents response)))

(defmacro rudel-irc-erc-make-handler (var-peer-self &rest body)
  ""
  (declare (indent 1))
  (destructuring-bind (data-var peer self) var-peer-self
    (let ((peer1 (make-symbol "peer"))
	  (self1 (make-symbol "self")))
      `(lexical-let ((,peer1 ,peer)
		     (,self1 ,self))
	 #'(lambda (server ,data-var)
	     ;; Parse the response and check whether we want to handle
	     ;; it.
	     (destructuring-bind
		 (args sender data) (rudel-irc-erc-parse-response ,data-var)
	       (when (and (or (not ,peer1)
			      (string= sender ,peer1))
			  (or (not ,self1)
			      (string= (car args) ,self1)))
		 ;; Run the provided body.
		 (progn
		   ,@body)
		 ;; Prevent other handlers from running when we could
		 ;; handle the response.
		 t))))
      ))
  )

(provide 'rudel-irc-erc-util)
;;; rudel-irc-erc-util.el ends here
