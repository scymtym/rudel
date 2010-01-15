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
		    "")  ;; TODO can change later; handle in
			 ;; transport?
   (self-name       :initarg  :self-name
		    :type     (or null string)
		    :initform nil
		    :documentation
		    "")  ;; TODO can change later; handle in
			 ;; transport?
   (ctcp-type       :initarg  :ctcp-type
		    :type     string
		    :documentation
		    "CTCP message type used when sending and
receiving messages.")
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

  ;; Store nick name.
  (with-slots (buffer self-name) this
    (with-current-buffer buffer
      (setq self-name (erc-current-nick))))
  )

(defmethod initialize-instance :after ((this rudel-irc-erc-base) slots)
  "TODO"
  ;; Install the handlers.
  (with-slots (buffer ctcp-type message-handler part-handler) this
    (let ((hook-symbol (intern (format "erc-ctcp-query-%s-hook"
				       ctcp-type)))) ;; TODO make a function for this
      (with-current-buffer buffer
	(add-hook hook-symbol message-handler)
	;;(add-hook 'erc-server-PART-functions part-handler)
	)))
  )

(defmethod rudel-send ((this rudel-irc-erc-base) &rest args)
  "Send DATA through THIS."
  (with-slots (buffer peer-name ctcp-type) this
    (with-current-buffer buffer
      (apply #'erc-cmd-CTCP peer-name ctcp-type args))))

(defmethod rudel-close ((this rudel-irc-erc-base))
  ""
  ;; Remove handlers.
  (with-slots (buffer ctcp-type message-handler part-handler) this
    (let ((hook-symbol (intern (format "erc-ctcp-query-%s-hook"
				       ctcp-type)))) ;; TODO make a function for this
    (with-current-buffer buffer
      (remove-hook hook-symbol message-handler)
      ;;(remove-hook 'erc-server-PART-functions part-handler)
      )))
  )


;;; Utility functions
;;

(defun rudel-irc-erc-parse-response (response)
  ""
  (list
   (erc-response.command-args response)
   (nth 0 (erc-parse-user (erc-response.sender response)))
   (erc-response.contents response)))

(defmacro rudel-irc-erc-make-handler (vars-peer-self &rest body)
  ""
  (declare (indent 1))
  (destructuring-bind (data-var sender-var peer self type) vars-peer-self
    (let ((peer1  (make-symbol "peer"))
	  (self1  (make-symbol "self"))
	  (start1 (make-symbol "start")))
      `(lexical-let ((,peer1 ,peer)
		     (,self1 ,self)
		     (,start1 (+ (length ,type) 1)))
	 #'(lambda (proc from-name login host to-name msg)
	     ;; Parse the response and check whether we want to handle
	     ;; it.
	     (when (and (or (not ,peer1)
			    (string= from-name ,peer1))
			(or (not ,self1)
			    (string= to-name ,self1)))
	       (let ((,sender-var from-name)
		     (,data-var   (substring msg ,start1)))
		 ;; Run the provided body.
		 ,@body)
	       ;; Prevent other handlers from running when we could
	       ;; handle the response.
	       t)))
    ))
  )

(provide 'rudel-irc-erc-util)
;;; rudel-irc-erc-util.el ends here
