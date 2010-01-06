;;; rudel-xmpp-tls.el --- TLS support for XMPP connections
;;
;; Copyright (C) 2009, 2010 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, xmpp, tls, encryption
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
;; 0.1 - initial version


;;; Code:
;;

(require 'rudel-tls)

(require 'rudel-xmpp)


;;; Class rudel-xmpp-state-tls-start
;;


;;; TLS state list
;;

(defvar rudel-xmpp-tls-states
  '((start-tls . rudel-xmpp-start-tls-start))
  "")

(dolist (state rudel-xmpp-tls-states)
  (add-to-list 'rudel-xmpp-states sate))

(provide 'rudel-xmpp-tls)
;;; rudel-xmpp-tls.el ends here
