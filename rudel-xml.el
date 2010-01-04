;;; rudel-xml.el --- XML processing functions used by Rudel
;;
;; Copyright (C) 2009, 2010 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, xml
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
;; Conversion functions:
;; + `xml->string'
;; + `string->xml'
;;
;; XML Macros:
;; + `with-tag-attrs'
;; + `do-tag-children'
;;
;; Stream parsing functions:
;; + `rudel-xml-toplevel-tag-positions'
;; + `rudel-xml-toplevel-tags'


;;; History:
;;
;; 0.1 - Initial version


;;; Code:
;;

(require 'xml)


;;; Miscellaneous functions
;;

(defun xml->string (xml &optional pretty-print)
  "Convert infoset XML to string representation.
PRETTY-PRINT is currently ignored."
  (with-temp-buffer
    (xml-print (list xml))
    (buffer-string)))

(defun string->xml (string)
  "Convert STRING to XML infoset."
  (with-temp-buffer
    (insert string)
    (car (xml-parse-region (point-min) (point-max)))))


;;; Additional XML macros
;;

(defmacro with-tag-attrs (attrs tag &rest body)
  "Execute BODY with bindings of attribute values of TAG according to forms in ATTRS.
ATTRS is structured as follows:
ATTRS   ::= (BINDING*)
BINDING ::= VAR | (VAR ATTR) | (VAR ATTR TYPE)
VAR is a symbol. ATTR is a symbol whose symbol-name is used as
tag name. TYPE can be 'number."
  (declare (indent 2)
	   (debug (listp form &rest form)))
  (let* ((tag-var (make-symbol "tag-var"))
	 (bindings
	  (mapcar
	   (lambda (attr)
	     (cond

	      ;; Simple form
	      ((symbolp attr)
	       `(,attr (xml-get-attribute ,tag-var (quote ,attr))))
	      ;; Variable name, attribute name and type
	      ((= (length attr) 3)
	       (let* ((attr-var     (nth 0 attr))
		      (name         (nth 1 attr))
		      (type         (nth 2 attr))
		      (value-expr   `(xml-get-attribute ,tag-var (quote ,name)))
		      (value-string (make-symbol "value-string")))
		 `(,attr-var (let ((,value-string ,value-expr))
			       ,(cond
				 ;; Convert to number
				 ((eq type 'number)
				  `(when ,value-string
				     (string-to-number ,value-string)))

				 ;; String; no conversion
				 ((eq type 'string)
				  value-string)

				 ;; For other types, signal an error.
				 (t
				  (error "Invalid type: %s" type)))))))
	      ;; Variable name and attribute name
	      ((= (length attr) 2)
	       (let* ((attr-var (nth 0 attr))
		      (name     (nth 1 attr))
		      (value    `(xml-get-attribute ,tag-var (quote ,name))))
		 `(,attr-var ,value)))

	      ;; Invalid form
	      (t (error "Invalid tag clause: %s" attr)))) ;; TODO define a proper condition or use signal?
	   attrs)))
    `(let ((,tag-var ,tag))
       (let (,@bindings)
	 (progn
	   ,@body))))
  )

(defmacro do-tag-children (var-and-tag &rest body)
  ""
  (declare (indent 1)
	   (debug ((symbolp form) &rest form)))
  (let ((var      (nth 0 var-and-tag))
	(tag      (nth 1 var-and-tag))
	(children (make-symbol "children")))
    `(let ((,children (xml-node-children ,tag)))
       (dolist (,var ,children)
	 ,@body)))
  )


;;; Stream-based parsing
;;

(defun rudel-xml-toplevel-tag-positions (string)
  "Return positions of top-level XML tags in STRING."
  (let ((depth       0)
	(tag-opening nil)
	(start)
	(tags        nil))
    (dolist (index (number-sequence 0 (- (length string) 1)))
      (cond
       ((= (aref string index) ?<)
	(setq tag-opening (/= (aref string (+ index 1)) ?/))
	(when (and (= depth 0)
		   tag-opening)
	  (setq start index)))

       ((= (aref string index) ?>)
	(unless (or (= (aref string (- index 1)) ?/)
		    (= (aref string (- index 1)) ??))
	  (if tag-opening
	      (incf depth)
	    (decf depth)))
	(when (= depth 0)
	  (push (cons start (+ index 1)) tags)))))

    (nreverse tags)))

(defun rudel-xml-toplevel-tags (string)
  "Parse STRING as partial XML document, return complete and partial tags."
  (let ((tags (rudel-xml-toplevel-tag-positions string)))
    (list

     ;; Map top-level tag ranges into substrings.
     (mapcar
      (lambda (tag-range)
	(substring string (car tag-range) (cdr tag-range)))
      tags)

     ;; Add rest of the string
     (if tags
	 (substring string (apply #'max (mapcar #'cdr tags)))
       string)))
  )

(defun rudel-xml-assemble-tags (data)
  "Assemble complete XML tags in DATA, return list of tags and a rest."
  (destructuring-bind (tags buffer)
      (rudel-xml-toplevel-tags (apply #'concat data))
    (list tags (list buffer))))

(provide 'rudel-xml)
;;; rudel-xml.el ends here
