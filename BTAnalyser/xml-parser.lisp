
;;;======================================================================
;;;                                                                     |
;;;  Copyright (c) 2016, Sentot Kromodimoeljo                           |
;;;  All Rights Reserved.                                               |
;;;                                                                     |
;;;  Redistribution and use in source and binary forms, with or without |
;;;  modification, are permitted provided the following conditions are  |
;;;  met:                                                               |
;;;                                                                     |
;;;  1. Redistributions of source code must retain the above copyright  |
;;;     notice, this list of conditions and the following disclaimer.   |
;;;  2. Redistributions in binary form must reproduce the above         |
;;;     copyright notice, this list of conditions and the following     |
;;;     disclaimer in the documentation and/or other materials provided |
;;;     with the distribution.                                          |
;;;                                                                     |
;;;  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND             |
;;;  CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,        |
;;;  INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF           |
;;;  MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE           |
;;;  DISCLAIMED. IN NO EVENT SHALL SENTOT KROMODIMOELJO BE LIABLE FOR   |
;;;  ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR           |
;;;  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT  |
;;;  OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR |
;;;  BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF         |
;;;  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT          |
;;;  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE  |
;;;  USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH   |
;;;  DAMAGE.                                                            |
;;;                                                                     |
;;;======================================================================


(in-package "bt")


;;; A recursive-descent non-validating XML parser

;;; Input: a string representing the XML document.
;;; Output: a DOM tree s-expression for the XML document
;;;         and the remaining unconsumed string.

;;; Format for each element produced in the DOM tree:
;;;   (name-string (attr1 attr2 ...) (text1 ...) (child1-element ...))
;;;   attr = (attr-name text)

(defun parse-xml-doc (str)
  (let ((rest-str (consume-xml-declaration str))
	(result nil))
    (multiple-value-setq (result rest-str) (parse-xml-element rest-str))
    result))

;;; Define accessors for a "DOM" element:

(defun xml-element-name (element)
  (first element))

(defun xml-element-attributes (element)
  (second element))

(defun xml-element-text (element)
  ;; Note that this gives a list of strings.
  ;; In general, text segments are interspersed with children of the element.
  ;; However, currently the result of xml parsing doesn't say how they are
  ;; interspersed. This may need to be addressed. ******
  (third element))

(defun xml-element-children (element)
  (fourth element))


(defun consume-xml-declaration (str)
  ;; trim leading white space first, then consume declaration if any
  (let ((st (string-trim '(#\Space #\Tab #\Return #\Newline) str)))
    (cond ((and (> (string-length st) 5) (string= "<?xml" (substring st 0 5)))
	   (multiple-value-bind
	    (attrs remaining-str)
	    (parse-xml-attributes (substring st 5))
	    (setq remaining-str
		  (string-trim '(#\Space #\Tab #\Return #\Newline)
			       remaining-str))
	    (cond ((and (>= (string-length remaining-str) 2)
			(string= "?>" (substring remaining-str 0 2)))
		   (substring remaining-str 2))
		  (t
		   ;; error
		   remaining-str))))
	  (t st))))

(defun parse-xml-attributes (str)
  (let ((st (string-trim '(#\Space #\Tab #\Return #\Newline) str))
	(attrs nil))
    (cond ((and (>= (string-length st) 1) (starts-xml-name st))
	   (multiple-value-bind
	    (attr remaining-str)
	    (parse-xml-attribute st)
	    (multiple-value-setq
	     (attrs remaining-str)
	     (parse-xml-attributes remaining-str))
	    (values (cons attr attrs) remaining-str)))
	  (t (values nil st)))))

;;; Right now don't know what a combining character is,
;;; so combining characters are not handled.
;;; Thus a name starts with a letter, _ or :.
;;; Letter, digit, ., -, _ and : can appear afterwards in the name.

(defun starts-xml-name (str)
  (and (>= (string-length str) 1)
       (or (alpha-char-p (aref str 0))
	   (equal (aref str 0) #\:)
	   (equal (aref str 0) #\-))))

(defun is-xml-name-char (char)
  (or (alpha-char-p char)
      (digit-char-p char)
      (equal char #\.)
      (equal char #\-)
      (equal char #\_)
      (equal char #\:)))

(defun parse-xml-equal-sign (str)
  (let ((st (string-trim '(#\Space #\Tab #\Return #\Newline) str)))
    (cond ((and (> (string-length st) 0) (equal (aref st 0) #\=))
	   (values "=" (substring st 1)))
	  (t (values nil st)))))

(defun parse-xml-name (str)
  (let ((st (string-trim '(#\Space #\Tab #\Return #\Newline) str)))
    (cond ((starts-xml-name st)
	   (let ((length 1))
	     (loop for i from 1
		   while (and (< i (string-length st))
			      (is-xml-name-char (aref st i)))
		   do (setq length (+ i 1)))
	     (values (substring st 0 length) (substring st length))))
	  (t (values "" st)))))

(defun parse-xml-attribute (str)
  (let ((st (string-trim '(#\Space #\Tab #\Return #\Newline) str)))
    (cond ((starts-xml-name st)
	   (let ((name "")
		 (val ""))
	     (multiple-value-setq
	      (name st)
	      (parse-xml-name st))
	     (multiple-value-setq
	      (val st)
	      (parse-xml-equal-sign st))
	     (multiple-value-setq
	      (val st)
	      (parse-xml-attribute-value st))
	     (values (list name val) st)))
	  (t (values nil st)))))

(defun parse-xml-attribute-value (str)
  (let ((st (string-trim '(#\Space #\Tab #\Return #\Newline) str)))
    (cond ((and (> (string-length st) 0) (equal (aref st 0) #\"))
	   (let ((val "")
		 (done nil))
	     (setq st (substring st 1))
	     (loop while (and (< 0 (string-length st)) (not done))
		   do
		   (cond ((equal (aref st 0) #\")
			  (setq st (substring st 1))
			  (setq done t))
			 ((equal (aref st 0) #\&)
			  (let ((next nil))
			    (multiple-value-setq
			     (next st)
			     (parse-xml-entity st))
			    (setq val (string-append val next))))
			 (t
			  (setq val (string-append val (substring st 0 1)))
			  (setq st (substring st 1)))))
	     (values val st)))
	  ;;
	  ((and (> (string-length st) 0) (equal (aref st 0) #\'))
	   (let ((val "")
		 (done nil))
	     (setq st (substring st 1))
	     (loop while (and (< 0 (string-length st)) (not done))
		   do
		   (cond ((equal (aref st 0) #\')
			  (setq st (substring st 1))
			  (setq done t))
			 ((equal (aref st 0) #\&)
			  ;;
			  )
			 (t
			  (setq val (string-append val (substring st 0 1)))
			  (setq st (substring st 1)))))
	     (values val st)))
	  (t (values "" st)))))

(defun parse-xml-entity (str)
  (cond ((and (>= (string-length str) 6)
	      (string= "&quot;" (substring str 0 6)))
	 (values "\"" (substring str 6)))
	((and (>= (string-length str) 5)
	      (string= "&amp;" (substring str 0 5)))
	 (values "&" (substring str 5)))
	((and (>= (string-length str) 6)
	      (string= "&apos;" (substring str 0 6)))
	 (values "'" (substring str 6)))
	((and (>= (string-length str) 4)
	      (string= "&lt;" (substring str 0 4)))
	 (values "<" (substring str 4)))
	((and (>= (string-length str) 4)
	      (string= "&gt;" (substring str 0 4)))
	 (values ">" (substring str 4)))
	((and (>= (string-length str) 6)
	      (string= "&nbsp;" (substring str 0 6)))
	 (values " " (substring str 6)))
	(t
	 ;; for now ignoring other entities ******
	 (values (substring str 0 1) (substring str 1)))))
	

(defun parse-xml-element (str)
  (let ((st (string-trim '(#\Space #\Tab #\Return #\Newline) str))
	(name nil)
	(attributes nil)
	(text-list nil)
	(children nil))
    (cond ((and (> (string-length st) 0) (equal (aref st 0) #\<))
	   (setq st (string-trim '(#\Space #\Tab #\Return #\Newline)
				 (substring st 1)))
	   (multiple-value-setq (name st) (parse-xml-name st))
	   (multiple-value-setq (attributes st) (parse-xml-attributes st))
	   (let ((done nil))
	     (setq st (string-trim '(#\Space #\Tab #\Return #\Newline) st))
	     (cond ((and (> (string-length st) 1)
			 (string= "/>" (substring st 0 2)))
		    (setq done t)
		    (setq st (substring st 2)))
		   ((and (> (string-length st) 0)
			 (equal (aref st 0) #\>))
		    (setq st (substring st 1)))
		   (t (format t "Syntax error~%")))
	     (loop while (not done)
		   do
		   (progn (setq st (string-trim
				    '(#\Space #\Tab #\Return #\Newline) st))
			  (cond ((and (> (string-length st) 1)
				      (string= "</" (substring st 0 2)))
				 (setq st (substring st 2))
				 (setq st
				       (string-trim
					'(#\Space #\Tab #\Return #\Newline) st))
				 (let ((name2 ""))
				   (multiple-value-setq (name2 st)
							(parse-xml-name st))
				   (unless (equal name name2)
				     (format
				      t "Mismatched element names: ~A - ~A~%"
				      name name2))
				   (setq
				    st
				    (string-trim
				     '(#\Space #\Tab #\Return #\Newline) st))
				   (cond ((and (> (string-length st) 0)
					       (equal (aref st 0) #\>))
					  (setq st (substring st 1)))
					 (t
					  (format t "Syntax error~%"))))
				 (setq done t))
				((and (> (string-length st) 0)
				      (equal (aref st 0) #\<))
				 (let ((child nil))
				   (multiple-value-setq
				    (child st) (parse-xml-element st))
				   (setq children
					 (append children (list child)))))
				((and (>= (string-length st) 2)
				      (string= "/>" (substring st 0 2)))
				 (setq done t)
				 (setq st (substring st 2)))
				((= (string-length st) 0)
				 (setq done t))
				(t
				 (let ((text nil))
				   (multiple-value-setq
				    (text st) (parse-xml-text st))
				   (setq text-list
					 (append text-list (list text))))))))
	   (values (list name attributes text-list children) st)))
	  (t (values (list name attributes text-list children) st)))))

(defun parse-xml-text (str)
  (let ((i 0)
	(done nil)
	(max (string-length str)))
    (loop while (not done)
	  do
	  (cond ((and (< i max) (equal (aref str i) #\<))
		 (setq done t))
		((and (< (+ i 1) max)
		      (string= "/>" (substring str i (+ i 2))))
		 (setq done t))
		(t (incf i))))
    (values (substring str 0 i) (substring str i))))

(defun parse-xml-file (filename)
  (with-open-file (stream filename)
    (cond ((null stream)
           (format *error-output* "File not found: ~A~%" filename)
           nil)
          (t
	   (let ((str "")
		 (done nil))
	     (loop while (not done)
		   do
		   (multiple-value-bind
		    (st eof)
		    (read-line stream nil)
		    (setq done eof)
		    (setq str (string-append str (format nil "~%") st))))
	     (parse-xml-doc str))))))


