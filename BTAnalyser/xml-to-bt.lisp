
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

;;; The translator is the counterpart of parse-top-level in bt-parser.
;;; It works on an s-expression produced by parse-xml-doc.
;;; Whereas bt-parser supports TextBE and ComBE files,
;;; the translator here supports BESE files.


(defun xml-to-bt (xml)
  (setq *bt-root* (xml-to-bt-node (car (fourth xml))))
  )

(defun xml-to-node-type (str)
  (cond ((equal str "STATE") 'update)
	((equal str "GUARDED_EVENT") 'guard)
	((equal str "SELECTION") 'select)
	((equal str "QUANTIFICATION") 'quantification)
	((or (equal str "EVENT")
	     (equal str "EXTERNAL_INPUT")
	     (equal str "EXTERNAL_OUTPUT"))
	 'event)
	((or (equal str "INTERNAL_INPUT")
	     (equal str "INTENAL_INPUT"))
	 'input)
	((or (equal str "INTERNAL_OUTPUT")
	     (equal str "INTENAL_OUTPUT"))
	 'output)
	(t 'unknown)))

(defun xml-to-node-token (str)
  (cond ((equal str "STATE") 's-token)
	((equal str "GUARDED_EVENT") 'g-token)
	((equal str "SELECTION") 'l-token)
	((equal str "QUANTIFICATION") 'q-token)
	((equal str "EVENT") 'e-token)
	((equal str "EXTERNAL_INPUT") 'ei-token)
	((equal str "EXTERNAL_OUTPUT") 'eo-token)
	((or (equal str "INTERNAL_INPUT")
	     (equal str "INTENAL_INPUT"))
	 'ii-token)
	((or (equal str "INTERNAL_OUTPUT")
	     (equal str "INTENAL_OUTPUT"))
	 'io-token)
	(t 'unknown-token)))

(defun xml-to-flag (str)
  (cond ((equal str " ^ ") 'reversion)
	((equal str "- -") 'kill)
	((equal str "=>") 'reference)
	((equal str " = ") 'synchronise)))

(defun xml-to-requirement-label (xml)
  (let ((result nil))
    (loop for requirement in xml
	  do
	  (let ((id (assoc-equal "ID" (xml-element-attributes requirement))))
	    (cond ((null result)
		   (unless (null id)
		     (setq result (second id))))
		  (t
		   (unless (null id)
		     (setq result (string-append result "," (second id))))))))
    result))

(defun xml-to-requirement-status (xml)
  (cond ((equal xml "Implied") "+")
	((equal xml "Updated") "++")
	((equal xml "Refined") "+-")
	((equal xml "Deleted") "--")
	((equal xml "Invalid") "-")))

(defun xml-to-atomic (xml)
  (and xml
       (equal (second (assoc-equal "PARENT_REL_TYPE"
				   (xml-element-attributes (first xml))))
	      "atomic")))

(defun xml-to-branch-type (xml)
  (cond ((null xml) nil)
	((= (length xml) 1) nil)
	((equal (second (assoc-equal "ND_CHOICE"
				     (xml-element-attributes (first xml))))
		"true")
	 'alternative)
	(t 'parallel)))

(defun xml-to-bt-node (xml)
  (let ((name (xml-element-name xml))
	(attributes (xml-element-attributes xml))
	(subelements (xml-element-children xml))
	(node (make-bt-node nil)))
    (let ((type (xml-to-node-type name))
	  (behaviour-type (xml-to-node-token name))
	  (component (second (assoc-equal "COMP_NAME" attributes)))
	  (behaviour (second (assoc-equal "NAME" attributes)))
	  (requirements (xml-element-children
			 (assoc-equal "REQUIREMENTS" subelements)))
	  (requirement-status (second (assoc-equal "VALIDITY" attributes)))
	  (flag (xml-to-flag
		 (second (assoc-equal "OPERATOR" attributes))))
	  (children (xml-element-children (assoc-equal "CHILDREN" subelements)))
	  )
      (when (eq type 'quantification)
	(let ((quant-type (second (assoc-equal "QUANT" attributes)))
	      (quant-var (second (assoc-equal "INSTANCE" attributes)))
	      (quant-set (second (assoc-equal "SET" attributes))))
	  (unless (and (> (length quant-set) 2)
		       (equal (aref quant-set 0) #\{)
		       (equal (aref quant-set (- (length quant-set) 1)) #\}))
	    (setq quant-set (string-append "{" quant-set "}")))
	  (setq behaviour (string-append quant-type quant-var ":" quant-set))))
      (setf (bt-node-type node) type)
      (setf (bt-node-requirement-label node)
	    (xml-to-requirement-label requirements))
      (setf (bt-node-requirement-status node)
	    (xml-to-requirement-status requirement-status))
      (setf (bt-node-component node) component)
      (setf (bt-node-behaviour node) behaviour)
      (setf (bt-node-behaviour-type node) behaviour-type)
      (setf (bt-node-flag node) flag)
      ;;(setf (bt-node-label node) ???)
      (setf (bt-node-atomic node) (xml-to-atomic children))
      (setf (bt-node-children node)
	    (loop for child in children
		  collect (xml-to-bt-node child)))
      (loop for child in (bt-node-children node)
	    do (setf (bt-node-parent child) node))
      (setf (bt-node-branch-type node) (xml-to-branch-type children))
      (set-l-value-and-r-value-event node)
      node)))

