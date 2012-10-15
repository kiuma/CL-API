;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; $Header: src/generator.lisp $

;;; Copyright (c) 2010, Andrea Chiumenti.  All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :cl-api)

(defvar *html-output* *standard-output*)

(defvar *api-css*
  "
body {margin: 0 2em .5em 2em;font-family: Verdana,Arial,sans-serif;}
.package {background: #efefef; 
          padding: 1.5em 0 1em 0; 
          text-align: center; 
          font-size: x-large;}
.definition {background: #efefef; padding: .3em 1em;}
a.symbolname, a:visited.symbolname {font-weight: bold;}
.initargs {font-size: small;}
.slots {font-size: small;}
div.label {border-bottom: 1px solid #efefef; margin-bottom: .5em}
.symboldecl, .footer {margin: 0 2em 2em 2em;}
.symbolname {font-weight: bold; color: gray;}
.symboltype {font-style: italic;margin-left: 1.5em; font-size: smaller;}
.documentation {color: gray; font-family: Fixed,monospace;margin: 0 0 1.5em 0.5em;}
.packagedocumentation {color: gray; 
                       font-family: Fixed,monospace;
                       margin: 0 0 1.5em 0; 
                       border: 1px solid #efefef; 
                       padding-left: 1.5em;}
.symbolcomments span {font-weight: bold;}
.footer {font-size: x-small; text-align: right; margin-top: 2em; padding-top: 1em; border-top: 1px solid gray;}
.undocumented {color: red; font-weight: bold;}
a, a:visited {font-weight: bold; color: gray; text-decoration: none; font-weight: normal;}
a:hover {border-bottom: 1px solid gray; }
.label {font-weight: bold; font-style: italic;color: gray;}
.labeltitle {font-weight: bold; font-style: italic;color: gray; border: 1px solid #efefef; padding: .25em;margin-bottom: .5em}
.frame {marin-top: 1.5em}
.expander {border: 2px solid #efefef; color: gray; 
           font-weight: bold; 
           font-family: Fixed,monospace; 
           margin-right: .25em; padding: 0 .25em;cursor: pointer;}
"
  "Default css for generated HTML document")


(defvar *current-package* nil
  "Current package used during elaboration")

(defun replace<> (string)
  (let ((scanner& (create-scanner "&" :single-line-mode t))
        (scanner> (create-scanner ">" :single-line-mode t))
        (scanner< (create-scanner "<" :single-line-mode t)))
    (regex-replace-all scanner< 
                       (regex-replace-all scanner> 
                                          (regex-replace-all scanner& string "&amp;") 
                                          "&gt;") 
                       "&lt;")))

(defun ht> (name attributes body &optional (stream *html-output*))
  "- NAME tag name
- ATTRIBUTES plist of html tag attributes
- BODY tag body (string or lambda)"
  (lambda ()
    (format stream "~%<~(~a~)~{~^ ~(~a~)=\"~a\"~}>"
            name
            attributes)
    (when body
      (if (listp body)
          (loop for element in (flatten body)
             when element
             do (if (functionp element)
                    (funcall element)
                    (replace<> (format stream "~a" element))))
          (if (and body (functionp body))
              (funcall body)
              (replace<> (format stream "~a" body)))))
    (format stream "</~(~a~)>" name)))

(defun ht< (name attributes &optional (stream *html-output*))
  "- NAME tag name
- ATTRIBUTES plist of html tag attributes"
  (lambda ()
    (format stream "~%<~(~a~)~{~^ ~(~a~)=\"~a\"~}>"
            name
            attributes)))

(defun variablep (symbol)
  (and (boundp symbol) (not (constantp symbol))))

(defun lambdalist (function)
  #+sbcl(sb-introspect:function-lambda-list function))

(defun external-p (symbol &optional (package *current-package*))
  (multiple-value-bind (result status)
      (find-symbol (symbol-name symbol) package)
    (declare (ignore result))
    (equal :external status)))

(defun doc-function (symbol &optional (setf-p nil))
  (let* ((symbol-function (if setf-p `(setf ,symbol) symbol))
         (function (and (fboundp symbol-function) (fdefinition symbol-function))))
    (when function
      (let ((gf-documentation (or (documentation symbol-function 'function)
                                  (format nil "~(~s~)" :undocumented))))
        (list :name symbol-function
              :type (type-of function)
              :documentation gf-documentation
              :lambdalist (format nil "~{~(~a~)~^ ~}" (lambdalist symbol-function)))))))

(defun doc-macro (symbol)
  (let ((macro (macro-function symbol)))
    (when macro
      (let ((documentation (or (documentation macro 't)
                               (format nil "~(~s~)" :undocumented))))
        (list :name symbol
              :type 'macro
              :documentation documentation
              :lambdalist (format nil "~{~(~a~)~^ ~}" (lambdalist macro)))))))



(defun doc-class (symbol condition-p)
  (let ((class (handler-case (find-class symbol)
                 (error () nil))))
    (when (and class (or (and (not condition-p) (typep class 'standard-class))
                         (and condition-p (not (typep class 'standard-class)))))
      (finalize-inheritance class)
      (list :name symbol
            :type 'class
            :documentation (documentation class 't)
            :superclasses (loop for class in (class-direct-superclasses class)
                             collect (class-name class))
            :default-initargs (loop for initarg in (class-default-initargs class)
                                 collect (format nil "~(~s~)" (first initarg))
                                 collect (if (stringp (second initarg))
                                             (format nil "~s" (second initarg))
                                             (format nil "~(~s~)" (second initarg))))
            :slots (loop for slot in (class-direct-slots class)
                      for reader = (first (slot-definition-readers slot))
                      for writer = (second (first (slot-definition-writers slot)))
                      for documentation = (documentation slot 't)
                      when (or (external-p (slot-definition-name slot))
                               (external-p reader)
                               (external-p writer))
                      collect (list :name (slot-definition-name slot)
                                    :reader (when (external-p reader)
                                              reader)
                                    :writer (when (external-p writer)
                                              writer)
                                    :documentation (or documentation
                                                       :undocumented)))))))

(defun doc-classes (symbols &optional condition-p exclude-function)
  (loop for symbol in symbols
     when (and symbol (or (null exclude-function) (not (funcall exclude-function symbol)))
               (handler-case (find-class symbol) (error () nil)))
     collect (doc-class symbol condition-p)))


(defun doc-varibles (symbols &optional exclude-function)
  (loop for symbol in symbols
     when (and symbol (variablep symbol) (or (null exclude-function) (not (funcall exclude-function symbol))))
     collect (list :name symbol
                   :type 'variable
                   :documentation (or (documentation symbol 'variable)
                                      :undocumented)
                   :value (if (boundp symbol)
                              (format nil "~s" (symbol-value symbol))
                              :unbound))))

(defun doc-constants (symbols &optional exclude-function)
  (loop for symbol in symbols
     when (and symbol (constantp symbol) (or (null exclude-function) (not (funcall exclude-function symbol))))
     collect (list :name symbol
                   :type 'constant
                   :documentation (or (documentation symbol 'variable)
                                      ":undocumented")
                   :value (format nil "~s" (symbol-value symbol)))))

(defun doc-functions (symbols &optional exclude-function)
  (loop for symbol in symbols
     when (and symbol (or (null exclude-function) (not (funcall exclude-function symbol))))
     collect (doc-function symbol)
     when (and symbol (or (null exclude-function) (not (funcall exclude-function symbol))))
     collect (doc-function symbol t)))

(defun doc-macros (symbols &optional exclude-function)
  (loop for symbol in symbols
     when (and symbol (or (null exclude-function) (not (funcall exclude-function symbol))))
     collect (doc-macro symbol)))

(defun render-html-symbolname (item &optional (symboltype nil))
  (let ((name (or (and (atom item) item) (getf item :name))))
    (if symboltype
        (ht> :a `(:class "symbolname"
                         :name ,(format nil "~(~a~)_~(~a~)" name symboltype)
                         :href ,(format nil "#~(~a~)_~(~a~)" name symboltype))
             `(,(format nil "~(~a~)" name)))
        (ht> :span `(:class "symbolname")
             `(,(format nil "~(~a~)" name))))))

(defun render-html-symboltype (item)
  (ht> :span '(:class "symboltype")
       (format nil "~(~a~)" (getf item :type))))

(defun render-html-documentation (item)
  (ht> :div '(:class "documentation")
       (ht> :pre '() (getf item :documentation))))


(defun html-render-vars-and-const (items &optional (type 'vars))
  (loop for var in items
     when var
     collect (ht> :div '(:class "symboldecl")
                  (list (ht> :div '(:class "definition")
                             (list (render-html-symbolname var type)
                                   ": "
                                   (ht> :span '(:class "value")
                                        (getf var :value))
                                   (render-html-symboltype var)))
                        (render-html-documentation var)))))

(defun html-render-functions (items)
  (loop for var in items
     when var
     collect (ht> :div '(:class "symboldecl")
                  (list (ht> :div '(:class "definition")
                             (list (render-html-symbolname var 'func)
                                   (ht> :span '(:class "lambdalist")
                                        (format nil "~(~a~)" (getf var :lambdalist)))
                                   (render-html-symboltype var)))
                        (render-html-documentation var)))))

(defun html-render-macros (items)
  (loop for var in items
     when var
     collect (ht> :div '(:class "symboldecl")
                  (list (ht> :div '(:class "definition")
                             (list (render-html-symbolname var 'macro)
                                   (ht> :span '(:class "lambdalist")
                                        (format nil "~(~a~)" (getf var :lambdalist)))
                                   (render-html-symboltype var)))
                        (render-html-documentation var)))))

(defun html-render-classes (items)
  (loop for var in items
     when var
     collect (ht> :div '(:class "symboldecl")
                  (list (ht> :div '(:class "definition")
                             (list (render-html-symbolname var 'class)
                                   (ht> :span '(:class "lambdalist") (format nil "(~{~(~a~)~^ ~})" (getf var :superclasses)))
                                   (render-html-symboltype var)
                                   (when (getf var :default-initargs)
                                     (ht> :div '(:class "initargs")
                                          (append (list (ht> :span '(:class "label") "default-initargs: "))
                                                  (loop for (k v) on (getf var :default-initargs) by #'cddr
                                                     collect (ht> :span '(:class "initarg") k)
                                                     collect (ht> :span '(:class "value") v)))))))
                        (render-html-documentation var)
                        (when (getf var :slots)
                          (ht> :div '(:class "slots")
                               (list
                                (ht> :div '(class "label") "SLOTS")
                                (loop for slot in (getf var :slots)
                                   :collect (render-html-symbolname (getf slot :name) nil)
                                   when (getf slot :reader)
                                   :collect (list " reader: " (render-html-symbolname (getf slot :reader) nil ))
                                   when (getf slot :writer)
                                   :collect (list " writer: " (render-html-symbolname (getf slot :writer) nil))
                                   :collect (render-html-documentation slot)
                                     ))))
                        ))))

(defun html-render-package (package)
  (list (ht> :div '(:class "package")
             (list (ht> :div '(:class "definition")
                        (list "API for package: "
                              (render-html-symbolname (format nil "~(~a~)" package) 'package)))))
        (ht> :div '(:class "packagedocumentation")
             (ht> :pre '() (or (documentation (find-package package) 't) (format nil "~(~s~)" :undocumented))))))

(defun render-html (package constants vars classes conditions functions macros)
  (ht> :html '(:lang "en")
       (list (ht> :head '()
                  (list (ht< :meta '(:http-equiv "Content-Type"
                                     :content "text/html; charset=UTF-8"))
                        (ht> :title '() (format nil "API for ~a" package))
                        (ht> :style '(:type "text/css"
                                      :media "all")
                             *api-css*)
                        (ht> :script '(:type "text/javascript")
                             "
function expand (expander, id) {
var text = expander.innerHTML;
if (text == '-')
{
  expander.innerHTML = '+';
  document.getElementById(id).style.display = 'none';
}
else
{
  expander.innerHTML = '-';
  document.getElementById(id).style.display = '';
}
}")))
             (ht> :body '()
                  (list (html-render-package package)
                        (when constants
                          (ht> :div '(:class "frame")
                               (list (ht> :div '(:class "labeltitle")
                                          (list
                                           (ht> :span '(:class "expander" :onclick "expand(this, 'constants');")
                                                "-")
                                           "Constants"))
                                     (ht> :div '(:id "constants")
                                          (html-render-vars-and-const constants 'const)))))
                        (when vars
                          (ht> :div '(:class "frame")
                               (list (ht> :div '(:class "labeltitle")
                                          (list
                                           (ht> :span '(:class "expander" :onclick "expand(this, 'vars');")
                                                "-")
                                           "Variables"))
                                     (ht> :div '(:id "vars")
                                          (html-render-vars-and-const vars)))))
                        (when classes
                          (ht> :div '(:class "frame")
                               (list (ht> :div '(:class "labeltitle")
                                          (list
                                           (ht> :span '(:class "expander" :onclick "expand(this, 'classes');")
                                                "-")
                                           "Classes"))
                                     (ht> :div '(:id "classes")
                                          (html-render-classes classes)))))
                        (when conditions
                          (ht> :div '(:class "frame")
                               (list (ht> :div '(:class "labeltitle")
                                          (list
                                           (ht> :span '(:class "expander" :onclick "expand(this, 'conditions');")
                                                "-")
                                           "Conditions"))
                                     (ht> :div '(:id "conditions")
                                          (html-render-classes conditions)))))
                        (when functions
                          (ht> :div '(:class "frame")
                               (list (ht> :div '(:class "labeltitle")
                                          (list
                                           (ht> :span '(:class "expander" :onclick "expand(this, 'functions');")
                                                "-")
                                           "Functions"))
                                     (ht> :div '(:id "functions")
                                          (html-render-functions functions)))))
                        (when macros
                          (ht> :div '(:class "frame")
                               (list (ht> :div '(:class "labeltitle")
                                          (list
                                           (ht> :span '(:class "expander" :onclick "expand(this, 'macros');")
                                                "-")
                                           "Macros"))
                                     (ht> :div '(:id "macros")
                                          (html-render-macros macros)))))
                        (ht> :div '(:class "footer")
                             (list
                              "Generated by: "
                              (ht> :a '(:href "http://common-lisp.net/project/cl-api") "CL-API"))))))))

(defun package-external-symbols (package)
  (let ((lst ()))
    (do-external-symbols (s (find-package package))
      (push s lst))
    (sort lst #'string-lessp)))

(defun document (package &key exclude-const exclude-var exclude-class exclude-cond exclude-func exclude-macro)
  (let* ((*current-package* package)
         (symbols (package-external-symbols package))
         (consts (doc-constants symbols exclude-const))
         (vars (doc-varibles symbols exclude-var))
         (classes (doc-classes symbols exclude-class))
         (conditions (doc-classes symbols t exclude-cond))
         (functions (doc-functions symbols exclude-func))
         (macros (doc-macros symbols exclude-macro)))
    (render-html package (remove-if #'null consts) (remove-if #'null vars) (remove-if #'null classes) (remove-if #'null conditions) (remove-if #'null functions) (remove-if #'null macros))))

(defun api-gen (package dir-pathname &key exclude-const exclude-var exclude-class exclude-cond exclude-func exclude-macro)
  "Create an html API document of the given PACKAGE.
The document will be named with [package-name].html

DIR-PATHNAME Is the output directory
:EXCLUDE-CONST :EXCLUDE-VAR :EXCLUDE-CLASS :EXCLUDE-COND :EXCLUDE-FUNC :EXCLUDE-MACRO keys may be 
valorized with exclusion functions that take as parameter the symbol name.

For example an exclusion key will be somethink like (lambda (s) (string-equal \"foo\" (symbol-name s))) 
that will exclude FOO symbol.

When one of these functions return true the API is not generated for the current symbol
"
  (let ((file-pathname (merge-pathnames dir-pathname (format nil "~(~a~).html" package))))
    (with-open-file (*html-output* file-pathname
                                   :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create)
      (funcall (document package
                         :exclude-const exclude-const
                         :exclude-var exclude-var
                         :exclude-class exclude-class
                         :exclude-cond exclude-cond
                         :exclude-func exclude-func
                         :exclude-macro exclude-macro))
      (format t "Generation complete: \"~a\"" file-pathname))))