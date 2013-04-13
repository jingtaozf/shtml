;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: SHTML; Base: 10 -*-
;;; $Header$

;;; Copyright (c) 2003-2007, Dr. Edmund Weitz. All rights reserved.
;;; Copyright (c) 2008-2012, Dr. Dmitriy Ivanov. All rights reserved.

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

;;; Additions and changes by Dmitriy Ivanov
;;;
;;; 1. A Lisp form in place of "plane attribute" is allowed.
;;;    Forms are read by the standard read function.
;;;    All starting tags (except TMPL_INCLUDE) accept forms
;;;    instead of "old style" attributes.
;;;
;;;    The *attributes-are-lisp-forms* special variable was introduced.
;;;    Bind or set it to NIL to treating those tags in "compatibility" mode.
;;;
;;; 2. Symbols follow standard Lisp syntax: a package specifier is allowed
;;;    in front of the name. By default, symbols are interned into the
;;;    package *template-package*, a new special variable.
;;;    (Shall we reuse the same *template-symbol-package* instead?)
;;;
;;; 3. To interpret the forms in run-time, the template-eval was introduced.
;;;    It is a simple evaluator akin to the standard eval function except for:
;;;    - A limited number of special-forms is supported, namely:
;;;      IF WHEN UNLESS AND OR NOT QUOTE.
;;;    - The symbol with a name like *var* is treated as a dynamic variable
;;;      and is retrieved via symbol-value.
;;;    - The values of other symbols are looked up via *value-access-function*.
;;;
;;; 4. The TMPL_EVAL tag and create-eval-printer were introduced;
;;;    the former should be used instead of the TMPL_VAR tag.
;;;
;;; 5. The value of *format-non-strings* has got an additional meaning.
;;;    If eq to T, the result is produced by means of princ-to-string,
;;;    i.e. (format nil "~A" ...).
;;;    If it is true but not eq to T, the value must be a single-parameter
;;;    function, which returns two values:
;;;    (1) a string resulted from its argument, and optionally
;;;    (2) do-not-modify flag controlling whether *string-modifier* is
;;;        applied afterwards.
;;;    The truth as second value can prevent the result of converting
;;;    from predefined format, e.g. LHTML, from further escaping.
;;;
;;;  6. Tag TMPL_ELSE and all ending tags /TMPL_... can embed an optional text
;;;	between the tag name and the closing marker "-->". This text is intended
;;;	for readability only and completely ignored by the template parser.
;;;	For example:
;;;	   <!-- /TMPL_LOOP rows -->
;;;
;;;  7. The TMPL_ELIF tag was introduced to allow a more concise code.
;;;     In full, now the "if" pattern looks like:
;;;	   <!-- TMPL_IF condition_1 -->
;;;        text_1
;;;	   <!-- TMPL_ELIF condition_2 -->
;;;        text_2
;;;        ...
;;;	   <!-- TMPL_ELSE -->
;;;        text_else
;;;	   <!-- /TMPL_IF -->
;;;
;;; CAUTION:
;;;  When delivering with LispWorks, do not forget to keep
;;;  - all the symbol names and definitions referred by templates,
;;;  - all the packages mentioned.
;;;
;;; CONCEPTUAL NOTE:
;;;  The default value of *template-symbol-package* is the keyword package.
;;;  Interning symbols there is not a good idea from general Lisp concepts
;;;  because keywords are actually constants in Lisp.

(in-package #:shtml)

(defmacro with-use-value-restart ((symbol) error-form)
  "Provide a USE-VALUE restart for ERROR-FORM in case the value
associated with SYMBOL isn't to our liking."
  `(restart-case
      ,error-form
    (use-value (other-value)
      :report (lambda (stream)
                (format stream
                        "Use another value for symbol ~S: "
                        ,symbol))
      :interactive (lambda ()
                     (format t
                             "Enter another value for symbol ~S: "
                             ,symbol)
                     (multiple-value-list (eval (read))))
      other-value)))

;; indentation for LispWorks editor
#+:lispworks
(editor:setup-indent "with-use-value-restart" 1 2 4)

(defun template-eval (form &optional (environment *template-environment*) in-loop-p)
  "Template evaluator combining both eval and apply.
Arguments:
 FORM
   If it is list, the evaluator treats specially the functions that are standard
   macros or special operators in Lisp. For now, the following are recognized:
     IF WHEN UNLESS AND OR NOT QUOTE.
   If it is a symbol, the evaluator looks at its name first;
   when the name is like *var*, it tries to interpret the symbol as
   dynamic variable and retreive its value via symbol-value;
   otherwise the look-up proceeds via *value-access-function*.
 ENVIRONMENT
   Binds template variables with values (aka template structure)."
  #+ylib
  (let ((yl:*tiny-symbol-value-function* *value-access-function*))
    (yl:tiny-eval form environment in-loop-p))
  #-ylib
  (cond ((consp form)
         (let ((function (first form))
               (args (rest form)))
           (case function
             ((IF WHEN)
              (if (template-eval (first args) environment nil)  ; in-loop-p = nil!
                  (template-eval (second args) environment in-loop-p)
                  (template-eval (third args) environment in-loop-p)))
             (UNLESS
              (if (template-eval (first args) environment nil)  ; in-loop-p = nil!
                  (template-eval (third args) environment in-loop-p)
                  (template-eval (second args) environment in-loop-p)))
             (AND 
              (do ((value t)
                   (rest args (rest rest)))
                  ((null rest) value)
                (unless (setq value (template-eval (first rest) environment in-loop-p))
                  (return nil))))
             (OR
              (loop for arg in args
                    thereis (template-eval arg environment in-loop-p)))
             (NOT
              (not (template-eval (first args) environment in-loop-p)))
             (QUOTE
              (first args))
             (otherwise
              ;; May invoke -> (error 'undefined-function :name function)
              (apply function
                     (loop for arg in args
                           collect (template-eval arg environment in-loop-p)))))))
        ((and (symbolp form) (not (constantp form)))
         ;(let ((symbol-name (symbol-name form)))		; CAUTION: not reliable in
         ;(if (and (char= (schar symbol-name 0) #\*)		;  delivered application!
         ;         (char= (schar symbol-name (1- (length symbol-name))) #\*)
         (if (boundp form)
             ;; The symbol is a Lisp special variable - take its dymanic value,
             ;; i.e. either global or bound within the excution process.
             (symbol-value form)
             ;; The symbol is a "usual" template variable
             ;; - seek in the "lexical" environment
             (funcall *value-access-function* form environment in-loop-p)))
        (t form)))						; constant

(defun create-eval-printer (string-list form next-fn)
  "Used internally to create template printers for TMPL_EVAL.
Arguments:
 FORM is a lisp form associated with the tag.
 NEXT-FN is the next function to be called in the chain of closures.
 STRING-LIST is a list of strings in reverse order to be printed first."
  (let ((string (list-to-string string-list)))
    (lambda (environment)
      (write-string string *template-output*)
      (let ((value (template-eval form environment))
            string
            (do-not-modify nil))
        (cond ((null value)
               (setq string (if *convert-nil-to-empty-string*
                                ""
                                (with-use-value-restart (form)
                                  (signal-template-missing-value-error
                                   "The value of form ~S is NIL"
                                   form)))))
              ((stringp value)
               (setq string value))
              ((eq *format-non-strings* t)
               (setq string (princ-to-string value)))
              (*format-non-strings*
               (multiple-value-setq (string do-not-modify)
                   (funcall *format-non-strings* value)))
              (t
               (with-use-value-restart (form)
                 (error 'template-not-a-string-error
                        :value value
                        :format-control "The value ~S of form ~S is not a string"
                        :format-arguments (list value form)))))
        (write-string (if do-not-modify
                          string
                          (funcall *string-modifier* string))
                      *template-output*))
      (funcall next-fn environment))))

(defun create-simple-printer (string-list &optional (next-fn #'no-values))
  "Used internally to create template printers for strings which don't
include template tags. NEXT-FN is the next function to be called in
the chain of closures. STRING-LIST is a list of strings in reverse
order to be printed first."
  (let ((string (list-to-string string-list)))
    (lambda (environment)
      (write-string string *template-output*)
      (funcall next-fn environment))))

(defun create-var-printer (string-list symbol next-fn)
  "Used internally to create template printers for TMPL_VAR. SYMBOL is
the symbol associated with the tag. NEXT-FN is the next function to be
called in the chain of closures. STRING-LIST is a list of strings in
reverse order to be printed first."
  (let ((string (list-to-string string-list)))
    (lambda (values)
      (write-string string *template-output*)
      (let* ((value (funcall *value-access-function* symbol values))
             (string (typecase value
                       (null
                        (if *convert-nil-to-empty-string*
                            ""
                            (with-use-value-restart (symbol)
                              (signal-template-missing-value-error 
                               "Value for symbol ~S is NIL"
                               symbol))))
                       (string value)                            
                       (otherwise
                        (cond ((eq *format-non-strings* t) (princ-to-string value))
                              (*format-non-strings* (funcall *format-non-strings* value))
                              (t (with-use-value-restart (symbol)
                                   (error 'template-not-a-string-error
                                          :value value
                                          :format-control "Value ~S for symbol ~S is not a string"
                                          :format-arguments (list value symbol)))))))))
        (write-string (funcall *string-modifier* string) *template-output*))
      (funcall next-fn values))))

(defun create-include-printer (string-list pathname next-fn)
  "Used internally to create template printers for TMPL_INCLUDE.
PATHNAME is the pathname associated with the tag. NEXT-FN is the next
function to be called in the chain of closures. STRING-LIST is a list
of strings in reverse order to be printed first."
  (let ((string (list-to-string string-list)))
    (lambda (environment)
      (write-string string *template-output*)
      (funcall (car (gethash pathname *printer-hash*)) environment)
      (funcall next-fn environment))))

(defun create-if-printer (string-list form if-fn else-fn next-fn unlessp)
  "Used internally to create template printers for TMPL_IF and TMPL_UNLESS tags.
Arguments:
 FORM is a list form associated with the tag.
 IF-FN is the printer for the IF branch,
 ELSE-FN is the printer for the ELSE branch.
 NEXT-FN is the next function to be called in the chain of closures.
 STRING-LIST is a list of strings in reverse order to be printed first.
 UNLESSP is boolean, if it is true, IF-FN and ELSE-FN are switched."
  (let ((string (list-to-string string-list)))
    (when unlessp
      (rotatef if-fn else-fn))
    (lambda (environment)
      (write-string string *template-output*)
      (if (if *attributes-are-lisp-forms*
              (template-eval form environment)
              (funcall *value-access-function* form environment))
          (funcall if-fn environment)
          (funcall else-fn environment))
      (funcall next-fn environment))))

(defun create-loop-printer (string-list form body-fn next-fn)
  "Used internally to create template printers for TMPL_LOOP tags.
Arguments:
 SYMBOL is the symbol associated with the tag.
 BODY-FN is the template printer for the body of the loop.
 NEXT-FN is the next function to be called in the chain of closures.
 STRING-LIST is a list of strings in reverse order to be printed first."
  (let ((string (list-to-string string-list)))
    (if *sequences-are-lists*
        (lambda (environment)
          (write-string string *template-output*)
          (dolist (value (if *attributes-are-lisp-forms*
                             (template-eval form environment t)
                             (funcall *value-access-function* form environment t)))
            (funcall body-fn value))
          (funcall next-fn environment))
        (lambda (environment)
          (write-string string *template-output*)
          (loop for value across (if *attributes-are-lisp-forms*
                                     (template-eval form environment t)
                                     (funcall *value-access-function* form environment t))
                do (funcall body-fn value))
          (funcall next-fn environment)))))

(defun create-repeat-printer (string-list form body-fn next-fn)
  "Used internally to create template printers for TMPL_REPEAT
tags. SYMBOL is the symbol associated with the tag. BODY-FN is the
template printer for the body of the loop. NEXT-FN is the next
function to be called in the chain of closures. STRING-LIST is a list
of strings in reverse order to be printed first."
  (let ((string (list-to-string string-list)))
    (lambda (environment)
      (write-string string *template-output*)
      (let ((factor (if *attributes-are-lisp-forms*
                        (template-eval form environment)
                        (funcall *value-access-function* form environment))))
        (when (and (integerp factor) (plusp factor))
          (loop repeat factor
                do (funcall body-fn environment))))
      (funcall next-fn environment))))

(defun create-call-printer (string-list form next-fn)
  "Used internally to create template printers for TMPL_CALL tags.
SYMBOL is the symbol associated with the tag. BODY-FN is the template
printer for the body of the loop. NEXT-FN is the next function to be
called in the chain of closures. STRING-LIST is a list of strings in
reverse order to be printed first."
  (let ((string (list-to-string string-list)))
    (if *sequences-are-lists*
        (lambda (environment)
          (write-string string *template-output*)
          (dolist (call (if *attributes-are-lisp-forms*
                            (template-eval form environment t)
                            (funcall *value-access-function* form environment t)))
            (fill-and-print-template
             (funcall *call-template-access-function* call)
             (funcall *call-value-access-function* call)
             :stream *template-output*))
          (funcall next-fn environment))
        (lambda (environment)
          (write-string string *template-output*)
          (loop for call across (if *attributes-are-lisp-forms*
                                    (template-eval form environment t)
                                    (funcall *value-access-function* form environment t))
                do (fill-and-print-template
                    (funcall *call-template-access-function* call)
                    (funcall *call-value-access-function* call)
                    :stream *template-output*))
          (funcall next-fn environment)))))

(defun create-template-printer-aux (string-stack end-token)
  "Reads from *STANDARD-INPUT* and returns a template printer from
what it reads.  When this function is entered the stream pointer must
not be inside a template tag.
Arguments:
  STRING-STACK is a list of strings (in reverse order) read so far
     which haven't been used to build a template printer.
  END-TOKEN is either NIL or one of :LOOP, :REPEAT, :IF,
    :IF-ELSE, or :UNLESS-ELSE denoting that we expect certain tags to
    close open TMPL_LOOP, TMPL_REPEAT, TMPL_IF, or TMPL_UNLESS tags.
This function returns a second value which is true if, after reading
TMPL_IF or TMPL_UNLESS, a corresponding TMPL_ELSE was seen."
  (let* ((string
           ;; read text up to the next template start marker
           (read-until *template-start-marker*
                       ;; don't skip it, return it
                       :skip nil
                       :eof-action (lambda (collector)
                                     (when end-token
                                       ;; make sure we don't accept
                                       ;; EOF if there are still tags
                                       ;; waiting to be closed
                                       (signal-template-syntax-error
                                        "Unexpected EOF, ~A tag is missing"
                                        (case end-token
                                          ((:loop) "<!-- /TMPL_LOOP -->")
                                          ((:repeat) "<!-- /TMPL_REPEAT -->")
                                          ((:if :if-else) "<!-- /TMPL_IF -->")
                                          ((:unless :unless-else) "<!-- /TMPL_UNLESS -->"))))
                                     ;; otherwise (EOF before another
                                     ;; start marker was seen) just
                                     ;; return a template printer
                                     ;; which unconditionally prints
                                     ;; the rest of the stream
                                     (return-from create-template-printer-aux
                                       (create-simple-printer
                                        (cons collector string-stack))))))
         (whitespace
           ;; skip whitespace but keep it in case this turns out not
           ;; to be a template tag
           (skip-whitespace :skip nil))
         (ssi-p nil)
         (token
           ;; read what could be a template token's name
           (with-syntax-error-location ()
             (read-while (lambda (c)
                           (or (alpha-char-p c)
                               (char= c #\#)
                               (char= c #\_)
                               (char= c #\/)))
                         :skip nil
                         :eof-action (lambda (collector)
                                       (declare (ignore collector))
                                       ;; complain about tags which
                                       ;; haven't been closed
                                       (signal-template-syntax-error
                                        "EOF while inside of tag starting with ~S"
                                        *template-start-marker*))))))
    (cond ((or (if (string-equal token "#include")
                 (setf ssi-p t))
               (string-equal token "TMPL_INCLUDE"))
            ;; TMPL_INCLUDE tag - first read the pathname which has to
            ;; follow and merge it with *DEFAULT-TEMPLATE-PATHNAME*
            (let* ((pathname (read-tag-rest :read-attribute t :ssi-p ssi-p :intern nil))
                   (merged-pathname
                     (merge-pathnames pathname
                                      *default-template-pathname*)))
              (when (member merged-pathname *included-files*
                            :test #'equal)
                ;; raise an error if this file has been included
                ;; before - infinite recursion ahead!
                (with-syntax-error-location ()
                  (signal-template-syntax-error
                   "Infinite recursion - file ~S includes itself"
                   merged-pathname)))
              (add-template-dependence merged-pathname)
              ;; otherwise create (and cache) a template printer
              (create-template-printer merged-pathname)
              (multiple-value-bind (next-fn else-follows)
                  ;; first we recursively create the template printer
                  ;; for the rest of the stream
                  (create-template-printer-aux (skip-trailing-whitespace)
                                               end-token)
                ;; then we combine it with the strings before the tag
                ;; to create a template printer for TMPL_INCLUDE
                (values
                 (create-include-printer (cons (skip-leading-whitespace string)
                                               string-stack)
                                         merged-pathname
                                         next-fn)
                 else-follows))))
          ((string-equal token "TMPL_EVAL")
            ;; TMPL_EVAL form - first read the form which has to follow
            ;; with usual lisp reader
            (let ((form (read-tag-rest :read-form t)))
              (multiple-value-bind (next-fn else-follows)
                  ;; first we recursively create the template printer
                  ;; for the rest of the stream
                  (create-template-printer-aux nil end-token)
                (values
                 ;; then we combine it with the strings before the tag
                 ;; to create a template printer for TMPL_EVAL -
                 ;; note that we don't skip leading and trailing whitespace
                 ;; here
                 (create-eval-printer (cons string string-stack)
                                      form
                                      next-fn)
                 else-follows))))
          ((string-equal token "TMPL_VAR")
            ;; TMPL_VAR tag - first read the symbol which has to
            ;; follow and intern it
            (let ((symbol (read-tag-rest :read-attribute t)))
              (multiple-value-bind (next-fn else-follows)
                  ;; first we recursively create the template printer
                  ;; for the rest of the stream
                  (create-template-printer-aux nil end-token)
                (values
                 ;; then we combine it with the strings before the tag
                 ;; to create a template printer for TMPL_VAR - note
                 ;; that we don't skip leading and trailing whitespace
                 ;; here
                 (create-var-printer (cons string string-stack)
                                     symbol
                                     next-fn)
                 else-follows))))
          ((or (string-equal token "TMPL_LOOP")
               (string-equal token "TMPL_REPEAT"))
            ;; TMPL_LOOP or TMPL_REPEAT tag - first read the symbol
            ;; which has to follow and intern it
            (let* ((kind (if (string-equal token "TMPL_LOOP") :loop :repeat))
                   (symbol (if *attributes-are-lisp-forms*
                               (read-tag-rest :read-form t)
                               (read-tag-rest :read-attribute t)))
                   ;; then read the stream up to the corresponding
                   ;; end tag and create a template printer for the
                   ;; loop body
                   (body-fn (with-syntax-error-location ()
                              (create-template-printer-aux
                               (skip-trailing-whitespace)
                               ;; this argument denotes that we expect
                               ;; to see /TMPL_LOOP or /TMPL_REPEAT and
                               ;; want to stop there
                               kind))))
              (multiple-value-bind (next-fn else-follows)
                  ;; now we recursively create the template printer
                  ;; for the rest of the stream
                  (create-template-printer-aux (skip-trailing-whitespace)
                                               end-token)
                (values
                 ;; then we combine it with the strings before the tag
                 ;; and the body printer to create a template printer
                 ;; for TMPL_LOOP
                 (funcall (case kind
                            (:loop #'create-loop-printer)
                            (:repeat #'create-repeat-printer))
                          (cons (skip-leading-whitespace string)
                                string-stack)
                          symbol
                          body-fn
                          next-fn)
                 else-follows))))
	  ((string-equal token "TMPL_CALL")
            ;; TMPL_CALL tag - first read the symbol which has to
            ;; follow and intern it
           (let ((symbol (if *attributes-are-lisp-forms*
                             (read-tag-rest :read-form t)
                             (read-tag-rest :read-attribute t))))
             (multiple-value-bind (next-fn else-follows)
                  ;; recursively create the template printer for the
                  ;; rest of the stream
                 (create-template-printer-aux (skip-trailing-whitespace)
                                               end-token)
               ;; create the printer that will output the strings
               ;; before this tag and call the templates stored under
               ;; SYMBOL
               (values (funcall #'create-call-printer
                                (cons (skip-leading-whitespace string)
                                      string-stack)
                                symbol
                                next-fn)
                       else-follows))))
          ((string-equal token "/TMPL_LOOP")
            (unless (eq end-token :loop)
              ;; check if we expected /TMPL_LOOP here, i.e. if an open
              ;; TMPL_LOOP was pending
              (with-syntax-error-location ()
                (signal-template-syntax-error "Unexpected /TMPL_LOOP")))
            ;; read the rest of the tag but ignore it - no attributes
            ;; expected
            (read-tag-rest :skip t)
            ;; just create a simple template printer for strings -
            ;; this is the end of some TMPL_LOOP body
            (create-simple-printer (cons (skip-leading-whitespace string)
                                         string-stack)))
          ((string-equal token "/TMPL_REPEAT")
            (unless (eq end-token :repeat)
              ;; check if we expected /TMPL_REPEAT here, i.e. if an open
              ;; TMPL_REPEAT was pending
              (with-syntax-error-location ()
                (signal-template-syntax-error "Unexpected /TMPL_REPEAT")))
            ;; read the rest of the tag but ignore it - no attributes
            ;; expected
            (read-tag-rest :skip t)
            ;; just create a simple template printer for strings -
            ;; this is the end of some TMPL_REPEAT body
            (create-simple-printer (cons (skip-leading-whitespace string)
                                         string-stack)))
          ((or (string-equal token "TMPL_IF")
               (string-equal token "TMPL_UNLESS"))
            ;; TMPL_IF or TMPL_UNLESS tag - first read the symbol
            ;; which has to follow and intern it
            (let ((symbol (if *attributes-are-lisp-forms*
                              (read-tag-rest :read-form t)
                              (read-tag-rest :read-attribute t)))
                  (unlessp (string-equal token "TMPL_UNLESS")))
              (multiple-value-bind (if-fn else-follows)
                  (with-syntax-error-location ()
                    ;; then read the stream up to the corresponding
                    ;; TMPL_ELSE, /TMPL_IF, or /TMPL_UNLESS and create
                    ;; a template printer for the "if" (or "unless") part
                    (create-template-printer-aux
                     (skip-trailing-whitespace)
                     ;; this argument denotes that we expect to see
                     ;; TMPL_ELSE _or_ one of /TMPL_IF, /TMPL_UNLESS and,
                     ;; in the second case, want to stop there
                     (if unlessp :unless-else :if-else)))
                (let ((else-fn (cond ((functionp else-follows)
                                      ;; TMPL_ELIF has been was encountered -
                                      ;; it returns the off-the-shelf function
                                      else-follows)
                                     (else-follows
                                      ;; if we encountered TMPL_ELSE read
                                      ;; the stream up to the corresponding
                                      ;; /TMPL_IF or /TMPL_UNLESS and
                                      ;; create a template printer for the "else" part
                                      (with-syntax-error-location ()
                                        (create-template-printer-aux
                                         (skip-trailing-whitespace)
                                         ;; From now on, we expect to see /TMPL_IF
                                         ;; or /TMPL_UNLESS (but not TMPL_ELSE) 
                                         ;; and want to stop there
                                         (if unlessp :unless :if))))
                                     (t ;; use a dummy printer for the "else"
                                        ;; part if we didn't see TMPL_ELSE
                                        #'no-values))))
                  (multiple-value-bind (next-fn else-follows)
                      ;; now we recursively create the template printer
                      ;; for the rest of the stream
                      (create-template-printer-aux (skip-trailing-whitespace)
                                                   end-token)
                    (values
                     ;; then we combine it with the strings before the
                     ;; tag and the "if" and "else" parts to create a
                     ;; template printer for TMPL_IF or TMPL_UNLESS
                     (create-if-printer (cons (skip-leading-whitespace string)
                                              string-stack)
                                        symbol
                                        if-fn
                                        else-fn
                                        next-fn
                                        unlessp)
                     else-follows))))))
          ((string-equal token "TMPL_ELIF")
            (unless (eq end-token :if-else)
              ;; Check if we expected /TMPL_ELIF here, i.e. if an open
              ;; TMPL_IF was pending and we haven't seen TMPL_ELSE before
              (with-syntax-error-location ()
                (signal-template-syntax-error "Unexpected TMPL_ELIF")))
            ;; Next read the conditional expression which has to follow
            (let* ((symbol (if *attributes-are-lisp-forms*
                               (read-tag-rest :read-form t)
                               (read-tag-rest :read-attribute t)))
                   ;; The value to be returned and assigned to if-fn of the caller
                   (fn (create-simple-printer (cons (skip-leading-whitespace string)
                                                    string-stack))))
              ;; Then read the stream up to the following TMPL_ELIF, TMPL_ELSE
              ;; or /TMPL_IF and create a template printer for the "elif" part.
              (multiple-value-bind (if-fn else-follows)
                  (with-syntax-error-location ()
                    (create-template-printer-aux (skip-trailing-whitespace) end-token))
                (let ((else-fn (cond ((functionp else-follows)
                                      ;; Another TMPL_ELIF was encountered -
                                      ;; it returns the off-the-shelf function
                                      else-follows)
                                     (else-follows
                                      ;; As we encountered TMPL_ELSE, read
                                      ;; the stream up to the closing /TMPL_IF
                                      ;; and create a template printer for the "else" part
                                      (with-syntax-error-location ()
                                        (create-template-printer-aux
                                         (skip-trailing-whitespace)
                                         ;; From now on, we expect /TMPL_IF
                                         ;; but not TMPL_ELSE
                                         :if)))
                                     (t ;; use a dummy printer for the "else"
                                        ;; part if we didn't see TMPL_ELSE
                                        #'no-values))))
                  (values
                   fn
                   ;; then we combine it with the strings before the
                   ;; tag and the "if" and "else" parts to create a
                   ;; template printer for TMPL_IF or TMPL_UNLESS
                   (create-if-printer nil                 ;(skip-trailing-whitespace)
                                      symbol
                                      if-fn
                                      else-fn
                                      #'no-values         ; no next-fn
                                      nil))))))
          ((string-equal token "TMPL_ELSE")
            (unless (or (eq end-token :if-else) (eq end-token :unless-else))
              ;; check if we expected /TMPL_ELSE here, i.e. if an open
              ;; TMPL_IF or TMPL_UNLESS was pending and we haven't
              ;; seen TMPL_ELSE before
              (with-syntax-error-location ()
                (signal-template-syntax-error "Unexpected TMPL_ELSE")))
            ;; read the rest of the tag but ignore it - no attributes expected
            (read-tag-rest :skip t)
            ;; just create a simple template printer for strings -
            ;; this is the end of some "if" or "elif" part
            ;; The actual "else" printer will be created by the TMPL_IF, 
            ;; TMPL_ELIF, or TMPL_UNLESS branch.
            (values
             (create-simple-printer (cons (skip-leading-whitespace string)
                                          string-stack))
             ;; return a true second value to denote that we've seen TMPL_ELSE
             t))
          ((string-equal token "/TMPL_IF")
            (unless (or (eq end-token :if) (eq end-token :if-else))
              ;; check if we expected /TMPL_IF here, i.e. if an open
              ;; TMPL_IF was pending
              (with-syntax-error-location ()
                (signal-template-syntax-error "Unexpected /TMPL_IF")))
            ;; read the rest of the tag but ignore it - no attributes
            ;; expected
            (read-tag-rest :skip t)
            ;; just create a simple template printer for strings -
            ;; this is the end of some "if" or "else" part
            (create-simple-printer (cons (skip-leading-whitespace string)
                                         string-stack)))
          ((string-equal token "/TMPL_UNLESS")
            (unless (or (eq end-token :unless) (eq end-token :unless-else))
              ;; check if we expected /TMPL_UNLESS here, i.e. if an open
              ;; TMPL_UNLESS was pending
              (with-syntax-error-location ()
                (signal-template-syntax-error "Unexpected /TMPL_UNLESS")))
            ;; read the rest of the tag but ignore it - no attributes
            ;; expected
            (read-tag-rest :skip t)
            ;; just create a simple template printer for strings -
            ;; this is the end of some "unless" or "else" part
            (create-simple-printer (cons (skip-leading-whitespace string)
                                         string-stack)))
          (t
            ;; we couldn't identify a valid tag, so we treat
            ;; everything we've read so far as a literal string and
            ;; carry on - if we're lucky our CL implementation will
            ;; optimize this tail call into an iterative loop
            (create-template-printer-aux
             (cons token
                   (cons whitespace
                         (cons *template-start-marker*
                               (cons string string-stack))))
             end-token)))))

(defun %create-template-printer-aux (&rest args)
  "Wrapper for CREATE-TEMPLATE-PRINTER-AUX to initialize
*CURRENT-COLUMN* and *CURRENT-LINE*."
  (let ((*current-column* 0)
        (*current-line* 1))
    (apply #'create-template-printer-aux args)))
