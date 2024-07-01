(in-package :au-benchmark.base)

;; ############################################################################

;; ----------------------------------------------------------------------------
;; configuration:

(defvar *default-id-base-name* "ID")
(defvar *default-var-id-base-name* "?VAR")
(defvar *default-const-id-base-name* "CONST")
(defvar *default-sticker-name* "STICKER")
;; ----------------------------------------------------------------------------
;; private system functionality:

(defparameter *nid-table* (make-hash-table :test #'equal))

(proclaim '(inline get-next-id-number))
(defun get-next-id-number (name)
  "Return the next number to create the next unique id with the given name."
  (declare (type string name))
  (if (gethash name *nid-table*)
      (incf (gethash name *nid-table*))
      (setf (gethash name *nid-table*) 1)))

(proclaim '(inline remove-numeric-tail))
(defun remove-numeric-tail (name)
  (declare (type string name))
  (loop for i from (- (length name) 1) downto 0
        for char = (elt name i)
        when (not (digit-char-p char))
        do (if (equal #\- char)
             (return (subseq name 0 i))
             (return name))
        finally (return name)))

(proclaim '(inline get-base-name))
(defun get-base-name (name &key
                      (remove-numeric-tail t)
                      (remove-question-mark t))
  "Return the base of the given name.
   - If base is a symbol then the base name is the symbol's name.
   - If base is a string then this string is the base name.
   - If remove-question-mark is true and the base name starts with a
     question-mark then this question-mark is removed from the base name.
   - If remove-numeric-tail is true and name is of the form 's-n',
     where s is a string of alphanumerical characters, and n is a string of
     numerical character, then the base is 's', i.e. the hyphen and trailing
     numerical characters are removed."
  (declare (type (or string symbol) name))
  (let* ((name (cond ((stringp name) (upcase name))
                    ((symbolp name) (symbol-name name))
                    (t (write-to-string name))))
        (name-as-string name))
    (if remove-numeric-tail (setq name (remove-numeric-tail name)))
    (if (string= name "") ;; for symbols like -5
      name-as-string
      (if (and remove-question-mark (char-equal #\? (elt name 0)))
        (subseq name 1)
        name))))
;; ----------------------------------------------------------------------------
;; public utilities:

(export '(get-base-name
          make-id
          make-var
          variable-p
          reset-id-counters
          mkstr
          fresh-variables))

(unless (fboundp 'string-append)
  (defun string-append (&rest strings)
    "concatenates strings"
    (format nil "~{~a~}" strings)))

(defun mkstr (&rest arguments)
  "Returns a string containing all arguments."
  (format nil "~{~a~}" arguments))

(defun make-id (&optional name)
  "Create and return a unique numbered id, which is a symbol whose symbol-name
   consists of a name and a number."
  #-lispworks
  (declare (type (or symbol string null) name))
  (let ((base-name (cond ((null name) *default-id-base-name*)
                         ((symbolp name) (symbol-name name))
                         ((stringp name) name))))
    (make-symbol (format nil "~:@(~a~)-~a" base-name (get-next-id-number base-name)))))

(defun make-var (&optional name)
  "Create and return a unique FCG variable symbol.
   Note that if you have the choice between passing a string or a symbol as the
   argument to make-var, make-const or make-id, then pass it the string. If you
   pass it the symbol then the implementation will simply take the symbol-name
   from it further ignore the symbol."
  #-lispworks
  (declare (type (or null string symbol) name))
  (make-id (if name
               (format nil "?~a" (get-base-name name))
               *default-var-id-base-name*)))

(defun variable-p (x)
  "Test whether x is a variable, i.e. whether it is a symbol of which the name
   starts with a question mark."
  (and (symbolp x) (equal (char (symbol-name x) 0) #\?)))

(defun reset-id-counters ()
  "Reset all the counters for the numbered ids."
  (setf *nid-table* (make-hash-table :test #'equal))
  t)

(defun fresh-variables (set-of-predicates)
  (labels ((subst-bindings (bindings)
             (loop for predicate in set-of-predicates
                   collect (loop for elem in predicate
                                 for subst = (assoc elem bindings)
                                 if subst collect (cdr subst)
                                 else collect elem))))
    (let* ((all-variables (find-all-anywhere-if #'variable-p set-of-predicates))
           (unique-variables (remove-duplicates all-variables))
           (renamings (loop for var in unique-variables
                            for base-name = (get-base-name var)
                            collect (cons var (intern (format nil "~a" (make-var base-name)))))))
      (values (subst-bindings renamings) renamings))))