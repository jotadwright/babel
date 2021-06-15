;;;; fcg-utils.lisp

(in-package :clevr-learning)

(defun extract-meanings-from-cipn (cipn)
  (extract-meanings
   (left-pole-structure
    (car-resulting-cfs
     (cipn-car cipn)))))

(defun extract-forms-from-cipn (cipn)
  (extract-forms
   (left-pole-structure
    (car-resulting-cfs
     (cipn-car cipn)))))

(defun get-cxn-type (cxn)
  (attr-val cxn :cxn-type))

(defun get-cxn-chunk (cxn)
  (attr-val cxn :chunk))

(defun cxn-score (cxn)
  (attr-val cxn :score))

(defun item-based-number-of-slots (cxn)
  (when (eql (get-cxn-type cxn) 'item-based)
    (1- (length (contributing-part cxn)))))

(defun get-strings-from-root (node)
  (gl::form-predicates-with-variables
   (extract-string
    (get-root
     (if (find 'fcg::second-merge-failed (fcg::statuses node))
       (car-first-merge-structure (cipn-car node))
       (left-pole-structure
        (car-resulting-cfs
         (cipn-car node))))))))

(defun set-cxn-last-used (agent cxn)
  (let ((current-interaction-nr
         (interaction-number
          (current-interaction
           (experiment agent)))))
    (setf (attr-val cxn :last-used) current-interaction-nr)))

(defun extract-and-render (cxn)
  (list-of-strings->string
   (render (extract-form-predicates cxn)
           (get-configuration (cxn-inventory cxn) :render-mode))))

(defun get-cxns-of-type (agent type)
  (if (eql type 'all)
    (constructions-list (grammar agent))
    (find-all type (constructions-list (grammar agent))
              :key #'get-cxn-type)))

(defun find-cxn-by-type-form-and-meaning (type form meaning cxn-inventory)
  "returns a cxn with the same meaning and form if it's in the cxn-inventory"
  (loop for cxn in (find-all type (constructions-list cxn-inventory) :key #'get-cxn-type)
        when (and (irl:equivalent-irl-programs? form (extract-form-predicates cxn))
                  (irl:equivalent-irl-programs? meaning (extract-meaning-predicates cxn)))
        return cxn))

(defun subunit-blocks-for-lex-cxns (lex-cxns lex-subunit-names args th-links)
  (loop for lex-cxn in lex-cxns
        for arg in args
        for lex-cxn-unit-name in lex-subunit-names
        for th-link in th-links
        for lex-slot-lex-class = (cdr th-link)
        collect `(,lex-cxn-unit-name
                  (syn-cat (gl::lex-class ,lex-slot-lex-class))) into contributing-units
        collect `(,lex-cxn-unit-name
                  (args (,arg))
                  --) into conditional-units
        finally (return (values conditional-units contributing-units))))

(defun form-predicates->hash-string (form-predicates)
  ;; the last string predicate
  (third
   (last-elt
    (find-all 'string form-predicates
              :key #'first))))

(defun meaning-predicates->hash-meaning (meaning-predicates)
  ;; the last meaning predicate (excluding get-context)
  (first
   (first
    (find-all-if-not #'(lambda (p)
                         (or (eql p 'bind)
                             (eql p 'get-context)))
                     meaning-predicates :key #'first))))