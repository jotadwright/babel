(in-package :mwm)

;; ---------------------
;; + Creating lex cxns +
;; ---------------------

(defun add-lex-cxn (agent form meaning)
  "Add a new lexical constructions. The meaning feature contains a
   simple predicate. The fuzzy-set representation of the meaning is
   stored in the :meaning attribute of the cxn. The cxn also
   keeps track of when it was created."
  (let ((cxn-name (make-symbol (upcase (string-append form "-cxn"))))
        (unit-name (make-var (upcase (string-append form "-unit"))))
        (new-var (make-var 'x)))
    (multiple-value-bind (cxn-set cxn)
        (eval `(def-fcg-cxn ,cxn-name
                            ((,unit-name
                              (referent ,new-var)
                              (args (,new-var)))
                             <-
                             (,unit-name
                              (HASH meaning ((,(intern form) ,new-var)))
                              --
                              (HASH form ((string ,unit-name ,form)))))
                            :cxn-set lex
                            :cxn-inventory ',(grammar agent)
                            :attributes (:form ,form
                                         :meaning ,meaning)))
      (declare (ignorable cxn-set))
      cxn)))

;; ---------------------
;; + Updating lex cxns +
;; ---------------------

(define-event attr-removed (cxn fcg-construction) (attr symbol))
(define-event cxn-removed (cxn fcg-construction))

(defun adjust-certainty (agent cxn attr delta
                               &key (upper-bound 1.0)
                               (lower-bound 0.0)
                               (remove-on-lower-bound t))
  ;; find the old entry and make a new one with
  ;; an updated certainty
  (let* ((entry (find attr (attr-val cxn :meaning)
                      :key #'(lambda (entry)
                               (attribute (car entry)))))
         (new-certainty (+ (cdr entry) delta))
         (new-entry (cons (car entry)
                          (cond ((> new-certainty upper-bound)
                                 upper-bound)
                                ((<= new-certainty lower-bound)
                                 (if remove-on-lower-bound nil lower-bound))
                                (t new-certainty)))))
    ;; if the certainty is exactly the same, do nothing
    ;; otherwise, do an update
    (when (or (null (cdr new-entry))
              (/= (cdr entry) (cdr new-entry)))
      (if (cdr new-entry)
        (setf (attr-val cxn :meaning)
              (append (list new-entry)
                      (remove attr (attr-val cxn :meaning)
                              :key #'(lambda (entry)
                                       (attribute (car entry))))))
        (progn
          (setf (attr-val cxn :meaning)
                (remove attr (attr-val cxn :meaning)
                        :key #'(lambda (entry)
                                 (attribute (car entry)))))
          (notify attr-removed cxn attr)
          (when (null (attr-val cxn :meaning))
            (delete-cxn cxn (grammar agent))
            (notify cxn-removed cxn)))))))

(defun expand-meaning (cxn new-meaning)
  (setf (attr-val cxn :meaning)
        (append (attr-val cxn :meaning)
                new-meaning)))
    

;; --------------------
;; + Finding lex cxns +
;; --------------------

(defun find-cxn-with-form (agent form)
  (find form (constructions (grammar agent))
        :key #'(lambda (cxn) (attr-val cxn :form))
        :test #'string=))

(defun find-cxns-with-form (agent form)
  (find-all form (constructions (grammar agent))
            :key #'(lambda (cxn) (attr-val cxn :form))
            :test #'string=))