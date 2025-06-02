
(in-package :clg)

(defun find-cxn-by-form-and-meaning (form meaning cxn-inventory)
  "returns a cxn with the same meaning and form if it's in the cxn-inventory"
  (loop for cxn in (constructions cxn-inventory)
        when (and (irl:equivalent-irl-programs? form (fcg::extract-form-predicates cxn))
                  (irl:equivalent-irl-programs? meaning (fcg::extract-meaning-predicates cxn)))
        return cxn))

(defun meaning-predicates-with-variables (meaning)
  "Transform meaning network with constants to meaning network with variables."
    (loop for predicate in meaning
          collect (if (equal (first predicate) 'bind)
                    (list (first predicate)
                          (second predicate)
                          (variablify (third predicate))
                          (fourth predicate))
                    (cons (first predicate)
                          (mapcar #'variablify (rest predicate))))))
        

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Finding possible alignments for predicate networks and form ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass alignment-state ()
  ((assigned-predicates
    :type (or list null)
    :initform nil
    :initarg :assigned-predicates
    :accessor assigned-predicates)
   (remaining-predicates-longest-list
    :type (or list null)
    :initform nil
    :initarg :remaining-predicates-longest-list
    :accessor remaining-predicates-longest-list)
   (remaining-predicates-shortest-list
    :type (or list null)
    :initform nil
    :initarg :remaining-predicates-shortest-list
    :accessor remaining-predicates-shortest-list)))

;; (defun all-possible-alignments (list-1 list-2 &key
;;                                        (nr-of-unpaired-predicates-allowed 0))
;;   (let* ((longest-list  (if (>= (length list-1) (length list-2))
;;                         list-1 list-2))
;;         (shortest-list (if (< (length list-1) (length list-2))
;;                          list-1 list-2))
;;         (queue (list (make-instance 'alignment-state
;;                                     :assigned-predicates nil
;;                                     :remaining-predicates-longest-list longest-list
;;                                     :remaining-predicates-shortest-list shortest-list)))
;;         (possible-alignments nil))
;;     (loop until (not queue)
;;           for current-state = (pop queue)
;;           for assigned-predicates = (assigned-predicates current-state)
;;           for remaining-predicates-longest-list = (remaining-predicates-longest-list current-state)
;;           for remaining-predicates-shortest-list = (remaining-predicates-shortest-list current-state)
;;           do
;;           (if (or remaining-predicates-longest-list remaining-predicates-shortest-list)
;;             (loop for predicate-1 in (cons nil remaining-predicates-longest-list)
;;                   do
;;                   (loop for predicate-2 in (cons nil remaining-predicates-shortest-list)
;;                         for state = (make-instance 'alignment-state
;;                                           :assigned-predicates (cons `(,predicate-1 ,predicate-2) assigned-predicates)
;;                                           :remaining-predicates-longest-list (remove predicate-1 remaining-predicates-longest-list)
;;                                           :remaining-predicates-shortest-list (remove predicate-2 remaining-predicates-shortest-list))
;;                         if (and (or predicate-1 predicate-2)
;;                                 (if (and predicate-1 predicate-2)
;;                                   (and (= (length predicate-1) (length predicate-2))
;;                                        (equalp (first predicate-1) (first predicate-2))
;;                                        (loop with equal-symbols = t
;;                                              for el-1 in predicate-1
;;                                              for el-2 in predicate-2
;;                                              if (and (not (or (variable-p el-1) (variable-p el-2)))
;;                                                      (not (equalp el-1 el-2)))
;;                                              do (setf equal-symbols nil)
;;                                              finally (return equal-symbols)))
;;                                   t)
;;                                 (<= (count nil (mapcar #'first (assigned-predicates state))) nr-of-unpaired-predicates-allowed)
;;                                 (<= (count nil (mapcar #'second (assigned-predicates state))) nr-of-unpaired-predicates-allowed)
;;                                 (<= (length assigned-predicates) (+ (length longest-list) (length shortest-list))))
;;                         do (push state queue)))
;;             (unless (find assigned-predicates possible-alignments :test #'(lambda (l1 l2) (permutation-of? l1 l2 :test #'equal)))
;;               (push assigned-predicates possible-alignments))))
;;     possible-alignments))



(defmethod irl::find-map-function ((v1 string) (v2 string) 
                        &optional (frame (irl::make-map-frame))
                        &key (extension-test #'irl::function-frame))
  "Adding case for strings, used when comparing predicate networks"
  (declare (ignore extension-test))
      (when (string= v1 v2) 
        frame))

(defun find-matching-lex-cxns (cxn-inventory observed-form gold-standard-meaning utterance)
  "return all lexical cxns that can apply by checking whether they are a subset of the observed form and meaning"
  ;; if a certain item matches twice, we'll discard it to avoid ambiguity
  ;; e.g.: is there a cylinder next to the blue cylinder? will only return blue (if in inventory), not cylinder
  (let ((remaining-form (form-predicates-with-variables observed-form)))
    (sort (loop for cxn in (constructions cxn-inventory)
                when (and (eql (phrase-type cxn) 'lexical) 
                          (irl:unify-irl-programs (fcg::extract-form-predicates cxn) remaining-form)
                          (setf remaining-form (set-difference remaining-form (fcg::extract-form-predicates cxn) :test #'irl:unify-irl-programs))
                          (irl:unify-irl-programs (fcg::extract-meaning-predicates cxn) gold-standard-meaning)
                          ;;we need to check if a cxn could match twice based on the meaning and discard these cases,
                          ;; if it matches multiple times, the size of the set diff will be larger than 1
                          (= 1 (- (length gold-standard-meaning)
                                  (length (set-difference gold-standard-meaning (fcg::extract-meaning-predicates cxn) :test #'irl:unify-irl-programs)))))   
                collect cxn)
          #'(lambda (x y)
              (<
               (search (third (first (fcg::extract-form-predicates x))) utterance)
               (search (third (first (fcg::extract-form-predicates y))) utterance))))))

(defun find-matching-lex-cxns-in-root (cxn-inventory root-strings)
  (remove nil (loop for remaining-form in root-strings
        for root-string = (third remaining-form)
        collect (loop for cxn in (constructions cxn-inventory)
                      when (and (eql (phrase-type cxn) 'lexical)
                                (string= (third (first (fcg::extract-form-predicates cxn))) root-string))
                      return cxn))))

(defun subtract-lex-cxn-meanings (lex-cxns gold-standard-meaning)
  (let ((lex-cxn-meanings (map 'list #'fcg::extract-meaning-predicates lex-cxns)))
    (loop for lex-cxn-meaning in lex-cxn-meanings
          do (setf gold-standard-meaning (set-difference gold-standard-meaning lex-cxn-meaning :test #'irl:unify-irl-programs)))
    gold-standard-meaning))

(defun subtract-lex-cxn-forms (lex-cxns string-predicates-in-root)
    (loop for lex-cxn in lex-cxns
          for lex-form = (fcg::extract-form-predicates lex-cxn)
          do (setf string-predicates-in-root (set-difference string-predicates-in-root lex-form :test #'irl:unify-irl-programs)))
    string-predicates-in-root)