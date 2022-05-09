(in-package :fcg)

(defmethod de-render ((utterance string) (mode (eql :de-render-regex)) &key &allow-other-keys)
  "passes the utterance string to the root"
  (make-instance 'coupled-feature-structure 
		   :left-pole `((root (meaning ())
                                      (sem-cat ())
                                      (form ((regex ,(make-const 'regex) ,utterance)))
                                      (syn-cat ())))
		   :right-pole '((root))))

(def-fcg-constructions test-set
    :fcg-configurations ((:de-render-mode .  :de-render-regex)))

(defun comprehend-with-regex (utterance)
  (set-data (blackboard *fcg-constructions*) :ranges-matched-in-root nil)
  (comprehend utterance)
  )

(def-fcg-cxn test-cxn 
             (<-
              (?test-unit
               (HASH meaning ((sphere ?y)(color ?x)))
               --
               (HASH form ((regex ?test-unit "what is the .* of the cube"))))))


(defun unify-atom (x y bindings &key cxn-inventory)
  (cond ((eq bindings +fail+) +fail+)
	;; handle strings and interned symbols
	((equal x y) bindings)
	;; unify uninterned symbols 
	((and (symbolp x) (symbolp y)
	      (equal (symbol-name x) (symbol-name y)))
         bindings)
        ;; handle regex
        ((and (stringp x) (stringp y))
         (use-string-from-root x y bindings cxn-inventory))
        ;; unify symbols on type-hierarchy-basis
        ((and cxn-inventory (categorial-network (original-cxn-set cxn-inventory))
              (symbolp x) (symbolp y)
              (category-exists-p x (categorial-network (original-cxn-set cxn-inventory)))
              (category-exists-p y (categorial-network (original-cxn-set cxn-inventory)))
              (categories-linked-p x y
                                   (categorial-network (original-cxn-set cxn-inventory))
                                   (get-configuration (original-cxn-set cxn-inventory) :category-linking-mode)))
         bindings)
	;; unify variables
	((variable-p x) (unify-variable x y bindings))
	((variable-p y) (unify-variable y x bindings))
        ((unify-equal x y) bindings)
	(t (values +fail+ x y))))

  
(defun use-string-from-root (regex string bindings cxn-inventory)
  (let ((regex-result (multiple-value-list (cl-ppcre::scan regex string))))
    (if (and (first regex-result))
             #|(string-still-available (first regex-result)
                                                  (second regex-result)
                                                  (find-data (blackboard cxn-inventory) :ranges-matched-in-root)))
|#
             (progn
        (push-data (blackboard cxn-inventory)
                   :ranges-matched-in-root
                   (cons (first regex-result) (second regex-result)))
        (extend-bindings regex (subseq string (first regex-result) (second regex-result)) bindings))
      +fail+)))
      
(defun string-still-available (start end ranges)
  (loop for (start-range . end-range) in ranges
        always (or (and (< start start-range) (< end start-range))
                   (> start end-range))))
            
(defun substitute-bindings (bindings x)
  "Substitute all variables in x with their binding as specified in bindings."
  (declare (type bindings bindings))
  ;; Remi 20/10: Quick solution to remove illegal bindings... but we need to
  ;;             understand where they come-from
  (setf bindings (loop for binding in bindings
                       when (rest binding) collect binding))
  ;; -------------------------------------------------------
  (labels ((aux (x)
             (cond ((variable-p x)
                    (let ((y (assoc x bindings :test #'eq)))
                      (if (and y (not (eq (cdr y) x))) (aux (cdr y)) x)))
                   ((stringp x)
                    (let ((y (assoc x bindings :test #'equalp)))
                      (if (and y (not (eq (cdr y) x))) (aux (cdr y)) x)))
                   ((atom x) x)
                   (t (cons (aux (car x)) (aux (cdr x)))))))
    (cond ((eq bindings +fail+) +fail+)
          ((eq bindings +no-bindings+) x)
          (t (aux x)))))

substitute-bindings
(comprehend-with-regex "what is the color of the cube?")

merge-structures merge-poles

;; (add-element (make-html (de-render "what is the color of the cube?" :de-render-regex)))