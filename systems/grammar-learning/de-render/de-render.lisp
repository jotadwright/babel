(in-package :fcg)

(defmethod de-render ((utterance string) (mode (eql :de-render-regex)) &key &allow-other-keys)
  "passes the utterance string to the root"
  (make-instance 'coupled-feature-structure 
		   :left-pole `((root (meaning ())
                                      (sem-cat ())
                                      (form ((regex ,(make-const 'regex) ,utterance)))
                                      (syn-cat ())))
		   :right-pole '((root))))


(def-fcg-cxn test-cxn 
             (<-
              (?test-unit
               (HASH meaning ((cube ?x)(color ?x)))
               --
               (HASH form ((regex ?test-unit ".*(color).*"))))))


(defun unify-atom (x y bindings &key cxn-inventory)
  (cond ((eq bindings +fail+) +fail+)
	;; handle strings and interned symbols
	((equal x y) bindings)
	;; unify uninterned symbols 
	((and (symbolp x) (symbolp y)
	      (equal (symbol-name x) (symbol-name y)))
         bindings)
        ;; handle regex
        ((and (stringp x) (stringp y)
              (cl-ppcre::scan x y))
         bindings)
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



            
(def-fcg-constructions test-set
    :fcg-configurations ((:de-render-mode .  :de-render-regex)))


(comprehend "what is the color of the cube?")

;; (add-element (make-html (de-render "what is the color of the cube?" :de-render-regex)))