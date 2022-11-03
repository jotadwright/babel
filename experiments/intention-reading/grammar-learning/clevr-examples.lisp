(in-package :cl-user)
(ql:quickload :grammar-learning)
(load (babel-pathname
       :directory '("systems" "fcg" "construction-inventory-processor")
       :name "construction-inventory-processor"
       :type "lisp")) ;; force recompilation


(in-package :intention-reading)

;; start the web monitor
 (activate-monitor trace-fcg)
;;(activate-monitor fcg::trace-fcg-search-process)

(defun set-up-cxn-inventory-and-repairs ()
  (wi::reset)
  (eval '(def-fcg-constructions-with-type-hierarchy grammar-learning
                                                    :feature-types ((args set)
                                                                    (form set-of-predicates)
                                                                    (meaning set-of-predicates)
                                                                    (subunits set)
                                                                    (footprints set))
                                                    :fcg-configurations ((:parse-goal-tests :non-gold-standard-meaning)
                                                                         (:production-goal-tests :non-gold-standard-utterance)
                                                                         (:de-render-mode . :de-render-string-meets)
                                                                         (:render-mode . :generate-and-test)
                                                                         (:consolidate-repairs . t)
                                                                         (:update-th-links . t))
                                                    :diagnostics (diagnose-non-gold-standard-meaning diagnose-non-gold-standard-utterance)
                                                    :repairs (add-lexical-cxn
                                                              repair-item-based+item-based->item-based-cxn
                                                              repair-item-based+item-based->item-based-cxn-addition
                                                              add-item-based-cxn
                                                              repair-holophrase-single-addition
                                                              repair-holophrase-single-deletion
                                                              repair-lexical->item-based-cxn
                                                              add-holophrase-cxn))))



(defun test-repair-holophrase-substitution-comprehension ()
  (set-up-cxn-inventory-and-repairs)
  (comprehend "what is the size of the red sphere" :gold-standard-meaning '((get-context ?source-1) (filter ?target-2 ?target-1 ?color-4) (unique ?target-object-1 ?target-2) (bind shape-category ?shape-4 sphere) (bind attribute-category ?attribute-6 size) (filter ?target-1 ?source-1 ?shape-4) (bind color-category ?color-4 red) (query ?target-4 ?target-object-1 ?attribute-6)))
  (comprehend "what is the size of the red cube" :gold-standard-meaning '((get-context ?source-1) (filter ?target-2 ?target-1 ?color-4) (unique ?target-object-1 ?target-2) (bind shape-category ?shape-2 cube) (bind attribute-category ?attribute-6 size) (filter ?target-1 ?source-1 ?shape-2) (bind color-category ?color-4 red) (query ?target-4 ?target-object-1 ?attribute-6)))
  (wi:add-element (make-html (categorial-network
                              *fcg-constructions*) :weights? t :colored-edges-0-1 t))
  (wi:add-element (make-html *fcg-constructions*)))

(defun test-repair-holophrase-addition-comprehension ()
  (set-up-cxn-inventory-and-repairs)
  (comprehend "what is the size of the cube" :gold-standard-meaning '((get-context ?source-1) (filter ?target-1 ?source-1 ?shape-2) (unique ?target-object-1 ?target-1) (bind attribute-category ?attribute-6 size) (bind shape-category ?shape-2 cube) (query ?target-4 ?target-object-1 ?attribute-6)))
  (comprehend "what is the size of the red cube" :gold-standard-meaning '((get-context ?source-1) (filter ?target-2 ?target-1 ?color-4) (unique ?target-object-1 ?target-2) (bind shape-category ?shape-2 cube) (bind attribute-category ?attribute-6 size) (filter ?target-1 ?source-1 ?shape-2) (bind color-category ?color-4 red) (query ?target-4 ?target-object-1 ?attribute-6)))
  (wi:add-element (make-html (categorial-network
                              *fcg-constructions*) :weights? t :colored-edges-0-1 t))
  (wi:add-element (make-html *fcg-constructions*)))

(defun test-repair-holophrase-deletion-comprehension ()
  (set-up-cxn-inventory-and-repairs)
  (comprehend "what is the size of the red cube" :gold-standard-meaning '((get-context ?source-1) (filter ?target-2 ?target-1 ?color-4) (unique ?target-object-1 ?target-2) (bind shape-category ?shape-2 cube) (bind attribute-category ?attribute-6 size) (filter ?target-1 ?source-1 ?shape-2) (bind color-category ?color-4 red) (query ?target-4 ?target-object-1 ?attribute-6)))
  (comprehend "what is the size of the cube" :gold-standard-meaning '((get-context ?source-1) (filter ?target-1 ?source-1 ?shape-2) (unique ?target-object-1 ?target-1) (bind attribute-category ?attribute-6 size) (bind shape-category ?shape-2 cube) (query ?target-4 ?target-object-1 ?attribute-6)))
  (wi:add-element (make-html (categorial-network
                              *fcg-constructions*) :weights? t :colored-edges-0-1 t))
  (wi:add-element (make-html *fcg-constructions*)))


(defun test-repair-add-lexical-comprehension ()
  (set-up-cxn-inventory-and-repairs)

  (comprehend "what is the size of the red sphere" :gold-standard-meaning '((get-context ?source-1) (filter ?target-2 ?target-1 ?color-4) (unique ?target-object-1 ?target-2) (bind shape-category ?shape-4 sphere) (bind attribute-category ?attribute-6 size) (filter ?target-1 ?source-1 ?shape-4) (bind color-category ?color-4 red) (query ?target-4 ?target-object-1 ?attribute-6)))

  (comprehend "what is the size of the red cube" :gold-standard-meaning '((get-context ?source-1) (filter ?target-2 ?target-1 ?color-4) (unique ?target-object-1 ?target-2) (bind shape-category ?shape-2 cube) (bind attribute-category ?attribute-6 size) (filter ?target-1 ?source-1 ?shape-2) (bind color-category ?color-4 red) (query ?target-4 ?target-object-1 ?attribute-6)))

  (comprehend "what is the size of the red cylinder" :gold-standard-meaning '((get-context ?source-1) (filter ?target-2 ?target-1 ?color-4) (unique ?target-object-1 ?target-2) (bind shape-category ?shape-6 cylinder) (bind attribute-category ?attribute-6 size) (filter ?target-1 ?source-1 ?shape-6) (bind color-category ?color-4 red) (query ?target-4 ?target-object-1 ?attribute-6)))
  (wi:add-element (make-html (categorial-network
                              *fcg-constructions*) :weights? t :colored-edges-0-1 t))
  (wi:add-element (make-html *fcg-constructions*)))


(defun test-item-based+item-based->item-based-substitution-cxn ()  
  (set-up-cxn-inventory-and-repairs)

  (comprehend "what is the size of the red cube" :gold-standard-meaning '((get-context ?source-1) (filter ?target-2 ?target-1 ?color-4) (unique ?target-object-1 ?target-2) (bind shape-category ?shape-2 cube) (bind attribute-category ?attribute-6 size) (filter ?target-1 ?source-1 ?shape-2) (bind color-category ?color-4 red) (query ?target-4 ?target-object-1 ?attribute-6)))
  
  (comprehend "what is the size of the blue cube" :gold-standard-meaning '((get-context ?source-1) (filter ?target-2 ?target-1 ?color-6) (unique ?target-object-1 ?target-2) (bind shape-category ?shape-2 cube) (bind attribute-category ?attribute-6 size) (filter ?target-1 ?source-1 ?shape-2) (bind color-category ?color-6 blue) (query ?target-4 ?target-object-1 ?attribute-6)))

  (comprehend "what is the size of the yellow cube" :gold-standard-meaning '((get-context ?source-1) (filter ?target-2 ?target-1 ?color-16) (unique ?target-object-1 ?target-2) (bind shape-category ?shape-2 cube) (bind attribute-category ?attribute-6 size) (filter ?target-1 ?source-1 ?shape-2) (bind color-category ?color-16 yellow) (query ?target-4 ?target-object-1 ?attribute-6)))
  
  (comprehend "what is the size of the blue cylinder" :gold-standard-meaning
              '((get-context ?source-1) (filter ?target-2 ?target-1 ?color-6) (unique ?target-object-1 ?target-2) (bind shape-category ?shape-6 cylinder) (bind attribute-category ?attribute-6 size) (filter ?target-1 ?source-1 ?shape-6) (bind color-category ?color-6 blue) (query ?target-4 ?target-object-1 ?attribute-6)))
  (wi:add-element (make-html (categorial-network
                              *fcg-constructions*) :weights? t :colored-edges-0-1 t))
  (wi:add-element (make-html *fcg-constructions*)))

(defun test-repair-lexical->item-based-cxn-comprehension ()
  (set-up-cxn-inventory-and-repairs)
  
  (comprehend "the metal cube" :gold-standard-meaning '((get-context ?source)
                                                      (filter ?cubes ?source ?cube) 
                                                      (filter ?metal-cubes ?cubes ?metal)
                                                      (unique ?the-metal-cube ?metal-cubes)
                                                      (bind shape-category ?cube cube) 
                                                      (bind attribute-category ?metal metal)))
  
  (comprehend "the metal sphere" :gold-standard-meaning '((get-context ?source)
                                                      (filter ?spheres ?source ?sphere) 
                                                      (filter ?metal-spheres ?spheres ?metal)
                                                      (unique ?the-metal-sphere ?metal-spheres)
                                                      (bind shape-category ?sphere sphere) 
                                                      (bind attribute-category ?metal metal)))
  
  (comprehend "the cube" :gold-standard-meaning '((get-context ?source)
                                                  (filter ?cubes ?source ?cube) 
                                                  (unique ?the-cube ?cubes)
                                                  (bind shape-category ?cube cube)))

    
  (comprehend "what is the color of the metal sphere"
            :gold-standard-meaning '((get-context ?source)
                                     (bind attribute-category ?attribute color)
                                     (bind shape-category ?sphere sphere)
                                     (bind attribute-category ?metal metal)
                                     (unique ?object ?metal-spheres)
                                     (filter ?spheres ?source ?sphere)
                                     (filter ?metal-spheres ?spheres ?metal)
                                     (query ?response ?object ?attribute)))
    (wi:add-element (make-html (categorial-network
                              *fcg-constructions*) :weights? t :colored-edges-0-1 t))
  (wi:add-element (make-html *fcg-constructions*)))


;;
;; 1. learning from holophrases
;;

;; 1.1 substitution
;; (test-repair-holophrase-substitution-comprehension)

;; 1.2 deletion
;; (test-repair-holophrase-addition-comprehension)

;; 1.3 addition
;; (test-repair-holophrase-deletion-comprehension)

;; bootstrapping from lexical cxns
; (test-repair-lexical->item-based-cxn-comprehension)

;;
;; 2. learning from item-based cxns
;;

;; 2.1 add-lexical cxn
;; (test-repair-add-lexical-comprehension)

;; 2.2 item-based->item-based-substitution
;; (test-item-based+item-based->item-based-substitution-cxn)