(in-package :fcg)

(export '(scene-unit))

;;;; custom create-initial-structure for clevr-grammar
;;;; -------------------------------------------------
(defmethod create-initial-structure ((meaning list) (mode (eql :clevr-initial-structure)))
  (make-instance 'coupled-feature-structure
                 :left-pole `((root
			       (referent)
			       (meaning ,meaning)
			       (sem-cat nil)
                               (form nil)
                               (syn-cat nil)
                               (subunits (scene-unit)))
                              (scene-unit
                               (scene ,(make-var 'scene))))
		 :right-pole '((root))
		 :left-pole-domain 'sem
		 :right-pole-domain 'syn))
    

;;;; custom de-render for clevr-grammar
;;;; ----------------------------------
(defmethod de-render ((utterance string) (mode (eql :clevr-de-renderer))
                      &key scene-var &allow-other-keys)
  ;; 1. lowercase the utterace
  ;; 2. remove punctuation
  ;; 3. generate all meets contraints, but only precedes constraints with a scope of 3
  ;; 4. create a custom root unit that has a subunit 'scene-unit' with a feature structure 'scene: ?scene'
  "There were too many precedes constraints in the root, slowing down the processing for very large
   sentences. However, only a few cxns actually need 'precedes' and don't need 'long distance' precedes.
   To solve this, we constrain the generation of precedes constraints to only contain precedes within 3."
  (de-render (clevr-grammar-v2::preprocess-utterance utterance)
             :de-render-string-meets-precedes-within-3
             :scene-var scene-var))
             

(defmethod de-render ((utterance list) (mode (eql :de-render-string-meets-precedes-within-3))
                      &key scene-var &allow-other-keys)
  "There were too many precedes constraints in the root, slowing down the processing for very large
   sentences. However, only a few cxns actually need 'precedes' and don't need 'long distance' precedes.
   To solve this, we constrain the generation of precedes constraints to only contain precedes within 3."
  (let ((strings nil)
        (sequence nil)
	(constraints nil))
    (loop for string in utterance
          for i from 0
          for unit-name = (make-const string nil)
          do (progn
               (push unit-name sequence)
               (loop for prev in strings
                     for j from (1- (length strings)) downto 0
                     when (< (- i j) 4)
                     do (push `(precedes ,(second prev) ,unit-name) constraints))
               (push `(string ,unit-name ,string) strings)))
    (do ((left strings (rest left)))
	((null (cdr left)))
      (push `(meets ,(second (second left)) ,(second (first left)))
	    constraints))
    (let ((root-unit
           `(root (meaning ())
                  (sem-cat ())
                  (form ,(cons (cons 'sequence (reverse sequence))
                               (append strings constraints)))
                  (syn-cat ())
                  (subunits (scene-unit))))
          (scene-unit
           `(scene-unit
             (scene ,scene-var))))
      (make-instance 'coupled-feature-structure
                     :left-pole `(,root-unit
                                  ,scene-unit)
                     :right-pole '((root))))))