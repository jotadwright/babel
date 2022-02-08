(in-package :fcg)

;;;; custom de-render for clevr-grammar
;;;; ----------------------------------
(defmethod de-render ((utterance string) (mode (eql :de-render-string-meets-precedes-within-3))
                      &key &allow-other-keys)
  "There were too many precedes constraints in the root, slowing down the processing for very large
   sentences. However, only a few cxns actually need 'precedes' and don't need 'long distance' precedes.
   To solve this, we constrain the generation of precedes constraints to only contain precedes within 3."
  (de-render (clevr-grammar::preprocess-utterance utterance) :de-render-string-meets-precedes-within-3))

(defmethod de-render ((utterance list) (mode (eql :de-render-string-meets-precedes-within-3))
                      &key &allow-other-keys)
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
    (make-instance 'coupled-feature-structure 
		   :left-pole `((root (meaning ())
                                      (sem-cat ())
                                      (form ,(cons (cons 'sequence (reverse sequence))
                                                   (append strings constraints)))
                                      (syn-cat ())))
		   :right-pole '((root)))))