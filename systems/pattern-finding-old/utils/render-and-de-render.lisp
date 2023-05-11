(in-package :fcg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Render mode :generate-and-test  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod render ((node cip-node) (mode (eql :generate-and-test)) &key &allow-other-keys)
  (render (car-resulting-cfs (cipn-car node)) :generate-and-test))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; :de-render-string-meets-no-punct ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tokenize (utterance)
  "Remove the punctuation from the sentence. Split the utterance in words, downcase every word"
  (let ((words (split (remove-spurious-spaces (remove-punctuation utterance)) #\space)))
    (mapcar #'downcase words)))

(defmethod de-render ((utterance string) (mode (eql :de-render-string-meets-no-punct))
                      &key &allow-other-keys)
  (de-render (tokenize utterance) :de-render-string-meets-no-punct))

(defmethod de-render ((utterance list) (mode (eql :de-render-string-meets-no-punct))
                      &key &allow-other-keys)
  (if (stringp (first utterance))
    (de-render utterance :de-render-string-meets)
    (make-instance 'coupled-feature-structure 
       :left-pole `((root (meaning ())
                                      (sem-cat ())
                                      (form ,(instantiate-form-constraints utterance))
                                      (syn-cat ())))
       :right-pole '((root)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; :de-render-string-meets-ignore-quotes+full-stops ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun instantiate-form-constraints (list-of-form-constraints)
  (loop for fc in list-of-form-constraints
        collect (loop for el in fc
                      if (variable-p el)
                      collect (intern (subseq (symbol-name el) 1))
                      else
                      collect el)))

(defun remove-quotes+full-stops (utterance)
  (let ((words (split (remove-spurious-spaces utterance) #\space)))
    (loop for word in words
          unless (member word '("\"" ".") :test #'string=)
          collect (downcase word))))

(defmethod de-render ((utterance string) (mode (eql :de-render-string-meets-ignore-quotes+full-stops))
                      &key &allow-other-keys)
  (de-render (remove-quotes+full-stops utterance) :de-render-string-meets-ignore-quotes+full-stops))

(defmethod de-render ((utterance list) (mode (eql :de-render-string-meets-ignore-quotes+full-stops))
                      &key &allow-other-keys)
  (de-render utterance :de-render-string-meets))