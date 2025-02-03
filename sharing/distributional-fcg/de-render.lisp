(in-package :fcg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                             ;;
;; De-render methods for distributional representations in FCG ;;
;;                                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod de-render ((utterance string) (mode (eql :de-render-token-embeddings)) &key cxn-inventory &allow-other-keys)
  "Retrieves tokens and embeddings for string, creating one unit per token."
  (multiple-value-bind (units embedding-data)
      (loop with token-embeddings = (nlp-tools:get-word-embeddings (mapcar #'downcase (cl-ppcre:split "[ .-]"  utterance)))
            for (token embedding) in token-embeddings
            for unit-name = (make-id token)
            for embedding-pointer = (intern (upcase (string-append "->" token)))
            collect (cons embedding-pointer embedding)
              into embedding-data
            collect (make-unit :name unit-name
                               :features `((token ((string ,token)
                                                   (embedding ,embedding-pointer)))))
              into units
            finally (return (values units embedding-data)))
    ;; adjecency-constraints
    (let ((adjacency-constraints (loop for (unit-name . rest) on (mapcar #'first units)
                                       when rest
                                         collect `(adjacent ,unit-name ,(first rest)))))
      (set-data (blackboard cxn-inventory) :ts-token-embeddings embedding-data)
      (make-instance 'coupled-feature-structure
                     :left-pole `((root (meaning ())
                                        (sem-cat ())
                                        (form ,adjacency-constraints)
                                        (syn-cat ()))
                                  ,@units)
                     :right-pole '((root))))))