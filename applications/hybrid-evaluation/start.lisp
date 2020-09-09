
(ql:quickload :hybrid-evaluation)
(in-package :hybrid-evaluation)

;; load the CLEVR scenes and questions
(defparameter *CLEVR-val*
  (make-instance 'clevr-world :data-sets '("val")
                 :load-questions t))

;; CLEVR grammar seq2seq configurations:
(set-configurations *CLEVR*
                    '((:cxn-supplier-mode . :hashed+seq2seq-heuristic)
                      (:priority-mode . :seq2seq-heuristic-additive)
                      (:seq2seq-endpoint . "http://localhost:9999/next-cxn")
                      (:seq2seq-model-formulation . "clevr_formulation_model_3")))

;; CLEVR grammar default configurations:
(set-configurations *CLEVR*
                   '((:cxn-supplier-mode . :hashed-simple-queue)
                     (:priority-mode . :priming)))

;; Set the address of the neural modules server
;; and store it in the ontology so all primitives
;; can access it
(defparameter *neural-modules-server* "http://localhost:8888/")
(set-data *clevr-ontology*
          'hybrid-primitives::server-address
          *neural-modules-server*)


;; some helper functions
(defun answer->str (answer-value)
  (typecase answer-value
    (string answer-value)
    (number (mkstr answer-value))
    (fixnum (mkstr answer-value))
    (integer (mkstr answer-value))
    (bit (mkstr answer-value))
    (boolean-category (mkstr (id answer-value)))
    (category (mkstr (category-value answer-value)))))

(defun compare-answers (gold-answer prediction)
  (string= (upcase (answer->str gold-answer))
           (upcase (answer->str prediction))))

(defmethod irl::handle-evaluate-irl-program-finished-event
           :before ((monitor monitor)
                    (monitor-id (eql 'irl::trace-irl))
                    (event-id (eql 'irl::evaluate-irl-program-finished))
                    solutions solution-nodes
                    processor primitive-inventory)
  ;; when the monitor is active
  ;; download all attention images
  ;; also check if the slot is bound
  ;; such that the same attention is not downloaded twice
  (when (monitors::active monitor)
    (loop for solution in solutions
          do (loop for binding in solution
                   when (and (eql (type-of (value binding)) 'attention)
                             (null (img-path (value binding))))
                   do (request-attn (get-data (ontology processor) 'hybrid-primitives::server-address)
                                    (value binding))))))


;; comprehend the question with the grammar, optionally  using seq2seq heuristics
;; execute the IRL network with the neural modules
;; compare the prediction with the gold answer
(defun main ()
  (multiple-value-bind (scene question-set)
      (get-scene-by-index *CLEVR-val* 0)
    (let* ((image-pathname (image scene))
           (image-name (format nil "~a.~a"
                               (pathname-name image-pathname)
                               (pathname-type image-pathname)))
           (idx (random-elt '(0 1 2 3 4 5)))
           (random-question (nth idx (questions question-set))))
      (load-image *neural-modules-server* image-name)
      (with-slots (question answer) random-question
        (let* ((meaning (comprehend question :cxn-inventory *CLEVR*))
               (target-var (get-target-var meaning))
               (solutions (evaluate-irl-program meaning *clevr-ontology*
                                                :primitive-inventory *hybrid-primitives*))
               (computed-answer (when solutions
                                  (value (find target-var
                                               (first solutions)
                                               :key #'var)))))
          (compare-answers answer computed-answer))))))

;; here goes!
(activate-monitor trace-fcg)
(activate-monitor trace-irl)
(main)

;; Try to execute an IRL program in a different direction
(evaluate-irl-program
 '((get-context ?context)
   (filter ?set-1 ?context ?thing-1)
   (unique ?obj-1 ?set-1)
   (query ?attribute ?obj-1 ?thing-2))
 *clevr-ontology*
 :primitive-inventory
 *hybrid-primitives*)

                    

         

