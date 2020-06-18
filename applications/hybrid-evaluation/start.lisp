
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
                      (:seq2seq-endpoint . "http://localhost:9999/next-cxn")))

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
    (category (mkstr (category-value answer-value)))))

(defun compare-answers (gold-answer prediction)
  (string= (upcase (answer->str gold-answer))
           (upcase (answer->str prediction))))

(defmethod irl::handle-evaluate-irl-program-finished-event :before ((monitor monitor)
                                                                    (monitor-id (eql 'irl::trace-irl-in-web-browser))
                                                                    (event-id (eql 'irl::evaluate-irl-program-finished))
                                                                    ontology
                                                                    solutions
                                                                    evaluation-tree)
  ;; when the monitor is active
  ;; download all attention images
  ;; also check if the slot is bound
  ;; such that the same attention is not downloaded twice
  (when (monitors::active monitor)
    (loop for solution in solutions
          do (loop for binding in solution
                   when (and (eql (type-of (value binding)) 'attention)
                             (null (img-path (value binding))))
                   do (request-attn (get-data ontology 'hybrid-primitives::server-address)
                                   (value binding))))))


;; comprehend the question with the grammar, using seq2seq heuristics
;; execute the IRL network with the neural modules
;; compare the prediction with the gold answer
(defun main ()
  (multiple-value-bind (scene question-set)
      (random-scene *CLEVR-val*)
    (let* ((image-pathname (image scene))
           (image-name (format nil "~a.~a"
                               (pathname-name image-pathname)
                               (pathname-type image-pathname))))
      (load-image *neural-modules-server* image-name)
      (loop for entry in (questions question-set)
            collect (with-slots (question answer) entry
                      (let* ((meaning (comprehend question :cxn-inventory *CLEVR*))
                             (target-var (get-target-var meaning))
                             (solutions (evaluate-irl-program meaning *clevr-ontology*))
                             (computed-answer (when solutions
                                                (value (find target-var
                                                             (first solutions)
                                                             :key #'var)))))
                        (compare-answers answer computed-answer)))))))

;; here goes!
(activate-monitor trace-fcg)
(activate-monitor trace-irl-in-web-browser)
(main)

                    

         

