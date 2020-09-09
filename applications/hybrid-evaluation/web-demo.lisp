
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


;; comprehend the question with the grammar, optionally  using seq2seq heuristics
;; execute the IRL network with the neural modules
;; compare the prediction with the gold answer
(defun run-random-example ()
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
               (solutions (evaluate-irl-program meaning *clevr-ontology*))
               (computed-answer (when solutions
                                  (value (find target-var
                                               (first solutions)
                                               :key #'var)))))
          (values answer (answer->str computed-answer)))))))

;; here goes!
(activate-monitor trace-fcg)
(activate-monitor trace-irl-in-web-browser)
(main)

;; Try to execute an IRL program in a different direction


;; HEADER
;#########

;; Larger font for text in <p> tags
(define-css 'main "p {font-size: 11pt}")

(defun header ()
  (clear-page)
  (deactivate-all-monitors)
  (activate-monitor trace-fcg)
  (activate-monitor trace-irl-in-web-browser)
  (add-element '((hr)))
  (add-element
   '((h1) "Hybrid Semantic Representation"))
  (add-element '((p) "This web demonstration showcases the integration of the Hybrid Semantic Representation in the Incremental Recruitment Language (IRL). The symbolic and subsymbolic modules of the Hybrid Semantic Representation have been made available through a web service. These are integrated into IRL by implementing the various primitive operations as web requests to this service. Through this integration, we unlock the full potention of the Hybrid Semantic Representation, in particular the ability to execute the semantic network in all directions. This will be demonstrated later on.")))

;(header)

(defun section-1 ()
  (add-element '((h2 :id "section-1") "I. Basic Example"))
  (add-element '((p) "First, we show the use of the Hybrid Semantic Representation in its most basic form. This process involves 2 steps: comprehending a question from the CLEVR dataset using the CLEVR grammar and executing the resulting meaning network with IRL using the symbolic and subsymbolic modules."))
  (multiple-value-bind (answer computed-answer)
      (run-random-example)
    (add-element `((h3) ,(format nil "The ground truth answer is \"~a\""
                                 (downcase answer))))
    (add-element `((h3) ,(format nil "The computed answer is \"~a\""
                                 (downcase computed-answer))))
    (add-element `((h3) ,(if (compare-answers answer computed-answer)
                          "Correct!" "Incorrect!")))))

;(section-1)

(defun section-2 ()
  (add-element '((h2 :id "section-2") "II. Multidirectionality"))
  (add-element '((p) "IRL offers the possibility for the primitive operations to be implemented in different directions. This is also extended to the Hybrid Semantic Representation. This way, the symbolic and subsymbolic modules can be used for multi-agent experiments using IRL's composer or flexible interpretation mechanisms."))
  (evaluate-irl-program
   '((get-context ?context)
     (filter ?set-1 ?context ?thing-1)
     (unique ?obj-1 ?set-1)
     (query ?attribute ?obj-1 ?thing-2))
   *clevr-ontology*)
  (add-element '((p) "This IRL program returns 24 possible solutions by varying the categories used for the 'filter' primtive and the attribute on which the 'query' primitive operates.")))

;(section-2)

(defun full-demo ()
  (header)
  (section-1)
  (section-2)
  (add-element '((h3) ((i) "The End"))))

;(full-demo)
  

                    

         

