
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
                      (:seq2seq-model-formulation . "clevr_formulation_model_4")))

;; CLEVR grammar default configurations:
(set-configurations *CLEVR*
                   '((:cxn-supplier-mode . :ordered-by-label-hashed)
                      (:priority-mode . :nr-of-applied-cxns)
                      (:parse-order hashed nom cxn)
                      (:production-order hashed-lex nom cxn hashed-morph)
                      (:max-nr-of-nodes . 15000)))

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
                   when (and (subtypep (type-of (value binding)) 'attention)
                             (null (img-path (value binding))))
                   do (request-attn (get-data (ontology processor) 'hybrid-primitives::server-address)
                                    (value binding))))))

(defmethod irl::handle-chunk-composer-finished-event
           :before ((monitor monitor)
                    (monitor-id (eql 'irl::trace-irl))
                    (event-id (eql 'irl::chunk-composer-finished))
                    solutions composer)
  (when (monitors::active monitor)
    (loop for solution in solutions
          do (loop for binding in (irl::bindings solution)
                   when (and (subtypep (type-of (value binding)) 'attention)
                             (null (img-path (value binding))))
                   do (request-attn (get-data (ontology composer) 'hybrid-primitives::server-address)
                                    (value binding))))))


;; comprehend the question with the grammar, optionally  using seq2seq heuristics
;; execute the IRL network with the neural modules
;; compare the prediction with the gold answer
(defun evaluate-hybrid-irl-test ()
  (multiple-value-bind (scene question-set)
      (random-scene *CLEVR-val*)
      ;(get-scene-by-index *CLEVR-val* 0)
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
          (sleep 1)
          (add-element `((h3) ,(format nil "The ground truth answer is \"~a\""
                                       (downcase answer))))
          (add-element `((h3) ,(format nil "The computed answer is \"~a\""
                                       (downcase (answer->str computed-answer)))))
          (add-element `((h3) ,(if (compare-answers answer computed-answer)
                                 "Correct!" "Incorrect!")))
          (compare-answers answer computed-answer))))))

(defun show-image-on-wi (image-path)
  (let ((img-dst-path
         (make-pathname :directory `(:absolute "Users" ,(who-am-i) "Sites")
                        :name (pathname-name image-path)
                        :type (pathname-type image-path))))
    (copy-file image-path img-dst-path)
    (add-element '((h2) "Current Scene:"))
    (add-element `((img :src ,(mkstr cl-user::*localhost-user-dir*
                                     (pathname-name image-path)
                                     "." (pathname-type image-path)))))))

(defun hybrid-composer-test ()
  (multiple-value-bind (scene question-set) (random-scene *CLEVR-val*)
    (let* ((image-pathname (image scene))
           (image-name (format nil "~a.~a"
                               (pathname-name image-pathname)
                               (pathname-type image-pathname))))
      (load-image *neural-modules-server* image-name)
      (show-image-on-wi image-pathname)
      (let* ((random-type
              (random-elt '(shapes colors sizes materials booleans number)))
             (possible-target
              (if (eql random-type 'number)
                (random-elt (iota 11))
                (random-elt (find-data *clevr-ontology* random-type)))))
        (add-element '((h2) "Current target:"))
        (add-element (make-html possible-target))
        (let ((composer
              (make-chunk-composer
               :topic possible-target
               :initial-chunk (make-instance 'chunk :id 'initial
                                             :target-var `(?topic . ,(type-of possible-target))
                                             :open-vars `((?topic . ,(type-of possible-target))))
               :chunks (mapcar #'create-chunk-from-primitive
                               (list (find-primitive 'get-context *hybrid-primitives*)
                                     (find-primitive 'filter *hybrid-primitives*)
                                     (find-primitive 'query *hybrid-primitives*)
                                     (find-primitive 'count! *hybrid-primitives*)
                                     (find-primitive 'exist *hybrid-primitives*)
                                     (find-primitive 'unique *hybrid-primitives*)))
               :ontology *clevr-ontology* :primitive-inventory *hybrid-primitives*
               :configurations '((:max-search-depth . 8)))))
          (get-next-solutions composer))))))
             
;; here goes!
(activate-monitor trace-fcg)
(activate-monitor trace-irl)
(evaluate-hybrid-irl-test)

;; here goes!
(activate-monitor trace-fcg)
(activate-monitor trace-irl)
(hybrid-composer-test)

;; Try to execute an IRL program in a different direction
(evaluate-irl-program
 '((get-context ?context)
   (filter ?set-1 ?context ?thing-1)
   (unique ?obj-1 ?set-1)
   (query ?attribute ?obj-1 ?thing-2))
 *clevr-ontology*
 :primitive-inventory
 *hybrid-primitives*)

(evaluate-irl-program
 '((get-context ?context)
   (filter ?set-1 ?context ?thing-1)
   (unique ?obj-1 ?set-1)
   (query ?target ?obj-1 ?attribute)
   (bind attribute-category ?attribute color)
   (bind color-category ?target red))
 *clevr-ontology*
 :primitive-inventory
 *hybrid-primitives*)

(evaluate-irl-program
 '((get-context ?context)
   (filter ?set-1 ?context ?shape)
   (filter ?set-2 ?set-1 ?size)
   (unique ?obj-1 ?set-2)
   (query ?color ?obj-1 ?attribute)
   (bind shape-category ?shape cylinder)
   (bind size-category ?size small)
   (bind color-category ?color green)
   (bind attribute-category ?attribute color))
 *clevr-ontology*
 :primitive-inventory
 *hybrid-primitives*)
 

                    

         

