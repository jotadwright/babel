;;;; web-service.lisp

(in-package :clevr-web-service)

;;;; helper functions
(defun keys-present-p (json &rest keys)
  "Check if all keys are present in the given
   json object."
  (let (missing-keys)
    (loop for key in keys
          unless (assoc key json)
          do (push key missing-keys))
    missing-keys))

(defmethod explain-condition ((condition http-condition)
                              resource
                              ct)
  (encode-json-alist-to-string
   `((:status--code . ,(format nil "~a" (status-code condition)))
     (:details . ,(apply #'format nil (simple-condition-format-control condition)
                         (simple-condition-format-arguments condition))))))

;;;; /comprehend route
(defun handle-comprehend-route (json)
  (let* ((missing-keys (keys-present-p json :utterance))
         (utterance (rest (assoc :utterance json)))
         (irl-encoding (if (assoc :irl--encoding json)
                         (downcase (rest (assoc :irl--encoding json)))
                         "sexpr")))                
    (when missing-keys
      (http-condition 400 "JSON input missing key(s): (~{~a~^, ~})" missing-keys))
    (unless (stringp utterance)
      (http-condition 400 "utterance is of type ~a. Expected something of type string." (type-of utterance)))
    (unless (or (string= irl-encoding "sexpr")
                (string= irl-encoding "json"))
      (http-condition 400 "Invalid irl-encoding specified: ~a. Expected 'sexpr' or 'json'." irl-encoding))
    (multiple-value-bind (irl-program cipn)
        (handler-case (fcg:comprehend (preprocess-sentence utterance)
                                      :cxn-inventory *CLEVR*
                                      :silent t)
          (error (e)
            (http-condition 500 "Error in language processing module!" e)))
      (encode-json-alist-to-string
       `((:meaning . ,(when irl-program
                        (cond
                          ((string= irl-encoding "sexpr")
                           (mkstr irl-program))
                          ((string= irl-encoding "json")
                           (encode-irl-program irl-program nil)))))
         (:fcg--status . ,(downcase (mkstr (first (statuses cipn)))))
         (:applied--constructions . ,(when (applied-constructions cipn)
                                       (mapcar #'downcase
                                               (mapcar #'mkstr
                                                       (mapcar #'fcg::name
                                                               (applied-constructions cipn)))))))))))
(defroute comprehend (:post :application/json)
 (handle-comprehend-route
   (handler-case
       (decode-json-from-string
        (payload-as-string))
     (error (e)
       (http-condition 400 "Malformed JSON")))))
         
(defroute comprehend (:post :text/plain)
  (handle-comprehend-route
   (handler-case
       (decode-json-from-string
        (payload-as-string))
     (error (e)
       (http-condition 400 "Malformed JSON")))))

;;;; /comprehend-all route
(defroute comprehend-all (:post :application/json)
  (let* ((json
          (handler-case
              (decode-json-from-string
               (payload-as-string))
            (error (e)
              (http-condition 400 "Malformed JSON"))))
         (missing-keys (keys-present-p json :utterance))
         (utterance (rest (assoc :utterance json)))
         (n (when (assoc :n json) (rest (assoc :n json))))
         (irl-encoding (if (assoc :irl--encoding json)
                         (downcase (rest (assoc :irl--encoding json)))
                         "sexpr")))
    (when missing-keys
      (http-condition 400 "JSON input missing key(s): (~{~a~^, ~})" missing-keys))
    (unless (stringp utterance)
      (http-condition 400 "utterance is of type ~a. Expected something of type string." (type-of utterance)))
    (unless (or (string= irl-encoding "sexpr")
                (string= irl-encoding "json"))
      (http-condition 400 "Invalid irl-encoding specified: ~a. Expected 'sexpr' or 'json'." irl-encoding))
    (multiple-value-bind (irl-programs cipns)
        (handler-case (fcg:comprehend-all (preprocess-sentence utterance)
                                          :cxn-inventory *CLEVR*
                                          :silent t
                                          :n n)
          (error (e)
            (http-condition 500 "Error in language processing module!" e)))
      (encode-json-to-string
       (loop for irl-program in irl-programs
             for cipn in cipns
             collect `((:meaning . ,(when irl-program
                                      (cond
                                        ((string= irl-encoding "sexpr")
                                         (mkstr irl-program))
                                        ((string= irl-encoding "json")
                                         (encode-irl-program irl-program nil)))))
                       (:fcg--status . ,(mkstr (first (statuses cipn))))
                       (:applied--constructions . ,(when (applied-constructions cipn)
                                                     (mapcar #'downcase
                                                             (mapcar #'mkstr
                                                                     (mapcar #'fcg::name
                                                                             (applied-constructions cipn))))))))))))

;;;; /formulate route
(defroute formulate (:post :application/json)
  (let* ((json
          (handler-case
              (decode-json-from-string
               (payload-as-string))
            (error (e)
              (http-condition 400 "Malformed JSON"))))
         (missing-keys (keys-present-p json :meaning))
         (irl-encoding (if (assoc :irl--encoding json)
                         (downcase (rest (assoc :irl--encoding json)))
                         "sexpr"))
         (irl-program (cond
                       ((string= irl-encoding "sexpr")
                        (read-from-string (rest (assoc :meaning json))))
                       ((string= irl-encoding "json")
                        (decode-irl-program
                         (rest (assoc :meaning json)))))))
    (when missing-keys
      (http-condition 400 "JSON input missing key(s): (~{~a~^, ~})" missing-keys))
    (unless (or (string= irl-encoding "sexpr")
                (string= irl-encoding "json"))
      (http-condition 400 "Invalid irl-encoding specified: ~a. Expected 'sexpr' or 'json'." irl-encoding))
    ;; ccl requires to intern the symbols manually (why?)
    #+ccl (setf irl-program
                (loop for predicate in irl-program
                   collect (loop for symbol in predicate
                              collect (intern (mkstr symbol) 'clevr))))
    (handler-case
        (unless (irl-program-p irl-program)
          (http-condition 400 "Invalid IRL program: (~a)" irl-program))
      (error (e)
        (http-condition 400 "Unbound symbol in IRL program: (~a)" irl-program)))
    (multiple-value-bind (utterance cipn)
        (handler-case (fcg:formulate irl-program
                                     :cxn-inventory *CLEVR*
                                     :silent t)
          (error (e)
            (http-condition 500 "Error in language processing module!" e)))
      (encode-json-alist-to-string
       `((:utterance . ,(when utterance (mkstr (list-of-strings->string utterance))))
         (:fcg--status . ,(downcase (mkstr (first (statuses cipn)))))
         (:applied--constructions . ,(when (applied-constructions cipn)
                                       (mapcar #'downcase
                                               (mapcar #'mkstr
                                                       (mapcar #'fcg::name
                                                               (applied-constructions cipn)))))))))))

;;;; /formulate-all route
(defroute formulate-all (:post :application/json)
  (let* ((json (handler-case
                   (decode-json-from-string
                    (payload-as-string))
                 (error (e)
                   (http-condition 400 "Malformed JSON"))))
         (missing-keys (keys-present-p json :meaning))
         (n (when (assoc :n json) (rest (assoc :n json))))
         (irl-encoding (if (assoc :irl--encoding json)
                         (downcase (rest (assoc :irl--encoding json)))
                         "sexpr"))
         (irl-program (cond
                    ((string= irl-encoding "sexpr")
                     (read-from-string (rest (assoc :meaning json))))
                    ((string= irl-encoding "json")
                     (decode-irl-program
                      (rest (assoc :meaning json)))))))
    (when missing-keys
      (http-condition 400 "JSON input missing key(s): (~{~a~^, ~})" missing-keys))
    (unless (or (string= irl-encoding "sexpr")
                (string= irl-encoding "json"))
      (http-condition 400 "Invalid irl-encoding specified: ~a. Expected 'sexpr' or 'json'." irl-encoding))
    ;; ccl requires to intern the symbols manually (why?)
    #+ccl (setf irl-program
                (loop for predicate in irl-program
                   collect (loop for symbol in predicate
                              collect (intern (mkstr symbol) 'clevr))))
    (handler-case
        (unless (irl-program-p irl-program)
          (http-condition 400 "Invalid IRL program: (~a)" irl-program))
      (error (e)
        (http-condition 400 "Unbound symbol in IRL program: (~a)" irl-program)))
    (multiple-value-bind (utterances cipns)
        (handler-case (fcg:formulate-all irl-program
                                         :cxn-inventory *CLEVR*
                                         :silent t
                                         :n n)
          (error (e)
            (http-condition 500 "Error in language processing module!" e)))
      (encode-json-to-string
       (loop for utterance in utterances
             for cipn in cipns
             collect `((:utterance . ,(when utterance (mkstr (list-of-strings->string utterance))))
                       (:fcg--status . ,(downcase (mkstr (first (statuses cipn)))))
                       (:applied--constructions . ,(when (applied-constructions cipn)
                                                     (mapcar #'downcase
                                                             (mapcar #'mkstr
                                                                     (mapcar #'fcg::name
                                                                             (applied-constructions cipn))))))))))))

;;;; /comprehend-and-formulate route
(defroute comprehend-and-formulate (:post :application/json)
  (let* ((json (handler-case
                   (decode-json-from-string
                    (payload-as-string))
                 (error (e)
                   (http-condition 400 "Malformed JSON"))))
         (missing-keys (keys-present-p json :utterance))
         (utterance (rest (assoc :utterance json))))
    (when missing-keys
      (http-condition 400 "JSON input missing key(s): (~{~a~^, ~})" missing-keys))
    (unless (stringp utterance)
      (http-condition 400 "utterance is of type ~a. Expected something of type string." (type-of utterance)))
    (multiple-value-bind (utterance cipn)
        (handler-case (fcg:comprehend-and-formulate (preprocess-sentence utterance)
                                                    :cxn-inventory *CLEVR*
                                                    :silent t)
          (error (e)
            (http-condition 500 "Error in language processing module!" e)))
      (encode-json-alist-to-string
       `((:utterance . ,(when utterance (mkstr (list-of-strings->string utterance))))
         (:fcg--status . ,(downcase (mkstr (first (statuses cipn)))))
         (:applied--constructions . ,(when (applied-constructions cipn)
                                       (mapcar #'downcase
                                               (mapcar #'mkstr
                                                       (mapcar #'fcg::name
                                                               (applied-constructions cipn)))))))))))

;;;; Global variables to store the scenes
;;;; of the training and validation sets
(defvar *train-scenes* nil)
(defvar *val-scenes* nil)

(defun load-validation-set ()
  (let ((path 
           (merge-pathnames (make-pathname :directory '(:relative "CLEVR" "CLEVR-v1.0" "scenes")
                                           :name "CLEVR_val_full_per_line" :type "json")
                            cl-user:*babel-corpora*)))
      (setf *val-scenes*
            (read-contexts-from-file path))))

(defun load-training-set ()
  (let ((path 
           (merge-pathnames (make-pathname :directory '(:relative "CLEVR" "CLEVR-v1.0" "scenes")
                                           :name "CLEVR_train_full_per_line" :type "json")
                            cl-user:*babel-corpora*)))
      (setf *train-scenes*
            (read-contexts-from-file path))))

;;;; /scenes route  
(defroute scenes (:get :application/json)
  ;; get all scene names and return them
  (encode-json-alist-to-string
   `((:scenes . ,(append (mapcar #'image-filename *train-scenes*)
                         (mapcar #'image-filename *val-scenes*))))))

;;;; /comprehend-and-execute route
(defun copy-and-intern-context (context)
  "Create a copy of the context, interning all symbols
   into the clevr package. This is needed for the irl-program
   evaluation to work."
  (loop with list-of-objects = nil
     for object in (clevr::objects context)
     for idx from 0
     do (push
         (make-instance 'clevr::clevr-object :id (id object)
                        :shape (intern (mkstr (clevr::shape object)) 'clevr)
                        :size (intern (mkstr (clevr::size object)) 'clevr)
                        :color (intern (mkstr (clevr::color object)) 'clevr)
                        :material (intern (mkstr (clevr::material object)) 'clevr)
                        :relationships (loop for (key . lists) in (clevr::relationships object)
                                          collect (cons (intern (mkstr key) 'clevr)
                                                        lists)))
         list-of-objects)
     finally
       (return (make-instance 'clevr::clevr-object-set
                              :objects list-of-objects))))

(defun handle-comprehend-and-execute-route (json)
  (let* ((missing-keys (keys-present-p json :utterance :scene))
         (utterance (rest (assoc :utterance json)))
         (scene (rest (assoc :scene json)))
         (irl-encoding (if (assoc :irl--encoding json)
                         (downcase (rest (assoc :irl--encoding json)))
                         "sexpr")))
    (when missing-keys
      (http-condition 400 "JSON input missing key(s): (~{~a~^, ~})" missing-keys))
    (unless (stringp utterance)
      (http-condition 400 "utterance is of type ~a. Expected something of type string." (type-of utterance)))
    (unless (stringp scene)
      (http-condition 400 "scene is of type ~a. Expected something of type string." (type-of scene)))
    (unless (or (string= irl-encoding "sexpr")
                (string= irl-encoding "json"))
      (http-condition 400 "Invalid irl-encoding specified: ~a. Expected 'sexpr' or 'json'." irl-encoding))
    ;; add .png to scene name if needed
    (unless (find #\. scene)
      (setf scene (format nil "~a.png" scene)))
    ;; load the scene
    ;; do comprehension
    ;; evaluate irl-program
    ;; return the data
    (let* ((split (cond ((search "val" scene) *val-scenes*)
                        ((search "train" scene) *train-scenes*)))
           (context (find scene split :key #'image-filename :test #'string=)))
      (unless context
        (http-condition 400 "Could not find scene ~a" scene))
      (multiple-value-bind (irl-program cipn)
          (handler-case
              (fcg:comprehend (preprocess-sentence utterance)
                              :cnx-inventory *CLEVR*
                              :silent t)
            (error (e)
              (http-condition 500 "Error in language processing module!")))
        (let (answers id-subs)
          (when (eql (first (statuses cipn)) 'fcg::succeeded)
            ;; ccl requires to intern the symbols manually (why?)
            #+ccl (setf irl-program
                        (loop for predicate in irl-program
                           collect (loop for symbol in predicate
                                      collect (intern (mkstr symbol) 'clevr))))
            #+ccl (setf context (copy-and-intern-context context))
            (set-data *clevr-ontology* 'clevr:clevr-context context)
            (multiple-value-bind (solutions evaluator)
                (handler-case
                    (evaluate-irl-program irl-program *clevr-ontology*)
                  (error (e)
                    (http-condition 500 "Error in execution module!")))
              (setf answers
                    (loop for solution in solutions
                       collect (answer->str (get-target-value irl-program solution))))
              (setf id-subs
                    (loop for object in (objects context)
                          for i from 0
                          collect (cons (id object)
                                        (format nil "obj-~a" i))))
              (encode-json-alist-to-string
               `((:meaning . ,(when irl-program
                                (cond
                                 ((string= irl-encoding "sexpr")
                                  (mkstr irl-program id-subs))
                                 ((string= irl-encoding "json")
                                  (encode-irl-program irl-program id-subs (nodes evaluator))))))
                 (:fcg--status . ,(downcase (mkstr (first (statuses cipn)))))
                 (:applied--constructions . ,(when (applied-constructions cipn)
                                               (mapcar #'downcase
                                                       (mapcar #'mkstr
                                                               (mapcar #'fcg::name
                                                                       (applied-constructions cipn))))))
                 (:irl--status . ,(if answers "succeeded" "failed"))
                 (:solutions . ,(mapcar #'downcase answers)))))))))))

(defroute comprehend-and-execute (:post :application/json)
  (handle-comprehend-and-execute-route
   (handler-case
       (decode-json-from-string
        (payload-as-string))
     (error (e)
       (http-condition 400 "Malformed JSON")))))

(defroute comprehend-and-execute (:post :text/plain)
  (handle-comprehend-and-execute-route
   (handler-case
       (decode-json-from-string
        (payload-as-string))
     (error (e)
       (http-condition 400 "Malformed JSON")))))
    

;; Test the web interface on localhost:9003 or https://penelope.vub.be/clevr-api/ (may not contain the latest version)

;; curl -H "Content-Type: text/plain" -d '{"utterance" : "How many things are cubes or spheres?", "irl_encoding": "json"}' http://localhost:9003/comprehend

;; curl -H "Content-Type: application/json" -d '{"meaning" : "((GET-CONTEXT ?SOURCE-1153) (FILTER ?TARGET-2594 ?TARGET-2593 ?COLOR-205) (BIND SHAPE-CATEGORY ?SHAPE-175 CUBE) (FILTER ?TARGET-2593 ?SOURCE-1153 ?SHAPE-175) (BIND COLOR-CATEGORY ?COLOR-205 RED) (COUNT! ?TARGET-2681 ?TARGET-2594))"}' http://localhost:9003/formulate

;; curl -H "Content-Type: application/json" -d '{"meaning":[{"name":"get-context", "arity":0},{"name":"filter", "arity":1, "arg":"cube"}, {"name":"filter", "arity":1, "arg":"red"}, {"name":"count!", "arity":1}], "irl_encoding":"json"}' http://localhost:9003/formulate 

;; curl -H "Content-Type: application/json" -d '{"utterance" : "How many red cubes are there?"}' http://localhost:9003/comprehend-and-formulate

;; curl -H "Content-Type: text/plain" -d '{"utterance": "How many things are cubes or spheres?", "scene": "CLEVR_val_000000", "irl_encoding": "json"}' http://localhost:9003/comprehend-and-execute


