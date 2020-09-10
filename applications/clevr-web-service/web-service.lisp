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
      (http-condition 400 "JSON input missing key(s): (~{~a~^, ~})"
                      missing-keys))
    (unless (stringp utterance)
      (http-condition 400 "utterance is of type ~a. Expected something of type string."
                      (type-of utterance)))
    (unless (or (string= irl-encoding "sexpr")
                (string= irl-encoding "json")
                (string= irl-encoding "rpn"))
      (http-condition 400 "Invalid irl-encoding specified: ~a. Expected 'sexpr, 'json' or 'rpn'."
                      irl-encoding))
    (multiple-value-bind (irl-program cipn)
        (handler-case (fcg:comprehend utterance :cxn-inventory *CLEVR*
                                      :silent t)
          (error (e)
            (http-condition 500 "Error in language processing module!" e)))
      (let ((fcg-status (first (statuses cipn))))
        (encode-json-alist-to-string
         `((:meaning . ,(when (and irl-program
                                   (eql fcg-status 'fcg::succeeded))
                          (cond
                           ((string= irl-encoding "sexpr")
                            (mkstr irl-program))
                           ((string= irl-encoding "json")
                            (encode-irl-program-as-json irl-program nil))
                           ((string= irl-encoding "rpn")
                            (encode-irl-program-as-rpn irl-program)))))
           (:fcg--status . ,(downcase (mkstr fcg-status)))
           (:applied--constructions . ,(when (applied-constructions cipn)
                                         (mapcar #'downcase
                                                 (mapcar #'mkstr
                                                         (mapcar #'fcg::name
                                                                 (applied-constructions cipn))))))))))))
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


;;;; Global variables to store the scenes
;;;; of the training and validation sets
(defvar *clevr-world-val* nil)
(defvar *clevr-world-train* nil)

;;;; !!!!!! IMPORTANT
;;;; Here, we overwrite the *clevr-data-path*, since this is different
;;;; on the penelope server. On this server, we simply store the
;;;; necessary CLEVR scenes (0-9) in the Babel folder directly.

(defun load-validation-set ()
  ;; !! we only change the *clevr-data-path* when we are on ccl and linux
  ;; (i.e. we are on the penelope server). This trick is obviously not
  ;; waterproof, so be warned...
  #+(and ccl linux) (setf *clevr-data-path*
                          (babel-pathname :directory '("CLEVR" "CLEVR-v1.0")))
  (let ((world (make-instance 'clevr-world :data-sets '("val"))))
    (setf *clevr-world-val* world)))

(defun load-training-set ()
  (setf *clevr-data-path*
        (babel-pathname :directory '("CLEVR" "CLEVR-v1.0")))
  (let ((world (make-instance 'clevr-world :data-sets '("train"))))
    (setf *clevr-world-train* world)))

;;;; /comprehend-and-execute route
(defun copy-and-intern-context (context)
  "Create a copy of the context, interning all symbols
   into the clevr package. This is needed for the irl-program
   evaluation to work."
  (loop with list-of-objects = nil
        for object in (objects context)
        for idx from 0
        do (push
            (make-instance 'clevr-object :id (id object)
                           :shape (intern (mkstr (shape object)) 'clevr-world)
                           :size (intern (mkstr (size object)) 'clevr-world)
                           :color (intern (mkstr (color object)) 'clevr-world)
                           :material (intern (mkstr (material object)) 'clevr-world)
                           :relationships (loop for (key . lists) in (relationships object)
                                                collect (cons (intern (mkstr key) 'clevr-world)
                                                              lists)))
            list-of-objects)
        finally
        (return (make-instance 'clevr-object-set :objects (reverse list-of-objects)))))

(defun process-scene-name (scene)
  ;; remove the type when present
  (if (find #\. scene)
    (first (split scene "."))
    scene))

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
    (unless (or *clevr-world-val* *clevr-world-train*)
      (http-condition 400 "The validation set nor the training set is loaded. Cannot execute comprehend-and-execute without scenes."))
    (setf scene (process-scene-name scene))
    ;; load the scene
    ;; do comprehension
    ;; evaluate irl-program
    ;; return the data
    (let* ((split (cond ((search "val" scene) *clevr-world-val*)
                        ((search "train" scene) *clevr-world-train*)))
           (context-filename (find scene (scenes split)
                                   :key (lambda (p) (pathname-name p))
                                   :test #'string=))
           (context (load-clevr-scene context-filename)))
      (unless context
        (http-condition 400 "Could not find scene ~a" scene))
      (multiple-value-bind (irl-program cipn)
          (handler-case
              (fcg:comprehend utterance :cnx-inventory *CLEVR*
                              :silent t)
            (error (e)
              (http-condition 500 "Error in language processing module!")))
        (let ((fcg-status (first (statuses cipn)))
              answers id-subs irl-evaluator)
          (when (eql fcg-status 'fcg::succeeded)
            ;; ccl requires to intern the symbols manually (why?)
            #+ccl (setf irl-program
                        (loop for predicate in irl-program
                           collect (loop for symbol in predicate
                                      collect (intern (mkstr symbol) 'clevr-world))))
            #+ccl (setf context (copy-and-intern-context context))
            (set-data *clevr-ontology* 'clevr-context context)
            (multiple-value-bind (solutions evaluator)
                (handler-case
                    (evaluate-irl-program irl-program *clevr-ontology*
                                          :primitive-inventory *clevr-primitives*)
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
              (setf irl-evaluator evaluator)))
          (encode-json-alist-to-string
           `((:meaning . ,(when (and irl-program
                                     (eql fcg-status 'fcg::succeeded))
                            (cond
                             ((string= irl-encoding "sexpr")
                              (mkstr irl-program id-subs))
                             ((string= irl-encoding "json")
                              (encode-irl-program-as-json irl-program id-subs (nodes irl-evaluator))))))
             (:fcg--status . ,(downcase (mkstr fcg-status)))
             (:applied--constructions . ,(when (applied-constructions cipn)
                                           (mapcar #'downcase
                                                   (mapcar #'mkstr
                                                           (mapcar #'fcg::name
                                                                   (applied-constructions cipn))))))
             (:irl--status . ,(if answers "succeeded" "failed"))
             (:solutions . ,(mapcar #'downcase answers)))))))))

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

