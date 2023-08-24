(in-package :cle)

;; ---------------------------
;; + Web monitor experiments +
;; ---------------------------

(defun read-scene-ids (fname)
  (let* ((base-dir "~/Projects/babel/experiments/concept-emergence2/data/")
         (fpath (concatenate 'string base-dir fname))
         (raw (uiop:read-file-lines fpath))
         (scene-ids (map 'list #'parse-integer raw)))
    scene-ids))

(defun parse-keyword (string)
  (intern (string-upcase (string-left-trim ":" string)) :keyword))

(defun store-experiment (experiment)
  (let* ((experiment-name (get-configuration experiment :experiment-name))
         (output-dir (get-configuration experiment :output-dir))
         (current-stage (get-configuration experiment :current-stage))
         (path (babel-pathname
                :directory `("experiments" "concept-emergence2" "logging" ,(downcase output-dir) ,(downcase experiment-name) "stores")
                :name (list-of-strings->string (list (write-to-string (series-number experiment))
                                                     "history"
                                                     "stage"
                                                     (write-to-string current-stage))
                                               :separator "-") :type "store"))
         (tmp-world (copy-object (world experiment))))
    (ensure-directories-exist path)
    (setf (world experiment) nil)
    (cl-store:store experiment path)
    (setf (world experiment) tmp-world)))

(defun fix-configuration (experiment)
  "Method to fix configurations of previous experiment without make-configuration and switch condition."
  (setf (configuration experiment)
        (configuration (make-configuration :entries (configuration experiment))))
  (set-configuration experiment :switch-condition :none))

(defun find-agent (id experiment)
  "Given an integer id, returns the associated agent"
  (let ((agent (loop for agent in (agents experiment)
                     for found-id = (second (split-sequence:split-sequence #\- (mkstr (id agent))))
                       do (when (equal (mkstr id) found-id)
                         (return agent)))))
    agent))

(defun list-to-hash-table (lst &key (key #'identity))
  "Creates a hash table given a list."
  (loop with tbl = (make-hash-table)
        for el in lst
        do (setf (gethash (funcall key el) tbl) el)
        finally (return tbl)))

(defun create-configurations (parameters)
  "Generate a set of experiments. Specify which parameters are variable
   and what their possible values can be. Optionally specify an a-list
   of shared configurations."
  (let* ((pairs (loop for (key . values) in parameters
                      collect (loop for value in values
                                    collect (cons key value))))
         (configurations (apply #'combinations pairs)))
    configurations))

(defun calculate-amount-of-variations (parameters)
  (length (create-configurations parameters)))

(defun generate-csv-for-tuning (filename exp-prefix default-config tuned-params)
  (with-open-file (str (namestring (merge-pathnames (format nil "~a.csv" filename)
                                                    (asdf:system-relative-pathname "cle" "batch/data/")
                                                    ))
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)

    (loop for (key . def-val) in default-config and i from 1
          if (< i (length default-config))
            do (format str "~a" (replace-char (format nil "~(~a~)," key) #\- #\_))
          else
            do (format str "~a" (replace-char (format nil "~(~a~)" key) #\- #\_)))
    (loop for config in (create-configurations tuned-params) and i from 1
          do (loop for (key . def-val) in default-config and j from 1
                   for found = (assoc key config)
                   if (< j (length default-config))
                     do (cond ((eq key :id)
                               (format str "~%~a," i))
                              ((eq key :exp-name)
                               (format str "~a-~a," exp-prefix i))
                              (found
                               (cond ((keywordp (assqv key config))
                                      (format str "~(~s~)," (assqv key config)))
                                     (t
                                      (format str "~a," (assqv key config)))))
                              (t
                               (cond ((keywordp def-val)
                                      (format str "~(~s~)," def-val))
                                     (t
                                      (format str "~a," def-val)))))
                   else
                     do (cond ((eq key :id)
                               (format str "~%~a" i))
                              ((eq key :exp-name)
                               (format str "~a-~a" exp-prefix i))
                              (found
                               (cond ((keywordp (assqv key config))
                                      (format str "~(~s~)" (assqv key config)))
                                     (t
                                      (format str "~a" (assqv key config)))))
                              (t
                               (cond ((keywordp def-val)
                                      (format str "~(~s~)" def-val))
                                     (t
                                      (format str "~a" def-val)))))))))
            
#|(defun find-experiment-dir (base-dir exp-number)
  "Finds the path to the directory of an experiment." 
  (let* ((experiment-directories (uiop:subdirectories (asdf:system-relative-pathname "cle" (format nil "storage/~a/experiments/" "similarity"))))
         (exp-dir (loop for exp-dir in experiment-directories
                        for found-exp-number = (parse-integer (last-elt (split-sequence:split-sequence #\- (last-elt (pathname-directory exp-dir)))))
                        when (equal exp-number found-exp-number)
                          do (loop-finish)
                        finally
                          (return exp-dir))))
    exp-dir))|#

(defun load-experiment (store-dir &key (name "history"))
  "Loads and returns the store object in the given directory." 
  (let ((store-path (merge-pathnames (make-pathname :name name :type "store")
                                     store-dir)))
    (cl-store:restore store-path)))     

#|(generate-csv-for-tuning "tuning2"
                         "tune-mid-august2"
                         `((:id . "?")
                           (:exp-name . "?")
                           (:nr-of-series . 5)
                           (:nr-of-interactions . 500000)
                           (:population-size . 10)
                           (:dataset . "clevr")
                           (:dataset-split . "train")
                           (:available-channels . :clevr)
                           (:disable-channels . :none)
                           (:amount-disabled-channels . 0)
                           (:sensor-noise . :none)
                           (:sensor-std . 0.0)
                           (:observation-noise . :none)
                           (:observation-std . 0.0)
                           (:scene-sampling . :random)
                           (:topic-sampling . :random)
                           (:similarity-threshold . 0.0)
                           (:align . t)
                           (:entrenchment-incf . 0.1)
                           (:entrenchment-decf . -0.1)
                           (:entrenchment-li . -0.02)
                           (:trash-concepts . nil)
                           (:weight-update-strategy . :j-interpolation)
                           (:initial-weight . 0)
                           (:weight-incf . 1)
                           (:weight-decf . -1)
                           (:switch-condition . :after-n-interactions)
                           (:switch-conditions-after-n-interactions . 250000)
                           (:stage-parameters ,'((:do-nothing . t))))
                         `(;(:similarity-threshold 0.0 0.01 0.05); 0.1 0.2)
                           (:initial-weight 0 35)
                           (:weight-decf -1 -5 -10 -20)
                           (:entrenchment-li -0.001 -0.005 -0.01 -0.02); -0.05 -0.1)
                           (:trash-concepts nil t)
                           ))|#


;; -----------------------------------------
;; + Utility functions for CLEVR simulated +
;; -----------------------------------------

#|(defun find-scenes-with-size (context-size)
  (let* ((world (make-instance 'dataset-world
                               :dataset "clevr-extracted"
                               :dataset-split "val"
                               :available-channels (get-all-channels :clevr-extracted)))
         (scenes (all-scenes world))
         (filtered-scenes (loop for scene in scenes
                                if (length= (objects scene) context-size)
                                  collect scene)))
    filtered-scenes))|#

#|
(defun get-all-scenes ()
  (let* ((world (make-instance 'dataset-world
                               :dataset "cogenta-extracted"
                               :dataset-split "val"
                               :available-channels (get-all-channels :clevr-extracted))))
    (scenes world)))

(defun find-scenes-with-discriminative-topics (dataset scenes channels)
    (loop for fpath in scenes
          for cle-scene = (load-scene fpath channels)
          for candidate-topics = (filter-discriminative-topics dataset (objects cle-scene))
          if candidate-topics
            collect (cons (index cle-scene) candidate-topics)))

(progn
  (setf disc-scenes (find-scenes-with-discriminative-topics :clevr-extracted all-scenes (get-all-channels :clevr-extracted)))
  1)
|#

;;;

#|
 
(setf all-scenes (find-scenes-with-size))
(setf all-scenes (get-all-scenes))
(length all-scenes)

(setf res (find-scenes-with-discriminative-topics all-scenes (list 'color 'area 'roughness)))
(setf res (find-scenes-with-discriminative-topics all-scenes (list 'color 'area 'roughness 'sides-and-corners 'wh-ratio 'xpos 'ypos 'zpos)))
(length res)
(setf scene-ids (loop for (scene-id . candidate-topics) in res collect scene-id))

(setf scene-ids (loop for scene in all-scenes collect (index scene)))

(setf test (loop for (scene-id . candidate-topics) in res
      for scenes = (loop for candidate-topic in candidate-topics
                         collect (cons scene-id candidate-topic))
      append scenes))

(defun dupes (lst)
  (cond ((null lst) '())
        ((member (car lst) (cdr lst)) (cons (car lst) (dupes (cdr lst))))
        (t (dupes (cdr lst)))))

(dupes (loop for scene in res
             collect (first scene)))

(setf all-scenes (find-scenes-with-size 5))
          if (length> candidate-topics 0)
              do (loop for ecl-topic in candidate-topics
                       for types = (get-symbolic-discriminative-feature ecl-topic ecl-context)
                               
            do (let ((ecl-topic (random-elt candidate-topics)))
                 (set-data interaction 'attribute-type (get-symbolic-discriminative-feature ecl-topic ecl-context))
                 (loop for agent in (interacting-agents experiment)
                       do (set-data agent 'topic ecl-topic))))))|#


