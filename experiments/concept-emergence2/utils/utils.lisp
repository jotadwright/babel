(in-package :cle)

;; -------------
;; + Utilities +
;; -------------

(defun read-csv (fpath)
  "Reads a CSV file and returns a list of lists."
  (unless (probe-file fpath)
    (error "Could not find the file ~%~a" fpath))
  (with-open-file (stream fpath)
    (loop with skipped-header = nil
          for line = (read-line stream nil)
          while line
          for row = (split-sequence:split-sequence #\, line)
          if (not skipped-header)
            do (setf skipped-header t)
          else
            collect row)))

(defun get-current-date ()
  (multiple-value-bind
      (second minute hour day month year day-of-week dst-p tz)
      (get-decoded-time)
    (format nil "~d-~2,'0d-~d_~dh~dm~ds" year month day hour minute second)))

(defun generate-log-dir-name (seed)
  ;; set a random seed to generate the 5-character random number (to avoid collisions) 
  (set-seed -1)
  ;; create a log-dir-name based on the current-data, the seed, and the random number
  (mkstr (internal-symb (list-of-strings->string
                         (list (get-current-date)
                               (mkstr (format nil "seed~a" seed))
                               (mkstr (random 10) (random 10) (random 10) (random 10) (random 10)))
                         :separator "-"))))

(defun parse-keyword (string)
  (intern (string-upcase (string-left-trim ":" string)) :keyword))

(defun store-experiment (experiment)
  (let* ((exp-top-dir (get-configuration experiment :exp-top-dir))
         (log-dir-name (get-configuration experiment :log-dir-name))
         (exp-name (get-configuration experiment :exp-name))
         (current-stage (get-configuration experiment :current-stage))
         (path (babel-pathname
                :directory `("experiments"
                             "concept-emergence2"
                             "logging"
                             ,exp-top-dir
                             ,exp-name
                             ,log-dir-name
                             "stores")
                :name (list-of-strings->string (list (write-to-string (series-number experiment))
                                                     "history"
                                                     "stage"
                                                     (write-to-string current-stage))
                                               :separator "-") 
                :type "store"))
         (tmp-world (copy-object (world experiment))))
    (ensure-directories-exist path)
    (setf (world experiment) nil)
    (cl-store:store experiment path)
    (setf (world experiment) tmp-world)))

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

(defun alist->json-alist (alist)
  (mapcar (lambda (entry)
            (cons (if (keywordp (car entry))
                      (string-downcase (format nil ":~a" (car entry)))
                      (car entry))
                  (cond ((keywordp (cdr entry))
                         (string-downcase (format nil ":~a" (cdr entry))))
                        ((numberp (cdr entry))
                         (cdr entry))
                        ((stringp (cdr entry))
                         (cdr entry))
                        ((equal (cdr entry) (list nil))
                         "nil")
                        (t
                         (string-downcase (cdr entry))))))
          alist))

(defun hash-keys (ht)
  (loop for key being the hash-keys of ht
        collect key))

(defun hash-values (ht)
  (loop for value being the hash-values of ht
        collect value))

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
                                                    (asdf:system-relative-pathname "cle" "batch/data-train/")
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

(defun load-experiment (store-dir &key (name "history"))
  "Loads and returns the store object in the given directory." 
  (let ((store-path (merge-pathnames (make-pathname :name name :type "store")
                                     store-dir)))
    (cl-store:restore store-path)))

(defun set-up-monitors (monitors config)
  (monitors::deactivate-all-monitors)
  (loop for monitor-string in monitors
        for monitor = (monitors::get-monitor (read-from-string monitor-string))
        do (monitors::activate-monitor-method (read-from-string monitor-string))
        when (slot-exists-p monitor 'file-name)
          do (setf (slot-value monitor 'file-name)
                    (ensure-directories-exist
                    (merge-pathnames (make-pathname :directory `(:relative ,(assqv :log-dir-name config))
                                                    :name (pathname-name (file-name monitor)) 
                                                    :type (pathname-type (file-name monitor)))
                                      (babel-pathname :directory `("experiments"
                                                                  "concept-emergence2"
                                                                  "logging"
                                                                  ,(assqv :exp-top-dir config)
                                                                  ,(assqv :exp-name config))))))))

#|(generate-csv-for-tuning "tune-clevr"
                         "tune-clevr"
                         `((:id . "?")
                           (:exp-name . "?")
                           (:nr-of-series . 5)
                           (:nr-of-interactions . 500000)
                           (:population-size . 10)
                           (:dataset . "clevr")
                           (:dataset-split . "train")
                           (:feature-set . "clevr")
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
                           (:trash-concepts . t)
                           (:weight-update-strategy . :j-interpolation)
                           (:initial-weight . 0)
                           (:weight-incf . 1)
                           (:weight-decf . -5)
                           (:switch-condition . :none)
                           (:switch-conditions-after-n-interactions . 0)
                           (:stage-parameters ,'((:do-nothing . t))))
                         `((:similarity-threshold 0.0 0.001 0.005 0.01 0.05 0.1 0.2 0.3)
                           (:initial-weight 0 35)
                           (:weight-decf -1 -2 -3 -5)
                           (:entrenchment-li -0.0001 -0.0005 -0.001 -0.005 -0.01 -0.02 -0.05)
                           ))|#