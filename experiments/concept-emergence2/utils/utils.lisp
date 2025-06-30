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

(defmethod jzon::coerce-key ((key symbol))
  (let ((name (symbol-name key)))
    (when (keywordp key)
      (setf name (format nil ":~A" name)))
    (string-downcase name)))

(defmethod jzon::write-value ((writer jzon::writer) (value symbol))
  (let ((name (string-downcase (symbol-name value))))
    (when (keywordp value)
      (setf name (format nil ":~A" name)))
    (jzon::%write-json-atom writer name)))

(defun store-experiment (experiment &key (stage nil))
  (let* ((exp-top-dir (get-configuration experiment :exp-top-dir))
         (log-dir-name (get-configuration experiment :log-dir-name))
         (exp-name (get-configuration experiment :exp-name))
         (dataset-split (get-configuration experiment :dataset-split))
         (stage (if stage (get-configuration experiment :current-stage) ""))
         (path (babel-pathname
                :directory `("experiments" 
                             "concept-emergence2" 
                             "logging" 
                             ,exp-top-dir
                             ,dataset-split
                             ,exp-name
                             ,log-dir-name
                             "stores")
                :name (format nil "seed-~a~a" (get-configuration experiment :seed) stage)
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

(defun list->hash-table (lst &key (key #'identity))
  "Creates a hash table given a list."
  (loop with tbl = (make-hash-table)
        for el in lst
        do (setf (gethash (funcall key el) tbl) el)
        finally (return tbl)))

(defun load-experiment (store-dir name)
  "Loads and returns the store object in the given directory." 
  (let* ((store-path (merge-pathnames (make-pathname :name name :type "store")
                                      store-dir))
         (experiment (cl-store:restore store-path)))
    experiment))

(defun test-stored-experiment (experiment)
  "After loading a stored experiment, this function performs
    a number of checks to warn the developer if something is awry."
  (when (not (get-configuration experiment :coherence-perspective))
    (error "The required config :coherence-perspective was not found in the stored experiment.
              It is possible that you are loading an old .store file.
              Either load another experiment or set the configuration manually.
              Probably using `(set-configuration experiment :coherence-perspective :hearer)`")))
    

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
                                                                  ,(assqv :dataset-split config)
                                                                  ,(assqv :exp-name config))))))))

;; Utils for logging the evolution of concepts over time (ehai/covi)

(defun kv->ht (keys values)
  (loop with ht = (make-hash-table)
        for key in keys
        for value in values
        do (setf (gethash key ht) value)
        finally (return ht)))

(defun cxn->ht (cxn)
  (loop with ht-proto = (make-hash-table)
        for prototype being the hash-values of (prototypes (meaning cxn))
        for channel = (channel prototype)
        for weight = (weight prototype)
        for mean = (mean (distribution prototype))
        for st-dev = (st-dev (distribution prototype))
        do (setf (gethash (downcase (symbol-name channel)) ht-proto)
                 (kv->ht (list "weight" "mean" "st-dev")
                         (list weight mean st-dev)))
        finally (return
                 (kv->ht (list "form" "entrenchment" "prototypes")
                         (list (form cxn) (score cxn) ht-proto)))))

(defun store-to-json (fpath data)
  "Store stringified json"
  (ensure-directories-exist fpath)
  (with-open-file (stream fpath :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (write-string (format nil "~a" (jzon::stringify data))
                  stream)))

(defun log-inventory (dirs agent timestep inventory-type)
  (loop for cxn being the hash-values of (get-inventory (lexicon agent) inventory-type)
        do (store-to-json (babel-pathname :directory (append dirs
                                                             `(,(format nil "~a" timestep)
                                                               ,(downcase (format nil "~a" (symbol-name (id agent))))
                                                               ;,(downcase (symbol-name inventory-type))
                                                               ))
                                          :name (format nil "~a" (form cxn))
                                          :type "json")
                          (cxn->ht cxn))))