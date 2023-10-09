(setf cl-user::*automatically-start-web-interface* nil)
(setf test-framework::*dont-run-tests-when-loading-asdf-systems* t)

(ql:quickload :cle)
(in-package :cle)

(defmethod handle-batch-finished-event2 ((monitor data-file-writer) (exp-name string) (store string))
  (let* ((fname (file-name monitor))
         (fpath (mkstr (make-pathname :directory (pathname-directory fname))
                       (format nil "~a/~a/" exp-name store)
                       (pathname-name fname) "." (pathname-type fname))))
    (ensure-directories-exist fpath)
    (with-open-file (file fpath :direction :output 
			  :if-exists :supersede :if-does-not-exist :create)
      (write-data-to-file2 monitor file)
      (format t "~%monitor ~(~a~):~%  wrote ~a" (id monitor) fpath))))

(defmethod write-data-to-file2 ((monitor lisp-data-file-writer) stream)
  (format stream "~%; This file was created by the lisp-data-file-writer ~a" (id monitor))
  (format stream "~%; The elements in the lists come from these source(s): ~{~a~^ ~}"
	  (monitors::monitor-ids-of-sources monitor))
  (format stream "~%; You can either evaluate this file directly and then (defparameter foo *) or")
  (format stream "~%; (with-open-file (stream ~s) (defparameter foo (read stream)))" 
	  (file-name monitor))
  (format stream "~%((~{~f~^~%  ~}))" 
          (loop for series in (caar (sources monitor))
                collect (reverse series))))

(defun parse-config (args)
  (let ((config (loop for (a b) on args by #'cddr
                      collect (cons (parse-keyword a) (read-from-string b)))))
    (when (and (assoc :available-channels config) (keywordp (assqv :available-channels config)))
      (rplacd (assoc :available-channels config)
              (get-all-channels (assqv :available-channels config))))
    (loop for (key . val) in config
          when (find key (list :exp-name :dataset :dataset-split))
            do (rplacd (assoc key config)
                       (string-downcase (string (assqv key config)))))
    config))

(defun fixed-config ()
  `(;; fixed in stone
    ;; --------------
    (:dot-interval . 10)
    (:usage-table-window . 1000)))

(defun test-experiment (args)
  (loop for idx from 1 to 25
        for store = (format nil "~a" idx)
        do (let* ((config (append (fixed-config)
                                  (parse-config args)))
                  (experiment (cl-store:restore
                               (babel-pathname :directory `("experiments"
                                                            "concept-emergence2"
                                                            "storage"
                                                            "test"
                                                            ,(assqv :exp-name config)
                                                            )
                                               :name store
                                               :type "store"))))
             (format t "~% EXP-NAME = ~a" (assqv :exp-name config)) ;; log exp-name
             (setf *random-state* (make-random-state t)) ;; reset random state
             ;; set-up
             (set-configuration experiment :scene-sampling (assqv :scene-sampling config))
             (set-configuration experiment :topic-sampling (assqv :topic-sampling config))
             (set-configuration experiment :dataset (assqv :dataset config))
             (set-configuration experiment :dataset-split (assqv :dataset-split config))
             (set-configuration experiment :available-channels (assqv :available-channels config))
             (set-configuration experiment :align nil)
             ;; reset usage tables
             (loop for agent in (agents experiment)
                   do (setf (usage-table agent) (create-usage-table (assqv :usage-table-window config))))
             ;; initialise the world
             (initialise-world experiment)
             ;; set-up monitors
             (activate-monitor export-communicative-success)
             (activate-monitor export-lexicon-coherence)
             (activate-monitor export-unique-form-usage)
             (activate-monitor print-a-dot-for-each-interaction)
             (format t "~%---------- NEW GAME ----------~%")
             (time
              (loop for i from 1 to (assqv :nr-of-interactions config)
                    do (run-interaction experiment)))

             ;; log monitors to disk
             (loop for monitor-id in `(,'export-communicative-success
                                       ,'export-lexicon-coherence
                                       ,'export-unique-form-usage)
                   for monitor = (monitors::get-monitor monitor-id)
                   do (handle-batch-finished-event2 monitor (assqv :exp-name config) store))
             (notify reset-monitors))))
      
(test-experiment #+sbcl (rest sb-ext:*posix-argv*))
