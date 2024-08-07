(setf cl-user::*automatically-start-web-interface* nil)
(setf test-framework::*dont-run-tests-when-loading-asdf-systems* t)

(ql:quickload :cle)
(in-package :cle)

(defmethod write-monitor-to-file ((monitor data-file-writer) (exp-top-dir string) (exp-name string) (name string))
  (let* ((fname (file-name monitor))
         (fpath (mkstr (make-pathname :directory (pathname-directory fname))
                       (format nil "~a/~a/~a/" exp-top-dir exp-name name)
                       (pathname-name fname) "." (pathname-type fname))))
    (ensure-directories-exist fpath)
    (with-open-file (file fpath :direction :output 
			  :if-exists :supersede :if-does-not-exist :create)
      (write-data monitor file)
      (format t "~%monitor ~(~a~):~%  wrote ~a" (id monitor) fpath))))

(defmethod write-data ((monitor lisp-data-file-writer) stream)
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
    (loop for (key . val) in config
          when (find key (list :exp-name :dataset :dataset-split :exp-top-dir :feature-set))
            do (rplacd (assoc key config)
                       (string-downcase (string (assqv key config)))))
    config))

(defun fixed-config ()
  `(;; fixed in stone
    ;; --------------
    (:dot-interval . 10)
    (:usage-table-window . 5000)))

(defun test-experiment (args)
  (let* ((config (append (fixed-config)
                         (parse-config args)))
         (fname (format nil "seed-~a" (assqv :seed config)))
         (experiment (cl-store:restore
                      (babel-pathname :directory `("experiments"
                                                   "concept-emergence2"
                                                   "storage"
                                                   ,(assqv :exp-top-dir config)
                                                   ,(assqv :exp-name config)
                                                   )
                                      :name fname
                                      :type "store"))))
    (format t "~% EXP-NAME = ~a w/ seed ~a"
            (assqv :exp-name config)
            (assqv :seed config)) ;; log exp-name
    (set-seed (get-configuration experiment :seed)) ;; set random state
    ;; set-up
    (set-configuration experiment :scene-sampling (assqv :scene-sampling config))
    (set-configuration experiment :topic-sampling (assqv :topic-sampling config))
    (set-configuration experiment :dataset (assqv :dataset config))
    (set-configuration experiment :dataset-split (assqv :dataset-split config))
    (set-configuration experiment :feature-set (assqv :feature-set config))
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
          do (write-monitor-to-file monitor (assqv :exp-top-dir config) (assqv :exp-name config) fname))
    (notify reset-monitors)))
      
(test-experiment #+sbcl (rest sb-ext:*posix-argv*))
