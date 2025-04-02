(setf cl-user::*automatically-start-web-interface* nil)
(setf test-framework::*dont-run-tests-when-loading-asdf-systems* t)

(ql:quickload :crs-conventionality)
(in-package :crs-conventionality)

(defun parse-config (args)
  (let ((config (loop for (a b) on args by #'cddr
                      collect (cons (parse-keyword a) (read-from-string b)))))
    ;; make sure that the keys required in paths are lowercase
    (loop for (key . val) in config
          when (find key (list :exp-name :dataset :datasplit :exp-top-dir))
            do (rplacd (assoc key config)
                       (string-downcase (string (assqv key config)))))

     config))

(defun fixed-config ()
  `(;; fixed in stone§
    ;; --------------
    (:log-every-x-interactions . 100)
    ))

(defun run-experiment (args)
  (let* (;; parse command line arguments, append it to the fixed configuration
         (config (append (fixed-config) (parse-config args)))
         ;; generate a log-dir-name
         (log-dir-name (generate-log-dir-name (assqv :seed config))))
    ;; add log-dir-name to configuration
    (setf config (append config (list (cons :log-dir-name log-dir-name))))
    ;; adapt file-writing monitors so they output in the correct log-dir
    (set-up-monitors (list "log-every-x-interactions-in-output-browser"
                                    "export-communicative-success"
                                    "export-conventionalisation"
                                    "export-construction-inventory-size")
                     config)

    ;; Run experiment
    (format t "~%~% == Running the experiment, log at 'logging/~a/~a/~a'.~%"
            (assqv :exp-top-dir config)
            (assqv :exp-name config)
            (assqv :log-dir-name config))
    (time
     (run-batch 'concept-emergence-game-experiment (assqv :nr-of-interactions config) 1
                :configuration (make-configuration :entries config))
     )
    (format t "~%~% == Completed experiment.~%~%")
    ))

(sb-int:set-floating-point-modes :traps '(:INVALID :DIVIDE-BY-ZERO)) ;; Avoid floating-point-overflow error
(run-experiment #+sbcl (rest sb-ext:*posix-argv*))
(sb-ext:quit)