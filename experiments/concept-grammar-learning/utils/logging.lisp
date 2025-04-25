(in-package :clg)

(defun write-game-summary-to-log (experiment interaction)
  (let ((log-path
         (babel-pathname
          :directory '("experiments" "clevr-learning" "raw-data")
          :name (get-configuration experiment :log-filename)
          :type "log")))
    (with-open-file (stream log-path :direction :output
                            :if-exists :append
                            :if-does-not-exist :create)
      (write-line "####################" stream)
      (write-line (format nil "Interaction ~a" (interaction-number interaction)) stream)
      ;; write the roles of the game
      (write-line
       (format nil "The tutor is the ~(~a~) and the learner is the ~(~a~)"
               (discourse-role (tutor experiment))
               (discourse-role (learner experiment)))
       stream)
      (if (eq (learner experiment) (speaker interaction))
        ;; if formulation; write if it succeeded
        ;;   and if the tutor could interpret
        ;;   and the applied cxns with their score
        ;;   and the applied repair (if any)
        (if (task-result (learner experiment))
          (let* ((task-result (task-result (learner experiment)))
                 (cipn (find-data task-result 'cipn))
                 (applied-cxns (find-data task-result 'applied-cxns))
                 (applied-repair (find-data interaction :applied-repair)))
            (if (and cipn (find 'fcg::succeeded (fcg::statuses cipn)))
              (progn
                (when applied-repair
                  (write-line (format nil "The ~(~a~) repair applied" applied-repair) stream))
                (write-line "Formulation succeeded" stream)
                (write-line "Applied constructions:" stream)
                (loop for cxn in applied-cxns
                      do (write-line (format nil "~(~a~) (~,2f)" (name cxn) (cxn-score cxn))
                                     stream))
                (write-line "Tutor's interpretation failed" stream)
                (write-line "Game failed" stream))
              (write-line "Formulation failed" stream)))
          (write-line "Formulation failed" stream))
        ;; if comprehension; write if it succeeded
        ;;   and if interpretation succeeded
        ;;   and if any repair applied
        ;;   and the applied cxns (if any)
        (if (task-result (learner experiment))
          (let* ((task-result (task-result (learner experiment)))
                 (cipn (find-data task-result 'cipn))
                 (applied-cxns (find-data task-result 'applied-cxns))
                 (applied-repair (find-data interaction :applied-repair)))
            (if (and cipn (find 'fcg::succeeded (fcg::statuses cipn)))
              (progn 
                (when applied-repair
                  (write-line (format nil "The ~(~a~) repair applied" applied-repair) stream))
                (write-line "Comprehension succeeded" stream)
                (write-line "Applied constructions:" stream)
                (loop for cxn in applied-cxns
                      do (write-line (format nil "~(~a~) (~,2f)" (name cxn) (cxn-score cxn))
                                     stream))
                (if (find-data task-result 'computed-topic)
                  (progn (write-line "Interpretation succeeded, but incorrect" stream)
                    (write-line "Game failed" stream))
                  (write-line "Interpretation failed" stream)))
              (write-line "Comprehension failed" stream)))
          (write-line "Comprehension failed" stream)))
      (write-line "####################" stream)
      (force-output stream))))