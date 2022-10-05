(in-package :cooking-bot-new)

(defun measure-and-understand (recipe)
  (format t "------- Measuring understanding -------~%")
  (my-initialize-values)
  (defparameter *pdm* (initialise-personal-dynamic-memory
                       *fcg-constructions*
                       `((get-kitchen ,(make-var 'kitchen-state)))))
  (print-initial-output)
  (process-uterances recipe *pdm*))
  
(defun my-initialize-values ()
  (setf (slot-value (monitors::get-monitor 'questions-introduced-by-grammar) 'values) nil
        (slot-value (monitors::get-monitor 'questions-solved-by-grammar) 'values) nil
        (slot-value (monitors::get-monitor 'questions-solved-by-mental-simulation) 'values) nil
        (slot-value (monitors::get-monitor 'questions-solved-by-discourse) 'values) nil
        (slot-value (monitors::get-monitor 'questions-introduced-by-discourse) 'values) nil
        (slot-value (monitors::get-monitor 'node-names-fcg) 'values) nil
        (slot-value (monitors::get-monitor 'node-names-irl) 'values) nil
        (slot-value (monitors::get-monitor 'questions-introduced-by-ontology) 'values) nil
        (slot-value (monitors::get-monitor 'questions-solved-by-ontology) 'values) nil
        (slot-value (monitors::get-monitor 'questions-introduced-by-mental-simulation) 'values) nil
        *persistent-id-table-introduced* (make-hash-table)
        *persistent-id-table-solved* (make-hash-table)
        *persistent-id-table-defaults* (make-hash-table)
        *total-number-of-questions* 0
        *total-number-of-answers* 0
        *total-number-of-questions-grammar* 0
        *total-number-of-questions-ontology* 0
        *total-number-of-questions-simulation* 0
        *total-number-of-questions-discourse* 0
        *total-number-of-answers-grammar* 0
        *total-number-of-answers-ontology* 0
        *total-number-of-answers-simulation* 0
        *total-number-of-answers-discourse* 0))

(defun write-measures-to-file (fcg-nodenames irl-nodenames measures)
  (let* ((dir (ensure-directories-exist (babel-pathname :directory `("applications" "cooking-bot-new" "data" ))))
         (fcg-names (reverse (rest (slot-value (monitors::get-monitor fcg-nodenames) 'values))))
         (fcg-path (babel-pathname :directory `("applications" "cooking-bot-new" "data")
                              :name "fcg-nodenames" :type "csv"))
         (irl-names (reverse (rest (slot-value (monitors::get-monitor irl-nodenames) 'values))))
         (irl-path (babel-pathname :directory `("applications" "cooking-bot-new" "data")
                              :name "irl-nodenames" :type "csv")))
    (print (length (flatten fcg-names)))
    (print (length (flatten irl-names)))
    (with-open-file (str fcg-path
                         :if-does-not-exist :create
                         :if-exists :overwrite
                         :direction :output)
      (loop for name in fcg-names
           do (format str "~{~a~^ ~}~%" name)))
    (with-open-file (str irl-path
                         :if-does-not-exist :create
                         :if-exists :overwrite
                         :direction :output)
      (loop for name in irl-names
           do (format str "~{~a~^ ~}~%" name)))
    (loop for measure in measures
          for data = (reverse (rest (slot-value (monitors::get-monitor measure) 'values)))
          for pathname = (babel-pathname :directory `("applications" "cooking-bot-new" "data" )
                                         :name (format nil "~a" measure) :type "csv")
          do (progn
               (with-open-file (str pathname
                                  :if-does-not-exist :create
                                  :if-exists :overwrite
                                  :direction :output)
                 (if (equal measure 'questions-introduced-by-discourse)
                   (let ((node-names (reverse (slot-value (monitors::get-monitor 'node-names-irl) 'values))))
                     (loop for d in data
                           for names in node-names
                           for new-d = (append (make-list (if (> (- (length names) 1) 0)
                                                            (- (length names) 1) 0)
                                                          :initial-element '0)
                                               (first d))
                           do (format str "~{~a~^ ~}~%"  new-d)))
                   (loop for d in  data
                         do (format str "~{~a~^ ~}~%" (first d)))))))))

(defun print-initial-output ()
  (let* ((node-names-irl (first (slot-value (monitors::get-monitor 'node-names-irl) 'values)))
         (questions-introduced-by-discourse (append (make-list (if (> (- (length node-names-irl) 1) 0)
                                                                   (- (length node-names-irl) 1) 0)
                                                                 :initial-element '0)
                                                      (first (first (slot-value (monitors::get-monitor 'questions-introduced-by-discourse) 'values)))))
         (vars-introduced-by-discourse (append (make-list (if (> (- (length node-names-irl) 1) 0)
                                                            (- (length node-names-irl) 1) 0)
                                                          :initial-element 'nil)
                                               (list (loop for binding in (last-elt (first (slot-value (monitors::get-monitor 'questions-introduced-by-discourse) 'values)))
                                                     collect (var binding)))))
         (questions-introduced-by-mental-simulation (first (first (slot-value (monitors::get-monitor 'questions-introduced-by-mental-simulation) 'values))))
         (vars-introduced-by-mental-simulation (last-elt (first (slot-value (monitors::get-monitor 'questions-introduced-by-mental-simulation) 'values))))
         (questions-solved-by-mental-simulation (first (first (slot-value (monitors::get-monitor 'questions-solved-by-mental-simulation) 'values))))
         (vars-solved-by-mental-simulation (last-elt (first (slot-value (monitors::get-monitor 'questions-solved-by-mental-simulation) 'values))))
         (number-of-questions (+ (sum questions-introduced-by-mental-simulation)
                                 (sum questions-introduced-by-discourse)))
         (number-of-answers (sum questions-solved-by-mental-simulation)))
    (format t "~%--------------------~%")
    (format t "Overview measures~%")
    (format t "--------------------~%")
    (format t "~%Instruction: ~a~%" "Initializing kitchen state")
    (format t "Steps in mental simulation: ~a~%" node-names-irl)
    (format t "Questions introduced by mental simulation ~a~%" questions-introduced-by-mental-simulation)
    (format t "Answers by mental simulation: ~a~%" questions-solved-by-mental-simulation)
    (format t "Questions introduced by discourse ~a~%" questions-introduced-by-discourse)
    (format t "~%--------------------~%")
    (format t "Detailed information~%")
    (format t "--------------------~%")
    (format t "~%Instruction: ~a~%" "Initializing kitchen state")
    (loop for node-name in node-names-irl
          for question-introduced-by-discourse in questions-introduced-by-discourse
          for var-introduced-by-discourse in vars-introduced-by-discourse
          for question-introduced-by-mental-simulation in questions-introduced-by-mental-simulation
          for var-introduced-by-mental-simulation in vars-introduced-by-mental-simulation
          for question-solved-by-mental-simulation in questions-solved-by-mental-simulation
          for var-solved-by-mental-simulation in vars-solved-by-mental-simulation
          do (format t "~%Step in mental simulation: ~a~%      Questions (~a):~%         Q-Simulation: ~a --> ~a;~%         Q-Discourse: ~a --> ~a;~%      Answers (~a):~%         A-Simulation: ~a --> ~a~%         ~%"
                     node-name
                     (if (and (listp question-introduced-by-discourse)
                              (listp question-introduced-by-mental-simulation))
                       (sum (append question-introduced-by-discourse question-introduced-by-mental-simulation))
                       (+ question-introduced-by-discourse question-introduced-by-mental-simulation))
                     question-introduced-by-mental-simulation
                     var-introduced-by-mental-simulation
                     question-introduced-by-discourse
                     var-introduced-by-discourse
                     question-solved-by-mental-simulation
                     question-solved-by-mental-simulation
                     var-solved-by-mental-simulation))
    (setf *total-number-of-questions* (+  number-of-questions *total-number-of-questions*))
    (setf *total-number-of-answers* (+  number-of-answers *total-number-of-answers*))
    ;(setf *total-number-of-questions-grammar* (+ (sum questions-introduced-by-grammar) *total-number-of-questions*))
    ;(setf *total-number-of-questions-ontology* (+ (sum questions-introduced-by-ontology) *total-number-of-questions*))
    (setf *total-number-of-questions-simulation* (+ (sum questions-introduced-by-mental-simulation) *total-number-of-questions-simulation*))
    (setf *total-number-of-questions-discourse* (+ (sum questions-introduced-by-discourse) *total-number-of-questions-discourse*))

   ; (setf *total-number-of-answers-grammar* (+ (sum answers-introduced-by-grammar) *total-number-of-answers-grammar*))
   ; (setf *total-number-of-answers-ontology* (+ (sum answers-introduced-by-ontology) *total-number-of-answers-ontology*))
    (setf *total-number-of-answers-simulation* (+ (sum questions-solved-by-mental-simulation) *total-number-of-answers-simulation*))
   ; (setf *total-number-of-answers-discourse* (+ (sum answers-introduced-by-discourse) *total-number-of-answers-discourse*))
    ))

(defun print-monitor-output (utterance)
  (let* ((node-names-fcg (first (slot-value (monitors::get-monitor 'node-names-fcg) 'values)))
           (node-names-irl (first (slot-value (monitors::get-monitor 'node-names-irl) 'values)))
           (questions-introduced-by-grammar (first (first (slot-value (monitors::get-monitor 'questions-introduced-by-grammar) 'values))))
           (vars-introduced-by-grammar (last-elt (first (slot-value (monitors::get-monitor 'questions-introduced-by-grammar) 'values))))
           (questions-solved-by-grammar (first (first (slot-value (monitors::get-monitor 'questions-solved-by-grammar) 'values))))
           (vars-solved-by-grammar (last-elt (first (slot-value (monitors::get-monitor 'questions-solved-by-grammar) 'values))))
           (questions-solved-by-discourse (first (first (slot-value (monitors::get-monitor 'questions-solved-by-discourse) 'values))))
           (vars-solved-by-discourse (last-elt (first (slot-value (monitors::get-monitor 'questions-solved-by-discourse) 'values))))
           (questions-solved-by-mental-simulation (first (first (slot-value (monitors::get-monitor 'questions-solved-by-mental-simulation) 'values))))
           (vars-solved-by-mental-simulation (last-elt (first (slot-value (monitors::get-monitor 'questions-solved-by-mental-simulation) 'values))))
           (questions-introduced-by-ontology (first (first (slot-value (monitors::get-monitor 'questions-introduced-by-ontology) 'values))))
           (vars-introduced-by-ontology (last-elt (first (slot-value (monitors::get-monitor 'questions-introduced-by-ontology) 'values))))
           (questions-solved-by-ontology (first (first (slot-value (monitors::get-monitor 'questions-solved-by-ontology) 'values))))
           (vars-solved-by-ontology (second (first (slot-value (monitors::get-monitor 'questions-solved-by-ontology) 'values))))
           (questions-introduced-by-discourse (append (make-list (if (> (- (length node-names-irl) 1) 0)
                                                                   (- (length node-names-irl) 1) 0)
                                                                 :initial-element '0)
                                                      (first (first (slot-value (monitors::get-monitor 'questions-introduced-by-discourse) 'values)))))
           (vars-introduced-by-discourse (append (make-list (if (> (- (length node-names-irl) 1) 0)
                                                                   (- (length node-names-irl) 1) 0)
                                                                 :initial-element 'nil)
                                                      (list (loop for binding in (last-elt (first (slot-value (monitors::get-monitor 'questions-introduced-by-discourse) 'values)))
                                                     collect (var binding)))))
           (number-of-questions (+
                                 (sum questions-introduced-by-grammar)
                                 (sum questions-introduced-by-ontology)
                                 (sum questions-introduced-by-discourse) ))
           (number-of-answers (+
                               (sum questions-solved-by-grammar)
                               (sum questions-solved-by-mental-simulation)
                               (sum questions-solved-by-ontology)
                               (sum questions-solved-by-discourse))))

      (format t "~%--------------------~%")
      (format t "Overview measures~%")
      (format t "--------------------~%")
      (format t "~%Instruction: ~a~%" utterance)
      (format t "Steps in grammar application: ~a~%" node-names-fcg)
      (format t "Questions introduced by language ~a --> ~a~% " questions-introduced-by-grammar (sum questions-introduced-by-grammar))
      (format t "Answers by language ~a --> ~a~%" questions-solved-by-grammar (sum questions-solved-by-grammar))
      (format t "Answers by discourse ~a --> ~a~%" questions-solved-by-discourse (sum questions-solved-by-discourse))
      (format t "Steps in mental simulation: ~a~%" node-names-irl)
      (format t "Answers by mental simulation: ~a --> ~a~%" questions-solved-by-mental-simulation (sum questions-solved-by-mental-simulation))
      (format t "Questions introduced by ontology: ~a --> ~a~%" questions-introduced-by-ontology (sum questions-introduced-by-ontology))
      (format t "Answers by ontology: ~a --> ~a~%" questions-solved-by-ontology (sum questions-solved-by-ontology))
      (format t "Questions introduced by discourse ~a --> ~a~%" questions-introduced-by-discourse (sum questions-introduced-by-discourse))
      (format t "~%~%Questions introduced during instruction: ~a~%"
              number-of-questions)

      (format t "Answers during instruction: ~a~%"
              number-of-answers)
      
      (format t "~%--------------------~%")
      (format t "Detailed information~%")
      (format t "--------------------~%")
      (format t "~%Instruction: ~a~%" utterance)
      (loop for node-name in node-names-fcg
            for question-introduced-by-grammar in questions-introduced-by-grammar
            for var-introduced-by-grammar in vars-introduced-by-grammar
            for question-solved-by-grammar in questions-solved-by-grammar
            for var-solved-by-grammar in vars-solved-by-grammar
            for question-solved-by-discourse in questions-solved-by-discourse
            for var-solved-by-discourse in vars-solved-by-discourse
            do (format t "~%Step in grammar application: ~a~%      Questions (~a):~%         Q-Grammar: ~a ~a~%      Answers (~a):~%         A-Grammar: ~a ~a;~%         A-Discourse: ~a ~a~%"
                       node-name
                       (if (listp question-introduced-by-grammar)
                         (sum question-introduced-by-grammar)
                         question-introduced-by-grammar)
                       question-introduced-by-grammar
                       var-introduced-by-grammar
                       (if (and (listp question-solved-by-grammar)
                                (listp question-solved-by-discourse))
                         (sum (append question-solved-by-grammar question-solved-by-discourse))
                         (+ question-solved-by-grammar question-solved-by-discourse))
                       question-solved-by-grammar
                       var-solved-by-grammar
                       question-solved-by-discourse var-solved-by-discourse)) 
      (loop for node-name in node-names-irl
            for question-solved-by-mental-simulation in questions-solved-by-mental-simulation
            for question-introduced-by-ontology in questions-introduced-by-ontology
            for question-solved-by-ontology in questions-solved-by-ontology
            for question-introduced-by-discourse in questions-introduced-by-discourse
            for var-solved-by-mental-simulation in vars-solved-by-mental-simulation
            for var-introduced-by-discourse in vars-introduced-by-discourse
            for var-introduced-by-ontology in vars-introduced-by-ontology
            for var-solved-by-ontology in vars-solved-by-ontology
            do (format t "~%Step in mental simulation: ~a~%      Questions (~a):~%         Q-Ontology: ~a --> ~a;~%         Q-Discourse: ~a --> ~a;~%      Answers (~a):~%         A-Simulation: ~a --> ~a~%         A-Ontology: ~a --> ~a ~%"
                       node-name
                       (if (and (listp question-introduced-by-ontology)
                                (listp question-introduced-by-discourse))
                         (sum (append question-introduced-by-ontology question-introduced-by-discourse))
                         (+ question-introduced-by-ontology question-introduced-by-discourse))
                       question-introduced-by-ontology
                       var-introduced-by-ontology
                       question-introduced-by-discourse
                       var-introduced-by-discourse
                       (if (and (listp question-solved-by-mental-simulation)
                                (listp question-solved-by-ontology))
                         (sum (append question-solved-by-mental-simulation question-solved-by-ontology))
                         (+ question-solved-by-mental-simulation question-solved-by-ontology))
                       question-solved-by-mental-simulation
                       var-solved-by-mental-simulation
                       question-solved-by-ontology
                       var-solved-by-ontology
                       ))
             
      (setf *total-number-of-questions* (+  number-of-questions *total-number-of-questions*))
      (setf *total-number-of-answers* (+  number-of-answers *total-number-of-answers*))
      (setf *total-number-of-questions-grammar* (+ (sum questions-introduced-by-grammar) *total-number-of-questions-grammar*))
      (setf *total-number-of-questions-ontology* (+ (sum questions-introduced-by-ontology) *total-number-of-questions-ontology*))
    ;  (setf *total-number-of-questions-simulation* (+ (sum questions-introduced-by-mental-simulation) *total-number-of-questions*))
      (setf *total-number-of-questions-discourse* (+ (sum questions-introduced-by-discourse) *total-number-of-questions-discourse*))
      (setf *total-number-of-answers-grammar* (+ (sum questions-solved-by-grammar) *total-number-of-answers-grammar*))
      (setf *total-number-of-answers-ontology* (+ (sum questions-solved-by-ontology) *total-number-of-answers-ontology*))
      (setf *total-number-of-answers-simulation* (+ (sum questions-solved-by-mental-simulation) *total-number-of-answers-simulation*))
      (setf *total-number-of-answers-discourse* (+ (sum questions-solved-by-discourse) *total-number-of-answers-discourse*))
      
      (format t "~%~%Questions introduced during instruction: ~a~%"
              number-of-questions)

      (format t "Answers during instruction: ~a~%"
              number-of-answers)

      (format t "~%~%questions: grammar ~a, ontology ~a,simulation ~a, discourse ~a ~% answers: grammar ~a, ontology ~a, simulation ~a, discourse ~a~%~%"
        *total-number-of-questions-grammar*
        *total-number-of-questions-ontology*
        *total-number-of-questions-simulation*
        *total-number-of-questions-discourse*
        *total-number-of-answers-grammar*
        *total-number-of-answers-ontology*
        *total-number-of-answers-simulation*
        *total-number-of-answers-discourse*)
      
      (format t "Total number of questions: ~a~%"
              *total-number-of-questions*)

      (format t "Total number of answers: ~a~%"
              *total-number-of-answers*)))

