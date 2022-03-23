(in-package :visual-dialog)

(defun evaluate-dialog (&key scene-index dialog-index world ontology (silent t))
  "run dialog with specific scene and a specific dialog (1-5), check result with gold-answers"
  "returns T if whole dialog succeeded otherwise nil and the success list per question"
  (let* ((scene-pathname (get-scene-pathname-by-index scene-index world))
         (dataset (get-configuration world :dataset))
         (dialog (get-dialog-by-index scene-index dialog-index world dataset))
         (gold-answers (get-gold-answers-by-index scene-index dialog-index world dataset))
         (computed-answers (run-dialog scene-pathname dialog world ontology dataset :silent silent))
         (correct-answers (check-answers gold-answers computed-answers)))
    ;; print in web interface
    (if (not silent)
      (progn 
        (add-element `((h1) ,(format nil "Dialog ~a" dialog-index)))
        (if (eq dataset :clevr)
          (add-element `((h3) ,(format nil "Caption: ~a" (caption (nth dialog-index (dialogs (current-dialog-set world))))))))
        (loop for question in (questions (nth dialog-index (dialogs (current-dialog-set world))))
              for answer in computed-answers
              for gold-answer in gold-answers
              for a in correct-answers
              do (progn
                   (add-element  `((h3) ,(format nil "Question: ~a" question)))
                   (add-element  `((h3) ,(format nil "Computed Answer: ~a" answer)))
                   (if (numberp gold-answer)
                     (add-element  `((h3) ,(format nil "Gold Answer: ~r" gold-answer)))
                     (add-element  `((h3) ,(format nil "Gold Answer: ~a" gold-answer))))
                   (if (= a 1)
                     (add-element  `((h3) ,(format nil "SUCCESS")))
                     (add-element  `((h3) ,(format nil "FAILURE"))))))))
    ;; return success of whole dialog and detailed success of questions 
    (values (loop for a in correct-answers always a) correct-answers)))

(defun evaluate-dialogs (start-scene end-scene world )
  "evaluate all dialogs from start-scene to end-scene"
  "returns question-level-accuracy"
  (with-open-file (str (make-file-name-with-time
                        (babel-pathname
                         :directory '("applications" "visual-dialog" "evaluation" "results")
                         :name (format nil "evaluation-~a-~a-~a" (get-configuration world :dataset) start-scene end-scene)
                         :type "txt"))
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (unwind-protect
        (let* ((ontology
                (build-ontology))
               (number-of-dialogs
                (compute-number-of-dialogs world))
               (results
                 (loop for scene from start-scene to end-scene
                       append (progn
                                 (format str "evaluation of scene ~a~%" scene) (force-output str)
                                 (loop for dialog from 0 to number-of-dialogs
                                       for (result-whole-dialog result-one-dialog) = (multiple-value-list
                                                                                      (evaluate-dialog :scene-index scene
                                                                                                       :dialog-index dialog
                                                                                                       :world world
                                                                                                       :ontology ontology))
                                       do (progn
                                            (format str "~a : ~a~%" dialog result-one-dialog) (force-output str))
                                       collect (list result-whole-dialog result-one-dialog)))))
               (dialog-level-accuracy
                (average (loop for result in results
                                        collect (if (eql (first result) T)
                                                  1 0))))
               ;;append
               (question-level-accuracy
                (average (loop for result in results
                                        append (second result)))))
          (format str "dialog-level-accuracy: ~a~%" dialog-level-accuracy) (force-output str)
          (format str "question-level-accuracy: ~a~%" question-level-accuracy) (force-output str)
          question-level-accuracy))))


(defun evaluate-clevr-dialogs-symbolic (start-scene end-scene)
  (let ((world (make-instance 'world 
                              :entries '((:dataset . :clevr)
                                         (:datasplit . :train)
                                         (:mode . :symbolic)))))
    (evaluate-dialogs start-scene end-scene world)))

(defun evaluate-mnist-dialogs-symbolic (start-scene end-scene)
  (let ((world (make-instance 'world 
                              :entries '((:dataset . :mnist)
                                         (:datasplit . :train)
                                         (:mode . :symbolic)))))
    (evaluate-dialogs start-scene end-scene world)))

(defun evaluate-clevr-dialogs-hybrid (start-scene end-scene)
  (let ((world (make-instance 'world 
                              :entries '((:dataset . :clevr)
                                         (:datasplit . :train)
                                         (:mode . :hybrid)))))
    (evaluate-dialogs start-scene end-scene world)))

(defun evaluate-mnist-dialogs-hybrid (start-scene end-scene)
  (let ((world (make-instance 'world 
                              :entries '((:dataset . :mnist)
                                         (:datasplit . :train)
                                         (:mode . :hybrid)))))
    (evaluate-dialogs start-scene end-scene world)))