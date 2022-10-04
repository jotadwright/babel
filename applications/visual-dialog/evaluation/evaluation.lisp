(in-package :visual-dialog)

(defun evaluate-dialog (&key scene-index dialog-index world ontology (silent t))
  "run dialog with specific scene and a specific dialog (1-5), check result with gold-answers, returns T if whole dialog succeeded otherwise nil and the success list per question"

  ;(notify dialog-started scene-index dialog-index)
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
          (add-element `((h3) ,(format nil "Caption: ~a" (first dialog)))))
        (loop for question in (if (equal (get-configuration world :world) :clevr) (rest dialog) dialog)
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
    (values (loop for a in correct-answers always (= a 1)) correct-answers computed-answers gold-answers)))

(defun evaluate-dialogs (start-scene end-scene world)
  "evaluate all dialogs from start-scene to end-scene, returns question-level-accuracy"
  
  (ensure-directories-exist
   (babel-pathname :directory `("applications" "visual-dialog" "evaluation" "results"
                                ,(format nil "~a-~a-~a-~a"
                                         (get-configuration world :dataset)
                                         (get-configuration world :mode)
                                         (get-configuration world :datasplit)
                                         (get-configuration world :evaluation-mode)))))
  (ensure-directories-exist
   (babel-pathname :directory `("applications" "visual-dialog" "evaluation" "results" "answers"
                                ,(format nil "~a-~a-~a-~a"
                                         (get-configuration world :dataset)
                                         (get-configuration world :mode)
                                         (get-configuration world :datasplit)
                                         (get-configuration world :evaluation-mode)))))
  (with-open-file (str (make-file-name-with-time 
                        (babel-pathname
                         :directory `("applications" "visual-dialog" "evaluation" "results"
                                      ,(format nil "~a-~a-~a-~a" (get-configuration world :dataset)
                                               (get-configuration world :mode)
                                               (get-configuration world :datasplit)
                                               (get-configuration world :evaluation-mode)))
                         :name (format nil "evaluation-~a-~a-~a-~a-~a"
                                       (get-configuration world :dataset)
                                       (get-configuration world :mode)
                                       (get-configuration world :datasplit)
                                       start-scene end-scene)
                         :type "txt"))
                       
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (with-open-file (answer-str (make-file-name-with-time 
                                 (babel-pathname
                                  :directory `("applications" "visual-dialog" "evaluation" "results" "answers"
                                               ,(format nil "~a-~a-~a-~a"
                                                        (get-configuration world :dataset)
                                                        (get-configuration world :mode)
                                                        (get-configuration world :datasplit)
                                                        (get-configuration world :evaluation-mode)))
                                  :name (format nil "evaluation-~a-~a-~a-~a-~a"
                                                (get-configuration world :dataset)
                                                (get-configuration world :mode)
                                                (get-configuration world :datasplit)
                                                start-scene end-scene)
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
                                (format t "evaluation of scene ~a~%" scene)
                                (loop for dialog from 0 to number-of-dialogs
                                      for (result-whole-dialog result-one-dialog computed-answers gold-answers) = (multiple-value-list
                                                                                     (evaluate-dialog :scene-index scene
                                                                                                      :dialog-index dialog
                                                                                                      :world world
                                                                                                      :ontology ontology))
                                      do (progn
                                           (format str "~a, ~a : ~a~%" scene dialog result-one-dialog) (force-output str)
                                           (format answer-str "~a, ~a : computed-answers : ~a : gold-answers : ~a~%" scene dialog computed-answers gold-answers) (force-output answer-str))
                                      collect (list result-whole-dialog result-one-dialog)))))
               (dialog-level-accuracy
                (average (loop for result in results
                               collect (if (eql (first result) T)
                                         1 0))))
               ;;append
               (question-level-accuracy
                (average (loop for result in results
                               append (second result)))))
          (format str "dialog-level-accuracy : ~a~%" dialog-level-accuracy) (force-output str)
          (format str "question-level-accuracy : ~a~%" question-level-accuracy) (force-output str)
          question-level-accuracy)))))



(defun comprehend-dialogs (start-scene end-scene world)
  "comprehend all dialogs from start-scene to end-scene, write meaning representation to file"
  (ensure-directories-exist
   (babel-pathname :directory `("applications" "visual-dialog" "evaluation" "comprehension-results"
                                ,(format nil "~a-~a" (get-configuration world :dataset) (get-configuration world :datasplit)))))
                 (with-open-file (str (make-file-name-with-time 
                                       (babel-pathname
                                        :directory `("applications" "visual-dialog" "evaluation" "comprehension-results"
                                                     ,(format nil "~a-~a" (get-configuration world :dataset) (get-configuration world :datasplit)))
                                        :name (format nil "evaluation-~a-~a" start-scene end-scene)
                                        :type "csv"))
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
                                (format t "comprehension of scene ~a~%" scene)
                                (loop for dialog from 0 to number-of-dialogs
                                      for (questions meanings) = (multiple-value-list (comprehend-dialog :scene-index scene :dialog-index dialog :world world :ontology ontology))
                                      do (progn
                                           (loop for question in questions
                                                   for meaning in meanings  
                                                 do (format str "~a, ~a~%" question meaning) (force-output str))))))))))))


(defun comprehend-dialog (&key scene-index dialog-index world ontology (silent t))
  "comprehend dialog of specified scene and dialog-index, returns meaning"
  (let* ((scene-pathname (get-scene-pathname-by-index scene-index world))
         (dataset (get-configuration world :dataset))
         (dialog (get-dialog-by-index scene-index dialog-index world dataset))
         meanings questions)
    (loop for question in dialog
          for (meaning cipn) = (multiple-value-list (comprehend question))
          if (find 'fcg::succeeded (statuses cipn))
            collect meaning into meanings
            and collect question into questions
          finally (return (values questions meanings)))))

