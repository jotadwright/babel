(in-package :visual-dialog)

(defun evaluate-dialog (&key scene-index dialog-index world ontology (silent t))
  "run dialog with specific scene and a specific dialog (1-5), check result with gold-answers"
  "returns T if whole dialog succeeded otherwise nil and the success list per question"
  (if (equal (get-configuration world :mode) :hybrid)
    (progn
      
      (clear-scenes (get-configuration world :server-address) (get-configuration world :cookie-jar))
      (clear-attentions (get-configuration world :server-address) (get-configuration world :cookie-jar))))
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
    (values (loop for a in correct-answers always a) correct-answers)))

(defun evaluate-dialogs (start-scene end-scene world)
  "evaluate all dialogs from start-scene to end-scene"
  "returns question-level-accuracy"
  
  (ensure-directories-exist
   (babel-pathname :directory `("applications" "visual-dialog" "evaluation" "results"
                                ,(format nil "~a-~a-~a" (get-configuration world :dataset) (get-configuration world :mode) (get-configuration world :datasplit)))))
                 (with-open-file (str (make-file-name-with-time 
                                       (babel-pathname
                                        :directory `("applications" "visual-dialog" "evaluation" "results" ,(format nil "~a-~a-~a" (get-configuration world :dataset) (get-configuration world :mode) (get-configuration world :datasplit)))
                                        :name (format nil "evaluation-~a-~a-~a-~a-~a" (get-configuration world :dataset) (get-configuration world :mode) (get-configuration world :datasplit) start-scene end-scene)
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
                                      for (result-whole-dialog result-one-dialog) = (multiple-value-list
                                                                                     (evaluate-dialog :scene-index scene
                                                                                                      :dialog-index dialog
                                                                                                      :world world
                                                                                                      :ontology ontology))
                                      do (progn
                                           (format str "~a, ~a : ~a~%" scene dialog result-one-dialog) (force-output str))
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
          question-level-accuracy))))



(defun calculate-accuracy-from-dir (dir)
  (let* ((files (directory dir))
         (results
          (loop for file in files
                for file-content = (open file)
                append (result-file->result-list file-content))))
    (average results)))

(defun result-file->result-list (stream &key number-of-lines)
  "collect all the lines that are results"
    (loop for line = (read-line stream nil nil)
          while line
          when (and (not (string= (first-word line) "evaluation"))
                    (not (string= (first-word line) "dialog-level-accuracy"))
                    (not (string= (first-word line) "question-level-accuracy")))
            append (read-from-string (last-elt (split-string line ":")))))

(defun collect-failed-dialogs (dir)
   (let* ((files (directory dir))
          (failed-dialogs
           (loop for file in files
                 for file-content = (open file)
                 for question-result = (last-elt (split-string (last-elt (stream->list file-content)) " "))
                 when (not (equal question-result "1.0"))
                   collect file
                 do (close file-content))))
     failed-dialogs))

(defun check-failed-dialogs (failed-dialogs multiple-middles-file)
  (with-open-file (str multiple-middles-file)
    (let ((middles-list (read-from-string (read-line  str))))
      (loop for dialog in failed-dialogs
            do (with-open-file (dialog-file dialog)
                 (let ((lines (stream->list dialog-file)))
                   (loop for line in lines
                           when (and (not (string= (first-word line) "dialog-level-accuracy"))
                                     (not (string= (first-word line) "question-level-accuracy")))
                           do (let* ((split-line (split-string line ":"))
                                     (scene (parse-integer (first (split-string (first split-line) ","))))
                                     (results (read-from-string (last-elt split-line))))
                                (if (and (not (equal (average results) 1.0))
                                         (not (find scene middles-list)))
                                  (format t "~a~%" line))))))))))

                

(defun collect-problematic-middle-scenes ()
  (let* ((ontology (build-ontology))
         (world (make-instance 'world :configuration '((:dataset . :clevr)
                                                       (:datasplit . :train)
                                                       (:mode . :symbolic))))
         (multiple-middles
          (loop with i = 0
                for scene in (scenes world)
                  
                for s = (get-scene-by-index world i)
                for context = (make-context world)
                for number-of-middles = (length (middle (scene-configuration (object-set (first (set-items context))))))
                if (< number-of-middles 2)
                  do (progn (print i)
                       (incf i))
                else
                  collect i
                  and do (progn (print i)
                           (incf i))))
         (outfile (babel-pathname :directory '("applications" "visual-dialog" "evaluation")
                               :name "scenes-with-multiple-middles"
                               :type "lisp")))
    (with-open-file (str outfile
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
      (format str "~a" multiple-middles) (force-output str))
    multiple-middles))
                 
