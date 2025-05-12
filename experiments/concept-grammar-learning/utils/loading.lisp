(in-package :clg)


(define-event challenge-level-questions-loaded (level number))

(defgeneric load-questions-for-current-challenge-level (experiment  mode &optional all-files)
  (:documentation "Load all data for the current challenge level"))

(defmethod load-questions-for-current-challenge-level :around ((experiment clevr-learning-experiment)
                                                               mode
                                                               &optional all-files)
  (let* ((sort-by-func (if (get-configuration experiment :sort-questions-on-length)
                         (lambda (path) (let* ((fname (pathname-name path))
                                               (len (last-elt (split-string fname "_"))))
                                          len))
                         #'namestring))
         (all-challenge-files (sort (directory (merge-pathnames
                                                (case (get-configuration experiment :current-challenge-level)
                                                  (1 (get-configuration experiment :challenge-1-files))
                                                  (2 (get-configuration experiment :challenge-2-files))
                                                  (3 (get-configuration experiment :challenge-3-files)))
                                                (get-configuration experiment :challenge-files-root)))
                                    #'string< :key sort-by-func)))
    (when (null all-challenge-files)
      (warn "~%~%No data found. You probably specified the wrong path...~%~%"))
    (format t "~%Loading data...")
    (call-next-method experiment mode all-challenge-files)
    (format t "~%Done!")
    (notify challenge-level-questions-loaded (get-configuration experiment :current-challenge-level))))

(defmethod load-questions-for-current-challenge-level ((experiment clevr-learning-experiment)
                                                       (mode (eql :all))
                                                       &optional all-files)
  (let* ((scenes-per-questions (get-configuration experiment :scenes-per-question))
         (data (loop for file in all-files
                     for file-data = (with-open-file (stream file :direction :input) (read stream))
                     for count-question-p = (find 'count! (second file-data) :key #'first)
                     for available-scenes-and-answers = (if count-question-p
                                                          (find-all-if-not #'(lambda (scene-answer-cons) (= 0 (cdr scene-answer-cons))) (third file-data))
                                                          (third file-data))
                     for sampled-scenes-and-answers = (if (> (length available-scenes-and-answers) scenes-per-questions)
                                                        (random-elts available-scenes-and-answers scenes-per-questions)
                                                        available-scenes-and-answers)
                     collect (list (first file-data) (second file-data) sampled-scenes-and-answers))))
    (setf (question-data experiment) data)))