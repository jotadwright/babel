(ql:quickload :clevr-grammar-learning)
(in-package :cgl)

;; use this file to run some tests

;;;; MONITORS
;;;; --------
(progn
  (activate-monitor trace-fcg)
  (activate-monitor trace-irl)
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor trace-interactions-in-wi)
  )

;;;; TESTING
;;;; -------

(defparameter *experiment*
   (make-instance 'clevr-learning-experiment
                  :entries '((:determine-interacting-agents-mode . :tutor-learner)
                             (:questions-per-challenge . 100)
                             (:scenes-per-question . 20)
                             (:confidence-threshold . 1.1)
                             (:tutor-sample-mode . :random)
                             (:cxn-decf-score . 0.4)
                             (:cxn-inhibit-score . 0.1)
                             (:primitives . :symbolic)
                             (:learner-cxn-supplier . :hashed-and-scored)
                             (:alignment-strategy . :lateral-inhibition)
                             (:hide-type-hierarchy . nil)
                             (:remove-cxn-on-lower-bound . t)
                             (:composer-strategy . :store-past-scenes)
                             (:th-link-repair-mode-comprehension . :no-path-required)
                             (:th-link-repair-mode-formulation . :path-required))))

(def-fcg-cxn cubes-cxn
             ((?cubes-unit
               (args ?cube)
               (syn-cat
                (gl::phrase-type lexical)
                (gl::lex-class cube)))
              <-
              (?cubes-unit
               (HASH meaning ((bind shape-category ?cube cube)))
               --
               (HASH form ((string ?cubes-unit "cubes")))))
             :attributes (:cxn-type lexical
                          :repair holo->item
                          :score 1.0
                          :string "cubes"
                          :meaning cube)
             :cxn-inventory (grammar (learner *experiment*))
             :cxn-set non-holophrase)

(add-element (make-html (grammar (learner *experiment*))))

;; Here, we set up a series of utterances to properly test the generalisation steps
(defun load-file-data (file)
  (let* ((file-data
          (with-open-file (stream file :direction :input)
            (read stream)))
         (question (first file-data))
         (meaning (second file-data))
         (scenes-and-answers (third file-data))
         (count-question-p
          (find 'count! meaning :key #'first))
         (usable-scenes-and-answers
          (if count-question-p
            (find-all-if-not #'(lambda (scene-answer-pair)
                                 (= 0 (cdr scene-answer-pair)))
                             scenes-and-answers)
            scenes-and-answers)))
    (cons question usable-scenes-and-answers)))

(setf (question-data *experiment*)
      (list
       (cons "How many blue cubes are there?"
             (list (cons "CLEVR_val_000053" 2)))))

(run-interaction *experiment*)



;; :holophrase->item-based--substitution
;; question_017092_len_008.lisp => What is the material of the big purple cylinder?
(setf (question-data *experiment*)
      (list
       (load-file-data (merge-pathnames
                        (make-pathname :directory '(:relative "stage-1")
                                       :name "question_017092_len_008"
                                       :type "lisp")
                        (get-configuration *experiment* :challenge-files-root)))))
(loop repeat 5 do (run-interaction *experiment*))


;; question_065729_len_008.lisp => What is the material of the big purple cube?
;; => learn what-is-the-material-of-the-big-purple-X and cylinder-lex and cube-lex
(setf (question-data *experiment*)
      (list
       (load-file-data
        (merge-pathnames
             (make-pathname :directory '(:relative "stage-1")
                            :name "question_065729_len_008"
                            :type "lisp")
             (get-configuration *experiment* :challenge-files-root)))))
(loop repeat 10 do (run-interaction *experiment*))











;;:holophrase->item-based--addition
;; question_023516_len_007.lisp => What is the material of the big object?
(setf (question-data *experiment*)
      (list
       (load-file-data
        (merge-pathnames
             (make-pathname :directory '(:relative "stage-1")
                            :name "question_023516_len_007"
                            :type "lisp")
             (get-configuration *experiment* :challenge-files-root)))))
(run-interaction *experiment*)

;;question_000003_len_008.lisp => What is the material of the big purple object?
;; => learn what-is-the-material-of-the-X-object and purple-lex
(setf (question-data *experiment*)
      (list
       (load-file-data
        (merge-pathnames
             (make-pathname :directory '(:relative "stage-1")
                            :name "question_000003_len_008"
                            :type "lisp")
             (get-configuration *experiment* :challenge-files-root)))))
(run-interaction *experiment*)






;;:holophrase->item-based--deletion
;;question_000003_len_008.lisp => What is the material of the big purple object?
;; => learn what-is-the-material-of-the-X-object and purple-lex
(setf (question-data *experiment*)
      (list
       (load-file-data
        (merge-pathnames
             (make-pathname :directory '(:relative "stage-1")
                            :name "question_000003_len_008"
                            :type "lisp")
             (get-configuration *experiment* :challenge-files-root)))))
(run-interaction *experiment*)

;; question_023516_len_007.lisp => What is the material of the big object?
(setf (question-data *experiment*)
      (list
       (load-file-data
        (merge-pathnames
             (make-pathname :directory '(:relative "stage-1")
                            :name "question_023516_len_007"
                            :type "lisp")
             (get-configuration *experiment* :challenge-files-root)))))
(run-interaction *experiment*)














;; :lexical->item-based
;; question_000003_len_008.lisp => What is the material of the big purple cube?
(setf (question-data *experiment*)
      (list
       (load-file-data
        (merge-pathnames
             (make-pathname :directory '(:relative "stage-1")
                            :name "question_065729_len_008"
                            :type "lisp")
             (get-configuration *experiment* :challenge-files-root)))))
(loop repeat 5 do (run-interaction *experiment*))

;; question_017092_len_008.lisp => What is the material of the big purple cylinder?
;; => learn what-is-the-material-of-the-big-purple-X and cube-lex and cylinder-lex
(setf (question-data *experiment*)
      (list
       (load-file-data
        (merge-pathnames
             (make-pathname :directory '(:relative "stage-1")
                            :name "question_017092_len_008"
                            :type "lisp")
             (get-configuration *experiment* :challenge-files-root)))))
(loop repeat 5 do (run-interaction *experiment*))

;; question_000027_len_006.lisp => The tiny shiny cylinder has what color?
;; => learn the-tiny-shiny-X-has-what-color
(setf (question-data *experiment*)
      (list
       (load-file-data
        (merge-pathnames
             (make-pathname :directory '(:relative "stage-1")
                            :name "question_000027_len_006"
                            :type "lisp")
             (get-configuration *experiment* :challenge-files-root)))))
(loop repeat 5 do (run-interaction *experiment*))






;; :lexical->item-based with 2 lexical cxns
;; question_000003_len_008.lisp => What is the material of the big purple cube?
(setf (question-data *experiment*)
      (list
       (load-file-data
        (merge-pathnames
             (make-pathname :directory '(:relative "stage-1")
                            :name "question_065729_len_008"
                            :type "lisp")
             (get-configuration *experiment* :challenge-files-root)))))
(loop repeat 5 do (run-interaction *experiment*))

;; question_017092_len_008.lisp => What is the material of the big purple cylinder?
;; => learn what-is-the-material-of-the-big-purple-X and cube-lex and cylinder-lex
(setf (question-data *experiment*)
      (list
       (load-file-data
        (merge-pathnames
             (make-pathname :directory '(:relative "stage-1")
                            :name "question_017092_len_008"
                            :type "lisp")
             (get-configuration *experiment* :challenge-files-root)))))
(loop repeat 5 do (run-interaction *experiment*))

;; question_000008_len_008.lisp => What is the color of the large shiny sphere?
(setf (question-data *experiment*)
      (list
       (load-file-data
        (merge-pathnames
             (make-pathname :directory '(:relative "stage-1")
                            :name "question_000008_len_008"
                            :type "lisp")
             (get-configuration *experiment* :challenge-files-root)))))
(loop repeat 5 do (run-interaction *experiment*))

;; question_142574_len_008.lisp => What is the color of the small shiny sphere?
;; => learn what-is-the-color-of-the-X-shiny-sphere and small-lex and large-lex
(setf (question-data *experiment*)
      (list
       (load-file-data
        (merge-pathnames
             (make-pathname :directory '(:relative "stage-1")
                            :name "question_142574_len_008"
                            :type "lisp")
             (get-configuration *experiment* :challenge-files-root)))))
(loop repeat 5 do (run-interaction *experiment*))

;; question_000183_len_007.lisp => What is the large red cylinder made of?
;; => learn what-is-the-X-red-Y-made-of
(setf (question-data *experiment*)
      (list
       (load-file-data
        (merge-pathnames
             (make-pathname :directory '(:relative "stage-1")
                            :name "question_000183_len_007"
                            :type "lisp")
             (get-configuration *experiment* :challenge-files-root)))))
(run-interaction *experiment*)







;; item-based->lexical
;; question_000003_len_008.lisp => What is the material of the big purple cube?
(setf (question-data *experiment*)
      (list
       (load-file-data
        (merge-pathnames
             (make-pathname :directory '(:relative "stage-1")
                            :name "question_065729_len_008"
                            :type "lisp")
             (get-configuration *experiment* :challenge-files-root)))))
(loop repeat 10 do (run-interaction *experiment*))

;; question_017092_len_008.lisp => What is the material of the big purple cylinder?
;; => learn what-is-the-material-of-the-big-purple-X and cube-lex and cylinder-lex
(setf (question-data *experiment*)
      (list
       (load-file-data
        (merge-pathnames
             (make-pathname :directory '(:relative "stage-1")
                            :name "question_017092_len_008"
                            :type "lisp")
             (get-configuration *experiment* :challenge-files-root)))))
(loop repeat 10 do (run-interaction *experiment*))

;; question_137460_len_008.lisp => What is the material of the big purple sphere?
;; => learn sphere-lex
(setf (question-data *experiment*)
      (list
       (load-file-data
        (merge-pathnames
             (make-pathname :directory '(:relative "stage-1")
                            :name "question_137460_len_008"
                            :type "lisp")
             (get-configuration *experiment* :challenge-files-root)))))
(run-interaction *experiment*)










;; add-th-links
;; question_065729_len_008.lisp => What is the material of the big purple cube?
(setf (question-data *experiment*)
      (list
       (load-file-data
        (merge-pathnames
             (make-pathname :directory '(:relative "stage-1")
                            :name "question_065729_len_008"
                            :type "lisp")
             (get-configuration *experiment* :challenge-files-root)))))
(loop repeat 5 do (run-interaction *experiment*))

;; question_080785_len_008.lisp => What is the material of the big red cube?
(setf (question-data *experiment*)
      (list
       (load-file-data
        (merge-pathnames
             (make-pathname :directory '(:relative "stage-1")
                            :name "question_080785_len_008"
                            :type "lisp")
             (get-configuration *experiment* :challenge-files-root)))))
(loop repeat 10 do (run-interaction *experiment*))
;; => learn what-is-the-material-of-the-big-X-cube and purple-lex and red-lex

;; question_040079_len_006.lisp => How many small blue cubes are there?
(setf (question-data *experiment*)
      (list
       (load-file-data
        (merge-pathnames
             (make-pathname :directory '(:relative "stage-1")
                            :name "question_040079_len_006"
                            :type "lisp")
             (get-configuration *experiment* :challenge-files-root)))))
(loop repeat 10 do (run-interaction *experiment*))

;; question_097431_len_006.lisp => How many small green cubes are there?
(setf (question-data *experiment*)
      (list
       (load-file-data
        (merge-pathnames
             (make-pathname :directory '(:relative "stage-1")
                            :name "question_097431_len_006"
                            :type "lisp")
             (get-configuration *experiment* :challenge-files-root)))))
(loop repeat 10 do (run-interaction *experiment*))
;; => learn how-many-small-X-cubes-are-there and blue-lex and green-lex



;; question_040444_len_008.lisp => What is the material of the big blue cube?
(setf (question-data *experiment*)
      (list
       (load-file-data
        (merge-pathnames
             (make-pathname :directory '(:relative "stage-1")
                            :name "question_040444_len_008"
                            :type "lisp")
             (get-configuration *experiment* :challenge-files-root)))))
(run-interaction *experiment*)
;; => add th links


;; question_058375_len_008.lisp => What is the material of the big green cube?
(setf (question-data *experiment*)
      (list
       (load-file-data
        (merge-pathnames
             (make-pathname :directory '(:relative "stage-1")
                            :name "question_058375_len_008"
                            :type "lisp")
             (get-configuration *experiment* :challenge-files-root)))))
(run-interaction *experiment*)
;; => add th links

;; question_022818_len_006.lisp => How many small purple cubes are there?
(setf (question-data *experiment*)
      (list
       (load-file-data
        (merge-pathnames
             (make-pathname :directory '(:relative "stage-1")
                            :name "question_022818_len_006"
                            :type "lisp")
             (get-configuration *experiment* :challenge-files-root)))))
(run-interaction *experiment*)
;; => add th links

;; question_025486_len_006.lisp => How many small red cubes are there?
(setf (question-data *experiment*)
      (list
       (load-file-data
        (merge-pathnames
             (make-pathname :directory '(:relative "stage-1")
                            :name "question_025486_len_006"
                            :type "lisp")
             (get-configuration *experiment* :challenge-files-root)))))
(run-interaction *experiment*)
;; => add th links