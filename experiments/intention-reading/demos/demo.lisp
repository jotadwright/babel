;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loading the package and setup ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ql:quickload :clevr-learning)
(in-package :intention-reading)

(activate-monitor trace-fcg)
(activate-monitor trace-irl)
(activate-monitor trace-interactions-in-wi)

;;;; HELPER FUNCTION
;;;; ---------------

(defun load-file-data (file)
  "Function to load a specific data file
   for demo purposes."
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

(defun cxn-with-meaning (cxn-inventory meaning)
  (loop for cxn in (constructions-list cxn-inventory)
        for cxn-meaning = (extract-meaning-predicates cxn)
        thereis (equivalent-irl-programs? cxn-meaning meaning)))

(defun run-interactions-until-cxn-with-meaning (experiment meaning)
  "Run interactions until the agent has learned a cxn with the given meaning"
  (loop
     do (run-interaction experiment)
     until (cxn-with-meaning (grammar (learner experiment)) meaning)))

(defun run-interactions-until-cxn-inventory-size (experiment n)
  "Run interactions until the agent's cxn inventory has size n"
  (loop 
     do (run-interaction experiment)
     until (length= (constructions-list (grammar (learner experiment))) n)))

;;;; EXPERIMENT
;;;; ----------

(defparameter *experiment*
   (make-instance 'clevr-learning-experiment
                  :entries '((:determine-interacting-agents-mode . :tutor-learner)
                             (:questions-per-challenge . 100)
                             (:scenes-per-question . 50)
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



;; LEARN HOLOPHRASE
;; question_000210_len_005.lisp => "What material is the cyan cube?"
(progn
  (setf (question-data *experiment*)
        (list (load-file-data
               (merge-pathnames
                (make-pathname :directory '(:relative "stage-1")
                               :name "question_000210_len_005" :type "lisp")
                (get-configuration *experiment* :challenge-files-root)))))
  (run-interactions-until-cxn-with-meaning
   *experiment* '((get-context ?context)
                  (filter ?set-1 ?context ?shape-1)
                  (bind shape-category ?shape-1 cube)
                  (filter ?set-2 ?set-1 ?color-1)
                  (bind color-category ?color-1 cyan)
                  (unique ?object-1 ?set-2)
                  (query ?target ?object-1 ?attribute-1)
                  (bind attribute-category ?attribute-1 material)))
  (run-interactions-until-cxn-inventory-size *experiment* 1))





;; SUBSTITUTION
;; question_000450_len_005.lisp => "What material is the big cube?"
(progn
  (setf (question-data *experiment*)
        (list (load-file-data
               (merge-pathnames
                (make-pathname :directory '(:relative "stage-1")
                               :name "question_000450_len_005" :type "lisp")
                (get-configuration *experiment* :challenge-files-root)))))
  (run-interactions-until-cxn-with-meaning
   *experiment* '((bind size-category ?size large)))
  (run-interactions-until-cxn-inventory-size *experiment* 4))




;; LEARN FROM PARTIAL MEANING
;; question_007114_len_006.lisp => "What shape is the big cyan object?"
(progn
  (setf (question-data *experiment*)
        (list (load-file-data
               (merge-pathnames
                (make-pathname :directory '(:relative "stage-1")
                               :name "question_007114_len_006" :type "lisp")
                (get-configuration *experiment* :challenge-files-root)))))
  (run-interaction *experiment*))





















;; question_146153_len_005.lisp => ""What material is the gray cube?""
;; Learn gray-cxn
(progn
  (setf (question-data *experiment*)
        (list (load-file-data
               (merge-pathnames
                (make-pathname :directory '(:relative "stage-1")
                               :name "question_146153_len_005" :type "lisp")
                (get-configuration *experiment* :challenge-files-root)))))
  (run-interactions-until-cxn-with-meaning
   *experiment* '((bind color-category ?color gray)))
  (run-interaction *experiment*))




;; LEARN LINKS
;; question_022307_len_006.lisp => "What shape is the big gray object?"
;; add link to categorial network!
(progn
  (setf (question-data *experiment*)
        (list (load-file-data
               (merge-pathnames
                (make-pathname :directory '(:relative "stage-1")
                               :name "question_022307_len_006" :type "lisp")
                (get-configuration *experiment* :challenge-files-root)))))
  (run-interaction *experiment*))
