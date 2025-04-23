;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loading the package and setup ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ql:quickload :clevr-grammar-learning)
(in-package :cgl)

(activate-monitor trace-fcg)
(activate-monitor trace-irl)
(activate-monitor trace-interactions-in-wi)

(defun inspect-clevr-scene (index experiment)
  "Open the clevr image and show a symbolic description
   of the scene on the web interface"
  (let* ((scene (get-scene-by-index (world experiment) index))
         (img-src-path (image scene)))
    (open-file-in-os img-src-path)
    (add-element `((h2) ,(pathname-name img-src-path)))
    (add-element (make-html scene))))

(defun set-interaction-data (experiment scene-index question answer)
  "Set a scene, question, and answer for the following interaction"
  (let ((scene (get-scene-by-index (world experiment) scene-index)))
    (setf (question-data experiment)
          (list (cons question (list (cons (name scene) answer)))))))

;; you get this folder from Canvas
(setf *clevr-data-path*
      (parse-namestring "/Users/jensnevens/ehaidocs/Vakken/2022.NLP/wpo/wpo-8/CLEVR/"))

(defparameter *experiment*
   (make-instance 'clevr-learning-experiment
                  :entries '((:determine-interacting-agents-mode . :tutor-learner)
                             (:challenge-files-root . "/Users/jensnevens/")
                             (:questions-per-challenge . 1)
                             (:question-sample-mode . :all)
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


(inspect-clevr-scene 0 *experiment*) ;; open scene image and toont scene op web interface
(set-interaction-data *experiment* 0 "Hoeveel kubussen zijn er?" 2)
(run-interaction *experiment*)
(set-interaction-data *experiment* 1 "Hoeveel kubussen zijn er?" 3)
(run-interaction *experiment*)
(set-interaction-data *experiment* 0 "Hoeveel cylinders zijn er?" 2)
(run-interaction *experiment*)
(set-interaction-data *experiment* 1 "Hoeveel cylinders zijn er?" 6)
(run-interaction *experiment*)
(set-interaction-data *experiment* 2 "Hoeveel cylinders zijn er?" 3)
(run-interaction *experiment*)
(set-interaction-data *experiment* 3 "Hoeveel cylinders zijn er?" 3)
(run-interaction *experiment*)
(set-interaction-data *experiment* 4 "Hoeveel cylinders zijn er?" 3)
(run-interaction *experiment*)

(set-interaction-data *experiment* 0 "Welke kleur heeft de bal?" 'purple)
(run-interaction *experiment*)

(set-interaction-data *experiment* 0 "Zijn er cylinders?" 'yes)
(run-interaction *experiment*)