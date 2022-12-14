(in-package :naming-game)

;;see if interaction is successful
(Define-monitor record-communicative-success
                :class 'data-recorder
                :average-window 100
                :documentation "records the game outcome of each game (1 or 0).")

(define-monitor display-communicative-success ;needs to be activated
                :class 'gnuplot-display
                :documentation "Plots the communicative success."
                :data-sources '((average record-communicative-success))
                :update-interval 100
                :caption '("communicative success")
                :x-label "# Games" 
                :y1-label "Communicative Success" 
                :y1-max 1.0 :y1-min 0 
                :draw-y1-grid t)

(define-monitor export-communicative-success ;idem
                :class 'lisp-data-file-writer
                :documentation "Exports communicative success"
                :data-sources '((average record-communicative-success))
                :file-name (babel-pathname :name "communicative-success" :type "lisp"
                                           :directory '("tutorial" "raw-data" "naming-game"))
                :add-time-and-experiment-to-file-name nil
                :column-separator " "
                :comment-string "#")

;; get vocabulary size
(Define-event-handler (record-communicative-success interaction-finished)
    (let ((speaker (first (interacting-agents interaction))))
      (record-value monitor (if (communicated-successfully speaker) 1 0))))



(define-monitor record-lexicon-size
                :class 'data-recorder
                :average-window 1
                :documentation "records the avg lexicon size.")

(define-monitor export-lexicon-size
                :class 'lisp-data-file-writer
                :documentation "Exports lexicon size"
                :data-sources '(record-lexicon-size)
                :file-name (babel-pathname :name "lexicon-size" :type "lisp"
                                           :directory '("tutorial" "raw-data" "naming-game"))
                :add-time-and-experiment-to-file-name nil
                :column-separator " "
                :comment-string "#")

(defun get-lexicon-size (agent)
  (length (lexicon agent)))

(define-event-handler (record-lexicon-size interaction-finished)
    (let ((agent-1 (first (agents experiment))))
      (record-value monitor (get-lexicon-size agent-1))))

(define-event-handler (export-lexicon-size interaction-finished))
