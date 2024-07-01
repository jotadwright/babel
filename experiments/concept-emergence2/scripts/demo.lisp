(ql:quickload :cle)

(in-package :cle)

;; experiments with entrenchment values - keep the same
(progn
  (defparameter *baseline-simulated*
    (make-configuration
     :entries `(
                ;; monitoring
                (:dot-interval . 100)
                (:usage-table-window . 100)
                (:save-distribution-history . nil)
                ;; setup interacting agents
                (:interacting-agents-strategy . :standard)
                (:population-size . 10)
                ;; setup data scene
                (:dataset . "clevr")
                (:dataset-split . "train")
                ;(:data-fname . "all.lisp")
                (:available-channels ,@(get-all-channels :clevr))
                ;; disable channels
                (:disable-channels . :none)
                (:amount-disabled-channels . 0)
                ;; noised channels
                (:sensor-noise . :none)
                (:sensor-std . 0.0)
                (:observation-noise . :none)
                (:observation-std . 0.0)
                ;; scene sampling
                (:scene-sampling . :random)
                (:topic-sampling . :random)
                ;; general strategy
                (:align . t)
                (:similarity-threshold . 0.0)
                ;; entrenchment of constructions
                (:initial-cxn-entrenchement . 0.5)
                (:entrenchment-incf . 0.1)
                (:entrenchment-decf . -0.1)
                (:entrenchment-li . -0.02) ;; lateral inhibition
                ;; new options
                (:trash-threshold . 0.0)
                (:slow-threshold . -0.1)
                (:conceptualisation-heuristics . :heuristic-1)
                (:speaker-competitors . nil)
                (:hearer-competitors . nil)
                ;; concept representations
                (:concept-representation . :distribution)
                (:distribution . :gaussian-welford)
                (:M2 . 0.0001) ;; only for gaussian-welford
                ;; prototype weight inits
                (:weight-update-strategy . :j-interpolation)
                (:initial-weight . 0)
                (:weight-incf . 1)
                (:weight-decf . -5)
                ;; staging
                (:switch-condition . :none) ; :after-n-interactions)
                (:switch-conditions-after-n-interactions . 2500) 
                (:stage-parameters nil)
                ;; saving
                (:experiment-name . "test")
                (:output-dir . "test")
                )))
  (setf *experiment* (make-instance 'cle-experiment :configuration *baseline-simulated*))
  (notify reset-monitors)
  (wi::reset))

(progn
  (wi::reset)
  (notify reset-monitors)
  (deactivate-all-monitors)
  (activate-monitor export-communicative-success)
  (activate-monitor export-lexicon-coherence)
  ;(activate-monitor export-lexicon-size-fast)
  ;(activate-monitor export-lexicon-size-slow)
  ;(activate-monitor export-lexicon-size-trash)
    ;(activate-monitor export-unique-form-usage)
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor record-time)
  (format t "~%---------- NEW GAME ----------~%")
  (time
   (loop for i from 1 to 100000
         do (run-interaction *experiment*))))

(progn
  (wi::reset)
  (deactivate-all-monitors)
  (activate-monitor trace-interaction-in-web-interface)
  (loop for idx from 1 to 1
        do (run-interaction *experiment*)))

;;;;;;;

(get-configuration *experiment* :dataset)
(get-configuration *experiment* :dataset-split)



(add-element `((h2) ,(format nil "~a" (file-namestring fpath))))

(defun show-image (object)
  `((div :class "image" :style ,(format nil "margin-left: 50px; margin-bottom: 20px; width: fit-content; border-radius: 8px; overflow: hidden; border: 1px; border-color: #000000; box-shadow: 8px 8px 12px 1px rgb(0 0 0 / 10%);"))
                 ((img :src ,(string-append
                              cl-user::*localhost-user-dir*
                              (mkstr (make-pathname :directory
                                                    `(:relative
                                                      "Corpora/mscoco/train2014")
                                                    :name
                                                    (assqv :fname (description object)))))))))
(setf pipi (current-scene (world *experiment*)))

(show-image (first (objects (current-scene (world *experiment*)))))


(display-lexicon (first (agents *experiment*)) :ENTRENCHMENT-THRESHOLD 0.5 :CERTAINTY-THRESHOLD 0.1 :sort t)

(setf zakufi (find-form-in-lexicon (lexicon (first (agents *experiment*))) "zakufi"))
                   
(length (loop for cxn being the hash-values in (fast-inventory (lexicon (first (agents *experiment*))))
              if (> (score cxn) 0.6)
                collect cxn))


;;;;;;;


(progn
  (setf *experiment*
        (cl-store:restore (babel-pathname :directory '("experiments"
                                                       "concept-emergence2"
                                                       "storage"
                                                       "dinov2"
                                                       )
                                          :name "1-history"
                                          :type "store"))))


(setf ag (first (agents *experiment*)))

(lexicon-size (lexicon ag))


(set-configuration *experiment* :align nil)

(initialise-world *experiment*)


(loop for key being the hash-keys of (fast-inventory (lexicon (first (agents *experiment*))))
       collect key)


(setf new-lexicon (sort (loop for cxn being the hash-values of
                                (get-inventory (lexicon (first (agents *experiment*))) :fast) collect cxn)
                        #'(lambda (x y) (> (score x) (score y)))))


#|(let* ((agent (first (agents *experiment*)))
       (lexicon (loop for cxn being the hash-values of (get-inventory (lexicon agent) :fast) collect cxn))
       (new-lexicon (sort lexicon #'(lambda (x y) (> (score x) (score y))))))
  new-lexicon)|#

(loop for cxn in new-lexicon
      do (add-cxn-to-interface cxn :certainty))

(setf vobowe (find-form-in-lexicon (lexicon (first (agents *experiment*))) "vobowe"))
 

;;;;;

read-scene-ids







(map 'list #'parse-integer (uiop:read-file-lines
                            (concatenate 'string
                                         "~/Corpora/concept-emergence2/"
                                         (mkstr (make-pathname :directory
                                                               `(:relative "gqaglove50")
                                                               :name
                                                               "all.lisp")))))

(defun read-scene-ids (fname)
  (let* ((base-dir "~/Corpora/concept-emergence2/gqaglove50/")
         (fpath (concatenate 'string base-dir fname))
         (raw (uiop:read-file-lines fpath))
         (scene-ids (map 'list #'parse-integer raw)))
    scene-ids))

(defun get-scene-by-indexes (experiment)
  (let ((scene-ids (get-configuration experiment :scene-ids))
        (current-idx (get-configuration experiment :current-scene-idx)))
    (set-configuration experiment :current-scene-idx (mod (+ current-idx 1) (length scene-ids)))
    (get-scene-by-index (world experiment) (nth current-idx scene-ids))))

(read-scene-ids
 (mkstr (make-pathname :directory
                       `(:relative ,dataset)
                       :name
                       (get-configuration experiment :data-fname))))
;;;;;




(lexicon-size (lexicon (first (agents *experiment*))))


(add-cxn-to-interface (first new-lexicon))



(setf cxns50 (loop for cxn in new-lexicon
                   if (= (score cxn) 0.5)
                     collect cxn))

(length cxns50)

(setf res
      (loop for cxn1 in cxns50 and idx from 0
            do (format t "~%~a," idx)
            do (loop for cxn2 in cxns50
                     collect (format nil "~,3f," (similar-concepts (first (agents *experiment*))
                                                                   (meaning cxn1)
                                                                   (meaning cxn2))))))

(nr-of-samples (distribution (gethash 'dim-0 (prototypes (meaning (nth 723 new-lexicon))))))

(loop for cxn in new-lexicon and idx from 1
      for avg-weight = (average-weight cxn)
      for score = (score cxn)
      for samples = (nr-of-samples (distribution (gethash 'dim-0 (prototypes (meaning cxn)))))
      ; if 
      do (format t "~% ~a. ~,2f [score = ~,2f] - [samples = ~a]" idx avg-weight score samples))

(setf cxnio (nth 6 new-lexicon))
(average-weight cxnio)




;;;;;;;


(setf lexor1 (lexicon (first (agents *experiment*))))
(setf lexor2 (lexicon (first (agents *experiment2*))))
(setf lexor2 (lexicon (first (agents *experiment3*))))


(setf inventory (loop for cxn being the hash-values of (fast-inventory (lexicon (first (agents *experiment3*))))
                      collect cxn))

(setf sorted-inventory (sort inventory #'(lambda (x y) (> (score x) (score y)))))

(agents *experiment3*)

(loop for experiment in (list *experiment* *experiment1* *experiment2* *experiment3*)
      do (loop for agent in (agents experiment)
               for lexicon = (lexicon agent)
               sum (hash-table-count (fast-inventory lexicon)) into fasts
               sum (hash-table-count (slow-inventory lexicon)) into slows
               sum (hash-table-count (trash-inventory lexicon)) into trashes
               finally (format t "~% ~a / ~a /~a "
                               (float (/ fasts (length (agents experiment))))
                               (float (/ slows (length (agents experiment))))
                               (float (/ trashes (length (agents experiment)))))))

(display-lexicon (second (agents *experiment*)) :entrenchment-threshold 0.1 :certainty-threshold 0.1 :sort t)

(similar-concepts
 (second (agents *experiment*))
 (meaning (find-form-in-lexicon (lexicon (second (agents *experiment*))) "woxuzu"))
 (meaning (find-form-in-lexicon (lexicon (second (agents *experiment*))) "xogiwo") ))



(initialise-world *experiment*)















