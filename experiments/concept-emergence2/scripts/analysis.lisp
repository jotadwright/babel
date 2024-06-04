(ql:quickload :cle)

(in-package :cle)


;; ------------------
;; + Graph creation +
;; ------------------
#|(graph-batch-experiments2 "2023-8-mid_august4"
                          "ra"
                          "clevr"
                          `()
                          `()
                         ;:plot :communicative-success
                         ;:plot :lexicon-coherence
                          :plot :unique-form-usage
                          :y-min 0
                          :y-max 100
                         ;:plot :lexicon-size
                         ;:y-max 10
                          :start  950000
                          :end   1000000
                          :average-windows 5000
                          )|#

(get-statistics "2023-8-mid_august3"
                "tuning2"
                `(
                  (:similarity-threshold 0.0)
                  (:initial-weight 0 35)
                  (:weight-decf -1 -5 -10 -20)
                  (:entrenchment-li -0.001 -0.005 -0.01 -0.02); -0.05 -0.1)
                  (:trash-concepts nil t)
                  )
                         
                `(;(:similarity-threshold 0.0 0.01 0.05); 0.1 0.2)
                  (:initial-weight 0 35)
                  (:weight-decf -1 -5 -10 -20)
                  (:entrenchment-li -0.001 -0.005 -0.01 -0.02); -0.05 -0.1)
                  (:trash-concepts t)
                  )
                )

;;;;;;;;;;;;;









;;;;;;;;;;;;;
#|(create-graph-comparing-strategies
 :base-dir "2023-8-mid_august3/ra/"
 :experiment-name 

 
 :experiment-names '("clevr-di-0-defects-in-half-of-pop/2023-08-22_2h21m51s-exp-0"
                     "clevr-di-1-defects-in-half-of-pop/2023-08-22_2h21m53s-exp-0"
                     "clevr-di-2-defects-in-half-of-pop/2023-08-22_2h21m53s-exp-0"
                     "clevr-di-5-defects-in-half-of-pop/2023-08-22_2h22m30s-exp-0"
                     "clevr-di-10-defects-in-half-of-pop/2023-08-22_2h21m51s-exp-0"
                     )
 :measure-name "communicative-success"
 :y1-label "Communicative success"
 :y-max 1
 :average-windows 200
 :captions '("14-12-2022-exp-1"
             "14-12-2022-exp-2"
             "14-12-2022-exp-3"
             "14-12-2022-exp-4"
             ))|#


;(list (length exp-names) title exp-names captions)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|(create-graph-for-single-strategy "2023-8-mid_august4/ra" "clevr/clevr-ra/2023-08-22_2h19m47s-exp-0" '("communicative-success" "lexicon-coherence" "unique-form-usage")
                                  ;:plot-file-name "plot-randomscenes"
                                  :average-windows '(5000 5000 5000)
                                  :use-y-axis '(1 1 2)
                                  :y1-min 0 :y1-max 1
                                  :y2-min 0 :y2-max 100
                                  :x-label "Number of Games"
                                  :y1-label "Communicative Success/Lexicon Coherence"
                                  :y2-label "Number of unique forms used"
                                  :captions '("communicative success"
                                              "lexicon coherence"
                                              "Unique forms used (last 5k)")
                                  :error-bars '(:percentile 5 95)
                                  :error-bar-modes '(:lines)
                                  :key-location "bottom"
                                  :open t)

(create-graph-comparing-strategies
 :experiment-names '("14-12-2022-exp-1"
                     "14-12-2022-exp-2"
                     "14-12-2022-exp-3"
                     "14-12-2022-exp-4"
                     )
 :measure-name "communicative-success"
 :y1-label "Communicative success"
 :y-max 1
 :average-windows 200
 :captions '("14-12-2022-exp-1"
             "14-12-2022-exp-2"
             "14-12-2022-exp-3"
             "14-12-2022-exp-4"
             ))

(create-graph-comparing-strategies
 :experiment-names '("exp-standard"
                     "exp-interpolation"
                     )
 :measure-name "lexicon-coherence"
 :y1-label "Lexicon coherence"
 :y-max 1
 :average-windows 500
 :captions '("exp-standard"
             "exp-interpolation"
             ))

(create-graph-comparing-strategies
 :experiment-names '("exp-standard"
                     "exp-interpolation"
                     )
 :measure-name "lexicon-size"
 :y1-label "lexicon size"
 :y-max 50
 :average-windows 200
 :captions '("exp-standard"
             "exp-interpolation"
             ))|#


;;;;

#|(setf an-experiment (cl-store:restore (babel-pathname :directory `("experiments"
                                                                   "concept-emergence2"
                                                                   "logging"
                                                                   "10-all-random-bugfix"
                                                                   "2023-06-29_18h51m17s-exp-0")
                                                      :name "history"
                                                      :type "store")))|#

;;;;;;;
;;/Users/jerome/Projects/^productivity/plot-babel-fast/storage/dinov2/dinov2-1/2024-04-11_11h20m33s-exp-0-22481/stores/1-history-stage-1.store

(progn
  (setf *experiment*
        (cl-store:restore (babel-pathname :directory '("experiments"
                                                       "concept-emergence2"
                                                       "storage"
                                                       "dinov2"
                                                       "df"
                                                       "dinov2-4"
                                                       "stores"
                                                     
                                                       )
                                          :name "1"
                                          :type "store"))))

(progn
  (set-configuration *experiment* :align nil)

  (set-configuration *experiment* :dot-interval 10)
  (set-configuration *experiment* :dataset-split "test")
    ;(set-configuration *experiment* :topic-sampling :random)
  (set-configuration *experiment* :align nil)
  (initialise-world *experiment*))

(initialise-world *experiment*)

(display-lexicon (first (agents *experiment*)) :entrenchment-threshold 0.1 :certainty-threshold 0.5 :sort t)


(defun print-hash-entry (key value)
    (format t "~S: [~{\"~a~^\", ~}\"],~%" key value))

(defun context-usage-dictionary ()
  (loop with hash-table = (make-hash-table :test 'equal)
        for idx from 1 to 5000
        for ag = (first (interacting-agents (current-interaction *experiment*)))
        for used-form = (find-data ag 'applied-cxn)
        do (format t "~% ~a." idx)
        if (and used-form (gethash (form used-form) hash-table))
          do (setf (gethash (form used-form) hash-table) (adjoin (assqv :name (description (find-data ag 'topic)))
                                                                 (gethash (form used-form) hash-table)))
        else
          do (when used-form
               (setf (gethash (form used-form) hash-table) (list (assqv :name (description (find-data ag 'topic))))))
        do (run-interaction *experiment*)
        finally (return hash-table)))

(progn
  (setf ht (context-usage-dictionary)))

(progn
  (format t "{ ")
  (maphash #'print-hash-entry ht)
  (format t " }"))

;;;

(defun average-weight (cxn)
  (loop for prototype being the hash-values of (prototypes (meaning cxn))
        sum (weight prototype) into weight-sum
        finally (return (/ weight-sum (hash-table-count (prototypes (meaning cxn)))))))

(loop for cxn being the hash-values of (trash-inventory (lexicon (first (agents *experiment*))))
      collect (average-weight cxn))


(hash-table-count (fast-inventory (lexicon (first (agents *experiment*)))))
(hash-table-count (trash-inventory (lexicon (first (agents *experiment*)))))


;;;





#|(setf results (testi3 an-experiment))






;;;;


(loop for exp in (mapcar (lambda (x) (first (last (pathname-directory x))))
                         (uiop:subdirectories (babel-pathname :directory '("experiments" "concept-emergence2"
                                                                           "logging" "similarity"
                                                                           "10-channels"
                                                                           ))))
      do (setf *experiment*
               (cl-store:restore (babel-pathname :directory `("experiments"
                                                              "concept-emergence2"
                                                              "logging"
                                                              "similarity"
                                                              "10-channels"
                                                              ,exp)
                                                 :name "history"
                                                 :type "store")))
         (let ((results (testi)))
           (destructuring-bind (not-coherent
                                not-solvable+coherent
                                not-solvable+not-coherent
                                coherent)
               results
             (format t "~% exp: ~a => ~a, ~a"
                     exp
                     (/ (sum (mapcar (lambda (lst) (float (sum (mapcar (lambda (x) (if (fourth x) 1 0)) lst)))) results))
                        (sum (mapcar #'length results)))
                     (float (/ (+ (length not-solvable+coherent)
                                  (length coherent))
                               (sum (mapcar #'length results))))))))
      
;;;;

(defun string-to-list (str)
  (if (not (streamp str))
    (string-to-list (make-string-input-stream str))
    (if (listen str)
      (cons (read str) (string-to-list str))
      nil)))

(defun read-config (fname)
  (let* ((base-dir "~/Projects/babel/experiments/concept-emergence2/logging/similarity/10-channels/")
         (fpath (concatenate 'string base-dir fname "/experiment-configurations.lisp"))
         (raw (uiop:read-file-lines fpath))
         (config (first (string-to-list (first raw)))))
    config))


(let* ((path (babel-pathname :directory '("experiments"
                                          "concept-emergence2"
                                          "logging"
                                          "similarity"
                                          "3-channels"
                                          )))
       (dir-paths (uiop:subdirectories path)))
  (loop for dir-path in dir-paths
        for dir-name = (first (last (pathname-directory dir-path)))
        for config = (read-config dir-name)
        for clevr-channels = (assqv :CHANNELS config)
        do (format t "~% ~a" (cons dir-name (list-of-strings->string (mapcar #'mkstr clevr-channels) :separator "-")))))


(let* ((path (babel-pathname :directory '("experiments"
                                          "concept-emergence2"
                                          "logging"
                                          "similarity"
                                          "10-channels"
                                          )))
       (dir-paths (uiop:subdirectories path)))
  (loop for dir-path in dir-paths
        for dir-name = (first (last (pathname-directory dir-path)))
        for config = (read-config dir-name)
        for info = (assqv :SIMILARITY-THRESHOLD config)
        
        do (format t "~% ~a" (cons dir-name info))))
          
 

;; restore experiments
        
(setf *experiment2*
      (cl-store:restore (babel-pathname :directory '("experiments"
                                                     "concept-emergence2"
                                                     "logging"
                                                     "similarity"
                                                     "10-channels"
                                                     "s005-2023-06-21_16h22m41s-exp-0")
                                        :name "history"
                                        :type "store")))

(display-lexicon (first (agents *experiment2*)) :sort t :entrenchment-threshold 0.1)



0.9938, 0.948


(progn
  (setf results (testi3 (cl-store:restore (babel-pathname :directory `("experiments"
                                                                       "concept-emergence2"
                                                                       "logging"
                                                                       "similarity"
                                                                       "10-channels"
                                                                       "s0-2023-06-22_9h38m52s-exp-1")
                                                          :name "history"
                                                          :type "store"))))

  (destructuring-bind (not-coherent
                                not-solvable+coherent
                                not-solvable+not-coherent
                                coherent)
               results

  
  (let* ((lex-coh (mapcar (lambda (x) (float (/ x (sum (mapcar #'length results))))) (mapcar #'length results)))
         (com-suc (mapcar (lambda (lst)
                            (float (/ (sum (mapcar (lambda (x) (if (fourth x) 1 0)) lst)) (length lst))))
                          results))
         (gen-com-suc (/ (sum (mapcar (lambda (lst) (float (sum (mapcar (lambda (x) (if (fourth x) 1 0)) lst)))) results))
                         (sum (mapcar #'length results))))
         (gen-lex-coh (float (/ (+ (length not-solvable+coherent)
                                   (length coherent))
                                (sum (mapcar #'length results))))))
    (format t "~a: ~a, ~a, ~a, ~a"
            "s0-2023-06-22_9h38m52s-exp-1"
            lex-coh
            com-suc
            gen-com-suc
            gen-lex-coh)))

(length not-coherent)
(length

(float (/ (+ (length not-solvable+coherent)
             (length coherent))
          (sum (mapcar #'length results))))
  
                

;; not-coherent not-solvable+coherent not-solvable-and-coh-fail scenes-with-coherence
;; => lex coherenc (0.0248    0.7334 0.0616    0.1802)

;; => comm success (0.8145161 1.0    0.8961039 1.0)

;; => general comm success 0.989

;; => general lex coherence 0.914




(defun testi3 (experiment)
  (progn
    (wi::reset)
    (deactivate-all-monitors)
    ;; run to find a scene without lex coherence
    (loop with not-coherent = '()
          with not-solvable+coherent = '()
          with not-solvable+not-coherent = '()
          with scenes-with-coherence = '()
          for i from 1 to 20000
          for speaker = (speaker (first (interactions experiment)))
          for hearer = (hearer (first (interactions experiment)))
          for lex-coherence = (find-data (current-interaction experiment) 'lexicon-coherence)
          for comm-success = (communicated-successfully (first (interactions experiment)))
          do (when (zerop (mod i 100))
               (format t "~a " i))
          if (and (not lex-coherence)
                  (not (is-discriminative-strict (find-data (first (interacting-agents (current-interaction experiment))) 'topic)
                                                 (remove (find-data (first (interacting-agents (current-interaction experiment))) 'topic)
                                                         (objects (find-data (first (interacting-agents (current-interaction experiment))) 'context))))))
            do (progn
                 (setf not-solvable+not-coherent (cons (list (interacting-agents (current-interaction experiment))
                                                             (index (current-scene (world experiment)))
                                                             (find-data (first (interacting-agents (current-interaction experiment))) 'topic)
                                                             comm-success)
                                                       not-solvable+not-coherent
                                                       ))
                 (run-interaction experiment))
          else if (not (is-discriminative-strict (find-data (first (interacting-agents (current-interaction experiment))) 'topic)
                                                 (remove (find-data (first (interacting-agents (current-interaction experiment))) 'topic)
                                                         (objects (find-data (first (interacting-agents (current-interaction experiment))) 'context)))))
                 do (progn
                      (setf not-solvable+coherent (cons (list (interacting-agents (current-interaction experiment))
                                                              (index (current-scene (world experiment)))
                                                              (find-data (first (interacting-agents (current-interaction experiment))) 'topic)
                                                              comm-success)
                                                        not-solvable+coherent
                                                        ))
                      (run-interaction experiment))
            else if (not lex-coherence)
                   do (progn
                        (setf not-coherent (cons (list (interacting-agents (current-interaction experiment))
                                                       (index (current-scene (world experiment)))
                                                       (find-data (first (interacting-agents (current-interaction experiment))) 'topic)
                                                       comm-success)
                                                 not-coherent
                                                 ))
                        (run-interaction experiment)
                        )
                 else
                   do (progn
                        (setf scenes-with-coherence (cons (list (interacting-agents (current-interaction experiment))
                                                                (index (current-scene (world experiment)))
                                                                (find-data (first (interacting-agents (current-interaction experiment))) 'topic)
                                                                comm-success)
                                                          scenes-with-coherence
                                                          ))
                        (run-interaction experiment))
          finally (return (list not-coherent not-solvable+coherent not-solvable+not-coherent scenes-with-coherence)))))



(setf *experiment*
      (cl-store:restore (babel-pathname :directory '("experiments"
                                                     "concept-emergence2"
                                                     "logging"
                                                     "similarity22june"
                                                     "2023-06-21_16h30m19s-exp-1")
                                        :name "history"
                                        :type "store")))



(add-element `((h2) "-----"))
(display-lexicon  (first (agents *experiment*)) :sort t :entrenchment-threshold 0.1)
(length (lexicon (first (agents *experiment*))))|#