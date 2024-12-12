(in-package :au-lib)

(export '(trace-kswap-in-buffer
          kswap-started
          kswap-finished
          pair-added-to-phi
          pair-swapped-in-phi
          debug-add-event
          debug-swap-event))

;;;;
;;;; Trace kswap in output buffer
;;;; ----------------------------

(define-monitor trace-kswap-in-buffer :class 'trace-monitor)

(define-event kswap-started (G1 list) (G2 list))
(define-event kswap-finished (phi-delta-i number) (number-of-swaps number))
(define-event pair-added-to-phi (A-pair list) (A-pair-gain number) (phi list) (phi-delta-i number) (comp list))
(define-event pair-swapped-in-phi (phi-s list) (phi-c list) (phi list) (phi-delta-i number))

(define-event debug-add-event (Q-pair list) (candidates list) (gains hash-table))
(define-event debug-swap-event (phi-s list) (phi-c list) (A-pair list) (phi-s-rank number) (phi-c-rank number))


(define-event-handler (trace-kswap-in-buffer kswap-started)
  (format-monitor "~%~%#### STARTING K-SWAP ####")
  (format-monitor "~%##########################")
  (format-monitor "~%G1 is ~a" G1)
  (format-monitor "~%G2 is ~a" G2)
  (format-monitor "~%"))


(define-event-handler (trace-kswap-in-buffer kswap-finished)
  (format-monitor "~%##########################"))


(define-event-handler (trace-kswap-in-buffer pair-added-to-phi)
  (format-monitor "~%ADD")
  (format-monitor "~%---")
  (format-monitor "~%There are ~a compatible pairs" (length comp))
  (format-monitor "~%Added pair ~a" A-pair)
  (format-monitor "~%Current phi is ~a" (mkstr phi))
  (format-monitor "~%Current phi has Delta_I value of ~a" phi-delta-i)
  (format-monitor "~%Current phi has length ~a" (length phi))
  (format-monitor "~%"))


(define-event-handler (trace-kswap-in-buffer pair-swapped-in-phi)
  (format-monitor "~%SWAP")
  (format-monitor "~%----")
  (format-monitor "~%Swapped phi-s ~a~%with phi-c ~a" phi-s phi-c)
  (format-monitor "~%Current phi is ~a" (mkstr phi))
  (format-monitor "~%Current phi has Delta_I value of ~a" phi-delta-i)
  (format-monitor "~%Current phi has length ~a" (length phi))
  (format-monitor "~%"))

;;;;
;;;; Trace if 2-swap contains the added pair
;;;; -------------------------------------

(export '(added-pair-in-2-swap
          export-added-pair-in-2-swap
          test-added-pair-in-2-swap))

(define-monitor added-pair-in-2-swap :class 'data-recorder :average-window 0)

(define-monitor export-added-pair-in-2-swap
                :class 'lisp-data-file-writer
                :data-sources '(added-pair-in-2-swap)
                :file-name (parse-namestring "/Users/jensnevens/quicklisp/local-projects/k-swap-anti-unification/code/results/added-pair-in-2-swap.lisp") 
                :add-time-to-file-name t)

(define-event-handler (added-pair-in-2-swap debug-swap-event)
  (let ((value-to-record (if (member A-pair phi-s :test #'equal) 1 0)))
    (record-value monitor value-to-record)
    (setf (slot-value monitor 'values)
          (cons value-to-record (slot-value monitor 'values)))))

;;;;
;;;; Trace if 1-swap contains the added pair
;;;; ---------------------------------------

(export '(added-pair-in-1-swap
          test-added-pair-in-1-swap))

(define-monitor added-pair-in-1-swap :class 'data-recorder :average-window 1)

(define-event test-added-pair-in-1-swap (A-pair list) (phi-s list))

(define-event-handler (added-pair-in-1-swap test-added-pair-in-1-swap)
  (let ((value-to-record (if (equal A-pair phi-s) 1 0)))
    (record-value monitor value-to-record)
    (setf (slot-value monitor 'values)
          (cons value-to-record (slot-value monitor 'values)))))

;;;;
;;;; Trace if phi-s and phi-c have a pair in common
;;;; ----------------------------------------------

;; this would mean that the best 2-swap removes a pair (A,A') together with (B,B')
;; and adds (A,A') again, but now together with (C,C')
;; so this is actually a 1-swap that comes out as the best 2-swap?

(export '(count-phi-s-phi-c-overlap
          count-A-pair-overlap))

(define-monitor count-phi-s-phi-c-overlap :class 'data-recorder :average-window 1)
(define-monitor count-A-pair-overlap :class 'data-recorder :average-window 1)

(define-event-handler (count-phi-s-phi-c-overlap debug-swap-event)
  (let* ((overlap (intersection phi-s phi-c :test #'equal))
         (monitor-value (if overlap 1 0)))
    (assert (or (and (= (length phi-s) 1) (= (length phi-c) 1) (= (length overlap) 0))
                (and (= (length phi-s) 2) (= (length phi-c) 2)
                     (or (= (length overlap) 0) (= (length overlap) 1)))))
    (record-value monitor monitor-value)
    (setf (slot-value monitor 'values)
          (cons monitor-value (slot-value monitor 'values)))))

(define-event-handler (count-A-pair-overlap debug-swap-event)
  (let* ((overlap (intersection phi-s phi-c :test #'equal))
         (equals (equal (first overlap) A-pair))
         (monitor-value (if equals 1 0)))
    (record-value monitor monitor-value)
    (setf (slot-value monitor 'values)
          (cons monitor-value (slot-value monitor 'values)))))


;;;;
;;;; Trace if phi-s and phi-c have a predicate in common
;;;; ---------------------------------------------------

(export '(count-phi-s-phi-c-predicate-overlap
          count-A-pair-predicate-overlap))

(define-monitor count-phi-s-phi-c-predicate-overlap :class 'data-recorder :average-window 1)
(define-monitor count-A-pair-predicate-overlap :class 'data-recorder :average-window 1)

(define-event-handler (count-phi-s-phi-c-predicate-overlap debug-swap-event)
  ;; check if phi-s and phi-c have a predicate in common (so one part of a pair)
  ;; this would mean that ((A,A'),(B,B') are removed and ((A,C'),(D,D')) are added
  (let* ((overlap (intersection (append (mapcar #'first phi-s) (mapcar #'second phi-s))
                                (append (mapcar #'first phi-c) (mapcar #'second phi-c))
                                :test #'equal))
         (monitor-value (if overlap 1 0)))
    (record-value monitor monitor-value)
    (setf (slot-value monitor 'values)
          (cons monitor-value (slot-value monitor 'values)))))

(define-event-handler (count-A-pair-predicate-overlap debug-swap-event)
  (let* ((overlap (intersection (append (mapcar #'first phi-s) (mapcar #'second phi-s))
                                (append (mapcar #'first phi-c) (mapcar #'second phi-c))
                                :test #'equal))
         (A-pair-overlap (intersection overlap A-pair :test #'equal))
         (monitor-value (if A-pair-overlap 1 0)))
    (record-value monitor monitor-value)
    (setf (slot-value monitor 'values)
          (cons monitor-value (slot-value monitor 'values)))))


;;;;
;;;; Trace if Q-pair is completely removed from phi
;;;; ----------------------------------------------

(export '(Q-pair-removed-in-swap))

(define-monitor Q-pair-removed-in-swap :class 'data-recorder :average-window 1)

(define-event-handler (Q-pair-removed-in-swap debug-swap-event)
  ;; test if Q-pair is completely removed by the swap
  ;; this occurs when there is no overlap between phi-c and Q-pair
  ;; (on the level of predicates)
  (let* ((overlap (intersection (append (mapcar #'first phi-c) (mapcar #'second phi-c))
                                (butlast A-pair) :test #'equal))
         (monitor-value (if overlap 0 1)))
    (record-value monitor monitor-value)
    (setf (slot-value monitor 'values)
          (cons monitor-value (slot-value monitor 'values)))))

;;;;
;;;; Trace if there are multiple Q-pairs that have the same gain
;;;; -----------------------------------------------------------

(export '(same-gain-A-pairs))

(define-monitor same-gain-A-pairs :class 'data-recorder :average-window 1)

(define-event-handler (same-gain-A-pairs debug-add-event)
  ;; test if there are multiple candidate A-pairs that have the same gain as A-pair
  (let* ((other-candidates (remove Q-pair candidates :test #'equal))
         (Q-pair-gain (gethash Q-pair gains))
         (same-gain-p (loop for candidate in other-candidates
                            for candidate-gain = (gethash candidate gains)
                            thereis (= candidate-gain Q-pair-gain)))
         (monitor-value (if same-gain-p 1 0)))
    (record-value monitor monitor-value)
    (setf (slot-value monitor 'values)
          (cons monitor-value (slot-value monitor 'values)))))




;;;;
;;;; Data recorders
;;;; --------------

(export '(record-phi-delta-i
          record-number-of-swaps
          record-phi-s-ranks
          record-phi-c-ranks
          record-number-of-candidate-phi-s
          record-number-of-candidate-phi-c
          candidate-phi-s-generated
          candidate-phi-c-generated))


(define-monitor record-phi-delta-i :class 'data-recorder :average-window 0)
(define-monitor record-number-of-swaps :class 'data-recorder :average-window 0)
(define-monitor record-phi-s-ranks :class 'data-recorder :average-window 0)
(define-monitor record-phi-c-ranks :class 'data-recorder :average-window 0)
(define-monitor record-number-of-candidate-phi-s :class 'data-recorder :average-window 0)
(define-monitor record-number-of-candidate-phi-c :class 'data-recorder :average-window 0)

(define-event candidate-phi-s-generated (phi-s-candidates list))
(define-event candidate-phi-c-generated (phi-c-candidates list))


(define-event-handler (record-phi-delta-i kswap-finished)
  (record-value monitor phi-delta-i)
  (setf (slot-value monitor 'values)
        (cons phi-delta-i (slot-value monitor 'values))))

(define-event-handler (record-number-of-swaps kswap-finished)
  (record-value monitor number-of-swaps)
  (setf (slot-value monitor 'values)
        (cons number-of-swaps (slot-value monitor 'values))))

(define-event-handler (record-phi-s-ranks debug-swap-event)
  (record-value monitor phi-s-rank)
  (setf (slot-value monitor 'values)
        (cons phi-s-rank (slot-value monitor 'values))))

(define-event-handler (record-phi-c-ranks debug-swap-event)
  (record-value monitor phi-c-rank)
  (setf (slot-value monitor 'values)
        (cons phi-c-rank (slot-value monitor 'values))))

(define-event-handler (record-number-of-candidate-phi-s candidate-phi-s-generated)
  (record-value monitor (length phi-s-candidates))
  (setf (slot-value monitor 'values)
        (cons (length phi-s-candidates)
              (slot-value monitor 'values))))

(define-event-handler (record-number-of-candidate-phi-c candidate-phi-c-generated)
  (record-value monitor (length phi-c-candidates))
  (setf (slot-value monitor 'values)
        (cons (length phi-c-candidates)
              (slot-value monitor 'values))))


