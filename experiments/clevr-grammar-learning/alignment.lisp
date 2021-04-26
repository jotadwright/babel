;;;; alignment.lisp

(in-package :clevr-grammar-learning)

;;;; store past programs (composer strategy)
;;;; ==> store all past programs and use these when composing a new program
;;;;     (the new program must be different from all previous programs).

(defun add-past-program (agent irl-program)
  ;; Add the current irl-program to the list of
  ;; failed programs for the current utterance
  (when irl-program
    (let ((hash-key (sxhash (utterance agent))))
      (if (gethash hash-key (memory agent))
        (push irl-program (gethash hash-key (memory agent)))
        (setf (gethash hash-key (memory agent)) (list irl-program))))))

;;;; store past scenes (composer strategy)
;;;; ==> store all past scenes with the applied utterance and correct answer
;;;;     and use these when composing a new program (the new program must
;;;;     lead to the correct answer in all of these scenes).

(defun add-past-scene (agent)
  ;; Add the current scene and answer to the list
  ;; of past scenes for the current utterance
  (let* ((clevr-scene (find-data (ontology agent) 'clevr-context))
         (sample (cons (index clevr-scene) (topic agent)))
         (hash-key (sxhash (utterance agent))))
    (if (gethash hash-key (memory agent))
      (push sample (gethash hash-key (memory agent)))
      (setf (gethash hash-key (memory agent)) (list sample)))))


(define-event alignment-started)
(define-event cxns-rewarded (cxns list))
(define-event cxns-punished (cxns list))

;;;; minimal (alignment strategy)
;;;; ==> Only keep a single cxn for each utterance, so delete the previous one.

(defmethod run-alignment ((agent clevr-learning-learner)
                          process-input (strategy (eql :minimal)))
  (let ((success (find-data process-input 'success)))
    (notify alignment-started)
    ;; when unsuccessful ...
    (unless success
      ;; remove the cxn for this utterance
      (when (find-data process-input 'applied-cxns)
        (loop for cxn in (find-data process-input 'applied-cxns)
              do (delete-cxn-and-th-node cxn agent))))))

;;;; lateral-inhibition (alignment strategy)
;;;; ==> keep all cxns for all utterances, but reward them using lateral
;;;;     inhibition.

(defmethod run-alignment ((agent clevr-learning-learner)
                          process-input (strategy (eql :lateral-inhibition)))
  (let ((success (find-data process-input 'success))
        (utterance (utterance agent)))
    (notify alignment-started)
    ;; reward the applied cxns and punish competitors
    ;; or punish the applied cxns in case of failure
    (if success
      (progn
        (loop with delta = (get-configuration agent :cxn-incf-score)
              with applied-cxns = (find-data process-input 'applied-cxns)
              for cxn in applied-cxns
              do (inc-cxn-score cxn :delta delta)
              finally (notify cxns-rewarded applied-cxns))
        (loop with delta = (get-configuration agent :cxn-decf-score)
              with applied-cxns = (find-data process-input 'applied-cxns)
              for competitor in (get-meaning-competitors agent applied-cxns utterance)
              do (dec-cxn-score agent competitor :delta delta)
              collect competitor into punished-cxns
              finally (notify cxns-punished punished-cxns)))
      (when (find-data process-input 'applied-cxns)
        (loop with applied-cxns = (find-data process-input 'applied-cxns)
              with delta = (get-configuration agent :cxn-decf-score)
              for cxn in applied-cxns
              do (dec-cxn-score agent cxn :delta delta)
              finally (notify cxns-punished applied-cxns))))))

;;;; minimal-holophrases+lateral-inhibition (alignment strategy)
;;;; ==> we only want to keep a single cxn for each holophrase since
;;;;     we only want to be generalising over the last/best holophrase
;;;;     cxns. Therefore, we use the minimal strategy when concerning
;;;;     holophrases and use the lateral inhibition strategy when
;;;;     concerning the other cxns (e.g. there can be different versions
;;;;     of the same lexical cxns and we need to learn which one is correct)

(defmethod run-alignment ((agent clevr-learning-learner)
                          process-input
                          (strategy (eql :minimal-holophrases+lateral-inhibition)))
  (let ((success (find-data process-input 'success))
        (applied-cxns (find-data process-input 'applied-cxns))
        (utterance (utterance agent)))
    (notify alignment-started)
    (if success
      (progn
        (loop with delta = (get-configuration agent :cxn-incf-score)
              for cxn in applied-cxns
              do (inc-cxn-score cxn :delta delta)
              finally (notify cxns-rewarded applied-cxns))
        (loop with delta = (get-configuration agent :cxn-decf-score)
              for competitor in (get-meaning-competitors agent applied-cxns utterance)
              do (dec-cxn-score agent competitor :delta delta)
              collect competitor into punished-cxns
              finally (notify cxns-punished punished-cxns)))
      (if (and (length= applied-cxns 1)
               (eql (get-cxn-type (first applied-cxns)) 'holophrase))
        (delete-cxn (first applied-cxns) (grammar agent))
        (loop with delta = (get-configuration agent :cxn-decf-score)
              for cxn in applied-cxns
              do (dec-cxn-score agent cxn :delta delta)
              finally (notify cxns-punished applied-cxns))))))
                            
