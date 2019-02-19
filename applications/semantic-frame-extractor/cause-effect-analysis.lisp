(in-package :frame-extractor)


(defparameter *extracted-frames*
  (with-open-file (inputstream
                   (babel-pathname :directory '(:up "Corpora" "Guardian")
                                   :name "guardian-causation-frames-all"
                                   :type "json")
                   :direction :input :if-does-not-exist :error)
                 
    (loop for line = (read-line inputstream nil)
          until (eq line nil)
          collect (decode-json-from-string line))))


;((:SENTENCE-ID . 339) (:SENTENCE . "Salter said 300 ships would increase cloud reflectivity enough to cancel out the temperature rise caused by man-made climate change so far, but 1,800 would be needed to offset a doubling of CO2, something expected within a few") (:FRAME-ELEMENTS ((:FRAME-EVOKING-ELEMENT . "cause") (:CAUSE . "man - made climate change") (:EFFECT . "the temperature rise"))) (:ARTICLE-ID (:$OID . "59cccf135f730404dc913dce")) (:TIMEOUT))

(defun build-hash-tables-for-causes-and-effects (frames)
  (let ((causes (make-hash-table :test #'equalp))
        (effects (make-hash-table :test #'equalp)))
                
    (loop for frame in frames
          for cause = (cdr (assoc :CAUSE (second (assoc :FRAME-ELEMENTS frame))))
          for effect = (cdr (assoc :EFFECT (second (assoc :FRAME-ELEMENTS frame))))
          if (gethash cause causes)
          do (setf (gethash cause causes) (cons effect (gethash cause causes)))
          else do (setf (gethash cause causes) (list effect))
          if (gethash effect effects)
          do (setf (gethash effect effects) (cons cause (gethash effect effects)))
          else do (setf (gethash effect effects) (list cause)))

    (values causes effects)))

(defun list-to-set-with-frequencies (list)
  (loop with set = nil
        for item in list
        if (assoc item set :test #'equalp)
        do (incf (cdr (assoc item set :test #'equalp)))
        else do (unless (member item '("it" "that" "which" "they") :test #'equalp)
                  (push (cons item 1) set))
        finally (return (sort set #'> :key #'cdr))))

;(list-to-set-with-frequencies *global-warming-causes*)

(multiple-value-bind (causes effects)
    (build-hash-tables-for-causes-and-effects *extracted-frames*)
  (defparameter *causes* causes) ;;hash table with causes as key and effect as values
  (defparameter *effects* effects))

(with-open-file (outputstream
                   (babel-pathname :directory '("applications" "semantic-frame-extractor" "data")
                                   :name "guardian-causes"
                                   :type "json")
                   :direction :output)
  (format outputstream "~a"
          (encode-json-to-string (loop for k being the hash-keys in *causes*
                                             using (hash-value v)
                                             collect `((:cause . ,k)
                                                       (:effects . ,v))))))


(with-open-file (outputstream
                   (babel-pathname :directory '("applications" "semantic-frame-extractor" "data")
                                   :name "guardian-effect"
                                   :type "json")
                   :direction :output)
  (format outputstream "~a"
          (encode-json-to-string (loop for k being the hash-keys in *effects*
                                             using (hash-value v)
                                             collect `((:effect . ,k)
                                                       (:causes . ,v))))))


(defun query-cause (given-effect &key (hash-table *effects*))
  "Query the cause for a given effect"
  (list-to-set-with-frequencies (gethash given-effect hash-table)))

(defun query-effect (given-cause &key (hash-table *causes*))
  "Query the effect for a given cause"
  (list-to-set-with-frequencies (gethash given-cause hash-table)))

(setf *global-warming-causes* (query-cause "global warming"))
(setf *climate-change-effects* (query-effect "climate change"))
(query-cause "rising sea levels")
(query-cause "extreme weather")

(defstruct chain-state
   (chain)
  (possible-continuations)
  (continuation-function))

(defun build-causation-chain (search-term &key (continuation-function #'query-effect) (length 10))
  "Build a causation chain for a given search term."
  
  (let ((queue (list (make-chain-state
                      :chain (list search-term)
                      :continuation-function continuation-function
                      :possible-continuations (funcall continuation-function search-term)))))
    
    (loop while queue
          for state = (pop queue)
          if (= (length (chain-state-chain state)) length)
          return (chain-state-chain state)
          else do (loop for (new-term . freq) in (sort (chain-state-possible-continuations state) #'< :key #'cdr)
                          for new-state = (make-chain-state
                                           :chain (append (chain-state-chain state) (list new-term))
                                           :possible-continuations (funcall continuation-function new-term))
                          do (push new-state queue))
          finally (return (chain-state-chain state)))))
  
;; (build-causation-chain "rising sea levels")
;; 
<<<<<<< Updated upstream
<<<<<<< Updated upstream
;; (build-causation-chain "extreme weather")
=======
;; (build-causation-chain "humans")
>>>>>>> Stashed changes
=======
;; (build-causation-chain "humans")
>>>>>>> Stashed changes

