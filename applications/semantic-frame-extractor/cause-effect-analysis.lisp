(in-package :frame-extractor)

;
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

(defun list-to-set-wit-frequencies (list)
  (loop with set = nil
        for item in list
        if (assoc item set :test #'equalp)
        do (incf (cdr (assoc item set :test #'equalp)))
        else do (push (cons item 1) set)
        finally (return (sort set #'> :key #'cdr))))

;(list-to-set-wit-frequencies *global-warming-causes*)

(multiple-value-bind (causes effects)
    (build-hash-tables-for-causes-and-effects *extracted-frames*)
  (defparameter *causes* causes) ;;hash table with causes as key and effect as values
  (defparameter *effects* effects))

(defun query-cause (given-effect &key (hash-table *effects*))
  "Query the cause for a given effect"
  (list-to-set-wit-frequencies (gethash given-effect hash-table)))

(defun query-effect (given-cause &key (hash-table *causes*))
  "Query the effect for a given cause"
  (list-to-set-wit-frequencies (gethash given-cause hash-table)))

(setf *global-warming-causes* (query-cause "global warming"))
(setf *climate-change-effects* (query-effect "climate change"))
(query-cause "rising sea levels" *effects*)
(query-cause "extreme weather" *effects*)

;;(defun build-causation-chain (given-cause &key (length 3) )
;;  ""
  
;;  (query-effect given-cause)
  