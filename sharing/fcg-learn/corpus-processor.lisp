(in-package :fcg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                          ;;
;; Corpus processor for development puposes ;;
;;                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass corpus-processor ()
  ((counter :accessor counter :initform 0 :type number)
   (current-speech-act :accessor current-speech-act :initform nil)
   (corpus :accessor corpus :initarg :corpus)
   (source-file :accessor source-file :initarg :source-file :type pathname :initform nil))
  (:documentation "Class for corpus processor"))

(defmethod load-corpus ((path pathname) &key (sort-p nil))
  "Loads the json corpus at path, creates speech acts and adds them to corpus processor. Returns corpus processor."
  (let ((speech-acts (with-open-file (stream path)
                       (loop for line = (read-line stream nil)
                             for data = (when line (cl-json:decode-json-from-string line))
                             while data
                             collect (make-instance 'speech-act
                                                   :form (cdr (assoc :utterance data))
                                                   :meaning (pn:instantiate-predicate-network
                                                             (read-from-string (cdr (assoc :meaning data)))))))))
    ;; optionally sort speech acts by utterance length
    (when sort-p
      (sort speech-acts #'< :key #'(lambda (speech-act) (count #\space (form speech-act)))))
    ;; Return corpus-processor
    (make-instance 'corpus-processor
                   :corpus (make-array (length speech-acts) :initial-contents speech-acts)
                   :source-file path)))

(defun shuffle-first-nth-speech-acts (cp n)
  "Selects the first nth speech acts of the cp, and shuffles them."
  (setf (corpus cp) (make-array n :initial-contents (shuffle (subseq (corpus cp) 0 n))))
  (reset-cp cp))
                   
(defmethod next-speech-act ((cp corpus-processor))
  "Return the next speech act for given corpus processor and increase counter."
  (let ((speech-act (aref (corpus cp) (counter cp))))
    (setf (current-speech-act cp) speech-act)
    (incf (counter cp))
    (values speech-act (counter cp))))

(defmethod nth-speech-act ((cp corpus-processor) (n number))
  "Get nth speech act from corpus processor."
  (aref (corpus cp) (- n 1)))

(defmethod reset-cp ((cp corpus-processor))
  "Reset corpus processor counter and current-speech act field."
  (setf (counter cp) 0)
  (setf (current-speech-act cp) nil)
  cp)