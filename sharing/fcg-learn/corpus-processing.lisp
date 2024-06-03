(in-package :fcg)

(defclass corpus-processor ()
  ((counter :accessor counter :initform 0 :type number)
   (current-speech-act :accessor current-speech-act :initform nil)
   (corpus :accessor corpus :initarg :corpus))
  (:documentation "Class for corpus processor"))

(defmethod load-corpus ((path pathname) &key (sort-p nil))
  "Loads the json corpus, creates speech acts and adds them to corpus processor. Returns corpus processor."
  (let ((speech-acts (with-open-file (stream path)
                       (loop for line = (read-line stream nil)
                             for data = (when line (cl-json:decode-json-from-string line))
                            
                             while data
                             collect (make-instance 'speech-act
                                                   :form (cdr (assoc :utterance data))
                                                   :meaning 
                                                   (pn::instantiate-predicate-network (read-from-string (cdr (assoc :meaning data)))))))))

    (when sort-p
      (sort speech-acts #'< :key #'(lambda (speech-act) (count #\space (form speech-act)))))

    (make-instance 'corpus-processor
                   :corpus (make-array (length speech-acts) :initial-contents speech-acts))))


(defun shuffle-first-nth-speech-acts (cp n)

  (reset-cp cp)
  (setf (corpus cp) (make-array n :initial-contents (shuffle (subseq (corpus cp) 0 n))))
  cp
  )
                   

(defmethod next-speech-act ((cp corpus-processor))
  "Get next speech act for given corpus processor and increase counter."
  (setf (current-speech-act cp)
        (aref (corpus cp) (counter cp)))

  (incf (counter cp))

  (values (current-speech-act cp) (counter cp)))

(defmethod nth-speech-act ((cp corpus-processor) (n number))
  "Get nth speech act from corpus processor."
  (aref (corpus cp) (- n 1)))

 
(defmethod reset-cp ((cp corpus-processor))
  "Reset corpus processor counter and current-speech act field."
  (setf (counter cp) 0)
  (setf (current-speech-act cp) nil))