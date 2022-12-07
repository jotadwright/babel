(ql:quickload :grammar-learning)
(in-package :grammar-learning)

(defparameter *learned-cxn-inventory* (cl-store::restore (babel-pathname :directory '("experiments" "grammar-learning" "German-cases") :name "german-grammar-train-500" :type "store")))


(set-configuration *learned-cxn-inventory* :update-categorial-links nil)
(set-configuration *learned-cxn-inventory* :use-meta-layer nil)
(set-configuration *learned-cxn-inventory* :consolidate-repairs nil)
(set-configuration *learned-cxn-inventory* :production-goal-tests '(:no-applicable-cxns
                                                                    :connected-structure
                                                                    :no-meaning-in-root))

(defun load-data (file)
  (with-open-file (stream file)
    (in-package :grammar-learning)
    (loop for line = (read-line stream nil)
          for data = (when line (cl-json:decode-json-from-string line))
          while data
          collect (cons (cdr (assoc :utterance data)) (remove-duplicates (read-from-string (cdr (assoc :meaning data))) :test #'equal)))))




(defparameter *data* (load-data (merge-pathnames
                                 (make-pathname :name "German-cases-500" :type "json")     ;137 sentences
                                 (merge-pathnames
                                  (make-pathname :directory '(:relative "German-cases"))
                                  cl-user:*babel-corpora*))))

(defun evaluate-formulation (data)
  (loop for line in data
        for meaning = (rest line)
        for predicted-form = (formulate meaning :cxn-inventory gl::*learned-cxn-inventory*)
        do (format t "~{~a ~}~%" predicted-form)))

(evaluate-formulation *data*)
  

                        


(activate-monitor trace-fcg)

(formulate '((suchen-01 ?s) (policeman ?w) (clown ?b) (arg0 ?s ?w) (arg1 ?s ?b) (topicalized ?p)) :cxn-inventory gl::*learned-cxn-inventory*)
