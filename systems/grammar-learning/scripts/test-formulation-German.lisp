(ql:quickload :grammar-learning)
(in-package :grammar-learning)

(defparameter *learned-cxn-inventory* (cl-store::restore (babel-pathname :directory '("experiments" "grammar-learning" "German-cases") :name "german-grammar-train-mod-1200" :type "store")))


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
                                 (make-pathname :name "German-cases-mod-1000" :type "json")     ;137 sentences
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

(comprehend '("Der Sohn zeigt dem Vater den neuen Brillen" () :cxn-inventory gl::*learned-cxn-inventory*)
(formulate '((geben-01 ?g) (queen ?q) (man ?m)  (mod ?a ?g) (arg0 ?g ?q) (arg1 ?g ?a) (arg2 ?g ?m) (topicalized ?q)) :cxn-inventory gl::*learned-cxn-inventory*)
