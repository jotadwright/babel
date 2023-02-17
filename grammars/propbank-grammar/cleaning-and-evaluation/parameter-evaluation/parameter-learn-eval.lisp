
(in-package :propbank-grammar)

(defun parameters-learn-comprehend-evaluate-grammar (parameters &optional (old-training-configuration *training-configuration-all*) (new-training-configuration *training-configuration-new*) (train-corpus *train-corpus*))
  "Learn and make predictions for a PropBank grammar using specified parameters, training configurations and corpus."  
  (let ((new-training-configuration (copy-list old-training-configuration)))
    (let ((comb-parameters (all-combinations-parameters parameters)))
      (let ((count 0) (total (length comb-parameters)))
        (dolist (combination comb-parameters)
          (incf count)
          (let ((updated-training-config (adjust-training-configuration combination new-training-configuration)))
            (format t "Running configuration ~A/~A: ~A~% " count total updated-training-config)
            (learn-propbank-grammar-roles
             train-corpus
             :cxn-inventory '*test-grammar*
             :fcg-configuration updated-training-config)
            (store-learned-grammar combination)
            (let ((predictions-comprehend (comprehend-propbank-corpus-parameters *test-grammar* combination)))
              (evaluate-predictions-f1 predictions-comprehend))))))))


(defun evaluate-predictions-f1 (predictions &key (core-roles-only t) (selected-rolesets nil) (include-word-sense t) (include-timed-out-sentences t) (excluded-rolesets nil) (include-sentences-with-incomplete-role-constituent-mapping t))
  "Computes precision, recall and F1 score for a given list of predictions."
  (loop for (sentence annotation solution) in predictions
        when (and (or include-timed-out-sentences
                      (not (eql solution 'time-out)))
                  (or include-sentences-with-incomplete-role-constituent-mapping
                      (loop for gold-frame in annotation
                            always (spacy-benepar-compatible-annotation sentence (frame-name gold-frame)
                                                                        :selected-role-types (if core-roles-only
                                                                                               'core-only 'all)))))
        ;;gold standard predictions
        sum (loop for frame in annotation
                  for frame-name = (if include-word-sense
                                     (frame-name frame)
                                     (truncate-frame-name (frame-name frame)))
                  if (and (null (find frame-name excluded-rolesets :test #'equalp))
                          (or (null selected-rolesets)
                              (find frame-name selected-rolesets :test #'equalp)))
                  sum (loop for role in (frame-roles frame)
                            if core-roles-only
                            sum (if (core-role-p role)
                                  (length (indices role)) 0)
                            else sum (length (indices role))))
        into number-of-gold-standard-predictions
        ;;grammar predictions
        when (and (not (eql solution 'time-out))
                  (or include-sentences-with-incomplete-role-constituent-mapping
                      (loop for gold-frame in annotation
                            always (spacy-benepar-compatible-annotation sentence (frame-name gold-frame)
                                                                        :selected-role-types (if core-roles-only
                                                                                               'core-only 'all)))))
        sum (loop for predicted-frame in solution
                  for frame-name = (if include-word-sense
                                     (symbol-name (frame-name predicted-frame))
                                     (truncate-frame-name (symbol-name (frame-name predicted-frame))))
                  when (and frame-name
                            (null (find frame-name excluded-rolesets :test #'equalp))
                            (or (null selected-rolesets)
                                (find frame-name selected-rolesets :test #'equalp))
                            (find (truncate-frame-name frame-name) annotation
                                    :key #'(lambda (frame)
                                             (truncate-frame-name (frame-name frame)))
                                    :test #'equalp))
                  sum (+ (loop for role in (frame-elements predicted-frame)
                               if core-roles-only
                               sum (if (core-role-p role)
                                     (length (indices role)) 0)
                               else sum (length (indices role)))
                         (length (indices (frame-evoking-element predicted-frame))))) ;;FEE
        into number-of-grammar-predictions
        ;;correct predictions
        when (and (not (eql solution 'time-out))
                  (or include-sentences-with-incomplete-role-constituent-mapping
                      (loop for gold-frame in annotation
                            always (spacy-benepar-compatible-annotation sentence (frame-name gold-frame)
                                                                        :selected-role-types (if core-roles-only
                                                                                               'core-only 'all)))))
        sum (loop for predicted-frame in solution
                  for frame-name = (if include-word-sense
                                     (symbol-name (frame-name predicted-frame))
                                     (truncate-frame-name (symbol-name (frame-name predicted-frame))))
                  when (and frame-name
                            (null (find frame-name excluded-rolesets :test #'equalp))
                            (or (null selected-rolesets)
                                (find frame-name selected-rolesets :test #'equalp)))
                  sum (+ (loop for predicted-frame-element in (frame-elements predicted-frame) ;;frame elements
                            for predicted-indices = (indices predicted-frame-element)
                            if core-roles-only
                            sum (if (core-role-p predicted-frame-element)
                                  (loop for index in predicted-indices
                                        when (correctly-predicted-index-p index predicted-frame-element predicted-frame
                                                                           annotation include-word-sense)
                                         sum 1)
                                  0)
                            else sum (loop for index in predicted-indices
                                           when (correctly-predicted-index-p index predicted-frame-element predicted-frame
                                                                             annotation include-word-sense)
                                           sum 1))
                         (if (correctly-predicted-fee-index-p (indices (frame-evoking-element predicted-frame)) ;;FEE
                                                              predicted-frame annotation include-word-sense)
                           (length (indices (frame-evoking-element predicted-frame)))
                           0)))
        into number-of-correct-predictions
        finally (let ((f1-score `,(compute-f1-score number-of-correct-predictions number-of-grammar-predictions number-of-gold-standard-predictions)
                                          
                                           ))
                  (format t "F1 Score: ~A " f1-score)
                  (return f1-score))))

;; Brute force without evaluation of predictions
(defun parameters-learn-comprehend-grammar (parameters &optional (old-training-configuration *training-configuration-all*) (new-training-configuration *training-configuration-new*) (train-corpus *train-corpus*))
  "Learn and make predictions for a PropBank grammar using specified parameters, training configurations and corpus."  
  (let ((new-training-configuration (copy-list old-training-configuration)))
    (let ((comb-parameters (all-combinations-parameters parameters)))
      (let ((count 0) (total (length comb-parameters)))
        (dolist (combination comb-parameters)
          (incf count)
          (let ((updated-training-config (adjust-training-configuration combination new-training-configuration)))
            (format t "Running configuration ~A/~A: ~A~% " count total updated-training-config)
            (learn-propbank-grammar-roles
             train-corpus
             :cxn-inventory '*test-grammar*
             :fcg-configuration updated-training-config)
            (store-learned-grammar combination)
            (comprehend-propbank-corpus-parameters *test-grammar* combination)))))))


(defun comprehend-propbank-corpus-parameters (cxn-inventory combination &optional (list-of-propbank-sentences *dev-corpus*) &key (output-file nil) (timeout 60) (silent t))
  "Make predictions for a PropBank grammar using specified parameters, training configurations and corpus."
  (let ((predictions nil)
        (output-file (or output-file
                         (babel-pathname :directory '("grammars" "propbank-grammar" "cleaning-and-evaluation" "parameter-evaluation" "predictions-parameter")
                                         :name (format nil "~a~a" combination "-predictions")
                                         :type "store"))))
    (loop for sentence in list-of-propbank-sentences
          for sentence-number from 1
          do (format t "~%Sentence ~a: ~a" sentence-number (sentence-string sentence))
          collect (let* ((cipn (second (multiple-value-list (comprehend-and-extract-frames sentence :cxn-inventory cxn-inventory :silent silent :timeout timeout))))
                         (annotation (propbank-frames sentence)))
                    (if (eql cipn 'time-out)
                      (progn (format t " --> timed out .~%")
                        (list sentence annotation 'time-out))
                      (let ((solution (remove-if-not #'frame-with-name (frames (extract-frames (car-resulting-cfs (cipn-car cipn)))))))
                        (format t " --> done .~%")
                        (list sentence annotation solution))))
          into predictions
          finally (progn (cl-store:store predictions output-file)
                        (return predictions)))))

(defun parameters-learn-grammar (parameters &optional (old-training-configuration *training-configuration-all*) (new-training-configuration *training-configuration-new*) (train-corpus *train-corpus*))
  "Learn PropBank grammar using specified parameters, training configurations and corpus."
  (let ((new-training-configuration (copy-list old-training-configuration)))
    (let ((comb-parameters (all-combinations-parameters parameters)))
      (let ((count 0) (total (length comb-parameters)))
        (dolist (combination comb-parameters)
          (incf count)
          (let ((updated-training-config (adjust-training-configuration combination new-training-configuration)))
            (format t "Running configuration ~A/~A: ~A~% " count total updated-training-config)
            (learn-propbank-grammar-roles
             train-corpus
             :cxn-inventory '*test-grammar*
             :fcg-configuration updated-training-config)
            (store-learned-grammar combination)))))))

(defun learn-propbank-grammar-roles (list-of-propbank-sentences &key
                                                          (selected-rolesets nil)
                                                          
                                                          (cxn-inventory '*propbank-learned-cxn-inventory*)
                                                          (fcg-configuration nil))
  "Learns a PropBank grammar based on a corpus of PropBank-annotated sentences."
  (let ((cxn-inventory (eval `(def-fcg-constructions propbank-learned-english
                                :fcg-configurations ,fcg-configuration
                                :visualization-configurations ((:show-constructional-dependencies . nil)
                                                               (:show-categorial-network . nil)
                                                               (:hide-attributes . t)
                                                               (:hide-features . nil))
                                :hierarchy-features (constituents dependents)
                                :feature-types ((constituents sequence)
                                                (dependents sequence)
                                                (span sequence)
                                                (syn-class set)
                                                (args set-of-predicates)
                                                (word-order set-of-predicates)
                                                (meaning set-of-predicates)
                                                (footprints set))
                                :cxn-inventory ,cxn-inventory
                                :hashed t))))
    
    (set-data (blackboard cxn-inventory) :training-corpus-size 0)
(let ((excluded-rolesets (mapcar (lambda (roleset) (format nil "~a" roleset)) (get-configuration cxn-inventory :excluded-rolesets))))
  (loop for sentence in list-of-propbank-sentences
        for sentence-number from 1
        for training-corpus-size = (get-data (blackboard cxn-inventory) :training-corpus-size)
        for rolesets = (cond (selected-rolesets
                              (intersection selected-rolesets (all-rolesets sentence) :test #'equalp))
                             (excluded-rolesets
                               (loop for roleset in (all-rolesets sentence)
                                     unless (member roleset excluded-rolesets :test #'equalp)
                                     collect roleset))
                             (t
                              (all-rolesets sentence)))
        do
        (when (= 0 (mod sentence-number 100))
          (format t "~%---> Sentence ~a." sentence-number))
        (when rolesets
          (set-data (blackboard cxn-inventory) :training-corpus-size (incf training-corpus-size)))
        (loop for roleset in rolesets
              do 
              (loop for mode in (get-configuration cxn-inventory :learning-modes)
                    do
                      (learn-from-propbank-annotation sentence roleset cxn-inventory mode)))
        finally
          (notify learning-finished cxn-inventory)
          (return cxn-inventory)))))


(defun store-learned-grammar (combination &optional (grammar *test-grammar*))
  "Stores the learned grammar using the configuration specified in 'updated-training-config' for the given 'parameter'
   in a file with a name specified in 'config-file-name' and path"
    (cl-store:store grammar (babel-pathname :directory '("grammars" "propbank-grammar" "cleaning-and-evaluation" "parameter-evaluation" "grammars-parameter")
                                :name (format nil "~a~a" combination "-grammar")
                                :type "fcg")))


(defun adjust-training-configuration (updated-parameters &optional (training-configuration *training-configuration-new*))
  "Adjusts the given training configuration with the updated parameters."
  (let ((updated-config (copy-list training-configuration)))
    (dolist (parameter updated-parameters)
      (loop for (key . value) in updated-config
            when (eq key (car parameter))
            do (setf (cdr (assoc key updated-config)) (if (listp (cdr parameter)) (cdr parameter) (list (cdr parameter))))))
    updated-config))

(defun filter-combinations (combinations &key parameters-to-include parameters-to-exclude)
  (remove-if (lambda (combination)
               (let ((flattened-combination (flatten combination)))
                 (or (and parameters-to-include
                          (some (lambda (include)
                                  (not (every (lambda (param) (member param flattened-combination)) include)))
                                parameters-to-include))
                     (and parameters-to-exclude
                          (some (lambda (exclude)
                                  (every (lambda (param) (member param flattened-combination)) exclude))
                                parameters-to-exclude)))))
             combinations))

(defun all-combinations-parameters (parameters &optional (training-configuration *training-configuration-all*))
  "Returns all possible combinations of values for the given 'parameter' from 'training-configuration'"
  (let ((hash-table-combinations (combinations-parameters parameters training-configuration)))
    (let ((list-of-all-combinations-parameters (make-combinations-from-ht hash-table-combinations)))
      list-of-all-combinations-parameters)))

(defun make-combinations-from-ht (ht)
  "Returns a list of all possible combinations of the values associated with the keys in a given hash table."
  (let ((keys (hash-table-keys ht)))
    (let ((values (mapcar #'(lambda (k) (gethash k ht)) keys)))
      (loop for values-combination in (apply #'cartesian-product values)
            collect (loop for i from 0 below (length keys)
                          collect (cons (nth i keys) (nth i values-combination)))))))

(defun combinations-parameters (parameters &optional (training-configuration *training-configuration-all*))
  "Returns all possible combinations of values for the given 'parameter' from 'training-configuration'"
  (let ((collected-parameters (collect-training-parameters parameters training-configuration)))
    (let ((hash-table-combinations-parameters (create-combinations-hash-table collected-parameters)))
      hash-table-combinations-parameters)))

(defun create-combinations-hash-table (data)
  "Creates a hash table with the specified keys and values."
  (let ((ht (make-hash-table)))
    (dolist (item data)
      (let ((key (car item))
            (values (cdr item)))
        (setf (gethash key ht) (get-combinations-parameter values))))
    ht))

(defun hash-table-keys (hash-table)
  (loop for key being the hash-keys in hash-table
        collect key))

(defun get-combinations-parameter (input-list)
  "Returns all possible combinations of a given input list."
  (let ((combinations '()))
    (loop for i from 0 to (length input-list) do
          (loop for combination in (combinations-of-length input-list i) do
                (push combination combinations)))
    (let ((result (make-hash-table :test 'equal)))
      (dolist (combination combinations)
        (if (not (gethash combination result))
            (setf (gethash combination result) t)))
      (let ((result-list '()))
        (maphash (lambda (k v) (push k result-list)) result)
        (sort (sort result-list #'(lambda (a b) (< (length a) (length b)))) #'(lambda (a b) (string< (prin1-to-string a) (prin1-to-string b))))))))

(defun collect-training-parameters (training-parameters &optional (training-configuration *training-configuration-all*))
  "Collects multiple training parameters from a given training configuration."
  (let ((collected-parameters '()))
    (dolist (training-parameter training-parameters)
      (setf collected-parameters (append (collect-training-parameter training-parameter training-configuration) collected-parameters)))
    collected-parameters))

(defun collect-training-parameter (training-parameter &optional (training-configuration *training-configuration-all*))
  "Collects the training parameter from a given training configuration."
  (loop for (key . value) in training-configuration
        when (eq key training-parameter)
        collect (cons training-parameter value)))



