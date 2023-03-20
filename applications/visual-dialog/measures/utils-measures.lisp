(in-package :visual-dialog)

;; Utils
(defun all-questions-solved? ()
  "Check whether all questions are solved. "
  (equal (total-questions-introduced-by-grammar)
         (+ (total-questions-solved-by-grammar) (total-questions-solved-by-perception))))

(defun interpretation-detailed ()
  "Gives detailed overview by which knowledge source a question was solved"
  (loop for knowledge-source in (list :initial :perception :discourse :inference)
        for knowledge-source-measures = (which-knowledge-source knowledge-source
                                                                (slot-value (monitors::get-monitor 'questions-solved-by-mental-simulation) 'values))
        collect (list knowledge-source
                      knowledge-source-measures)))

(defun which-knowledge-source (knowledge-source measures)
  "Check from which knowledge source the binding is"
  (loop for measure in measures
        for vars = (last-elt measure)
         collect  (loop for var in vars
                        if (not (eq (first var) :no-new-bindings))
                          sum (length (find-all knowledge-source var :key #'first)))))


(defun detailed-measures-lists (measures)
  (let ((vars (loop for measure in measures
                    collect (second measure))))
  (loop for knowledge-source in (list :initial :perception :discourse :inference)
       collect (list knowledge-source
                     (loop for var in vars
                           collect (loop for v in var
                                         if (equal (first v) :no-new-bindings)
                                           collect 0
                                         else
                                           collect (count knowledge-source v :key #'first)))))))




(defun total-questions-solved-by-mental-simulation ()
  "Calculate total number of questions that are solved by mental simulation"
  (loop for line in (slot-value (monitors::get-monitor 'questions-solved-by-mental-simulation) 'values)
        sum (loop for num in (first line)
                      sum num)))

(defun variables-solved-by-mental-simulation ()
  "Returns variables that are solved by mental simulation"
  (loop for line in (slot-value (monitors::get-monitor 'questions-solved-by-mental-simulation) 'values)
        for values = (second line)
        append (loop for value in values
                     append (loop for binding in value
                                    when (listp binding)
                                    collect (var (last-elt binding))))))

(defun replace-vars (vars resulting-bindings)
  "replace variable from source with new variable in resulting-bindings, only if source var in resulting binding can be found in source vars"
  (loop for var in vars
        for resulting-binding-var = (assoc var resulting-bindings)
        if (and resulting-binding-var
                (or (not (equal (first resulting-binding-var) '?scene))
                    (not (equal (first resulting-binding-var) '?memory))))
          collect (cdr resulting-binding-var)
            else
          collect var))

(defun collect-resulting-bindings (vars resulting-bindings)
  (loop for var in vars
        for resulting-binding-var = (assoc var resulting-bindings)
        if (and resulting-binding-var
                (or (not (equal (first resulting-binding-var) '?scene))
                    (not (equal (first resulting-binding-var) '?memory))))
          collect resulting-binding-var
        else
          collect nil))
          
(defun collect-irl-vars (meaning)
  (loop for primitive in meaning
        if (not (equal (first primitive) 'bind))
         append (rest primitive)
        else
          collect (third primitive)))

(defun multiset-difference (set1 set2)
  (loop for el in set1
        do (setf set2 (remove el set2 :count 1)))
  set2)

(defun collect-vars (meaning)
  (loop for prim in meaning
        when (not (equal (first prim) 'bind))
        append (rest prim)))


(defun remove-unique-vars (lst)
  "remove vars that are unique"
  (remove-if
   #'(lambda (x)
       (and (= 1 (count x lst))))
   lst))

(defun total-questions-introduced-by-grammar ()
  (loop for line in (slot-value (monitors::get-monitor 'questions-introduced-by-grammar) 'values)
        sum (loop for num in (first line)
                      sum num)))

(defun variables-introduced-by-grammar ()
  (flatten
   (loop for line in (slot-value (monitors::get-monitor 'questions-introduced-by-grammar) 'values)
         append (loop for vars in (second line)
                      collect vars))))

(defun total-questions-solved-by-grammar ()
  (loop for line in (slot-value (monitors::get-monitor 'questions-solved-by-grammar) 'values)
        sum (loop for num in (first line)
                      sum num)))

(defun variables-solved-by-grammar ()
  (loop for line in (slot-value (monitors::get-monitor 'questions-solved-by-grammar) 'values)
        append (loop for vars in (second line)
                      append vars)))

(defun instantiate-meaning-network (meaning)
  (let* ((binds (find-all 'bind meaning :key #'first))
         (meaning-without-binds (set-difference meaning binds :test #'unify-irl-programs)))
    (loop for prim in meaning-without-binds
          for potential-bind-var = (last-elt prim)
          for potential-bind-statement = (find potential-bind-var meaning :key #'third)
          when potential-bind-statement
            collect (subst (last-elt potential-bind-statement) potential-bind-var prim))))




;; Visualisation utils

(defun visualise-bindings (answer-vars)
  "for every binding in answer-vars, visualise this binding"
  (loop for answer-var in answer-vars
        do (loop for var in answer-var
                 for binding = (when (not (equal var :no-new-bindings))
                                 (last-elt var))
                 when binding
                   do (visualise-binding binding))))

(defun visualise-binding (binding)
  "visualise a binding by finding the nodes and edges to add"
  (sleep *time*)
  (identify-network-updates binding *visual-dialog-inn*) 
  (multiple-value-bind (to-add to-update)
      (handle-node-updates *visual-dialog-inn*)
    (vis-add-node (wi::vis-format-many to-add))
    (vis-update-node (wi::vis-format-many to-update)))
  (vis-add-edge (wi::vis-format-many (collect-new-edges *visual-dialog-inn*))))

(defun visualise-link (binding)
  "Visualise a resulting binding link"
  (when binding
    (loop for b in binding
          if b
            do (progn
                 (identify-network-updates b *visual-dialog-inn*)
                 (multiple-value-bind (to-add to-update)
                     (handle-node-updates *visual-dialog-inn*)
                   (vis-add-node (wi::vis-format-many to-add))
                   (vis-update-node (wi::vis-format-many to-update)))
                 (vis-add-edge (wi::vis-format-many (collect-new-edges *visual-dialog-inn*)))))))

(defun visualise-meaning (meaning)
  "Visualises meaning network by identifying updates (check with previous meaning network) and adding nodes and edges."
  (sleep *time*)
  (identify-network-updates (make-semantic-network :primitives meaning) *visual-dialog-inn*) 
  (multiple-value-bind (to-add to-update)
      (handle-node-updates *visual-dialog-inn*)
    (vis-add-node (wi::vis-format-many to-add))
    (vis-update-node (wi::vis-format-many to-update)))
  (vis-add-edge (wi::vis-format-many (collect-new-edges *visual-dialog-inn*))))


(defun preprocess-meaning-networks (meanings resulting-bindings)
  "Preprocesses meaning network, so that primitives that are multiple times in network (filter-by-attribute or unique) get id, which is necessary for visualisations."
  (let ((table nil)
        (filter-counter 1))
    (loop for meaning in meanings
          collect (loop for predicate in meaning
                        if (not (equal (first predicate) 'bind))
                          collect (if (assoc predicate table :test #'equal)
                                    (substitute-predicate-name predicate (rest (assoc predicate table :test #'equal)))
                                    (if (find-renaming predicate resulting-bindings table)
                                      (substitute-predicate-name predicate (first (find-renaming predicate resulting-bindings table)))
                                      (progn (push (cons predicate *predicate-counter*) table)
                                        (incf *predicate-counter*)
                                        (substitute-predicate-name predicate (rest (assoc predicate table :test #'equal))))))
                        else
                          collect predicate))))


(defun find-renaming (predicate resulting-bindings table)
  "Check whether a predicate is already in the table with the previous bindings (by substituting bindings with resulting-bindings)"
  (loop for rbs in resulting-bindings
        when rbs
          append (loop for rb in rbs
                       for renamed-predicate = (substitute (car rb) (cdr rb) predicate)
                       when (assoc renamed-predicate table :test #'equal)
                         collect (rest (assoc renamed-predicate table :test #'equal)))))
  
(defun substitute-predicate-name (predicate id)
  "Give ID to predicate name of predicate."
  (substitute (intern (format nil "~a-~a" (first predicate) id))
              (first predicate)
              predicate))


;; writing to files for plotting

(defun write-measures-to-file (fcg-nodenames irl-nodenames measures)
  (let* ((directory (ensure-directories-exist (babel-pathname :directory `("applications" "visual-dialog" "measures" "data"))))
         (fcg-names (reverse  (slot-value (monitors::get-monitor fcg-nodenames) 'values)))
         (fcg-path (merge-pathnames (make-pathname :name "fcg-nodenames" :type "csv") directory))
         (irl-names (reverse  (slot-value (monitors::get-monitor irl-nodenames) 'values)))
         (irl-path (merge-pathnames (make-pathname :name "irl-nodenames" :type "csv") directory))
         (detailed-list (detailed-measures-lists (slot-value (monitors::get-monitor 'questions-solved-by-mental-simulation) 'values))))
    (with-open-file (str fcg-path
                         :if-does-not-exist :create
                         :if-exists :overwrite
                         :direction :output)
      (loop for name in fcg-names
            if (first name) ;; do not write empty list
              do (format str "~{~a~^ ~}~%" name)))
    (with-open-file (str irl-path
                         :if-does-not-exist :create
                         :if-exists :overwrite
                         :direction :output)
      (loop for name in irl-names
              if (first name) ;; do not write empty list
              do (format str "~{~a~^ ~}~%" name)))
    (loop for measure in measures
          for data = (reverse (slot-value (monitors::get-monitor measure) 'values))
          for pathname = (merge-pathnames (make-pathname :name (format nil "~a" measure) :type "csv") directory)
          do (progn
               (with-open-file (str pathname
                                  :if-does-not-exist :create
                                  :if-exists :overwrite
                                  :direction :output)
                   (loop for d in data
                         do (format str "~{~a~^ ~}~%" (first d))))))
    (loop for measure in detailed-list
          for pathname = (merge-pathnames (make-pathname :name (format nil "~a" (first measure)) :type "csv") directory)
          do (with-open-file (str pathname
                                  :if-does-not-exist :create
                                  :if-exists :overwrite
                                  :direction :output)
                   (loop for d in (reverse (second measure))
                         do (format str "~{~a~^ ~}~%" d))))))
