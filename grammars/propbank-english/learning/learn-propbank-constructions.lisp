(in-package :propbank-english)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                              ;;
;; Learning constructions based on Propbank annotated corpora.  ;;
;;                                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Learning a grammar cxn ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun learn-propbank-grammar (list-of-propbank-sentences &key (selected-rolesets nil) (silent t)
                                                          (tokenize? nil) (cxn-inventory '*propbank-learned-cxn-inventory*)
                                                          (list-of-syntactic-analyses nil))
  (let ((cxn-inventory (eval `(def-fcg-constructions propbank-learned-english
                                :fcg-configurations ((:de-render-mode .  ,(if tokenize?
                                                                            :de-render-constituents-dependents
                                                                            :de-render-constituents-dependents-without-tokenisation))
                                                     (:node-tests  :restrict-nr-of-nodes)
                                                     (:max-nr-of-nodes . 100)
                                                     (:node-expansion-mode . :multiple-cxns)
                                                     (:priority-mode . :nr-of-applied-cxns)
                                                     (:queue-mode . :greedy-best-first)
                                                     (:hash-mode . :hash-lemma)
                                                     (:cxn-supplier-mode . :hashed-and-scored))
                                :visualization-configurations ((:show-constructional-dependencies . nil))
                                :hierarchy-features (constituents dependents)
                                :feature-types ((constituents set)
                                                (dependents set)
                                                (span sequence)
                                                (phrase-type set)
                                                (word-order set-of-predicates)
                                                (meaning set-of-predicates)
                                                (footprints set))
                                :cxn-inventory ,cxn-inventory
                                :hashed t))))
    (loop for sentence in list-of-propbank-sentences
          for sentence-number from 1
          for sentence-string = (sentence-string sentence)
          for syntactic-analysis = (or (nth1 sentence-number list-of-syntactic-analyses)
                                       (if tokenize?
                                         (nlp-tools:get-penelope-syntactic-analysis sentence-string)
                                         (nlp-tools:get-penelope-syntactic-analysis
                                          (split-sequence:split-sequence #\Space sentence-string
                                                                         :remove-empty-subseqs t))))
          for rolesets = (if selected-rolesets
                           (intersection selected-rolesets (all-rolesets sentence) :test #'equalp)
                           (all-rolesets sentence))
          do
          (format t "~%~%---> Sentence ~a: ~a~%" sentence-number sentence-string)
          (loop for roleset in rolesets
                if (spacy-benepar-compatible-annotation sentence roleset :syntactic-analysis syntactic-analysis)
                do
                (let ((f1-score (cdr (assoc :f1-score (evaluate-propbank-sentences
                                                       (list sentence) cxn-inventory
                                                       :list-of-syntactic-analyses (list syntactic-analysis)
                                                       :selected-rolesets (list roleset)
                                                       :silent silent
                                                       :print-to-standard-output nil)))))
                  ;; if f1-score under 1.0
                  (if (< f1-score 1.0)
                    (progn
                      (format t "~%Roleset ~a: f1-score ~a --> Learning.~%"  roleset f1-score)
                      ;; First try learning with copy of cxn-inventory
                      (let ((temp-cxn-inventory
                             (learn-cxn-from-propbank-annotation sentence roleset (copy-object cxn-inventory)
                                                                 :syntactic-analysis syntactic-analysis)))
                        ;; If now not under .95 anymore, learn with actual cxn-inventory
                        (if temp-cxn-inventory
                          (let ((new-f1-score (cdr (assoc :f1-score (evaluate-propbank-sentences
                                                                     (list sentence) temp-cxn-inventory
                                                                     :list-of-syntactic-analyses (list syntactic-analysis)
                                                                     :selected-rolesets (list roleset)
                                                                     :silent silent
                                                                     :print-to-standard-output nil)))))
                            (if  (< new-f1-score f1-score)
                              (format t "Learning failed, f1-score ~a.~%" new-f1-score)
                              (progn
                                (format t "Learning was successful, f1-score ~a.~%"  new-f1-score)
                                (learn-cxn-from-propbank-annotation sentence roleset cxn-inventory :syntactic-analysis syntactic-analysis))))
                          (format t "Nothing could be learned. ~%"))))
                    (format t "~%Roleset ~a: f1-score ~a.~%"  roleset f1-score)))
                finally return cxn-inventory))))
                   
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Learning a single cxn ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun learn-cxn-from-propbank-annotation (propbank-sentence roleset cxn-inventory &key (syntactic-analysis nil))
  "Adds a new construction to the cxn-inventory based on a propbank
sentence object and a roleset (e.g. 'believe.01')"
  (let ((frames (find-all roleset (propbank-frames propbank-sentence) :key #'frame-name :test #'equalp)))
      (dolist (frame frames cxn-inventory)
        (let* ((unit-structure (left-pole-structure (de-render (sentence-string propbank-sentence)
                                                               (get-configuration cxn-inventory :de-render-mode)
                                                               :syntactic-analysis syntactic-analysis)))
                (units-with-role (loop for role in (frame-roles frame) ;;find all units that correspond to annotated frame elements
                                       for role-start = (first (indices role))
                                       for role-end = (+ (last-elt (indices role)) 1)
                                       for unit = (find-unit-by-span unit-structure (list role-start role-end))
                                       when unit
                                       collect (cons role unit)))
                (cxn-name-list (loop for (role . unit) in units-with-role
                                     collect (format nil "~a:~a" (role-type role) ;;create a name based on role-types and lex-class/phrase-type
                                                     (if (find '(node-type leaf) (unit-body unit) :test #'equal)
                                                       (format nil "~a" (cadr (find 'lex-class (unit-body unit) :key #'feature-name)))
                                                       (format nil "~{~a~}" (cadr (find 'phrase-type (unit-body unit) :key #'feature-name)))))))
                (lemma (loop for (role . unit) in units-with-role
                             when (string= "V" (role-type role))
                             return (feature-value (find 'lemma (unit-body unit) :key #'feature-name))))
                (cxn-name (format nil "~a-~{~a~^+~}-cxn" roleset cxn-name-list))
                (contributing-unit (make-propbank-contributing-unit units-with-role frame cxn-name))
                (cxn-units-with-role (loop for unit in units-with-role collect (make-propbank-conditional-unit-with-role unit cxn-name)))
                (cxn-units-without-role (make-propbank-conditional-units-without-role units-with-role cxn-units-with-role unit-structure)))

           (when (and cxn-units-with-role lemma)
             ;;create a new construction and add it to the cxn-inventory
             (eval `(def-fcg-cxn ,(make-id cxn-name)
                                 (,contributing-unit
                                  <-
                                  ,@cxn-units-with-role
                                  ,@cxn-units-without-role)
                                 :disable-automatic-footprints t
                                 :attributes (:lemma ,lemma :score ,(length cxn-units-with-role))
                                 :cxn-inventory ,cxn-inventory)))))))


(defun find-unit-by-span (transient-structure span)
  "Return a unit with span span"
  (loop for unit in transient-structure
        for unit-span = (cadr (find 'span (unit-body unit) :key #'first))
        when (equal unit-span span)
        return unit))

(defun make-propbank-contributing-unit (units-with-role frame cxn-name)
  "Make a contributing unit based on a frame and units-with-role."
  (let* ((v-unit (cdr (assoc "V" units-with-role :key #'role-type :test #'string=)))
         (unit-name (variablify (unit-name v-unit)))
         (args (loop for r in (frame-roles frame)
                     if (string= (role-type r) "V")
                     collect '(referent ?f)
                     else collect `(,(make-kw (role-type r)) ,(variablify (unit-name (cdr (assoc r units-with-role)))))))
         (meaning (loop for r in (frame-roles frame)
                     if (string= (role-type r) "V")
                     collect `(frame ,(intern (upcase (frame-name frame))) ?f)
                     else collect `(frame-element ,(intern (upcase (role-type r))) ?f
                                                  ,(variablify (unit-name (cdr (assoc r units-with-role))))))))
    `(,unit-name
      (args ,@args)
      (frame-evoking +)
      (meaning ,meaning)
      (footprints (,cxn-name)))))

(defun make-propbank-conditional-unit-with-role (unit-with-role cxn-name)
  "Makes a conditional unit for a propbank cxn based on a unit in the
initial transient structure that plays a role in the frame."
  (let* ((unit (cdr unit-with-role))
         (unit-name (variablify (unit-name unit)))
         (parent (when (cadr (find 'parent (unit-body unit) :key #'feature-name))
                   (variablify (cadr (find 'parent (unit-body unit) :key #'feature-name)))))
         (phrase-type-or-lex-class (if (find '(node-type leaf) (unit-body unit) :test #'equal)
                                     `(lex-class ,(cadr (find 'lex-class (unit-body unit) :key #'feature-name)))
                                     `(phrase-type ,(cadr (find 'phrase-type (unit-body unit) :key #'feature-name))))))
    (if  (string= (role-type (car unit-with-role)) "V")
      ;;a FEE unit also has the feature lemma
      `(,unit-name
        --
        (lemma ,(cadr (find 'lemma (unit-body unit) :key #'feature-name)))
        (parent ,parent)
        (footprints (NOT ,cxn-name))
        ,phrase-type-or-lex-class)
      ;;other units only have a parent feature and a phrase-type/lex-class feature
      `(,unit-name
        --
        (parent ,parent)
        ,phrase-type-or-lex-class))))


(defun make-propbank-conditional-units-without-role (units-with-role cxn-units-with-role unit-structure)
  "Makes conditional units that are needed in a propbank cxn to encode
the paths in the syntactic tree between units that function as slot
fillers (arg0, arg1) and the frame-evoking element unit."
  (remove-duplicates
   (loop with fee-unit = (cdr (find-if #'(lambda(unit-with-role) (string= (role-type (car unit-with-role)) "V"))
                                                               units-with-role))
         for unit-with-role in (remove fee-unit units-with-role :test #'equal) ;;discard the frame-evoking element (FEE) unit
         for path = (find-path-in-syntactic-tree (cdr unit-with-role) fee-unit unit-structure) ;;find path between a unit in the transient structure and the FEE unit

         append (loop for unit-name in path
                      for unit = (find unit-name unit-structure :key #'unit-name)
                      for parent = (when (cadr (find 'parent (unit-body unit) :key #'feature-name))
                                      (variablify (cadr (find 'parent (unit-body unit) :key #'feature-name))))
                      for form-constraints-for-children-with-role-and-same-type = (make-form-constraints-for-children-with-role-and-same-type unit cxn-units-with-role)

                      unless (find (variablify unit-name) cxn-units-with-role :key #'unit-name) ;;check that the unit is not a frame-element
                      collect `(,(variablify unit-name)
                                --
                                (parent ,parent)
                                ,(when form-constraints-for-children-with-role-and-same-type
                                   `(word-order ,form-constraints-for-children-with-role-and-same-type))
                                ,(if (find '(node-type leaf) (unit-body unit) :test #'equal)
                                   `(lex-class ,(cadr (find 'lex-class (unit-body unit) :key #'feature-name)))
                                   `(phrase-type ,(cadr (find 'phrase-type (unit-body unit) :key #'feature-name)))))))
                                                    :key #'unit-name))


(defun make-form-constraints-for-children-with-role-and-same-type (unit cxn-units-with-role)
  (let* ((children (loop for role-unit in cxn-units-with-role
                         when (equalp (subseq (symbol-name (feature-value (find 'parent (cddr role-unit) :key #'first :test #'equalp))) 1)
                                      (symbol-name (unit-name unit)))
                         collect role-unit))
         (children-per-type (mapcar #'rest
                                    (group-by children #'(lambda (unit)
                                                           (if (find '(node-type leaf) (cddr unit) :test #'equal)
                                                             (feature-value (find 'lex-class (cddr unit) :key #'first :test #'equalp))
                                                             (feature-value (find 'phrase-type (cddr unit) :key #'first :test #'equalp))))
                                              :test #'equalp )))
         (form-constraints-in-unit (feature-value (find 'word-order (unit-body unit) :key #'first :test #'equalp))))

    (loop with fcs-to-keep = nil
          for fc in form-constraints-in-unit
          for first-unit-name = (variablify (second fc))
          for second-unit-name = (variablify (third fc))
          do (let ((fc-to-keep (loop for group in children-per-type
                                  if (and (find first-unit-name group :key #'unit-name :test #'string=)
                                          (find second-unit-name group :key #'unit-name :test #'string=))
                                  collect (list (first fc) first-unit-name second-unit-name))))
               (when fc-to-keep 
                 (setf fcs-to-keep (append fcs-to-keep fc-to-keep))))
          
          finally return fcs-to-keep)))


(defun find-path-in-syntactic-tree (unit v-unit unit-structure)
  "A search process that finds a path between two units in a transient
structure. The path is returned as a list of unit names, ordered from
start to end(v-unit)"
  (let ((queue (list `((:unit-name . ,(unit-name unit))
                       (:parent . ,(cadr (find 'parent (unit-body unit) :key #'feature-name)))
                       (:constituents .  ,(cadr (find 'constituents (unit-body unit) :key #'feature-name)))
                       (:path . ((,(unit-name unit) . :initial)))))))

    (loop while queue
          for state = (pop queue)
          if (equal (cdr (assoc :unit-name state)) (unit-name v-unit)) ;;solution found
          return (mapcar #'car (cdr (assoc :path state)))
          else do (setf queue (append queue (make-constituent-and-parent-states state unit-structure))))))

(defun make-constituent-and-parent-states (state unit-structure)
  "Creates new states for parent and constituents of current state"
  (let ((new-states nil))
    ;;First create states based on the constituents of the current state
    (loop for constituent-name in (cdr (assoc :constituents state)) 
          for constituent-unit = (find constituent-name unit-structure :key #'unit-name)
          for new-state = `((:unit-name . ,constituent-name)
                            (:parent . ,(cadr (find 'parent (unit-body constituent-unit) :key #'feature-name)))
                            (:constituents . ,(cadr (find 'constituents (unit-body constituent-unit) :key #'feature-name)))
                            (:path . ,(append (cdr (assoc :path state)) (list (cons constituent-name :constituent)))))
          unless (find constituent-name (cdr (assoc :path state)) :key #'car) ;;avoid circular paths
          do (push new-state new-states))

    ;;Now create a state for the parent of the current state
    (when (assoc :parent state)
      (let* ((parent-name (cdr (assoc :parent state)))
             (parent-unit (find parent-name unit-structure :key #'unit-name))
             (new-state `((:unit-name . ,parent-name)
                          (:parent . ,(cadr (find 'parent (unit-body parent-unit) :key #'feature-name)))
                          (:constituents . ,(cadr (find 'constituents (unit-body parent-unit) :key #'feature-name)))
                          (:path . ,(append (cdr (assoc :path state)) (list (cons parent-name :parent)))))))
        (unless (find parent-name (cdr (assoc :path state)) :key #'car) ;;avoid circular paths
          (push new-state new-states))))
    new-states))

(defmethod all-rolesets ((sentence conll-sentence))
  "Returns all propbank frames with which a sentence was annotated."
  (mapcar #'frame-name (propbank-frames sentence)))

