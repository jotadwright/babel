;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                 ;;
;; Learning grammars from propbank-annotated data. ;;
;;                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,;;

;; Loading the :propbank-english sytem
;;(ql:quickload :propbank-english)
(in-package :propbank-english)

;; Loading the Propbank frames (takes a few seconds)
;(load-pb-data :store-data t :ignore-stored-data nil)
;(length *pb-data*)

;; Loading the Propbank annotations (takes a minute)
(load-propbank-annotations :store-data t :ignore-stored-data nil)
;(length (train-split *propbank-annotations*))


(defun all-rolesets-for-framenet-frame (framenet-frame-name)
  (loop for predicate in *pb-data*
        for rolesets = (rolesets predicate)
        for rolesets-for-framenet-frame = (loop for roleset in rolesets
                                                    when (find framenet-frame-name (aliases roleset) :key #'framenet :test #'member)
                                                    collect (id roleset))
        when rolesets-for-framenet-frame
        collect it))

;; (all-rolesets-for-framenet-frame 'opinion)


(defun all-sentences-annotated-with-roleset (roleset)
  (loop for sentence in (train-split *propbank-annotations*)
        when (find roleset (propbank-frames sentence) :key #'frame-name :test #'string=)
        collect sentence))

;; (length (all-sentences-annotated-with-roleset "believe.01"))


(defun find-unit-by-span (transient-structure span)
  "Return a unit with span span"
  (loop for unit in transient-structure
        for unit-span = (cadr (find 'span (unit-body unit) :key #'first))
        when (equal unit-span span)
        return unit))

(defun make-propbank-contributing-unit (units-with-role frame)
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
      (meaning ,meaning))))

(defun make-propbank-unit-with-role (unit-with-role)
  (let* ((unit (cdr unit-with-role))
         (unit-name (variablify (unit-name unit)))
         (parent (variablify (cadr (find 'parent (unit-body unit) :key #'feature-name))))
         (phrase-type-or-lex-class (if (find '(node-type leaf) (unit-body unit) :test #'equal)
                                     `(lex-class ,(cadr (find 'lex-class (unit-body unit) :key #'feature-name)))
                                     `(phrase-type ,(cadr (find 'phrase-type (unit-body unit) :key #'feature-name))))))
    (if  (string= (role-type (car unit-with-role)) "V")
      `(,unit-name
        --
        (lemma ,(cadr (find 'lemma (unit-body unit) :key #'feature-name)))
        (parent ,parent)
        ,phrase-type-or-lex-class)
      `(,unit-name
        --
        (parent ,parent)
        ,phrase-type-or-lex-class))))

(defun make-constituent-and-parent-states (state queue unit-structure)
  "Expands the queue with states for parent and constituents"
  (let ((new-states nil))
    ;;First expand constituents of the current state
    (loop for constituent-name in (cdr (assoc :constituents state)) 
          for constituent-unit = (find constituent-name unit-structure :key #'unit-name)
          for new-state = `((:unit-name . ,constituent-name)
                            (:parent . ,(cadr (find 'parent (unit-body constituent-unit) :key #'feature-name)))
                            (:constituents . ,(cadr (find 'constituents (unit-body constituent-unit) :key #'feature-name)))
                            (:path . ,(append (cdr (assoc :path state)) (list (cons constituent-name :constituent)))))
          unless  (find constituent-name (cdr (assoc :path state)) :key #'car)
          do (push new-state new-states))

    ;;Expand parent of the current state
    (when (assoc :parent state)
      (let* ((parent-name (cdr (assoc :parent state)))
             (parent-unit (find parent-name unit-structure :key #'unit-name))
             (new-state `((:unit-name . ,parent-name)
                          (:parent . ,(cadr (find 'parent (unit-body parent-unit) :key #'feature-name)))
                          (:constituents . ,(cadr (find 'constituents (unit-body parent-unit) :key #'feature-name)))
                          (:path . ,(append (cdr (assoc :path state)) (list (cons parent-name :parent)))))))
        (unless (find parent-name (cdr (assoc :path state)) :key #'car)
          (push new-state new-states))))
    new-states))



(defun find-path-in-syntactic-tree (unit v-unit transient-structure)
  ""
  (let ((unit-structure (left-pole-structure transient-structure))
        (queue (list `((:unit-name . ,(unit-name unit))
                       (:parent . ,(cadr (find 'parent (unit-body unit) :key #'feature-name)))
                       (:constituents .  ,(cadr (find 'constituents (unit-body unit) :key #'feature-name)))
                       (:path . ((,(unit-name unit) . :initial)))))))

    (loop while queue
          for state = (pop queue)
          if (equal (cdr (assoc :unit-name state)) (unit-name v-unit)) ;;solution found
          return (mapcar #'car (cdr (assoc :path state)))
          else do (setf queue (append queue (make-constituent-and-parent-states state queue unit-structure))))))
               
       
(defun learn-cxn-from-propbank-annotation (propbank-sentence roleset cxn-inventory)
  (let* ((frame (find roleset (propbank-frames propbank-sentence) :key #'frame-name :test #'string=))
         (initial-transient-structure (de-render (sentence-string propbank-sentence) :de-render-constituents-dependents))
         (units-with-role (loop for role in (frame-roles frame)
                                for role-start = (first (indices role))
                                for role-end = (+ (last-elt (indices role)) 1)
                                for unit = (find-unit-by-span
                                            (left-pole-structure initial-transient-structure)
                                            (list role-start role-end))
                                collect (cons role unit)))
         (cxn-name-list (loop for (role . unit) in units-with-role
                              collect (format nil "~a:~a" (role-type role)
                                              (if (find '(node-type leaf) (unit-body unit) :test #'equal)
                                                (format nil "~a" (cadr (find 'lex-class (unit-body unit) :key #'feature-name)))
                                                (format nil "~{~a~}" (cadr (find 'phrase-type (unit-body unit) :key #'feature-name)))))))
         (contributing-unit (make-propbank-contributing-unit units-with-role frame))
         (cxn-units-with-role (loop for unit in units-with-role collect (make-propbank-unit-with-role unit)))
         (cxn-units-without-role (remove-duplicates (loop for unit-with-role in (remove-if #'(lambda(unit-with-role) (string= (role-type (car unit-with-role)) "V"))
                                                                                           units-with-role)
                                                          for path = (find-path-in-syntactic-tree (cdr unit-with-role)
                                                                                                  (cdr (find-if #'(lambda(unit-with-role) (string= (role-type (car unit-with-role)) "V"))
                                                                                                                units-with-role))
                                                                                                  initial-transient-structure)
                                                          append (loop for unit-name in path
                                                                       for unit = (find unit-name (left-pole-structure initial-transient-structure) :key #'unit-name)
                                                                       unless (find (variablify unit-name) cxn-units-with-role :key #'unit-name)
                                                                       collect `(,(variablify unit-name)
                                                                                 --
                                                                                 (parent ,(variablify (cadr (find 'parent (unit-body unit) :key #'feature-name))))
                                                                                 ,(if (find '(node-type leaf) (unit-body unit) :test #'equal)
                                                                                    `(lex-class ,(cadr (find 'lex-class (unit-body unit) :key #'feature-name)))
                                                                                    `(phrase-type ,(cadr (find 'phrase-type (unit-body unit) :key #'feature-name)))))))
                                                    :key #'unit-name)))
         
    ;; TODO path-encoding-units)
    (eval `(def-fcg-cxn ,(make-id (format nil "~{~a~^+~}-cxn" cxn-name-list))
                        (,contributing-unit
                         <-
                         ,@cxn-units-with-role
                         ,@cxn-units-without-role)
                        :cxn-inventory ,cxn-inventory))))

;;  (learn-cxn-from-propbank-annotation *believe-sentence* "believe.01" *fcg-constructions*)

(setf *believe-sentence* (second (all-sentences-annotated-with-roleset "believe.01")))

(all-sentences-annotated-with-roleset "believe.01")

(comprehend-and-extract-frames (sentence-string *believe-sentence*))

(def-fcg-cxn believe.01-np-aux-sentential-complement-cxn
               ((?believe-unit
                 (args (referent ?o)
                       (:arg0 ?x-unit)
                       (:arg1 ?y-unit))
                 (frame-evoking +)
                 (meaning ((frame believe.01 ?o)
                           (frame-element believer ?o ?x-unit)
                           (frame-element believed ?o ?y-unit))))
                <-
                (?x-unit
                 --
                 (parent ?s-unit)
                 (phrase-type (np))) 
                (?s-unit
                 --
                 (phrase-type (s)))
                 (?vp-unit2
                 --
                 (parent ?s-unit)
                 (phrase-type (vp)))
                (?vp-unit1
                 --
                 (parent ?vp-unit2)
                 (phrase-type (vp)))
                (?believe-unit
                 --
                 (lemma believe)
                 (parent ?vp-unit1))
                (?y-unit
                 --
                 (parent ?vp-unit1)
                 (phrase-type (sbar)))))