(in-package :grammar-learning)


  

(defmethod diff-networks (network-1 network-2 first-predicate-fn next-predicate-fn compare-networks-fn add-atoms-fn rem-atoms-fn)
  "traverse both networks, return the longest possible sequence of overlapping predicates, assumes the network to be linear, and the variables to have a consistent position for traversal"
  ;todo: identify all subnetworks, apply this function to them, then loop through parent network and combine resolved result with subnetwork
  (loop with current-predicate-1 = (funcall first-predicate-fn network-1)
        with current-predicate-2 = (funcall first-predicate-fn network-2)
        with last-equivalent-predicate-1 = current-predicate-1
        with overlapping-predicates-1 = nil
        with overlapping-predicates-2 = nil
        with sorted-network-1 = nil
        with sorted-network-2 = nil
        with rest-network-1 = (set-difference network-1 overlapping-predicates-1)
        with rest-network-2 = (set-difference network-2 overlapping-predicates-2)
        while (or current-predicate-1 current-predicate-2)
        for next-predicate-1 = (funcall next-predicate-fn current-predicate-1 rest-network-1)
        for next-predicate-2 = (funcall next-predicate-fn current-predicate-2 rest-network-2)
        do (multiple-value-bind (equivalent-predicates-p
                                 bind-1
                                 bind-2)
               (funcall compare-networks-fn current-predicate-1 current-predicate-2 rest-network-1 rest-network-2)
             (pushnew current-predicate-1 sorted-network-1)
             (pushnew current-predicate-2 sorted-network-2)
             (if equivalent-predicates-p
               (progn ;; true condition
                 (setf last-equivalent-predicate-1 current-predicate-1) ;; keep track of last successful comparison
                 (push current-predicate-1 overlapping-predicates-1)
                 (push current-predicate-2 overlapping-predicates-2)
                 ;(when bind-1 (push bind-1 overlapping-predicates-1))
                 ;(when bind-2 (push bind-2 overlapping-predicates-2))
                 (setf current-predicate-1 next-predicate-1)
                 (setf current-predicate-2 next-predicate-2))
               ;; false condition
               (setf current-predicate-1 next-predicate-1)) ; traverse network 1 while network 2 stays static
             (when (and (not current-predicate-1) current-predicate-2) ;; stack 1 is empty, stack 2 is not so go back to the last equivalent predicate, and take the next predicate
               (setf current-predicate-1 (funcall next-predicate-fn last-equivalent-predicate-1 rest-network-1))
               (setf current-predicate-2 next-predicate-2)))
               
        finally (let* ((non-overlapping-predicates-1 (sort-meaning-predicates
                                                      (set-difference (funcall rem-atoms-fn network-1) overlapping-predicates-1)
                                                      sorted-network-1))
                      (non-overlapping-predicates-2 (sort-meaning-predicates
                                                     (set-difference (funcall rem-atoms-fn network-2) overlapping-predicates-2)
                                                     sorted-network-2))
                      (start-pos-1 (cond ((position (first non-overlapping-predicates-1) sorted-network-1))
                                         (t 0)))
                      (end-pos-1 (position (last-elt non-overlapping-predicates-1) sorted-network-1))
                      (non-overlapping-continuous-predicates-1
                       (if (and non-overlapping-predicates-1
                                end-pos-1)
                         (funcall add-atoms-fn
                                (subseq sorted-network-1 start-pos-1 (+ 1 end-pos-1))
                                network-1)
                         network-1))
                      (start-pos-2 (position (first non-overlapping-predicates-2) sorted-network-2))
                      (end-pos-2 (position (last-elt non-overlapping-predicates-2) sorted-network-2))
                      (non-overlapping-continuous-predicates-2
                       (when non-overlapping-predicates-2
                         (funcall add-atoms-fn
                                (subseq sorted-network-2 start-pos-2 (+ 1 end-pos-2))
                                network-2))))
                      
                       (return (values non-overlapping-continuous-predicates-1
                                       non-overlapping-continuous-predicates-2
                                       ;sorted-network-1
                                       ;sorted-network-2
                                       )))))

(defun sort-meaning-predicates (network-to-sort sorted-source-network)
  (sort network-to-sort #'< :key #'(lambda (x) (cond ((position x sorted-source-network))
                                                     (t 0)))))

#|(defmethod diff-meaning-networks (network-1 network-2 (mode (eql :irl)))
  (diff-networks network-1
                 network-2
                 #'get-predicate-with-target-var
                 #'get-next-irl-predicate
                 #'compare-irl-predicates
                 #'add-bind-statements
                 #'remove-bind-statements))
|#

(defun add-dummy-end-meets (form-constraints)
  (append form-constraints
          (list (list 'fcg::meets (second (get-boundary-units form-constraints)) 'gl::dummy))))

(defmethod diff-form-constraints (fc-1 fc-2)
  (multiple-value-bind (fc-1-diff fc-2-diff)
      (diff-networks (add-dummy-end-meets fc-1)
                 (add-dummy-end-meets fc-2)
                 #'get-first-form-constraint
                 #'get-next-form-constraint
                 #'compare-form-constraints
                 #'add-string-atoms
                 #'remove-string-atoms)
    (values (remove-dangling-meets fc-1-diff) (remove-dangling-meets fc-2-diff))
    ))

(defun remove-dangling-meets (form-constraints)
  (loop with string-predicates = (extract-form-predicate-by-type form-constraints 'string)
        for predicate in (extract-form-predicate-by-type form-constraints 'meets)
        when (and (member (second predicate) string-predicates :key #'second)
                  (member (third predicate) string-predicates :key #'second))
        collect predicate into resulting-meets
        finally (return (append resulting-meets string-predicates)))
  
  )

(defun remove-string-atoms (network)
  (extract-form-predicate-by-type network 'meets))

(defun add-string-atoms (new-network orig-network)
  (loop with result-strings = nil
        for predicate in new-network
        for left-var = (second predicate)
        for left-string-predicate = (find left-var (extract-form-predicate-by-type orig-network 'string) :key #'second)
        do (pushnew left-string-predicate result-strings)
        finally (return (append new-network result-strings))))

(defun remove-bind-statements (network)
  (loop for predicate in network
        unless (equal (first predicate) 'utils::bind)
        collect predicate))

(defun add-bind-statements (new-network orig-network)
  (loop for predicate in new-network
        for open-var = (first (third (multiple-value-list (extract-vars-from-irl-network (list predicate)))))
        for bind = (get-irl-predicate-from-in-var open-var orig-network)
        when bind
        collect bind into binds
        finally (return (append new-network binds))))
       
(defun compare-form-constraints (predicate-1 predicate-2 network-1 network-2)
  (let* ((left-var-1 (second predicate-1))
         (left-string-1 (third (find left-var-1 (extract-form-predicate-by-type network-1 'string) :key #'second)))
         (left-var-2 (second predicate-2))
         (left-string-2 (third (find left-var-2 (extract-form-predicate-by-type network-2 'string) :key #'second)))
         )
    (string= left-string-1 left-string-2)))



(defun compare-irl-predicates (predicate-1 predicate-2 network-1 network-2)
  ; todo: find subnetworks of open vars, then use equivalent-irl-programs? on those! these subnetworks can have a depth larger than 1!
  (let* ((open-vars-1 (third (multiple-value-list (extract-vars-from-irl-network (list predicate-1)))))
         (open-vars-2 (third (multiple-value-list (extract-vars-from-irl-network (list predicate-2)))))
         (predicate-unification-bindings (unify-irl-programs (list predicate-1) (list predicate-2)))
         (sub-predicate-1 (get-irl-predicate-from-in-var (first open-vars-1) network-1))
         (sub-predicate-2 (get-irl-predicate-from-in-var (first open-vars-2) network-2))
         (sub-predicate-unification-bindings (unify-irl-programs (list sub-predicate-1)
                                                                 (list sub-predicate-2))))
    (cond ((and (not (or open-vars-1 open-vars-2))
                predicate-unification-bindings)
           ;; no open vars, and it unifies
           (values t nil nil))
             
          ((and open-vars-1
                open-vars-2
                predicate-unification-bindings
                sub-predicate-unification-bindings)
           ;; both have open vars, and the bound predicates unify
           (values t sub-predicate-1 sub-predicate-2))
          )))

(defun get-predicate-with-target-var (irl-program)
  "Find the predicate with the target var, given an irl program"
  (find (get-target-var irl-program) irl-program :test #'member))

(defun get-next-irl-predicate (predicate irl-program)
  "Find the next predicate, given a variable"
  (let ((in-var (first (multiple-value-list (extract-vars-from-irl-network (list predicate))))))
    (find in-var (remove predicate irl-program) :test #'member)))

(defun get-next-form-constraint (curr-predicate form-constraints)
  (let ((start-var (third curr-predicate)))
    (find start-var (extract-form-predicate-by-type form-constraints 'meets) :key #'second)))

(defun get-first-form-constraint (form-constraints)
  (let ((start-var (first (get-boundary-units form-constraints))))
    (find start-var (extract-form-predicate-by-type form-constraints 'meets) :key #'second)))

(defun compare-irl-networks-with-common-predicate-in-diff ()
  (let ((fc-1 '((STRING GRAMMAR-LEARNING::?HOW-180 "how")
                     (STRING GRAMMAR-LEARNING::?MANY-120 "many")
                     (STRING GRAMMAR-LEARNING::?BROWN-144 "brown")
                     (STRING GRAMMAR-LEARNING::?SHINY-216 "shiny")
                     (STRING GRAMMAR-LEARNING::?OBJECTS-56 "objects")
                     (STRING GRAMMAR-LEARNING::?ARE-357 "are")
                     (STRING GRAMMAR-LEARNING::?THERE-371 "there")
                     (FCG:MEETS GRAMMAR-LEARNING::?HOW-180 GRAMMAR-LEARNING::?MANY-120)
                     (FCG:MEETS GRAMMAR-LEARNING::?MANY-120 GRAMMAR-LEARNING::?BROWN-144)
                     (FCG:MEETS GRAMMAR-LEARNING::?BROWN-144 GRAMMAR-LEARNING::?SHINY-216)
                     (FCG:MEETS GRAMMAR-LEARNING::?SHINY-216 GRAMMAR-LEARNING::?OBJECTS-56)
                     (FCG:MEETS GRAMMAR-LEARNING::?OBJECTS-56 GRAMMAR-LEARNING::?ARE-357)
                     (FCG:MEETS GRAMMAR-LEARNING::?ARE-357 GRAMMAR-LEARNING::?THERE-371)))
        (fc-2 '((STRING GRAMMAR-LEARNING::?HOW-185 "how")
                     (STRING GRAMMAR-LEARNING::?MANY-125 "many")
                     (STRING GRAMMAR-LEARNING::?BIG-259 "big")
                     (STRING GRAMMAR-LEARNING::?GREEN-101 "green")
                     (STRING GRAMMAR-LEARNING::?SHINY-221 "shiny")
                     (STRING GRAMMAR-LEARNING::?BLOCKS-50 "blocks")
                     (STRING GRAMMAR-LEARNING::?ARE-367 "are")
                     (STRING GRAMMAR-LEARNING::?THERE-381 "there")
                     (FCG:MEETS GRAMMAR-LEARNING::?HOW-185 GRAMMAR-LEARNING::?MANY-125)
                     (FCG:MEETS GRAMMAR-LEARNING::?MANY-125 GRAMMAR-LEARNING::?BIG-259)
                     (FCG:MEETS GRAMMAR-LEARNING::?BIG-259 GRAMMAR-LEARNING::?GREEN-101)
                     (FCG:MEETS GRAMMAR-LEARNING::?GREEN-101 GRAMMAR-LEARNING::?SHINY-221)
                     (FCG:MEETS GRAMMAR-LEARNING::?SHINY-221 GRAMMAR-LEARNING::?BLOCKS-50)
                     (FCG:MEETS GRAMMAR-LEARNING::?BLOCKS-50 GRAMMAR-LEARNING::?ARE-367)
                     (FCG:MEETS GRAMMAR-LEARNING::?ARE-367 GRAMMAR-LEARNING::?THERE-381)))
        (fc-3 '((STRING GRAMMAR-LEARNING::?HOW-180 "how")
                     (STRING GRAMMAR-LEARNING::?MANY-120 "many")
                     (STRING GRAMMAR-LEARNING::?BROWN-144 "yellow")
                     (STRING GRAMMAR-LEARNING::?SHINY-216 "shiny")
                     (STRING GRAMMAR-LEARNING::?OBJECTS-56 "spheres")
                     (STRING GRAMMAR-LEARNING::?ARE-357 "are")
                     (STRING GRAMMAR-LEARNING::?THERE-371 "there")
                     (FCG:MEETS GRAMMAR-LEARNING::?HOW-180 GRAMMAR-LEARNING::?MANY-120)
                     (FCG:MEETS GRAMMAR-LEARNING::?MANY-120 GRAMMAR-LEARNING::?BROWN-144)
                     (FCG:MEETS GRAMMAR-LEARNING::?BROWN-144 GRAMMAR-LEARNING::?SHINY-216)
                     (FCG:MEETS GRAMMAR-LEARNING::?SHINY-216 GRAMMAR-LEARNING::?OBJECTS-56)
                     (FCG:MEETS GRAMMAR-LEARNING::?OBJECTS-56 GRAMMAR-LEARNING::?ARE-357)
                     (FCG:MEETS GRAMMAR-LEARNING::?ARE-357 GRAMMAR-LEARNING::?THERE-371)))
        (m-1 '((CLEVR-WORLD:GET-CONTEXT GRAMMAR-LEARNING::?SOURCE-1)
              (CLEVR-WORLD:FILTER GRAMMAR-LEARNING::?TARGET-5862 GRAMMAR-LEARNING::?TARGET-2 GRAMMAR-LEARNING::?COLOR-10)
              (UTILS:BIND CLEVR-WORLD:MATERIAL-CATEGORY GRAMMAR-LEARNING::?MATERIAL-4 CLEVR-WORLD:METAL)
              (CLEVR-WORLD:FILTER GRAMMAR-LEARNING::?TARGET-1 GRAMMAR-LEARNING::?SOURCE-1 GRAMMAR-LEARNING::?SHAPE-8)
              (UTILS:BIND CLEVR-WORLD:SHAPE-CATEGORY GRAMMAR-LEARNING::?SHAPE-8 CLEVR-WORLD:THING)
              (CLEVR-WORLD:FILTER GRAMMAR-LEARNING::?TARGET-2 GRAMMAR-LEARNING::?TARGET-1 GRAMMAR-LEARNING::?MATERIAL-4)
              (UTILS:BIND CLEVR-WORLD:COLOR-CATEGORY GRAMMAR-LEARNING::?COLOR-10 CLEVR-WORLD:BROWN)
              (CLEVR-WORLD:COUNT! GRAMMAR-LEARNING::?TARGET-16 GRAMMAR-LEARNING::?TARGET-5862)))

        (m-2 '((CLEVR-WORLD:GET-CONTEXT GRAMMAR-LEARNING::?SOURCE-1)
              (CLEVR-WORLD:FILTER GRAMMAR-LEARNING::?TARGET-9641 GRAMMAR-LEARNING::?TARGET-9626 GRAMMAR-LEARNING::?SIZE-4)
              (UTILS:BIND CLEVR-WORLD:COLOR-CATEGORY GRAMMAR-LEARNING::?COLOR-8 CLEVR-WORLD:GREEN)
              (CLEVR-WORLD:FILTER GRAMMAR-LEARNING::?TARGET-2 GRAMMAR-LEARNING::?TARGET-1 GRAMMAR-LEARNING::?MATERIAL-4)
              (UTILS:BIND CLEVR-WORLD:SHAPE-CATEGORY GRAMMAR-LEARNING::?SHAPE-2 CLEVR-WORLD:CUBE)
              (CLEVR-WORLD:FILTER GRAMMAR-LEARNING::?TARGET-1 GRAMMAR-LEARNING::?SOURCE-1 GRAMMAR-LEARNING::?SHAPE-2)
              (UTILS:BIND CLEVR-WORLD:MATERIAL-CATEGORY GRAMMAR-LEARNING::?MATERIAL-4 CLEVR-WORLD:METAL)
              (CLEVR-WORLD:FILTER GRAMMAR-LEARNING::?TARGET-9626 GRAMMAR-LEARNING::?TARGET-2 GRAMMAR-LEARNING::?COLOR-8)
              (UTILS:BIND CLEVR-WORLD:SIZE-CATEGORY GRAMMAR-LEARNING::?SIZE-4 CLEVR-WORLD:LARGE)
              (CLEVR-WORLD:COUNT! GRAMMAR-LEARNING::?TARGET-16 GRAMMAR-LEARNING::?TARGET-9641))))
    (diff-meaning-networks m-1 m-2 :irl)
    ;(diff-form-constraints fc-1 fc-3)
    )
  )
;(compare-irl-networks-with-common-predicate-in-diff)

(defun get-bind-statement (predicate network)
  (let* ((open-vars (third (multiple-value-list (extract-vars-from-irl-network (list predicate)))))
        (first-open-var (first open-vars)))
    (when first-open-var
      (get-irl-predicate-from-in-var first-open-var network))))

(defun sort-clevr-stage-1-network (network)
  (loop with current-pred = (get-predicate-with-target-var network)
        with sorted-network = nil
        for next-pred = (get-next-irl-predicate current-pred network)
        for bind-statement = (get-bind-statement current-pred network)
        while next-pred
        do (when bind-statement 
             (push bind-statement sorted-network))
        (push current-pred sorted-network) 
        (setf current-pred next-pred)
        finally 
        (when bind-statement 
          (push bind-statement sorted-network))
        (push current-pred sorted-network)
        (return sorted-network)))

(defun next-irl-predicate (current-predicate network)
  (when current-predicate
    (nth (+ 1 (position current-predicate network)) network)))

(defun extract-continuous-subnetwork (ordered-predicates sorted-network)
  (let* ((end (+ 1 (position (first ordered-predicates) sorted-network)))
         (start (position (last-elt ordered-predicates) sorted-network)))
    (subseq sorted-network start end)))
                                         
                      

(defmethod diff-meaning-networks (network-1 network-2 (mode (eql :irl)))
  "traverse both networks, return the longest possible sequence of non-overlapping predicates, assumes the network to be linear, and the variables to have a consistent position for traversal"
  (loop with sorted-network-1 = (sort-clevr-stage-1-network network-1)
        with sorted-network-2 = (sort-clevr-stage-1-network network-2)
        with current-predicate-1 = (first sorted-network-1)
        with current-predicate-2 = (first sorted-network-2)
        with last-equivalent-predicate-1 = current-predicate-1
        with overlapping-predicates-1 = nil
        with overlapping-predicates-2 = nil
        while (or current-predicate-1 current-predicate-2)
        for next-predicate-1 = (next-irl-predicate current-predicate-1 sorted-network-1)
        for next-predicate-2 = (next-irl-predicate current-predicate-2 sorted-network-2)
        for equivalent-predicates-p = (unify-irl-programs (list current-predicate-1) (list current-predicate-2))
        do (if equivalent-predicates-p
             (progn ;; true condition
               (setf last-equivalent-predicate-1 current-predicate-1) ;; keep track of last successful comparison
               (push current-predicate-1 overlapping-predicates-1)
               (push current-predicate-2 overlapping-predicates-2)
               (setf current-predicate-1 next-predicate-1)
               (setf current-predicate-2 next-predicate-2))
             ;; false condition
             (setf current-predicate-1 next-predicate-1)) ; traverse network 1 while network 2 stays static
        (when (and (not current-predicate-1) current-predicate-2) ;; stack 1 is empty, stack 2 is not so go back to the last equivalent predicate, and take the next predicate
          (setf current-predicate-1 (next-irl-predicate last-equivalent-predicate-1 sorted-network-1))
          (setf current-predicate-2 next-predicate-2))
               
        finally (return (values (extract-continuous-subnetwork (set-difference sorted-network-1 overlapping-predicates-1 :test #'equal) sorted-network-1)
                                
                                (extract-continuous-subnetwork (set-difference sorted-network-2 overlapping-predicates-2 :test #'equal) sorted-network-2)))))

#|
(diff-meaning-networks '((CLEVR-WORLD:QUERY GRAMMAR-LEARNING::?VAR-3220 GRAMMAR-LEARNING::?VAR-3218 GRAMMAR-LEARNING::?VAR-3219)
                         (UTILS:BIND CLEVR-WORLD:ATTRIBUTE-CATEGORY GRAMMAR-LEARNING::?VAR-3219 FCG:SIZE)
                         (CLEVR-WORLD:UNIQUE GRAMMAR-LEARNING::?VAR-3218 GRAMMAR-LEARNING::?VAR-3217)
                         (CLEVR-WORLD:FILTER GRAMMAR-LEARNING::?VAR-3217 GRAMMAR-LEARNING::?VAR-3215 GRAMMAR-LEARNING::?VAR-3216)
                         (CLEVR-WORLD:FILTER GRAMMAR-LEARNING::?VAR-3215 GRAMMAR-LEARNING::?VAR-3213 GRAMMAR-LEARNING::?VAR-3214)
                         (UTILS:BIND CLEVR-WORLD:SHAPE-CATEGORY GRAMMAR-LEARNING::?VAR-3216 CLEVR-WORLD:CYLINDER)
                         (UTILS:BIND CLEVR-WORLD:MATERIAL-CATEGORY GRAMMAR-LEARNING::?VAR-3214 CLEVR-WORLD:RUBBER)
                         (CLEVR-WORLD:GET-CONTEXT GRAMMAR-LEARNING::?VAR-3213))
                       '((CLEVR-WORLD:QUERY GRAMMAR-LEARNING::?VAR-3184 GRAMMAR-LEARNING::?VAR-3182 GRAMMAR-LEARNING::?VAR-3183)
                         (UTILS:BIND CLEVR-WORLD:ATTRIBUTE-CATEGORY GRAMMAR-LEARNING::?VAR-3183 CLEVR-WORLD:COLOR)
                         (CLEVR-WORLD:UNIQUE GRAMMAR-LEARNING::?VAR-3182 GRAMMAR-LEARNING::?VAR-3181)
                         (CLEVR-WORLD:FILTER GRAMMAR-LEARNING::?VAR-3181 GRAMMAR-LEARNING::?VAR-3179 GRAMMAR-LEARNING::?VAR-3180)
                         (CLEVR-WORLD:FILTER GRAMMAR-LEARNING::?VAR-3179 GRAMMAR-LEARNING::?VAR-3177 GRAMMAR-LEARNING::?VAR-3178)
                         (UTILS:BIND CLEVR-WORLD:SHAPE-CATEGORY GRAMMAR-LEARNING::?VAR-3180 CLEVR-WORLD:CYLINDER)
                         (UTILS:BIND CLEVR-WORLD:MATERIAL-CATEGORY GRAMMAR-LEARNING::?VAR-3178 CLEVR-WORLD:RUBBER)
                         (CLEVR-WORLD:GET-CONTEXT GRAMMAR-LEARNING::?VAR-3177)) :irl)

(set-difference 
                '((CLEVR-WORLD:QUERY GRAMMAR-LEARNING::?VAR-3220 GRAMMAR-LEARNING::?VAR-3218 GRAMMAR-LEARNING::?VAR-3219)
                  (UTILS:BIND CLEVR-WORLD:ATTRIBUTE-CATEGORY GRAMMAR-LEARNING::?VAR-3219 FCG:SIZE)
                  (CLEVR-WORLD:UNIQUE GRAMMAR-LEARNING::?VAR-3218 GRAMMAR-LEARNING::?VAR-3217)
                  (CLEVR-WORLD:FILTER GRAMMAR-LEARNING::?VAR-3217 GRAMMAR-LEARNING::?VAR-3215 GRAMMAR-LEARNING::?VAR-3216)
                  (CLEVR-WORLD:FILTER GRAMMAR-LEARNING::?VAR-3215 GRAMMAR-LEARNING::?VAR-3213 GRAMMAR-LEARNING::?VAR-3214)
                  (UTILS:BIND CLEVR-WORLD:SHAPE-CATEGORY GRAMMAR-LEARNING::?VAR-3216 CLEVR-WORLD:CYLINDER)
                  (UTILS:BIND CLEVR-WORLD:MATERIAL-CATEGORY GRAMMAR-LEARNING::?VAR-3214 CLEVR-WORLD:RUBBER)
                  (CLEVR-WORLD:GET-CONTEXT GRAMMAR-LEARNING::?VAR-3213))
                '((UTILS:BIND CLEVR-WORLD:ATTRIBUTE-CATEGORY GRAMMAR-LEARNING::?VAR-3219 FCG:SIZE)) :test #'equal)

=> '((CLEVR-WORLD:GET-CONTEXT GRAMMAR-LEARNING::?VAR-3213)
     (UTILS:BIND CLEVR-WORLD:MATERIAL-CATEGORY GRAMMAR-LEARNING::?VAR-3214 CLEVR-WORLD:RUBBER)
     (UTILS:BIND CLEVR-WORLD:SHAPE-CATEGORY GRAMMAR-LEARNING::?VAR-3216 CLEVR-WORLD:CYLINDER)
     (CLEVR-WORLD:FILTER GRAMMAR-LEARNING::?VAR-3215 GRAMMAR-LEARNING::?VAR-3213 GRAMMAR-LEARNING::?VAR-3214)
     (CLEVR-WORLD:FILTER GRAMMAR-LEARNING::?VAR-3217 GRAMMAR-LEARNING::?VAR-3215 GRAMMAR-LEARNING::?VAR-3216)
     (CLEVR-WORLD:UNIQUE GRAMMAR-LEARNING::?VAR-3218 GRAMMAR-LEARNING::?VAR-3217)
     (CLEVR-WORLD:QUERY GRAMMAR-LEARNING::?VAR-3220 GRAMMAR-LEARNING::?VAR-3218 GRAMMAR-LEARNING::?VAR-3219))

(diff-meaning-networks '((CLEVR-WORLD:GET-CONTEXT GRAMMAR-LEARNING::?SOURCE-1) 
                         (CLEVR-WORLD:FILTER GRAMMAR-LEARNING::?TARGET-1 GRAMMAR-LEARNING::?SOURCE-1 GRAMMAR-LEARNING::?SHAPE-4) 
                         (UTILS:BIND CLEVR-WORLD:SHAPE-CATEGORY GRAMMAR-LEARNING::?SHAPE-4 CLEVR-WORLD:SPHERE) 
                         (CLEVR-WORLD:FILTER GRAMMAR-LEARNING::?TARGET-2 GRAMMAR-LEARNING::?TARGET-1 GRAMMAR-LEARNING::?MATERIAL-4) 
                         (UTILS:BIND CLEVR-WORLD:MATERIAL-CATEGORY GRAMMAR-LEARNING::?MATERIAL-4 CLEVR-WORLD:METAL) 
                         (CLEVR-WORLD:FILTER GRAMMAR-LEARNING::?TARGET-46777 GRAMMAR-LEARNING::?TARGET-2 GRAMMAR-LEARNING::?SIZE-4) 
                         (UTILS:BIND CLEVR-WORLD:SIZE-CATEGORY GRAMMAR-LEARNING::?SIZE-4 CLEVR-WORLD:LARGE) 
                         (CLEVR-WORLD:UNIQUE GRAMMAR-LEARNING::?TARGET-OBJECT-1 GRAMMAR-LEARNING::?TARGET-46777) 
                         (CLEVR-WORLD:QUERY GRAMMAR-LEARNING::?TARGET-3 GRAMMAR-LEARNING::?TARGET-OBJECT-1 GRAMMAR-LEARNING::?ATTRIBUTE-4) 
                         (UTILS:BIND CLEVR-WORLD:ATTRIBUTE-CATEGORY GRAMMAR-LEARNING::?ATTRIBUTE-4 CLEVR-WORLD:COLOR))
                       '((CLEVR-WORLD:GET-CONTEXT GRAMMAR-LEARNING::?SOURCE-1) 
                         (CLEVR-WORLD:FILTER GRAMMAR-LEARNING::?TARGET-1 GRAMMAR-LEARNING::?SOURCE-1 GRAMMAR-LEARNING::?SHAPE-2) 
                         (UTILS:BIND CLEVR-WORLD:SHAPE-CATEGORY GRAMMAR-LEARNING::?SHAPE-2 CLEVR-WORLD:CUBE) 
                         (CLEVR-WORLD:FILTER GRAMMAR-LEARNING::?TARGET-2 GRAMMAR-LEARNING::?TARGET-1 GRAMMAR-LEARNING::?MATERIAL-4) 
                         (UTILS:BIND CLEVR-WORLD:MATERIAL-CATEGORY GRAMMAR-LEARNING::?MATERIAL-4 CLEVR-WORLD:METAL) 
                         (CLEVR-WORLD:FILTER GRAMMAR-LEARNING::?TARGET-114412 GRAMMAR-LEARNING::?TARGET-2 GRAMMAR-LEARNING::?SIZE-2) 
                         (UTILS:BIND CLEVR-WORLD:SIZE-CATEGORY GRAMMAR-LEARNING::?SIZE-2 CLEVR-WORLD:SMALL) 
                         (CLEVR-WORLD:UNIQUE GRAMMAR-LEARNING::?TARGET-OBJECT-1 GRAMMAR-LEARNING::?TARGET-114412) 
                         (CLEVR-WORLD:QUERY GRAMMAR-LEARNING::?TARGET-3 GRAMMAR-LEARNING::?TARGET-OBJECT-1 GRAMMAR-LEARNING::?ATTRIBUTE-4) 
                         (UTILS:BIND CLEVR-WORLD:ATTRIBUTE-CATEGORY GRAMMAR-LEARNING::?ATTRIBUTE-4 CLEVR-WORLD:COLOR)) :irl)


(extract-args-from-meaning-networks '((UTILS:BIND CLEVR-WORLD:SHAPE-CATEGORY GRAMMAR-LEARNING::?SHAPE-4 CLEVR-WORLD:SPHERE)
                                      (CLEVR-WORLD:FILTER GRAMMAR-LEARNING::?TARGET-2 GRAMMAR-LEARNING::?TARGET-1 GRAMMAR-LEARNING::?MATERIAL-4)
                                      (UTILS:BIND CLEVR-WORLD:MATERIAL-CATEGORY GRAMMAR-LEARNING::?MATERIAL-4 CLEVR-WORLD:METAL)
                                      (CLEVR-WORLD:FILTER GRAMMAR-LEARNING::?TARGET-46777 GRAMMAR-LEARNING::?TARGET-2 GRAMMAR-LEARNING::?SIZE-4)
                                      (UTILS:BIND CLEVR-WORLD:SIZE-CATEGORY GRAMMAR-LEARNING::?SIZE-4 CLEVR-WORLD:LARGE))
                                    '((UTILS:BIND CLEVR-WORLD:ATTRIBUTE-CATEGORY GRAMMAR-LEARNING::?ATTRIBUTE-4 CLEVR-WORLD:COLOR)
                                      (CLEVR-WORLD:QUERY GRAMMAR-LEARNING::?TARGET-3 GRAMMAR-LEARNING::?TARGET-OBJECT-1 GRAMMAR-LEARNING::?ATTRIBUTE-4)
                                      (CLEVR-WORLD:UNIQUE GRAMMAR-LEARNING::?TARGET-OBJECT-1 GRAMMAR-LEARNING::?TARGET-46777)
                                      (CLEVR-WORLD:FILTER GRAMMAR-LEARNING::?TARGET-1 GRAMMAR-LEARNING::?SOURCE-1 GRAMMAR-LEARNING::?SHAPE-4)
                                      (CLEVR-WORLD:GET-CONTEXT GRAMMAR-LEARNING::?SOURCE-1)) :amr)

(extract-args-from-meaning-networks
'((UTILS:BIND CLEVR-WORLD:ATTRIBUTE-CATEGORY GRAMMAR-LEARNING::?VAR-3219 FCG:SIZE))
 '((CLEVR-WORLD:GET-CONTEXT GRAMMAR-LEARNING::?VAR-3213)
     (UTILS:BIND CLEVR-WORLD:MATERIAL-CATEGORY GRAMMAR-LEARNING::?VAR-3214 CLEVR-WORLD:RUBBER)
     (UTILS:BIND CLEVR-WORLD:SHAPE-CATEGORY GRAMMAR-LEARNING::?VAR-3216 CLEVR-WORLD:CYLINDER)
     (CLEVR-WORLD:FILTER GRAMMAR-LEARNING::?VAR-3215 GRAMMAR-LEARNING::?VAR-3213 GRAMMAR-LEARNING::?VAR-3214)
     (CLEVR-WORLD:FILTER GRAMMAR-LEARNING::?VAR-3217 GRAMMAR-LEARNING::?VAR-3215 GRAMMAR-LEARNING::?VAR-3216)
     (CLEVR-WORLD:UNIQUE GRAMMAR-LEARNING::?VAR-3218 GRAMMAR-LEARNING::?VAR-3217)
     (CLEVR-WORLD:QUERY GRAMMAR-LEARNING::?VAR-3220 GRAMMAR-LEARNING::?VAR-3218 GRAMMAR-LEARNING::?VAR-3219)) :amr)

=> (GRAMMAR-LEARNING::?VAR-3219)


 => (GRAMMAR-LEARNING::?SHAPE-4 GRAMMAR-LEARNING::?TARGET-1 GRAMMAR-LEARNING::?TARGET-46777)

(set-difference '((CLEVR-WORLD:GET-CONTEXT GRAMMAR-LEARNING::?SOURCE-1)                                                                                                                
                  (CLEVR-WORLD:FILTER GRAMMAR-LEARNING::?TARGET-1 GRAMMAR-LEARNING::?SOURCE-1 GRAMMAR-LEARNING::?SHAPE-4) 
                  (UTILS:BIND CLEVR-WORLD:SHAPE-CATEGORY GRAMMAR-LEARNING::?SHAPE-4 CLEVR-WORLD:SPHERE) 
                  (CLEVR-WORLD:FILTER GRAMMAR-LEARNING::?TARGET-2 GRAMMAR-LEARNING::?TARGET-1 GRAMMAR-LEARNING::?MATERIAL-4) 
                  (UTILS:BIND CLEVR-WORLD:MATERIAL-CATEGORY GRAMMAR-LEARNING::?MATERIAL-4 CLEVR-WORLD:METAL) 
                  (CLEVR-WORLD:FILTER GRAMMAR-LEARNING::?TARGET-46777 GRAMMAR-LEARNING::?TARGET-2 GRAMMAR-LEARNING::?SIZE-4) 
                  (UTILS:BIND CLEVR-WORLD:SIZE-CATEGORY GRAMMAR-LEARNING::?SIZE-4 CLEVR-WORLD:LARGE) 
                  (CLEVR-WORLD:UNIQUE GRAMMAR-LEARNING::?TARGET-OBJECT-1 GRAMMAR-LEARNING::?TARGET-46777) 
                  (CLEVR-WORLD:QUERY GRAMMAR-LEARNING::?TARGET-3 GRAMMAR-LEARNING::?TARGET-OBJECT-1 GRAMMAR-LEARNING::?ATTRIBUTE-4) 
                  (UTILS:BIND CLEVR-WORLD:ATTRIBUTE-CATEGORY GRAMMAR-LEARNING::?ATTRIBUTE-4 CLEVR-WORLD:COLOR))
                '((UTILS:BIND CLEVR-WORLD:SHAPE-CATEGORY GRAMMAR-LEARNING::?SHAPE-4 CLEVR-WORLD:SPHERE)
                  (CLEVR-WORLD:FILTER GRAMMAR-LEARNING::?TARGET-2 GRAMMAR-LEARNING::?TARGET-1 GRAMMAR-LEARNING::?MATERIAL-4)
                  (UTILS:BIND CLEVR-WORLD:MATERIAL-CATEGORY GRAMMAR-LEARNING::?MATERIAL-4 CLEVR-WORLD:METAL)
                  (CLEVR-WORLD:FILTER GRAMMAR-LEARNING::?TARGET-46777 GRAMMAR-LEARNING::?TARGET-2 GRAMMAR-LEARNING::?SIZE-4)
                  (UTILS:BIND CLEVR-WORLD:SIZE-CATEGORY GRAMMAR-LEARNING::?SIZE-4 CLEVR-WORLD:LARGE))

                :test #'equal

                )

|#