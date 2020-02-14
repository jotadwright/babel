
(ql:quickload :clevr-evaluation)
(in-package :clevr-evaluation)

(activate-monitor trace-fcg)
(activate-monitor trace-irl-in-web-browser)

;; Data needed for demo
(defparameter *clevr-world* (make-instance 'clevr-world :data-sets '("val")))
(defparameter *scene-1* (get-scene-by-index *clevr-world* 0))
(defparameter *scene-2* (get-scene-by-index *clevr-world* 1))
(defparameter *scene-3* (get-scene-by-index *clevr-world* 2))
(defparameter *scene-4* (get-scene-by-index *clevr-world* 3))
(defparameter *scene-5* (get-scene-by-index *clevr-world* 4))

(defun show-image-in-web-interface (img-path)
  (copy-file img-path
             (make-pathname :directory '(:absolute "~" "Sites")
                            :name (pathname-name img-path)
                            :type (pathname-type img-path)))
  (if (null cl-user::*localhost-user-dir*)
    (error "Please set your *localhost-user-dir* in init-babel-user.lisp to be able to display images in the web interface.")
    (add-element
     `((img :src ,(merge-pathnames
                   (make-pathname :name (pathname-name img-path)
                                  :type (pathname-type img-path))
                   cl-user::*localhost-user-dir*))))))


                                  
;; ##################
;; + CLEVR IRL Demo +
;; ##################

(defun header ()
  (clear-page)
  (add-element '((h1) ((u) "CLEVR IRL Demo")))
  (add-element '((h2) ((i) "Natural Language Processing")))
  (add-element '((h3) "This document assumes that you have read and are familiar
with the concepts introduced in the paper \"Open-ended Procedural Semantics\"."))
  (add-element '((h3) "In this demo, we give a pratical example of how IRL can
be used in emergent communication research."))
  (add-element '((h3) "In particular, we focus on a question-answer type game,
set in the CLEVR environment."))
  (add-element '((hr))))


;; Part 1: Building blocks of IRL
;; ------------------------------

(defun part-1 ()
  (add-element '((h2) ((i) "1. The Building Blocks of IRL")))
  (add-element '((h3) "The building blocks of IRL are the semantic entities.
These can be used to represent the agent's knowledge (the ontology) and the world model."))
  (add-element '((h3) "The ontology for the CLEVR world contains knowledge about the
objects and their properties:"))
  (remove-data *clevr-ontology* 'clevr-context)
  (add-element (make-html *clevr-ontology*))
  (add-element '((h3) "Each of these is a semantic category or " ((i) "entity")))
  (add-element (make-html (last-elt (get-data *clevr-ontology* 'shapes))))
  (add-element '((h3) "Entities are also used to represent the CLEVR scenes"))
  (add-element (make-html *scene-1* :expand-initially t))
  (add-element '((h3) "All cognitive operations operate over these semantic entities."))
  (add-element '((h3) "In the CLEVR world, the following primitive operations are
available and implemented in multiple directions:"))
  (add-element `((ul)
                 ((li) ,(irl-program->svg '((get-context ?context))))
                 ((li) ,(irl-program->svg '((filter ?target-set ?source-set ?binding))))
                 ((li) ,(irl-program->svg '((query ?target-attribute ?source-object ?binding))))
                 ((li) ,(irl-program->svg '((count! ?target-num ?source-set))))
                 ((li) ,(irl-program->svg '((unique ?target-object ?source-set))))
                 ((li) ,(irl-program->svg '((exist ?target-bool ?source-set))))
                 ((li) ,(irl-program->svg '((intersect ?target-set ?source-set-1 ?source-set-2))))
                 ((li) ,(irl-program->svg '((union! ?target-set ?source-set-1 ?source-set-2))))
                 ((li) ,(irl-program->svg '((relate ?target-set ?source-object ?binding))))
                 ((li) ,(irl-program->svg '((same ?target-set ?source-object ?binding))))
                 ((li) ,(irl-program->svg '((equal-integer ?target-bool ?source-num-1 ?source-num-2))))
                 ((li) ,(irl-program->svg '((less-than ?target-bool ?source-num-1 ?source-num-2))))
                 ((li) ,(irl-program->svg '((greater-than ?target-bool ?source-num-1 ?source-num-2))))
                 ((li) ,(irl-program->svg '((equal? ?target-bool ?source-attribute-1 ?source-attribute-2 ?binding))))))
  (add-element '((hr))))



;; Part 2: Executing IRL Networks
;; ------------------------------

(defun part-2 ()
  (add-element '((h2) ((i) "2. Executing IRL Networks")))
  (add-element '((h3) "The execution of IRL networks is mostly used by the
hearer in order to interpret an utterance in the environment."))
  (add-element '((h3) "The language level, in this case Fluid Construction Grammar,
will map the natural language questions from the CLEVR dataset to a procedural
semantics representation (i.e. an IRL network)."))
  (add-element '((h3) "This network is than executed
on the particular CLEVR scene in order to obtain the answer."))
  (add-element '((h3) "Here are some example sentences on the following scene:"))
  (add-element (make-html *scene-1* :expand-initially t))
  (set-data *clevr-ontology* 'clevr-context *scene-1*)
  (let ((meaning (comprehend (preprocess-sentence "Are there any other things that are the same shape as the big metallic object?") :silent t)))
    (add-element '((h3) ((i) "Are there any other things that are the same shape as the big metallic object?")))
    (evaluate-irl-program meaning *clevr-ontology*))
  (let ((meaning (comprehend (preprocess-sentence "Is there a big brown object of the same shape as the green thing?") :silent t)))
    (add-element '((h3) ((i) "Is there a big brown object of the same shape as the green thing?")))
    (evaluate-irl-program meaning *clevr-ontology*))
  (add-element '((h3) "In these examples, there is no search as all variables except
the target variable are bound."))
  (add-element '((h3) "With more unbound variables, the IRL network execution
process actually creates a search space and can return multiple solutions,
as shown by the following example:"))
  (evaluate-irl-program
   '((get-context ?context)
     (filter ?set-1 ?context ?binding)
     (unique ?object ?set-1)
     (query ?target ?object ?attribute)
     (bind attribute-category ?attribute color)
     (bind color-category ?target purple))
   *clevr-ontology*)
  (add-element '((hr))))



;; Part 3: Building IRL Networks
;; -----------------------------

;; + Composer Class +
(defclass program-composer (irl:single-topic-composer)
  (;:expand-chunk-fns
   ;:check-chunk-fns
   ;:node-rating-fn
   ;:initial-chunk-score-fn
   ;:chunk-wrapper-fn
   ;:check-evaluation-result-fn
   ;:evaluation-result-scoring-fn
   )
  (:documentation "The program composer"))

;; + Check-chunk functions +
(defun counts-allowed-p (primitive-counts)
  (let ((allowed t))
    (loop for (primitive . count) in primitive-counts
          when allowed
          do (case primitive
               (count! (when (> count 2) (setf allowed nil)))
               (equal-integer (when (> count 1) (setf allowed nil)))
               (less-than (when (> count 1) (setf allowed nil)))
               (greater-than (when (> count 1) (setf allowed nil)))
               (equal? (when (> count 1) (setf allowed nil)))
               (exist (when (> count 1) (setf allowed nil)))
               (filter ())
               (get-context (when (> count 1) (setf allowed nil)))
               (intersect (when (> count 1) (setf allowed nil)))
               (query (when (> count 2) (setf allowed nil)))
               (relate (when (> count 3) (setf allowed nil)))
               (same (when (> count 1) (setf allowed nil)))
               (union! (when (> count 1) (setf allowed nil)))
               (unique (when (> count 4) (setf allowed nil)))))
    allowed))

(defun check-primitive-occurrence-counts (chunk)
  "check for every primitive if the allowed occurrence count
   is not violated, e.g. there can be only 1 'exist' primitive"
  (let* ((irl-program (irl:irl-program chunk))
         (unique-primitives (remove-duplicates (mapcar #'first irl-program)))
         (primitive-counts (loop for primitive in unique-primitives
                                 collect (cons primitive (count primitive irl-program :key #'first)))))
    (counts-allowed-p primitive-counts)))

; + Expand-chunk functions +
(defun add-context-var-to-open-vars (chunk)
  "The get-context var is allowed to occur 3 times in the irl-program,
   even though the get-context primitive is allowed to occur once.
   The variable needs to be shared. To enforce this, it is added to the
   open variables of the chunk."
  (let ((get-context-predicate (find 'get-context (irl:irl-program chunk) :key #'first)))
    (when get-context-predicate
      (let* ((context-var (last-elt get-context-predicate))
             (all-variables (find-all-anywhere-if #'variable-p (irl:irl-program chunk)))
             (context-var-count (count context-var all-variables)))
        (when (< context-var-count 3)
          (push (cons context-var 'clevr-object-set) (irl:open-vars chunk))))))
  chunk)

(defun check-duplicate-variables (expand-chunk-solutions)
  "Since the chunk was expanded using 'link-open-variables',
   it can happen that duplicate variables inside a predicate
   were introduced, e.g. (union ?out ?in ?in). This is not
   allowed. This function will filter out those bad chunks."
  (loop for (new-chunk . other-chunk) in expand-chunk-solutions
        when (loop for expr in (irl:irl-program new-chunk)
                   for variables = (cdr expr)
                   always (length= variables (remove-duplicates variables)))
        collect (cons new-chunk other-chunk)))

(defun expand-chunk-for-clevr (chunk composer)
  "This chunk expander combines two existing chunk expanders.
   First, it will check if the 'get-context' predicate is present.
   The variable ?context is allowed to occur 3 times in the CLEVR
   irl-programs. So, if occurring less than 3 times, it is added to
   the open variables of the chunk. Using this modified chunk, the
   link-open-variables chunk expander is tried and bad results are
   filtered out. These solutions are combines with the regular
   combine-program chunk expander that will try to add a primitive
   to the chunk."
  (append (check-duplicate-variables
           (irl:expand-chunk-link-open-variables
            (add-context-var-to-open-vars chunk)
            composer))
          (irl:expand-chunk-combine-program chunk composer)))


;; + Make default composer +
(defun make-default-composer (target-category &key chunks)
  "Make the composer"
  (make-instance 'program-composer
                 ;; what to look for
                 :topic target-category
                 ;; where to start from
                 :initial-chunk (make-instance 'irl:chunk :id 'initial
                                               :target-var '(?answer . t)
                                               :open-vars '((?answer . t)))
                 ;; available primitives
                 :chunks chunks
                 ;; search depth (depending on question types)
                 :max-search-depth 10
                 ;; available categories (for bind statements)
                 :ontology *clevr-ontology*
                 ;; filter out bad chunks
                 :check-chunk-fns (list #'check-primitive-occurrence-counts)
                 ;; how to expand chunks
                 :expand-chunk-fns (list #'expand-chunk-for-clevr)))

(defun part-3 ()
  (add-element '((h2) ((i) "3. Building IRL Networks")))
  (add-element '((h3) "The speaker will typically use the composer to
build a semantic network for the thing that it wants to say."))
  (add-element '((h3) "In the context of CLEVR, the speaker would like to
ask a question to which the answer is the large blue metal cube."))
  (add-element '((h3) "To contain the search space for the purpose of this demo, we don't allow the composer
to use all cognitive operations of the CLEVR world."))
  (add-element (make-html *scene-2* :expand-initially t))
  (set-data *clevr-ontology* 'clevr-context *scene-2*)
  (let ((composer (make-default-composer (nth 7 (objects *scene-2*))
                                         :chunks (mapcar #'irl:create-chunk-from-primitive
                                                         '(filter get-context unique)))))
    (irl:get-next-solutions composer))
  (add-element '((h3) "The composer finds four solutions that lead to the large blue metal cube."))
  (add-element '((hr))))




;; Part 4: Chunking
;; -----------------------------

(defparameter *chunk-1*
  (irl:create-chunk-from-irl-program
   '((filter ?set-2 ?set-1 ?shape)
     (filter ?set-3 ?set-2 ?attribute))
   :id 'single-attribute-object))

(defparameter *chunk-2*
  (irl:create-chunk-from-irl-program
   '((filter ?set-2 ?set-1 ?shape)
     (filter ?set-3 ?set-2 ?attribute-1)
     (filter ?set-4 ?set-3 ?attribute-2))
   :id 'double-attribute-object))

(defun part-4 ()
  (add-element '((h2) ((i) "4. Chunking")))
  (add-element '((h3) "The search process generated by the composer
can get very costly very fast."))
  (add-element '((h3) "To reduce the search space, we
introduce " ((i) "chunks") ". These are subnetworks that are "
                 ((i) "conventialized.")))
  (add-element '((h3) "A chunk is used in the same way as a regular
cognitive operation, the difference being that it contains in itself
a number of these operations."))
  (add-element '((h3) "In the CLEVR world, we could make a chunk
for an object with a single attribute or for an object with two
attributes:"))
  (add-element (make-html *chunk-1* :expand-initially t))
  (add-element (make-html *chunk-2* :expand-initially t))
  (add-element '((h3) "The composer can now use these chunks
to find a solution faster"))
  (add-element (make-html *scene-2* :expand-initially t))
  (set-data *clevr-ontology* 'clevr-context *scene-2*)
  (let ((composer (make-default-composer (nth 7 (objects *scene-2*))
                                         :chunks (append
                                                  (mapcar #'irl:create-chunk-from-primitive
                                                          '(get-context unique))
                                                  (list *chunk-1* *chunk-2*)))))
    (irl:get-next-solutions composer))
  (add-element '((h3) "In this case, the depth of the search tree was reduced by 1
using the single attribute chunk."))
  (add-element '((hr))))



;; Part 5: Matching
;; ----------------


(defparameter *partial-meaning*
  '((bind size-category ?size small)
    (count! ?target ?source-set)))

(defclass my-composer (irl:chunk-composer)
  ())

(defun duplicate-attribute-bindings (result)
  (let* ((attribute-bindings
          (loop for binding in (irl:bindings result)
                when (typep (irl::value binding) 'attribute)
                collect binding)))
    (loop for binding in attribute-bindings
          for rest = (remove binding attribute-bindings)
          thereis (find (irl::value binding) rest :key #'irl::value))))

(defmethod irl:handle-node ((node irl:chunk-composer-node)
                            (handler (eql 'irl:evaluate))
                            (composer my-composer))
  (let ((evaluation-results (call-next-method)))
    (values (loop for result in evaluation-results
                  ;; check if the evaluation results indeed discriminates
                  ;; the topic
                  unless (or (= (irl:target-entity result) 0)
                             (duplicate-attribute-bindings result))
                  collect result)
            nil)))


(defun part-5 ()
  (add-element '((h2) ((i) "5. Matching")))
  (add-element '((h3) "IRL offers flexible interpretation. This can be
used in case of ungrammatical sentences when the hearer can only
partly parse the utterance."))
  (add-element '((h3) "In this case, the hearer has an incomplete IRL network."))
  (add-element '((h3) "The matcher looks for a IRL network that matches
the incomplete network and can be successfully executed at the same time."))
  (add-element '((h3) "In other words, the hearer tries to complete the partial
plan that it received such that it makes sense in his world model."))
  (add-element '((h3) "The matches essentially does the same process as the composer, but
with a different starting point."))
  (add-element '((h3) "Suppose the hearer only understood the words \"count\" and \"small\"."))
  (add-element '((h3) "It gets a partial program involving some cognitive operations and bind statelents, but
does not know how these are related."))
  (add-element (irl-program->svg *partial-meaning*))
  (let ((composer
         (make-instance 'my-composer
                        :meaning *partial-meaning*
                        :initial-chunk (make-instance 'irl:chunk :id 'initial
                                                      :target-var '(?answer . number)
                                                      :open-vars '((?answer . number)))
                        :chunks (mapcar #'irl:create-chunk-from-primitive
                                        '(get-context filter count!))
                        :max-search-depth 5
                        :ontology *clevr-ontology*)))
    (irl:get-all-solutions composer))
  (add-element '((h3) "The matcher finds many different ways to count small things."))
  (add-element '((h1) ((i) "The End")))
  (add-element '((hr))))
                               

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Full Demo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun full-demo ()
  (header)
  (part-1)
  (part-2)
  (part-3)
  (part-4)
  (part-5))

;(full-demo)


;;;; Static web page
(setf web-interface::*static-html-output-dir* (make-pathname :directory '(:absolute "tmp" "jnevens")))
(web-interface:create-static-html-page "clevr-irl-demo" (full-demo))