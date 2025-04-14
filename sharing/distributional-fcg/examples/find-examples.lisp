(in-package :propbank-grammar)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                              ;;
;;              Code for finding the examples                   ;;
;;                                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-closest-match (lemma cxn-inventory)
  (let ((vector (second (nlp-tools:get-word-embedding (downcase lemma))))
        (lex-cxns (loop for cxn in (constructions-list cxn-inventory)
                        when (attr-val cxn :lex-category)
                          collect cxn)))
       (loop for cxn in lex-cxns
             for lemma = (downcase (attr-val cxn :lemma))
             for pointer =  (attr-val cxn :lemma-embedding-pointer)
             for cxn-vector = (cdr (assoc pointer (get-data (blackboard cxn-inventory) :cxn-token-embeddings)))
             for similarity = (cosine-similarity vector cxn-vector)
             collect (cons lemma similarity) into similarities
             finally (return (sort similarities #'> :key #'cdr)))))


(defun add-grammatical-cxn (gold-frame core-units-with-role cxn-inventory propbank-sentence lex-category)
  "Learns a grammatical construction capturing all core roles and adds
a grammatical category to the categorial network. Returns the
grammatical category."
  
  (let* ((ts-unit-structure (ts-unit-structure propbank-sentence cxn-inventory))
         (gram-category (make-gram-category core-units-with-role))
         (cxn-units-with-role (loop for unit in core-units-with-role
                                    collect (make-propbank-conditional-unit-with-role unit gram-category 'fee)))
         (cxn-units-without-role (make-propbank-conditional-units-without-role core-units-with-role
                                                                               cxn-units-with-role ts-unit-structure))
         (passive (loop for unit in cxn-units-without-role
                        when (eql '+ (unit-feature-value (cdr unit) 'passive))
                        return t))
         (contributing-unit (make-propbank-contributing-unit core-units-with-role gold-frame gram-category 'fee))
         (schema (make-cxn-schema core-units-with-role cxn-units-with-role :core-roles :passive? passive))
         (cxn-name (intern (upcase (format nil "~a+~a-cxn" gram-category (length cxn-units-without-role)))))
         (equivalent-cxn (find-equivalent-cxn schema
                                              (syn-classes (append cxn-units-with-role
                                                                   cxn-units-without-role))
                                              cxn-inventory)))
    
    (if equivalent-cxn
      
      ;; Grammatical construction already exists
      ;;----------------------------------------
      (progn
        ;;1) Increase its frequency
        (incf (attr-val equivalent-cxn :score))
        ;;2) Check if there was already a link in the categorial network between the lex-category and the gram-category:
        (if (link-exists-p lex-category (attr-val equivalent-cxn :gram-category) cxn-inventory)
          ;;a) If yes, increase edge weight
          (progn
            (incf-link-weight lex-category (attr-val equivalent-cxn :gram-category) cxn-inventory :delta 1.0 :link-type nil)
            (incf-link-weight lex-category (attr-val equivalent-cxn :gram-category) cxn-inventory :delta 1.0 :link-type 'lex-gram))
          ;;b) Otherwise, add new connection (weight 1.0)
          (progn
            (if (not *check-links*) ;; proceed as normal
              (progn
                (add-link lex-category (attr-val equivalent-cxn :gram-category) cxn-inventory :weight 1.0 :link-type nil
                          :recompute-transitive-closure nil)
                (add-link lex-category (attr-val equivalent-cxn :gram-category) cxn-inventory :weight 1.0 :link-type 'lex-gram
                          :recompute-transitive-closure nil))
              (progn ;; here we are finding the ones in the test/dev set
                ;; get the similarities, only consider the ones with a similarity above a certain threshold (maybe not necessary?)
                (let* ((node-similarities (fcg::get-similar-lex-categories lex-category (fcg::graph (categorial-network cxn-inventory))))
                       (node-similarities-high (loop for sim in node-similarities
                                                     when (> (cdr sim) 0.2)
                                                       collect sim)))

                  ;; when the highest similarity is between 0.2 and 0.4 (second because first one is the node itself)
                  (when (and (> (cdr (second node-similarities)) 0.2)
                             (< (cdr (second node-similarities)) 0.4))
                    ;; loop for all similar nodes in the nodes above a certain threshold. 
                    (loop for s in node-similarities-high
                          ;; when length of sentence is above a certain threshold and you find the gram-cat for the cxn that needs to apply in the neighbors of the node with a certain similarity, THEN do comprehend
                          when (and (find (graph-utils::lookup-node (fcg::graph (categorial-network cxn-inventory))
                                                                    (attr-val equivalent-cxn :gram-category))
                                          (graph-utils::my-neighbors (fcg::graph (categorial-network cxn-inventory))
                                                                     (graph-utils::lookup-node (fcg::graph (categorial-network cxn-inventory)) (first s))))
                                    (< (length (split-sequence::split-sequence #\Space (sentence-string propbank-sentence))) 20))
                            ;; comprehend and get the cats of the applied grammatical cxns and the neighbors of the lexical cxn for which link needs to be created
                            do (let* ((cipn (second (multiple-value-list (comprehend-and-extract-frames propbank-sentence :cxn-inventory cxn-inventory :silent t :timeout nil))))
                                      (applied-gram-cats (loop for cxn in (applied-constructions cipn)
                                                               when (eq (cdr (attr-val :label cxn)) 'argument-structure-cxn )
                                                                 collect (attr-val cxn :gram-category)))
                                      (neighbors-of-lex (graph-utils:neighbors (fcg::graph (categorial-network cxn-inventory)) lex-category)))
                                 ;; check the neighbors of the lexical for which link needs to be created and check whether one of its neighbors is in the applied grammatical cxns
                                 ;; if not, then collect
                                 (when (not (loop for n in neighbors-of-lex
                                                  for node = (graph-utils:lookup-node
                                                              (fcg::graph (categorial-network cxn-inventory)) (cdr n))
                                                  when (find node applied-gram-cats)
                                                    collect node))
                                   (push (cons propbank-sentence cipn) *candidate-sentences*)
                                   (format t "---------------------------------- ~%" propbank-sentence)
                                   (format t "New link needed between: ~a and ~a~%" lex-category (attr-val equivalent-cxn :gram-category))
                                   (format t "Sentence: ~a~%" propbank-sentence)
                                   (format t "---------------------------------- ~%" propbank-sentence))))))))))
        ;;3) Return gram-category
        (attr-val equivalent-cxn :gram-category))

      ;; Else: Create a new grammatical category for the observed pattern + add category and link to the categorial network
      ;;--------------------------------------------------------------------------------------------------------------------
      (when (and cxn-units-with-role (v-lemma core-units-with-role))
        
        (add-category gram-category cxn-inventory :recompute-transitive-closure nil)
        (add-link lex-category gram-category cxn-inventory :weight 1.0 :link-type nil :recompute-transitive-closure nil)
        (add-link lex-category gram-category cxn-inventory :weight 1.0 :link-type 'lex-gram :recompute-transitive-closure nil)
        
        (eval `(def-fcg-cxn ,cxn-name
                            (,contributing-unit
                             <-
                             ,@cxn-units-with-role
                             ,@cxn-units-without-role
                             )
                            :disable-automatic-footprints t
                            :attributes (:schema ,schema
                                         :lemma nil
                                         :label argument-structure-cxn
                                         :score 1
                                         :gram-category ,gram-category)
                            :description ,(sentence-string propbank-sentence)
                            :cxn-inventory ,cxn-inventory))
        gram-category))))


#|(defun add-grammatical-cxn (gold-frame core-units-with-role cxn-inventory propbank-sentence lex-category)
  "Learns a grammatical construction capturing all core roles and adds
a grammatical category to the categorial network. Returns the
grammatical category."
  
  (let* ((ts-unit-structure (ts-unit-structure propbank-sentence cxn-inventory))
         (gram-category (make-gram-category core-units-with-role))
         (cxn-units-with-role (loop for unit in core-units-with-role
                                    collect (make-propbank-conditional-unit-with-role unit gram-category 'fee)))
         (cxn-units-without-role (make-propbank-conditional-units-without-role core-units-with-role
                                                                               cxn-units-with-role ts-unit-structure))
         (passive (loop for unit in cxn-units-without-role
                        when (eql '+ (unit-feature-value (cdr unit) 'passive))
                        return t))
         (contributing-unit (make-propbank-contributing-unit core-units-with-role gold-frame gram-category 'fee))
         (schema (make-cxn-schema core-units-with-role cxn-units-with-role :core-roles :passive? passive))
         (cxn-name (intern (upcase (format nil "~a+~a-cxn" gram-category (length cxn-units-without-role)))))
         (equivalent-cxn (find-equivalent-cxn schema
                                              (syn-classes (append cxn-units-with-role
                                                                   cxn-units-without-role))
                                              cxn-inventory)))
    
    (if equivalent-cxn
      
      ;; Grammatical construction already exists
      ;;----------------------------------------
      (progn
        ;;1) Increase its frequency
        (incf (attr-val equivalent-cxn :score))
        ;;2) Check if there was already a link in the categorial network between the lex-category and the gram-category:
        (if (link-exists-p lex-category (attr-val equivalent-cxn :gram-category) cxn-inventory)
          ;;a) If yes, increase edge weight
          (progn
            (incf-link-weight lex-category (attr-val equivalent-cxn :gram-category) cxn-inventory :delta 1.0 :link-type nil)
            (incf-link-weight lex-category (attr-val equivalent-cxn :gram-category) cxn-inventory :delta 1.0 :link-type 'lex-gram))
          ;;b) Otherwise, add new connection (weight 1.0)
          )
        ;;3) Return gram-category
        (attr-val equivalent-cxn :gram-category))

      ;; Else: Create a new grammatical category for the observed pattern + add category and link to the categorial network
      ;;--------------------------------------------------------------------------------------------------------------------
      (when (and cxn-units-with-role (v-lemma core-units-with-role))
        
        (add-category gram-category cxn-inventory :recompute-transitive-closure nil)
        (add-link lex-category gram-category cxn-inventory :weight 1.0 :link-type nil :recompute-transitive-closure nil)
        (add-link lex-category gram-category cxn-inventory :weight 1.0 :link-type 'lex-gram :recompute-transitive-closure nil)
        
        (eval `(def-fcg-cxn ,cxn-name
                            (,contributing-unit
                             <-
                             ,@cxn-units-with-role
                             ,@cxn-units-without-role
                             )
                            :disable-automatic-footprints t
                            :attributes (:schema ,schema
                                         :lemma nil
                                         :label argument-structure-cxn
                                         :score 1
                                         :gram-category ,gram-category)
                            :description ,(sentence-string propbank-sentence)
                            :cxn-inventory ,cxn-inventory))
        gram-category))))|#



(progn
  
  (defparameter *node-similarities-ht* (make-hash-table))
  
  (defun get-similar-lex-categories (lex-category graph)
    (let ((similar-lex-categories (gethash lex-category *node-similarities-ht*)))
      (when (not similar-lex-categories)
        (setf similar-lex-categories (graph-utils::my-similar-nodes-weighted-cosine-same-node-type lex-category graph))
        (setf (gethash lex-category *node-similarities-ht*) similar-lex-categories))
      similar-lex-categories))

  (defparameter *candidate-sentences* nil)
  (defparameter *check-links* nil)
  ;(defparameter *nr-check-links* (length (append (train-split *ontonotes-annotations*) (train-split *ewt-annotations*))))
  (defparameter *nr-check-links* (length (append (train-split *ontonotes-annotations*) (train-split *ewt-annotations*))))
  
  (defparameter *propbank-test* nil)
  
  (learn-propbank-grammar
   (append (train-split *ewt-annotations*)
           (train-split *ontonotes-annotations*)
           (dev-split *ewt-annotations*)
           (dev-split *ontonotes-annotations*)
           (test-split *ewt-annotations*)
           (test-split *ontonotes-annotations*)
           )
   :excluded-rolesets '("be.01" "be.02" "be.03"
                        "do.01" "do.02" "do.04" "do.11" "do.12"
                        "have.01" "have.02" "have.03" "have.04" "have.05" "have.06" "have.07" "have.08" "have.09" "have.10" "have.11"
                        "get.03" "get.06" "get.24")
   :selected-rolesets nil
   :cxn-inventory '*propbank-test*
   :fcg-configuration *training-configuration*)
  )

(activate-monitor trace-fcg)
(preprocessing-and-configs *propbank-test* :step-3 :make-role-embeddings nil)
(comprehend-all "Try googling it for more info :-RRB-" :cxn-inventory *propbank-test* :n 1)
(deactivate-monitor trace-fcg)

;(pprint (graph-utils::neighbors (fcg::graph (categorial-network *propbank-test*)) 'propbank-grammar::SERVE\(NN\)-1283 :return-ids? nil :edge-type 'lex-gram))


(cl-store:store *candidate-sentences*
   (babel-pathname :directory '(".tmp")
                   :name "possible-examples"
                   :type "store"))



New link needed between: COEXIST(V)-46 and ARG1(NP)+V(V)-79453
Sentence: <Sentence: "Here , eastern and western cultures have gathered , and the new and the old coexist .">

New link needed between: GOOGLE(V)-30 and V(V)+ARG1(NP)-42155
Sentence: <Sentence: "Try googling it for more info :-RRB-">

New link needed between: GOOGLE(V)-30 and V(V)+ARG1(NP)-42155
Sentence: <Sentence: "Try googling it or type it into youtube you might get lucky .">

New link needed between: UNLOCK(V)-15 and ARG0(NP)+V(V)+ARG1(NP)-194630
Sentence: <Sentence: "So how much easier will that make it for you to unlock this case , do you think ?"> 

New link needed between: NAIL(V)-79 and ARG0(NP)+V(V)+ARG1(NP)-194676
Sentence: <Sentence: "You have to trust people and nail those attempts to control and hypercontrol this case .">

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun comprehend-and-evaluate (list-of-propbank-sentences cxn-inventory &key (timeout 60) (core-roles-only t)
                                                           (selected-rolesets nil) (excluded-rolesets nil)
                                                           (include-word-sense t) (include-timed-out-sentences t)
                                                           (include-sentences-with-incomplete-role-constituent-mapping t)
                                                           (silent nil))
  (let ((output-file (babel-pathname :directory '(".tmp")
                                     :name "results"
                                     :type "store")))

    (evaluate-propbank-corpus list-of-propbank-sentences cxn-inventory :output-file output-file :timeout timeout :silent silent)

    (let ((predictions (cl-store:restore (babel-pathname :directory '(".tmp")
                                                :name "results"
                                                :type "store"))))
    
      (evaluate-predictions predictions
                            :core-roles-only core-roles-only
                            :selected-rolesets selected-rolesets
                            :excluded-rolesets excluded-rolesets
                            :include-word-sense include-word-sense 
                            :include-timed-out-sentences include-timed-out-sentences
                            :include-sentences-with-incomplete-role-constituent-mapping include-sentences-with-incomplete-role-constituent-mapping))))
 
(defun find-non-covered-lemmas (list-of-propbank-sentences-train list-of-propbank-sentences-dev)
  (let* ((lemmas-train (loop for sentence in list-of-propbank-sentences-train
                             for annotation = (propbank-frames sentence)
                             for frames-strings-train = (loop for frame in annotation
                                                              for frame-roles = (frame-roles frame)
                                                              for v = (loop for role in frame-roles
                                                                            when (string= (role-type role) "V")
                                                                              return role)
                                                              when v
                                                                collect (role-string v))
                             for lemmas = (loop for frame-string in frames-strings-train
                                                for lemma = (loop for node in (syntactic-analysis sentence)
                                                                  for string = (node-string node)
                                                                  when (string= string frame-string)
                                                                    collect (cdr (assoc :lemma node)))
                                                append lemma)
                             append lemmas))
         (lemmas-dev (loop for sentence in list-of-propbank-sentences-dev
                           for annotation = (propbank-frames sentence)
                           for frames-strings-train = (loop for frame in annotation
                                                            for frame-roles = (frame-roles frame)
                                                            for v = (loop for role in frame-roles
                                                                          when (string= (role-type role) "V")
                                                                            return role)
                                                            when v
                                                              collect (role-string v))
                           for lemmas = (loop for frame-string in frames-strings-train
                                              for lemma = (loop for node in (syntactic-analysis sentence)
                                                                for string = (node-string node)
                                                                when (string= string frame-string)
                                                                  collect (cdr (assoc :lemma node)))
                                              append lemma)
                           append lemmas))
        (clean-lemmas-train  (remove-duplicates lemmas-train :test #'string=))
        (clean-lemmas-dev  (remove-duplicates lemmas-dev :test #'string=))
        (diff-lemmas (set-difference   clean-lemmas-dev clean-lemmas-train :test #'string=))
         )

    
    ;(set-difference (remove-duplicates (flatten lemmas-train)) (remove-duplicates (flatten lemmas-dev)) :test #'string=)
    
    
    (list clean-lemmas-train clean-lemmas-dev diff-lemmas)))

(time
 (defparameter *non-covered-test* (find-non-covered-lemmas
                                   (append (train-split *ontonotes-annotations*)
                                           (train-split *ewt-annotations*))
                                   (append (test-split *ontonotes-annotations*)
                                           (test-split *ewt-annotations*)))))

#|
  
(defparameter *lemmas*
  (find-non-covered-lemmas (train-split *ewt-annotations*) (dev-split *ewt-annotations*)))

(defparameter *non-covered-lemmas*
  (loop for lemma in (second *lemmas*)
        when (not (find lemma (first *lemmas*) :test #'string=))
          collect lemma))

(defparameter *lemmas-large*
  (find-non-covered-lemmas (append (train-split *ontonotes-annotations*) (train-split *ewt-annotations*))
                           (append (dev-split *ontonotes-annotations*) (dev-split *ewt-annotations*))))

(defparameter *non-covered-lemmas-large*
  (loop for lemma in (second *lemmas-large*)
        when (not (find lemma (first *lemmas-large*) :test #'string=))
          collect lemma))


(defparameter *final-lemma-for-step-1*
  (remove-duplicates (remove nil *non-covered-lemmas-large*) :test #'string=))

(print *final-lemma-for-step-1*)



(fcg::closest-token "enchanting" (first *lemmas-large*))

(cosine-similarity (second (nlp-tools:get-word-embedding "captivate"))
                   (second (nlp-tools:get-word-embedding "enchant")))

(loop for lemma in (subseq (first *lemmas-large*) 0 1000)
      when lemma
        do (let ((sim (cosine-similarity (second (nlp-tools:get-word-embedding lemma))
                                         (second (nlp-tools:get-word-embedding "enchanting")))))
             (when (> sim 0.7)
               (print (cons lemma sim)))))
(length (first *lemmas-large*))


(let ((lijst (list 0 50000 100000 150000 200000 250000 300000 350000 (length (first *lemmas-large*)))))
  (loop for i from 0 to (length lijst)
        do (let ((embeddings (nlp-tools:get-word-embeddings (remove nil (subseq (first *lemmas-large*) (nth i lijst) (nth (+ i 1) lijst))))))
             (print (length embeddings))
             (loop for embedding in embeddings
                   for sim = (cosine-similarity (second embedding)
                                                (second (nlp-tools:get-word-embedding "enchanting")))
                   when (> sim 0.7)
                     do (print (cons (first embedding) sim))))))

(setf nlp-tools::*penelope-host* "http://127.0.0.1:5001")

(find "captivate" (first *lemmas-large*) :test #'string=)




|#



(defun evaluate-propbank-corpus (list-of-propbank-sentences cxn-inventory &key (output-file nil) (timeout 60) (silent t))
  "Runs FCG comprehend on a corpus of sentences and stores the solutions to an external file."

  (loop for sentence in list-of-propbank-sentences
        for sentence-number from 1
        do (let* ((cipn (second (multiple-value-list (comprehend-and-extract-frames sentence :cxn-inventory cxn-inventory :silent silent :timeout timeout)))))
             (if (not (eql cipn 'time-out))
               (let* ((cxns (loop for cxn in (applied-constructions cipn)
                                  for label = (cdr (assoc :label (attributes cxn)))
                                  if (equal label 'propbank-grammar::lexical-cxn)
                                    collect cxn into lex-cxns
                                  else if (equal label 'propbank-grammar::word-sense-cxn)
                                         collect cxn into sense-cxns
                                    else
                                         collect cxn into gram-cxns
                                  finally (return (list lex-cxns sense-cxns gram-cxns))))
                      (lex-cxns (first cxns))
                      (sense-cxns (second cxns))
                      (gram-cxns (third cxns))
                      (annotation (propbank-frames sentence))
                      (lexical-FEEs-gold (loop for frame in annotation
                                               for frame-roles = (frame-roles frame)
                                               for v = (loop for role in frame-roles
                                                             when (eq (role-type role) "V")
                                                               collect role)
                                               when v
                                                 collect (role-string v)))

                      (easy-transitive (loop for frame in annotation
                                             for frame-roles = (frame-roles frame)
                                             for v = (loop for role in frame-roles
                                                           when (string= (role-type role) "V")
                                                             collect role)
                                             for arg0 = (loop for role in frame-roles
                                                              when (string= (role-type role) "ARG0")
                                                                collect role)
                                             for arg1 = (loop for role in frame-roles
                                                              when (string= (role-type role) "ARG1")
                                                                collect role)
                                             if (and arg0 arg1)
                                               collect (list frame arg0 arg1)
                                                 ))
                      (frames (extract-frames (car-resulting-cfs (cipn-car cipn))))
                      (transitive-not-extracted? (loop for frame in (frames frames)
                                                       for frame-roles = (frame-elements frame)
                                                       for v = (find (downcase (symbol-name (frame-name frame)))
                                                                     (mapcar #'(lambda (x) (frame-name (first x)))
                                                                             easy-transitive)
                                                                     :test #'string=)
                                                                    
                                                       for arg0 = (loop for role in frame-roles
                                                                        when (eq (fe-role role) 'ARG0)
                                                                          collect role)
                                                       for arg1 = (loop for role in frame-roles
                                                                        when (eq (fe-role role) 'ARG1)
                                                                          collect role)
                                                       when (and v
                                                                 (or (not (and arg0 arg1))
                                                                     (not frame-roles)))
                                                         collect (list frame arg0 arg1))))
                 (format t "~%----------------~%")
                 (format t "~%Sentence ~a: ~a" sentence-number (sentence-string sentence))

                 (when (and easy-transitive transitive-not-extracted?)
               
                    
                   (format t "~%----------------~%")
                 
                   (format t "~%----------------~%")
                   (format t "STEP 3: ")
                   (format t "~%-------~%")
                 
                   (format t "Lexical cxns: ~a~%" lex-cxns)
                   (format t "Sense cxns: ~a~%" sense-cxns)
                   (format t "Gramm cxns: ~a~%" gram-cxns)

                   (format t "Extracted frames: ~a~%" frames)
                   (format t "Not extracted: ~a~%" transitive-not-extracted?)

                   (format t "Transitive: ~a~%" easy-transitive)
                 
                   (format t "~%----------------~%")))))))

;(pprint (graph-utils::neighbors (fcg::graph (categorial-network *propbank-ontonotes-ewt-train-corpus-core-roles*)) 'propbank-grammar::HOPE\(V\)-1 ))
                     

;(pprint (find-schemata-for-lex-item  'propbank-grammar::HOPE\(V\)-1 *propbank-ontonotes-ewt-train-corpus-core-roles*))



;(defparameter *dev-set* (append (dev-split *ewt-annotations*) (dev-split *ontonotes-annotations*)))

;(evaluate-propbank-corpus (subseq *dev-set* 13 15) *propbank-ontonotes-ewt-train-corpus-core-roles*)

;(activate-monitor trace-fcg)
;(comprehend-and-extract-frames (nth 13 *dev-set*) :cxn-inventory *propbank-ontonotes-ewt-train-corpus-core-roles* )

;(categorial-network *propbank-ontonotes-ewt-train-corpus-all-roles*)

(defun evaluate-predictions (predictions &key (core-roles-only t) (selected-rolesets nil) (include-word-sense t) (include-timed-out-sentences t) (excluded-rolesets nil) (include-sentences-with-incomplete-role-constituent-mapping t))
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
        finally (let ((evaluation-result `((:precision . ,(compute-precision number-of-correct-predictions number-of-grammar-predictions))
                                           (:recall . ,(compute-recall number-of-correct-predictions number-of-gold-standard-predictions))
                                           (:f1-score . ,(compute-f1-score number-of-correct-predictions number-of-grammar-predictions number-of-gold-standard-predictions))
                                           (:nr-of-correct-predictions . ,number-of-correct-predictions)
                                           (:nr-of-predictions . ,number-of-grammar-predictions)
                                           (:nr-of-gold-standard-predictions . ,number-of-gold-standard-predictions))))
                  (format t "~%~%~%############## EVALUATION RESULTS ##############~%")
                  (format t "~a" evaluation-result)
                  (return evaluation-result))))




      
