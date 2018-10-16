(in-package :frame-extractor)

(defun load-grammar ()
  (load (babel-pathname :directory '("applications" "semantic-frame-extractor")
                        :name "grammar"
                        :type "lisp")))

(defun load-frames ()
  (dolist (filename (cl-fad:list-directory (babel-pathname :directory '("applications" "semantic-frame-extractor" "lexical-units" "causation" ))  )) ;"causation"
    (ignore-errors (load filename))))


(def-frame cause-to-start-frame
           nil
           :parent-frame causation-frame)

(def-frame warning-frame
           nil
           :parent-frame statement-frame)

(def-fcg-constructions penelope-english
  :feature-types ((categories set)
                  (footprints set)
                  (dependents set)
                  (boundaries set-of-predicates)
                  (agreement sequence)
                  (args sequence)
                  (ev-args sequence)
                  (sem-class set)
                  (syn-roles set-of-predicates)
                  (form set-of-predicates)
                  (meaning set-of-predicates))
  :fcg-configurations ((:production-order hashed-meaning unhashed hashed-lex-id)
                       (:parse-order hashed-string hashed-lex-id unhashed)
                       (:hashed-labels hashed-string hashed-meaning hashed-lex-id)
                       (:de-render-mode .  :raw-dependency-translation) ;;:english-noun-chunks)
                       (:form-predicates first meets)
                       (:node-tests :check-duplicate :restrict-nr-of-nodes)
                       ;; Goal tests
                       (:parse-goal-tests :no-applicable-cxns)
                       (:production-goal-tests :no-applicable-cxns )
                       (:cxn-supplier-mode . :hashed-cxn-supplier)
                       (:shuffle-cxns-before-application . t)
                       (:hash-mode . :hash-string-meaning-lex-id)
                       ;; For guiding search:
                       (:node-expansion-mode . :expand-cip-node-with-hashed-cxns)
                       (:priority-mode . :depth-first)
                       (:queue-mode . :depth-first-avoid-duplicates)
                       (:max-search-depth . 100)
                       (:max-nr-of-nodes . 1000))
                  
  :visualization-configurations ((:show-wiki-links-in-predicate-networks . nil)
                                 (:show-constructional-dependencies . nil)
                                 (:hide-features ()) 
                                 (:with-search-debug-data . t))
  :hierarchy-features (dependents)
  :hashed t
                  
  (load-grammar)
  (load-frames)

  )
