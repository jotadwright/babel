(in-package :fcg)

;; (ql:quickload '(:fcg :amr))
;; (activate-monitor trace-fcg)

(load (babel-pathname :directory '("sharing" "fcg-learn")
                      :name "learn"
                      :type "lisp"))

;;#########################################################################
;; Examples for testing
;;#########################################################################

(def-fcg-constructions sandbox-grammar
  :feature-types ((form set-of-predicates :handle-regex-sequences)
                  (meaning set-of-predicates)
                  (form-args sequence)
                  (meaning-args sequence)
                  (subunits set)
                  (footprints set))
  :hashed t
  :fcg-configurations (;; to activate heuristic search
                       (:construction-inventory-processor-mode . :heuristic-search) ;; use dedicated cip
                       (:node-expansion-mode . :full-expansion) ;; always fully expands node immediately
                       (:cxn-supplier-mode . :all-cxns) 
                       ;; for using heuristics
                       (:search-algorithm . :best-first) ;; :depth-first, :breadth-first
                       (:heuristics :nr-of-applied-cxns :nr-of-units-matched) ;; list of heuristic functions (modes of #'apply-heuristic)
                       (:heuristic-value-mode . :sum-heuristics-and-parent) ;; how to use results of heuristic functions for scoring a node
                     ;  (:hash-mode . :hash-sequence-meaning)
                       (:de-render-mode . :de-render-sequence)
                       (:render-mode . :render-sequences)
                       (:category-linking-mode . :neighbours)
                       (:parse-goal-tests :no-applicable-cxns :connected-semantic-network :no-sequence-in-root))
  :visualization-configurations ((:show-constructional-dependencies . nil)
                                 (:show-categorial-network . t)))



;;++++++++++++++++++++++++++++++++++++++++++++
;; Substitution
;;++++++++++++++++++++++++++++++++++++++++++++
 
(defparameter *what-color-is-the-cube* '((:form . "what color is the cube?")
                                         (:meaning . ((get-context ?context-1)
                                                      (filter ?set-1 ?context-1 ?shape-1)
                                                      (bind shape-category ?shape-1 cube)
                                                      (unique ?object-1 ?set-1)
                                                      (query ?target-1 ?object-1 ?attribute-1)
                                                      (bind attribute-category ?attribute-1 color)))))

(defparameter *what-size-is-the-cube* '((:form . "what size is the cube?")
                                        (:meaning . ((get-context ?context-2)
                                                     (filter ?set-2 ?context-2 ?shape-2)
                                                     (bind shape-category ?shape-2 cube)
                                                     (unique ?object-2 ?set-2)
                                                     (query ?target-2 ?object-2 ?attribute-2)
                                                     (bind attribute-category ?attribute-2 size)))))

(let ((*fcg-constructions* (make-sandbox-grammar-cxns))
      (holophrastic-what-color-is-the-cube (induce-cxns *what-color-is-the-cube* nil)))

  (induce-cxns *what-size-is-the-cube* holophrastic-what-color-is-the-cube)

  (comprehend-all (form-string *what-color-is-the-cube*))
  (comprehend-all (form-string *what-size-is-the-cube*)))

;;++++++++++++++++++++++++++++++++++++++++++++
;; Deletion + addition
;;++++++++++++++++++++++++++++++++++++++++++++

(defparameter *how-many-cubes-are-there* '((:form . "how many cubes are there?")
                                           (:meaning . ((get-context ?context-5)
                                                        (filter ?set-5 ?context-5 ?shape-5)
                                                        (bind shape-category ?shape-5 cube)
                                                        (count ?number-5 ?set-5)))))

(defparameter *how-many-red-cubes-are-there* '((:form . "how many red cubes are there?")
                                               (:meaning . ((get-context ?context-6)
                                                            (filter ?set-6 ?context-6 ?color-6)
                                                            (bind shape-category ?color-6 red)
                                                            (filter ?set-7 ?set-6 ?shape-6)
                                                            (bind shape-category ?shape-6 cube)
                                                            (count ?number-6 ?set-7)))))

;;---DEBUGGING SEQUENCES ------------------------------------------------
(progn
  (setf *fcg-constructions* (make-sandbox-grammar-cxns))
  (induce-cxns *what-size-is-the-cube* (induce-cxns *how-many-red-cubes-are-there* nil))
  (comprehend (form-string *what-size-is-the-cube*)))
;;-----------------------------------------------------------------------


;; Deletion
;;--------------

(let ((*fcg-constructions* (make-sandbox-grammar-cxns))
      (holophrastic-how-many-red-cubes-are-there (induce-cxns *how-many-red-cubes-are-there* nil)))

  (induce-cxns *how-many-cubes-are-there* holophrastic-how-many-red-cubes-are-there)

  (comprehend (form *how-many-cubes-are-there*)) ;;TO DO: Always add holophrastic cxn
  (comprehend-all (form-string *how-many-red-cubes-are-there*)))


;; Addition
;;--------------

(let ((*fcg-constructions* (make-sandbox-grammar-cxns))
      (holophrastic-how-many-cubes-are-there (induce-cxns *how-many-cubes-are-there* nil)))

  (induce-cxns *how-many-red-cubes-are-there* holophrastic-how-many-cubes-are-there)

  (comprehend (form-string *how-many-cubes-are-there*))
  (comprehend-all (form-string *how-many-red-cubes-are-there*)))


;;++++++++++++++++++++++++++++++++++++++++++++
;; Filler to slot repair
;;++++++++++++++++++++++++++++++++++++++++++++

(defparameter *what-size-is-the-sphere* '((:form . "what size is the sphere?")
                                        (:meaning . ((get-context ?context-3)
                                                     (filter ?set-3 ?context-3 ?shape-3)
                                                     (bind shape-category ?shape-3 sphere)
                                                     (unique ?object-3 ?set-3)
                                                     (query ?target-3 ?object-3 ?attribute-3)
                                                     (bind attribute-category ?attribute-3 size)))))

(defparameter *what-size-is-the-block* '((:form . "what size is the block?")
                                         (:meaning . ((get-context ?context-4)
                                                      (filter ?set-4 ?context-4 ?shape-4)
                                                      (bind shape-category ?shape-4 cube)
                                                      (unique ?object-4 ?set-4)
                                                      (query ?target-4 ?object-4 ?attribute-4)
                                                      (bind attribute-category ?attribute-4 size)))))

(defparameter *what-color-is-the-block* '((:form . "what color is the block?")
                                         (:meaning . ((get-context ?context-5)
                                                      (filter ?set-5 ?context-5 ?shape-5)
                                                      (bind shape-category ?shape-5 cube)
                                                      (unique ?object-5 ?set-5)
                                                      (query ?target-5 ?object-5 ?attribute-5)
                                                      (bind attribute-category ?attribute-5 color)))))


(let ((*fcg-constructions* (make-sandbox-grammar-cxns))
      (holophrastic-what-size-is-the-sphere (induce-cxns *what-size-is-the-sphere* nil))
      (block-cxn nil))

  (induce-cxns *what-size-is-the-block* holophrastic-what-size-is-the-sphere)
  (comprehend (form-string *what-size-is-the-block*))

  (setf block-cxn (find-cxn "block" *fcg-constructions*
                            :key #'(lambda (cxn) (second (first (attr-val cxn :sequence)))) :test #'string=))

  (induce-cxns *what-color-is-the-block* block-cxn)

  (comprehend (form-string *what-color-is-the-block*)))



;;++++++++++++++++++++++++++++++++++++++++++++
;; AMR examples
;;++++++++++++++++++++++++++++++++++++++++++++

(progn
(defparameter *amr-1* `((:form . "Hotel rooms available as of this weekend")
                        (:meaning . ,(amr:penman->predicates
                                      '(a / available-02
                                          :ARG2 (r / room
                                                   :mod (h / hotel))
                                          :time (a2 / as-of
                                                    :op1 (w / weekend
                                                            :mod (t / this))))
                                      :variablify? t))))

(defparameter *amr-2* `((:form . "Hotel rooms available as of tomorrow")
                        (:meaning .  ,(amr:penman->predicates 
                                       '(a3 / available-02
                                            :ARG2 (r2 / room
                                                      :mod (h2 / hotel))
                                            :time (a4 / as-of
                                                      :op1 (t2 / tomorrow)))
                                       :variablify? t))))

(defparameter *amr-3* `((:form . "Gonna be winter again tomorrow and Sunday")
                        (:meaning .  ,(amr:penman->predicates 
                                       '(w / winter
                                           :time (a5 / and
                                                    :op1 (t3 / tomorrow)
                                                    :op2 (d / date-entity
                                                            :weekday (s / sunday)))
                                           :mod (a6 / again))
                                       :variablify? t))))

(defparameter *amr-4* `((:form . "Gonna be winter again Saturday and Sunday")
                        (:meaning .  ,(amr:penman->predicates 
                                       '(w2 / winter
                                           :time (a7 / and
                                                    :op1 (d2 / date-entity
                                                            :weekday (s2 / saturday))
                                                    :op2 (d3 / date-entity
                                                            :weekday (s3 / sunday)))
                                           :mod (a7 / again))
                                       :variablify? t))))

(defparameter *amr-5* `((:form . "The man will be buried at a funeral service tomorrow")
                        (:meaning .  ,(amr:penman->predicates 
                                       '(b / bury-01
                                           :ARG1 (m / man)
                                           :time (s4 / service-06
                                                    :ARG1 (f / funeral)
                                                    :time (t4 / tomorrow)))
                                       :variablify? t))))

(defparameter *amr-6* `((:form . "The man will be buried at a funeral service")
                        (:meaning .  ,(amr:penman->predicates 
                                       '(b2 / bury-01
                                           :ARG1 (m2 / man)
                                           :time (s5 / service-06
                                                    :ARG1 (f2 / funeral)))
                                       :variablify? t))))
)

;; Learning 'omorrow-cxn'
(let ((*fcg-constructions*  (make-sandbox-grammar-cxns))
      (holophrastic-amr-cxn-1 (induce-cxns *amr-1* nil))
      (holophrastic-amr-cxn-2 (induce-cxns *amr-2* nil))
      (holophrastic-amr-cxn-3 (induce-cxns *amr-3* nil))
      (holophrastic-amr-cxn-6 (induce-cxns *amr-6* nil))
      (ommorrow-cxn nil))

  (induce-cxns *amr-2* holophrastic-amr-cxn-1)

  (comprehend-all (form-string *amr-1*))
  (comprehend-all (form-string *amr-2*))

  (setf ommorrow-cxn (find-cxn "omorrow" *fcg-constructions*
                               :key #'(lambda (cxn) (second (first (attr-val cxn :sequence)))) :test #'string=))

  (induce-cxns *amr-3* ommorrow-cxn)
  (induce-cxns *amr-4* holophrastic-amr-cxn-3)

  (comprehend-all (form-string *amr-4*))
  (comprehend-all (form-string *amr-3*))
  
  (induce-cxns *amr-5* holophrastic-amr-cxn-6)

  (comprehend-all (form-string *amr-5*))
  (comprehend-all (form-string *amr-3*)))




;;++++++++++++++++++++++++++++++++++++++++++++
;; Simplified AMR example
;;++++++++++++++++++++++++++++++++++++++++++++

(defparameter *amr-simple-1* `((:form . " local ")
                               (:meaning . ((local-02 ?l-1)))))

(defparameter *amr-simple-2* `((:form . " global ")
                               (:meaning . ((global-02 ?g-1)))))


(progn
  (setf *fcg-constructions*  (make-sandbox-grammar-cxns))
  (defparameter *cxn-1* (induce-cxns *amr-simple-1* nil))
  (add-element (make-html *cxn-1*))
  (induce-cxns *amr-simple-2* *cxn-1*)
  (comprehend-all (form-string *amr-simple-1*)) ;; here construction doesn't apply
; (comprehend (form-string *amr-simple-2*))
  )







;;########################################################################
;; Stappenplan
;;------------------------------------------

;; Stap 1: 
; voorbeelden -> nadenken is dit de correcte generalisatie gegeven 2 voorbeelden
; -> beginnen van holophrases en meerdere voorbeelden
; exploreren wat er mogelijk is met pure generalisatie 
; zin + constructie



; -> idee strategies
