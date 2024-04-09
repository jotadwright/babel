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
 
(defparameter *what-color-is-the-cube* '((:form . ((sequence "what color is the cube?" ?l1 ?r1)))
                                         (:meaning . ((get-context ?context-1)
                                                      (filter ?set-1 ?context-1 ?shape-1)
                                                      (bind shape-category ?shape-1 cube)
                                                      (unique ?object-1 ?set-1)
                                                      (query ?target-1 ?object-1 ?attribute-1)
                                                      (bind attribute-category ?attribute-1 color)))))

(defparameter *what-size-is-the-cube* '((:form . ((sequence "what size is the cube?" ?l2 ?r2)))
                                        (:meaning . ((get-context ?context-2)
                                                     (filter ?set-2 ?context-2 ?shape-2)
                                                     (bind shape-category ?shape-2 cube)
                                                     (unique ?object-2 ?set-2)
                                                     (query ?target-2 ?object-2 ?attribute-2)
                                                     (bind attribute-category ?attribute-2 size)))))

(let ((*fcg-constructions* (make-sandbox-grammar-cxns))
      (holophrastic-what-color-is-the-cube (induce-cxns *what-color-is-the-cube* nil :cxn-inventory *fcg-constructions*)))

  (induce-cxns *what-size-is-the-cube* holophrastic-what-color-is-the-cube :cxn-inventory *fcg-constructions*)

  (comprehend-all (form-string *what-color-is-the-cube*))
  (comprehend-all (form-string *what-size-is-the-cube*)))

;;++++++++++++++++++++++++++++++++++++++++++++
;; Deletion + addition
;;++++++++++++++++++++++++++++++++++++++++++++

(defparameter *how-many-cubes-are-there* '((:form . ((sequence "how many cubes are there?" ?l3 ?r3)))
                                           (:meaning . ((get-context ?context-5)
                                                        (filter ?set-5 ?context-5 ?shape-5)
                                                        (bind shape-category ?shape-5 cube)
                                                        (count ?number-5 ?set-5)))))

(defparameter *how-many-red-cubes-are-there* '((:form . ((sequence "how many red cubes are there?" ?l4 ?r4)))
                                               (:meaning . ((get-context ?context-6)
                                                            (filter ?set-6 ?context-6 ?color-6)
                                                            (bind color-category ?color-6 red)
                                                            (filter ?set-7 ?set-6 ?shape-6)
                                                            (bind shape-category ?shape-6 cube)
                                                            (count ?number-6 ?set-7)))))

;; Deletion
;;--------------

(let (;(*fcg-constructions* (make-sandbox-grammar-cxns))
      (holophrastic-how-many-red-cubes-are-there (induce-cxns *how-many-red-cubes-are-there* nil :cxn-inventory *fcg-constructions*)))

  (induce-cxns *how-many-cubes-are-there* holophrastic-how-many-red-cubes-are-there :cxn-inventory *fcg-constructions*)

  (comprehend-all (form-string *how-many-cubes-are-there*))
  (comprehend-all (form-string *how-many-red-cubes-are-there*)))


;; Addition
;;--------------

(let ((*fcg-constructions* (make-sandbox-grammar-cxns))
      (holophrastic-how-many-cubes-are-there (induce-cxns *how-many-cubes-are-there* nil :cxn-inventory *fcg-constructions*)))

  (induce-cxns *how-many-red-cubes-are-there* holophrastic-how-many-cubes-are-there :cxn-inventory *fcg-constructions*)

  (comprehend-all (form-string *how-many-cubes-are-there*))
  (comprehend-all (form-string *how-many-red-cubes-are-there*)))


;;++++++++++++++++++++++++++++++++++++++++++++
;; Deletion + Existing slot-cxn -> filler-cxn
;;++++++++++++++++++++++++++++++++++++++++++++

(defparameter *how-many-blue-cubes-are-there* '((:form . ((sequence "how many blue cubes are there?" ?l40 ?r40)))
                                                (:meaning . ((get-context ?context-60)
                                                             (filter ?set-60 ?context-60 ?color-60)
                                                             (bind color-category ?color-60 blue)
                                                             (filter ?set-70 ?set-60 ?shape-60)
                                                             (bind shape-category ?shape-60 cube)
                                                             (count ?number-60 ?set-70)))))

(defparameter *how-many-blue-spheres-are-there* '((:form . ((sequence "how many blue spheres are there?" ?l42 ?r42)))
                                                  (:meaning . ((get-context ?context-62)
                                                               (filter ?set-62 ?context-62 ?color-62)
                                                               (bind color-category ?color-62 blue)
                                                               (filter ?set-72 ?set-62 ?shape-62)
                                                               (bind shape-category ?shape-62 sphere)
                                                               (count ?number-62 ?set-72)))))



;;EXAMPLE 1 OF ANTI-UNIFYING CONSTRUCTIONS
;;----------------------------------------------------------------------------------------------------------
(let ((*fcg-constructions* (make-sandbox-grammar-cxns))
      holophrastic-how-many-red-cubes-are-there
      how-many-X-cubes-are-there-cxn
      blue-filler-cxn
      blue-spher-filler-cxn)
  
  ;;Learn a holophrastic cxn first
  (setf holophrastic-how-many-red-cubes-are-there (induce-cxns *how-many-red-cubes-are-there* nil :cxn-inventory *fcg-constructions*))

  ;;Learn a first slot cxn and one filler cxn for red
  (setf how-many-X-cubes-are-there-cxn
        (induce-cxns *how-many-cubes-are-there* holophrastic-how-many-red-cubes-are-there :cxn-inventory *fcg-constructions*))

  ;(comprehend-all (form-string *how-many-cubes-are-there*))
  ;(comprehend-all (form-string *how-many-red-cubes-are-there*))
  
  ;;Learn a filler cxn for blue
  (induce-cxns *how-many-blue-cubes-are-there* how-many-X-cubes-are-there-cxn :cxn-inventory *fcg-constructions*)

  ;(comprehend-all (form-string *how-many-blue-cubes-are-there*))

  ;;Learn a slot cxn for how-many-Xes-are-there and two filler cxns for blue-spher and cub
  (induce-cxns *how-many-blue-spheres-are-there* how-many-X-cubes-are-there-cxn :cxn-inventory *fcg-constructions*)

  ;(comprehend-all (form-string *how-many-blue-spheres-are-there*)))
  ;(comprehend-all (form-string *how-many-cubes-are-there*))

  ;;Learn a slot-and-filler cxn for blue and filler cxn for spher based on two filler cxns blue-spher-cxn and blue-cxn
  (setf blue-filler-cxn (find-cxn "blue " *fcg-constructions*
                                  :key #'(lambda (cxn) (second (first (attr-val cxn :sequence)))) :test #'string=))
  
  (setf blue-spher-filler-cxn (find-cxn "blue spher" *fcg-constructions*
                                        :key #'(lambda (cxn) (second (first (attr-val cxn :sequence)))) :test #'string=))

  (induce-cxns blue-spher-filler-cxn blue-filler-cxn :cxn-inventory *fcg-constructions*)

  (comprehend-all (form-string *how-many-blue-spheres-are-there*))

  )
#|
(setf *res* (anti-unify-sequences
             '((sequence "how many blue spheres are there?" ?l6 ?r6))
             '((sequence "how many " ?l7 ?r7)
               
               (sequence "cubes are there?" ?l8 ?r8)
               (sequence "_" ?r7 ?l8))))|#

;;----------------------------------------------------------------------------------------------------------
;;EXAMPLE 2 OF ANTI-UNIFYING CONSTRUCTIONS
;;----------------------------------------------------------------------------------------------------------

(defparameter *are-there-any-small-matte-cubes* '((:form . ((sequence "are there any small matte cubes?" ?l80 ?r80)))
                                                  (:meaning . ((get-context ?source-1) (filter ?target-1300 ?target-2 ?size-2) (bind material-category ?material-2 rubber) (filter ?target-1 ?source-1 ?shape-2) (bind shape-category ?shape-2 cube) (filter ?target-2 ?target-1 ?material-2) (bind size-category ?size-2 small) (exist ?target-23 ?target-1300)))))

(defparameter *are-there-any-small-red-spheres* '((:form . ((sequence "are there any small red spheres?" ?l81 ?r81)))
                                                     (:meaning . ((get-context ?source-10) (filter ?target-5764 ?target-20 ?size-20) (bind color-category ?color-16 red) (filter ?target-10 ?source-10 ?shape-40) (bind shape-category ?shape-40 sphere) (filter ?target-20 ?target-10 ?color-16) (bind size-category ?size-20 small) (exist ?target-230 ?target-5764)))))

(defparameter *are-there-any-small-cyan-spheres* '((:form . ((sequence "are there any small cyan spheres?" ?l85 ?r85)))
                                                  (:meaning . ((get-context ?source-20)
                                                               (filter ?target-20 ?source-20 ?shape-50)
                                                               (bind shape-category ?shape-50 sphere)
                                                               (filter ?target-30 ?target-20 ?color-19)
                                                               (bind color-category ?color-19 cyan)
                                                               (filter ?target-5768 ?target-30 ?size-30)
                                                               (bind size-category ?size-30 small)
                                                               (exist ?target-231 ?target-5768)))))

(defparameter *are-there-any-huge-red-things* '((:form . ((sequence "are there any huge red things?" ?l82 ?r82)))
                                                 (:meaning . ((get-context ?source-1) (filter ?target-111343 ?target-2 ?size-4) (bind color-category ?color-4 red) (filter ?target-1 ?source-1 ?shape-8) (bind shape-category ?shape-8 thing) (filter ?target-2 ?target-1 ?color-4) (bind size-category ?size-4 large) (exist ?target-23 ?target-111343)))))

(defparameter *are-there-any-huge-cyan-spheres* '((:form . ((sequence "are there any huge cyan spheres?" ?l84 ?r84)))
                                                  (:meaning . ((get-context ?source-25)
                                                               (filter ?target-25 ?source-25 ?shape-55)
                                                               (bind shape-category ?shape-55 sphere)
                                                               (filter ?target-35 ?target-25 ?color-24)
                                                               (bind color-category ?color-24 cyan)
                                                               (filter ?target-5773 ?target-35 ?size-35)
                                                               (bind size-category ?size-35 large)
                                                               (exist ?target-236 ?target-5773)))))


(let ((*fcg-constructions* (make-sandbox-grammar-cxns))
      holophrastic-are-there-any-small-matte-cubes
      are-there-any-small-Xes-cxn red-cxn
      holophrastic-are-there-any-small-red-spheres
      holophrastic-are-there-any-huge-cyan-spheres)
  
  (setf holophrastic-are-there-any-small-matte-cubes (induce-cxns *are-there-any-small-matte-cubes* nil :cxn-inventory *fcg-constructions*))

  (setf are-there-any-small-Xes-cxn
        (induce-cxns *are-there-any-small-red-spheres* holophrastic-are-there-any-small-matte-cubes :cxn-inventory *fcg-constructions*))

  ;(comprehend-all (form-string *are-there-any-small-red-spheres*))

  (setf holophrastic-are-there-any-small-red-spheres (find-cxn "are there any small red spheres?" *fcg-constructions*
                                  :key #'(lambda (cxn) (second (first (attr-val cxn :sequence)))) :test #'string=))

  (induce-cxns *are-there-any-small-cyan-spheres* holophrastic-are-there-any-small-red-spheres)

  ;(comprehend-all (form-string *are-there-any-small-cyan-spheres*))

  (setf red-cxn (find-cxn "red" *fcg-constructions*
                                  :key #'(lambda (cxn) (second (first (attr-val cxn :sequence)))) :test #'string=))
  
  (induce-cxns *are-there-any-huge-red-things* are-there-any-small-Xes-cxn)
  (induce-cxns *are-there-any-huge-red-things* red-cxn)

  (comprehend-all (form-string *are-there-any-huge-red-things*))
  
  (setf holophrastic-are-there-any-huge-red-things (find-cxn "are there any huge red things?" *fcg-constructions*
                                  :key #'(lambda (cxn) (second (first (attr-val cxn :sequence)))) :test #'string=))
  
  (induce-cxns *are-there-any-huge-cyan-spheres* holophrastic-are-there-any-huge-red-things)

  (comprehend-all (form-string *are-there-any-huge-cyan-spheres*))
  
  )





;;++++++++++++++++++++++++++++++++++++++++++++
;; Filler to slot repair
;;++++++++++++++++++++++++++++++++++++++++++++

(defparameter *what-size-is-the-sphere* '((:form . ((sequence "what size is the sphere?" ?l5 ?r5)))
                                        (:meaning . ((get-context ?context-3)
                                                     (filter ?set-3 ?context-3 ?shape-3)
                                                     (bind shape-category ?shape-3 sphere)
                                                     (unique ?object-3 ?set-3)
                                                     (query ?target-3 ?object-3 ?attribute-3)
                                                     (bind attribute-category ?attribute-3 size)))))

(defparameter *what-size-is-the-block* '((:form . ((sequence "what size is the block?" ?l6 ?r6)))
                                         (:meaning . ((get-context ?context-4)
                                                      (filter ?set-4 ?context-4 ?shape-4)
                                                      (bind shape-category ?shape-4 cube)
                                                      (unique ?object-4 ?set-4)
                                                      (query ?target-4 ?object-4 ?attribute-4)
                                                      (bind attribute-category ?attribute-4 size)))))

(defparameter *what-color-is-the-block* '((:form . ((sequence "what color is the block?" ?l7 ?r7)))
                                         (:meaning . ((get-context ?context-5)
                                                      (filter ?set-5 ?context-5 ?shape-5)
                                                      (bind shape-category ?shape-5 cube)
                                                      (unique ?object-5 ?set-5)
                                                      (query ?target-5 ?object-5 ?attribute-5)
                                                      (bind attribute-category ?attribute-5 color)))))

(defparameter *how-many-blocks-are-there* '((:form . ((sequence "how many blocks are there?" ?l41 ?r41)))
                                            (:meaning . ((get-context ?context-61)
                                                         (filter ?set-71 ?context-61 ?shape-61)
                                                         (bind shape-category ?shape-61 cube)
                                                         (count ?number-61 ?set-71)))))


(let ((*fcg-constructions* (make-sandbox-grammar-cxns))
      (holophrastic-what-size-is-the-sphere (induce-cxns *what-size-is-the-sphere* nil :cxn-inventory *fcg-constructions*))
      block-cxn what-color-is-the-X-cxn what-size-is-the-X-cxn how-many-Xs-are-there-cxn sphere-cxn)

  ;;leer op basis van holophrase:
  (setf what-size-is-the-X-cxn (induce-cxns *what-size-is-the-block* holophrastic-what-size-is-the-sphere :cxn-inventory *fcg-constructions*))
  ;(comprehend-all (form-string *what-size-is-the-block*))

  (setf block-cxn (find-cxn "block" *fcg-constructions*
                            :key #'(lambda (cxn) (second (first (attr-val cxn :sequence)))) :test #'string=))

  ;; leer op basis van filler cxn:
  (setf what-color-is-the-X-cxn (induce-cxns *what-color-is-the-block* block-cxn :cxn-inventory *fcg-constructions*))
  ;(comprehend-all (form-string *what-color-is-the-block*))

  (induce-cxns *how-many-blocks-are-there* block-cxn :cxn-inventory *fcg-constructions*)
  ;(comprehend-all (form-string *how-many-blocks-are-there*))


  ;;TO DO: anti-unify two constructions
  ;;leer op basis van slot cxn
  (induce-cxns what-size-is-the-X-cxn what-color-is-the-X-cxn) 

  (comprehend-all (form-string *what-size-is-the-block*))
  
  )



;;++++++++++++++++++++++++++++++++++++++++++++
;; AMR examples
;;++++++++++++++++++++++++++++++++++++++++++++

(progn
(defparameter *amr-1* `((:form . ((sequence "Hotel rooms available as of this weekend" ?l8 ?r8)))
                        (:meaning . ,(amr:penman->predicates
                                      '(a / available-02
                                          :ARG2 (r / room
                                                   :mod (h / hotel))
                                          :time (a2 / as-of
                                                    :op1 (w / weekend
                                                            :mod (t / this))))
                                      :variablify? t))))

(defparameter *amr-2* `((:form . ((sequence "Hotel rooms available as of tomorrow" ?l9 ?r9)))
                        (:meaning .  ,(amr:penman->predicates 
                                       '(a3 / available-02
                                            :ARG2 (r2 / room
                                                      :mod (h2 / hotel))
                                            :time (a4 / as-of
                                                      :op1 (t2 / tomorrow)))
                                       :variablify? t))))

(defparameter *amr-3* `((:form . ((sequence "Gonna be winter again tomorrow and Sunday" ?l10 ?r10)))
                        (:meaning .  ,(amr:penman->predicates 
                                       '(w / winter
                                           :time (a5 / and
                                                    :op1 (t3 / tomorrow)
                                                    :op2 (d / date-entity
                                                            :weekday (s / sunday)))
                                           :mod (a6 / again))
                                       :variablify? t))))

(defparameter *amr-4* `((:form . ((sequence "Gonna be winter again Saturday and Sunday" ?l11 ?r11)))
                        (:meaning .  ,(amr:penman->predicates 
                                       '(w2 / winter
                                           :time (a7 / and
                                                    :op1 (d2 / date-entity
                                                            :weekday (s2 / saturday))
                                                    :op2 (d3 / date-entity
                                                            :weekday (s3 / sunday)))
                                           :mod (a7 / again))
                                       :variablify? t))))

(defparameter *amr-5* `((:form . ((sequence "The man will be buried at a funeral service tomorrow" ?l12 ?r12)))
                        (:meaning .  ,(amr:penman->predicates 
                                       '(b / bury-01
                                           :ARG1 (m / man)
                                           :time (s4 / service-06
                                                    :ARG1 (f / funeral)
                                                    :time (t4 / tomorrow)))
                                       :variablify? t))))

(defparameter *amr-6* `((:form . ((sequence "The man will be buried at a funeral service" ?l13 ?r13)))
                        (:meaning .  ,(amr:penman->predicates 
                                       '(b2 / bury-01
                                           :ARG1 (m2 / man)
                                           :time (s5 / service-06
                                                    :ARG1 (f2 / funeral)))
                                       :variablify? t))))
)

;; Learning 'omorrow-cxn'
(let ((*fcg-constructions*  (make-sandbox-grammar-cxns))
      (holophrastic-amr-cxn-1 (induce-cxns *amr-1* nil :cxn-inventory *fcg-constructions*))
      (holophrastic-amr-cxn-2 (induce-cxns *amr-2* nil :cxn-inventory *fcg-constructions*))
      (holophrastic-amr-cxn-3 (induce-cxns *amr-3* nil :cxn-inventory *fcg-constructions*))
      (holophrastic-amr-cxn-6 (induce-cxns *amr-6* nil :cxn-inventory *fcg-constructions*))
      (ommorrow-cxn nil))

  (induce-cxns *amr-2* holophrastic-amr-cxn-1 :cxn-inventory *fcg-constructions*)

  (comprehend-all (form-string *amr-1*))
  (comprehend-all (form-string *amr-2*))

  (setf ommorrow-cxn (find-cxn "omorrow" *fcg-constructions*
                               :key #'(lambda (cxn) (second (first (attr-val cxn :sequence)))) :test #'string=))

  (induce-cxns *amr-3* ommorrow-cxn :cxn-inventory *fcg-constructions*)
  (induce-cxns *amr-4* holophrastic-amr-cxn-3 :cxn-inventory *fcg-constructions*)

  (comprehend-all (form-string *amr-4*))
  (comprehend-all (form-string *amr-3*))
  
  (induce-cxns *amr-5* holophrastic-amr-cxn-6 :cxn-inventory *fcg-constructions*)

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
  (defparameter *cxn-1* (induce-cxns *amr-simple-1* nil :cxn-inventory *fcg-constructions*))
  (add-element (make-html *cxn-1*))
  (induce-cxns *amr-simple-2* *cxn-1* :cxn-inventory *fcg-constructions*)
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
