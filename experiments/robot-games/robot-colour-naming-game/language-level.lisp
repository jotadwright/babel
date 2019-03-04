(in-package :roconaga)

;; -------------
;; + Invention +
;; -------------

(defmethod add-lex-cxn (agent (form string) (meaning list) &key (score 0.5))
  (let* ((cxn-name (make-symbol (string-append form "-cxn")))
         (unit-name (make-var (string-append form "-unit")))
         (current-interaction-number
          (interaction-number (current-interaction (experiment agent))))
         (chunk (create-chunk-from-irl-program meaning :id (make-id 'chunk)))
         (bind-statement (find 'bind meaning :key #'first))
         (color-category-id (last-elt bind-statement)))
    (multiple-value-bind (cxn-set cxn)
        (eval `(def-fcg-cxn ,cxn-name
                            ((,unit-name
                              (referent ,(car (target-var chunk)))
                              (args (,(car (target-var chunk)))))
                             <-
                             (,unit-name
                              (HASH meaning ,meaning)
                              --
                              (HASH form ((string ,unit-name ,form)))))
                            :cxn-set lex
                            :cxn-inventory ',(grammar agent)
                            :attributes (:score ,score
                                         :form ,form
                                         :chunk ,(id chunk)
                                         :category ,color-category-id
                                         :added ,current-interaction-number)))
      (declare (ignorable cxn-set))
      cxn)))

(defmethod add-lex-cxn (agent (form string) (meaning color-category) &key (score 0.5))
  (let* ((cxn-name (make-symbol (string-append form "-cxn")))
         (unit-name (make-var (string-append form "-unit")))
         (current-interaction-number
          (interaction-number (current-interaction (experiment agent))))
         (new-var (make-var 'x))
         (irl-program (category->irl-program meaning new-var))
         (chunk (create-chunk-from-irl-program irl-program :id (make-id 'chunk))))
    (multiple-value-bind (cxn-set cxn)
        (eval `(def-fcg-cxn ,cxn-name
                            ((,unit-name
                              (referent ,(car (target-var chunk)))
                              (args (,(car (target-var chunk)))))
                             <-
                             (,unit-name
                              (HASH meaning ,irl-program)
                              --
                              (HASH form ((string ,unit-name ,form)))))
                            :cxn-set lex
                            :cxn-inventory ',(grammar agent)
                            :attributes (:score ,score
                                         :form ,form
                                         :chunk ,(id chunk)
                                         :category ,(id meaning)
                                         :added ,current-interaction-number)))
      (declare (ignorable cxn-set))
      cxn)))

(define-event new-cxn-created (cxn fcg-construction))

(defmethod invent ((agent grounded-color-naming-game-agent)
                   (irl-program list))
  "Invent a new word"
  (let* ((form (make-new-word))
         (new-cxn (add-lex-cxn agent form irl-program)))
    (notify new-cxn-created new-cxn)
    (unless (get-configuration agent :silent)
      (speak (robot agent) "I invented a new word"))
    new-cxn))

;; --------------
;; + Production +
;; --------------

(define-event production-succeeded (cxn fcg-construction) (utterance string))

(defmethod produce-utterance ((agent grounded-color-naming-game-agent)
                              (irl-program list))
  "Produce a word for the color category associated with the topic"
  (multiple-value-bind (utterance cipn)
      (formulate (fcg::instantiate-variables irl-program)
                 :cxn-inventory (grammar agent))
    (if utterance
      (progn (setf (applied-cxn agent) (get-original-cxn (first (applied-constructions cipn))))
        (setf (utterance agent) (first utterance))
        (notify production-succeeded (applied-cxn agent) (utterance agent))
        (unless (get-configuration agent :silent)
          (speak (robot agent)
                 (format nil "I call the topic ~a"
                         (first utterance)))))
      (progn (invent agent irl-program)
        (produce-utterance agent irl-program))))
  (utterance agent))

;; ------------------
;; + Pass Utterance +
;; ------------------

(define-event utterance-passed (utterance string))

(defmethod pass-utterance ((speaker grounded-color-naming-game-agent)
                           (hearer grounded-color-naming-game-agent)
                           (speaker-utterance string))
  (declare (ignorable speaker))
  (notify utterance-passed speaker-utterance)
  (setf (utterance hearer) speaker-utterance))

;; -----------------
;; + Comprehension +
;; -----------------

(define-event parsing-succeeded (cxn fcg-construction) (irl-program list))
(define-event parsing-failed)

(defmethod comprehend-utterance ((agent grounded-color-naming-game-agent)
                                 (utterance string))
  "Comprehend the utterance from the speaker"
  (multiple-value-bind (irl-program cipn)
      (comprehend (list utterance) :cxn-inventory (grammar agent))
    (if irl-program
      (progn (unless (get-configuration agent :silent)
               (speak (robot agent) "I parsed the utterance"))
        (setf (irl-program agent) irl-program
              (applied-cxn agent) (get-original-cxn (first (applied-constructions cipn))))
        (notify parsing-succeeded (applied-cxn agent) (irl-program agent)))
      (progn (unless (get-configuration agent :silent)
               (speak (robot agent) "I could not parse the utterance"))
          (notify parsing-failed))))
  (applied-cxn agent))

