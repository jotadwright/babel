(in-package :duckie-language-learning)

;; ---------------
;; + Competitors +
;; ---------------

(defun get-meaning-competitors (agent applied-cxns utterance)
  "Get cxns with the same form as cxn"
  (loop for cxn in applied-cxns
        for cxn-type = (get-cxn-type cxn)
        for competitors = (meaning-competitors-for-cxn-type
                           cxn (grammar agent) cxn-type
                           agent utterance)
        append competitors))

;; ----------------------
;; + Construction types +
;; ----------------------

;; utility function
(defun placeholderp (str)
  (eql (char str 0) #\?))

(defmethod meaning-competitors-for-cxn-type ((cxn construction)
                                             (cxn-inventory construction-inventory)
                                             (cxn-type (eql 'holophrase))
                                             agent
                                             utterance)
  "Finds competitor holophrases constructions"
  (declare (ignorable utterance))
  ;; holophrase competitors have exactly the same form
  (let* ((all-cxns-of-type (remove cxn (find-all cxn-type
                                                 (constructions-list cxn-inventory)
                                                 :key #'get-cxn-type)))
         (cxn-form (extract-and-render cxn))
         (competitors (find-all cxn-form
                                all-cxns-of-type
                                :key #'extract-and-render
                                :test #'string=)))
    competitors))

(defmethod meaning-competitors-for-cxn-type ((cxn construction)
                                             (cxn-inventory construction-inventory)
                                             (cxn-type (eql 'lexical))
                                             agent
                                             utterance)
  "Finds competitor lexical constructions"
  (declare (ignorable utterance))
  ;; lexical competitors have exactly the same form
  (let* ((all-cxns-of-type (remove cxn (find-all cxn-type
                                                 (constructions-list cxn-inventory)
                                                 :key #'get-cxn-type)))
         (cxn-form (extract-and-render cxn))
         (competitors (find-all cxn-form
                                all-cxns-of-type
                                :key #'extract-and-render
                                :test #'string=)))
    competitors))


(defmethod meaning-competitors-for-cxn-type ((cxn construction)
                                             (cxn-inventory construction-inventory)
                                             (cxn-type (eql 'item-based))
                                             agent
                                             utterance)
  "Finds competitor item-based constructions

   Meaning competitors for item-based cxns are
     less or more general item-based cxns
     that also work for the current utterance
     and holophrase cxns."
  (let* ((cxn-name-with-placeholders (make-cxn-placeholder-name
                                      (extract-form-predicates cxn)
                                      cxn-inventory))
         (de-rendered-utterance (tokenize utterance))
         (possible-item-based-competitors (find-all 'item-based
                                                    (constructions-list cxn-inventory)
                                                    :key #'get-cxn-type))
         (item-based-competitors (loop for comp in possible-item-based-competitors
                                       for comp-name-with-placeholders = (make-cxn-placeholder-name (extract-form-predicates comp)
                                                                                                    cxn-inventory)
                                       when (and (length= cxn-name-with-placeholders comp-name-with-placeholders)
                                                 (competitorp cxn-name-with-placeholders
                                                              comp-name-with-placeholders
                                                              de-rendered-utterance))
                                         collect comp))
         (holophrase-competitors (loop for other-cxn in (constructions-list cxn-inventory)
                                       when (and (eql (get-cxn-type other-cxn) 'holophrase)
                                                 (string= (extract-and-render other-cxn) (list-of-strings->string de-rendered-utterance)))
                                         collect other-cxn)))
    (remove cxn (append holophrase-competitors item-based-competitors))))

;; --------------------
;; + Utility functions+
;; --------------------

(defun placeholderp (str)
  (eql (char str 0) #\?))

(defun competitorp (cxn-name-with-placeholders
                    other-name-with-placeholders
                    de-rendered-utterance)
  "Determines wether two cxns are competitors.

   TODO:
     Also need to check links in the categorial network
     for more abstract item-based cxns??"
  (loop for cxn-elem in cxn-name-with-placeholders
        for other-elem in other-name-with-placeholders
        for i from 0
        for nth-word = (nth i de-rendered-utterance)
        always (or ;; same word
                   (string= cxn-elem other-elem)
                   ;; both placeholders
                   (and (placeholderp cxn-elem)
                        (placeholderp other-elem))
                   ;; less abstract
                   (and (placeholderp cxn-elem)
                        (string= other-elem nth-word))
                   ;; more abstract
                   (and (placeholderp other-elem)
                        (stringp cxn-elem)))))

(defun extract-and-render (cxn)
  (list-of-strings->string
   (render (extract-form-predicates cxn)
           (get-configuration (cxn-inventory cxn) :render-mode))))
