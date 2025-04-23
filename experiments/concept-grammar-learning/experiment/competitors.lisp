(in-package :clg)

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

(defun get-form-competitors (agent applied-cxns irl-program)
  "Get cxns that can process the same irl-program"
  (multiple-value-bind (utterances cipns)
      (formulate-all irl-program :cxn-inventory (grammar agent))
    (declare (ignorable utterances))
    (when (> (length cipns) 1)
      (loop for cipn in cipns
            append (original-applied-constructions cipn)
            into other-applied-cxns
            finally
            (return
             (remove-duplicates
              (set-difference other-applied-cxns applied-cxns)))))))

;; ----------------------
;; + Construction types +
;; ----------------------

(defmethod meaning-competitors-for-cxn-type ((cxn construction)
                                             (cxn-inventory construction-inventory)
                                             (cxn-type (eql 'holophrase))
                                             agent utterance)
  (declare (ignorable utterance))
  ;; holophrase competitors have exactly the same form
  (let* ((all-cxns-of-type
          (remove cxn
                  (find-all cxn-type (constructions-list cxn-inventory)
                            :key #'get-cxn-type)))
         (cxn-form (extract-and-render cxn))
         (competitors
          (find-all cxn-form all-cxns-of-type
                    :key #'extract-and-render
                    :test #'string=)))
    competitors))

(defmethod meaning-competitors-for-cxn-type ((cxn construction)
                                             (cxn-inventory construction-inventory)
                                             (cxn-type (eql 'lexical))
                                             agent utterance)
  (declare (ignorable utterance))
  ;; lexical competitors have exactly the same form
  (let* ((all-cxns-of-type
          (remove cxn
                  (find-all cxn-type (constructions-list cxn-inventory)
                            :key #'get-cxn-type)))
         (cxn-form (extract-and-render cxn))
         (competitors
          (find-all cxn-form all-cxns-of-type
                    :key #'extract-and-render
                    :test #'string=)))
    competitors))

(defmethod meaning-competitors-for-cxn-type ((cxn construction)
                                             (cxn-inventory construction-inventory)
                                             (cxn-type (eql 'item-based))
                                             agent utterance)
  ;; meaning competitors for item-based cxns are
  ;; less or more general item-based cxns
  ;; that also work for the current utterance
  ;; and holophrase cxns
  (let* ((cxn-name-with-placeholders
          (make-cxn-placeholder-name
           (extract-form-predicates cxn)
           cxn-inventory))
         (de-rendered-utterance
          (fcg::tokenize utterance))
         (possible-item-based-competitors
          (find-all 'item-based
                    (constructions-list cxn-inventory)
                    :key #'get-cxn-type))
         (item-based-competitors
          (loop for comp in possible-item-based-competitors
                for comp-name-with-placeholders =
                (make-cxn-placeholder-name
                 (extract-form-predicates comp)
                 cxn-inventory)
                when (and (length= cxn-name-with-placeholders
                                   comp-name-with-placeholders)
                          (competitorp cxn-name-with-placeholders
                                       comp-name-with-placeholders
                                       de-rendered-utterance))
                collect comp))
         (holophrase-competitors
          (loop for other-cxn in (constructions-list cxn-inventory)
                when (and (eql (get-cxn-type other-cxn) 'holophrase)
                          (string= (extract-and-render other-cxn)
                                   (list-of-strings->string
                                    (fcg::tokenize utterance))))
                collect other-cxn)))
    (remove cxn (append holophrase-competitors item-based-competitors))))

;; helper functions

(defun placeholderp (str)
  (eql (char str 0) #\?))

(defun competitorp (cxn-name-with-placeholders
                    other-name-with-placeholders
                    de-rendered-utterance)
  "Determines whether two cxns are competitors."
  ;; also need to check links in the categorial network
  ;; for more abstract item-based cxns??
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