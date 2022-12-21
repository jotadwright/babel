(ql:quickload :grammar-learning) 
(in-package :grammar-learning)

;; full logging
(progn
  (deactivate-all-monitors)
  (activate-monitor display-metrics)
  (activate-monitor trace-fcg)
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor summarize-results-after-n-interactions)
  (activate-monitor show-type-hierarchy-after-n-interactions)
  (activate-monitor trace-interactions-in-wi))


(defun test-addition-repair-comprehension-clevr ()
  (let* ((experiment (set-up-cxn-inventory-and-repairs))
         (cxn-inventory (grammar (first (agents experiment)))))
    (comprehend "The gray object is what shape?"
                    :cxn-inventory cxn-inventory
                    :gold-standard-meaning '((get-context ?source-1)
                                             (filter ?target-2 ?target-1 ?color-2)
                                             (unique ?source-9 ?target-2)
                                             (bind shape-category ?shape-8 thing)
                                             (bind attribute-category ?attribute-2 shape)
                                             (filter ?target-1 ?source-1 ?shape-8)
                                             (bind color-category ?color-2 gray)
                                             (query ?target-7 ?source-9 ?attribute-2)))
    (test-repair-status 'holistic->item-based--addition
                        (second (multiple-value-list
                                 (comprehend "The large gray object is what shape?"
                    :cxn-inventory cxn-inventory
                    :gold-standard-meaning '((get-context ?source-1)
                                             (filter ?target-39552 ?target-2 ?size-4)
                                             (unique ?source-10 ?target-39552)
                                             (bind color-category ?color-2 gray)
                                             (filter ?target-1 ?source-1 ?shape-8)
                                             (bind attribute-category ?attribute-2 shape)
                                             (bind shape-category ?shape-8 thing)
                                             (filter ?target-2 ?target-1 ?color-2)
                                             (bind size-category ?size-4 large)
                                             (query ?target-8 ?source-10 ?attribute-2))))))))


(defun test-deletion-repair-comprehension-clevr ()
  (let* ((experiment (set-up-cxn-inventory-and-repairs))
         (cxn-inventory (grammar (first (agents experiment)))))
    (comprehend "The large gray object is what shape?"
                    :cxn-inventory cxn-inventory
                    :gold-standard-meaning '((get-context ?source-1)
                                             (filter ?target-39552 ?target-2 ?size-4)
                                             (unique ?source-10 ?target-39552)
                                             (bind color-category ?color-2 gray)
                                             (filter ?target-1 ?source-1 ?shape-8)
                                             (bind attribute-category ?attribute-2 shape)
                                             (bind shape-category ?shape-8 thing)
                                             (filter ?target-2 ?target-1 ?color-2)
                                             (bind size-category ?size-4 large)
                                             (query ?target-8 ?source-10 ?attribute-2)))
    (test-repair-status 'holistic->item-based--deletion
                        (second (multiple-value-list
                                 (comprehend "The gray object is what shape?"
                    :cxn-inventory cxn-inventory
                    :gold-standard-meaning '((get-context ?source-1)
                                             (filter ?target-2 ?target-1 ?color-2)
                                             (unique ?source-9 ?target-2)
                                             (bind shape-category ?shape-8 thing)
                                             (bind attribute-category ?attribute-2 shape)
                                             (filter ?target-1 ?source-1 ?shape-8)
                                             (bind color-category ?color-2 gray)
                                             (query ?target-7 ?source-9 ?attribute-2))))))
    (comprehend "The large gray object is what shape?"
                    :cxn-inventory cxn-inventory
                    :gold-standard-meaning '((get-context ?source-1)
                                             (filter ?target-39552 ?target-2 ?size-4)
                                             (unique ?source-10 ?target-39552)
                                             (bind color-category ?color-2 gray)
                                             (filter ?target-1 ?source-1 ?shape-8)
                                             (bind attribute-category ?attribute-2 shape)
                                             (bind shape-category ?shape-8 thing)
                                             (filter ?target-2 ?target-1 ?color-2)
                                             (bind size-category ?size-4 large)
                                             (query ?target-8 ?source-10 ?attribute-2)))
    (add-element (make-html cxn-inventory))))


(defun test-substitution-repair-comprehension-clevr ()
  (let* ((experiment (set-up-cxn-inventory-and-repairs))
         (cxn-inventory (grammar (first (agents experiment)))))
    (comprehend "The large gray object is what shape?"
                :cxn-inventory cxn-inventory
                :gold-standard-meaning '((get-context ?source-1)
                                         (filter ?target-39552 ?target-2 ?size-4)
                                         (unique ?source-10 ?target-39552)
                                         (bind color-category ?color-2 gray)
                                         (filter ?target-1 ?source-1 ?shape-8)
                                         (bind attribute-category ?attribute-2 shape)
                                         (bind shape-category ?shape-8 thing)
                                         (filter ?target-2 ?target-1 ?color-2)
                                         (bind size-category ?size-4 large)
                                         (query ?target-8 ?source-10 ?attribute-2)))
    (test-repair-status 'holistic->item-based--substitution
                        (second (multiple-value-list
                                 (comprehend "The large yellow object is what shape?"
                                             :cxn-inventory cxn-inventory
                                             :gold-standard-meaning '((get-context ?source-1)
                                                                      (filter ?target-14197 ?target-2 ?size-2)
                                                                      (unique ?source-10 ?target-14197)
                                                                      (bind color-category ?color-16 yellow)
                                                                      (filter ?target-1 ?source-1 ?shape-8)
                                                                      (bind attribute-category ?attribute-2 shape)
                                                                      (bind shape-category ?shape-8 thing)
                                                                      (filter ?target-2 ?target-1 ?color-16)
                                                                      (bind size-category ?size-2 large)
                                                                      (query ?target-8 ?source-10 ?attribute-2))))))))



(defun test-addition-repair-comprehension-german ()
  (let* ((experiment (set-up-cxn-inventory-and-repairs-german))
         (cxn-inventory (grammar (first (agents experiment)))))
    (comprehend "Die junge Frau gibt dem Mann den Apfel"
                    :cxn-inventory cxn-inventory
                    :gold-standard-meaning '((geben-01 ?g)
                                             (young ?y)
                                             (woman ?w)
                                             (mod ?w ?y)
                                             (man ?m)
                                             (apple ?a)
                                             (arg0 ?g ?w)
                                             (arg1 ?g ?a)
                                             (arg2 ?g ?m)
                                             (topicalized ?w)))
    (test-repair-status 'holistic->item-based--addition
                        (second (multiple-value-list
                                 (comprehend "Die junge Frau gibt dem Mann den gruenen Apfel"
                    :cxn-inventory cxn-inventory
                    :gold-standard-meaning '((geben-01 ?g)
                                             (young ?y)
                                             (woman ?w)
                                             (mod ?w ?y)
                                             (man ?m)
                                             (apple ?a)
                                             (green ?gr)
                                             (mod ?a ?gr)
                                             (arg0 ?g ?w)
                                             (arg1 ?g ?a)
                                             (arg2 ?g ?m)
                                             (topicalized ?w))))))))




(defun test-X-Y-adj ()
  (let* ((experiment (set-up-cxn-inventory-and-repairs-german))
         (cxn-inventory (grammar (first (agents experiment)))))
    (comprehend "Die gute intelligente Frau"
                    :cxn-inventory cxn-inventory
                    :gold-standard-meaning '(
                                             (good ?g)
                                             (intelligent ?i)
                                             (woman ?w)
                                             (mod ?w ?g)
                                             (mod ?w ?i)))

    (comprehend "Die grosse Frau"
                    :cxn-inventory cxn-inventory
                    :gold-standard-meaning '((tall ?t)
                                             (woman ?w)
                                             (mod ?w ?t)
                                             ))
    (comprehend "Die alte Koenigin"
                    :cxn-inventory cxn-inventory
                    :gold-standard-meaning '((old ?o)
                                             (queen ?q)
                                             (mod ?q ?o)
                                             ))
    (comprehend "Die blonde Koenigin"
                    :cxn-inventory cxn-inventory
                    :gold-standard-meaning '((blonde ?b)
                                             (queen ?q)
                                             (mod ?q ?b)
                                             ))
    (comprehend "Die alte blonde Frau"
                    :cxn-inventory cxn-inventory
                    :gold-standard-meaning '(
                                             (old ?o)
                                             (blond ?b)
                                             (woman ?w)
                                             (mod ?w ?o)
                                             (mod ?w ?b)))
    
  ))





(defun test-substitution-repair-comprehension-german ()
  (let* ((experiment (set-up-cxn-inventory-and-repairs-german))
         (cxn-inventory (grammar (first (agents experiment)))))
    (comprehend "Die junge Frau gibt dem Mann den Apfel"
                    :cxn-inventory cxn-inventory
                    :gold-standard-meaning '((geben-01 ?g)
                                             (young ?y)
                                             (woman ?w)
                                             (mod ?w ?y)
                                             (man ?m)
                                             (apple ?a)
                                             (arg0 ?g ?w)
                                             (arg1 ?g ?a)
                                             (arg2 ?g ?m)
                                             (topicalized ?w)))
    (test-repair-status 'holistic->item-based--substitution
                        (second (multiple-value-list
                                 (comprehend "Die junge Frau gibt dem Mann den Hund"
                    :cxn-inventory cxn-inventory
                    :gold-standard-meaning '((geben-01 ?g)
                                             (young ?y)
                                             (woman ?w)
                                             (mod ?w ?y)
                                             (man ?m)
                                             (dog ?d)
                                             (arg0 ?g ?w)
                                             (arg1 ?g ?d)
                                             (arg2 ?g ?m)
                                             (topicalized ?w))))))))

(defun test-deletion-repair-comprehension-german ()
  (let* ((experiment (set-up-cxn-inventory-and-repairs-german))
         (cxn-inventory (grammar (first (agents experiment)))))
    (comprehend "Die junge Frau gibt dem Mann den gruenen Apfel"
                    :cxn-inventory cxn-inventory
                    :gold-standard-meaning '((geben-01 ?g)
                                             (young ?y)
                                             (woman ?w)
                                             (mod ?w ?y)
                                             (man ?m)
                                             (apple ?a)
                                             (green ?gr)
                                             (mod ?a ?gr)
                                             (arg0 ?g ?w)
                                             (arg1 ?g ?a)
                                             (arg2 ?g ?m)
                                             (topicalized ?w)))
    
    (test-repair-status 'holistic->item-based--deletion
                        (second (multiple-value-list
                                 (comprehend "Die junge Frau gibt dem Mann den Apfel"
                    :cxn-inventory cxn-inventory
                    :gold-standard-meaning '((geben-01 ?g)
                                             (young ?y)
                                             (woman ?w)
                                             (mod ?w ?y)
                                             (man ?m)
                                             (apple ?a)
                                             (arg0 ?g ?w)
                                             (arg1 ?g ?a)
                                             (arg2 ?g ?m)
                                             (topicalized ?w))))))))



(defun test-form-diff-german-shortest-first ()     ;starts from obs to cxn addition
  (let* ((longer-forms '((STRING GRAMMAR-LEARNING::?DIE-66 "die")
                         (STRING GRAMMAR-LEARNING::?JUNGE-66 "junge")
                         (STRING GRAMMAR-LEARNING::?FRAU-66 "frau")
                         (STRING GRAMMAR-LEARNING::?GIBT-66 "gibt")
                         (STRING GRAMMAR-LEARNING::?DEM-66 "dem")
                         (STRING GRAMMAR-LEARNING::?MANN-66 "mann")
                         (STRING GRAMMAR-LEARNING::?DEN-66 "den")
                         (STRING GRAMMAR-LEARNING::?GRUENEN-36 "gruenen")
                         (STRING GRAMMAR-LEARNING::?APFEL-66 "apfel")
                         (FCG:MEETS GRAMMAR-LEARNING::?DIE-66 GRAMMAR-LEARNING::?JUNGE-66)
                         (FCG:MEETS GRAMMAR-LEARNING::?JUNGE-66 GRAMMAR-LEARNING::?FRAU-66)
                         (FCG:MEETS GRAMMAR-LEARNING::?FRAU-66 GRAMMAR-LEARNING::?GIBT-66)
                         (FCG:MEETS GRAMMAR-LEARNING::?GIBT-66 GRAMMAR-LEARNING::?DEM-66)
                         (FCG:MEETS GRAMMAR-LEARNING::?DEM-66 GRAMMAR-LEARNING::?MANN-66)
                         (FCG:MEETS GRAMMAR-LEARNING::?MANN-66 GRAMMAR-LEARNING::?DEN-66)
                         (FCG:MEETS GRAMMAR-LEARNING::?DEN-66 GRAMMAR-LEARNING::?GRUENEN-36)
                         (FCG:MEETS GRAMMAR-LEARNING::?GRUENEN-36 GRAMMAR-LEARNING::?APFEL-66)))
                       (shorter-forms
                        '((STRING GRAMMAR-LEARNING::?DIE-51 "die")
                          (STRING GRAMMAR-LEARNING::?JUNGE-51 "junge")
                          (STRING GRAMMAR-LEARNING::?FRAU-51 "frau")
                          (STRING GRAMMAR-LEARNING::?GIBT-51 "gibt")
                          (STRING GRAMMAR-LEARNING::?DEM-51 "dem")
                          (STRING GRAMMAR-LEARNING::?MANN-51 "mann")
                          (STRING GRAMMAR-LEARNING::?DEN-51 "den")
                          (STRING GRAMMAR-LEARNING::?APFEL-51 "apfel")
                          (FCG:MEETS GRAMMAR-LEARNING::?DIE-51 GRAMMAR-LEARNING::?JUNGE-51)
                          (FCG:MEETS GRAMMAR-LEARNING::?JUNGE-51 GRAMMAR-LEARNING::?FRAU-51)
                          (FCG:MEETS GRAMMAR-LEARNING::?FRAU-51 GRAMMAR-LEARNING::?GIBT-51)
                          (FCG:MEETS GRAMMAR-LEARNING::?GIBT-51 GRAMMAR-LEARNING::?DEM-51)
                          (FCG:MEETS GRAMMAR-LEARNING::?DEM-51 GRAMMAR-LEARNING::?MANN-51)
                          (FCG:MEETS GRAMMAR-LEARNING::?MANN-51 GRAMMAR-LEARNING::?DEN-51)
                          (FCG:MEETS GRAMMAR-LEARNING::?DEN-51 GRAMMAR-LEARNING::?APFEL-51))))
         (values (multiple-value-bind (non-overlapping-form-observation ; nil
                               non-overlapping-form-cxn ; gruenen
                               overlapping-form-observation ; everything
                               overlapping-form-cxn) ; everything else except gruenen
             (diff-form-constraints shorter-forms longer-forms))))) ; diff-form-constraints takes observation + cxn
;; looks like non-overlapping-form-observation and overlapping-form-observation are switched!

(defun test-form-diff-german-longest-first ()   ;deletion
  (let* ((longer-forms '((STRING GRAMMAR-LEARNING::?DIE-66 "die")
                         (STRING GRAMMAR-LEARNING::?JUNGE-66 "junge")
                         (STRING GRAMMAR-LEARNING::?FRAU-66 "frau")
                         (STRING GRAMMAR-LEARNING::?GIBT-66 "gibt")
                         (STRING GRAMMAR-LEARNING::?DEM-66 "dem")
                         (STRING GRAMMAR-LEARNING::?MANN-66 "mann")
                         (STRING GRAMMAR-LEARNING::?DEN-66 "den")
                         (STRING GRAMMAR-LEARNING::?GRUENEN-36 "gruenen")
                         (STRING GRAMMAR-LEARNING::?APFEL-66 "apfel")
                         (FCG:MEETS GRAMMAR-LEARNING::?DIE-66 GRAMMAR-LEARNING::?JUNGE-66)
                         (FCG:MEETS GRAMMAR-LEARNING::?JUNGE-66 GRAMMAR-LEARNING::?FRAU-66)
                         (FCG:MEETS GRAMMAR-LEARNING::?FRAU-66 GRAMMAR-LEARNING::?GIBT-66)
                         (FCG:MEETS GRAMMAR-LEARNING::?GIBT-66 GRAMMAR-LEARNING::?DEM-66)
                         (FCG:MEETS GRAMMAR-LEARNING::?DEM-66 GRAMMAR-LEARNING::?MANN-66)
                         (FCG:MEETS GRAMMAR-LEARNING::?MANN-66 GRAMMAR-LEARNING::?DEN-66)
                         (FCG:MEETS GRAMMAR-LEARNING::?DEN-66 GRAMMAR-LEARNING::?GRUENEN-36)
                         (FCG:MEETS GRAMMAR-LEARNING::?GRUENEN-36 GRAMMAR-LEARNING::?APFEL-66)))
                       (shorter-forms
                        '((STRING GRAMMAR-LEARNING::?DIE-51 "die")
                          (STRING GRAMMAR-LEARNING::?JUNGE-51 "junge")
                          (STRING GRAMMAR-LEARNING::?FRAU-51 "frau")
                          (STRING GRAMMAR-LEARNING::?GIBT-51 "gibt")
                          (STRING GRAMMAR-LEARNING::?DEM-51 "dem")
                          (STRING GRAMMAR-LEARNING::?MANN-51 "mann")
                          (STRING GRAMMAR-LEARNING::?DEN-51 "den")
                          (STRING GRAMMAR-LEARNING::?APFEL-51 "apfel")
                          (FCG:MEETS GRAMMAR-LEARNING::?DIE-51 GRAMMAR-LEARNING::?JUNGE-51)
                          (FCG:MEETS GRAMMAR-LEARNING::?JUNGE-51 GRAMMAR-LEARNING::?FRAU-51)
                          (FCG:MEETS GRAMMAR-LEARNING::?FRAU-51 GRAMMAR-LEARNING::?GIBT-51)
                          (FCG:MEETS GRAMMAR-LEARNING::?GIBT-51 GRAMMAR-LEARNING::?DEM-51)
                          (FCG:MEETS GRAMMAR-LEARNING::?DEM-51 GRAMMAR-LEARNING::?MANN-51)
                          (FCG:MEETS GRAMMAR-LEARNING::?MANN-51 GRAMMAR-LEARNING::?DEN-51)
                          (FCG:MEETS GRAMMAR-LEARNING::?DEN-51 GRAMMAR-LEARNING::?APFEL-51))))
         (values (multiple-value-bind (non-overlapping-form-observation ; gruenen
                               non-overlapping-form-cxn ; nil
                               overlapping-form-observation ; everything else except gruenen
                               overlapping-form-cxn) ; everything
             (diff-form-constraints longer-forms shorter-forms)))))
;; looks like non-overlapping-form-cxn and overlapping-form-cxn are switched!
;(values fc-1-diff ;non-overlapping-obs
            ;fc-2-diff ; non-overlapping-cxn
            ;(set-difference fc-1 fc-1-diff :test #'equal) ; overlapping obs
            ;(set-difference fc-2 fc-2-diff :test #'equal)) ; overlapping cxn



; (activate-monitor trace-fcg)
(defun run-test-corpora()

  (test-X-Y-adj)
  (test-addition-repair-comprehension-german)
  (test-deletion-repair-comprehension-german)
  (test-substitution-repair-comprehension-german)
  
  (test-addition-repair-comprehension-clevr)
  (test-deletion-repair-comprehension-clevr)
  (test-substitution-repair-comprehension-clevr)
  
  (test-form-diff-german-shortest-first)
  (test-form-diff-german-longest-first))

;(run-tests-corpora)




         ;(values (multiple-value-bind (non-overlapping-form-observation ; nil
                               ;non-overlapping-form-cxn ; gruenen
                               ;overlapping-form-observation ; everything
                               ;overlapping-form-cxn) ; everything else except gruenen
             ;(diff-form-constraints shorter-forms longer-forms))))) ; diff-form-constraints takes observation + cxn
;; looks like non-overlapping-form-observation and overlapping-form-observation are switched!


;;;ADDITION VARS CLEVR 

;the gray object is what shape?  (cxn)
;the large gray object is what shape?  (obs)

;non-overlapping-meaning-obs  -correct
;((CLEVR-WORLD:FILTER GRAMMAR-LEARNING::?TARGET-39552 GRAMMAR-LEARNING::?TARGET-2 GRAMMAR-LEARNING::?SIZE-4) (UTILS:BIND CLEVR-WORLD:SIZE-CATEGORY GRAMMAR-LEARNING::?SIZE-4 CLEVR-WORLD:LARGE))

;non-overlapping-meaning-cxn   - should be nil
;((CLEVR-WORLD:GET-CONTEXT GRAMMAR-LEARNING::?SOURCE-1) (CLEVR-WORLD:FILTER GRAMMAR-LEARNING::?TARGET-2 GRAMMAR-LEARNING::?TARGET-1 GRAMMAR-LEARNING::?COLOR-2) (CLEVR-WORLD:UNIQUE GRAMMAR-LEARNING::?SOURCE-9 GRAMMAR-LEARNING::?TARGET-2) (UTILS:BIND CLEVR-WORLD:SHAPE-CATEGORY GRAMMAR-LEARNING::?SHAPE-8 CLEVR-WORLD:THING) (UTILS:BIND CLEVR-WORLD:ATTRIBUTE-CATEGORY GRAMMAR-LEARNING::?ATTRIBUTE-2 CLEVR-WORLD:SHAPE) (CLEVR-WORLD:FILTER GRAMMAR-LEARNING::?TARGET-1 GRAMMAR-LEARNING::?SOURCE-1 GRAMMAR-LEARNING::?SHAPE-8) (UTILS:BIND CLEVR-WORLD:COLOR-CATEGORY GRAMMAR-LEARNING::?COLOR-2 CLEVR-WORLD:GRAY) (CLEVR-WORLD:QUERY GRAMMAR-LEARNING::?TARGET-7 GRAMMAR-LEARNING::?SOURCE-9 GRAMMAR-LEARNING::?ATTRIBUTE-2))

;non-overlapping-form-obs  -correct USED
;((STRING GRAMMAR-LEARNING::?LARGE-15 "large"))

;non-overlapping-form-cxn   - should be NIL
;((FCG:MEETS GRAMMAR-LEARNING::?THE-37 GRAMMAR-LEARNING::?GRAY-37) (FCG:MEETS GRAMMAR-LEARNING::?GRAY-37 GRAMMAR-LEARNING::?OBJECT-37) (FCG:MEETS GRAMMAR-LEARNING::?OBJECT-37 GRAMMAR-LEARNING::?IS-37) (FCG:MEETS GRAMMAR-LEARNING::?IS-37 GRAMMAR-LEARNING::?WHAT-37) (FCG:MEETS GRAMMAR-LEARNING::?WHAT-37 GRAMMAR-LEARNING::?SHAPE-46) (STRING GRAMMAR-LEARNING::?THE-37 "the") (STRING GRAMMAR-LEARNING::?GRAY-37 "gray") (STRING GRAMMAR-LEARNING::?OBJECT-37 "object") (STRING GRAMMAR-LEARNING::?IS-37 "is") (STRING GRAMMAR-LEARNING::?WHAT-37 "what") (STRING GRAMMAR-LEARNING::?SHAPE-46 "shape"))


;overlapping-form-cxn  - should be everything 
;NIL

;overlapping-meaning-obs
;((CLEVR-WORLD:QUERY GRAMMAR-LEARNING::?TARGET-8 GRAMMAR-LEARNING::?SOURCE-10 GRAMMAR-LEARNING::?ATTRIBUTE-2) (CLEVR-WORLD:FILTER GRAMMAR-LEARNING::?TARGET-2 GRAMMAR-LEARNING::?TARGET-1 GRAMMAR-LEARNING::?COLOR-2) (UTILS:BIND CLEVR-WORLD:SHAPE-CATEGORY GRAMMAR-LEARNING::?SHAPE-8 CLEVR-WORLD:THING) (UTILS:BIND CLEVR-WORLD:ATTRIBUTE-CATEGORY GRAMMAR-LEARNING::?ATTRIBUTE-2 CLEVR-WORLD:SHAPE) (CLEVR-WORLD:FILTER GRAMMAR-LEARNING::?TARGET-1 GRAMMAR-LEARNING::?SOURCE-1 GRAMMAR-LEARNING::?SHAPE-8) (UTILS:BIND CLEVR-WORLD:COLOR-CATEGORY GRAMMAR-LEARNING::?COLOR-2 CLEVR-WORLD:GRAY) (CLEVR-WORLD:UNIQUE GRAMMAR-LEARNING::?SOURCE-10 GRAMMAR-LEARNING::?TARGET-39552) (CLEVR-WORLD:GET-CONTEXT GRAMMAR-LEARNING::?SOURCE-1))

;overlapping-meaning-cxn    - should be everything
;NIL

;overlapping-form-obs   -correct  USED
;((FCG:MEETS GRAMMAR-LEARNING::?WHAT-42 GRAMMAR-LEARNING::?SHAPE-51) (FCG:MEETS GRAMMAR-LEARNING::?IS-42 GRAMMAR-LEARNING::?WHAT-42) (FCG:MEETS GRAMMAR-LEARNING::?OBJECT-42 GRAMMAR-LEARNING::?IS-42) (FCG:MEETS GRAMMAR-LEARNING::?GRAY-42 GRAMMAR-LEARNING::?OBJECT-42) (FCG:MEETS GRAMMAR-LEARNING::?LARGE-15 GRAMMAR-LEARNING::?GRAY-42) (FCG:MEETS GRAMMAR-LEARNING::?THE-42 GRAMMAR-LEARNING::?LARGE-15) (STRING GRAMMAR-LEARNING::?SHAPE-51 "shape") (STRING GRAMMAR-LEARNING::?WHAT-42 "what") (STRING GRAMMAR-LEARNING::?IS-42 "is") (STRING GRAMMAR-LEARNING::?OBJECT-42 "object") (STRING GRAMMAR-LEARNING::?GRAY-42 "gray") (STRING GRAMMAR-LEARNING::?THE-42 "the"))



;;;SUBSTITUTION CLEVR

;The large gray object is what shape? (cxn)
;the large yellow object is what shape? (obs)


;non-overlapping-meaning-obs   -yellow-  correct
;((CLEVR-WORLD:FILTER GRAMMAR-LEARNING::?TARGET-2 GRAMMAR-LEARNING::?TARGET-1 GRAMMAR-LEARNING::?COLOR-16) (UTILS:BIND CLEVR-WORLD:COLOR-CATEGORY GRAMMAR-LEARNING::?COLOR-16 CLEVR-WORLD:YELLOW))

;non-overlapping-meaning-cxn
;((CLEVR-WORLD:FILTER GRAMMAR-LEARNING::?TARGET-2 GRAMMAR-LEARNING::?TARGET-1 GRAMMAR-LEARNING::?COLOR-2) (UTILS:BIND CLEVR-WORLD:COLOR-CATEGORY GRAMMAR-LEARNING::?COLOR-2 CLEVR-WORLD:GRAY))

;non-overlapping-form-obs
;((STRING GRAMMAR-LEARNING::?YELLOW-8 "yellow")))

;non-overlapping-form-cxn   - should be just gray 
;((STRING GRAMMAR-LEARNING::?GRAY-83 "gray"))

;overlapping-form-cxn  correct 
;((FCG:MEETS GRAMMAR-LEARNING::?WHAT-91 GRAMMAR-LEARNING::?SHAPE-100) (FCG:MEETS GRAMMAR-LEARNING::?IS-91 GRAMMAR-LEARNING::?WHAT-91) (FCG:MEETS GRAMMAR-LEARNING::?OBJECT-91 GRAMMAR-LEARNING::?IS-91) (FCG:MEETS GRAMMAR-LEARNING::?GRAY-83 GRAMMAR-LEARNING::?OBJECT-91) (FCG:MEETS GRAMMAR-LEARNING::?LARGE-55 GRAMMAR-LEARNING::?GRAY-83) (FCG:MEETS GRAMMAR-LEARNING::?THE-91 GRAMMAR-LEARNING::?LARGE-55) (STRING GRAMMAR-LEARNING::?SHAPE-100 "shape") (STRING GRAMMAR-LEARNING::?WHAT-91 "what") (STRING GRAMMAR-LEARNING::?IS-91 "is") (STRING GRAMMAR-LEARNING::?OBJECT-91 "object") (STRING GRAMMAR-LEARNING::?LARGE-55 "large") (STRING GRAMMAR-LEARNING::?THE-91 "the"))

;overlapping-meaning-obs  - correct -everything but yellow 
;((CLEVR-WORLD:QUERY GRAMMAR-LEARNING::?TARGET-8 GRAMMAR-LEARNING::?SOURCE-10 GRAMMAR-LEARNING::?ATTRIBUTE-2) (UTILS:BIND CLEVR-WORLD:SIZE-CATEGORY GRAMMAR-LEARNING::?SIZE-2 CLEVR-WORLD:LARGE) (UTILS:BIND CLEVR-WORLD:SHAPE-CATEGORY GRAMMAR-LEARNING::?SHAPE-8 CLEVR-WORLD:THING) (UTILS:BIND CLEVR-WORLD:ATTRIBUTE-CATEGORY GRAMMAR-LEARNING::?ATTRIBUTE-2 CLEVR-WORLD:SHAPE) (CLEVR-WORLD:FILTER GRAMMAR-LEARNING::?TARGET-1 GRAMMAR-LEARNING::?SOURCE-1 GRAMMAR-LEARNING::?SHAPE-8) (CLEVR-WORLD:UNIQUE GRAMMAR-LEARNING::?SOURCE-10 GRAMMAR-LEARNING::?TARGET-14197) (CLEVR-WORLD:FILTER GRAMMAR-LEARNING::?TARGET-14197 GRAMMAR-LEARNING::?TARGET-2 GRAMMAR-LEARNING::?SIZE-2) (CLEVR-WORLD:GET-CONTEXT GRAMMAR-LEARNING::?SOURCE-1))

;overlapping-meaning-cxn   -correct  -everything but gray
;((CLEVR-WORLD:QUERY GRAMMAR-LEARNING::?TARGET-8 GRAMMAR-LEARNING::?SOURCE-10 GRAMMAR-LEARNING::?ATTRIBUTE-2) (UTILS:BIND CLEVR-WORLD:SIZE-CATEGORY GRAMMAR-LEARNING::?SIZE-4 CLEVR-WORLD:LARGE) (UTILS:BIND CLEVR-WORLD:SHAPE-CATEGORY GRAMMAR-LEARNING::?SHAPE-8 CLEVR-WORLD:THING) (UTILS:BIND CLEVR-WORLD:ATTRIBUTE-CATEGORY GRAMMAR-LEARNING::?ATTRIBUTE-2 CLEVR-WORLD:SHAPE) (CLEVR-WORLD:FILTER GRAMMAR-LEARNING::?TARGET-1 GRAMMAR-LEARNING::?SOURCE-1 GRAMMAR-LEARNING::?SHAPE-8) (CLEVR-WORLD:UNIQUE GRAMMAR-LEARNING::?SOURCE-10 GRAMMAR-LEARNING::?TARGET-39552) (CLEVR-WORLD:FILTER GRAMMAR-LEARNING::?TARGET-39552 GRAMMAR-LEARNING::?TARGET-2 GRAMMAR-LEARNING::?SIZE-4) (CLEVR-WORLD:GET-CONTEXT GRAMMAR-LEARNING::?SOURCE-1))

;overlapping-form-obs
;((FCG:MEETS GRAMMAR-LEARNING::?WHAT-95 GRAMMAR-LEARNING::?SHAPE-104) (FCG:MEETS GRAMMAR-LEARNING::?IS-95 GRAMMAR-LEARNING::?WHAT-95) (FCG:MEETS GRAMMAR-LEARNING::?OBJECT-95 GRAMMAR-LEARNING::?IS-95) (FCG:MEETS GRAMMAR-LEARNING::?YELLOW-12 GRAMMAR-LEARNING::?OBJECT-95) (FCG:MEETS GRAMMAR-LEARNING::?LARGE-59 GRAMMAR-LEARNING::?YELLOW-12) (FCG:MEETS GRAMMAR-LEARNING::?THE-95 GRAMMAR-LEARNING::?LARGE-59) (STRING GRAMMAR-LEARNING::?SHAPE-104 "shape") (STRING GRAMMAR-LEARNING::?WHAT-95 "what") (STRING GRAMMAR-LEARNING::?IS-95 "is") (STRING GRAMMAR-LEARNING::?OBJECT-95 "object") (STRING GRAMMAR-LEARNING::?LARGE-59 "large") (STRING GRAMMAR-LEARNING::?THE-95 "the"))


;;DELETION CLEVER
;the large gray object is what shape? (cxn)
;the gray object is what shape?      (obs)

  ;; obs is shorter than cxn
  ;; overlapping cxn = item-based
  ;; non-overlapping cxn = the holistic part
  ;; overlapping observation = the entire observation
  ;; non-overlapping-meaning/form of obs is nil

;non-overlapping-meaning-obs  - should be nil 
;((CLEVR-WORLD:GET-CONTEXT GRAMMAR-LEARNING::?SOURCE-1) (CLEVR-WORLD:FILTER GRAMMAR-LEARNING::?TARGET-2 GRAMMAR-LEARNING::?TARGET-1 GRAMMAR-LEARNING::?COLOR-2) (CLEVR-WORLD:UNIQUE GRAMMAR-LEARNING::?SOURCE-9 GRAMMAR-LEARNING::?TARGET-2) (UTILS:BIND CLEVR-WORLD:SHAPE-CATEGORY GRAMMAR-LEARNING::?SHAPE-8 CLEVR-WORLD:THING) (UTILS:BIND CLEVR-WORLD:ATTRIBUTE-CATEGORY GRAMMAR-LEARNING::?ATTRIBUTE-2 CLEVR-WORLD:SHAPE) (CLEVR-WORLD:FILTER GRAMMAR-LEARNING::?TARGET-1 GRAMMAR-LEARNING::?SOURCE-1 GRAMMAR-LEARNING::?SHAPE-8) (UTILS:BIND CLEVR-WORLD:COLOR-CATEGORY GRAMMAR-LEARNING::?COLOR-2 CLEVR-WORLD:GRAY) (CLEVR-WORLD:QUERY GRAMMAR-LEARNING::?TARGET-7 GRAMMAR-LEARNING::?SOURCE-9 GRAMMAR-LEARNING::?ATTRIBUTE-2))

;non-overlapping-meaning-cxn -correct 
;((CLEVR-WORLD:FILTER GRAMMAR-LEARNING::?TARGET-39552 GRAMMAR-LEARNING::?TARGET-2 GRAMMAR-LEARNING::?SIZE-4) (UTILS:BIND CLEVR-WORLD:SIZE-CATEGORY GRAMMAR-LEARNING::?SIZE-4 CLEVR-WORLD:LARGE))

;non-overlapping-form-obs -should be NIL but it's everything 
;((FCG:MEETS GRAMMAR-LEARNING::?THE-84 GRAMMAR-LEARNING::?GRAY-80) (FCG:MEETS GRAMMAR-LEARNING::?GRAY-80 GRAMMAR-LEARNING::?OBJECT-84) (FCG:MEETS GRAMMAR-LEARNING::?OBJECT-84 GRAMMAR-LEARNING::?IS-84) (FCG:MEETS GRAMMAR-LEARNING::?IS-84 GRAMMAR-LEARNING::?WHAT-84) (FCG:MEETS GRAMMAR-LEARNING::?WHAT-84 GRAMMAR-LEARNING::?SHAPE-93) (STRING GRAMMAR-LEARNING::?THE-84 "the") (STRING GRAMMAR-LEARNING::?GRAY-80 "gray") (STRING GRAMMAR-LEARNING::?OBJECT-84 "object") (STRING GRAMMAR-LEARNING::?IS-84 "is") (STRING GRAMMAR-LEARNING::?WHAT-84 "what") (STRING GRAMMAR-LEARNING::?SHAPE-93 "shape"))

;non-overlapping-form-cxn  -correct
;((STRING GRAMMAR-LEARNING::?LARGE-55 "large"))

;overlapping-meaning-obs   - should be all
;NIL

;overlapping-meaning-cxn  -correct  -everything but large
;((CLEVR-WORLD:QUERY GRAMMAR-LEARNING::?TARGET-8 GRAMMAR-LEARNING::?SOURCE-10 GRAMMAR-LEARNING::?ATTRIBUTE-2) (CLEVR-WORLD:FILTER GRAMMAR-LEARNING::?TARGET-2 GRAMMAR-LEARNING::?TARGET-1 GRAMMAR-LEARNING::?COLOR-2) (UTILS:BIND CLEVR-WORLD:SHAPE-CATEGORY GRAMMAR-LEARNING::?SHAPE-8 CLEVR-WORLD:THING) (UTILS:BIND CLEVR-WORLD:ATTRIBUTE-CATEGORY GRAMMAR-LEARNING::?ATTRIBUTE-2 CLEVR-WORLD:SHAPE) (CLEVR-WORLD:FILTER GRAMMAR-LEARNING::?TARGET-1 GRAMMAR-LEARNING::?SOURCE-1 GRAMMAR-LEARNING::?SHAPE-8) (UTILS:BIND CLEVR-WORLD:COLOR-CATEGORY GRAMMAR-LEARNING::?COLOR-2 CLEVR-WORLD:GRAY) (CLEVR-WORLD:UNIQUE GRAMMAR-LEARNING::?SOURCE-10 GRAMMAR-LEARNING::?TARGET-39552) (CLEVR-WORLD:GET-CONTEXT GRAMMAR-LEARNING::?SOURCE-1))

;overlapping-form-obs -should be everything
;NIL

;overlapping-form-cxn -correct 
;((FCG:MEETS GRAMMAR-LEARNING::?WHAT-80 GRAMMAR-LEARNING::?SHAPE-89) (FCG:MEETS GRAMMAR-LEARNING::?IS-80 GRAMMAR-LEARNING::?WHAT-80) (FCG:MEETS GRAMMAR-LEARNING::?OBJECT-80 GRAMMAR-LEARNING::?IS-80) (FCG:MEETS GRAMMAR-LEARNING::?GRAY-76 GRAMMAR-LEARNING::?OBJECT-80) (FCG:MEETS GRAMMAR-LEARNING::?LARGE-55 GRAMMAR-LEARNING::?GRAY-76) (FCG:MEETS GRAMMAR-LEARNING::?THE-80 GRAMMAR-LEARNING::?LARGE-55) (STRING GRAMMAR-LEARNING::?SHAPE-89 "shape") (STRING GRAMMAR-LEARNING::?WHAT-80 "what") (STRING GRAMMAR-LEARNING::?IS-80 "is") (STRING GRAMMAR-LEARNING::?OBJECT-80 "object") (STRING GRAMMAR-LEARNING::?GRAY-76 "gray") (STRING GRAMMAR-LEARNING::?THE-80 "the"))



;;;SUBSTITUTION VARS GERMAN 
;die junge Frau gibt dem Mann den Apfel (cxn)
;die junge Frau gibt dem Mann den Hund (obs)

;non-overlapping-meaning-obs
;((GRAMMAR-LEARNING::DOG GRAMMAR-LEARNING::?D))

;non-overlapping-meaning-cxn
;((GRAMMAR-LEARNING::APPLE GRAMMAR-LEARNING::?D))

;non-overlapping-form-obs
;((STRING GRAMMAR-LEARNING::?HUND-10 "hund"))

;non-overlapping-form-cxn
;((STRING GRAMMAR-LEARNING::?APFEL-56 "apfel"))

;overlapping-form-cxn
;((FCG:MEETS GRAMMAR-LEARNING::?DEN-61 GRAMMAR-LEARNING::?APFEL-56) (FCG:MEETS GRAMMAR-LEARNING::?MANN-61 GRAMMAR-LEARNING::?DEN-61) (FCG:MEETS GRAMMAR-LEARNING::?DEM-61 GRAMMAR-LEARNING::?MANN-61) (FCG:MEETS GRAMMAR-LEARNING::?GIBT-61 GRAMMAR-LEARNING::?DEM-61) (FCG:MEETS GRAMMAR-LEARNING::?FRAU-61 GRAMMAR-LEARNING::?GIBT-61) (FCG:MEETS GRAMMAR-LEARNING::?JUNGE-61 GRAMMAR-LEARNING::?FRAU-61) (FCG:MEETS GRAMMAR-LEARNING::?DIE-61 GRAMMAR-LEARNING::?JUNGE-61) (STRING GRAMMAR-LEARNING::?DEN-61 "den") (STRING GRAMMAR-LEARNING::?MANN-61 "mann") (STRING GRAMMAR-LEARNING::?DEM-61 "dem") (STRING GRAMMAR-LEARNING::?GIBT-61 "gibt") (STRING GRAMMAR-LEARNING::?FRAU-61 "frau") (STRING GRAMMAR-LEARNING::?JUNGE-61 "junge") (STRING GRAMMAR-LEARNING::?DIE-61 "die"))

;overlapping-meaning-obs
;((GRAMMAR-LEARNING::GEBEN-01 GRAMMAR-LEARNING::?G) (GRAMMAR-LEARNING::YOUNG GRAMMAR-LEARNING::?Y) (GRAMMAR-LEARNING::WOMAN GRAMMAR-LEARNING::?W) (MOD GRAMMAR-LEARNING::?W GRAMMAR-LEARNING::?Y) (GRAMMAR-LEARNING::MAN GRAMMAR-LEARNING::?M) (GRAMMAR-LEARNING::ARG0 GRAMMAR-LEARNING::?G GRAMMAR-LEARNING::?W) (GRAMMAR-LEARNING::ARG1 GRAMMAR-LEARNING::?G GRAMMAR-LEARNING::?D) (GRAMMAR-LEARNING::ARG2 GRAMMAR-LEARNING::?G GRAMMAR-LEARNING::?M) (GRAMMAR-LEARNING::TOPICALIZED GRAMMAR-LEARNING::?W))

;overlapping-meaning-cxn
;((GRAMMAR-LEARNING::GEBEN-01 GRAMMAR-LEARNING::?G) (GRAMMAR-LEARNING::YOUNG GRAMMAR-LEARNING::?Y) (GRAMMAR-LEARNING::WOMAN GRAMMAR-LEARNING::?W) (MOD GRAMMAR-LEARNING::?W GRAMMAR-LEARNING::?Y) (GRAMMAR-LEARNING::MAN GRAMMAR-LEARNING::?M) (GRAMMAR-LEARNING::ARG0 GRAMMAR-LEARNING::?G GRAMMAR-LEARNING::?W) (GRAMMAR-LEARNING::ARG1 GRAMMAR-LEARNING::?G GRAMMAR-LEARNING::?D) (GRAMMAR-LEARNING::ARG2 GRAMMAR-LEARNING::?G GRAMMAR-LEARNING::?M) (GRAMMAR-LEARNING::TOPICALIZED GRAMMAR-LEARNING::?W))

;overlapping-form-obs
;((FCG:MEETS GRAMMAR-LEARNING::?DEN-66 GRAMMAR-LEARNING::?HUND-10) (FCG:MEETS GRAMMAR-LEARNING::?MANN-66 GRAMMAR-LEARNING::?DEN-66) (FCG:MEETS GRAMMAR-LEARNING::?DEM-66 GRAMMAR-LEARNING::?MANN-66) (FCG:MEETS GRAMMAR-LEARNING::?GIBT-66 GRAMMAR-LEARNING::?DEM-66) (FCG:MEETS GRAMMAR-LEARNING::?FRAU-66 GRAMMAR-LEARNING::?GIBT-66) (FCG:MEETS GRAMMAR-LEARNING::?JUNGE-66 GRAMMAR-LEARNING::?FRAU-66) (FCG:MEETS GRAMMAR-LEARNING::?DIE-66 GRAMMAR-LEARNING::?JUNGE-66) (STRING GRAMMAR-LEARNING::?DEN-66 "den") (STRING GRAMMAR-LEARNING::?MANN-66 "mann") (STRING GRAMMAR-LEARNING::?DEM-66 "dem") (STRING GRAMMAR-LEARNING::?GIBT-66 "gibt") (STRING GRAMMAR-LEARNING::?FRAU-66 "frau") (STRING GRAMMAR-LEARNING::?JUNGE-66 "junge") (STRING GRAMMAR-LEARNING::?DIE-66 "die"))
