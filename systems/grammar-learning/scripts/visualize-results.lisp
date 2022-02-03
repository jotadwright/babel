(ql:quickload :clevr-grammar-learning)
(in-package :clevr-grammar-learning)

(defun summarize-cxn-types (cxn-inventory)
  (let ((holophrase-cxns (sort (find-all 'gl::holophrase (constructions-list cxn-inventory)
                                         :key #'get-cxn-type)  #'> :key (lambda (cxn) (attr-val cxn :score))))
        (lexical-cxns (sort (find-all 'gl::lexical (constructions-list cxn-inventory)
                                      :key #'get-cxn-type)  #'> :key (lambda (cxn) (attr-val cxn :score))))
        (item-based-cxns (sort (find-all 'gl::item-based (constructions-list cxn-inventory)
                                         :key #'get-cxn-type)  #'> :key (lambda (cxn) (attr-val cxn :score)))))
        
    (add-element `((h2) ,(format nil "Holophrases: ~a" (length holophrase-cxns))))
    ;(loop for cxn in holophrase-cxns
    ;      do (add-element (make-html cxn)))
    (add-element '((hr)))
    (add-element `((h2) ,(format nil "Lexical cxns: ~a" (length lexical-cxns))))
    ;(loop for cxn in lexical-cxns
    ;      do (add-element (make-html cxn)))
    (add-element '((hr)))
    (add-element `((h2) ,(format nil "Item-based cxns: ~a" (length item-based-cxns))))
    ;(loop for cxn in item-based-cxns
    ;      do (add-element (make-html cxn)))
    (add-element '((hr)))
    (add-element '((hr)))))


(defun failure-analysis (error-file cxn-inventory)
  (activate-monitor trace-fcg)
  (loop for (utterance meaning) in error-file
        do (comprehend utterance :gold-standard-meaning meaning :cxn-inventory cxn-inventory))
  (deactivate-monitor trace-fcg))