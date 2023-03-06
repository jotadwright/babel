(in-package :grammar-learning)

(defun set-up-cxn-inventory-and-repairs ()
  (wi::reset)
  (notify reset-monitors)
  (make-instance 'grammar-learning-experiment
                 :entries '((:observation-sample-mode . :debug) ;; random or sequential
                            (:determine-interacting-agents-mode . :corpus-learner)
                            (:max-nr-of-nodes . 300)
                            (:de-render-mode . :de-render-string-meets-no-punct)
                            (:remove-cxn-on-lower-bound . nil)
                            (:learner-th-connected-mode . :neighbours)
                            (:update-categorial-links . t)
                            (:use-meta-layer . t)
                            (:consolidate-repairs . t))))

(defun set-up-cxn-inventory-and-repairs-amr ()
  (wi::reset)
  (notify reset-monitors)
  (make-instance 'grammar-learning-experiment
                 :entries '((:observation-sample-mode . :debug) ;; random or sequential
                            (:determine-interacting-agents-mode . :corpus-learner)
                            (:de-render-mode . :de-render-string-meets-ignore-quotes+full-stops)
                            (:meaning-representation . :amr)
                            (:max-nr-of-nodes . 300)
                            (:remove-cxn-on-lower-bound . nil)
                            (:learner-th-connected-mode . :neighbours)
                            (:update-categorial-links . t)
                            (:use-meta-layer . t)
                            (:consolidate-repairs . t))))


(defun set-up-cxn-inventory-and-repairs-geo ()
  (wi::reset)
  (notify reset-monitors)
  (make-instance 'grammar-learning-experiment
                          :entries '((:repairs . (add-categorial-links
                                                  ;item-based->item-based--substitution
                                                  ;item-based->holistic
                                                  ;holistic->item-based--substitution
                                                  holistic->item-based--addition
                                                  holistic->item-based--deletion
                                                  ;holistic->item-based
                                                  nothing->holistic))
                                     (:observation-sample-mode . :debug)
                                     (:meaning-representation . :geo)
                                     (:max-nr-of-nodes . 300)
                                     (:cxn-decf-score . 0.2)
                                     (:cxn-incf-score . 0.1)
                                     (:alignment-strategy . :lateral-inhibition)
                                     (:de-render-mode . :de-render-string-meets-no-punct)
                                     )))


(defun set-up-cxn-inventory-and-repairs-german ()
  (wi::reset)
  (notify reset-monitors)
  (eval `(make-instance 'grammar-learning-experiment
                          :entries '((:repairs . (add-categorial-links
                                                  item-based->item-based--substitution
                                                  item-based->holistic
                                                  holistic->item-based--substitution
                                                  holistic->item-based--addition
                                                  holistic->item-based--deletion
                                                  holistic->item-based
                                                  nothing->holistic))
                                     (:observation-sample-mode . :debug)    ;:train shuffled 
                                     (:meaning-representation . :geo) ;geo
                                     (:max-nr-of-nodes . 300)
                                     (:alignment-strategy . :lateral-inhibition)
                                     (:de-render-mode . :de-render-string-meets-no-punct)
                                     (:corpus-files-root . ,(merge-pathnames
                                                             (make-pathname :directory '(:relative "German-cases"))
                                                             cl-user:*babel-corpora*))
                                     (:corpus-data-file . ,(make-pathname
                                                            :name "German-cases-mod-1000" :type "json"))))))

(defun test-repair-status (class cipn)
  (test-assert (and
                (find 'fcg::succeeded (statuses cipn))
                (find class (statuses cipn)))))