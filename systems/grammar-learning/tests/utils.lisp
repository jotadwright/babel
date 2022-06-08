(in-package :grammar-learning)

(defun set-up-cxn-inventory-and-repairs ()
  (wi::reset)
  (notify reset-monitors)
  (make-instance 'grammar-learning-experiment
                 :entries '((:observation-sample-mode . :debug) ;; random or sequential
                            (:determine-interacting-agents-mode . :corpus-learner)
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
                            (:remove-cxn-on-lower-bound . nil)
                            (:learner-th-connected-mode . :neighbours)
                            (:update-categorial-links . t)
                            (:use-meta-layer . t)
                            (:consolidate-repairs . t))))

(defun test-repair-status (class cipn)
  (test-assert (and
                (find 'fcg::succeeded (statuses cipn))
                (find class (statuses cipn)))))