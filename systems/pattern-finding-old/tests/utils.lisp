(in-package :pattern-finding-old)

(defun set-up-cxn-inventory-and-repairs ()
  (wi::reset)
  (notify reset-monitors)
  (make-instance 'pattern-finding-experiment
                 :entries `((:meaning-representation . :irl) 
                            (:mode . :testing))))

(defun test-repair-status (class cipn)
  (test-assert (and
                (find 'fcg::succeeded (statuses cipn))
                (find class (statuses cipn)))))