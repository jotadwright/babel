(in-package :qc)


;; #########################################
;; run
;; --------------------------------------------------------------------

(defun run (quest)
  (let* ((composer-obj-1 (make-instance 'query-composer))
         (composer-obj-2 (make-instance 'query-composer))
         (composer-obj-3 (make-instance 'query-composer))
         (composer-obj-4 (make-instance 'query-composer))
         (start-time (get-internal-real-time))
         (node-found nil))
    (write (query-associated quest))
    (terpri)
    ;;All sort
    (if (query (query-associated quest))
      (progn
        (setf node-found (compose-query composer-obj-1 (query (query-associated quest))
                                        :exclude-constraint t
                                        :sort-table t
                                        :star-shortcut t))
        (make-html-report composer-obj-1 quest (- (get-internal-real-time) start-time) node-found '("Exclude constraint" "Sort table" "Star shortcut"))
        ;;Sort-table & shortcut-sort
        (setf start-time (get-internal-real-time))
        (setf node-found (compose-query composer-obj-2 (query (query-associated quest))
                                        :sort-table t
                                        :star-shortcut t))
        (make-html-report composer-obj-2 quest (- (get-internal-real-time) start-time) node-found '("Sort table" "Star shortcut"))
        ;;Only short-cut sort
        (setf start-time (get-internal-real-time))
        (setf node-found (compose-query composer-obj-3 (query (query-associated quest))
                                        :star-shortcut t))
        (make-html-report composer-obj-3 quest (- (get-internal-real-time) start-time) node-found '("Star shortcut"))
        ;;None sort
        (setf start-time (get-internal-real-time))
        (setf node-found (compose-query composer-obj-4 (query (query-associated quest))))
        (make-html-report composer-obj-4 quest (- (get-internal-real-time) start-time) node-found '()))
      "No Answer")))
