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
    (populate-city 50)
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

(defun test-algo(quest test-number)
  (let ((start-time (get-internal-real-time))
         (node-found nil)
         (number 50)
         (results '()))
    (populate-city 50)
    (dotimes (total 10)
      (let ((current-result '()))
        (dotimes (iterate 2)
          (let ((composer-obj-1 (make-instance 'query-composer)))
            (setf start-time (get-internal-real-time))
            (setf node-found (compose-query composer-obj-1 (query (query-associated quest))
                                        :exclude-constraint t
                                        :sort-table t
                                        :star-shortcut t))
            
            (setf current-result (append current-result (list (- (get-internal-real-time) start-time)))))
            (setf node-found nil))
        (setf results (append results (list (list  (average current-result) number))))
        (setf current-result '())
        (setf number (+ number 50))
        (populate-city number)))
    (write-to-csv results (concatenate 'string  "C:/Users/Utilisateur/Documents/GitHub/babel/experiments/query-composition/" (format nil "~a" test-number) "-best.csv"))
    (populate-city 50)
    (setf current-result '())
    (setf number 50)
    (dotimes (total 10)
      (let ((current-result '()))
        (dotimes (iterate 2)
          (let ((composer-obj-1 (make-instance 'query-composer)))
            (setf start-time (get-internal-real-time))
            (setf node-found (compose-query composer-obj-1 (query (query-associated quest))))   
            (setf current-result (append current-result (list (- (get-internal-real-time) start-time)))))
            (setf node-found nil))
        (setf results (append results (list (list  (average current-result) number))))
        (setf current-result '())
        (setf number (+ number 50))
        (populate-city number)))
    (write-to-csv results (concatenate 'string  "C:/Users/Utilisateur/Documents/GitHub/babel/experiments/query-composition/" (format nil "~a" test-number) "-worse.csv"))))

(defun populate-city (number)
  (let ((data '())
        (id-countries (flatten (query "SELECT id FROM country ORDER BY id ASC"))))
    (dotimes (i number)
      (let ((row '()))
        (push (concatenate 'string "city" (write-to-string i)) row)
        (push (random-between 300 8000000) row)
        (push (random-between 500 10000) row)
        (push nil row)
        (push nil row)
        (push (random-between (first id-countries) (first (last id-countries))) row)
        (push row data)))
    ;;Drop and create table
    (execute "DROP TABLE IF EXISTS city CASCADE")
    (execute (dao-table-definition 'city))
    ;;insert data into Database
    (query (:insert-rows-into 'city :columns 'countryid 'iscapital 'isprimary 'size 'population 'name
            :values data))))

(defun write-to-csv (data filename)
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (dolist (row data)
      (format stream "~{~a~^,~}~%" row))))