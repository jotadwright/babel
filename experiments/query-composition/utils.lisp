(in-package :qc)

; not empty
(defun notempty (list)
  (not (null list)))

;empty
(defun empty (list)
  (null list))

(defun change-type (val)
  "Function that change the type of returning value of database"
  (cond ((typep val 'double-float)
         (return-from change-type (write-to-string (round val)))))
  (cond ((typep val 'integer)
         (return-from change-type (write-to-string val))))
  (return-from change-type val))

(defun concat-array (array)
  (concatenate 'string "" (first array)))

(defun push-end (item lst)
  "Push at the end of a sequence an item and returning the sequence"
  (setf lst (append lst (list item))))

(defun is-table-present (name lst &key get-obj)
  (let ((result nil))
    (dolist (table lst)
      (if (equal name (name table))
        (if get-obj
          (setf result table)
          (setf result t))))
    result))

(defun sort-type (table answer)
  (let ((result '()))
    (dolist (val answer)
      (dolist (att (attributes table))
        (if (typep val (type-att att))
          (push att result))))
    result))