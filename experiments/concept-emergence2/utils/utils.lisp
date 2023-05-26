(in-package :cle)

;; ---------------------------
;; + Web monitor experiments +
;; ---------------------------
(defun read-scene-ids (fname)
  (let* ((base-dir "~/Projects/babel/experiments/concept-emergence2/data/")
         (fpath (concatenate 'string base-dir fname))
         (raw (uiop:read-file-lines fpath))
         (scene-ids (map 'list #'parse-integer raw)))
    scene-ids))

(defun first-n (n list)
  "Returns the first N elements of the LIST."
  (butlast list (max (- (list-length list) n) 0)))