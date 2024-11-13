(in-package :cle)

(defparameter *seed-path*
  (babel-pathname
   :directory `("experiments"
                "concept-emergence2"
                "batch")
   :name #+lispworks "lw-seeds" #+sbcl "sbcl-seeds"
   :type "store"))

(defun set-seed (idx)
  "Set the seed to the idx-th seed in the seed file."
  (cond ;; use a random-seed (if idx is negative or nil)
        ((or (null idx) (< idx 0)) (setf *random-state* (make-random-state t)))
        ;; seed not possible
        ((>= idx 100) (error "Invalid seed provided (outside possible values)."))
        ;; get seed in seed-store
        (t
         (let* ((seeds (cl-store:restore *seed-path*))
                (seed (aref seeds idx)))
           (setf *random-state* seed)))))

;; ------------------
;; + Generate seeds +
;; ------------------
(defun generate-seeds (amount)
  (loop for i from 1 to amount
        for seed = (make-random-state t)
        collect seed into seeds
        finally (cl-store:store (list->array seeds) *seed-path*)))

;; the seed file contains 100 seeds (in an array)
;; (ql:quickload :cle)
;; (generate-seeds 100)

;; examples
#|
(progn
  (set-seed 1)
  (loop for i from 1 to 5
        collect (random 100) into lst
        finally (format t "~% ~a" lst))
  )
|#