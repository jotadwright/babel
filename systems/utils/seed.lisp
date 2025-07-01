(in-package :utils)

(defparameter *seed-path*
  (babel-pathname
   :name #+lispworks "lw-seeds" #+sbcl "sbcl-seeds"
   :type "store"))

(export '(set-seed generate-seeds))

(defun set-seed (idx)
  "Set the seed to the idx-th seed in the seed file."
  (cond ;; use a random seed (if idx is negative or nil)
        ((or (null idx) (< idx 0)) (setf *random-state* (make-random-state t)))
        ;; get seed in seed-store
        (t
         (if (probe-file *seed-path*)
           (let ((seeds (cl-store:restore *seed-path*)))
             (if (< idx (length seeds))
               (let ((seed (aref seeds idx)))
                 ;; sbcl can store seeds as integers
                 ;; the integer is then used to create deterministically a 'random-state'
                 #+sbcl (setf *random-state* (sb-ext:seed-random-state seed))
                 ;; lispworks must store the random-state objects directly
                 #+lispworks (setf *random-state* seed))
               (error "Invalid seed provided (outside possible values).")))
           (error "Seed file not found at ~S. To fix this:
                 1. Call (set-seed nil) with a new random state
                 2. Create a seed bank (see generate-seeds in seeds.lisp)
                 3. Specify the correct path to an existing seed bank"
                  *seed-path*)))))

;; ------------------
;; + Generate seeds +
;; ------------------
(defun generate-seeds (amount &key (overwrite nil))
  (if (or (probe-file *seed-path*) overwrite)
    (error "Seed file already exists at ~a. Are you sure you want to overwrite the seed bank? Call generate-seeds with :overwrite t if that is what you want." *seed-path*)
    (loop for i from 1 to amount
          for seed = (make-random-state t)
          collect seed into seeds
          finally (cl-store:store (list->array seeds) *seed-path*))))

;; the seed file contains n seeds (in an array)
;; (ql:quickload :utils)
;; (generate-seeds 100)

;; test
#|
(progn
  (set-seed 1)
  (loop for i from 1 to 5
        collect (random 100) into lst
        finally (format t "~% ~a" lst))
  )
|#

