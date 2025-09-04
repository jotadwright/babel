;; -------------------------------
;; + HOW TO GENERATE A SEED BANK +
;; -------------------------------

;; execute the following three lines in LW or SBCL
;; (ql:quickload :crs-conventionality)
;; (in-package :crs-conventionality)
;; (generate-seeds 100) ;; generates a seed bank of 100 seeds!

(in-package :crs-conventionality)

(defparameter *seed-path*
  (babel-pathname
   :directory `("experiments"
                "crs-conventionality"
                "batch")
   :name #+lispworks "lw-seeds" #+sbcl "sbcl-seeds"
   :type "store"))

(defun set-seed (idx)
  "Set the seed to the idx-th seed in the seed file."
  (cond ;; use a random seed (if idx is negative or nil)
        ((or (null idx) (< idx 0)) (setf *random-state* (make-random-state t)))
    
        ;; seed not possible
        ((>= idx 100) (error "Invalid seed provided (outside possible values)."))
    
        ;; get seed in seed-store
        (t
         (if (probe-file *seed-path*)
           (let* ((seeds (cl-store:restore *seed-path*))
                  (seed (aref seeds idx)))
             ;; sbcl can store seeds as integers
             ;; the integer is then used to create deterministically a 'random-state'
             #+sbcl (setf *random-state* (sb-ext:seed-random-state seed))
             ;; lispworks must store the random-state objects directly
             #+lispworks (setf *random-state* seed)) 
           (error "Seed file not found at ~S. To fix this:
                 1. Call (set-seed nil) with a new random state
                 2. Create a seed bank (see crs-conventionality/seed.lisp)
                 3. Specify the correct path to an existing seed bank"
                  *seed-path*)))))

(defun generate-seeds (amount)
  "Generate a list of random integer seeds and store them."
  (ensure-directories-exist (uiop:pathname-directory-pathname *seed-path*))
  (let ((seeds (make-array amount :element-type 'integer)))
    (loop for i from 0 below amount
          for seed = #+sbcl (random (expt 2 31)) #+lispworks (make-random-state t)
          do (setf (aref seeds i) seed))
    (cl-store:store seeds *seed-path*)))


