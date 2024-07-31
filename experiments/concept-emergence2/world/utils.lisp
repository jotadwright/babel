(in-package :cle)


;; ---------------------
;; + World - load file +
;; ---------------------

(defmethod load-scene (fpath available-channels)
  "Load a scene from filepath."
  (let* ((dataset (fifth (pathname-directory fpath)))
         (split (seventh (pathname-directory fpath)))
         (s-expr (decode-json-as-alist-from-source fpath)))
    (s-expr->cle-scene s-expr
                       :dataset dataset
                       :dataset-split split
                       :available-channels available-channels)))

;; ---------------------
;; + World - get scene +
;; ---------------------
(defmethod get-scene-by-index ((world dataset-world) index)
  "Get a particular scene by its index."
  (assert (and (>= index 0) (< index (length (scenes world)))))
  (setf (current-scene world)
        (load-scene (nth index (scenes world))
                    (available-channels world)))
  (current-scene world))

(defmethod find-scene-by-name ((world dataset-world) name)
  "Get a scene by its name."
  (let ((fpath (find name (scenes world) :key #'namestring :test #'search)))
    (when fpath
      (setf (current-scene world) (load-scene fpath
                                              (available-channels world)))
      (current-scene world))))

(defmethod random-scene ((world dataset-world))
  "Choose a random scene and load it into memory."
  (setf (current-scene world) (load-scene (random-elt (scenes world))
                                          (available-channels world)))
  (current-scene world))

;; ------------------------
;; + World - actual utils +
;; ------------------------

#|(defmethod all-scenes ((world dataset-world))
  "Loads all scenes into memory and returns them in a flat list"
  (loop for fname in (scenes world)
        collect (load-scene fname)))

(defmethod do-for-scenes ((world dataset-world) fn &key shuffled)
  "Do function 'fn' for each scene. Stop when 'fn' returns NIL"
  ;; this could be a macro
  (loop for fname in (if shuffled (shuffle (scenes world)) (scenes world))
        for scene = (load-scene fname)
        for result = (funcall fn scene)
        unless result
        return nil))

(defun identical-shuffle (&rest lists)
  "Shuffle a number of lists in an identical manner"
  (assert
      (loop with len = (length (first lists))
            for list in lists
            always (= (length list) len)))
  (let* ((list-1 (first lists))
         (index (iota (length list-1)))
         (shuffled-index (shuffle index)))
    (apply #'values
           (loop for list in lists
                 collect (loop for i in shuffled-index
                               collect (nth i list))))))|#

(defun complete-digits (index)
  "Given an index (as string), prepend zeros
   until it is 6 digits long"
  ;; this can be done using a format statement
  (format nil "~6,'0d" index))