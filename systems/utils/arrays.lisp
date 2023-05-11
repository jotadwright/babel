(in-package :utils)

(export '(setf-matrix-row setf-matrix-column array-map))

(defun setf-matrix-row (matrix row array)
  "Copy array to row in matrix"
  (assert (= (length (array-dimensions matrix)) 2))
  (let ((columns (array-dimension matrix 1)))
    (assert (= (length array) columns))
    (loop for c from 0 below columns         
          for n across array                      
          do (setf (aref matrix row c) n)))
  matrix)

(defun setf-matrix-column (matrix column array)
  "Copy array to column in matrix"
  (assert (= (length (array-dimensions matrix)) 2))
  (let ((rows (array-dimension matrix 0)))
    (assert (= (length array) rows))
    (loop for r from 0 below rows                 
          for n across array
          do (setf (aref matrix r column) n)))
   matrix)

(defun array-map (function array
                  &optional (resulting-array (make-array (array-dimensions array))))
  "Apply FUNCTION to each element of ARRAY.
   Return a new array, or write into the optional 3rd argument."
  (dotimes (i (array-total-size array) resulting-array)
    (setf (row-major-aref resulting-array i)
          (funcall function (row-major-aref array i)))))