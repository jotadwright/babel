(in-package :au-benchmark.base)

(export '(shuffle
          random-from-range
          random-elt
          find-all
          find-all-anywhere-if
          mapconcat
          mappend
          permutation-of?
          group-by
          compose
          the-biggest
          the-smallest
          all-subsets
          next-au-var))

(defun shuffle (l)
  (let ((vector (coerce l 'simple-vector)))
    (loop for i of-type fixnum from (length vector) downto 2
          do (rotatef (svref vector (1- i)) (svref vector (random i))))
    (coerce vector 'list)))

(defun random-from-range (start end)
  "Generate a random integer/float in the range [start,end["
  (if (= start end)
    start
    (+ start (random (- end start)))))

(defun random-elt (l)
  (declare (list l))
  (unless (null l)
    (elt l (random (length l) #+ccl(make-random-state t)))))

(defun find-all (what lst &key (test #'eq) (key #'identity))
  (declare (function test) (function key))
  (loop for el in lst when (funcall test what (funcall key el)) collect el))

(defun find-all-anywhere-if (test tree &key (key #'identity))
  (declare (function test))
  (cond ((funcall test (funcall key tree)) (list tree))
        ((consp tree)
         (append (find-all-anywhere-if test (car tree) :key key)
                 (find-all-anywhere-if test (cdr tree) :key key)))))

(defun mapconcat (function sequence separator)
  "mapconcat applies function to each element of sequence;
   the results, which must be sequences of characters (strings, vectors, or lists),
   are concatenated into a single string return value.
   Between each pair of result sequences, mapconcat inserts the characters
   from separator, which also must be a string, or a vector or list of characters.
   The argument function must be a function that can take one argument and
   returns a sequence of characters: a string, a vector, or a list.
   The argument sequence can be any kind of sequence except a char-table;
   that is, a list, a vector, a bool-vector, or a string."
  (format nil (format nil "~~{~~a~~^~a~~}" separator)
          (mapcar function sequence)))

(defun mappend (fn &rest lists)
  "Append the results of calling fn on each element of list"
  (declare (dynamic-extent lists))
  (loop for result in (apply #'mapcar fn lists) append result))

(defun permutation-of? (l1 l2 &key (key #'identity) (test #'eql))
  #-lispworks
  (declare (list l1) (list l2) (function key) (function test))
  (and (= (length l1) (length l2))
       (let ((fail nil)
	     (cl2 (copy-list l2)))
	 (loop for el1 in l1 until fail do
	       (let ((m (member (funcall key el1) cl2 :key key :test test)))
		 (if m 
		     (if (cdr m)
			 (setf (car m) (cadr m)
			       (cdr m) (cddr m))
		       (setf cl2 (reverse (rest (reverse cl2)))))
		   (setq fail t))))
	 (not fail))))

(defun group-by (sequence group-fn &key (test #'eql))
  "applies fn to each elem of sequence. the elems for which fn
   yields the same value are grouped together. use :test to
   compare the results of applying fn to each elem."
  (loop with groups = nil
        for elem in sequence
        for key = (funcall group-fn elem)
        if (assoc key groups :test test)
        do (push elem (cdr (assoc key groups :test test)))
        else
        do (push (cons key (list elem)) groups)
        finally
        (return groups)))

(defun compose (&rest functions)
  "Given an arbitrary number of functions
   (e.g. f, g and h), return a function F
   such that (F x) = (f (g (h x)))"
  (if (= (length functions) 1)
      (car functions)
      (lambda (&rest args)
        (funcall (first functions)
                 (apply (apply #'compose (rest functions))
                        args)))))

(defun the-biggest (fn l &optional cut-of)
  (declare (list l) (function fn))
  (when l
    (let ((biggest (list (first l)))
	  (best-val (funcall fn (first l))))
      (loop for x in (rest l) 
            until (and cut-of (>= best-val cut-of))
            do (let ((val (funcall fn x)))
                 (cond ((= val best-val)
                        (push x biggest))
                       ((> val best-val)
                        (setf (car biggest) x)
                        (setf (cdr biggest) nil)
                        (setq best-val val)))))
      (values (random-elt biggest) best-val))))

(defun the-smallest (fn l)
  (declare (function fn) (list l))
  (the-biggest (compose #'- fn) l))

(defun bin-list (n)
  "Convert the integer n to its binary representation in list format,
   e.g. 0 = (0); 1 = (1); 2 = (1 0); 3 = (1 1); etc."
  (labels ((bin-list-aux (n)
             (cond ((< n 1) '(0))
                   ((= n 1) '(1))
                   ((zerop (mod n 2)) (cons 0 (bin-list-aux (/ n 2))))
                   (t (cons 1 (bin-list-aux (floor (/ n 2))))))))
    (reverse (bin-list-aux n))))

(defun all-subsets (list &key (min-length 1) max-length)
  "There are 2^n subsets of a list of length n.
   Iterate from 0 to (2^n)-1 and represent this number in binary format.
   The binary format has the same length as the list itself.
   Loop over the binary format and the list simultaneously.
   If a 1 appears in the n'th position, we take the n'th element from the list
   for the current subset. Otherwise, we leave it and continue."
  (let ((subset-len (- (expt 2 (length list)) 1))
        (list-len (length list))
        (reverse-list (reverse list))
        all-subsets)

    (loop for len from 0 to subset-len
          for len-binary = (bin-list len)
          for num-elems = (count 1 len-binary)
          when (and (> num-elems 0)
                    (>= num-elems min-length)
                    (<= num-elems (or max-length list-len)))
          do (loop with subset = nil
                   for elem in reverse-list
                   for bit in (reverse len-binary)
                   when (= bit 1)
                   do (push elem subset)
                   finally (push subset all-subsets))
          finally (return all-subsets))))


;;;; Jens - 04/01/2024
;;;; The global variable *alpahbet* is implemented as a circular list
;;;; by having the cdr of the last cons-cell in the list point to the
;;;; start of the list instead of to NIL. This allows to cycle through
;;;; the alphabet by each time calling 'pop'. However, this makes it
;;;; impossible to print the variable *alphabet* (as it would be
;;;; infinitely long). So do not try this, unless you want LispWorks
;;;; to be blocked...
;;;; You _can_ print *alphabet* when setting the system variable
;;;; *print-circle* to T, but this has other effects that are not
;;;; desirable. For instance, the predicate (F ?B ?B) would be printed
;;;; as (F #1=?B #1#). 

(defun make-circular-list (list)
  ;(setf *print-circle* t)
  (setf (cdr (last list)) list)
  list)

(defparameter *alphabet*
  (make-circular-list
   (mapcar #'(lambda (s) (format nil "~a" s))
           '(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z))))

(defun next-au-var ()
  (pop *alphabet*))