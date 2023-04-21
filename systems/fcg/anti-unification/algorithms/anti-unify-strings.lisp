(in-package :fcg)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Anti-unifying string ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun setf-matrix-row (matrix row array)
  "Copy array to row in matrix"
  (let ((columns (aops:ncol matrix)))
    (assert (= (length array) columns))     
    (loop for c from 0 below columns         
          for n across array                      
          do (setf (aref matrix row c) n)))
  matrix)

(defun setf-matrix-column (matrix column array)
  "Copy array to column in matrix"
  (let ((rows (aops:nrow matrix)))
    (assert (= (length array) rows))
    (loop for r from 0 below rows                 
          for n across array
          do (setf (aref matrix r column) n)))
   matrix)

(defgeneric needleman-wunsch (x y &key match mismatch gap)
  (:documentation "Computes the maximal alignment of two input strings x and y"))

(defmethod needleman-wunsch ((x list) (y list) &key (match 1) (mismatch 1)  (gap 1))
  (let* ((nx (length x))
         (ny (length y))
         (opt-score (make-array (list (+ nx 1) (+ ny 1)) :initial-element 0))
         (pointers-al (make-array (list (+ nx 1) (+ ny 1)) :initial-element 0)))
    (setf-matrix-column opt-score 0
                        (aops:linspace 0 (* nx gap) (+ nx 1))) 
    (setf-matrix-row opt-score 0
                     (aops:linspace 0 (* ny gap) (+ ny 1)))    
    (setf-matrix-column pointers-al 0
                        (aops:each-index i                          
                          (setf (aref pointers-al i 0) 3)))    
    (setf-matrix-row pointers-al 0
                     (make-array (list (aops:ncol pointers-al))
                                 :initial-element 4))                                               
    (let* ((temp-scores (make-array 3 :initial-element 0)))                                               
      (loop for i from 0 below nx                                                                         
            do (loop for j from 0 below ny
                     do (if (eql (nth i x) (nth j y))
                          (setf (aref temp-scores 0) (+ (aref opt-score i j) match))                      
                          (setf (aref temp-scores 0) (- (aref opt-score i j) mismatch)))                  
                        (setf (aref temp-scores 1) (- (aref opt-score i (+ j 1)) gap))                          
                        (setf (aref temp-scores 2) (- (aref opt-score (+ i 1) j) gap))
                        (let* ((tmax (reduce #'max (map 'list #'identity temp-scores))))                      
                          (setf (aref opt-score (+ i 1) (+ j 1)) tmax)                                      
                          (when (eql (aref temp-scores 0) tmax)
                            (setf (aref pointers-al (+ i 1) (+ j 1)) (+ 2 (aref pointers-al (+ i 1) (+ j 1)))))      
                          (when (eql (aref temp-scores 1) tmax)
                            (setf (aref pointers-al (+ i 1) (+ j 1)) (+ 3 (aref pointers-al (+ i 1) (+ j 1)))))      
                          (when (eql (aref temp-scores 2) tmax)
                            (setf (aref pointers-al (+ i 1) (+ j 1)) (+ 4 (aref pointers-al (+ i 1) (+ j 1)))))))))  
    (let* ((i nx)       
           (j ny)       
           (rx nil)     
           (ry nil)) 
      (loop while (or (> i 0) (> j 0))
            do (cond ((member (aref pointers-al i j) '(2 5 6 9))                                          
                      (push (nth (- i 1) x) rx)           
                      (push (nth (- j 1) y) ry)
                      (decf i 1)                               
                      (decf j 1))
                     ((member (aref pointers-al i j) '(3 5 7 9))             
                      (push (nth (- i 1) x) rx)                       
                      (push #\_ ry)
                      (decf i 1))
                     ((member (aref pointers-al i j) '(4 6 7 9))                 
                      (push #\_ rx)     
                      (push (nth (- j 1) y) ry)
                      (decf j 1))))
      (values rx ry))))

(defmethod needleman-wunsch ((x string) (y string) &key (match 1) (mismatch 1)  (gap 1))
  (needleman-wunsch (coerce x 'list) (coerce y 'list)
                    :match match :mismatch mismatch :gap gap))

(defun anti-unify-aligned-strings (pattern source)
  "Takes Needleman-Wunsch ALIGNED sequences and outputs a generalisation with bindings (deltas)"
  (let (generalisation pattern-delta source-delta
        pattern-delta-temp source-delta-temp flag)
    (loop for char-p in pattern  
          for char-s in source  
          if (eql char-p char-s)                 
          do (progn
               (when flag                        
                 (let ((var (make-var)))       
                   (push var generalisation)    
                   (push (cons var (reverse pattern-delta-temp)) pattern-delta) 
                   (setf pattern-delta-temp nil)                               
                   (push (cons var (reverse source-delta-temp)) source-delta)  
                   (setf source-delta-temp nil)
                   (setf flag nil)))                                           
               (unless (eql char-p #\_)                                      
                 (push char-p generalisation)))                                 
          else                                                                  
          do (progn
               (setf flag t)           
               (unless (eql char-p #\_)                                    
                 (push char-p pattern-delta-temp))                              
               (unless (eql char-s #\_)
                 (push char-s source-delta-temp)))                             
          finally
          (when flag              
            (let ((var (make-var)))                                     
              (push var generalisation)
              (push (cons var (reverse pattern-delta-temp)) pattern-delta) 
              (setf pattern-delta-temp nil)                                
              (push (cons var (reverse source-delta-temp)) source-delta)
              (setf source-delta-temp nil))))
    (values (reverse generalisation)  pattern-delta  source-delta)))


(defun generalisation-chars->strings (generalisation)
  (let (buffer result)
    (dolist (elem generalisation)
      (cond (; collect characters in the buffer
             (characterp elem)
             (push elem buffer))
            (; when reaching a variable
             (variable-p elem)
             ; turn the buffer into string
             ; and push to the result
             (when buffer
               (push (coerce (reverse buffer) 'string) result))
             (setf buffer nil)
             (push elem result))))
    (when buffer
      (push (coerce (reverse buffer) 'string) result))
    (reverse result)))


(defun anti-unify-strings (pattern source)
  "Anti-unify strings by (1) using needleman-wunsch to compute the
   maximal alignment, and (2) make a generalisation of overlapping characters
   and place the rest in delta's. The results are converted back to strings
   and the anti-unification cost corresponds to the number of elements in
   the delta's."
  (multiple-value-bind (pattern-alignment source-alignment)
      (needleman-wunsch pattern source)
    (multiple-value-bind (generalisation pattern-delta source-delta)
        (anti-unify-aligned-strings pattern-alignment source-alignment)
      (values (generalisation-chars->strings generalisation)
              (loop for (var . chars) in pattern-delta
                    collect (cons var (coerce chars 'string)))
              (loop for (var . chars) in source-delta
                    collect (cons var (coerce chars 'string)))
              ;; cost
              (+ (length pattern-delta)
                 (length source-delta))))))


(defun string-anti-unification-result->sequence-predicates (generalisation pattern-delta source-delta)
  "Convert the string anti-unification result to sequence predicates.
   Left and right boundaries are set such that the combination of the
   pattern/source delta and the generalisation is identical to the
   original pattern/source."
  (let (generalisation-predicates
        pattern-delta-predicates
        source-delta-predicates
        prev-was-variable-p)
    (dolist (elem generalisation)
      (cond ((stringp elem)
             (if prev-was-variable-p
               (let ((left-boundary (fourth (first pattern-delta-predicates))))
                 (push `(sequence ,elem ,left-boundary ,(make-var 'rb))
                       generalisation-predicates)
                 (setf prev-was-variable-p nil))
               (push `(sequence ,elem ,(make-var 'lb) ,(make-var 'rb))
                     generalisation-predicates)))
            ((variable-p elem)
             (let ((left-boundary (or (fourth (first generalisation-predicates))
                                      (make-var 'lb)))
                   (right-boundary (make-var 'rb))
                   (pattern-string (rest (assoc elem pattern-delta)))
                   (source-string (rest (assoc elem source-delta))))
               (push `(sequence ,pattern-string ,left-boundary ,right-boundary)
                     pattern-delta-predicates)
               (push `(sequence ,source-string ,left-boundary ,right-boundary)
                     source-delta-predicates)
               (setf prev-was-variable-p t)))))
    (values (reverse generalisation-predicates)
            (reverse pattern-delta-predicates)
            (reverse source-delta-predicates))))


#|

(multiple-value-bind (gen pd sd cost)
    (anti-unify-strings "What is the color of the sphere?" "What is the size of the cube?")
  (string-anti-unification-result->sequence-predicates gen pd sd))
 
=> ((SEQUENCE "What is the " #:?LB-18 #:?RB-65) (SEQUENCE " of the " #:?RB-66 #:?RB-67) (SEQUENCE "e?" #:?RB-68 #:?RB-69)),
   ((SEQUENCE "color" #:?RB-65 #:?RB-66) (SEQUENCE "spher" #:?RB-67 #:?RB-68)),
   ((SEQUENCE "size" #:?RB-65 #:?RB-66) (SEQUENCE "cub" #:?RB-67 #:?RB-68))

|#
