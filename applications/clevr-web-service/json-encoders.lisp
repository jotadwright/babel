;;;; json-encoders.lisp

(in-package :clevr-web-service)

;;;; IRL-program -> JSON
(defun encode-predicate (predicate predicates-w-bindings)
  `((:name . ,(downcase (mkstr (first predicate))))
    (:output . ,(second predicate))
    (:inputs . ,(if (member (first predicate) predicates-w-bindings)
                  (subseq predicate 2 (- (length predicate) 1))
                  (subseq predicate 2)))
    (:binding . ,(when (member (first predicate) predicates-w-bindings)
                    (last-elt predicate)))))

(defun encode-bind-statements (binding)
  `((:type . ,(downcase (mkstr (second binding))))
    (:var . ,(third binding))
    (:value . ,(downcase (mkstr (fourth binding))))))

(defun encode-irl-program (irl-program)
  "Encode an irl program into a json string"
  (let ((predicates
         (remove-if (lambda (p) (eql p 'bind))
                    irl-program :key #'first))
        (bind-statements
         (remove-if-not (lambda (p) (eql p 'bind))
                        irl-program :key #'first))
        (predicates-w-bindings '(equal? filter query relate same)))
    (encode-json-alist-to-string
     (list (cons :predicates
                 (loop for p in predicates
                       collect (encode-predicate p predicates-w-bindings)))
           (cons :bindings
                 (loop for b in bind-statements
                       collect (encode-bind-statements b)))))))

;;;; JSON -> IRL-program
(defun decode-predicate (a-list)
  (append
   (list (read-from-string (rest (assoc :name a-list))))
   (list (read-from-string (rest (assoc :output a-list))))
   (when (rest (assoc :inputs a-list))
     (mapcar #'read-from-string
             (rest (assoc :inputs a-list))))
   (when (rest (assoc :binding a-list))
     (list (read-from-string (rest (assoc :binding a-list)))))))

(defun decode-bind-statements (a-list)
  (list 'bind
        (read-from-string (rest (assoc :type a-list)))
        (read-from-string (rest (assoc :var a-list)))
        (read-from-string (rest (assoc :value a-list)))))

(defun decode-irl-program (a-list)
  "Decode an a-list into an irl-program"
  (append
   (loop for p in (rest (assoc :predicates a-list))
         collect (decode-predicate p))
   (loop for b in (rest (assoc :bindings a-list))
         collect (decode-bind-statements b))))

