(in-package :visual-dialog)

(defun update-memory-query (irl-program solutions source-value-set target-value new-object-set)
  "updates history when question type is query"
  "adds attributes of the queried object and also searches for relate primitive if that exists"
  (let* ((attribute-value (attribute (get-fourth-value-target-primitive irl-program (first solutions))))
         (source-value (first (objects source-value-set)))
         (object-in-memory (if source-value
                             (find (id source-value) (objects new-object-set) :test #'equal :key #'id)))
         (category-and-attribute (cons (intern (symbol-name attribute-value) "KEYWORD")
                                       (if (numberp (slot-value target-value attribute-value))
                                         (internal-symb (upcase (format nil "~r" (slot-value target-value attribute-value))))
                                         (slot-value target-value attribute-value)))))
    (if (not (equal (id target-value) 'none))
      (if object-in-memory
        (progn 
          (if (symbolp (attributes object-in-memory))
            (setf (attributes object-in-memory) (list category-and-attribute))
            (push category-and-attribute (attributes object-in-memory))))
        (progn
          (let ((new-object (make-instance 'object :id (id source-value)
                                           :attributes (list category-and-attribute))))
            (push new-object (objects new-object-set))))))
    new-object-set))

(defun update-memory-count-or-exist (irl-program target-primitive source-value new-object-set solutions last-set)
  "updates history when question type is count or exist"
  "sets mentioned object of the queried object to T"
  (let* ((id-list (loop for obj in (objects new-object-set)
                        collect (id obj)))
         (source-variable (third (find target-primitive irl-program :test #'equal :key #'first)))
         (target-variable (second (find target-primitive irl-program :test #'equal :key #'first)))
         (mnist (if (find 'digit-category irl-program :test #'equal :key #'second)
                  t nil))
         (other-objects (if (find 'set-diff irl-program :test #'equal :key #'first)
                  t nil))
         attributes)
    "add objects from target set in new-object-set, if they are not yet in there"
    (loop for object in (objects (object-set (first (set-items source-value))))
              do (if (not (member (id object) id-list))
                   (push (make-instance 'object :id (id object))
                         (objects new-object-set))))
    "find attributes but when set-diff is in irl-program; don't add attributes"
    (if other-objects
      ;in case of mnist, attribute needs to found as input of set-diff instead of output
      (if mnist
        (progn
          (setf var (third (find 'set-diff irl-program :test #'equal :key #'first)))
          (setf attributes (find-input-attributes-of-set-diff irl-program var))))
      ;otherwise, find attributes
      (setf attributes (find-attributes-of-unique irl-program target-variable)))
    "add attributes to objects"
    (if (equal (length (objects (object-set (first (set-items source-value))))) 1)
      (if attributes
        (loop for object in (objects (object-set (first (set-items source-value))))
              for object-in-memory = (find (id object) (objects new-object-set) :key #'id)
              do (if (attributes object-in-memory)
                   (loop for attr in attributes
                         do (push attr (attributes object-in-memory)))
                   (setf (attributes object-in-memory) attributes)))))
    new-object-set))