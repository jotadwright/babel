(in-package :visual-dialog)

;; ################################
;; ontology-related utils
;; ################################

(defun collect-relations-for-object (all-relationships index)
  "Collect the spatial relationships for a single object for CLEVR"
  (loop for (key . list-of-lists) in all-relationships
        for relationship = (internal-symb (upcase (mkstr key)))
        collect (cons relationship (nth index list-of-lists))))

(defun key->symbol (s-expr key)
  (internal-symb
   (upcase
    (mkstr
     (rest
      (assoc key s-expr))))))

;; ################################
;; load-objects
;; ################################

#|(defmethod load-object ((type (eql 'scene)) filename &key dataset)
  "Load a scene from file"
  (let* ((s-expr (decode-json-from-source filename)))
    (if (eq dataset 'clevr)
      (s-expr->object 'clevr-scene s-expr :directory filename :dataset dataset)
      (s-expr->object 'mnist-scene s-expr :directory filename :dataset dataset))))|#

(defmethod load-object ((type (eql 'clevr-scene)) filename &key dataset)
  "Load a scene from file"
  (let* ((s-expr (decode-json-from-source filename)))
      (s-expr->object 'clevr-scene s-expr :directory filename :dataset dataset)))

(defmethod load-object ((type (eql 'gqa-scene)) filename &key dataset)
  "Load a scene from file"
  (let* ((s-expr (decode-json-from-source filename)))
      (s-expr->object 'gqa-scene s-expr :directory filename :dataset dataset)))

(defmethod load-object ((type (eql 'mnist-scene)) filename &key dataset)
  "Load a scene from file"
  (let* ((s-expr (decode-json-from-source filename)))
      (s-expr->object 'mnist-scene s-expr :directory filename :dataset dataset)))

#|(defmethod load-object ((type (eql 'dialogs)) filename  &key)
  "Load the dialogs from file"
  (let* ((s-expr (decode-json-from-source filename)))
     (s-expr->object 'clevr-scene s-expr :directory filename )))|#

(defmethod load-object ((type (eql 'clevr-dialog-set)) filename &key)
  "Load a dialog-set from file"
  (let ((s-expr (decode-json-from-source filename)))
      (s-expr->object 'clevr-dialog-set s-expr :directory filename)))

(defmethod load-object ((type (eql 'gqa-dialog-set)) filename &key)
  "Load a dialog-set from file"
  (let* ((s-expr (decode-json-from-source filename))
         (s-expr (if (= (length s-expr) 1) (first s-expr) s-expr)))
      (s-expr->object 'gqa-dialog-set s-expr :directory filename)))

(defmethod load-object ((type (eql 'mnist-dialog-set)) filename &key)
  "Load a dialog-set from file"
  (let ((s-expr (decode-json-from-source filename)))
      (s-expr->object 'mnist-dialog-set s-expr :directory filename)))

;; ################################
;; s-expr->object
;; ################################

(defmethod s-expr->object ((type (eql 'clevr-object)) s-expr
                           &key relationships id-dict-entry id-dict)
  "Create an instance of 'object from an s-expression"
    (make-instance 'object :id (cdr id-dict-entry)
                   :attributes (list (cons :shape (key->symbol s-expr :shape)) 
                                     (cons :size (key->symbol s-expr :size))
                                     (cons :color (key->symbol s-expr :color))
                                     (cons :material (key->symbol s-expr :material)))
                   :relationships relationships
                   :coordinates (rest (assoc :pixel--coords s-expr))
                   :rotation (rest (assoc :rotation s-expr))))

(defmethod s-expr->object ((type (eql 'mnist-object)) s-expr
                           &key relationships id-dict-entry id-dict)
  "Create an instance of 'object from an s-expression"
    (make-instance 'object :id (cdr id-dict-entry)
                   :attributes (list (cons :bgcolor (internal-symb (upcase (format nil "~a-bg" (key->symbol s-expr :bgcolor)))))
                                     (cons :style (key->symbol s-expr :style))
                                     (cons :color (key->symbol s-expr :color))
                                     (cons :digit ;(rest (assoc :number s-expr))
                                           (internal-symb (upcase (format nil "~r" (parse-integer (string (key->symbol s-expr :number))))))))
                   :relationships relationships
                   :coordinates (rest (assoc :pixel--coords s-expr))
                   :rotation (rest (assoc :rotation s-expr))))

(defmethod s-expr->object ((type (eql 'gqa-object)) s-expr
                           &key relationships id-dict-entry id-dict)
  "Create an instance of 'object from an s-expression"
    (make-instance 'object :id (cdr id-dict-entry)
                   :attributes (collect-attributes s-expr)
                   :relationships relationships
                   :coordinates (rest (assoc :pixel--coords s-expr))
                   :rotation (rest (assoc :rotation s-expr))))

(defun collect-attributes (s-expr)
  (loop for attr in s-expr
          collect (cons (intern (upcase (format nil "GQA-~a" (symbol-name (car attr)))) "KEYWORD")
                        (key->gqa-symbol s-expr (car attr)))))

(defun key->gqa-symbol (s-expr key)
  (internal-symb
   (upcase
    (format nil "gqa-~a"
     (rest
      (assoc key s-expr))))))

(defmethod s-expr->object ((type (eql 'clevr-scene)) s-expr
                           &key directory filename dataset)
  "Create an instance of clevr-scene from an s-expression"
  (let* ((all-objects (rest (assoc :objects s-expr)))
         (all-relationships (rest (assoc :relationships s-expr)))
         (id-dict (loop for i from 0 below (length all-objects)
                        collect (cons i (make-id 'obj))))
         ;(data-set (last-elt (pathname-directory directory)))
         (data-set (rest (assoc :split s-expr)))
         (image-filename-and-type (split (rest (assoc :image--filename s-expr)) #\.))
         (img-filename (first image-filename-and-type))
         (img-filetype (second image-filename-and-type)))
    ;(print all-objects)
    (make-instance 'scene
                   :index (rest (assoc :image--index s-expr))
                   :source-path directory
                   :name img-filename
                   :data-set data-set
                   :image (merge-pathnames
                           (make-pathname :directory `(:relative "images" ,data-set)
                                          :name img-filename :type img-filetype)
                           *clevr-data-path*)
                   :objects (loop for object in all-objects
                                  for (index . id) in id-dict
                                  for object-relationships = (collect-relations-for-object all-relationships index)
                                  collect (s-expr->object 'clevr-object object
                                                          :relationships  (loop for (relation . list-of-ids) in object-relationships
                                                                                              collect (cons relation
                                                                                                            (loop for id in list-of-ids
                                                                                                                  collect (rest (assoc id id-dict)))))
                                                          :id-dict-entry (cons index id)
                                                          :id-dict id-dict)))))

(defmethod s-expr->object ((type (eql 'gqa-scene)) s-expr
                           &key directory filename dataset)
  "Create an instance of clevr-scene from an s-expression"
  (let* ((all-objects (rest (assoc :objects s-expr)))
         (all-relationships (rest (assoc :relationships s-expr)))
         (id-dict (loop for i from 0 below (length all-objects)
                        collect (cons i (make-id 'obj))))
         ;(data-set (last-elt (pathname-directory directory)))
         (data-set (rest (assoc :split s-expr)))
         (image-filename-and-type (split (rest (assoc :image--filename s-expr)) #\.))
         (img-filename (first image-filename-and-type))
         (img-filetype (second image-filename-and-type)))
    ;(print all-objects)
    (make-instance 'scene
                   :index (rest (assoc :image--index s-expr))
                   :source-path directory
                   :name img-filename
                   :data-set data-set
                   :image (merge-pathnames
                           (make-pathname :directory `(:relative "images" ,data-set)
                                          :name img-filename :type img-filetype)
                           *gqa-data-path*)
                   :objects (loop for object in all-objects
                                  for (index . id) in id-dict
                                  for object-relationships = (collect-relations-for-object all-relationships index)
                                  collect (s-expr->object 'gqa-object object
                                                          :relationships  (loop for (relation . list-of-ids) in object-relationships
                                                                                              collect (cons relation
                                                                                                            (loop for id in list-of-ids
                                                                                                                  collect (rest (assoc id id-dict)))))
                                                          :id-dict-entry (cons index id)
                                                          :id-dict id-dict)))))

(defmethod s-expr->object ((type (eql 'mnist-scene)) s-expr
                           &key directory dataset)
  "Create an instance of an mnist-scene from an s-expression"
  (let* ((first-row-objects (first s-expr))
         (second-row-objects (second s-expr))
         (third-row-objects (third s-expr))
         (fourth-row-objects (fourth s-expr))
         (objects (append first-row-objects second-row-objects third-row-objects fourth-row-objects))         
         (id-dict (loop for i from 0 below (length objects)
                        collect (cons i (make-id 'obj))))
         (all-relationships 
          (cons :relations
                (list (cons :right (list  (list (rest (assoc '0 id-dict)) (rest (assoc '1 id-dict)))
                                          (list (rest (assoc '1 id-dict)) (rest (assoc '2 id-dict)))
                                          (list (rest (assoc '2 id-dict)) (rest (assoc '3 id-dict)))
                                          (list (rest (assoc '4 id-dict)) (rest (assoc '5 id-dict)))
                                          (list (rest (assoc '5 id-dict)) (rest (assoc '6 id-dict)))
                                          (list (rest (assoc '6 id-dict)) (rest (assoc '7 id-dict)))
                                          (list (rest (assoc '8 id-dict)) (rest (assoc '9 id-dict)))
                                          (list (rest (assoc '9 id-dict)) (rest (assoc '10 id-dict)))
                                          (list (rest (assoc '10 id-dict)) (rest (assoc '11 id-dict)))
                                          (list (rest (assoc '12 id-dict)) (rest (assoc '13 id-dict)))
                                          (list (rest (assoc '13 id-dict)) (rest (assoc '14 id-dict)))
                                          (list (rest (assoc '14 id-dict)) (rest (assoc '15 id-dict)))))
                      (cons :below (list  (list (rest (assoc '0 id-dict)) (rest (assoc '4 id-dict)))
                                          (list (rest (assoc '4 id-dict)) (rest (assoc '8 id-dict)))
                                          (list (rest (assoc '8 id-dict)) (rest (assoc '12 id-dict)))
                                          (list (rest (assoc '1 id-dict)) (rest (assoc '5 id-dict)))
                                          (list (rest (assoc '5 id-dict)) (rest (assoc '9 id-dict)))
                                          (list (rest (assoc '9 id-dict)) (rest (assoc '13 id-dict)))
                                          (list(rest (assoc '2 id-dict)) (rest (assoc '6 id-dict)))
                                          (list (rest (assoc '6 id-dict)) (rest (assoc '10 id-dict)))
                                          (list (rest (assoc '10 id-dict)) (rest (assoc '14 id-dict)))
                                          (list (rest (assoc '3 id-dict)) (rest (assoc '7 id-dict)))
                                          (list (rest (assoc '7 id-dict)) (rest (assoc '11 id-dict)))
                                          (list (rest (assoc '11 id-dict)) (rest (assoc '15 id-dict))))))))
         (data-set (last-elt (pathname-directory directory)))
         (image-filename-and-type (split (rest (assoc :image--filename s-expr)) #\.))
         (img-filename (first image-filename-and-type))
         (img-filetype (second image-filename-and-type)))
       (make-instance 'scene
                   :index (parse-integer (third (split  (pathname-name directory)  #\_)))
                   :source-path directory
                   :name (pathname-name directory)
                   :data-set (last-elt (pathname-directory directory))
                   :image (merge-pathnames
                           (make-pathname :directory `(:relative "images" ,data-set)
                                          :name img-filename :type img-filetype)
                           *mnist-data-path*)
                   :objects (loop for obj in objects
                                  for (index . id) in id-dict
                            collect (s-expr->object 'mnist-object obj
                                                    :relationships all-relationships
                                                    :id-dict-entry (cons index id)
                                                    :id-dict id-dict)))))

(defmethod s-expr->object ((type (eql 'clevr-dialog-set)) s-expr
                           &key directory)
  (let ((scene-index
         (rest (assoc :image--index s-expr)))
        (data-set
         (last-elt (pathname-directory directory))))
    (make-instance 'dialog-set
                   :scene-index scene-index
                   :source-path directory
                   :data-set data-set
                   :dialogs (loop for expr in (rest (assoc :dialogs s-expr))
                                  collect (s-expr->object 'clevr-dialog expr)))))

(defmethod s-expr->object ((type (eql 'gqa-dialog-set)) s-expr
                           &key directory)
  (let* (;(s-expr (first s-expr))
         (scene-index
         (rest (assoc :image--index s-expr)))
        (data-set
         (last-elt (pathname-directory directory))))
    (make-instance 'dialog-set
                   :scene-index scene-index
                   :source-path directory
                   :data-set data-set
                   :dialogs (loop for expr in (rest (assoc :dialogs s-expr))
                                  collect (s-expr->object 'clevr-dialog expr)))))

(defmethod s-expr->object ((type (eql 'mnist-dialog-set)) s-expr
                           &key directory)
  (let ((scene-index
         (rest (assoc :img (rest (assoc :dialog-1 s-expr)))))
        (data-set
         (last-elt (pathname-directory directory))))
    (make-instance 'dialog-set
                   :scene-index scene-index
                   :source-path directory
                   :data-set data-set
                   :dialogs (list (s-expr->object 'mnist-dialog (rest (assoc :qa (rest (assoc :dialog-1 s-expr)))))
                                  (s-expr->object 'mnist-dialog (rest (assoc :qa (rest (assoc :dialog-2 s-expr)))))
                                  (s-expr->object 'mnist-dialog (rest (assoc :qa (rest (assoc :dialog-3 s-expr)))))))))

(defmethod s-expr->object ((type (eql 'clevr-dialog)) s-expr &key)
  (make-instance 'dialog
                 :caption (remove-spurious-spaces
                           (remove-punctuation
                            (downcase (rest (assoc :caption s-expr)))))
                 :questions (mapcar #'(lambda (entry)
                                        (remove-spurious-spaces
                                         (remove-punctuation
                                          (downcase (rest (assoc :question entry))))))
                                    (rest (assoc :dialog s-expr)))
                 :answers (mapcar #'(lambda (entry)
                                      (rest (assoc :answer entry)))
                                  (rest (assoc :dialog s-expr)))
                 ;:partial-scene-graph (rest (assoc :history (rest (assoc :graph s-expr))))
                 ))

(defmethod s-expr->object ((type (eql 'mnist-dialog)) s-expr &key)
  (make-instance 'dialog       
                 :questions (mapcar #'(lambda (entry)
                                        (remove-spurious-spaces
                                         (remove-punctuation
                                          (space-between-number-and-s
                                           (downcase (rest (assoc :question entry)))))))
                                    s-expr)
                 :answers (mapcar #'(lambda (entry)
                                      (rest (assoc :answer entry)))
                                  s-expr)))


;; ################################
;; run-dialog-related utils
;; ################################

(defmethod get-scene-by-index (world index &key (get-dialog t))
  (let ((dataset (get-configuration world :dataset)))
    (setf (current-scene world)
          (cond ((eql dataset :clevr)
                 (load-object 'clevr-scene (nth index (scenes world))))
                ((eql dataset :mnist)
                 (load-object 'mnist-scene (nth index (scenes world))))
                ((eql dataset :gqa)
                 (load-object 'gqa-scene (nth index (scenes world))))))
    (when (and get-dialog (dialog-sets world))
      (let* ((scene-index
              (position (source-path (current-scene world)) (scenes world)))
             (dialog-set-path (nth scene-index (dialog-sets world))))
        (setf (current-dialog-set world)
              (cond
               ((eql dataset :clevr)
                (load-object 'clevr-dialog-set (nth index (scenes world))))
               ((eql dataset :mnist)
                (load-object 'mnist-dialog-set (nth index (scenes world))))
               ((eql dataset :gqa)
                (load-object 'gqa-dialog-set (nth index (scenes world)))))
              #|(if (eql dataset :clevr)
                (load-object 'clevr-dialog-set dialog-set-path )
                (load-object 'mnist-dialog-set dialog-set-path))|#)))
    (values (current-scene world)
            (current-dialog-set world))))
  
(defmethod nth-dialog ((world world) number)
  (let* ((dialog (nth number (dialogs (current-dialog-set world))))
         (questions (questions dialog)))
    (when (eq (dataset world) 'clevr)
      (push (caption dialog) questions))
    questions))




;; ################################
;; copy-object utils
;; ################################

(defmethod copy-object-no-relations ((object object))
  (make-instance 'object :id (id object)
                 :attributes (attributes object)))

(defmethod copy-object ((object object))
  (make-instance 'object :id (id object)
                 :attributes (attributes object)
                 :attention (attention object)
                 :relationships (copy-object (relationships object))
                 :coordinates (copy-object (coordinates object))
                 :rotation (copy-object (rotation object))))

(defmethod copy-object ((object-set object-set))
  (make-instance 'object-set
                 :id (id object-set)
                 :objects (loop for obj in (objects object-set)
                                collect (copy-object obj))
                 :scene-configuration (copy-object (scene-configuration object-set))
                 :image-index (image-index object-set)
                 :image-filename (image-filename object-set)))

(defmethod copy-object ((relation-set relation-set))
  (let ((leftmost (leftmost relation-set))
        (rightmost (rightmost relation-set))
        (most-in-front (most-in-front relation-set))
        (most-in-back (most-in-back relation-set))
        (immediate-right (immediate-right relation-set))
        (immediate-front (immediate-front relation-set)))
    (make-instance 'relation-set
                     :leftmost leftmost
                     :rightmost rightmost
                     :most-in-back most-in-back
                     :most-in-front most-in-front
                     :immediate-right immediate-right
                     :immediate-front immediate-front)))

(defmethod copy-object ((wm world-model))
  (make-instance 'world-model
                 :id (id wm)
                 :set-items (loop for item in (set-items wm)
                                  collect (copy-object item))
                 :path (path wm)))

(defmethod copy-object ((turn turn))
  (make-instance 'turn
                 :id (id turn)
                 :timestamp (timestamp turn)
                 :object-set (copy-object (object-set turn))
                 :question-type (question-type turn)
                 :question (question turn)
                 :answer (answer turn)
                 :topic-list (topic-list turn)))


;; ################################
;; scene configuration
;; ################################

(defmethod make-clevr-scene-configuration ((world world))
  (make-instance 'relation-set
                 :leftmost (get-extremes world 'left)
                 :rightmost (get-extremes world 'right)
                 :most-in-front (get-extremes world 'front)
                 :most-in-back (get-extremes world 'back)
                 :immediate-right (get-relations-list world 'right)
                 :immediate-front (get-relations-list world 'front)
                 :middle (get-middle-objects world)))

(defmethod make-mnist-scene-configuration ((world world))
  (make-instance 'relation-set
                 :immediate-right (list (rest (second (relationships (first (objects (current-scene world)))))))
                 :immediate-front (list (rest (third (relationships (first (objects (current-scene world)))))))))


(defmethod get-middle-objects ((world world))
   (let ((id-list nil)
         (object-list-length (length (objects (current-scene world)))))
     (loop for object in (objects (current-scene world))
           do (if (and (<= (- (length (first (relationships object))) 1) (/ object-list-length 2))
                       (<= (- (length (second (relationships object))) 1) (/ object-list-length 2))
                       (<= (- (length (third (relationships object))) 1) (/ object-list-length 2))
                       (<= (- (length (fourth (relationships object))) 1) (/ object-list-length 2)))
                (push (id object) id-list)))
     id-list))

(defmethod get-extremes ((world world) direction)
  (let ((id nil))
    (cond ((eql direction 'left) (loop for object in (objects (current-scene world))
                                       do (if (not (second (fourth (relationships object))))
                                            (setf id (id object)))))
          ((eql direction 'right) (loop for object in (objects (current-scene world))
                                        do (if (not (second (first (relationships object))))
                                             (setf id (id object)))))
          ((eql direction 'front) (loop for object in (objects (current-scene world))
                                        do (if (not (second (third (relationships object))))
                                             (setf id (id object)))))
          ((eql direction 'back) (loop for object in (objects (current-scene world))
                                       do (if (not (second (second (relationships object))))
                                            (setf id (id object))))))
    id))

(defmethod get-extremes ((world world) direction)
  (let ((id nil))
    (cond ((eql direction 'left) (loop for object in (objects (current-scene world))
                                       do (if (not (second  (assoc 'left (relationships object))))
                                            (setf id (id object)))))
          ((eql direction 'right) (loop for object in (objects (current-scene world))
                                        do (if (not (second (assoc 'right (relationships object))))
                                             (setf id (id object)))))
          ((eql direction 'front) (loop for object in (objects (current-scene world))
                                        do (if (not (second (assoc 'front (relationships object))))
                                             (setf id (id object)))))
          ((eql direction 'back) (loop for object in (objects (current-scene world))
                                       do (if (not (second (assoc 'back (relationships object))))
                                            (setf id (id object))))))
    id))

(defmethod get-relations-list ((world world) direction)
  (let ((relations-list nil) (number 0))
    (cond ((eql direction 'right) (progn
                                    (loop for object in (objects (current-scene world))
                                          do (if (and (second (assoc 'right  (relationships object)))
                                                      (not (third (assoc 'right  (relationships object)))))
                                               (push (list (id object) (second (assoc 'right  (relationships object)))) relations-list)))
                                    (loop for i from 0 to (- (length (objects (current-scene world))) 2)
                                          do (loop for object in (objects (current-scene world))
                                                     do  (if (eql (length (assoc 'right  (relationships object))) (+ 2 (length relations-list)))
                                                            (loop for el in (assoc 'right  (relationships object))
                                                                  do (if (and (member el (mapcar #'first relations-list ))
                                                                              (not (member el (mapcar #'second relations-list))))
                                                                       (push (list (id object) el) relations-list))))))))
          ((eql direction 'front)
           (progn
             (loop for object in (objects (current-scene world))
                   do (if (and (second (third (relationships object)))
                               (not (third (third (relationships object)))))
                        (push (list (id object) (second (third (relationships object)))) relations-list)))
             (loop for i from 0 to (- (length (objects (current-scene world))) 2)
                   do (loop for object in (objects (current-scene world))
                            do  (if (eql (length (third (relationships object))) (+ 2 (length relations-list)))
                                  (loop for el in (third (relationships object))
                                        do (if (and (member el (mapcar #'first relations-list ))
                                                    (not (member el (mapcar #'second relations-list))))
                                             (push (list (id object) el) relations-list)))))))))
    relations-list))



;; ################################
;; other utils
;; ################################

(defun space-between-number-and-s (string)
  "removes newlines inside string"
  (cl-ppcre:regex-replace-all "'s" string " 's"))

(defmethod find-entity-by-id ((thing t) (id fixnum))
  nil)

(defmethod find-entity-by-id ((blackboard blackboard) (id fixnum))
  (loop for field in (data-fields blackboard)
        thereis (find-entity-by-id (cdr field) id)))

(defmethod find-entity-by-id ((cons cons) (id fixnum))
  (if (and (typep (car cons) 'entity)
           (typep (car cons) 'fixnum)
           (= (id (car cons)) id))
    (car cons)
    (or (find-entity-by-id (car cons) id)
        (find-entity-by-id (cdr cons) id))))

(defmethod find-entity-by-id ((entity entity) (id fixnum))
  (when (and (typep (id entity) 'fixnum)
             (= (id entity) id))
    entity))

;(find-entity-by-id *ontology* '0)





(defgeneric category-value (category)
  (:documentation "Obtain the value of the category"))

(defmethod category-value ((shape-category shape-category))
  (shape shape-category))
(defmethod category-value ((size-category size-category))
  (size size-category))
(defmethod category-value ((color-category color-category))
  (color color-category))
(defmethod category-value ((material-category material-category))
  (material material-category))
(defmethod category-value ((spatial-relation-category spatial-relation-category))
  (spatial-relation spatial-relation-category))
(defmethod category-value ((boolean-category boolean-category))
  (bool boolean-category))
(defmethod category-value ((attribute-category attribute-category))
  (attribute attribute-category))
(defmethod category-value ((attention attention))
  (id attention))
(defmethod category-value ((digit-category digit-category))
  (digit digit-category))
(defmethod category-value ((style-category style-category))
  (style style-category))
(defmethod category-value ((bgcolor-category bgcolor-category))
  (check-if-bgcolor (bgcolor bgcolor-category)))