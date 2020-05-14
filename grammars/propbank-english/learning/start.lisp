

;; (ql:quickload :propbank-english)

(in-package :propbank-english)

;; Loading the Propbank Frames
(cl-propbank::load-pb-data :store-data t :ignore-stored-data nil)


(length cl-propbank::*pb-data*)



;; Loading the Ontonotes Propbank-annotated corpus


(defparameter *propbank-annotations-directory* (merge-pathnames "English/propbank-release/data/" *babel-corpora*))
(defparameter *propbank-annotations-example-file* (merge-pathnames (make-pathname :directory '(:relative "ontonotes" "bc" "cnn" "00")
                                                                                  :name "cnn_0000" :type "gold_conll")
                                                                   *propbank-annotations-directory*))

(defun read-propbank-conll-file (pathname)
  "Reads a propbank-conll file and returns a list of conll-sentence objects."
  (let* ((conll-lines (with-open-file (inputstream pathname :direction :input)
                       (uiop/stream:read-file-lines inputstream)))
         (conll-tokens (loop for line in conll-lines
                             for conll-fields = (split-sequence:split-sequence #\Space line :remove-empty-subseqs t)
                             unless (equalp line "")
                             collect (make-instance 'conll-token
                                                    :source-file (first conll-fields)
                                                    :sentence-id (parse-integer (second conll-fields))
                                                    :token-id (parse-integer (third conll-fields))
                                                    :token-string (fourth conll-fields)
                                                    :pos (fifth conll-fields)
                                                    :parse-bit (sixth conll-fields)
                                                    :propbank-frame-file (seventh conll-fields)
                                                    :propbank-roleset (eighth conll-fields)
                                                    :propbank-roles (subseq conll-fields 8)))))
    ;; Collect all propbank sentences as objects.
    (loop with conll-sentences = nil
          with previous-sentence-id = nil
          with current-sentence-tokens = nil
          for conll-token in conll-tokens
          for sentence-id = (sentence-id conll-token)
          if (and previous-sentence-id
                  (not (= sentence-id previous-sentence-id)))
          do
          (setf conll-sentences (append conll-sentences (list (make-instance 'conll-sentence :tokens current-sentence-tokens))))
          (setf current-sentence-tokens (list conll-token))
          (setf previous-sentence-id sentence-id)
          else
          do
          (setf current-sentence-tokens (append current-sentence-tokens (list conll-token)))
          (setf previous-sentence-id sentence-id)
          finally
          do (setf conll-sentences (append conll-sentences (list (make-instance 'conll-sentence :tokens current-sentence-tokens))))
          (return conll-sentences))))
          
        
(setf *conll-sentences* (read-propbank-conll-file *propbank-annotations-example-file*))



(defclass conll-sentence ()
  ((source-file
    :type string :initarg :source-file 
    :accessor source-file
    :initform nil 
    :documentation "The path to the source file.")
   (sentence-id 
    :type number :initarg :sentence-id 
    :accessor sentence-id
    :initform nil 
    :documentation "The id of the sentence.")
   (tokens 
    :type list :initarg :tokens 
    :accessor tokens
    :initform nil 
    :documentation "The tokens belonging to the sentence.")
   (sentence-string 
    :type string :initarg :sentence-string 
    :accessor sentence-string
    :initform nil 
    :documentation "The sentence as a string.")
  (propbank-frames 
    :type string :initarg :propbank-frames 
    :accessor propbank-frames
    :initform nil 
    :documentation "The propbank frames annotated in the sentence."))
   (:documentation "Representation of a conll sentence."))

(defmethod initialize-instance :after ((sentence conll-sentence) &key &allow-other-keys)
  "Sets the all other fields based on the tokens."
  ;; source file
  (setf (source-file sentence) (source-file (first (tokens sentence))))
  ;; sentence id
  (setf (sentence-id sentence) (sentence-id (first (tokens sentence))))
  ;; sentence string
  (setf (sentence-string sentence) (format nil "~{~a~^ ~}" (mapcar #'token-string (tokens sentence))))
  ;; propbank frames
  )



(defclass propbank-frame ()
  ((frame-name
    :type string :initarg :frame-name 
    :accessor frame-name
    :initform nil 
    :documentation "The name of the frame.")
   (propbank-frame-file 
    :type string :initarg :propbank-frame-file 
    :accessor propbank-frame-file
    :initform nil 
   (frame-roles 
    :type list :initarg :frame-roles 
    :accessor frame-roles
    :initform nil 
    :documentation "The list of frame roles."))
   (:documentation "Representation of a propbank frame."))

(defclass propbank-frame-role ()
  ((role-type
    :type string :initarg :role-type 
    :accessor role-type
    :initform nil 
    :documentation "The type of the role (e.g. V, arg0, arg1, etc.)")
   (indices 
    :type list :initarg :indices 
    :accessor indices
    :initform nil 
    :documentation "The indices of the words belonging to the role.")
   (role-string 
    :type string :initarg :role-string 
    :accessor role-string
    :initform nil 
    :documentation "The string of the words belonging to the role."))
   (:documentation "Representation of a propbank frame role."))

(defclass conll-token ()
  ((source-file
    :type string :initarg :source-file 
    :accessor source-file
    :initform nil 
    :documentation "The path to the source file.")
   (sentence-id 
    :type number :initarg :sentence-id 
    :accessor sentence-id
    :initform nil 
    :documentation "The id of the sentence.")
   (token-id 
    :type number :initarg :token-id 
    :accessor token-id
    :initform nil 
    :documentation "The id of the token.")
   (token-string 
    :type string :initarg :token-string 
    :accessor token-string
    :initform nil 
    :documentation "The string of the token.")
   (pos 
    :type string :initarg :pos 
    :accessor pos
    :initform nil 
    :documentation "The pos of the token.")
   (parse-bit
    :type string :initarg :parse-bit 
    :accessor parse-bit
    :initform nil 
    :documentation "The parse bit of the token.")
   (propbank-frame-file 
    :type string :initarg :propbank-frame-file 
    :accessor propbank-frame-file
    :initform nil 
    :documentation "The propbank frame file (.xml) of the predicate.")
   (propbank-roleset 
    :type string :initarg :propbank-roleset 
    :accessor propbank-roleset
    :initform nil 
    :documentation "The propbank frame roleset of the predicate.")
   (propbank-roles 
    :type list :initarg :propbank-roles 
    :accessor propbank-roles
    :initform nil 
    :documentation "A list of propbank roles for this token."))
   (:documentation "Representation of a conll token."))

