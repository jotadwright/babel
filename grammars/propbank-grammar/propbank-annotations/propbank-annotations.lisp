(in-package :propbank-grammar)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                  ;;
;; Representing, parsing and processing Propbank annotated corpora. ;;
;;                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;
;; Parameters ;;
;;;;;;;;;;;;;;;;

;; Pointer to propbank annotated corpora
(defparameter *ontonotes-annotations-directory* (merge-pathnames "English/propbank-release/data/" *babel-corpora*))
(defparameter *ewt-annotations-directory* (merge-pathnames "English/propbank-release/data/google/ewt/" *babel-corpora*))

;; Global variables where propbank annotations will be loaded.
(defparameter *ontonotes-annotations* "Ontonotes annotations will be stored here.")
(defparameter *ewt-annotations* "Ewt annotations will be stored here.")

;; File where propbank annotations will be stored in binary format
(defparameter *ontonotes-annotations-storage-file* (babel-pathname :directory '("grammars" "propbank-grammar" "propbank-annotations")
                                                                   :name "ontonotes-annotations"
                                                                   :type #+lispworks "lw.store" #+ccl "ccl.store" #+sbcl "sbcl.store"))

(defparameter *ewt-annotations-storage-file* (babel-pathname :directory '("grammars" "propbank-grammar" "propbank-annotations")
                                                             :name "ewt-annotations"
                                                             :type #+lispworks "lw.store" #+ccl "ccl.store" #+sbcl "sbcl.store"))

;;;;;;;;;;;;;
;; Classes ;;
;;;;;;;;;;;;;

;; propbank-annotations ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass propbank-annotations ()
  ((corpus-name
    :type symbol
    :accessor corpus-name
    :initarg :corpus-name
    :documentation "The name of the corpus.")
   (train-split
    :type list
    :accessor train-split
    :initarg :train-split 
    :documentation "The training portion of the corpus.")
   (dev-split 
    :type list
    :accessor dev-split
    :initarg :dev-split 
    :documentation "The development portion of the corpus.")
   (test-split 
    :type list 
    :accessor test-split
    :initarg :test-split 
    :documentation "The test portion of the corpus"))
   (:documentation "Object holding the propbank annotations of a given corpus."))

(defmethod print-object ((pa propbank-annotations) (stream t))
  "Printing a conll token."
  (format stream "<Propbank-annotations (~a): train (~a), dev (~a), test (~a)>"
          (corpus-name pa)
          (length (train-split pa))
          (length (dev-split pa))
          (length (test-split pa))))


;; Token ;;
;;;;;;;;;;;

(defclass conll-token ()
  ((source-file
    :type string
    :accessor source-file
    :initarg :source-file 
    :documentation "The path to the source file.")
   (sentence-id 
    :type number 
    :accessor sentence-id
    :initarg :sentence-id 
    :documentation "The id of the sentence in the file.")
   (token-id 
    :type number
    :accessor token-id
    :initarg :token-id 
    :documentation "The id of the token in the sentence.")
   (token-string 
    :type string 
    :accessor token-string
    :initarg :token-string 
    :initform nil 
    :documentation "The string of the token.")
   (propbank-frame-file 
    :type string 
    :accessor propbank-frame-file
    :initarg :propbank-frame-file 
    :documentation "The propbank frame file (.xml) of the predicate (if applicable).")
   (propbank-roleset 
    :type string 
    :accessor propbank-roleset
    :initarg :propbank-roleset 
    :documentation "The propbank frame roleset of the predicate (if applicable).")
   (propbank-roles 
    :type list 
    :accessor propbank-roles
    :initarg :propbank-roles 
    :documentation "A list of propbank roles for this token."))
   (:documentation "Representation of a CONLL token."))

(defmethod print-object ((token conll-token) (stream t))
  "Printing a conll token."
  (format stream "<Token: ~s>" (token-string token)))


;; Frame ;;
;;;;;;;;;;;

(defclass propbank-frame ()
  ((frame-name
    :type string 
    :accessor frame-name
    :initarg :frame-name 
    :initform nil 
    :documentation "The name of the frame.")
   (propbank-frame-file 
    :type string 
    :accessor propbank-frame-file
    :initarg :propbank-frame-file 
    :initform nil)
   (frame-roles 
    :type list 
    :accessor frame-roles
    :initarg :frame-roles 
    :documentation "The list of frame roles."))
   (:documentation "Representation of a propbank frame."))

(defmethod print-object ((frame propbank-frame) (stream t))
  "Printing a frame."
  (format stream "<Frame: ~s>" (frame-name frame)))

(defmethod copy-object-content ((source propbank-frame) (destination propbank-frame))
  "Copying a propbank-frame."
  (setf (frame-name destination) (frame-name source))
  (setf (propbank-frame-file destination) (propbank-frame-file source))
  (setf (frame-roles destination) (frame-roles source)))


;; Frame Role ;;
;;;;;;;;;;;;;;;;

(defclass propbank-frame-role ()
  ((role-type
    :type string
    :accessor role-type
    :initarg :role-type 
    :documentation "The type of the role (e.g. 'V', 'arg0', 'arg1', etc.)")
   (indices 
    :type list 
    :accessor indices
    :initarg :indices 
    :documentation "The indices of the words belonging to the role.")
   (role-string 
    :type string 
    :accessor role-string
    :initarg :role-string 
    :documentation "The string of the words belonging to the role."))
   (:documentation "Representation of a propbank frame role."))

(defmethod print-object ((role propbank-frame-role) (stream t))
  "Printing a frame."
  (format stream "<Role: ~s>" (role-type role)))


;; sentence ;;
;;;;;;;;;;;;;;

(defclass conll-sentence ()
  ((source-file
    :type string
    :accessor source-file
    :documentation "The path to the source file.")
   (sentence-id 
    :type number 
    :accessor sentence-id
    :documentation "The id of the sentence.")
   (tokens 
    :type list 
    :accessor tokens
    :initarg :tokens 
    :documentation "The tokens belonging to the sentence.")
   (sentence-string 
    :type string 
    :accessor sentence-string
    :documentation "The sentence as a string.")
   (propbank-frames 
    :type list
    :accessor propbank-frames
    :documentation "The propbank frames annotated in the sentence.")
   (syntactic-analysis 
    :type list 
    :accessor syntactic-analysis
    :documentation "The Spacy-Benepar syntactic analysis of the sentence.")
   (initial-transient-structure 
    :type coupled-feature-structure 
    :accessor initial-transient-structure
    :documentation "The initial transient structure for the utterance."))
  (:documentation "Representation of a conll sentence."))

(defmethod print-object ((s conll-sentence) (stream t))
  "Printing a frame."
  (format stream "<Sentence: ~s>" (sentence-string s)))

(defmethod initialize-instance :after ((sentence conll-sentence) &key &allow-other-keys)
  "Sets all other fields in conll-sentence based on the tokens."
  ;; source file
  (setf (source-file sentence) (source-file (first (tokens sentence))))
  ;; sentence id
  (setf (sentence-id sentence) (sentence-id (first (tokens sentence))))
  ;; sentence string
  (setf (sentence-string sentence) (format nil "~{~a~^ ~}" (mapcar #'token-string (tokens sentence))))
  ;; syntactic anlysis
  (setf (syntactic-analysis sentence) (nlp-tools:get-penelope-syntactic-analysis (format nil "~{~a~^ ~}" (mapcar #'token-string (tokens sentence)))))
  ;; initial transient structure
  (setf (initial-transient-structure sentence) (create-initial-transient-structure-based-on-benepar-analysis (syntactic-analysis sentence)))
  ;; propbank frames
  (setf (propbank-frames sentence) (loop for role-number from 0 upto (- (length (propbank-roles (first (tokens sentence)))) 1)
        collect (loop with frame-name = nil
                      with frame-file = nil
                      with propbank-roles = nil
                      with current-open-role = nil
                      with current-open-role-indices = nil
                      with current-open-role-strings = nil

                      for token in  (tokens sentence)
                      for role-field = (nth role-number (propbank-roles token))
                      ;; Role opening and closing (single token)
                      if
                      (and (not current-open-role)
                           (string= "(" (subseq role-field 0 1))
                           (string= ")" (subseq role-field (- (length role-field) 1))))
                      do
                      (when (string= "V" (subseq role-field 1 (- (length role-field) 2)))
                        (setf frame-name (propbank-roleset token))
                        (setf frame-file (propbank-frame-file token)))
                      (setf propbank-roles (append propbank-roles (list (make-instance 'propbank-frame-role
                                                                                       :role-type (subseq role-field 1 (- (length role-field) 2))
                                                                                       :indices (list (token-id token))
                                                                                       :role-string (token-string token)))))
                      else
                      
                      ;; Role opening
                      if
                      (and (not current-open-role)
                           (string= "(" (subseq role-field 0 1)))
                      do
                      (when (string= "V" (subseq role-field 1 (- (length role-field) 1)))
                        (setf frame-name (propbank-roleset token))
                        (setf frame-file (propbank-frame-file token)))
                      (setf current-open-role (subseq role-field 1 (- (length role-field) 1)))
                      (setf current-open-role-indices (append current-open-role-indices (list (token-id token))))
                      (setf current-open-role-strings (append current-open-role-strings (list (token-string token))))
                      else
                      ;; Role continuing
                      if
                      (and current-open-role
                           (string= "*" role-field))
                      do
                      (setf current-open-role-indices (append current-open-role-indices (list (token-id token))))
                      (setf current-open-role-strings (append current-open-role-strings (list (token-string token))))
                      else
                      ;; Role closing
                      if
                      (and current-open-role
                           (string= ")" (subseq role-field (- (length role-field) 1))))
                      do
                      (setf current-open-role-indices (append current-open-role-indices (list (token-id token))))
                      (setf current-open-role-strings (append current-open-role-strings (list (token-string token))))
                      (setf propbank-roles (append propbank-roles (list (make-instance 'propbank-frame-role
                                                                                       :role-type current-open-role
                                                                                       :indices current-open-role-indices
                                                                                       :role-string (format nil "~{~a~^ ~}" current-open-role-strings)))))
                      (setf current-open-role nil)
                      (setf current-open-role-indices nil)
                      (setf current-open-role-strings nil)
                      finally (return (make-instance 'propbank-frame
                                                    :frame-name frame-name
                                                    :propbank-frame-file frame-file
                                                    :frame-roles propbank-roles))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reading in propbank annotated corpora ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-corpus-annotations-storage-file (corpus-name)
  (case corpus-name
    (ewt *ewt-annotations-storage-file*)
    (ontonotes *ontonotes-annotations-storage-file*)))


(defun load-propbank-annotations (corpus-name &key (store-data t) ignore-stored-data)
  "Loads ProbBank annotations and stores the result. It is loaded from a pb-annotations.store file if it
   is available, from the raw pb-annoation files otherwise. :store-data nil avoids storing the data,
   ignore-stored-data t forces to load the data from the original files."
  (let ((corpus-annotations-storage-file (get-corpus-annotations-storage-file corpus-name)))
    (if (and (probe-file corpus-annotations-storage-file)
             (not ignore-stored-data))
      ;; Option 1: Load from storage file if file exists ignore-stored-data is nil.
      (case corpus-name
        (ewt (setf *ewt-annotations* (cl-store:restore corpus-annotations-storage-file)))
        (ontonotes (setf *ontonotes-annotations* (cl-store:restore corpus-annotations-storage-file))))
      ;; Option 2: Load from original propbank files.
      (case corpus-name
        (ewt (setf *ewt-annotations* (load-propbank-annotations-from-files corpus-name)))
        (ontonotes (setf *ontonotes-annotations* (load-propbank-annotations-from-files corpus-name)))))
    ;; Store the data into an *fn-data-storage-file*
    (if (and store-data (or ignore-stored-data
                            (not (probe-file corpus-annotations-storage-file))))
      (case corpus-name
        (ewt (cl-store:store *ewt-annotations* corpus-annotations-storage-file))
        (ontonotes (cl-store:store *ontonotes-annotations* corpus-annotations-storage-file))))

    (format nil "Loaded ~a annotated sentences into ~a"
            (+ (length (train-split (case corpus-name
                                      (ewt *ewt-annotations*)
                                      (ontonotes *ontonotes-annotations*))))
               (length (dev-split (case corpus-name
                                    (ewt *ewt-annotations*)
                                    (ontonotes *ontonotes-annotations*))))
               (length (test-split (case corpus-name
                                     (ewt *ewt-annotations*)
                                     (ontonotes *ontonotes-annotations*)))))
            (case corpus-name
              (ewt '*ewt-annotations*)
              (ontonotes '*ontonotes-annotations*)))))


(defun load-propbank-annotations-from-files (corpus-name)
  "Loads all framesets and returns the predicate objects for a given corpus."
  (let* ((file-names-for-splits (get-corpus-file-lists corpus-name))
         (file-names-train (first file-names-for-splits))
         (file-names-dev (second file-names-for-splits))
         (file-names-test (third file-names-for-splits)))
    (make-instance 'propbank-annotations
                   :corpus-name corpus-name
                   :train-split (load-split-from-files file-names-train)
                   :dev-split (load-split-from-files file-names-dev)
                   :test-split (load-split-from-files file-names-test))))

(defun load-split-from-files (file-names-for-split)
  (loop for file in file-names-for-split
        if (probe-file file)
        do (format t "Loading file: ~s.~%" file)
        and
        append (read-propbank-conll-file file)
        into sentences
        else
        do (warn (format nil "File not found: ~s." file))
        finally
        (return sentences)))

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
                                                    :token-string (if (and (string= (subseq (fourth conll-fields) 0 1) "/")
                                                                           (not (string= (subseq (fourth conll-fields) 1) "")))
                                                                    (subseq (fourth conll-fields) 1)
                                                                    (fourth conll-fields))
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
          (progn (setf conll-sentences (append conll-sentences (list (make-instance 'conll-sentence :tokens current-sentence-tokens))))
                (return conll-sentences)))))

(defun get-corpus-file-lists (corpus-name)
  (case corpus-name
    (ewt (ewt-file-lists))
    (ontonotes (ontonotes-file-lists))))

(defun ontonotes-file-lists ()
  "Returns the filelists for train, dev and test splits."
  (loop for split in (list (babel-pathname :directory '("grammars" "propbank-grammar" "propbank-annotations") :name "ontonotes-train-list" :type "txt")
                           (babel-pathname :directory '("grammars" "propbank-grammar" "propbank-annotations") :name "ontonotes-dev-list" :type "txt")
                           (babel-pathname :directory '("grammars" "propbank-grammar" "propbank-annotations") :name "ontonotes-test-list" :type "txt"))
        collect
        (with-open-file (inputstream split :direction :input)
          (mapcar #'(lambda (subpath)
                     (merge-pathnames (string-append subpath ".gold_conll") *ontonotes-annotations-directory*))
                  (uiop/stream:read-file-lines inputstream)))))

(defun ewt-file-lists ()
  "Returns the filelists for train, dev and test splits."
  (loop for split in (list (babel-pathname :directory '("grammars" "propbank-grammar" "propbank-annotations") :name "ewt-train-list" :type "txt")
                            (babel-pathname :directory '("grammars" "propbank-grammar" "propbank-annotations") :name "ewt-dev-list" :type "txt")
                            (babel-pathname :directory '("grammars" "propbank-grammar" "propbank-annotations") :name "ewt-test-list" :type "txt"))
        collect
        (with-open-file (inputstream split :direction :input)
          (mapcar #'(lambda (subpath)
                     (merge-pathnames (string-append subpath ".gold_conll") *ewt-annotations-directory*))
                  (uiop/stream:read-file-lines inputstream)))))

;; (ontonotes-file-lists)

(defmethod spacy-benepar-compatible-annotation ((sentence conll-sentence) role-set &key (selected-role-types 'all))
  "Returns T if all selected role types (all, core-only, argm-only) or a selected role type correspond to a constituent in de spacy-benepar tree."
  (let ((syntactic-analysis (remove '(nil) (left-pole-structure (initial-transient-structure sentence)) :test #'equalp))
        (selected-roles (loop for frame in (propbank-frames sentence)
                              if (equalp (frame-name frame) role-set)
                              append (loop for frame-role in (frame-roles frame)
                                           if (case selected-role-types
                                                (all t)
                                                (core-only (unless (search "ARGM" (role-type frame-role))
                                                             t))
                                                (argm-only (when (search "ARGM" (role-type frame-role))
                                                             t)))
                                           collect frame-role))))
    
    (loop for role in selected-roles
          for first-index-for-role = (first (indices role))
          for last-index-for-role = (last-elt (indices role))
          always (loop for unit in syntactic-analysis
                       for unit-start = (first (unit-feature-value unit 'span))
                       for unit-end = (second (unit-feature-value unit 'span))
                       thereis (and (= first-index-for-role unit-start)
                                    (= last-index-for-role (- unit-end 1)))))))
