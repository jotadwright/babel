(in-package :fcg-propbank)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  ;;
;; Data structures for fcg-propbank ;; 
;;                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass corpus ()
  ((name
    :type symbol
    :accessor name
    :initarg :name
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
  (:documentation "Object holding the different splits of a corpus."))

(defmethod print-object ((c corpus) (stream t))
  "String representation of corpus object."
  (format stream "<Corpus (~a): train (~a), dev (~a), test (~a)>"
          (name c)
          (length (train-split c))
          (length (dev-split c))
          (length (test-split c))))

(defmethod all-sentences ((c corpus) (stream t))
  "Returns all sentences in the corpus"
  (append (train-split c) (dev-split c) (test-split c)))





(defclass fcg-propbank-sentence ()
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
    :documentation "The tokens as a list of strings.")
   (propbank-frames 
    :type list
    :accessor propbank-frames
    :documentation "The propbank frames annotated in the sentence.")
   (sentence-string 
    :type string
    :accessor sentence-string
    :documentation "The string that serves as input to syntactic analysis.")
   (initial-transient-structure 
    :type coupled-feature-structure
    :accessor initial-transient-structure
    :initarg :intial-transient-structure
    :documentation "Initial transient structure")
   (language 
    :type string
    :accessor language
    :initarg :language
    :documentation "Language code for the sentence."))
  (:documentation "Representation of an fcg-propbank sentence."))
