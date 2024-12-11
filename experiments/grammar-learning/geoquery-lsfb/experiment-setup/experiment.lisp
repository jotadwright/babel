(in-package :geoquery-lsfb)

(defclass geoquery-lsfb-exeriment (experiment)
  ((list-of-training-files
    :documentation "A list of training files"
    :type list
    :initarg :list-of-training-files
    :accessor list-of-training-files
    :initform nil)
   (list-of-test-files
    :documentation "A list of test files"
    :type list
    :initarg :list-of-test-files
    :accessor list-of-test-files
    :initform nil)))

(defclass geoquery-example ()
  ((template-nr
    :documentation "the template-nr of to the geoquery-example"
    :type integer
    :initarg :template-nr
    :accessor template-nr
    :initform nil)
   (utterance
    :documentation "the utterance of the geoquery-example"
    :type string
    :initarg :utterance
    :accessor utterance
    :initform nil)
   (meaning
    :documentation "the meaning of the geoquery-example"
    :type list
    :initarg :meaning
    :accessor meaning
    :initform nil)))


(defun set-up-geoquery-lsfb-experiment (pathname)
  (let ((experiment (make-instance 'geoquery-lsfb-exeriment)))
    (setf (population experiment)
          (make-geoquery-agents experiment))
    (setf (list-of-training-files experiment)
          (directory (concatenate 'string pathname "train/")))
    (setf (list-of-test-files experiment)
          (directory (concatenate 'string pathname "test/")))
    experiment))

(defmethod speaker ((experiment geoquery-lsfb-exeriment) &key (discourse-role 'speaker))
  (loop for agent in (population experiment)
        when (eql (discourse-role agent) 'speaker)
          return agent))

(defmethod hearer ((experiment geoquery-lsfb-exeriment) &key (discourse-role 'hearer))
  (loop for agent in (population experiment)
        when (eql (discourse-role agent) 'hearer)
          return agent))

                 