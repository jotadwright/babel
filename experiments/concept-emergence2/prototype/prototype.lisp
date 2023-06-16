(in-package :cle)

;; -------------
;; + Prototype +
;; -------------

(defclass prototype ()
  ((channel
    :initarg :channel :accessor channel :initform nil :type symbol)
   (weight-val
    :initarg :weight :accessor weight-val :initform nil :type number)
   (weight-mode
    :accessor weight-mode :initarg :weight-mode :type keyword)
   (distribution
    :initarg :distribution :accessor distribution :initform nil :type distribution))
  (:documentation "A prototype is a mapping between a feature channel and a distribution."))

(defmethod copy-object ((prototype prototype))
  (make-instance 'prototype
                 :channel (channel prototype)
                 :weight (copy-object (weight-val prototype))
                 :weight-mode (copy-object (weight-mode prototype))
                 :distribution (copy-object (distribution prototype))))

(defmethod weight ((prototype prototype))
  (case (weight-mode prototype)
    (:standard (weight-val prototype))
    (:j-interpolation (sigmoid (weight-val prototype)))))

(defun sigmoid (x &key (c -1/2))
  "Sigmoid function where c changes the slope of the function. 
  
    When c is a fraction the slope is less steep, when c is a larger the slope is steeper."
  (/ 1 (+ 1 (exp (* c x)))))

(defmethod print-object ((prototype prototype) stream)
  (pprint-logical-block (stream nil)
    (format stream "<Prototype:~
                        ~:_ channel: ~a,~:_ weight ~a~:_"
            (channel prototype) (weight prototype))
    (format stream ">")))
