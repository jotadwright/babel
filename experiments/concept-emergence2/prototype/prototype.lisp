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
    :initarg :distribution :accessor distribution :initform nil :type distribution)
   (history
    :initarg :history :accessor history :initform '() :type list))
  (:documentation "A prototype is a mapping between a feature channel and a distribution."))

(defmethod weight ((prototype prototype))
  "Returns the weight of the prototype based on it's mode."
  (case (weight-mode prototype)
    (:standard (weight-val prototype))
    (:j-interpolation (j-interpolation (weight-val prototype)))))

(defun j-interpolation (value)
  "Applies the step-wise sigmoid function.

   Made faster by already returning known values for common values."
  (cond ((= value 0) 0.5)
        ((= value 1) 0.622)
        ((= value -1) 0.378)
        ((< value -35) 0)
        ((> value  35) 1)
        (t (sigmoid value))))

(defun sigmoid (x &key (c 0.5))
  "Sigmoid function where c changes the slope of the function. 
  
    When c is a fraction the slope is less steep, when c is a larger the slope is steeper."
  (/ 1 (+ 1 (exp (- (* c x))))))

;; --------------------
;; + helper-functions +
;; --------------------
(defmethod copy-object ((prototype prototype))
  (make-instance 'prototype
                 :channel (channel prototype)
                 :weight (copy-object (weight-val prototype))
                 :weight-mode (copy-object (weight-mode prototype))
                 :distribution (copy-object (distribution prototype))
                 :history (copy-object (history prototype))))

(defmethod print-object ((prototype prototype) stream)
  (pprint-logical-block (stream nil)
    (format stream "<Prototype:~
                        ~:_ channel: ~a,~:_ weight ~a~:_"
            (channel prototype) (weight prototype))
    (format stream ">")))
