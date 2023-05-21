(in-package :cle)

;; -------------
;; + Prototype +
;; -------------
(defclass prototype ()
  ((channel
    :initarg :channel :accessor channel :initform nil :type symbol)
   (weight
    :initarg :weight :accessor weight :initform nil :type number)
   (distribution
    :initarg :distribution :accessor distribution :initform nil :type distribution))
  (:documentation "A prototype is a mapping between a feature channel and a distribution."))
  