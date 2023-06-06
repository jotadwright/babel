(in-package :cle)

;; ------------------
;; + Abstract class +
;; ------------------
(defclass distribution ()
  ()
  (:documentation "Abstract class for distributions"))

;; -------------------------
;; + Gaussian distribution +
;; -------------------------
(defclass gaussian (distribution)
  ((mean
    :initarg :mean :accessor mean :initform nil :type number)
   (st-dev
    :initarg :st-dev :accessor st-dev :initform nil :type number)
   (history
    :initarg :history :accessor history :initform nil :type list))
  (:documentation "A gaussian distribution."))


;; --------------------
;; + Helper functions +
;; --------------------
(defmethod print-object ((distribution gaussian) stream)
  (pprint-logical-block (stream nil)
    (format stream "<Gaussian:~
                        ~:_ mean: ~,3f,~:_ st-dev: ~,3f~:_"
            (mean distribution) (st-dev distribution))
    (format stream ">")))

(defmethod copy-object ((distribution gaussian))
  (make-instance 'gaussian
                 :mean (copy-object (mean distribution))
                 :st-dev (copy-object (st-dev distribution))
                 :history (copy-object (history distribution))))
