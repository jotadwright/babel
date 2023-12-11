(in-package :cle)

;; ----------------------
;; + Available channels +
;; ----------------------

(defun credit-channels ()
  "CREDIT-FRAUD dataset."
  `(,'V1
    ,'V2
    ,'V3
    ,'V4
    ,'V5
    ,'V6
    ,'V7
    ,'V8
    ,'V9
    ,'V10
    ,'V11
    ,'V12
    ,'V13
    ,'V14
    ,'V15
    ,'V16
    ,'V17
    ,'V18
    ,'V19
    ,'V20
    ,'V21
    ,'V22
    ,'V23
    ,'V24
    ,'V25
    ,'V26
    ,'V27
    ,'V28))

(defmethod get-all-channels ((mode (eql :credit)))
  (credit-channels))

(defmethod is-channel-available ((mode (eql :credit)) symbolic-attribute raw-attributes)
  t)

;; credit fraud
(defmethod get-all-channels ((mode (eql :credit-fraud)))
  (credit-channels))

(defmethod is-channel-available ((mode (eql :credit-fraud)) symbolic-attribute raw-attributes)
  t)

(defmethod get-all-channels ((mode (eql :credit-fraud-l)))
  (credit-channels))

(defmethod is-channel-available ((mode (eql :credit-fraud-l)) symbolic-attribute raw-attributes)
  t)

;; credit legit
(defmethod get-all-channels ((mode (eql :credit-legit)))
  (credit-channels))

(defmethod is-channel-available ((mode (eql :credit-legit)) symbolic-attribute raw-attributes)
  t)

(defmethod get-all-channels ((mode (eql :credit-legit-l)))
  (credit-channels))

(defmethod is-channel-available ((mode (eql :credit-legit-l)) symbolic-attribute raw-attributes)
  t)