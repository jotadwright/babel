(in-package :cle)

;; ----------------------
;; + Available channels +
;; ----------------------

(defmethod get-all-channels ((mode (eql :credit)))
  (reverse `(,'V1
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
             ,'V28)))

(defmethod is-channel-available ((mode (eql :credit)) symbolic-attribute raw-attributes)
  t)