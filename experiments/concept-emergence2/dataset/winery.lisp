(in-package :cle)

;; ----------------------
;; + Available channels +
;; ----------------------

(defmethod get-all-channels ((mode (eql :winery)))
  "WINE dataset."
  (reverse `(
             ,'fixed-acidity
             ,'volatile-acidity
             ,'citric-acid
             ,'residual-sugar
             ,'chlorides
             ,'free-sulfur-dioxide
             ,'total-sulfur-dioxide
             ,'density
             ,'pH
             ,'sulphates
             ,'alcohol)))

(defmethod is-channel-available ((mode (eql :winery)) symbolic-attribute raw-attributes)
  t)