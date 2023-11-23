(in-package :cle)

;; ----------------------
;; + Available channels +
;; ----------------------

(defmethod get-all-channels ((mode (eql :beans)))
  "BEANS dataset."
  (reverse `(,'area
             ,'perimeter
             ,'major-axis-length
             ,'minor-axis-length
             ,'aspect-ratio
             ,'eccentricity
             ,'convex-area
             ,'equiv-diameter
             ,'extent
             ,'solidity
             ,'roundness
             ,'compactness
             ,'shape-factor-1
             ,'shape-factor-2
             ,'shape-factor-3
             ,'shape-factor-4)))

(defmethod is-channel-available ((mode (eql :beans)) symbolic-attribute raw-attributes)
  t)
