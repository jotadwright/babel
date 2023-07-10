(in-package :cle)

;; ----------------------------------------
;; + Available channels - CLEVR SIMULATED +
;; ----------------------------------------
(defmethod get-all-channels ((mode (eql :clevr-simulated)))
  (reverse `(
             ,'xpos ,'ypos ,'zpos ;; position
             ,'area ;; size
             ,'wh-ratio ;; shape
             ,'nr-of-sides ,'nr-of-corners ;; shape
             ,'r ,'g ,'b ;; color
             ,'roughness ;; material
             ,'xpos-3d ,'ypos-3d ,'zpos-3d ;; 3d-position
             ,'rotation ;; rotation
             )))

(defmethod is-channel-available ((mode (eql :clevr-simulated)) symbolic-attribute raw-attributes)
  (let ((continuous-attributes (mapcar 'first raw-attributes)))
    (case symbolic-attribute
      (:COLOR (or (if (member 'R continuous-attributes) t nil)
                  (if (member 'G continuous-attributes) t nil)
                  (if (member 'B continuous-attributes) t nil)))
      (:SIZE (if (member 'AREA continuous-attributes) t nil))
      (:SHAPE (or (if (member 'NR-OF-CORNERS continuous-attributes) t nil)
                  (if (member 'NR-OF-SIDES continuous-attributes) t nil)
                  (if (member 'WH-RATIO continuous-attributes) t nil)))
      (:MATERIAL (if (member 'ROUGHNESS continuous-attributes) t nil))
      (:XPOS (if (member 'XPOS continuous-attributes) t nil))
      (:ZPOS (or (if (member 'ZPOS continuous-attributes) t nil)
                 (if (member 'YPOS continuous-attributes) t nil))))))

;; ----------------------------------------
;; + Available channels - CLEVR EXTRACTED +
;; ----------------------------------------
(defmethod get-all-channels ((mode (eql :clevr-extracted)))
  (reverse `(
             ,'xpos ,'ypos
             ,'width ,'height
             ,'angle
             ,'corners
             ,'area ,'relative-area
             ,'bb-area ,'bb-area-ratio
             ,'wh-ratio
             ,'circle-distance
             ,'white-level ,'black-level
             ,'color-mean-l ,'color-mean-a ,'color-mean-b
             ,'color-std-l ,'color-std-a ,'color-std-b
             )))

(defmethod is-channel-available ((mode (eql :clevr-extracted)) symbolic-attribute raw-attributes)
  (let ((continuous-attributes (mapcar 'first raw-attributes)))
    (case symbolic-attribute
      (:COLOR (or (if (member 'color-mean-l continuous-attributes) t nil)
                  (if (member 'color-mean-a continuous-attributes) t nil)
                  (if (member 'color-mean-b continuous-attributes) t nil)
                  (if (member 'color-std-l continuous-attributes) t nil)
                  (if (member 'color-std-a continuous-attributes) t nil)
                  (if (member 'color-std-b continuous-attributes) t nil)))
      (:SIZE (or (if (member 'width continuous-attributes) t nil)
                  (if (member 'height continuous-attributes) t nil)
                  (if (member 'area continuous-attributes) t nil)
                  (if (member 'relative-area continuous-attributes) t nil)
                  (if (member 'bb-area continuous-attributes) t nil)
                  (if (member 'bb-area-ration continuous-attributes) t nil)))
                  
      (:SHAPE (or (if (member 'corners continuous-attributes) t nil)
                  (if (member 'circle-distance continuous-attributes) t nil)
                  (if (member 'wh-ratio continuous-attributes) t nil)))
      (:MATERIAL (or (if (member 'white-level continuous-attributes) t nil)
                     (if (member 'black-level continuous-attributes) t nil))
      (:XPOS (if (member 'xpos continuous-attributes) t nil))
      (:ZPOS (if (member 'ypos continuous-attributes) t nil))))))
