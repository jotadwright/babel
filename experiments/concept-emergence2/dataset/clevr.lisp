(in-package :cle)

(defmethod get-all-channels ((mode (eql :clevr)))
  "CLEVR dataset."
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
             ,'rgb-mean-r ,'rgb-mean-g ,'rgb-mean-b
             ,'rgb-std-r ,'rgb-std-g ,'rgb-std-b
             )))

(defmethod is-channel-available ((mode (eql :clevr)) symbolic-attribute raw-attributes)
  (let ((continuous-attributes (loop for key being the hash-keys of raw-attributes
                                     collect key)))
    (case symbolic-attribute
      (:COLOR (or (if (member 'rgb-mean-r continuous-attributes) t nil)
                  (if (member 'rgb-mean-g continuous-attributes) t nil)
                  (if (member 'rgb-mean-b continuous-attributes) t nil)
                  (if (member 'rgb-std-r continuous-attributes) t nil)
                  (if (member 'rgb-std-g continuous-attributes) t nil)
                  (if (member 'rgb-std-b continuous-attributes) t nil)
                  ))
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
                     (if (member 'black-level continuous-attributes) t nil)))
      (:XPOS (if (member 'xpos continuous-attributes) t nil))
      (:ZPOS (if (member 'ypos continuous-attributes) t nil)))))

(defmethod get-all-channels ((mode (eql :cogent)))
  "CLEVR COGENT dataset."
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
             ,'rgb-mean-r ,'rgb-mean-g ,'rgb-mean-b
             ,'rgb-std-r ,'rgb-std-g ,'rgb-std-b
             )))

(defmethod is-channel-available ((mode (eql :cogent)) symbolic-attribute raw-attributes)
  (let ((continuous-attributes (loop for key being the hash-keys of raw-attributes
                                     collect key)))
    (case symbolic-attribute
      (:COLOR (or (if (member 'rgb-mean-r continuous-attributes) t nil)
                  (if (member 'rgb-mean-g continuous-attributes) t nil)
                  (if (member 'rgb-mean-b continuous-attributes) t nil)
                  (if (member 'rgb-std-r continuous-attributes) t nil)
                  (if (member 'rgb-std-g continuous-attributes) t nil)
                  (if (member 'rgb-std-b continuous-attributes) t nil)
                  ))
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
                     (if (member 'black-level continuous-attributes) t nil)))
      (:XPOS (if (member 'xpos continuous-attributes) t nil))
      (:ZPOS (if (member 'ypos continuous-attributes) t nil)))))
