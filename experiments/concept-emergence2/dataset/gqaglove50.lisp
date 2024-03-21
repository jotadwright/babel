(in-package :cle)

;; ----------------------
;; + Available channels +
;; ----------------------

(defun gqaglove50-channels ()
  "GQA-GLOVE50 dataset."
  `(,'dim-0
    ,'dim-1
    ,'dim-2
    ,'dim-3
    ,'dim-4
    ,'dim-5
    ,'dim-6
    ,'dim-7
    ,'dim-8
    ,'dim-9
    ,'dim-10
    ,'dim-11
    ,'dim-12
    ,'dim-13
    ,'dim-14
    ,'dim-15
    ,'dim-16
    ,'dim-17
    ,'dim-18
    ,'dim-19
    ,'dim-20
    ,'dim-21
    ,'dim-22
    ,'dim-23
    ,'dim-24
    ,'dim-25
    ,'dim-26
    ,'dim-27
    ,'dim-28
    ,'dim-29
    ,'dim-30
    ,'dim-31
    ,'dim-32
    ,'dim-33
    ,'dim-34
    ,'dim-35
    ,'dim-36
    ,'dim-37
    ,'dim-38
    ,'dim-39
    ,'dim-40
    ,'dim-41
    ,'dim-42
    ,'dim-43
    ,'dim-44
    ,'dim-45
    ,'dim-46
    ,'dim-47
    ,'dim-48
    ,'dim-49))

(defmethod get-all-channels ((mode (eql :gqaglove50)))
  (gqaglove50-channels))

(defmethod is-channel-available ((mode (eql :gqaglove50)) symbolic-attribute raw-attributes)
  t)