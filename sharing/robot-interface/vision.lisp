(in-package :robot-interface)

(export '(take-picture observe-world))

(defgeneric take-picture (robot &key open)
  (:documentation "Takes a picture, copies it from the robot to your computer and
   optionally opens it. Returns the filename of the image."))

(defgeneric observe-world (robot &key open)
  (:documentation "Takes a picture and performs object segmentation on it.
   The 'open' keyword can be used to show the image with the result of
   segmentation as an overlay. Returns both the detected features and
   the filename of the image."))
