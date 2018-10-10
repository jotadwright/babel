(in-package :scene-generator)


;;;; Constants that are used to define where the SVGs will be placed

(defparameter *window-width* 1600)
(defparameter *window-height* 1100)
(defparameter *xrange* (cons 250 1200))
(defparameter *yrange* (cons 200 800))

;; Wrap html code in an svg tag; probably the html
;; will be svg elements
(defun wrap-in-svg (html)
  "Wrap the given html in an svg tag"
  (with-html-output-to-string (*standard-output*)
    (htm (:svg :xmlns "http://www.w3.org/2000/svg"
               :version "1.1"
               :viewBox (format nil "0 0 ~a ~a" *window-width* *window-height*)
               :preserveAspectRatio "xMidYMid meet"
               :id "svg-context"
               (str html)))))