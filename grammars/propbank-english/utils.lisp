(in-package :propbank-english)

(defun comprehend-and-extract-frames (utterance &key (cxn-inventory *fcg-constructions*) (silent nil) (syntactic-analysis nil))
  (multiple-value-bind (solution cipn)
      (comprehend utterance :cxn-inventory cxn-inventory :silent silent :syntactic-analysis syntactic-analysis)
    (declare (ignore solution))
    (unless silent
      (add-element `((h3 :style "margin-bottom:3px;") "Frame representation:"))
      (add-element (make-html (extract-frames (car-resulting-cfs (cipn-car cipn))) :expand-initially t)))))