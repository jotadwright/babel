(in-package :propbank-english)

(defun comprehend-and-extract-frames (utterance &key (cxn-inventory *fcg-constructions*))
  (multiple-value-bind (solution cipn)
      (comprehend utterance :cxn-inventory cxn-inventory)
    (add-element (make-html (extract-frames (car-resulting-cfs (cipn-car cipn))) :expand-initially t))))