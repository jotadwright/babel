;(ql:quickload :coco-grammar)
(in-package :coco-grammar)
(activate-monitor trace-fcg)

(wi::add-element (make-html *COCO*))

(comprehend "how many cows are there?")
(comprehend "is there a sheep left of the cow?")
(comprehend "how many sheep are right of the cow?")
(comprehend "what category is the thing left of the dog?")
(comprehend "how many cats are left of the dog and right of the cow?")
(comprehend "how many things are cats or dogs?")
(comprehend "are there an equal number of cows and sheep?")
(comprehend "are there more cows than sheep?")
(comprehend "are there fewer cows than sheep?")

(comprehend "is there a zebra left of the giraffe?")
(comprehend "is there a sea below the sky?")

(defun test ()
  (multiple-value-bind (irl cipn)
      (comprehend "is there a sea below the sky?")
    (let* ((data (fcg::analyse-solution cipn '<-))
           (s-dot (fcg::unit-bindings->graph :data data
                                             :construction-inventory *COCO*
                                             :prefered-font "Arial"))
           (svg (wi::s-dot->svg s-dot)))
      (add-element
       `((div) ,svg)))))

(test)

