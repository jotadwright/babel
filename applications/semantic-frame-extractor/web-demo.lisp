
;;Frame-based parsing to retrieve argumentation structure
;;Katrien Beuls (Odycceus)
;;########################################################

;; (ql:quickload :frame-extractor)
(in-package :frame-extractor)

(defun header ()
  (clear-page)
  (add-element '((hr)))
  (add-element '((h1) "Argumentation frames with FCG"))
  (add-element '((h3) "Katrien Beuls"))
  
#|  
  (add-element '((hr)))
  (add-element '((p) ((a :name "start") "Menu:")))
  (add-element '((p)  ((a :href "#WD-1") "1.  Introduction")))
  (add-element '((p)  ((a :href "#WD-2") "2.  Creativity through the free combination of constructions")))
  (add-element '((p)  ((a :href "#WD-3") "3.  Creativity through the appropriate violation of usual constraints")))
  (add-element '((p)  ((a :href "#WD-4") "4.  References")))
    |#
  (add-element '((hr))))


(defun introduction ()
  (add-element '((a :name "WD-1")))
  (add-element '((h2) "1.  Introduction"))
  (add-element '((p) "This interactive web demonstration introduces the idea of frame-based parsing to retrieve argumentation structures from newspaper articles and comments. It uses a grammar fragment implemented in Fluid Construction Grammar specifically for this target." ))
  (add-element '((hr))))

(defun due-to-frame ()
  (let ((solution nil))
    (add-element '((a :name "WD-2")))
    (add-element '((h2) "2.  X due to Y"))
    (add-element '((p) ""))
  
    (activate-monitor trace-fcg)
    (setf solution
          (second (multiple-value-list (comprehend "Oxygen levels in oceans have fallen 2% in 50 years due to climate change."))))

    (run-pie solution)
  
    (add-element '((hr)))))



(defun make-demo ()
  (create-static-html-page "Argumentation frames with FCG"
    (header)
    (introduction)
    (due-to-frame))
  )

;; (make-demo)
