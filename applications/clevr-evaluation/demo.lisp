
;; (ql:quickload :clevr-evaluation)

(in-package :clevr-evaluation)

;; FCG MONITORS
(activate-monitor trace-fcg)
;; IRL MONITORS
(activate-monitor trace-irl-in-web-browser)
;; CLEVR MONITOR
(activate-monitor clevr-web-monitor)

(comprehend (preprocess-sentence "what material is the red cube?"))

(evaluate-clevr
 (make-instance 'clevr-world :data-sets '("val") :load-questions t)
 :nr-of-contexts 10 :nr-of-questions 10)

;;;; Manually create a scene for in the paper
(defparameter *scene-from-paper*
  (make-instance 'clevr-object-set :id 'clevr-context
                 :objects (list (make-instance 'clevr-object :shape 'cube :size 'large :color 'yellow :material 'metal)
                                (make-instance 'clevr-object :shape 'cylinder :size 'large :color 'purple :material 'rubber)
                                (make-instance 'clevr-object :shape 'cube :size 'large :color 'yellow :material 'metal)
                                (make-instance 'clevr-object :shape 'cylinder :size 'large :color 'cyan :material 'rubber)
                                (make-instance 'clevr-object :shape 'sphere :size 'large :color 'red :material 'metal)
                                (make-instance 'clevr-object :shape 'cube :size 'small :color 'brown :material 'metal)
                                (make-instance 'clevr-object :shape 'cube :size 'small :color 'red :material 'metal)
                                (make-instance 'clevr-object :shape 'cylinder :size 'small :color 'red :material 'metal)
                                (make-instance 'clevr-object :shape 'sphere :size 'small :color 'blue :material 'rubber)
                                (make-instance 'clevr-object :shape 'sphere :size 'small :color 'green :material 'rubber))))

(set-data *clevr-ontology* 'clevr-context *scene-from-paper*)

(evaluate-irl-program
 '((get-context ?context)
   (filter ?cube-set ?context ?shape-1)
   (filter ?red-cube-set ?cube-set ?color-1)
   (unique ?red-cube ?red-cube-set)
   (query ?target ?red-cube ?attribute-1)
   (bind shape-category ?shape-1 cube)
   (bind color-category ?color-1 red)
   (bind attribute-category ?attribute-1 material))
 *clevr-ontology*)