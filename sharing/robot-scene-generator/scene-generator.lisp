(in-package :scene-generator)

(export '(start-scene-server stop-scene-server scene-server-running? generate-new-scene put-back-scene generate-unique-scene))


;;;; API
;;;;;;;;

(defparameter *scene-server-host* "localhost")
(defparameter *scene-server-port* 4242)
(defparameter *scene-server* nil)

(defun start-scene-server ()  
  (unless (scene-server-running?)
    (setf (html-mode) :html5) 
    (setf *scene-server*
          (start (make-instance 'hunchentoot:easy-acceptor
                                :address *scene-server-host*
                                :port *scene-server-port*
                                :message-log-destination nil
                                :access-log-destination nil)))
    (format nil "Scene server started at ~a:~a" *scene-server-host* *scene-server-port*)))

(defun stop-scene-server ()
  (when (scene-server-running?)
    (stop *scene-server*)
    (setf *scene-server* nil)))

(defun scene-server-running? ()
  (not (null *scene-server*)))

(defvar *current-scene* nil)

(defun generate-new-scene (n)
  (reset-colors) ;; when using a limited number of colors
  (let* ((svg-objects (random-svgs n :to-html nil :draw-bbox nil))
         (svg-html (wrap-in-svg (list-of-strings->string (mapcar #'to-html svg-objects)))))
    (setf *current-scene* svg-html)
    (clear-page)
    (add-element svg-html)
    (reload-content)))

(defun color->value (color)
  (case color
    (red (list (randint :start 200 :end 255)
               (randint :start 0 :end 10)
               (randint :start 0 :end 10)))
    (blue (list (randint :start 0 :end 10)
               (randint :start 0 :end 10)
               (randint :start 200 :end 255)))))

(defun ypos->value (ypos)
  (case ypos
    (top (randint :start 200 :end 500))
    (bottom (randint :start 500 :end 800))))

(defun xpos->value (xpos)
  (case xpos
    (left (randint :start 250 :end 650))
    (right (randint :start 700 :end 1200))))

(defun area->value (area)
  (case area
    (large (randint :start 100 :end 125))
    (small (randint :start 25 :end 50))))

(defun combo->value (combo)
  (list (color->value (first combo))
        (ypos->value (second combo))
        (xpos->value (third combo))
        ;(area->value (third combo))
        ))

(defun combos->values (combos)
  (loop for combo in combos
        collect (combo->value combo)))

(defun generate-unique-scene (n)
  (let* ((features '( (left right) (top bottom) (red blue) ;(small large)
                     ))
         (all-combos (apply #'combinations features))
         (n-combos (random-elts all-combos n))
         (data (combos->values n-combos))
         svg-objects
         svg-html)
    (setf svg-objects
          (loop for d in data
                collect (make-instance 'svg-circle
                                       :color (first d)
                                       :ypos (second d)
                                       :xpos (third d)
                                       :radius 45
                                       )))
    (setf svg-html
          (wrap-in-svg (list-of-strings->string (mapcar #'to-html svg-objects))))
    (setf *current-scene* svg-html)
    (clear-page)
    (add-element svg-html)
    (reload-content)))

(defun put-back-scene ()
  (clear-page)
  (add-element *current-scene*)
  (reload-content))
