(in-package :cle)

;; --------------
;; + Experiment +
;; --------------

(defclass cle-experiment (experiment)
  ()
  (:documentation "The experiment class."))

(defmethod initialize-instance :after ((experiment cle-experiment) &key)
  "Create the population and load the scenes from file."
  ;; 0. read data
  (let* ((fname (get-configuration experiment :data-fname))
         ;; convert fname to list of strings representing channels
         (channels (cdr (split-string (pathname-name fname) "-")))
         ;; convert naming convention to actual interned channels
         (clevr-channels  (if (equal channels '("all"))
                            `(,'area ,'color ,'roughness
                                     ,'sides-and-corners ,'wh-ratio 
                                     ,'xpos ,'ypos ,'zpos)
                            (loop for channel in channels
                                  collect (intern (string-upcase channel)))))(convert-fname-to-channel-list fname))
    (set-configuration experiment :scene-ids (read-scene-ids fname))
    (set-configuration experiment :current-scene-idx 0)
    (set-configuration experiment :clevr-channels clevr-channels))
  ;; 1. population
  (setf (population experiment)
        (loop for i from 1 to (get-configuration experiment :population-size)
              collect (make-instance 'cle-agent :experiment experiment)))
  ;; 2. load clevr scenes
  (setf (world experiment) (make-instance 'clevr-world :data-sets (list "t-val"))))
