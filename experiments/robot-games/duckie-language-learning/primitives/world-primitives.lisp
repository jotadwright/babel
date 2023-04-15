(in-package :duckie-language-learning)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; SCAN-WORLD ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprimitive scan-world ((world object-set))
  ;;Case 1 world is unbound
  ((=> world)
   (let ((world (get-data *ontology* 'world)))
     (if world
       (bind (world 1.0 world))
       (let* ((response 
               (dex:get "http://192.168.2.5:7000/scan-world"
                        :read-timeout 300))
              (world-json
               (cl-json:decode-json-from-string response))
              (world
               (json-to-world world-json)))
         (set-data  *ontology* 'world world)
         (bind (world 1.0 world))))))
  
  ;;Case 2: world is bound
  ((world =>)
   (let ((w (get-data *ontology* 'world)))
     (if w
       (equal-entity world w)
       (let* ((response 
          (dex:get "http://192.168.2.5:7000/scan-world"
                   :read-timeout 300))
          (world-json
           (cl-json::decode-json-from-string response))
          (observed-world
           (json-to-world world-json)))
     (equal-entity world observed-world)))))
  :primitive-inventory *duckie-world-primitives*)


(defun json-to-world (json)
  (let ((objects (loop for zone in json
                       for object-type = (intern (upcase (replace-char (rest (assoc :object--type (rest zone)))  #\_ #\-)))
                       for building-function = (rest (assoc :building--function (rest zone)))
                       for color = (rest (assoc :color (rest zone)))
                       for rfid = (rest (assoc :rfid (rest zone)))
                       for z = (first (rest zone))
                       for obj = (make-instance object-type
                                       ;  :building-function building-function
                                         :color color
                                         :rfid rfid
                                         :zone z)
                       do (if building-function (setf (building-function obj) building-function))
                       collect obj
                       )))
    (make-instance 'object-set :objects objects)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; MOVE-TO ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprimitive move-to ((zone zone-category))
  ;;first case: object is bound
  ((zone =>)
   (dex:get (format nil "http://192.168.2.5:7000/move-to/~a" (id zone))
                        :read-timeout 300))
  :primitive-inventory *duckie-world-primitives*)