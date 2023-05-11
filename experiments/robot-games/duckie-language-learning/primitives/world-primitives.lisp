(in-package :duckie-language-learning)

;; -----------------------------------------
;; + Primitives for a physical environment +
;; -----------------------------------------

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
               (dex:get (concatenate 'string *server-url* "scan-world")
                        :read-timeout 300))
              (printlijn (progn (pprint (cl-json:decode-json-from-string response))))
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
               (dex:get (concatenate 'string *server-url* "scan-world")
                        :read-timeout 300))
              (world-json
               (cl-json::decode-json-from-string response))
              (observed-world
               (json-to-world world-json)))
         (equal-entity world observed-world)))))
  :primitive-inventory *duckie-world-primitives*)

(defun json-to-world (json)
  (let (#|(zones (loop for zone in json
                     collect (intern (upcase
                                      (replace-char (first (rest zone))
                                                    #\_ #\-)))))|#
        (objects (loop for zone in json
                       if (rest (rest zone))
                         collect (json-object-to-object (rest zone)))))
    (make-instance 'object-set :objects objects)))

(defun json-object-to-object (json-object)
  (let* ((object-type
          (intern
           (upcase
            (replace-char
             (rest (assoc :object--type json-object))
             #\_ #\-))))
         (building-fun
          (rest (assoc :building--function json-object)))
         (building-function
          (if building-fun
            (intern
             (upcase
              (replace-char
               building-fun
               #\_ #\-)))))
         (color
          (intern (upcase (rest (assoc :color json-object)))))
         (rfid
          (rest (assoc :rfid json-object)))
         #|(zone
          (intern (upcase
                   (replace-char (first json-object)
                                 #\_ #\-))))|#
         (obj
          (make-instance object-type
                         :color color
                         :rfid rfid
                         ;:zone zone
                         )))
    (if building-function
      (setf (building-function obj) building-function))
    obj))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; MOVE-TO ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprimitive move-to ((car duckie-agent-car)
                       (zone zone-category))
  ;;first case: object is bound
  ((zone => car)
   (dex:get (format nil "http://192.168.1.2:7000/move-to/~a" (id zone))
            :read-timeout 300)
   (setf (zone (get-data *ontology* 'agent-car))
         zone)
   (bind (car 1.0 (get-data *ontology* 'agent-car))))
  ((car zone => )
   ;;set location duckie-car from world to zone
   (equal (zone car) (zone zone)))
  :primitive-inventory *duckie-world-primitives*)
