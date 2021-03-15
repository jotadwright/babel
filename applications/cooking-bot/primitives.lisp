(in-package :cooking-bot)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; This file contains the primitives involved in the cooking-bot environment. ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; get-kitchen ;;
;;;;;;;;;;;;;;;;;

(defgeneric get-kitchen (?kitchen)
  (:documentation "Provides access to the current state of the kitchen."))

(defmethod get-kitchen ((?kitchen var))
  "Returns a binding to *kitchen*."
  (make-instance 'binding :binding-var ?kitchen :binding-value *kitchen*))

(defmethod get-kitchen ((kitchen kitchen))
  "Returns a binding to *kitchen*."
  (equal-ontology-objects kitchen *kitchen*))


;; fetch-ingredient ;;
;;;;;;;;;;;;;;;;;;;;;;

(defgeneric fetch-ingredient (?ingredient ?new-kitchen-state -kitchen-state -object-class -amount -unit)
  (:documentation "Fetches an ingredient, doses it and puts it on the countertop."))

(defmethod fetch-ingredient ((?new-ingredient t) (?new-kitchen-state t)
                             (kitchen-state t) (object-class t)
                             (amount t) (unit t))
  nil)

(defmethod fetch-ingredient ((?new-ingredient var) (?new-kitchen-state var)
                             (kitchen-state kitchen) (object-class ingredient)
                             (amount number) (unit unit))
  (let ((new-kitchen-state (copy-ontology-object kitchen-state)))
    (loop for ingredient in (append (contents (fridge new-kitchen-state)) (contents (pantry new-kitchen-state)))
           when (and (eql (type-of object-class) (type-of ingredient))
                  (>= (amount (quantity ingredient)) amount))
           return
           (let ((new-ingredient (copy-ontology-object ingredient)))
             (setf (amount (quantity ingredient)) (- (amount (quantity ingredient)) amount))
             (setf (amount (quantity new-ingredient)) amount)
             (setf (contents (countertop new-kitchen-state)) (append (list new-ingredient) (contents (countertop new-kitchen-state))))
             (list (make-instance 'binding :binding-var ?new-ingredient :binding-value new-ingredient)
                   (make-instance 'binding :binding-var ?new-kitchen-state :binding-value new-kitchen-state))))))
             
             
