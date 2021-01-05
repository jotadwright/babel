;; (ql:quickload :cooking-bot)

(in-package :cooking-bot)



(setf *kitchen* (make-instance 'kitchen
                               :fridge (make-instance 'fridge
                                                      :contents (list (make-instance 'milk
                                                                                     :quantity (make-instance 'quantity
                                                                                                              :amount 1000
                                                                                                              :unit (make-instance 'ml)))
                                                                      (make-instance 'butter
                                                                                     :quantity (make-instance 'quantity
                                                                                                              :amount 500
                                                                                                              :unit (make-instance 'g)))
                                                                      (make-instance 'egg
                                                                                     :quantity (make-instance 'quantity
                                                                                                              :amount 12
                                                                                                              :unit (make-instance 'piece)))))
                               :pantry (make-instance 'pantry
                                                      :contents (list (make-instance 'flour
                                                                                     :quantity (make-instance 'quantity
                                                                                                              :amount 1000
                                                                                                              :unit (make-instance 'g)))
                                                                      (make-instance 'sugar
                                                                                     :quantity (make-instance 'quantity
                                                                                                              :amount 1000
                                                                                                              :unit '(make-instance 'g)))))))


(get-kitchen (var a))


(def-predicate-network ((bind ?milk-concept milk) (bind ?milk-amount 500) (bind ?mik-unit ml)
                        (get-kitchen ?kitchen) (fetch-ingredient ?ingredient ?new-kitchen ?kitchen ?milk-concept ?milk-amount ?milk-unit)))