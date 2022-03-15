(in-package :grammar-learning)

(deftest test-holistic-to-item-based-from-substitution-comprehension ()
  (let* ((experiment (set-up-cxn-inventory-and-repairs))
         (cxn-inventory (grammar (first (agents experiment)))))
    (comprehend "The tiny gray object is what shape?"
              :cxn-inventory cxn-inventory
              :gold-standard-meaning '((get-context ?source-1)
                                       (filter ?target-39552 ?target-2 ?size-4)
                                       (unique ?source-10 ?target-39552)
                                       (bind color-category ?color-2 gray)
                                       (filter ?target-1 ?source-1 ?shape-8)
                                       (bind attribute-category ?attribute-2 shape)
                                       (bind shape-category ?shape-8 thing)
                                       (filter ?target-2 ?target-1 ?color-2)
                                       (bind size-category ?size-4 small)
                                       (query ?target-8 ?source-10 ?attribute-2)))
    (comprehend "The large gray object is what shape?"
              :cxn-inventory cxn-inventory
              :gold-standard-meaning '((get-context ?source-1)
                                       (filter ?target-39552 ?target-2 ?size-4)
                                       (unique ?source-10 ?target-39552)
                                       (bind color-category ?color-2 gray)
                                       (filter ?target-1 ?source-1 ?shape-8)
                                       (bind attribute-category ?attribute-2 shape)
                                       (bind shape-category ?shape-8 thing)
                                       (filter ?target-2 ?target-1 ?color-2)
                                       (bind size-category ?size-4 large)
                                       (query ?target-8 ?source-10 ?attribute-2)))
    (test-repair-status 'repair-holistic->item-based-cxn
                        (second (multiple-value-list
                                 (comprehend "What is the color of the large object?"
              :cxn-inventory cxn-inventory
              :gold-standard-meaning '((get-context ?source-1)
                                       (filter ?target-2 ?target-1 ?size-4)
                                       (unique ?target-object-1 ?target-2)
                                       (bind shape-category ?shape-8 thing)
                                       (bind attribute-category ?attribute-4 color)
                                       (filter ?target-1 ?source-1 ?shape-8)
                                       (bind size-category ?size-4 large)
                                       (query ?target-4 ?target-object-1 ?attribute-4))))))))

(deftest test-holistic-to-item-based-from-double-substitution-comprehension ()
  (let* ((experiment (set-up-cxn-inventory-and-repairs))
         (cxn-inventory (grammar (first (agents experiment)))))
    (comprehend "The tiny gray object is what shape?"
              :cxn-inventory cxn-inventory
              :gold-standard-meaning '((get-context ?source-1)
                                       (filter ?target-39552 ?target-2 ?size-4)
                                       (unique ?source-10 ?target-39552)
                                       (bind color-category ?color-2 gray)
                                       (filter ?target-1 ?source-1 ?shape-8)
                                       (bind attribute-category ?attribute-2 shape)
                                       (bind shape-category ?shape-8 thing)
                                       (filter ?target-2 ?target-1 ?color-2)
                                       (bind size-category ?size-4 small)
                                       (query ?target-8 ?source-10 ?attribute-2)))
    (comprehend "The large red shiny object is what shape?"
              :cxn-inventory cxn-inventory
              :gold-standard-meaning '((get-context ?source-1)
                                       (filter ?target-61009 ?target-61008 ?size-4)
                                       (unique ?source-9 ?target-61009)
                                       (bind color-category ?color-4 red)
                                       (filter ?target-2 ?target-1 ?material-4)
                                       (bind shape-category ?shape-8 thing)
                                       (bind attribute-category ?attribute-2 shape)
                                       (filter ?target-1 ?source-1 ?shape-8)
                                       (bind material-category ?material-4 metal)
                                       (filter ?target-61008 ?target-2 ?color-4)
                                       (bind size-category ?size-4 large)
                                       (query ?target-7 ?source-9 ?attribute-2)))
    (test-repair-status 'repair-holistic->item-based-cxn
                        (second (multiple-value-list
                                 (comprehend "What is the material of the tiny gray object?"
              :cxn-inventory cxn-inventory
              :gold-standard-meaning '((get-context ?source-1)
                                       (bind attribute-category ?attribute-8 material)
                                       (bind size-category ?size-2 small)
                                       (filter ?target-2 ?target-1 ?color-2)
                                       (bind shape-category ?shape-8 thing)
                                       (filter ?target-1 ?source-1 ?shape-8)
                                       (bind color-category ?color-2 gray)
                                       (unique ?target-object-1 ?target-77105)
                                       (filter ?target-77105 ?target-2 ?size-2)
                                       (query ?target-4 ?target-object-1 ?attribute-8))))))))

(deftest test-multiple-holistic-to-item-based-repair-comprehension ()
  (let* ((experiment (set-up-cxn-inventory-and-repairs))
         (cxn-inventory (grammar (first (agents experiment)))))
    (comprehend "The gray object is what shape?"
              :cxn-inventory cxn-inventory
              :gold-standard-meaning '((get-context ?source-1)
                                       (filter ?target-2 ?target-1 ?color-2)
                                       (unique ?source-9 ?target-2)
                                       (bind shape-category ?shape-8 thing)
                                       (bind attribute-category ?attribute-2 shape)
                                       (filter ?target-1 ?source-1 ?shape-8)
                                       (bind color-category ?color-2 gray)
                                       (query ?target-7 ?source-9 ?attribute-2)))
    (comprehend "The large gray object is what shape?"
              :cxn-inventory cxn-inventory
              :gold-standard-meaning '((get-context ?source-1)
                                       (filter ?target-39552 ?target-2 ?size-4)
                                       (unique ?source-10 ?target-39552)
                                       (bind color-category ?color-2 gray)
                                       (filter ?target-1 ?source-1 ?shape-8)
                                       (bind attribute-category ?attribute-2 shape)
                                       (bind shape-category ?shape-8 thing)
                                       (filter ?target-2 ?target-1 ?color-2)
                                       (bind size-category ?size-4 large)
                                       (query ?target-8 ?source-10 ?attribute-2)))
    (comprehend "The yellow object is what shape?"
              :cxn-inventory cxn-inventory
              :gold-standard-meaning '((get-context ?source-1)
                                       (filter ?target-2 ?target-1 ?color-2)
                                       (unique ?source-9 ?target-2)
                                       (bind shape-category ?shape-8 thing)
                                       (bind attribute-category ?attribute-2 shape)
                                       (filter ?target-1 ?source-1 ?shape-8)
                                       (bind color-category ?color-2 yellow)
                                       (query ?target-7 ?source-9 ?attribute-2)))
    (test-repair-status 'repair-holistic->item-based-cxn
                        (second (multiple-value-list
                                 (comprehend "What is the shape of the large gray thing?"
              :cxn-inventory cxn-inventory
              :gold-standard-meaning '((get-context ?source-1)
                                       (filter ?target-67855 ?target-2 ?size-4)
                                       (unique ?target-object-1 ?target-67855)
                                       (bind color-category ?color-2 gray)
                                       (filter ?target-1 ?source-1 ?shape-8)
                                       (bind attribute-category ?attribute-2 shape)
                                       (bind shape-category ?shape-8 thing)
                                       (filter ?target-2 ?target-1 ?color-2)
                                       (bind size-category ?size-4 large)
                                       (query ?target-4 ?target-object-1 ?attribute-2))))))))

(defun test-holistic-to-item-based-duplicates-comprehension ()
  (let* ((experiment (set-up-cxn-inventory-and-repairs))
         (cxn-inventory (grammar (first (agents experiment)))))
    (comprehend "Are any green cubes visible?"
              :cxn-inventory cxn-inventory
              :gold-standard-meaning '((get-context ?source-1)
                                       (exist ?target-44 ?target-2)
                                       (bind color-category ?color-8 green)
                                       (filter ?target-1 ?source-1 ?shape-2)
                                       (bind shape-category ?shape-2 cube)
                                       (filter ?target-2 ?target-1 ?color-8)))
    (comprehend "Are any green spheres visible?"
              :cxn-inventory cxn-inventory
              :gold-standard-meaning '((get-context ?source-1)
                                       (exist ?target-44 ?target-2)
                                       (bind color-category ?color-8 green)
                                       (filter ?target-1 ?source-1 ?shape-2)
                                       (bind shape-category ?shape-2 sphere)
                                       (filter ?target-2 ?target-1 ?color-8)))
    (test-repair-status 'repair-holistic->item-based-cxn
                        (second (multiple-value-list
                                 (comprehend "Are there fewer matte cubes than large green shiny cubes?"
              :cxn-inventory cxn-inventory
              :gold-standard-meaning '((bind material-category ?material-2 rubber)
                                       (filter ?target-9839 ?source-1 ?shape-5228)
                                       (bind color-category ?color-8 green)
                                       (filter ?target-2 ?target-1 ?material-4)
                                       (bind material-category ?material-4 metal)
                                       (filter ?target-1 ?source-1 ?shape-2)
                                       (bind shape-category ?shape-2 cube)
                                       (bind shape-category ?shape-5228 cube)
                                       (filter ?target-9842 ?target-2 ?color-8)
                                       (bind size-category ?size-4 large)
                                       (get-context ?source-1)
                                       (filter ?target-9843 ?target-9842 ?size-4)
                                       (filter ?target-9845 ?target-9839 ?material-2)
                                       (count! ?count-5 ?target-9845)
                                       (count! ?count-6 ?target-9843)
                                       (less-than ?target-74 ?count-5 ?count-6))))))))

(defun run-holistic-to-item-based-tests ()
  (test-holistic-to-item-based-from-substitution-comprehension)
  (test-holistic-to-item-based-from-double-substitution-comprehension)
  (test-multiple-holistic-to-item-based-repair-comprehension)
  (test-holistic-to-item-based-duplicates-comprehension)
  )

#|

(defun test-multiple-holistic-to-item-based-repair-comprehension ()
  (defparameter *experiment* (set-up-cxn-inventory-and-repairs))
   (let* (
         (cxn-inventory (grammar (first (agents *experiment*)))))
     (enable-learning cxn-inventory)
    
  (comprehend "The gray object is what shape?"
              :cxn-inventory cxn-inventory
              :gold-standard-meaning '((get-context ?source-1)
                                       (filter ?target-2 ?target-1 ?color-2)
                                       (unique ?source-9 ?target-2)
                                       (bind shape-category ?shape-8 thing)
                                       (bind attribute-category ?attribute-2 shape)
                                       (filter ?target-1 ?source-1 ?shape-8)
                                       (bind color-category ?color-2 gray)
                                       (query ?target-7 ?source-9 ?attribute-2)))
    
  (comprehend "The large gray object is what shape?"
              :cxn-inventory cxn-inventory
              :gold-standard-meaning '((get-context ?source-1)
                                       (filter ?target-39552 ?target-2 ?size-4)
                                       (unique ?source-10 ?target-39552)
                                       (bind color-category ?color-2 gray)
                                       (filter ?target-1 ?source-1 ?shape-8)
                                       (bind attribute-category ?attribute-2 shape)
                                       (bind shape-category ?shape-8 thing)
                                       (filter ?target-2 ?target-1 ?color-2)
                                       (bind size-category ?size-4 large)
                                       (query ?target-8 ?source-10 ?attribute-2)))

  (comprehend "The yellow object is what shape?"
              :cxn-inventory cxn-inventory
              :gold-standard-meaning '((get-context ?source-1)
                                       (filter ?target-2 ?target-1 ?color-2)
                                       (unique ?source-9 ?target-2)
                                       (bind shape-category ?shape-8 thing)
                                       (bind attribute-category ?attribute-2 shape)
                                       (filter ?target-1 ?source-1 ?shape-8)
                                       (bind color-category ?color-2 yellow)
                                       (query ?target-7 ?source-9 ?attribute-2)))

  (comprehend "What is the shape of the large gray thing?"
              :cxn-inventory cxn-inventory
              :gold-standard-meaning '((get-context ?source-1)
                                       (filter ?target-67855 ?target-2 ?size-4)
                                       (unique ?target-object-1 ?target-67855)
                                       (bind color-category ?color-2 gray)
                                       (filter ?target-1 ?source-1 ?shape-8)
                                       (bind attribute-category ?attribute-2 shape)
                                       (bind shape-category ?shape-8 thing)
                                       (filter ?target-2 ?target-1 ?color-2)
                                       (bind size-category ?size-4 large)
                                       (query ?target-4 ?target-object-1 ?attribute-2)))))

(defun test-double-holistic-to-item-based-from-substitution-repair-comprehension ()
  (defparameter *experiment* (set-up-cxn-inventory-and-repairs))
   (let* (
         (cxn-inventory (grammar (first (agents *experiment*)))))
     (enable-learning cxn-inventory)
    
  (comprehend "The tiny gray object is what shape?"
              :cxn-inventory cxn-inventory
              :gold-standard-meaning '((get-context ?source-1)
                                       (filter ?target-39552 ?target-2 ?size-4)
                                       (unique ?source-10 ?target-39552)
                                       (bind color-category ?color-2 gray)
                                       (filter ?target-1 ?source-1 ?shape-8)
                                       (bind attribute-category ?attribute-2 shape)
                                       (bind shape-category ?shape-8 thing)
                                       (filter ?target-2 ?target-1 ?color-2)
                                       (bind size-category ?size-4 small)
                                       (query ?target-8 ?source-10 ?attribute-2)))
    
  (comprehend "The large yellow object is what shape?"
              :cxn-inventory cxn-inventory
              :gold-standard-meaning '((get-context ?source-1)
                                       (filter ?target-39552 ?target-2 ?size-4)
                                       (unique ?source-10 ?target-39552)
                                       (bind color-category ?color-2 yellow)
                                       (filter ?target-1 ?source-1 ?shape-8)
                                       (bind attribute-category ?attribute-2 shape)
                                       (bind shape-category ?shape-8 thing)
                                       (filter ?target-2 ?target-1 ?color-2)
                                       (bind size-category ?size-4 large)
                                       (query ?target-8 ?source-10 ?attribute-2)))

  (comprehend "What is the shape of the large yellow object?"
              :cxn-inventory cxn-inventory
              :gold-standard-meaning '((get-context ?source-1)
                                       (filter ?target-56342 ?target-2 ?size-4)
                                       (unique ?target-object-1 ?target-56342)
                                       (bind color-category ?color-16 yellow)
                                       (filter ?target-1 ?source-1 ?shape-8)
                                       (bind attribute-category ?attribute-2 shape)
                                       (bind shape-category ?shape-8 thing)
                                       (filter ?target-2 ?target-1 ?color-16)
                                       (bind size-category ?size-4 large)
                                       (query ?target-4 ?target-object-1 ?attribute-2)))))
 |#