(in-package :pattern-finding-old)

;;;;;;;;;;;;;;;;;;;;;;;
;; make holistic cxn ;;
;;;;;;;;;;;;;;;;;;;;;;;
        
(defun make-holistic-cxn (form meaning form-args meaning-args cxn-inventory)
  (let* (;; make the cxn names
         (cxn-name
          (make-cxn-name form cxn-inventory :holistic-suffix t :numeric-suffix t))
         (cxn-name-apply-last
          (intern (upcase (format nil "~a-apply-last" cxn-name))))
         (cxn-name-apply-first
          (intern (upcase (format nil "~a-apply-first" cxn-name))))
         ;; find an identical existing holistic cxn
         (existing-routine-holistic-cxn
          (find-identical-holistic-cxn form meaning form-args meaning-args cxn-inventory))
         (existing-meta-holistic-cxn
          (when existing-routine-holistic-cxn
            (alter-ego-cxn existing-routine-holistic-cxn cxn-inventory)))
         ;; lex class
         (lex-class-holistic-cxn
          (if existing-routine-holistic-cxn
            (extract-lex-class-holistic-cxn existing-routine-holistic-cxn)
            (make-lex-class cxn-name :trim-cxn-suffix t :numeric-suffix t)))
         ;; temp cxn inventory
         (cxn-inventory-copy (copy-object cxn-inventory))
         ;; apply first cxn
         (holistic-cxn-apply-first
          (or existing-routine-holistic-cxn
              (holistic-cxn-apply-first-skeleton cxn-name cxn-name-apply-first lex-class-holistic-cxn
                                                 form meaning form-args meaning-args
                                                 (get-configuration cxn-inventory :initial-cxn-score)
                                                 nil cxn-inventory-copy)))
         ;; apply last cxn
         (holistic-cxn-apply-last
          (or existing-meta-holistic-cxn
              (holistic-cxn-apply-last-skeleton cxn-name cxn-name-apply-last lex-class-holistic-cxn
                                                form meaning form-args meaning-args
                                                (get-configuration cxn-inventory :initial-cxn-score)
                                                nil cxn-inventory-copy))))
    (list holistic-cxn-apply-first
          holistic-cxn-apply-last
          lex-class-holistic-cxn)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make n holistic cxns ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-n-holistic-cxns (form meaning form-args meaning-args form-arg-groups meaning-arg-groups cxn-inventory)
  ;; make as many holistic cxns as there are arg-groups
  ;; form and meaning should be entirely consumed by following variables from the arg-group
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make generalisation cxn ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-generalisation-cxn (form meaning top-lvl-form-args top-lvl-meaning-args slot-form-args slot-meaning-args cxn-inventory)
  (let* (;; cxn names
         (bare-cxn-name
          (make-cxn-name form cxn-inventory :item-based-suffix t :numeric-suffix t))
         (cxn-name-apply-last
          (intern (upcase (format nil "~a-apply-last" bare-cxn-name))))
         (cxn-name-apply-first
          (intern (upcase (format nil "~a-apply-first" bare-cxn-name))))
         ;; find an identical existing item-based cxn
         (existing-routine-item-based-cxn
          (find-identical-item-based-cxn form meaning top-lvl-form-args top-lvl-meaning-args
                                         slot-form-args slot-meaning-args cxn-inventory))
         (existing-meta-item-based-cxn
          (when existing-routine-item-based-cxn
            (alter-ego-cxn existing-routine-item-based-cxn cxn-inventory)))
         ;; lex classes
         (lex-class-item-based
          (if existing-routine-item-based-cxn
            (extract-lex-class-item-based-cxn existing-routine-item-based-cxn)
            (make-lex-class (symbol-name bare-cxn-name) :trim-cxn-suffix t :numeric-suffix t)))
         (lex-class-slot
          (if existing-routine-item-based-cxn
            (first (extract-lex-class-slot-item-based-cxn existing-routine-item-based-cxn))  ;; !!!
            (make-lex-class (symbol-name bare-cxn-name) :trim-cxn-suffix t :numeric-suffix t :slotp t)))
         ;; cxn inventory
         (cxn-inventory-copy (copy-object cxn-inventory))
         ;; build cxns!
         (item-based-cxn-apply-last
          (or existing-routine-item-based-cxn
              (item-based-cxn-apply-last-skeleton bare-cxn-name cxn-name-apply-last
                                                  lex-class-item-based lex-class-slot
                                                  form meaning
                                                  top-lvl-form-args top-lvl-meaning-args
                                                  slot-form-args slot-meaning-args
                                                  (get-configuration cxn-inventory :initial-cxn-score)
                                                  cxn-inventory-copy)))
         (item-based-cxn-apply-first
          (or existing-meta-item-based-cxn
              (item-based-cxn-apply-first-skeleton bare-cxn-name cxn-name-apply-first
                                                   lex-class-item-based lex-class-slot
                                                   form meaning
                                                   top-lvl-form-args top-lvl-meaning-args
                                                   slot-form-args slot-meaning-args
                                                   (get-configuration cxn-inventory :initial-cxn-score)
                                                   cxn-inventory-copy))))
    ;; done!
    (list item-based-cxn-apply-last
          item-based-cxn-apply-first
          lex-class-item-based
          (list lex-class-slot))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make generalisation cxn with n units ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-generalisation-cxn-with-n-units (form meaning top-lvl-form-args top-lvl-meaning-args slot-form-args slot-meaning-args form-arg-groups meaning-arg-groups cxn-inventory)
  (assert (length= form-arg-groups meaning-arg-groups))
  (let* (;; cxn names
         (bare-cxn-name
          (make-cxn-name form cxn-inventory :item-based-suffix t :numeric-suffix t))
         (cxn-name-apply-last
          (intern (upcase (format nil "~a-apply-last" bare-cxn-name))))
         (cxn-name-apply-first
          (intern (upcase (format nil "~a-apply-first" bare-cxn-name))))
         ;; find an identical existing item-based cxn
         (existing-routine-item-based-cxn
          (find-identical-item-based-cxn form meaning top-lvl-form-args top-lvl-meaning-args
                                         slot-form-args slot-meaning-args cxn-inventory))
         (existing-meta-item-based-cxn
          (when existing-routine-item-based-cxn
            (alter-ego-cxn existing-routine-item-based-cxn cxn-inventory)))
         ;; lex classes
         (lex-class-item-based
          (if existing-routine-item-based-cxn
            (extract-lex-class-item-based-cxn existing-routine-item-based-cxn)
            (make-lex-class (symbol-name bare-cxn-name) :trim-cxn-suffix t :numeric-suffix t)))
         (lex-classes-slot
          (if existing-routine-item-based-cxn
            (extract-lex-class-slot-item-based-cxn existing-routine-item-based-cxn)  ;; !!!
            (loop repeat (length form-arg-groups)
                  collect (make-lex-class (symbol-name bare-cxn-name)
                                          :trim-cxn-suffix t :numeric-suffix t :slotp t))))
         ;; cxn inventory
         (cxn-inventory-copy (copy-object cxn-inventory))
         ;; build cxns!
         (contributing-units-apply-last
          (contributing-units-apply-last-skeleton (length form-arg-groups)))
         (conditional-units-apply-last
          (conditional-units-apply-last-skeleton form-arg-groups meaning-arg-groups lex-classes-slot))
         (item-based-cxn-apply-last
          (or existing-routine-item-based-cxn
              (item-based-cxn-apply-last-from-units-skeleton bare-cxn-name cxn-name-apply-last
                                                             lex-class-item-based form meaning
                                                             top-lvl-form-args top-lvl-meaning-args
                                                             (get-configuration cxn-inventory :initial-cxn-score)
                                                             cxn-inventory-copy
                                                             contributing-units-apply-last conditional-units-apply-last
                                                             (mapcar #'first conditional-units-apply-last))))

         (contributing-units-apply-first
          (contributing-units-apply-first-skeleton form-arg-groups meaning-arg-groups lex-classes-slot))
         (item-based-cxn-apply-first
          (or existing-meta-item-based-cxn
              (item-based-cxn-apply-first-from-units-skeleton bare-cxn-name cxn-name-apply-first
                                                              lex-class-item-based form meaning
                                                              top-lvl-form-args top-lvl-meaning-args
                                                              (get-configuration cxn-inventory :initial-cxn-score)
                                                              cxn-inventory-copy
                                                              contributing-units-apply-first
                                                              (mapcar #'first contributing-units-apply-first)))))
    ;; done!
    (list item-based-cxn-apply-last
          item-based-cxn-apply-first
          lex-class-item-based
          lex-classes-slot)))

