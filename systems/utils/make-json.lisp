(in-package :utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                       ;;
;; JSON Serialisation of Babel Objects   ;;
;;                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export '(make-json render-json))

;; Make-json
;;;;;;;;;;;;;;;;;

(defgeneric make-json (thing &optional objects-processed)
  (:documentation "Return serialised lisp expression for thing, which can be rendered as a json string."))

(defmethod make-json ((thing symbol) &optional objects-processed)
  "Return symbol."
  (values thing objects-processed))

(defmethod make-json ((thing character) &optional objects-processed)
  "Return character as a string."
  (values (string thing) objects-processed))

(defmethod make-json ((thing cons) &optional objects-processed)
  "Return cons." 
  (multiple-value-bind (car-json car-objects-processed)
      (make-json (car thing) objects-processed)
    
    (multiple-value-bind (cdr-json cdr-objects-processed)
        (make-json (cdr thing) car-objects-processed)

      (values (cons car-json cdr-json) cdr-objects-processed))))

(defmethod make-json ((thing string) &optional objects-processed)
  "Return string."
   (values thing objects-processed))

(defmethod make-json ((thing number) &optional objects-processed)
  "Return number."
  (values thing objects-processed))

(defmethod make-json ((thing array) &optional objects-processed)
  "Return array."
  (make-array (length thing) :initial-contents (map 'list #'make-json thing)))

(defmethod make-json ((thing function) &optional objects-processed)
  "Return function as a string."
  (values (format nil "~a" thing) objects-processed))

(defmethod make-json ((thing t) &optional objects-processed)
  "Stringify thing."
  (values (format nil "~a" thing) objects-processed))

;;(defparameter *test* (closer-mop:class-slots (find-class 'fcg::construction-inventory-processor)))

(defun make-json-clos-slot (slot thing objects-processed)
  (let ((objects-processed-extended (cons thing objects-processed)))
    (values (cons (make-json (make-kw (clos:slot-definition-name slot)) objects-processed-extended)
                  (make-json (slot-value thing (clos:slot-definition-name slot)) objects-processed-extended))
            objects-processed-extended)))

(defmethod make-json ((thing standard-object) &optional objects-processed)
  ""
  (if (find thing objects-processed)
    (values "***" objects-processed)
    (loop with slot-objects-processed = objects-processed
          for slot in (closer-mop:class-slots (find-class (type-of thing)))
          collect (multiple-value-bind (jsonified-slot objects-processed-extended)
                      (make-json-clos-slot slot thing slot-objects-processed)
                    (setf slot-objects-processed objects-processed-extended)
                    jsonified-slot)
            into jsonified-slots
          finally (return (values (append (list (cons :class (type-of thing))) jsonified-slots)
                                  slot-objects-processed)))))


;;(pprint (make-json (make-instance 'fcg::fcg-construction :cxn-inventory fcg::*fcg-constructions* )))
                                

;; Render-json
;;;;;;;;;;;;;;;;;

(defun render-json (serialised-lisp-expression)
  "Render a json string based on serialised-lisp-expression."
  (if (consp serialised-lisp-expression)
    (encode-json-alist-to-string serialised-lisp-expression)
    (cl-json:encode-json-to-string serialised-lisp-expression)))