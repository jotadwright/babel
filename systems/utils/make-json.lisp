(in-package :utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                       ;;
;; JSON Serialisation of Babel Objects   ;;
;;                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export '(make-json render-json))

;; Make-json
;;;;;;;;;;;;;;;;;

(defgeneric make-json (thing)
  (:documentation "Return serialised lisp expression for thing, which can be rendered as a json string."))

(defmethod make-json ((thing symbol))
  "Return symbol."
  thing)

(defmethod make-json ((thing character))
  "Return character as a string."
  (string thing))

(defmethod make-json ((thing sequence))
  "Call make-json on each element in thing."
  (mapcar #'make-json thing))

(defmethod make-json ((thing cons))
  "Return cons." 
  (cons (car thing)
        (make-json (cdr thing))))

(defmethod make-json ((thing string))
  "Return string."
   thing)

(defmethod make-json ((thing number))
  "Return number."
  thing)

(defmethod make-json ((thing array))
  "Return array."
  (make-array (length thing) :initial-contents (map 'list #'make-json thing)))

(defmethod make-json ((thing function))
  "Return function as a string."
  (format nil "~a" thing))

(defmethod make-json ((thing t))
  "Stringify thing."
  (format nil "~a" thing))


;; Render-json
;;;;;;;;;;;;;;;;;

(defun render-json (serialised-lisp-expression)
  "Render a json string based on serialised-lisp-expression."
  (if (consp serialised-lisp-expression)
    (encode-json-alist-to-string serialised-lisp-expression)
    (cl-json:encode-json-to-string serialised-lisp-expression)))