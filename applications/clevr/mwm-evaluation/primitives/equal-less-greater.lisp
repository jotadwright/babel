;;;; equal-less-greater.lisp

(in-package :mwm-evaluation)

;(export '(equal-integer less-than greater-than))

;; -------------------------
;; EQUAL-INTEGER primtive ;;
;; -------------------------
;; Check if two two integers are the same

;; For the moment, this is implemented without using the integer
;; categories in the ontology. Let's see if it works like this.
(defprimitive equal-integer ((target-bool boolean-category)
                             (source-num-1 number)
                             (source-num-2 number))
  ((source-num-1 source-num-2 => target-bool)
   (let ((boolean-category
          (find-entity-by-id
           ontology
           (if (= source-num-1 source-num-2)
             'yes 'no))))
     (bind (target-bool 1.0 boolean-category))))
  
  ((source-num-1 source-num-2 target-bool =>)
   (let ((boolean-category
          (find-entity-by-id
           ontology
           (if (= source-num-1 source-num-2)
             'yes 'no))))
     (equal-entity target-bool boolean-category)))
  :primitive-inventory *mwm-primitives*)

;; ---------------------
;; LESS-THAN primtive ;;
;; ---------------------
;; Check if integer-1 is less than integer-2

;; For the moment, this is implemented without using the integer
;; categories in the ontology. Let's see if it works like this.
(defprimitive less-than ((target-bool boolean-category)
                         (source-num-1 number)
                         (source-num-2 number))
  ((source-num-1 source-num-2 => target-bool)
   (let ((boolean-category
          (find-entity-by-id
           ontology
           (if (< source-num-1 source-num-2)
             'yes 'no))))
     (bind (target-bool 1.0 boolean-category))))

  ((source-num-1 source-num-2 target-bool =>)
   (let ((boolean-category
          (find-entity-by-id
           ontology
           (if (< source-num-1 source-num-2)
             'yes 'no))))
     (equal-entity target-bool boolean-category)))
  :primitive-inventory *mwm-primitives*)

;; ------------------------
;; GREATER-THAN primtive ;;
;; ------------------------
;; Check if integer-1 is greater than integer-2

;; For the moment, this is implemented without using the integer
;; categories in the ontology. Let's see if it works like this.
(defprimitive greater-than ((target-bool boolean-category)
                            (source-num-1 number)
                            (source-num-2 number))
  ((source-num-1 source-num-2 => target-bool)
   (let ((boolean-category
          (find-entity-by-id
           ontology
           (if (> source-num-1 source-num-2)
             'yes 'no))))
     (bind (target-bool 1.0 boolean-category))))

  ((source-num-1 source-num-2 target-bool =>)
   (let ((boolean-category
          (find-entity-by-id
           ontology
           (if (> source-num-1 source-num-2)
             'yes 'no))))
     (equal-entity target-bool boolean-category)))
  :primitive-inventory *mwm-primitives*)