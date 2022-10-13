;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: DISTRIBUTIONS.INTERNALS -*-
;;; Copyright (c) 2019-2020 Symbolics Pte. Ltd. All rights reserved.
(cl:in-package #:distributions.internals)

;;; internal representation of floats

(deftype internal-float (&optional lower-limit upper-limit)
  "Type used for internal representation of floats in the DISTRIBUTIONS library."
  `(double-float ,(if (eq lower-limit '*)
                      lower-limit
                      (float lower-limit 1d0))
                 ,(if (eq upper-limit '*)
                      upper-limit
                      (float upper-limit 1d0))))

(deftype float-vector (&optional n)
  `(simple-array internal-float (,n)))

(declaim (inline as-float))
(defun as-float (x)
  "Return the argument coerced to the DISTRIBUTIONS library's internal float type."
  (coerce x 'internal-float))

(defmacro with-floats ((&rest variables) &body body)
  "Rebind each variable, coerced to the internal float type used by DISTRIBUTIONS."
  `(let ,(mapcar (lambda (variable)
                   `(,variable (as-float ,variable)))
          variables)
     ,@body))

(declaim (inline as-float-vector))
(defun as-float-vector (vector &key copy?)
  "Return VECTOR converted to another vector with elements converted to INTERNAL-FLOAT if necessary.  When COPY?, the vector is always copied."
  (if (or copy? (not (typep vector 'float-vector)))
      (map 'float-vector #'as-float vector)
      vector))

(defun as-float-probabilities (vector)
  "Normalize vector as probabilities, assert that all are positive, return them as a VECTOR-DOUBLE-FLOAT.  Vector is always copied."
  (let ((sum (nu:sum vector)))
    (map 'float-vector
         (lambda (x)
           (assert (<= 0 x) (x) "Element is not positive.")
           (as-float (/ x sum)))
         vector)))

;;;; Miscellaneous macros

(defmacro try ((&rest bindings) condition value)
  "Evaluate bindings (expanding into LET+, so all features can be used) until condition is satisfied, then return value."
  (with-unique-names (top)
    `(prog ()
        ,top
        (let+ ,bindings
          (if ,condition
              (return ,value)
              (go ,top))))))

(defmacro maybe-ignore-constant (ignore-constant? value constant)
  "Handle a constant that is calculated only when IGNORE-CONSTANT? is NIL and VALUE is not negative infinity (represented by NIL)."
  (once-only (value)
    `(when ,value
       (if ,ignore-constant?
           ,value
           (+ ,value ,constant)))))
