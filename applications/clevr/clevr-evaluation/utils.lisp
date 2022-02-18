(in-package :clevr-evaluation)

(defun answer->str (answer-value)
  (case #+lispworks (type-of answer-value)
        #+ccl (if (listp (type-of answer-value))
                  (first (type-of answer-value))
                  (type-of answer-value))
        #+sbcl (if (listp (type-of answer-value))
                  (first (type-of answer-value))
                  (type-of answer-value))
    (number (mkstr answer-value))
    (fixnum (mkstr answer-value))
    (integer (mkstr answer-value))
    (bit (mkstr answer-value))
    (shape-category (mkstr (shape answer-value)))
    (size-category (mkstr (clevr-world::size answer-value)))
    (color-category (mkstr (color answer-value)))
    (material-category (mkstr (material answer-value)))
    (boolean-category (mkstr (id answer-value)))))

(defun compute-answer (irl-program scene-var scene-path-entity)
  "Given an irl-program, a variable and a scene path,
   compute the answer."
  (let ((solutions
         (evaluate-irl-program
          (cons `(bind pathname-entity ,scene-var ,scene-path-entity) irl-program)
          *clevr-ontology* :primitive-inventory *clevr-primitives*)))
    (when (and solutions (length= solutions 1))
      (let* ((target-var (get-target-var irl-program))
             (target-value (value (find target-var (first solutions) :key #'var))))
        (answer->str target-value)))))


(defun extract-scene-unit-variable (cipn)
  "returns scene-variable from resulting cfs of given cipn"
  (let* ((cfs (pole-structure (left-pole (car-resulting-cfs (cipn-car cipn)))))
         (scene-unit (find 'fcg::scene-unit cfs :key #'first))
         (scene-var (second (find 'fcg::scene (rest scene-unit) :key #'first))))
    scene-var))