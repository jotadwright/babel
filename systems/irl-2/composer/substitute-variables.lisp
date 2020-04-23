(in-package :irl-2)

;; #########################################
;; substitute-variables
;; -----------------------------------------

(export '(substitute-variables))

(defgeneric substitute-variables (thing substitutions)
  (:documentation "Substitutes variables in thing. Substitutions is an
                   association list. The result is a new structure
                   with unsubstituted symbols eq to those in thing"))

(defmethod substitute-variables ((thing t) (substitutions list))
  thing)

(defmethod substitute-variables ((symbol symbol) (substitutions list))
  (or (assqv symbol substitutions) symbol))

(defmethod substitute-variables ((cons cons) (substitutions list))
  (cons (substitute-variables (car cons) substitutions)
        (when (cdr cons) (substitute-variables (cdr cons) substitutions))))

(defmethod substitute-variables ((chunk chunk) (substitutions list))
  (make-instance 'chunk :primitive-inventory (primitive-inventory chunk)
                 :irl-program (substitute-variables (irl-program chunk) substitutions)
                 :target-var (substitute-variables (target-var chunk) substitutions)
                 :open-vars (substitute-variables (open-vars chunk) substitutions)
                 :score (score chunk)))