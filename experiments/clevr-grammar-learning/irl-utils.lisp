;;;; irl-utils.lisp

(in-package :cgl)

(defun replace-anywhere-if (new-fn predicate-fn tree)
  "Replace all items in 'tree' that return t
   by 'predicate-fn' with the result from 'new-fn'
   applied to the old element."
  (cond ((atom tree)
         ;; if the predicate returns t on some element,
         ;; apply new-fn to the old element
         ;; and use this as the new element
         (if (funcall predicate-fn tree)
           (funcall new-fn tree) tree))
        (t
         (cons (replace-anywhere-if new-fn predicate-fn (car tree))
               (replace-anywhere-if new-fn predicate-fn (cdr tree))))))

(defun deduplicate-variables (irl-program)
  "Find if there are any duplicate variables.
   If there are, deduplicate them."
  (let* ((all-vars (all-variables irl-program))
         (unique-vars (remove-duplicates all-vars))
         (duplicates
          (loop for var in unique-vars
                when (> (count var all-vars) 1)
                collect var)))
    (if duplicates
      (replace-anywhere-if
       #'(lambda (v) (make-var (get-base-name v)))
       #'(lambda (v) (member v duplicates))
       irl-program)
      irl-program)))