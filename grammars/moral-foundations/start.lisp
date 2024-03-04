
(ql:quickload :moral-foundations)

(in-package :moral-foundations)

;; Below is some testing.
(defparameter *test-tweet* nil)

;; Naive table-lookup
(defun naive-lookup (sentence)
  (let* ((tokens (nlp-tools:get-penelope-lemmas sentence))
         (stems (mapcar #'stem tokens))
         (results nil))
    (loop for token in tokens
          for stem in stems
          do (let ((match (or (token-in-dictionary-p token)
                              (token-in-dictionary-p stem))))
               (if match
                 (push (list token match) results)))
          finally (return (reverse results)))))
;; (naive-lookup *test-tweet*)

(defun naive-lookups ()
  (let ((sentence-tokens (nlp-tools:get-penelope-sentence-tokens *test-tweet*)))
    (loop for sentence in sentence-tokens
          append (naive-lookup sentence))))
;; (naive-lookups)

(progn
  (setf *test-tweet* "The escalation of confrontations between Palestinian resistance fighters and the occupation forces in the Jenin camp threatens to explode the situation in the West Bank.")
  (naive-lookups))

(progn
  (setf *test-tweet* "I said this before, but look at this #RHOP thread and notice how most white people don't like Wendy. They can't stand educated, black women. It threatens their existence and they can't understand it. Interesting... #WWHL")
  (naive-lookups))

(progn
  (setf *test-tweet* "Are we now expected to respect the adulterous Camilla as the NEW QUEEN. It would appear so, yet he talks of morality and values. Whose values? His as a fellow adulterer? It's all baloney.")
  (naive-lookups))

(progn
  (setf *test-tweet* "How is it that we are now expected to respect liars?")
  (naive-lookups))

(progn
  (setf *test-tweet* "I'll take the slow steadfast steps of a wise man and woman leading us to a more perfect union over the authoritarian goose stepping the MAGA cult marches to any day.")
  (naive-lookups))

(progn
  (setf *test-tweet* "Cutting off food funding for a people under bombardment wouldn't have been justified even if the allegations were true. You arrest the individuals involved. You don't starve millions. I'm so angry I can't breathe.")
  (naive-lookups))

(progn
  (setf *test-tweet* "the fact that he probably won't suffer any consequences for saying this is actually terrifying")
  (naive-lookups))

(progn
  (setf *test-tweet* "One thing about me is that I will forever support Nicki with my all. She has made such an impact on my life. No one will ever take that support away. You can't steal or destroy real love and support.")
  (naive-lookups))

(progn
  (setf *test-tweet* "Look at this fucking banker pretending Stripe stands for anything. It actually disgusts me. This goblin freak thinks his company that serves no purpose except to skim 3% off every card transaction in the country stands for 'freedom of speech', and that he has dignity or values.")
  (naive-lookups))

(progn
  (setf *test-tweet* "Playing the victim and being a victim are two different things. The former hopes to make you a victim due to their antics and the latter is trying to heal from being unfairly hurt by someone else. Understanding this can help save many people from suicide and a life of torment.")
  (naive-lookups))
