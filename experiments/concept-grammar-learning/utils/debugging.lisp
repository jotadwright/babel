(in-package :clg)


(defun print-concepts-of-lexicals (form cxn-inventory agent)
  (add-element `((h3) ,(format nil "-------------- PRINTING CONCEPTS --------------")))
  (loop for cxn in (constructions-list cxn-inventory)
          for string = (attr-val cxn :string)
          for meaning = (attr-val cxn :meaning)
          for concept = (gethash meaning (get-data (ontology agent) 'concepts))
          for score = (attr-val cxn :score)
          when (string= string form)
          
           do (add-element `((h) ,(format nil "Concepts of with form and entrenchment score: ~a, ~a" form score)))
          and do (concept-representations::add-concept-to-interface (meaning concept) :weight-threshold 0.5)))



