(in-package :slp)

(let ((categorial-network (categorial-network *geoquery-lsfb*)))
  (add-categories
   '(states-next_to-reference-cat
     ;mississippi-cat
     states-next_to-state-cat
     highest-points-of-reference-cat
     what-reference-cat
     highest-points-of-cat)
   categorial-network)
  ;(add-link 'states-next_to-reference-cat 'mississippi-cat categorial-network)
  (add-link 'states-next_to-state-cat 'highest-points-of-reference-cat categorial-network)
  (add-link 'what-reference-cat 'highest-points-of-cat categorial-network))

(loop with categorial-network = (categorial-network *geoquery-lsfb*)
      for state in *geoquery-states*
      for state-meaning = (cdr (assoc :name state))
      for state-category = (read-from-string (format nil "~a-cat" state-meaning))
      do (add-category state-category categorial-network)
         (add-link 'states-next_to-reference-cat state-category categorial-network))