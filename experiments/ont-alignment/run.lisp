(ql:quickload :ont-alignment)
(in-package :ont-alignment)

(defparameter *experiment*
  (make-instance 'ont-alignment-experiment))

(run-interaction *experiment*)

;(execute-postmodern-query '(:select (:count actor) (:avg year) :from actorsfilms))
;(execute-postmodern-query '(:select film :from actorsfilms :where (:in actor (:set "Gérard Depardieu" "Fred Astaire"))))
;(execute-postmodern-query '(:select film :from actorsfilms :where (:not (:in actor (:set "Gérard Depardieu" "Fred Astaire")))))
;(execute-postmodern-query '(:select actor (:count film) :from actorsfilms :group-by actor :having (:> (:count film) 100)))
;(execute-postmodern-query '(:select film :from actorsfilms :where (:and (:= actor "Gerard Depardieu") (:<= rating 5))))
;(execute-postmodern-query '(:select actor (:count film) :from actorsfilms :group-by actor :having (:between (:count film) 100 101)))

;; '(:select actor film :from actorsfilms)
'((comma ?columns ?first-column ?second-column)
  (bind column ?first-column actor)
  (bind column ?second-column film)
  (select ?result ?columns ?table ?where-clause)
  (bind table ?table actorsfilms))

;; '(:select (:count actor) :from actorsfilms)
'((aggregate ?aggregate-clause ?aggregator ?column)
  (bind aggregator ?aggregator count)
  (bind column ?column actor)
  (select ?result ?aggregate-clause ?table ?where-clause)
  (bind table ?table actorsfilms))

;; '(:select film :from actorsfilms :where (:= actor "Gerard Depardieu"))
'((bind column ?column film)
  (select ?result ?column ?table ?where-clause)
  (bind table ?table actorsfilms)
  (where ?where-clause ?filter-condition)
  (equals ?filter-condition ?column-2 ?comparator)
  (bind column ?column-2 actor)
  (bind concept ?comparator "Gerard Depardieu"))

;; '(:select film :from actorsfilms :where (:in actor (:set "Gérard Depardieu" "Fred Astaire")))
'((bind column ?column film)
  (select ?result ?column ?table ?where-clause)
  (bind table ?table actorsfilms)
  (where ?where-clause ?filter-condition)
  (in ?filter-condition ?column-2 ?set-condition)
  (bind column ?column-2 actor)
  (set ?set-condition ?set-values)
  (bind set-values ?set-values (set "Gérard Depardieu" "Fred Astaire")))

;; '(:select film :from actorsfilms :where (:not (:in actor (:set "Gérard Depardieu" "Fred Astaire")))))
'((bind column ?column film)
  (select ?result ?column ?table ?where-clause)
  (bind table ?table actorsfilms)
  (where ?where-clause ?filter-condition)
  (not-in ?filter-condition ?column-2 ?set-condition)
  (bind column ?column-2 actor)
  (set ?set-condition ?set-values)
  (bind set-values ?set-values ("Gérard Depardieu" "Fred Astaire")))


;; '(:select actor (:count film) :from actorsfilms :group-by actor :having (:> (:count film) 100))
'((aggregate ?aggregate-clause ?aggregator ?column)
  (bind aggregator ?aggregator count)
  (bind column ?column actor)
  (select ?result ?aggregate-clause ?table ?where-clause ?group-by-clause)
  (bind table ?table actorsfilms)
  (group-by ?group-by-clause ?column-2 ?having-clause)
  (bing column ?column-2 actor)
  (having ?having-clause ?filter-condition)
  (superior ?filter-condition ?aggregate-clause-2 ?comparator)
  (aggregate ?aggregate-clause-2 ?aggregator-2 ?column-3)
  (bind aggregator ?aggregator-2)
  (bind concept ?comparator 100)
  (bind aggregator ?aggregator-2 count))


;; '(:select actor (:count film) :from actorsfilms :group-by actor :having (:between (:count film) 100 101))
'((aggregate ?aggregate-clause ?aggregator ?column)
  (bind aggregator ?aggregator count)
  (bind column ?column actor)
  (select ?result ?aggregate-clause ?table ?where-clause ?group-by-clause)
  (bind table ?table actorsfilms)
  (group-by ?group-by-clause ?column-2 ?having-clause)
  (bing column ?column-2 actor)
  (having ?having-clause ?filter-condition)
  (between ?filter-condition ?aggregate-clause-2 ?comparator-1 ?comparator-2)
  (aggregate ?aggregate-clause-2 ?aggregator-2 ?column-3)
  (bind aggregator ?aggregator-2)
  (bind concept ?comparator-1 100)
  (bind concept ?comparator-2 101)
  (bind aggregator ?aggregator-2 count))

;; '(:select film :from actorsfilms :where (:and (:= actor "Gerard Depardieu") (:<= rating 5)))
'((bind column ?column film)
  (select ?result ?column ?table ?where-clause)
  (bind table ?table actorsfilms)
  (where ?where-clause ?filter-condition)
  (and-keyword ?filter-condition ?condition-1 ?condition-2)
  (equals ?condition-1 ?column-2 ?comparator)
  (inferior ?condition-2 ?column-3 ?comparator-2)
  (bind column ?column-2 actor)
  (bind column ?column-3 rating)
  (bind concept ?comparator "Gerard Depardieu")
  (bind concept ?comparator-2 5))


;(postmodern::disconnect-toplevel)
;(postmodern::connect-toplevel "db3_actors_films_multiple_tables.db" "postgres" "postgres" "localhost")

;; (execute-postmodern-query '(:select (:count film) :from films :inner-join actorfilm_relations :on (:= films.film_id actorfilm_relations.film_id) :inner-join actors :on (:= actorfilm_relations.actor_id actors.actor_id) :where (:= actors.actor "Gerard Depardieu")))
'((

;; (execute-postmodern-query '(:select film :from films :inner-join actorfilm_relations :on (:= films.film_id actorfilm_relations.film_id) :inner-join actors :on (:= actorfilm_relations.actor_id actors.actor_id) :where (:and (:= actors.actor "Gerard Depardieu") (:= films.year 1996))))


;; (execute-postmodern-query '(:select film :from films :inner-join actorfilm_relations :on (:= films.film_id actorfilm_relations.film_id) :inner-join actors :on (:= actorfilm_relations.actor_id actors.actor_id) :where (:and (:like actors.actor "Jean%") (:> films.year 2020))))


;; (execute-postmodern-query '(:select film :from films :inner-join actorfilm_relations :on (:= films.film_id actorfilm_relations.film_id) :inner-join actors :on (:= actorfilm_relations.actor_id actors.actor_id) :where (:in actors.actor (:set "Gerard Depardieu" "Fred Astaire")) :group-by films.film :having (:> (:avg films.rating) 7.5)))


;; (execute-postmodern-query '(:select film :from films :inner-join actorfilm_relations :on (:= films.film_id actorfilm_relations.film_id) :inner-join actors :on (:= actorfilm_relations.actor_id actors.actor_id) :where (:or (:in actors.actor (:set "Gerard Depardieu" "Fred Astaire")) (:not (:like actors.actor "Brigitte"))) :group-by films.film :having (:> (:avg films.rating) 9)))





 