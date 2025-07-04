(in-package :geoquery-lsfb-grammar)

(let ((categorial-network (categorial-network *geoquery-lsfb*)))
  (add-categories
   '(states-next-to-?x\(?x\)-cat
     states-next-to-?x-cat
     highest-points-of-?x\(?x\)-cat
     highest-points-of-?x-cat
     cities-in-?x\(?x\)-cat
     largest-of-?x\(?x\)-cat
     cities-in-?x-cat
     answer-?x\(?x\)-cat
     largest-of-?x-cat
     x-population-of-?y\(?y\)-cat
     x-population-of-?y\(?x\)-cat
     largest-cat
     x-population-of-?y-cat
     answer-?x\(?x\)-cat-2
     state-cat
     cityid-cat
     stateid-cat
     states-next-to-cat
     x-has-most-?y\(?x\)-cat
     x-has-most-?y\(?y\)-cat
     x-has-most-?y-cat
     highest-place-of-?x-cat
     highest-place-of-?x\(?x\)-cat
     state-with-capital-?x-cat
     state-with-capital-?x\(?x\)-cat
     rivers-traversing-?x\(?x\)-cat
     rivers-traversing-?x-cat
     rivers-in-?x-cat
     rivers-in-?x\(?x\)-cat
     usa-country-cat
     shortest-?x\(?x\)-cat
     shortest-?x-cat
     answer-?x\(?x\)-cat-3
     answer-?x\(?x\)-cat-4
     major-?x-cat
     major-?x\(?x\)-cat
     longest-?x\(?x\)-cat
     longest-?x-cat
     has-largest-population-of-?x\(?x\)-cat
     has-largest-population-of-?x-cat
     summed-population-of-?x\(?x\)-cat
     summed-population-of-?x-cat
     answer-?x\(?x\)-cat-5
     largest-city-in-?x-cat
     largest-city-in-?x\(?x\)-cat
     population-of-?x-cat
     population-of-?x\(?x\)-cat
     smallest-of-?x\(?x\)-cat
     smallest-of-?x-cat)
   categorial-network)
  (add-link 'smallest-of-?x-cat 'answer-?x\(?x\)-cat categorial-network)
  (add-link 'smallest-of-?x\(?x\)-cat 'cities-in-?x-cat categorial-network)
  (add-link 'largest-city-in-?x-cat 'answer-?x\(?x\)-cat categorial-network)
  (add-link 'population-of-?x-cat 'answer-?x\(?x\)-cat-2 categorial-network)
  (add-link 'population-of-?x\(?x\)-cat 'major-?x-cat categorial-network)
  (add-link 'states-next-to-?x-cat 'highest-points-of-?x\(?x\)-cat categorial-network)
  (add-link 'states-next-to-?x-cat 'largest-city-in-?x\(?x\)-cat categorial-network)
  (add-link 'answer-?x\(?x\)-cat 'highest-points-of-?x-cat categorial-network)
  (add-link 'answer-?x\(?x\)-cat 'states-next-to-?x-cat categorial-network)
  (add-link 'answer-?x\(?x\)-cat 'has-largest-population-of-?x-cat categorial-network)
  (add-link 'answer-?x\(?x\)-cat-2 'summed-population-of-?x-cat categorial-network)
  (add-link 'states-next-to-?x-cat 'cities-in-?x\(?x\)-cat categorial-network)
  (add-link 'cities-in-?x-cat 'largest-of-?x\(?x\)-cat categorial-network)
  (add-link 'answer-?x\(?x\)-cat 'largest-of-?x-cat categorial-network)
  (add-link 'has-largest-population-of-?x\(?x\)-cat 'states-next-to-?x-cat categorial-network)
  (add-link 'summed-population-of-?x\(?x\)-cat 'states-next-to-?x-cat categorial-network)
  (add-link 'x-has-most-?y\(?x\)-cat 'state-cat categorial-network)
  (add-link 'states-next-to-cat 'x-has-most-?y\(?y\)-cat categorial-network)
  (add-link 'answer-?x\(?x\)-cat 'x-has-most-?y-cat categorial-network)
  (add-link 'highest-place-of-?x\(?x\)-cat 'states-next-to-?x-cat categorial-network)
  (add-link 'highest-place-of-?x-cat 'answer-?x\(?x\)-cat categorial-network)
  (add-link 'state-with-capital-?x-cat 'states-next-to-?x\(?x\)-cat categorial-network)
  (add-link 'states-next-to-?x\(?x\)-cat 'stateid-cat categorial-network)
  (add-link 'state-with-capital-?x\(?x\)-cat 'cityid-cat categorial-network)
  (add-link 'states-next-to-?x-cat 'rivers-traversing-?x\(?x\)-cat categorial-network)
  (add-link 'rivers-traversing-?x-cat 'answer-?x\(?x\)-cat categorial-network)
  (add-link 'stateid-cat 'rivers-in-?x\(?x\)-cat categorial-network)
  (add-link 'stateid-cat 'cities-in-?x\(?x\)-cat categorial-network)
  (add-link 'rivers-in-?x-cat 'answer-?x\(?x\)-cat categorial-network)
  (add-link 'rivers-in-?x\(?x\)-cat 'usa-country-cat categorial-network)
  (add-link 'rivers-in-?x-cat 'shortest-?x\(?x\)-cat categorial-network)
  (add-link 'answer-?x\(?x\)-cat-3 'shortest-?x-cat categorial-network)
  (add-link 'answer-?x\(?x\)-cat-4 'rivers-in-?x-cat categorial-network)
  (add-link 'major-?x\(?x\)-cat 'rivers-in-?x-cat categorial-network)
  (add-link 'major-?x\(?x\)-cat 'cities-in-?x-cat categorial-network)
  (add-link 'major-?x-cat 'answer-?x\(?x\)-cat categorial-network)
  (add-link 'largest-of-?x\(?x\)-cat 'rivers-in-?x-cat categorial-network)
  (add-link 'largest-of-?x-cat 'answer-?x\(?x\)-cat categorial-network)
  (add-link 'longest-?x-cat 'answer-?x\(?x\)-cat categorial-network)
  (add-link 'longest-?x\(?x\)-cat 'rivers-in-?x-cat categorial-network)
  (add-link 'shortest-?x-cat 'answer-?x\(?x\)-cat categorial-network)
  (add-link 'cities-in-?x-cat 'answer-?x\(?x\)-cat categorial-network)
  (add-link 'answer-?x\(?x\)-cat-5 'largest-of-?x-cat categorial-network)
  (add-link 'answer-?x\(?x\)-cat-4 'cities-in-?x-cat categorial-network)
  (add-link 'usa-country-cat 'cities-in-?x\(?x\)-cat categorial-network)
  (add-link 'major-?x-cat 'answer-?x\(?x\)-cat-4 categorial-network)
  (add-link 'cities-in-?x-cat 'has-largest-population-of-?x\(?x\)-cat categorial-network))
