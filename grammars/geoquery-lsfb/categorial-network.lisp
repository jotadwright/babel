(in-package :geoquery-lsfb-grammar-copy)

(let ((categorial-network (categorial-network *geoquery-lsfb*)))
  (add-categories
   '(state-or-country 
     state-or-city
     city-or-capital
     capital
     city
     country
     state
     /
     
     
     /
     
     
     
     /
     
     
     /
     
     /
     
     /
     
     
     /
     /
     
     
     /
     
     
     /
     
     /
     /
     
     
     /
     )
   categorial-network)
  (add-link 'state-or-country 'country categorial-network)
  (add-link 'state-or-city 'state categorial-network)
  (add-link 'state-or-city 'city categorial-network)
  (add-link 'state-or-country 'state categorial-network)
  (add-link 'city-or-capital 'city categorial-network)
  (add-link 'city-or-capital 'capital categorial-network)
  (add-link '/ ' categorial-network)
  (add-link '/ ' categorial-network)
  (add-link '/ ' categorial-network)
  (add-link '/ ' categorial-network)
  (add-link '/ ' categorial-network)
  (add-link '/ ' categorial-network)
  (add-link '/ ' categorial-network)
  (add-link '/ ' categorial-network)
  (add-link '/ ' categorial-network)
  (add-link '/ ' categorial-network)
  (add-link '/ ' categorial-network)
  (add-link '/ ' categorial-network)
  (add-link '/ ' categorial-network)
  (add-link '/ ' categorial-network)
  (add-link '/ ' categorial-network)
  (add-link '/ ' categorial-network)
  (add-link '/ ' categorial-network)
  (add-link '/ ' categorial-network)
  (add-link '/ ' categorial-network)
  (add-link '/ ' categorial-network)
  (add-link '/ ' categorial-network)
  (add-link '/ ' categorial-network)
  (add-link '/ ' categorial-network)
  (add-link '/ ' categorial-network)
  (add-link '/ ' categorial-network)
  (add-link '/ ' categorial-network))
