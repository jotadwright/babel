(in-package :geoquery-lsfb)

(defmethod de-render ((predicates list) (mode (eql :set-of-predicates)) &key &allow-other-keys)
  "De-renders a set of predicates into a transient structure containing a collection of form predicates"
    (make-instance
     'coupled-feature-structure 
     :left-pole `((root (form ,predicates)))
     :right-pole `((root))))










