(ql:quickload :geoquery-lsfb-grammar)
(in-package :slp)

(defparameter *geoquery-lsfb-data*
  (merge-pathnames
   (make-pathname :directory '(:relative "GeoQuery-LSFB"))
   *babel-corpora*))

(activate-monitor trace-slp)

(defparameter *train-set*
  (load-geoquery-corpus-jsonl "/Users/liesbetdevos/Projects/Corpora/GeoQuery-LSFB/train-test/train.jsonl"))

(defparameter *test-set*
  (load-geoquery-corpus-jsonl "/Users/liesbetdevos/Projects/Corpora/GeoQuery-LSFB/train-test/test.jsonl"))

(comprehend-list-of-ids '(1 386 485 597 637 754 801 809 810 852))

           
(comprehend
 (get-example-form 1 *train-set*)
 :cxn-inventory *geoquery-lsfb*)

(formulate '((ANSWER ?D ?A ?E) (HIGH_POINT ?E ?B ?A) (STATE ?E ?B) (NEXT_TO ?E ?B ?C) (CONST ?E ?C ?F) (STATEID ?F ?G) (MISSISSIPPI ?G))
           :cxn-inventory *geoquery-lsfb*)

(test-coverage *train-set* *geoquery-lsfb*)
    

(pprint (get-example-meaning 1 *train-set*))
((ANSWER ?D ?A ?E) (HIGH_POINT ?E ?B ?A) (STATE ?E ?B) (NEXT_TO ?E ?B ?C) (CONST ?E ?C ?F) (STATEID ?F ?G) (MISSISSIPPI ?G))

#|
(ql:quickload :au-lib)
(au-lib::print-anti-unification-results  
 (au-lib::anti-unify-predicate-networks  
  ;; what color is the x cube?
  '((ANSWER ?D ?A ?E) (HIGH_POINT ?E ?B ?A) (STATE ?E ?B) (NEXT_TO ?E ?B ?C) (CONST ?E ?C ?F) (STATEID ?F ?G) (MISSISSIPPI ?G))
  ;; what color is the small blue cube
  '((ANSWER ?D ?A ?E) (ELEVATION ?E ?B ?A) (HIGHEST ?E ?B ?F) (PLACE ?F ?B) (LOC ?F ?B ?C) (CONST ?F ?C ?G) (STATEID ?G ?H) (MONTANA ?H))
  :baseline))

(au-lib::print-anti-unification-results  
 (au-lib::anti-unify-predicate-networks  
  ;; what color is the x cube?
  '((TWO-HAND-ARTICULATION ?PALM-UP-1 "")
    (TWO-HAND-ARTICULATION ?DANS-1 "")
    (RIGHT-HAND-ARTICULATION ?FS\:MISSISSIPPI-1 "")
    (RIGHT-HAND-ARTICULATION ?IL-Y-A-1 "")
    (RIGHT-HAND-ARTICULATION ?DS[BENT5]\:ETAT+-1 "")
    (RIGHT-HAND-ARTICULATION ?PT\:LOC[1]+-1 "")
    (RIGHT-HAND-ARTICULATION ?HAUT-1 "")
    (RIGHT-HAND-ARTICULATION ?QUOI-1 "")
    (LEFT-HAND-ARTICULATION ?DS[BENT5]\:ETAT-1 "")
    (ADJACENT ?PALM-UP-1 ?DANS-1)
    (ADJACENT ?DANS-1 ?FS\:MISSISSIPPI-1)
    (ADJACENT ?FS\:MISSISSIPPI-1 ?IL-Y-A-1)
    (ADJACENT ?IL-Y-A-1 ?DS[BENT5]\:ETAT+-1)
    (ADJACENT ?DS[BENT5]\:ETAT+-1 ?PT\:LOC[1]+-1)
    (ADJACENT ?PT\:LOC[1]+-1 ?HAUT-1)
    (ADJACENT ?HAUT-1 ?QUOI-1)
    (START-COINCIDES ?DS[BENT5]\:ETAT+-1 ?DS[BENT5]\:ETAT-1)
    (DURING ?PT\:LOC[1]+-1 ?DS[BENT5]\:ETAT-1)
    (END-COINCIDES ?HAUT-1 ?DS[BENT5]\:ETAT-1)
    (ADJACENT ?IL-Y-A-1 ?DS[BENT5]\:ETAT-1)
    (ADJACENT ?DS[BENT5]\:ETAT-1 ?QUOI-1))
  '((TWO-HAND-ARTICULATION ?DANS-2 "")
    (RIGHT-HAND-ARTICULATION ?PT\:DET/LOC[1]-1 "")
    (RIGHT-HAND-ARTICULATION ?FS\:ALABAMA-1 "")
    (RIGHT-HAND-ARTICULATION ?PT\:DET/LOC[5]+-1 "")
    (RIGHT-HAND-ARTICULATION ?PT\:DET/LOC[1]-2 "")
    (RIGHT-HAND-ARTICULATION ?COMBIEN-1 "")
    (TWO-HAND-ARTICULATION ?HABITER-2 "")
    (RIGHT-HAND-ARTICULATION ?PT\:DET/LOC[5]+-2 "")
    (LEFT-HAND-ARTICULATION ?DS[7]\:CARTE-1 "")
    (LEFT-HAND-ARTICULATION ?DS[7]\:CARTE-2 "")
    (ADJACENT ?DANS-2 ?PT\:DET/LOC[1]-1)
    (ADJACENT ?PT\:DET/LOC[1]-1 ?FS\:ALABAMA-1)
    (ADJACENT ?FS\:ALABAMA-1 ?PT\:DET/LOC[5]+-1)
    (ADJACENT ?PT\:DET/LOC[5]+-1 ?PT\:DET/LOC[1]-2)
    (ADJACENT ?PT\:DET/LOC[1]-2 ?COMBIEN-1)
    (ADJACENT ?COMBIEN-1 ?HABITER-2)
    (ADJACENT ?HABITER-2 ?PT\:DET/LOC[5]+-2)
    (START-COINCIDES ?PT\:DET/LOC[5]+-1 ?DS[7]\:CARTE-1)
    (END-COINCIDES ?PT\:DET/LOC[1]-2 ?DS[7]\:CARTE-1)
    (ADJACENT ?FS\:ALABAMA-1 ?DS[7]\:CARTE-1)
    (ADJACENT ?DS[7]\:CARTE-1 ?COMBIEN-1)
    (START-COINCIDES ?PT\:DET/LOC[5]+-2 ?DS[7]\:CARTE-2)
    (END-COINCIDES ?PT\:DET/LOC[5]+-2 ?DS[7]\:CARTE-2)
    (ADJACENT ?HABITER-2 ?DS[7]\:CARTE-2))
  ;; what color is the small blue cube
  :baseline))

|#