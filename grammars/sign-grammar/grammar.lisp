(in-package :sign-grammar)

;;------------------------;;
;; Grammar configurations ;;
;;------------------------;;

(def-fcg-constructions sign-grammar
  :feature-types ((subunits set)
                  (args set)
                  (footprints set)
                  (form-args set-of-predicates)
                  (form set-of-predicates)
                  (meaning-args set-of-predicates)
                  (meaning set-of-predicates))
  :hierarchy-features (subunits)
  :diagnostics ()
  :repairs ()
  :fcg-configurations ((:parse-goal-tests :no-applicable-cxns)
                       (:production-goal-tests :no-applicable-cxns)
                       (:node-tests :check-duplicate :restrict-nr-of-nodes :restrict-search-depth)
                       (:max-number-of-nodes . 200)
                       (:max-search-depth . 50)
                       (:render-mode . :set-of-predicates)
                       (:de-render-mode . :set-of-predicates)
                       (:shuffle-cxns-before-application . t)
                       (:draw-meaning-as-network . t)
                       (:construction-inventory-processor-mode . :heuristic-search)
                       (:node-expansion-mode . :full-expansion)
                       (:search-algorithm . :best-first)
                       (:heuristic-value-mode . :sum-heuristics-and-parent)
                       (:cxn-supplier-mode . :all-cxns)
                       (:heuristics :nr-of-applied-cxns))
  
  :visualization-configurations ((:with-search-debug-data . t)
                                 (:remove-empty-units . nil)
                                 (:show-constructional-dependencies . t)
                                 (:labeled-paths . nil)
                                 (:colored-paths . nil)
                                 (:hide-features . nil)
                                 (:hierarchy-features subunits)
                                 (:selected-hierarchy . subunits)
                                 (:select-subfeatures . nil))
  
  )

;------------------------------------
; structure of learned constructions
;------------------------------------
; holophrase: one unit with as name the name of the cxn. The entire form and meaning of the utterance are coupled in the conditional part of this utterance. For the moment contributing part is empty?

; item-based-cxn: unit-name for the complete structure contains the same name as the cxn. The slot-units contain the name slot1, slot2. the order of the slots is chosen by looking which one comes first in the form for readability. Larger unit pairs complete meaning and form of the item-based part in the conditional part. Units for the slots contain meaning-args (partial-network and target), boundaries (left and right boundary for both hands), the category for the slot of the construction, and a location.

;holistic-cxn: cxn and unit name refer to the meaning of the holistic part. Meaning and form of the holistic part are paired in the conditional part of the construction. In the contributing part, meaning args are added (partial-network and target) that refer to variables in the meaning of the holistic cxn. A category for the holistic-cxn is also added in the contributing part, and the overall location of the holistic cxn. Boundaries for the left and right hand are added by looking at which sign comes first and last for each of the hands (using the temporal alignment and meets constraints).



;---------------------------
; types of existing repairs:
;---------------------------
; 1) add-categorial-links: when there is no form left in the root, but the meaning network is not connected. This means that cxn exist to cover the observed utterance, but that the holistic-cxns were not yet observed in combination with a given item-based cxn. So we add a new link in the network between this item-based and holistic-cxn so that in the future the holistic-cxn can fill the slot of the item-based
;---------------------------------------------------------------------------------------------------------------------------------------
;2) item-based->holistic: if there is a item-based-cxn that can apply, but there is still some form left in the root. We make a new holistic-cxn that pairs this remaining form with the part of the observed meaning that is not in the item-based-cxn, and we add a link between the category of this holistic-cxn and the item-based-cxn that applied.
;---------------------------------------------------------------------------------------------------------------------------------------
;3) holophrase->item-based+holistic: abstract away from differences between observed observation and a known holophrase-cxn. It adds a item-based-cxn with the similarities and a holistic-cxn for the difference
;3a. addition: the observed utterance contains more than the known holophrase. A new item-based-cxn is created with a slot and a holistic-cxn that covers the part of the observed form and meaning that is not in the known holophrase-cxn. The categories of these two cxns are connected in the network
;3b. deletion: the observed utterance contains less than the known holophrase. A new item-based-cxn is learned with a slot for the part of the meaning and form of the known holophrase that does not occur in the observed meaning and form. This non-overlapping info is also added to a new holistic-cxn that is linked to the item-based-cxn in the network. A new holophrase is created for the observed utterance as well.
;---------------------------------------------------------------------------------------------------------------------------------------
;4) holistic->item-based: when a holistic-cxn can apply, but surrounding form remains in root. --> a new item-based-cxn is learned that pairs the remaining-form with the meaning not covered by the holistic-cxn. The category of this item-based-cxn is connected to the one of the holistic-cxn that applied.
;---------------------------------------------------------------------------------------------------------------------------------------
;5) noting->holophrase: create a holistic-cxn that covers the entire meaning and form of the utterance that was observed.

;-----------------------------
; New repairs that are needed:
;-----------------------------
;1) relaxing on location: if no solution is found, but a cxn exists that has as only difference that one constant is diferent throughout (a location constant, e.g. right is replaced by left everywhere), then the cxn is duplicated and right is replaced by a variable everywhere. Since in an itembased cxn, the location of the slot is kept by default (e.g. right), if there is some agreement between parts of the rest of the item-based cxn and the slot, there will be agreement between them through the variable. --> but: how do we extract the location of a slot?? --> just look for any instance of a location predicate?


;--------------------------------;
; + Holophrastic constructions + ;
;--------------------------------;

; example #11_0: "what is the largest state in the us?" ((ANSWER ?B ?A ?C)(LARGEST ?C ?A ?D)(STATE ?D ?A))
(def-fcg-cxn answer-largest-state-1-cxn
             (<-
              (?answer-largest-state-unit
               (HASH meaning ((ANSWER ?B ?A ?C)(LARGEST ?C ?A ?D)(STATE ?D ?A)))
               --
               (HASH form (;AMERIQUE.FRITES --> two-handed
                           (left-hand-articulation ?ns-amerique.frites-1 ns-amerique.frites)
                           (right-hand-articulation ?ns-amerique.frites-2 ns-amerique.frites)
                           (temporal-relation ?ns-amerique.frites-1 ?ns-amerique.frites-2 equals)
                           ;DSS-GRAND-ENTITE-GEOGRAPHIQUE --> two-handed, location = right
                           (left-hand-articulation ?dss-grand-entite-geographique-1 dss-grand-entite-geographique)
                           (right-hand-articulation ?dss-grand-entite-geographique-2 dss-grand-entite-geographique)
                           (location ?dss-grand-entite-geographique-1 rssp)
                           (location ?dss-grand-entite-geographique-2 rssp)
                           (temporal-relation ?dss-grand-entite-geographique-1 ?dss-grand-entite-geographique-2 equals)
                           ;IL-Y-A --> dominant hand
                           (left-hand-articulation ?il-y-a-1 il-y-a)
                           ;DSS-PETIT-ENTITE-GEOGRAPHIQUE --> dominant hand, location = right, modification = reduplicated
                           (left-hand-articulation ?dss-petit-entite-geographique-1 dss-petit-entite-geographique)
                           (location ?dss-petit-entite-geographique-1 rssp)
                           (modification ?dss-petit-entite-geographique-1 reduplicated)
                           ;FBUOY-GRAND-ENTITE-GEOGRAPHIQE --> non-dominant hand, location = right
                           (right-hand-articulation ?fbuoy-grand-entite-geographique-1 fbuoy-grand-entite-geographique)
                           (location ?fbuoy-grand-entite-geographique-1 rssp)
                           ;UN --> dominant hand
                           (left-hand-articulation ?un-1 un)
                           ;PT --> dominant hand, location = right
                           (left-hand-articulation ?pt-1 pt)
                           (location ?pt-1 rssp)
                           ;PLUS.P --> dominant hand
                           (left-hand-articulation ?plus.p-1 plus.p)
                           ;GRAND.5 --> two-handed
                           (left-hand-articulation ?grand.5-1 grand.5) 
                           (right-hand-articulation ?grand.5-2 grand.5) 
                           (temporal-relation ?grand.5-1 ?grand.5-2 equals)
                           ;OU-LIEU.O --> two-handed
                           (left-hand-articulation ?ou-lieu.o-1 ou-lieu.o)
                           (right-hand-articulation ?ou-lieu.o-2 ou-lieu.o)
                           (temporal-relation ?ou-lieu.o-1 ?ou-lieu.o-2 equals)
                           ; MEETS
                           (meets ?ns-amerique.frites-1 ?dss-grand-entite-geographique-1)
                           (meets ?dss-grand-entite-geographique-1 ?il-y-a-1)
                           (meets ?il-y-a-1 ?dss-petit-entite-geographique-1)
                           (meets ?dss-petit-entite-geographique-1 ?un-1)
                           (meets ?un-1 ?pt-1)
                           (meets ?pt-1 ?plus.p-1)
                           (meets ?plus.p-1 ?grand.5-1)
                           (meets ?grand.5-1 ?ou-lieu.o-1)
                           ; BUOY TEMPORAL RELATIONS
                           (temporal-relation ?plus.p-1 ?fbuoy-grand-entite-geographique-1 finishes)
                           (temporal-relation ?il-y-a-1 ?fbuoy-grand-entite-geographique-1 starts)
                           (temporal-relation ?un-1 ?fbuoy-grand-entite-geographique-1 during)
                           (temporal-relation ?pt-1 ?fbuoy-grand-entite-geographique-1 during))))))


; example #32_0: "what is the largest capital?" ((ANSWER ?B ?A ?C)(LARGEST ?C ?A ?D)(CAPITAL ?D ?A))


(def-fcg-cxn answer-largest-capital-1-cxn
             (<-
              (?answer-largest-capital-unit
               (HASH meaning ((ANSWER ?B ?A ?C)(LARGEST ?C ?A ?D)(CAPITAL ?D ?A)))
               --
               (HASH form (;DANS --> two-handed
                           (left-hand-articulation ?dans-1 dans)
                           (right-hand-articulation ?dans-2 dans)
                           (temporal-relation ?dans-1 ?dans-2 equals)
                           ;NS-AMERIQUE.FRITES --> two-handed
                           (left-hand-articulation ?ns-amerique.frites-1 ns-amerique.frites)
                           (right-hand-articulation ?ns-amerique.frites-2 ns-amerique.frites)
                           (temporal-relation ?ns-amerique.frites-1 ?ns-amerique.frites-2 equals)
                           ;DSS-GRAND-ENTITE-GEOGRAPHIQUE --> two-handed, location = right
                           (left-hand-articulation ?dss-grand-entite-geographique-1 dss-grand-entite-geographique)
                           (location ?dss-grand-entite-geographique-1 rssp)
                           (right-hand-articulation ?dss-grand-entite-geographique-2 dss-grand-entite-geographique)
                           (location ?dss-grand-entite-geographique-2 rssp)
                           (temporal-relation ?dss-grand-entite-geographique-1 ?dss-grand-entite-geographique-2 equals)
                           ;IL-Y-A --> dominant hand
                           (left-hand-articulation ?il-y-a-1 il-y-a)
                           ;DIFFERENT --> two-handed
                           (left-hand-articulation ?different-1 different)
                           (right-hand-articulation ?different-2 different)
                           (temporal-relation ?different-1 ?different-2 equals)
                           ;CAPITALE.MID --> two-handed
                           (left-hand-articulation ?capitale.mid-1 capitale.mid)
                           (right-hand-articulation ?capitale.mid-2 capitale.mid)
                           (temporal-relation ?capitale.mid-1 ?capitale.mid-2 equals)
                           ;PT --> dominant hand, location = right, reduplicated
                           (left-hand-articulation ?pt-1 pt)
                           (location ?pt-1 rssp)
                           (modification ?pt-1 reduplicated)
                           ;FBUOY-GRAND-ENTITE-GEOGRAPHIQUE --> non-dominant hand, location = right
                           (right-hand-articulation ?fbuoy-grand-entite-geographique-1 fbuoy-grand-entite-geographique)
                           (location ?fbuoy-grand-entite-geographique-1 rssp)
                           ;UN --> dominant hand
                           (left-hand-articulation ?un-1 un)
                           ;PT --> dominant hand, location = right
                           (left-hand-articulation ?pt-2 pt) 
                           (location ?pt-2 rssp)
                           ;PLUS.P --> dominant hand
                           (left-hand-articulation ?plus.p-1 plus.p)
                           ;GRAND.5 --> two-handed
                           (left-hand-articulation ?grand.5-1 grand.5)
                           (right-hand-articulation ?grand.5-2 grand.5)
                           (temporal-relation ?grand.5-1 ?grand.5-2 equals)
                           ;OU-LIEU.O --> two-handed
                           (left-hand-articulation ?ou-lieu.o-1 ou-lieu.o)
                           (right-hand-articulation ?ou-lieu.o-2 ou-lieu.o)
                           (temporal-relation ?ou-lieu.o-1 ?ou-lieu.o-2 equals)
                           ;MEETS
                           (meets ?dans-1 ?ns-amerique.frites-1)
                           (meets ?ns-amerique.frites-1 ?dss-grand-entite-geographique-1)
                           (meets ?dss-grand-entite-geographique-1 ?il-y-a-1)
                           (meets ?il-y-a-1 ?different-1)
                           (meets ?different-1 ?capitale.mid-1)
                           (meets ?capitale.mid-1 ?pt-1)
                           (meets ?pt-1 ?un-1)
                           (meets ?un-1 ?pt-2)
                           (meets ?pt-2 ?plus.p-1)
                           (meets ?plus.p-1 ?grand.5-1)
                           (meets ?grand.5-1 ?ou-lieu.o-1)
                           ;FBUOY temporal relations
                           (temporal-relation ?pt-1 ?fbuoy-grand-entite-geographique-1 starts)
                           (temporal-relation ?un-1 ?fbuoy-grand-entite-geographique-1 during)
                           (temporal-relation ?pt-2 ?fbuoy-grand-entite-geographique-1 during)
                           (temporal-relation ?plus.p-1 ?fbuoy-grand-entite-geographique-1 finishes)
                           )))))

(def-fcg-cxn answer-size-const-stateid-alaska-1-cxn
             (<-
              (?answer-size-const-stateid-alaska-1-unit
               (HASH meaning ((ANSWER ?C ?A ?D)(SIZE ?D ?B ?A)(CONST ?D ?B ?E)(STATEID ?E ?F)(ALASKA ?F)))
               --
               (HASH form (;FS-ALASKA --> dominant hand
                           (left-hand-articulation ?fs-alaska-1 fs-alaska)
                           ;PT5 --> dominant hand, location = middsp, modification = cercle
                           (left-hand-articulation ?pt5-1 pt5)
                           (location ?pt5-1 midssp)
                           (modification ?pt5-1 cercle)
                           ;COMBIEN --> dominant hand
                           (left-hand-articulation ?combien-1 combien)
                           ;METRE-CARRE --> dominant hand
                           (left-hand-articulation ?metre-carre-1 metre-carre)
                           ;DSH-MESURER --> two-handed, location = midssp
                           (left-hand-articulation ?dsh-mesurer-1 dsh-mesurer)
                           (location ?dsh-mesurer-1 midssp)
                           (right-hand-articulation ?dsh-mesurer-2 dsh-mesurer)
                           (location ?dsh-mesurer-2 midssp)
                           (temporal-relation ?dsh-mesurer-1 ?dsh-mesurer-2 equals)
                           ;DSS-GRAND-ENTITE-GEOGRAPHIQUE --> two-handed, location = right
                           (left-hand-articulation ?dss-grand-entite-geographique-1 dss-grand-entite-geographique)
                           (location ?dss-grand-entite-geographique-1 midssp)
                           (modification ?dss-grand-entite-geographique-1 **)
                           (right-hand-articulation ?dss-grand-entite-geographique-2 dss-grand-entite-geographique)
                           (location ?dss-grand-entite-geographique-2 midssp)
                           (modification ?dss-grand-entite-geographique-2 **)
                           (temporal-relation ?dss-grand-entite-geographique-1 ?dss-grand-entite-geographique-2 equals)
                           ;FBUOY-GRAND-ENTITE-GEOGRAPHIQUE --> non dominant hand, location = midssp
                           (right-hand-articulation ?fbuoy-grand-entite-geographique-1 fbuoy-grand-entite-geographique)
                           (location ?fbuoy-grand-entite-geographique-1 midssp)
                           ;MEETS
                           (meets ?fs-alaska-1 ?pt5-1)
                           (meets ?pt5-1 ?combien-1)
                           (meets ?combien-1 ?metre-carre-1)
                           (meets ?metre-carre-1 ?dsh-mesurer-1)
                           (meets ?dsh-mesurer-1 ?dss-grand-entite-geographique-1)
                           ;FBUOY temporal relations
                           (temporal-relation ?pt5-1 ?fbuoy-grand-entite-geographique-1 starts)
                           (temporal-relation ?combien-1 ?fbuoy-grand-entite-geographique-1 during)
                           (temporal-relation ?metre-carre-1 ?fbuoy-grand-entite-geographique-1 finishes))))))

(def-fcg-cxn answer-size-const-cityid-new_york-1-cxn ;--> differences between this cxn and answer-size-const-stateid-alaska-1-cxn are to big to learn another item-based cxn with two slots. But the differences do not seem to be related to the fact that it is a city or state in the form. It rather seems to be a different form for expressing the same meaning. Maybe ask Sybille if she feels both forms can be used for states and cities or if there would be a difference. if both forms can be used for states and cities, then we can move stateid and cityid to the holistic cxns and have a general pattern that works for both 
             (<-
              (?answer-size-const-cityid-new_york-1-unit
               (HASH meaning ((ANSWER ?C ?A ?D)(SIZE ?D ?B ?A)(CONST ?D ?B ?E)(CITYID ?E ?F)(NEW_YORK ?F)))
               --
               (HASH form (;PT --> dominant hand, location = right
                           (left-hand-articulation ?pt-1 pt)
                           (location ?pt-1 rssp)
                           ;NS-NEW-YORK.Y-LOC --> two-handed
                           (left-hand-articulation ?ns-new-york.y-loc-1 ns-new-york.y-loc)
                           (right-hand-articulation ?ns-new-york.y-loc-2 ns-new-york.y-loc)
                           (temporal-relation ?ns-new-york.y-loc-1 ?ns-new-york.y-loc-2 equals)
                           ;DSS-GRAND-ENTITE-GEOGRAPHIQUE --> two-handed, location = right, modification = reduplicated
                           (left-hand-articulation ?dss-grand-entite-geographique-1 dss-grand-entite-geographique)
                           (location ?dss-grand-entite-geographique-1 rssp)
                           (modification ?dss-grand-entite-geographique-1 reduplicated)
                           (right-hand-articulation ?dss-grand-entite-geographique-2 dss-grand-entite-geographique)
                           (location ?dss-grand-entite-geographique-2 rssp)
                           (modification ?dss-grand-entite-geographique-2 reduplicated)
                           (temporal-relation ?dss-grand-entite-geographique-1 ?dss-grand-entite-geographique-2 equals)
                           ;PT --> dominant hand, location = middsp, modification = cercle
                           (left-hand-articulation ?pt-2 pt)
                           (location ?pt-2 rssp)
                           (modification ?pt-2 cercle)
                           ;COMBIEN --> dominant hand
                           (left-hand-articulation ?combien-1 combien)
                           ;METRE-CARRE --> dominant hand
                           (left-hand-articulation ?metre-carre-1 metre-carre)
                           ;FBUOY-GRAND-ENTITE-GEOGRAPHIQUE --> non dominant hand, location = midssp
                           (right-hand-articulation ?fbuoy-grand-entite-geographique-1 fbuoy-grand-entite-geographique)
                           (location ?fbuoy-grand-entite-geographique-1 rssp)
                           ;MEETS
                           (meets ?pt-1 ?ns-new-york.y-loc-1)
                           (meets ?ns-new-york.y-loc-1 ?dss-grand-entite-geographique-1)
                           (meets ?dss-grand-entite-geographique-1 ?pt-2)
                           (meets ?pt-2 ?combien-1)
                           (meets ?combien-1 ?metre-carre-1)
                           ;FBUOY temporal relations
                           (temporal-relation ?pt-2 ?fbuoy-grand-entite-geographique-1 equals))))))



(def-fcg-cxn answer-elevation-const-placeid-guadalupe_peak-1-cxn
             (<-
              (?answer-elevation-const-placeid-guadalupe_peak-1-unit
               (HASH meaning ((ANSWER ?C ?A ?D)(ELEVATION ?D ?B ?A)(CONST ?D ?B ?E)(PLACEID ?E ?F)(GUADALUPE_PEAK ?F)))
               --
               (HASH form (;MONTAGNE --> two-handed
                           (left-hand-articulation ?montagne-1 montagne)
                           (right-hand-articulation ?montagne-2 montagne)
                           (temporal-relation ?montagne-1 ?montagne-2 equals)
                           ;PT --> dominant hand, location = right
                           (left-hand-articulation ?pt-1 pt)
                           (location ?pt-1 rssp)
                           ;PT --> dominant hand, location = right, modification = down-up
                           (left-hand-articulation ?pt-2 pt)
                           (location ?pt-2 rssp)
                           (modification ?pt-2 down-up)
                           ;FS-GUADALUPE --> dominant hand
                           (left-hand-articulation ?fs-guadalupe-1 fs-guadalupe)
                           ;PT --> dominant hand, location = right, modification = down-up
                           (left-hand-articulation ?pt-3 pt)
                           (location ?pt-3 rssp)
                           (modification ?pt-3 down-up)
                           ;COMBIEN --> dominant hand
                           (left-hand-articulation ?combien-1 combien)
                           ;METRE --> two-handed
                           (left-hand-articulation ?metre-1 metre)
                           (right-hand-articulation ?metre-2 metre)
                           (temporal-relation ?metre-1 ?metre-2 equals)
                           ;FBUOY-MONTAGE --> non-dominant hand
                           (right-hand-articulation ?fbuoy-montagne-1 fbuoy-montagne)
                           ;MEETS
                           (meets ?montagne-1 ?pt-1)
                           (meets ?pt-1 ?pt-2)
                           (meets ?pt-2 ?fs-guadalupe-1)
                           (meets ?fs-guadalupe-1 ?pt-3)
                           (meets ?pt-3 ?combien-1)
                           (meets ?combien-1 ?metre-1)
                           ;FBUOY temporal relations
                           (temporal-relation ?pt-1 ?fbuoy-montagne-1 starts)
                           (temporal-relation ?pt-2 ?fbuoy-montagne-1 during)
                           (temporal-relation ?fs-guadalupe-1 ?fbuoy-montagne-1 during)
                           (temporal-relation ?pt-3 ?fbuoy-montagne-1 finishes))))))
                           


;------------------------------;
; + item-based constructions + ;
;------------------------------;

(def-fcg-cxn answer-largest-slot1-1-cxn
             ((?answer-largest-slot1-unit
               (subunits (?slot1-unit)))
              <-
              (?answer-largest-slot1-unit
               (HASH meaning ((ANSWER ?B ?A ?C)(LARGEST ?C ?A ?D)))
               --
               (HASH form (;NS-AMERIQUE.FRITES --> two-handed
                           (left-hand-articulation ?ns-amerique.frites-1 ns-amerique.frites)
                           (right-hand-articulation ?ns-amerique.frites-2 ns-amerique.frites)
                           (temporal-relation ?ns-amerique.frites-1 ?ns-amerique.frites-2 equals)
                           ;DSS-GRAND-ENTITE-GEOGRAPHIQUE --> two-handed, location = ?location-1
                           (left-hand-articulation ?dss-grand-entite-geographique-1 dss-grand-entite-geographique)
                           (right-hand-articulation ?dss-grand-entite-geographique-2 dss-grand-entite-geographique)
                           (location ?dss-grand-entite-geographique-1 rssp)
                           (location ?dss-grand-entite-geographique-2 rssp)
                           (temporal-relation ?dss-grand-entite-geographique-1 ?dss-grand-entite-geographique-2 equals)
                           ;IL-Y-A --> dominant hand
                           (left-hand-articulation ?il-y-a-1 il-y-a)
                           ;UN --> dominant hand
                           (left-hand-articulation ?un-1 un)
                           ;PT --> dominant hand, location = ?location-1
                           (left-hand-articulation ?pt-1 pt)
                           (location ?pt-1 rssp)
                           ;PLUS.P --> dominant hand
                           (left-hand-articulation ?plus.p-1 plus.p)
                           ;GRAND.5 --> two-handed
                           (left-hand-articulation ?grand.5-1 grand.5)
                           (right-hand-articulation ?grand.5-2 grand.5)
                           (temporal-relation ?grand.5-1 ?grand.5-2 equals)
                           ;OU-LIEU.O --> two-handed
                           (left-hand-articulation ?ou-lieu.o-1 ou-lieu.o)
                           (right-hand-articulation ?ou-lieu.o-2 ou-lieu.o)
                           (temporal-relation ?ou-lieu.o-1 ?ou-lieu.o-2 equals)
                           ;MEETS
                           (meets ?ns-amerique.frites-1 ?dss-grand-entite-geographique-1)
                           (meets ?dss-grand-entite-geographique-1 ?il-y-a-1)
                           (meets ?il-y-a-1 ?slot1-lh-left)
                           (meets ?slot1-lh-right ?un-1)
                           (meets ?un-1 ?pt-1)
                           (meets ?pt-1 ?plus.p-1)
                           (meets ?plus.p-1 ?grand.5-1)
                           (meets ?grand.5-1 ?ou-lieu.o-1)
                           ;FBUOY temporal relations
                           (temporal-relation ?un-1 ?slot1-rh-right during)
                           (temporal-relation ?pt-1 ?slot1-rh-right during)
                           (temporal-relation ?plus.p-1 ?slot1-rh-right finishes)
                           )))
              (?slot1-unit
               (meaning-args ((partial-network ?D)
                              (target ?A)))
               --
               (category answer-largest-slot1-1-slot1-cat)
               (boundaries (lh-leftmost ?slot1-lh-left)
                           (lh-rightmost ?slot1-lh-right)
                           (rh-rightmost ?slot1-rh-right))
               (location rssp))))


(def-fcg-cxn answer-area-largest-slot1-1-cxn
             ((?answer-area-largest-slot1-unit
               (subunits (?slot1-unit)))
              <-
              (?answer-area-largest-slot1-unit
               (HASH meaning ((ANSWER ?C ?A ?D)(AREA ?D ?B ?A)(LARGEST ?D ?B ?E)))
               --
               (HASH form (;DANS --> two-handed
                           (left-hand-articulation ?dans-1 dans)
                           (right-hand-articulation ?dans-2 dans)
                           (temporal-relation ?dans-1 ?dans-2 equals)
                           ;NS-AMERIQUE.FRITES --> two-handed
                           (left-hand-articulation ?ns-amerique.frites-1 ns-amerique.frites)
                           (right-hand-articulation ?ns-amerique.frites-2 ns-amerique.frites)
                           (temporal-relation ?ns-amerique.frites-1 ?ns-amerique.frites-2 equals)
                           ;DSS-GRAND-ENTITE-GEOGRAPHIQUE 
                           (left-hand-articulation ?dss-grand-entite-geographique-1 dss-grand-entite-geographique)
                           (right-hand-articulation ?dss-grand-entite-geographique-2 dss-grand-entite-geographique)
                           (location ?dss-grand-entite-geographique-1 rssp)
                           (location ?dss-grand-entite-geographique-2 rssp)
                           (temporal-relation ?dss-grand-entite-geographique-1 ?dss-grand-entite-geographique-2 equals)
                           ;IL-Y-A --> dominant hand
                           (left-hand-articulation ?il-y-a-1 il-y-a)
                           ;UN --> dominant hand
                           (left-hand-articulation ?un-1 un)
                           ;PT --> dominant hand, location = location-1
                           (left-hand-articulation ?pt-1 pt)
                           (location ?pt-1 rssp)
                           ;GRAND.GROS --> two-handed
                           (left-hand-articulation ?grand.gros-1 grand.gros)
                           (right-hand-articulation ?grand.gros-2 grand.gros)
                           (temporal-relation ?grand.gros-1 ?grand.gros-2 equals)
                           ;PT --> dominant hand, location = location-1
                           (left-hand-articulation ?pt-2 pt)
                           (location ?pt-2 rssp)
                           ;COMBIEN --> dominant hand
                           (left-hand-articulation ?combien-1 combien)
                           ;METRE-CARRE --> dominant hand
                           (left-hand-articulation ?metre-carre-1 metre-carre)
                           ;PT --> dominant hand, location = location-1
                           (left-hand-articulation ?pt-3 pt)
                           (location ?pt-3 rssp)
                           ;MEETS
                           (meets ?dans-1 ?ns-amerique.frites-1)
                           (meets ?ns-amerique.frites-1 ?dss-grand-entite-geographique-1)
                           (meets ?dss-grand-entite-geographique-1 ?il-y-a-1)
                           (meets ?il-y-a-1 ?slot1-lh-left)
                           (meets ?slot1-lh-right ?un-1)
                           (meets ?un-1 ?pt-1)
                           (meets ?pt-1 ?grand.gros-1)
                           (meets ?grand.gros-1 ?pt-2)
                           (meets ?pt-2 ?combien-1)
                           (meets ?combien-1 ?metre-carre-1)
                           (meets ?metre-carre-1 ?pt-3)
                           ;FBUOY temporal relations
                           (temporal-relation ?il-y-a-1 ?slot1-rh-right starts)
                           (temporal-relation ?un-1 ?slot1-rh-right during)
                           (temporal-relation ?pt-1 ?slot1-rh-right finishes))))
              (?slot1-unit
               (meaning-args ((partial-network ?E)
                              (target ?B)))
               --
               (category answer-area-largest-slot1-1-slot1-cat)
               (boundaries (lh-leftmost ?slot1-lh-left)
                           (lh-rightmost ?slot1-lh-right)
                           (rh-rightmost ?slot1-rh-right))
               (location rssp))))

(def-fcg-cxn answer-population-largest-density-slot1-1-cxn
             ((?answer-population-largest-density-slot1-unit
               (subunits (?slot1-unit)))
              <-
              (?answer-population-largest-density-slot1-unit
               (HASH meaning ((ANSWER ?D ?A ?E)(POPULATION ?E ?B ?A)(LARGEST ?E ?C ?F)(DENSITY ?F ?B ?C)))
               --
               (HASH form (;DANS --> two-handed
                           (left-hand-articulation ?dans-1 dans)
                           (right-hand-articulation ?dans-2 dans)
                           (temporal-relation ?dans-1 ?dans-2 equals)
                           ;NS-AMERIQUE.FRITES --> two-handed
                           (left-hand-articulation ?ns-amerique.frites-1 ns-amerique.frites)
                           (right-hand-articulation ?ns-amerique.frites-2 ns-amerique.frites)
                           (temporal-relation ?ns-amerique.frites-1 ?ns-amerique.frites-2 equals)
                           ;IL-Y-A --> dominant hand
                           (left-hand-articulation ?il-y-a-1 il-y-a)
                            ;UN --> dominant hand
                           (left-hand-articulation ?un-1 un)
                           ;PT --> dominant hand, location = location-1, modification = cercle
                           (left-hand-articulation ?pt-1 pt)
                           (location ?pt-1 midssp)
                           (modification ?pt-1 cercle)
                           ;PT --> dominant hand, location = location-1
                           (left-hand-articulation ?pt-2 pt)
                           (location ?pt-2 midssp)
                           ;PERSONNE.HUMAIN --> dominant hand
                           (left-hand-articulation ?personne.humain-1 personne.humain)
                           ;MASSE --> two-handed
                           (left-hand-articulation ?masse-1 masse)
                           (right-hand-articulation ?masse-2 masse)
                           (temporal-relation ?masse-1 ?masse-2 equals)
                           ;SERRER --> two-handed
                           (left-hand-articulation ?serrer-1 serrer)
                           (right-hand-articulation ?serrer-2 serrer)
                           (temporal-relation ?serrer-1 ?serrer-2 equals)
                           ;UN --> dominant hand
                           (left-hand-articulation ?un-2 un)
                           ;PT --> dominant hand, location, location-1
                           (left-hand-articulation ?pt-3 pt)
                           (location ?pt-3 midssp)
                           ;COMBIEN --> dominant hand
                           (left-hand-articulation ?combien-1 combien)
                           ;PERSONNE.HUMAIN --> two-handed
                           (left-hand-articulation ?personne.humain-3 personne.humain)
                           (right-hand-articulation ?personne.humain-4 personne.humain)
                           (temporal-relation ?personne.humain-3 ?personne.humain-4 equals)
                           ;PALM-UP --> dominant hand
                           (left-hand-articulation ?palm-up-1 palm-up)
                           ;MEETS
                           (meets ?dans-1 ?ns-amerique.frites-1)
                           (meets ?ns-amerique.frites-1 ?il-y-a-1)
                           (meets ?il-y-a-1 ?slot1-lh-left)
                           (meets ?slot1-lh-right ?un-1)
                           (meets ?un-1 ?pt-1)
                           (meets ?pt-1 ?pt-2)
                           (meets ?pt-2 ?personne.humain-1)
                           (meets ?personne.humain-1 ?masse-1)
                           (meets ?masse-1 ?serrer-1)
                           (meets ?serrer-1 ?un-2)
                           (meets ?un-2 ?pt-3)
                           (meets ?pt-3 ?combien-1)
                           (meets ?combien-1 ?personne.humain-3)
                           (meets ?personne.humain-3 ?palm-up-1)
                           ;FBUOY temporal relations
                           (temporal-relation ?il-y-a-1 ?slot1-rh-right starts)
                           (temporal-relation ?un-1 ?slot1-rh-right during)
                           (temporal-relation ?pt-1 ?slot1-rh-right during)
                           (temporal-relation ?pt-2 ?slot1-rh-right during)
                           (temporal-relation ?personne.humain-1 ?slot1-rh-right finishes)
                           )))
               (?slot1-unit
                (meaning-args ((partial-network ?F)
                               (target ?B)))
                --
                (category answer-population-largest-density-slot1-1-slot1-cat)
                (boundaries (lh-leftmost ?slot1-lh-left)
                            (lh-rightmost ?slot1-lh-right)
                            (rh-rightmost ?slot1-rh-right))
                (location midssp))))


(def-fcg-cxn answer-largest-state-slot1-2-cxn
             ((?answer-largest-state-slot1-2-unit
               (subunits (?slot1-unit)))
              <-
              (?answer-largest-state-slot1-2-unit
               (HASH meaning ((ANSWER ?B ?A ?C)(LARGEST ?C ?A ?D)))
               --
               (HASH form (;DANS --> two-handed
                           (left-hand-articulation ?dans-1 dans)
                           (right-hand-articulation ?dans-2 dans)
                           (temporal-relation ?dans-1 ?dans-2 equals)
                           ;NS-AMERIQUE.FRITES --> two-handed
                           (left-hand-articulation ?ns-amerique.frites-1 ns-amerique.frites)
                           (right-hand-articulation ?ns-amerique.frites-2 ns-amerique.frites)
                           (temporal-relation ?ns-amerique.frites-1 ?ns-amerique.frites-2 equals)
                           ;DSS-GRAND-ENTITE-GEOGRAPHIQUE --> two-handed, location = right
                           (left-hand-articulation ?dss-grand-entite-geographique-1 dss-grand-entite-geographique)
                           (right-hand-articulation ?dss-grand-entite-geographique-2 dss-grand-entite-geographique)
                           (location ?dss-grand-entite-geographique-1 rssp)
                           (location ?dss-grand-entite-geographique-2 rssp)
                           (temporal-relation ?dss-grand-entite-geographique-1 ?dss-grand-entite-geographique-2 equals)                         
                           ;IL-Y-A --> dominant hand
                           (left-hand-articulation ?il-y-a-1 il-y-a)
                           ;GRAND.5 --> two-handed
                           (left-hand-articulation ?grand.5-1 grand.5)
                           (right-hand-articulation ?grand.5-2 grand.5)
                           (temporal-relation ?grand.5-1 ?grand.5-2 equals)
                           ;PLUS.P --> dominant hand
                           (left-hand-articulation ?plus.p-1 plus.p)
                           ;GRAND.5 --> two-handed
                           (left-hand-articulation ?grand.5-3 grand.5)
                           (right-hand-articulation ?grand.5-4 grand.5)
                           (temporal-relation ?grand.5-3 ?grand.5-4 equals)
                           ;PT --> dominant hand, location = rssp, modification = cercle
                           (left-hand-articulation ?pt-2 pt)
                           (location ?pt-2 rssp)
                           (modification ?pt-2 cercle)
                           ;OU-LIEU.O --> dominant hand
                           (left-hand-articulation ?ou-lieu.o-1 ou-lieu.o)
                           ;FBUOY-GRAND-ENTITE-GEOGRAPHIQUE, location = rssp
                           (right-hand-articulation ?fbuoy-grand-entite-geographique-2 fbuoy-grand-entite-geographique)
                           (location ?fbuoy-grand-entite-geographique-2 rssp)
                           ;MEETS
                           (meets ?dans-1 ?ns-amerique.frites-1)
                           (meets ?ns-amerique.frites-1 ?dss-grand-entite-geographique-1)
                           (meets ?dss-grand-entite-geographique-1 ?il-y-a-1)
                           (meets ?il-y-a-1 ?slot1-lh-left)
                           (meets ?slot1-lh-right ?grand.5-1)
                           (meets ?grand.5-1 ?plus.p-1)
                           (meets ?plus.p-1 ?grand.5-3)
                           (meets ?grand.5-3 ?pt-2)
                           (meets ?pt-2 ?ou-lieu.o-1)
                           ;FBUOY temporal relations
                           (temporal-relation ?il-y-a-1 ?slot1-rh-right starts)
                           (temporal-relation ?pt-2 ?fbuoy-grand-entite-geographique-2 starts)
                           (temporal-relation ?ou-lieu.o-1 ?fbuoy-grand-entite-geographique-2 finishes)
                           )))
              (?slot1-unit
               (meaning-args ((partial-network ?D)
                               (target ?A)))
               --
               (category answer-largest-state-slot1-2-slot1-cat)
               (boundaries (lh-leftmost ?slot1-lh-left)
                           (lh-rightmost ?slot1-lh-right)
                           (rh-rightmost ?slot1-rh-right))
               (location rssp))))
              
(def-fcg-cxn answer-size-const-stateid-slot1-1-cxn
             ((?answer-size-const-stateid-slot1-1-unit
               (subunits (slot1-unit)))
              <-
              (?answer-size-const-stateid-slot1-1-unit
               (HASH meaning ((ANSWER ?C ?A ?D)(SIZE ?D ?B ?A)(CONST ?D ?B ?E)(STATEID ?E ?F)))
               --
               (HASH form (;PT5 --> dominant hand, location = middsp, modification = cercle
                           (left-hand-articulation ?pt5-1 pt5)
                           (location ?pt5-1 midssp)
                           (modification ?pt5-1 cercle)
                           ;COMBIEN --> dominant hand
                           (left-hand-articulation ?combien-1 combien)
                           ;METRE-CARRE --> dominant hand
                           (left-hand-articulation ?metre-carre-1 metre-carre)
                           ;DSH-MESURER --> two-handed, location = midssp
                           (left-hand-articulation ?dsh-mesurer-1 dsh-mesurer)
                           (location ?dsh-mesurer-1 midssp)
                           (right-hand-articulation ?dsh-mesurer-2 dsh-mesurer)
                           (location ?dsh-mesurer-2 midssp)
                           (temporal-relation ?dsh-mesurer-1 ?dsh-mesurer-2 equals)
                           ;DSS-GRAND-ENTITE-GEOGRAPHIQUE --> two-handed, location = right
                           (left-hand-articulation ?dss-grand-entite-geographique-1 dss-grand-entite-geographique)
                           (location ?dss-grand-entite-geographique-1 midssp)
                           (modification ?dss-grand-entite-geographique-1 **)
                           (right-hand-articulation ?dss-grand-entite-geographique-2 dss-grand-entite-geographique)
                           (location ?dss-grand-entite-geographique-2 midssp)
                           (modification ?dss-grand-entite-geographique-2 **)
                           (temporal-relation ?dss-grand-entite-geographique-1 ?dss-grand-entite-geographique-2 equals)
                           ;FBUOY-GRAND-ENTITE-GEOGRAPHIQUE --> non dominant hand, location = midssp
                           (right-hand-articulation ?fbuoy-grand-entite-geographique-1 fbuoy-grand-entite-geographique)
                           (location ?fbuoy-grand-entite-geographique-1 midssp)
                           ;MEETS
                           (meets ?slot1-lh-right ?pt5-1)
                           (meets ?pt5-1 ?combien-1)
                           (meets ?combien-1 ?metre-carre-1)
                           (meets ?metre-carre-1 ?dsh-mesurer-1)
                           (meets ?dsh-mesurer-1 ?dss-grand-entite-geographique-1)
                           ;FBUOY temporal relations
                           (temporal-relation ?pt5-1 ?fbuoy-grand-entite-geographique-1 starts)
                           (temporal-relation ?combien-1 ?fbuoy-grand-entite-geographique-1 during)
                           (temporal-relation ?metre-carre-1 ?fbuoy-grand-entite-geographique-1 finishes))))
              (?slot1-unit
               (meaning-args ((target ?F)))
               --
               (category answer-size-const-stateid-slot1-1-slot1-cat)
               (boundaries (lh-rightmost ?slot1-lh-right)))))

(def-fcg-cxn answer-size-const-cityid-slot1-1-cxn
             ((?answer-size-const-cityid-slot1-1-unit
               (subunits (?slot1-unit)))
              <-
              (?answer-size-const-cityid-slot1-1-unit
               (HASH meaning ((ANSWER ?C ?A ?D)(SIZE ?D ?B ?A)(CONST ?D ?B ?E)(CITYID ?E ?F)))
               --
               (HASH form (;PT --> dominant hand, location = right
                           (left-hand-articulation ?pt-1 pt)
                           (location ?pt-1 rssp)
                           ;DSS-GRAND-ENTITE-GEOGRAPHIQUE --> two-handed, location = right, modification = reduplicated
                           (left-hand-articulation ?dss-grand-entite-geographique-1 dss-grand-entite-geographique)
                           (location ?dss-grand-entite-geographique-1 rssp)
                           (modification ?dss-grand-entite-geographique-1 reduplicated)
                           (right-hand-articulation ?dss-grand-entite-geographique-2 dss-grand-entite-geographique)
                           (location ?dss-grand-entite-geographique-2 rssp)
                           (modification ?dss-grand-entite-geographique-2 reduplicated)
                           (temporal-relation ?dss-grand-entite-geographique-1 ?dss-grand-entite-geographique-2 equals)
                           ;PT --> dominant hand, location = middsp, modification = cercle
                           (left-hand-articulation ?pt-2 pt)
                           (location ?pt-2 rssp)
                           (modification ?pt-2 cercle)
                           ;COMBIEN --> dominant hand
                           (left-hand-articulation ?combien-1 combien)
                           ;METRE-CARRE --> dominant hand
                           (left-hand-articulation ?metre-carre-1 metre-carre)
                           ;FBUOY-GRAND-ENTITE-GEOGRAPHIQUE --> non dominant hand, location = midssp
                           (right-hand-articulation ?fbuoy-grand-entite-geographique-1 fbuoy-grand-entite-geographique)
                           (location ?fbuoy-grand-entite-geographique-1 rssp)
                           ;MEETS
                           (meets ?pt-1 ?slot1-lh-left)
                           (meets ?slot1-lh-right ?dss-grand-entite-geographique-1)
                           (meets ?dss-grand-entite-geographique-1 ?pt-2)
                           (meets ?pt-2 ?combien-1)
                           (meets ?combien-1 ?metre-carre-1)
                           ;FBUOY temporal relations
                           (temporal-relation ?pt-2 ?fbuoy-grand-entite-geographique-1 equals))))
              (?slot1-unit
               (meaning-args ((target ?F)))
               --
               (category answer-size-const-cityid-slot1-1-slot1-cat)
               (boundaries (lh-leftmost ?slot1-lh-left)
                           (lh-rightmost ?slot1-lh-right)))))

(def-fcg-cxn answer-elevation-const-placeid-slot1-1-cxn
             ((?answer-elevation-const-placeid-slot1-1-unit
               (subunits (?slot1-unit)))
              <-
              (?answer-elevation-const-placeid-slot1-1-unit
               (HASH meaning ((ANSWER ?C ?A ?D)(ELEVATION ?D ?B ?A)(CONST ?D ?B ?E)(PLACEID ?E ?F)))
               --
               (HASH form (;MONTAGNE --> two-handed
                           (left-hand-articulation ?montagne-1 montagne)
                           (right-hand-articulation ?montagne-2 montagne)
                           (temporal-relation ?montagne-1 ?montagne-2 equals)
                           ;PT --> dominant hand, location = right
                           (left-hand-articulation ?pt-1 pt)
                           (location ?pt-1 rssp)
                           ;PT --> dominant hand, location = right, modification = down-up
                           (left-hand-articulation ?pt-2 pt)
                           (location ?pt-2 rssp)
                           (modification ?pt-2 down-up)
                           ;PT --> dominant hand, location = right, modification = down-up
                           (left-hand-articulation ?pt-3 pt)
                           (location ?pt-3 rssp)
                           (modification ?pt-3 down-up)
                           ;COMBIEN --> dominant hand
                           (left-hand-articulation ?combien-1 combien)
                           ;METRE --> two-handed
                           (left-hand-articulation ?metre-1 metre)
                           (right-hand-articulation ?metre-2 metre)
                           (temporal-relation ?metre-1 ?metre-2 equals)
                           ;FBUOY-MONTAGE --> non-dominant hand
                           (right-hand-articulation ?fbuoy-montagne-1 fbuoy-montagne)
                           ;MEETS
                           (meets ?montagne-1 ?pt-1)
                           (meets ?pt-1 ?pt-2)
                           (meets ?pt-2 ?slot1-lh-left)
                           (meets ?slot1-lh-right ?pt-3)
                           (meets ?pt-3 ?combien-1)
                           (meets ?combien-1 ?metre-1)
                           ;FBUOY temporal relations
                           (temporal-relation ?pt-1 ?fbuoy-montagne-1 starts)
                           (temporal-relation ?pt-2 ?fbuoy-montagne-1 during)
                           (temporal-relation ?slot1-lh-left ?fbuoy-montagne-1 during)
                           (temporal-relation ?pt-3 ?fbuoy-montagne-1 finishes))))
              (?slot1-unit
               (meaning-args ((target ?F)))
               --
               (category answer-elevation-const-placeid-slot1-1-slot1-cat)
               (boundaries (lh-leftmost ?slot1-lh-left)
                           (lh-rightmost ?slot1-lh-right)))))
               

;----------------------------;
; + holistic constructions + ;
;----------------------------;

(def-fcg-cxn guadalupe_peak-1-cxn
             ((?guadalupe_peak-1-unit
               (meaning-args ((target ?A)))
               (boundaries (lh-leftmost ?fs-guadalupe-1)
                           (lh-rightmost ?fs-guadalupe-1))
               (category guadalupe_peak-1-cat))
              <-
              (?guadalupe_peak-1-unit
               (HASH meaning ((GUADALUPE_PEAK ?A)))
               --
               (HASH form ((left-hand-articulation ?fs-guadalupe-1 fs-guadalupe))))))

(def-fcg-cxn mount_mckinley-1-cxn
             ((?mount_mckinley-1-unit
               (meaning-args ((target ?A)))
               (boundaries (lh-leftmost ?fs-mckinley-1)
                           (lh-rightmost ?fs-mckinley-1))
               (category mount_mckinley-1-cat))
              <-
              (?mount_mckinley-1-unit
               (HASH meaning ((MOUNT_MCKINLEY ?A)))
               --
               (HASH form ((left-hand-articulation ?fs-mckinley-1 fs-mckinley))))))

(def-fcg-cxn death_valley-1-cxn
             ((?death_valley-1-unit
               (meaning-args ((target ?A)))
               (boundaries (lh-leftmost ?fs-death-valley-1)
                           (lh-rightmost ?fs-death-valley-1))
               (category death_valley-1-cat))
              <-
              (?death_valley-1-unit
               (HASH meaning ((DEATH_VALLEY ?A)))
               --
               (HASH form ((left-hand-articulation ?fs-death-valley-1 fs-death-valley))))))

(def-fcg-cxn new_york-1-cxn
             ((?new_york-1-unit
               (meaning-args ((target ?A)))
               (boundaries (lh-leftmost ?ns-new-york.y-loc-1)
                           (lh-rightmost ?ns-new-york.y-loc-1))
               (category new_york-1-cat))
              <-
              (?new_york-1-unit
               (HASH meaning ((NEW_YORK ?A)))
               --
               (HASH form ((left-hand-articulation ?ns-new-york.y-loc-1 ns-new-york.y-loc)
                           (right-hand-articulation ?ns-new-york.y-loc-2 ns-new-york.y-loc))))))

(def-fcg-cxn los_angeles-1-cxn
             ((?los_angeles-1-unit
               (meaning-args ((target ?A)))
               (boundaries (lh-leftmost ?ns-los-angeles-1)
                           (lh-rightmost ?ns-los-angeles-1))
               (category los_angeles-1-cat))
              <-
              (?los_angeles-1-unit
               (HASH meaning ((LOS_ANGELES ?A)))
               --
               (HASH form ((left-hand-articulation ?ns-los-angeles-1 ns-los-angeles))))))

(def-fcg-cxn chicago-1-cxn
             ((?chicago-1-unit
               (meaning-args ((target ?A)))
               (boundaries (lh-leftmost ?ns-chicago-1)
                           (lh-rightmost ?ns-chicago-1))
               (category chicago-1-cat))
              <-
              (?chicago-1-unit
               (HASH meaning ((CHICAGO ?A)))
               --
               (HASH form ((left-hand-articulation ?ns-chicago-1 ns-chicago))))))

(def-fcg-cxn houston-1-cxn
             ((?houston-1-unit
               (meaning-args ((target ?A)))
               (boundaries (lh-leftmost ?ns-houston-1)
                           (lh-rightmost ?ns-houston-1))
               (category houston-1-cat))
              <-
              (?houston-1-unit
               (HASH meaning ((HOUSTON ?A)))
               --
               (HASH form ((left-hand-articulation ?ns-houston-1 ns-houston))))))

(def-fcg-cxn phoenix-1-cxn
             ((?phoenix-1-unit
               (meaning-args ((target ?A)))
               (boundaries (lh-leftmost ?ns-phoenix-1)
                           (lh-rightmost ?ns-phoenix-1))
               (category phoenix-1-cat))
              <-
              (?phoenix-1-unit
               (HASH meaning ((PHOENIX ?A)))
               --
               (HASH form ((left-hand-articulation ?ns-phoenix-1 ns-phoenix))))))

(def-fcg-cxn philadelphia-1-cxn
             ((?philadelphia-1-unit
               (meaning-args ((target ?A)))
               (boundaries (lh-leftmost ?ns-philadelphia-1)
                           (lh-rightmost ?ns-philadelphia-1))
               (category philadelphia-1-cat))
              <-
              (?philadelphia-1-unit
               (HASH meaning ((PHILADELPHIA ?A)))
               --
               (HASH form ((left-hand-articulation ?ns-philadelphia-1 ns-philadelphia))))))


(def-fcg-cxn san_antonio-1-cxn
             ((?san_antonio-1-unit
               (meaning-args ((target ?A)))
               (boundaries (lh-leftmost ?ns-san-antonio-1)
                           (lh-rightmost ?ns-san-antonio-1))
               (category san_antonio-1-cat))
              <-
              (?san_antonio-1-unit
               (HASH meaning ((SAN_ANTONIO ?A)))
               --
               (HASH form ((left-hand-articulation ?ns-san-antonio-1 ns-san-antonio))))))


(def-fcg-cxn alaska-1-cxn
             ((?alaska-1-unit
               (meaning-args ((target ?A)))
               (boundaries (lh-leftmost ?fs-alaska-1)
                           (lh-rightmost ?fs-alaska-1))
               (category alaska-1-cat))
              <-
              (?alaska-1-unit
               (HASH meaning ((ALASKA ?A)))
               --
               (HASH form ((left-hand-articulation ?fs-alaska-1 fs-alaska))))))

(def-fcg-cxn california-1-cxn
             ((?california-1-unit
               (meaning-args ((target ?A)))
               (boundaries (lh-leftmost ?fs-california-1)
                           (lh-rightmost ?fs-california-1))
               (category california-1-cat))
              <-
              (?california-1-unit
               (HASH meaning ((CALIFORNIA ?A)))
               --
               (HASH form ((left-hand-articulation ?fs-california-1 fs-california))))))

(def-fcg-cxn florida-1-cxn
             ((?florida-1-unit
               (meaning-args ((target ?A)))
               (boundaries (lh-leftmost ?fs-florida-1)
                           (lh-rightmost ?fs-florida-1))
               (category florida-1-cat))
              <-
              (?florida-1-unit
               (HASH meaning ((FLORIDA ?A)))
               --
               (HASH form ((left-hand-articulation ?fs-florida-1 fs-florida))))))

(def-fcg-cxn massachusetts-1-cxn
             ((?massachusetts-1-unit
               (meaning-args ((target ?A)))
               (boundaries (lh-leftmost ?fs-massachusetts-1)
                           (lh-rightmost ?fs-massachusetts-1))
               (category massachusetts-1-cat))
              <-
              (?massachusetts-1-unit
               (HASH meaning ((MASSACHUSETTS ?A)))
               --
               (HASH form ((left-hand-articulation ?fs-massachusetts-1 fs-massachusetts))))))

(def-fcg-cxn new_mexico-cxn
             ((?new_mexico-1-unit
               (meaning-args ((target ?A)))
               (boundaries (lh-leftmost ?fs-new-mexico-1)
                           (lh-rightmost ?fs-new-mexico-1))
               (category new_mexico-1-cat))
              <-
              (?new_mexico-1-unit
               (HASH meaning ((NEW_MEXICO ?A)))
               --
               (HASH form ((left-hand-articulation ?fs-new-mexico-1 fs-new-mexico))))))

(def-fcg-cxn north_dakota-1-cxn
             ((?north_dakota-1-unit
               (meaning-args ((target ?A)))
               (boundaries (lh-leftmost ?fs-north-dakota-1)
                           (lh-rightmost ?fs-north-dakota-1))
               (category north_dakota-1-cat))
              <-
              (?north_dakota-1-unit
               (HASH meaning ((NORTH_DAKOTA ?A)))
               --
               (HASH form ((left-hand-articulation ?fs-north-dakota-1 fs-north-dakota))))))

(def-fcg-cxn texas-1-cxn
             ((?texas-1-unit
               (meaning-args ((target ?A)))
               (boundaries (lh-leftmost ?fs-texas-1)
                           (lh-rightmost ?fs-texas-1))
               (category texas-1-cat))
              <-
              (?texas-1-unit
               (HASH meaning ((TEXAS ?A)))
               --
               (HASH form ((left-hand-articulation ?fs-texas-1 fs-texas))))))

(def-fcg-cxn state-1-cxn
             ((?state-1-unit
               (meaning-args ((partial-network ?D)
                              (target ?A)))
               (boundaries (lh-leftmost ?dss-petit-entite-geographique-1)
                           (lh-rightmost ?dss-petit-entite-geographique-1)
                           (rh-leftmost ?fbuoy-grand-entite-geographique-1)
                           (rh-rightmost ?fbuoy-grand-entite-geographique-1))
               (category state-1-cat)
               (location ?location-1))
              
              <-
              (?state-1-unit
               (HASH meaning ((STATE ?D ?A)))
               --
               (HASH form (;DSS-PETIT-ENTITE-GEOGRAPHIQUE --> dominant hand, location = ?location-1, modification = reduplication
                           (left-hand-articulation ?dss-petit-entite-geographique-1 dss-petit-entite-geographique)
                           (location ?dss-petit-entite-geographique-1 ?location-1)
                           (modification ?dss-petit-entite-geographique-1 reduplicated)
                           ;FBUOY-GRAND-ENTITE-GEOGRAPHIQUE, location = ?location-1
                           (right-hand-articulation ?fbuoy-grand-entite-geographique-1 fbuoy-grand-entite-geographique)
                           (location ?fbuoy-grand-entite-geographique-1 ?location-1)
                           ;FBUOY temporal relation
                           (temporal-relation ?dss-petit-entite-geographique-1 ?fbuoy-grand-entite-geographique-1 ?temporal-relation-1))))))

(def-fcg-cxn capital-1-cxn
             ((?capital-1-unit
               (meaning-args ((partial-network ?D)
                              (target ?A)))
               (boundaries (lh-leftmost ?different-1)
                           (lh-rightmost ?pt-1)
                           (rh-leftmost ?different-2)
                           (rh-rightmost ?fbuoy-grand-entite-geographique-1))
               (category capital-1-cat)
               (location rssp))
              <-
              (?capital-1-unit
               (HASH meaning ((CAPITAL ?D ?A)))
               --
               (HASH form (;DIFFERENT --> two-handed
                           (left-hand-articulation ?different-1 different)
                           (right-hand-articulation ?different-2 different)
                           (temporal-relation ?different-1 ?different-2 equals)
                           ;CAPITALE.MID --> two-handed
                           (left-hand-articulation ?capitale.mid-1 capitale.mid)
                           (right-hand-articulation ?capitale.mid-2 capitale.mid)
                           (temporal-relation ?capitale.mid-1 ?capitale.mid-2 equals)
                           ;PT --> dominant hand, location = ?location-1, modification = reduplicated
                           (left-hand-articulation ?pt-1 pt)
                           (location ?pt-1 rssp)
                           (modification ?pt-1 reduplicated)
                           ;FBUOY-GRAND-ENTITE-GEOGRAPHIQUE, location = ?location-1
                           (right-hand-articulation ?fbuoy-grand-entite-geographique-1 fbuoy-grand-entite-geographique)
                           (location ?fbuoy-grand-entite-geographique-1 rssp)
                           ;MEETS
                           (meets ?different-1 ?capitale.mid-1)
                           (meets ?capitale.mid-1 ?pt-1)
                           ;FBUOY temporal relation
                           (temporal-relation ?pt-1 ?fbuoy-grand-entite-geographique-1 starts)
                           )))))

;------------------------;
; + categorial network + ;
;------------------------;

(add-categories '(answer-largest-slot1-1-slot1-cat
                  state-1-cat
                  capital-1-cat
                  answer-area-largest-slot1-1-slot1-cat
                  answer-population-largest-density-slot1-1-slot1-cat
                  answer-largest-state-slot1-2-slot1-cat
                  answer-size-const-stateid-slot1-1-slot1-cat
                  answer-size-const-cityid-slot1-1-slot1-cat
                  answer-elevation-const-placeid-slot1-1-slot1-cat
                  alaska-1-cat
                  california-1-cat
                  florida-1-cat
                  massachusetts-1-cat
                  new_mexico-1-cat
                  north_dakota-1-cat
                  texas-1-cat
                  new_york-1-cat
                  los_angeles-1-cat
                  chicago-1-cat
                  houston-1-cat
                  phoenix-1-cat
                  philadelphia-1-cat
                  san_antonio-1-cat
                  guadalupe_peak-1-cat
                  mount_mckinley-1-cat
                  death_valley-1-cat)
                *fcg-constructions*)

(progn
  (add-link 'state-1-cat 'answer-population-largest-density-slot1-1-slot1-cat *fcg-constructions*)
  (add-link 'state-1-cat 'answer-largest-state-slot1-2-slot1-cat *fcg-constructions*)
  (add-link 'state-1-cat 'answer-largest-slot1-1-slot1-cat *fcg-constructions*)
  (add-link 'capital-1-cat 'answer-largest-slot1-1-slot1-cat *fcg-constructions*)
  (add-link 'state-1-cat 'answer-area-largest-slot1-1-slot1-cat *fcg-constructions*)
  (add-link 'alaska-1-cat 'answer-size-const-stateid-slot1-1-slot1-cat *fcg-constructions*)
  (add-link 'california-1-cat 'answer-size-const-stateid-slot1-1-slot1-cat *fcg-constructions*)
  (add-link 'florida-1-cat 'answer-size-const-stateid-slot1-1-slot1-cat *fcg-constructions*)
  (add-link 'massachusetts-1-cat 'answer-size-const-stateid-slot1-1-slot1-cat *fcg-constructions*)
  (add-link 'new_mexico-1-cat 'answer-size-const-stateid-slot1-1-slot1-cat *fcg-constructions*)
  (add-link 'north_dakota-1-cat 'answer-size-const-stateid-slot1-1-slot1-cat *fcg-constructions*)
  (add-link 'texas-1-cat 'answer-size-const-stateid-slot1-1-slot1-cat *fcg-constructions*)
  (add-link 'new_york-1-cat 'answer-size-const-cityid-slot1-1-slot1-cat *fcg-constructions*)
  (add-link 'los_angeles-1-cat 'answer-size-const-cityid-slot1-1-slot1-cat *fcg-constructions*)
  (add-link 'chicago-1-cat 'answer-size-const-cityid-slot1-1-slot1-cat *fcg-constructions*)
  (add-link 'houston-1-cat 'answer-size-const-cityid-slot1-1-slot1-cat *fcg-constructions*)
  (add-link 'phoenix-1-cat 'answer-size-const-cityid-slot1-1-slot1-cat *fcg-constructions*)
  (add-link 'philadelphia-1-cat 'answer-size-const-cityid-slot1-1-slot1-cat *fcg-constructions*)
  (add-link 'san_antonio-1-cat 'answer-size-const-cityid-slot1-1-slot1-cat *fcg-constructions*)
  (add-link 'guadalupe_peak-1-cat 'answer-elevation-const-placeid-slot1-1-slot1-cat *fcg-constructions*)
  (add-link 'death_valley-1-cat 'answer-elevation-const-placeid-slot1-1-slot1-cat *fcg-constructions*)
  (add-link 'mount_mckinley-1-cat 'answer-elevation-const-placeid-slot1-1-slot1-cat *fcg-constructions*))
