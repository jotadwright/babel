# Pattern Finding

## Repairs

Repairs are applied in the reverse order as reported here.

### **1. Nothing -> Holistic**

Make a holistic construction of whatever form and meaning is left. `form-args` and `meaning-args` for the contributing part of the construction can be passed along from the recursion of repairs.

### **2. Holistic -> Item-based**

Anti-unify the observation with holistic constructions and learn constructions from the result. 

  1. _Select constructions for anti-unification._ These constructions need to be hash-compatible with the observation, and they need to be holistic constructions of the routine set with a score greater than zero.
  2. _Anti-unify the observation with the selected constructions._ On both the meaning side and the form side, only the anti-unification result with the lowest cost is considered. Anti-unification results with a cost greater than `:max-au-cost` (now set to 10) are removed. Collect all anti-unification results, one for each construction. Return the anti-unification result with the lowest summed cost (cost of form AU + cost of meaning AU). 
  3. _Compute args for constructions._ From the anti-unification results, this function computes `form-args` and `meaning-args` for the contributing part of the construction(s) that can be learned from the delta's (steps 4 and 5) and for the slot-unit on the conditional part of the item-based construction that can be learned (step 6), as all of these args need to correspond to each other.
  4. _Learn construction(s) from the source delta._ If the source delta is empty, this means we are in a deletion case. Learn a holistic construction for the entire observation. Otherwise, apply all repairs recursively. Pass along the args that were computed in step 3.
  5. _Learn construction(s) from the pattern delta._ If the pattern delta is empty, do nothing. Otherwise, apply all repairs recursively. Pass along the args that were computed in step 3.
  6. _Learn an item-based construction from the generalisation._ In this repair, item-based constructions always have a single slot that can be filled by whatever is learned in steps 4 and 5. Form and meaning are tied together through the `form-args` and `meaning-args` that are computed in step 3. Grammatical categories are generated such that they reflect the fact that there is only a single slot, e.g. the `what-?x-is-the-?y-?z-cxn` has a grammatical category `what-?x-is-the-?y-?z-(xyz)` for its slot.

This repair covers the holistic->item-based substitution, addition and deletion cases from before. 

**Questions:**

  - Always learn a construction with a single (possible noncontiguous) slot? Or learn a construction with as many slots as there are noncontiuguous parts? For example, should the `what-?x-is-the-?y-?z-cxn` have one slot (`xyz`) or two slots (`x` and `yz`)? **NO**, how to assign meaning to each slot??
  - Construction like the `the-?y-?x-object-is-what-shape-cxn` can have two adjacent slots because the anti-unification places all `meets` in the generalisation. This entails that there are always exactly two fillers. Would it be beneficial to check for this and remove adjacent slots, such that we immediately learn the `the-?x-object-is-what-shape-cxn` where the `?x` slot can be filled by a variable length filler? Or will this construction be learned later on, e.g. using the item-based->item-based repair? **NO**, this would then also need to be enforced on the meaning side, i.e. decoupling variables as well. How to know which variables??
  - Now constructions use `form-args`. Switch to `boundaries`? What are the downsides or benefits? Using `boundaries` corresponds to the current progress in form generalisation (Veronica), but `form-args` maybe corresponds better to the noncontiguous nature of the slots now?

### **3. Holistic Partial Analysis**

Some construction(s) could apply. Anti-unify the observation with the partial analysis and learn an item-based construction from the result.

  1. _Obtain the partial analysis._ Run comprehension with categorial links disabled and only constructions from the routine set. Select the cip node which covers as much as possible of the form of the observation. This can be one or more holistic constructions, or an item-based construction with all its slots already filled in, or a combination of these two cases.
  2. _Anti-unify the observation with the partial analysis._ On both the form side and the meaning side, only the anti-unification result with the lowest cost is considered. Here, there is no filter in terms of cost. When anti-unifying the observation with a partial analysis, the pattern delta is empty and the generalisation is identical to the partial analysis. A construction can thus be learned from the source delta. 
  3. _Learn an item-based construction._ First, top-level units from the transient structure are extracted. The item-based construction will have a slot for every top-level unit in the TS. `form-args` and `meaning-args` are extracted from each unit in the TS and the corresponding variables for the `form-args` and `meaning-args` of the conditional units in the item-based construction are recovered via the bindings lists. The grammatical categories of the slots reflect which placeholders are filled by the slot, e.g. when the partial analysis of `"What color is the large cube?"`consists of the `color-cxn` and the `large-cube-cxn`, the `what-?x-is-the-?y-?z-cxn` will have the `what-?x-is-the-?y-?z-(x)` slot and the `what-?x-is-the-?y-?z-(yz)` slot.
  4. _Extract categorial links._ The newly created item-based construction and the applied constructions of the partial analysis are added to a sandbox construction inventory and comprehension is ran. From the result, the categorial links are extracted. This is because it is difficult/expensive/impossible to know from the units in the TS which construction(s) created these units and how to make links between these constructions and the slots of the newly created item-based construction.

This repair is identical to the previous version.

**Questions:**

 - On the form side, no need to anti-unify in step 2? Just take whatever is left in the root of the cipn from step 1 and determine the `form-args` by taking the unconnected variables? **However**, can I still translate the `form-args` from the TS to the `form-args` for the item-based cxn?
 - Why run comprehension again to obtain the partial analysis? Why not trigger the diagnostic in the leaf node? **Because** the comprehension in step 1 is ran without categorial links; this leads to more results than "regular" comprehension.

### **4. Item-based Partial Analysis**

Some construction(s) could apply. Anti-unify the observation with the partial analysis and learn holistic construction(s) from the result.

  1. _Obtain the partial analysis._ Run comprehension with categorial links disabled and only constructions from the meta set. Select the cip node which covers most of the observation. This can be one or more item-based construction, or an item-based construction with some (but not all) slots already filled in, or a combination of these two cases.
  2. _Anti-unify the observation with the partial analysis._ On both the form side and the meaning side, only the anti-unification result with the lowest cost is considered. Here, there is no filter in terms of cost. When anti-unifying the observation with a partial analysis, the pattern delta is empty and the generalisation is identical to the partial analysis. Construction(s) can thus be learned from the source delta.  
  3. _Check for ambiguity._ The units in the transient structure are checked to see if there is only one open slot left. A holistic construction will be learned to fill this slot. Compatibility of the slot is checked by comparing the `form-args` and `meaning-args` of the unit in the TS with the `form-args` and the `meaning-args` as computed from the anti-unification result. The latter can be retrieved from the former through the bindings lists. When compatible, the `form-args` and the `meaning-args` for the holistic construction are immediately placed in the order that corresponds to the unit in the TS.
  4. _Learn construction(s) that fill the slot._ The repairs are applied recursively to the form and meaning in the source delta's. The resulting construction(s) will fill the open slot. 
  5. _Extract categorial links._ The newly created construction(s) and the applied constructions of the partial analysis are added to a sandbox construction inventory and comprehension is ran. From the result, the categorial links are extracted. This is because it is difficult/expensive/impossible to know from the units in the TS which construction(s) created these units and how to make links between the slots of constructions and the the newly created construction(s).

This repair extends the previous partial analysis repair in that the remaining meaning should not necessarily be connected and the remaining form should not necessarily be contiguous in terms of meets constraints. 

**Questions:**

  - Can this repair be generalised further to learn multiple holistic constructions for multiple open slots? The assignment of form and meaning to the slots might happen via the bindings lists of the anti-unification results.
  - On the form side, no need to anti-unify? Just take whatever is left in the root and determine the form-args by taking the unconnected variables?

### **5. Item-based -> Item-Based**

Anti-unify the observation with item-based constructions and learn constructions from the result.

  1. _Apply the holistic partial analysis repair._ In other words, check if an item-based construction can be learned from the partial analysis of the observation. 
  2. _Select constructions for anti-unification._ These constructions need to be hash-compatible with the item-based construction of step 1, and they need to be item-based constructions of the routine set with a score greater than zero.
  3. _Anti-unify the item-based construction of step 1 with the selected constructions of step 2._ On both the meaning side and the form side, only the anti-unification result with the lowest cost is considered. Anti-unification results with a cost greater than `:max-au-cost` (now set to 10) are removed. Collect all anti-unification results, one for each construction. Return the anti-unification result with the lowest summed cost (cost of form AU + cost of meaning AU). 
  4. _Compute args for constructions._ From the anti-unification results, this function computes `form-args` and `meaning-args` for the contributing part of the construction(s) that can be learned from the delta's (steps 5 and 6) and for the slot-unit on the conditional part of the item-based construction that can be learned (step 6), as all of these args need to correspond to each other.
  5. _Learn construction(s) from the source delta._ Apply all repairs recursively. Pass along the args that were computed in step 4.
  5. _Learn construction(s) from the pattern delta._ Apply all repairs recursively. Pass along the args that were computed in step 4.
  6. _Learn an item-based construction._ Create a sandbox construction inventory with the constructions that could apply in the partial analysis of step 1 and the constructions that where learned in step 5. In this sandbox construction inventory, again apply the holistic partial analysis repair. This will yield an item-based construction that is a generalisation over the item-based construction that could be learned in step 1 and the item-based construction that was selected for anti-unification in step 2. 

**Example from Jonas:**

  - Construction inventory contains the `big-cxn` and the `how-?X-is-Washington-cxn`
  - Observation is `"How big is Alaska?"`
  - Holistic partial analysis repair yields a `how-?X-is-Alaska-cxn`
  - The `how-?X-is-Alaska-cxn` is anti-unified with the `how-?X-is-Washington-cxn`
  - From the delta's, the `Alaska-cxn` and the `Washington-cxn` are learned
  - Holistic partial analysis repair now yields `how-?X-is-?Y-cxn`

**Questions:**

  - Extend to additions and deletions? See holistic->item-based repair on how to handle empty delta's in the anti-unification results. 
  - Is the computation of the args in step 4 correct? Does it provide args for only the new slot, or does it also include variables for the already existing slots in the item-based constructions that are being anti-unified with each other?
  - Is it necessary to run the holistic partial analysis repair again to obtain the final item-based construction? Or can it be derived from the anti-unification results? Maybe it becomes to difficult to make the units for the slots?
  - **!!!** This repair should be deleted and the holistic->item-based (with anti-unification) should be generalised to also anti-unify over item-based constructions. It can be renamed to the anti-unification repair and ran after the partial analysis repairs have been tried. To get the same effect as the item-based->item-based repair, also go in the recursion with the item-based part of the partial analysis repairs?

### **6. Add Categorial Links**

Add categorial links between grammatical categories of constructions that already cover the observation. 

  1. _Run comprehension without categorial links._ 
  2. _Reject incompatible solution._ Remove solution nodes where the top level `form-args` and `meaning-args` in the transient structure do not correspond to the unconnected variables of the observed form and meaning. 
  3. _Extract categorial links._ From the solution node, extract categorial links.

**Questions:**

  - Step 2 is now only done on the meaning side. Should also be done on the form side. 

## Computing args


## Alignment