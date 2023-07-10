# Pattern Finding

## 1. Repairs

Repairs are applied in the **reverse order** as reported here. Repairs are **not yet** applied recursively.

### **1.1. Learn Holistic**

Source: `diagnostics-and-repairs/repair-nothing-to-holophrase-cxn.lisp`

Learn a holistic construction of the entire form and meaning of the observation. These constructions have no `form-args` or `meaning-args`.

### **1.2. Anti-unify cxns**

Source: `diagnostics-and-repairs/repair-anti-unify-cxns.lisp`

Learn new constructions by anti-unifying the observation with existing constructions. This repair has the following steps:

1. Obtain candidate cxns for anti-unification by selecting all cxns that have any of the words of the observation as their hash key (`nil` hash included).

2. Filter the candidate cxns by only selecting routine cxns that have a positive score.

3. Find the least general generalisation by anti-unifying all candidate cxns with the observation. For every cxn, keep all valid anti-unification results on both form side and meaning side. Make all valid combinations of these anti-unifcation results. Take the anti-unification result with the lowest summed cost (cost on form side + cost on meaning side). As a tie breaker, take the anti-unification result where the cxn that was used for anti-unification has the highest score. See Section 2 for more details on the anti-unification process.

4. Compute `form-args` and `meaning-args` from the anti-unification result. See Section 3 for more details on this process.

5. Learn an item-based cxn from the generalisation on the form side and the meaning side. This item-based cxn will always have a single slot, where the grammatical categories of the cxns of the source delta and pattern delta will fit into. 

6. Learn a holistic cxn from the source delta. This is the part of the observation that does not fit in the generalisation.

7. Learn a holistic **or** an item-based cxn from the pattern delta. This is the part of the cxn inventory that does not fit in the generalisation. If anti-unified with a holistic cxn, learn a holistic cxn. If anti-unified with an item-based cxn, learn an item-based cxn with an equal amount of slots.

8. Add grammatical categories and links to the categorial network. The cxns learned from both delta's provide grammatical categories that are suitable fillers for the slot of the cxn learned from the generalisation. Furthermore, if an item-based cxn was learned for the pattern delta, add links between its slot(s) and the filler(s) of the item-based cxn that was anti-unified with. This allows the new cxns to cover the same observations as were supported by the cxn inventory before.

**Questions:**

 - The anti-unification process (step 3) includes a maximum cost (set to 10). Anti-unification results on either the form side or the meaning side that have a cost that exceed 10 are rejected. Do we want to keep this cost?

### **1.3. Anti-unify cipns**

Source: `diagnostics-and-repairs/repair-anti-unify-cipn.lisp`

Learn new constructions that combine with existing construction by anti-unifying the observation with a cip-node. This repair has the following steps:

1. Obtain partial analyses.

    1.1 Run comprehension with routine cxns (holistic-apply-first + item-based-apply-last). CIP nodes are accepted when (i) the resulting meaning is non-nil, (ii) there are form predicates left in the root, and (iii) the resulting meaning is compatible (`irl:embedding`) with the gold standard.

    1.2 Run comprehension with meta cxns (holistic-apply-last + item-based-apply-first). CIP nodes are accepted when (i) the resulting meaning is non-nil, (ii) there are form predicates left in the root, (iii) the resulting meaning is compatible (`irl:embedding`) with the gold standard, and (iv) the units in the transient structure are connected. 

 2. Find the least general generalisation by anti-unifying all candidate CIP nodes with the observation. For every CIP node, keep all valid anti-unification results on both form side and meaning side. Make all valid combinations of these anti-unification results. Take the anti-unification result with the lowest summed cost (cost on form side + cost on meaning side). As a tie breaker, take the anti-unification result where the average of the score of the applied cxns in the CIP node is highest. See Section 2 for more details on the anti-unification process.

 3. Learn cxns _from the source delta only_ depending on the partial analysis. 
 
     3.1 If the partial analysis is a holistic chunk (obtained by the application of holistic cxns or item-based cxns with holistic cxn(s) filling their slots), then learn an item-based cxn with as many slots as there are connected structures in the transient structure. 

     3.2 If the partial analysis is a pattern with open slot(s) (obtained by the application of at least one item-based cxn where there is no holistic cxn to fill the slot), then learn as many holistic cxns as there units without form and meaning in the transient structure. 

 4. Add grammatical categories and links to the categorial network connected the cxns of the partial analysis to the newly created cxns. To extract these links, the applied cxns of the CIP node and the newly created cxns are combined and comprehension is ran in a sandbox cxn inventory. From this cxn application result, the links are extracted. 

**Questions:**

 - When anti-unifying with cxns, we want as much as possible in the generalisation, and thus as little as possible in the delta's, and thus the lowest cost. Is this also true for partial analysis, where we are only learning cxns from the source delta?
 - Related, do we want a maximum cost when anti-unifying the observation with CIP nodes?
 - When are CIP nodes compatible, especially in the case of meta-cxns? The `connected-structure` was added because it occurs that multiple item-based cxns (apply-first variant) could apply, leaving some form predicates in the root. However, even after learning cxns for the open slot(s), the transient structure (and thus the meaning) was not fully connected. Moreover, not all form predicates from the root where consumed. Do we want to learn multiple holistic cxns (for filling open slots) and an item-based cxn (for connecting the structure) in one go?


### **1.4. Add Categorial Links**

Source: `diagnostics-and-repairs/repair-add-categorial-links.lisp`

Add categorial links between grammatical categories of constructions that already cover the observation. 

1. Run comprehension without categorial links.

2. Reject incompatible solutions. Accept CIP nodes when (i) it is succeeded, (ii) it has more than 1 applied cxn, and (iii) its meaning is equivalent to the gold standard. If multiple such CIP nodes exist, take the first one.

3. Extract categorial links from the solution node and add them to the categorial network.

## 2. Anti-Unification

Source: `diagnostics-and-repairs/anti-unify-utils.lisp`

### 2.1 Anti-unifying sets of predicates

_The following explanation holds for both the meaning side and for the form side when using `string` and `meets` predicates._

To support both Repair 1.2 and 1.3, the observation (the source) can be anti-unified with either cxns or CIP nodes (the pattern). Before running the anti-unification algorithm, special predicates called `top-arg` and `slot-arg` are added to the set of predicates of the pattern! These `top-arg` and `slot-arg` predicates represent `args` that can be found in the pattern. The `top-arg` and `slot-arg` predicates always have the same structure: `(<predicate-name> <variable> <lex-class>)`. The variable used in the `top-arg` and `slot-arg` predicate also occurs in the pattern, they are thus connected to the predicates of the pattern. Notice that the `lex-class` (a constant!) of the unit of where the `arg` was found is added to the predicate! Including these additional predicates in the anti-unification process makes it easier to build cxns from the anti-unification result later on. 

**Extracting top-arg/slot-arg predicates from cxns.** Top args are args on the contributing side of the cxn. Slot args are args from units that represent slots on the conditional side of the cxn. Holophrase cxns have neither top-args nor slot-args, holistic cxns have only top-args and item-based cxns always have slot-args and can have top-args. 

**Using top-arg/slot-arg predicates in anti-unify-cxns repair.** When anti-unifying the observation with an item-based cxn, the args of the units representing slots in the item-based cxn will end up as `slot-arg` predicates in the pattern delta. To construct the item-based cxn from the pattern delta, it suffices to take the variables in these `slot-arg` predicates and use them as args in the units that represent the slots. Moreover, given that the lex class is part of the `slot-arg` predicates, we immediately know how many slots the new item-based cxn should have (as many as there are unique lex classes in all `slot-arg` predicates) and which variables should go in which units.

**Extracting top-arg/slot-arg predicates from CIP nodes.** Top args are args from units in the transient structure that represent unfilled slots. They can be detected as units that have the footprint `used-as-slot-filler`, but have no `form` feature and no `meaning` feature. Slot args are args from units that are at the top level in the transient structure. They can be detected as units that do not have the footprint `used-as-slot-filler`. 

**Using top-arg predicates to learn holistic cxns from CIP nodes.** Given that `top-args` from a CIP node represent unfilled slots, these args can be used to learn holistic cxns to fill those slots. The `top-arg` predicates will end up in the pattern delta of the anti-unification result. First, they are copied to the source-delta. While copying, the variables are 'translated' from the pattern delta to the source delta through the bindings lists. Given that the `top-arg` predicates contain the lex-class of the unit of the transient structure from which they were extracted, we immediately know how many open slots there are in the transient structure, and consequently, how many holistic cxns should be learned and which args should be provided by which holistic cxn. 

**Using slot-arg predicates to learn item-based cxns from CIP nodes.** Given that `slot-args` from a CIP node represent args of top-level units in the transient structure, these args can be used to learn an item-based cxn that takes these units as slots. The `slot-arg` predicates will end up in the pattern delta of the anti-unification result. First, they are copied to the source-delta. While copying, the variables are 'translated' from the pattern delta to the source delta through the bindings lists. Given that the `slot-arg` predicates contain the lex-class of the unit of the transient structure from which they were extracted, we immediately know how many top level units there are in the transient structure, and consequently, how many slot units should be present in the item-based cxn and which args should be provided by which slot unit. 

### 2.2 Anti-unifying sequences of characters

**TO DO**

## 3. Computing args

Source: `diagnostics-and-repairs/compute-args.lisp`

_The following explanation holds for computing the args in Repair 1.2. Repair 1.3 completely relies on the `top-args` and `slot-args` in the anti-unification result (see Section 2)._

From the anti-unification result, compute the sets of args that should go in (i) the slot of the item-based cxn learned from the generalisation, (ii) the contributing part of the cxn learned from the source delta, and (iii) the contributing part of the cxn learned from the pattern delta. These three sets should have the same length. Computing the args goes as follows:

- Loop over the variables from the bindings lists `P`, `G`, and `S`, with `P` being a variable from the pattern delta, `G` being the corresponding variable from the generalisation and `S` being the corresponding variable from the source delta.
- Whenever `P` occurs somehwere in the pattern delta **or** `S` occurs somewhere in the source delta **or** `P` is a `slot-arg` **or** `S` is a `slot-arg`; add `P` to the args on the contributing part of the pattern delta cxn, add `G` to the args of the slot unit of the generalisation cxn, and add `S` to the args on the contributing part of the source delta cxn.
- All variables that occur in `slot-arg` predicates are marked as slot args for the delta in which they occur (for learning an item-based cxn from the pattern delta). 

## 4. Alignment

Source: `experiment-setup/alignment.lisp`