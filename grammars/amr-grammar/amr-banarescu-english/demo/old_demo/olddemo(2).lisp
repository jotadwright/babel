(ql:quickload :amr-grammar)
(in-package :amr-grammar)

(create-static-html-page "AMR parsing with the Fluid Construction Grammar"
  (header)
  (amr-notation)
  (theproject)
  (np-constructions)
  (lists)
  (agentandpatient)
  (complicateclause)
  )

(defun header ()
  (clear-page)
  (deactivate-all-monitors) 
  (activate-monitor trace-fcg)
  (add-element
   '((h1) "Semantic parsing with Fluid Construction Grammar and Abstract Meaning Representation"))
  (add-element '((p) "This is a web demo that supplements the Research Thesis:"))
  (add-element
   '((p) ((i)"Galletti, Martina. (June 2019). "
          ((a :href 
              "?")
           "Semantic parsing with Fluid Construction Grammar and Abstract Meaning Representation"))))
  (add-element '((p) "This demonstration has the following parts:"))
  (add-element '((h3)  ((a :href "#amr-notation") "I. AMR notation formalism")))
  (add-element '((h3)  ((a :href "theproject") "II.  Goals of the project and Research Questions")))  
  (add-element '((h3)  ((a :href "#np-constructions") "III. A Simple Noun-Phrase-Construction")))
    (add-element '((h3)  ((a :href "#lists") "IV. Proper Names and Lists")))
       (add-element '((h3)  ((a :href "#complements") "V. Complements : time, location and manner."))) 
  (add-element '((h3)  ((a :href "#agentandpatient") "VI. Comprehending a Clause")))
  (add-element '((h3)  ((a :href "#complicateclause") "VII. More complicated clause: generalization of the constructions"))))


(defun amr-notation ()
 (add-element '((h2 :id "amr-notation") "I. Abstract Meaning Representation notation formalism"))
  (add-element '((p)"Abstract Meaning Representation is a semantic representation language introduced by Banarescu et al. in 2013. AMR is a graph-structured language : the logical meaning of the sentences is mapped into a single rooted,labelled, a-cycled and directed graph where different linguistic relations and phenomena are represented.
Since AMR unifies different semantic annotations is a combination of semantic role labeling, named entity recognition and co-reference resultion. 
The AMR formalism is straight-forward: it is based on PENMAN notation where the semantic concepts are the nodes of the graph and the edges represent the relations that bound the different nodes.
Every semantic concept in the graph is linked to a variable and it is represented by english words (ex: boy ?b), by the PropBank notation (ex: say-01 ?s)  or, in certain case, by special keywords (ex: amr-unknown ?a). The possible relations between the edges can be represented by Frame Arguments (ex: :arg0), general semantic relations (ex: :polarity), relations for quantities (ex: :quant) and for lists (ex: :op1).
It is important to highlight that a certain number of syntactic elements have no meaning. In fact, AMR does not represent any meaning for quantifiers, tense, number and aspect.")))

(defun theproject ()
 (add-element '((h2 :id "theproject") "II. Goals of the project and Research Questions "))
(add-element '((p) "The project is dedicated to investigate how Abstract Meaning Representation is well suited as a meaning representation for Fluid Construction Grammar and how it can perform compared to Frame Semantics.The project had a practical approach, since a construction grammar had been implemented using the Fluid Construction Grammar toolkit. The project had been divided into four phases. 
1) A number of annotated Abstract Meaning Representation sentences from existing corpora or Abstract Meaning Representation related publications needed to be selected as a development corpus during the internship.
2) The grips of the basics of the Fluid Construction Grammar needed to be acquired, following tutorial files and example grammars.
3) A Construction Grammar grammar covering the selected sentences needed to be implemented by a continuous evaluation cycle that made use of established Abstract Meaning Representation metrics.
4) Finally, a discussion of the results and a comparison with Frame Semantics needed to be done.")))

 
(defun np-constructions ()
  (add-element '((h2 :id "np-constructions") "III. Simple Noun-Phrase-Constructions"))
  (add-element '((p) "(The grammar implemented includes three different types of constructions. First of all, the lexicon is composed by 149 lexical and morphological constructions which invole only one unit (representing the different words in the sentence) and where in the comprehension lock there is a string in the input. Secondly, there are the meaning constructions which are the heart of the amr-fcg-grammar since they make the edges between the nodes in the graph; and finally, the phrasal constructions which do not add any meaning, butthey form basic units as noun phrase or verb phrase. They are not really important for the Abstract Meaning Representation since typically they comprehend elements that have no meaning in AMR, but they are important from a syntactical point of view, since they form units as noun-phrase, verb-phrase, compound-tense and light-verbs. We will start our demonstration of the grammar from these types of constructions, since they were the easier to implement having no meaning"))
  (add-element '((h3 :id "np-constructions") "III.I. A simple Phrasal Construction"))
  (add-element '((p) "The most easy example of phrasal construction is the noun-phrase-cxn where in the comprehension lock two units are combined: an '?article-unit' and a '?noun-unit'. The '?article-unit' must have the value article for its lex-cat and the '?noun-unit' must have the value nominal. The article unit must come before the unit asspecified by the 'meets' constraint. Articles have no meaning in AMR since AMR is focused to represent the logical meaning of a sentence instead of its syntactic structure."))
  (add-element (make-html (find-cxn 'noun-phrase-cxn *fcg-constructions*)))
  (add-element '((h3 :id "np-constructions") "III.II. A simple Meaning Construction"))
  (add-element '((p) "The meaning constructions are the hearth of the amr-fcg-grammar since they implement the edges between the different nodes. They can have as units phrasal constructions, lexical and morphological constructions or they can form basic units for phrasal constructions. For example, if we comprehend the sentence 'a taxable fund', six different constructions are involved : three lexical constructions (the-cxn, taxable-cxn and fund-cxn), one phrasal construction (noun-phrase-cxn) and one meaning construction (inverse-patient-nominal-cxn) which add the meaning :arg1-f ?nominal is ?adjective. The taxable-cxn and fund-cxn are bound together in the inverse-patient-nominal-cxn which has two subunits : a nominal-unit which must be a nominal and adjective-unit which must be an adjective,have a semantic-class possibility and a syn-function adjectival. The 'meets' constraint specifiy that the ?adjective-unit must preeceed the ?nominal-unit. The inverse-patient-cxn form itself a nominal which can be bound to the article thanks to the noun-phrase unit."))
  (comprehend "a taxable fund"))

(defun lists ()
  (add-element '((h2 :id "lists") "IV.Proper Names and Lists"))
  (add-element '((p) "AMR has a particular Meaning Representation for proper nouns. The operators list :op1 and :op2 can bound the meaning of the proper names to the semantic concept represented by the noun entity. The Fluid Construction Grammar toolkit can pretty well deal also with these particular types of meaning representations. Starting from a very simple sentence as Obama the president, the named-entity-title-unit-cxn enables to add tthe :op1 is implemented in the comprehension lock of Obama-cxn. For really short sentence as 'Obama the president' comprehending proper nouns is really easy, but for more complex sentence as 'Elsevier N.V. , the Dutch publishing group' representing the operator list can be really challenging. In fact, this sentence is represented in AMR as : '((GROUP G) (NAME N) (COUNTRY C) (NAME N2) (PUBLISH-01 P) (:NAME G N) (:MOD G C) (:ARG0-OF G P) (:OP1 N "Elsevier") (:OP2 N N.V.) (:NAME C N2) (:OP1 N2 "Netherlands"))' where both Netherlands and Country are not in the lexicon. Fluid Construction Grammar toolkit enables the linguist to deal also with these type of Meaning Representation since it is a quite flexible toolkit capable of making the linguist the master of his own grammar. It is important to notice how in the amr-fcg-grammar the same construction (named-entity-title) comprehending Obama the president comprehends as well Elsevier N.V. , the Dutch publishing group. I tried, in fact, to generalize whenever possible my constructions to avoid writing a specific construction for each sentence of my corpys"))
   (comprehend "Obama the President")
   (comprehend "Elsevier N.V. , the Dutch publishing group")) 

(defun agentandpatient ()
  (add-element '((h2 :id "agentandpatient") "VI. A simple clause"))
    (add-element '((p) "If comprehending nominal phrases is realtively easy, clause can be really challenging. In fact, the more semantic concepts there are in a sentence, the more complicated is its meaning representation and the more fcg-constructions are involved. Let's start with a simple transitive sentence : the soldier feared battle which has the following Abstract Meaning Representation : ((FEAR-01 F) (SOLDIER S) (BATTLE-01 B) (:ARG0 F S) (:ARG1 F B))). In this simple example, an agent-cxn and a patient-cxn will be needed to convey respectively the two meanings :arg0 of ?verb is ?nominal and the :arg1 of the ?verb is a ?nominal. The search process goes as follow : firstly, the nominal construction is bound to the lexical construction for soldier, then the new nominal is bound to the article 'the' thanks to the noun-phrase-cxn; at this point the search engine bound the nominal-cxn to a second noun battle-cxn; feared-cxn is bound as well to the verbal phrase construction forming a verb-unit. At this point the patient-cxn can apply; this construction adds the last necessary meaning. Another example of generalization is the comprehension of the two sentences 'the girl adjusted the machine' and 'the judge saw the explosion' where exactly the same constructions apply in the comprehension process"))
     (comprehend "the soldier feared battle")
     (comprehend "the girl adjusted the machine")
     (comprehend "the judge saw the explosion"))

(defun complicateclause ()
    (add-element '((h2 :id "complicateclause") "VII. More complicated clauses : generalization of the constructions"))
      (add-element '((h3 :id "complicateclause") "VII.I. Generalization is possible"))
    (add-element '((p) "It is important to highlight that the agent-cxn and the patient-cxn are really general with a purpose ; in fact, these constructions apply to many others different sentences in my corpus. For example, in the comprehension of the sentence where did the girl find the boy?, with AMR '((FIND-01 F) (GIRL G) (BOY B) (AMR-UNKNOWN A) (:ARG0 F G) (:ARG1 F B) (:LOCATION F A))), the same constructions agent-cxn and patient-cxn apply. The search process is the same as the precedent sentence, but the main-clause-unit created by the agent-unit is bound to the interrogative pronoun, the interrogative clause  and the auxiliary form to form a new unit : the interrogative-clause-location-unit. The same happen with 'the boy thinks the team won-t win where the agent-cxn is applied to both to the agent of the main clause and the subordinate clause and a new constrctuions subordinate-cxn combine the two agent-cxn building a new unit."))
         (comprehend "where did the girl find the boy ?")
         (comprehend "the boy thinks the team won't win")
(add-element '((h3 :id "complicateclause") "VII.II. Generalization is not possible"))
       (add-element '((p) " In other cases, generalizing a construction was not possible for different reasons. For example, the semantic relation :locations is used in three different sentences really similar : 'the marble in the jar', ' yesterday 's marble in the non-jar' and  with the AMR ((MARBLE M) (JAR J) (YESTERDAY Y) (:LOCATION M J) (:TIME M Y) (:POLARITY J -))), but writing only one constructions for these two meanings do not appear to be possible a cause of both the syntactic structure of the semantic concepts, but also for their position in the root-structure. In fact, if in the first sentence, the location is attributed to the interrogative pronoun where, in the second case it is a prepositional phrase"))
     (comprehend "yesterday 's marble in the non-jar"))
 