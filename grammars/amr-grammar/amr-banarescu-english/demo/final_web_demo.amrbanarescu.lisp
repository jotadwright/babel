(ql:quickload :amr-grammar)
(in-package :amr-grammar)


(defun header ()
  (clear-page)
  (deactivate-all-monitors)
  (activate-monitor trace-fcg)
  (add-element
   '((h1) "Semantic parsing with Fluid Construction Grammar and Abstract Meaning Representation"))
  (add-element '((p) "This web demo supplements the research internship:"))
  (add-element
   '((p) "Galletti, Martina. (2019). " ((i) "Semantic parsing with Fluid Construction Grammar and Abstract Meaning Representation. ") "Supervisors: Katrien Beuls &amp; Paul Van Eecke."))
  (add-element '((p) "This demonstration has the following parts:"))
  (add-element '((h3)  ((a :href "intro") "I. Background information")))
  (add-element '((h3)  ((a :href "#thegrammar") "II. The Grammar Implemented")))
  (add-element '((h3)  ((a :href "#somecases") "III. Some more complex examples")))
  (add-element '((h3)  ((a :href "#generalization") "IV. Is the generalization of the constructions always possible?")))
  (add-element '((h3)  ((a :href "#discussionoftheresults") "V. Conclusions and further research")))
  (add-element '((h3)  ((a :href "#references") "VI. References"))))


(defun intro ()
 (add-element '((h2 :id "intro") "I. Background information"))
 
 (add-element '((h3 :id "intro") "I.I Goals of the project and Research Questions"))
 (add-element '((p) "The project was dedicated to investigate how Abstract Meaning Representation is well suited as a meaning representation for Fluid Construction Grammar and how it can perform compared to Frame Semantics. It had a practical approach, since a construction grammar had been implemented using the Fluid Construction Grammar toolkit with the goal of answering the following questions :"))
 (add-element '((ol)
                ((li) "To what extent Abstract Meaning Representation can be considered a good meaning representation for Computational Construction grammar? Is Computational Construction Grammar well suited for parsing AMR annotated sentences?")
                ((li) "Which conflicts which can arise between the Fluid Construction Grammar Toolkit and Abstract Meaning Representation? Which are the advantages and disadvantages of using AMR as a meaning representation for Fluid Construction Grammar?")))
 (add-element '((p) "The project had been divided into four tasks. The first task was to acquire the grips of the basics of the Fluid Construction Grammar and AMR, following tutorial files and example grammars. Secondly, a corpus of seventy-five annotated Abstract Meaning Representation sentences had been selected as a development corpus. Thirdly, a Construction Grammar covering the selected sentences had to be implemented by a continuous evaluation cycle that made use of established Abstract Meaning Representation metrics. Finally, a discussion of the results and a comparison with Frame Semantics needed to be written"))

 (add-element '((h3 :id "intro") "I.II. Abstract Meaning Representation notation formalism"))
 (add-element '((p)"Semantic Parsing is the task of representing the meaning of a sentence in a formal language which is understandable by a computer (1). There are multiple ways of parsing the meaning of a sentence as Frame Semantics or First Order Logic. In 2013, a team of ten researchers (2) invented a new semantic representation language: Abstract Meaning Representation with the purpose 'to support natural language generation (NLG) and to provide a logical semantic input'(3).
In fact, AMR is a graph-structured language where the logical meaning of the sentences is mapped into a single rooted, labelled, a-cycled and directed graph.
The AMR formalism is quite straight-forward: it is based on PENMAN notation where the semantic concepts are the nodes of the graph and the edges represent the relations that bound the different nodes. Every semantic concept in the graph is linked to a variable and it can be represented by English words (ex: boy ?b), by PropBank notation (ex: say-01 ?s)  or, in certain case, by special keywords (ex: amr-unknown ?a). The possible relations between the edges can be represented by Frame Arguments (ex: :arg0), general semantic relations (ex: :polarity), relations for quantities (ex: :quant), for time (ex: :time) and for lists (ex: :op1).
There are three fundamental characteristics that distinguish Abstract Meaning Representation from the others meaning representations. First of all, Abstract Meaning Representation  'it is geared toward English and the vocabulary of English'(4), even if some efforts had been made towards other languages (5). Secondly, in Abstract Meaning Representation 'there are no nouns and verbs'(6), since both of these syntactic categories are treated as the same by AMR. Finally, a certain number of syntactic elements have no meaning in AMR. In fact, AMR does not represent any meaning for quantifiers, tense, number and aspect.")))

(defun thegrammar ()
  (add-element '((h2 :id "thegrammar") "II. An Overview of the Amr-Grammar Implemented"))
  
  (add-element '((h3 :id "thegrammar") "II.I. A high level decription of the Amr-Grammar"))
  (add-element '((p) "The Amr-Grammar implemented includes three different types of constructions: lexical and morphological constructions, form-only constructions and grammatical constructions. In the first category, there are 149 lexical and morphological constructions which involve only one unit and which represent the different words of the sentences. The features that I chose to design for this first type of constructions are the followings : (referent ?ref), (meaning ((ref ?r ))), (syn-cat (lex-class ?lex-class) (syn-function ?function)) and (HASH form ((string ?ref-unit 'ref'))). The verbs have some additional Boolean features which specify their characteristics (ex: (passive +) or (aux +)). It's important to notice that in any of my constructions, I tried to write the minimal number of features possible with the goal to avoiding confusion within the Amr-Grammar.  
The second category is composed by the form-only constructions which do not add any AMR meaning, since they typically comprehend elements that have no meaning in AMR (ex: articles or auxiliaries). Even if they are not important from a semantic point of view, I believed they were important from a syntactical point of view, since they form units as noun-phrase, verb-phrase, compound-tense and light-verbs.
Finally, the third and last category is composed by the Grammatical Constructions: they are the heart of the Amr-Grammar since they actually make the edges between the nodes in the AMR graphs."))
  
  (add-element '((h3 :id "thegrammar") "II.II. A Simple Form-Only Construction"))
  (add-element '((p) "The easiest example of phrasal construction is the noun-phrase-cxn. As we can see in the comprehension lock, two units are combined: an '?article-unit' and a '?noun-unit'. The '?article-unit' must have the value article for the (lex-cat) and the '?noun-unit' must have the value nominal. It doesn't matter if the article-unit is definite or not since in both cases the meaning won't change in AMR. The ?article-unit must meet the unit as specified by the 'meets' constraint."))
  (add-element (make-html (find-cxn 'noun-phrase-cxn *fcg-constructions*)))
  
  (add-element '((h3 :id "thegrammar") "II.III. A Simple Grammatical Construction"))
  (add-element '((p) "The Form-Only Constructions interact with the Grammatical Constructions unceasingly. In fact, they can both be the super-units or minor units of the Grammatical Constructions. For example, if we comprehend the sentence 'a taxable fund', six different constructions are involved : three lexical constructions (the-cxn, taxable-cxn and fund-cxn), one phrasal construction (noun-phrase-cxn) and one meaning construction (inverse-patient-nominal-cxn) which add the meaning :arg1-f ?nominal is ?adjective. The taxable-cxn and fund-cxn are bound together in the inverse-patient-nominal-cxn; this construction, in fact, has two subunits : a nominal-unit which must have be (phrase-type nominal) and adjective-unit which must have a (lex-class adjective), a (sem-class possibility) and a (syn-function adjectival). The 'meets' constraint specify that the ?adjective-unit must 'meets' the leftmost part of the ?nominal-unit. The inverse-patient-cxn form another nominal-unit which can be bound to the article thanks to the noun-phrase unit."))
  (comprehend "a taxable fund"))

(defun somecases ()
  (add-element '((h2 :id "somecases") "III. Some more complex examples"))
  
  (add-element '((h3 :id "somecases") "III.I.Relations for Lists"))
  (add-element '((p) "AMR has a particular Meaning Representation for proper nouns, as the ':op1 and :op2' lists. The Fluid Construction Grammar toolkit can pretty well deal also with these particular types of meaning representations. In fact, for short nominal phrases as 'Obama the president' comprehending the AMR of proper nouns can be relatively easy and, even for more complex phrases as 'Elsevier N.V. , the Dutch publishing group', representing the operator list the comprehension process can be challenging, but still possible. More precisely, if we look at the comprehension process of the first one, we can see that the noun-phrase-cxn bounds the two units nominal-cxn and the-cxn; the :op1 meaning is added directly to the lexical construction for Obama-cxn and finally the named-entity-title-unit is created binding a nominal ('Obama-cxn') with (sem-class person) and a noun-phrase ('The President').
The AMR of the second sentence is : '((GROUP G) (NAME N) (COUNTRY C) (NAME N2) (PUBLISH-01 P) (:NAME G N) (:MOD G C) (:ARG0-OF G P) (:OP1 N "Elsevier") (:OP2 N N.V.) (:NAME C N2) (:OP1 N2 "Netherlands"))'. We can instantly notice that both 'Netherlands' and 'Country' are not words in the sentence, but this is not a problem for the Fluid Construction Grammar toolkit which enables the linguist to deal with these types of incosistencies, thanks to its flexibility. In fact, the two amr-meanings 'Country ?c' and ':op1 Netherlands' are added directly to the lexical construction for 'Dutch'. The presentative-unit and the named-entity-unit are created thanks to the presentative-cxn and the nv-elsevier-cxn. Finally, the Amr-Grammar applies the same construction that comprehended 'Obama the president' (the named-entity-title-cxn),to these two units adding the last meaning necessary to comprehend the sentence: name of a certain ?group is a certain ?name"))
  (comprehend "Obama the President")
  (comprehend "Elsevier N.V. , the Dutch publishing group")
  
(add-element '((h3 :id "somecases") "III.II. Comprehending a simple clause"))
(add-element '((p) "Fluid Construction Grammar permits not only to comprehend AMR annotated nominal phrases, but also clauses. Let's start with a simple transitive sentence : 'the soldier feared battle', which has the following Abstract Meaning Representation : ((FEAR-01 F) (SOLDIER S) (BATTLE-01 B) (:ARG0 F S) (:ARG1 F B))). In this simple example, an agent-cxn and a patient-cxn are needed to convey respectively the two meanings :arg0 of ?verb is ?nominal and the :arg1 of the ?verb is a ?nominal. The search process goes as follow : firstly, the nominal construction is bound to the lexical construction for soldier, then the new nominal is bound to the article 'the' thanks to the noun-phrase-cxn; at this point the search engine bound the nominal-cxn to a second noun battle-cxn; feared-cxn is bound as well to the verbal phrase construction forming a verb-unit. Finally, the patient-cxn can apply, adding the last necessary meaning. The sentence is then comprehended correctly."))
(comprehend "the soldier feared battle"))

(defun generalization ()
  (add-element '((h2)  ((a :href "#generalization") "IV. Is the generalization of the constructions always possible?")))
  (add-element '((h3 :id "generalization") "IV.I. The Generalization is possible"))
  (add-element '((p) "The agent-cxn and the patient-cxn had been implemented as general as possible since these constructions needed to apply to many others different sentences in my corpus. For example, in the comprehension of the sentence 'the boy thinks the team won't win', (with AMR ((THINK-01 T-1) (BOY B) (WIN-01 W) (TEAM T-2) (:ARG0 T-1 B) (:ARG1 T-1 W) (:ARG0 W T-2) (:POLARITY W -))) the agent-cxn applies two times, each for each clause and it forms two subunits required by the subordinate-positive-cxn which then coordinates the two sentences. Another example of generalization can be the comprehension process of the two sentences 'the girl adjusted the machine' and 'the judge saw the explosion' where exactly the same constructions apply in the comprehension process"))
     (comprehend "the girl adjusted the machine")
     (comprehend "the judge saw the explosion")
     (comprehend "the boy thinks the team won't win")
     
(add-element '((h3 :id "generalization") "IV.II. Specific constructions were needed"))
(add-element '((p) " In other cases, generalizing a construction was not possible; for example, two different constructions were needed for the nominal phrases 'atom bomb' and 'atomic bomb' which not only look pretty similar, but also have exactly the same amr-meaning; for comprehending these two phrases, two constructions, the pertainymy-nominal-nominal-cxn and the pertainymy-nominal-adjective-cxn had been implemented : in the first case, the construction bound a nominal with a (sem-role: pertainym) with another nominal, in the second case a nominal with an adjective that has (syn-cat (pertainym: +)). 
 An unique construction comprehending the ':location' amr-meaning was not possible as well. In fact the two sentences ('yesterday 's marble in the non-jar','where did the girl find the boy?') with this amr-meaning are really different both for the syntactic nature of their linguistic elements and for the complete different order present in the root-structure."))
(comprehend "atom bomb")
(comprehend "atomic bomb")
(comprehend "where did the girl find the boy ?")
(comprehend "yesterday 's marble in the non-jar"))

(defun discussionoftheresults ()
 (add-element '((h3)  ((a :href "#discussionoftheresults") "V. Conclusions and further research")))
 (add-element '((p) "In conclusion, AMR is a good meaning representation for Fluid Construction Grammar and I think that it can perform better than frame semantics. In fact, as an abstract meaning representation, AMR is not only more intuitive from a logical point of view, but also it's easier to implement, being more scalable. As far as I saw in my corpus, there are no particularly unavoidable conflicts between AMR and the FCG toolkit, FCG being a quite flexible framework. Moreover, computational construction grammar seems be sued for parsing Abstract Meaning Representation since it permits to adds extra meaning predicates also in the lexical and morphological constructions, as we saw with the :operators examples. A further and even more challenging project could be the implementation of a bidirectional Amr-Grammar using the FCG toolkit.")))

(defun references ()
  (add-element '((h3)  ((a :href "#references") "VI. References")))
  (add-element '((ul)
                 ((li) "(1) Speech and Language Processing: An Introduction to Natural Language Processing, Computational Linguistics, and Speech Recognition, Jurafsky, Daniel and Martin, James H., Draft 2018, https://web.stanford.edu/~jurafsky/slp3/")
                 ((li) "(2) Abstract Meaning Representation for Sembanking, Laura Banarescu, Claire Bonial, Shu Cai, Madalina Georgescu, Kira Griffitt, Ulf Hermjakob, Kevin Knight, Philipp Koehn, Martha Palmer, Nathan Schneider, Proceedings of the 7th Linguistic Annotation Workshop and Interoperability with Discourse, 2013,http://www.aclweb.org/anthology/W13-2322")
                 ((li) "(3) Ibidem")
                 ((li) "(4) Abstract Meaning Representation (AMR) 1.2 Specification, Laura Banarescu, Claire Bonial, Shu Cai, Madalina Georgescu, Kira Griffitt, Ulf Hermjakob, Kevin Knight, Philipp Koehn, Martha Palmer, Nathan Schneider, John Benjamins Publishing Company, Amsterdam, 2011")
                 ((li) "(5) For further references see : 'Annotating the Little Prince with Chinese AMRs', B. Li, Y. Wen, L. Bu, W. Qu, and N. Xue, Proc. 10th Linguistic Annotation Workshop (LAW X), 2016; 'An AMR parser for English, French, German, Spanish and Japanese and a new AMR-annotated corpus, Lucy Vanderwende, Arul Menezes and Chris Quirk, Proc. NAACL (Demo session), 2015; ;Cross-lingual Abstract Meaning Representation Parsing', Marco Damonte and Shay B. Cohen; 'A Study Towards Spanish Abstract Meaning Representation', Noelia Migueles-Abraira, MSc thesis, University of the Basque Country, 2017")
                 ((li) "(6) Abstract Meaning Representation (AMR) 1.2 Specification, Laura Banarescu, Claire Bonial, Shu Cai, Madalina Georgescu, Kira Griffitt, Ulf Hermjakob, Kevin Knight, Philipp Koehn, Martha Palmer, Nathan Schneider, John Benjamins Publishing Company, Amsterdam, 2011")
                 )))


(create-static-html-page "Semantic Parsing with Amr-Grammar"
  (header)
  (intro)
  (thegrammar)
  (somecases)
  (generalization)
  (discussionoftheresults)
  (references)
  )
