;(ql:quickload :frame-extractor)
(in-package :frame-extractor)

;(activate-monitor trace-fcg)


;;############################
;;GIVE RISE TO
;##############################
(evaluate-grammar-output-for-evoking-elem '("give rise"))
;;correct slots and total slots: (43 60): 0.71666664 
;;correct sentences and total sentences: (9 20): 0.45 
;;correct words and total words overall: (423 524): 0.80725194 

;;annotation = shorter effect (lawsuits)
(pie-comprehend "This could give rise to lawsuits in future, though Blood said he hoped that could be avoided, if the report's recommendations were followed.") ; (2 3) (slots) (22 24) (words)

(pie-comprehend "Italy's combination of sea coast, mountains, deep valleys and plains gives rise to a rich variety of food products but climate change could tip the balance, Mr Ferrante said.") ; (2 3) (slots) (27 30) (words)

"The emerging ecological revolution (and counter-revolution) also gives rise to new fears and social inequalities.": (2 3) (slots) (9 16) (words)

"Sulphur and soot emissions, which give rise to lung cancers, acid rain and respiratory problems are expected to rise more than 30% over the next 12 years.": (2 3) (slots) (23 28) (words)

"That level of migration is bound to give rise to severe social conflicts.": (2 3) (slots) (9 13) (words)

"Work by the dendro-climatologists there gave rise to two papers Dr Keith Briffa and colleagues that convinced us that British/Irish oak was not a good subject for the reconstruction of instrumental-style temperature and rainfall records.": (2 3) (slots) (9 37) (words)


;;############################
;;LEAD TO
;##############################
(evaluate-grammar-output-for-evoking-elem '("lead to"))
;;correct slots and total slots: (95 105): 0.9047619 
;;correct sentences and total sentences: (27 35): 0.7714286 
;;correct words and total words overall: (933 987): 0.9452888 

;;Incorrectly parsed sentences:
;;------------------------------

;;problem with subframe:
(pie-comprehend "Shell warned environmentalists and ethical investors yesterday that failure to exploit tar sands and other unconventional oil products would worsen climate change because it would lead to the world burning even more carbon-heavy coal.") ;;(0 3) (slots) (24 35) (words)

;;cause is no subject:
(pie-comprehend "The 1990 Clean Air Act Amendments match that ratio: $1 of investments led to $30 in benefits") ;; (2 3) (slots) (15 19) (words)

;;seems correct for lead to??
(pie-comprehend "#nojobsonadeadplanet https://t.co/bnFl197uVWCraig Kelly, the Liberal MP who chairs the Coalition's backbench energy committee, was also criticised after he claimed renewable energy would lead to people dying of cold because it was pushing up energy prices.") ; (2 3) (slots) (35 37) (words)

;;annotated effect also includes subclause:
(pie-comprehend "But soaring temperatures in the Arctic at the end of the world's hottest ever recorded year led to melting and heavy rain, when light snow should have been falling.") ; (2 3) (slots) (23 30) (words)

;; problem with capitalization in Dudley (??) and weird calculation of word accuracy
(pie-comprehend "However, for the first time in three years, shareholders will also decide on a binding new pay policy for 2017-20, which would lead to a 2.9m cut in Dudley's maximum payout.") ; (2 3) (slots) (25 33) (words)

;; annotation does not include subclause:
(pie-comprehend "Finally, extreme weather can lead to acute outbreaks of infectious disease while at the same time reducing access to health care.") ; (2 3) (slots) (11 21) (words)

;; annotation does not include subclause:
(pie-comprehend "Second, increased temperatures lead to more water vapor in the atmosphere, which results in heavier rain/snow events.") ;(2 3) (slots) (9 17) (words)

;; Spacy mistake:
(pie-comprehend "The IMF economists said in the past low oil prices were self-correcting, because they resulted in a lack of investment in new production capacity which led to supply failing to meet demand.") ;;(2 3) (slots) (29 33) (words)



