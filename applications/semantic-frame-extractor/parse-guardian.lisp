;(ql:quickload :frame-extractor)
(in-package :frame-extractor)

;(activate-monitor trace-fcg)

(pie-comprehend "With the growing number of natural disasters due to climate change, the sums spent by governments on catastrophe management have risen to unprecedented levels.")

(pie-comprehend "The International Red Cross says more people are already being made refugees due to environmental issues.")

(pie-comprehend "The Lancet estimated that China suffers 1.2 million premature deaths due to fossil fuel pollution.")


;;############################
;;BECAUSE OF
;##############################
(evaluate-grammar-output-for-evoking-elem '("because of"))
;correct slots and total slots: (42 60): 0.7 
;correct sentences and total sentences: (8 20): 0.4 
;correct words and total words overall: (650 749): 0.8678238

;;resultaat sinds aanpassingen met CONLL de-renderer?
;correct slots and total slots: (43 78): 0.55128205 
;correct sentences and total sentences: (7 20): 0.35 
;correct words and total words overall: (574 749): 0.76635516 

(pie-comprehend "After seven hours' delay because of bad weather we got our first sight of the floating Arctic sea ice as we came in to land.")

;;Problem with dependency parser (partial parse - not due to period...):
(pie-comprehend "such a stupid construction. Hege Njaa Aschim, the Norwegian government's spokeswoman for the vault, said:  The construction was planned like that because it was practical as a way to go inside and it should not be a problem because of the permafrost keeping it safe." :strings-as-output t) ; (0 3) (slots) (33 46) (words)

;;Punctuation in effect slot, missing capitalisation of 'Reports'
(pie-comprehend "Reports that the issue is polarising the Warsaw talks appear to be overblown, mostly because of the much tweeted 'walk out'.") ; (2 3) (slots) (16 22) (words)

;;annotation longer effect
(pie-comprehend "The culprit is a mini cicada called a cicadelle which French lavender producers believe has proliferated because of hotter, drier summers, blamed on global warming.") ; (2 3) (slots) (18 25) (words)

"However, clean energy groups have warned that political uncertainty over the future of the RET is undermining investment in the sector, potentially compromising the 20% target. There's no reason why Australia won't hit its renewable target unless the RET is changed,  said Ric Brazzale, director of Green Energy Markets and author of the report. Even if the carbon price remains in place after the election, it will be low because of its link to the European market.": (0 3) (slots) (68 80) (words)

(pie-comprehend "It's a choice confronting more than 180 native communities in Alaska, which are flooding and losing land because of the ice melt that is part of the changing climate.") ; (2 3) (slots) (18 30) (words)

(pie-comprehend "It's easy enough to think of similar examples and certainly at least some groups would be doomed from such an event simply because of where they were.") (1 3) (slots) (14 28) (words)

"Current Coalition Direct Action climate policies are projected to result in increased greenhouse gas emissions, albeit at a level that might allow Australia to meet its 2020 targets because of accounting rules.": (2 3) (slots) (27 32) (words)

"But he said he would not be staying for the final vote \"because of weather constraints in Washington\".Lumumba Di-Aping, chief negotiator for the G77 group of 130 developing countries, was scathing: \"This deal will definitely result in massive devastation in Africa and small island states.": (2 3) (slots) (43 47) (words)

"This did result in the suspension of negotiations, but because of a dispute over a vital pillar of the climate regime not a newspaper story.": (1 3) (slots) (13 25) (words)

;;############################
;;GIVE RISE TO
;##############################
(evaluate-grammar-output-for-evoking-elem '("give rise"))
;;correct slots and total slots: (51 60): 0.85 
;;correct sentences and total sentences: (13 20): 0.65 
;;correct words and total words overall: (463 524): 0.8835878 

;;annotation = shorter effect (lawsuits)
(pie-comprehend "This could give rise to lawsuits in future, though Blood said he hoped that could be avoided, if the report's recommendations were followed.") ; (2 3) (slots) (22 24) (words)

;;correct!! but perhaps problem with capitalised 'The', hyphen in counter-revolution and round brackets?
(pie-comprehend "The emerging ecological revolution (and counter-revolution) also gives rise to new fears and social inequalities.") ; (2 3) (slots) (9 16) (words)

;;problem with Spacy for effect (comma)
(pie-comprehend "Sulphur and soot emissions, which give rise to lung cancers, acid rain and respiratory problems are expected to rise more than 30% over the next 12 years.") ; (2 3) (slots) (23 28) (words)
;; annotation = MUCH shorter (two papers)
(pie-comprehend "Work by the dendro-climatologists there gave rise to two papers Dr Keith Briffa and colleagues that convinced us that British/Irish oak was not a good subject for the reconstruction of instrumental-style temperature and rainfall records." :strings-as-output nil) ; (2 3) (slots) (9 37) (words)
;;annotation = longer (effect: that view with some vs. that view)
(pie-comprehend "Your links to China and North Korea give rise to that view with some.") ; (2 3) (slots) (12 14) (words)

;;Not the complete sentence is dependency parsed (until CLOUD)
(pie-comprehend "Jasper Kirkby, head of the CLOUD (Cosmics Leaving OUtdoor Droplets) experiment at Cern, the particle physics laboratory near Geneva, studied various gas mixtures of sulphuric acid, water and ammonia  the three gases thought to give rise to aerosol particles at the low altitudes where clouds form.") ; (0 3) (slots) (32 46) (words)

;(get-penelope-dependency-analysis "Jasper Kirkby, head of the CLOUD (Cosmics Leaving OUtdoor Droplets) experiment at Cern, the particle physics laboratory near Geneva, studied various gas mixtures of sulphuric acid, water and ammonia  the three gases thought to give rise to aerosol particles at the low altitudes where clouds form.")



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



