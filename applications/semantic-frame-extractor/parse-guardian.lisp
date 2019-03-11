;;This file is used for grammar development. It includes sentences
;;that do not work perfectly, together with comments.

;;For demonstration purposes, please use run-guardian.lisp instead !!!


;(ql:quickload :frame-extractor)

(in-package :frame-extractor)

;;Active the FCG web interface:
;(activate-monitor trace-fcg)

;;If you have run the CONLL evaluation before, please reset the de-render mode:
;(set-configuration *fcg-constructions* :de-render-mode :raw-dependency-translation)

;;############################
;;RESULT IN
;;############################
(pie-comprehend "The scientists ' letter cites peer - reviewed research over several years , some commissioned by the European Commission , which show that displaced human activity caused by converting forests and grasslands to biofuels production can result in 'substantial CO2' emissions .")


;;############################
;;CAUSE
;;############################
(pie-comprehend "The ozone hole causes an increase in westerly winds which, by a complex interaction of wind, sea and ice, results in lower temperatures in the east.")

(pie-comprehend "The unseemly rush into CSG resulted in substantial processing overcapacity , with economic pressure increasing as CSG production was constrained by community objection to the damage caused to arable land and water .")
;;############################
;;DUE TO
;;############################
(evaluate-grammar-during-development :frame-evoking-elements '("due to"))

;;correct slots and total slots: (80 111): 0.7207207 
;;correct sentences and total sentences: (16 33): 0.4848485 
;;correct words and total words overall: (598 824): 0.72572816

;correct slots and total slots: (76 111): 0.6846847 
;correct sentences and total sentences: (15 33): 0.45454547 
;correct words and total words overall: (570 824): 0.69174755 

(pie-comprehend "A decline in the size of some species of fish in the North Sea could be due to a rise in
water temperatures , according to research .")

(pie-comprehend "Almost all of the fatalities last year , 93 % , were due to weather - related events .")

(pie-comprehend "Nevertheless, it seems to me that our collective failure to tackle climate change is not just due to political deadlock or insufficient knowledge.")

(pie-comprehend "That difference is due to the long-term, human-caused global warming trend.")

(pie-comprehend "Property damage due to weather extremes has risen sharply in North America, including the Caribbean, over the last 30 years, the report said.")

(pie-comprehend "California senator Diane Feinstein argues that we should curb carbon emissions because the Sierra snowpack, which accounts for much of California's drinking water, will be reduced by 40% by 2050 due to global warming.") ; (0 6) (slots) (-27 36) (words)

(pie-comprehend "Almost all of the fatalities last year, 93%, were due to weather-related events.") ; (0 3) (slots) (1 15) (words)


;;due-to correct, apart from capitalisation
(pie-comprehend "This, as we examine in this chapter, is due to the rapidly falling costs of renewables, which combined with lower fuel usage from energy efficiency investments actually result in significantly lower long term fuel bill.") ; (0 6) (slots) (-7 35) (words)

;;units wegsnijden uit partial transient structure is nefast:
(pie-comprehend "Loss of Arctic sea ice results in enhanced warming of the Arctic Ocean due to a strong positive feedback.") ; (4 6) (slots) (12 19) (words)


;;NOT A CAUSAL DUE TO:
(pie-comprehend "The state of emergency in France, which is due to last three months, has resulted in hundreds of events around the conference being been called off.")

;;longer FCG annotations:
(pie-comprehend "California senator Diane Feinstein argues that we should curb carbon emissions because the Sierra snowpack, which accounts for much of California's drinking water, will be reduced by 40% by 2050 due to global warming.")

(pie-comprehend "With the growing number of natural disasters due to climate change, the sums spent by governments on catastrophe management have risen to unprecedented levels.")

(pie-comprehend "The International Red Cross says more people are already being made refugees due to environmental issues.")

(pie-comprehend "The Lancet estimated that China suffers 1.2 million premature deaths due to fossil fuel pollution.")


;;############################
;;BECAUSE OF
;##############################
(evaluate-grammar-during-development :frame-evoking-elements '("because of"))
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

(pie-comprehend "This has given rise to a third problem in addressing climate change , which stems from a combination of the economic implications of the issue and the uncertainty that surrounds it .")
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
(pie-comprehend "Jasper Kirkby, head of the CLOUD experiment at Cern, the particle physics laboratory near Geneva, studied various gas mixtures of sulphuric acid, water and ammonia  the three gases thought to give rise to aerosol particles at the low altitudes where clouds form.") ; (0 3) (slots) (32 46) (words)

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

(pie-comprehend "Finally , extreme weather can lead to acute outbreaks of infectious disease while at the same time reducing access to health care .")
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



