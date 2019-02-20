;(ql:quickload :frame-extractor)
(in-package :frame-extractor)

;(activate-monitor trace-fcg)


;;############################
;;BECAUSE OF
;##############################

(pie-comprehend "Those gorillas are losing their habitat because of rapid urbanisation.") ; (0 3) (slots) (1 10) (words)

(pie-comprehend "Australians also generate more carbon pollution per head than any other developed country, largely because of their heavy reliance on coal-fired power stations to generate electricity.") ; ;(0 3) (slots) (2 27) (words)


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
(pie-comprehend "Work by the dendro-climatologists there gave rise to two papers Dr Keith Briffa and colleagues that convinced us that British/Irish oak was not a good subject for the reconstruction of instrumental-style temperature and rainfall records.") ; (2 3) (slots) (9 37) (words)
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



