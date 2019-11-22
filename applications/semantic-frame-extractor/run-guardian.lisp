;(ql:quickload :frame-extractor)

(in-package :frame-extractor)

;;(deactivate-all-monitors)
(activate-monitor trace-fcg)
(clear-page)

(set-configuration (visualization-configuration *fcg-constructions*) :hide-features '(footprints syn-valence ))
(pie-comprehend "Journalists reported from Ethiopia about a famine caused by widespread drought.")

(length (find-all 'morph (constructions *fcg-constructions*) :key #'(lambda (cxn) (attr-val cxn :label)))) ;;17

(length (find-all 'lex (constructions *fcg-constructions*) :key #'(lambda (cxn) (attr-val cxn :label)))) ;;11

(length (find-all 'cxn (constructions *fcg-constructions*) :key #'(lambda (cxn) (attr-val cxn :label)))) ;;30

(length (constructions *fcg-constructions*))



(pie-comprehend "Over two-thirds agreed that if they had caused damage to their own clothes at work, the company should not be liable for repairs caused by people.")
;; Lead to
;;-----------------

(pie-comprehend "If the mine goes ahead and that leads to the development of other mines in the basin, their potential combined maximum output would result in more than 705m tonnes of CO2 being emitted each year.")



;; Because
;;-----------------
(pie-comprehend "Satellite measurements have problems because of calibration changes when they are replaced every few years.")

(pie-comprehend "Shell warned environmentalists and ethical investors yesterday that failure to exploit tar sands and other unconventional oil products would worsen climate change because it would lead to the world burning even more carbon-heavy coal.")

(pie-comprehend
"To overcome this market failure, they argue, we need to internalise the costs of future environmental damage by putting a price on the thing that causes it.")

(pie-comprehend "This flooding is a sharp reminder that everyone, sceptic or not, has to think about the risk of flooding, whatever they think causes it")


(pie-comprehend "Because the phenomenon causes less rain to fall in many areas of the tropics, forests become especially vulnerable to man-made fires, which accelerate carbon dioxide buildup in the atmosphere and reduce air quality.")



;; Due to
;;-----------------
(pie-comprehend "Canada faced fresh calls to shut down its commercial seal hunt on Thursday, following new evidence that death rates among seal pups had dramatically increased due to thinning winter sea ice.")

(pie-comprehend "As a consequence, the Newtok Traditional Council is not now eligible for disaster relief funding despite the fact that erosion is causing an ongoing disaster and a humanitarian crisis in the community.")

(pie-comprehend "This includes the extinction of the dinosaurs 65m years ago , thought to have been caused by the impact of a large asteroid on the Yucatan peninsula and beneath the Gulf of Mexico.")






(pie-comprehend "Everything from economic growth to the weather can cause short-term fluctuations beyond any government's control.")


(pie-comprehend "The money is aimed at encouraging the sustainable use of land, including ensuring that fewer forests are lost to agriculture  the biggest cause of deforestation  and that there is a market for sustainably produced forestry goods, including food, fibre and timber.")

(pie-comprehend "If you accept that humans are causing global warming, as over 97% of peer-reviewed scientific papers do, then this conclusion should not be at all controversial.")


(pie-comprehend "With the growing number of natural disasters due to climate change, the sums spent by governments on catastrophe management have risen to unprecedented levels.")