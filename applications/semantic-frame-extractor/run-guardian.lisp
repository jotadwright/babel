;(ql:quickload :frame-extractor)

(in-package :frame-extractor)

;;(deactivate-all-monitors)
(activate-monitor trace-fcg)
(clear-page)

(pie-comprehend "The money is aimed at encouraging the sustainable use of land, including ensuring that fewer forests are lost to agriculture  the biggest cause of deforestation  and that there is a market for sustainably produced forestry goods, including food, fibre and timber.")

(pie-comprehend "Everything from economic growth to the weather can cause short-term fluctuations beyond any government's control.")

(pie-comprehend "If you accept that humans are causing global warming, as over 97% of peer-reviewed scientific papers do, then this conclusion should not be at all controversial.")

(pie-comprehend "As a consequence, the Newtok Traditional Council is not now eligible for disaster relief funding despite the fact that erosion is causing an ongoing disaster and a humanitarian crisis in the community.")


(pie-comprehend "\u00C2\u00A0Loss and damage caused by climate change will be a result of unsuccessful mitigation and adaptation.")
(pie-comprehend "6.18pm GMT The Guardian's US environment correspondent\u00C2\u00A0Suzanne Goldenberg\u00C2\u00A0has just filed a report\u00C2\u00A0in which she says:The climate crisis of the 21st century has been caused largely by just 90 companies, which between them produced nearly two-thirds of the greenhouse gas emissions generated since the dawning of the industrial age, new research suggests.")

(get-penelope-dependency-analysis "The  money.")

de-render


(make-symbol (downcase ""))