
(in-package :fcg-api)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INTERFACE MACRO'S                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-constructicon (grammar-name &body keys-and-constructions)
  "Creates a construction inventory."
  `(fcg:def-fcg-constructions ,grammar-name ,@keys-and-constructions))

(defmacro define-cxn (cxn-name feature-structure &key
                                (cxn-inventory '*fcg-constructions*)
                                (cxn-set 'cxn) (score 0.5) (feature-types nil)
                                (attributes nil) (disable-automatic-footprints nil)
                                (description nil))
  "Creates a construction and adds it to a construction inventory."
  `(fcg:def-fcg-cxn ,cxn-name ,feature-structure :cxn-inventory ,cxn-inventory
                   :cxn-set ,cxn-set :score ,score :feature-types ,feature-types
                   :attributes ,attributes
                   :disable-automatic-footprints ,disable-automatic-footprints
                   :description ,description))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMPREHENSION AND FORMULATION      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric comprehend (utterance &key cxn-inventory silent &allow-other-keys)
  (:documentation "Comprehends an utterance using a constrution inventory."))

(defmethod comprehend ((utterance t) &key (cxn-inventory t) (silent nil) &allow-other-keys)
  (fcg:comprehend utterance :cxn-inventory (or cxn-inventory *fcg-constructions*) :silent silent))

(defgeneric produce (utterance &key cxn-inventory silent &allow-other-keys)
  (:documentation "Comprehends an utterance using a constrution inventory."))

(defmethod produce ((meaning t) &key (cxn-inventory t) (silent nil) &allow-other-keys)
  (fcg:formulate meaning :cxn-inventory cxn-inventory :silent silent))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Visualisation                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import 'monitors:activate-monitor)
(import 'fcg:trace-fcg)