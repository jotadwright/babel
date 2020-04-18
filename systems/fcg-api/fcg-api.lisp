
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
;; Rendering and De-rendering         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod de-render ((utterance string) (mode (eql :de-render-string-adjacent)) &key &allow-other-keys)
  "Splits utterance by space and calls de-render-string-adjacent with this list."
  (de-render (split-sequence:split-sequence #\Space utterance :remove-empty-subseqs t)
             :de-render-string-adjacent))

(defmethod de-render ((utterance list) (mode (eql :de-render-string-adjacent)) &key &allow-other-keys)
  "De-renders a list of strings into string and meets."
  (let ((strings nil)
	(constraints nil))
    (dolist (string utterance)
      (let ((new (make-const string nil)))
	(push `(string ,new ,string) strings)))
    (do ((left strings (rest left)))
	((null (cdr left)))
      (push `(adjacent ,(second (second left)) ,(second (first left)))
	    constraints))
    (make-instance 'coupled-feature-structure 
		   :left-pole `((root (meaning ())
                                      (sem-cat ())
                                      (form ,(append (reverse strings) constraints))
                                      (syn-cat ())))
		   :right-pole '((root)))))
(with-package :fcg 
  (defun adjacent (el-1 el-2 string-constraints)
    "If el-1 and el-2 are in string-constraints el-1 must be immediately left-adjacent to el-2"
    (let ((index-el-1 (position el-1 string-constraints :key #'second :test #'equal))
          (index-el-2 (position el-2 string-constraints :key #'second :test #'equal)))
      (cond
       ;; for efficiency
       ((and (null index-el-1)
             index-el-2)
        nil)
       ;; real testing
       ((or (null index-el-1) (null index-el-2))
        t)
       ((= index-el-1 (- index-el-2 1))
        t)
       (t
        nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Visualisation                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import 'monitors:activate-monitor)
(import 'fcg:trace-fcg)