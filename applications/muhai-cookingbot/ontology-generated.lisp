(in-package :muhai-cookingbot)

(defclass kitchen-entity (entity)
  ((id :type symbol
      :initarg :id
      :accessor id
      :initform nil)
   (name :type string
      :initarg :name
      :accessor name
      :initform nil)
   (persistent-id :type symbol
      :initarg :persistent-id
      :accessor persistent-id
      :initform nil)
   (simulation-data :type list
      :initarg :simulation-data
      :accessor simulation-data
      :initform nil))
  (:documentation "Abstract class for all kitchen entities. All items in the cookingbot ontology should subclass of kitchen-entity."))

(defmethod initialize-instance :after ((orig kitchen-entity) &key)
  (let ((persistent-id (make-id (type-of orig))))
    (setf (persistent-id orig) persistent-id)
    (setf (id orig) (make-id persistent-id))))

(defmethod copy-object-content ((original kitchen-entity) (copy kitchen-entity))
  (setf (id copy) (make-id (id original)))
  (setf (name copy) (copy-object (name original)))
  (setf (persistent-id copy) (persistent-id original))
  (setf (simulation-data copy) (copy-object (simulation-data original))))


(defclass agent (kitchen-entity)
  ()
  (:documentation "The cook."))


(defclass conceptualizable (kitchen-entity)
  ((is-concept :type boolean
      :initarg :is-concept
      :accessor is-concept
      :initform nil))
  (:documentation "An object that can be used to stand for its class as a whole."))

(defmethod copy-object-content ((original conceptualizable) (copy conceptualizable))
  (setf (is-concept copy) (copy-object (is-concept original))))


(defclass pattern (conceptualizable)
  ((is-concept :initform T))
  (:documentation "A geometric pattern, such as that of an arrangement."))

(defmethod copy-object-content ((original pattern) (copy pattern))
  (setf (is-concept copy) (copy-object (is-concept original))))


(defclass shape (conceptualizable)
  ((is-concept :initform T))
  (:documentation "A geometric shape, such as that of a clump of dough."))

(defmethod copy-object-content ((original shape) (copy shape))
  (setf (is-concept copy) (copy-object (is-concept original))))


(defclass heating-mode (conceptualizable)
  ()
  (:documentation "Heating setting."))


(defclass stove-mode (heating-mode)
  ()
  (:documentation "Stove heating setting."))


(defclass low-heat (stove-mode)
  ()
  (:documentation "A low heating setting for the stove."))


(defclass medium-heat (stove-mode)
  ()
  (:documentation "A medium heating setting for the stove."))


(defclass medium-high-heat (stove-mode)
  ()
  (:documentation "A medium-high heating setting for the stove."))


(defclass high-heat (stove-mode)
  ()
  (:documentation "A high heating setting for the stove."))


(defclass arrangeable (kitchen-entity)
  ((arrangement 
      :initarg :arrangement
      :accessor arrangement
      :initform nil))
  (:documentation "An object with a convention on how to arrange items contained/supported by the object."))

(defmethod copy-object-content ((original arrangeable) (copy arrangeable))
  (setf (arrangement copy) (copy-object (arrangement original))))


(defclass fetchable (kitchen-entity)
  ()
  (:documentation "An object that can be grasped, manipulated and carried by a humanoid agent."))


(defclass container (arrangeable)
  ((contents 
      :initarg :contents
      :accessor contents
      :initform nil))
  (:documentation "An object that can have contents; this is understood here somewhat loosely in that the contents are located at the containing object, even if the kinematic constraints between container and containees are weak."))

(defmethod copy-object-content ((original container) (copy container))
  (setf (contents copy) (loop for item in (contents original) collect (copy-object item))))


(defclass transferable-container (container fetchable conceptualizable)
  ((arrangement :initform nil))
  (:documentation "A container that can be carried from place to place."))

(defmethod copy-object-content ((original transferable-container) (copy transferable-container))
  (setf (arrangement copy) (copy-object (arrangement original))))


(defclass microwave (container has-temperature)
  ()
  (:documentation "The microwave. It's a container."))


(defclass cooking-utensil (fetchable conceptualizable)
  ()
  (:documentation "A manipulable, carriable object used as an instrument in cooking tasks."))


(defclass kitchen-state (container has-temperature)
  ((constraints :type list
      :initarg :constraints
      :accessor constraints
      :initform nil)
   (kitchen-time :type integer
      :initarg :kitchen-time
      :accessor kitchen-time
      :initform 0)
   (temperature 
      :initarg :temperature
      :accessor temperature
      :initform (make-instance 'amount
                               :unit (make-instance 'degrees-celsius)
                               :quantity (make-instance 'quantity
                                                        :value 18))))
  (:documentation "Representation of the state of the kitchen."))

(defmethod initialize-instance :after ((orig kitchen-state) &key)
  (when (null (arrangement orig)) (setf (arrangement orig) (make-instance 'sectionalized)))
  (when (null (find 'kitchen-cabinet (contents orig) :test (lambda (x y) (eq x (type-of y))))) (setf (contents orig) (cons (make-instance 'kitchen-cabinet) (contents orig))))
  (when (null (find 'pantry (contents orig) :test (lambda (x y) (eq x (type-of y))))) (setf (contents orig) (cons (make-instance 'pantry) (contents orig))))
  (when (null (find 'fridge (contents orig) :test (lambda (x y) (eq x (type-of y))))) (setf (contents orig) (cons (make-instance 'fridge) (contents orig))))
  (when (null (find 'freezer (contents orig) :test (lambda (x y) (eq x (type-of y))))) (setf (contents orig) (cons (make-instance 'freezer) (contents orig))))
  (when (null (find 'oven (contents orig) :test (lambda (x y) (eq x (type-of y))))) (setf (contents orig) (cons (make-instance 'oven) (contents orig))))
  (when (null (find 'counter-top (contents orig) :test (lambda (x y) (eq x (type-of y))))) (setf (contents orig) (cons (make-instance 'counter-top) (contents orig))))
  (when (null (find 'microwave (contents orig) :test (lambda (x y) (eq x (type-of y))))) (setf (contents orig) (cons (make-instance 'microwave) (contents orig))))
  (when (null (find 'stove (contents orig) :test (lambda (x y) (eq x (type-of y))))) (setf (contents orig) (cons (make-instance 'stove) (contents orig)))))

(defmethod copy-object-content ((original kitchen-state) (copy kitchen-state))
  (setf (constraints copy) (copy-object (loop for item in (constraints original) collect item)))
  (setf (kitchen-time copy) (copy-object (kitchen-time original)))
  (setf (arrangement copy) (copy-object (arrangement original)))
  (setf (contents copy) (loop for item in (contents original) collect (copy-object item)))
  (setf (temperature copy) (copy-object (temperature original))))


(defclass vr-kitchen-state (container)
  ()
  (:documentation "Representation of the Abe-sim kitchen state."))


(defclass aggregate (kitchen-entity)
  ()
  (:documentation "An object formed by combining several separate elements from VR, e.g. butter or liquids."))


(defclass has-temperature (kitchen-entity)
  ((temperature 
      :initarg :temperature
      :accessor temperature
      :initform nil))
  (:documentation "An object that has a temperature quality."))

(defmethod copy-object-content ((original has-temperature) (copy has-temperature))
  (setf (temperature copy) (copy-object (temperature original))))


(defclass bakeable (has-temperature)
  ((baked :type boolean
      :initarg :baked
      :accessor baked
      :initform nil))
  (:documentation "An object that can transform via a baking process."))

(defmethod copy-object-content ((original bakeable) (copy bakeable))
  (setf (baked copy) (copy-object (baked original))))


(defclass boilable (kitchen-entity)
  ((boiled :type boolean
      :initarg :boiled
      :accessor boiled
      :initform nil)
   (boiled-with 
      :initarg :boiled-with
      :accessor boiled-with
      :initform nil))
  (:documentation "An object that can be subjected to boiling."))

(defmethod copy-object-content ((original boilable) (copy boilable))
  (setf (boiled copy) (copy-object (boiled original)))
  (setf (boiled-with copy) (copy-object (boiled-with original))))


(defclass beatable (kitchen-entity)
  ((beaten :type boolean
      :initarg :beaten
      :accessor beaten
      :initform nil))
  (:documentation "An object that can be subjected to mixing by beating it with e.g. a whisk."))

(defmethod copy-object-content ((original beatable) (copy beatable))
  (setf (beaten copy) (copy-object (beaten original))))


(defclass uncut (cutting-pattern)
  ()
  (:documentation "An uncut pattern."))


(defclass can-beat (cooking-utensil)
  ()
  (:documentation "A tool that can be used for beating ingredients together."))


(defclass brushable (kitchen-entity)
  ((brushed-with 
      :initarg :brushed-with
      :accessor brushed-with
      :initform nil))
  (:documentation "An object with an inner surface upon which something else could be brushed."))

(defmethod copy-object-content ((original brushable) (copy brushable))
  (setf (brushed-with copy) (copy-object (brushed-with original))))


(defclass can-be-cut-on (kitchen-entity)
  ()
  (:documentation "An object that can be cut on."))


(defclass can-be-brushed-with (kitchen-entity)
  ((is-brushed-with :type boolean
      :initarg :is-brushed-with
      :accessor is-brushed-with
      :initform nil))
  (:documentation "An object, usually a substance, that can be applied via brushing to some other object."))

(defmethod copy-object-content ((original can-be-brushed-with) (copy can-be-brushed-with))
  (setf (is-brushed-with copy) (copy-object (is-brushed-with original))))


(defclass can-be-lined-with (cooking-utensil)
  ((is-lining :type boolean
      :initarg :is-lining
      :accessor is-lining
      :initform nil))
  (:documentation "An object that can be applied onto another object's surface to cover or protect it."))

(defmethod copy-object-content ((original can-be-lined-with) (copy can-be-lined-with))
  (setf (is-lining copy) (copy-object (is-lining original))))


(defclass can-be-dipped-in (kitchen-entity)
  ()
  (:documentation "An object into which things can be dipped."))


(defclass can-have-on-top (kitchen-entity)
  ((has-on-top 
      :initarg :has-on-top
      :accessor has-on-top
      :initform nil))
  (:documentation "An object that, as part of a dish or food item, can have other ingredients on top of itself."))

(defmethod copy-object-content ((original can-have-on-top) (copy can-have-on-top))
  (setf (has-on-top copy) (copy-object (has-on-top original))))


(defclass can-be-spread-upon (kitchen-entity)
  ((spread-with 
      :initarg :spread-with
      :accessor spread-with
      :initform nil))
  (:documentation "An object that can be spread upon others."))

(defmethod copy-object-content ((original can-be-spread-upon) (copy can-be-spread-upon))
  (setf (spread-with copy) (copy-object (spread-with original))))


(defclass can-be-sprinkled-with (kitchen-entity)
  ()
  (:documentation "An object that can be sprinkled upon others."))


(defclass can-be-sprinkled-on (kitchen-entity)
  ((sprinkled-with 
      :initarg :sprinkled-with
      :accessor sprinkled-with
      :initform nil))
  (:documentation "An object upon which something can be sprinkled."))

(defmethod copy-object-content ((original can-be-sprinkled-on) (copy can-be-sprinkled-on))
  (setf (sprinkled-with copy) (copy-object (sprinkled-with original))))


(defclass can-cover (cooking-utensil)
  ((is-covering :type boolean
      :initarg :is-covering
      :accessor is-covering
      :initform nil))
  (:documentation "An object that can be used to cover a coverable container."))

(defmethod copy-object-content ((original can-cover) (copy can-cover))
  (setf (is-covering copy) (copy-object (is-covering original))))


(defclass can-brush (cooking-utensil)
  ()
  (:documentation "A tool that can be used for brushing."))


(defclass can-grind (cooking-utensil)
  ()
  (:documentation "A tool that can be used for grinding."))


(defclass can-cut (cooking-utensil)
  ()
  (:documentation "A tool that can be used for cutting."))


(defclass can-drain (cooking-utensil)
  ()
  (:documentation "A tool that can be used for draining."))


(defclass can-flatten (cooking-utensil)
  ()
  (:documentation "A tool that can be used to flatten dough."))


(defclass can-mingle ()
  ()
  (:documentation "A tool that can be used for mingling."))


(defclass can-mash (cooking-utensil)
  ()
  (:documentation "A tool that can be used for mashing."))


(defclass can-mix (cooking-utensil)
  ()
  (:documentation "A tool that can be used for mixing."))


(defclass can-peel (cooking-utensil)
  ()
  (:documentation "A tool that can be used for peeling."))


(defclass can-seed (cooking-utensil)
  ()
  (:documentation "A tool that can be used for removing seeds from an ingredient."))


(defclass can-sift (cooking-utensil)
  ()
  (:documentation "A tool that can be used for sifting."))


(defclass liquefiable ()
  ((is-liquid :type boolean
      :initarg :is-liquid
      :accessor is-liquid
      :initform nil))
  (:documentation "An ingredient that can be liquefied."))

(defmethod copy-object-content ((original liquefiable) (copy liquefiable))
  (setf (is-liquid copy) (copy-object (is-liquid original))))


(defclass can-spread (cooking-utensil)
  ()
  (:documentation "A tool that can be used for spreading."))


(defclass coverable-container (container)
  ((covered-with 
      :initarg :covered-with
      :accessor covered-with
      :initform nil))
  (:documentation "A (loose) container that can also be covered."))

(defmethod copy-object-content ((original coverable-container) (copy coverable-container))
  (setf (covered-with copy) (copy-object (covered-with original))))


(defclass crackable (kitchen-entity)
  ((cracked :type boolean
      :initarg :cracked
      :accessor cracked
      :initform nil))
  (:documentation "An object that can be cracked, e.g. by impact, in particular one whose contents become accessible through cracking."))

(defmethod copy-object-content ((original crackable) (copy crackable))
  (setf (cracked copy) (copy-object (cracked original))))


(defclass cuttable (kitchen-entity)
  ((is-cut :type cutting-pattern
      :initarg :is-cut
      :accessor is-cut
      :initform (make-instance 'uncut)))
  (:documentation "An object that can be cut into several pieces."))

(defmethod copy-object-content ((original cuttable) (copy cuttable))
  (setf (is-cut copy) (copy-object (is-cut original))))


(defclass dippable (kitchen-entity)
  ((dipped-in :type can-be-dipped-in
      :initarg :dipped-in
      :accessor dipped-in
      :initform nil))
  (:documentation "An object that can be dipped into something."))

(defmethod copy-object-content ((original dippable) (copy dippable))
  (setf (dipped-in copy) (copy-object (dipped-in original))))


(defclass dough (homogeneous-mixture flattenable bakeable shapeable cuttable)
  ()
  (:documentation "A type of homogeneous mixture used to create pastry, bread, cakes and similar."))


(defclass flattenable (shapeable)
  ((flattened :type boolean
      :initarg :flattened
      :accessor flattened
      :initform nil))
  (:documentation "An object that can be deformed, in particular flattened, by applying pressure to it."))

(defmethod copy-object-content ((original flattenable) (copy flattenable))
  (setf (flattened copy) (copy-object (flattened original))))


(defclass sprinklable (kitchen-entity)
  ()
  (:documentation "Something that can be sprinkled over something."))


(defclass mixture (ingredient beatable cuttable mashable meltable mixable can-be-sprinkled-with siftable sprinklable bakeable shapeable dippable spreadable boilable liquefiable can-be-sprinkled-on can-be-spread-upon has-temperature shakeable)
  ((components 
      :initarg :components
      :accessor components
      :initform nil))
  (:documentation "An object that is a mixture of ingredients."))

(defmethod copy-object-content ((original mixture) (copy mixture))
  (setf (components copy) (loop for item in (components original) collect (copy-object item))))


(defclass homogeneous-mixture (mixture flattenable)
  ()
  (:documentation "A homogeneous mixture: parts are indistinguishable from each other."))


(defclass drainable (kitchen-entity)
  ((drained :type boolean
      :initarg :drained
      :accessor drained
      :initform nil))
  (:documentation "An object that can be drained -- separated from some other object of a different consistency (a solid separated from liquids being a typical example)."))

(defmethod copy-object-content ((original drainable) (copy drainable))
  (setf (drained copy) (copy-object (drained original))))


(defclass fryable (kitchen-entity)
  ((fried :type boolean
      :initarg :fried
      :accessor fried
      :initform nil))
  (:documentation "For objects that can be fried."))

(defmethod copy-object-content ((original fryable) (copy fryable))
  (setf (fried copy) (copy-object (fried original))))


(defclass fluid (ingredient)
  ()
  (:documentation "An object that is a fluid."))


(defclass ingredient (fetchable conceptualizable perishable can-be-sprinkled-on)
  ((amount :type amount
      :initarg :amount
      :accessor amount
      :initform (make-instance 'amount)))
  (:documentation "An object that is an ingredient in a recipe, and associated to an amount."))

(defmethod copy-object-content ((original ingredient) (copy ingredient))
  (setf (amount copy) (copy-object (amount original))))


(defclass list-of-kitchen-entities (kitchen-entity)
  ((items :type list
      :initarg :items
      :accessor items
      :initform nil))
  (:documentation "Auxiliary type to store a list of kitchen entities."))

(defmethod copy-object-content ((original list-of-kitchen-entities) (copy list-of-kitchen-entities))
  (setf (items copy) (loop for item in (items original) collect (copy-object item))))


(defclass perishable (kitchen-entity)
  ((keep-frozen :type boolean
      :initarg :keep-frozen
      :accessor keep-frozen
      :initform nil)
   (keep-refrigerated :type boolean
      :initarg :keep-refrigerated
      :accessor keep-refrigerated
      :initform nil))
  (:documentation "An object that is perishable and may need to be refrigerated for storage."))

(defmethod copy-object-content ((original perishable) (copy perishable))
  (setf (keep-frozen copy) (copy-object (keep-frozen original)))
  (setf (keep-refrigerated copy) (copy-object (keep-refrigerated original))))


(defclass amount (kitchen-entity)
  ((quantity :type quantity
      :initarg :quantity
      :accessor quantity
      :initform (make-instance 'quantity))
   (unit :type unit
      :initarg :unit
      :accessor unit
      :initform (make-instance 'piece)))
  (:documentation "A reified description including a quantity and a unit of measurement."))

(defmethod copy-object-content ((original amount) (copy amount))
  (setf (quantity copy) (copy-object (quantity original)))
  (setf (unit copy) (copy-object (unit original))))


(defclass quantity (kitchen-entity)
  ((value :type number
      :initarg :value
      :accessor value
      :initform 1))
  (:documentation "Stores a value."))

(defmethod copy-object-content ((original quantity) (copy quantity))
  (setf (value copy) (copy-object (value original))))


(defclass unit (conceptualizable)
  ((is-concept :initform T))
  (:documentation "A unit of measurement."))

(defmethod copy-object-content ((original unit) (copy unit))
  (setf (is-concept copy) (copy-object (is-concept original))))


(defclass time-unit (unit)
  ()
  (:documentation "Unit to describe a duration."))


(defclass lineable (kitchen-entity)
  ((lined-with 
      :initarg :lined-with
      :accessor lined-with
      :initform nil))
  (:documentation "An object with an inner surface that can be lined -- i.e. covered in close contact -- with something."))

(defmethod copy-object-content ((original lineable) (copy lineable))
  (setf (lined-with copy) (copy-object (lined-with original))))


(defclass mashable (kitchen-entity)
  ((mashed :type boolean
      :initarg :mashed
      :accessor mashed
      :initform nil))
  (:documentation "An object that can be mashed."))

(defmethod copy-object-content ((original mashable) (copy mashable))
  (setf (mashed copy) (copy-object (mashed original))))


(defclass meltable (kitchen-entity)
  ((melted :type boolean
      :initarg :melted
      :accessor melted
      :initform nil))
  (:documentation "An object that can melt when brought to a high enough temperature."))

(defmethod copy-object-content ((original meltable) (copy meltable))
  (setf (melted copy) (copy-object (melted original))))


(defclass mixable (kitchen-entity)
  ((mixed :type boolean
      :initarg :mixed
      :accessor mixed
      :initform nil))
  (:documentation "An object that can be mixed."))

(defmethod copy-object-content ((original mixable) (copy mixable))
  (setf (mixed copy) (copy-object (mixed original))))


(defclass peelable (kitchen-entity)
  ((peeled :type boolean
      :initarg :peeled
      :accessor peeled
      :initform nil))
  (:documentation "An object that can be peeled."))

(defmethod copy-object-content ((original peelable) (copy peelable))
  (setf (peeled copy) (copy-object (peeled original))))


(defclass pluckable (kitchen-entity)
  ((is-plucked :type boolean
      :initarg :is-plucked
      :accessor is-plucked
      :initform nil)
   (plucked :type (or ingredient null)
      :initarg :plucked
      :accessor plucked
      :initform nil)
   (pluckee :type (or ingredient null)
      :initarg :pluckee
      :accessor pluckee
      :initform nil))
  (:documentation "An object that can be plucked such as fresh herbs."))

(defmethod copy-object-content ((original pluckable) (copy pluckable))
  (setf (is-plucked copy) (copy-object (is-plucked original)))
  (setf (plucked copy) (copy-object (plucked original)))
  (setf (pluckee copy) (copy-object (pluckee original))))


(defclass reusable (kitchen-entity)
  ((used :type boolean
      :initarg :used
      :accessor used
      :initform nil))
  (:documentation "An object that can be reused; reuse may involve cleaning."))

(defmethod copy-object-content ((original reusable) (copy reusable))
  (setf (used copy) (copy-object (used original))))


(defclass seedable (kitchen-entity)
  ((seeded :type boolean
      :initarg :seeded
      :accessor seeded
      :initform nil))
  (:documentation "A fruit with seeds that can be removed."))

(defmethod copy-object-content ((original seedable) (copy seedable))
  (setf (seeded copy) (copy-object (seeded original))))


(defclass shakeable (kitchen-entity)
  ((shaken :type boolean
      :initarg :shaken
      :accessor shaken
      :initform nil))
  (:documentation "An object that can be mixed in a particular way, by shaking."))

(defmethod copy-object-content ((original shakeable) (copy shakeable))
  (setf (shaken copy) (copy-object (shaken original))))


(defclass shapeable (kitchen-entity)
  ((current-shape :type shape
      :initarg :current-shape
      :accessor current-shape
      :initform nil))
  (:documentation "An object that can be shaped through manipulation."))

(defmethod copy-object-content ((original shapeable) (copy shapeable))
  (setf (current-shape copy) (copy-object (current-shape original))))


(defclass siftable (kitchen-entity)
  ((sifted :type boolean
      :initarg :sifted
      :accessor sifted
      :initform nil))
  (:documentation "An object that can be separated into various components, or separated from some powdery substance, by sifting."))

(defmethod copy-object-content ((original siftable) (copy siftable))
  (setf (sifted copy) (copy-object (sifted original))))


(defclass spreadable (kitchen-entity)
  ((spread :type boolean
      :initarg :spread
      :accessor spread
      :initform nil))
  (:documentation "An object providing a surface upon which something can be spread."))

(defmethod copy-object-content ((original spreadable) (copy spreadable))
  (setf (spread copy) (copy-object (spread original))))


(defclass baking-paper (reusable can-be-lined-with)
  ()
  (:documentation "A piece of baking paper; can be used to line something with."))


(defclass baking-dish (transferable-container can-have-on-top reusable)
  ()
  (:documentation "A dish which is oven safe and can be used to hold something while baking."))


(defclass baking-tray (transferable-container lineable brushable reusable)
  ()
  (:documentation "A baking tray, can be lined with e.g. baking paper."))


(defclass bowl (transferable-container coverable-container reusable)
  ()
  (:documentation "A bowl to hold ingredients at various stages of preparation."))


(defclass bowl-lid (can-cover reusable)
  ()
  (:documentation "A bowl lid."))


(defclass plate (transferable-container reusable can-cover lineable can-have-on-top can-be-sprinkled-on)
  ()
  (:documentation "A plate. It's a container."))


(defclass small-plate (plate)
  ()
  (:documentation "A small plate. It's a plate."))


(defclass medium-plate (plate)
  ()
  (:documentation "A medium plate. It's a plate."))


(defclass large-plate (plate)
  ()
  (:documentation "A large plate. It's a plate."))


(defclass brush (reusable can-brush)
  ()
  (:documentation "A brush to spread substances by brushing with."))


(defclass colander (can-drain reusable)
  ()
  (:documentation "A colander, can be used to drain."))


(defclass cookie-sheet (transferable-container lineable brushable reusable)
  ()
  (:documentation "A cookie sheet, i.e. an oven-safe container for baking in."))


(defclass counter-top (container conceptualizable)
  ((arrangement 
      :initarg :arrangement
      :accessor arrangement
      :initform (make-instance 'side-to-side)))
  (:documentation "The countertop where various food preparation tasks will take place."))

(defmethod copy-object-content ((original counter-top) (copy counter-top))
  (setf (arrangement copy) (copy-object (arrangement original))))


(defclass fork (can-mix can-beat can-mash reusable can-mingle)
  ()
  (:documentation "A fork; can be used for mixing, beating, mashing."))


(defclass freezer (container)
  ((arrangement 
      :initarg :arrangement
      :accessor arrangement
      :initform (make-instance 'shelved))
   (temperature 
      :initarg :temperature
      :accessor temperature
      :initform (make-instance 'amount
                               :unit (make-instance 'degrees-celsius)
                               :quantity (make-instance 'quantity
                                                        :value 5))))
  (:documentation "A freezer in which various items can be stored at sub-zero temperatures."))

(defmethod copy-object-content ((original freezer) (copy freezer))
  (setf (arrangement copy) (copy-object (arrangement original)))
  (setf (temperature copy) (copy-object (temperature original))))


(defclass fridge (container)
  ((arrangement 
      :initarg :arrangement
      :accessor arrangement
      :initform (make-instance 'shelved))
   (temperature 
      :initarg :temperature
      :accessor temperature
      :initform (make-instance 'amount
                               :unit (make-instance 'degrees-celsius)
                               :quantity (make-instance 'quantity
                                                        :value 5))))
  (:documentation "A fridge in which various items can be stored at cooler, though not sub-zero, temperatures."))

(defmethod copy-object-content ((original fridge) (copy fridge))
  (setf (arrangement copy) (copy-object (arrangement original)))
  (setf (temperature copy) (copy-object (temperature original))))


(defclass jar (transferable-container coverable-container reusable shakeable)
  ()
  (:documentation "A (usually glass) jar to store various ingredients in."))


(defclass jar-lid (can-cover reusable)
  ()
  (:documentation "A jar lid. Used to cover/close a jar."))


(defclass kitchen-floor (container kitchen-entity)
  ()
  (:documentation "The floor."))


(defclass kitchen-cabinet (container)
  ((arrangement 
      :initarg :arrangement
      :accessor arrangement
      :initform (make-instance 'shelved)))
  (:documentation "The kitchen cabinet, where several utensils are stored."))

(defmethod copy-object-content ((original kitchen-cabinet) (copy kitchen-cabinet))
  (setf (arrangement copy) (copy-object (arrangement original))))


(defclass knife (can-cut can-peel can-seed can-spread reusable)
  ()
  (:documentation "A knife. Can be used to cut, peel, take seeds out or spread substances such as butter or honey."))


(defclass large-bowl (bowl)
  ()
  (:documentation "A larger bowl to hold ingredients at various stages of preparation."))


(defclass large-bowl-lid (bowl-lid)
  ()
  (:documentation "A larger bowl lid used to cover a large bowl."))


(defclass medium-bowl (bowl)
  ()
  (:documentation "A medium-sized bowl to hold ingredients at various stages of preparation."))


(defclass medium-bowl-lid (bowl-lid)
  ()
  (:documentation "A lid used to cover a medium-sized bowl."))


(defclass oven (container has-temperature)
  ((arrangement 
      :initarg :arrangement
      :accessor arrangement
      :initform (make-instance 'shelved)))
  (:documentation "An oven to bake stuff in."))

(defmethod copy-object-content ((original oven) (copy oven))
  (setf (arrangement copy) (copy-object (arrangement original))))


(defclass pan (brushable can-be-sprinkled-on reusable spreadable transferable-container)
  ()
  (:documentation "A frying pan to fry stuff in."))


(defclass pantry (container)
  ((arrangement 
      :initarg :arrangement
      :accessor arrangement
      :initform (make-instance 'shelved)))
  (:documentation "A pantry to store ingredients that do not need refrigeration."))

(defmethod copy-object-content ((original pantry) (copy pantry))
  (setf (arrangement copy) (copy-object (arrangement original))))


(defclass peel (kitchen-entity)
  ((peel-of 
      :initarg :peel-of
      :accessor peel-of
      :initform nil))
  (:documentation "For peels of an object."))

(defmethod copy-object-content ((original peel) (copy peel))
  (setf (peel-of copy) (copy-object (peel-of original))))


(defclass rolling-pin (can-flatten reusable)
  ()
  (:documentation "A rolling pin to roll dough into flat shapes."))


(defclass saucepan (transferable-container reusable)
  ()
  (:documentation "A sauce pan to cook food in."))


(defclass sift (can-sift reusable)
  ()
  (:documentation "A tool used for sifting."))


(defclass small-bowl (bowl)
  ()
  (:documentation "A small-sized bowl to hold ingredients at various stages of preparation."))


(defclass small-bowl-lid (bowl-lid)
  ()
  (:documentation "A lid used to cover a small-sized bowl."))


(defclass spatula (can-spread reusable)
  ()
  (:documentation "A spatula that can be used to flip items over or spread ingredients."))


(defclass stove (container has-temperature)
  ()
  (:documentation "A stove to cook on."))


(defclass table-spoon (can-spread reusable)
  ()
  (:documentation "A table spoon, used for eating liquid or mostly liquid food, but can also be used for other purposes such as spreading jam."))


(defclass whisk (can-mix can-beat can-mingle reusable)
  ()
  (:documentation "A whisk, which is a tool for beating and mixing."))


(defclass wire-rack (transferable-container lineable reusable)
  ()
  (:documentation "Wire rack for cooking on."))


(defclass wooden-spoon (can-mix reusable can-mingle)
  ()
  (:documentation "A wooden spoon, typically used to mix soup."))


(defclass conserved-ingredient (ingredient drainable)
  ((fluid-parts :type (list ingredient)
      :initarg :fluid-parts
      :accessor fluid-parts
      :initform nil)
   (solid-parts :type (list ingredient)
      :initarg :solid-parts
      :accessor solid-parts
      :initform nil))
  (:documentation "A canned ingredient that consists of a solid part and a liquid that is there to preserve the solid against alteration."))

(defmethod copy-object-content ((original conserved-ingredient) (copy conserved-ingredient))
  (setf (fluid-parts copy) (loop for item in (fluid-parts original) collect (copy-object item)))
  (setf (solid-parts copy) (loop for item in (solid-parts original) collect (copy-object item))))


(defclass heterogeneous-mixture (mixture)
  ()
  (:documentation "A mixture in which components are distinguishable from one another."))

(defmethod copy-object-content ((original heterogeneous-mixture) (copy heterogeneous-mixture))
  (setf (components copy) (loop for item in (components original) collect (copy-object item))))


(defclass all-purpose-flour (flour)
  ()
  (:documentation "All-purpose (cereal) flour used for baking."))


(defclass almond (ingredient cuttable fryable has-temperature)
  ()
  (:documentation "Almond-related substance, typically used for baking."))


(defclass almond-extract (flavoring-extract)
  ()
  (:documentation "Almond extract, used for flavoring and baking."))


(defclass almond-flakes (almond sprinklable)
  ()
  (:documentation "Flakes obtained from coarsely grating almond nuts."))


(defclass almond-flour (almond flour)
  ()
  (:documentation "Powder made from finely grinding almond nuts."))


(defclass baking-powder (ingredient)
  ()
  (:documentation "Baking powder used as a leavening agent in baking."))


(defclass baking-soda (ingredient)
  ()
  (:documentation "Baking soda used as a leavening agent in baking."))


(defclass seed (kitchen-entity)
  ((seed-of 
      :initarg :seed-of
      :accessor seed-of
      :initform nil))
  (:documentation "For seeds of an object."))

(defmethod copy-object-content ((original seed) (copy seed))
  (setf (seed-of copy) (copy-object (seed-of original))))


(defclass avocado (ingredient cuttable seedable peelable mashable)
  ()
  (:documentation "Avocado."))


(defclass apple (ingredient cuttable seedable peelable)
  ()
  (:documentation "Apple fruit."))


(defclass black-olive (ingredient cuttable seedable)
  ()
  (:documentation "Black Olive."))


(defclass carrot (ingredient cuttable)
  ()
  (:documentation "Carrot."))


(defclass broccoli (ingredient cuttable)
  ()
  (:documentation "Broccoli."))


(defclass banana (ingredient mashable)
  ()
  (:documentation "Banana fruit."))


(defclass black-bean (ingredient)
  ()
  (:documentation "Black beans."))


(defclass brown-lentils (lentils)
  ()
  (:documentation "Brown lentils."))


(defclass coconut-oil (ingredient liquid)
  ()
  (:documentation "Coconut oil."))


(defclass cooked-bacon (ingredient cuttable)
  ()
  (:documentation "Cooked bacon."))


(defclass cooked-chicken (ingredient cuttable)
  ()
  (:documentation "Cooked chicken."))


(defclass butter (ingredient mixable beatable meltable has-temperature can-be-brushed-with spreadable can-have-on-top aggregate)
  ((keep-refrigerated :initform T))
  (:documentation "Butter."))

(defmethod copy-object-content ((original butter) (copy butter))
  (setf (keep-refrigerated copy) (copy-object (keep-refrigerated original))))


(defclass butter-particle (butter)
  ()
  (:documentation "A particle of butter."))


(defclass celery (ingredient cuttable)
  ((keep-refrigerated :initform T))
  (:documentation "Celery."))

(defmethod copy-object-content ((original celery) (copy celery))
  (setf (keep-refrigerated copy) (copy-object (keep-refrigerated original))))


(defclass cherry-tomato (ingredient cuttable)
  ((keep-refrigerated :initform T))
  (:documentation "Cherry tomato."))

(defmethod copy-object-content ((original cherry-tomato) (copy cherry-tomato))
  (setf (keep-refrigerated copy) (copy-object (keep-refrigerated original))))


(defclass grindable (kitchen-entity)
  ((ground :type boolean
      :initarg :ground
      :accessor ground
      :initform nil))
  (:documentation "For objects that can be ground."))

(defmethod copy-object-content ((original grindable) (copy grindable))
  (setf (ground copy) (copy-object (ground original))))


(defclass walnut (ingredient cuttable grindable)
  ()
  (:documentation "Walnut."))


(defclass white-bread-slice (ingredient cuttable)
  ()
  (:documentation "A slice of white bread."))


(defclass heatable-container (transferable-container)
  ()
  (:documentation "A transferable container that can be heated up. i.e., that can be used for cooking."))


(defclass frying-pan (heatable-container brushable reusable)
  ()
  (:documentation "Frying pan. It can be heated up."))


(defclass plastic-wrap (can-cover reusable)
  ()
  (:documentation "Plastic foil. Can be used to cover most things."))


(defclass tea-spoon (can-spread reusable)
  ()
  (:documentation "A tea spoon."))


(defclass bread-knife (can-cut reusable)
  ()
  (:documentation "A ridged knife that can be used for cutting"))


(defclass can-separate-eggs (cooking-utensil)
  ()
  (:documentation "A tool that can be user to separate eggs into egg yolks and egg whites."))


(defclass egg-separator (can-separate-eggs reusable)
  ()
  (:documentation "A tool to separate eggs into egg yolks and egg whites."))


(defclass cutting-board (can-be-cut-on reusable)
  ()
  (:documentation "An object where ingredients can be safely cut on."))


(defclass mixer (can-mix can-beat reusable)
  ()
  (:documentation "A mixer. It's an electric tool for mixing or beating."))


(defclass paper-baking-cup (reusable can-be-lined-with conceptualizable)
  ()
  (:documentation "A piece of baking paper. It can be used to line muffins with."))


(defclass paper-baking-cups (list-of-kitchen-entities reusable can-be-lined-with conceptualizable)
  ()
  (:documentation "A collection of paper-baking-cups."))


(defclass muffin-tins (transferable-container lineable brushable reusable conceptualizable)
  ()
  (:documentation "Abstract class for muffin tins. It's a lineable container."))


(defclass muffin-tins-12 (muffin-tins)
  ((number-of-tins :type quantity
      :initarg :number-of-tins
      :accessor number-of-tins
      :initform (make-instance 'quantity 
:value 12)))
  (:documentation "Muffin tins with 12 places for muffins."))

(defmethod copy-object-content ((original muffin-tins-12) (copy muffin-tins-12))
  (setf (number-of-tins copy) (copy-object (number-of-tins original))))


(defclass food-processor (can-mix can-beat can-grind reusable)
  ()
  (:documentation "A food processor. It's a tool for mixing, beating and grinding."))


(defclass cooking-pot (heatable-container reusable)
  ()
  (:documentation "Cooking pot. It can be heated up."))


(defclass chopped-walnut (ingredient)
  ()
  (:documentation "Walnut (chopped)."))


(defclass cocoa-powder (ingredient siftable)
  ()
  (:documentation "Cocoa powder."))


(defclass corn-flakes (ingredient)
  ()
  (:documentation "Corn flakes."))


(defclass cucumber (ingredient cuttable peelable seedable)
  ((keep-refrigerated :initform T))
  (:documentation "Cucumber."))

(defmethod copy-object-content ((original cucumber) (copy cucumber))
  (setf (keep-refrigerated copy) (copy-object (keep-refrigerated original))))


(defclass devils-food-cake-mix (ingredient)
  ()
  (:documentation "Devil's food cake mix."))


(defclass dried-dill-weed (ingredient)
  ()
  (:documentation "Dried (and chopped) dill weed."))


(defclass dry-white-wine (ingredient cuttable)
  ()
  (:documentation "Dry white wine."))


(defclass egg (ingredient has-temperature crackable)
  ((keep-refrigerated :initform T))
  (:documentation "Egg (whole and with shell)."))

(defmethod copy-object-content ((original egg) (copy egg))
  (setf (keep-refrigerated copy) (copy-object (keep-refrigerated original))))


(defclass egg-shell (ingredient crackable)
  ()
  (:documentation "Eggshell."))


(defclass egg-yolk (ingredient has-temperature)
  ((keep-refrigerated :initform T))
  (:documentation "Egg yolk."))

(defmethod copy-object-content ((original egg-yolk) (copy egg-yolk))
  (setf (keep-refrigerated copy) (copy-object (keep-refrigerated original))))


(defclass flavoring-extract (ingredient)
  ()
  (:documentation "Flavoring extract."))


(defclass flour (ingredient siftable can-be-sprinkled-on)
  ()
  (:documentation "Flour."))


(defclass fresh-basil (spice cuttable pluckable)
  ()
  (:documentation "Fresh basil."))


(defclass fresh-cilantro (spice cuttable pluckable)
  ()
  (:documentation "Fresh cilantro."))


(defclass fresh-parsley (spice cuttable pluckable)
  ()
  (:documentation "Fresh parsley."))


(defclass fresh-rosemary (spice cuttable pluckable)
  ()
  (:documentation "Fresh rosemary."))


(defclass cranberry (ingredient cuttable)
  ()
  (:documentation "Cranberry."))


(defclass cream-cheese (ingredient has-temperature)
  ()
  (:documentation "Cream cheese."))


(defclass syrup (ingredient liquid)
  ()
  (:documentation "Syrup."))


(defclass crushed-pineapple (ingredient)
  ()
  (:documentation "Crushed-pineapple."))


(defclass crushed-pineapple-in-syrup (ingredient)
  ((components 
      :initarg :components
      :accessor components
      :initform (list (make-instance 'crushed-pineapple 
:amount (make-instance 'amount
                                                      :quantity (make-instance 'quantity
                                                                               :value 0)
                                                      :unit (make-instance 'percent
                                                                           )))(make-instance 'syrup 
:amount (make-instance 'amount
                                                      :quantity (make-instance 'quantity
                                                                               :value 0)
                                                      :unit (make-instance 'percent
                                                                           ))))))
  (:documentation "Crushed-pineapple in sryup."))

(defmethod copy-object-content ((original crushed-pineapple-in-syrup) (copy crushed-pineapple-in-syrup))
  (setf (components copy) (loop for item in (components original) collect (copy-object item))))


(defclass egg-white (ingredient has-temperature)
  ()
  (:documentation "Egg white."))


(defclass feta-cheese (ingredient cuttable)
  ()
  (:documentation "Feta cheese."))


(defclass fresh-oregano (spice pluckable cuttable)
  ()
  (:documentation "Fresh oregano."))


(defclass grated-horseradish (spice)
  ()
  (:documentation "Grated horseradish."))


(defclass radish (ingredient cuttable)
  ()
  (:documentation "Radish."))


(defclass grated-mozzarella (ingredient cuttable)
  ()
  (:documentation "Grated mozzarella."))


(defclass green-cabbage (ingredient cuttable)
  ()
  (:documentation "Green cabbage."))


(defclass green-chili-pepper (ingredient cuttable)
  ()
  (:documentation "Green chili pepper."))


(defclass green-onion (ingredient cuttable peelable)
  ()
  (:documentation "Green onion (scallion)."))


(defclass corn (ingredient)
  ()
  (:documentation "Corn."))


(defclass frozen-corn (ingredient has-temperature)
  ((keep-frozen :initform T))
  (:documentation "Frozen corn."))

(defmethod copy-object-content ((original frozen-corn) (copy frozen-corn))
  (setf (keep-frozen copy) (copy-object (keep-frozen original))))


(defclass garlic (ingredient cuttable)
  ()
  (:documentation "Garlic."))


(defclass garlic-powder (ingredient)
  ()
  (:documentation "Garlic powder."))


(defclass ground-allspice (ground-spice)
  ()
  (:documentation "Ground allspice."))


(defclass paprika-powder (ingredient)
  ()
  (:documentation "Paprika powder."))


(defclass red-wine-vinegar (vinegar)
  ()
  (:documentation "Red wine vinegar."))


(defclass tabasco (ingredient)
  ()
  (:documentation "Tabasco."))


(defclass trader-joes-cilantro-salad-dressing (ingredient)
  ()
  (:documentation "Trader Joe's cilantro salad dressing."))


(defclass turmeric-powder (spice)
  ()
  (:documentation "Turmeric powder."))


(defclass potato (ingredient cuttable peelable boilable has-temperature)
  ()
  (:documentation "Potato."))


(defclass mustard-seed (spice)
  ()
  (:documentation "Mustard seed."))


(defclass ground-spice (spice siftable)
  ((ground :type boolean
      :initarg :ground
      :accessor ground
      :initform T))
  (:documentation "Ground spice."))

(defmethod copy-object-content ((original ground-spice) (copy ground-spice))
  (setf (ground copy) (copy-object (ground original))))


(defclass ground-black-pepper (ground-spice)
  ()
  (:documentation "Ground black pepper."))


(defclass ground-cinnamon (ground-spice)
  ()
  (:documentation "Ground cinnamon."))


(defclass ground-cloves (ground-spice)
  ()
  (:documentation "Ground cloves."))


(defclass ground-cumin (ground-spice)
  ()
  (:documentation "Ground cumin."))


(defclass ground-ginger (ground-spice)
  ()
  (:documentation "Ground ginger."))


(defclass ground-nutmeg (ground-spice)
  ()
  (:documentation "Ground nutmeg."))


(defclass hard-boiled-egg (ingredient)
  ()
  (:documentation "Hard boiled egg."))


(defclass heavy-cream (ingredient beatable)
  ()
  (:documentation "Heavy cream."))


(defclass jalapeno (ingredient cuttable seedable)
  ((keep-refrigerated :initform T))
  (:documentation "Jalapeno pepper."))

(defmethod copy-object-content ((original jalapeno) (copy jalapeno))
  (setf (keep-refrigerated copy) (copy-object (keep-refrigerated original))))


(defclass juice (fluid)
  ()
  (:documentation "A liquid produced usually from some fruit or vegetable."))


(defclass lemon-juice (ingredient)
  ()
  (:documentation "Lemon juice."))


(defclass lentils (ingredient)
  ()
  (:documentation "Some kind of lentil."))


(defclass lime-juice (ingredient)
  ()
  (:documentation "Lime juice."))


(defclass linguine (ingredient)
  ()
  (:documentation "Linguine."))


(defclass marshmallow (ingredient)
  ()
  (:documentation "Marshmallow."))


(defclass mango (ingredient cuttable)
  ()
  (:documentation "Mango."))


(defclass mayonnaise (ingredient)
  ()
  (:documentation "Mayonnaise."))


(defclass liquid (boilable)
  ()
  (:documentation "A liquid ingredient."))


(defclass milk (ingredient has-temperature liquid)
  ((keep-refrigerated :initform T))
  (:documentation "Milk."))

(defmethod copy-object-content ((original milk) (copy milk))
  (setf (keep-refrigerated copy) (copy-object (keep-refrigerated original))))


(defclass washable (kitchen-entity)
  ((washed :type boolean
      :initarg :washed
      :accessor washed
      :initform nil))
  (:documentation "For objects that can be washed."))

(defmethod copy-object-content ((original washable) (copy washable))
  (setf (washed copy) (copy-object (washed original))))


(defclass mixed-greens (ingredient cuttable washable)
  ()
  (:documentation "Mixed greens."))


(defclass molasses (fluid)
  ()
  (:documentation "Molasses."))


(defclass oats (ingredient)
  ()
  (:documentation "Oats."))


(defclass olive-oil (oil)
  ()
  (:documentation "Olive oil."))


(defclass onion (ingredient cuttable peelable)
  ()
  (:documentation "Onion."))


(defclass pancetta (ingredient cuttable)
  ()
  (:documentation "Pancetta."))


(defclass raisin (ingredient)
  ()
  (:documentation "Rain(s)."))


(defclass red-bell-pepper (ingredient seedable cuttable)
  ()
  (:documentation "Red bell pepper."))


(defclass red-chili-pepper (ingredient cuttable)
  ()
  (:documentation "Red hot chili pepper."))


(defclass red-onion (ingredient cuttable peelable)
  ()
  (:documentation "Red onion."))


(defclass red-pepper-flakes (spice)
  ()
  (:documentation "Red pepper flakes."))


(defclass romaine-lettuce (ingredient cuttable washable can-be-lined-with)
  ()
  (:documentation "Romaine lettuce."))


(defclass salt (spice)
  ()
  (:documentation "Table salt."))


(defclass salted-butter (ingredient mixable beatable meltable has-temperature can-be-brushed-with spreadable can-have-on-top)
  ((keep-refrigerated :initform T))
  (:documentation "Salted butter."))

(defmethod copy-object-content ((original salted-butter) (copy salted-butter))
  (setf (keep-refrigerated copy) (copy-object (keep-refrigerated original))))


(defclass shredded-coconut (ingredient has-temperature)
  ()
  (:documentation "Shredded coconut."))


(defclass self-rising-flour (flour)
  ()
  (:documentation "Mixture of all-purpose flour, baking powder, salt."))


(defclass tomato (ingredient cuttable seedable peelable)
  ()
  (:documentation "Tomato."))


(defclass yellow-mustard (ingredient)
  ()
  (:documentation "Yellow mustard."))


(defclass bisquick-baking-mix (ingredient)
  ()
  (:documentation "Bisquick baking mix."))


(defclass chocolate-chips (ingredient meltable)
  ()
  (:documentation "Chocolate chips."))


(defclass celery-seed (spice)
  ()
  (:documentation "Celery seed."))


(defclass oil (ingredient liquid)
  ()
  (:documentation "Cooking oil."))


(defclass coarse-salt (salt)
  ()
  (:documentation "Coarse salt."))


(defclass cubes (cutting-pattern)
  ()
  (:documentation "A pattern for cutting objects into cubes."))


(defclass shredded (cutting-pattern)
  ()
  (:documentation "A shredded pattern."))


(defclass finely-chopped (cutting-pattern)
  ()
  (:documentation "A chopped pattern."))


(defclass fine-slices (cutting-pattern)
  ()
  (:documentation "A finely sliced pattern."))


(defclass cider-vinegar (vinegar)
  ()
  (:documentation "Cider vinegar."))


(defclass extra-virgin-olive-oil (olive-oil)
  ()
  (:documentation "Extra-virgin olive oil."))


(defclass semisweet-chocolate-chips (chocolate-chips)
  ()
  (:documentation "Semisweet chocolate chips."))


(defclass shallot (ingredient cuttable peelable)
  ()
  (:documentation "Shallot."))


(defclass spice (ingredient boilable)
  ()
  (:documentation "Spice."))


(defclass sugar (spice ingredient mixable)
  ()
  (:documentation "Sugar."))


(defclass brown-sugar (sugar)
  ()
  (:documentation "Plain brown sugar."))


(defclass sweet-potato (ingredient cuttable peelable)
  ()
  (:documentation "Sweet potato."))


(defclass caster-sugar (sugar)
  ()
  (:documentation "Finely granulated sugar."))


(defclass icing-sugar (sugar)
  ()
  (:documentation "Icing sugar."))


(defclass powdered-white-sugar (sugar sprinklable can-be-dipped-in)
  ()
  (:documentation "Powdered white sugar."))


(defclass toast (ingredient spreadable bakeable can-have-on-top)
  ()
  (:documentation "Toast."))


(defclass vanilla (ingredient)
  ()
  (:documentation "Vanilla."))


(defclass vanilla-extract (flavoring-extract)
  ()
  (:documentation "Vanilla extract."))


(defclass vegetable-oil (ingredient can-be-brushed-with liquid)
  ()
  (:documentation "Vegetable oil."))


(defclass vinegar (ingredient liquid)
  ()
  (:documentation "Vinegar."))


(defclass water (fluid has-temperature liquid)
  ()
  (:documentation "Water."))


(defclass white-sugar (sugar)
  ()
  (:documentation "Plain white sugar."))


(defclass white-vinegar (vinegar)
  ()
  (:documentation "White vinegar."))


(defclass whole-egg (ingredient)
  ()
  (:documentation "A whole egg, but without its shell."))


(defclass whole-wheat-flour (flour)
  ()
  (:documentation "Whole wheat flour."))


(defclass arrangement-pattern (pattern)
  ()
  (:documentation "A pattern in which things are arranged in or on something."))


(defclass side-to-side (arrangement-pattern)
  ()
  (:documentation "Items are arranged such that their sides face each other."))


(defclass shelved (arrangement-pattern)
  ()
  (:documentation "Items are arranged such that all shelves are subsequently filled."))


(defclass sectionalized (arrangement-pattern)
  ()
  (:documentation "Items are arranged such that they fill sections."))


(defclass unordered-heap (arrangement-pattern)
  ()
  (:documentation "Items are randomly piled into a heap."))


(defclass evenly-spread (arrangement-pattern)
  ()
  (:documentation "Items are arranged in an even spread over the available surface."))


(defclass on-angle (arrangement-pattern)
  ()
  (:documentation "Items are arranged to form a particular angle."))


(defclass two-inch-apart (arrangement-pattern)
  ()
  (:documentation "Items are arranged to be two inches apart from each other."))


(defclass squares (cutting-pattern)
  ()
  (:documentation "A squares pattern."))


(defclass two-cm-cubes (cutting-pattern)
  ()
  (:documentation "A pattern for cutting objects into 2 cm cubes "))


(defclass slices (cutting-pattern)
  ()
  (:documentation "A sliced pattern."))


(defclass 5-cm-apart (arrangement-pattern)
  ()
  (:documentation "A pattern in which objects are arranged with a distance of 5 cm."))


(defclass cutting-pattern (pattern)
  ()
  (:documentation "A pattern in which something can be cut."))


(defclass chopped (cutting-pattern)
  ()
  (:documentation "A cutting pattern for objects that should be cut into somewhat small pieces."))


(defclass finey-chopped (cutting-pattern)
  ()
  (:documentation "A cutting pattern for objects that should be cut into very small pieces."))


(defclass sliced (cutting-pattern)
  ()
  (:documentation "A cutting pattern for objects that should be cut into thin flat pieces."))


(defclass diced (cutting-pattern)
  ()
  (:documentation "A cutting pattern for objects that should be cut into more or less cubic pieces."))


(defclass minced (cutting-pattern)
  ()
  (:documentation "A cutting pattern for objects that should be cut into very small pieces, to the point that the pieces are hard to distinguish from each other."))


(defclass halved (cutting-pattern)
  ()
  (:documentation "A cutting pattern for objects that should be cut into two more or less equal pieces."))


(defclass thin-slivers (cutting-pattern)
  ()
  (:documentation "A cutting pattern for objects that should be cut into very thin slices."))


(defclass three-quarter-inch-cubes (diced)
  ()
  (:documentation "A cutting pattern for objects that should be cut into somewhat cubic pieces of about 3/4ths of an inch on the side."))


(defclass peasized-cubes (diced)
  ()
  (:documentation "A cutting pattern for objects that should be cut into somewhat cubic small pieces."))


(defclass cover-pattern (pattern)
  ()
  (:documentation "A pattern in which something is covered."))


(defclass lid-on-angle (cover-pattern)
  ()
  (:documentation "A pattern in which a container is partially covered by putting its lid on an angle."))


(defclass crescent-shape (shape)
  ()
  (:documentation "A crescent shape."))


(defclass ball-shape (shape)
  ()
  (:documentation "A ball shape."))


(defclass flattened-ball-shape (ball-shape)
  ()
  (:documentation "A ball shape which is slightly flattened."))


(defclass walnut-ball-shape (ballshape)
  ()
  (:documentation "A walnut ball shape."))


(defclass piece (unit)
  ()
  (:documentation "Unit: piece."))


(defclass stalk (unit)
  ()
  (:documentation "Unit: stalk."))


(defclass clove (unit)
  ()
  (:documentation "Unit: clove."))


(defclass cm (unit)
  ()
  (:documentation "Unit: centimeter."))


(defclass g (unit)
  ()
  (:documentation "Unit: gram."))


(defclass cup (unit)
  ()
  (:documentation "Unit: cup."))


(defclass handful (unit)
  ()
  (:documentation "Unit: handful."))


(defclass very-low-heat (unit)
  ()
  (:documentation "Unit: very low heat."))


(defclass tablespoon (unit)
  ()
  (:documentation "Unit: tablespoon."))


(defclass teaspoon (unit)
  ()
  (:documentation "Unit: teaspoon."))


(defclass l (unit)
  ()
  (:documentation "Unit: liter."))


(defclass ml (unit)
  ()
  (:documentation "Unit: milliliter."))


(defclass minute (time-unit)
  ()
  (:documentation "Unit: minute."))


(defclass hour (time-unit)
  ()
  (:documentation "Unit: hour."))


(defclass percent (unit)
  ()
  (:documentation "Unit: percent."))


(defclass degrees-celsius (unit)
  ()
  (:documentation "Unit: degrees Celsius."))


(defclass failed-object (arrangement-pattern can-cover can-cut can-mash can-mingle can-mix can-peel can-seed can-spread can-be-cut-on can-drain coverable-container fridge kitchen-state lineable list-of-kitchen-entities oven stove quantity shape sift time-unit transferable-container)
  ()
  (:documentation "Evaluation"))



(defmethod find-in-kitchen-state-contents ((kitchen-state kitchen-state) (classname symbol))
  (labels ((traverse (root classname)
             (if (eq (type-of root) classname)
                 root
                 (when (slot-exists-p root 'contents)
                   (loop for child in (slot-value root 'contents)
                         for found = (traverse child classname)
                         when found
                           return found)))))
    (traverse kitchen-state classname)))


(defmethod counter-top ((kitchen-state kitchen-state))
  (find-in-kitchen-state-contents kitchen-state 'counter-top))

(defmethod pantry ((kitchen-state kitchen-state))
  (find-in-kitchen-state-contents kitchen-state 'pantry))

(defmethod fridge ((kitchen-state kitchen-state))
  (find-in-kitchen-state-contents kitchen-state 'fridge))

(defmethod freezer ((kitchen-state kitchen-state))
  (find-in-kitchen-state-contents kitchen-state 'freezer))

(defmethod oven ((kitchen-state kitchen-state))
  (find-in-kitchen-state-contents kitchen-state 'oven))

(defmethod microwave ((kitchen-state kitchen-state))
  (find-in-kitchen-state-contents kitchen-state 'microwave))

(defmethod kitchen-cabinet ((kitchen-state kitchen-state))
  (find-in-kitchen-state-contents kitchen-state 'kitchen-cabinet))

(defmethod stove ((kitchen-state kitchen-state))
  (find-in-kitchen-state-contents kitchen-state 'stove))
