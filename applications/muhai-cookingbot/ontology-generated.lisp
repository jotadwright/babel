(in-package :muhai-cookingbot)

(defclass aggregate (kitchen-entity)
  ()
  (:documentation "An object formed by combining several separate elements from VR, e.g. butter or liquids."))


(defclass all-purpose-flour (flour)
  ()
  (:documentation "All-purpose (cereal) flour used for baking."))


(defclass almond (ingredient)
  ()
  (:documentation "Almond-related substance, typically used for baking."))


(defclass almond-extract (almond flavoring-extract)
  ()
  (:documentation "Almond extract, used for flavoring and baking."))


(defclass almond-flakes (almond can-be-sprinkled-with)
  ()
  (:documentation "Flakes obtained from coarsely grating almond nuts."))


(defclass almond-flour (almond flour)
  ()
  (:documentation "Powder made from finely grinding almond nuts."))


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

(defmethod copy-object-content (original amount) (copy amount)
  (setf (quantity copy) (copy-object (quantity original)))
  (setf (unit copy) (copy-object (unit original))))


(defclass arrangeable (kitchen-entity)
  ((arrangement :type arrangement-pattern
      :initarg :arrangement
      :accessor arrangement
      :initform nil))
  (:documentation "An object with a convention on how to arrange items contained/supported by the object."))

(defmethod copy-object-content (original arrangeable) (copy arrangeable)
  (setf (arrangement copy) (copy-object (arrangement original))))


(defclass arrangement-pattern (pattern)
  ()
  (:documentation "A pattern in which things are arranged in or on something."))


(defclass bakeable (has-temperature)
  ((baked :type boolean
      :initarg :baked
      :accessor baked
      :initform nil))
  (:documentation "An object that can transform via a baking process."))

(defmethod copy-object-content (original bakeable) (copy bakeable)
  (setf (baked copy) (baked original)))


(defclass baking-dish (transferable-container can-have-on-top reusable)
  ()
  (:documentation "A dish which is oven safe and can be used to hold something while baking."))


(defclass baking-paper (reusable can-be-lined-with)
  ()
  (:documentation "A piece of baking paper; can be used to line something with."))


(defclass baking-powder (ingredient)
  ()
  (:documentation "Baking powder used as a leavening agent in baking."))


(defclass baking-soda (ingredient)
  ()
  (:documentation "Baking soda used as a leavening agent in baking."))


(defclass baking-tray (transferable-container lineable brushable reusable)
  ()
  (:documentation "A baking tray, can be lined with e.g. baking paper."))


(defclass ball-shape (shape)
  ()
  (:documentation "A ball shape."))


(defclass banana (ingredient mashable)
  ()
  (:documentation "Banana fruit."))


(defclass beatable (kitchen-entity)
  ((beaten :type boolean
      :initarg :beaten
      :accessor beaten
      :initform nil))
  (:documentation "An object that can be subjected to mixing by beating it with e.g. a whisk."))

(defmethod copy-object-content (original beatable) (copy beatable)
  (setf (beaten copy) (beaten original)))


(defclass black-bean (ingredient)
  ()
  (:documentation "Black beans."))


(defclass bowl (transferable-container coverable-container reusable)
  ()
  (:documentation "A bowl to hold ingredients at various stages of preparation."))


(defclass bowl-lid (can-cover reusable)
  ()
  (:documentation "A bowl lid."))


(defclass brown-lentils (lentils)
  ()
  (:documentation "Brown lentils."))


(defclass brown-sugar (sugar)
  ()
  (:documentation "Plain brown sugar."))


(defclass brush (reusable can-brush)
  ()
  (:documentation "A brush to spread substances by brushing with."))


(defclass brushable (kitchen-entity)
  ((brushed-with :type list
      :initarg :brushed-with
      :accessor brushed-with
      :initform nil))
  (:documentation "An object with an inner surface upon which something else could be brushed."))

(defmethod copy-object-content (original brushable) (copy brushable)
  (setf (brushed-with copy) (loop for item in (brushed-with original) collect (copy-object item))))


(defclass butter (ingredient mixable beatable meltable has-temperature can-be-brushed-with can-be-spread-upon can-have-on-top aggregate)
  ((keep-refrigerated :initform T))
  (:documentation "Butter."))

(defmethod copy-object-content (original butter) (copy butter)
  (setf (keep-refrigerated copy) (keep-refrigerated original)))


(defclass can-be-brushed-with (kitchen-entity)
  ((is-brushed-with :type boolean
      :initarg :is-brushed-with
      :accessor is-brushed-with
      :initform nil))
  (:documentation "An object, usually a substance, that can be applied via brushing to some other object."))

(defmethod copy-object-content (original can-be-brushed-with) (copy can-be-brushed-with)
  (setf (is-brushed-with copy) (is-brushed-with original)))


(defclass can-be-dipped-in (kitchen-entity)
  ()
  (:documentation "An object into which things can be dipped."))


(defclass can-be-lined-with (cooking-utensil)
  ((is-lining :type boolean
      :initarg :is-lining
      :accessor is-lining
      :initform nil))
  (:documentation "An object that can be applied onto another object's surface to cover or protect it."))

(defmethod copy-object-content (original can-be-lined-with) (copy can-be-lined-with)
  (setf (is-lining copy) (is-lining original)))


(defclass can-be-spread-upon (kitchen-entity)
  ((spread-with :type boolean
      :initarg :spread-with
      :accessor spread-with
      :initform nil))
  (:documentation "An object that can be spread upon others."))

(defmethod copy-object-content (original can-be-spread-upon) (copy can-be-spread-upon)
  (setf (spread-with copy) (spread-with original)))


(defclass can-be-sprinkled-on (kitchen-entity)
  ()
  (:documentation "An object upon which something can be sprinkled."))


(defclass can-be-sprinkled-with (kitchen-entity)
  ((sprinkled-with :type boolean
      :initarg :sprinkled-with
      :accessor sprinkled-with
      :initform nil))
  (:documentation "An object that can be sprinkled upon others."))

(defmethod copy-object-content (original can-be-sprinkled-with) (copy can-be-sprinkled-with)
  (setf (sprinkled-with copy) (sprinkled-with original)))


(defclass can-beat (cooking-utensil)
  ()
  (:documentation "A tool that can be used for beating ingredients together."))


(defclass can-brush (cooking-utensil)
  ()
  (:documentation "A tool that can be used for brushing."))


(defclass can-cover (cooking-utensil)
  ((covered-container :type boolean
      :initarg :covered-container
      :accessor covered-container
      :initform nil))
  (:documentation "An object that can be used to cover a coverable container."))

(defmethod copy-object-content (original can-cover) (copy can-cover)
  (setf (covered-container copy) (covered-container original)))


(defclass can-cut (cooking-utensil)
  ()
  (:documentation "A tool that can be used for cutting."))


(defclass can-drain (cooking-utensil)
  ()
  (:documentation "A tool that can be used for draining."))


(defclass can-flatten (cooking-utensil)
  ()
  (:documentation "A tool that can be used to flatten dough."))


(defclass can-have-on-top (kitchen-entity)
  ((has-on-top :type list
      :initarg :has-on-top
      :accessor has-on-top
      :initform nil))
  (:documentation "An object that, as part of a dish or food item, can have other ingredients on top of itself."))

(defmethod copy-object-content (original can-have-on-top) (copy can-have-on-top)
  (setf (has-on-top copy) (loop for item in (has-on-top original) collect (copy-object item))))


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


(defclass can-spread (cooking-utensil)
  ()
  (:documentation "A tool that can be used for spreading."))


(defclass caster-sugar (sugar)
  ()
  (:documentation "Finely granulated sugar."))


(defclass celery (ingredient cuttable)
  ((keep-refrigerated :initform T))
  (:documentation "Celery."))

(defmethod copy-object-content (original celery) (copy celery)
  (setf (keep-refrigerated copy) (keep-refrigerated original)))


(defclass cherry-tomato (ingredient cuttable)
  ((keep-refrigerated :initform T))
  (:documentation "Cherry tomato."))

(defmethod copy-object-content (original cherry-tomato) (copy cherry-tomato)
  (setf (keep-refrigerated copy) (keep-refrigerated original)))


(defclass chopped (cutting-pattern)
  ()
  (:documentation "A cutting pattern for objects that should be cut into somewhat small pieces."))


(defclass chopped-walnut (ingredient)
  ()
  (:documentation "Walnut (chopped)."))


(defclass clove (unit)
  ()
  (:documentation "Unit: clove."))


(defclass cm (unit)
  ()
  (:documentation "Unit: centimeter."))


(defclass cocoa-powder (ingredient siftable)
  ()
  (:documentation "Cocoa powder."))


(defclass colander (can-drain reusable)
  ()
  (:documentation "A colander, can be used to drain."))


(defclass conceptualizable (kitchen-entity)
  ((is-concept :type boolean
      :initarg :is-concept
      :accessor is-concept
      :initform nil))
  (:documentation "An object that can be used to stand for its class as a whole."))

(defmethod copy-object-content (original conceptualizable) (copy conceptualizable)
  (setf (is-concept copy) (is-concept original)))


(defclass conserved-ingredient (ingredient drainable)
  ((fluid-parts :type list
      :initarg :fluid-parts
      :accessor fluid-parts
      :initform nil)
  (solid-parts :type list
      :initarg :solid-parts
      :accessor solid-parts
      :initform nil))
  (:documentation "A canned ingredient that consists of a solid part and a liquid that is there to preserve the solid against alteration."))

(defmethod copy-object-content (original conserved-ingredient) (copy conserved-ingredient)
  (setf (fluid-parts copy) (loop for item in (fluid-parts original) collect (copy-object item)))
  (setf (solid-parts copy) (loop for item in (solid-parts original) collect (copy-object item))))


(defclass container (arrangeable)
  ((contents :type list
      :initarg :contents
      :accessor contents
      :initform nil))
  (:documentation "An object that can have contents; this is understood here somewhat loosely in that the contents are located at the containing object, even if the kinematic constraints between container and containees are weak."))

(defmethod copy-object-content (original container) (copy container)
  (setf (contents copy) (loop for item in (contents original) collect (copy-object item))))


(defclass cookie-sheet (transferable-container lineable brushable reusable)
  ()
  (:documentation "A cookie sheet, i.e. an oven-safe container for baking in."))


(defclass cooking-utensil (fetchable)
  ()
  (:documentation "A manipulable, carriable object used as an instrument in cooking tasks."))


(defclass corn-flakes (ingredient)
  ()
  (:documentation "Corn flakes."))


(defclass counter-top (container conceptualizable)
  ((arrangement :initform (make-instance 'side-to-side)))
  (:documentation "The countertop where various food preparation tasks will take place."))

(defmethod copy-object-content (original counter-top) (copy counter-top)
  (setf (arrangement copy) (copy-object (arrangement original))))


(defclass cover-pattern (pattern)
  ()
  (:documentation "A pattern in which something is covered."))


(defclass coverable-container (container)
  ((cover :type can-cover
      :initarg :cover
      :accessor cover
      :initform nil))
  (:documentation "A (loose) container that can also be covered."))

(defmethod copy-object-content (original coverable-container) (copy coverable-container)
  (setf (cover copy) (copy-object (cover original))))


(defclass crackable (kitchen-entity)
  ((cracked :type boolean
      :initarg :cracked
      :accessor cracked
      :initform nil))
  (:documentation "An object that can be cracked, e.g. by impact, in particular one whose contents become accessible through cracking."))

(defmethod copy-object-content (original crackable) (copy crackable)
  (setf (cracked copy) (cracked original)))


(defclass crescent-shape (shape)
  ()
  (:documentation "A crescent shape."))


(defclass cucumber (ingredient cuttable peelable seedable)
  ((keep-refrigerated :initform T))
  (:documentation "Cucumber."))

(defmethod copy-object-content (original cucumber) (copy cucumber)
  (setf (keep-refrigerated copy) (keep-refrigerated original)))


(defclass cup (unit)
  ()
  (:documentation "Unit: cup."))


(defclass cuttable (kitchen-entity)
  ((is-cut :type boolean
      :initarg :is-cut
      :accessor is-cut
      :initform nil))
  (:documentation "An object that can be cut into several pieces."))

(defmethod copy-object-content (original cuttable) (copy cuttable)
  (setf (is-cut copy) (is-cut original)))


(defclass cutting-pattern (pattern)
  ()
  (:documentation "A pattern in which something can be cut."))


(defclass degrees-celsius (unit)
  ()
  (:documentation "Unit: degrees Celsius."))


(defclass devils-food-cake-mix (ingredient)
  ()
  (:documentation "Devil's food cake mix."))


(defclass diced (cutting-pattern)
  ()
  (:documentation "A cutting pattern for objects that should be cut into more or less cubic pieces."))


(defclass dippable (kitchen-entity)
  ((dipped-in :type can-be-dipped-in
      :initarg :dipped-in
      :accessor dipped-in
      :initform nil))
  (:documentation "An object that can be dipped into something."))

(defmethod copy-object-content (original dippable) (copy dippable)
  (setf (dipped-in copy) (copy-object (dipped-in original))))


(defclass dough (homogeneous-mixture flattenable bakeable shapeable cuttable)
  ()
  (:documentation "A type of homogeneous mixture used to create pastry, bread, cakes and similar."))


(defclass drainable (kitchen-entity)
  ((drained :type boolean
      :initarg :drained
      :accessor drained
      :initform nil))
  (:documentation "An object that can be drained -- separated from some other object of a different consistency (a solid separated from liquids being a typical example)."))

(defmethod copy-object-content (original drainable) (copy drainable)
  (setf (drained copy) (drained original)))


(defclass dried-dill-weed (ingredient)
  ()
  (:documentation "Dried (and chopped) dill weed."))


(defclass dry-white-wine (ingredient cuttable)
  ()
  (:documentation "Dry white wine."))


(defclass egg (ingredient has-temperature crackable)
  ((keep-refrigerated :initform T))
  (:documentation "Egg (whole and with shell)."))

(defmethod copy-object-content (original egg) (copy egg)
  (setf (keep-refrigerated copy) (keep-refrigerated original)))


(defclass egg-shell (ingredient crackable)
  ()
  (:documentation "Eggshell."))


(defclass evenly-spread (arrangement-pattern)
  ()
  (:documentation "Items are arranged in an even spread over the available surface."))


(defclass fetchable (kitchen-entity)
  ()
  (:documentation "An object that can be grasped, manipulated and carried by a humanoid agent."))


(defclass finey-chopped (cutting-pattern)
  ()
  (:documentation "A cutting pattern for objects that should be cut into very small pieces."))


(defclass flattenable (shapeable)
  ((flattened :type boolean
      :initarg :flattened
      :accessor flattened
      :initform nil))
  (:documentation "An object that can be deformed, in particular flattened, by applying pressure to it."))

(defmethod copy-object-content (original flattenable) (copy flattenable)
  (setf (flattened copy) (flattened original)))


(defclass flattened-ball-shape (ball-shape)
  ()
  (:documentation "A ball shape which is slightly flattened."))


(defclass flavoring-extract (ingredient)
  ()
  (:documentation "Flavoring extract."))


(defclass flour (ingredient siftable can-be-sprinkled-on)
  ()
  (:documentation "Flour."))


(defclass fluid (ingredient)
  ()
  (:documentation "An object that is a fluid."))


(defclass fork (can-beat can-mash can-mix reusable)
  ()
  (:documentation "A fork; can be used for mixing, beating, mashing."))


(defclass freezer (container)
  ((arrangement :initform (make-instance 'shelved)))
  (:documentation "A freezer in which various items can be stored at sub-zero temperatures."))

(defmethod copy-object-content (original freezer) (copy freezer)
  (setf (arrangement copy) (copy-object (arrangement original))))


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


(defclass fridge (container)
  ((arrangement :initform (make-instance 'shelved)))
  (:documentation "A fridge in which various items can be stored at cooler, though not sub-zero, temperatures."))

(defmethod copy-object-content (original fridge) (copy fridge)
  (setf (arrangement copy) (copy-object (arrangement original))))


(defclass frozen-corn (ingredient has-temperature)
  ((keep-frozen :initform T))
  (:documentation "Frozen corn."))

(defmethod copy-object-content (original frozen-corn) (copy frozen-corn)
  (setf (keep-frozen copy) (keep-frozen original)))


(defclass g (unit)
  ()
  (:documentation "Unit: gram."))


(defclass garlic (ingredient cuttable)
  ()
  (:documentation "Garlic."))


(defclass ground-allspice (ground-spice)
  ()
  (:documentation "Ground allspice."))


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


(defclass ground-spice (spice siftable can-be-sprinkled-with)
  ()
  (:documentation "Ground spice."))


(defclass halved (cutting-pattern)
  ()
  (:documentation "A cutting pattern for objects that should be cut into two more or less equal pieces."))


(defclass handful (unit)
  ()
  (:documentation "Unit: handful."))


(defclass has-temperature (kitchen-entity)
  ((temperature :type integer
      :initarg :temperature
      :accessor temperature
      :initform nil))
  (:documentation "An object that has a temperature quality."))

(defmethod copy-object-content (original has-temperature) (copy has-temperature)
  (setf (temperature copy) (temperature original)))


(defclass heterogeneous-mixture (mixture)
  ((components :type list
      :initarg :components
      :accessor components
      :initform nil))
  (:documentation "A mixture in which components are distinguishable from one another."))

(defmethod copy-object-content (original heterogeneous-mixture) (copy heterogeneous-mixture)
  (setf (components copy) (loop for item in (components original) collect (copy-object item))))


(defclass homogeneous-mixture (mixture flattenable)
  ()
  (:documentation "A homogeneous mixture: parts are indistinguishable from each other."))


(defclass icing-sugar (sugar)
  ()
  (:documentation "Icing sugar."))


(defclass ingredient (fetchable conceptualizable perishable)
  ((amount :type amount
      :initarg :amount
      :accessor amount
      :initform nil))
  (:documentation "An object that is an ingredient in a recipe, and associated to an amount."))

(defmethod copy-object-content (original ingredient) (copy ingredient)
  (setf (amount copy) (copy-object (amount original))))


(defclass jalapeno (ingredient cuttable seedable)
  ((keep-refrigerated :initform T))
  (:documentation "Jalapeno pepper."))

(defmethod copy-object-content (original jalapeno) (copy jalapeno)
  (setf (keep-refrigerated copy) (keep-refrigerated original)))


(defclass jar (transferable-container coverable-container reusable shakeable)
  ()
  (:documentation "A (usually glass) jar to store various ingredients in."))


(defclass jar-lid (can-cover reusable)
  ()
  (:documentation "A jar lid. Used to cover/close a jar."))


(defclass juice (fluid)
  ()
  (:documentation "A liquid produced usually from some fruit or vegetable."))


(defclass kitchen-cabinet (container)
  ((arrangement :initform (make-instance 'shelved)))
  (:documentation "The kitchen cabinet, where several utensils are stored."))

(defmethod copy-object-content (original kitchen-cabinet) (copy kitchen-cabinet)
  (setf (arrangement copy) (copy-object (arrangement original))))


(defclass kitchen-entity (entity)
  ((id :type symbol
      :initarg :id
      :accessor id
      :initform nil)
  (persistent-id :type symbol
      :initarg :persistent-id
      :accessor persistent-id
      :initform nil))
  (:documentation "Abstract class for all kitchen entities. All items in the cookingbot ontology should subclass of kitchen-entity."))

(defmethod initialize-instance :after ((orig kitchen-entity) &key)
  (let ((persistent-id (make-id (type-of orig))))
    (setf (persistent-id orig) persistent-id)
    (setf (id orig) (make-id persistent-id))))

(defmethod copy-object-content (original kitchen-entity) (copy kitchen-entity)
  (setf (id copy) (make-id (id original)))
  (setf (persistent-id copy) (persistent-id original)))


(defclass kitchen-state (container)
  ((kitchen-time :type integer
      :initarg :kitchen-time
      :accessor kitchen-time
      :initform 0))
  (:documentation "Representation of the state of the kitchen."))

(defmethod initialize-instance :after ((orig kitchen-state) &key)
  (when (null (arrangement orig)) (setf (arrangement orig) (make-instance 'sectionalized)))
  (when (null (find 'kitchen-cabinet (contents orig) :test (lambda (x y) (eq x (type-of y))))) (setf (contents orig) (cons (make-instance 'kitchen-cabinet) (contents orig))))
  (when (null (find 'pantry (contents orig) :test (lambda (x y) (eq x (type-of y))))) (setf (contents orig) (cons (make-instance 'pantry) (contents orig))))
  (when (null (find 'fridge (contents orig) :test (lambda (x y) (eq x (type-of y))))) (setf (contents orig) (cons (make-instance 'fridge) (contents orig))))
  (when (null (find 'freezer (contents orig) :test (lambda (x y) (eq x (type-of y))))) (setf (contents orig) (cons (make-instance 'freezer) (contents orig))))
  (when (null (find 'oven (contents orig) :test (lambda (x y) (eq x (type-of y))))) (setf (contents orig) (cons (make-instance 'oven) (contents orig))))
  (when (null (find 'counter-top (contents orig) :test (lambda (x y) (eq x (type-of y))))) (setf (contents orig) (cons (make-instance 'counter-top) (contents orig)))))

(defmethod copy-object-content (original kitchen-state) (copy kitchen-state)
  (setf (kitchen-time copy) (kitchen-time original))
  (setf (arrangement copy) (copy-object (arrangement original)))
  (setf (contents copy) (loop for item in (contents original) collect (copy-object item))))


(defclass knife (can-cut can-peel can-seed can-spread reusable)
  ()
  (:documentation "A knife. Can be used to cut, peel, take seeds out or spread substances such as butter or honey."))


(defclass l (unit)
  ()
  (:documentation "Unit: liter."))


(defclass large-bowl (bowl)
  ()
  (:documentation "A larger bowl to hold ingredients at various stages of preparation."))


(defclass large-bowl-lid (bowl-lid)
  ()
  (:documentation "A larger bowl lid used to cover a large bowl."))


(defclass lentils (ingredient)
  ()
  (:documentation "Some kind of lentil."))


(defclass lid-on-angle (cover-pattern)
  ()
  (:documentation "A pattern in which a container is partially covered by putting its lid on an angle."))


(defclass lime-juice (juice)
  ()
  (:documentation "Lime juice."))


(defclass lineable (kitchen-entity)
  ((lined-with :type can-be-lined-with
      :initarg :lined-with
      :accessor lined-with
      :initform nil))
  (:documentation "An object with an inner surface that can be lined -- i.e. covered in close contact -- with something."))

(defmethod copy-object-content (original lineable) (copy lineable)
  (setf (lined-with copy) (copy-object (lined-with original))))


(defclass linguine (ingredient)
  ()
  (:documentation "Linguine."))


(defclass list-of-kitchen-entities (kitchen-entity)
  ((items :type list
      :initarg :items
      :accessor items
      :initform nil))
  (:documentation "Auxiliary type to store a list of kitchen entities."))

(defmethod copy-object-content (original list-of-kitchen-entities) (copy list-of-kitchen-entities)
  (setf (items copy) (loop for item in (items original) collect (copy-object item))))


(defclass low-heat (unit)
  ()
  (:documentation "Unit: low heat."))


(defclass mashable (kitchen-entity)
  ((mashed :type boolean
      :initarg :mashed
      :accessor mashed
      :initform nil))
  (:documentation "An object that can be mashed."))

(defmethod copy-object-content (original mashable) (copy mashable)
  (setf (mashed copy) (mashed original)))


(defclass medium-bowl (bowl)
  ()
  (:documentation "A medium-sized bowl to hold ingredients at various stages of preparation."))


(defclass medium-bowl-lid (bowl-lid)
  ()
  (:documentation "A lid used to cover a medium-sized bowl."))


(defclass meltable (kitchen-entity)
  ((melted :type boolean
      :initarg :melted
      :accessor melted
      :initform nil))
  (:documentation "An object that can melt when brought to a high enough temperature."))

(defmethod copy-object-content (original meltable) (copy meltable)
  (setf (melted copy) (melted original)))


(defclass milk (ingredient)
  ((keep-refrigerated :initform T))
  (:documentation "Milk."))

(defmethod copy-object-content (original milk) (copy milk)
  (setf (keep-refrigerated copy) (keep-refrigerated original)))


(defclass minced (cutting-pattern)
  ()
  (:documentation "A cutting pattern for objects that should be cut into very small pieces, to the point that the pieces are hard to distinguish from each other."))


(defclass minute (unit)
  ()
  (:documentation "Unit: minute."))


(defclass mixable (kitchen-entity)
  ((mixed :type boolean
      :initarg :mixed
      :accessor mixed
      :initform nil))
  (:documentation "An object that can be mixed."))

(defmethod copy-object-content (original mixable) (copy mixable)
  (setf (mixed copy) (mixed original)))


(defclass mixture (ingredient beatable mashable mixable can-be-sprinkled-with siftable dippable spreadable can-be-sprinkled-on can-be-spread-upon has-temperature shakeable)
  ()
  (:documentation "An object that is a mixture of ingredients."))


(defclass ml (unit)
  ()
  (:documentation "Unit: milliliter."))


(defclass molasses (fluid)
  ()
  (:documentation "Molasses."))


(defclass oats (ingredient)
  ()
  (:documentation "Oats."))


(defclass olive-oil (vegetable-oil)
  ()
  (:documentation "Olive oil."))


(defclass on-angle (arrangement-pattern)
  ()
  (:documentation "Items are arranged to form a particular angle."))


(defclass onion (ingredient cuttable)
  ()
  (:documentation "Onion."))


(defclass oven (container has-temperature)
  ((arrangement :initform (make-instance 'shelved)))
  (:documentation "An oven to bake stuff in."))

(defmethod copy-object-content (original oven) (copy oven)
  (setf (arrangement copy) (copy-object (arrangement original))))


(defclass pan (brushable can-be-sprinkled-on reusable spreadable transferable-container)
  ()
  (:documentation "A frying pan to fry stuff in."))


(defclass pancetta (ingredient cuttable)
  ()
  (:documentation "Pancetta."))


(defclass pantry (container)
  ((arrangement :initform (make-instance 'shelved)))
  (:documentation "A pantry to store ingredients that do not need refrigeration."))

(defmethod copy-object-content (original pantry) (copy pantry)
  (setf (arrangement copy) (copy-object (arrangement original))))


(defclass pattern (conceptualizable)
  ((is-concept :initform T))
  (:documentation "A geometric pattern, such as that of an arrangement."))

(defmethod copy-object-content (original pattern) (copy pattern)
  (setf (is-concept copy) (is-concept original)))


(defclass peasized-cubes (diced)
  ()
  (:documentation "A cutting pattern for objects that should be cut into somewhat cubic small pieces."))


(defclass peelable (kitchen-entity)
  ((peeled :type boolean
      :initarg :peeled
      :accessor peeled
      :initform nil))
  (:documentation "An object that can be peeled."))

(defmethod copy-object-content (original peelable) (copy peelable)
  (setf (peeled copy) (peeled original)))


(defclass percent (unit)
  ()
  (:documentation "Unit: percent."))


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

(defmethod copy-object-content (original perishable) (copy perishable)
  (setf (keep-frozen copy) (keep-frozen original))
  (setf (keep-refrigerated copy) (keep-refrigerated original)))


(defclass piece (unit)
  ()
  (:documentation "Unit: piece."))


(defclass pluckable (kitchen-entity)
  ((is-plucked :type boolean
      :initarg :is-plucked
      :accessor is-plucked
      :initform nil)
  (plucked :type ingredient
      :initarg :plucked
      :accessor plucked
      :initform nil)
  (pluckee :type ingredient
      :initarg :pluckee
      :accessor pluckee
      :initform nil))
  (:documentation "An object that can be plucked such as fresh herbs."))

(defmethod copy-object-content (original pluckable) (copy pluckable)
  (setf (is-plucked copy) (is-plucked original))
  (setf (plucked copy) (copy-object (plucked original)))
  (setf (pluckee copy) (copy-object (pluckee original))))


(defclass powdered-white-sugar (sugar can-be-sprinkled-with can-be-dipped-in)
  ()
  (:documentation "Powdered white sugar."))


(defclass quantity (kitchen-entity)
  ((value :type number
      :initarg :value
      :accessor value
      :initform 1))
  (:documentation "Stores a value."))

(defmethod copy-object-content (original quantity) (copy quantity)
  (setf (value copy) (value original)))


(defclass raisin (ingredient)
  ()
  (:documentation "Rain(s)."))


(defclass red-chilipepper (ingredient)
  ()
  (:documentation "Red hot chili pepper."))


(defclass red-onion (ingredient cuttable)
  ()
  (:documentation "Red onion."))


(defclass red-pepper-flakes (spice)
  ()
  (:documentation "Red pepper flakes."))


(defclass reusable (kitchen-entity)
  ((used :type boolean
      :initarg :used
      :accessor used
      :initform nil))
  (:documentation "An object that can be reused; reuse may involve cleaning."))

(defmethod copy-object-content (original reusable) (copy reusable)
  (setf (used copy) (used original)))


(defclass rolling-pin (can-flatten reusable)
  ()
  (:documentation "A rolling pin to roll dough into flat shapes."))


(defclass salt (spice)
  ()
  (:documentation "Table salt."))


(defclass saucepan (transferable-container reusable)
  ()
  (:documentation "A sauce pan to cook food in."))


(defclass sectionalized (arrangement-pattern)
  ()
  (:documentation "Items are arranged such that they fill sections."))


(defclass seedable (kitchen-entity)
  ((seeded :type boolean
      :initarg :seeded
      :accessor seeded
      :initform nil))
  (:documentation "A fruit with seeds that can be removed."))

(defmethod copy-object-content (original seedable) (copy seedable)
  (setf (seeded copy) (seeded original)))


(defclass self-rising-flour (flour)
  ()
  (:documentation "Mixture of all-purpose flour, baking powder, salt."))


(defclass semisweet-chocolate-chips (ingredient)
  ()
  (:documentation "Semisweet chocolate chips."))


(defclass shakeable (kitchen-entity)
  ((shaken :type boolean
      :initarg :shaken
      :accessor shaken
      :initform nil))
  (:documentation "An object that can be mixed in a particular way, by shaking."))

(defmethod copy-object-content (original shakeable) (copy shakeable)
  (setf (shaken copy) (shaken original)))


(defclass shallot (ingredient cuttable peelable)
  ()
  (:documentation "Shallot."))


(defclass shape (conceptualizable)
  ((is-concept :initform T))
  (:documentation "A geometric shape, such as that of a clump of dough."))

(defmethod copy-object-content (original shape) (copy shape)
  (setf (is-concept copy) (is-concept original)))


(defclass shapeable (kitchen-entity)
  ((current-shape :type shape
      :initarg :current-shape
      :accessor current-shape
      :initform nil))
  (:documentation "An object that can be shaped through manipulation."))

(defmethod copy-object-content (original shapeable) (copy shapeable)
  (setf (current-shape copy) (copy-object (current-shape original))))


(defclass shelved (arrangement-pattern)
  ()
  (:documentation "Items are arranged such that all shelves are subsequently filled."))


(defclass side-to-side (arrangement-pattern)
  ()
  (:documentation "Items are arranged such that their sides face each other."))


(defclass sift (can-sift reusable)
  ()
  (:documentation "A tool used for sifting."))


(defclass siftable (kitchen-entity)
  ((sifted :type boolean
      :initarg :sifted
      :accessor sifted
      :initform nil))
  (:documentation "An object that can be separated into various components, or separated from some powdery substance, by sifting."))

(defmethod copy-object-content (original siftable) (copy siftable)
  (setf (sifted copy) (sifted original)))


(defclass sliced (cutting-pattern)
  ()
  (:documentation "A cutting pattern for objects that should be cut into thin flat pieces."))


(defclass small-bowl (bowl)
  ()
  (:documentation "A small-sized bowl to hold ingredients at various stages of preparation."))


(defclass small-bowl-lid (bowl-lid)
  ()
  (:documentation "A lid used to cover a small-sized bowl."))


(defclass spatula (can-spread reusable)
  ()
  (:documentation "A spatula that can be used to flip items over or spread ingredients."))


(defclass spice (ingredient)
  ()
  (:documentation "Spice."))


(defclass spreadable (kitchen-entity)
  ((spread :type boolean
      :initarg :spread
      :accessor spread
      :initform nil))
  (:documentation "An object providing a surface upon which something can be spread."))

(defmethod copy-object-content (original spreadable) (copy spreadable)
  (setf (spread copy) (spread original)))


(defclass stalk (unit)
  ()
  (:documentation "Unit: stalk."))


(defclass stove (container)
  ((arrangement :initform (make-instance 'side-to-side)))
  (:documentation "A stove to cook on."))

(defmethod copy-object-content (original stove) (copy stove)
  (setf (arrangement copy) (copy-object (arrangement original))))


(defclass sugar (ingredient mixable beatable)
  ()
  (:documentation "Sugar."))


(defclass sweet-potato (ingredient cuttable peelable)
  ()
  (:documentation "Sweet potato."))


(defclass table-spoon (can-spread reusable)
  ()
  (:documentation "A table spoon, used for eating liquid or mostly liquid food, but can also be used for other purposes such as spreading jam."))


(defclass tablespoon (unit)
  ()
  (:documentation "Unit: tablespoon."))


(defclass teaspoon (unit)
  ()
  (:documentation "Unit: teaspoon."))


(defclass thin-slivers (cutting-pattern)
  ()
  (:documentation "A cutting pattern for objects that should be cut into very thin slices."))


(defclass three-quarter-inch-cubes (diced)
  ()
  (:documentation "A cutting pattern for objects that should be cut into somewhat cubic pieces of about 3/4ths of an inch on the side."))


(defclass toast (ingredient spreadable bakeable can-have-on-top)
  ()
  (:documentation "Toast."))


(defclass transferable-container (container fetchable conceptualizable)
  ((used :type boolean
      :initarg :used
      :accessor used
      :initform nil))
  (:documentation "A container that can be carried from place to place."))

(defmethod copy-object-content (original transferable-container) (copy transferable-container)
  (setf (used copy) (used original)))


(defclass two-inch-apart (arrangement-pattern)
  ()
  (:documentation "Items are arranged to be two inches apart from each other."))


(defclass unit (conceptualizable)
  ((is-concept :initform T))
  (:documentation "A unit of measurement."))

(defmethod copy-object-content (original unit) (copy unit)
  (setf (is-concept copy) (is-concept original)))


(defclass unordered-heap (arrangement-pattern)
  ()
  (:documentation "Items are randomly piled into a heap."))


(defclass vanilla (ingredient)
  ()
  (:documentation "Vanilla."))


(defclass vanilla-extract (flavoring-extract vanilla)
  ()
  (:documentation "Vanilla extract."))


(defclass vegetable-oil (ingredient can-be-brushed-with)
  ()
  (:documentation "Vegetable oil."))


(defclass very-low-heat (unit)
  ()
  (:documentation "Unit: very low heat."))


(defclass vinegar (fluid)
  ()
  (:documentation "Vinegar."))


(defclass vr-kitchen-state (container)
  ()
  (:documentation "Representation of the Abe_sim kitchen state."))


(defclass walnut-ball-shape (ballshape)
  ()
  (:documentation "A walnut ball shape."))


(defclass water (fluid has-temperature)
  ()
  (:documentation "Water."))


(defclass whisk (can-beat can-mix reusable)
  ()
  (:documentation "A whisk, which is a tool for beating and mixing."))


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


(defclass wire-rack (transferable-container lineable reusable)
  ()
  (:documentation "Wire rack for cooking on."))


(defclass wooden-spoon (can-mix reusable)
  ()
  (:documentation "A wooden spoon, typically used to mix soup."))


