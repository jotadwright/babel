(in-package :muhai-cookingbot)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                             ;;
;; This file contains the ontology underlying the cooking bot. ;;
;;                                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; General ;;
;;;;;;;;;;;;;

(defclass kitchen-entity (entity)
  ((persistent-id :type symbol :initarg :persistent-id :accessor persistent-id :initform nil))
  (:documentation "Abstract class for all kitchen entities. All items
in the cookingbot ontology should subclass of kitchen-entity."))

(defmethod initialize-instance :after ((kitchen-entity kitchen-entity) &key)
  (let ((persistent-id  (make-id (type-of kitchen-entity))))
    (setf (persistent-id kitchen-entity) persistent-id)
    (setf (id kitchen-entity) (make-id persistent-id))))

(defmethod copy-object-content ((kitchen-entity kitchen-entity) (copy kitchen-entity))
  (setf (persistent-id copy) (persistent-id kitchen-entity))
  (setf (id copy)  (make-id (persistent-id kitchen-entity))))

(defclass kitchen-state (container)
  ((kitchen-time :type integer
                 :accessor kitchen-time
                 :initarg :kitchen-time
                 :initform 0))
  (:documentation "Representation of the state of the kitchen."))

(defmethod copy-object-content ((kitchen-state kitchen-state) (copy kitchen-state))
  (setf (kitchen-time copy) (kitchen-time kitchen-state))
  )

(defclass vr-kitchen-state (container)
  ()
  (:documentation "Representation of the state of the Abe_sim kitchen."))

(defmethod initialize-instance :after ((kitchen-state kitchen-state) &key)
  (when (null (arrangement kitchen-state))
    (setf (arrangement kitchen-state) (make-instance 'sectionalized)))
  (let ((counter-top-instance (find-in-kitchen-state-contents kitchen-state 'counter-top))
        (pantry-instance (find-in-kitchen-state-contents kitchen-state 'pantry))
        (fridge-instance (find-in-kitchen-state-contents kitchen-state 'fridge))
        (freezer-instance (find-in-kitchen-state-contents kitchen-state 'freezer))
        (oven-instance (find-in-kitchen-state-contents kitchen-state 'oven))
        (kitchen-cabinet-instance (find-in-kitchen-state-contents kitchen-state 'kitchen-cabinet)))
    (when (null kitchen-cabinet-instance) (setf (contents kitchen-state) (cons (make-instance 'kitchen-cabinet) (contents kitchen-state))))
    (when (null pantry-instance) (setf (contents kitchen-state) (cons (make-instance 'pantry) (contents kitchen-state))))
    (when (null fridge-instance) (setf (contents kitchen-state) (cons (make-instance 'fridge) (contents kitchen-state))))
    (when (null freezer-instance) (setf (contents kitchen-state) (cons (make-instance 'freezer) (contents kitchen-state))))
    (when (null oven-instance) (setf (contents kitchen-state) (cons (make-instance 'oven) (contents kitchen-state))))
    (when (null counter-top-instance) (setf (contents kitchen-state) (cons (make-instance 'counter-top) (contents kitchen-state))))))

;; Readers for kitchen-state contents
(defmethod find-in-kitchen-state-contents ((kitchen-state kitchen-state) (classname symbol))
  (loop for item in (contents kitchen-state)
        when (eq (type-of item) classname)
        return item))

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

(defmethod kitchen-cabinet ((kitchen-state kitchen-state))
  (find-in-kitchen-state-contents kitchen-state 'kitchen-cabinet))

(defmethod stove ((kitchen-state kitchen-state))
  (find-in-kitchen-state-contents kitchen-state 'stove))

;; Abstract classes for properties ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass aggregate (kitchen-entity)
  ()
  (:documentation "For ingredients that are formed by combining several separate elements in VR, e.g. butter or liquids"))

(defclass arrangeable (kitchen-entity)
  ((arrangement  :initarg :arrangement :accessor arrangement :initform nil))
  (:documentation "For objects that can have an arrangement."))

(defmethod copy-object-content ((arrangeable arrangeable) (copy arrangeable))
  "Copying arrangeable objects."
  (setf (arrangement copy) (copy-object (arrangement arrangeable))))

(defclass bakeable (has-temperature)
  ((baked :type boolean :initarg :baked :accessor baked :initform nil))
  (:documentation "For objects that can be baked"))

(defmethod copy-object-content ((bakeable bakeable) (copy bakeable))
  "Copying bakeable objects."
  (setf (baked copy) (copy-object (baked bakeable))))

(defclass beatable (kitchen-entity)
  ((beaten :type boolean :initarg :beaten :accessor beaten :initform nil))
  (:documentation "For objects that can be beaten."))

(defmethod copy-object-content ((beatable beatable) (copy beatable))
  "Copying beatable objects."
  (setf (beaten copy) (copy-object (beaten beatable))))

(defclass brushable (kitchen-entity)
  ((brushed-with  :initarg :brushed-with :accessor brushed-with :initform nil))
  (:documentation "Something of which the inner surface can be brushed with something that can-be-brushed-with."))

(defmethod copy-object-content ((brushable brushable) (copy brushable))
  "Copying brushable object."
  (setf (brushed-with copy) (copy-object (brushed-with brushable)))) 

(defclass can-beat (cooking-utensil)
  ()
  (:documentation "A tool that can be used for beating."))

(defclass can-be-brushed-with (kitchen-entity)
  ((is-brushed-with :type boolean :initarg :is-brushed-with :accessor is-brushed-with :initform nil))
  (:documentation "Something to brush the inner surface of something with."))

(defmethod copy-object-content ((can-be-brushed-with can-be-brushed-with) (copy can-be-brushed-with))
  "Copying can-be-brushed-with objects."
  (setf (is-brushed-with copy) (copy-object (is-brushed-with can-be-brushed-with))))

(defclass can-be-lined-with (cooking-utensil)
  ((is-lining :type boolean :initarg :is-lining :accessor is-lining :initform nil))
  (:documentation "Something to cover the inner surface of something with."))

(defmethod copy-object-content ((can-be-lined-with can-be-lined-with) (copy can-be-lined-with))
  "Copying can-be-lined-with objects."
  (setf (is-lining copy) (copy-object (is-lining can-be-lined-with))))

(defclass can-be-dipped-in (kitchen-entity)
  ()
  (:documentation "Something in which something can can be dipped."))

(defclass can-have-on-top (kitchen-entity)
  ((has-on-top :initarg :has-on-top :accessor has-on-top :initform nil))
  (:documentation "Something which can have something on top."))

(defmethod copy-object-content ((can-have-on-top can-have-on-top) (copy can-have-on-top))
  "Copying can-have-on-top objects."
  (setf (has-on-top copy) (copy-object (has-on-top can-have-on-top))))

(defclass can-be-spread-upon (kitchen-entity)
  ((spread-with :initarg :spread-with :accessor spread-with :initform nil))
  (:documentation "Something that can be spread upon."))

(defmethod copy-object-content ((can-be-spread-upon can-be-spread-upon) (copy can-be-spread-upon))
  "Copying can-be-spread-upon objects."
  (setf (spread-with copy) (copy-object (spread-with can-be-spread-upon))))

(defclass can-be-sprinkled-with (kitchen-entity)
  ()
  (:documentation "Something that can be sprinkled over something."))

(defclass can-be-sprinkled-on (kitchen-entity)
  ((sprinkled-with  :initarg :sprinkled-with :accessor sprinkled-with :initform nil))
  (:documentation "For objects that can be sprinkled on."))

(defmethod copy-object-content ((can-be-sprinkled-on can-be-sprinkled-on) (copy can-be-sprinkled-on))
  "Copying can-be-sprinkled-on objects."
  (setf (sprinkled-with copy) (copy-object (sprinkled-with can-be-sprinkled-on))))

(defclass can-cover(cooking-utensil)
  ((covered-container :type boolean :initarg :covered-container :accessor covered-container :initform nil))
  (:documentation "Something that can be used to cover a coverable container"))

(defmethod copy-object-content ((can-cover can-cover) (copy can-cover))
  "Copying coverable objects."
  (setf (covered-container copy) (copy-object (covered-container can-cover))))

(defclass can-brush (cooking-utensil)
  ()
  (:documentation "A tool that can be used to brush."))

(defclass can-cut(cooking-utensil)
  ()
  (:documentation "A tool that can be used for cutting."))

(defclass can-drain (cooking-utensil)
  ()
  (:documentation "A tool that can drain"))

(defclass can-mash (cooking-utensil)
  ()
  (:documentation "A tool that can be used for mashing."))

(defclass can-mix (cooking-utensil)
  ()
  (:documentation "A tool that can be used for beating."))

(defclass can-peel(cooking-utensil)
  ()
  (:documentation "A tool that can be used for peeling."))

(defclass can-seed(cooking-utensil)
  ()
  (:documentation "A tool that can be used for seeding."))

(defclass can-sift(cooking-utensil)
  ()
  (:documentation "A tool that can be used for sifting."))

(defclass can-spread (cooking-utensil)
  ()
  (:documentation "A tool that can be used for spreading."))

(defclass conceptualizable (kitchen-entity)
  ((is-concept :type boolean :initarg :is-concept :accessor is-concept :initform nil))
  (:documentation "For objects that can be a concept."))

(defmethod copy-object-content ((conceptualizable conceptualizable) (copy conceptualizable))
  "Copying conceptualizable objects."
  (setf (is-concept copy) (copy-object (is-concept conceptualizable))))

(defclass container (arrangeable)
  ((contents :type list :initarg :contents :accessor contents :initform '()))
  (:documentation "For objects that are containers (i.e. they have contents)."))

(defmethod copy-object-content ((container container) (copy container))
  "Copying containers."
  (setf (contents copy) (copy-object (contents container))))

(defclass coverable-container (container)
  ((cover :initarg :cover :accessor cover :initform nil))
  (:documentation "Containers that can also be covered"))

(defmethod copy-object-content ((coverable-container coverable-container) (copy coverable-container))
  "Copying coverable objects."
  (setf (cover copy) (copy-object (cover coverable-container))))


(defclass crackable (kitchen-entity)
  ((cracked :type boolean :initarg :cracked :accessor cracked :initform nil))
  (:documentation "For objects that can be cracked."))

(defmethod copy-object-content ((crackable crackable) (copy crackable))
  "Copying crackable objects."
  (setf (cracked copy) (copy-object (cracked crackable))))

(defclass cuttable (kitchen-entity)
  ((is-cut :type boolean :initarg :is-cut :accessor is-cut :initform nil))
  (:documentation "For objects that can be cut."))

(defmethod copy-object-content ((cuttable cuttable) (copy cuttable))
  "Copying cuttable objects."
  (setf (is-cut copy) (copy-object (is-cut cuttable))))

(defclass dippable (kitchen-entity)
  ((dipped-in  :initarg :dipped-in :accessor dipped-in :initform nil))
  (:documentation "Something of which the inner surface can be covered with something that can-be-lined-with."))

(defmethod copy-object-content ((dippable dippable) (copy dippable))
  "Copying dippable objects."
  (setf (dipped-in copy) (copy-object (dipped-in dippable))))

(defclass dough (homogeneous-mixture)
  ()
  (:documentation "Dough. Type of homogenous mixture."))

(defclass drainable (kitchen-entity)
  ((drained :type boolean :initarg :drained :accessor drained :initform nil))
  (:documentation "Something that can be drained."))

(defmethod copy-object-content ((drainable drainable) (copy drainable))
  "Copying drainable objects."
  (setf (drained copy) (copy-object (drained drainable))))

(defclass fetchable (kitchen-entity)
  ()
  (:documentation "For objects that can be fetched."))

(defclass fluid (kitchen-entity)
  ()
  (:documentation "An ingredient that is fluid."))
  
(defclass has-temperature (kitchen-entity)                                                      
  ((temperature  :initarg :temperature :accessor temperature :initform nil))
  (:documentation "For object/containers with a temperature."))

(defmethod copy-object-content ((has-temperature  has-temperature) (copy has-temperature))      
  "Copying  objects with temperature."
  (setf (temperature copy) (copy-object (temperature has-temperature))))

(defclass ingredient (fetchable conceptualizable perishable)
  ((amount :type amount :initarg :amount :accessor amount :initform (make-instance 'amount)))
  (:documentation "For objects that are ingredients (they have an amount)."))

(defmethod copy-object-content ((ingredient ingredient) (copy ingredient))
  "Copying ingredients."
  (setf (amount copy) (copy-object (amount ingredient))))

(defclass list-of-kitchen-entities (kitchen-entity)
  ((items :type list :initarg :items :accessor items :initform nil)))

(defmethod copy-object-content ((list list-of-kitchen-entities) (copy list-of-kitchen-entities))
  "Copying list of kitchen entities."
  (setf (items copy) (copy-object (items list))))


(defclass lineable (kitchen-entity)
  ((lined-with :initarg :lined-with :accessor lined-with :initform nil))
  (:documentation "Something of which the inner surface can be covered with something that can-be-lined-with."))

(defmethod copy-object-content ((lineable lineable) (copy lineable))
  "Copying lineable objects."
  (setf (lined-with copy) (copy-object (lined-with lineable))))

(defclass mashable (kitchen-entity)
  ((mashed :type boolean :initarg :mashed :accessor mashed :initform nil))
  (:documentation "For objects that can be mashed."))

(defmethod copy-object-content ((mashable mashable) (copy mashable))
  "Copying mashable objects."
  (setf (mashed copy) (copy-object (mashed mashable))))

(defclass meltable (kitchen-entity)
  ((melted :type boolean :initarg :melted :accessor melted :initform nil))
  (:documentation "For objects that can be melted."))

(defmethod copy-object-content ((meltable meltable) (copy meltable))
  "Copying meltable objects."
  (setf (melted copy) (copy-object (melted meltable))))

(defclass mixable (kitchen-entity)
  ((mixed :type boolean :initarg :mixed :accessor mixed :initform nil))
  (:documentation "For objects that can be mixed."))

(defmethod copy-object-content ((mixable mixable) (copy mixable))
  "Copying mixable objects."
  (setf (mixed copy) (copy-object (mixed mixable))))

(defclass peelable (kitchen-entity)
  ((peeled :type boolean :initarg :peeled :accessor peeled :initform nil))
  (:documentation "For objects that can be peeled."))

(defmethod copy-object-content ((peelable peelable) (copy peelable))
  "Copying peelable objects."
  (setf (peeled copy) (copy-object (peeled peelable))))

(defclass perishable (kitchen-entity)
  ((keep-refrigerated :type boolean :initarg :keep-refrigerated :accessor keep-refrigerated :initform nil)
   (keep-frozen :type boolean :initarg :keep-frozen :accessor keep-frozen :initform nil))
  (:documentation "Something that is perishable and might need to be refrigerated."))

(defmethod copy-object-content ((perishable perishable) (copy perishable))
  "Copying perishable objects."
  (setf (keep-refrigerated copy) (copy-object (keep-refrigerated perishable))))


(defclass pluckable (kitchen-entity)
  ((is-plucked :type boolean :initarg :is-plucked :accessor is-plucked :initform nil)
   (plucked :type ingredient :initarg :plucked :accessor plucked :initform nil) ;;leaves
   (pluckee :type ingredient :initarg :pluckee :accessor pluckee :initform nil)) ;;sprig
  (:documentation "For objects that can be plucked such as fresh herbs"))

(defmethod copy-object-content ((pluckable pluckable) (copy pluckable))
  "Copying pluckable objects."
  (setf (is-plucked copy) (copy-object (is-plucked pluckable)))
  (setf (plucked copy) (copy-object (plucked pluckable)))
  (setf (pluckee copy) (copy-object (pluckee pluckable))))

(defclass reusable (kitchen-entity)
  ((used :type boolean :initarg :used :accessor used :initform nil))
  (:documentation "For objects that can be reused (and might not cleaning first)."))

(defmethod copy-object-content ((reusable reusable) (copy reusable))
  "Copying reusable objects."
  (setf (used copy) (copy-object (used reusable))))

(defclass seedable (kitchen-entity)
  ((seeded :type boolean :initarg :seeded :accessor seeded :initform nil))
  (:documentation "For objects that can be seeded."))

(defmethod copy-object-content ((seedable seedable) (copy seedable))
  "Copying seedable objects."
  (setf (seeded copy) (copy-object (seeded seedable))))

(defclass shakeable (kitchen-entity)
  ((shaken :type boolean :initarg :shaken :accessor shaken :initform nil))
  (:documentation "For objects that can be shaken."))

(defmethod copy-object-content ((shakeable shakeable) (copy shakeable))
  "Copying shakeable objects."
  (setf (shaken copy) (copy-object (shaken shakeable))))

(defclass shapeable (kitchen-entity)
  ((current-shape :initarg :current-shape :accessor current-shape :initform nil))
  (:documentation "For objects that can be shaped."))

(defmethod copy-object-content ((shapeable shapeable) (copy shapeable))
  "Copying shapeable objects."
  (setf (current-shape copy) (copy-object (current-shape shapeable))))

(defclass siftable (kitchen-entity)
  ((sifted :type boolean :initarg :sifted :accessor sifted :initform nil))
  (:documentation "For objects that can be sifted"))

(defmethod copy-object-content ((siftable siftable) (copy siftable))
  "Copying siftable objects."
  (setf (sifted copy) (copy-object (sifted siftable))))

(defclass spreadable  (kitchen-entity)
  ((spread :type boolean :initarg :spread :accessor spread :initform nil))
  (:documentation "For objects that can be spread"))

(defmethod copy-object-content ((spreadable spreadable) (copy spreadable))
  "Copying spreadable objects."
  (setf (spread copy) (copy-object (spread  spreadable))))

(defclass sprinkable (kitchen-entity)
  ()
  (:documentation "Something that can be sprinkled over something."))


;; Kitchen Equipment ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defclass baking-paper (reusable can-be-lined-with)
  ()
  (:documentation "A piece of baking paper. It can be used to line something with."))

(defclass baking-dish (transferable-container can-have-on-top reusable)
  ()
  (:documentation "A baking dish"))

(defclass baking-tray (transferable-container lineable brushable reusable)
  ()
  (:documentation "A baking tray. It's a lineable container."))

(defclass bowl (transferable-container coverable-container reusable)
  ()
  (:documentation "A bowl. It's a container."))

(defclass brush (can-brush reusable)
  ()
  (:documentation "A brush. It can brush."))
  
(defclass bowl-lid (can-cover reusable)
  ()
  (:documentation "A bowl lid. Used to cover a bowl"))

(defclass colander (can-drain reusable)
  ()
  (:documentation "A colander...It can drain..."))

(defclass cookie-sheet (transferable-container lineable brushable reusable)
  ()
  (:documentation "Cookie-sheet. Its' a transferable container."))

(defclass cooking-utensil (fetchable conceptualizable)
  ()
  (:documentation "A tool to be used in the kitchen."))

(defclass counter-top (container conceptualizable)
  ((arrangement :initform (make-instance 'side-to-side)))
  (:documentation "The counter-top. It's a container."))

(defmethod copy-object-content ((counter-top counter-top) (copy counter-top))
  "Copying counter-tops."
  (setf (arrangement copy) (copy-object (arrangement counter-top))))

(defclass fork (can-mix can-beat can-mash reusable)
  ()
  (:documentation "A fork. It's a tool for mixing, beating and mashing."))

(defclass freezer (container)
  ((arrangement :initform (make-instance 'shelved)))
  (:documentation "The freezer. It's a container."))

(defmethod copy-object-content ((freezer freezer) (copy freezer))
  "Copying freezers."
  (setf (arrangement copy) (copy-object (arrangement freezer))))

(defclass fridge (container)
  ((arrangement :initform (make-instance 'shelved)))
  (:documentation "The fridge. It's a container."))

(defmethod copy-object-content ((fridge fridge) (copy fridge))
  "Copying fridges."
  (setf (arrangement copy) (copy-object (arrangement fridge))))


(defclass jar (transferable-container coverable-container reusable shakeable)
  ()
  (:documentation "A jar. It's a container, coverable and tranferable."))

(defclass jar-lid (can-cover reusable)
  ()
  (:documentation "A jar-lid. Used to cover (close) a jar"))

(defclass kitchen-cabinet (container)
  ((arrangement :initform (make-instance 'shelved)))
  (:documentation "The kitchen-cabinet. It's a container where kitchen utensils are stored."))

(defmethod copy-object-content ((kitchen-cabinet kitchen-cabinet) (copy kitchen-cabinet))
  "Copying kitchen-cabinets."
  (setf (arrangement copy) (copy-object (arrangement kitchen-cabinet))))

(defclass knife (can-peel can-cut can-seed can-spread reusable)
  ()
  (:documentation "A knife that can be used for cutting and peeling."))

(defclass large-bowl (bowl)
  ()
  (:documentation "A large bowl. It's a bowl."))

(defclass large-bowl-lid (bowl-lid)
  ()
  (:documentation "A large bowl lid. Used to cover a large bowl"))

(defclass medium-bowl (bowl)
  ()
  (:documentation "A medium bowl. It's a bowl."))

(defclass medium-bowl-lid (bowl-lid)
  ()
  (:documentation "A medium bowl lid. Used to cover a medium bowl"))

(defclass oven (container has-temperature) 
  ((arrangement :initform (make-instance 'shelved)))
  (:documentation "The oven. It's a container."))

(defmethod copy-object-content ((oven oven) (copy oven))
  "Copying ovens."
  (setf (arrangement copy) (copy-object (arrangement oven))))

(defclass pan (transferable-container brushable can-be-spread-upon reusable)
  ()
  (:documentation "A pan. It's a transferable container."))

(defclass pantry (container)
  ((arrangement :initform (make-instance 'shelved)))
  (:documentation "The pantry. It's a container."))

(defmethod copy-object-content ((pantry pantry) (copy pantry))
  "Copying pantries."
  (setf (arrangement copy) (copy-object (arrangement pantry))))


(defclass saucepan (transferable-container reusable)
  ()
  (:documentation "A sauce pan. It's a transferable container."))
  
(defclass sift (can-sift reusable)
 ()
 (:documentation "A tool that can be used for sifting."))

(defclass small-bowl (bowl)
  ()
  (:documentation "A small bowl. It's a bowl."))

(defclass small-bowl-lid (bowl-lid)
  ()
  (:documentation "A small bowl lid. Used to cover a small bowl"))

(defclass spatula (can-spread reusable)
  ()
  (:documentation "A spatula that can spread."))

(defclass stove (container)
  ((arrangement :initform (make-instance 'side-to-side)))
  (:documentation "The stove It's a container."))

(defmethod copy-object-content ((stove stove) (copy stove))
  "Copying stoves"
  (setf (arrangement copy) (copy-object (arrangement stove))))

(defclass table-spoon (can-spread reusable)
  ()
  (:documentation "A table spoon."))

(defclass transferable-container (container fetchable conceptualizable)
  ((arrangement :initform nil)) 
  (:documentation "A container that can transferred."))

(defmethod copy-object-content ((transferable-container transferable-container) (copy transferable-container))
  "Copying transferable-containers."
  (setf (arrangement copy) (copy-object (arrangement transferable-container))))

(defclass whisk (can-mix can-beat reusable)
  ()
  (:documentation "A whisk. It's a tool for mixing or beating."))

(defclass wire-rack (transferable-container lineable reusable)
  ()
  (:documentation "Wire-racks. It's a transferable container."))

(defclass wooden-spoon (can-mix reusable)
  ()
  (:documentation "A wooden spoon. It's a tool for mixing."))

;; Ingredients  ;;
;;;;;;;;;;;;;;;;;;

(defclass all-purpose-flour (flour)
  ()
  (:documentation "All-purpose flour."))

(defclass almond (ingredient)
  ()
  (:documentation "Almond."))

(defclass almond-extract (flavoring-extract almond)
  ()
  (:documentation "Almond extract."))

(defclass almond-flakes (ingredient sprinkable)
  ()
  (:documentation "Almond flakes."))

(defclass almond-flour (flour)
  ()
  (:documentation "Almond flour."))

(defclass baking-powder (ingredient)
  ()
  (:documentation "Baking-powder"))

(defclass baking-soda (ingredient)
  ()
  (:documentation "Baking soda."))

(defclass banana (ingredient mashable)
  ()
  (:documentation "Banana."))

(defclass black-bean (ingredient)
  ()
  (:documentation "Black beans."))

(defclass brown-lentils (lentils)
  ()
  (:documentation "Brown lentils."))


(defclass brown-sugar (sugar)
  ()
  (:documentation "Plain brown sugar."))

(defclass butter (ingredient mixable beatable meltable has-temperature can-be-brushed-with spreadable can-have-on-top aggregate)
  ((keep-refrigerated :initform t))
  (:documentation "Butter."))

(defmethod copy-object-content ((butter butter) (copy butter))
  "Copying butter objects."
  (setf (keep-refrigerated copy) (copy-object (keep-refrigerated butter))))

(defclass conserved-ingredient (ingredient drainable)
   ((solid-parts :type list  :initarg :solid-parts :accessor solid-parts :initform '())
    (fluid-parts :type list  :initarg :fluid-parts :accessor fluid-parts :initform '()))
   (:documentation "An abstract class for canned ingredients that consist of a solid part and a fluid part."))

(defmethod copy-object-content ((conserved-ingredient conserved-ingredient) (copy conserved-ingredient))
  "Copying conserved ingredients"
  (setf (solid-parts copy) (copy-object (solid-parts conserved-ingredient)))
  (setf (fluid-parts copy) (copy-object (fluid-parts conserved-ingredient))))

(defclass canned-peaches (ingredient drainable bakeable can-have-on-top)
  ()
  (:documentation "Canned peaches, this are the peaches in the can without the fluid."))

(defclass caster-sugar (sugar)
  ()
  (:documentation "Caster sugar, granulated sugar with a very fine consistency."))

(defclass celery (ingredient cuttable) ;;stalkable?
  ((keep-refrigerated :initform T))
  (:documentation "Celery"))

(defmethod copy-object-content ((celery celery) (copy celery))
  "Copying celery objects."
  (setf (keep-refrigerated copy) (copy-object (keep-refrigerated celery))))

(defclass cherry-tomato (ingredient cuttable)
  ((keep-refrigerated :initform T))
  (:documentation "Cherry tomatoes."))

(defmethod copy-object-content ((cherry-tomato cherry-tomato) (copy cherry-tomato))
  "Copying cherry-tomato objects."
  (setf (keep-refrigerated copy) (copy-object (keep-refrigerated cherry-tomato))))

(defclass chopped-walnut (ingredient)
  ()
  (:documentation "Walnut."))

(defclass cocoa-powder (ingredient)
  ()
  (:documentation "Cocoa powder."))

(defclass corn-flakes (ingredient)
  ()
  (:documentation "Corn flakes."))


(defclass cucumber (ingredient cuttable peelable seedable)
  ((keep-refrigerated :initform T))
  (:documentation "Cucumber."))

(defmethod copy-object-content ((cucumber cucumber) (copy cucumber))
  "Copying cucumber objects."
  (setf (keep-refrigerated copy) (copy-object (keep-refrigerated cucumber))))

(defclass devils-food-cake-mix (ingredient)
  ()
  (:documentation "Devil's food cake mix."))

(defclass dried-dill-weed (ingredient)
  ()
  (:documentation "Dried dill weed."))

(defclass dry-white-wine (ingredient fluid)
  ()
  (:documentation "Dry white wine"))

(defclass egg (ingredient)
  ((keep-refrigerated :initform T))
  (:documentation "Eggs."))

(defmethod copy-object-content ((egg egg) (copy egg))
  "Copying egg objects."
  (setf (keep-refrigerated copy) (copy-object (keep-refrigerated egg))))

(defclass flavoring-extract (ingredient)
  ()
  (:documentation "Abstract class for all flavoring extracts."))

(defclass flour (ingredient)
  ()
  (:documentation "Abstract class for all flour."))

(defclass fresh-basil (ingredient cuttable pluckable)
  ()
  (:documentation "Fresh basil."))

(defclass fresh-cilantro (ingredient cuttable pluckable)
  ()
  (:documentation "Fresh cilantro (coriander)."))

(defclass fresh-parsley (ingredient cuttable pluckable)
  ()
  (:documentation "Fresh parsley"))

(defclass fresh-rosemary (ingredient cuttable pluckable)
  ()
  (:documentation "Fresh rosemary"))

(defclass frozen-corn (ingredient has-temperature)
  ((keep-frozen :initform T))
  (:documentation "Frozen corn."))

(defmethod copy-object-content ((frozen-corn frozen-corn) (copy frozen-corn))
  "Copying frozen-corn objects."
  (setf (keep-frozen copy) (copy-object (keep-frozen frozen-corn))))


(defclass garlic (ingredient cuttable)
  ()
  (:documentation "Garlic"))

(defclass ground-allspice (ingredient)
  ()
  (:documentation "Ground-allspice."))

(defclass ground-black-pepper (ingredient) 
  ()
  (:documentation "Ground black pepper."))

(defclass ground-cinnamon (ingredient)
  ()
  (:documentation "Ground cinnamon."))

(defclass ground-cloves (ingredient)
  ()
  (:documentation "Ground-cloves."))

(defclass ground-cumin (ingredient)
  ()
  (:documentation "Ground Cumin."))

(defclass ground-ginger (ingredient)
  ()
  (:documentation "Ground-ginger."))

(defclass ground-nutmeg (ingredient)
  ()
  (:documentation "Nutmeg."))

(defclass icing-sugar (sugar)
  ()
  (:documentation "Icing sugar."))

(defclass jalapeno (ingredient cuttable seedable)
  ((keep-refrigerated :initform T))
  (:documentation "Jalapeno pepper."))

(defmethod copy-object-content ((jalapeno jalapeno) (copy jalapeno))
  "Copying jalapeno objects."
  (setf (keep-refrigerated copy) (copy-object (keep-refrigerated jalapeno))))

(defclass juice (ingredient fluid)
  ()
  (:documentation "Juice is an abstract class"))

(defclass lentils (ingredient)
  ()
  (:documentation "Abstract class for all types of lentils"))

(defclass lime-juice (ingredient)
  ()
  (:documentation "Lime juice."))

(defclass linguine (ingredient)
  ()
  (:documentation "Linguine."))

(defclass milk (ingredient)
  ((keep-refrigerated :initform T))
  (:documentation "Milk."))

(defmethod copy-object-content ((milk milk) (copy milk))
  "Copying milk objects."
  (setf (keep-refrigerated copy) (copy-object (keep-refrigerated milk))))

(defclass mixture (ingredient beatable cuttable mashable meltable mixable can-be-sprinkled-with siftable sprinkable bakeable shapeable dippable spreadable can-be-sprinkled-on can-be-spread-upon  has-temperature shakeable)
  ()
  (:documentation "An abstract class for a mixture of ingredients."))

(defclass homogeneous-mixture (mixture)
  ()
  (:documentation "A homogeneous mixture. Components are indistinguishable from the whole."))

(defclass heterogeneous-mixture (mixture)
  ((components :type list :initarg :components :accessor components :initform '()))
  (:documentation "A heterogeneous mixture. Components are still known."))

(defmethod copy-object-content ((heterogeneous-mixture heterogeneous-mixture) (copy heterogeneous-mixture))
  "Copying heterogeneous-mixtures."
  (setf (components copy) (copy-object (components heterogeneous-mixture))))

(defclass molasses (ingredient)
  ()
  (:documentation "Molasses."))

(defclass olive-oil (ingredient)
  ()
  (:documentation "Olive oil."))

(defclass onion (ingredient cuttable)
  ()
  (:documentation "Onion."))

(defclass pancetta (ingredient cuttable)
  ()
  (:documentation "Pancetta."))


(defclass peach-juice (juice)
  ()
  (:documentation "Peach-juice."))

(defclass powdered-white-sugar (sugar sprinkable can-be-dipped-in)
  ()
  (:documentation "Powdered white sugar."))

(defclass quick-cooking-oats (ingredient)
  ()
  (:documentation "Quick cooking oats."))

(defclass raisin (ingredient)
  ()
  (:documentation "Raisin."))

(defclass red-chilipepper (ingredient)
  ()
  (:documentation "Ret hot chili pepper."))

(defclass red-onion (ingredient cuttable)
  ()
  (:documentation "Red Onion."))

(defclass red-pepper-flakes (ingredient)
  ()
  (:documentation "Red pepper flakes."))

(defclass salt (ingredient)
  ()
  (:documentation "Salt."))

(defclass self-rising-flour (flour)
  ()
  (:documentation "Self-rising flour: mixture of all-purpose flour, baking powder, and salt"))

(defclass semisweet-chocolate-chips (ingredient)
  ()
  (:documentation "Semisweet chcocolate chips"))

(defclass shallot (ingredient cuttable peelable)
  ()
  (:documentation "Shallot."))

(defclass sugar (ingredient beatable mixable)
  ()
  (:documentation "Abstract class for all sugars."))

(defclass sweet-potato (ingredient cuttable peelable)
  ()
  (:documentation "Sweet Potato"))

(defclass toast (ingredient can-be-spread-upon bakeable can-have-on-top)
  ()
  (:documentation "Toast."))

(defclass vanilla (ingredient)
  ()
  (:documentation "Vanilla."))

(defclass vanilla-extract (flavoring-extract vanilla)
  ()
  (:documentation "Vanilla extract."))

(defclass vegetable-oil (ingredient can-be-brushed-with)
  ()
  (:documentation "Vegetable oil."))

(defclass vinegar (ingredient)
  ()
  (:documentation "Vinegar."))

(defclass water (ingredient has-temperature)
  ()
  (:documentation "H2O."))

(defclass white-sugar (sugar)
  ()
  (:documentation "Plain white sugar."))

(defclass white-vinegar (vinegar)
  ()
  (:documentation "White vinegar."))

(defclass whole-egg (ingredient)
  ()
  (:documentation "A whole egg (without its shell)"))

(defclass whole-wheat-flour (flour)
  ()
  (:documentation "Whole-wheat-flour."))



;;           Patterns             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass pattern (conceptualizable)
  ((is-concept :initform T))
  (:documentation "For patterns. Patterns are concepts."))

(defmethod copy-object-content ((pattern pattern) (copy pattern))      
  "Copying pattern objects."
  (setf (is-concept copy) (copy-object (is-concept pattern))))

(defclass arrangement-pattern (pattern)
  () 
  (:documentation "A pattern in which something is arranged."))

(defclass side-to-side (arrangement-pattern)
  () 
  (:documentation "Filling up the available space from side to side."))

(defclass shelved (arrangement-pattern)
  () 
  (:documentation "A pattern where all shelves are subsequently filled with contents."))

(defclass sectionalized (arrangement-pattern)
  () 
  (:documentation "A pattern where contents are divided into sensible sections."))

(defclass unordered-heap (arrangement-pattern)
  ()
  (:documentation "A pattern in which contents are just randomly piled together on a heap."))

(defclass evenly-spread (arrangement-pattern)
  ()
  (:documentation "A pattern in which something is arranged in an evenly spread way over the available surface."))

(defclass cutting-pattern (pattern) 
  () 
  (:documentation "A pattern in which something can be divided."))

(defclass chopped (cutting-pattern)
  ()
  (:documentation "A chopped pattern."))

(defclass finely-chopped (cutting-pattern)
  ()
  (:documentation "A finely chopped pattern."))

(defclass slices (cutting-pattern)
  ()
  (:documentation "A sliced pattern."))

(defclass minced (cutting-pattern)
  ()
  (:documentation "A minced pattern."))

(defclass halved (cutting-pattern)
  ()
  (:documentation "A halved pattern."))

(defclass thin-slivers (cutting-pattern)
  ()
  (:documentation "A pattern for cutting objects into thin slivers "))

(defclass three-quarter-inch-cubes (cutting-pattern)
  ()
  (:documentation "A pattern for cutting objects into 3/4 inch cubes "))

(defclass on-angle (arrangement-pattern)
  ()
  (:documentation "A pattern in which one object is placed in a certain angle compared to a second object."))

(defclass peasized-cubes (cutting-pattern)
  ()
  (:documentation "A pattern for cutting objects into peasized cubes "))

(defclass two-inch (arrangement-pattern)
  ()
  (:documentation "A pattern in which objects are arranged with a distance of 2 inch."))

(defclass cover-pattern (pattern)
  () 
  (:documentation "A pattern in which something is covered"))

(defclass lid-on-angle (cover-pattern)
  ()
  (:documentation "A pattern in which a container is partially covered by putting its lid on an angle."))

(defclass shape (conceptualizable)
  ((is-concept :initform T))
  (:documentation "For shapes. Shapes are concepts."))

(defmethod copy-object-content ((shape shape) (copy shape))      
  "Copying shape objects."
  (setf (is-concept copy) (copy-object (is-concept shape))))

(defclass crescent-shape (shape)
  ()
  (:documentation "A crescent shape."))

(defclass ball-shape (shape)
  ()
  (:documentation " A ball shape."))

(defclass flattened-ball-shape (shape)
  ()
  (:documentation "A ball shape which is slightly flattened"))

(defclass walnut-ball-shape (shape)
  ()
  (:documentation " A walnut ball shape."))


;; Amounts, quantities and units  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass amount (kitchen-entity)
  ((unit :type unit :initarg :unit :accessor unit :initform (make-instance 'piece))
   (quantity :type quantity :initarg :quantity :accessor quantity :initform (make-instance 'quantity)))
  (:documentation "Amounts have a quantity and a unit."))

(defmethod copy-object-content ((amount amount) (copy amount))
  "Copying amounts."
  (setf (unit copy) (copy-object (unit amount)))
  (setf (quantity copy) (copy-object (quantity amount))))

(defclass quantity (kitchen-entity)
  ((value :type number :initarg :value :accessor value :initform 1))
  (:documentation "Quantities have a value."))

(defmethod copy-object-content ((quantity quantity) (copy quantity))
  "Copying quantities"
  (setf (value copy) (copy-object (value quantity))))

(defclass unit (conceptualizable)
  ((is-concept :initform T))
  (:documentation "Units. Units are concepts."))

(defmethod copy-object-content ((unit unit) (copy unit))      
  "Copying pattern objects."
  (setf (is-concept copy) (copy-object (is-concept unit))))

(defclass piece (unit)
  ()
  (:documentation "Unit: piece."))

(defclass stalk (unit)
  ()
  (:documentation "Unit: stalk"))

(defclass clove (unit)
  ()
  (:documentation "Unit: clove"))

(defclass cm (unit)
  ()
  (:documentation "Unit: cm"))

(defclass g (unit)
  ()
  (:documentation "Unit: gram."))

(defclass cup (unit)
  ()
  (:documentation "Unit: cup."))

(defclass handful (unit)
  ()
  (:documentation "Unit: handful"))

(defclass low-heat (unit)
  ()
  (:documentation "Unit: low heat"))

(defclass very-low-heat (unit)
  ()
  (:documentation "Unit: very low heat"))

(defclass tablespoon (unit)
  ()
  (:documentation "Unit: tablespoon."))

(defclass teaspoon (unit)
  ()
  (:documentation "Unit: teaspoon"))

(defclass l (unit)
  ()
  (:documentation "Unit: liter."))

(defclass ml (unit)
  ()
  (:documentation "Unit: milliliter."))

(defclass minute (unit)
  ()
  (:documentation "Unit: minute."))

(defclass percent (unit)
  ()
  (:documentation "Relative unit: percent."))

(defclass degrees-celsius (unit)  
  ()
  (:documentation "Unit: Celsius."))
