(in-package :clevr-dialog-grammar)

;;; ADJECTIVES


#|(def-fcg-cxn one-morph-cxn
             (<-
              (?one-morph-unit
               (lex-id unique)
               --
               (HASH form ((string ?one-morph-unit "one")))))
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn one-lex-cxn
             ((?one-lex-unit
               (args ((target ?unique) (source ?source)))
               (sem-cat (sem-class unique))
               (syn-cat (leftmost-unit ?one-lex-unit)
                        (rightmost-unit ?one-lex-unit))
               (footprints (unique)))
              <-
              (?one-lex-unit
               (lex-id unique)
               (footprints (NOT unique))
               (HASH meaning ((unique ?unique ?source)))
               --
               (footprints (NOT unique))
               (lex-id unique)))
             :cxn-inventory *clevr-dialog*)
|#
(def-fcg-cxn 0-digit-morph-cxn
             ((?0-digit-morph-unit
               (footprints (number)))
              <-
              (?0-digit-morph-unit
               (lex-id number)
               (number zero)
               (footprints (NOT number))
               --
               (HASH form ((string ?0-digit-morph-unit "0")))))
             :attributes (:string "0")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn 1-digit-morph-cxn
             ((?1-digit-morph-unit
               (footprints (number)))
              <-
              (?1-digit-morph-unit
               (lex-id number)
               (number one)
               (footprints (NOT number))
               --
               (HASH form ((string ?1-digit-morph-unit "1")))))
             :attributes (:string "1")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn 2-morph-cxn
             ((?2-morph-unit
               (footprints (number)))
              <-
              (?2-morph-unit
               (lex-id number)
               (number two)
               (footprints (NOT number))
               --
               (HASH form ((string ?2-morph-unit "2")))))
             :attributes (:string "2")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn 3-morph-cxn
             ((?3-morph-unit
               (footprints (number)))
              <-
              (?3-morph-unit
               (lex-id number)
               (number three)
               (footprints (NOT number))
               --
               (HASH form ((string ?3-morph-unit "3")))))
             :attributes (:string "3")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn 4-morph-cxn
             ((?4-morph-unit
               (footprints (number)))
              <-
              (?4-morph-unit
               (lex-id number)
               (number four)
               (footprints (NOT number))
               --
               (HASH form ((string ?4-morph-unit "4")))))
             :attributes (:string "4")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn 5-morph-cxn
             ((?5-morph-unit
               (footprints (number)))
              <-
              (?5-morph-unit
               (lex-id number)
               (number five)
               (footprints (NOT number))
               --
               (HASH form ((string ?5-morph-unit "5")))))
             :attributes (:string "5")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn 6-morph-cxn
             ((?6-morph-unit
               (footprints (number)))
              <-
              (?6-morph-unit
               (lex-id number)
               (number six)
               (footprints (NOT number))
               --
               (HASH form ((string ?6-morph-unit "6")))))
             :attributes (:string "6")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn 7-morph-cxn
             ((?7-morph-unit
               (footprints (number)))
              <-
              (?7-morph-unit
               (lex-id number)
               (number seven)
               (footprints (NOT number))
               --
               (HASH form ((string ?7-morph-unit "7")))))
             :attributes (:string "7")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn 8-morph-cxn
             ((?8-morph-unit
               (footprints (number)))
              <-
              (?8-morph-unit
               (lex-id number)
               (number eight)
               (footprints (NOT number))
               --
               (HASH form ((string ?8-morph-unit "8")))))
             :attributes (:string "8")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn 9-morph-cxn
             ((?9-morph-unit
               (footprints (number)))
              <-
              (?9-morph-unit
               (lex-id number)
               (number nine)
               (footprints (NOT number))
               --
               (HASH form ((string ?9-morph-unit "9")))))
             :attributes (:string "9")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn 10-morph-cxn
             ((?10-morph-unit
               (footprints (number)))
              <-
              (?10-morph-unit
               (lex-id number)
               (number ten)
               (footprints (NOT number))
               --
               (HASH form ((string ?10-morph-unit "10")))))
             :attributes (:string "10")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn number-cxn
             ((?number-unit
               (args ((source ?source)))
               (sem-cat (sem-class number))
               (syn-cat (syn-class adjective))
               (footprints (digit)))
              
                <-
              (?number-unit
               (HASH meaning ((count-objects ?digit-category ?source)
                              (bind digit-category ?digit-category ?number)))
               --
               (footprints (NOT digit))
               (lex-id number)
               (number ?number)))
             :attributes (:meaning count-objects
                          :lex-id number)
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn digit-cxn
             ((?digit-unit
               (args ((source ?digit)))
               (sem-cat (sem-class digit)
                        (grammar mnist))
               (syn-cat (syn-class noun))
               (footprints (digit)))            
                <-
              (?digit-unit
               (HASH meaning ((bind digit-category ?digit ?number)))
               --
               (footprints (NOT digit))
               (lex-id number)
               (number ?number)))
             :attributes (:meaning digit-category
                          :lex-id number)
             :cxn-inventory *clevr-dialog*)


(def-fcg-cxn x-digit-cxn
             ((?x-digit-unit
               (subunits (?digit-unit ?x-unit))
               (args ((target ?target)
                      (source ?source)
                      (original-source ?source)))
               (sem-cat (sem-class plural-adj-nom)
                        (grammar mnist))
               (syn-cat (number plural)
                        (leftmost-unit ?x-unit)
                        (rightmost-unit ?digit-unit)
                        (syn-class np)))
              (?x-unit
               (footprints (digit)))
                <-
              (?x-digit-unit
               (HASH meaning ((filter-by-attribute ?target ?source ?scene ?number-category)
                              (bind digit-category ?number-category ?number)))
               --
               (HASH form ((meets ?x-unit ?digit-unit))))
              (?digit-unit
               --
               (HASH form ((string ?digit-unit "'s"))))
              (?x-unit
               --
               (footprints (NOT digit))
               (lex-id number)
               (number ?number))
              (scene-unit
               --
               (scene ?scene)))
             :attributes (:meaning digit-category
                          :lex-id number)
             :cxn-inventory *clevr-dialog*)


#|(def-fcg-cxn exactly-one-lex-cxn
             ((?exactly-one-lex-unit
               (args ((target ?unique)
                      (source ?source)
                      ))
               (sem-cat (sem-class unique))
               (syn-cat (leftmost-unit ?exactly-unit)
                        (rightmost-unit ?exactly-one-lex-unit))
               (subunits (?exactly-unit))
               (footprints (unique)))
              <-
              (?exactly-unit
               --
               (HASH form ((string ?exactly-unit "exactly"))))
              (?exactly-one-lex-unit
               (lex-id unique)
               (footprints (NOT unique))
               (HASH meaning ((unique ?unique ?source)))
               --
               (lex-id unique)
               (footprints (NOT unique))))
             :cxn-inventory *clevr-dialog*)|#

(def-fcg-cxn several-morph-cxn
             ((?several-morph-unit
               (footprints (multiple)))
               <-
              (?several-morph-unit
               (lex-id multiple)
               (leftmost-unit ?several-morph-unit)
               (rightmost-unit ?several-morph-unit)
               (footprints (NOT multiple))
               --
               (HASH form ((string ?several-morph-unit "several")))))
             :attributes (:string "several")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn multiple-morph-cxn
             ((?multiple-morph-unit
               (footprints (multiple)))
               <-
              (?multiple-morph-unit
               (lex-id multiple)
               (leftmost-unit ?multiple-morph-unit)
               (rightmost-unit ?multiple-morph-unit)
               (footprints (NOT multiple))
               --
               (HASH form ((string ?multiple-morph-unit "multiple")))))
             :attributes (:string "multiple")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn many-morph-cxn
             ((?many-morph-unit
               (footprints (multiple)))
               <-
              (?many-morph-unit
               (lex-id multiple)
               (leftmost-unit ?many-morph-unit)
               (rightmost-unit ?many-morph-unit)
               (footprints (NOT multiple))
               --
               (HASH form ((string ?many-morph-unit "many")))))
             :attributes (:string "many")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn a-number-of-cxn
             ((?a-number-of-unit
               (footprints (multiple)))
               <-
              (?a-number-of-unit
               (lex-id multiple)
               (leftmost-unit ?a-unit)
               (rightmost-unit ?of-unit)
               (footprints (NOT multiple))
               --
               (HASH form ((string ?a-unit "a") (string ?number-unit "number") (string ?of-unit "of") (meets ?a-unit ?number-unit) (meets ?number-unit ?of-unit)))))
             :attributes (:string "number")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn a-lot-of-cxn
             ((?a-lot-of-unit
               (footprints (multiple)))
               <-
              (?a-lot-of-unit
               (lex-id multiple)
               (leftmost-unit ?a-unit)
               (rightmost-unit ?of-unit)
               (footprints (NOT multiple))
               --
               (HASH form ((string ?a-unit "a") (string ?lot-unit "lot") (string ?of-unit "of") (meets ?a-unit ?lot-unit) (meets ?lot-unit ?of-unit)))))
             :attributes (:string "lot")
             :cxn-inventory *clevr-dialog*)


(def-fcg-cxn multiple-cxn
             ((?multiple-unit
               (args ((target ?bool) (source ?source)))
               (sem-cat (sem-class multiple))
               (syn-cat (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?rightmost-unit)))
              <-
              (?multiple-unit 
               (HASH meaning ((more-than-1 ?bool ?source)))
               --
               (lex-id multiple)
               (leftmost-unit ?leftmost-unit)
               (rightmost-unit ?rightmost-unit)))
             :attributes (:meaning more-than-1
                          :lex-id multiple)
             :cxn-inventory *clevr-dialog*)


;;size --> large/big & small/tiny
(def-fcg-cxn large-morph-cxn
             ((?large-morph-unit
               (footprints (size)))
              <-
              (?large-morph-unit
               (lex-id size)
               (size large)
               (footprints (NOT size))
               --
               (HASH form ((string ?large-morph-unit "large")))))
             :attributes (:string "large")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn big-morph-cxn
             ((?big-morph-unit
               (footprints (size)))
              <-
              (?big-morph-unit
               (lex-id size)
               (size large)
               (footprints (NOT size))
               --
               (HASH form ((string ?big-morph-unit "big")))))
             :attributes (:string "big")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn small-morph-cxn
             ((?small-morph-unit
               (footprints (size)))
              <-
              (?small-morph-unit
               (lex-id size)
               (size small)
               (footprints (NOT size))
               --
               (HASH form ((string ?small-morph-unit "small")))))
             :attributes (:string "small")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn tiny-morph-cxn
             ((?tiny-morph-unit
               (footprints (size)))
              <-
              (?tiny-morph-unit
               (lex-id size)
               (size small)
               (footprints (NOT size))
               --
               (HASH form ((string ?tiny-morph-unit "tiny")))))
             :attributes (:string "tiny")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn size-lex-cxn
             ((?size-lex-unit
               (args ((target ?size)))
               (sem-cat (sem-class property))
               (syn-cat (syn-class adjective)
                        (starts-with consonant)
                        (leftmost-unit ?size-lex-unit)))
              <-
              (?size-lex-unit
               (HASH meaning ((bind size-category ?size ?size-morph)))
               --
               (lex-id size)
               (size ?size-morph)))
             :attributes (:meaning size-category
                          :lex-id size)
             :cxn-inventory *clevr-dialog*)


;; color --> gray red blue green brown purple cyan yellow
(def-fcg-cxn gray-lex-cxn
             ((?gray-lex-unit
               (args ((target ?color)))
               (sem-cat (sem-class property))
               (syn-cat (syn-class adjective)
                        (starts-with consonant)))
              <-
              (?gray-lex-unit
               (HASH meaning ((bind color-category ?color gray)))
               --
               (HASH form ((string ?gray-lex-unit "gray")))))
             :attributes (:meaning gray
                          :string "gray")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn red-lex-cxn
             ((?red-lex-unit
               (args ((target ?color)))
               (sem-cat (sem-class property))
               (syn-cat (syn-class adjective)
                        (starts-with consonant)))
              <-
              (?red-lex-unit
               (HASH meaning ((bind color-category ?color red)))
               --
               (HASH form ((string ?red-lex-unit "red")))))
             :attributes (:meaning red
                          :string "red")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn blue-lex-cxn
             ((?blue-lex-unit
               (args ((target ?color)))
               (sem-cat (sem-class property))
               (syn-cat (syn-class adjective)
                        (starts-with consonant)))
              <-
              (?blue-lex-unit
               (HASH meaning ((bind color-category ?color blue)))
               --
               (HASH form ((string ?blue-lex-unit "blue")))))
             :attributes (:meaning blue
                          :string "blue")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn green-lex-cxn
             ((?green-lex-unit
               (args ((target ?color)))
               (sem-cat (sem-class property))
               (syn-cat (syn-class adjective)
                        (starts-with consonant)))
              <-
              (?green-lex-unit
               (HASH meaning ((bind color-category ?color green)))
               --
               (HASH form ((string ?green-lex-unit "green")))))
             :attributes (:meaning green
                          :string "green")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn brown-lex-cxn
             ((?brown-lex-unit
               (args ((target ?color)))
               (sem-cat (sem-class property))
               (syn-cat (syn-class adjective)
                        (starts-with consonant)))
              <-
              (?brown-lex-unit
               (HASH meaning ((bind color-category ?color brown)))
               --
               (HASH form ((string ?brown-lex-unit "brown")))))
             :attributes (:meaning brown
                          :string "brown")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn purple-lex-cxn
             ((?purple-lex-unit
               (args ((target ?color)))
               (sem-cat (sem-class property))
               (syn-cat (syn-class adjective)
                        (starts-with consonant)))
              <-
              (?purple-lex-unit
               (HASH meaning ((bind color-category ?color purple)))
               --
               (HASH form ((string ?purple-lex-unit "purple")))))
             :attributes (:meaning purple
                          :string "purple")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn cyan-lex-cxn
             ((?cyan-lex-unit
               (args ((target ?color)))
               (sem-cat (sem-class property))
               (syn-cat (syn-class adjective)
                        (starts-with consonant)))
              <-
              (?cyan-lex-unit
               (HASH meaning ((bind color-category ?color cyan)))
               --
               (HASH form ((string ?cyan-lex-unit "cyan")))))
             :attributes (:meaning cyan
                          :string "cyan")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn yellow-lex-cxn
             ((?yellow-lex-unit
               (args ((target ?color)))
               (sem-cat (sem-class property))
               (syn-cat (syn-class adjective)
                        (starts-with consonant)))
              <-
              (?yellow-lex-unit
               (HASH meaning ((bind color-category ?color yellow)))
               --
               (HASH form ((string ?yellow-lex-unit "yellow")))))
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn violet-lex-cxn
             ((?violet-lex-unit
               (args ((target ?color)))
               (sem-cat (sem-class property))
               (syn-cat (syn-class adjective)
                        (starts-with consonant)))
              <-
              (?violet-lex-unit
               (HASH meaning ((bind color-category ?color violet)))
               --
               (HASH form ((string ?violet-lex-unit "violet")))))
             :attributes (:meaning violet
                          :string "violet")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn white-morph-cxn
             ((?white-morph-unit
               (args ((color white-bg)
                      (background +)))
               (sem-cat (sem-class property))
               (syn-cat (syn-class adjective)
                        (starts-with consonant)))
              <-
              (?white-morph-unit
               --
               (HASH form ((string ?white-morph-unit "white")))))
             :attributes (:string "white")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn cyan-morph-cxn
             ((?cyan-morph-unit
               (args ((color cyan-bg)
                      (background +)))
               (sem-cat (sem-class property))
               (syn-cat (syn-class adjective)
                        (starts-with consonant)))
              <-
              (?cyan-morph-unit
               --
               (HASH form ((string ?cyan-morph-unit "cyan")))))
             :attributes (:string "cyan")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn salmon-morph-cxn
             ((?salmon-morph-unit
               (args ((color salmon-bg)
                      (background +)))
               (sem-cat (sem-class property))
               (syn-cat (syn-class adjective)
                        (starts-with consonant)))
              <-
              (?salmon-morph-unit
               --
               (HASH form ((string ?salmon-morph-unit "salmon")))))
             :attributes (:string "salmon")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn yellow-morph-cxn
             ((?yellow-morph-unit
               (args ((color yellow-bg)
                      (background +)))
               (sem-cat (sem-class property))
               (syn-cat (syn-class adjective)
                        (starts-with consonant)))
              <-
              (?yellow-morph-unit
               --
               (HASH form ((string ?yellow-morph-unit "yellow")))))
             :attributes (:string "yellow")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn silver-morph-cxn
             ((?silver-morph-unit
               (args ((color silver-bg)
                      (background +)))
               (sem-cat (sem-class property))
               (syn-cat (syn-class adjective)
                        (starts-with consonant)))
              <-
              (?silver-morph-unit
               --
               (HASH form ((string ?silver-morph-unit "silver")))))
             :attributes (:string "silver")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn color-background-color-cxn
             ((?color-background-color-unit
               (args ((target ?target)
                      (source ?source)))
               (subunits (?color-unit ?background-unit))
               (syn-cat (syn-function nominal)
                        (number singular)
                        (starts-with ?letter)
                        (leftmost-unit ?color-unit)
                        (rightmost-unit ?background-unit)))
              <-
              (?color-background-color-unit
               (HASH meaning ((bind bgcolor-category ?bg-color ?color)
                              (filter-by-attribute ?target ?source ?scene ?bg-color)))
               
               --
               (HASH form ((meets ?color-unit ?background-unit))))
              (?color-unit
               (args ((color ?color)
                      (background +)))
               (sem-cat (sem-class property))
               (syn-cat (syn-class adjective)
                        (starts-with ?letter))
               --
               (args ((color ?color)
                      (background +)))
               (sem-cat (sem-class property))
               (syn-cat (syn-class adjective)
                        (starts-with ?letter)))
              (?background-unit
               --
               (HASH form ((string ?background-unit "background"))))
              (scene-unit
               --
               (scene ?scene)))
             :attributes (:meaning bgcolor-category
                          :string "background")
             :cxn-inventory *clevr-dialog*)



;;material
;; metal --> metal, metallic, shiny
;; rubber --> rubber, matte

(def-fcg-cxn metal-morph-cxn
             ((?metal-morph-unit
               (footprints (material)))
               <-
              (?metal-morph-unit
               (lex-id material)
               (material metal)
               (footprints (NOT material))
               --
               (HASH form ((string ?metal-morph-unit "metal")))))
             :attributes (:string "metal")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn metallic-morph-cxn
             ((?metallic-morph-unit
               (footprints (material)))
               <-
              (?metallic-morph-unit
               (lex-id material)
               (material metal)
               (footprints (NOT material))
               --
               (HASH form ((string ?metallic-morph-unit "metallic")))))
             :attributes (:string "metallic")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn shiny-morph-cxn
             ((?shiny-morph-unit
               (footprints (material)))
               <-
              (?shiny-morph-unit
               (lex-id material)
               (material metal)
               (footprints (NOT material))
               --
               (HASH form ((string ?shiny-morph-unit "shiny")))))
             :attributes (:string "shiny")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn rubber-morph-cxn
             ((?rubber-morph-unit
               (footprints (material)))
               <-
              (?rubber-morph-unit
               (lex-id material)
               (material rubber)
               (footprints (NOT material))
               --
               (HASH form ((string ?rubber-morph-unit "rubber")))))
             :attributes (:string "rubber")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn matte-morph-cxn
             ((?matte-morph-unit
               (footprints (material)))
               <-
              (?matte-morph-unit
               (lex-id material)
               (material rubber)
               (footprints (NOT material))
               --
               (HASH form ((string ?matte-morph-unit "matte")))))
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn metal-lex-cxn
             ((?metal-lex-unit
               (args ((target ?material)))
               (sem-cat (sem-class property))
               (syn-cat (syn-class adjective)
                        (starts-with consonant)))
              <-
              (?metal-lex-unit
               (HASH meaning ((bind material-category ?material ?material-morph)))
               --
               (lex-id material)
               (material ?material-morph)))
             :attributes (:meaning material-category
                          :lex-id material)
             :cxn-inventory *clevr-dialog*)


(def-fcg-cxn round-lex-cxn
             ((?round-lex-unit
               (args ((target ?round)))
               (sem-cat (sem-class property))
               (syn-cat (syn-class adjective)
                        (starts-with consonant)))
              <-
              (?round-lex-unit
               (HASH meaning ((bind shape-category ?round sphere)))
               --
               (HASH form ((string ?round-lex-unit "round")))))
             :attributes (:meaning sphere
                          :string "round")
             :cxn-inventory *clevr-dialog*)

;; styles

(def-fcg-cxn flat-lex-cxn
             ((?flat-lex-unit
               (args ((target ?style)))
               (sem-cat (sem-class property))
               (syn-cat (syn-class adjective)
                        (starts-with consonant)))
              <-
              (?flat-lex-unit
               (HASH meaning ((bind style-category ?style flat)))
               --
               (HASH form ((string ?flat-lex-unit "flat")))))
             :attributes (:meaning flat
                          :string "flat")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn stroke-lex-cxn
             ((?stroke-lex-unit
               (subunits (?with-unit ?a-unit ?stroke-unit))
               (args ((target ?style)))
               (sem-cat (sem-class property))
               (syn-cat (syn-class pp)
                        (starts-with consonant)
                        (leftmost-unit ?with-unit)
                        (rightmost-unit ?stroke-unit))
               )
              <-
              (?stroke-lex-unit
               (HASH meaning ((bind style-category ?style stroke)))
               --
               (HASH form ((meets ?with-unit ?a-unit)
                           (meets ?a-unit ?stroke-unit))))
              (?with-unit
               --
               (HASH form ((string ?with-unit "with"))))
              (?a-unit
               --
               (HASH form ((string ?a-unit "a"))))
              (?stroke-unit
               --
               (HASH form ((string ?stroke-unit "stroke")))))
             :attributes (:meaning stroke
                          :string "stroke")
             :cxn-inventory *clevr-dialog*)


;; anaphores --> above, aforementiones, previous, earlier (also that???) 

(def-fcg-cxn above-lex-cxn
             ((?above-lex-unit
               (args ((target ?memory)))
               (sem-cat (sem-class anaphoric))
               (syn-cat (syn-class adjective)
                        (starts-with vowel)))
              <-
              (?above-lex-unit
              ; (HASH meaning ((get-memory ?history)))
               --
               (HASH form ((string ?above-lex-unit "above"))))
              (memory-unit
               --
               (memory ?memory)))
             :attributes (:string "above")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn earlier-lex-cxn
             ((?earlier-lex-unit
               (args ((target ?memory)))
               (sem-cat (sem-class anaphoric))
               (syn-cat (syn-class adjective)))
              <-
              (?earlier-lex-unit
              ; (HASH meaning ((get-memory ?history)))
               --
               (HASH form ((string ?earlier-lex-unit "earlier"))))
              (memory-unit
               --
               (memory ?memory)))
             :attributes (:string "earlier")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn previous-lex-cxn
             ((?previous-lex-unit
               (args ((target ?memory)))
               (sem-cat (sem-class anaphoric))
               (syn-cat (syn-class adjective)))
              <-
              (?previous-lex-unit
              ; (HASH meaning ((get-memory ?history )))
               --
               (HASH form ((string ?previous-lex-unit "previous"))))
              (memory-unit
               --
               (memory ?memory)))
             :attributes (:string "previous")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn aforementioned-lex-cxn
             ((?aforementioned-lex-unit
               (args ((target ?memory)))
               (sem-cat (sem-class anaphoric))
               (syn-cat (syn-class adjective)))
              <-
              (?aforementioned-lex-unit
              ; (HASH meaning ((get-memory ?history )))
               --
               (HASH form ((string ?aforementioned-lex-unit "aforementioned"))))
              (memory-unit
               --
               (memory ?memory)))
             :attributes (:string "aforementioned")
             :cxn-inventory *clevr-dialog*)


(def-fcg-cxn other-lex-cxn
             ((?other-lex-unit
               (args ((target ?target)
                      (source ?context)
                      (input ?inset)))
               (sem-cat (sem-class diff))
               (syn-cat (syn-class adjective)
                        (leftmost-unit ?other-lex-unit)))
              <-
              (?other-lex-unit
               (HASH meaning ((set-diff ?target ?context ?inset ?scene))) 
               --
               (HASH form ((string ?other-lex-unit "other"))))
              (scene-unit
               --
               (scene ?scene)))
             :attributes (:meaning set-diff
                          :string "other")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn more-lex-cxn
             ((?more-lex-unit
               (args ((target ?target)
                      (source ?context)
                      (input ?inset)))
               (sem-cat (sem-class diff))
               (syn-cat (syn-class adjective)
                        (leftmost-unit ?more-lex-unit)))
              <-
              (?more-lex-unit
               (HASH meaning ((set-diff ?target ?context ?inset ?scene))) 
               --
               (HASH form ((string ?more-lex-unit "more"))))
              (scene-unit
               --
               (scene ?scene)))
             :attributes (:meaning set-diff
                          :string "more")
             :cxn-inventory *clevr-dialog*)



