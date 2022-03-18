(in-package :clevr-dialog-grammar)

;;; NOUNS


(def-fcg-cxn object-morph-cxn
             ((?object-morph-unit
               (footprints (thing)))
              <-
              (?object-morph-unit
               (lex-id obj)
               (number singular)
               (starts-with vowel)
               (footprints (NOT thing))
               --
               (HASH form ((string ?object-morph-unit "object")))))
             :attributes (:string "object")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn objects-morph-cxn
             ((?objects-morph-unit
               (footprints (thing)))
              <-
              (?objects-morph-unit
               (lex-id obj)
               (number plural)
               (starts-with vowel)
               (footprints (NOT thing))
               -- 
               (HASH form ((string ?objects-morph-unit "objects")))))
             :attributes (:string "objects")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn thing-morph-cxn
             ((?thing-morph-unit
               (footprints (thing)))
               <-
              (?thing-morph-unit
               (lex-id obj)
               (number singular)
               (starts-with consonant)
                (footprints (NOT thing))
               --
               (HASH form ((string ?thing-morph-unit "thing")))))
             :attributes (:string "thing")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn things-morph-cxn
             ((?things-morph-unit
               (footprints (thing)))
              <-
              (?things-morph-unit
               (lex-id obj)
               (number plural)
               (starts-with consonant)
               (footprints (NOT thing))
               -- 
               (HASH form ((string ?things-morph-unit "things")))))
             :attributes (:string "things")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn thing-lex-cxn
             ((?thing-lex-unit
               (args ((target ?object)))
               (sem-cat (sem-class thing)
                        (grammar clevr))
               (syn-cat (syn-class noun)
                        (number ?number)
                        (starts-with ?first-letter)
                        (leftmost-unit ?thing-lex-unit)
                        (rightmost-unit ?thing-lex-unit)))
              <-
              (?thing-lex-unit
               (HASH meaning ((bind shape-category ?object thing)))
               --
               (lex-id obj)
               (number ?number)
               (starts-with ?first-letter)))
             :attributes (:meaning "thing"
                          :lex-id obj)
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn digit-morph-cxn
             ((?digit-morph-unit
               (footprints (digit)))
              <-
              (?digit-morph-unit
               (lex-id digit)
               (number singular)
               (starts-with consonant)
               (footprints (NOT digit))
               --
               (HASH form ((string ?digit-morph-unit "digit")))))
             :attributes (:string "digit")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn digits-morph-cxn
             ((?digits-morph-unit
               (footprints (digit)))
              <-
              (?digits-morph-unit
               (lex-id digit)
               (number plural)
               (starts-with consonant)
               (footprints (NOT digit))
               -- 
               (HASH form ((string ?digits-morph-unit "digits")))))
             :attributes (:string "digits")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn digit-lex-cxn
             ((?digit-lex-unit
               (args ((target ?digit)))
               (sem-cat (sem-class digit)
                        (grammar mnist))
               (syn-cat (syn-class noun)
                        (number ?number)
                        (starts-with ?first-letter)
                        (leftmost-unit ?digit-lex-unit)
                        (rightmost-unit ?digit-lex-unit)))
              <-
              (?digit-lex-unit
               (HASH meaning ((bind digit-category ?digit number)))
               --
               (lex-id digit)
               (number ?number)
               (starts-with ?first-letter)))
             :attributes (:meaning number
                          :lex-id digit)
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn material-noun-lex-cxn
             ((?material-lex-unit
               (args ((target ?attribute)))
               (sem-cat (sem-class attribute)
                        (grammar clevr))
               (syn-cat (syn-class noun)
                        (leftmost-unit ?material-lex-unit)
                        (rightmost-unit ?material-lex-unit)))
              <-
              (?material-lex-unit
               (HASH meaning ((bind attribute-category ?attribute material) ))
               --
               (HASH form ((string ?material-lex-unit "material")))))
             :attributes (:meaning material
                          :string "material")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn color-noun-lex-cxn
             ((?color-lex-unit
               (args ((target ?attribute)))
               (sem-cat (sem-class attribute)
                        (grammar clevr))
               (syn-cat (syn-class noun)
                        (leftmost-unit ?color-lex-unit)
                        (rightmost-unit ?color-lex-unit)))
              <-
              (?color-lex-unit
               (HASH meaning ((bind attribute-category ?attribute color)))
               --
               (HASH form ((string ?color-lex-unit "color")))))
             :attributes (:meaning color
                          :string "color")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn size-noun-lex-cxn
             ((?size-lex-unit
               (args ((target ?attribute)))
               (sem-cat (sem-class attribute)
                        (grammar clevr))
               (syn-cat (syn-class noun)
                        (leftmost-unit ?size-lex-unit)
                        (rightmost-unit ?size-lex-unit)))
              <-
              (?size-lex-unit
               (HASH meaning ((bind attribute-category ?attribute size)))
               --
               (HASH form ((string ?size-lex-unit "size")))))
             :cxn-inventory *clevr-dialog*)



(def-fcg-cxn shape-noun-lex-cxn
             ((?shape-lex-unit
               (args ((target ?attribute)))
               (sem-cat (sem-class attribute)
                        (grammar clevr))
               (syn-cat (syn-class noun)
                        (leftmost-unit ?shape-lex-unit)
                        (rightmost-unit ?shape-lex-unit)))
              <-
              (?shape-lex-unit
               (HASH meaning ((bind attribute-category ?attribute shape)))
               --
               (HASH form ((string ?shape-lex-unit "shape")))))
             :attributes (:meaning shape
                          :string "shape")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn style-noun-lex-cxn
             ((?style-lex-unit
               (args ((target ?attribute)))
               (sem-cat (sem-class attribute)
                        (grammar mnist))
               (syn-cat (syn-class noun)
                        (leftmost-unit ?style-lex-unit)
                        (rightmost-unit ?style-lex-unit)))
              <-
              (?style-lex-unit
               (HASH meaning ((bind attribute-category ?attribute style)))
               --
               (HASH form ((string ?style-lex-unit "style")))))
             :attributes (:meaning style
                          :string "style")
             :cxn-inventory *clevr-dialog*)


#|(def-fcg-cxn number-noun-lex-cxn
             ((?number-lex-unit
               (args ((target ?attribute)))
               (sem-cat (sem-class attribute)
                        (grammar mnist))
               (syn-cat (syn-class noun)
                        (leftmost-unit ?number-lex-unit)
                        (rightmost-unit ?number-lex-unit)))
              <-
              (?number-lex-unit
               (HASH meaning ((bind attribute-category ?attribute digit)))
               --
               (HASH form ((string ?number-lex-unit "number")))))
             :cxn-inventory *clevr-dialog*)|#

(def-fcg-cxn background-color-noun-lex-cxn
             ((?background-color-lex-unit
               (args ((target ?attribute)))
               (subunits (?background-lex-unit ?color-lex-unit))
               (sem-cat (sem-class attribute)
                        (grammar mnist))
               (syn-cat (syn-class noun)
                        (leftmost-unit ?background-lex-unit)
                        (rightmost-unit ?color-lex-unit)))
            
              <-
              (?background-color-lex-unit
               (HASH meaning ((bind attribute-category ?attribute bgcolor)))
               --
               (HASH form ((string ?background-lex-unit "background")
                           (string ?color-lex-unit "color")
                           (meets ?background-lex-unit ?color-lex-unit)))))
             :attributes (:meaning bgcolor
                          :string "color")
             :cxn-inventory *clevr-dialog*)

;; cube --> cube, block



(def-fcg-cxn blocks-morph-cxn
             ((?blocks-morph-unit
               (footprints (cube)))
               <-
              (?blocks-morph-unit
               (lex-id cube)
               (number plural)
               (starts-with consonant)
               (footprints (NOT cube))
               -- 
               (HASH form ((string ?blocks-morph-unit "blocks")))))
             :attributes (:string "blocks")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn block-morph-cxn
             ((?block-morph-unit
               (footprints (cube)))
               <-
              (?block-morph-unit
               (lex-id cube)
               (number singular)
               (starts-with consonant)
               (footprints (NOT cube))
               -- 
               (HASH form ((string ?block-morph-unit "block")))))
             :attributes (:string "block")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn cube-morph-cxn
             ((?cube-morph-unit
               (footprints (cube)))
               <-
              (?cube-morph-unit
               (lex-id cube)
               (number singular)
               (starts-with consonant)
               (footprints (NOT cube))
               -- 
               (HASH form ((string ?cube-morph-unit "cube")))))
             :attributes (:string "cube")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn cubes-morph-cxn
             ((?cubes-morph-unit
               (footprints (cube)))
               <-
              (?cubes-morph-unit
               (lex-id cube)
               (number plural)
               (starts-with consonant)
               (footprints (NOT cube))
               -- 
               (HASH form ((string ?cubes-morph-unit "cubes")))))
             :attributes (:string "cubes")
             :cxn-inventory *clevr-dialog*)



(def-fcg-cxn cube-lex-cxn
             ((?cube-lex-unit
               (args ((target ?shape)))
               (sem-cat (sem-class shape)
                        (grammar clevr))
               (syn-cat (syn-class noun)
                        (number ?number)
                        (starts-with ?first-letter)
                        (leftmost-unit ?cube-lex-unit)
                        (rightmost-unit ?cube-lex-unit)))
              <-
              (?cube-lex-unit
               (HASH meaning ((bind shape-category ?shape cube)))
               --
               (lex-id cube)
               (number ?number)
               (starts-with ?first-letter)))
             :attributes (:meaning cube
                          :lex-id cube)
             :cxn-inventory *clevr-dialog*)

;; cylinder

(def-fcg-cxn cylinder-morph-cxn
             ( <-
              (?cylinder-morph-unit
               (lex-id cylinder)
               (number singular)
               (starts-with consonant)
               -- 
               (HASH form ((string ?cylinder-morph-unit "cylinder")))))
             :attributes (:string "cylinder")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn cylinders-morph-cxn
             ( <-
              (?cylinders-morph-unit
               (lex-id cylinder)
               (number plural)
               (starts-with consonant)
               -- 
               (HASH form ((string ?cylinders-morph-unit "cylinders")))))
             :attributes (:string "cylinders")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn cylinder-lex-cxn
             ((?cylinder-lex-unit
               (args ((target ?shape)))
               (sem-cat (sem-class shape)
                        (grammar clevr))
               (syn-cat (syn-class noun)
                        (number ?number)
                        (starts-with ?first-letter)
                        (leftmost-unit ?cylinder-lex-unit)
                        (rightmost-unit ?cylinder-lex-unit)))
              <-
              (?cylinder-lex-unit
               (HASH meaning ((bind shape-category ?shape cylinder)))
               --
               (lex-id cylinder)
               (number ?number)
               (starts-with ?first-letter)))
             :attributes (:meaning cylinder
                          :lex-id cylinder)
             :cxn-inventory *clevr-dialog*)

;; sphere --> sphere, ball, round thing

(def-fcg-cxn ball-morph-cxn
             ((?ball-morph-unit
               (footprints (sphere))
               (syn-cat (leftmost-unit ?ball-morph-unit)
                        (rightmost-unit ?ball-morph-unit)))
              <-
              (?ball-morph-unit
               (lex-id sphere)
               (number singular)
               (starts-with consonant)
               (footprints (NOT sphere))
               --
               (HASH form ((string ?ball-morph-unit "ball")))))
             :attributes (:string "ball")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn sphere-morph-cxn
             ((?sphere-morph-unit
               (footprints (sphere))
               (syn-cat (leftmost-unit ?sphere-morph-unit)
                        (rightmost-unit ?sphere-morph-unit)))
              <-
              (?sphere-morph-unit
               (lex-id sphere)
               (number singular)
               (starts-with consonant)
               (footprints (NOT sphere))
               --
               (HASH form ((string ?sphere-morph-unit "sphere")))))
             :attributes (:string "sphere")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn round-thing-morph-cxn
             ((?round-thing-morph-unit
               (footprints (sphere))
               (syn-cat (leftmost-unit ?round-morph-unit)
                        (rightmost-unit ?thing-morph-unit)))
              <-
              (?round-thing-morph-unit
               (lex-id sphere)
               (number singular)
               (starts-with consonant)
               (footprints (NOT sphere))
               --
               (HASH form ((string ?round-morph-unit "round")
                           (string ?thing-morph-unit "thing")
                           (meets ?round-morph-unit ?thing-morph-unit)))))
             :attributes (:string "thing")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn balls-morph-cxn
             ((?ball-morph-unit
               (footprints (sphere))
               (syn-cat (leftmost-unit ?ball-morph-unit)
                        (rightmost-unit ?ball-morph-unit)))
              <-
              (?ball-morph-unit
               (lex-id sphere)
               (number plural)
               (starts-with consonant)
               (footprints (NOT sphere))
               --
               (HASH form ((string ?ball-morph-unit "balls")))))
             :attributes (:string "balls")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn spheres-morph-cxn
             ((?sphere-morph-unit
               (footprints (sphere))
               (syn-cat (leftmost-unit ?sphere-morph-unit)
                        (rightmost-unit ?sphere-morph-unit)))
              <-
              (?sphere-morph-unit
               (lex-id sphere)
               (number plural)
               (starts-with consonant)
               (footprints (NOT sphere))
               --
               (HASH form ((string ?sphere-morph-unit "spheres")))))
             :attributes (:string "spheres")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn round-things-morph-cxn
             ((?round-thing-morph-unit
               (footprints (sphere))
               (syn-cat (leftmost-unit ?round-morph-unit)
                        (rightmost-unit ?thing-morph-unit)))
              <-
              (?round-thing-morph-unit
               (lex-id sphere)
               (number plural)
               (starts-with consonant)
               (footprints (NOT sphere))
               --
               (HASH form ((string ?round-morph-unit "round") (string ?thing-morph-unit "things") (meets ?round-morph-unit ?thing-morph-unit)))))
             :attributes (:string "things")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn sphere-lex-cxn
             ((?sphere-lex-unit
               (args ((target ?shape)))
               (sem-cat (sem-class shape)
                        (grammar clevr))
               (syn-cat (syn-class noun)
                        (number ?number)
                        (starts-with ?first-letter)
                        (leftmost-unit ?round-morph-unit)
                        (rightmost-unit ?thing-morph-unit)))
              <-
              (?sphere-lex-unit
               (HASH meaning ((bind shape-category ?shape sphere)))
               --
               (syn-cat (leftmost-unit ?round-morph-unit)
                        (rightmost-unit ?thing-morph-unit))
               (lex-id sphere)
               (number ?number)
               (starts-with ?first-letter)))
             :attributes (:meaning sphere
                          :lex-id sphere)
             :cxn-inventory *clevr-dialog*)

;; image

(def-fcg-cxn image-morph-cxn
             ((?image-morph-unit
               (footprints (image)))
              <-
              (?image-morph-unit
               (lex-id image)
               (footprints (NOT image))
               --
               (HASH form ((string ?image-morph-unit "image")))))
             :attributes (:string "image")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn picture-morph-cxn
             ((?picture-morph-unit
              (footprints (image)))
             <-
              (?picture-morph-unit
               (lex-id image)
               (footprints (NOT image))
               --
               (HASH form ((string ?picture-morph-unit "picture")))))
             :attributes (:string "picture")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn scene-morph-cxn
             ((?scene-morph-unit
               (footprints (image)))
               <-
              (?scene-morph-unit
               (lex-id image)
               (footprints (NOT image))
               --
               (HASH form ((string ?scene-morph-unit "scene")))))
             :attributes (:string "scene")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn view-morph-cxn
             ((?view-morph-unit
               (footprints (image)))
              <-
              (?view-morph-unit
               (lex-id image)
               (footprints (NOT image))
               --
               (HASH form ((string ?view-morph-unit "view")))))
             :attributes (:string "view")
             :cxn-inventory *clevr-dialog*)


