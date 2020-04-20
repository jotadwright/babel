;(ql:quickload :clevr-grammar)
(in-package :clevr-grammar)
(activate-monitor trace-fcg)


;; Seq2seq example:
(set-configurations *fcg-constructions* '((:cxn-supplier-mode . :hashed+seq2seq-heuristic)
                                          (:priority-mode . :seq2seq-heuristic-additive)))

(comprehend "What color is the large metal cube?")
(comprehend "There is a large metal cube left of the red thing; does it have the same color as the small cylinder?")

;; depth-first example:

(set-configurations *fcg-constructions* '((:cxn-supplier-mode . :all-cxns-except-incompatible-hashed-cxns)
                                          (:priority-mode . :nr-of-applied-cxns)))

(comprehend "There is a large metal cube left of the red thing; does it have the same color as the small cylinder?")


;; ------------------------ ;;
;; zero hop question family ;;
;; ------------------------ ;;

;; question type 1; count
(fcg:comprehend '("how many" "blue" "things" "are" "there"))
(fcg:comprehend-all '("what number of" "blue" "things" "are" "there"))

(fcg:formulate-all
 '((get-context context)
   (filter set-1 context shape-1)
   (filter set-2 set-1 color-1)
   (count! target set-2)
   
   (bind shape-category shape-1 thing)
   (bind color-category color-1 blue)))

;; question type 2; exist
(fcg:comprehend-all '("are any" "blue" "things" "visible"))
(fcg:comprehend-all '("are there any" "blue" "things"))
(fcg:comprehend-all '("is there a" "blue" "thing"))

(fcg:formulate-all
 '((get-context context)
   (filter set-1 context shape-1)
   (filter set-2 set-2 color-1)
   (exist target set-2)
   
   (bind shape-category shape-1 thing)
   (bind color-category color-1 blue)))

;; question type 3; query size
(fcg:comprehend-all '("what" "size" "is" "the" "blue" "metal" "thing"))
(fcg:comprehend-all '("what is" "the" "size" "of" "the" "blue" "metal" "thing"))
(fcg:comprehend-all '("there is a" "blue" "metal" "thing" ";" "what" "size" "is it"))
(fcg:comprehend-all '("there is a" "blue" "metal" "thing" ";" "what is its" "size"))
(fcg:comprehend-all '("the" "blue" "metal" "thing" "has what" "size"))
(fcg:comprehend-all '("the" "blue" "metal" "thing" "is what" "size"))
;; exceptions to the patterns
(fcg:comprehend-all '("how big is" "the" "blue" "metal" "thing"))
(fcg:comprehend-all '("there is a" "blue" "metal" "thing" ";" "how big is it"))

(fcg:formulate-all
 '((get-context context)
   (filter set-1 context shape-1)
   (filter set-2 set-1 material-1)
   (filter set-3 set-2 color-1)
   (unique obj-1 set-3)
   (query target obj-1 attr-1)
   
   (bind shape-category shape-1 thing)
   (bind material-category material-1 metal)
   (bind color-category color-1 blue)
   (bind attribute-category attr-1 size)))

;; question type 4; query color
(fcg:comprehend-all '("what" "color" "is" "the" "big" "metal" "thing"))
(fcg:comprehend-all '("what is" "the" "color" "of" "the" "big" "metal" "thing"))
(fcg:comprehend-all '("there is a" "big" "metal" "thing" ";" "what" "color" "is it"))
(fcg:comprehend-all '("there is a" "big" "metal" "thing" ";" "what is its" "color"))
(fcg:comprehend-all '("the" "big" "metal" "thing" "has what" "color"))
(fcg:comprehend-all '("the" "big" "metal" "thing" "is what" "color"))

(fcg:formulate-all
 '((get-context context)
   (filter set-1 context shape-1)
   (filter set-2 set-1 mat-1)
   (filter set-3 set-2 size-1)
   (unique obj-1 set-3)
   (query target obj-1 attr-1)
   
   (bind shape-category shape-1 thing)
   (bind material-category mat-1 metal)
   (bind size-category size-1 large)
   (bind attribute-category attr-1 color)))

;; question type 5; query material
(fcg:comprehend-all '("what" "material" "is" "the" "big" "blue" "thing"))
(fcg:comprehend-all '("what is" "the" "material" "of" "the" "big" "blue" "thing"))
(fcg:comprehend-all '("there is a" "big" "blue" "thing" ";" "what" "material" "is it"))
(fcg:comprehend-all '("there is a" "big" "blue" "thing" ";" "what is its" "material"))
;; exceptions to the patterns
(fcg:comprehend-all '("what is" "the" "big" "blue" "thing" "made of"))
(fcg:comprehend-all '("what" "material" "is" "the" "big" "blue" "thing" "made of"))
(fcg:comprehend-all '("there is a" "big" "blue" "thing" ";" "what is it made of"))
(fcg:comprehend-all '("the" "big" "blue" "thing" "is made of what material"))

(fcg:formulate-all
 '((get-context context)
   (filter set-1 context shape-1)
   (filter set-2 set-1 color-1)
   (filter set-3 set-2 size-1)
   (unique obj-1 set-3)
   (query target obj-1 attr-1)
   
   (bind shape-category shape-1 thing)
   (bind color-category color-1 blue)
   (bind size-category size-1 large)
   (bind attribute-category attr-1 material)))

;; question type 6; query shape
(fcg:comprehend-all '("what" "shape" "is" "the" "big" "blue" "metal" "thing"))
(fcg:comprehend-all '("what is" "the" "shape" "of" "the" "big" "blue" "metal" "thing"))
(fcg:comprehend-all '("there is a" "big" "blue" "metal" "thing" ";" "what" "shape" "is it"))
(fcg:comprehend-all '("there is a" "big" "blue" "metal" "thing" ";" "what is its" "shape"))
(fcg:comprehend-all '("the" "big" "blue" "metal" "thing" "has what" "shape"))
(fcg:comprehend-all '("the" "big" "blue" "metal" "thing" "is what" "shape"))

(fcg:formulate-all
 '((get-context context)
   (filter set-1 context shape-1)
   (filter set-2 set-1 mat-1)
   (filter set-3 set-2 color-1)
   (filter set-4 set-3 size-1)
   (unique obj-1 set-4)
   (query target obj-1 attr-1)
   
   (bind shape-category shape-1 thing)
   (bind material-category mat-1 metal)
   (bind color-category color-1 blue)
   (bind size-category size-1 large)
   (bind attribute-category attr-1 shape)))

;; ----------------------- ;;
;; one hop question family ;;
;; ----------------------- ;;

;; question type 1; count
(fcg:comprehend-all '("how many" "big" "blue" "metal" "things" "are" "left of" "the" "green" "ball"))
(fcg:comprehend-all '("what number of" "big" "blue" "metal" "things" "are" "left of" "the" "green" "ball"))
(fcg:comprehend-all '("there is a" "green" "ball" ";" "how many" "other" "big" "blue" "metal" "things" "are" "left of" "it"))
(fcg:comprehend-all '("there is a" "green" "ball" ";" "what number of" "big" "blue" "metal" "things" "are" "left of" "it"))

(fcg:formulate-all
 '((get-context context)
   (filter set-1 context shape-1)
   (filter set-2 set-1 color-1)
   (unique obj-1 set-2)
   (relate set-3 obj-1 rel-1)
   (filter set-4 set-3 shape-2)
   (filter set-5 set-4 mat-1)
   (filter set-6 set-5 color-2)
   (filter set-7 set-6 size-1)
   (count! target set-7)

   (bind shape-category shape-1 sphere)
   (bind color-category color-1 green)
   (bind spatial-relation-category rel-1 left)
   (bind shape-category shape-2 thing)
   (bind material-category mat-1 metal)
   (bind color-category color-2 blue)
   (bind size-category size-1 large)))

;; question type 2; exists
(fcg:comprehend-all '("are there any" "big" "blue" "metal" "things"            "left of" "the" "green" "ball"))
(fcg:comprehend-all '("are there any" "big" "blue" "metal" "things" "that are" "left of" "the" "green" "ball"))
(fcg:comprehend-all '("is there a" "big" "blue" "metal" "thing"           "left of" "the" "green" "ball"))
(fcg:comprehend-all '("is there a" "big" "blue" "metal" "thing" "that is" "left of" "the" "green" "ball"))
(fcg:comprehend-all '("there is a" "green" "ball" ";" "are there any" "big" "blue" "metal" "things"            "left of" "it"))
(fcg:comprehend-all '("there is a" "green" "ball" ";" "are there any" "big" "blue" "metal" "things" "that are" "left of" "it"))
(fcg:comprehend-all '("there is a" "green" "ball" ";" "is there a" "big" "blue" "metal" "thing"           "left of" "it"))
(fcg:comprehend-all '("there is a" "green" "ball" ";" "is there a" "big" "blue" "metal" "thing" "that is" "left of" "it"))

(fcg:formulate-all
 '((get-context context)
   (filter set-1 context shape-1)
   (filter set-2 set-1 color-1)
   (unique obj-1 set-2)
   (relate set-3 obj-1 rel-1)
   (filter set-4 set-3 shape-2)
   (filter set-5 set-4 mat-1)
   (filter set-6 set-5 color-2)
   (filter set-7 set-6 size-1)
   (exist target set-7)

   (bind shape-category shape-1 sphere)
   (bind color-category color-1 green)
   (bind spatial-relation-category rel-1 left)
   (bind shape-category shape-2 thing)
   (bind material-category mat-1 metal)
   (bind color-category color-2 blue)
   (bind size-category size-1 large)))



;; question type 3; query size
(fcg:comprehend-all '("what" "size" "is" "the" "blue" "metal" "thing"           "left of" "the" "green" "ball"))
(fcg:comprehend-all '("what" "size" "is" "the" "blue" "metal" "thing" "that is" "left of" "the" "green" "ball"))
(fcg:comprehend-all '("what is" "the" "size" "of" "the" "blue" "metal" "thing"           "left of" "the" "green" "ball"))
(fcg:comprehend-all '("what is" "the" "size" "of" "the" "blue" "metal" "thing" "that is" "left of" "the" "green" "ball"))
(fcg:comprehend-all '("there is a" "blue" "metal" "thing"           "left of" "the" "green" "ball" ";" "what" "size" "is it"))
(fcg:comprehend-all '("there is a" "blue" "metal" "thing" "that is" "left of" "the" "green" "ball" ";" "what" "size" "is it"))
(fcg:comprehend-all '("there is a" "blue" "metal" "thing"           "left of" "the" "green" "ball" ";" "what is its" "size"))
(fcg:comprehend-all '("there is a" "blue" "metal" "thing" "that is" "left of" "the" "green" "ball" ";" "what is its" "size"))
(fcg:comprehend-all '("the" "blue" "metal" "thing"           "left of" "the" "green" "ball" "is what" "size"))
(fcg:comprehend-all '("the" "blue" "metal" "thing" "that is" "left of" "the" "green" "ball" "is what" "size"))
(fcg:comprehend-all '("the" "blue" "metal" "thing"           "left of" "the" "green" "ball" "has what" "size"))
(fcg:comprehend-all '("the" "blue" "metal" "thing" "that is" "left of" "the" "green" "ball" "has what" "size"))
;; exceptions to the patterns
(fcg:comprehend-all '("how big is" "the" "blue" "metal" "thing"           "left of" "the" "green" "ball"))
(fcg:comprehend-all '("how big is" "the" "blue" "metal" "thing" "that is" "left of" "the" "green" "ball"))
(fcg:comprehend-all '("there is a" "big" "metal" "thing"           "left of" "the" "green" "ball" ";" "how big is it"))
(fcg:comprehend-all '("there is a" "big" "metal" "thing" "that is" "left of" "the" "green" "ball" ";" "how big is it"))

(fcg:formulate-all
 '((get-context context)
   (filter set-1 context shape-1)
   (filter set-2 set-1 color-1)
   (unique obj-1 set-2)
   (relate set-3 obj-1 rel-1)
   (filter set-4 set-3 shape-2)
   (filter set-5 set-4 mat-1)
   (filter set-6 set-5 color-2)
   (unique obj-2 set-6)
   (query target obj-2 attr-1)

   (bind shape-category shape-1 sphere)
   (bind color-category color-1 green)
   (bind spatial-relation-category rel-1 left)
   (bind shape-category shape-2 thing)
   (bind material-category mat-1 metal)
   (bind color-category color-2 blue)
   (bind attribute-category attr-1 size)))

;; question type 4; query color
(fcg:comprehend-all '("what" "color" "is" "the" "big" "metal" "thing"           "left of" "the" "green" "ball"))
(fcg:comprehend-all '("what" "color" "is" "the" "big" "metal" "thing" "that is" "left of" "the" "green" "ball"))
(fcg:comprehend-all '("what is" "the" "color" "of" "the" "big" "metal" "thing"           "left of" "the" "green" "ball"))
(fcg:comprehend-all '("what is" "the" "color" "of" "the" "big" "metal" "thing" "that is" "left of" "the" "green" "ball"))
(fcg:comprehend-all '("there is a" "big" "metal" "thing"           "left of" "the" "green" "ball" ";" "what" "color" "is it"))
(fcg:comprehend-all '("there is a" "big" "metal" "thing" "that is" "left of" "the" "green" "ball" ";" "what" "color" "is it"))
(fcg:comprehend-all '("there is a" "big" "metal" "thing"           "left of" "the" "green" "ball" ";" "what is its" "color"))
(fcg:comprehend-all '("there is a" "big" "metal" "thing" "that is" "left of" "the" "green" "ball" ";" "what is its" "color"))
(fcg:comprehend-all '("the" "big" "metal" "thing"           "left of" "the" "green" "ball" "is what" "color"))
(fcg:comprehend-all '("the" "big" "metal" "thing" "that is" "left of" "the" "green" "ball" "is what" "color"))
(fcg:comprehend-all '("the" "big" "metal" "thing"           "left of" "the" "green" "ball" "has what" "color"))
(fcg:comprehend-all '("the" "big" "metal" "thing" "that is" "left of" "the" "green" "ball" "has what" "color"))

(fcg:formulate-all
 '((get-context context)
   (filter set-1 context shape-1)
   (filter set-2 set-1 color-1)
   (unique obj-1 set-2)
   (relate set-3 obj-1 rel-1)
   (filter set-4 set-3 shape-2)
   (filter set-5 set-4 mat-1)
   (filter set-6 set-5 size-1)
   (unique obj-2 set-6)
   (query target obj-2 attr-1)

   (bind shape-category shape-1 sphere)
   (bind color-category color-1 green)
   (bind spatial-relation-category rel-1 left)
   (bind shape-category shape-2 thing)
   (bind material-category mat-1 metal)
   (bind size-category size-1 large)
   (bind attribute-category attr-1 size)))

;; question type 5; query material
(fcg:comprehend-all '("what" "material" "is" "the" "big" "blue"  "thing"           "left of" "the" "green" "ball"))
(fcg:comprehend-all '("what" "material" "is" "the" "big" "blue"  "thing" "that is" "left of" "the" "green" "ball"))
(fcg:comprehend-all '("what is" "the" "material" "of" "the" "big" "blue"  "thing"           "left of" "the" "green" "ball"))
(fcg:comprehend-all '("what is" "the" "material" "of" "the" "big" "blue"  "thing" "that is" "left of" "the" "green" "ball"))
(fcg:comprehend-all '("there is a" "big" "blue"  "thing"           "left of" "the" "green" "ball" ";" "what" "material" "is it"))
(fcg:comprehend-all '("there is a" "big" "blue"  "thing" "that is" "left of" "the" "green" "ball" ";" "what" "material" "is it"))
(fcg:comprehend-all '("there is a" "big" "blue"  "thing"           "left of" "the" "green" "ball" ";" "what is its" "material"))
(fcg:comprehend-all '("there is a" "big" "blue"  "thing" "that is" "left of" "the" "green" "ball" ";" "what is its" "material"))
;; exceptions to the patterns
(fcg:comprehend-all '("what is" "the" "big" "blue"  "thing"           "left of" "the" "green" "ball" "made of"))
(fcg:comprehend-all '("what is" "the" "big" "blue"  "thing" "that is" "left of" "the" "green" "ball" "made of"))
(fcg:comprehend-all '("what" "material" "is" "the" "big" "blue"  "thing"           "left of" "the" "green" "ball" "made of"))
(fcg:comprehend-all '("what" "material" "is" "the" "big" "blue"  "thing" "that is" "left of" "the" "green" "ball" "made of"))
(fcg:comprehend-all '("there is a" "big" "blue"  "thing"           "left of" "the" "green" "ball" ";" "what is it made of"))
(fcg:comprehend-all '("there is a" "big" "blue"  "thing" "that is" "left of" "the" "green" "ball" ";" "what is it made of"))
(fcg:comprehend-all '("the" "big" "blue"  "thing"           "left of" "the" "green" "ball" "is made of what material"))
(fcg:comprehend-all '("the" "big" "blue"  "thing" "that is" "left of" "the" "green" "ball" "is made of what material"))

(fcg:formulate-all
 '((get-context context)
   (filter set-1 context shape-1)
   (filter set-2 set-1 color-1)
   (unique obj-1 set-2)
   (relate set-3 obj-1 rel-1)
   (filter set-4 set-3 shape-2)
   (filter set-5 set-4 color-2)
   (filter set-6 set-5 size-1)
   (unique obj-2 set-6)
   (query target obj-2 attr-1)

   (bind shape-category shape-1 sphere)
   (bind color-category color-1 green)
   (bind spatial-relation-category rel-1 left)
   (bind shape-category shape-2 thing)
   (bind color-category color-2 blue)
   (bind size-category size-1 large)
   (bind attribute-category attr-1 size)))

;; question type 6; query shape
(fcg:comprehend-all '("what" "shape" "is" "the" "big" "blue" "metal" "thing"           "left of" "the" "green" "ball"))
(fcg:comprehend-all '("what" "shape" "is" "the" "big" "blue" "metal" "thing" "that is" "left of" "the" "green" "ball"))
(fcg:comprehend-all '("what is" "the" "shape" "of" "the" "big" "blue" "metal" "thing"           "left of" "the" "green" "ball"))
(fcg:comprehend-all '("what is" "the" "shape" "of" "the" "big" "blue" "metal" "thing" "that is" "left of" "the" "green" "ball"))
(fcg:comprehend-all '("there is a" "big" "blue" "metal" "thing"           "left of" "the" "green" "ball" ";" "what" "shape" "is it"))
(fcg:comprehend-all '("there is a" "big" "blue" "metal" "thing" "that is" "left of" "the" "green" "ball" ";" "what" "shape" "is it"))
(fcg:comprehend-all '("there is a" "big" "blue" "metal" "thing"           "left of" "the" "green" "ball" ";" "what is its" "shape"))
(fcg:comprehend-all '("there is a" "big" "blue" "metal" "thing" "that is" "left of" "the" "green" "ball" ";" "what is its" "shape"))
(fcg:comprehend-all '("the" "big" "blue" "metal" "thing"           "left of" "the" "green" "ball" "has what" "shape"))
(fcg:comprehend-all '("the" "big" "blue" "metal" "thing" "that is" "left of" "the" "green" "ball" "has what" "shape"))
(fcg:comprehend-all '("the" "big" "blue" "metal" "thing"           "left of" "the" "green" "ball" "is what" "shape"))
(fcg:comprehend-all '("the" "big" "blue" "metal" "thing" "that is" "left of" "the" "green" "ball" "is what" "shape"))

(fcg:formulate-all
 '((get-context context)
   (filter set-1 context shape-1)
   (filter set-2 set-1 color-1)
   (unique obj-1 set-2)
   (relate set-3 obj-1 rel-1)
   (filter set-4 set-3 shape-2)
   (filter set-5 set-4 mat-1)
   (filter set-6 set-5 color-2)
   (filter set-7 set-6 size-1)
   (unique obj-2 set-7)
   (query target obj-2 attr-1)

   (bind shape-category shape-1 sphere)
   (bind color-category color-1 green)
   (bind spatial-relation-category rel-1 left)
   (bind shape-category shape-2 thing)
   (bind material-category mat-1 metal)
   (bind color-category color-2 blue)
   (bind size-category size-1 large)
   (bind attribute-category attr-1 size)))

;; ----------------------- ;;
;; two hop question family ;;
;; ----------------------- ;;
;; NOTE: these already take some time (seconds) to complete

;; question type 1; count
(fcg:comprehend-all '("how many" "big" "blue" "metal" "things" "are" "left of" "the" "green" "ball" "behind" "the" "red" "thing"))

;; does not produce all solutions, since max-nr-of-nodes is reached
(fcg:formulate-all
 '((get-context context)
   (filter set-1 context shape-1)
   (filter set-2 set-1 color-1)
   (unique obj-1 set-2)
   (relate set-3 obj-1 rel-1)
   (filter set-4 set-3 shape-2)
   (filter set-5 set-4 color-2)
   (unique obj-2 set-5)
   (relate set-6 obj-2 rel-2)
   (filter set-7 set-6 shape-3)
   (filter set-8 set-7 mat-1)
   (filter set-9 set-8 color-3)
   (filter set-10 set-9 size-1)
   (count! target set-10)

   (bind shape-category shape-1 thing)
   (bind color-category color-1 red)
   (bind spatial-relation-category rel-1 behind)
   (bind shape-category shape-2 sphere)
   (bind color-category color-2 green)
   (bind spatial-relation-category rel-2 left)
   (bind shape-category shape-3 thing)
   (bind material-category mat-1 metal)
   (bind color-category color-3 blue)
   (bind size-category size-1 large)))

;; question type 2; exist
(fcg:comprehend-all '("are there any" "big" "blue" "metal" "things" "left of" "the" "green" "ball" "behind" "the" "red" "thing"))

;; does not produce all solutions, since max-nr-of-nodes is reached
(fcg:formulate-all
 '((get-context context)
   (filter set-1 context shape-1)
   (filter set-2 set-1 color-1)
   (unique obj-1 set-2)
   (relate set-3 obj-1 rel-1)
   (filter set-4 set-3 shape-2)
   (filter set-5 set-4 color-2)
   (unique obj-2 set-5)
   (relate set-6 obj-2 rel-2)
   (filter set-7 set-6 shape-3)
   (filter set-8 set-7 mat-1)
   (filter set-9 set-8 color-3)
   (filter set-10 set-9 size-1)
   (exist target set-10)

   (bind shape-category shape-1 thing)
   (bind color-category color-1 red)
   (bind spatial-relation-category rel-1 behind)
   (bind shape-category shape-2 sphere)
   (bind color-category color-2 green)
   (bind spatial-relation-category rel-2 left)
   (bind shape-category shape-3 thing)
   (bind material-category mat-1 metal)
   (bind color-category color-3 blue)
   (bind size-category size-1 large)))

;; question type 3; query size
(fcg:comprehend-all '("what" "size" "is" "the" "blue" "metal" "thing" "left of" "the" "green" "ball" "behind" "the" "red" "thing"))
;; question type 4; query color
(fcg:comprehend-all '("what is" "the" "color" "of" "the" "large" "metal" "thing" "left of" "the" "green" "ball" "behind" "the" "red" "thing"))
;; question type 5; query material
(fcg:comprehend-all '("there is a" "large" "blue" "thing" "left of" "the" "green" "ball" "behind" "the" "red" "thing" ";" "what" "material" "is it"))
;; question type 6; query shape
(fcg:comprehend-all '("there is a" "large" "blue" "metal" "thing" "left of" "the" "green" "ball" "behind" "the" "red" "thing" ";" "what is its" "shape"))

(fcg:formulate-all
 '((get-context context)
   (filter set-1 context shape-1)
   (filter set-2 set-1 color-1)
   (unique obj-1 set-2)
   (relate set-3 obj-1 rel-1)
   (filter set-4 set-3 shape-2)
   (filter set-5 set-4 color-2)
   (unique obj-2 set-5)
   (relate set-6 obj-2 rel-2)
   (filter set-7 set-6 shape-3)
   (filter set-8 set-7 mat-1)
   (filter set-9 set-8 color-3)
   (filter set-10 set-9 size-1)
   (unique obj-3 set-10)
   (query target obj-3 attr-1)

   (bind shape-category shape-1 thing)
   (bind color-category color-1 red)
   (bind spatial-relation-category rel-1 behind)
   (bind shape-category shape-2 sphere)
   (bind color-category color-2 green)
   (bind spatial-relation-category rel-2 left)
   (bind shape-category shape-3 thing)
   (bind material-category mat-1 metal)
   (bind color-category color-3 blue)
   (bind size-category size-1 large)
   (bind attribute-category attr-1 ?attr)))

;; ------------------------- ;;
;; three hop question family ;;
;; ------------------------- ;;
;; NOTE; these questions are getting ridiculous...
;;       also, you need a big screen to print the meaning network
;;       but the speed in which it finds a solution is impressive
;;       considering the size of the meaning network (formulation)
;;       comprehension is a bit slower

;; question type 1; count
(fcg:comprehend '("how many" "big" "blue" "metal" "things" "are"
                  "left of" "the" "green" "ball"
                  "behind" "the" "red" "thing"
                  "right of" "the" "purple" "cube"))

(fcg:formulate
 '((get-context context)
   (filter set-1 context shape-1)
   (filter set-2 set-1 color-1)
   (unique obj-1 set-2)
   (relate set-3 obj-1 rel-1)
   (filter set-4 set-3 shape-2)
   (filter set-5 set-4 color-2)
   (unique obj-2 set-5)
   (relate set-6 obj-2 rel-2)
   (filter set-7 set-6 shape-3)
   (filter set-8 set-7 color-3)
   (unique obj-3 set-8)
   (relate set-9 obj-3 rel-3)
   (filter set-10 set-9 shape-4)
   (filter set-11 set-10 mat-1)
   (filter set-12 set-11 color-4)
   (filter set-13 set-12 size-1)
   (count! target set-13)

   (bind shape-category shape-1 cube)
   (bind color-category color-1 purple)
   (bind spatial-relation-category rel-1 right)
   (bind shape-category shape-2 thing)
   (bind color-category color-2 red)
   (bind spatial-relation-category rel-2 behind)
   (bind shape-category shape-3 sphere)
   (bind color-category color-3 green)
   (bind spatial-relation-category rel-3 left)
   (bind shape-category shape-4 thing)
   (bind material-category mat-1 metal)
   (bind color-category color-4 blue)
   (bind size-category size-1 large)))

;; question type 2; exist
(fcg:comprehend '("are there any" "big" "blue" "metal" "things"
                  "left of" "the" "green" "ball"
                  "behind" "the" "red" "thing"
                  "right of" "the" "purple" "cube"))

(fcg:formulate
 '((get-context context)
   (filter set-1 context shape-1)
   (filter set-2 set-1 color-1)
   (unique obj-1 set-2)
   (relate set-3 obj-1 rel-1)
   (filter set-4 set-3 shape-2)
   (filter set-5 set-4 color-2)
   (unique obj-2 set-5)
   (relate set-6 obj-2 rel-2)
   (filter set-7 set-6 shape-3)
   (filter set-8 set-7 color-3)
   (unique obj-3 set-8)
   (relate set-9 obj-3 rel-3)
   (filter set-10 set-9 shape-4)
   (filter set-11 set-10 mat-1)
   (filter set-12 set-11 color-4)
   (filter set-13 set-12 size-1)
   (exist target set-13)

   (bind shape-category shape-1 cube)
   (bind color-category color-1 purple)
   (bind spatial-relation-category rel-1 right)
   (bind shape-category shape-2 thing)
   (bind color-category color-2 red)
   (bind spatial-relation-category rel-2 behind)
   (bind shape-category shape-3 sphere)
   (bind color-category color-3 green)
   (bind spatial-relation-category rel-3 left)
   (bind shape-category shape-4 thing)
   (bind material-category mat-1 metal)
   (bind color-category color-4 blue)
   (bind size-category size-1 large)))

;; question type 3; query size
(fcg:comprehend '("what" "size" "is" "the" "blue" "metal" "thing"
                  "left of" "the" "green" "ball"
                  "behind" "the" "red" "thing"
                  "right of" "the" "purple" "cube"))

;; question type 4; query color
(fcg:comprehend '("what is" "the" "color" "of" "the" "large" "metal" "thing"
                  "left of" "the" "green" "ball"
                  "behind" "the" "red" "thing"
                  "right of" "the" "purple" "cube"))

;; question type 5; query material
(fcg:comprehend '("there is a" "large" "blue" "thing"
                  "left of" "the" "green" "ball"
                  "behind" "the" "red" "thing"
                  "right of" "the" "purple" "cube"
                  ";" "what" "material" "is it"))

;; question type 6; query shape
(fcg:comprehend '("there is a" "large" "blue" "metal" "thing"
                  "left of" "the" "green" "ball"
                  "behind" "the" "red" "thing"
                  "right of" "the" "purple" "cube"
                  ";" "what is its" "shape"))

;; the solution is not always found (max-nr-of-nodes reached)
(fcg:formulate
 '((get-context context)
   (filter set-1 context shape-1)
   (filter set-2 set-1 color-1)
   (unique obj-1 set-2)
   (relate set-3 obj-1 rel-1)
   (filter set-4 set-3 shape-2)
   (filter set-5 set-4 color-2)
   (unique obj-2 set-5)
   (relate set-6 obj-2 rel-2)
   (filter set-7 set-6 shape-3)
   (filter set-8 set-7 color-3)
   (unique obj-3 set-8)
   (relate set-9 obj-3 rel-3)
   (filter set-10 set-9 shape-4)
   (filter set-11 set-10 mat-1)
   (filter set-12 set-11 color-4)
   (filter set-13 set-12 size-1)
   (unique obj-3 set-13)
   (query target obj-2 attr-1)

   (bind shape-category shape-1 cube)
   (bind color-category color-1 purple)
   (bind spatial-relation-category rel-1 right)
   (bind shape-category shape-2 thing)
   (bind color-category color-2 red)
   (bind spatial-relation-category rel-2 behind)
   (bind shape-category shape-3 sphere)
   (bind color-category color-3 green)
   (bind spatial-relation-category rel-3 left)
   (bind shape-category shape-4 thing)
   (bind material-category mat-1 metal)
   (bind color-category color-4 blue)
   (bind size-category size-1 large)
   (bind attribute-category attr-1 ?attr)))

;; -------------------------- ;;
;; single and question family ;;
;; -------------------------- ;;
;; question type 1; count
(fcg:comprehend-all '("how many" "blue" "metal" "things" "are"        "left of" "the" "green" "ball" "and" "behind" "the" "red" "thing"))
(fcg:comprehend-all '("how many" "blue" "metal" "things" "are" "both" "left of" "the" "green" "ball" "and" "behind" "the" "red" "thing"))
(fcg:comprehend-all '("what number of" "blue" "metal" "things" "are"        "left of" "the" "green" "ball" "and" "behind" "the" "red" "thing"))
(fcg:comprehend-all '("what number of" "blue" "metal" "things" "are" "both" "left of" "the" "green" "ball" "and" "behind" "the" "red" "thing"))

(fcg:formulate
 '((get-context context)
   (filter set-1 context shape-1)
   (filter set-2 set-1 color-1)
   (unique obj-1 set-2)
   (relate behind-set obj-1 rel-1)

   (filter set-3 context shape-2)
   (filter set-4 set-3 color-2)
   (unique obj-2 set-4)
   (relate left-set obj-2 rel-2)

   (intersect iset left-set behind-set)
   (filter set-6 iset shape-3)
   (filter set-7 set-6 mat-1)
   (filter set-8 set-7 color-3)
   (count! target set-8)

   (bind shape-category shape-1 thing)
   (bind color-category color-1 red)
   (bind spatial-relation-category rel-1 behind)
   (bind shape-category shape-2 sphere)
   (bind color-category color-2 green)
   (bind spatial-relation-category rel-2 left)
   (bind shape-category shape-3 thing)
   (bind material-category mat-1 metal)
   (bind color-category color-3 blue)))

;; question type 2; query size
(fcg:comprehend-all '("what is" "the" "size" "of" "the" "blue" "metal" "thing" "that is"        "left of" "the" "green" "ball" "and" "behind" "the" "red" "thing"))
(fcg:comprehend '("what is" "the" "size" "of" "the" "blue" "metal" "thing" "that is" "both" "left of" "the" "green" "ball" "and" "behind" "the" "red" "cylinder"))
(fcg:comprehend '("what" "size" "is" "the" "blue" "metal" "thing" "that is"        "left of" "the" "green" "ball" "and" "behind" "the" "red" "thing"))
(fcg:comprehend '("what" "size" "is" "the" "blue" "metal" "thing" "that is" "both" "left of" "the" "green" "ball" "and" "behind" "the" "red" "thing"))
(fcg:comprehend '("there is a" "blue" "metal" "thing" "that is"        "left of" "the" "green" "ball" "and" "behind" "the" "red" "thing" ";" "what" "size" "is it"))
(fcg:comprehend '("there is a" "blue" "metal" "thing" "that is" "both" "left of" "the" "green" "ball" "and" "behind" "the" "red" "thing" ";" "what" "size" "is it"))
(fcg:comprehend '("there is a" "blue" "metal" "thing" "that is"        "left of" "the" "green" "ball" "and" "behind" "the" "red" "thing" ";" "what is its" "size"))
(fcg:comprehend '("there is a" "blue" "metal" "thing" "that is" "both" "left of" "the" "green" "ball" "and" "behind" "the" "red" "thing" ";" "what is its" "size"))
(fcg:comprehend '("the" "blue" "metal" "thing" "that is"        "left of" "the" "green" "ball" "and" "behind" "the" "red" "thing" "has what" "size"))
(fcg:comprehend '("the" "blue" "metal" "thing" "that is" "both" "left of" "the" "green" "ball" "and" "behind" "the" "red" "thing" "has what" "size"))
(fcg:comprehend '("the" "blue" "metal" "thing" "that is"        "left of" "the" "green" "ball" "and" "behind" "the" "red" "thing" "is what" "size"))
(fcg:comprehend '("the" "blue" "metal" "thing" "that is" "both" "left of" "the" "green" "ball" "and" "behind" "the" "red" "thing" "is what" "size"))

(fcg:comprehend '("how big is" "the" "blue" "metal" "thing" "that is"        "left of" "the" "green" "ball" "and" "behind" "the" "red" "thing"))
(fcg:comprehend '("how big is" "the" "blue" "metal" "thing" "that is" "both" "left of" "the" "green" "ball" "and" "behind" "the" "red" "thing"))
(fcg:comprehend '("there is a" "blue" "metal" "thing" "that is"        "left of" "the" "green" "ball" "and" "behind" "the" "red" "thing" ";" "how big is it"))
(fcg:comprehend '("there is a" "blue" "metal" "thing" "that is" "both" "left of" "the" "green" "ball" "and" "behind" "the" "red" "thing" ";" "how big is it"))

;; question type 3; query color (idem query size)

;; question type 4; query material
(fcg:comprehend '("what is" "the" "large" "blue" "thing" "that is"        "left of" "the" "green" "ball" "and" "behind" "the" "red" "thing" "made of"))
(fcg:comprehend '("what is" "the" "large" "blue" "thing" "that is" "both" "left of" "the" "green" "ball" "and" "behind" "the" "red" "thing" "made of"))
(fcg:comprehend '("the" "large" "blue" "thing" "that is"        "left of" "the" "green" "ball" "and" "behind" "the" "red" "thing" "is made of what material"))
(fcg:comprehend '("the" "large" "blue" "thing" "that is" "both" "left of" "the" "green" "ball" "and" "behind" "the" "red" "thing" "is made of what material"))
(fcg:comprehend '("there is a" "large" "blue" "thing" "that is"        "left of" "the" "green" "ball" "and" "behind" "the" "red" "thing" ";" "what is it made of"))
(fcg:comprehend '("there is a" "large" "blue" "thing" "that is" "both" "left of" "the" "green" "ball" "and" "behind" "the" "red" "thing" ";" "what is it made of"))
(fcg:comprehend '("there is a" "large" "blue" "thing" "that is"        "left of" "the" "green" "ball" "and" "behind" "the" "red" "thing" ";" "what material is it made of"))
(fcg:comprehend '("there is a" "large" "blue" "thing" "that is" "both" "left of" "the" "green" "ball" "and" "behind" "the" "red" "thing" ";" "what material is it made of"))

;; question type 5; query shape (idem query size)

(fcg:formulate
 '((get-context context)
   (filter set-1 context shape-1)
   (filter set-2 set-1 color-1)
   (unique obj-1 set-2)
   (relate behind-set obj-1 rel-1)

   (filter set-3 context shape-2)
   (filter set-4 set-3 color-2)
   (unique obj-2 set-4)
   (relate left-set obj-2 rel-2)

   (intersect iset left-set behind-set)
   (filter set-6 iset shape-3)
   (filter set-7 set-6 mat-1)
   (filter set-8 set-7 color-3)
   (unique obj-3 set-8)
   (query target obj-3 attr-1)

   (bind shape-category shape-1 thing)
   (bind color-category color-1 red)
   (bind spatial-relation-category rel-1 behind)
   (bind shape-category shape-2 sphere)
   (bind color-category color-2 green)
   (bind spatial-relation-category rel-2 left)
   (bind shape-category shape-3 thing)
   (bind material-category mat-1 metal)
   (bind color-category color-3 blue)
   (bind attribute-category attr-1 ?attr)))

;; ------------------------- ;;
;; single or question family ;;
;; ------------------------- ;;
;; question type 1; count, no relate
(fcg:comprehend-all '("how many" "things" "are"          "large" "blue" "things" "or" "green" "balls"))
(fcg:comprehend-all '("how many" "things" "are" "either" "large" "blue" "things" "or" "green" "balls"))
(fcg:comprehend-all '("what number of" "things" "are"          "large" "blue" "things" "or" "green" "balls"))
(fcg:comprehend-all '("what number of" "things" "are" "either" "large" "blue" "things" "or" "green" "balls"))

(fcg:formulate
 '((get-context context)
   (filter set-1 context shape-1)
   (filter set-2 set-1 color-1)

   (filter set-3 context shape-2)
   (filter set-4 set-3 color-2)
   (filter set-5 set-4 size-1)

   (union! u-set set-5 set-2)
   (filter count-set u-set shape-3)
   (count! target count-set)

   (bind shape-category shape-1 sphere)
   (bind color-category color-1 green)
   (bind shape-category shape-2 thing)
   (bind color-category color-2 blue)
   (bind size-category size-1 large)
   (bind shape-category shape-3 thing)))

;; question type 2; count, relate left
(fcg:comprehend-all '("how many" "things" "are"          "large" "blue" "things"            "left of" "the" "green" "ball" "or" "red" "things"))
(fcg:comprehend-all '("how many" "things" "are" "either" "large" "blue" "things"            "left of" "the" "green" "ball" "or" "red" "things"))
(fcg:comprehend-all '("how many" "things" "are"          "large" "blue" "things" "that are" "left of" "the" "green" "ball" "or" "red" "things"))
(fcg:comprehend-all '("how many" "things" "are" "either" "large" "blue" "things" "that are" "left of" "the" "green" "ball" "or" "red" "things"))

(fcg:formulate
 '((get-context context)
   (filter set-1 context shape-1)
   (filter set-2 set-1 color-1)

   (filter set-3 context shape-2)
   (filter set-4 set-3 color-2)
   (unique obj-1 set-4)
   (relate set-5 obj-1 rel-1)
   (filter set-6 set-5 shape-3)
   (filter set-7 set-6 color-3)
   (filter set-8 set-7 size-1)

   (union! u-set set-8 set-2)
   (filter count-set u-set shape-4)
   (count! target count-set)

   (bind shape-category shape-1 thing)
   (bind color-category color-1 red)
   (bind shape-category shape-2 sphere)
   (bind color-category color-2 green)
   (bind spatial-relation-category rel-1 left)
   (bind shape-category shape-3 thing)
   (bind color-category color-3 blue)
   (bind size-category size-1 large)
   (bind shape-category shape-4 thing)))

;; question type 3; count, relate right
(fcg:comprehend-all '("how many" "things" "are"          "large" "blue" "things" "or" "green" "balls"            "behind" "the" "red" "thing"))
(fcg:comprehend-all '("how many" "things" "are" "either" "large" "blue" "things" "or" "green" "balls"            "behind" "the" "red" "thing"))
(fcg:comprehend-all '("how many" "things" "are"          "large" "blue" "things" "or" "green" "balls" "that are" "behind" "the" "red" "thing"))
(fcg:comprehend-all '("how many" "things" "are" "either" "large" "blue" "things" "or" "green" "balls" "that are" "behind" "the" "red" "thing"))

(fcg:formulate
 '((get-context context)
   (filter set-1 context shape-1)
   (filter set-2 set-1 color-1)

   (filter set-3 context shape-2)
   (filter set-4 set-3 color-2)
   (unique obj-1 set-4)
   (relate set-5 obj-1 rel-1)
   (filter set-6 set-5 shape-3)
   (filter set-7 set-6 color-3)
   (filter set-8 set-7 size-1)

   (union! u-set set-2 set-8)
   (filter count-set u-set shape-4)
   (count! target count-set)

   (bind shape-category shape-1 thing)
   (bind color-category color-1 red)
   (bind shape-category shape-2 sphere)
   (bind color-category color-2 green)
   (bind spatial-relation-category rel-1 left)
   (bind shape-category shape-3 thing)
   (bind color-category color-3 blue)
   (bind size-category size-1 large)
   (bind shape-category shape-4 thing)))

;; question type 4; count, relate both
(fcg:comprehend-all '("how many" "things" "are"          "large" "blue" "things"            "left of" "the" "green" "ball" "or" "red" "things"            "behind" "the" "purple" "cube"))
(fcg:comprehend-all '("how many" "things" "are" "either" "large" "blue" "things"            "left of" "the" "green" "ball" "or" "red" "things"            "behind" "the" "purple" "cube"))
(fcg:comprehend-all '("how many" "things" "are" "either" "large" "blue" "things"            "left of" "the" "green" "ball" "or" "red" "things" "that are" "behind" "the" "purple" "cube"))
(fcg:comprehend-all '("how many" "things" "are" "either" "large" "blue" "things" "that are" "left of" "the" "green" "ball" "or" "red" "things"            "behind" "the" "purple" "cube"))
(fcg:comprehend-all '("how many" "things" "are" "either" "large" "blue" "things" "that are" "left of" "the" "green" "ball" "or" "red" "things" "that are" "behind" "the" "purple" "cube"))
(fcg:comprehend-all '("how many" "things" "are"          "large" "blue" "things" "that are" "left of" "the" "green" "ball" "or" "red" "things"            "behind" "the" "purple" "cube"))
(fcg:comprehend-all '("how many" "things" "are"          "large" "blue" "things"            "left of" "the" "green" "ball" "or" "red" "things" "that are" "behind" "the" "purple" "cube"))
(fcg:comprehend-all '("how many" "things" "are"          "large" "blue" "things" "that are" "left of" "the" "green" "ball" "or" "red" "things" "that are" "behind" "the" "purple" "cube"))

(fcg:formulate
 '((get-context context)
   (filter set-1 context shape-1)
   (filter set-2 set-1 color-1)
   (unique obj-1 set-2)
   (relate set-3 obj-1 rel-1)
   (filter set-4 set-3 shape-2)
   (filter set-5 set-4 color-2)

   (filter set-6 context shape-3)
   (filter set-7 set-6 color-3)
   (unique obj-2 set-7)
   (relate set-8 obj-2 rel-2)
   (filter set-9 set-8 shape-4)
   (filter set-10 set-9 color-4)
   (filter set-11 set-10 size-1)

   (union! u-set set-11 set-5)
   (filter count-set u-set shape-5)
   (count! target count-set)

   (bind shape-category shape-1 cube)
   (bind color-category color-1 purple)
   (bind spatial-relation-category rel-1 behind)
   (bind shape-category shape-2 thing)
   (bind color-category color-2 red)
   (bind shape-category shape-3 sphere)
   (bind color-category color-3 green)
   (bind spatial-relation-category rel-2 left)
   (bind shape-category shape-4 thing)
   (bind color-category color-4 blue)
   (bind size-category size-1 large)
   (bind shape-category shape-5 thing)))

;; question type 5; count, size
(fcg:comprehend-all '("how many" "large" "things" "are"          "blue" "things" "or" "balls"))
(fcg:comprehend-all '("how many" "large" "things" "are" "either" "blue" "things" "or" "balls"))

;; question type 6; count, color
(fcg:comprehend-all '("how many" "blue" "things" "are"          "large" "things" "or" "balls"))
(fcg:comprehend-all '("how many" "blue" "things" "are" "either" "large" "things" "or" "balls"))

;; question type 7; count, shape
(fcg:comprehend-all '("how many" "cubes" "are"          "blue" "things" "or" "large" "objects"))
(fcg:comprehend-all '("how many" "cubes" "are" "either" "blue" "things" "or" "large" "objects"))

;; question type 8; count, material
(fcg:comprehend-all '("how many" "metal" "things" "are"          "blue" "things" "or" "balls"))
(fcg:comprehend-all '("how many" "metal" "things" "are" "either" "blue" "things" "or" "balls"))

;; ------------------------------- ;;
;; compare integer question family ;;
;; ------------------------------- ;;
;; question type 1, same number
(fcg:comprehend-all '("are there" "an equal number of" "blue" "things" "and" "green" "balls"))
(fcg:comprehend-all '("are there" "the same number of" "blue" "things" "and" "green" "balls"))
(fcg:comprehend-all '("is the number of" "blue" "things" "the same as" "the number of" "green" "balls"))

(fcg:formulate
 '((get-context context)
   (filter set-1 context shape-1)
   (filter set-2 set-1 color-1)
   (count! count-1 set-2)

   (filter set-3 context shape-2)
   (filter set-4 set-3 color-2)
   (count! count-2 set-4)

   (equal-integer target count-1 count-2)

   (bind shape-category shape-1 sphere)
   (bind color-category color-1 green)
   (bind shape-category shape-2 thing)
   (bind color-category color-2 blue)))

(fcg:comprehend-all '("are there" "an equal number of" "blue" "things" "left of" "the" "green" "ball" "and" "red" "cubes"))
(fcg:comprehend-all '("are there" "the same number of" "blue" "things" "left of" "the" "green" "ball" "and" "red" "cubes"))
(fcg:comprehend-all '("is the number of" "blue" "things" "left of" "the" "green" "ball" "the same as" "the number of" "red" "cubes"))

(fcg:formulate
 '((get-context context)
   (filter set-1 context shape-1)
   (filter set-2 set-1 color-1)
   (count! count-1 set-2)

   (filter set-3 context shape-2)
   (filter set-4 set-3 color-2)
   (unique obj-1 set-4)
   (relate set-5 obj-1 rel-1)
   (filter set-6 set-5 shape-3)
   (filter set-7 set-6 color-3)
   (count! count-2 set-7)

   (equal-integer target count-1 count-2)

   (bind shape-category shape-1 cube)
   (bind color-category color-1 red)
   (bind shape-category shape-2 sphere)
   (bind color-category color-2 green)
   (bind spatial-relation-category rel-1 left)
   (bind shape-category shape-3 thing)
   (bind color-category color-3 blue)))

(fcg:comprehend-all '("are there" "an equal number of" "blue" "things" "left of" "the" "green" "ball" "and" "red" "cubes" "behind" "the" "purple" "cube"))
(fcg:comprehend-all '("are there" "the same number of" "blue" "things" "left of" "the" "green" "ball" "and" "red" "cubes" "behind" "the" "purple" "cube"))
(fcg:comprehend '("is the number of" "blue" "things" "left of" "the" "green" "ball" "the same as" "the number of" "red" "cubes" "behind" "the" "purple" "cube"))

(fcg:formulate
 '((get-context context)
   (filter set-1 context shape-1)
   (filter set-2 set-1 color-1)
   (unique obj-1 set-2)
   (relate set-3 obj-1 rel-1)
   (filter set-4 set-3 shape-2)
   (filter set-5 set-4 color-2)
   (count! count-1 set-5)

   (filter set-6 context shape-3)
   (filter set-7 set-6 color-3)
   (unique obj-2 set-7)
   (relate set-8 obj-2 rel-2)
   (filter set-9 set-8 shape-4)
   (filter set-10 set-9 color-4)
   (count! count-2 set-10)

   (equal-integer target count-1 count-2)

   (bind shape-category shape-1 cube)
   (bind color-category color-1 purple)
   (bind spatial-relation-category rel-1 behind)
   (bind shape-category shape-2 cube)
   (bind color-category color-2 red)
   (bind shape-category shape-3 sphere)
   (bind color-category color-3 green)
   (bind spatial-relation-category rel-2 left)
   (bind shape-category shape-4 thing)
   (bind color-category color-4 blue)))

;; question type 2, less than
(fcg:comprehend-all '("are there" "fewer" "blue" "things" "than" "green" "balls"))
(fcg:comprehend-all '("is the number of" "blue" "things" "less than" "the number of" "green" "balls"))

(fcg:formulate
 '((get-context context)
   (filter set-1 context shape-1)
   (filter set-2 set-1 color-1)
   (count! count-1 set-2)

   (filter set-3 context shape-2)
   (filter set-4 set-3 color-2)
   (count! count-2 set-4)

   (less-than target count-1 count-2)

   (bind shape-category shape-1 sphere)
   (bind color-category color-1 green)
   (bind shape-category shape-2 thing)
   (bind color-category color-2 blue)))

(fcg:comprehend-all '("are there" "fewer" "blue" "things" "left of" "the" "green" "ball" "than" "red" "cubes"))
(fcg:comprehend-all '("is the number of" "blue" "things" "left of" "the" "green" "ball" "less than" "the number of" "red" "cubes"))

(fcg:formulate
 '((get-context context)
   (filter set-1 context shape-1)
   (filter set-2 set-1 color-1)
   (count! count-1 set-2)

   (filter set-3 context shape-2)
   (filter set-4 set-3 color-2)
   (unique obj-1 set-4)
   (relate set-5 obj-1 rel-1)
   (filter set-6 set-5 shape-3)
   (filter set-7 set-6 color-3)
   (count! count-2 set-7)

   (less-than target count-1 count-2)

   (bind shape-category shape-1 cube)
   (bind color-category color-1 red)
   (bind shape-category shape-2 sphere)
   (bind color-category color-2 green)
   (bind spatial-relation-category rel-1 left)
   (bind shape-category shape-3 thing)
   (bind color-category color-3 blue)))

(fcg:comprehend-all '("are there" "fewer" "blue" "things" "left of" "the" "green" "ball" "than" "red" "cubes" "behind" "the" "purple" "cube"))
(fcg:comprehend-all '("is the number of" "blue" "things" "left of" "the" "green" "ball" "less than" "the number of" "red" "cubes" "behind" "the" "purple" "cube"))

(fcg:formulate
 '((get-context context)
   (filter set-1 context shape-1)
   (filter set-2 set-1 color-1)
   (unique obj-1 set-2)
   (relate set-3 obj-1 rel-1)
   (filter set-4 set-3 shape-2)
   (filter set-5 set-4 color-2)
   (count! count-1 set-5)

   (filter set-6 context shape-3)
   (filter set-7 set-6 color-3)
   (unique obj-2 set-7)
   (relate set-8 obj-2 rel-2)
   (filter set-9 set-8 shape-4)
   (filter set-10 set-9 color-4)
   (count! count-2 set-10)

   (less-than target count-1 count-2)

   (bind shape-category shape-1 cube)
   (bind color-category color-1 purple)
   (bind spatial-relation-category rel-1 behind)
   (bind shape-category shape-2 cube)
   (bind color-category color-2 red)
   (bind shape-category shape-3 sphere)
   (bind color-category color-3 green)
   (bind spatial-relation-category rel-2 left)
   (bind shape-category shape-4 thing)
   (bind color-category color-4 blue)))

;; question type 3, greater than
(fcg:comprehend-all '("are there" "more" "blue" "things" "than" "green" "balls"))
(fcg:comprehend-all '("is the number of" "blue" "things" "greater than" "the number of" "green" "balls"))

(fcg:formulate
 '((get-context context)
   (filter set-1 context shape-1)
   (filter set-2 set-1 color-1)
   (count! count-1 set-2)

   (filter set-3 context shape-2)
   (filter set-4 set-3 color-2)
   (count! count-2 set-4)

   (greater-than target count-1 count-2)

   (bind shape-category shape-1 sphere)
   (bind color-category color-1 green)
   (bind shape-category shape-2 thing)
   (bind color-category color-2 blue)))

(fcg:comprehend-all '("are there" "more" "blue" "things" "left of" "the" "green" "ball" "than" "red" "cubes"))
(fcg:comprehend-all '("is the number of" "blue" "things" "left of" "the" "green" "ball" "greater than" "the number of" "red" "cubes"))

(fcg:formulate
 '((get-context context)
   (filter set-1 context shape-1)
   (filter set-2 set-1 color-1)
   (count! count-1 set-2)

   (filter set-3 context shape-2)
   (filter set-4 set-3 color-2)
   (unique obj-1 set-4)
   (relate set-5 obj-1 rel-1)
   (filter set-6 set-5 shape-3)
   (filter set-7 set-6 color-3)
   (count! count-2 set-7)

   (greater-than target count-1 count-2)

   (bind shape-category shape-1 cube)
   (bind color-category color-1 red)
   (bind shape-category shape-2 sphere)
   (bind color-category color-2 green)
   (bind spatial-relation-category rel-1 left)
   (bind shape-category shape-3 thing)
   (bind color-category color-3 blue)))

(fcg:comprehend '("are there" "more" "blue" "things" "left of" "the" "green" "ball" "than" "red" "cubes" "behind" "the" "purple" "cube"))
(fcg:comprehend-all '("is the number of" "blue" "things" "left of" "the" "green" "ball" "greater than" "the number of" "red" "cubes" "behind" "the" "purple" "cube"))

(fcg:formulate
 '((get-context context)
   (filter set-1 context shape-1)
   (filter set-2 set-1 color-1)
   (unique obj-1 set-2)
   (relate set-3 obj-1 rel-1)
   (filter set-4 set-3 shape-2)
   (filter set-5 set-4 color-2)
   (count! count-1 set-5)

   (filter set-6 context shape-3)
   (filter set-7 set-6 color-3)
   (unique obj-2 set-7)
   (relate set-8 obj-2 rel-2)
   (filter set-9 set-8 shape-4)
   (filter set-10 set-9 color-4)
   (count! count-2 set-10)

   (greater-than target count-1 count-2)

   (bind shape-category shape-1 cube)
   (bind color-category color-1 purple)
   (bind spatial-relation-category rel-1 behind)
   (bind shape-category shape-2 cube)
   (bind color-category color-2 red)
   (bind shape-category shape-3 sphere)
   (bind color-category color-3 green)
   (bind spatial-relation-category rel-2 left)
   (bind shape-category shape-4 thing)
   (bind color-category color-4 blue)))

;; --------------------------- ;;
;; same relate question family ;;
;; --------------------------- ;;
;; question type 1, exist, same size, (+ filter)
(fcg:comprehend-all '("are there any"         "things" "that have" "the same" "size" "as" "the" "blue" "ball"))
(fcg:comprehend-all '("are there any" "other" "things" "that have" "the same" "size" "as" "the" "blue" "ball"))
(fcg:comprehend-all '("are there any"         "things" "that are" "the same" "size" "as" "the" "blue" "ball"))
(fcg:comprehend-all '("are there any" "other" "things" "that are" "the same" "size" "as" "the" "blue" "ball"))

(fcg:comprehend-all '("is there anything else that has" "the same" "size" "as" "the" "blue" "ball"))
(fcg:comprehend-all '("is there any other" "thing" "that has" "the same" "size" "as" "the" "blue" "ball"))
(fcg:comprehend-all '("is there anything else that is" "the same" "size" "as" "the" "blue" "ball"))
(fcg:comprehend-all '("is there any other" "thing" "that is" "the same" "size" "as" "the" "blue" "ball"))

(fcg:comprehend-all '("are there any"         "red" "cubes" "that have" "the same" "size" "as" "the" "blue" "ball"))
(fcg:comprehend-all '("are there any" "other" "red" "cubes" "that have" "the same" "size" "as" "the" "blue" "ball"))
(fcg:comprehend-all '("are there any"         "red" "cubes" "of" "the same" "size" "as" "the" "blue" "ball"))
(fcg:comprehend-all '("are there any" "other" "red" "cubes" "of" "the same" "size" "as" "the" "blue" "ball"))

(fcg:comprehend-all '("is there another" "red" "cube" "that has" "the same" "size" "as" "the" "blue" "ball"))
(fcg:comprehend-all '("is there another" "red" "cube" "of" "the same" "size" "as" "the" "blue" "ball"))

(fcg:formulate
 '((get-context context)
   (filter set-1 context shape-1)
   (filter set-2 set-1 color-1)
   (unique obj-2 set-2)
   (same set-3 obj-2 attr-1)
   (filter set-4 set-3 shape-2)
   (filter set-5 set-4 color-2)
   (exist target set-5)

   (bind shape-category shape-1 sphere)
   (bind color-category color-1 blue)
   (bind attribute-category attr-1 shape)
   (bind shape-category shape-2 cube)
   (bind color-category color-2 red)))

;; EXCEPTIONS
(fcg:comprehend-all '("are there any"         "things" "that are" "made of" "the same" "material" "as" "the" "blue" "ball"))
(fcg:comprehend-all '("are there any" "other" "things" "that are" "made of" "the same" "material" "as" "the" "blue" "ball"))
(fcg:comprehend-all '("are there any"         "red" "cubes" "made of" "the same" "material" "as" "the" "blue" "ball"))
(fcg:comprehend-all '("are there any" "other" "red" "cubes" "made of" "the same" "material" "as" "the" "blue" "ball"))

(fcg:comprehend-all '("is there anything else that is" "made of" "the same" "material" "as" "the" "blue" "ball"))
(fcg:comprehend-all '("is there any other" "thing" "that is" "made of" "the same" "material" "as" "the" "blue" "ball"))
(fcg:comprehend-all '("is there another" "red" "cube" "made of" "the same" "material" "as" "the" "blue" "ball"))

;; question type 2, exist, same color, (+ filter)
;; question type 3, exist, same material, (+ filter)
;; question type 4, exist, same shape, (+ filter)

;; question type 5, count, same size, (+ filter)
(fcg:comprehend-all '("how many"         "things" "are" "the same" "size" "as" "the" "blue" "ball"))
(fcg:comprehend-all '("how many" "other" "things" "are" "the same" "size" "as" "the" "blue" "ball"))
(fcg:comprehend-all '("what number of"         "things" "are" "the same" "size" "as" "the" "blue" "ball"))
(fcg:comprehend-all '("what number of" "other" "things" "are" "the same" "size" "as" "the" "blue" "ball"))
(fcg:comprehend-all '("how many"         "things" "are there of" "the same" "size" "as" "the" "blue" "ball"))
(fcg:comprehend-all '("how many" "other" "things" "are there of" "the same" "size" "as" "the" "blue" "ball"))
(fcg:comprehend-all '("what number of"         "things" "are there of" "the same" "size" "as" "the" "blue" "ball"))
(fcg:comprehend-all '("what number of" "other" "things" "are there of" "the same" "size" "as" "the" "blue" "ball"))

(fcg:formulate
 '((get-context context)
   (filter set-1 context shape-1)
   (filter set-2 set-1 color-1)
   (unique obj-2 set-2)
   (same set-3 obj-2 attr-1)
   (filter set-4 set-3 shape-2)
   (filter set-5 set-4 color-2)
   (count! target set-5)

   (bind shape-category shape-1 sphere)
   (bind color-category color-1 blue)
   (bind attribute-category attr-1 shape)
   (bind shape-category shape-2 cube)
   (bind color-category color-2 red)))

(fcg:comprehend-all '("how many"         "red" "cubes" "have" "the same" "size" "as" "the" "blue" "ball"))
(fcg:comprehend-all '("how many" "other" "red" "cubes" "have" "the same" "size" "as" "the" "blue" "ball"))
(fcg:comprehend-all '("how many"          "red" "cubes" "are" "the same" "size" "as" "the" "blue" "ball"))
(fcg:comprehend-all '("how many" "other" "red" "cubes" "are" "the same" "size" "as" "the" "blue" "ball"))
(fcg:comprehend-all '("what number of"         "red" "cubes" "have" "the same" "size" "as" "the" "blue" "ball"))
(fcg:comprehend-all '("what number of" "other" "red" "cubes" "have" "the same" "size" "as" "the" "blue" "ball"))
(fcg:comprehend-all '("what number of"         "red" "cubes" "are" "the same" "size" "as" "the" "blue" "ball"))
(fcg:comprehend-all '("what number of" "other" "red" "cubes" "are" "the same" "size" "as" "the" "blue" "ball"))

;; EXCEPTIONS
(fcg:comprehend-all '("how many"         "things" "are" "made of" "the same" "material" "as" "the" "blue" "ball"))
(fcg:comprehend-all '("how many" "other" "things" "are" "made of" "the same" "material" "as" "the" "blue" "ball"))
(fcg:comprehend-all '("what number of"         "objects" "are" "made of" "the same" "material" "as" "the" "blue" "ball"))
(fcg:comprehend-all '("what number of" "other" "objects" "are" "made of" "the same" "material" "as" "the" "blue" "ball"))

;; question type 6, count, same color, (+ filter)
;; question type 7, count, same material, (+ filter)
;; question type 8, count, same shape, (+ filter)

;; question type 10, query, same size
(fcg:comprehend-all '("what" "color" "is" "the" "other" "metal" "cube" "that is" "the same" "size" "as" "the" "blue" "ball"))
(fcg:comprehend-all '("what is" "the" "color" "of" "the" "other" "metal" "cube" "that is" "the same" "size" "as" "the" "blue" "ball"))
(fcg:comprehend-all '("the" "other" "metal" "cube" "that is" "the same" "size" "as" "the" "blue" "ball" "is what" "color"))
(fcg:comprehend-all '("there is another" "metal" "cube" "that is" "the same" "size" "as" "the" "blue" "ball" ";" "what" "color" "is it"))
(fcg:comprehend-all '("there is another" "metal" "cube" "that is" "the same" "size" "as" "the" "blue" "ball" ";" "what is its" "color"))
(fcg:comprehend-all '("there is a" "metal" "cube" "that is" "the same" "size" "as" "the" "blue" "ball" ";" "what is its" "color"))

(fcg:formulate
 '((get-context context)
   (filter set-1 context shape-1)
   (filter set-2 set-1 color-1)
   (unique obj-2 set-2)
   (same set-3 obj-2 attr-1)
   (filter set-4 set-3 shape-2)
   (filter set-5 set-4 color-2)
   (unique obj-2 set-5)
   (query target obj-2 attr-2)

   (bind shape-category shape-1 sphere)
   (bind color-category color-1 blue)
   (bind attribute-category attr-1 shape)
   (bind shape-category shape-2 cube)
   (bind color-category color-2 red)
   (bind attribute-category attr-2 ?attr)))

;; EXCEPTIONS:
(fcg:comprehend-all '("there is another" "metal" "thing" "that is" "the same" "size" "as" "the" "blue" "ball" ";" "what is it made of"))
(fcg:comprehend-all '("there is a" "metal" "cube" "that is" "the same" "size" "as" "the" "blue" "ball" ";" "what is it made of"))

(fcg:comprehend-all '("what" "shape" "is" "the" "other" "metal" "thing" "that is" "made of" "the same" "material" "as" "the" "blue" "ball"))
(fcg:comprehend-all '("what is" "the" "shape" "of" "the" "other" "metal" "thing" "that is" "made of" "the same" "material" "as" "the" "blue" "ball"))

(fcg:comprehend-all '("the" "other" "metal" "thing" "that is" "made of" "the same" "material" "as" "the" "blue" "ball" "is what" "shape"))

(fcg:comprehend-all '("there is another" "metal" "thing" "that is" "made of" "the same" "material" "as" "the" "blue" "ball" ";" "what is its" "shape"))
(fcg:comprehend-all '("there is another" "metal" "thing" "that is" "made of" "the same" "material" "as" "the" "blue" "ball" ";" "what" "shape" "is it"))

;; question type 11, query, same color
;; question type 12, query, same material
;; question type 13, query, same shape

;; -------------------------- ;;
;; comparison question family ;;
;; -------------------------- ;;

;; question type 1; equal size, no relate
(fcg:comprehend-all '("do" "the" "blue" "metal" "cube" "and" "the" "red" "thing" "have" "the same" "size"))
(fcg:comprehend-all '("is" "the" "size" "of" "the" "blue" "metal" "cube" "the same as" "the" "red" "thing"))
(fcg:comprehend-all '("is" "the" "blue" "metal" "cube" "the same" "size" "as" "the" "red" "thing"))
(fcg:comprehend-all '("does" "the" "blue" "metal" "cube" "have" "the same" "size" "as" "the" "red" "thing"))
(fcg:comprehend-all '("there is a" "blue" "metal" "cube" ";" "does it have" "the same" "size" "as" "the" "red" "thing"))
(fcg:comprehend-all '("there is a" "blue" "metal" "cube" ";" "is it" "the same" "size" "as" "the" "red" "thing"))
(fcg:comprehend-all '("there is a" "blue" "metal" "cube" ";" "is its" "size" "the same as" "the" "red" "thing"))

;; FORMULATION DOES NOT FIND A SOLUTION HERE!
(fcg:formulate
 '((get-context context)
   (filter set-1 context shape-1)
   (filter set-2 set-1 mat-1)
   (filter set-3 set-2 color-1)
   (unique obj-1 set-3)
   (query a-1 obj-1 attr-1)

   (filter set-4 context shape-2)
   (filter set-5 set-4 color-2)
   (unique obj-2 set-5)
   (query a-2 obj-2 attr-1)

   (equal? target a-1 a-2 attr-1)

   (bind shape-category shape-1 cube)
   (bind material-category mat-1 metal)
   (bind color-category color-1 blue)
   (bind attribute-category attr-1 ?attr)
   (bind shape-category shape-2 thing)
   (bind color-category color-2 red)))

;; EXCEPTIONS
(fcg:comprehend-all '("are" "the" "large" "blue" "cube" "and" "the" "red" "thing" "made of" "the same" "material"))
(fcg:comprehend-all '("is" "the" "large" "blue" "cube" "made of" "the same" "material" "as" "the" "red" "thing"))

;; question type 2; equal color, no relate
;; question type 3; equal shape, no relate
;; question type 4; equal material, no relate

;; question type 1; equal size, relate left
;; question type 2; equal color, relate left
(fcg:comprehend '("do" "the" "large" "metal" "cube" "left of" "the" "red" "thing" "and" "the" "small" "cylinder" "have" "the same" "color"))
(fcg:comprehend '("is" "the" "color" "of" "the" "large" "metal" "cube" "left of" "the" "red" "thing" "the same as" "the" "small" "cylinder"))
(fcg:comprehend '("is" "the" "large" "metal" "cube" "left of" "the" "red" "thing" "the same" "color" "as" "the" "small" "cylinder"))
(fcg:comprehend '("does" "the" "large" "metal" "cube" "left of" "the" "red" "thing" "have" "the same" "color" "as" "the" "small" "cylinder"))
(fcg:comprehend '("there is a" "large" "metal" "cube" "left of" "the" "red" "thing" ";" "does it have" "the same" "color" "as" "the" "small" "cylinder"))
(fcg:comprehend '("there is a" "large" "metal" "cube" "left of" "the" "red" "thing" ";" "is it" "the same" "color" "as" "the" "small" "cylinder"))
(fcg:comprehend '("there is a" "large" "metal" "cube" "left of" "the" "red" "thing" ";" "is its" "color" "the same as" "the" "small" "cylinder"))
;; question type 3; equal shape, relate left
;; question type 4; equal material, relate left

;; question type 1; equal size, relate right
;; question type 2; equal color, relate right
;; question type 3; equal shape, relate right
(fcg:comprehend '("do" "the" "large" "metal" "cube" "and" "the" "red" "thing" "left of" "the" "small" "cylinder" "have" "the same" "shape"))
;; question type 4; equal material, relate right

;; question type 1; equal size, relate both
(fcg:comprehend '("is" "the" "size" "of" "the" "blue" "metal" "cube" "left of" "the" "red" "thing" "the same as" "the" "small" "cylinder" "behind" "the" "rubber" "ball"))
;; question type 2; equal color, relate both
;; question type 3; equal shape, relate both
;; question type 4; equal material, relate both
