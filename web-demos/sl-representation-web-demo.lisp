(ql:quickload :slp)
(in-package :slp)


(define-css 'main "p {font-size: 11pt}")
(define-css 'body "body {margin: 10px;}")
(define-css 'h1 "h1 {background-color:#002333; color:#FFFFFF; margin-top: 2px; padding: 10px; margin-left: 0; margin-right: 5px;}")
(define-css 'h1-banner ".banner {background: #B4BEC9; padding: 10px; margin-left: 0px;}")
(define-css 'simple-table " .simple-table, .simple-th, .simple-td {margin-top: 30px; border: 1px solid;border-collapse: collapse; text-align: center; padding: 10px;}")
(define-css 'th " .simple-th { text-align: center; }")
(define-css 'td " .simple-td { text-align: left; }")
(define-css 'ham "ham {font-size: 10pt; font-family: hamnosysunicoderegular}")

(defun header ()
  (activate-monitor trace-slp)
  (add-element
   `((h1) "Representing Continuously Signed Expressions with FCG"))
  (add-element
   `((p) "This demo describes how continuously signed forms can be represented in Fluid Construction Grammar using the slp package."))
  (add-element
   `((h2) "Contents"))
  (add-element
   `((ol)
     ((li) "HamNoSys")
     ((li) "Multilinear form descriptions")
     ((li) "A predicate notation for multilinear representations")
     ((li) "Reading elan-files")
     ((li) "Visually displaying the predicates"))))

(defun hamnosys-section ()
  (add-element
   `((h2 :class "banner") "HamNoSys"))
  (add-element
   `((p) "HamNoSys is a phonetic notation system for writing down the signed forms of any sign language. It was developed at the University of Hamburg as a sign language equivalent for the IPA. The HamNoSys alphabet consists of 210 symbols representing the possible handshapes, orientations, locations and movements of a sign. The general structure of a HamNoSys string is defined as follows:"))
   (add-element `((img :src "/HamNoSys-structure.png")))
   (add-element
   `((p) "Sequences of signs can be represented by placing their hamnosys strings in sequence, indicating the separation between signs using a comma:"))
   (add-element `((img :src "/HamNoSys-sequence.png")))
   (add-element
   `((p) "However, this method assumes an exlusively linear ordering of signs, which does not align with the reality of many continuously signed expressions, where both manual articulators produce different signs at the same time. Consider the following example for instance:"))
   (add-element `((video :src "/multilinear-signing.mp4")))
   (add-element
    `((p) "(example retrieved from the "
      ((a :href "") "GeoQuery-LSFB corpus") ")"))
   (add-element
    `((p) "In this example, the left hand signs ... and the right hand ..."))
   (add-element
    `((p) "With HamNoSys strings alone, such constructions cannot be represented. However, there exist many annotation software tools that provide the tools for multilinear annotations (e.g. Elan/ILex)")))

(defun multilinear-form-descriptions ()
  (add-element
   `((h2 :class "banner") "Multilinear Form Descriptions"))
  (add-element
   `((p) "Annotation software such as Elan or Ilex provide the tools to annotate videos using multiple annotation layers that are synchronised to the timetrack of the video. The graphical result of an annotation in Elan for the example above would be displayed as follows"))
  (add-element `((video :src "/Elan-annotation.png")))
  (add-element
   `((p) "As shown in the example, a tool such as Elan can successfully convey that there are two signs occurring at the same time, and that each has its own HamNoSys representation. The advantage of the Elan software is that annotation files are saved as XML files that can easily be processed computationally. However, looking at the XML file of this small example shows that it contains a lot of information specific to the Elan software, and that the XML does not directly specify how the two signs in the example relate to each other specifically. Rather, it assigns a begin and end time to each of them, which happen to overlap in this case. For processing purposes, we develop a representation that is independent of the Elan XML structure (i.e. the same representation could be extracted from similar software such as ILex) and that highlights the relevant temporal relationships between the signs, abstracting away from their concrete start and end-points."))
   )

(defun predicate-notation ()
  (add-element
   `((h2 :class "banner") "A predicate notation for multilinear representations"))
  (add-element
   `((p) "The notation we propose uses predicates to describe signed expressions. There are two types of predicates which occur in the notation: predicates describing the form of a sign (using hamnosys) and predicates describing the temporal relationships between these signs. The different predicate types can be described as follows:"))
  (add-element `((table :class "simple-table")
                 ((thead)
                  ((tr)
                   ((th :class "simple-th") "Sign Type")
                   ((th :class "simple-th") "Predicate Form")))
                 ((tbody)
                  ((tr)
                   ((td :class "simple-td") "two handed sign")
                   ((td :class "simple-td") "two-hand-articulation(sign-1, hamnosys)"))
                  ((tr)
                   ((td :class "simple-td") "left handed sign")
                   ((td :class "simple-td") "left-hand-articulation(sign-1, hamnosys)"))
                  ((tr)
                   ((td :class "simple-td") "right handed sign")
                   ((td :class "simple-td") "right-hand-articulation(sign-1, hamnosys)")))))
  (add-element
   `((table :class "simple-table")
     ((thead)
      ((tr)
       ((th :class "simple-th") "Relationship Type")
       ((th :class "simple-th") "Predicate Form")
       ((th :class "simple-th") "Description")))
     ((tbody)
      ((tr)
       ((td :class "simple-td") "temporal adjacency")
       ((td :class "simple-td") "adjacent(sign-1, sign-2)")
       ((td :class "simple-td") "sign-2 starts immediately after sign-1"))
      ((tr)
       ((td :class "simple-td") "equal start time")
       ((td :class "simple-td") "start-coincides(sign-1, sign-2)")
       ((td :class "simple-td") "sign-1 and sign-2 start at the same time"))
      ((tr)
       ((td :class "simple-td") "inclusion in")
       ((td :class "simple-td") "during(sign-1, sign-2)")
       ((td :class "simple-td") "the starting and end time of sign-1 are included within the temporal interval of sign-2"))
      ((tr)
       ((td :class "simple-td") "equal end")
       ((td :class "simple-td") "end-coincides(sign-1, sign-2)")
       ((td :class "simple-td") "sign-1 and sign-2 end at the same time")))))
  (add-element
   `((p) "The relationships can be visualized as follows:"))
  (add-element `((img :src "/predicate-types.png")))
  (add-element
   `((p) "An example of a signed expression written down using the predicate notation:"))
  (add-element `((img :src "/predicate-types.png"))))


(defun read-elan ()
  (add-element
   `((h2 :class "banner") "Transforming elan-files to the predicate notation"))
  (add-element
  `((p) "Annotation files created in Elan with the"
    ((a :href "") " provided template ")
    "can automatically be transformed into predicate-notation by reading in the XML-file and passing the resulting object to the "
    ((code) "elan->xml") " function"))
  (add-element `((p) "This function takes into account the start and end points of all annotation segments in the xml file and extracts the necessary temporal relations, creating the more abstract predicate form.")))

(defun visual-display ()
  (add-element
   `((h2 :class "banner") "Graphical representation"))
  (add-element
  `((p) "The predicate-notation can be represented in a graphical format which presents the temporal relationships in a more intuitive manner (similar to the way they are represented in Elan, only without information about the exact length of each sign). In addition, integration of the CWASA avatar allows users to create animations for each sign in the expression on the fly. CWASA uses the hamnosys notation for each sign. An example of the graphical and avatar representation for a signed expression in predicate form is shown below:"))
  (add-element (make-html (elan->predicates (read-xml "/Users/liesbetdevos/Projects/GeoQuery-LSFB/elan-files/1_0_1.eaf"))))
  (add-element `((p) "")))
  
(defun full-demo ()
  (header)
  (hamnosys-section)
  (multilinear-form-descriptions)
  (predicate-notation)
  (read-elan)
  (visual-display))

(full-demo)

; (web-interface:create-static-html-page "sign-language-representation" (full-demo))



