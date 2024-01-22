(ql:quickload :fillmores-grammar)
(in-package :fillmores-grammar)

#| This code is an implementation of examples described in the article
   "The Mechanisms of 'Construction Grammar'" by Charles J. Fillmore (1988) |#

;; external syntax <- internal syntax
;; postconditions <- preconditions

; cat (short for category) accepts Noun, Verb, Adjective

;; writing a readme

;; Joe
;; (comprehend-and-formulate "Joe" :cxn-inventory *fillmores-cxns*)

;; she
;; (comprehend-and-formulate "she" :cxn-inventory *fillmores-cxns*)

;; A child.
(comprehend "a child" :cxn-inventory *fillmores-cxns*)

;; A foolish child.
;; The book.
;; The air.
;; A book.
;; Foolish child that I was.
;; I was a foolish child.
;; I was chairman of the committee.
;; I was president of the club.
;; She is a member of the club.
;; The president of the club resigned.
;; I removed the bottle from the book.
;; second cousin once removed
;; She gives a book to Joe.
;; Were she here, she would vote.
;; Have I lied?
;; Was she here?
;; Never have I lied.
;; Where have I lied?
;; Why was she here?
;; She contributed to this book.
;; She seems worth knowing.
;; She did her best.
;; The sooner you learn how to pronouce her name, the more likely is she to go out with you.
