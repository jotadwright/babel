(in-package :clevr-dialog-grammar)

;; CAPTIONS
; unique-object

;;;test deze allemaal terug opnieuw
(understand "there is one large object") ;werkt
(understand "there is exactly one large object") ;werkt
(understand "there is one large object in the image") ;werkt
(understand "there is exactly one large object in the image") ;werkt
(understand "the image has exactly one large object") ;werkt
(understand "the image has one large object") ;werkt

(understand "no other large object except for one") ;werkt
(understand "no other large object except for exactly one") ;werkt

(understand "there is a gray object") ;werkt
(understand "there is a gray object in the image") ;werkt

(understand "the image has a yellow cube") ;werkt

(understand "a green sphere is present") ;werkt
(understand "a green sphere is present in the image") ;werkt

;multiple-objects

(understand "there are several metal cylinders") ;werkt
(understand "there are several metal cylinders in the image") ;werkt

(understand "the image has several red spheres") ;werkt

(understand "several small objects are present") ;verkt
(understand "several small objects are present in the image") ;werkt

(understand "a number of large cubes are present") ;werkt
(understand "a number of large cubes are present in the image") ;werkt

(understand "there are 5 metal cylinders") ;werkt
(understand "there are 5 metal cylinders in the image") ;werkt

(understand "the image has 5 red spheres") ;werkt

;object-relations 

(understand "there is a gray object right of a large object") ;werkt
(understand "there is a gray object right of a large object in the image") ;werkt

(understand "the image has a yellow cube left of a gray object") ;werkt
(understand "the image has a yellow cube to the left of a gray object") ;werkt

(understand "a cyan sphere is present right of a large cube") ;werkt
(understand "a cyan sphere is present right of a large cube in the image") ;werkt

(understand "a cyan sphere stands right of a large cube") ;werkt
(understand "a cyan sphere stands right of a large cube in the image") ;werkt


;extreme-location

(understand "there is a gray object sitting at the extreme front") ;werkt 
(understand "there is a gray object sitting at the extreme front of the image") ;werkt

(understand "a gray object is at the extreme front") ;werkt
(understand "a gray object is at the extreme front of the image") ;werkt

(understand "there is a green cube to the front of all things") ;werkt
(understand "there is a green cube to the front of all things in the image") ;werkt

(understand "a matte cylinder is to the front of all things") ;werkt
(understand "a matte cylinder is to the front of all things in the image") ;werkt

(understand "the foremost thing is a purple cylinder") ;werkt
(understand "the foremost thing in the image is a purple sphere") ;werkt

(understand "there is a gray object sitting in the center") ;werkt
 
(understand "a large object is in the center") ;werkt

(understand "there is a blue object at the center of all things") ;werkt

(understand "a block is at the center of all things") ;werkt

(understand "the midmost thing is a cube") ;werkt


;;SEEK/QUERY QUESTIONS

;; seek attribute immediate 

(understand "what is its shape") ;werkt
(understand "what size is it") ;werkt
(understand "what about its color") ;werkt
(understand "what is the material of this thing") ;werkt

;; seek attribute earlier

(understand "what is the shape of the earlier gray object") ;werkt
(understand "what shape is the earlier gray object") ;werkt
(understand "what is the shape of the earlier gray object") ;werkt
(understand "what shape is the earlier gray object") ;werkt

;; seek attribute relation earlier

(understand "if there is a thing right of the earlier gray object what is its shape") ;werkt
(understand "if there is an object left of the above large object what color is it") ;werkt

;; seek attribute relation immediate 

(understand "if there is a thing right of it what is its shape") ;werkt
(understand "if there is an object left of it what color is it") ;werkt


;; seek attribute immediate pair 

(understand "what about color");werkt
(understand "and color");werkt
(understand "how about color");werkt
(understand "what about color");werkt
(understand "and color");werkt
(understand "how about color");werkt

;; seek attribute early object 

(understand "how about the earlier metal object") ;werkt
(understand "what about the earlier metal object") ;werkt
(understand "and that of the earlier metal object") ;werkt
(understand "how about the earlier metal object") ;werkt
(understand "what about the earlier metal object") ;werkt
(understand "and that of the earlier metal object") ;werkt


;; COUNT QUESTIONS

;;count-all objects
(understand "how many things are in the image") ;werkt
(understand "how many things in total") ;werkt
(understand "how many things in total in the image") ;werkt
(understand "what is the number of things") ;werkt
(understand "what is the number of things in the image") ;werkt
(understand "what is the number of all the things in the image") ;werkt
(understand "what is the number of all the things") ;werkt

;;count-other objects
(understand "how many other things are in the image") ;werkt
(understand "what is the count of other things") ;werkt
(understand "what is the count of other things in the image") ;werkt
(understand "what is the number of other things") ;werkt
(understand "what is the number of other things in the image") ;werkt

;;count-all group immediate
(understand "how many things in the group") ;werkt
(understand "how many") ;werkt
(understand "what number of things in the group") ;werkt
(understand "how many of them") ;werkt
(understand "what number") ;werkt
(understand "what number of them") ;werkt
(understand "how many such things in the group") ;werkt
(understand "how many such things") ;werkt

;; count-attribute
(understand "if present how many cubes are in the image") ;werkt
(understand "what is the count of cubes in the image if present") ;werkt
(understand "what is the count of cubes if present") ;werkt
(understand "what is the number of cubes in the image if present") ;werkt
(understand "what is the number of cubes if present") ;werkt
(understand "what number of cubes are present") ;werkt
(understand "what number of cubes are present in the image") ;werkt


;;count-attribute-group
(understand "how many cubes among them") ;werkt
(understand "how many cubes in the group") ;werkt
(understand "what is the count of cubes among them") ;werkt
(understand "what is the number of cubes among them") ;werkt
(understand "what is the count of cubes in this group") ;werkt
(understand "what is the number of cubes in this group") ;werkt
(understand "how many of them are cubes") ;werkt


;;count relation object immediate
(understand "how many things to its right") ;werkt
(understand "how many things to its right in the image") ;werkt
(understand "how many things are present to its right") ;werkt
(understand "how many things are present to its right in the image") ;werkt
(understand "what number of things to its right") ;werkt
(understand "what number of things to its right in the image") ;werkt
(understand "what number of things are present to its right") ;werkt
(understand "what number of things are present to its right in the image") ;werkt
(understand "what is the count of things to its right") ;werkt
(understand "what is the count of things to its right in the image") ;werkt
(understand "how many things does it have to its right") ;werkt
(understand "how many things does it have to its right in the image") ;werkt
(understand "what is the count of things to its right") ;werkt
(understand "what is the count of things to its right in the image") ;werkt

;count relation object immediate pair
(understand "how about to its right")
(understand "what about to its right")
(understand "and to its right")


;;count relation object earlier
(understand "how many things right of the earlier gray object") ;werkt
(understand "how many things are present right of the earlier gray object") ;werkt
(understand "what number of things are present right of the earlier gray object") ;werkt
(understand "what number of things right of the earlier gray object") ;werkt
(understand "how many things does the earlier gray object have to its right") ;werkt
(understand "what is the count of things the earlier gray object has to its right") ;werkt

;;count-exclude-attr-imm ;; werkt 
(understand "how many other things have its shape") ;werkt
(understand "how many other things share its shape") ;werkt
(understand "how many other things have its shape in the image") ;werkt
(understand "how many other things share its shape in the image") ;werkt
(understand "how many other things in the image have its shape") ;werkt
(understand "how many other things in the image share its shape") ;werkt
(understand "what number of other things share its shape") ;werkt
(understand "what number of other things share its shape in the image") ;werkt
(understand "what is the count of other things that share its shape") ;werkt
(understand "what is the count of other things in the image that share its shape") ;werkt

;;count-obj-exclude-early ;; werkt 
(understand "how many other things have the same shape as the earlier gray object") ;werkt
(understand "how many other things share similar shape with the earlier gray object") ;werkt
(understand "how many other things are of same shape as the earlier gray object")
(understand "how many other things in the image have the same shape as the earlier gray object")
(understand "how many other things in the image share similar shape with the earlier gray object")
(understand "how many other things in the image are of same shape as the earlier gray object")
(understand "what number of other things in the image are of same shape with the earlier gray object")
(understand "what number of other things in the image are of same shape as the earlier gray object")
(understand "what number of other things are of same shape as the earlier gray object")
(understand "what number of other things in the image share similar shape with the earlier gray object")
(understand "what number of other things share similar shape with the earlier gray object")



;; EXIST QUESTIONS

;; exist-other-objects

(understand "are there other things in the image") ;werkt
(understand "are there other things") ;werkt
(understand "are there more things in the image") ;werkt
(understand "are there more things") ;werkt
(understand "any other things") ;werkt
(understand "any other things in the image") ;werkt
(understand "does the image have other things") ;werkt


;; exist-attribute

(understand "are there gray things") ;werkt
(understand "are there gray things in the image") ;werkt
(understand "any blue objects") ;werkt
(understand "any blue objects in the image") ;werkt
(understand "are there any gray things") ;werkt
(understand "are there any gray things in the image") ;werkt
(understand "does the image contain any yellow cubes") ;werkt


;; exist-attribute-group

(understand "any cubes among them") ;werkt
(understand "any cubes in this group") ;werkt
(understand "are there any cubes among them") ;werkt
(understand "are there cubes among them") ;werkt
(understand "are there any cubes in this group") ;werkt
(understand "are there cubes in this group") ;werkt
(understand "do they have any cubes") ;werkt
(understand "do they have cubes") ;werkt

;; exist-relation-object-immediate
(understand "are there things present to its right in the image") ;werkt
(understand "are there things to its right in the image") ;werkt
(understand "are there things present to its right") ;werkt
(understand "are there things to its right") ;werkt
(understand "does it have things to its right in the image") ;werkt
(understand "does it have things to its right") ;werkt
(understand "does the earlier gray object have things to its right") ;werkt
(understand "are there things right of itself") ;werkt
(understand "are there things right of itself in the image") ;werkt
(understand "does it have things to the right of itself") ;werkt
(understand "does it have things to the right of itself in the image") ;werkt
(understand "are there things right of the earlier gray object") ;werkt
(understand "are there things present to the right of the earlier gray object") ;werkt

;exist relation object immediate pair
(understand "how about to its right") ;werkt
(understand "what about to its right") ;werkt
(understand "and to its right") ;werkt

;; existence exclude attribute immediate
(understand "are there other things that share its shape in the image") ;werkt
(understand "are there other things that share its shape") ;werkt
(understand "are there other things sharing its shape") ;werkt
(understand "are there other things in the image sharing its shape") ;werkt
(understand "are there other things present in the image sharing its shape") ;werkt
(understand "are there other things present sharing its shape") ;werkt
(understand "does the image have other things that share its shape") ;werkt
(understand "does the image have other things sharing its shape") ;werkt


;; existence relation object earlier
(understand "are there other things that share the same shape with the earlier gray object") ;werkt
(understand "are there other things that are of same shape as the earlier gray object") ;werkt


(understand "does the image have other things that share the same shape with the earlier gray object") ;werkt
(understand "does the image have other things that are of same shape with the earlier gray object")
(understand "are there other things in the image that are of same shape as the earlier gray object") 
(understand "are there other things in the image that share the same shape with the earlier gray object") 
