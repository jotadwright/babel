;; Copyright 2019 AI Lab, Vrije Universiteit Brussel - Sony CSL Paris

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at

;;     http://www.apache.org/licenses/LICENSE-2.0

;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;=========================================================================

(in-package :fcg)

(deftest test-render ()
  (test-assert
   (loop for i from 1 to 10 always 
         (equal (render (make-instance 'coupled-feature-structure
                                       :left-pole '((root
                                                     (form ((meets u1 u2) (meets u2 u3))))
                                                    (u1 (form ((string u1 "s1"))))
                                                    (u2 (form ((string u2 "s2"))))
                                                    (u3 (form ((string u3 "s3"))))))
                        :render-string-meets-precedes)
                '("s1" "s2" "s3"))))
   (test-assert
    (loop for i from 1 to 10 always 
         (equal (render (make-instance 'coupled-feature-structure
                                       :left-pole '((root
                                                     (form ((meets u1 u2) (meets u2 u3))))
                                                    (u1 (form ((string u1 "s1"))))
                                                    (u2 (form ((string u2 "s2"))))
                                                    (u3 (form ((string u3 "s3"))))))
                        :render-string-meets)
                '("s1" "s2" "s3"))))
   )

;; (test-render)

(deftest test-render-sequences ()
  (test-equalp
   (render (make-instance 'coupled-feature-structure
                       :left-pole '((root
                                     (form ((sequence " " ?r2 ?r3))))
                                    (u1
                                     (form ((sequence "g" ?l2 ?r2))))
                                    (u2
                                     (form ((sequence "flour" ?l4 ?r4))))
                                    (u3
                                     (form ((sequence "120" ?l1 ?r1))))
                                    (u4
                                     (form ((precedes ?r1 ?l2)
                                            (precedes ?r2 ?l4))))))
           :render-sequences)
   '("120" "g" " " "flour"))


  (test-equalp
   (render (make-instance 'coupled-feature-structure
                       :left-pole '((root
                                     (form ((sequence " " ?l3 ?r3))))
                                    (u1
                                     (form ((sequence "g" ?l2 ?r2))))
                                    (u2
                                     (form ((sequence "flour" ?l4 ?r4))))
                                    (u3
                                     (form ((sequence "120" ?l1 ?r1))))
                                    (u4
                                     (form ((precedes ?r1 ?l2)
                                            (precedes ?r2 ?l3)
                                            (precedes ?r2 ?l4)
                                            (precedes ?r3 ?l4)
                                            )))))
           :render-sequences)
   '("120" "g" " " "flour")))

 

  

;;(test-render-sequences)


