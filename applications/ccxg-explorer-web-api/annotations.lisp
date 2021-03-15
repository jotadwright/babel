(in-package :propbank-english)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; This file contains code for annotating PropBank-annotated corpora in the format  ;;
;; required by the CCxG explorer.                                                   ;;
;;                                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The annotations are stored here:
(defparameter *CCxG-explorer-annotations* (make-hash-table))

(defun store-annotations ()
  (store *CCxG-explorer-annotations* (make-pathname :directory (pathname-directory (or *load-truename*
                                                                                       *compile-file-truename*)) :name "annotations" :type "store")))

(defun restore-annotations ()
  (setf *CCxG-explorer-annotations* (restore (make-pathname :directory (pathname-directory (or *load-truename*
                                                                                       *compile-file-truename*)) :name "annotations" :type "store"))))

(defun make-ccxg-explorer-annotations-for-propbank-sentences (corpus-name list-of-propbank-sentences)
  "Makes ccxg explorer annotations for list-of-propbank-sentences and populates *CCxG-explorer-annotations*."
  (loop for n from 0
        for propbank-sentence in list-of-propbank-sentences
        do (when (= 0 (mod n 1000))
             (format t "(~a)~%" n))
        append (make-ccxg-explorer-annotations-for-propbank-sentence propbank-sentence)
        into ccxg-explorer-annotations
        finally (setf (gethash corpus-name *CCxG-explorer-annotations*) ccxg-explorer-annotations)))

(defun make-ccxg-explorer-annotations-for-propbank-sentence (propbank-sentence)
  "Makes ccxg explorer annotations for a propbank-sentence."
  (loop for frame in (propbank-frames propbank-sentence)
        if (spacy-benepar-compatible-annotation propbank-sentence (frame-name frame) :selected-role-types 'core-only)
        collect (make-ccxg-explorer-annotation-for-propbank-frame propbank-sentence frame)))

(defun make-ccxg-explorer-annotation-for-propbank-frame (propbank-sentence propbank-frame)
  "Makes ccxg explorer annotation for a propbank-frame of a propbank-sentence."
  (let* ((ts-unit-structure (left-pole-structure (initial-transient-structure propbank-sentence)))
         (core-units-with-role (remove-if #'(lambda (unit-with-role)
                                              (and (search "ARGM" (role-type (car unit-with-role)))
                                                   (not (search "ARGM-PRD" (role-type (car unit-with-role))))))
                                          (units-with-role ts-unit-structure propbank-frame))))
    (loop with utterance = (sentence-string propbank-sentence)
          with roleset = (intern (upcase (frame-name propbank-frame)))
          with v-lemma = (second (find 'lemma (rest (v-unit core-units-with-role)) :key #'first :test #'equalp))
          for (role . unit) in core-units-with-role
          for role-type = (intern (upcase (role-type role)))
          for indices = (indices role)
          for string = (role-string role)
          for pos = (second (find 'syn-class (rest unit) :key #'first :test #'equalp))
          collect (if (eq role-type 'V)
                    `((:role-type ,role-type) (:pos ,pos) (:string ,string) (:indices ,indices) (:roleset ,roleset) (:lemma ,v-lemma))
                    `((:role-type ,role-type) (:pos ,pos) (:string ,string) (:indices ,indices))) into roles
          finally (return `((:utterance ,utterance)
                           (:roles ,roles))))))