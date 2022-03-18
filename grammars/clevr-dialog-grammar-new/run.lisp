
(ql:quickload :clevr-dialog-grammar)

(in-package :clevr-dialog-grammar)

(activate-monitor trace-fcg)
(activate-monitor trace-fcg-search-process)
(deactivate-all-monitors)

;; default configurations:
(set-configuration *fcg-constructions*
                   '((:cxn-supplier-mode . :ordered-by-label-and-score)
                     (:priority-mode . :nr-of-applied-cxns)))

;; Seq2seq configurations:
(set-configurations *fcg-constructions*
                    '((:cxn-supplier-mode . :seq2seq-heuristic)
                      (:priority-mode . :seq2seq-heuristic-additive)
                      ;; running the server locally
                      (:seq2seq-endpoint . "http://localhost:8888/next-cxn")
                      ;; running through penelope server
                      ;; WATCH OUT; the clevr dialog pretrained model is not yet
                      ;; present on penelope...
                      ;(:seq2seq-endpoint . "https://ehai.ai.vub.ac.be/seq2seq-heuristics/next-cxn")
                      (:seq2seq-model-comprehension . "clevr_dialog_comprehension_model")))

(set-configurations *fcg-constructions*
                    '((:de-render-mode . :de-render-scene-and-memory)))

(understand "if there is an object above it what is its shape")
;; first dialog mnist
(understand "objects")
(understand "are there small brown digits")
(understand "are there brown digits in the image" )
(understand "are there digits in a yellow background among them" )
(understand "what is its size" )
(understand "how many flat digits are there")
(understand "how many cubes among them" )
(understand "are there any flat digits in the image" )
(understand "what is the color of the digit above it" )
(understand "what is the material of the above large object" )
(understand "what is the count of other objects")
(understand "does the image have other things that share the same shape with the earlier gray object") )
(understand "how many other things in the image have the same shape as the earlier gray object" )
(understand "there is a gray object right of a small object" )


;;; first dialog clevr 
(understand "what is the material of the above large object" )
(understand "there is an object right of a large object")
(understand "what is the material of the above large object" )
(understand "what is the count of other objects")

(understand "if there is an object right of the earlier gray object what is its color")
(understand "what is its material" )
(understand "and that of the previous gray object" )
(understand "does the picture contain any blocks" )
(understand "what is the shape of the above large object")
(understand "what color is the above metal object")
(understand "are there other objects present in the view sharing its color")
(understand "are there other objects that share the same shape with the aforementioned gray object" )

(setf *fcg-constructions* *clevr-dialog*)

(progn
  (set-configuration *fcg-constructions* :search-algorithm :best-first)
  (set-configuration *fcg-constructions* :heuristics '(:nr-of-applied-cxns :nr-of-units-matched))
  (set-configuration *fcg-constructions* :heuristic-value-mode :sum-heuristics-and-parent))

(understand "what color is the above metal thing")
(understand "if there is an object above it what is its shape")

