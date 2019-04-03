
;; (ql:quickload :clevr-evaluation)

(in-package :clevr-evaluation)

(comprehend (preprocess-sentence "what color is the large cube?"))

;; FCG MONITORS
(activate-monitor trace-fcg)
;; IRL MONITORS
(activate-monitor trace-irl-in-web-browser)
;; CLEVR MONITOR
(activate-monitor clevr-web-monitor)

(evaluate-clevr
 ;; file containing context data
 (merge-pathnames (make-pathname :directory '(:relative "CLEVR" "CLEVR-v1.0" "scenes")
                                 :name "CLEVR_val_full_per_line" :type "json")
                  cl-user:*babel-corpora*)
 ;; folder containing question data
 (merge-pathnames (make-pathname :directory '(:relative "CLEVR" "CLEVR-v1.0" "questions" "val"))
                  cl-user:*babel-corpora*)
 ;; how many contexts to evaluate (default nil; nil = all)
 :nr-of-contexts 1
 ;; how many questions per context (default nil; nil = all; questions are shuffled)
 :nr-of-questions 4
 ;; wait after every question (for demo purposes; default nil)
 ;:wait-per-question t
 )
