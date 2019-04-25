(in-package :plot-raw-data)

(export '(create-function-plot))

(defun create-function-plot (equations
                             &key (title nil) (captions nil)
                             (plot-file-name nil)
                             (plot-directory nil)
                             (graphic-type "pdf")
                             (key-location "below")
                             (x-min 0) (x-max 1)
                             (y-min -1) (y-max 1)
                             (colors *great-gnuplot-colors*)
                             (draw-y1-grid t)
                             (x-label "Number of games played")
                             (y1-label nil)
                             (grid-line-width 0.5)
                             (open t) (dashed nil)
                             (fsize 10)
                             (typeface "Helvetica"))
  "Plot several functions using gnuplot. This function expects a list
   of function equations (in a list of strings) specified with x as a variable
   and in a format that can be handled by gnuplot (e.g. 4*x instead of 4x).
   Example: (create-function-plot '(\"2*x\" \"3*x\" \"4*x\"))"
  (let ((colors (loop for color in colors
                      for other-color in *great-gnuplot-colors*
                      collect (or color other-color)))
        (captions (loop for f in equations
                        for i from 0
                        collect (or (nth i captions)
                                    (format nil "f~a(x)=~a" (+ i 1) f))))
        (file-path (babel-pathname :name (or plot-file-name "function-plot")
                                   :type (if (equal graphic-type "postscript") "ps" graphic-type)
                                   :directory (or plot-directory '(".tmp")))))
    (ensure-directories-exist file-path)
    (with-open-stream
      (stream (monitors::pipe-to-gnuplot))
      (set-gnuplot-parameters stream
                              :output file-path :terminal graphic-type :title (or title "")
                              :draw-y1-grid draw-y1-grid :grid-line-width grid-line-width
                              :key-location key-location :x-label x-label :y1-label y1-label
                              :y2-label nil :y1-min y-min :y1-max y-max
                              :y2-min nil :y2-max nil
                              :dashed dashed :fsize fsize :typeface typeface)
      (set-range stream "x" x-min x-max)
      (format stream "~cset grid back noxtics" #\linefeed)
    
      (format stream "~cset ytics nomirror" #\linefeed)
      (format stream "~cset style fill transparent solid 0.20 border" #\linefeed)
      
      (loop for f in equations
            for i from 1
            do (format stream "~cf~a(x) = ~a" #\linefeed i f))

      (format stream "~cplot " #\linefeed)
      (loop for f in equations
            for source-number from 0
            for color = (nth (mod source-number (length colors)) colors)
            do (format stream "f~a(x) title ~s dt ~a lc rgb ~s ~:[~;, ~]"
                       (+ source-number 1)
                       (nth source-number captions) ;; caption
                       (+ 2 (mod source-number 8)) ;; dash type
                       color ;; color
                       (< source-number (- (length equations) 1)) ;; adding , to the end or not
                       ))

      (format stream "~cexit~c"  #\linefeed #\linefeed)
      (finish-output stream)
      ;(close-pipe stream)
      (when open
        (sleep 0.5)
        (open-file-in-os file-path)))))

;(create-function-plot '("2*x" "3*x" "4*x") :title "pretty functions")    