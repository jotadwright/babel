(in-package :demo-wtnschp)
;; from Babel2/experiments/colour/monitoring/colours.lisp

(defvar *colour-appearance-model* 'rgb)

(defun check-colours-for-gnuplot (lists-of-colours)
  (let* ((colours (mappend #'identity lists-of-colours))
         (cam *colour-appearance-model*)
         (x-min 0)
         (x-max (ecase cam (lab 100) (luv 100) (rgb 255)))
         (y-min (ecase cam (lab -165) (luv -135) (rgb 0)))
         (y-max (ecase cam (lab 165) (luv 180) (rgb 255)))
	 (z-min (ecase cam (lab -135) (luv -135) (rgb 0)))
	 (z-max (ecase cam (lab 145) (luv 125) (rgb 255))))
    (assert (loop for colour in colours
                  always (and (>= (first colour) x-min)
                              (<= (first colour) x-max)
                              (>= (second colour) y-min)
                              (<= (second colour) y-max)
                              (>= (third colour) z-min)
                              (<= (third colour) z-max))))
    (values cam x-min x-max y-min y-max z-min z-max)))

(defun colours-to-gnuplot (colours stream cam)
  (loop for colour in (sort colours #'< :key #'(lambda (c) (first c)))
        do (format stream "~c~{~a ~}0x~a" #\linefeed colour
                   (ecase cam
                     (rgb (rgb->rgbhex (normalize-rgb colour)))
                     (luv (rgb->rgbhex (xyz->rgb (luv->xyz colour *reference-white-D65*))))
                     (lab (rgb->rgbhex (xyz->rgb (lab->xyz colour *reference-white-D65*)))))))
  (format stream "~ce~c"  #\linefeed #\linefeed))

(defun compute-3d-gnuplot-borders (colours cam)
  (unless (= (run-prog "voro++" :args '("-h")) 0)
    (error "Voro++ needs to be installed to compute borders. Please visit http://math.lbl.gov/voro++/ for more information."))
  (let* ((voro-working-dir (babel-pathname :directory '(".tmp")))
         (voro-filename (string-downcase (format nil "~a" (make-id "borders"))))
         (voro-input-filename (merge-pathnames (make-pathname :name voro-filename) voro-working-dir))
         (voro-output-filename (merge-pathnames (make-pathname :type "gnu") voro-input-filename)))
    (ensure-directories-exist voro-working-dir)
    (with-open-file (stream voro-input-filename :direction :output :if-exists :supersede)
      (loop for colour in colours
            for i = 1 then (incf i)
            do (format stream "~a~{ ~a~}~c" i colour #\linefeed)))
    (ecase cam
      (lab (run-prog (format nil "cd ~a;voro++ -g 0 100 -165 165 -135 145 ~a" voro-working-dir voro-filename)))
      (luv (run-prog (format nil "cd ~a;voro++ -g 0 100 -135 180 -135 125 ~a" voro-working-dir voro-filename)))
      (rgb (run-prog (format nil "cd ~a;voro++ -g 0 255 0    255 0    255 ~a" voro-working-dir voro-filename))))
    voro-output-filename))

;;--------------
;; plot-colours
;;--------------

#|
(defun plot-colours (lists-of-colours pathname &key (point-types '(7)) (point-sizes '(1.5)))
  (assert (= (length lists-of-colours)
             (length point-types)
             (length point-sizes)))
  (multiple-value-bind (cam x-min x-max y-min y-max z-min z-max)
      (check-colours-for-gnuplot lists-of-colours)
    (declare (ignorable x-min x-max))
    (let ((stream (pipe-to-gnuplot)))
      (format stream "~cset term postscript" #\linefeed)
      (format stream "~cset output \"~a\"" #\linefeed (mkstr pathname))
      (format stream "~cset xlabel '~a'" #\linefeed (ecase cam (lab "a*") (luv "u*")))
      (format stream "~cset ylabel '~a'" #\linefeed (ecase cam (lab "b*") (luv "v*")))
      (format stream "~cset xrange [~a:~a]" #\linefeed y-min y-max)
      (format stream "~cset yrange [~a:~a]" #\linefeed z-min z-max)
      (format stream "~cplot " #\linefeed)
      (loop for (point-type . remaining-point-types) on point-types
            for point-size in point-sizes
            do (format stream "'-' using 2:3:4 notitle with points pt ~a ps ~a lc rgb variable"
                       point-type point-size)
            when remaining-point-types
            do (format stream ", "))
      (loop for colours in lists-of-colours
            do (colours-to-gnuplot colours stream cam))
      (finish-output stream)
      (close-pipe stream))))
|#

(defvar *gnuplot-colour-display-stream* nil)

(defun display-colours (lists-of-colours &key (list-of-borders '(nil)) (point-types '(7)) (point-sizes '(1.5)))
  (assert (= (length lists-of-colours) (length list-of-borders) (length point-types) (length point-sizes)))
  (multiple-value-bind (cam x-min x-max y-min y-max z-min z-max)
      (check-colours-for-gnuplot lists-of-colours)
    (unless *gnuplot-colour-display-stream*
       (setf *gnuplot-colour-display-stream* (pipe-to-gnuplot)))
    (let ((stream *gnuplot-colour-display-stream*))
      (when *gnuplot-colour-display-stream*
        (format stream "~creset" #\linefeed))
      (format stream "~cset term x11" #\linefeed)
      (format stream "~cset hidden3d" #\linefeed)
      (format stream "~cset xlabel '~a'" #\linefeed (ecase cam (lab "l*") (luv "l*") (rgb "r")))
      (format stream "~cset ylabel '~a'" #\linefeed (ecase cam (lab "a*") (luv "u*") (rgb "g")))
      (format stream "~cset zlabel '~a'" #\linefeed (ecase cam (lab "b*") (luv "v*") (rgb "b")))
      (format stream "~cset xrange [~a:~a]" #\linefeed x-min x-max)
      (format stream "~cset yrange [~a:~a]" #\linefeed y-min y-max)
      (format stream "~cset zrange [~a:~a]" #\linefeed z-min z-max)
      (format stream "~csplot " #\linefeed)
      (loop for (point-type . remaining-point-types) on point-types
            for point-size in point-sizes
            for borders in list-of-borders
            for colours in lists-of-colours
            do (format stream "'-' using 1:2:3:4 notitle with points pt ~a ps ~a lc rgb variable"
                       point-type point-size)
            when borders
            do (let ((border-file-name (compute-3d-gnuplot-borders colours cam)))
                 (format stream ", '~a' notitle with lines" border-file-name))
            when remaining-point-types
            do (format stream ", "))
      (loop for colours in lists-of-colours
            do (colours-to-gnuplot colours stream cam))
      (finish-output stream)
      ;; pipe to gnuplot should not be closed as you want to be able
      ;; to rotate the 3d plot
      )))

(defun display-category-set (category-set)
  (let ((category-colours (mapcar #'prototype (entities category-set))))
    (display-colours (list category-colours)
                     :list-of-borders '(t)
                     :point-types '(7)
                     :point-sizes '(1.5))))  

(defun display-category-and-entity-set (category-set entity-set)
  (let ((category-colours (mapcar #'prototype (entities category-set)))
        (entity-colours (mapcar #'rgbcolor (entities entity-set))))
    (display-colours (list category-colours entity-colours)
                     :list-of-borders '(t nil)
                     :point-types '(7 3)
                     :point-sizes '(1.5 1))))

(defun colours->s-dot (colours)
  (let ((graph '(((s-dot::margin "0")) s-dot::graph))
        (id 0))
    (loop for colour in colours
          do (push
              `(s-dot::node
                ((s-dot::id ,(format nil "colour~a" (incf id)))
                 (s-dot::label "")
                 (s-dot::shape "box")
                 (s-dot::fillcolor ,(format nil "#~a" (rgb->rgbhex colour)))
                 (s-dot::style "filled")
                 (s-dot::height "0.75")))
              graph))
    (reverse graph)))