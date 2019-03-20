(in-package :mwm)

;; based on http://www.brucelindbloom.com/index.html?Eqn_ChromAdapt.html
(defparameter *reference-white-D50* (list 0.96422 1.00000 0.82521)
  "Represents natural daylight.")
(defparameter *reference-white-D65* (list 0.95047 1.00000 1.08883)
  "Represents natural daylight.")
(defparameter *reference-white-c* (list 0.98074 1.00000 1.18232))

(defun check-domain (vals min max)
  (loop for val in vals
     unless (and (>= val min) (<= val max))
     do (return-from check-domain nil))
  vals)

;; ---------------
;; + XYZ <-> LAB +
;; ---------------

;; based on http://www.brucelindbloom.com/Eqn_XYZ_to_Lab.html
(defun xyz->lab (xyz reference-white)
  "Converts values from XYZ [0,1] to CIEL*a*b* colour space."
  (let ((x (first xyz))
	(y (second xyz))
	(z (third xyz))
	(x_r (first reference-white))
	(y_r (second reference-white))
	(z_r (third reference-white))
	(epsilon (/ 216 24389))
	(kappa (/ 24389 27)))
    (let ((x-_r (/ x x_r))
	  (y-_r (/ y y_r))
	  (z-_r (/ z z_r)))
      (let ((f_x (if (> x-_r epsilon)
		     (expt x-_r (/ 1 3))
		     (/ (+ (* kappa x-_r) 16) 116)))
	    (f_y (if (> y-_r epsilon)
		     (expt y-_r (/ 1 3))
		     (/ (+ (* kappa y-_r) 16) 116)))
	    (f_z (if (> z-_r epsilon)
		     (expt z-_r (/ 1 3))
		     (/ (+ (* kappa z-_r) 16) 116))))
	(list (- (* 116 f_y) 16)
	      (* 500 (- f_x f_y))
	      (* 200 (- f_y f_z)))))))

;; based on http://www.brucelindbloom.com/Eqn_Lab_to_XYZ.html
(defun lab->xyz (lab reference-white)
  "Converts values from CIEL*a*b* colour space to XYZ [0,1]."
  (let ((l (first lab))
	(a (second lab))
	(b (third lab))
	(x_r (first reference-white))
	(y_r (second reference-white))
	(z_r (third reference-white))
	(epsilon (/ 216 24389))
	(kappa (/ 24389 27)))
    (let ((f_y (/ (+ l 16) 116)))
      (let ((f_x (+ (/ a 500) f_y))
	    (f_z (- f_y (/ b 200))))
	(let ((x-_r (if (> (expt f_x 3) epsilon)
			(expt f_x 3)
			(/ (- (* 116 f_x) 16) kappa)))
	      (y-_r (if (> l (* kappa epsilon))
			(expt (/ (+ l 16) 116) 3)
			(/ l kappa)))
	      (z-_r (if (> (expt f_z 3) epsilon)
			(expt f_z 3)
			(/ (- (* 116 f_z) 16) kappa))))
	  (list (* x-_r x_r)
		(* y-_r y_r)
		(* z-_r z_r)))))))

;; ---------------
;; + XYZ <-> LUV +
;; ---------------

;; based on http://www.brucelindbloom.com/index.html?Eqn_Luv_to_XYZ.html
(defun luv->xyz (luv reference-white)
  "Converts values from CIEL*u*v* colour space to XYZ [0,1]."
  (let ((l (first luv))
	(u (second luv))
	(v (third luv))
	(x_r (first reference-white))
	(y_r (second reference-white))
	(z_r (third reference-white))
	(epsilon (/ 216 24389))
	(kappa (/ 24389 27)))
    (let* ((y (if (> l (* kappa epsilon))
		  (expt (/ (+ l 16) 116) 3)
		  (/ l kappa)))
	   (den_0 (+ x_r (* 15 y_r) (* 3 z_r)))
	   (u_0 (/ (* 4 x_r) den_0))
	   (v_0 (/ (* 9 y_r) den_0)))
      (let ((a (/ (- (/ (* 52 l) (+ u (* 13 l u_0))) 1) 3))
	    (b (- (* 5 y)))
	    (c (- (/ 1 3)))
	    (d (* y (- (/ (* 39 l) (+ v (* 13 l v_0))) 5))))
	(let ((x (/ (- d b) (- a c))))
	  (list x y (+ (* x a) b)))))))

;; based on http://www.brucelindbloom.com/Eqn_XYZ_to_Luv.html
(defun xyz->luv (xyz reference-white)
  "Converts values from XYZ [0,1] to CIEL*u*v* colour space."
  (let ((x (first xyz))
	(y (second xyz))
	(z (third xyz))
	(x_r (first reference-white))
	(y_r (second reference-white))
	(z_r (third reference-white))
	(epsilon (/ 216 24389))
	(kappa (/ 24389 27)))
    (let ((den (+ x (* 15 y) (* 3 z)))
	  (den_r (+ x_r (* 15 y_r) (* 3 z_r)))
	  (y-_r (/ y y_r)))
      (let ((uq (/ (* 4 x) den))
	    (vq (/ (* 9 y) den))
	    (uq_r (/ (* 4 x_r) den_r))
	    (vq_r (/ (* 9 y_r) den_r))
	    (l (if (> y-_r epsilon)
		   (- (* 116 (expt y-_r (/ 1 3))) 16)
		   (* kappa y-_r))))
	(list l (* 13 l (- uq uq_r)) (* 13 l (- vq vq_r)))))))

;; ---------------
;; + RGB <-> XYZ +
;; ---------------

;; based on http://www.brucelindbloom.com/Eqn_XYZ_to_RGB.html
;;      and http://www.brucelindbloom.com/index.html?Eqn_RGB_XYZ_Matrix.html
(defun xyz->rgb (xyz &optional (gamma 2.2) (allow-unreproducable-colours t))
  "Transformation from XYZ to Adobe (1998) RGB working space (gamma=2.2, d65)."
  (let ((x (first xyz))
	(y (second xyz))
	(z (third xyz)))
    (let ((rgb (list (+ (* 2.04148 x) (* -0.564977 y) (* -0.344713 z))
		     (+ (* -0.969258 x) (* 1.87599 y) (* 0.0415557 z))
		     (+ (* 0.0134455 x) (* -0.118373 y) (* 1.01527 z)))))
      (when (and (not allow-unreproducable-colours)
		 (not (check-domain rgb 0 1)))
       (return-from xyz->rgb nil))
      (mapcar #'(lambda (x) (expt (max (min x 1) 0) 
				  (/ 1 gamma)))
	      rgb))))

;; based on http://www.brucelindbloom.com/Eqn_RGB_XYZ_Matrix.html
(defun rgb->xyz (rgb &optional (gamma 2.2))
  "Transforms from RGB (Adobe 1998, d=2.2, d65) to XYZ."
  (let ((r (expt (first rgb) gamma))
	(g (expt (second rgb) gamma))
	(b (expt (third rgb) gamma)))
    (let ((xyz (list (+ (* 0.5767309 r) (* 0.1855540 g) (* 0.1881852 b))
		     (+ (* 0.2973769 r) (* 0.6273491 g) (* 0.0752741 b))
		     (+ (* 0.0270343 r) (* 0.0706872 g) (* 0.9911085 b)))))
      (mapcar #'(lambda (x) (max 0 (min 1 x))) xyz))))


;; ---------------
;; + RGB <-> LAB +
;; ---------------

(defun rgb->lab (rgb)
  "Transforms from RGB to CIEL*a*b*"
  (let ((scaled-rgb (mapcar (lambda (x) (/ x 255.0)) rgb)))
    (xyz->lab (rgb->xyz scaled-rgb) *reference-white-D65*)))

(defun lab->rgb (lab)
  "Transforms from CIEL*a*b* to RGB"
  (let ((scaled-rgb (xyz->rgb (lab->xyz lab *reference-white-D65*))))
    (mapcar (lambda (x) (* x 255.0)) scaled-rgb)))

;; -----------------
;; + _ -> RGBHEX +
;; -----------------

(defun rgb->rgbhex (rgb)
  "Converts a RGB [0,1] value to an 8-bit hexadecimal string."
  (format nil "~{~2,'0X~}" rgb))

(defun normalize-rgb (rgb)
  (mapcar #'(lambda (x) (float (/ x 255.0))) rgb))

(defun lab->rgbhex (lab reference-white)
  "Convert from CIEL*a*b* coordinates to rgbhex"
  (rgb->rgbhex (xyz->rgb (lab->xyz lab reference-white))))