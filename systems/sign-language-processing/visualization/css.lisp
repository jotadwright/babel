(in-package :slp)

;;-------------------------------------------;;
;; adding hamnosys font to the web-interface ;;
;;-------------------------------------------;;
(progn
  (setf web-interface::*dispatch-table*
        (append web-interface::*dispatch-table*
                (list (web-interface::create-static-file-dispatcher-and-handler 
                       "/hamnosys.css" (babel-pathname 
                                        :directory '("systems" "web-interface" "hamnosys")
                                        :name "hamnosys" :type "css") "text/css"))))
    (progn 
      (web-interface::define-css-link 'hamnosys.css "/hamnosys.css")
      (web-interface::define-css 'main "ham {font-size: 10pt; font-family: hamnosysunicoderegular}")))

;;-----------------;;
;; CSS definitions ;;
;;-----------------;;

;; the whole table
(define-css 'sign-table  "
.sign-table {width: 50%%; border: 1px solid black; border-collapse: collapse;}")

;; empty cells in table
(define-css 'empty "
.empty {background-color: #FFFFFF; upper-border: 1px solid black;}
")

;; row header cells
(define-css 'header "
.header {border: 1px solid black;}
")

;; row header text
(define-css 'header-text "
.header-text {font-weight: bold; font-color: black;}
")

; a cell with manual information
(define-css 'id-gloss-cell "
.id-gloss-cell {background-color: #ffcc00; border: 1px solid black;border-collapse: collapse; text-align: center;}
")

; a cell with hamnosys
(define-css 'hamnosys-cell "
.hamnosys-cell {border: 1px solid black; border-collapse: collapse; padding-right: 10px; padding-left: 10px; text-align: center;}
")

;; text representing an articulation
(define-css 'articulation-text "
.articulation-tag {font-color: black;}
")