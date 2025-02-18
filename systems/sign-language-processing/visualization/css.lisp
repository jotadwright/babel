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
.sign-table {width: 80%; table-layout:fixed;}")

;; empty cells in table
(define-css 'empty "
.empty {background-color: #FFFFFF; upper-border: 1px solid black;}
")

;; row header cells
(define-css 'header "
.header {padding-left: 5px; padding-right:5px; font-weight:500; width:25px; text-align: center;}
")

;; row header text
(define-css 'header-text "
.header-text {font-weight: bold; font-color: black;}
")

; a cell with manual information
(define-css 'id-gloss-cell "
.id-gloss-cell {padding-left: 10px; padding-right: 10px; background: #e28744; border-radius: 5px; box-shadow: 1px 1px 1px #ddd; text-align: center; overflow: hidden; text-overflow: ellipsis; word-wrap: break-word; width: fit-content;}")

; a cell with hamnosys
(define-css 'hamnosys-cell "
.hamnosys-cell {padding-left: 10px; padding-right: 10px; background #fff; text-align: center; overflow: hidden; text-overflow: ellipsis; word-wrap: break-word; width: fit-content;}
")

;; text representing an articulation
(define-css 'articulation-text "
.articulation-tag {color: white; display: inline-block; font-weight:400px;}
")