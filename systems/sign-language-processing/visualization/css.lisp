(in-package :slp)

;;----------------------------------;;
;; links to helvetica hamnosys font ;;
;;----------------------------------;;
  (setf web-interface::*dispatch-table*
        (append web-interface::*dispatch-table*
                (list (web-interface::create-static-file-dispatcher-and-handler 
                       "/HelveticaNeue-Hamnosys-Roman.ttf" (merge-pathnames
                                        (make-pathname :directory '(:relative "GeoQuery-LSFB" "helveticaneue-hamnosys" "roman")
                                        :name "HelveticaNeue-Hamnosys-Roman" :type "ttf")
                                        *babel-corpora*))
                      (web-interface::create-static-file-dispatcher-and-handler 
                       "/HelveticaNeue-Hamnosys-Roman.woff" (merge-pathnames
                                        (make-pathname :directory '(:relative "GeoQuery-LSFB" "helveticaneue-hamnosys" "roman")
                                        :name "HelveticaNeue-Hamnosys-Roman" :type "woff")
                                        *babel-corpora*))
                      (web-interface::create-static-file-dispatcher-and-handler 
                       "/HelveticaNeue-Hamnosys-Roman.woff2" (merge-pathnames
                                        (make-pathname :directory '(:relative "GeoQuery-LSFB" "helveticaneue-hamnosys" "roman")
                                        :name "HelveticaNeue-Hamnosys-Roman" :type "woff2")
                                        *babel-corpora*))
                      (web-interface::create-static-file-dispatcher-and-handler 
                       "/HelveticaNeue-Hamnosys-Bold.ttf" (merge-pathnames
                                        (make-pathname :directory '(:relative "GeoQuery-LSFB" "helveticaneue-hamnosys" "bold")
                                        :name "HelveticaNeue-Hamnosys-Bold" :type "ttf")
                                        *babel-corpora*))
                      (web-interface::create-static-file-dispatcher-and-handler 
                       "/HelveticaNeue-Hamnosys-Bold.woff" (merge-pathnames
                                        (make-pathname :directory '(:relative "GeoQuery-LSFB" "helveticaneue-hamnosys" "bold")
                                        :name "HelveticaNeue-Hamnosys-Bold" :type "woff")
                                        *babel-corpora*))
                      (web-interface::create-static-file-dispatcher-and-handler 
                       "/HelveticaNeue-Hamnosys-Bold.woff2" (merge-pathnames
                                        (make-pathname :directory '(:relative "GeoQuery-LSFB" "helveticaneue-hamnosys" "bold")
                                        :name "HelveticaNeue-Hamnosys-Bold" :type "woff2")
                                        *babel-corpora*))
                      (web-interface::create-static-file-dispatcher-and-handler 
                       "/courier-hamnosys.ttf" (merge-pathnames
                                        (make-pathname :directory '(:relative "GeoQuery-LSFB" "courier-hamnosys")
                                        :name "Courier-HamNoSys-Normal" :type "ttf")
                                        *babel-corpora*))
                      (web-interface::create-static-file-dispatcher-and-handler 
                       "/courier-hamnosys.woff" (merge-pathnames
                                        (make-pathname :directory '(:relative "GeoQuery-LSFB" "courier-hamnosys")
                                        :name "Courier-HamNoSys-Normal" :type "woff")
                                        *babel-corpora*))
                      (web-interface::create-static-file-dispatcher-and-handler 
                       "/courier-hamnosys.woff2" (merge-pathnames
                                        (make-pathname :directory '(:relative "GeoQuery-LSFB" "courier-hamnosys")
                                        :name "Courier-HamNoSys-Normal" :type "woff2")
                                        *babel-corpora*)))))
    ;(progn 
      ;(web-interface::define-css-link 'hamnosys.css "/hamnosys.css")
      ;(web-interface::define-css 'main "ham {font-size: 10pt; font-family: hamnosysunicoderegular}"))))

 
(web-interface::define-css-link 'cwasa.css "https://vhg.cmp.uea.ac.uk/tech/jas/vhg2025/cwa/cwasa.css")
 

;;-----------------;;
;; CSS definitions ;;
;;-----------------;;

(define-css 'helvetica-hamnosys-normal "
@font-face {
    font-family: 'Helvetica Neue Hamnosys';
    src: url('http://localhost:8000/HelveticaNeue-Hamnosys-Roman.woff2') format('woff2'),
         url('http://localhost:8000/HelveticaNeue-Hamnosys-Roman.woff') format('woff'),
         url('http://localhost:8000/HelveticaNeue-Hamnosys-Roman.ttf') format('truetype');
    font-weight: normal;
    font-style: normal;

}")

(define-css 'helvetica-hamnosys-bold "
@font-face {
    font-family: 'Helvetica Neue Hamnosys';
    src: url('http://localhost:8000/HelveticaNeue-Hamnosys-Bold.woff2') format('woff2'),
         url('http://localhost:8000/HelveticaNeue-Hamnosys-Bold.woff') format('woff'),
         url('http://localhost:8000/HelveticaNeue-Hamnosys-Bold.ttf') format('truetype');
    font-weight: bold;
    font-style: bold;

}")

(define-css 'courier-hamnosys-normal "
@font-face {
    font-family: 'Courier Hamnosys';
    src: url('http://localhost:8000/courier-hamnosys.woff2') format('woff2'),
         url('http://localhost:8000/courier-hamnosys.woff') format('woff'),
         url('http://localhost:8000/courier-hamnosys.ttf') format('truetype');
    font-weight: normal;
    font-style: normal;

}")


;; overwriting some of the standard css definitions of the web-interface to switch fonts to ones that include HamNoSys

(define-css 'web-interface::main "
body, td { font-size: 9pt; font-family: Helvetica Neue Hamnosys, Helvetica Neue, Helvetica, Arial;}
body {background-color:#FFFFFF;}
a { color: #000066; text-decoration:none; }
a:hover {text-decoration: underline}
a.button { font-size: 8pt;}
hr { border:0px;color:#777;background-color:#777;height:1px;width:100%;}
")

(define-css 'web-interface::pprint "
div.pprint { margin-top:0px;}
div.pprint * { font-family: Courier HamNoSys, Courier;font-weight:normal;font-size:9pt;line-height:10px;display:inline-block; }
div.pprint span.table { margin-top:0px; margin-bottom:0px; display:inline-table;border-collapse:collapse;}
div.pprint span.table > span { display:table-cell;vertical-align:top; margin-top:0px; margin-bottom:0px; }
")

;; the whole table
(define-css 'sign-table  "
.sign-table {width: 100%; fixed-layout:fixed;}")

;; empty cells in table
(define-css 'empty "
.empty {background-color: transparent; upper-border: 1px solid black;}
")

;; row header cells
(define-css 'header "
.header {padding-right:5px; font-weight:500; width:25px; text-align: center;}
")

;; row header text
(define-css 'header-text "
.header-text {font-weight: bold; font-color: black;}
")

; a cell with manual information
(define-css 'id-gloss-cell "
.id-gloss-cell {width: 100% ;padding-left: 10px; padding-right: 10px; background: #159A9C; border-radius: 5px; box-shadow: 1px 1px 1px #ddd; text-align: center; overflow: hidden; text-overflow: ellipsis; word-wrap: break-word;}")

; a cell with hamnosys
(define-css 'hamnosys-cell "
.hamnosys-cell {padding-left: 10px; padding-right: 10px; background: transparent; text-align: center; overflow: hidden; text-overflow: ellipsis; word-wrap: break-word; width: fit-content;}
")

;; text representing an articulation
(define-css 'articulation-text "
.articulation-tag {color: white; display: inline-block; font-weight:400px; font-size: 10px;}
")

(define-css 'play-sigml-button "
.playsigml {color: white; font-size: 10px;font-weight: 400px; margin-left: 2px; border: none; background-color: transparent; overflow: hidden;}
")

(define-css 'h1 "
h1 {background-color:#002333; color:#FFFFFF; margin-top: 2px; padding: 10px; margin-left: 0; margin-right: 5px;}")

(define-css 'h2 "
h2 {background-color:none; color:black; margin-top: 2px; margin-left: 10px; margin-right: 5px;}")

(define-css 'h3 "
h3 {background-color:none; color:black; margin-left: 10px; font-family: Helvetica Neue;}")

(define-css 'divav "
.divAv {
	box-sizing: border-box;
	position: relative; background: #DEEFE7;
	width: 100%; height: 100%;
	margin: 0px; border: 1px solid; padding: 0px;
}")

(define-css 'banner "
.banner {
	background: #B4BEC9;
        padding: 10px;
        margin-left: 0px;
}")

(web-interface::clear-page)