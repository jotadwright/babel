;;;;; -----------------------------------------------------------------------------------
;;;;; (c) Sony Computer Science Laboratories Paris
;;;;;     Author: Remi van Trijp - remi.vantrijp@sony.com
;;;;; -----------------------------------------------------------------------------------

(in-package :asdf)

(defsystem :cl-wikimedia
  :description "A lightweight library for interfacing with the Wikimedia REST APIs."
  :author "Remi van Trijp <remi.vantrijp@sony.com>"
  :version "0.1"
  :depends-on (:utils #+lispworks :drakma #-lispworks dexador
               :cl-json :yason :cl-ppcre)
  :components ((:file "package")
               (:file "config")
               (:file "wikimedia-rest-api"))) 