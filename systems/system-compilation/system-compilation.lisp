(in-package :system-compilation)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                     ;;
;; Compiling ASDF systems for delivery ;;
;;                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Use this system to compile ASDF systems for delivery. 

;; 1. Make sure the system is defined (e.g. by evaluating the defsystem macro in its asd file.)
;; 2. Call 'compile-asdf-system, pass the name of the system and the target directory for the compiled code.
;; 3. The file with the compiled code can be loaded by the externale application (load PATHNAME))

;; (ql:quickload :system-compilation)

(defun compile-asdf-system (system-name target-directory)
  "Compiles an ASDF system, defined through asdf:defsystem, into a single loadable file."
  (let* ((system-directory (pathname-directory (slot-value (asdf:find-system system-name) 'asdf::absolute-pathname)))
         (system-file-name (format nil "~(~a~)--all-systems" system-name))
         (target-directory (pathname-directory (uiop:ensure-directory-pathname (uiop:ensure-absolute-pathname target-directory))))
         (target-file-name (format nil "~(~a~)" system-name))
         (target-file-type #+:ARM64-DARWIN "64yfasl"))
    ;; Create compiled system file
    (asdf:operate 'asdf:monolithic-compile-bundle-op system-name)
    ;; Move file to target-directory
    (ensure-directories-exist (make-pathname :directory target-directory))
    (rename-file (asdf:apply-output-translations (make-pathname :directory system-directory :name system-file-name :type target-file-type))
                 (make-pathname :directory target-directory :name target-file-name :type target-file-type))))


#|
(compile-asdf-system :nlp-tools "~/Desktop/nlp-tools/")
(compile-asdf-system :dexador "~/Desktop/dexador/")
(compile-asdf-system :drakma "~/Desktop/drakma/")

(load "/Users/paul/Desktop/propbank/propbank-grammar.64yfasl")
|#