(in-package :cl-user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  ;;
;; Getting Started with cl-framenet ;;
;;                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This package requires that either your the global variable *framenet-data-directory* is set to point to the directory where the raw framenet data can be found, or that a file framenet-data.store is found in this directory.

(ql:quickload :cl-framenet)
(in-package :cl-framenet)

(ql:quickload :s-xml)
(ql:quickload :web-interface)

(defun parse-frame )

(defun read-frame-from-xml (frame-name)
  (let ((opinion-frame-file (merge-pathnames (make-pathname :directory '(:relative "frame")
                                                            :name (string-downcase frame-name :start 1)
                                                            :type "xml")
                                             *framenet-data-directory*)))
    (with-open-file (inputstream opinion-frame-file :direction :input)
      (xmls:parse inputstream))))


;; (read-frame-from-xml 'opinion)


;; Frames ;;
;;;;;;;;;;;;

(defun xml-frame-name (xml-frame)
  "Returns the name of the frame as a symbol."
  (make-symbol (string-upcase (xmls:xmlrep-attrib-value "name" xml-frame))))

;; (xml-frame-name (read-frame-from-xml 'opinion))


;; Frame Elements ;;
;;;;;;;;;;;;;;;;;;;;

(defun make-frame-elements (xml-frame)
  "Returns a list of frame-element object for the given frame."
  (loop for FE in (xmls:xmlrep-find-child-tags "FE" xml-frame)
        collect (make-instance 'frame-element
                               :name (xml-frame-element-name FE)
                               :abbrev (xml-frame-element-abbrev FE)
                               :core-type (xml-frame-element-core-type FE))))

;; (setf *A* (make-frame-elements (read-frame-from-xml 'opinion)))


(defun xml-frame-element-name (xml-frame-element)
  "Returns the name of the frame-element as a symbol."
  (let ((value-as-string (xmls:xmlrep-attrib-value "name" xml-frame-element)))
    (unless (equalp "" value-as-string)
      (make-symbol (string-upcase value-as-string)))))

;; (xml-frame-element-name (fourth (xml-frame-elements (read-frame-from-xml 'opinion))))


(defun xml-frame-element-abbrev (xml-frame-element)
  "Returns the name of the frame-element as a symbol."
  (let ((value-as-string (xmls:xmlrep-attrib-value "abbrev" xml-frame-element)))
    (unless (equalp "" value-as-string)
      (make-symbol (string-upcase value-as-string)))))

;; (xml-frame-element-abbrev (first (xml-frame-elements (read-frame-from-xml 'opinion))))


(defun xml-frame-element-core-type (xml-frame-element)
  "Returns the name of the frame-element as a symbol."
  (let ((value-as-string  (xmls:xmlrep-attrib-value "coreType" xml-frame-element)))
    (unless (equalp "" value-as-string)
      (make-symbol (string-upcase value-as-string)))))

;; (xml-frame-element-core-type (fourth (xml-frame-elements (read-frame-from-xml 'opinion))))

