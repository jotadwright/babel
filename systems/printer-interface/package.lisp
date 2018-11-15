(defpackage printer-interface
  (:use :cl)
  (:import-from :uiop :run-program)
  (:import-from :split-sequence :split-sequence)
  (:export :print-document :available-printers :default-printer))