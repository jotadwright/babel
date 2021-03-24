;;;; monitors.lisp

(in-package :clevr-learning)

;;;; Printing dots
(define-monitor print-a-dot-for-each-interaction
                :documentation "Prints a '.' for each interaction
                 and prints the number after :dot-interval")

(define-event-handler (print-a-dot-for-each-interaction interaction-finished)
  (cond ((= (interaction-number interaction) 1)
         (format t "~%."))
        ((= (mod (interaction-number interaction)
                 (get-configuration experiment :dot-interval)) 0)
         (format t ". (~a)~%" (interaction-number interaction))
         (wi:clear-page))
        (t (format t "."))))

;;;; export type hierarchy after series
(define-monitor export-type-hierarchy
                :class 'store-monitor
                :file-name (make-file-name-with-time
                            (babel-pathname :name "type-hierarchy" :type "pdf"
                                            :directory '("experiments" "clevr-learning" "graphs"))))

(defun export-type-hierarchy (type-hierarchy path)
  (type-hierarchy->image
   type-hierarchy :render-program "circo" :weights? t
   :path (make-pathname :directory (pathname-directory path))
   :file-name (pathname-name path)
   :format "pdf"))

(define-event-handler (export-type-hierarchy run-series-finished)
  (let ((th (get-type-hierarchy (grammar (learner experiment))))
        (path (file-name monitor)))
    (export-type-hierarchy th path)))

;;;; export type hierarchy every nth interaction
(define-monitor export-type-hierarchy-every-nth-interaction
                :class 'store-monitor
                :file-name (make-file-name-with-time
                            (babel-pathname :name "type-hierarchy" :type "pdf"
                                            :directory '("experiments" "clevr-learning" "graphs"))))

(define-event-handler (export-type-hierarchy-every-nth-interaction interaction-finished)
  (let ((interaction-nr (interaction-number (current-interaction experiment)))
        (n (get-configuration-or-default experiment :export-interval 100)))
    (when (= (mod interaction-nr n) 0)
      (let ((th (get-type-hierarchy (grammar (learner experiment))))
            (path (make-pathname
                   :directory (pathname-directory (file-name monitor))
                   :name (format nil "~a-~a"
                                 (pathname-name (file-name monitor))
                                 interaction-nr)
                   :type (pathname-type (file-name monitor)))))
        (type-hierarchy-components->images
         th :render-program "circo" :weights? t
         :path (pathname-directory path)
         :file-name (pathname-name path) :format "pdf"
         :minimum-component-size 1)))))

;;;; export grammar after series
(define-monitor export-learner-grammar
                :class 'store-monitor
                :file-name (make-file-name-with-time
                            (babel-pathname :name "learner-grammar" :type "store"
                                            :directory '("experiments" "clevr-learning" "raw-data"))))

(defun export-grammar (cxn-inventory path)
  #-ccl (cl-store:store cxn-inventory path))

(define-event-handler (export-learner-grammar run-series-finished)
  (export-grammar (grammar (learner experiment))
                  (file-name monitor)))

;;;; export grammar every nth interaction
(define-monitor export-learner-grammar-every-nth-interaction
                :class 'store-monitor
                :file-name (make-file-name-with-time
                            (babel-pathname :name "learner-grammar" :type "store"
                                            :directory '("experiments" "clevr-learning" "raw-data"))))

(define-event-handler (export-learner-grammar-every-nth-interaction interaction-finished)
  (let ((interaction-nr (interaction-number (current-interaction experiment)))
        (n (get-configuration-or-default experiment :export-interval 100)))
    (when (= (mod interaction-nr n) 0)
      (let ((pathname
             (make-pathname :directory (pathname-directory (file-name monitor))
                            :name (format nil "~a-~a" (pathname-name (file-name monitor))
                                          interaction-nr)
                            :type (pathname-type (file-name monitor)))))
        (export-grammar (grammar (learner experiment))
                        pathname)))))





(defun get-all-export-monitors ()
  '("export-type-hierarchy"
    "export-learner-grammar"))
  
    
