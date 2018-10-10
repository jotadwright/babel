(in-package :scene-generator)

;;;; Ajax Processor
;;;;;;;;;;;;;;;;;;;

(defparameter *scene-ajax-processor* (make-instance 'ajax-processor :server-uri "/svg/ajax"))

(push (create-ajax-dispatcher *scene-ajax-processor*)
      *dispatch-table*)

;;;; JavaScript function definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; a collection of different javascript code blocks
(defparameter *js-libs* nil)

;; use this function to get your javascript libraries loaded by the client
(defun define-js-library (id url)
  (let ((found (assoc id *js-libs*)))
    (if found 
        (setf (rest found) url)
        (setf *js-libs* (append *js-libs* (list (cons id url)))))))

(defun get-combined-js-library-definitions ()
  (format nil "~{~a~%~}"
          (loop for (nil . url) in  *js-libs*
                collect (with-html-output-to-string (*standard-output*)
                          (:script :type "text/javascript"
                                   :src url)))))

(define-js-library 'jQuery "https://ajax.googleapis.com/ajax/libs/jquery/3.3.1/jquery.min.js")

(defparameter *js-functions* (make-hash-table))

(defun define-js (id js)
  (setf (gethash id *js-functions*) js))

(defun get-combined-js-definitions ()
  (with-html-output-to-string (*standard-output*)
    (:script :type "text/javascript"
             (str (format nil "~% //<![CDATA[~%  ~{~a~%~} ~%//]]>~%"
                          (loop for js-func being the hash-values of *js-functions*
                                collect js-func into funcs
                                finally return (reverse funcs)))))))

;;;; Request Queue
;;;;;;;;;;;;;;;;;;

(defvar *request-queue* nil)

;; empty the requests list when the page is reset
(defun reset-requests () (setf *request-queue* nil))

;;; The ajax handler to get to the instructions
(defun-ajax get-requests () (*scene-ajax-processor*)
  (let ((result nil))
    #+(or mcl openmcl) (ccl:without-interrupts 
                         (setf result *request-queue*
                               *request-queue* nil))
    #-(or mcl openmcl) (setf result *request-queue*
			     *request-queue* nil)
    (with-html-output-to-string (*standard-output*)
      (:requests (str (list-of-strings->string (reverse result)))))))

#|
function requestCallback(result) {
       $(result).find("requests").children().each(function () {
          var request = $(this).prop("tagName");
          switch (request) {
          case 'RESET':
      		$('#content').empty();
        	break;
    		case 'ADD-ELEMENT':
      		$(this).children().each(function () {
        		return $("#content").append(this);
        	});
        	break;
          case 'REPLACE-ELEMENT-CONTENT':
      		var id = $(this).children("id").text();
        	var content = $(this).children("content").contents();
        	$('#' + id).html(content);
        	break;
      	  case 'APPEND-TO-ELEMENT':
      		var id = $(this).children("id").text();
        	var content = $(this).children("content").contents();
        	$('#' + id).append(content);
        	break;
      	  case 'RELOAD-CONTENT':
      		$('#content').html($('#content').html());
        	break;
      	  default:
      		alert('Unhandled request');
                break;
    	  }
  	});
}
|#

(define-js 'request-callback
           (ps:ps
             (defun requests-callback (result)
               (ps:chain ($ result) (find "requests") (children)
                         (each (lambda ()
                                 (let ((request (ps:chain ($ this) (prop "tagName"))))
                                   (ps:switch request
                                              ("reset"
                                               (progn
                                                 (ps:chain ($ "#content") (empty))
                                                 break))
                                              ("add-element"
                                               (progn
                                                 (ps:chain ($ this) (children)
                                                           (each (lambda ()
                                                                   (ps:chain ($ "#content") (append this)))))
                                                 break))
                                              ("replace-element-content"
                                               (let ((id (ps:chain ($ this) (children "id") (text)))
                                                     (content (ps:chain ($ this) (children "content") (contents))))
                                                 (ps:chain ($ (+ "#" id)) (html content))
                                                 break))
                                              ("append-to-element"
                                               (let ((id (ps:chain ($ this) (children "id") (text)))
                                                     (content (ps:chain ($ this) (children "content") (contents))))
                                                 (ps:chain ($ (+ "#" id)) (append content))
                                                 break))
                                              ("reload-content"
                                               (progn
                                                 (ps:chain ($ "#content") (html (ps:chain ($ "#content") (html))))
                                                 break))
                                              ("add-js"
                                               (progn
                                                 (ps:chain ($ this) (children)
                                                           (each (lambda ()
                                                                   (ps:chain ($ "head") (append this)))))
                                                 break))
                                              (ps:default
                                               (let ((msg (+ "Unhandled request " request)))
                                                 (alert msg))))))))
               (ps:chain window (set-timeout get-requests 200)))))

(define-js 'get-requests
           (ps:ps
             (defun get-requests ()
               (ajax_get_requests requests-callback))))

(define-js 'init-request-queue
           (ps:ps
             (defun initialize-request-queue ()
               (ps:chain window (set-timeout get-requests 500)))))
                                                    

;;;; Standard page
;;;;;;;;;;;;;;;;;;

(defmacro standard-page ((&key title) &body body)
  `(with-html-output-to-string (*standard-output* nil :indent t :prologue t)
     (:html :lang "en"
            :xmlns "http://www.w3.org/1999/xhtml"
      (:head 
       (:meta :http-equiv "Content-Type" 
              :content "text/html;charset=utf-8")
       (:title ,title)
       (str (get-combined-js-library-definitions))
       (str (generate-prologue *scene-ajax-processor*))
       (str (get-combined-js-definitions)))
      (:body :onLoad "window.setTimeout(getRequests,500);"
             (:div :id "content"
                   ,@body)))))

(hunchentoot:define-easy-handler (svg :uri "/svg") ()
  (with-html-output (*standard-output* nil :indent t :prologue nil)
    (standard-page (:title "Robot Scene") ())))

;;;; Get stuff on the main page
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun add-element (element)
  (if (listp element)
    (mapcar #'add-element element)
    (push (with-html-output-to-string (*standard-output*)
            (:add-element (str element)))
          *request-queue*))
  t)

(defun append-to-element (id element)
  (push (with-html-output-to-string (*standard-output*)
          (:append-to-element
           (:id (str id))
           (:content (str element))))
        *request-queue*)
  t)

(defun replace-element-content (id element)
  (push (with-html-output-to-string (*standard-output*)
          (:replace-element-content
           (:id (str id))
           (:content (str element))))
        *request-queue*)
  t)

(defun clear-page ()
  (reset-requests)
  (push (with-html-output-to-string (*standard-output*)
          (:reset))
        *request-queue*))

(defun reload-content ()
  (push (with-html-output-to-string (*standard-output*)
          (:reload-content))
        *request-queue*)
  t)

(defun add-js (js)
  (push (with-html-output-to-string (*standard-output*)
          (:add-js (str js)))
        *request-queue*)
  t)

(defun-ajax reset () (*scene-ajax-processor*)
  (clear-page)
  (with-html-output-to-string (*standard-output*)
    nil))

