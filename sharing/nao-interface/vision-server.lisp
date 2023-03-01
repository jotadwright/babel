(in-package :nao-interface)

;;---------------;;
;; Vision server ;;
;;---------------;;

(export '(vision-server))

; to keep track of all vision servers in use
(defparameter *vision-servers* nil
  "A list containing triples of (ip port container-name)")

(defun vision-port-occupied? (server-port)
  "Check if a port is already occupied"
  (find server-port *vision-servers* :key #'second :test #'equalp))

(defun vision-ip-occupied? (nao-ip)
  "Check if an IP address is already occupied"
  (find *vision-servers* :key #'first :test #'string=))

; vision server has the same ip as the nao-server, but a different port
(defclass vision-server ()
  ((nao-ip :initarg :ip :type string :accessor ip :initform ""
       :documentation "IP address of the nao")
   (server-host :initarg :server-host :type string :accessor server-host :initform "localhost"
                :documentation "Host of the vision server")
   (server-port :initarg :server-port :type string :accessor server-port :initform ""
                :documentation "Port to which the vision server should listen"))
  (:documentation "class for python3 server"))

(defmethod vision-send-http ((vision vision-server) &key endpoint data)
  (assert (alistp data))
  (assert (stringp endpoint))
  (let ((json-data (json::encode-json-alist-to-string data))
        (uri (format nil "http://~a:~a~a"
                     (server-host vision) (server-port vision) endpoint)))
    (with-open-stream
        #+LISPWORKS (stream (drakma::http-request uri :method :post :content json-data
                                          :want-stream t :connection-timeout nil
                                          ;:read-timeout nil :write-timeout nil
                                          ))
        #+SBCL (stream (drakma::http-request uri :method :post :context json-data
                                     :want-stream t :connection-timeout nil))
        #+CCL (stream (drakma::ttp-request uri :method :post :content json-data
                                    :want-stream t :deadline (+ (get-universal-time) 1000000)))
        (json::decode-json-from-string
         (list-of-strings->string
          (stream->list stream))))))

(defgeneric test-server-connection (vision &key silent)
  (:documentation "Returns t if communication with python3-server succeeded, nil if it failed. Use silent = t for quick checks."))

(defmethod test-server-connection ((vision vision-server) &key (silent nil))
  (let ((response (vision-send-http vision :endpoint "/test_connection"
                                 :data '((message . "test-server-connection")))))
    (unless silent
      (format t "Connected to python3 server at port ~a" (server-port vision))
      (warn "If you did not get the server-port of the python3 server, check whether you are connected to the CiscoNao network and that the server is running"))
    (string= (rest (assoc :message response)) "test-server-connection")))

(defun push-vision-server (nao-ip server-port server-host)
  "Pushes a new nao-container to the list of running containers"
  (push (list nao-ip server-port server-host) *vision-servers*))

(defmethod start-vision-server ((vision vision-server) &key (test-connection t))
  ;; check/create folder for nao images
  (ensure-directories-exist (babel-pathname :directory '(".tmp" "nao-img")))
  (cond ((vision-port-occupied? (server-port vision))
         (error (format nil "The port number ~a is already in use" (server-port vision))))
        (t
         ;; Push to the running containers
         (push-vision-server (ip vision) (server-port vision) (server-host vision))
         ;; Test the connection
         (when test-connection
           (test-server-connection vision)))))

; when in configurations, server starts up automatically
(defmethod initialize-instance :after ((vision vision-server) &key (connect-automatically t) &allow-other-keys)
  (when connect-automatically
    (start-vision-server vision)))

(defun pop-vision-server (nao-ip server-port server-host)
  "Pops a nao-container from the list of running containers"
  (setf *vision-servers*
        (remove (list nao-ip server-port server-host)
                *vision-servers* :test #'equalp)))



(defmethod stop-vision-server ((vision vision-server)(nao nao) &key)
  (let ((uri (format nil "http://~a:~a~a"
                     (server-host vision)
                     (server-port vision)
                     "/shutdown")))
    (multiple-value-bind (response code headers
                                   uri stream must-close
                                   reason-phrase)
        (http-request uri :method :get)
      (declare (ignorable response headers uri stream must-close reason-phrase))
      (if (= code 200)
        (progn (pop-vision-server (ip nao) (server-port vision) (server-host vision))
          *nao-servers*)
        (error "Something went wrong during server shutdown (~a:~a)"
               (server-host vision) (server-port vision))))))