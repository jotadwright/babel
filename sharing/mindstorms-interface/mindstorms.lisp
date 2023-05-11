(in-package :mindstorms-interface)

;;;;;;;;;;;;;;;;;
;; Mindstorms Servers ;;
;;;;;;;;;;;;;;;;;

;; This will keep track of the running mindstorms containers and their ports and container names
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *mindstorms-servers* nil
  "A list containing triples of (ip port container-name)")

(defun mindstorms-servers ()
  *mindstorms-servers*)

(defun port-occupied? (server-port)
  "Check if a port is already occupied"
  (find server-port (mindstorms-servers) :key #'second :test #'equalp))

(defun ip-occupied? (mindstorms-ip)
  "Check if an IP address is already occupied"
  (find mindstorms-ip (mindstorms-servers) :key #'first :test #'string=))

(defun push-mindstorms-server (mindstorms-ip server-port server-host)
  "Pushes a new mindstorms-container to the list of running containers"
  (push (list mindstorms-ip server-port server-host) *mindstorms-servers*))

(defun pop-mindstorms-server (mindstorms-ip server-port server-host)
  "Pops a mindstorms-container from the list of running containers"
  (setf *mindstorms-servers*
        (remove (list mindstorms-ip server-port server-host)
                *mindstorms-servers* :test #'equalp)))

;;;;;;;;;;;;;;;;;;;;;
;; Mindstorms Robot Class ;;
;;;;;;;;;;;;;;;;;;;;;

(export '(mindstorms))

(defclass mindstorms ()
  ((server-host :initarg :server-host :type string :accessor server-host :initform "localhost"
                :documentation "Host of the mindstorms server")
   (server-port :initarg :server-port :type string :accessor server-port :initform ""
                :documentation "Port to which the mindstorms server should listen"))
  (:documentation "Mindstorms robot class"))


;; Implementing the Robot Interface API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export '(make-new-connection robot-connected-p disconnect-robot))

(defmethod make-new-connection ((mindstorms mindstorms) &key (test-connection t))
  (unless (container-name mindstorms)
    (let ((container-name (format nil "mindstorms-~a-~a" (ip mindstorms) (server-port mindstorms))))
      (setf (container-name mindstorms) container-name)))
  (start-mindstorms-server mindstorms :test-connection test-connection))

(defmethod robot-connected-p ((mindstorms mindstorms))
  "Check if the mindstorms is still connected"
  (and (ip-occupied? (ip mindstorms))
       (port-occupied? (server-port mindstorms))))

(defmethod disconnect-robot ((mindstorms mindstorms))
  (stop-mindstorms-server mindstorms))

;; Starting and stopping mindstorms servers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export '(stop-all-mindstorms-servers))

(defgeneric start-mindstorms-server (mindstorms &key test-connection)
  (:documentation "Make connection to this mindstorms"))

(defgeneric stop-mindstorms-server (mindstorms &key)
  (:documentation "Stop the connection to the mindstorms"))

#|
(defun docker-container-exists-p (container-name)
  (not (string= (first (exec-and-return "docker" "container" "inspect" container-name)) "[]")))

(defmethod start-mindstorms-server ((mindstorms mindstorms) &key (test-connection t))
  ;; check/create folder for mindstorms images
  (ensure-directories-exist (babel-pathname :directory '(".tmp" "mindstorms-img")))
  ;; start the mindstorms server
  (cond ((ip-occupied? (ip mindstorms))
         (error (format nil "The IP address ~a is already in use" (ip mindstorms))))
        ((port-occupied? (server-port mindstorms))
         (error (format nil "The port number ~a is already in use" (server-port mindstorms))))
        (t
         (if (docker-container-exists-p (container-name mindstorms))
           ;; Container does exist, make it run again
           (run-prog "docker" :args `("start" ,(container-name mindstorms)))
           ;; Container does not yet exist, create one
           (run-prog "docker" :args `("run" "-it" "-d"
                                      "-p" ,(format nil "~a:80" (server-port mindstorms))
                                      "-v" ,(format nil "~a:/mindstormsqi/src"
                                                    (babel-pathname :directory '("sharing" "mindstorms-interface" "flask-server-v2")))
                                      "-v" ,(format nil "~a:/mindstormsqi/src/img" (babel-pathname :directory '(".tmp" "mindstorms-img")))
                                      "--name" ,(container-name mindstorms)
                                      "mindstormsqi-python")))
         ;; Push to the running containers
         (push-mindstorms-server (ip mindstorms) (server-port mindstorms) (container-name mindstorms))
         ;; Give some time to start the container
         (sleep 2)
         ;; Start the mindstorms server inside the docker container
         (run-prog "docker" :args `("exec" "-d"
                                    ,(container-name mindstorms)
                                    "/usr/bin/python" "/mindstormsqi/src/mindstorms_flask_server.py"
                                    "--robot-ip" ,(ip mindstorms)
                                    "--robot-port" ,(port mindstorms)))
         (when test-connection
           (sleep 2)
           (test-server-connection mindstorms)))))

(defmethod stop-mindstorms-server ((mindstorms mindstorms) &key)
  "Stops the python server associated to the given mindstorms instance. Updates *mindstorms-servers*"
  (run-prog "docker" :args `("stop" ,(container-name mindstorms)))
  (pop-mindstorms-server (ip mindstorms) (server-port mindstorms) (container-name mindstorms))
  *mindstorms-servers*)

(defun stop-all-mindstorms-servers ()
  "Stops all known mindstorms-servers."
  (when *mindstorms-servers*
    (loop for entry in *mindstorms-servers*
          do (run-prog "docker" :args `("stop" ,(third entry))))
    (setf *mindstorms-servers* nil))
  *mindstorms-servers*)
|#


#|
;; cannot start Python from Lisp
(defmethod start-nao-server ((nao nao) &key (test-connection t))
  ;; flask server v3 no longer requires Docker
  ;; check/create folder for nao images
  (ensure-directories-exist (babel-pathname :directory '(".tmp" "nao-img")))
  ;; start the nao server
  (cond ((ip-occupied? (ip nao))
         (error (format nil "The IP address ~a is already in use" (ip nao))))
        ((port-occupied? (server-port nao))
         (error (format nil "The port number ~a is already in use" (server-port nao))))
        (t
         ;; start the Python 2 script
         (run-prog "/usr/local/bin/python" :args `(,(babel-pathname :directory '("sharing" "nao-interface" "flask-server-v3")
                                                                    :name "flask_server" :type "py")
                                                   "--robot-ip" ,(ip nao)
                                                   "--server-port" ,(server-port nao))
                   :wait nil)
         ;; Push to the running containers
         (push-nao-server (ip nao) (server-port nao) (server-host nao))
         ;; Give some time to start the server
         ;; Loading the Mask RCNN model takes some time
         (sleep 10)
         ;; Test the connection
         (when test-connection
           (test-server-connection nao)))))
|#

(defmethod start-mindstorms-server ((mindstorms mindstorms) &key (test-connection t))
  ;; check/create folder for mindstorms images
  (ensure-directories-exist (babel-pathname :directory '(".tmp" "mindstorms-img")))
  (cond ((ip-occupied? (ip mindstorms))
         (error (format nil "The IP address ~a is already in use" (ip mindstorms))))
        ((port-occupied? (server-port mindstorms))
         (error (format nil "The port number ~a is already in use" (server-port mindstorms))))
        (t
         ;; Push to the running containers
         (push-mindstorms-server (ip mindstorms) (server-port mindstorms) (server-host mindstorms))
         ;; Test the connection
         (when test-connection
           (test-server-connection mindstorms)))))

(defmethod stop-mindstorms-server ((mindstorms mindstorms) &key)
  (let ((uri (format nil "http://~a:~a~a"
                     (server-host mindstorms)
                     (server-port mindstorms)
                     "/shutdown")))
    (multiple-value-bind (response code headers
                                   uri stream must-close
                                   reason-phrase)
        (http-request uri :method :get)
      (declare (ignorable response headers uri stream must-close reason-phrase))
      (if (= code 200)
        (progn (pop-mindstorms-server (ip mindstorms) (server-port mindstorms) (server-host mindstorms))
          *mindstorms-servers*)
        (error "Something went wrong during server shutdown (~a:~a)"
               (server-host mindstorms) (server-port mindstorms))))))

(defun stop-all-mindstorms-server ()
  (when *mindstorms-servers*
    (loop for server in *mindstorms-servers*
          for uri = (format nil "http://~a:~a~a"
                            (third server)
                            (second server)
                            "/shutdown")
          do (http-request uri :method :get))
    (setf *mindstorms-servers* nil))
  *mindstorms-servers*)
                           




;; Sending and receiving data from/to the mindstorms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric mindstorms-send-http (mindstorms &key endpoint data)
  (:documentation "Encode the data to json, send it to the mindstorms,
    decode the response and return it. This method expects the data
    to be an a-list."))

(defmethod mindstorms-send-http ((mindstorms mindstorms) &key endpoint data)
  (assert (alistp data))
  (assert (stringp endpoint))
  (let ((json-data (encode-json-alist-to-string data))
        (uri (format nil "http://~a:~a~a"
                     (server-host mindstorms) (server-port mindstorms) endpoint)))
    (with-open-stream
        #+LISPWORKS (stream (http-request uri :method :post :content json-data
                                          :want-stream t :connection-timeout nil
                                          :read-timeout nil :write-timeout nil))
        #+SBCL (stream (http-request uri :method :post :context json-data
                                     :want-stream t :connection-timeout nil))
        #+CCL (stream (http-request uri :method :post :content json-data
                                    :want-stream t :deadline (+ (get-universal-time) 1000000)))
        (decode-json-from-string
         (list-of-strings->string
          (stream->list stream))))))
           

;; Testing the connection with mindstorms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric test-server-connection (mindstorms &key silent)
  (:documentation "Returns t if communication with mindstorms-server succeeded, nil if it failed. Use silent = t for quick checks."))

(defmethod test-server-connection ((mindstorms mindstorms) &key (silent nil))
  (let ((response (mindstorms-send-http mindstorms :endpoint "/test_connection"
                                 :data '((message . "test-server-connection")))))
    (unless silent
      (speak mindstorms (format nil "Connected to Babel 2 at port ~a" (server-port mindstorms)))
      (warn "Did Mindstorms speak? If not, check whether you are connected to the same WiFi network (CiscoMindstorms) and that Docker is running!"))
    (string= (rest (assoc :message response)) "test-server-connection")))

;; Getting files from mindstorms
;;;;;;;;;;;;;;;;;;;;;;;;;;

(export '(mindstorms-scp-get))

(defgeneric mindstorms-scp-get (mindstorms remote-file local-file)
  (:documentation "Gets remote-file from the Mindstorms and stores it in local-file"))

(defmethod mindstorms-scp-get ((mindstorms mindstorms) remote-file local-file)
  (utils::scp-get (ip mindstorms) (username mindstorms) (password mindstorms)
                  remote-file local-file))

;; Clear images from mindstorms
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric clear-mindstorms-recordings (mindstorms &key dir)
  (:documentation "Remove all images from the Mindstorms's hard drive"))

(defmethod clear-mindstorms-recordings ((mindstorms mindstorms)
                                 &key (dir "/var/persistent/home/mindstorms/recordings/cameras"))
  (utils::ssh-clear-dir (ip mindstorms) (username mindstorms) (password mindstorms) dir))
