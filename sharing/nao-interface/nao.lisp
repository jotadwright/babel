
(in-package :nao-interface)

;;;;;;;;;;;;;;;;;
;; Nao Servers ;;
;;;;;;;;;;;;;;;;;

;; This will keep track of the running nao containers and their ports and container names
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *nao-servers* nil
  "A list containing triples of (ip port container-name)")

(defun nao-servers ()
  *nao-servers*)

(defun port-occupied? (server-port)
  "Check if a port is already occupied"
  (find server-port (nao-servers) :key #'second :test #'equalp))

(defun ip-occupied? (nao-ip)
  "Check if an IP address is already occupied"
  (find nao-ip (nao-servers) :key #'first :test #'string=))

(defun push-nao-server (nao-ip server-port container-name)
  "Pushes a new nao-container to the list of running containers"
  (push (list nao-ip server-port container-name) *nao-servers*))

(defun pop-nao-server (nao-ip server-port container-name)
  "Pops a nao-container from the list of running containers"
  (setf *nao-servers*
        (remove (list nao-ip server-port container-name)
                *nao-servers* :test #'equalp)))

;;;;;;;;;;;;;;;;;;;;;
;; Nao Robot Class ;;
;;;;;;;;;;;;;;;;;;;;;

(export '(nao make-nao))

(defclass nao ()
  ((ip :initarg :ip :type string :accessor ip :initform ""
       :documentation "IP address of the nao")
   (port :initarg :port :type string :accessor port :initform "9559"
         :documentation "Port number of the nao")
   (username :initarg :username :type string :accessor username :initform "nao"
             :documentation "Username of the nao")
   (password :initarg :password :type string :accessor password :initform "nao"
             :documentation "Password of the nao")
   (server-host :initarg :server-host :type string :accessor server-host :initform "localhost"
                :documentation "Host of the nao server")
   (server-port :initarg :server-port :type string :accessor server-port :initform ""
                :documentation "Port to which the nao server should listen")
   (container-name :initarg :container-name :type string :accessor container-name :initform ""
                   :documentation "Name of the Docker container of this Nao"))
  (:documentation "Nao robot class"))

(defmethod initialize-instance :after ((nao nao) &key (connect-automatically t))
  (when connect-automatically
    (let ((container-name (format nil "nao-~a-~a" (ip nao) (server-port nao))))
      (setf (container-name nao) container-name)
      (start-nao-server nao))))

(defun make-nao (&key ip server-port
                      (port "9559")
                      (username "nao")
                      (password "nao")
                      (server-host "localhost")
                      (connect-automatically t))
  (make-instance 'nao
                 :ip ip
                 :port port
                 :username username
                 :password password
                 :server-host server-host
                 :server-port server-port
                 :connect-automatically connect-automatically))


;; Starting and stopping nao servers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export '(start-nao-server stop-nao-server stop-all-nao-servers))

(defgeneric start-nao-server (nao &key test-connection)
  (:documentation "Make connection to this nao"))
(defgeneric stop-nao-server (nao &key)
  (:documentation "Stop the connection to the nao"))

(defun docker-container-exists-p (container-name)
  (not (string= (first (exec-and-return "docker" "container" "inspect" container-name)) "[]")))

(defmethod start-nao-server ((nao nao) &key (test-connection t))
  ;; check/create folder for nao images
  (ensure-directories-exist (babel-pathname :directory '(".tmp" "nao-img")))
  ;; start the nao server
  (cond ((ip-occupied? (ip nao))
         (error (format nil "The IP address ~a is already in use" (ip nao))))
        ((port-occupied? (server-port nao))
         (error (format nil "The port number ~a is already in use" (server-port nao))))
        (t
         (if (docker-container-exists-p (container-name nao))
           ;; Container does exist, make it run again
           (run-prog "docker" :args `("start" ,(container-name nao)))
           ;; Container does not yet exist, create one
           (run-prog "docker" :args `("run" "-it" "-d"
                                      "-p" ,(format nil "~a:80" (server-port nao))
                                      "-v" ,(format nil "~a:/naoqi/src" (babel-pathname :directory '("sharing" "nao-interface" "flask-server")))
                                      "-v" ,(format nil "~a:/naoqi/src/img" (babel-pathname :directory '(".tmp" "nao-img")))
                                      "--name" ,(container-name nao)
                                      "naoqi-python")))
         ;; Push to the running containers
         (push-nao-server (ip nao) (server-port nao) (container-name nao))
         ;; Give some time to start the container
         (sleep 1)
         ;; Start the nao server inside the docker container
         (run-prog "docker" :args `("exec" "-d"
                                    ,(container-name nao)
                                    "/usr/bin/python" "/naoqi/src/nao_flask_server.py"
                                    "--robot-ip" ,(ip nao)
                                    "--robot-port" ,(port nao)))
         (when test-connection
           (sleep 1)
           (test-server-connection nao)))))

(defmethod stop-nao-server ((nao nao) &key)
  "Stops the python server associated to the given nao instance. Updates *nao-servers*"
  (run-prog "docker" :args `("stop" ,(container-name nao)))
  (pop-nao-server (ip nao) (server-port nao) (container-name nao))
  *nao-servers*)

(defun stop-all-nao-servers ()
  "Stops all known nao-servers."
  (when *nao-servers*
    (loop for entry in *nao-servers*
          do (run-prog "docker" :args `("stop" ,(third entry))))
    (setf *nao-servers* nil))
  *nao-servers*)

;; Sending and receiving data from the nao
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric nao-send-http (nao route &key data)
  (:documentation "Encode the data to json, send it to the nao,
    decode the response and return it. This found expects the data
    to be an a-list."))

(defmethod nao-send-http ((nao nao) route &key data)
  (assert (alistp data))
  (assert (stringp route))
  (let ((json-data (encode-json-to-string data))
        (uri (format nil "http://~a:~a~a"
                     (server-host nao) (server-port nao) route)))
    (with-open-stream
        #+LISPWORKS (stream (http-request uri :method :post :content json-data
                                          :want-stream t :connection-timeout nil
                                          :read-timeout nil :write-timeout nil))
        #+CCL (stream (http-request uri :method :post :content json-data
                                    :want-stream t :deadline (+ (get-universal-time) 1000000)))
      (decode-json-from-string (read-line stream)))))
           

;; Testing the connection with nao
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export '(test-server-connection))

(defgeneric test-server-connection (nao &key silent)
  (:documentation "Returns t if communication with nao-server succeeded, nil if it failed. Use silent = t for quick checks."))

(defmethod test-server-connection ((nao nao) &key (silent nil))
  (let ((response (nao-send-http nao "/test_connection"
                                 :data '((message . "test-server-connection")))))
    (unless silent
      (nao-speak nao (format nil "Connected to Babel 2 at port ~a" (server-port nao)))
      (warn "Did Nao speak? If not, check whether you are connected to the same WiFi network (CiscoNao) and that Docker is running!"))
    (string= (rest (assoc :message response)) "test-server-connection")))

;; Getting files from nao
;;;;;;;;;;;;;;;;;;;;;;;;;;

(export '(nao-scp-get))

(defgeneric nao-scp-get (nao remote-file local-file)
  (:documentation "Gets remote-file from the Nao and stores it in local-file"))

(defmethod nao-scp-get ((nao nao) remote-file local-file)
  (utils::scp-get (ip nao) (username nao) (password nao)
                  remote-file local-file))

;; Clear images from nao
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric clear-nao-recordings (nao &key dir)
  (:documentation "Remove all images from the Nao's hard drive"))

(defmethod clear-nao-recordings ((nao nao)
                                 &key (dir "/var/persistent/home/nao/recordings/cameras"))
  (utils::ssh-clear-dir (ip nao) (username nao) (password nao) dir))
