(in-package :muhai-cookingbot)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;                                 utils                                                    ;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *kitchen-host* "http://127.0.0.1:54321")

(defun send-request (route json &key (host *kitchen-host*) (connection-timeout 3600))
  "Send curl request and returns the answer."
  (let* ((url (concatenate 'string *kitchen-host* route))
         (response (dex:post (concatenate 'string *kitchen-host* route)
                             :headers '(("content-type" . "application/json"))
                             :content json
                             :read-timeout 2000)))
    (cl-json::decode-json-from-string response)))


(defun symbol-keyword (x)
  (intern (string-upcase x) :keyword))

(defun cdrassoc (key alist)
  (cdr (assoc key alist)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;                                 commands                                                 ;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; to-get-kitchen ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun request-get-kitchen (kitchen-state-variable)
  (let ((response (send-request "/abe-sim-command/to-get-kitchen"
                                (cl-json:encode-json-to-string `((:kitchen-state-in . ,kitchen-state-variable))))))
    (when response (handler-case  (assoc (symbol-keyword kitchen-state-variable) (cdr (assoc :response response)))
                     (error (e) (format t
                                        "Error in response from abe-sim api service (route: to-get-kitchen): ~S.~&"
                                        e))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; to-set-kitchen  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun request-set-kitchen (kitchen-state-in)
  (cdr (assoc  :response
               (send-request "/abe-sim-command/to-set-kitchen"
                             (progn ;(break)
                               (cl-json:encode-json-to-string `((:kitchen-state-in . ,kitchen-state-in))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; to-get-time ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun request-to-get-time ()
  (let ((time (cdr (assoc :response
                          (send-request "/abe-sim-command/to-get-time"
                                        (cl-json:encode-json-to-string `()))))))
    time))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; to-wait ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun request-to-wait (frames-to-wait)
  (cdr (assoc :response
              (send-request "/abe-sim-command/to-wait"
                            (cl-json:encode-json-to-string `((:frames . ,frames-to-wait)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; to-cut ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun request-to-cut (object cutting-tool cutting-pattern &optional (kitchen-state-in nil))
  (cdr (assoc :response
              (send-request "/abe-sim-command/to-cut"
                            (cl-json:encode-json-to-string `((:object           . ,object)
                                                             (:cutting-tool     . ,cutting-cool)
                                                             (:cutting-pattern  . ,cutting-pattern)
                                                             (:kitchen-state-in . ,kitchen-state-in)
                                                             (:set-world-state  . ,(not (not kitchen-state-in)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; to-refrigerate ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun request-to-refrigerate (container-with-ingredients refrigerator cooling-quantity cooling-unit &optional (kitchen-state-in nil))
  (cdr (assoc :response
              (send-request "/abe-sim-command/to-refrigerate"
                            (cl-json:encode-json-to-string `((:container-with-ingredients . ,container-with-ingredients)
                                                             (:refrigerator               . ,refrigerator)
                                                             (:cooling-quantity           . ,cooling-quantity)
                                                             (:cooling-unit               . ,cooling-unit)
                                                             (:kitchen-state-in           . ,kitchen-state-in)
                                                             (:set-world-state            . ,(not (not kitchen-state-in)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; to-sprinkle ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun request-to-sprinkle (object topping-container &optional (kitchen-state-in nil))
  (cdr (assoc :response
              (send-request "/abe-sim-command/to-sprinkle"
                            (cl-json:encode-json-to-string `((:object            . ,object)
                                                             (:topping-container . ,topping-container)
                                                             (:kitchen-state-in  . ,kitchen-state-in)
                                                             (:set-world-state   . ,(not (not kitchen-state-in)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; to-bake ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun request-to-bake (thing-to-bake oven input-destination-container &optional (kitchen-state-in nil))
  (cdr (assoc :response
              (send-request "/abe-sim-command/to-bake"
                            (cl-json:encode-json-to-string `((:thing-to-bake               . ,thing-to-bake)
                                                             (:oven                        . ,oven)
                                                             (:input-destination-container . ,input-destination-container)
                                                             (:kitchen-state-in            . ,kitchen-state-in)
                                                             (:set-world-state             . ,(not (not kitchen-state-in)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; to-shape ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun request-to-shape (container-with-dough destination &optional (kitchen-state-in nil))
  (cdr (assoc :response
              (send-request "/abe-sim-command/to-shape"
                            (cl-json:encode-json-to-string `((:container-with-dough . ,container-with-dough)
                                                             (:destination          . ,destination)
                                                             (:kitchen-state-in     . ,kitchen-state-in)
                                                             (:set-world-state      . ,(not (not kitchen-state-in)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; to-line ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun request-to-line (baking-tray baking-paper &optional (kitchen-state-in nil))
  (cdr (assoc :response
              (send-request "/abe-sim-command/to-line"
                            (cl-json:encode-json-to-string `((:baking-tray      . ,baking-tray)
                                                             (:baking-paper     . ,baking-paper)
                                                             (:kitchen-state-in . ,kitchen-state-in)
                                                             (:set-world-state  . ,(not (not kitchen-state-in)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; to-mix  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun request-to-mix (container-with-input-ingredients mixing-tool &optional (kitchen-state-in nil))
  (cdr (assoc :response
              (send-request "/abe-sim-command/to-mix"
                            (cl-json:encode-json-to-string `((:container-with-input-ingredients . ,container-with-input-ingredients)
                                                             (:mixing-tool                      . ,mixing-tool)
                                                             (:kitchen-state-in                 . ,kitchen-state-in)
                                                             (:set-world-state                  . ,(not (not kitchen-state-in)))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; to-beat  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun request-to-beat (container-with-input-ingredients beating-tool &optional (kitchen-state-in nil))
  (cdr (assoc :response
              (send-request "/abe-sim-command/to-beat"
                            (cl-json:encode-json-to-string `((:container-with-input-ingredients . ,container-with-input-ingredients)
                                                             (:beating-tool                     . ,beating-tool)
                                                             (:kitchen-state-in                 . ,kitchen-state-in)
                                                             (:set-world-state                  . ,(not (not kitchen-state-in)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; to-transfer ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun request-to-transfer (container-with-input-ingredients target-container &optional (kitchen-state-in nil)) ;
  (cdr (assoc :response
              (send-request "/abe-sim-command/to-transfer"
                            (cl-json:encode-json-to-string `((:container-with-input-ingredients . ,container-with-input-ingredients)
                                                             (:target-container                 . ,target-container)
                                                             (:kitchen-state-in                 . ,kitchen-state-in)
                                                             (:set-world-state                  . ,(not (not kitchen-state-in)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; to-portion ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun request-to-portion (container-with-ingredient target-container quantity &optional (kitchen-state-in nil))
  (cdr (assoc :response (send-request
                         "/abe-sim-command/to-portion"
                         (cl-json:encode-json-to-string `((:container-with-ingredient . ,container-with-ingredient)
                                                          (:target-container          . ,target-container)
                                                          (:quantity                  . ,quantity)
                                                          (:kitchen-state-in          . ,kitchen-state-in)
                                                          (:set-world-state           . ,(not (not kitchen-state-in)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; to-fetch ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun request-to-fetch (object &optional (kitchen-state-in nil))
  (cdr (assoc :response
              (send-request "/abe-sim-command/to-fetch"
                            (cl-json:encode-json-to-string `((:object           . ,object)
                                                             (:kitchen-state-in . ,kitchen-state-in)
                                                             (:set-world-state  . ,(not (not kitchen-state-in)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; to-get-location ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun request-get-location (available-location-variable type &optional (kitchen-state-in nil))
  (cdr (assoc :response
              (send-request "/abe-sim-command/to-get-location"
                            (cl-json:encode-json-to-string `((:available-location . ,available-location-variable)
                                                             (:type               . ,type)
                                                             (:kitchen-state-in   . ,kitchen-state-in)
                                                             (:set-world-state    . ,(not (not kitchen-state-in)))))))))
