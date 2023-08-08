;;;;
;;;; File: configuration.lisp
;;;;
;;;; Methods for configuring information processing
;;;; e.g. for an agent or experiment
;;;;

(in-package :utils)

;; ############################################################################

(export '(configuration
	  make-configuration
          make-config
	  entries
	  parent-configuration
	  get-configuration
          set-configuration
          set-configurations
          get-configuration-or-default
	  define-configuration-default-value
          require-configuration
          with-configurations))

(defclass configuration ()
  ((configuration :initarg :configuration
                  :accessor configuration
                  :initform nil :type hash-table
                  :documentation "A hash table containing the configuration entries")
   (parent-configuration :initarg :parent-configuration
                         :accessor parent-configuration
                         :initform nil :type (or null configuration))))

(defmethod initialize-instance :after ((c configuration)
                                       &key entries configuration &allow-other-keys)
  "make-instance of a configuration can be called with :entries and :configuration.
   :entries is always an alist, while :configuration can be an alist, hash-table or
   a configuration object"
  ;; you can't use both :entries and :configuration as initargs
  (assert (not (and configuration entries)))
  (when entries
    (setf (configuration c)
          (let ((hash (make-hash-table :test
                                       #'(lambda (entry-1 entry-2)
                                           (equalp (symbol-name entry-1)
                                                   (symbol-name entry-2))))))
            (loop for (key . value) in entries
                  do (setf (gethash key hash) value))
            hash)))
  (when configuration
    (setf (configuration c)
          (cond ((typep configuration 'configuration)
                 (configuration configuration))
                ((hash-table-p configuration)
                 configuration)
                ((listp configuration)
                 (let ((hash (make-hash-table :test
                                       #'(lambda (entry-1 entry-2)
                                           (equalp (symbol-name entry-1)
                                                   (symbol-name entry-2))))))
                   (loop for (key . value) in configuration
                         do (setf (gethash key hash) value))
                   hash))))))


;; ----------------------------------------------------------------------------  

(defun make-configuration (&key entries parent-configuration)
  "Creates a configuration based on a alist of key value pairs.
   Example: (make-configuration :entries `((key1 . 123) (key2 . 'symbol)))"
  (let ((hash-table (make-hash-table :test
                                     #'(lambda (entry-1 entry-2)
                                               (equalp (symbol-name entry-1)
                                                       (symbol-name entry-2))))))
    ;; The test of 'make-hash-table' compares keys using symbol-name equality
    ;; this enables to set a configuration with keywords and retrieve it with 
    ;; the symbol with the same symbol-name (e.g. :key1 and 'key1).
    ;; This test was added to ensure backwards compatibility, however
    ;; we do encourage to not use this feature anymore.
    (loop for (key . value) in entries
          do (setf (gethash key hash-table) value))
    (make-instance 'configuration
                   :configuration hash-table
                   :parent-configuration parent-configuration)))

(defmacro make-config (&rest key-value-lists)
  "Example: (make-config (key1 123) (key2 'symbol))
   The keys are quoted while the values are evaluated."
  `(make-configuration :entries
                       (list . ,(loop for (key value) in key-value-lists
                                      collect `(cons ',key ,value)))))

(defmethod entries ((configuration configuration))
  (loop for key being the hash-keys of (configuration configuration)
          using (hash-value value)
        collect (cons key value)))

;; ----------------------------------------------------------------------------

(defmethod print-object ((configuration configuration) stream)
  (if *print-pretty*
    (pprint-logical-block (stream nil)
      (format stream "<configuration:~:_ ~{~a~^,~:_ ~}" 
              (loop for key being the hash-keys of (configuration configuration)
                    using (hash-value value)
                    collect (format nil "~(~a~): ~a" key value)))
      (format stream ">"))
    (format stream "<configuration (~a entries)>" 
            (hash-table-count (configuration configuration)))))

;; ----------------------------------------------------------------------------

(defmethod copy-object ((configuration configuration))
  (let ((copy (make-configuration)))
    (copy-object-content configuration copy)
    copy))

(defmethod copy-object-content ((source configuration)
				(destination configuration))
  (setf (configuration destination)
	(copy-object (configuration source))))

;; ----------------------------------------------------------------------------
;; get-configuration

(defgeneric get-configuration (object key &key)
  (:documentation "Gets the configuration value for a key"))

(defmethod get-configuration ((list list) key &key)
  "Note: due to implementation decisions in the past,
         get-configuration is sometimes called with nil.
   This method catches this case and returns two values: (nil, nil)
   The second nil represents whether the entry was found.
   The first nil represents the found entry.
   It is up to the method caller to determine whether the
   returned nil is actually nil or whether nothing was found."
  (values nil nil))

(defmethod get-configuration ((hash-table hash-table) key &key)
  (gethash key hash-table))

(defmethod get-configuration ((configuration configuration) (key t)
                              &key omit-owner)
  (multiple-value-bind
      ;; get-configuration returns two values: (value, key-exists)
      ;; if the key existed, the value was returned
      ;; otherwise the returned value is nil
      (entry found) (get-configuration (configuration configuration) key)
    (cond
     ;; if the entry was not found, check the parent configuration
     ((and (not found) (parent-configuration configuration))
      (get-configuration (parent-configuration configuration) key))
     ;; if the entry was not found and the owner should not be omitted, check the owner
     ((and (not found) (not omit-owner) (owner configuration))
      (get-configuration (owner configuration) key))
     ;; otherwise, return the found entry (and whether it was found)
     (t
      (values entry found)))))

(defun get-configuration-or-default (configuration key default)
  "checks configuration (can be nil) and returns the found configuration or
   default"
  (multiple-value-bind (value key-exists)
      (get-configuration configuration key)
    (if key-exists
      value
      default)))

;; ----------------------------------------------------------------------------
;; set-configuration

(defgeneric set-configuration (object key value &key replace)
  (:documentation "Sets the configuration value for the given key, replacing
     an already present value if :replace is true."))

(defmethod set-configuration ((configuration configuration) key value
                              &key (replace t)
                              &allow-other-keys)
  (let* ((hash-table (configuration configuration))
         (previous-entry (gethash key hash-table)))
    (if previous-entry
      ;; if the entry already exists, only replace it if :replace is true
      (when replace (setf (gethash key hash-table) value))
      ;; otherwise, add the new entry
      (setf (gethash key hash-table) value))
    configuration))

(defun set-configurations (configuration configurations &key (replace t))
  (loop for (key . value) in configurations
        do (set-configuration configuration key value :replace replace)))

;; ----------------------------------------------------------------------------
;; 

(defmacro define-configuration-default-value (key value)
  "Defines a default value for a configuration key. 
   This value is returned when the key is not found in the configuration."
  `(defmethod get-configuration :around ((configuration configuration) (key (eql ,key)) &key)
       (multiple-value-bind (returned-value key-exists)
           (call-next-method)
         (if key-exists
             (values returned-value key-exists)
             (values ,value :default)))))

;; ----------------------------------------------------------------------------
;; 

(defmacro require-configuration (key)
  "Throws an error when a configuration was not set"
  `(defmethod get-configuration :around ((object t) (key (eql ,key)) &key)
     (multiple-value-bind (value key-exists)
	 (call-next-method)
       (unless key-exists 
	 (error 
	  (format nil "Please set the ~(~:w~) configuration in ~a" ,key object)))
       (values value key-exists))))

;; ----------------------------------------------------------------------------
;;

(defmacro with-configurations (local-names-and-keys configuration &body body)
  `(symbol-macrolet (,@(loop for (name key) in local-names-and-keys
                             collect `(,name (get-configuration ,configuration ,key))))
     ,@body))

;; ############################################################################