(in-package :clingon)

(defclass option-integer/nil (option-integer)
  () (:documentation "Option that is either an integer or NIL"))

(defmethod make-option ((kind (eql :integer/nil)) &rest rest)
  (apply #'make-instance 'option-integer/nil rest))

(defun parse-integer-or-nil (value &key (radix 10))
  (when (integerp value)
    (return-from parse-integer-or-nil value))

  (when (or (string= value "nil")
            (string= value "NIL"))
    (return-from parse-integer-or-nil nil))

  (let ((int (parse-integer value :radix radix :junk-allowed t)))
    (unless int
      (error 'option-derive-error :reason (format nil "Cannot parse ~A as integer" value)))
    int))

(defmethod initialize-option ((option option-integer/nil) &key)
  "Initializes the integer option. In case the option was
  first initialized by other means, such as environment variables,
  we make sure that the provided value is a valid integer."
  (call-next-method)

  ;; Nothing to initialize further
  (unless (option-value option)
    (return-from initialize-option))

  (let ((value (option-value option)))
    (setf (option-value option)
          (etypecase value
            (integer value)
            (string (parse-integer-or-nil value :radix (option-integer-radix option)))))))

(defmethod derive-option-value ((option option-integer/nil) arg &key)
  (parse-integer-or-nil arg :radix (option-integer-radix option)))



;;;; Command Line Interface
;;;; ----------------------

(in-package :au-benchmark)

(defun cli-options ()
  (list
   (clingon:make-option
    :filepath
    :description "input file"
    :short-name #\i
    :long-name "input"
    :required t
    :key :input)
   (clingon:make-option
    :filepath
    :description "output dir"
    :short-name #\o
    :long-name "output-dir"
    :required t
    :key :output-dir)
   (clingon:make-option
    :filepath
    :description "temp dir"
    :short-name #\d
    :long-name "temp-dir"
    :required nil
    :key :temp-dir)
   (clingon:make-option
    :list
    :description "algorithms to run"
    :short-name #\a
    :long-name "algorithms"
    :required t
    :key :algorithms)
   (clingon:make-option
    :integer/nil
    :description "the value of k"
    :short-name #\k
    :required nil
    :initial-value nil
    :key :k)
   (clingon:make-option
    :integer/nil
    :description "the value of W"
    :short-name #\W
    :required nil
    :initial-value nil
    :key :W)
   (clingon:make-option
    :integer/nil
    :description "the value of V"
    :short-name #\V
    :required nil
    :initial-value nil
    :key :V)
   (clingon:make-option
    :integer/nil
    :description "the timeout value"
    :short-name #\t
    :long-name "timeout"
    :required nil
    :initial-value nil
    :key :timeout)
   (clingon:make-option
    :integer
    :description "number of processes"
    :short-name #\p
    :long-name "processes"
    :required nil
    :initial-value 50
    :key :processes)
   (clingon:make-option
    :enum
    :description "omega scope"
    :short-name #\c
    :long-name "omega-scope"
    :required nil
    :initial-value "global"
    :key :omega-scope
    :items '(("global" . :global)
             ("local" . :local)))
   (clingon:make-option
    :boolean/true
    :description "shuffle input"
    :short-name #\s
    :long-name "shuffle"
    :required nil
    :key :shuffle)
   ;; temporary flags to test out configurations
   (clingon:make-option
    :boolean
    :description "option 1: only consider local swaps that improve quality"
    :short-name #\x
    :required nil
    :key :config-1)
   (clingon:make-option
    :boolean
    :description "option 2: stop to search as soon as improving swap is found"
    :short-name #\y
    :required nil
    :key :config-2)
   (clingon:make-option
    :boolean
    :description "option 3: sort the queue"
    :short-name #\z
    :required nil
    :key :config-3)))

(defun cli-handler (cmd)
  ;; get command line args
  (let ((input-file (clingon:getopt cmd :input))
        (output-dir (clingon:getopt cmd :output-dir))
        (temp-dir (clingon:getopt cmd :temp-dir))
        (list-of-algorithms (mapcar #'make-kw (clingon:getopt cmd :algorithms)))
        (k (clingon:getopt cmd :k))
        (W (clingon:getopt cmd :W))
        (V (clingon:getopt cmd :V))
        (omega-scope (clingon:getopt cmd :omega-scope))
        (timeout (clingon:getopt cmd :timeout))
        (processes (clingon:getopt cmd :processes))
        (shuffle (clingon:getopt cmd :shuffle))
        (x (clingon:getopt cmd :config-1))
        (y (clingon:getopt cmd :config-2))
        (z (clingon:getopt cmd :config-3)))
    (if (= processes 0)
      ;; run sequentially
      (run-anti-unification-benchmark input-file output-dir list-of-algorithms
                                      :k k :W W :V V
                                      :omega-scope omega-scope
                                      :timeout timeout
                                      :shuffle shuffle
                                      :x x :y y :z z)
      ;; run in parallel
      (run-anti-unification-benchmark-in-parallel input-file output-dir list-of-algorithms processes
                                                  :k k :W W :V V
                                                  :omega-scope omega-scope
                                                  :temp-dir temp-dir
                                                  :timeout timeout
                                                  :shuffle shuffle
                                                  :x x :y y :z z))))

(defun cli-command ()
  (clingon:make-command
   :name "run-benchmark"
   :description "run the benchmark"
   :options (cli-options)
   :handler #'cli-handler))

(defun main (args)
  (format t "Received args: ~a~%" args)
  (clingon:run (cli-command) args))