(in-package :cle)

;; -------------------
;; + Generate Graphs +
;; -------------------
(defun get-statistics (top-dir exp-dir raw-parameters filters)
  "Helper function to find experiment names with particular filters." 
  (let* ((exp-dir-path (asdf:system-relative-pathname "cle" (format nil "logging/~a/~a/experiments/" top-dir exp-dir)))
         (all-configs (sort (get-configurations exp-dir-path) (lambda (x y) (< (first x) (first y)))))
         (configs (filter-experiments all-configs filters)) ; (loop for (index . config) in (filter-experiments all-configs filters) collect config))
         (exp-names (loop for (index . config) in configs collect (downcase (assqv :EXPERIMENT-NAME config))))
         (captions (generate-captions configs raw-parameters filters))
         (title (generate-title exp-dir filters)))
    (list (length exp-names) title exp-names captions)))

(defun graph-batch-experiments (top-dir exp-dir raw-parameters filters &key (start nil) (end nil) (plot :all) (y-max 100) (y-min 0))
  "Plot a batch of experiments."
  (let* ((exp-dir-path (asdf:system-relative-pathname "cle" (format nil "logging/~a/~a/experiments/" top-dir exp-dir)))
         (all-configs (sort (get-configurations exp-dir-path) (lambda (x y) (< (first x) (first y)))))
         (configs (filter-experiments all-configs filters))
         (exp-names (loop for (index . config) in configs collect (downcase (assqv :EXPERIMENT-NAME config))))
         (captions (generate-captions configs raw-parameters filters))
         (title (generate-title exp-dir filters)))
    (when (or (eq plot :all) (eq plot :communicative-success))
      (create-graph-comparing-strategies
       :base-dir (format nil "~a/~a" top-dir exp-dir)
       :title title
       :experiment-names exp-names
       :measure-name "communicative-success"
       :y1-label "Communicative success"
       :y-min y-min
       :y-max 1
       :start start
       :end end
       :average-windows 1000
       :plot-file-name (format nil "~{~a~^-~}" (list title "comm-success"))
       :captions captions
       ))
    (when (or (eq plot :all) (eq plot :lexicon-coherence))
      (create-graph-comparing-strategies
       :base-dir (format nil "~a/~a" top-dir exp-dir)
       :title title
       :experiment-names exp-names
       :measure-name "lexicon-coherence"
       :y1-label "Lexicon coherence"
       :y-min y-min
       :y-max 1
       :start start
       :end end
       :average-windows 1000
       :plot-file-name (format nil "~{~a~^-~}" (list title "lex-coherence"))
       :captions captions
       ))
    ))

(defun get-configurations (base-exp-dir)
  "Returns a list of experiments by walking the directory."
  (loop for experiment-dir in (uiop:subdirectories base-exp-dir)
        for fpath = (merge-pathnames (make-pathname :name "experiment-configurations" :type "lisp")
                                     experiment-dir)
        for config = (with-open-file (stream fpath :direction :input) (read stream))
        for exp-index = (parse-integer (first (last (split-sequence:split-sequence #\- (assqv :EXPERIMENT-NAME config)))))
        collect (cons exp-index config)))

(defun member-nested (el l)
  "Whether el is a member of l, el can be atom or cons, l can be list of atoms or not"
  (cond
   ((null l) nil)
   ((equal el (car l)) t)
   ((consp (car l)) (or (member-nested el (car l))
                        (member-nested el (cdr l))))
   (t (member-nested el (cdr l)))))

(defun config-has-params-p (config filters)
  "Checks if a config has (exactly) a set of given parameters (the filters)."
  (loop for (name . vals) in filters
        always (member-nested (assqv name config) vals)))

(defun filter-experiments (configurations &optional filter-params)
  "Generate a set of experiments. Specify which parameters are variable
   and what their possible values can be. Optionally specify an a-list
   of shared configurations."
  (if filter-params
    (loop for (index . config) in configurations
          if (config-has-params-p config filter-params)
            collect (cons index config))
    configurations))
                                                                                                
(defun generate-title (base-dir filters)
  (format nil "~a = ~{~a~^ ~}" base-dir (loop for (param-name . param-values) in filters
                                              for name-string = (mkstr param-name)
                                              for str-length = (length name-string)
                                              for value-string = (format nil "[~{~a~^ ~}]"
                                                                         (loop for param-value in param-values
                                                                               for raw = (cond ((typep param-value 'ratio) (float param-value))
                                                                                               ((typep param-value 'keyword) (subseq (mkstr param-value) 0 (if (>= (length (mkstr param-value)) 9)
                                                                                                                                                             9
                                                                                                                                                             (length (mkstr param-value)))))
                                                                                               (t param-value))
                                                                               collect (mkstr raw)))
                                              if (length= param-values 1)
                                                collect (format nil "~a-~a"
                                                                (subseq name-string 0 (if (>= str-length 3)
                                                                                        3
                                                                                        str-length))
                                                                value-string))))

(defun generate-captions (configurations raw-parameters filters)
  "Generate a set of experiments. Specify which parameters are variable
   and what their possible values can be. Optionally specify an a-list
   of shared configurations."
  (loop with variable-params = (loop for (name . val) in raw-parameters collect name)
        for (index . config) in configurations
        
        for caption = (format nil "E~a ~{~a~^ ~}"
                              (mkstr index)
                              (loop for (param-name . param-value) in config
                                    for name-string = (mkstr param-name)
                                    for name-string-length = (length name-string)
                                    for value-string = (mkstr param-value)
                                    for value-string-length = (length value-string)
                                    if (or
                                        (and (member param-name variable-params) (> (length (assqv param-name filters)) 1))
                                        (and (not (assq param-name filters)) (member param-name variable-params)))
                                      collect (format nil "~a:~a"
                                                      (subseq name-string 0 (if (>= name-string-length 3)
                                                                              3
                                                                              name-string-length))
                                                      (subseq value-string 0 (if (>= value-string-length 9)
                                                                               9
                                                                               value-string-length)))))
        collect caption))

#|(defun generate-configurations (parameters)
  "Generate a set of experiments. Specify which parameters are variable
   and what their possible values can be. Optionally specify an a-list
   of shared configurations."
  (let* ((pairs (loop for (key . values) in parameters
                      collect (loop for value in values
                                    collect (cons key value))))
         (configurations (apply #'combinations pairs)))
    configurations))|#
