(in-package :mwm)

;;;; Export learner lexicon to pdf
(defun experiment-name-from-configurations (experiment)
  (downcase
   (string-append
    (get-configuration experiment :experiment-type) "-"
    (get-configuration experiment :world-type) "-"
    (get-configuration experiment :determine-interacting-agents-mode) "-"
    (if (eql (get-configuration experiment :experiment-type) :cogent)
      (string-append "train-" (mkstr (get-configuration experiment :switch-conditions-after-n-interactions)) "-")
      "")
    (if (eql (get-configuration experiment :experiment-type) :incremental)
      (string-append "phase-" (mkstr (get-configuration experiment :incremental-stage)) "-")
      "")
    "lexicon")))

(defun lexicon->pdf (agent &key dirname serie)
  (let* ((experiment-name
          (if dirname dirname (experiment-name-from-configurations (experiment agent))))
         (base-path
          (if serie
            (babel-pathname
             :directory `("experiments" "multidimensional-word-meanings"
                          "graphs" ,(downcase experiment-name)
                          ,(format nil "serie-~a" serie)))
            (babel-pathname
             :directory `("experiments" "multidimensional-word-meanings"
                          "graphs" ,(downcase experiment-name))))))
    (ensure-directories-exist base-path)
    (loop for concept in (average-over-concept-history agent)
          do (s-dot->image
              (concept->s-dot concept)
              :path (merge-pathnames
                     (make-pathname :name (format nil "~a-cxn" (form concept))
                                    :type "pdf")
                     base-path)
              :format "pdf" :open nil))))

(defun lexicon->store (agent &key dirname serie)
  (let* ((experiment-name
          (if dirname dirname (experiment-name-from-configurations (experiment agent))))
         (base-path
          (if serie
            (babel-pathname
             :directory `("experiments" "multidimensional-word-meanings"
                          "store" ,(downcase experiment-name)
                          ,(format nil "serie-~a" serie)))
            (babel-pathname
             :directory `("experiments" "multidimensional-word-meanings"
                          "store" ,(downcase experiment-name))))))
    (ensure-directories-exist base-path)
    (loop for concept in (average-over-concept-history agent)
          for pathname = (merge-pathnames
                          (make-pathname :name (format nil "~a-cxn" (form concept))
                                         :type "store")
                          base-path)
          do (cl-store:store concept pathname))))






#|
;; lexicon -> function plots
(defun lexicon->function-plots (agent)
  (loop for cxn in (constructions (grammar agent)) 
        for experiment-name = (downcase (mkstr (get-configuration agent :category-representation)))
        do (cxn->function-plot cxn (get-configuration agent :category-representation)
                               :directory `("experiments" "multidimensional-word-meanings"
                                            "graphs" ,experiment-name "function-plots"))))

;;;; Export concepts (= cxns) as functions
(defgeneric cxn->function-plot (cxn category-representation &key directory)
  (:documentation "Plot the functions that are stored in the categories"))

(defmethod cxn->function-plot ((cxn fcg-construction) (category-representation (eql :prototype))
                                &key (directory '(".tmp")))
  (let ((equations
         (loop for (category . certainty) in (attr-val cxn :meaning)
               collect (format nil "normal(x,~a,~a)"
                               (prototype category)
                               (sqrt (/ (M2 category) (nr-samples category)))))))
    (create-function-plot equations
                          :function-definitions '("normal(x,mu,sd) = (1/(sd*sqrt(2*pi)))*exp(-(x-mu)**2/(2*sd**2))")
                          :title (format nil "~a" (downcase (mkstr (name cxn))))
                          :captions (loop for (category . certainty) in (attr-val cxn :meaning)
                                          collect (format nil "~a" (downcase (mkstr (attribute category)))))
                          :plot-file-name (format nil "~a" (downcase (mkstr (name cxn))))
                          :plot-directory directory
                          :x-label nil :y-min 0 :y-max nil
                          :open nil)))
|#