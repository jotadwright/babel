(in-package :nao-interface)

(export '(take-picture observe-world))

(defmethod take-picture ((nao nao) &key (open t))
  (let* ((response (nao-send-http nao :endpoint "/vision/capture"))
         (pathname (parse-namestring (rest (assoc :pathname response))))
         (filename (pathname-name pathname))
         (type (pathname-type pathname))
         (local-pathname (babel-pathname :directory '(".tmp" "nao-img")
                                         :name filename :type type)))
      (nao-scp-get nao pathname local-pathname)
      (when open
        (let ((arg (format nil "open ~a" local-pathname)))
          (run-prog "/bin/sh" :args (list "-c" arg))))
      local-pathname))

(defmethod observe-world ((nao nao) &key (open t))
  (let* ((img-filename (take-picture nao :open nil))
         (analysis (nao-send-http nao :endpoint "/vision/analyse"
                                  :data `((filename . ,(namestring img-filename)))))
         (analysis-img (rest (assoc :filename analysis)))
         (analysis-data (rest (assoc :data analysis))))
    (when open
      (let* ((local-pathname (babel-pathname :directory '(".tmp" "nao-img")
                                             :name analysis-img
                                             :type "jpg"))
             (arg (format nil "open ~a" local-pathname)))
        (run-prog "/bin/sh" :args (list "-c" arg))))
    (values analysis-data analysis-img)))

