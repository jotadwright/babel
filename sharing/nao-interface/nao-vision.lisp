(in-package :nao-interface)

(export '(take-picture observe-world))

(defmethod take-picture ((nao nao) &key (open t))
  (let ((picture-details (nao-send-http nao :endpoint "/vision/capture")))
    (let* ((path (rest (assoc :directory picture-details)))
           (file (rest (assoc :name picture-details)))
           (ext (rest (assoc :type picture-details)))
           (remote-pathname (pathname (format nil "~a~a.~a" path file ext)))
           (local-pathname (pathname (babel-pathname :directory '(".tmp" "nao-img")
                                                     :name file :type ext))))
      (nao-scp-get nao remote-pathname local-pathname)
      (when open
        (let ((arg (format nil "open ~a" local-pathname)))
          (run-prog "/bin/sh" :args (list "-c" arg))))
      (format nil "~a.~a" file ext))))

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

