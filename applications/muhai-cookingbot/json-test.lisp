(ql:quickload :muhai-cookingbot)
(in-package :muhai-cookingbot)

(defparameter *ks*
  (babel-pathname
   :directory '("applications" "muhai-cookingbot")
   :name "almondCookiesInitialState" :type "json"))

(defparameter *data*
  (with-open-file (stream *ks* :direction :input)
    (jzon:parse stream :key-fn #'(lambda (key) (make-kw (camel-case->lisp key))))))

(defun hash-table->alist-rec (data)
  (if (hash-table-p data)
    (let ((alist (or (hash-table-alist data) 'empty-hash)))
      (if (eql alist 'empty-hash)
        'empty-hash
        (loop for (key . value) in alist
              if (hash-table-p value)
                collect (cons key (hash-table->alist-rec value))
              else collect (cons key value))))
    data))

(defun alist->hash-table-rec (data)
  (cond ((and data (alistp data))
         (alist-hash-table
          (loop for (key . value) in data
                collect (cons (lisp->camel-case (mkstr key) :from-first nil)
                              (alist->hash-table-rec value)))))
        ((eql data 'empty-hash)
         (make-hash-table))
        (t data)))

(defparameter *alist-data* (hash-table->alist-rec *data*))

(defparameter *symbolic-ks* (vr-to-symbolic-kitchen *alist-data*))
(defparameter *vr-ks* (symbolic-to-vr-kitchen *symbolic-ks*))

(defparameter *hash-data* (alist->hash-table-rec *vr-ks*))

(jzon:stringify *hash-data* :stream (babel-pathname :directory '(".tmp") :name "test" :type "json"))