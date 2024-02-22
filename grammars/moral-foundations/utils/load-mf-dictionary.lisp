
(in-package :moral-foundations)

;;; -------------------------------------------------------------------------------------
;;; Helper Functions
;;; -------------------------------------------------------------------------------------
(defun mf-dictionary-pathname (&optional (language 'english))
  "Returns the pathname of a moral-foundation dictionary."
  (let ((filename (second (assoc language *mf-dictionaries*))))
    (if filename
      (merge-pathnames *ressources-pathname* filename)
      (warn (format nil "No dictionary ressource found for language ~a" language)))))
;; (mf-dictionary-pathname 'english)

(defun write-dictionary-entry (token label moral-foundation)
  (setf (gethash token *mf-vocabulary-list*)
        (list label moral-foundation)))

(defun write-dictionary-entries (tokens label moral-foundation)
  (dolist (token tokens)
    (write-dictionary-entry token label moral-foundation)))

(defun token-in-dictionary-p (token)
  (let ((entry (gethash token *mf-vocabulary-list*)))
    (if entry
      (values (first entry) (second entry))
      (values nil nil))))
;; (token-in-dictionary-p "threaten")
;; (token-in-dictionary-p "bla")

;;; -------------------------------------------------------------------------------------
;;; LOAD-MF-DICTIONARY
;;; -------------------------------------------------------------------------------------

(defgeneric load-mf-dictionary (language))

(defmethod load-mf-dictionary ((language t))
  (warn (format nil "No dictionary loaded for language ~a" language)))
;; (load-mf-dictionary 'english)
;; (load-mf-dictionary 'french)

(defmethod load-mf-dictionary ((language (eql 'english)))
  (let ((source-file (mf-dictionary-pathname language)))
    (if (not source-file)
      (call-next-method)
      (let (virtue vice
            virtues vices vice-p)

        ;; Read the dictionary line per line
        (with-open-file (in source-file :direction :input)
          ;; Split the line using white spaces and remove empty sequences:
          (loop for line = (read-line in nil)
                while line
                do (let ((str (first
                               (split-sequence::split-sequence
                                #\Space
                                (first (split-sequence::split-sequence 
                                        #\Tab line
                                        :remove-empty-subseqs t))))))
                     (cond (;; If we come at the beginning of a virtue series, 
                            (cl-ppcre:scan "\\.VIRTUE" str)
                            ;; We collect the vices of the previous series
                            (if vice (setf vices (cons (reverse vice) vices)))
                            ;; And we start collecting the virtues in virtue
                            (setf vice-p nil
                                  vice nil
                                  virtue (cons (first (split-sequence::split-sequence #\. str)) virtue)))
                           ;; If we come at the beginning of a vice series,
                           ((cl-ppcre:scan "\\.VICE" str)
                            ;; We collect the virtues of the previous series
                            (if virtue (setf virtues (cons (reverse virtue) virtues)))
                            ;; And we start collecting the vices in vice
                            (setf vice-p t
                                  virtue nil
                                  vice (cons (first (split-sequence::split-sequence #\. str)) vice)))
                           ;; If we are collecting vices, just add them
                           (vice-p
                            (push (downcase str) vice))
                           (t ;; Else if we are collecting virtues, just add them
                              (push (downcase str) virtue))))))
      ;; Not forgetting the last vices we were collecting:
      (setf vices (cons (reverse vice) vices))
      ;; Now we are ready to handle the virtues and vices:
      (loop for virtue-spec in virtues
            for vice-spec in vices
            do (let* ((name (read-from-string (first virtue-spec)))
                      (moral-foundation (find name *moral-foundations* :key #'name))
                      (antonym (antonym moral-foundation)))
                 (assert (string= (first virtue-spec) (first vice-spec)))
                 (setf (virtues moral-foundation) (rest virtue-spec)
                       (vices moral-foundation) (rest vice-spec))
                 (write-dictionary-entries (rest virtue-spec) name moral-foundation)
                 (write-dictionary-entries (rest vice-spec) antonym moral-foundation)))
      (values *moral-foundations* *mf-vocabulary-list*)))))
;; (load-mf-dictionary 'english)
;; (gethash "threatens" *mf-vocabulary-list*)
;; (token-in-dictionary-p "threatens")
