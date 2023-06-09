(ql:quickload :propbank-grammar)
(in-package :propbank-grammar)

;;;;;;;;;;;;;;;;;;;;;;;;;;


;;(defparameter *corpus-exp* (cl-store:restore "/Users/ehai-guest/Downloads/exp-dev-corpus-4000-sentences.store"))

;;(save-corpus-to-csv *corpus-exp* "corpus.csv")

(defun save-corpus-to-csv (corpus file-name)
  "Saves a corpus of sentences and their PropBank annotations to a CSV file.
    Each row of the CSV file contains the source file name, sentence ID, sentence string, frame name, lemma name, and frame roles.
    Arguments:
    - CORPUS: A list of sentence structures with PropBank annotations.
    - FILE-NAME: A string indicating the name of the output file."
  (let ((source-file-map (preprocess-source-files corpus)))
    (with-open-file (stream file-name
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (write-line "source_file,sentence_id,sentence_string,frame_name,lemma_name,frame_roles" stream)
      (dolist (sentence corpus)
        (let ((frame-strings (mapcar #'format-frame (propbank-frames sentence))))
          (dolist (frame-string frame-strings)
            (format stream "~A,~A,~A\,~A~%"
                    (source-file sentence)
                    (sentence-id sentence)
                    (replace-commas (sentence-string sentence))
                    frame-string)))))))


(defun preprocess-source-files (corpus)
  "Preprocesses source files of a corpus to assign a unique ID to each file.
    Returns a hash table mapping source file names to their corresponding IDs.
    Argument:
    - CORPUS: A list of sentence structures with PropBank annotations."
  (let ((source-file-map (make-hash-table :test 'equal))
        (index 0))
    (dolist (sentence corpus)
      (let ((source-file (source-file sentence)))
        (unless (gethash source-file source-file-map)
          (setf (gethash source-file source-file-map) index)
          (setf index (1+ index)))))
    source-file-map))


(defun format-frame (frame)
  "Formats a PropBank frame for output in a CSV file.
    Each frame is represented as a string containing the frame name, lemma name, and a list of frame roles.
    Argument:
    - FRAME: A PropBank frame structure."
  (let ((frame-name (frame-name frame))
        (frame-file (propbank-frame-file frame))
        (frame-roles (mapcar #'format-role (frame-roles frame))))
    (format nil "~A,~A,~{~A~^; ~}" frame-name frame-file frame-roles)))

(defun format-role (role)
  "Formats a PropBank role for output in a CSV file.
    Each role is represented as a string containing the role type, role indices, and the role string.
    Argument:
    - ROLE: A PropBank role structure."
  (let ((role-type (role-type role))
        (role-indices (indices role))
        (role-string (role-string role)))
    (format nil "~A [~{~A~^ | ~}]: ~A" role-type role-indices (replace-commas role-string))))

(defun replace-commas (string)
  "Replaces commas in a string with another character.
    Argument:
    - STRING: The string to process."
  (with-output-to-string (out)
    (loop for char across string
          do (if (char= char #\,)
                 (write-char #\~ out)
                 (write-char char out)))))


;;(defun replace-commas (string)
;;  (with-output-to-string (out)
;;    (loop for char across string
;;          unless (or (char= char #\,) (char= char #\.) (char= char #\!) (char= char #\?))
;;          do (write-char char out))))

(defun replace-all (string old-char new-string)
  (with-output-to-string (out)
    (loop for char across string
          do (if (char= char old-char)
                 (write-string new-string out)
                 (write-char char out)))))

