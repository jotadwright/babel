(in-package :slp)

;;---------------------------;;
;; Functions for making HTML ;;
;;---------------------------;;

(defparameter *avatar-counter* -1)

(defun make-empty-cell ()
  "makes and returns the html for an empty cell in the table"
  `((td :class "empty" :style "width: 20px;")))

(defun clean-file-name (filename)
  (let ((output filename))
    (setf output (remove #\\ output))
    (setf output (remove #\/ output))
    (setf output (remove #\: output))
    (setf output (remove #\* output))
    (setf output (remove #\? output))
    (setf output (remove #\" output))
    (setf output (remove #\< output))
    (setf output (remove #\> output))
    (setf output (remove #\| output))
    output))

(defun make-id-gloss-cell (span-id fcg-tag colspan hamnosys)
  "makes and returns the html for an id-gloss-cell with the given span-id,
   fcg-tag and colspan. The cell contains a play button, sending the sigml
   of the provided hamnosys to the avatar"
  (let* ((clean-fcg-tag (clean-file-name fcg-tag))
         (sigml (hamnosys->sigml hamnosys))
         (filename (babel-pathname :directory '(".tmp" "sigml-files")
                                   :name (format nil "~a" clean-fcg-tag)
                                   :type "sigml"))
         (url (format nil "http://localhost:8000/~a.sigml" clean-fcg-tag))
         (uri (format nil "/~a.sigml" clean-fcg-tag)))
    (ensure-directories-exist filename)
    (with-open-file
        (out-stream
         filename
         :direction :output
         :if-exists :supersede
         :if-does-not-exist :create
         :external-format :utf-8
         :element-type 'cl:character)
      (xmls:write-xml
       sigml
       out-stream
       :indent t))
    (setf web-interface::*dispatch-table*
          (append web-interface::*dispatch-table*
                  (list (web-interface::create-static-file-dispatcher-and-handler   
                         uri filename))))
  `((td
     :class "id-gloss-cell"
     :colspan
     ,(write-to-string colspan))
    ((span)
     ((span
       :id ,(string span-id)
       :onclick
       ,(format
         nil
         "highlightSymbol('~a','~a','~a');"
         fcg-tag
         span-id
         (web-interface::get-color-for-symbol fcg-tag)))
      ((p
        :class "articulation-tag"
        :name ,fcg-tag)
       ,fcg-tag)))
    ((button :type "button"
             :class "playsigml"
             :onclick
             ,(format
               nil
               "CWASA.playSiGMLURL(`~a`, ~a)" url *avatar-counter*)) "&#9658;" ))))

(defun make-hamnosys-cell (value colspan)
   "makes and returns the html for a hamnosys-cell
    with the given value and colspan"
  `((td
     :class "hamnosys-cell"
     :colspan
     ,(write-to-string colspan))
    ((span)
     ((ham)
      ,value))))

(defun make-right-hand-rows (right-hand-cells number-of-columns)
  "makes and returns html for both the id-gloss and hamnosys-rowsof the right hand"
  (loop with right-hand-id-gloss-row
          = `((TR :CLASS "header" :style "width:100%;")
              ((TD :CLASS "header")
               "RH"))
        with right-hand-hamnosys-row
          = `((TR :CLASS "header" :style "width:100%;")
              ((TD :CLASS "header")))
        for x from 0 to number-of-columns
        for right-hand-cell
          = (find-rh-cell-by-column-nr
             right-hand-cells
             x)
        for fcg-tag
          = (downcase
             (string
              (fcg-tag
               right-hand-cell)))
        for value
          = (string
             (hamnosys
              right-hand-cell))
        for span-id
          = (make-const "S")
        do
          (if (string=
               (utils::remove-numeric-tail
                fcg-tag)
               "dummy")
            (progn
              (pushend
               (make-empty-cell)
               right-hand-id-gloss-row)
              (pushend
               (make-empty-cell)
               right-hand-hamnosys-row))
            (progn
              (pushend
               (make-id-gloss-cell
                span-id
                fcg-tag
                1
                value)
               right-hand-id-gloss-row)
              (pushend
               (make-hamnosys-cell
                value
                1)
               right-hand-hamnosys-row)))
        finally
          (return
           (list
            right-hand-id-gloss-row
            right-hand-hamnosys-row))))

(defun make-left-hand-rows (left-hand-cells number-of-columns)
  "makes and returns html for both the id-gloss and hamnosys-rows of the left hand"
  (loop with left-hand-id-gloss-row
          = `((tr :class "header" :style "width:100%;")
              ((td :class "header")
               "LH"))
        with left-hand-hamnosys-row
          = `((tr :class "header" :style "width:100%;")
              ((td :class "header")))
        with colwait = nil
        for x from 0 to number-of-columns
        for left-hand-cell
          = (find-lh-cell-by-start-column-nr
             left-hand-cells
             x)
        for fcg-tag
          = (when left-hand-cell
              (downcase
               (string
                (fcg-tag
                 left-hand-cell))))
        for value
          = (when left-hand-cell
              (string
               (hamnosys
                left-hand-cell)))
        for sigml-source
          = (when left-hand-cell
              (if (or (eql (char value 0)
                         #\)
                        (eql (char value 0)
                         #\))
              value
              (concatenate 'string "" value)))
        for span-id = (make-const "S")
        for start = x
        for end
          = (when left-hand-cell
              (end-column-nr left-hand-cell))
        for col-dif
          = (when end
              (- end start))
        for colspan
          = (when col-dif
              (if (eql col-dif 0)
                nil
                (+ col-dif 1)))
        do 
          (unless colwait
            (if left-hand-cell
              (progn
                (pushend
                 (make-id-gloss-cell
                  span-id
                  fcg-tag
                  colspan
                  sigml-source)
                 left-hand-id-gloss-row)
                (pushend
                 (make-hamnosys-cell
                  value
                  colspan)
                 left-hand-hamnosys-row))
              (progn
                (pushend
                 (make-empty-cell)
                 left-hand-id-gloss-row)
                (pushend
                 (make-empty-cell)
                 left-hand-hamnosys-row))))
          (when colspan
            (setf colwait colspan))
          (when colwait
            (decf colwait))
          (when (eql colwait 0)
            (setf colwait nil))
        finally
          (return
              (list
               left-hand-id-gloss-row
               left-hand-hamnosys-row))))

(defun make-time-header (number-of-columns)
  "makes and returns the html for the header row of the table (time)"
  `((tr :class "header" :style "width:100%;")
    ((td)
     ((span)
      ((span)
       ((p :class "header-text")
        "time&#x2192;"))))
    ,@(loop for x from 0 to number-of-columns
            collect 
              `((td
                 :class "empty")))))
  

(defmethod make-html ((signed-form-predicates signed-form-predicates)
                      &key &allow-other-keys)
  "transforms a list of sign-predicates into a html object that can be added to the
   webinterface"
  (incf *avatar-counter*)
  (let* ((sign-table
          (make-sign-table (predicates signed-form-predicates)))
         (number-of-columns
          (determine-number-of-columns sign-table))
         (right-hand-rows
          (make-right-hand-rows
           (right-hand-cells sign-table)
           number-of-columns))
         (left-hand-rows
          (make-left-hand-rows
           (left-hand-cells sign-table)
           number-of-columns)))
    `((div :style "margin-top: 20px; margin-bottom: 20px; margin-left: 10px; width: 95%;")
      ((div :style "width:20%; height: 30%; margin-bottom: 10px; margin-top: 20px;")
       ((div :style "background: #DEEFE7;" :class ,(format nil "CWASAAvatar av~a" *avatar-counter*) :align "center" :onclick "CWASA.init({ambIdle:false,useClientConfig:false});")))
      ((table :class "sign-table" :style "margin-bottom:20px;")
       ((tbody)
        ((tr)
        ,(first right-hand-rows)
        ,(second right-hand-rows)
        ,(first left-hand-rows)
        ,(second left-hand-rows)))))))
  
     
     
