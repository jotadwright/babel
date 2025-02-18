(in-package :slp)

;;---------------------------;;
;; Functions for making HTML ;;
;;---------------------------;;

(defun make-empty-cell ()
  "makes and returns the html for an empty cell in the table"
  `((td :class "empty" :style "width: 20px;")))

(defun make-id-gloss-cell (span-id fcg-tag colspan)
  "makes and returns the html for an id-gloss-cell with the given span-id,
   fcg-tag and colspan"
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
       ,fcg-tag)))))

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
          = `((TR :CLASS "header")
              ((TD :CLASS "header")
               "RH"))
        with right-hand-hamnosys-row
          = `((TR :CLASS "header")
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
                1)
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
          = `((tr :class "header")
              ((td :class "header")
               "LH"))
        with left-hand-hamnosys-row
          = `((tr :class "header")
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
                  span-id fcg-tag
                  colspan)
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
  `((tr :class "header")
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
    `((table :class "sign-table")
       ((tbody)
        ,(first right-hand-rows)
        ,(second right-hand-rows)
        ,(first left-hand-rows)
        ,(second left-hand-rows)))))


        
     
     
