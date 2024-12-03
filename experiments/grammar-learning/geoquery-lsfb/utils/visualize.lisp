(in-package :geoquery-lsfb)

;;-----------------;;
;; CSS definitions ;;
;;-----------------;;

;; the whole table
(define-css 'sign-table  "
.sign-table {width: 100%%; border: 1px solid black; border-collapse: collapse;}")

;; empty cells in table
(define-css 'empty "
.empty {background-color: #FFFFFF; upper-border: 1px solid black;}
")

;; row header cells
(define-css 'header "
.header {border: 1px solid black;}
")

;; row header text
(define-css 'header-text "
.header-text {font-weight: bold; font-color: black;}
")

; a cell with manual information
(define-css 'manual-cell "
.manual-cell {background-color: #ffcc00; border: 1px solid black;border-collapse: collapse; text-align: center;}
")

(define-css 'hamnosys-cell "
.hamnosys-cell {border: 1px solid black; border-collapse: collapse; padding-right: 10px; padding-left: 10px; text-align: center;}
")

;; text representing an articulation
(define-css 'articulation-text "
.articulation-tag {font-color: black;}
")


(defun find-rh-var-column (var right-hand-articulations)
  "finds the column number for a given variable that
   refers to a right-hand-articulation"
  (loop for right-hand-articulation in right-hand-articulations
        when
          (eql
           (first
            right-hand-articulation)
           var)
          do
            (return
             (third
              right-hand-articulation))))

(defun find-column-range (predicate
                          right-hand-articulations
                          coincides-predicates)
  "finds the range of a left-hand-articulation predicate
   by relating it to its coinciding right-hand-articulations"
  (loop with start = nil
        with end = nil
        
        ;; variable referring to left hand articulation
        with left-hand-articulation-var
          = (second predicate)
        
        ;; loop over all coincides predicates
        for coincides-predicate in coincides-predicates
        
        ;; the left-hand-variable in the coincides predicate
        for coincides-lh-var
          = (third coincides-predicate)

        ;; the right-hand-variable in the coincides predicate
        for coincides-rh-var
          = (second coincides-predicate)

        ;; the column number of the right-hand-variable
        for column-nr
          = (find-rh-var-column
             coincides-rh-var
             right-hand-articulations)

        ;; if the coincides left hand var is equal to the predicate one's
        do (when
               (eql
                coincides-lh-var
                left-hand-articulation-var)

             ;; adapt start and end of the articulation accordingly
             (unless start
               (setf start column-nr))
             (unless end
               (setf end column-nr))
             (when (< column-nr start)
               (setf start column-nr))
             (when (> column-nr end)
               (setf end column-nr)))
        
        ;; return the start and end as a cons 
        finally (return (cons start end))))


  

(defun retrieve-articulation-info (sorted-predicates
                                   sorted-articulations)
  "gets all necessary information for the predicates of the specified type,
   i.e. what is the articulation's value, fcg-variable and column-nr."
  (let ((output sorted-articulations))       
      (loop with right-hand-predicates
              = (gethash
                 'right-hand-articulation
                 sorted-predicates)
            with two-hand-predicates
              = (gethash
                 'two-hand-articulation
                 sorted-predicates)
            ;; set column counter to 0
            with column-counter = 0
            with sorted-vars-and-during-preds
              = (sort-vars
                 (gethash
                  'adjacent
                  sorted-predicates)
                 (append
                  right-hand-predicates
                  two-hand-predicates)
                 (gethash
                  'during
                  sorted-predicates)
                 (gethash
                  'end-coincides
                  sorted-predicates))
            ;; loop over sorted vars
            for var in (first sorted-vars-and-during-preds)
            ;; find the predicate that corresponds to each var
            for corresponding-rh-predicate
              = (find-by-fcg-id
                 right-hand-predicates
                 var)
            for corresponding-2h-predicate
              = (find-by-fcg-id
                 two-hand-predicates
                 var)
            
            when corresponding-rh-predicate
              do
              ;; create a list with the var of the predicate
              ;; the value, and current column number. Push this list to output
              (push
               `(,(second corresponding-rh-predicate)
                 ,(third corresponding-rh-predicate)
                 ,column-counter)
               (gethash
                'right-hand-articulation
                output))
            when corresponding-2h-predicate
              do
              (push
               `(,(second corresponding-2h-predicate)
                 ,(third corresponding-2h-predicate)
                 ,column-counter)
               (gethash
                'right-hand-articulation
                output))
              (push
               `(,(second corresponding-2h-predicate)
                 ,(third corresponding-2h-predicate)
                 (,column-counter . ,column-counter))
               (gethash
                'left-hand-articulation
                output))
            unless (or
                    corresponding-2h-predicate
                    corresponding-rh-predicate)
              do (push
                  `(,var
                    nil
                    ,column-counter)
                  (gethash
                   'right-hand-articulation
                   output))

            do
              ;; increase column counter for next iteration
              (incf column-counter)
              (setf (gethash 'during sorted-predicates)
                    (second sorted-vars-and-during-preds))
              (setf (gethash 'end-coincides sorted-predicates)
                    (third sorted-vars-and-during-preds)))

           ;; get right-hand-articulations from type-hash:
           (loop with right-hand-articulations
                   = (gethash
                      'right-hand-articulation
                      sorted-articulations)
                 ;; get left-hand-predicates from type-hash
                 with predicates
                   = (gethash
                      'left-hand-articulation
                      sorted-predicates)
                 ;; get coincides-predicates from type-hash
                 with coincides-predicates
                   = (append
                      (gethash
                       'start-coincides
                       sorted-predicates)
                      (gethash
                       'end-coincides
                       sorted-predicates)
                      (gethash
                       'during
                       sorted-predicates))
                 ;; loop over predicates
                 for predicate in predicates
                 ;; determine column range
                 for column-range
                   = (find-column-range
                      predicate
                      right-hand-articulations
                      coincides-predicates)
             
              do
                   ;; create a list with the var of the predicate,
                   ;; the value, and current column number. Push this list to output
                   (push
                    `(,(second predicate)
                      ,(third predicate)
                      ,column-range)
                    (gethash
                     'left-hand-articulation
                     output)))
           output))

(defun make-type-information-table (predicates)
  "gathers information about each predicate
   (value, fcg-tag, begin and end column
   and stores it in a hash-table sorted by type"
  (let ((sorted-predicates
         (sort-predicates predicates))
        (type-information-table
         (make-hash-table))) 
    (setf
     type-information-table
     (retrieve-articulation-info
      sorted-predicates
      type-information-table))
    type-information-table))

(defun find-column-nr-in-range (expressions column-nr)
  "finds the left hand expression in expressions that
   has a column-range including the column-nr"
  (loop for expression in expressions
        for column-range = (third expression)
        for start = (car column-range)
        for end = (cdr column-range)
        when (or (eql start column-nr)
                 (eql end column-nr))
          do (return expression)
        when (and (< start column-nr)
                  (> end column-nr))
          do (return expression)))

(defun find-rh-by-column-nr-start (expressions column-nr)
  "find the right hand expression in expressions
   that starts with columnn-nr"
  (loop for expression in expressions
        for column-start = (first (last expression))
        when (eql column-nr column-start)
          do (return expression)))

(defun find-lh-by-column-nr-start (expressions column-nr)
  "find the left hand expression in expressions
   that starts with columnn-nr"
  (loop for expression in expressions
        for column-start = (car (first (last expression)))
        when (eql column-nr column-start)
          do (return expression)))


(defun make-row (sorted-expressions
                 number-of-columns
                 &key
                 (type 'right-hand-articulation))
    (cond
     ;; Condition A: the articulation is a right-hand-articulation
     ((eql
       type
       'right-hand-articulation)
      ;; make a header for the id-gloss row
       `((tr :class "header")
         ((td :class "header")
          "RH-ID-glosses")
         ;; creating all cells of the id-gloss row
         ,@(loop with output = '()
                 ;; loop over the number of right hand articulations
                 for x from 0 to number-of-columns
                 ;; get the expression with current column number
                 for expression
                   = (find-rh-by-column-nr-start
                      (gethash
                       'right-hand-articulation
                       sorted-expressions)
                      x)
                 ;; fcg-tag of the expression
                 for tag
                   = (string-downcase
                      (string (first expression)))
                 ;; creating span-ids (will be used for highlighting)
                 for span-id-1 = (make-const "S")
                 do
                   ;; make the html of the cell
                   (if (string=
                        (utils::remove-numeric-tail tag)
                        "dummy")
                     (pushend `((td :class "empty" :style "width: 20px;")) output)
                     (pushend
                      `((td
                         :class "manual-cell")
                        ((span)
                         ((span
                           :id ,(string span-id-1)
                           :onclick ,(format
                                      nil
                                      "highlightSymbol('~a','~a','~a');"
                                      tag
                                      span-id-1
                                      (web-interface::get-color-for-symbol tag)))
                          ((a
                            :class "articulation-tag"
                            :name ,tag)
                           ,tag))))
                      output))
                     finally
                     (return output))))
     
     ;; Condition B: type is right-hand-hamnosys
     ((eql
       type
       'right-hand-hamnosys)
      ;; creating header of RH hamnosys row
       `((tr :class "header")
         ((td :class "header")
          "RH-HamNoSys")
         ;; creating all cells of the hamnosys row
         ,@(loop with output = '()
                 ;; loop over the number of right hand articulations
                 for x from 0 to number-of-columns
                 ;; get the expression with current column number
                 for expression
                   = (find-rh-by-column-nr-start
                      (gethash
                       'right-hand-articulation
                       sorted-expressions)
                      x)
                 ;; value (= hamnosys) of the expression
                 for value
                   = (string
                      (second expression))
                 do
                   ;; make the html of the cell
                   (if (string= value "NIL")
                     (pushend `((td :class "empty")) output)
                   (pushend
                    `((td
                       :class "hamnosys-cell")
                      ((span)
                       ((ham)
                        ,value)))
                    output))
                 finally
                   (return output))))

   ;; Condition C: type is left-hand-articulation
   ((eql
     type
     'left-hand-articulation)
    
     ;; make a header for the row
    `((tr :class "header")
      ((td :class "header")
       "LH-ID-glosses")

      ;; make cells of the row
      ,@(loop with output = '()
              with colwait = nil
              for x from 0 to number-of-columns
              
              ;; get the expression that starts with column-nr
              for expression
                = (find-lh-by-column-nr-start 
                   (gethash
                    'left-hand-articulation
                    sorted-expressions)
                   x)

              ;; the fcg-tag of the expression
              for tag
                = (string-downcase
                   (string (first expression)))
              
              ;; creating span-ids
              for span-id-1 = (make-const "S")

              ;; collecting start and end of the cell
              for start = x
              for end
                = (cdr
                   (third expression))

              ;; check whether cell spans multiple columns
              for col-dif = (when end
                              (- end start))
              for colspan = (when col-dif
                              (if (eql col-dif 0)
                                nil
                                (+ col-dif 1)))

              ;; make html for the cell
              do 
                (unless colwait
                  (if expression
                    (pushend
                     `((td
                        :class "manual-cell"
                        :colspan ,(write-to-string colspan))
                       ((span)
                        ((span
                          :id ,(string span-id-1)
                          :onclick ,(format
                                     nil
                                     "highlightSymbol('~a','~a','~a');"
                                     tag
                                     span-id-1
                                     (web-interface::get-color-for-symbol tag)))
                         ((a
                          :class "articulation-tag"
                          :name ,tag)
                         ,tag))))
                     output)
                     (pushend `((td :class "empty")) output)))
                ;; adapt remaining wait and span
                (when colspan
                  (setf colwait colspan))
                (when colwait
                  (decf colwait))
                (when (eql colwait 0)
                  (setf colwait nil))
              finally (return output))))
   
   ;; Condition D: type is left-hand-hamnosys
   ((eql
     type
     'left-hand-hamnosys)
    
     ;; make a header for the row
    `((tr :class "header")
      ((td :class "header")
       "LH-HamNoSys")

      ;; make cells of the row
      ,@(loop with output = '()
              with colwait = nil
              for x from 0 to number-of-columns
              
              ;; get the expression that starts with column-nr
              for expression
                = (find-lh-by-column-nr-start 
                   (gethash
                    'left-hand-articulation
                    sorted-expressions)
                   x)

               ;; value (= hamnosys) of the expression
               for value
                = (string
                   (second expression))

              ;; collecting start and end of the cell
              for start = x
              for end
                = (cdr
                   (third expression))

              ;; check whether cell spans multiple columns
              for col-dif = (when end
                              (- end start))
              for colspan = (when col-dif
                              (if (eql col-dif 0)
                                nil
                                (+ col-dif 1)))

              ;; make html for the cell
              do 
                (unless colwait
                  (if expression
                    ;; if the cell is not a continuation of
                    ;; previously added sign, we make a new segment for it
                    (pushend
                     `((td
                        :class "hamnosys-cell"
                        :colspan ,(write-to-string colspan))
                       ((span)
                        ((ham)
                        ,value)
                        ))
                     output)

                    ;; if the cell is a continuation of
                    ;; previously added sign, we add an empty cell
                    (pushend `((td :class "empty")) output)))
                
                ;; adapt remaining wait and span
                (when colspan
                  (setf colwait colspan))
                (when colwait
                  (decf colwait))
                (when (eql colwait 0)
                  (setf colwait nil))
              finally (return output))))))

(defun represent-signs (predicates)
  "represents a list of sign-predicates
   visually in the webinterface
   in a multilinear fashion"
  (let* (;; extract necessary information from predicates
         (type-information-table
          (make-type-information-table
           predicates))
         ;; determine number of columns in html table
         (number-of-columns
          (-
           (length
            (gethash
             'right-hand-articulation
             type-information-table))
           1)))

    ;; create html table
    `((table
       :style "width: 50%%; border: 1px solid black; border-collapse: collapse; margin: 20px;")
      
      ;; body for the table
      ((tbody)
       
       ;upper header: time
       ((tr :class "header")
        ((td)
         ((span)
          ((span)
           ((a :class "header-text") "time&#x2192;"))))
        ;; all cells of the time row are empty
        ,@(loop with output = '()
                for x from 0 to number-of-columns
                do (pushend
                    `((td
                       :class "empty"))
                    output)
                finally
                  (return output)))

       ;; add the rows for left and right hand articulations
       ,@(loop with output = '()
               for type in '(right-hand-articulation
                             right-hand-hamnosys
                             left-hand-articulation
                             left-hand-hamnosys)
               do (pushend
                   (make-row
                    type-information-table
                    number-of-columns
                    :type type) output)
               finally (return output))))))


        
     
     
