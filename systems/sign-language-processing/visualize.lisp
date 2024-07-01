(in-package :slp)

(define-css 'sign-table  "
.sign-table {width: 100%%; border: 1px solid black; border-collapse: collapse;}")

(define-css 'empty "
.empty {background-color: #FFFFFF; upper-border: 1px solid black;}
")

(define-css 'header "
.header {border: 1px solid black;}
")

(define-css 'header-text "
.header-text {font-weight: bold; font-color: black;}
")

(define-css 'manual-cell "
.manual-cell {background-color: #ffcc00; border: 1px solid black;border-collapse: collapse;}
")

(define-css 'parameter-cell "
.parameter-cell {background-color: #80bfff; border: 1px solid black;border-collapse: collapse;}
")

(define-css 'modification-cell "
.modification-cell {background-color: #C26A77; border: 1px solid black;border-collapse: collapse;}
")

(define-css 'articulation-text "
.articulation-text {font-color: black;}
")

(defun compare-elem-to-first-arg (elem predicate)
  "a test function that compares element to the first argument of the predicate"
  (eql elem (second predicate)))

(defun sort-vars (meets)
  "sorts the variables of a set of meets constraints, and returns the variables in a ordered list"
  (let* ((all-vars '())
         (sorted-vars (loop with second-arg-vars = '()
                            for predicate in meets
                            do (unless (member (second predicate) all-vars)
                                 (push (second predicate) all-vars))
                               (unless (member (third predicate) all-vars)
                                 (push (third predicate) all-vars))
                               (unless (member (third predicate) second-arg-vars)
                                 (push (third predicate) second-arg-vars))
                            finally (return (set-difference all-vars second-arg-vars)))))
    (loop while (NOT (eql (length sorted-vars) (length all-vars)))
          for next-predicate = (find (first sorted-vars) meets :test #'compare-elem-to-first-arg)
          do (delete next-predicate meets :test #'equal)
             (push (third next-predicate) sorted-vars))
    (reverse sorted-vars)))

(defun find-by-fcg-id (intervals fcg-id)
  "finds an interval using its fcg-id"
  (loop for interval in intervals
        when (eql (fcg-id interval) fcg-id)
          do (return interval)))

(defun find-column (var dominant-intervals)
  "finds the column of an dominant-interval using a variable"
  (loop for dominant-interval in dominant-intervals
        when (eql (fcg-id dominant-interval) var)
          do (return (begin dominant-interval))))

(defun find-column-range (non-dominant-interval dominant-intervals coincides-predicates)
  "determines the column-range of nondominant intervals using the coincides predicates and the dominant intervals"
  (loop with start = nil
        with end = nil
        with non-dominant-fcg-id = (fcg-id non-dominant-interval)
        for coincides-predicate in coincides-predicates
        for coincides-var = (third coincides-predicate)
        for dominant-hand-var = (second coincides-predicate)
        for column-nr = (find-column dominant-hand-var dominant-intervals)
        do (when (eql coincides-var non-dominant-fcg-id)
             (unless start (setf start column-nr))
             (unless end (setf end column-nr))
             (if (and column-nr start)
               (cond ((< column-nr start) (setf start column-nr))
                      ((> column-nr end) (setf end column-nr)))
               (warn (format t "there was a problem during the visualisation including these variables: ~a and ~a" coincides-var dominant-hand-var))))
        finally (if (and start end)
                  (unless (= start end)
                  (setf end (+ end 1)))
                  (warn "Some constants are not linked as they should be in the transient structure. Check that there are no typo's in the variables of the cxns that applied"))
                (return (cons start end))))

(defun sort-predicates (predicates)
  (loop with sorted-predicates = (make-hash-table)
        for predicate in predicates
        for predicate-type = (first predicate)
        do (unless (eql (first (last predicate)) 'meets)
             (pushend predicate (gethash predicate-type sorted-predicates)))
        finally (return sorted-predicates)))

(defun find-dominant-meets (meets dominant-intervals)
  (let* ((dominant-variables (loop with output = '()
                             for interval in dominant-intervals
                             do (unless (member (fcg-id interval) output)
                                 (pushend (fcg-id interval) output))
                            finally (return output)))
         (dominant-meets (loop with output = '()
                         for predicate in meets
                         do (when (and (member (second predicate) dominant-variables)
                                       (member (third predicate) dominant-variables))
                              (pushend predicate output))
                         finally (return output))))
    dominant-meets))
                          

(defun find-by-column-nr (intervals column-nr)
  (loop for interval in intervals
        when (eql (begin interval) column-nr)
          do (return interval)))

(defun find-start-by-column-nr (intervals column-nr)
  (loop for interval in intervals
        for start = (begin interval)
        when (eql column-nr start)
          do (return interval)))


(defun make-dh-row (intervals number-of-columns row-name)
  `((tr :class "header")
    ((td :class "header") ,row-name)
    ,@(loop with output = '()
            for x from 0 to number-of-columns
            for interval = (find-by-column-nr intervals x)
            for fcg-id = (format nil "~a" (fcg-id interval))
            for span-id-2 = (make-const "S")
            do (pushend `((td :class "manual-cell")
                          ((span)
                           ((span :id ,(string span-id-2) :onclick ,(format nil "highlightSymbol('~a','~a','~a');" fcg-id span-id-2 (web-interface::get-color-for-symbol fcg-id)))
                            ((a :class "articulation-text" :name ,fcg-id) ,(string-downcase fcg-id))))) output)
            finally (return output))))

(defun make-dh-location-row (intervals number-of-columns row-name)
  `((tr :class "header")
    ((td :class "header") ,row-name)
  ,@(loop with output = '()
          for x from 0 to number-of-columns
          for interval = (find-by-column-nr intervals x)
          for location = (location interval)
          for value = (when location (format nil "~a" location))
          for span-id-2 = (make-const "S")
          do (if (NOT (eql value nil))
               (pushend `((td :class "parameter-cell")
                        ((span)
                         ((span :id ,(string span-id-2) :onclick ,(format nil "highlightSymbol('~a','~a','~a');" value span-id-2 (web-interface::get-color-for-symbol value)))
                          ((a :class "articulation-text" :name ,value) ,(string-downcase value))))) output)
               (pushend `((td :class "empty")) output))
          finally (return output))))

(defun make-dh-handshape-row (intervals number-of-columns row-name)
  `((tr :class "header")
    ((td :class "header") ,row-name)
    ,@(loop with output = '()
            for x from 0 to number-of-columns
            for interval = (find-by-column-nr intervals x)
            for handshape = (handshape interval)
            for value = (when handshape (format nil "~a" handshape))
            for span-id-2 = (make-const "S")
            do (if handshape
              (pushend `((td :class "parameter-cell")
                          ((span)
                           ((span :id ,(string span-id-2) :onclick ,(format nil "highlightSymbol('~a','~a','~a');" value span-id-2 (web-interface::get-color-for-symbol value)))
                            ((a :class "articulation-text" :name ,value) ,(string-downcase value))))) output)
              (pushend `((td :class "empty")) output))
            finally (return output))))

(defun make-dh-orientation-row (intervals number-of-columns row-name)
  `((tr :class "header")
    ((td :class "header") ,row-name)
    ,@(loop with output = '()
            for x from 0 to number-of-columns
            for interval = (find-by-column-nr intervals x)
            for orientation = (orientation interval)
            for value = (when orientation (format nil "~a" orientation))
            for span-id-2 = (make-const "S")
            do (if orientation
              (pushend `((td :class "parameter-cell")
                          ((span)
                           ((span :id ,(string span-id-2) :onclick ,(format nil "highlightSymbol('~a','~a','~a');" value span-id-2 (web-interface::get-color-for-symbol value)))
                            ((a :class "articulation-text" :name ,value) ,(string-downcase value))))) output)
              (pushend `((td :class "empty")) output))
            finally (return output))))

(defun make-dh-movement-row (intervals number-of-columns row-name)
   `((tr :class "header")
     ((td :class "header") ,row-name)
     ,@(loop with output = '()
             for x from 0 to number-of-columns
             for interval = (find-by-column-nr intervals x)
             for movement = (movement interval)
             for value = (when movement (format nil "~a" movement))
             for span-id-2 = (make-const "S")
             do (if movement
               (pushend `((td :class "parameter-cell")
                           ((span)
                            ((span :id ,(string span-id-2) :onclick ,(format nil "highlightSymbol('~a','~a','~a');" value span-id-2 (web-interface::get-color-for-symbol value)))
                             ((a :class "articulation-text" :name ,value) ,(string-downcase value))))) output)
               (pushend `((td :class "empty")) output))
             finally (return output))))

(defun make-dh-modification-row (intervals number-of-columns row-name)
  `((tr :class "header")
    ((td :class "header") ,row-name)
    ,@(loop with output = '()
            for x from 0 to number-of-columns
            for interval = (find-by-column-nr intervals x)
            for modification = (modification interval)
            for value = (when modification (format nil "~a" modification))
            for span-id-2 = (make-const "S")
            do (if modification
              (pushend `((td :class "modification-cell")
                          ((span)
                           ((span :id ,(string span-id-2) :onclick ,(format nil "highlightSymbol('~a','~a','~a');" value span-id-2 (web-interface::get-color-for-symbol value)))
                            ((a :class "articulation-text" :name ,value) ,(string-downcase value))))) output)
              (pushend `((td :class "empty")) output))
            finally (return output))))


(defun make-ndh-row (intervals number-of-columns row-name)
    `((tr :class "header")
      ((td :class "header") ,row-name)
      ,@(loop with output = '()
              with colwait = nil
              for x from 0 to number-of-columns
              for interval = (find-start-by-column-nr intervals x)
              for tag = (when interval (string-downcase (format nil "~a" (fcg-id interval))))
              for span-id-2 = (make-const "S")
              for start = x
              for end = (when interval (end interval))
              for col-dif = (when end (- end start))
              for colspan = (when col-dif (if (eql col-dif 0)
                                            nil
                                            col-dif))
              do 
                (unless colwait (if (NOT (eql tag nil)) (pushend `((td :class "manual-cell" :colspan ,(write-to-string colspan))
                                                        ((span)
                                                         ((span :id ,(string span-id-2) :onclick ,(format nil "highlightSymbol('~a','~a','~a');" tag span-id-2 (web-interface::get-color-for-symbol tag)))
                                                          ((a :class "articulation-text" :name ,tag) ,(string-downcase tag))))) output)
                                  (pushend `((td :class "empty")) output)))
                (when colspan
                  (setf colwait colspan))
                (when colwait
                  (decf colwait))
                (when (eql colwait 0)
                  (setf colwait nil))
              finally (return output))))

(defun make-ndh-location-row (intervals number-of-columns row-name)
    `((tr :class "header")
      ((td :class "header") ,row-name)
      ,@(loop with output = '()
              with colwait = nil
              for x from 0 to number-of-columns
              for interval = (find-start-by-column-nr intervals x)
              for location = (when interval (location interval))
              for value = (when location (format nil "~a" location))
              for span-id-2 = (make-const "S")
              for start = x
              for end = (when interval (end interval))
              for col-dif = (when end (- end start))
              for colspan = (when col-dif (if (eql col-dif 0)
                                            nil
                                            col-dif))
              do 
                (unless colwait (if (NOT (eql value nil)) (pushend `((td :class "parameter-cell" :colspan ,(write-to-string colspan))
                                                        ((span)
                                                         ((span :id ,(string span-id-2) :onclick ,(format nil "highlightSymbol('~a','~a','~a');" value span-id-2 (web-interface::get-color-for-symbol value)))
                                                          ((a :class "articulation-text" :name ,value) ,(string-downcase value))))) output)
                                  (pushend `((td :class "empty" :colspan ,(write-to-string colspan))) output)))
                (when colspan
                  (setf colwait colspan))
                (when colwait
                  (decf colwait))
                (when (eql colwait 0)
                  (setf colwait nil))
              finally (return output))))

(defun make-ndh-handshape-row (intervals number-of-columns row-name)
    `((tr :class "header")
      ((td :class "header") ,row-name)
      ,@(loop with output = '()
              with colwait = nil
              for x from 0 to number-of-columns
              for interval = (find-start-by-column-nr intervals x)
              for handshape = (when interval (handshape interval))
              for value = (when handshape (format nil "~a" handshape))
              for span-id-2 = (make-const "S")
              for start = x
              for end = (when interval (end interval))
              for col-dif = (when end (- end start))
              for colspan = (when col-dif (if (eql col-dif 0)
                                            nil
                                            col-dif))
              do 
                (unless colwait (if (NOT (eql value nil)) (pushend `((td :class "parameter-cell" :colspan ,(write-to-string colspan))
                                                        ((span)
                                                         ((span :id ,(string span-id-2) :onclick ,(format nil "highlightSymbol('~a','~a','~a');" value span-id-2 (web-interface::get-color-for-symbol value)))
                                                          ((a :class "articulation-text" :name ,value) ,(string-downcase value))))) output)
                                  (pushend `((td :class "empty" :colspan ,(write-to-string colspan))) output)))
                (when colspan
                  (setf colwait colspan))
                (when colwait
                  (decf colwait))
                (when (eql colwait 0)
                  (setf colwait nil))
              finally (return output))))

(defun make-ndh-orientation-row (intervals number-of-columns row-name)
    `((tr :class "header")
      ((td :class "header") ,row-name)
      ,@(loop with output = '()
              with colwait = nil
              for x from 0 to number-of-columns
              for interval = (find-start-by-column-nr intervals x)
              for orientation = (when interval (orientation interval))
              for value = (when orientation (format nil "~a" orientation))
              for span-id-2 = (make-const "S")
              for start = x
              for end = (when interval (end interval))
              for col-dif = (when end (- end start))
              for colspan = (when col-dif (if (eql col-dif 0)
                                            nil
                                            col-dif))
              do 
                (unless colwait (if (NOT (eql value nil)) (pushend `((td :class "parameter-cell" :colspan ,(write-to-string colspan))
                                                        ((span)
                                                         ((span :id ,(string span-id-2) :onclick ,(format nil "highlightSymbol('~a','~a','~a');" value span-id-2 (web-interface::get-color-for-symbol value)))
                                                          ((a :class "articulation-text" :name ,value) ,(string-downcase value))))) output)
                                  (pushend `((td :class "empty" :colspan ,(write-to-string colspan))) output)))
                (when colspan
                  (setf colwait colspan))
                (when colwait
                  (decf colwait))
                (when (eql colwait 0)
                  (setf colwait nil))
              finally (return output))))

(defun make-ndh-movement-row (intervals number-of-columns row-name)
    `((tr :class "header")
      ((td :class "header") ,row-name)
      ,@(loop with output = '()
              with colwait = nil
              for x from 0 to number-of-columns
              for interval = (find-start-by-column-nr intervals x)
              for movement = (when interval (movement interval))
              for value = (when movement (format nil "~a" movement))
              for span-id-2 = (make-const "S")
              for start = x
              for end = (when interval (end interval))
              for col-dif = (when end (- end start))
              for colspan = (when col-dif (if (eql col-dif 0)
                                            nil
                                            col-dif))
              do 
                (unless colwait (if (NOT (eql value nil)) (pushend `((td :class "parameter-cell" :colspan ,(write-to-string colspan))
                                                        ((span)
                                                         ((span :id ,(string span-id-2) :onclick ,(format nil "highlightSymbol('~a','~a','~a');" value span-id-2 (web-interface::get-color-for-symbol value)))
                                                          ((a :class "articulation-text" :name ,value) ,(string-downcase value))))) output)
                                  (pushend `((td :class "empty" :colspan ,(write-to-string colspan))) output)))
                (when colspan
                  (setf colwait colspan))
                (when colwait
                  (decf colwait))
                (when (eql colwait 0)
                  (setf colwait nil))
              finally (return output))))

(defun make-ndh-modification-row (intervals number-of-columns row-name)
    `((tr :class "header")
      ((td :class "header") ,row-name)
      ,@(loop with output = '()
              with colwait = nil
              for x from 0 to number-of-columns
              for interval = (find-start-by-column-nr intervals x)
              for modification = (when interval (modification interval))
              for value = (when modification (format nil "~a" modification))
              for span-id-2 = (make-const "S")
              for start = x
              for end = (when interval (end interval))
              for col-dif = (when end (- end start))
              for colspan = (when col-dif (if (eql col-dif 0)
                                            nil
                                            col-dif))
              do 
                (unless colwait (if (NOT (eql value nil)) (pushend `((td :class "modification-cell" :colspan ,(write-to-string colspan))
                                                        ((span)
                                                         ((span :id ,(string span-id-2) :onclick ,(format nil "highlightSymbol('~a','~a','~a');" value span-id-2 (web-interface::get-color-for-symbol value)))
                                                          ((a :class "articulation-text" :name ,value) ,(string-downcase value))))) output)
                                  (pushend `((td :class "empty" :colspan ,(write-to-string colspan))) output)))
                (when colspan
                  (setf colwait colspan))
                (when colwait
                  (decf colwait))
                (when (eql colwait 0)
                  (setf colwait nil))
              finally (return output))))



(defun retrieve-intervals-from-predicates (predicates)
  (loop with intervals = '()
        with meets = '()
        with coincides-relations = '()
        with buffer = '()
        for predicate in predicates
        do (if (or (eql (first predicate) 'left-hand-articulation)
                   (eql (first predicate) 'right-hand-articulation))
             (pushend (make-instance 'elan-interval
                                     :fcg-id (second predicate)
                                     :interval-type (first predicate)
                                     :value (third predicate)) intervals)
             (pushend predicate buffer))
           (loop for predicate in buffer
                 do (cond
                     ((eql (first predicate) 'meets)
                      (pushend predicate meets))
                     ((eql (first predicate) 'location)
                      (let ((ref-interval (when intervals (find-by-fcg-id intervals (second predicate)))))
                          (setf (location ref-interval) (third predicate))))
                     ((eql (first predicate) 'handshape)
                      (let ((ref-interval (when intervals (find-by-fcg-id intervals (second predicate)))))
                          (setf (handshape ref-interval) (third predicate))))
                     ((eql (first predicate) 'orientation)
                      (let ((ref-interval (when intervals (find-by-fcg-id intervals (second predicate)))))
                          (setf (orientation ref-interval) (third predicate))))
                     ((eql (first predicate) 'movement)
                      (let ((ref-interval (when intervals (find-by-fcg-id intervals (second predicate)))))
                          (setf (movement ref-interval) (third predicate))))
                     ((eql (first predicate) 'modification)
                      (let ((ref-interval (when intervals (find-by-fcg-id intervals (second predicate)))))
                          (setf (modification ref-interval) (third predicate))))
                     (t
                      (pushend predicate coincides-relations))
                 ))
        finally (return (values intervals meets coincides-relations))))

(defun sort-intervals (intervals sorted-vars)
  (loop with sorted-intervals = '()
        with column-counter = 0
        for var in sorted-vars
        for corresponding-interval = (find-by-fcg-id intervals var)
        do (setf (begin corresponding-interval) column-counter)
           (setf (end corresponding-interval) column-counter)
           (pushend corresponding-interval sorted-intervals)
           (incf column-counter)
        finally (return sorted-intervals)))

(defun find-columns (non-dominant-intervals dominant-intervals coincides-predicates)
  (loop for non-dominant-interval in non-dominant-intervals
        for column-range = (find-column-range non-dominant-interval dominant-intervals coincides-predicates)
        do (setf (begin non-dominant-interval) (car column-range))
           (setf (end non-dominant-interval) (cdr column-range))))
  
(defun represent-signs (predicates)
  (multiple-value-bind (intervals meets coincides-predicates)
      (retrieve-intervals-from-predicates predicates)
    (let* ((dominant-intervals '())
           (non-dominant-intervals '())
           (dh-sorted-intervals '())
           (number-of-columns nil)
           (dominant-hand-string (if (eql *dominant-hand* 'RH)
                                      "right hand"
                                      "left hand"))
           (non-dominant-hand-string (if (string= "right hand" dominant-hand-string)
                                          "left hand"
                                          "right hand")))
      (loop for interval in intervals
            do (if (eql (interval-type interval) (intern (upcase (string-replace (concatenate 'string dominant-hand-string " articulation") " " "-")) :slp))
                 (pushend interval dominant-intervals)
                 (pushend interval non-dominant-intervals)))
      (setf number-of-columns (- (length dominant-intervals) 1))
      (setf dh-sorted-intervals (sort-intervals dominant-intervals (sort-vars (find-dominant-meets meets dominant-intervals))))
      (find-columns non-dominant-intervals dh-sorted-intervals coincides-predicates)
  `((table :style "width: 100%%; border: 1px solid black; border-collapse: collapse;")
    ((tbody)
     ;upper header: time
     ((tr :class "header")
      ((td)
       ((span)
        ((span)
         ((a :class "header-text") "time&#x2192;"))))
      ,@(loop with output = '()
              for x from 0 to number-of-columns
              do (pushend `((td :class "empty")) output)
              finally (return output)))
     ,(make-dh-row dh-sorted-intervals number-of-columns (concatenate 'string dominant-hand-string " articulations"))
     ,(make-dh-location-row dh-sorted-intervals number-of-columns (concatenate 'string dominant-hand-string "location"))
     ,(make-dh-handshape-row dh-sorted-intervals number-of-columns (concatenate 'string dominant-hand-string " handshape"))
     ,(make-dh-orientation-row dh-sorted-intervals number-of-columns (concatenate 'string dominant-hand-string " orientation"))
     ,(make-dh-movement-row dh-sorted-intervals number-of-columns (concatenate 'string dominant-hand-string " movement"))
     ,(make-dh-modification-row dh-sorted-intervals number-of-columns (concatenate 'string dominant-hand-string " modification"))
     ,(make-ndh-row non-dominant-intervals number-of-columns (concatenate 'string non-dominant-hand-string " articulations"))
     ,(make-ndh-location-row non-dominant-intervals number-of-columns (concatenate 'string non-dominant-hand-string " location"))
     ,(make-ndh-handshape-row non-dominant-intervals number-of-columns (concatenate 'string non-dominant-hand-string " handshape"))
     ,(make-ndh-orientation-row non-dominant-intervals number-of-columns (concatenate 'string non-dominant-hand-string " orientation"))
     ,(make-ndh-movement-row non-dominant-intervals number-of-columns (concatenate 'string non-dominant-hand-string " movement"))
     ,(make-ndh-modification-row non-dominant-intervals number-of-columns (concatenate 'string non-dominant-hand-string " modification")))))))


             


     
     
