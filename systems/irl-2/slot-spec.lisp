
(in-package :irl-2)

;; ############################################################################
;; 'slot-spec' definition:
;; ----------------------------------------------------------------------------

(export '(slot-spec slot-spec-type))

(defclass slot-spec ()
  ((name :type symbol :reader slot-spec-name :initform nil :initarg :name
         :documentation "The name of the slot, which should be unique
                         in the scope of the primitive to which the
                         slot belongs")
   (type :type t :reader slot-spec-type :initform nil :initarg :type
         :documentation "The type of the slot, a Lisp typespec"))
  (:documentation "The primitive's slot spec (i.e. the arguments of the primitive)"))


(defmethod print-object ((slot-spec slot-spec) stream)
  (format stream "<slot-spec ~(~a~):~(~a~)>"
          (slot-spec-name slot-spec)
          (slot-spec-type slot-spec)))


(defun check-slot-spec-syntax (slot-spec-defs)
  "slot spec syntax checking"
  (assert (listp slot-spec-defs) ()
          "The slot-specs should be a list, got:~%  ~a"
          slot-spec-defs)
  (assert slot-spec-defs ()
          "The slot-specs should contain at least one element")
  (loop for slot-spec-def in slot-spec-defs
        do (assert (and (listp slot-spec-def) (= 2 (length slot-spec-def))
                        (symbolp (first slot-spec-def))
                        (symbolp (second slot-spec-def)))
                   () "Each slot-spec definition should be of form:~
                    ~%  (slot-name type),~%got:~%  ~a"
                   slot-spec-def)))


(defmacro make-slot-specs (slot-spec-defs)
  "Creates slot specs from a list of slot-spec definitions of the form:
   ((slot-name slot-type)*)"
  (check-slot-spec-syntax slot-spec-defs)
  `(list . ,(loop
             for slot-spec-def in slot-spec-defs
             collect
              `(make-instance 'slot-spec
                              :name ',(first slot-spec-def)
                              :type ',(second slot-spec-def)))))