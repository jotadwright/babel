(in-package :geoquery-lsfb)

(defclass geoquery-agent (agent)
  ((grammar
    :documentation "the grammar of the agent"
    :type 'hashed-fcg-construction-set
    :initarg :grammar
    :accessor grammar
    :initform nil)))

(defun make-geoquery-agents (experiment)
  (let ((speaker (make-instance 'geoquery-agent
                                :experiment experiment
                                :grammar (make-geoquery-lsfb-grammar)))
        (hearer (make-instance 'geoquery-agent
                               :experiment experiment
                               :grammar (make-geoquery-lsfb-grammar))))
    (setf (discourse-role speaker) 'speaker)
    (setf (discourse-role hearer) 'hearer)
    (list speaker hearer)))
    
                 
                                
    