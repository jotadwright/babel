(in-package :fcg)
(export '(export-categorial-network-evolution-to-jsonl))

(defun export-categorial-network-evolution-to-jsonl (categorial-network
                                                     &key
                                                     link-type
                                                     (path (babel-pathname
                                                            :directory '(".tmp")
                                                            :name (format nil "~acategorial-network"
                                                                          (multiple-value-bind (sec min hour day month year)
                                                                              (decode-universal-time (get-universal-time))
                                                                            (format nil "~d-~2,'0d-~2,'0d-~2,'0d-~2,'0d-~2,'0d-"
                                                                                    year month day hour min sec)))
                                                            :type "jsonl"))
                                                     (timestep 1) (interaction-number 1))
  "Exports the categorial network to a jsonl file where each line
represents a certain time step in the experiment. The interaction
number and time step do not have to correspond, as you can export the
network at a certain interval."
  (let* ((all-nodes ;; get a list of all node names
                    (loop for node in (categories categorial-network)
                          collect `((label . ,(mkstr node)))))
         ;; get a list of all the edges
         ;; this includes the edge-type
         ;; but excludes the weight
         (all-edges (links categorial-network :link-type link-type))
         ;; so get the weight separately
         (all-edges-with-weight
          (loop for (from to etype) in all-edges
                for w = (link-weight from to categorial-network :link-type etype)
                collect`((start-node . ,(mkstr from))
                         (end-node . ,(mkstr to))
                         (score . ,w)
                         (type . ,etype))))
         (json-hash (make-hash-table)))
    
    (setf (gethash 'edges json-hash) all-edges-with-weight)
    (setf (gethash 'nodes json-hash) all-nodes)
    
    (ensure-directories-exist path)
    (with-open-file (stream path :direction :output
                            :if-exists (if (= 1 timestep) :supersede :append)
                            :if-does-not-exist :create)
      (write-line
       (cl-json:encode-json-to-string
        `((time-step . ,timestep)
          (interaction-number . ,interaction-number)
          (graph . ,json-hash)))
       stream)
      (force-output stream))
    (format t "Categorial network written to ~a ~%" path)))

