
;; 03/02/2025: File created (Remi van Trijp)
;; 05/02/2025: Changes made by Lara to fit with crs-conventionality experiments: 
;;             - Producer and comprehender changed to speaker and hearer.
;;             - Speaker and hearer changed to get-speaker and get-hearer.
;;             - Value of population is an object and not a list.
;; 24/02/2025: Changes made by Jamie:
;;             - Made small word network rewiring undirected (remove/add old/new links from other agent).
;;             - Removed some functions defined elsewhere.

;; (ql:quickload :crs-conventionality)

(in-package :crs-conventionality)

;;;;; This code supports experiments that want to investigate 
;;;;; the impact of the topology of the social network of agents
;;;;; on conventionalization dynamics. Documentation is provided
;;;;; in the code itself. 

;;;;; ---------------------------------------------------------
;;;;; Social Network
;;;;; ---------------------------------------------------------
;;;;;
;;;;; A social network consists of the connections that an agent
;;;;; has to other agents in the network. We imagine all agents
;;;;; forming a circle. Social links can be directed clockwards
;;;;; and counter-clockwards.

(defsetf social-network (agent) (data)
  "Convenience setf"
  `(set-data ,agent :social-network ,data))

(defun social-network (agent)
  "Helper function to access the social network of an agent."
  (get-data agent :social-network))

;;;;; ---------------------------------------------------------
;;;;; Fully Connected Social Network
;;;;; ---------------------------------------------------------
;;;;;
;;;;; By default, we use a fully connected social network in 
;;;;; which each agent can interact with each other agent.

(defun create-fully-connected-network (population &optional reverse-population)
  (if (null population) 
    (reverse reverse-population)
    (progn
      (setf (social-network (first population))
            (append reverse-population (rest population)))
      (create-fully-connected-network (rest population)
                                      (cons (first population) reverse-population)))))

;;;;; ---------------------------------------------------------
;;;;; Regular Social Network
;;;;; ---------------------------------------------------------
;;;;;
;;;;; A "regular network" is a network where the node degree 
;;;;; distribution is a constant value for all nodes.

(defun create-regular-population-network (population &optional l) ;; l is a bad parameter name
  "Turn an unstructured population into a regular lattice 
   in which each agent has connections to its l nearest neighbors. 
   If l is not specified, create a fully connected network."
  (let ((agents (if (listp population) population (agents population))))
    (if (null l)
      (create-fully-connected-network agents)
      ;; Safety check:
      (let* ((population-size (length agents))
             (max-l (- (/ population-size 2) 1)))
        (when (> l max-l)
          (setf l max-l))
        (let ((clockwise-population (copy-list agents))
              (counterclockwise-population (reverse agents)))
          ;; We make these lists cyclic:
          (setf (rest (last clockwise-population)) clockwise-population
                (rest (last counterclockwise-population)) counterclockwise-population)
          ;; Now we define l clockwise links:
          (loop for agent in agents
                do (setf (social-network agent) (loop for i from 1 to l
                                                      collect (nth i clockwise-population))
                         clockwise-population (rest clockwise-population)))
          ;; Now we add l in counter-clockwise direction:
          (loop for agent in (reverse agents)
                do (setf (social-network agent) (append (loop for i from 1 to l
                                                              collect (nth i counterclockwise-population))
                                                        (social-network agent))
                         counterclockwise-population (rest counterclockwise-population)))
          ;; Return the population
          agents)))))

;;;;; ---------------------------------------------------------
;;;;; Small World Network
;;;;; ---------------------------------------------------------
;;;;; The Small World network model (~ Watts and Strogatz) is 
;;;;; a simple but popular social network structure because it 
;;;;; resembles real-world social networks. We create such
;;;;; networks by rewiring local links in clockwise (outgoing)
;;;;; direction with a long-distance link with probability p.

(defun rewire-social-links (agent rewiring-pairs)
  (let ((rewired-network (loop for social-link in (social-network agent)
                               collect (let ((rewiring-pair (assoc social-link rewiring-pairs :test #'equal)))
                                         (if rewiring-pair
                                           (second rewiring-pair)
                                           social-link)))))
    (setf (social-network agent) rewired-network))
  ;; Remove current agent from social network of agent connected before rewiring, add to the social network of the newly connected agent.
  (loop for pair in rewiring-pairs
        do (setf (social-network (first pair)) (remove agent (social-network (first pair))))
        do (setf (social-network (second pair)) (push agent (social-network (second pair))))))

(defun rewire-regular-network(population p)
  "Rewire local links with long-distance links with probability p."
  (let ((agents (if (listp population) population (agents population))))
    (dolist (agent agents)
      (let* ((old-links (social-network agent))
             ;; Any agent *NOT* in the local links is a candidate replacement. 
             (candidate-replacements (set-difference agents (cons agent (social-network agent))
                                                     :test #'equal))
             (rewirement-list (loop for old-link in (rest old-links)
                                    when (<= (random 1.0) p)
                                      collect (let ((new-link (random-elt candidate-replacements)))
                                                (setf candidate-replacements 
                                                      (remove new-link candidate-replacements :test #'equal))
                                                (list old-link new-link)))))
        (rewire-social-links agent rewirement-list)))
    agents))

(defgeneric initialize-social-network (experiment))

;; Documentation:
;; ------------------------------------------------------------------------------------------
;; To exploit the social network of agents, you have to initialize your experiment with 
;; at minimum these functions and configurations:
;;
;;        (initialize-social-network experiment) -> goes through the population to make links
;;        (set-configuration experiment :determine-interacting-agents-mode :random-from-social-network)
;;
;; Without any other configuration, the population will instantiate a fully connected network.
;; This behaviour can change using the following configurations:
;;
;;        (set-configuration experiment :network-topology :normal)
;;        (set-configuration experiment :local-connectivity 3)
;;
;;        -> Whataver the value specified, the code will currently assume that you want either 
;;           a normal network topology, or a small world topology.
;;        -> The configuration :local-connectivity specifies how many links the agent will have 
;;           clockwards in the population as well as counter-clockwise. In other words, if this
;;           configuration is set to 3, the agent will have 6 links with its nearest neighbours
;;           (three nearest to its left, and three nearest to its right).
;;
;; In a normal network, new conventions are expected to diffuse locally first and only then spread 
;; towards the rest of the network in an almost contiguous way. To turn this normal network into 
;; a small-world network, you need to set the rewiring probability to a value between 0.0 and 1.0:
;;
;;        (set-configuration experiment :rewiring-probability 0.3)
;;
;; This is the probability that a local link will be replaced by a long-distance link. These long-
;; distance links allow conventions to make bigger jumps in the network, which may speed up its 
;; diffusion among agents.

(defmethod initialize-social-network ((experiment crs-conventionality-experiment))
  (let ((population (agents (population experiment))))
    ;; If no network topology is specified in the configuration, make a fully
    ;; connected network.
    (if (null (get-configuration experiment :network-topology))
      (create-fully-connected-network population)
      ;; Otherwise we create a regular network that we might rewire
      (let ((local-connectivity (get-configuration experiment :local-connectivity))
            (rewiring-probability (or (get-configuration experiment :rewiring-probability)
                                      0.0)))
        (create-regular-population-network population local-connectivity)
        (rewire-regular-network population rewiring-probability)
        population))))

;;;;; ---------------------------------------------------------
;;;;; 7. Graphviz Visualizations of the Social Network
;;;;; ---------------------------------------------------------

;;; If you want to visualize the social network of the population, you can do so with
;;; the function #'population-network->graphviz below. This visualization can be 
;;; improved a lot but it helps to have an idea of the network structure.

(defun make-time-stamp ()
  (multiple-value-bind (seconds minutes hour day month year)
      (get-decoded-time)
    (format nil "~a-~a-~a-~a-~a-~a" year month day hour minutes seconds)))

(defun make-id-for-gv (agent)
  "Makes an ID for the graphviz output."
  (replace-char (symbol-name (id agent)) #\- #\_))

(defun population-network->graphviz (population &key (directory '(".tmp"))
                                                (name (format nil "population-graph-~a" (make-time-stamp)))
                                                (make-image nil) (open-image nil) (use-labels? nil))
  (let ((filename (babel-pathname :directory directory
                                  :name name
                                  :type "gv")))
    
    ;; First we create the graphviz file, which you can open in software packages like OmniGraffle.
    (with-open-file (out filename
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
      ;; Graph preambles:
      (format out "~%strict graph g1 {")
      (format out "~%     layout=\"circo\";")
      (format out "~%     node [shape=~a];" (if use-labels? "circle" "point"))
      ;; Adding the nodes:
      (let (edges)
        (dolist (agent population)
          (let ((agent-id (make-id-for-gv agent))
                (linked-agents (social-network agent)))
            ;; We add the node:
            (format nil "~%      ~a;" agent-id)
            ;; At the same time we store information about the edges for later.
            (dolist (linked-agent linked-agents)
              (let ((link (format nil "~a -- ~a" agent-id (make-id-for-gv linked-agent)))
                    (alternative-link (format nil "~a -- ~a" (make-id-for-gv linked-agent) agent-id)))
                ;; If it is a new edge, we keep it:
                (unless (loop for edge in edges
                              when (member edge (list link alternative-link) :test #'string=)
                                return t)
                  (push link edges))))))
        ;; Now add the edges to the graph. Note that graphviz automatically chooses the layout that it estimates
        ;; as the best one. So if you label the nodes, you might not get the order you imagine.
        (dolist (edge (reverse edges)) ; Reverse leads to slightly nicer graph
          (format out "~%     ~a" edge))
        ;; End of the graphviz file:
        (format out "~%}")))

    ;; If you do not have OmniGraffle, we can build the image ourselves. You can do this manually
    ;; in your terminal app if you want to customize the output.
    (when make-image
      (let ((filename-string (format nil "~a~a.gv" (directory-namestring filename) name))
            (png-filename-string (format nil "~a~a.png" (directory-namestring filename) name))
            (svg-filename-string (format nil "~a~a.svg" (directory-namestring filename) name)))    
        (run-prog (format nil "circo -Tpng ~a > ~a" filename-string png-filename-string))
        (run-prog (format nil "circo -Tsvg ~a > ~a" filename-string svg-filename-string))
        
        (when open-image
          (cond ((equal (software-type) "Darwin")
                 (run-prog "open" :args (list png-filename-string)))
                ;; Warning: not yet tested on Linux or Windows!
                ((equal (software-type) "Linux")
                 (run-prog "see" :args (list png-filename-string)))
                ((equal (software-type) "Microsoft Windows")
                 (run-prog "cmd"
                           :args (list "/C"
                                       (string-replace
                                        (format nil "c:~a" png-filename-string "/" "\\")))))))))))
