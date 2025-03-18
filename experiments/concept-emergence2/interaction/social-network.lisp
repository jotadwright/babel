(in-package :cle)

;;;;; ------------------
;;;;; + Social Network +
;;;;; ------------------

;;;;; A social network consists of the connections that an agent
;;;;; has to other agents in the network. We imagine all agents
;;;;; forming a circle. Social links can be directed clockwards
;;;;; and counter-clockwards.

;;;;; ----------------------------------
;;;;; + Fully Connected Social Network +
;;;;; ----------------------------------

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

;;;;; --------------------------
;;;;; + Regular Social Network +
;;;;; --------------------------

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
                do (setf (social-network agent) (loop for i from 1 to l collect (nth i clockwise-population)))
                do (setf clockwise-population (rest clockwise-population)))
          ;; Now we add l in counter-clockwise direction:
          (loop for agent in (reverse agents)
                do (setf (social-network agent) (append (loop for i from 1 to l
                                                              collect (nth i counterclockwise-population))
                                                        (social-network agent)))
                do (setf counterclockwise-population (rest counterclockwise-population)))
          ;; Return the population
          agents)))))

;;;;; -----------------------
;;;;; + Small World Network +
;;;;; -----------------------

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

(defgeneric initialise-social-network (experiment))

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

(defmethod initialise-social-network ((experiment cle-experiment))
  (case (get-configuration experiment :network-topology)
    ;; fully-connected
    (:fully-connected (create-fully-connected-network (agents experiment)))
    ;; regular
    (:regular (create-regular-population-network (agents experiment) (get-configuration experiment :local-connectivity)))
    ;; small-world
    (:small-world (progn
                    (create-regular-population-network (agents experiment) (get-configuration experiment :local-connectivity))
                    (rewire-regular-network (agents experiment) (get-configuration experiment :rewiring-probability))))))

