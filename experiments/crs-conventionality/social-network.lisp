
;; 03/02/2025: File created (Remi van Trijp)

;; (ql:quickload :crs-conventionality)

(in-package :crs-conventionality)

;;;;; This code supports experiments that want to investigate 
;;;;; the impact of the topology of the social network of agents
;;;;; on conventionalization dynamics. Documentation is provided
;;;;; in the code itself. 

;;;;; ---------------------------------------------------------
;;;;; 1.Social Network
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
;;;;; 2. Helper Functions for producers and comprehenders
;;;;; ---------------------------------------------------------
;;;;;
;;;;; Agents can play two basic roles: PRODUCER and COMPREHENDER.
;;;;; These terms are more general than "speaker" or "hearer", 
;;;;; which are specific to vocal languages only.

;; Helper functions.
(defun producer-p (agent)
  "Is the agent the producer?"
  (eql 'producer (discourse-role agent)))
;; (producer-p (make-instance 'agent))

(defmethod comprehender-p (agent)
  "Is the agent the comprehender?"
  (eql 'comprehender (discourse-role agent)))
;; (comprehender-p (make-instance 'agent))

(defun producer (agents)
  "Return the producer among a list of agents."
  (find-if #'producer-p agents))

(defun comprehender (agents)
  "Return the comprehender among a list of agents."
  (find-if #'comprehender-p agents))

;;;;; ---------------------------------------------------------
;;;;; 3. Determine Interacting Agents Using a Social Network
;;;;; ---------------------------------------------------------
;;;;; First a producer is selected, after which a comprehender
;;;;; is selected from the social network of the producer.

(defgeneric choose-producer (agents mode))

(defmethod choose-producer ((agents list)
                            (mode t))
  (declare (ignorable mode))
  ;; By defult we pick a random producer.
  (let ((producer (random-elt agents)))
    (setf (discourse-role producer) 'producer)
    producer))

(defmethod choose-producer ((experiment experiment)
                            (mode t))
  ;; In traditional experiments, the agents are listed in the agents slot.
  (choose-producer (agents experiment) mode))

(defmethod choose-producer ((population crs-conventionality-population)
                            (mode t))
  ;; In the crs-conventionality code, the agents are located in the population class.
  (choose-producer (agents population) mode))

(defgeneric choose-comprehender (agent mode))

(defmethod choose-comprehender ((agent agent)
                                (mode t))
  (declare (ignore mode))
  (let ((comprehender (random-elt (social-network agent))))
    (setf (discourse-role comprehender) 'comprehender)
    comprehender))

(defgeneric clean-agent (agent))

(defmethod clean-agent ((agent agent))
  (setf (communicated-successfully agent) nil
        (utterance agent) nil)
  agent)

(defmethod determine-interacting-agents ((experiment crs-conventionality-experiment)
                                         (interaction interaction) 
                                         (mode (eql :random-from-social-network))
                                         &key &allow-other-keys)
  "This method randomly picks one agent, and then randomly picks an agent 
   from its social network. In a fully connected network, this is the same 
   as picking two random agents."
  (let* ((agents (if (listp (agents experiment))
                   (agents experiment)
                   (agents (population experiment))))
         (producer (choose-producer agents mode))
         (comprehender (choose-comprehender producer mode))
         (interacting-agents (list producer comprehender)))
    ;; "clean" the agents:
    (mapcar #'clean-agent interacting-agents)
    (setf (interacting-agents interaction) interacting-agents)
    (notify interacting-agents-determined experiment interaction)))

;;;;; ---------------------------------------------------------
;;;;; 4. A Fully Connected Social Network
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
;;;;; 5. Regular Social Network
;;;;; ---------------------------------------------------------
;;;;;
;;;;; A "regular network" is a network where the node degree 
;;;;; distribution is a constant value for all nodes.

(defun create-regular-population-network (population &optional l)
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
;;;;; 6. Small World Network
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
    (setf (social-network agent) rewired-network)))

(defun rewire-regular-network (population p)
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
  (let ((population (or (agents experiment)
                        (agents (population experiment)))))
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
            (png-filename-string (format nil "~a~a.png" (directory-namestring filename) name)))
        (run-prog (format nil "circo -Tpng ~a > ~a" filename-string png-filename-string))

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

;; Fully connected network:
; (reset-id-counters)
;;; (let* ((experiment (make-instance 'crs-conventionality-experiment))
;;;        (agents (loop for i from 1 to 10 collect (make-instance 'agent))))
;;;   (setf (agents experiment) agents)
;;;   (initialize-social-network experiment)
;;;   (population-network->graphviz agents :make-image t :open-image t :use-labels? t))

;; Regular network:
;;; (let* ((experiment (make-instance 'crs-conventionality-experiment))
;;;        (agents (loop for i from 1 to 10 collect (make-instance 'agent))))
;;;   (setf (agents experiment) agents)
;;;   (set-configuration experiment :network-topology :regular)
;;;   (set-configuration experiment :local-connectivity 2)
;;;   (initialize-social-network experiment)
;;;   (population-network->graphviz agents :make-image t :open-image t :use-labels? t)
;;;   agents)

;; Small world network:
;;; (let* ((experiment (make-instance 'crs-conventionality-experiment))
;;;        (agents (loop for i from 1 to 10 collect (make-instance 'agent))))
;;;   (setf (agents experiment) agents)
;;;   (set-configuration experiment :network-topology :small-world)
;;;   (set-configuration experiment :local-connectivity 2)
;;;   (set-configuration experiment :rewiring-probability 0.3)
;;;   (initialize-social-network experiment)
;;;   (population-network->graphviz agents :make-image t :open-image t :use-labels? t)
;;;   agents)

;;;;; ---------------------------------------------------------
;;;;; 8. Tests.
;;;;; ---------------------------------------------------------

(deftest test-discourse-role ()
  (let ((agents (loop repeat 2 collect (make-instance 'agent))))
    (setf (discourse-role (first agents)) 'producer)
    (setf (discourse-role (second agents)) 'comprehender)
    (test-assert (equal (producer agents) (first agents)))
    (test-assert (equal (comprehender agents) (second agents)))))
;; (test-discourse-role)


(deftest test-social-network ()
  (labels ((make-ten-agents () (loop repeat 10 collect (make-instance 'agent))))
  (let* ((experiment (make-instance 'crs-conventionality-experiment)))
    (let* ((agents (setf (agents experiment) (make-ten-agents)))
           (agent (first agents)))
      (initialize-social-network experiment)
      (test-assert (social-network agent))
      (test-assert (= 9 (length (social-network agent))))
      (let* ((producer (choose-producer agents t))
             (comprehender (choose-comprehender producer t)))
        (test-assert (producer-p producer))
        (test-assert (comprehender-p comprehender))))
    (setf (agents experiment) nil
          (population experiment) (make-instance 'crs-conventionality-population
                                                 :agents (make-ten-agents)))
    (set-configuration experiment :network-topology :normal)
    (set-configuration experiment :local-connectivity 3)
    (initialize-social-network experiment)
    (test-assert (= 6 (length (social-network (first (agents (population experiment)))))))
    (setf (population experiment) nil
          (agents experiment) (make-ten-agents))
    (set-configuration experiment :network-topology :small-world)
    (set-configuration experiment :rewiring-probability 0.3)
    (initialize-social-network experiment)
    (test-assert (= 6 (length (social-network (first (agents experiment)))))))))
;; (test-social-network)