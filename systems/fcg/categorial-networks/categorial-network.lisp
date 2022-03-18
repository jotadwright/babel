;; Copyright 2019 AI Lab, Vrije Universiteit Brussel - Sony CSL Paris

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at

;;     http://www.apache.org/licenses/LICENSE-2.0

;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;=========================================================================

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                        ;;
;; This file implements categorial networks in Fluid Construction Grammar.;;       
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package :fcg)

(export '(categorial-network set-categorial-network
          categories links link-types
          category-exists-p link-exists-p
          add-category add-categories
          remove-category remove-categories
          add-link remove-link
          link-weight set-link-weight
          incf-link-weight decf-link-weight
          neighbouring-categories neighbouring-categories-p
          connected-categories connected-categories-p
          categories-linked-p
          nr-of-categories nr-of-links
          compute-transitive-closure))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Classes for using categorial networks ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass categorial-network ()
  ((graph
    :accessor graph
    :initarg :graph
    :initform (graph-utils:make-undirected-typed-graph)
    :type 'graph-utils:undirected-typed-graph
    :documentation "Graph representing categorial network.")
   (transitive-closure
    :accessor transitive-closure
    :initarg :transitive-closure
    :initform (make-hash-table)
    :type 'hash-table
    :documentation "Transitive closure of categorial network."))
  (:documentation "Categorial network class"))

(defmethod print-object ((categorial-network categorial-network) stream)
  (format stream "<categorial-network: ~a categories, ~a links, ~a link-types)>"
          (nr-of-categories categorial-network)
          (nr-of-links categorial-network)
          (length (link-types categorial-network))))

(defmethod copy-object-content ((source categorial-network) (destination categorial-network))
  (setf (graph destination) (graph-utils:copy-graph (graph source)))
  (setf (transitive-closure destination) (copy-object (transitive-closure source))))

;;;;;;;;;;;;;;;;;;;;;;;
;; Monitoring events ;;
;;;;;;;;;;;;;;;;;;;;;;;

(define-event category-added (category symbol) (categorial-network categorial-network))
(define-event category-removed (category symbol) (categorial-network categorial-network))
(define-event link-added (category-1 symbol) (category-2 symbol) (categorial-network categorial-network))
(define-event link-removed (category-1 symbol) (category-2 symbol) (categorial-network categorial-network))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Methods for handling categorial networks ;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Accessing and listing ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric categorial-network (thing)
  (:documentation "Returns the categorial network of a grammar, an agent, ..."))

(defmethod categorial-network ((cxn-inventory hashed-fcg-construction-set))
  "Returns the categorial-network of a hashed cxn-inventory."
  (when (field? (blackboard cxn-inventory) :categorial-network)
    (get-data (blackboard cxn-inventory) :categorial-network)))

(defmethod categorial-network ((cxn-inventory fcg-construction-set))
  "Returns the categorial-network of a cxn-inventory."
  (when (field? (blackboard cxn-inventory) :categorial-network)
    (get-data (blackboard cxn-inventory) :categorial-network)))

(defmethod categorial-network ((cxn-inventory construction-set))
  "Returns the categorial-network of a non-original cxn-inventory."
  (categorial-network (original-cxn-set cxn-inventory)))

(defgeneric set-categorial-network (thing categorial-network)
  (:documentation "Sets the categorial network of a grammar, an agent, ..."))

(defmethod set-categorial-network ((cxn-inventory hashed-fcg-construction-set) categorial-network)
  "Sets the categorial network of a hashed cxn-inventory"
  (set-data (blackboard cxn-inventory) :categorial-network categorial-network))

(defmethod set-categorial-network ((cxn-inventory fcg-construction-set) categorial-network)
  "Sets the categorial network of a cxn-inventory"
  (set-data (blackboard cxn-inventory) :categorial-network categorial-network))

(defmethod set-categorial-network ((cxn-inventory construction-set) categorial-network)
  "Sets the categorial network of a non-original cxn-inventory"
  (set-categorial-network (original-cxn-set cxn-inventory) categorial-network))

(defgeneric categories (thing)
  (:documentation "Returns the categories in the network of a grammar."))

(defmethod categories ((categorial-network categorial-network))
  "Lists categories in categorial-network."
  (graph-utils:list-nodes (graph categorial-network)))

(defmethod categories ((cxn-inventory fcg-construction-set))
  "Lists categories in categorial-network."
  (categories (categorial-network cxn-inventory)))

(defmethod categories ((cxn-inventory hashed-fcg-construction-set))
  "Lists categories in categorial-network."
  (categories (categorial-network cxn-inventory)))


(defgeneric links (thing)
  (:documentation "Returns the links in the network of a grammar
   as triples of type (category-1 category-2 link-type). All links
   are returned as directed edges even though the categorial network
   is undirected. For example, the edge A<->B is returned as both
   (A, B, NIL) and (B, A, NIL)."))

(defmethod links ((categorial-network categorial-network))
  "Lists links in categorial-network."
  (graph-utils:list-edges (graph categorial-network)))

(defmethod links ((cxn-inventory fcg-construction-set))
  "Lists links in categorial-network."
  (links (categorial-network cxn-inventory)))

(defmethod links ((cxn-inventory hashed-fcg-construction-set))
  "Lists links in categorial-network."
  (links (categorial-network cxn-inventory)))


(defgeneric link-types (thing)
  (:documentation "Returns the types of links in the network of a grammar."))

(defmethod link-types ((categorial-network categorial-network))
  "Lists link types in categorial-network."
  (graph-utils::edge-types (graph categorial-network)))

(defmethod link-types ((cxn-inventory fcg-construction-set))
  "Lists link types in categorial-network."
  (link-types (categorial-network cxn-inventory)))

(defmethod link-types ((cxn-inventory hashed-fcg-construction-set))
  "Lists link types in categorial-network."
  (link-types (categorial-network cxn-inventory)))


(defmethod category-exists-p (category thing))

(defmethod category-exists-p ((category symbol) (categorial-network categorial-network))
  "predicate for checking whether category is part of network"
  (when (gethash category (graph-utils::nodes (graph categorial-network)))
    t))

(defmethod category-exists-p ((category symbol) (cxn-inventory fcg-construction-set))
  "predicate for checking whether category is part of network"
  (category-exists-p category (categorial-network cxn-inventory)))

(defmethod category-exists-p ((category symbol) (cxn-inventory hashed-fcg-construction-set))
  "predicate for checking whether category is part of network"
  (category-exists-p category (categorial-network cxn-inventory)))


(defgeneric link-exists-p (category-1 category-2 thing &key link-type)
  (:documentation "Predicate that checks whether a link exists."))

(defmethod link-exists-p ((category-1 symbol) (category-2 symbol) (categorial-network categorial-network) &key link-type)
  "predicate for checking whether category is part of network"
  (second (multiple-value-list (graph-utils:edge-exists? (graph categorial-network) category-1 category-2 :edge-type link-type))))

(defmethod link-exists-p ((category-1 symbol) (category-2 symbol) (cxn-inventory fcg-construction-set) &key link-type)
  "predicate for checking whether category is part of network"
  (link-exists-p category-1 category-2 (categorial-network cxn-inventory) :link-type link-type))

(defmethod link-exists-p ((category-1 symbol) (category-2 symbol) (cxn-inventory hashed-fcg-construction-set) &key link-type)
  "predicate for checking whether category is part of network"
  (link-exists-p category-1 category-2 (categorial-network cxn-inventory) :link-type link-type))


;; Adding and removing categories and links ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric add-category (category thing &key recompute-transitive-closure)
  (:documentation "Adds a category to the categorial network."))

(defmethod add-category ((category symbol) (categorial-network categorial-network) &key (recompute-transitive-closure t))
  "Adds a category to the categorial network."
  (graph-utils:add-node (graph categorial-network) category)
  (when recompute-transitive-closure
    (compute-transitive-closure categorial-network))
  (notify category-added category categorial-network)
  categorial-network)

(defmethod add-category ((category symbol) (cxn-inventory fcg-construction-set) &key (recompute-transitive-closure t))
  "Adds a category to the categorial network of a grammar."
  (add-category category (categorial-network cxn-inventory) :recompute-transitive-closure recompute-transitive-closure))

(defmethod add-category ((category symbol) (cxn-inventory hashed-fcg-construction-set) &key (recompute-transitive-closure t))
  "Adds a category to the categorial network of a grammar."
  (add-category category (categorial-network cxn-inventory) :recompute-transitive-closure recompute-transitive-closure))


(defgeneric add-categories (categories thing &key recompute-transitive-closure))

(defmethod add-categories ((categories list) (categorial-network categorial-network) &key (recompute-transitive-closure nil))
  "Adds a list of categories to the categorial network of a grammar."
  (loop for category in categories
        do
        (add-category category categorial-network :recompute-transitive-closure nil)
        finally
        (when recompute-transitive-closure
          (compute-transitive-closure categorial-network))
        (return categorial-network)))

(defmethod add-categories ((categories list) (cxn-inventory fcg-construction-set) &key (recompute-transitive-closure nil))
  "Adds a list of categories to the categorial network of a grammar."
  (add-categories categories (categorial-network cxn-inventory) :recompute-transitive-closure recompute-transitive-closure))

(defmethod add-categories ((categories list) (cxn-inventory hashed-fcg-construction-set) &key (recompute-transitive-closure nil))
  "Adds a list of categories to the categorial network of a grammar."
  (add-categories categories (categorial-network cxn-inventory) :recompute-transitive-closure recompute-transitive-closure))


(defgeneric remove-category (category thing &key recompute-transitive-closure)
  (:documentation "Removes a category from the categorial network."))

(defmethod remove-category ((category symbol) (categorial-network categorial-network) &key (recompute-transitive-closure t))
  "Removes a category from the categorial network."
  (graph-utils:delete-node (graph categorial-network) category)
  (when recompute-transitive-closure
          (compute-transitive-closure categorial-network))
  (notify category-removed category categorial-network)
  categorial-network)

(defmethod remove-category ((category symbol) (cxn-inventory fcg-construction-set) &key (recompute-transitive-closure t))
  "Removes a category from the categorial network of a grammar."
  (remove-category category (categorial-network cxn-inventory) :recompute-transitive-closure recompute-transitive-closure))

(defmethod remove-category ((category symbol) (cxn-inventory hashed-fcg-construction-set) &key (recompute-transitive-closure t))
  "Removes a category from the categorial network of a grammar."
  (remove-category category (categorial-network cxn-inventory) :recompute-transitive-closure recompute-transitive-closure))


(defgeneric remove-categories (categories thing &key recompute-transitive-closure)
  (:documentation "Removes a list  of categories from the categorial network."))

(defmethod remove-categories ((categories list) (categorial-network categorial-network) &key (recompute-transitive-closure t))
  "Removes a list of categories from the categorial network of a grammar."
  (loop for category in categories
        do (remove-category category categorial-network :recompute-transitive-closure nil)
        finally
        (when recompute-transitive-closure
          (compute-transitive-closure categorial-network))
        (return categorial-network)))

(defmethod remove-categories ((categories list) (cxn-inventory fcg-construction-set) &key (recompute-transitive-closure t))
  "Removes a list of categories to the categorial network of a grammar."
  (remove-categories categories (categorial-network cxn-inventory) :recompute-transitive-closure recompute-transitive-closure))

(defmethod remove-categories ((categories list) (cxn-inventory hashed-fcg-construction-set) &key (recompute-transitive-closure t))
  "Removes a list of categories to the categorial network of a grammar."
  (remove-categories categories (categorial-network cxn-inventory) :recompute-transitive-closure recompute-transitive-closure))


(defgeneric add-link (category-1 category-2 categorial-network &key weight link-type recompute-transitive-closure))

(defmethod add-link ((category-1 symbol) (category-2 symbol) (categorial-network categorial-network) &key (weight 0.5) link-type (recompute-transitive-closure t))
  "add link to categorial-network"
  (unless (and (category-exists-p category-1 categorial-network)
               (category-exists-p category-2 categorial-network))
    (error (format nil "For adding a link to a categorial-network, both nodes need to be declared first. This was not the case for ~a."
                   (cond ((category-exists-p category-1 categorial-network)
                          (format nil "~a" category-2))
                         ((category-exists-p category-2 categorial-network)
                          (format nil "~a" category-1))
                         (t
                          (format nil "~a and ~a" category-1 category-2))))))
  (graph-utils:add-edge (graph categorial-network) category-1 category-2 :weight weight :edge-type link-type)
  (when recompute-transitive-closure
          (compute-transitive-closure categorial-network))
  (notify link-added category-1 category-2 categorial-network)
  categorial-network)

(defmethod add-link ((category-1 symbol) (category-2 symbol) (cxn-inventory fcg-construction-set) &key (weight 0.5) link-type (recompute-transitive-closure t))
  "Add link to categorial-network of cxn-inventory."
  (add-link category-1 category-2 (categorial-network cxn-inventory) :weight weight :link-type link-type :recompute-transitive-closure recompute-transitive-closure))

(defmethod add-link ((category-1 symbol) (category-2 symbol) (cxn-inventory hashed-fcg-construction-set) &key (weight 0.5) link-type (recompute-transitive-closure t))
  "Add link to categorial-network of cxn-inventory."
  (add-link category-1 category-2 (categorial-network cxn-inventory) :weight weight :link-type link-type :recompute-transitive-closure recompute-transitive-closure))


(defgeneric remove-link (category-1 category-2 categorial-network &key link-type recompute-transitive-closure)
  (:documentation "Remove link from categorial network."))

(defmethod remove-link ((category-1 symbol) (category-2 symbol) (categorial-network categorial-network) &key link-type (recompute-transitive-closure t))
  "Remove link from categorial-network"
  (unless (and (category-exists-p category-1 categorial-network)
               (category-exists-p category-2 categorial-network))
    (error (format nil "For removing a link from a categorial-network, both nodes need to be declared first. This was not the case for ~a."
                   (cond ((category-exists-p category-1 categorial-network)
                          (format nil "~a" category-2))
                         ((category-exists-p category-2 categorial-network)
                          (format nil "~a" category-1))
                         (t
                          (format nil "~a and ~a" category-1 category-2))))))
  (graph-utils:delete-edge (graph categorial-network) category-1 category-2 link-type)
  (when recompute-transitive-closure
          (compute-transitive-closure categorial-network))
  (notify link-removed category-1 category-2 categorial-network)
  categorial-network)

(defmethod remove-link ((category-1 symbol) (category-2 symbol) (cxn-inventory fcg-construction-set) &key link-type (recompute-transitive-closure t))
  "Remove link from categorial-network of cxn-inventory."
  (remove-link category-1 category-2 (categorial-network cxn-inventory) :link-type link-type :recompute-transitive-closure recompute-transitive-closure))

(defmethod remove-link ((category-1 symbol) (category-2 symbol) (cxn-inventory hashed-fcg-construction-set) &key link-type (recompute-transitive-closure t))
  "Remove link from categorial-network of cxn-inventory."
  (remove-link category-1 category-2 (categorial-network cxn-inventory) :link-type link-type :recompute-transitive-closure recompute-transitive-closure))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modifying the weights on the nodes ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric link-weight (category-1 category-2 thing &key link-type)
  (:documentation "Returns the weight of a link."))

(defmethod link-weight ((category-1 symbol) (category-2 symbol) (categorial-network categorial-network) &key link-type)
  "Returns the weight of a link."
  (unless (and (category-exists-p category-1 categorial-network)
               (category-exists-p category-2 categorial-network))
    (error (format nil "For querying the weight of a link, both nodes should be in the categorial network. This was not the case for ~a."
                   (cond ((category-exists-p category-1 categorial-network)
                          (format nil "~a" category-2))
                         ((category-exists-p category-2 categorial-network)
                          (format nil "~a" category-1))
                         (t
                          (format nil "~a and ~a" category-1 category-2))))))
  (graph-utils:edge-weight (graph categorial-network) category-1 category-2 link-type))

(defmethod link-weight ((category-1 symbol) (category-2 symbol) (cxn-inventory fcg-construction-set) &key link-type)
  "Returns the weight of a link."
  (link-weight category-1 category-2 (categorial-network cxn-inventory) :link-type link-type))

(defmethod link-weight ((category-1 symbol) (category-2 symbol) (cxn-inventory hashed-fcg-construction-set) &key link-type)
  "Returns the weight of a link."
  (link-weight category-1 category-2 (categorial-network cxn-inventory) :link-type link-type))


(defgeneric set-link-weight (category-1 category-2 thing weight &key link-type)
  (:documentation "Sets the weight of a link."))

(defmethod set-link-weight ((category-1 symbol) (category-2 symbol) (categorial-network categorial-network) (weight number) &key link-type)
  "Sets the weight of a link."
  (unless (typep weight 'float) (error "Weight should be of type FLOAT, not ~a." (type-of weight)))
  (unless (link-exists-p category-1 category-2 categorial-network :link-type link-type) (error "There is no link between ~a and ~a." category-1 category-2))
  (graph-utils:set-edge-weight (graph categorial-network) category-1 category-2 weight :edge-type link-type))

(defmethod set-link-weight ((category-1 symbol) (category-2 symbol) (cxn-inventory fcg-construction-set) (weight number) &key link-type)
  "Sets the weight of a link."
  (set-link-weight category-1 category-2 (categorial-network cxn-inventory) weight :link-type link-type))

(defmethod set-link-weight ((category-1 symbol) (category-2 symbol) (cxn-inventory hashed-fcg-construction-set) (weight number) &key link-type)
  "Sets the weight of a link."
  (set-link-weight category-1 category-2 (categorial-network cxn-inventory) weight :link-type link-type))


(defgeneric incf-link-weight (category-1 category-2 categorial-network &key delta link-type)
  (:documentation "Increments a link-weight."))

(defmethod incf-link-weight ((category-1 symbol) (category-2 symbol) (categorial-network categorial-network) &key (delta 0.1) link-type)
  "Increments a link-weight."
  (graph-utils:incf-edge-weight (graph categorial-network) category-1 category-2 :delta delta :edge-type link-type))

(defmethod incf-link-weight ((category-1 symbol) (category-2 symbol) (cxn-inventory fcg-construction-set) &key (delta 0.1) link-type)
  "Increments a link-weight."
  (incf-link-weight category-1 category-2 (categorial-network cxn-inventory) :delta delta :link-type link-type))

(defmethod incf-link-weight ((category-1 symbol) (category-2 symbol) (cxn-inventory hashed-fcg-construction-set) &key (delta 0.1) link-type)
  "Increments a link-weight."
  (incf-link-weight category-1 category-2 (categorial-network cxn-inventory) :delta delta :link-type link-type))


(defgeneric decf-link-weight (category-1 category-2 categorial-network &key delta link-type)
  (:documentation "Decrements a link-weight."))

(defmethod decf-link-weight ((category-1 symbol) (category-2 symbol) (categorial-network categorial-network) &key (delta 0.1) link-type)
  "Decrements a link-weight."
  (graph-utils:decf-edge-weight (graph categorial-network) category-1 category-2 :delta delta :edge-type link-type))

(defmethod decf-link-weight ((category-1 symbol) (category-2 symbol) (cxn-inventory fcg-construction-set) &key (delta 0.1) link-type)
  "Decrements a link-weight."
  (decf-link-weight category-1 category-2 (categorial-network cxn-inventory) :delta delta :link-type link-type))

(defmethod decf-link-weight ((category-1 symbol) (category-2 symbol) (cxn-inventory hashed-fcg-construction-set) &key (delta 0.1) link-type)
  "Decrements a link-weight."
  (decf-link-weight category-1 category-2 (categorial-network cxn-inventory) :delta delta :link-type link-type))


;; Checking connections between categories ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric neighbouring-categories (category thing &key link-type threshold)
  (:documentation "Lists all neighbouring categories of link-type above a link-weight threshold."))

(defmethod neighbouring-categories ((category symbol) (categorial-network categorial-network) &key link-type threshold)
  "Lists all neighbouring categories of link-type above a link-weight threshold (not including threshold)."
  (let ((neighbours (graph-utils:neighbors (graph categorial-network) category :return-ids? nil :edge-type link-type)))
    (if threshold
      (loop for neighbour in neighbours
            when (> (link-weight category neighbour categorial-network :link-type link-type) threshold)
            collect neighbour)
    neighbours)))

(defmethod neighbouring-categories ((category symbol) (cxn-inventory fcg-construction-set) &key link-type threshold)
  "Lists all neighbouring categories of link-type above a link-weight threshold."
  (neighbouring-categories category (categorial-network cxn-inventory) :link-type link-type :threshold threshold))

(defmethod neighbouring-categories ((category symbol) (cxn-inventory hashed-fcg-construction-set) &key link-type threshold)
  "Lists all neighbouring categories of link-type above a link-weight threshold."
  (neighbouring-categories category (categorial-network cxn-inventory) :link-type link-type :threshold threshold))


(defgeneric neighbouring-categories-p (category-1 category-2 thing &key link-type threshold)
  (:documentation "Checks whether two categories are neighbours above threshold."))

(defmethod neighbouring-categories-p ((category-1 symbol) (category-2 symbol) (categorial-network categorial-network) &key link-type threshold)
  "Checks whether two categories are neighbours above threshold."
  (let ((link-exists? (link-exists-p category-1 category-2 categorial-network :link-type link-type)))
    (cond ((and (not threshold)
                link-exists?))
          ((and threshold
                link-exists?
                (> (link-weight category-1 category-2 categorial-network :link-type link-type) threshold)))
          (t
           nil))))

(defmethod neighbouring-categories-p ((category-1 symbol) (category-2 symbol) (cxn-inventory fcg-construction-set) &key link-type threshold)
  "Checks whether two categories are neighbours above threshold."
  (neighbouring-categories-p category-1 category-2 (categorial-network cxn-inventory) :link-type link-type :threshold threshold))

(defmethod neighbouring-categories-p ((category-1 symbol) (category-2 symbol) (cxn-inventory hashed-fcg-construction-set) &key link-type threshold)
  "Checks whether two categories are neighbours above threshold."
  (neighbouring-categories-p category-1 category-2 (categorial-network cxn-inventory) :link-type link-type :threshold threshold))


(defgeneric connected-categories (category thing &key link-type threshold use-transitive-closure)
  (:documentation "Returns all categories that are connected to category with all links above threshold."))

(defmethod connected-categories ((category symbol) (categorial-network categorial-network) &key link-type threshold (use-transitive-closure t))
  "Returns all categories that are connected to category with all links above threshold."
  (cond ((and threshold use-transitive-closure)
         (warn "Either threshold or use-transitive-closure should be nil. Not using transitive closure")
         (connected-categories-aux category categorial-network link-type threshold nil))
        (use-transitive-closure
         (gethash link-type (gethash category (transitive-closure categorial-network))))
        (t
         (connected-categories-aux category categorial-network link-type threshold nil))))

(defmethod connected-categories ((category symbol) (cxn-inventory fcg-construction-set) &key link-type threshold  (use-transitive-closure t))
  "Returns all categories that are connected to category with all links above threshold."
  (connected-categories category (categorial-network cxn-inventory) :link-type link-type :threshold threshold :use-transitive-closure use-transitive-closure))

(defmethod connected-categories ((category symbol) (cxn-inventory hashed-fcg-construction-set) &key link-type threshold  (use-transitive-closure t))
  "Returns all categories that are connected to category with all links above threshold."
  (connected-categories category (categorial-network cxn-inventory) :link-type link-type :threshold threshold :use-transitive-closure use-transitive-closure))

(defun connected-categories-aux (category categorial-network link-type threshold visited-categories)
  "Returns all categories that are connected to category with all links above threshold."
  (let ((unvisited-neighbours (set-difference (neighbouring-categories category categorial-network :link-type link-type :threshold threshold)
                                              visited-categories)))
    (if unvisited-neighbours
      (loop for neighbour in unvisited-neighbours
            append (connected-categories-aux neighbour categorial-network link-type threshold (cons category visited-categories))
            into connected-categories
            finally (return (remove-duplicates connected-categories)))
      (cons category visited-categories))))
  

(defgeneric connected-categories-p (category-1 category-2 thing &key link-type threshold use-transitive-closure)
  (:documentation "Checks whether there is a path from category-1 to category-2 with all links above threshold."))

(defmethod connected-categories-p ((category-1 symbol) (category-2 symbol) (categorial-network categorial-network) &key link-type threshold (use-transitive-closure t))
  "Checks whether there is a path from category-1 to category-2 with all links above threshold."
  (cond ((and threshold use-transitive-closure)
         (warn "Either threshold or use-transitive-closure should be nil. Not using transitive closure")
         (connected-categories-p-aux category-1 category-2 categorial-network link-type threshold nil))
        (use-transitive-closure
         (find category-1 (gethash link-type (gethash category-2 (transitive-closure categorial-network)))))
        (t
         (connected-categories-p-aux category-1 category-2 categorial-network link-type threshold nil))))

(defmethod connected-categories-p ((category-1 symbol) (category-2 symbol) (cxn-inventory fcg-construction-set) &key link-type threshold (use-transitive-closure t))
  "Checks whether there is a path from category-1 to category-2 with all links above threshold."
  (connected-categories-p category-1 category-2 (categorial-network cxn-inventory) :link-type link-type :threshold threshold :use-transitive-closure use-transitive-closure))

(defmethod connected-categories-p ((category-1 symbol) (category-2 symbol) (cxn-inventory hashed-fcg-construction-set) &key link-type threshold (use-transitive-closure t))
  "Checks whether there is a path from category-1 to category-2 with all links above threshold."
  (connected-categories-p category-1 category-2 (categorial-network cxn-inventory) :link-type link-type :threshold threshold :use-transitive-closure use-transitive-closure))

(defun connected-categories-p-aux (category-1 category-2 categorial-network link-type threshold visited-categories)
  "Returns all categories that are connected to category with all links above threshold."
  (let ((unvisited-neighbours (set-difference (neighbouring-categories category-1 categorial-network :link-type link-type :threshold threshold)
                                              visited-categories)))
    (cond ((member category-2 unvisited-neighbours)
           t)
          (unvisited-neighbours
           (loop for neighbour in unvisited-neighbours
                 thereis (connected-categories-p-aux neighbour category-2 categorial-network link-type threshold (cons category-1 visited-categories))))
          (t
           nil))))


;; Methods for use by unify-atom ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(defgeneric categories-linked-p (category-1 category-2 thing mode)
  (:documentation "Used by unify-atom to check whether unification between to symbols should succeed."))

(defmethod categories-linked-p (category-1 category-2 (categorial-network categorial-network) (mode (eql :neighbours)))
  "Succeeds if categories are neighbours (= linked directly), including neighbours with weight 0."
  (neighbouring-categories-p category-1 category-2 categorial-network))

(defmethod categories-linked-p (category-1 category-2 (cxn-inventory fcg-construction-set) (mode (eql :neighbours)))
  "Succeeds if categories are neighbours (= linked directly), including neighbours with weight 0."
  (categories-linked-p category-1 category-2 (categorial-network cxn-inventory) mode))

(defmethod categories-linked-p (category-1 category-2 (cxn-inventory hashed-fcg-construction-set) (mode (eql :neighbours)))
  "Succeeds if categories are neighbours (= linked directly), including neighbours with weight 0."
  (categories-linked-p category-1 category-2 (categorial-network cxn-inventory) mode))

(defmethod categories-linked-p (category-1 category-2 (categorial-network categorial-network) (mode (eql :neighbours-w-weight-above-0)))
  "Succeeds if categories are neighbours (= linked directly), excluding neighbours with weight 0."
  (neighbouring-categories-p category-1 category-2 categorial-network :threshold 0.0))

(defmethod categories-linked-p (category-1 category-2 (cxn-inventory fcg-construction-set) (mode (eql :neighbours-w-weight-above-0)))
  "Succeeds if categories are neighbours (= linked directly), excluding neighbours with weight 0."
  (categories-linked-p category-1 category-2 (categorial-network cxn-inventory) mode))

(defmethod categories-linked-p (category-1 category-2 (cxn-inventory hashed-fcg-construction-set) (mode (eql :neighbours-w-weight-above-0)))
  "Succeeds if categories are neighbours (= linked directly), excluding neighbours with weight 0."
  (categories-linked-p category-1 category-2 (categorial-network cxn-inventory) mode))

(defmethod categories-linked-p (category-1 category-2 (categorial-network categorial-network) (mode (eql :path-exists)))
  "Succeeds if categories are connected (= linked directly or indirectly)."
  (connected-categories-p category-1 category-2 categorial-network))

(defmethod categories-linked-p (category-1 category-2 (cxn-inventory fcg-construction-set) (mode (eql :path-exists)))
  "Succeeds if categories are connected (= linked directly or indirectly)."
  (categories-linked-p category-1 category-2 (categorial-network cxn-inventory) mode))

(defmethod categories-linked-p (category-1 category-2 (cxn-inventory hashed-fcg-construction-set) (mode (eql :path-exists)))
  "Succeeds if categories are connected (= linked directly or indirectly)."
  (categories-linked-p category-1 category-2 (categorial-network cxn-inventory) mode))

(defmethod categories-linked-p (category-1 category-2 (categorial-network categorial-network) (mode (eql :path-exists-w-weight-above-0)))
  "Succeeds if categories are connected (= linked directly or indirectly) with all links above 0."
  (connected-categories-p category-1 category-2 categorial-network :threshold 0.0))

(defmethod categories-linked-p (category-1 category-2 (cxn-inventory fcg-construction-set) (mode (eql :path-exists-w-weight-above-0)))
  "Succeeds if categories are connected (= linked directly or indirectly) with all links above 0."
  (categories-linked-p category-1 category-2 (categorial-network cxn-inventory) mode))

(defmethod categories-linked-p (category-1 category-2 (cxn-inventory hashed-fcg-construction-set) (mode (eql :path-exists-w-weight-above-0)))
  "Succeeds if categories are connected (= linked directly or indirectly) with all links above 0."
  (categories-linked-p category-1 category-2 (categorial-network cxn-inventory) mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calculations based on categorial networks             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric nr-of-categories (thing)
  (:documentation "number of categories in categorial network"))

(defmethod nr-of-categories ((categorial-network categorial-network))
  "number of categories in categorial network"
  (graph-utils:node-count (graph categorial-network)))

(defmethod nr-of-categories ((cxn-inventory fcg-construction-set))
  "number of categories in categorial network"
  (nr-of-categories (categorial-network cxn-inventory)))

(defmethod nr-of-categories ((cxn-inventory hashed-fcg-construction-set))
  "number of categories in categorial network"
  (nr-of-categories (categorial-network cxn-inventory)))

(defgeneric nr-of-links (thing)
  (:documentation "number of links in categorial network"))

(defmethod nr-of-links ((categorial-network categorial-network))
  "number of links in categorial network"
  (graph-utils:edge-count (graph categorial-network)))

(defmethod nr-of-links ((cxn-inventory fcg-construction-set))
  "number of links in categorial network"
  (nr-of-links (categorial-network cxn-inventory)))

(defmethod nr-of-links ((cxn-inventory hashed-fcg-construction-set))
  "number of links in categorial network"
  (nr-of-links (categorial-network cxn-inventory)))


;; Computing the transitive closure ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
(defun compute-transitive-closure (categorial-network &key threshold)
  "Sets the transitive closure table of cxn-inventory."
  (loop with transitive-closure = (transitive-closure categorial-network)
        with link-types = (link-types categorial-network)
        for category in (categories categorial-network)
        for category-value = (make-hash-table)
        do
        (loop for link-type in link-types
              do (setf (gethash link-type category-value)
                       (connected-categories category categorial-network
                                             :link-type link-type
                                             :threshold threshold
                                             :use-transitive-closure nil)))
        (setf (gethash category transitive-closure) category-value)))
|#



(defun compute-transitive-closure (categorial-network)
  "Sets the transitive closure table of cxn-inventory
   using the Warshall algorithm."
  ;; NOTE: the transitive closure can contain self-loops.
  (labels (;; is there an edge from->to in the hash table?
           (traclo-connected-p (from to table)
             (loop with from-table = (gethash from table)
                   for link-type being the hash-keys in from-table
                   for categories = (gethash link-type from-table)
                   when (find to categories)
                   return (cons t link-type)))
           ;; add a link from->to in the hash table
           (add-traclo-link (from to link-type table)
             (unless (traclo-connected-p from to table)
               (push to (gethash link-type (gethash from table))))))
    (let ((traclo (make-hash-table)))
      ;; make a hash table for every category
      (loop for category in (categories categorial-network)
            do (setf (gethash category traclo) (make-hash-table)))
      ;; add the edges from the categorial network to the hash table
      (loop for (from to link-type) in (links categorial-network)
            do (add-traclo-link from to link-type traclo))
      ;; run the Warshall algorithm
      (loop for via in (categories categorial-network)
            do (loop for from in (categories categorial-network)
                     ; find edge from->via in hash table
                     for (from-via-connected . from-via-type)
                     = (traclo-connected-p from via traclo)
                     when from-via-connected
                     do (loop for to in (categories categorial-network)
                              ; find edge via->to in hash table
                              for (via-to-connected . via-to-type)
                              = (traclo-connected-p via to traclo)
                              when (and via-to-connected
                                        (eql from-via-type via-to-type))
                              ; when from->via and via->to exist
                              ; and the edge-types are the same
                              ; add from->to to the hash table
                              do (add-traclo-link from to via-to-type traclo))))
      ;; set the new traclo
      (setf (transitive-closure categorial-network) traclo))))

;; Jens 8/2/2022
;; Possible improvements regarding the transitive closure
;; 1. Only compute the transitive closure when absolutely necessary,
;;    i.e. when using 'connected-categories' or 'connected-categories-p'?
;; 2. Should self-loops be allowed in the transitive closure?
;;    The Warshall algorithm includes them now because the previous
;;    algorithm did, but could this cause problems?
;; 3. One of the connected modes for unify atom specifies :path-exists-w-weight-above-0.
;;    This could also be integrated in the transitive closure hash table.
;;    If we want the weights of the _shortest_ path, the Warshall algorithm
;;    could be expanded to the Floyd-Warshall algorithm (which has the same
;;    performance characteristics, i.e. O(|V|^3), where |V| is the number of vertices).
        