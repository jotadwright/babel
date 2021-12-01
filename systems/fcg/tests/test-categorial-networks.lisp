(in-package :fcg)


(deftest categorial-networks ()
  (let ((categorial-network (make-instance 'categorial-network)))
    
    (add-category 'a categorial-network)
    (add-categories '(b c d e f g h i j k l m n) categorial-network)
    (remove-category 'n categorial-network)
    (remove-categories '(l m) categorial-network)
    
    (test-assert (category-exists-p 'a categorial-network))
    (test-assert (category-exists-p 'k categorial-network))
    (test-assert (not (category-exists-p 'n categorial-network)))
    (test-assert (not (category-exists-p 'l categorial-network)))

    (add-link 'a 'b categorial-network)
    (set-link-weight 'a 'b categorial-network 0.3)
    (add-link 'a 'd categorial-network)
    (add-link 'a 'f categorial-network)
    (set-link-weight 'a 'f categorial-network 0.2)
    (add-link 'b 'f categorial-network)
    (add-link 'b 'h categorial-network)
    (add-link 'h 'f categorial-network)

    (test-assert (link-exists-p 'b 'f categorial-network))
    (test-assert (not (link-exists-p 'b 'd categorial-network)))

    (test-assert (permutation-of? '(a b f d h) (connected-categories 'a categorial-network)))
    (test-assert (permutation-of? '(a b f d h) (connected-categories 'a categorial-network :threshold 0.2 :use-transitive-closure nil)))
    (test-assert (permutation-of? '(a d) (connected-categories 'a categorial-network :threshold 0.3 :use-transitive-closure nil)))
    (test-assert (not (connected-categories-p 'h 'd categorial-network :threshold 0.3 :use-transitive-closure nil)))
    (test-assert (connected-categories-p 'h 'd categorial-network :threshold 0.2 :use-transitive-closure nil))
    (test-assert (connected-categories-p 'h 'd categorial-network))
  ))

;; (categorial-networks)