#demo-env
(get-kitchen ?ks-1)

(fetch-and-proportion ?out-1 ?ks-2 ?ks-1 ?target-container-1 butter 226 g)
(bring-to-temperature ?out-2 ?ks-3 ?ks-2 ?out-1 18 degrees-celsius)

(fetch-and-proportion ?out-3 ?ks-4 ?ks-3 ?target-container-2 white-sugar 116 g)
(fetch-and-proportion ?out-4 ?ks-5 ?ks-4 ?target-container-3 all-purpose-flour 340 g)

(fetch ?medium-bowl ?ks-6 ?ks-5 medium-bowl 1)

(transfer-contents ?out-x ?rest-x ?ks-out-x ?ks-6 ?medium-bowl ?out-2 ?quantity-x ?unit-x)
(transfer-contents ?out-y ?rest-y ?ks-out-y ?ks-out-x ?out-x ?out-3 ?quantity-y ?unit-y)
(transfer-contents ?out-z ?rest-z ?ks-out-z ?ks-out-y ?out-y ?out-4 ?quantity-z ?unit-z)

(mix ?mixed-bowl ?ks-mixed ?ks-out-z ?out-z ?tool)

(bake ?thing-baked ?ks-baked ?ks-mixed ?mixed-bowl ?oven-to-bake-in 15 minute 175 degrees-celsius)
