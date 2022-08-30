# MUHAI Cookingbot

The MUHAI cookingbot is a system that can perform recipe execution tasks in simulation. Given a recipe in natural (human) language, it is able to understand the instructions of this recipe and take the appropriate actions to cook the intended dish. The actions are simulated using a qualitative simulation engine implemented in IRL.

The cookingbot has been developed by VUB and UNamur in the context of the European project [MUHAI](https://www.muhai.org) and received funding from the European Union’s Horizon 2020 research and innovation programme under grant agreement No 951846.


## Recipes

The example recipes that are provided as examples for the recipe execution benchmark can be found in the folder 'recipe-annotations', and include the following fully annotated recipes:

1. Almond crescent cookies (annotation + grammar)
2. Afghan biscuits (annotation)
3. Best brownies (annotation)
4. Chocolate fudge cookies (annotation)
5. Easy banana bread
6. Easy bread pudding
7. Easy oatmeal cookies
8. Whole wheat ginger snaps
9. Cucumber slices with dill
10. Easy cherry tomato corn salad
11. Vegan black bean and sweet potato salad


## Primitives

The primitive operations that the cookingbot can actually handle, are listed here below. The number of arguments that a primitive takes is indicated between brackets behind its name.

#### **get-kitchen** (1)
- ks-out

#### **fetch-and-proportion** (7)
  - proportioned-ingredient
  - ks-out
  - ks-in
  - container
  - concept
  - amount
  - unit

#### **fetch** (5)
  - fetched-thing
  - ks-out
  - ks-in
  - concept-to-fetch-instance-of
  - quantity

#### **bring-to-temperature** (6)
  - ingredient-out
  - ks-out
  - ks-in
  - ingredient-in
  - amount
  - unit

#### **transfer-contents** (8)
  - container-out
  - container-rest
  - ks-out
  - ks-in
  - empty-container
  - container-in
  - amount
  - unit

#### **beat** (5)
  - container-out
  - ks-out
  - ks-in
  - container-in
  - tool

#### **mix** (5)
  - container-out
  - ks-out
  - ks-in
  - container-in
  - tool

#### **portion-and-arrange** (8)
  - portions
  - ks-out
  - ks-in
  - dough-to-be-portioned
  - portion-amount
  - portion-unit
  - arrangement-pattern
  - destination

#### **shape** (5)
  - shaped-portions
  - ks-out
  - ks-in
  - portions
  - shape

#### **line** (5)
  - lined-container
  - ks-out
  - ks-in
  - container-to-line (can be concept)
  - material-to-line-container-with (can be concept)

####  **transfer-items** (5)
  - items-at-destination
  - ks-out
  - ks-in
  - items-at-source
  - destination

#### **bake** (9)
  - thing-baked
  - ks-out
  - ks-in
  - thing-to-bake
  - oven-to-bake-in
  - time-to-bake-quantity
  - time-to-bake-unit
  - target-temperature-quantity
  - target-temperature-unit

#### **sprinkle** (5)
  - thing-sprinkled
  - ks-out
  - ks-in
  - thing-to-sprinkle
  - container-with-topping

*(end of recipe 1)*

#### **preheat-oven** (5)
  - preheated-oven
  - ks-out
  - ks-in
  - temperature-quantity
  - temperature-unit

#### **sift** (5)
  - container-with-sifted-contents
  - ks-out
  - ks-in
  - target-container
  - container-with-ingredients-to-be-sifted
  - tool

#### **flatten** (5)
  - flattened-dough
  - ks-out
  - ks-in
  - dough-to-flatten (can be set of items)
  - tool

#### **spread** (6)
  - thing-with-spread-on-top
  - ks-out
  - ks-in
  - thing-to-be-spread
  - spread
  - tool

*(end of recipe 2)*

#### **melt** (4)
  - ingredient-melted
  - ks-out
  - ks-in
  - ingredient-to-melt

#### **grease** (5)
  - greased-container
  - ks-out
  - ks-in
  - container-to-grease
  - grease

#### **flour** (5)
  - floured-container
  - ks-out
  - ks-in
  - container-to-flour
  - flour

#### **crack** (5)
  - container-with-cracked-eggs
  - ks-out
  - ks-in
  - container-with-eggs
  - destination-container

#### **cut** (6)
  - container-w-cut-object
  - ks-out
  - ks-in
  - container-w-object-to-cut
  - pattern
  - tool

*(end of recipe 3)*

#### **mash** (5)
  - container-w-mashed-ingredient
  - ks-out
  - ks-in
  - container-w-ingredient-to-mash
  - tool

*(end of recipe 4)*
