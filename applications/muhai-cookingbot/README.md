# MUHAI Cookingbot

The MUHAI cookingbot is a system that can perform recipe execution tasks in simulation. Given a recipe in natural (human) language, it is able to understand the instructions of this recipe and take the appropriate actions to cook the intended dish. The actions are simulated using a qualitative simulation engine implemented in IRL.

The cookingbot has been developed by VUB and UNamur in the context of the European project [MUHAI](https://www.muhai.org) and received funding from the European Union’s Horizon 2020 research and innovation programme under grant agreement No 951846.

## Recipes

The example recipes that are provided as examples for the recipe execution benchmark can be found in the folder 'recipe-annotations', and include the following fully annotated recipes:

1. Afghan biscuits (annotation)
2. Almond crescent cookies, from SimplyRecipes.com (annotation + grammar)
3. Almond Crescent Cookies, from Food.com (annotation)
4. Almond Crescent Cookies, from AllRecipes.com (annotation)
5. Almond Crescent Cookies, from Cooks.com (annotation)
6. Almond Crescent Cookies, from TheSpruceEats.com (annotation)
7. Avocado Chicken Salad (annotation)
8. Basic Chicken Salad (annotation)
9. Best brownies (annotation)
10. Bisquick Shortcake Biscuits (annotation)
11. Black Bean and Corn Salad (annotation)
12. Black Bean and Mango Salad (annotation)
13. Black Bean Salad, from Food.com (annotation)
14. Black Bean Salad, from SimplyRecipes.com (annotation)
15. Broccoli Salad (annotation)
16. Chocolate Cream Cheese Cupcakes (annotation)
17. Chocolate fudge cookies (annotation)
18. Classic Greek Salad (annotation)
15. Classic Potato Salad (annotation)
20. Coconut Tuiles (annotation)
21. Cole Slaw (annotation)
22. Cranberry Fluff Salad (annotation)
23. Cucumber slices with dill (annotation)
24. Easy banana bread (annotation)
25. Easy cherry tomato corn salad (annotation)
26. Easy oatmeal cookies (annotation)
27. Mexican Wedding Cookies (annotation)
28. Tossed Salad With Homemade Croutons and Oil and Vinegar Dressing (annotation)
29. Vegan black bean and sweet potato salad (annotation)
30. Whole wheat ginger snaps (annotation)

## Primitives

The primitive operations that the cookingbot can actually handle, can be found in the benchmark documentation available in the folder 'benchmark/documentation'. Extensions to these primitives can be made by modifying the 'primitives.lisp' file. However, this might also require modifications in the 'ontology.lisp' file if new ingredients or tools are introduced. 

## Initial Kitchen State
The initial kitchen state that is used in the simulator can be modified in the 'environments.lisp' file. Modifications in 'ontology.lisp' might be required in order to introduce new ingredients or tools.