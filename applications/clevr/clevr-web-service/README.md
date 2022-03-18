# CLEVR Web Service

The CLEVR Web Service defines a RESTful API that gives access to the CLEVR Grammar.

## API Documentation

### Comprehend

 - **URL:** https://penelope.vub.be/clevr-api/comprehend
 - **Method:** POST
 - **Headers:**
 	- Content-Type: application/json 
 - **URL Params:** None
 - **Data Params:** 
 	- Required: 
 		- `utterance: "string"`
   - Optional:
 		- `irl_encoding: "string"`
 - **Success Response:**
 	- Code: 200
 	
 ```
 {
     "meaning": "string"/{object},
     "fcg_status": "string",
     "applied_constructions": ["string"]
 }
 ```
 	  
 - **Error Response:**
  	- Code: 400

   ```
{
     "status_code": 400
     "details": error message
}
   ```

   - Code: 500

   ```
{
     "status_code": 500
     "details": "Error in language processing module"
}
   ```   

### Comprehend All

 - **URL:** https://penelope.vub.be/clevr-api/comprehend-all
 - **Method:** POST
 - **Headers:**
 	- Content-Type: application/json 
 - **URL Params:** None
 - **Data Params:** 
 	- Required: 
 		- `utterance: "string"`
   - Optional:
 		- `irl_encoding: "string"`
 		- `n: integer`
 - **Success Response:**
 	- Code: 200
 	
 ```
 [
     {
      "meaning": "string"/{object},
      "fcg_status": "string",
      "applied_constructions": ["string"]
     },
     {
      "meaning": "string"/{object},
      "fcg_status": "string",
      "applied_constructions": ["string"]
     }
 ]
 ```
 	  
 - **Error Response:**
  	- Code: 400

   ```
{
     "status_code": 400
     "details": error message
}
   ```

   - Code: 500

   ```
{
     "status_code": 500
     "details": "Error in language processing module"
}
   ```  

### Formulate

 - **URL:** https://penelope.vub.be/clevr-api/formulate
 - **Method:** POST
 - **Headers:**
 	- Content-Type: application/json 
 - **URL Params:** None
 - **Data Params:** 
 	- Required: 
 		- `meaning: "string"/{object}`
   - Optional:
 		- `irl_encoding: "string"`
 - **Success Response:**
 	- Code: 200
 	
 ```
 {
     "utterance": "string",
     "fcg_status": "string",
     "applied_constructions": ["string"]
 }
 ```
 	  
 - **Error Response:**
  	- Code: 400

   ```
{
     "status_code": 400
     "details": error message
}
   ```

   - Code: 500

   ```
{
     "status_code": 500
     "details": "Error in language processing module"
}
   ```   

### Formulate All

 - **URL:** https://penelope.vub.be/clevr-api/formulate-all
 - **Method:** POST
 - **Headers:**
 	- Content-Type: application/json 
 - **URL Params:** None
 - **Data Params:** 
 	- Required: 
 		- `meaning: "string"/{object}`
   - Optional:
 		- `irl_encoding: "string"`
 		- `n: integer`
 - **Success Response:**
 	- Code: 200
 	
 ```
 [
     {
      "utterance": "string",
      "fcg_status": "string",
      "applied_constructions": ["string"]
     },
     {
      "utterance": "string",
      "fcg_status": "string",
      "applied_constructions": ["string"]
     }
 ]
 ```
 	  
 - **Error Response:**
  	- Code: 400

   ```
{
     "status_code": 400
     "details": error message
}
   ```

   - Code: 500

   ```
{
     "status_code": 500
     "details": "Error in language processing module"
}
   ```  

### Comprehend And Formulate

 - **URL:** https://penelope.vub.be/clevr-api/comprehend-and-formulate
 - **Method:** POST
 - **Headers:**
 	- Content-Type: application/json 
 - **URL Params:** None
 - **Data Params:** 
 	- Required: 
 		- `utterance: "string"`
 - **Success Response:**
 	- Code: 200
 	
 ```
 {
     "utterance": "string",
     "fcg_status": "string",
     "applied_constructions": ["string"]
 }
 ```
 	  
 - **Error Response:**
  	- Code: 400

   ```
{
     "status_code": 400
     "details": error message
}
   ```

   - Code: 500

   ```
{
     "status_code": 500
     "details": "Error in language processing module"
}
   ```  

### Comprehend And Execute

 - **URL:** https://penelope.vub.be/clevr-api/comprehend-and-execute
 - **Method:** POST
 - **Headers:**
 	- Content-Type: application/json 
 - **URL Params:** None
 - **Data Params:** 
 	- Required: 
 		- `utterance: "string"`
 		- `scene: "string"`
   - Optional:
 		- `irl_encoding: "string"`
 - **Success Response:**
 	- Code: 200
 	
 ```
 {
     "meaning": "string"/{object},
     "fcg_status": "string"
     "applied_constructions": ["string"],
     "irl_status": "string",
     "solutions": ["string"]
 }
 ```
 	  
 - **Error Response:**
  	- Code: 400

   ```
{
     "status_code": 400
     "details": error message
}
   ```

   - Code: 500

   ```
{
     "status_code": 500
     "details": error message
}
   ```   
   
## IRL Encoding

Each predicate of the IRL network is a JSON object:

```
{
 "name": "filter",
 "arity": 1,
 "arg": "red",
 "output": integer/"string"/{object}/[{object}]
}
```

The `output` key contains the intermediate result after having executed the predicate. This key can contain a list of objects, a single object, a string or an integer.

A CLEVR object using the following keys:

```
{
 "color": "string",
 "shape": "string",
 "material": "string",
 "size": "string",
 "id": "string"
}
```

Note that order of the predicates is in reverse polish notation. Below, we show two examples of this ordering:

**IRL Program**:

```
((get-context ?context)
 (filter ?set-1 ?context ?shape-1)
 (unique ?obj-1 ?set-2)
 (exist ?target ?obj-1)
 (bind shape-category ?shape-1 cube))
```

**RPN Encoding:** `((get-context)(filter cube)(unique)(exist))`

**IRL Program:**

```
((get-context ?context)
 (filter ?set-1 ?context ?shape-1)
 (filter ?set-2 ?set-1 ?color-1)
 (filter ?set-3 ?context ?shape-2)
 (union! ?u-set ?set-2 ?set-3)
 (count! ?target ?u-set)
 (bind shape-category ?shape-1 cube)
 (bind color-category ?color-1 red)
 (bind shape-category ?shape-2 sphere))
```

**RPN Encoding:**

```
((get-context)
 (filter red)
 (filter cube)
 (get-context)
 (filter sphere)
 (union!)
 (count!))
```