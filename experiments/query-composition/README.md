
# Query composition

This project is part of my master thesis for the University of Namur. This thesis is realized in the framework of an internship at the VUB.

Query-composition is a concept aiming at creating an autonomous agent which with a question asked can generate an associated query.


## Initialization of the database via postgresql

First of all, you need to install postgresql and especially PgAdmin via this link : [Getting started with postgresql](https://www.postgresqltutorial.com/postgresql-getting-started/) __a restart is required__


After the installation of postgresql, u will have an UI like this :
*INSERT IMAGE*

After, u can right click on "databases" and choose "create" => "database".
Now, an another UI are open and u can specify a name and one superuser attach to your new database. In my case, I name my DB __master_db__.

### Init the schema

in order to create the schema, I invite you to "right click" on the newly created database and choose "Query-tool" in the options.

you can copy and paste the entire contents of the script __init_script.sql__ located in the __postgres__ folder and press __F5__ to execute the script in PgAdmin.

# Code Documentation

## Objects

### Node-obj 

__Attributes__ 

* __depth:__ depth of the node
* __parent:__ parent of the node
* __children:__ List of nodes that are children
* __tble:__ table present after the FROM keyword
* __attributes-selected:__ the list of attributes chosen in the selection
* __ref-tbles:__ set of referred tables (useful for inner-join queries)
* __selection:__ selection part of the query
* __conditions:__ set of query conditions
* __attrs:__ the attributes used in the conditions (this is the stop condition of the tree)
* __time-result:__ execution time of the request
* __q:__ query

__Functions__

* __init-node:__ basic function to form a query of type *SELECT x FROM y*

* __join-node:__ function that generates queries of type *SELECT x FROM y INNER JOIN z ON z.id=y.id*

* __where-node:__ function adding the *WHERE* part to queries to form the *SELECT x FROM Y WHERE attribute operator value* type

* __condition-node:__ function that adds a condition to the query to form the type *SELECT x FROM y WHERE attribute operator valueA AND/OR attribute operator valueB*

### Query-composer-obj 

__Attributes__

* __queue:__ set of nodes to browse and extend
* __tree:__ generated tree
* __tables:__ set of tables in the database
* __queries:__ set of queries found by the algorithm

__Functions__

* __compose-query:__ main function that applies the Bread-first algorithm. Depending on the size of the traversed node, there are two different cases:
    * __Depth = 0:__ we are in the SELECT part and we call the __select-compose__ function
    * __Depth = 0:__ we are in the conditional part and we call the function __condition-compose__.

* __select-compose:__ This function generates the set of nodes of type *SELECT x FROM y*. For join cases the function __inner-outer-compose__ is called to generate them. In order to reduce the scope of the search two sorts can be performed. For the sorting of the tables the function __out-table__ is called while for the attributes a shortcut can be used if the number of elements that compose the answer is equal to the number of attributes that compose the table.

* __condition-compose:__ This function generates the set of nodes of type *SELECT x FROM y WHERE Condition*. Several loops are used to iterate over the set of referred tables, the set of attributes, the set of values for the attribute and the set of operators that can be applied to it. 
(This function takes most of the search time)

* This function generates the set of possible inner-joins according to the table provided in the selection. This represents the y in the formulation *SELECT x FROM y*.

* __sort-table:__ This function tests the first row of the answer in order to "deduce" the most relevant tables to use. To do this, a test is performed on each table. If an answer is present, this table is added to the sorted list. If no answer is found, all the tables are returned.

* This function returns all the selection possibilities for a given table in relation to the answer. This function does not allow selection among several tables (this is an idea for adding a search, however the scope will be much larger depending on the number of tables considered)

### Reference-info-obj 

__Attributes__

* __table-name:__ name of the referring table
* __column-name:__ column ID of the referring table
* __foreign-table:__ name of the referred table
* __foreign-column:__ column ID of the referred table

__Functions__

* __init-reference-info:__ function that instantiates the Reference-info-obj object

### Table-obj

__Attributes__

* __name:__ table name
* __attributes:__ list of attributes for this table

__Functions__

* __init-table:__ function that instantiates a table

* __init-schema:__ function that instantiates an object version of the connected database


### Tree-obj 

__Attributes__

* __nodes:__ set of generated nodes
* __root:__ root of the tree

__Functions__

//
### Attribute-obj

__Attributes__

* __name:__ attribute name
* __operators:__ list of operators usable by the attribute
* __type-att:__ type of attribute
* __constraint:__ constraint type of the attribute

__Functions__

* __define-type:__ function that defines the type of the attribute passed in parameter

* __define-constraint:__ function that defines the constrain of the attribute passed in parameter




## Documentation

[Documentation](https://linktodocumentation)

