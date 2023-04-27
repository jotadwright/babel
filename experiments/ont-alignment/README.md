# Ontology alignment package

## What is it? 

This package contains elements to lead an experiment regarding ontology alignement on a language level. It uses a population of 3 agents, each having access to its own database. The aim for them is :
1) to learn a query language (at the moment, this query language is SQL)
2) to build an emergent language allowing them to exchange informations from their respective database
This exchange of informations can therefore take place without the need of an ontology alignement per se. The alignment exists in the emergent language allowing the agents to communicate and share informations from databases that don't necessarily have the same structure nor the same query language. 

## SQL predicates

### What are they?

The SQL predicates are based on the SQL query commands as used in Postmodern. Postmodern is a lisp library allowing the use of Postgresql in a lisp code. The following SQL predicates don't include the non-consensual commands (commands that PostgreSQL may use but not all SQL systems do).

### The predicates and their arguments
The following tables lists all the SQL query commands that consensually exist. Each SQL query command has a predicate equivalent. For each of these predicates, we list their possible arguments. Note that not all the arguments need to be bound. Only bound argument will be relevant in a given query. Some arguments have a -x suffix. When they do, it means that we can find multiple arguments of the same type or that call for the same predicate or type of predicate in a query.

| PREDICATE | PREDICATE ARGUMENTS | SQL command equivalent | Use |
|---|---|---|---|
| select | ?result : what goes out of the select predicate which is the main one ?aggregate-clause-1 : calls for an aggregation predicate such as "count".  To see how we can use multiple aggregation-clause, see the predicate "and' ?distinct-clause-1 : calls for a predicate called "distinct" ?column-1  : needs to be bound to a column name (thanks to the bind predicate) ?table-1 : needs to be bound to a table name (thanks to the bind predicate) ?join-clause-1 : calls for a join predicate (inner-join, full-join, left-join,  right-join, cross-join) ?where-clause : calls for a "where" predicate ?group-by-clause : calls for a "group-by" predicate | SELECT…FROM | Used to select columns from one or more tables.  We make of those two SQL commands one predicate as  they are intrinsically linked. |
| where | ?where-clause : this argument allows the "where" predicate to be linked to the  "select" predicate. ?filter-condition-1 : a filter condition is the restriction we are going to  impose in the query to filter the results.  ?filter-combination-1 : this argument allows the "where" predicate to call for  multiple filter-conditions. This filter-combination calls for an "and"/"or" predicate.  | WHERE | used to filter results based on conditions |
| group-by | ?group-by-clause : argument linking the "select" predicate to the "group-by predicate" ?column-x : the first (or only) column on which the "group-by" predicate affects ?having-clause : argument linking the "group-by" predicate to the possible "having"  predicate | GROUP BY | The GROUP BY clause is used to group together  those rows in a table that have the same values  in all the columns listed. The order in which the  columns are listed does not matter. The effect is  to combine each set of rows having common values into  one group row that represents all rows in the group.  This is done to eliminate redundancy in the output  and/or compute aggregates that apply to these groups. |
| having | ?having-clause : this argument allows the having predicate to be linked to the  "group-by" predicate ?filter-condition-1 : a filter condition is the restriction we are going to  impose in the query to filter the results.  ?filter-combination-1 : this argument allows the "having" predicate to call for  multiple filter-conditions. This filter-combination calls for an "and"/"or" predicate.  | HAVING | If a table has been grouped using GROUP BY, the  HAVING clause can be used, much like a WHERE clause  but with aggregate functions, to eliminate groups from the result. |
| count |  ?aggregate-clause-1 : allows the "count" predicate to be linked to the "select"  predicate or to a filter linked to the "having" predicate ?column-2 : the column on which the aggregator applies. The argument needs to  be bound to a column name. It is already the column number 2 because the first  one is part of the "select" predicate and can't be bound when we are using the  "count" predicate.  | COUNT | used to count the number of rows in a result set or  the number of occurrences of a specific value |
| round | ?aggregate-clause-1 : allows the aggregator to be bound to the "select" predicate  or to a filter linked to the "having" predicate ?column-2 : the column on which the aggregator applies. The argument needs to be  bound to a column name. | ROUND | used to round a numeric value to a specified number  of decimal places |
| average | idem | AVG | used to calculate the average of a set of values |
| max | idem | MAX | used to find the maximum value in a set of values |
| min | idem | MIN | used to find the minimum value in a set of values |
| sum | idem | SUM | used to calculate the sum of a set of values |
| not | ?filter-condition-x : links the "not" predicate to a "where" or "having" predicate,  or a filter-combination ?filter-condition-x+1 : the filter-condition the "not" predicate applies on | NOT | used  to neggate a condition |
| in | ?filter-condition-x :  links the "in" predicate to a "where" or "having" predicate,  or a filter-combination ?column-y : the column we are talking about ?set-clause-z : a set of values in which we are searching | IN | used to match a column to a list of possible values |
| like | ?filter-condition-x : links the "like" predicate to a "where" or "having" predicate,  or a filter-combination ?column-y : the column we are talking about ?comparator-z : a regex bound to the type "concept" | LIKE | used to match a value based on a pattern |
| between | ?filter-condition-x : links the "between" predicate to a "where" or "having" predicate,  or a filter-combination ?column-y : a column of type int ?lower-bound : a number used as lower bound (needs to be bound) ?higher-bound : a number used as higher bound (needs to be bound) | BETWEEN…AND | used to match a value within a range of values |
| inner-join | ?inner-join-clause-x : links the "inner-join" predicate to the "select" predicate or  the another "inner-join" predicate ?table-y : the table the join applies on ?column-z : a column extracted from the first table ?column-z+1 : an equivalent column in the second table ?inner-join-clause-x+1 : eventually links to another "inner-join" predicate | INNER JOIN…ON |  used to combine rows from two or more tables based on a  matching condition. Only retrieves matching rows |
| full-join | idem | FULL JOIN…ON | used to combine rows from two or more tables based on a  matching condition. Retrieves matching rows plus extra  rows from left and right table |
| right-join | idem | RIGHT JOIN…ON | used to combine rows from two or more tables based on a  matching condition. Retrieves selected rows plus rows on right table |
| left-join | idem | LEFT JOIN…ON | used to combine rows from two or more tables based on a  matching condition. Retrieves selected rows plus rows on left table |
| and | ?filter-combination-x : links the "and" predicate to a "where" or a "having" predicate ?filter-combination-x+1 : eventually calls for another combinatory predicate ("and"/"or")  as a left member ?filter-combination- x+2 :  eventually calls for another combinatory predicate ("and"/"or")  as a right member ?filter-condition-y : eventually calls for a filter as a member (left if first member,  right if second member) ?filter-condition-y+2 : eventually calls for a filter as a right member (the first member  can therefor be a filter-combination or a filter-condition) | AND | used to combine conditions with a logical AND operator |
| or | works as the "and" predicate | OR | used to combine conditions with a logical OR operator. |
| equals | ?filter-condition-x : links the operator to a "where" or "having" predicate, or a  filter-combination ?column-y : the column we are talking about ?comparator-z : most of the time a number, except for the "equals" predicate that  accepts strings | operator = | verifies equality |
| lower-than | idem | operator < | verifies if a value is lower than another |
| greater-than | idem | operator > | verifies if a value is greater than |
| lower-equal-than | idem | operator <= | verifies if a value is lower or equal to another |
| greater-equal-than | idem | operator >= | verifies if a value is greater or equal to another |
| bind | column : the type of the second argument ?column-x : the column we need to bind  a : the name of the column |  | (binds a ?column-x argument of type "column" to a column name |
| dot | ?column-x : what goes out of the predicate ?table-y : the table the column refers to  ?column-x+1 : a column we need to bind a column name |  | in case of join, the column are written as table-name.column-name ;  the "dot" predicate links a column to a table |
| comma | ?column-combination-x : what goes out of the "comma" predicate, can be used to  link the column combination to another column thanks to another "comma" predicate ?distinct-clause-y / ?column-y : eventually calls for a "distinct" predicate or  a column name ?distinct-clause-z / ?column-z : idem |  | when multiple columns are selected, they are separated by commas.  The predicate "comma" allows to add a column after another.  |

### Examples


