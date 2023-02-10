(in-package :ont-alignment)

(use-package :SQL) ;;;;allows us not to add sql: prefix
(require "sqlite") 
(setf *default-database-type* :sqlite)
*default-database-type*

;*sqlite-library-path*
;contains the FLI shared library name for SQLite

(initialize-database-type :database-type :sqlite)
(connect "experiments/ont-alignment/dbs/db1_actors_films_multiple_tables.db")  ;if db doesn't exist, creates db and stores it in current folder (babel)
(connect "experiments/ont-alignment/dbs/db2_actors_films_simple_table.db")

(list-tables)
(list-attributes "actorsfilms") ;returns a list of strings naming every column (attribute) in a given table / view.

(enable-sql-reader-syntax) ;;;allows us to use the brackets inside the queries

(create-table [test]
              '(([movie_name] (char 255) primary key)
                ([release_date] number not null)))

;(query "SELECT * FROM films")
(query "SELECT 'Actor :from actorsfilms WHERE Actor = Gérard")

(query "SELECT film FROM films WHERE film_id IN (
SELECT film_id from actorfilms_relations WHERE actor_id = 
(SELECT actor_id FROM actors WHERE actor = 'Gérard Depardieu'))")


(SELECT [movie_name] :from [films] :WHERE [movie_name] [=] "The godfather") ;in the crochets a table_name or anything that is inside a table
;(drop-table [test])

(query "INSERT INTO test (movie_name, release_date) VALUES ('The godfather', '1972')")
(query "SELECT * FROM [films]")
(query "SELECT movie_name FROM [films] WHERE release_date = 1972")

;;;; how to connect to multiple db

;if I need to connect to multiple databases at the same time:
(connect :database database) ;;;giving it the name of the databases
;;; so we can connect to multiple databases

;(list-tables) returns a list of strings naming every table and view in the database.
;(table-exists-p table) is a predicate for determining whether or not a named table / view exists.
;(attribute-type attribute table) returns the type of a given attribute.


