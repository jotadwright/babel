(in-package :ont-alignment)

(defun connecting-to-db (db_name db)
  "Allows to connect to a database : can be use mutliple times if connecting to more than one database."
  (require "sqlite") 
  (setf *default-database-type* :sqlite)
  *default-database-type*
  (initialize-database-type :database-type :sqlite)
  (setf db_name (connect db))
  (format nil "The connection is established with the following database : ~d" db_name)
)

;(connecting-to-db "db1" "experiments/ont-alignment/dbs/db1_actors_films_multiple_tables.db")


;------------below this line, all that is not used in this way anymore, but that we want to keep just in case-------------;;

#|
(use-package :SQL)
*sqlite-library-path* contains the FLI shared library name for SQLite

(require "sqlite") 
(setf *default-database-type* :sqlite)
*default-database-type*
(initialize-database-type :database-type :sqlite)

(setf db1 (connect "experiments/ont-alignment/dbs/db1_actors_films_multiple_tables.db"))  ;if db doesn't exist, this creates db and stores it in current folder (babel)
(setf db2 (connect "experiments/ont-alignment/dbs/db2_actors_films_simple_table.db"))
(setf db3 (connect "experiments/ont-alignment/dbs/db3_films_years.db"))


(list-tables :database db1)

(query "SELECT * FROM films" :database db3) ;we use the :database keyword to specify which database we wish to query
(list-tables db1)
(connected-databases) ;returns a list of all connected databases
(database-name db1) ;returns the name of a database

(list-tables)
(table-exists-p "actorsfilms") is a predicate for determining whether or not a named table
(attribute-type attribute table) returns the type of a given attribute
(drop-table [test])
(list-attributes "actorsfilms") returns a list of strings naming every column (attribute) in a given table.
(enable-sql-reader-syntax) allows us to use the brackets inside the queries

(create-table [test]
              '(([movie_name] (char 255) primary key)
                ([release_date] number not null)))

(query "SELECT * FROM films")

(query "SELECT 'Actor :from actorsfilms WHERE Actor = Gérard Depardieu")

(query "SELECT film FROM films WHERE film_id IN (
SELECT film_id from actorfilms_relations WHERE actor_id = 
(SELECT actor_id FROM actors WHERE actor = 'Gérard Depardieu'))")

(SELECT [movie_name] :from [films] :WHERE [movie_name] [=] "The godfather") ;in the crochets a table_name or anything that is inside a table

(query "INSERT INTO test (movie_name, release_date) VALUES ('The godfather', '1972')")
(query "SELECT * FROM [films]")
(query "SELECT movie_name FROM [films] WHERE release_date = 1972")

|#


#|
(use-package :SQL) ;;;;allows us not to add sql: prefix
(require "sqlite") 
(setf *default-database-type* :sqlite)
*default-database-type*

;*sqlite-library-path*
;contains the FLI shared library name for SQLite

(initialize-database-type :database-type :sqlite)
|#