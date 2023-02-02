(in-package :ont-alignment)

(load-all-patches)
(use-package :SQL) ;;;;allows us not to add sql: prefix
(require "sqlite") 
(setf *default-database-type* :sqlite)
*default-database-type*

;*sqlite-library-path*
;contains the FLI shared library name for SQLite

(initialize-database-type :database-type :sqlite)
(connect "test_db")
(enable-sql-reader-syntax) ;;;allows us to use the brackets inside the queries

(create-table [test]
              '(([movie_name] (char 255) primary key)
                ([release_date] number not null)))

;(select * :from [test])
;(drop-table [test])

(query "INSERT INTO test (movie_name, release_date) VALUES ('The godfather', '1972')")
(query "SELECT * FROM test")
(query "SELECT movie_name FROM test WHERE release_date = 1972")


