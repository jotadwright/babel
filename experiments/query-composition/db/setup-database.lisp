(in-package  :qc)

(defun continent-data ()
  (let ((data '(("Africa" 1393676444 30370000 46) ("Asia" 4694576167 44579000 100)("Europe" 745173774 10180000 73)("North America" 491921432 24930000 20)("Oceania" 44491724 8525990 4)("South America" 415897337 17840000 23))))
    (execute "DROP TABLE IF EXISTS continent CASCADE")
    (execute (dao-table-definition 'continent))
    ;;insert data into Database
    (query (:insert-rows-into 'continent :columns 'name 'population 'size 'density
            :values data))))

(defun country-data ()
  (let ((data-without-id '())
        (data '())
        (countries-info(cl-csv:read-csv #P "./experiments/query-composition/db/archive/country_informations.csv"))
        (country-continent (cl-csv:read-csv #P "./experiments/query-composition/db/archive/continentCountry.csv")))
    (dolist (c-i countries-info)
      (dolist (c-c country-continent)
        (if (equal (first c-i) (second c-c))
         (push (push (first c-c) c-i) data-without-id))))
    (dolist (row data-without-id)
      (let ((id-continent (flatten (query (concatenate 'string  "SELECT id FROM continent WHERE name='"(first row)"'")))))
        (push (substitute (first id-continent) (first row) row) data)))
    ;;Drop and create table
    (execute "DROP TABLE IF EXISTS country CASCADE")
    (execute (dao-table-definition 'country))
    ;;insert data into Database
    (query (:insert-rows-into 'country :columns 'continentid 'name 'population 'density 'size
            :values data))))

(defun city-data ()
  (let ((data '())
        (id-countries (flatten (query "SELECT id FROM country ORDER BY id ASC"))))
    (dotimes (i 50)
      (let ((row '()))
        (push (concatenate 'string "city" (write-to-string i)) row)
        (push (random-between 300 8000000) row)
        (push (random-between 500 10000) row)
        (push nil row)
        (push nil row)
        (push (random-between (first id-countries) (first (last id-countries))) row)
        (push row data)))
    ;;Drop and create table
    (execute "DROP TABLE IF EXISTS city CASCADE")
    (execute (dao-table-definition 'city))
    ;;insert data into Database
    (query (:insert-rows-into 'city :columns 'countryid 'iscapital 'isprimary 'size 'population 'name
            :values data))))

(defun road-data ()
  (let ((data '())
        (id-countries (flatten (query "SELECT id FROM country ORDER BY id ASC"))))
    (dotimes (i 50)
      (let ((row '()))
        (push (concatenate 'string "road" (write-to-string i)) row)
        (push (random-between 15 5000) row)
        (push (random-between 5 130) row)
        (push (random-between (first id-countries) (first (last id-countries))) row)
        (push row data)))
    ;;Drop and create table
    (execute "DROP TABLE IF EXISTS road CASCADE")
    (execute (dao-table-definition 'road))
    ;;insert data into Database
    (query (:insert-rows-into 'road :columns 'countryid 'speedaverage 'size 'name
            :values data))))

(defun river-data ()
  (let ((data '()))
    (dotimes (i 50)
      (let ((row '()))
        (push (concatenate 'string "river" (write-to-string i)) row)
        (push (random-between 5 4000) row)
        (push (random-between 500 3000) row)
        (push row data)))
    ;;Drop and create table
    (execute "DROP TABLE IF EXISTS river")
    (execute (dao-table-definition 'river))
    ;;insert data into Database
    (query (:insert-rows-into 'river :columns 'flow 'size 'name
            :values data))))

;;DEBUG NOT WORK
(defun country-river-data ()
  (let ((data '())
        (id-countries (flatten (query "SELECT id FROM country ORDER BY id ASC")))
        (id-rivers (flatten (query "SELECT id FROM river ORDER BY id ASC"))))
    (dotimes (i 20)
      (let ((row '()))
        (push (random-between (first id-countries) (first (last id-countries))) row)
        (push (random-between (first id-rivers) (first (last id-rivers))) row)
        (push row data)))
    ;;Drop duplicates
    (delete-duplicates data)
    ;;Drop and create table
    (execute "DROP TABLE IF EXISTS country_river")
    (execute (dao-table-definition 'country-river))
    ;;insert data into Database
    (query (:insert-rows-into 'country-river :columns 'riverid 'countryid
            :values data))))
        

(defun setup-database (&key dbname username password hostname)
  (connect-toplevel dbname username password hostname)
  (continent-data)
  (country-data)
  (city-data)
  (road-data)
  (river-data)
  (disconnect-toplevel))





