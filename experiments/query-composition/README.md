
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


