DROP TABLE IF EXISTS continent ;

CREATE TABLE IF NOT EXISTS continent (
  id INT NOT NULL PRIMARY KEY generated always as identity,
  name VARCHAR(50) NOT NULL,
  population VARCHAR(45) NOT NULL,
  size FLOAT NOT NULL,
  density INT NOT NULL);

DROP TABLE IF EXISTS country ;

CREATE TABLE IF NOT EXISTS country (
  id INT NOT NULL PRIMARY KEY generated always as identity,
  name VARCHAR(100) NOT NULL,
  continentId INT NOT NULL,
  population INT NOT NULL,
  size FLOAT NOT NULL,
  isPrimary BOOL NOT NULL,
  density INT NOT NULL,
  CONSTRAINT continent_country_FK
    FOREIGN KEY (continentId)
    REFERENCES continent (id));


DROP TABLE IF EXISTS city ;

CREATE TABLE IF NOT EXISTS city (
  id INT NOT NULL PRIMARY KEY generated always as identity,
  name VARCHAR(50) NOT NULL,
  population VARCHAR(45) NOT NULL,
  size FLOAT NOT NULL,
  countryId INT NOT NULL,
  isPrimary BOOL NOT NULL,
  isCapital BOOL NOT NULL,
  CONSTRAINT country_city_FK
    FOREIGN KEY (countryId)
    REFERENCES country (id));

DROP TABLE IF EXISTS river ;

CREATE TABLE IF NOT EXISTS river (
  id INT NOT NULL PRIMARY KEY generated always as identity,
  name VARCHAR(100) NOT NULL,
  size FLOAT NOT NULL,
  flow INT NULL);

DROP TABLE IF EXISTS road ;

CREATE TABLE IF NOT EXISTS road (
  id INT NOT NULL PRIMARY KEY generated always as identity,
  name VARCHAR(100) NOT NULL,
  size FLOAT NOT NULL,
  speedAverage INT NOT NULL,
  countryId INT NOT NULL,
  CONSTRAINT country_road_FK
    FOREIGN KEY (countryId)
    REFERENCES country (id));


DROP TABLE IF EXISTS country_river ;

CREATE TABLE IF NOT EXISTS country_river (
  countryId INT NOT NULL,
  riverId INT NOT NULL,
  PRIMARY KEY (countryId, riverId),
  CONSTRAINT country_country_river_FK
    FOREIGN KEY (countryId)
    REFERENCES country (id)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT country_river_river_FK
    FOREIGN KEY (riverId)
    REFERENCES river (id));

DROP TABLE IF EXISTS question ;

CREATE TABLE IF NOT EXISTS question (
  id INT NOT NULL PRIMARY KEY generated always as identity,
  question TEXT NOT NULL,
  query TEXT NOT NULL,
  response TEXT NOT NULL);
