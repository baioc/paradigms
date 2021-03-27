DROP TABLE crew_assignments;
DROP TABLE pilot_assignments;
DROP TABLE ship_assignments;
DROP TABLE crew_specialties;
DROP TABLE mission_targets;
DROP TABLE pilot_trainings;
DROP TABLE combat_ships;
DROP TABLE transport_ships;
DROP TABLE crews;
DROP INDEX mission_calendar;
DROP TABLE missions;
DROP TABLE mission_kinds;
DROP TABLE planets;
DROP TABLE terrain_kinds;
DROP TABLE galaxies;
DROP TABLE pilots;
DROP TABLE ships;

CREATE TABLE ships (id integer GENERATED AS IDENTITY PRIMARY KEY,
    max_speed real NOT NULL CHECK (max_speed > 0),
    action_radius real NOT NULL CHECK (action_radius > 0),
    required_pilots integer NOT NULL CHECK (required_pilots >= 0));

CREATE TABLE pilots (id integer GENERATED AS IDENTITY PRIMARY KEY,
    name varchar(255) NOT NULL,
    patent varchar(255) NOT NULL,
    birth date NOT NULL);

CREATE TABLE galaxies (id integer GENERATED AS IDENTITY PRIMARY KEY,
    name varchar(255) NOT NULL,
    distance real NOT NULL CHECK (distance >= 0));

CREATE TABLE terrain_kinds (kind varchar(10) PRIMARY KEY,
    CHECK (kind in ('acid', 'neutral', 'basic')));
INSERT INTO terrain_kinds VALUES ('acid');
INSERT INTO terrain_kinds VALUES ('neutral');
INSERT INTO terrain_kinds VALUES ('basic');

CREATE TABLE planets (PRIMARY KEY (galaxy, id),
    galaxy integer NOT NULL REFERENCES galaxies ON DELETE CASCADE,
    id integer GENERATED AS IDENTITY,
    name varchar(255) NOT NULL,
    status varchar(10) NOT NULL CHECK (status in ('unexplored', 'known', 'conquered')),
    escape_speed real NOT NULL CHECK (escape_speed > 0),
    terrain varchar(10) NOT NULL REFERENCES terrain_kinds);

CREATE TABLE mission_kinds (kind varchar(10) PRIMARY KEY,
    CHECK (kind in ('transport', 'combat', 'intercept', 'pillage')));
INSERT INTO mission_kinds VALUES ('transport');
INSERT INTO mission_kinds VALUES ('combat');
INSERT INTO mission_kinds VALUES ('intercept');
INSERT INTO mission_kinds VALUES ('pillage');

CREATE TABLE missions (id integer GENERATED AS IDENTITY PRIMARY KEY,
    kind varchar(10) NOT NULL REFERENCES mission_kinds,
    day date NOT NULL,
    required_ships integer NOT NULL CHECK (required_ships >= 0),
    min_speed real NOT NULL CHECK (min_speed >= 0),
    min_radius real NOT NULL check (min_radius >= 0));
CREATE INDEX mission_calendar ON missions (day);

CREATE TABLE crews (id integer GENERATED AS IDENTITY PRIMARY KEY,
    size_ integer NOT NULL CHECK (size_ >= 0));

CREATE TABLE transport_ships (id integer PRIMARY KEY REFERENCES ships ON DELETE CASCADE,
    capacity real NOT NULL CHECK (capacity > 0));

CREATE TABLE combat_ships (id integer PRIMARY KEY REFERENCES ships ON DELETE CASCADE,
    min_equip integer NOT NULL CHECK (min_equip >= 0),
    max_equip integer NOT NULL CHECK (max_equip >= 0),
    CHECK (min_equip <= max_equip));

CREATE TABLE pilot_trainings (PRIMARY KEY (pilot, training),
    pilot integer NOT NULL REFERENCES pilots ON DELETE CASCADE,
    training varchar(10) NOT NULL REFERENCES mission_kinds CHECK (training in ('transport', 'combat')));

CREATE TABLE mission_targets (mission integer PRIMARY KEY REFERENCES missions ON DELETE CASCADE,
    galaxy integer NOT NULL,
    planet integer NOT NULL,
    FOREIGN KEY (galaxy, planet) REFERENCES planets);

CREATE TABLE crew_specialties (PRIMARY KEY (crew, specialty),
    crew integer NOT NULL REFERENCES crews ON DELETE CASCADE,
    specialty varchar(10) NOT NULL REFERENCES mission_kinds CHECK (specialty in ('combat', 'intercept', 'pillage')));

CREATE TABLE ship_assignments (PRIMARY KEY (ship, mission),
    ship integer NOT NULL REFERENCES ships,
    mission integer NOT NULL REFERENCES missions ON DELETE CASCADE);

CREATE TABLE pilot_assignments (PRIMARY KEY (pilot, ship, mission),
    pilot integer NOT NULL REFERENCES pilots,
    mission integer NOT NULL REFERENCES missions ON DELETE CASCADE,
    ship integer NOT NULL REFERENCES ships,
    FOREIGN KEY (ship, mission) REFERENCES ship_assignments);

CREATE TABLE crew_assignments (PRIMARY KEY (crew, ship, mission),
    crew integer NOT NULL REFERENCES crews,
    mission integer NOT NULL REFERENCES missions ON DELETE CASCADE,
    ship integer NOT NULL REFERENCES combat_ships,
    FOREIGN KEY (ship, mission) REFERENCES ship_assignments);
