CREATE TABLE comptes (
    NC int PRIMARY KEY,
    Nom varchar(10) NOT NULL,
    Solde real CHECK (Solde >= 0)
);
COMMIT;
SELECT * FROM comptes;
--DELETE FROM comptes;

INSERT INTO comptes VALUES (1, 'Paul', 333.00);
INSERT INTO comptes VALUES (2, 'Paul', 666.00);
SELECT c.nom, SUM(c.solde) as sum FROM comptes c GROUP BY c.nom;
ROLLBACK;

INSERT INTO comptes VALUES (3, 'Pierre', 12345.00);
INSERT INTO comptes VALUES (4, 'Pierre', 43210.00);
COMMIT;

SET AUTOCOMMIT ON;
INSERT INTO comptes VALUES (5, 'Jacques', 1111.00);
INSERT INTO comptes VALUES (6, 'Jacques', 2222.00);
ROLLBACK;
SET AUTOCOMMIT OFF;

INSERT INTO comptes VALUES (7, 'Jean', 10000.00);
INSERT INTO comptes VALUES (8, 'Jean',  2000.00);
SAVEPOINT DeuxInserts;
INSERT INTO comptes VALUES (9, 'Jean', 20000.00);
INSERT INTO comptes VALUES (10, 'Jean', 1987.00);
SELECT SUM(c.solde) as sum FROM comptes c WHERE c.nom = 'Jean' GROUP BY c.nom;
ROLLBACK TO DeuxInserts;
ROLLBACK;

INSERT INTO comptes VALUES (11, 'Claude', 100.00);
INSERT INTO comptes VALUES (12, 'Henri', 200.00);
UPDATE comptes SET solde = solde + 50 WHERE nom = 'Henri';
UPDATE comptes SET solde = solde - 150 WHERE nom = 'Claude';
COMMIT;

DELETE FROM comptes WHERE nom = 'Jacques';
UPDATE comptes SET solde = solde + 10000;
COMMIT;

SELECT SUM(solde) FROM comptes;
SET TRANSACTION ISOLATION LEVEL SERIALIZABLE;
INSERT INTO comptes VALUES (13, 'Paul', 500.00);
COMMIT;

SET TRANSACTION ISOLATION LEVEL SERIALIZABLE;
SELECT SUM(solde) FROM comptes;
INSERT INTO comptes VALUES (14, 'Paul', 1000.00);
COMMIT;

UPDATE comptes SET solde = solde + 100 WHERE nom = 'Paul';
UPDATE comptes SET solde = solde + 50 WHERE nom = 'Pierre';
UPDATE comptes SET solde = solde - 100 WHERE nom = 'Pierre';
UPDATE comptes SET solde = solde - 200 WHERE nom = 'Paul';
COMMIT;
