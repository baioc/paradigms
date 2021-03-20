-- R1
SELECT NS, NomS, CapCh
FROM RESORTS
WHERE TypeS = 'montagne';

-- R2
SELECT r.NomS, r.NS, h.NH, h.AdrH, h.TelH, h.CatH
FROM RESORTS r, HOTELS h
WHERE r.TypeS = 'mer' AND r.NS = h.NS;

-- R3
SELECT DISTINCT r.noms
FROM RESORTS r, HOTELS h
WHERE r.ns = h.ns AND r.types = 'mer' AND h.cath = 4;

-- R4
SELECT DISTINCT g.nomcl
FROM BOOKINGS b, RESORTS r, GUESTS g
WHERE r.types = 'montagne' AND b.ns = r.NS AND b.ncl = g.ncl;

-- R5
SELECT DISTINCT r.NS, r.NH, r.NCH
FROM RESORTS s, HOTELS h, ROOMS r
WHERE r.prix < 50 AND h.cath = 2 AND s.types = 'montagne'
  AND r.ns = h.ns AND r.nh = h.nh AND h.ns = s.ns;

-- R6
SELECT DISTINCT g.nomcl
FROM GUESTS g, BOOKINGS b, ROOMS r, RESORTS s
WHERE g.ncl = b.ncl AND s.types = 'mer' AND r.typch in ('D', 'DWC')
  AND r.ns = b.ns AND r.nh = b.nh AND r.nch = b.nch AND b.ns = s.ns;

-- R8
(
    SELECT h.ns, h.nh FROM HOTELS h WHERE h.cath = 4
) MINUS (
    SELECT r.ns, r.nh FROM ROOMS r WHERE r.typch <> 'SDB'
);

-- R9
SELECT DISTINCT h.ns, h.nh, h.nomh, h.adrh, h.cath
FROM HOTELS h, ROOMS r
WHERE r.ns = h.ns AND r.nh = h.nh
GROUP BY h.ns, h.nh, r.prix, h.nomh, h.adrh, h.cath
HAVING COUNT(h.nbch) >= 2;

-- R10
SELECT h.ns, h.nh, h.nomh, h.adrh, h.cath, COUNT(b.ncl) as NbRes
FROM HOTELS h, BOOKINGS b
WHERE h.ns = b.ns AND h.nh = b.nh AND b.ncl is not NULL
GROUP BY h.ns, h.nh, h.nomh, h.adrh, h.cath;
