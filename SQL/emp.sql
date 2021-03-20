-- Q6
SELECT e.ename, e.job, e.sal
FROM EMP e
WHERE e.deptno = 30
ORDER BY e.sal;

-- Q13
SELECT dname, job, sumSal, nbEmp, avgSal
FROM (
    SELECT d.dname, e.job, COUNT(*) as nbEmp, SUM(e.sal) as sumSal, AVG(e.sal) as avgSal
    FROM EMP e, DEPT d
    WHERE e.deptno = d.deptno
    GROUP BY d.dname, e.job
)
WHERE nbEmp > 2;

--Q20
SELECT d.deptno
FROM EMP e, DEPT d
WHERE e.deptno = d.deptno
GROUP BY d.deptno, d.dname
ORDER BY COUNT(*) DESC
FETCH FIRST 1 ROWS ONLY;
