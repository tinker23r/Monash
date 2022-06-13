/* Part A*/

-- Question 1
SELECT
    empno,
    empname,
    empinit,
    empjob,
    to_char(empbdate, 'DD/MM/YYYY'),
    empmsal,
    empcomm,
    deptno,
    mgrno
FROM
    payroll.employee
WHERE
    empmsal < 1000
ORDER BY
    empno;
    
    
-- Question 2
SELECT DISTINCT
    ( deptno )
FROM
    payroll.employee
ORDER BY
    deptno;


-- Question 3
SELECT
    empno,
    empname,
    empinit,
    empjob,
    to_char(empbdate, 'DD/MM/YYYY'),
    empmsal,
    empcomm,
    deptno,
    mgrno
FROM
    payroll.employee
WHERE
        empjob = 'TRAINER'
    AND empmsal < 2500
    AND deptno = 20
ORDER BY
    empno;
    
-- Question 4
SELECT
    empname AS "Name",
    empjob  AS "Job",
    empmsal AS "Monthly Salary",
    empcomm AS "Commission"
FROM
    payroll.employee
WHERE
    empsal > empcomm
ORDER BY
    empname,
    desc(empmsal);

-- Question 5
SELECT
    empname,
    empjob
FROM
    payroll.employee
WHERE
    upper(empjob) LIKE '%R'
ORDER BY
    empname,
    empjob;
    
-- Question 6
SELECT
    empname,
    empjob
FROM
    payroll.employee
WHERE
    upper(empjob) LIKE in('J%', 'K%', 'R%')
ORDER BY
    empname,
    empjob;

-- Question 7
SELECT
    empname,
    empjob,
    to_char(empbdate, 'DD/MM/YYYY'),
    empmsal
FROM
    payroll.employee
WHERE
        extract(yyyy, empdate) < 1900
    AND empmsal > 1500
ORDER BY
    empname,
    empmsal;
    
-- Question 18
SELECT
    e.empname,
    d.deptname,
    to_char(h.histbegindate, 'DD/MM/YYYY'),
    to_char(h.histenddate, 'DD/MM/YYYY'),
    h.histmsal
FROM
         payroll.employee e
    JOIN payroll.department d
    ON ( e.deptno = d.deptno )
    JOIN payroll.history    h
    ON ( e.empno = h.empno )
ORDER BY
    e.empname,
    h.histbegindate DESC;

/*Part B*/

-- Question 1
SELECT
    empno,
    empname,
    empjob,
    empmsal,
    empmsal * 12       AS "Annual Salary",
    empmsal * 1.1 * 12 AS "Increased Annual Salary"
FROM
    payroll.employee
ORDER BY
    empno;
    
-- Question 2
SELECT
    empname,
    to_char(empbdate, 'DD/MM/YYYY')               AS "Birthday",
    floor(months_between(sysdate, empbdate) / 12) AS "Age"
FROM
    payroll.employee
ORDER BY
    birthday,
    empname;
    
-- Question 4
SELECT
    'EMPLOYEE '
    || upper(e.empinit)
    || '. '
    || initcap(e.empname)
    || 'IS A '
    || initcap(e.empjob)
    || 'AND WORKS IN THE '
    || initcap(d.deptname)
    || 'DEPARTMENT'
FROM
         payroll.employee e
    JOIN payroll.department d
    ON ( e.empno = d.empno )
ORDER BY
    e.empno;
    
-- Question 5
SELECT
    empname,
    to_char(empbdate, 'DD/MM/YYYY')                               AS "Birthday",
    lpad(to_char(months_between(sysdate, empbdate), '990.0'), 10) AS "Age in Months"
FROM
    payroll.employee
ORDER BY
    "Age in Months" DESC,
    empname;
    
-- Question 6
SELECT
    empname,
    to_char(empbdate, 'DD/MM/YYYY')
FROM
    payroll.employee
WHERE
    EXTRACT(MONTH FROM empbdate) = 2
ORDER BY
    empnamel;
    
-- Question 7 ??????????????????????????????
SELECT
    empname,
    empmsal,
    empcomm
FROM
    payroll.employee
WHERE
    empcomm > empsalm
ORDER BY
    empname,
    ( empmsal + empcomm );
    
    
-- Question 8
SELECT
    'EMPLOYEE '
    || upper(empinit)
    || '. '
    || initcap(empname)
    || ' was born on '
    || to_char(empbdate, 'DAY')
    || ' the '
    || EXTRACT(DAY FROM empbdate)
    || ' of '
    || to_char(empbdate, 'MONTH')
    || ', '
    || EXTRACT(YEAR FROM empbdate)
FROM
    payroll.employee
ORDER BY
    empname;
    
-- Question 9
SELECT
    e.empname,
    COUNT(r.empno)
FROM
         payroll.employee e
    JOIN registration r
    ON ( e.empno = r.empno )
GROUP BY
    e.empno,
    e.empname
ORDER BY
    e.empno;
    
-- Question 10
SELECT
    empno,
    empname,
    to_char(empbdate, 'DD/MM/YYYY')
FROM
    payroll.employee
WHERE
    empbdate = (
        SELECT
            MIN(empbdate)
        FROM
            payroll.employee
    )
ORDER BY
    empno;