-- Question 3
SELECT
    empno,
    empname                          AS "Employee Name",
    empjob,
    to_char(empmsal * 12, '$99,999') AS "Yearly Salary"
FROM
    payroll.employee
WHERE
    deptno = (
        SELECT
            deptno
        FROM
            payroll.department
        WHERE
            upper(deptname) = upper('Sales')
    )
ORDER BY
    "Yearly Salary" DESC,
    empno;


-- Question 4
SELECT
    c.crscode,
    c.crsdesc,
    c.crsduration || ' days' AS "Course Duration"
FROM
         payroll.course c
    JOIN payroll.registration r
    ON ( r.crscode = c.crscode )
WHERE
    r.regevaluation IS NOT NULL
GROUP BY
    c.crscode,
    c.crsdesc,
    c.crsduration
HAVING
    COUNT(r.empno) > 5
ORDER BY
    crscode;
    
-- Question 5
SELECT
    e.empno,
    empname,
    to_char(empbdate, 'dd-mm-yyyy'),
    COUNT(DISTINCT crscode)
FROM
         payroll.employee e
    JOIN payroll.registration r
    ON ( e.empno = r.empno )
GROUP BY
    e.empno,
    empname,
    to_char(empbdate, 'dd-mm-yyyy')
HAVING
    COUNT(DISTINCT crscode) < (
        SELECT
            AVG(COUNT(DISTINCT crscode))
        FROM
            payroll.registration
        GROUP BY
            empno
    );
    
-- Question 6
SELECT
    e.empno,
    empname,
    COUNT(e.empno) AS numb_appoints,
    lpad(to_char(COUNT(h.histcomments) / COUNT(e.empno) * 100, '990.0')
         || '%', 20)    AS percent_with_commet,
    (
        SELECT
            histmsal * 12
        FROM
            payroll.history
        WHERE
                histbegindate = (
                    SELECT
                        MIN(histbegindate)
                    FROM
                        payroll.history
                    WHERE
                        h.empno = e.empno
                )
            AND h.empno = e.empno
    )              AS starting_yearly_salary,
    empmsal * 12   AS current_yearly_salary
FROM
         payroll.employee e
    JOIN payroll.history h
    ON ( e.empno = h.empno )
GROUP BY
    e.empno,
    empname,
    histmsal * 12,
    empmsal * 12
HAVING
    COUNT(*) > 4
ORDER BY
    e.empno;