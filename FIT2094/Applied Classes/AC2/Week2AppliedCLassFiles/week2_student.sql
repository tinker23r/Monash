/*
Databases Week 2 Applied Class
week2_student.sql

student id:
student name:
last modified date:

*/

DROP TABLE student CASCADE CONSTRAINTS;

CREATE TABLE student (
    studid        NUMBER(8) NOT NULL,
    studfname     VARCHAR2(20) NOT NULL,
    studlname     VARCHAR2(20) NOT NULL,
    studdob       DATE NOT NULL,
    studaddress   VARCHAR2(100) NOT NULL,
    studphone     CHAR(15) NOT NULL,
    studemail     VARCHAR2(50) NOT NULL
);

COMMENT ON COLUMN student.studid IS
    'Student Identifier';

COMMENT ON COLUMN student.studfname IS
    'Student first name';

COMMENT ON COLUMN student.studlname IS
    'Student last name';

COMMENT ON COLUMN student.studdob IS
    'Student date of birth';

COMMENT ON COLUMN student.studaddress IS
    'Student address';

COMMENT ON COLUMN student.studphone IS
    'Student contact phone number';

COMMENT ON COLUMN student.studemail IS
    'Student email';

ALTER TABLE student ADD CONSTRAINT student_pk PRIMARY KEY ( studid );

--Populate table insert
insert into STUDENT (studid, studfname, studlname, studdob, studaddress, studphone, studemail) values (12489379, 'Gilberto', 'Bwy', '30-Aug-1992', '5664 Loomis Parkway, Melbourne', '7037621034', 'Gilberto.Bwy@student.monash.edu');
insert into STUDENT (studid, studfname, studlname, studdob, studaddress, studphone, studemail) values (12511467, 'Francyne', 'Rigney', '18-Jan-1992', '75 Buhler Street, Mulgrave', '6994152403', 'Francyne.Rigney@student.monash.edu');
insert into STUDENT (studid, studfname, studlname, studdob, studaddress, studphone, studemail) values (12609485, 'Cassondra', 'Sedcole', '07-Aug-1990', '6507 Tennessee Alley, Melbourne', '8343944500', 'Cassondra.Sedcole@student.monash.edu');
insert into STUDENT (studid, studfname, studlname, studdob, studaddress, studphone, studemail) values (12802225, 'Friedrick', 'Geist', '02-Mar-1997', '99271 Eliot Pass, Dingley', '6787553656', 'Friedrick.Geist@student.monash.edu');
insert into STUDENT (studid, studfname, studlname, studdob, studaddress, studphone, studemail) values (12842838, 'Herminia', 'Mendus', '25-Apr-1996', '64186 East Lane, Moorabbin', '4896374903', 'Herminia.Mendus@student.monash.edu');
insert into STUDENT (studid, studfname, studlname, studdob, studaddress, studphone, studemail) values (13028303, 'Herculie', 'Mendus', '02-Aug-1998', '44 Becker Street, Mulgrave', '2309618710', 'Herculie.Mendus@student.monash.edu');
insert into STUDENT (studid, studfname, studlname, studdob, studaddress, studphone, studemail) values (13119134, 'Shandra', 'Lindblom', '20-Apr-2000', '9241 Rieder Parkway, Chelsea', '4384142213', 'Shandra.Lindblom@student.monash.edu');
insert into STUDENT (studid, studfname, studlname, studdob, studaddress, studphone, studemail) values (13390148, 'Brier', 'Kilgour', '21-Feb-1995', '79776 Dryden Plaza, Moorabbin', '6981280319', 'Brier.Kilgour@student.monash.edu');

commit;
