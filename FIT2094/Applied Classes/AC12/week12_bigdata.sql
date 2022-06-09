12.1.2 MongoDB Create Update and Delete
1. Use an SQL select statement to generate a collection of documents using the following structure/format from the UNI database.

SET PAGESIZE 200

SELECT
    JSON_OBJECT(
        '_id' VALUE stuid, 
        'name' VALUE stufname || ' ' || stulname, 
        'contactInfo' VALUE JSON_OBJECT (
            'address' VALUE stuaddress, 
            'phone' VALUE rtrim(stuphone), 
            'email' VALUE stuemail
            ),
        'dob' VALUE to_char(studob, 'dd-mm-yyyy'),
        'enrolmentInfo' VALUE json_arrayagg(
                                    JSON_OBJECT(
                                        'unitcode' VALUE unitcode,
                                        'unitname' VALUE unitname,
                                        'year' VALUE to_char(ofyear, 'yyyy'),
                                        'semester' VALUE ofsemester,
                                        'mark' VALUE enrolmark,
                                        'grade' VALUE enrolgrade
                                        )
                                    )
        FORMAT JSON
        )
        || ','
FROM
    uni.student
    NATURAL JOIN uni.enrolment
    NATURAL JOIN uni.unit
GROUP BY
    stuid,
    stufname,
    stulname,
    stuaddress,
    stuphone,
    stuemail,
    studob
ORDER BY
    stuid;

2. Name the collection as enrolment (you may use any suitable name for your database) and insert the first 10 documents generated by the select statement into MongoDB (if it returns error, try to add maximum 5 documents at one time).

db.enrolment.insertMany(
    [
        {
            "_id": 11443959,
            "name": "Geraldine Lomb",
            "contactInfo": {
                "address": "55 Northwestern Trail, Toorak",
                "phone": "4819717953",
                "email": "Geraldine.Lomb@student.monash.edu"
            },
            "dob": "10-09-1996",
            "enrolmentInfo": [
                {
                    "unitcode": "FIT1045",
                    "unitname": "Algorithms and programming fundamentals in python",
                    "year": "2020",
                    "semester": 2,
                    "mark": 39,
                    "grade": "N"
                },
                {
                    "unitcode": "FIT3157",
                    "unitname": "Advanced web design",
                    "year": "2021",
                    "semester": 1,
                    "mark": 74,
                    "grade": "D"
                },
                {
                    "unitcode": "FIT2094",
                    "unitname": "Databases",
                    "year": "2021",
                    "semester": 2,
                    "mark": 51,
                    "grade": "P"
                },
                {
                    "unitcode": "FIT2094",
                    "unitname": "Databases",
                    "year": "2021",
                    "semester": 1,
                    "mark": 38,
                    "grade": "N"
                },
                {
                    "unitcode": "FIT1050",
                    "unitname": "Web fundamentals",
                    "year": "2020",
                    "semester": 2,
                    "mark": 50,
                    "grade": "P"
                }
            ]
        },
        {
            "_id": 11620237,
            "name": "Marlane Joiris",
            "contactInfo": {
                "address": "385 Warbler Road, Preston",
                "phone": "5493750951",
                "email": "Marlane.Joiris@student.monash.edu"
            },
            "dob": "29-05-1998",
            "enrolmentInfo": [
                {
                    "unitcode": "FIT1045",
                    "unitname": "Algorithms and programming fundamentals in python",
                    "year": "2020",
                    "semester": 2,
                    "mark": 74,
                    "grade": "D"
                },
                {
                    "unitcode": "FIT3176",
                    "unitname": "Advanced database design",
                    "year": "2021",
                    "semester": 2,
                    "mark": 93,
                    "grade": "HD"
                },
                {
                    "unitcode": "FIT3157",
                    "unitname": "Advanced web design",
                    "year": "2021",
                    "semester": 1,
                    "mark": null,
                    "grade": "DEF"
                },
                {
                    "unitcode": "FIT2094",
                    "unitname": "Databases",
                    "year": "2021",
                    "semester": 1,
                    "mark": 33,
                    "grade": "N"
                },
                {
                    "unitcode": "FIT1050",
                    "unitname": "Web fundamentals",
                    "year": "2020",
                    "semester": 2,
                    "mark": 57,
                    "grade": "P"
                }
            ]
        },
        {
            "_id": 12489379,
            "name": "Gilberto Bwy",
            "contactInfo": {
                "address": "5664 Loomis Parkway, Melbourne",
                "phone": "7037621034",
                "email": "Gilberto.Bwy@student.monash.edu"
            },
            "dob": "30-08-1992",
            "enrolmentInfo": [
                {
                    "unitcode": "FIT1045",
                    "unitname": "Algorithms and programming fundamentals in python",
                    "year": "2019",
                    "semester": 1,
                    "mark": 40,
                    "grade": "N"
                },
                {
                    "unitcode": "FIT2094",
                    "unitname": "Databases",
                    "year": "2020",
                    "semester": 1,
                    "mark": 63,
                    "grade": "C"
                },
                {
                    "unitcode": "FIT1050",
                    "unitname": "Web fundamentals",
                    "year": "2019",
                    "semester": 1,
                    "mark": 44,
                    "grade": "N"
                },
                {
                    "unitcode": "FIT1050",
                    "unitname": "Web fundamentals",
                    "year": "2019",
                    "semester": 2,
                    "mark": 92,
                    "grade": "HD"
                },
                {
                    "unitcode": "FIT1045",
                    "unitname": "Algorithms and programming fundamentals in python",
                    "year": "2019",
                    "semester": 2,
                    "mark": 89,
                    "grade": "HD"
                }
            ]
        },
        {
            "_id": 12511467,
            "name": "Francyne Rigney",
            "contactInfo": {
                "address": "75 Buhler Street, Mulgrave",
                "phone": "6994152403",
                "email": "Francyne.Rigney@student.monash.edu"
            },
            "dob": "18-01-1992",
            "enrolmentInfo": [
                {
                    "unitcode": "FIT1045",
                    "unitname": "Algorithms and programming fundamentals in python",
                    "year": "2019",
                    "semester": 1,
                    "mark": 87,
                    "grade": "HD"
                },
                {
                    "unitcode": "FIT3176",
                    "unitname": "Advanced database design",
                    "year": "2020",
                    "semester": 1,
                    "mark": null,
                    "grade": "WH"
                },
                {
                    "unitcode": "FIT3157",
                    "unitname": "Advanced web design",
                    "year": "2019",
                    "semester": 2,
                    "mark": 98,
                    "grade": "HD"
                },
                {
                    "unitcode": "FIT2094",
                    "unitname": "Databases",
                    "year": "2019",
                    "semester": 2,
                    "mark": 83,
                    "grade": "HD"
                },
                {
                    "unitcode": "FIT1050",
                    "unitname": "Web fundamentals",
                    "year": "2019",
                    "semester": 1,
                    "mark": 86,
                    "grade": "HD"
                }
            ]
        },
        {
            "_id": 12609485,
            "name": "Cassondra Sedcole",
            "contactInfo": {
                "address": "6507 Tennessee Alley, Melbourne",
                "phone": "8343944500",
                "email": "Cassondra.Sedcole@student.monash.edu"
            },
            "dob": "07-08-1990",
            "enrolmentInfo": [
                {
                    "unitcode": "FIT1045",
                    "unitname": "Algorithms and programming fundamentals in python",
                    "year": "2019",
                    "semester": 1,
                    "mark": 85,
                    "grade": "HD"
                },
                {
                    "unitcode": "FIT2094",
                    "unitname": "Databases",
                    "year": "2019",
                    "semester": 2,
                    "mark": 69,
                    "grade": "C"
                },
                {
                    "unitcode": "FIT1050",
                    "unitname": "Web fundamentals",
                    "year": "2019",
                    "semester": 1,
                    "mark": 23,
                    "grade": "N"
                },
                {
                    "unitcode": "FIT1050",
                    "unitname": "Web fundamentals",
                    "year": "2020",
                    "semester": 1,
                    "mark": 44,
                    "grade": "N"
                },
                {
                    "unitcode": "FIT1050",
                    "unitname": "Web fundamentals",
                    "year": "2019",
                    "semester": 2,
                    "mark": 36,
                    "grade": "N"
                }
            ]
        },
        {
            "_id": 12802225,
            "name": "Friedrick Geist",
            "contactInfo": {
                "address": "99271 Eliot Pass, Dingley",
                "phone": "6787553656",
                "email": "Friedrick.Geist@student.monash.edu"
            },
            "dob": "02-03-1997",
            "enrolmentInfo": [
                {
                    "unitcode": "FIT1045",
                    "unitname": "Algorithms and programming fundamentals in python",
                    "year": "2019",
                    "semester": 1,
                    "mark": 83,
                    "grade": "HD"
                },
                {
                    "unitcode": "FIT3176",
                    "unitname": "Advanced database design",
                    "year": "2020",
                    "semester": 1,
                    "mark": null,
                    "grade": "WH"
                },
                {
                    "unitcode": "FIT3157",
                    "unitname": "Advanced web design",
                    "year": "2019",
                    "semester": 2,
                    "mark": 62,
                    "grade": "C"
                },
                {
                    "unitcode": "FIT2094",
                    "unitname": "Databases",
                    "year": "2019",
                    "semester": 2,
                    "mark": 52,
                    "grade": "P"
                },
                {
                    "unitcode": "FIT1050",
                    "unitname": "Web fundamentals",
                    "year": "2019",
                    "semester": 1,
                    "mark": 61,
                    "grade": "C"
                }
            ]
        },
        {
            "_id": 12842838,
            "name": "Herminia Mendus",
            "contactInfo": {
                "address": "64186 East Lane, Moorabbin",
                "phone": "4896374903",
                "email": "Herminia.Mendus@student.monash.edu"
            },
            "dob": "25-04-1996",
            "enrolmentInfo": [
                {
                    "unitcode": "FIT1045",
                    "unitname": "Algorithms and programming fundamentals in python",
                    "year": "2019",
                    "semester": 1,
                    "mark": 74,
                    "grade": "D"
                },
                {
                    "unitcode": "FIT3176",
                    "unitname": "Advanced database design",
                    "year": "2020",
                    "semester": 1,
                    "mark": null,
                    "grade": "WH"
                },
                {
                    "unitcode": "FIT3157",
                    "unitname": "Advanced web design",
                    "year": "2019",
                    "semester": 2,
                    "mark": 71,
                    "grade": "D"
                },
                {
                    "unitcode": "FIT2094",
                    "unitname": "Databases",
                    "year": "2019",
                    "semester": 2,
                    "mark": 58,
                    "grade": "P"
                },
                {
                    "unitcode": "FIT1050",
                    "unitname": "Web fundamentals",
                    "year": "2019",
                    "semester": 1,
                    "mark": 76,
                    "grade": "D"
                }
            ]
        },
        {
            "_id": 13019582,
            "name": "Tani Aitchison",
            "contactInfo": {
                "address": "842 Paget Drive, Mount Waverley",
                "phone": "7352456677",
                "email": "Tani.Aitchison@student.monash.edu"
            },
            "dob": "16-07-1996",
            "enrolmentInfo": [
                {
                    "unitcode": "FIT1045",
                    "unitname": "Algorithms and programming fundamentals in python",
                    "year": "2020",
                    "semester": 2,
                    "mark": null,
                    "grade": "WH"
                },
                {
                    "unitcode": "FIT3157",
                    "unitname": "Advanced web design",
                    "year": "2021",
                    "semester": 1,
                    "mark": 81,
                    "grade": "HD"
                },
                {
                    "unitcode": "FIT2094",
                    "unitname": "Databases",
                    "year": "2021",
                    "semester": 1,
                    "mark": 42,
                    "grade": "N"
                },
                {
                    "unitcode": "FIT1050",
                    "unitname": "Web fundamentals",
                    "year": "2021",
                    "semester": 2,
                    "mark": 47,
                    "grade": "N"
                },
                {
                    "unitcode": "FIT1050",
                    "unitname": "Web fundamentals",
                    "year": "2020",
                    "semester": 2,
                    "mark": 20,
                    "grade": "N"
                }
            ]
        },
        {
            "_id": 13028303,
            "name": "Herculie Mendus",
            "contactInfo": {
                "address": "44 Becker Street, Mulgrave",
                "phone": "2309618710",
                "email": "Herculie.Mendus@student.monash.edu"
            },
            "dob": "02-08-1998",
            "enrolmentInfo": [
                {
                    "unitcode": "FIT1045",
                    "unitname": "Algorithms and programming fundamentals in python",
                    "year": "2019",
                    "semester": 1,
                    "mark": 61,
                    "grade": "C"
                },
                {
                    "unitcode": "FIT3176",
                    "unitname": "Advanced database design",
                    "year": "2020",
                    "semester": 1,
                    "mark": null,
                    "grade": null
                },
                {
                    "unitcode": "FIT3157",
                    "unitname": "Advanced web design",
                    "year": "2019",
                    "semester": 2,
                    "mark": 65,
                    "grade": "C"
                },
                {
                    "unitcode": "FIT2094",
                    "unitname": "Databases",
                    "year": "2019",
                    "semester": 2,
                    "mark": 88,
                    "grade": "HD"
                },
                {
                    "unitcode": "FIT1050",
                    "unitname": "Web fundamentals",
                    "year": "2019",
                    "semester": 1,
                    "mark": 97,
                    "grade": "HD"
                }
            ]
        },
        {
            "_id": 13119134,
            "name": "Shandra Lindblom",
            "contactInfo": {
                "address": "9241 Rieder Parkway, Chelsea",
                "phone": "4384142213",
                "email": "Shandra.Lindblom@student.monash.edu"
            },
            "dob": "20-04-2000",
            "enrolmentInfo": [
                {
                    "unitcode": "FIT1045",
                    "unitname": "Algorithms and programming fundamentals in python",
                    "year": "2019",
                    "semester": 1,
                    "mark": 62,
                    "grade": "C"
                },
                {
                    "unitcode": "FIT3176",
                    "unitname": "Advanced database design",
                    "year": "2020",
                    "semester": 1,
                    "mark": null,
                    "grade": null
                },
                {
                    "unitcode": "FIT3157",
                    "unitname": "Advanced web design",
                    "year": "2019",
                    "semester": 2,
                    "mark": 82,
                    "grade": "HD"
                },
                {
                    "unitcode": "FIT2094",
                    "unitname": "Databases",
                    "year": "2019",
                    "semester": 2,
                    "mark": 64,
                    "grade": "C"
                },
                {
                    "unitcode": "FIT1050",
                    "unitname": "Web fundamentals",
                    "year": "2019",
                    "semester": 1,
                    "mark": 91,
                    "grade": "HD"
                }
            ]
        }
    ]
);

3. Create a new enrolment for studid 12489379, the student is enrolled in FIT2002 (IT Project Management) in semester 1 2020. Since this is a new enrolment, set the mark and the grade as null.

db.enrolment.updateOne(
    {"_id": 12489379},
    {"$push": {"enrolmentInfo":{
        "unitcode": "FIT2002",
        "unitname": "IT Project Management",
        "year":"2022",
        "semester": 1,
        "mark": null,
        "grade": null
    }}}
);

4. Update this enrolment for studid 12489379 in FIT2002, set the mark to 65 and grade to C

db.enrolment.updateOne(
    {"_id": 12489379, "enrolmentInfo.unitcode": "FIT2002"},
    {"$set": {
        "enrolmentInfo.$.mark": 65,
        "enrolmentInfo.$.grade": "C"
    }}
);

5. Delete this enrolment for student id 12489379 in FIT2002

db.enrolment.update(
    {"_id":12489379},
    {"$pull": {"enrolmentInfo": {"unitcode": "FIT2002"}}}
);

db.enrolment.find({"_id":12489379});

12.1.3 MongoDB Read
Write db.find() commands for following questions:
1. Retrieve the document for student id = 12802225

db.enrolment.find({"_id": 12802225});

2. Show the id and name of students who have any mark greater than 95 in any enrolment (hint: use $gt:95)

db.enrolment.find({"enrolmentInfo.mark": {"$gt":95}}, {"_id": 1});


3. Retrieve the name and contact info of students who enrolled in any unit which has "web design" as part of its name

db.enrolment.find({"enrolmentInfo.unitname": /.*web design.*/}, {"name": 1, "contactInfo": 1});


4. Retrieve the  id and name of any students who have grades WH OR N

db.enrolment.find({"$or": [{"enrolmentInfo.grade": "WH", "enrolmentInfo.grade": "N"}]}, {"_id": 1, "name": 1});
