/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2014 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.reports.bc;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DataField;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.Staff;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.StudentSchool;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import com.x2dev.utils.types.PlainTime;
import java.sql.Timestamp;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class StudentLocatorReportData.
 */
public class StudentLocatorReportData extends ReportJavaSourceNet {
    /**
     * 
     */
    private static final long serialVersionUID = 1L;
    private final static String STUDENT_LAST_NAME = "StudentLastName";
    private final static String STUDENT_GENDER = "StudentGender";
    private final static String STUDENT_BIRTH_DATE = "StudentBirthDate";
    private static final String STUDENT_FIRST_NAME = "StudentFirstName";

    /*
     * Grid fields
     */
    private static final String FIELD_SCHOOL_FAX = "SchoolFax";
    private static final String FIELD_SCHOOL_PHONE = "SchoolPhone";

    /*
     * Aliases
     */
    private static final String SCHOOL_FAX_ALIAS = "skl-fax";

    private String studentFirstName;
    private String studentLastName;
    private String studentGender;
    private Date studentBirthDate;
    private final int dateRange = 45;
    private Logger m_logger;

    /**
     * Searches for a list of students in Aspen and in the BC eSIS database with the last name, date
     * of birth and gender
     * entered in the input dialog of the StudentLocator report in Aspen. The list of students from
     * Aspen and the list
     * from eSIS are merged into a single list which is then added to a report grid that is returned
     * to Aspen to be
     * formatted into a pdf formatted report.
     *
     * @author X2 Development Corporation
     * @return Object
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        m_logger = Logger.getLogger("StudentLocator", "com.x2dev.sis.web.LogResources");

        String dbServerName = (String) getParameter(StudentLocatorDAO.DB_SERVER_KEY);

        if (dbServerName == null) {

            // addSetupError("BC eSIS linked Oracle Database", "There is no host name provded in the
            // input definition for the Common Student Extract");
            throw new X2BaseException((String) null,
                    "No host name found for linked Oracle Database. Contact your system administrator");
        }
        studentLastName = (String) getParameter(STUDENT_LAST_NAME);
        studentFirstName = (String) getParameter(STUDENT_FIRST_NAME);
        studentFirstName = ("".equals(studentFirstName)) ? null : studentFirstName;
        studentGender = (String) getParameter(STUDENT_GENDER);
        studentGender = (studentGender.equals("")) ? null : studentGender;
        studentBirthDate = (Date) getParameter(STUDENT_BIRTH_DATE);
        /*
         * Build query object that will be used to retrieve export students.
         */
        Criteria studentCriteria = getPersonCriteria();
        QueryByCriteria studentQuery = new QueryByCriteria(SisStudent.class, studentCriteria);
        Collection<SisStudent> students = getBroker().getCollectionByQuery(studentQuery);

        Criteria secondarySchoolCriteria = getSecondarySchoolCriteria();
        QueryByCriteria secondarySchoolQuery = new QueryByCriteria(StudentSchool.class, secondarySchoolCriteria);

        Collection<StudentSchool> studentSecondarySchools = getBroker().getCollectionByQuery(secondarySchoolQuery);


        StudentLocatorDAO dao = new StudentLocatorDAO(getBroker(), dbServerName);
        Map<String, List<Object>> studentMaps = new HashMap<String, List<Object>>();
        try {
            studentMaps = dao.getStudentData(studentFirstName, studentLastName, studentGender, null,
                    getCalendar(studentBirthDate), dateRange);
        } catch (Exception ex) {
            // the dao will log any errors that occur in attempt to access eSIS students
        }
        for (SisStudent student : students) {
            // addStudentToGrid(student, grid);
            List<Object> studentMapList = dao.getStudentMapList(studentMaps, student.getStateId());
            studentMapList.add(student);
        }

        for (StudentSchool studentSchool : studentSecondarySchools) {
            // addStudentToGrid(student, grid);
            List<Object> studentMapList = dao.getStudentMapList(studentMaps, studentSchool.getStudent().getStateId());
            studentMapList.add(studentSchool);
        }

        ReportDataGrid grid = new ReportDataGrid();

        for (Entry<String, List<Object>> studentEntry : studentMaps.entrySet()) {
            for (Object studentObject : studentEntry.getValue()) {
                if (studentObject instanceof Map) {
                    addStudentToGrid((Map) studentObject, grid);
                } else if (studentObject instanceof StudentSchool) {
                    addStudentToGrid((StudentSchool) studentObject, grid);
                } else {
                    addStudentToGrid((SisStudent) studentObject, grid);
                }
            }
        }

        // studentCriteria.addEqualTo(SisStudent.COL_OID, getOid());
        grid.beforeTop();
        return grid;
    }

    /**
     * A utility method that converts a date to a calendar, adds a number of days to the calendar
     * date and
     * returns a date value representing this new date.
     *
     * @param oldDate the date to add days to
     * @param days the number of days to add
     *
     * @return the newly calculated date
     */
    private java.sql.Date addDays(Date oldDate, int days) {
        Calendar cal = getCalendar(oldDate);
        cal.add(Calendar.DATE, days);
        java.sql.Date newDate = new java.sql.Date(cal.getTimeInMillis());

        return newDate;
    }

    /**
     * Adds a student represented as a Map object into the report grid.
     *
     * @author X2 Development Corporation
     * @param studentMap the student to be added
     * @param grid the report grid to add the student to
     */
    private void addStudentToGrid(Map studentMap, ReportDataGrid grid) {
        grid.append();

        setMapValue(grid, "SisSystem", "BCeSIS");

        setMapValue(grid, "StudentFirstName", studentMap.get("FIRST_NAME"));
        setMapValue(grid, "StudentMiddleName", studentMap.get("MIDDLE_NAME"));
        setMapValue(grid, "StudentLastName", studentMap.get("LAST_NAME"));
        setMapValue(grid, "StudentID", studentMap.get("PEN"));
        grid.set("StudentGender", getGender((String) studentMap.get("GENDER")));
        setMapValue(grid, "StudentStatus", studentMap.get("STUDENT_STATUS"));
        grid.set("StudentBirthdate", getDate(studentMap, "BIRTHDATE"));
        grid.set("StudentWithdrawalDate", getDate(studentMap, "WITHDRAW_DATE"));

        setMapValue(grid, "SchoolName", studentMap.get("SCHOOL_NAME"));
        setMapValue(grid, "SchoolID", studentMap.get("SCHOOL_ID"));
        setMapValue(grid, "PrimarySchool", studentMap.get("PRIMARY_SCHOOL"));
        setMapValue(grid, FIELD_SCHOOL_PHONE, studentMap.get("SCHOOL_PHONE"));
        setMapValue(grid, FIELD_SCHOOL_FAX, studentMap.get("SCHOOL_FAX"));
        setMapValue(grid, "SchoolPrincipal", studentMap.get("PRINCIPAL"));
        setMapValue(grid, "SchoolVP", studentMap.get("VP_1"));
        setMapValue(grid, "SchoolAdministrator", studentMap.get("VP_2"));
        setMapValue(grid, "SchoolDistrict", studentMap.get("DISTRICT_NAME"));
    }

    /**
     * Adds a student represented as a SisStudent object into the report grid.
     *
     * @param student the student to be added
     * @param grid the report grid to add the student to
     */
    private void addStudentToGrid(SisStudent student, ReportDataGrid grid) {
        grid.append();
        try {
            appendToGrid(student, student.getSchool(), true, grid);
        } catch (Exception e) {
            getLogger().log(Level.SEVERE, "" +
                    "\r\n\r\n        *******************************************************************************\r\n"
                    +
                    "        **                                                                              \r\n" +
                    "        ** StudentLocatorReportData experienced an exception attempting to access \r\n" +
                    "        ** a field in one of the members of the student record. \r\n" +
                    "        **                                                                              \r\n" +
                    "        ** The student was: \r\n" +
                    "        **                                                                              \r\n" +
                    "        ** " + student.getNameView() + " / " + student.getStateId() + "\r\n" +
                    "        **                                                                              \r\n" +
                    "        *******************************************************************************");
        }
    }

    /**
     * Appends the student/school information to the grid based on the StudentSchool association.
     *
     * @param studentSchoolObject StudentSchool
     * @param grid ReportDataGrid
     */
    private void addStudentToGrid(StudentSchool studentSchoolObject, ReportDataGrid grid) {
        grid.append();

        try {
            appendToGrid(studentSchoolObject.getStudent(), studentSchoolObject.getSchool(), false, grid);
        } catch (Exception e) {
            getLogger().log(Level.SEVERE, "" +
                    "\r\n\r\n        *******************************************************************************\r\n"
                    +
                    "        **                                                                              \r\n" +
                    "        ** StudentLocatorReportData experienced an exception attempting to access \r\n" +
                    "        ** a field in one of the members of the studentSchool record. \r\n" +
                    "        **                                                                              \r\n" +
                    "        ** The student was: \r\n" +
                    "        **                                                                              \r\n" +
                    "        ** " + studentSchoolObject.getStudent().getNameView() + " / "
                    + studentSchoolObject.getStudent().getStateId() + "\r\n" +
                    "        **                                                                              \r\n" +
                    "        *******************************************************************************");
        }
    }

    /**
     * Sets the appropriate fields on the grid.
     *
     * @param student Student
     * @param school School
     * @param primary boolean
     * @param grid ReportDataGrid
     */
    private void appendToGrid(Student student, School school, boolean primary, ReportDataGrid grid) {
        grid.set("StudentFirstName", student.getPerson().getFirstName());
        grid.set("StudentID", student.getStateId());
        grid.set("StudentGender", getGender(student.getPerson().getGenderCode()));
        grid.set("StudentStatus", student.getEnrollmentStatus());
        grid.set("StudentBirthdate", student.getPerson().getDob());
        grid.set("StudentWithdrawalDate", getWithdrawalDate(student));
        grid.set("StudentMiddleName", student.getPerson().getMiddleName());
        grid.set("StudentLastName", student.getPerson().getLastName());

        grid.set("SisSystem", "MyEducation BC");
        grid.set("PrimarySchool", primary ? "Y" : "N");

        grid.set("SchoolID", school.getSchoolId());
        grid.set("SchoolName", school.getName());
        grid.set(FIELD_SCHOOL_FAX, school.getFieldValueByAlias(SCHOOL_FAX_ALIAS));
        grid.set("SchoolPrincipal", getStaff(school.getAdministrator1()));
        grid.set("SchoolVP", getStaff(school.getAdministrator2()));
        grid.set("SchoolAdministrator", getStaff(school.getAdministrator3()));

        if (school.getOrganization2() != null) {
            grid.set("SchoolDistrict", school.getOrganization2().getName());
        }

        if (school.getAddress() != null) {
            grid.set(FIELD_SCHOOL_PHONE, student.getSchool().getAddress().getPhone01());
        }
    }

    /**
     * Utility method that converts a Date object into a Calendar object.
     *
     * @param date the Date object to be converted
     *
     * @return the calendar object representing the original date
     */
    private Calendar getCalendar(Date date) {
        Calendar cal = null;
        if (date != null) {
            cal = Calendar.getInstance();
            cal.setTime(date);
        }
        return cal;
    }

    /**
     * Retrieve a value from the student map and safely parse it as a date.
     *
     * @param studentMap Map<String,String>
     * @param string String
     * @return Object Date
     */
    private Object getDate(Map<String, String> studentMap, String string) {
        Date date = null;
        if (studentMap.get(string) != null) {
            try {
                String dateString = studentMap.get(string).substring(0, 19);
                SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
                date = dateFormat.parse(dateString);
            } catch (ParseException e) {
                getLogger().log(Level.WARNING, "StudentLocator experienced a parse exception in the getDate() method.",
                        e);
            }
        }

        return date;
    }

    /**
     * Translates gender code into text, e.g. M => Male, F => Female, otherwise empty string
     *
     * @param genderCode String
     * @return String
     */
    private String getGender(String genderCode) {
        String gender = ("M".equals(genderCode)) ? "Male" : ("F".equals(genderCode)) ? "Female" : "";

        return gender;
    }

    /**
     * Retrieve the system logger if the report logger is not available.
     *
     * @return Logger
     */
    private Logger getLogger() {
        return (m_logger == null) ? AppGlobals.getLog() : m_logger;
    }

    /**
     * Returns the criteria that retrieves all students that should be included in the export.
     * Students are searched based on last name, age and gender. The date of birth can be any
     * date within {@link #dateRange} (45 days) of the birth date entered.
     * 
     * @return Criteria
     */
    private Criteria getPersonCriteria() {
        /*
         * Build the criteria based on the students identified in the schedule and transcript
         * queries.
         */
        X2Criteria studentCriteria = new X2Criteria();
        studentCriteria.addEqualToIgnoreCase(SisStudent.REL_PERSON + "." + SisPerson.COL_LAST_NAME, studentLastName);
        if (studentFirstName != null) {
            studentCriteria.addEqualToIgnoreCase(SisStudent.REL_PERSON + "." + SisPerson.COL_FIRST_NAME,
                    studentFirstName);
        }
        if (studentGender != null) {
            studentCriteria.addEqualToIgnoreCase(SisStudent.REL_PERSON + "." + SisPerson.COL_GENDER_CODE,
                    studentGender);
        }
        if (studentBirthDate != null) {
            studentCriteria.addBetween(SisStudent.REL_PERSON + "." + SisPerson.COL_DOB,
                    addDays(studentBirthDate, -1 * dateRange), addDays(studentBirthDate, dateRange));
        }
        studentCriteria.addEqualTo(Student.REL_SCHOOL + "." + School.COL_FIELD_A003, "PROD");
        studentCriteria.addNotEqualTo(Student.REL_PERSON + "." + Person.COL_PRIVATE_INDICATOR, Boolean.TRUE);

        return studentCriteria;
    }

    /**
     * Returns the criteria for the secondary school associations.
     *
     * @return Criteria
     */
    private Criteria getSecondarySchoolCriteria() {
        X2Criteria secondarySchoolCriteria = new X2Criteria();
        StudentManager.buildSecondaryStudentDateCriteria(null, secondarySchoolCriteria, new PlainDate(),
                new PlainDate(), getBroker().getPersistenceKey());
        secondarySchoolCriteria.addNotEqualTo(
                StudentSchool.REL_STUDENT + "." + Student.REL_PERSON + "." + Person.COL_PRIVATE_INDICATOR,
                Boolean.TRUE);
        secondarySchoolCriteria.addEqualTo(StudentSchool.REL_SCHOOL + "." + School.COL_FIELD_A003, "PROD");
        secondarySchoolCriteria.addEqualToIgnoreCase(
                StudentSchool.REL_STUDENT + "." + SisStudent.REL_PERSON + "." + SisPerson.COL_LAST_NAME,
                studentLastName);
        if (studentFirstName != null) {
            secondarySchoolCriteria.addEqualToIgnoreCase(
                    StudentSchool.REL_STUDENT + "." + SisStudent.REL_PERSON + "." + SisPerson.COL_FIRST_NAME,
                    studentFirstName);
        }
        if (studentGender != null) {
            secondarySchoolCriteria.addEqualToIgnoreCase(
                    StudentSchool.REL_STUDENT + "." + SisStudent.REL_PERSON + "." + SisPerson.COL_GENDER_CODE,
                    studentGender);
        }
        if (studentBirthDate != null) {
            secondarySchoolCriteria.addBetween(
                    StudentSchool.REL_STUDENT + "." + SisStudent.REL_PERSON + "." + SisPerson.COL_DOB,
                    addDays(studentBirthDate, -1 * dateRange), addDays(studentBirthDate, dateRange));
        }

        return secondarySchoolCriteria;
    }

    /**
     * Return a staff name view if it exists.
     *
     * @param staff Staff
     * @return String
     */
    private String getStaff(Staff staff) {
        return (staff != null) ? staff.getPerson().getNameView() : "";
    }

    /**
     * Calculates the date the student withdrew based on student status neither active
     * nor active no primary. Withdrawal date is the date of the last record in the
     * enrollment table for this student.
     *
     * @param student Student
     * @return the student withdrawal date.
     */
    private PlainDate getWithdrawalDate(Student student) {
        PlainDate withdrawalDate = null;

        SubQuery query = null;
        if (!StudentManager.isActiveStudent(getOrganization(), student.getEnrollmentStatus())) {
            Criteria criteria = new Criteria();
            criteria.addEqualTo(StudentEnrollment.COL_ENROLLMENT_TYPE, StudentEnrollment.WITHDRAWAL);
            criteria.addEqualTo(StudentEnrollment.COL_STUDENT_OID, student.getOid());
            try {
                query = new SubQuery(StudentEnrollment.class, "MAX(" + StudentEnrollment.COL_ENROLLMENT_DATE + ")",
                        criteria);
                if (query != null) {
                    withdrawalDate = queryWithdrawalDate(query);
                }
            } catch (Exception exception) {
                getLogger().log(Level.WARNING,
                        "StudentLocator experienced an exception attempting to get the withdrawal date.");
            }
        }

        return withdrawalDate;
    }

    /**
     * Retrieve the withdrawal date from a report query,
     * safely convert type based on database type.
     * (OJB/Broker does not safely interpret column aggregate functions when doing this.
     * It generated an error.)
     *
     * @param query SubQuery
     * @return PlainDate
     */
    private PlainDate queryWithdrawalDate(SubQuery query) {
        ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);
        Object value = null;
        try {
            if (iterator.hasNext()) {
                value = ((Object[]) iterator.next())[0];
                if (value instanceof java.sql.Time) {
                    value = new PlainTime((java.sql.Time) value);
                } else if (value instanceof java.sql.Date) {
                    value = new PlainDate((java.sql.Date) value);
                } else if (value instanceof Timestamp) {
                    DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
                    DataDictionaryField field = dictionary.findDataDictionaryField(StudentEnrollment.class.getName(),
                            StudentEnrollment.COL_ENROLLMENT_DATE);
                    if (DataField.DATE_DATABASE_TYPE.equals(field.getDatabaseType())) {
                        value = new PlainDate((java.sql.Timestamp) value);
                    } else {
                        value = new PlainTime((java.sql.Timestamp) value);
                    }
                }
            }
        } finally {
            iterator.close();
        }

        return (PlainDate) value;
    }

    /**
     * Utility method to add a value to a grid. This method adds the value if it is not null. If the
     * value
     * is null, it adds an empty string.
     *
     * @param grid ReportDataGrid
     * @param key String
     * @param value Object
     */
    private void setMapValue(ReportDataGrid grid, String key, Object value) {
        grid.set(key, (value != null) ? value.toString() : "");
    }
}
