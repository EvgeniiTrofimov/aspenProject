/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2017 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.reports.bc;

import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.web.AppGlobals;
import java.sql.Connection;
import java.sql.Date;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;


/**
 * The Class StudentLocatorDAO.
 */
public class StudentLocatorDAO {
    public static final String DB_SERVER_KEY = "LINKED_DB_SERVER";
    private static final String KEY_COLUMN_NAME = "PEN";
    private static final String SCHOOL_ID_KEY = "SCHOOL_ID";
    private static final String AND_STRING = " and ";
    private static final String FIRST_NAME_COLUMN = "FIRST_NAME";
    private static final String LAST_NAME_COLUMN = "LAST_NAME";
    private static final String GENDER_COLUMN = "GENDER";
    private static final String BIRTHDATE_COLUMN = "BIRTHDATE";
    private static final String STUDENT_ID_COLUMN = "STUDENT_ID";

    private static final String SELECT_STUDENT_COURSE_STATEMENT = "SELECT  * FROM BCESIS.ESIS_STUDENT_COURSE_VIEW ";
    private static final String SELECT_STATEMENT = "SELECT  * FROM BCESIS.ESIS_STUDENT_SCHOOL_VIEW  ";

    private static final String SELECT_PEN_STATEMENT_SQLSERVER_PREFIX =
            " SELECT PEN  COLLATE SQL_Latin1_General_CP1_CS_AS FROM OPENQUERY (";
    private static final String SELECT_PEN_STATEMENT_SQLSERVER_SUFFIX =
            ", 'SELECT DISTINCT PEN FROM BCESIS.ESIS_STUDENT_COURSE_VIEW') ";
    private static final String SELECT_PEN_STATEMENT_MYSQL = " SELECT PEN FROM BCESIS_STUDENT_COURSE_VIEW";


    private static final String SELECT_OPENQUERY_PREFIX = " SELECT * FROM OPENQUERY (";
    private static final String SELECT_OPENQUERY_SUFFIX = "') ";

    private static final String WHERE_STRING = " WHERE ";
    private static final String EQUALS_CONDITION_VALUE = " = ";
    private X2Broker m_broker;
    private Logger m_logger;
    private String m_dbServerName;
    private String m_quotes;
    private boolean m_isMySQL;


    /**
     * Constructor with no parameters in the event the DAO is used outside of the X2 Broker context,
     * e.g., with JDBC.
     *
     * @author X2 Development Corporation
     */
    public StudentLocatorDAO() {
        //
    }

    /**
     * The DAO constructor caches the the X2Broker for access to database connections.
     *
     * @author X2 Development Corporation
     * @param broker X2Broker
     * @param dbServerName String
     */
    public StudentLocatorDAO(X2Broker broker, String dbServerName) {
        m_dbServerName = dbServerName;
        m_broker = broker;
        m_isMySQL = m_broker.getPersistenceKey().getDbms().toLowerCase().contains("mysql");
        m_quotes = (m_isMySQL) ? "'" : "''";
    }

    /**
     * gets the query string used to get the list of all student PENS in the eSIS database .
     *
     * @return the query string
     */
    public String getQueryForPEN() {
        String dbmsName = m_broker.getPersistenceKey().getDbms();
        String penQueryString = (dbmsName.toLowerCase().contains("mysql"))
                ? SELECT_PEN_STATEMENT_MYSQL
                : SELECT_PEN_STATEMENT_SQLSERVER_PREFIX + m_dbServerName + SELECT_PEN_STATEMENT_SQLSERVER_SUFFIX;
        return penQueryString;
    }

    /**
     * gets a list of courses from the esis_student_course_view for the student whose pen matches
     * studentId .
     *
     * @param studentId String
     * @return a list of maps, each containing one course record
     */
    public List<Map> getStudentCourseData(String studentId) {
        StringBuffer queryBuffer = new StringBuffer(SELECT_STUDENT_COURSE_STATEMENT).append(WHERE_STRING);
        queryBuffer = queryBuffer.append(KEY_COLUMN_NAME).append(EQUALS_CONDITION_VALUE);
        queryBuffer = appendQuoted(queryBuffer, studentId);
        return queryStudentData(makeOpenQuery(queryBuffer.toString()));
    }

    /**
     * queries the database for the student identified by studentStateId. This method is
     * useful for finding a specific student record from eSIS such as when searching for a
     * student who has been registered to a holding school in Aspen.
     *
     * @param studentStateId String
     * @return a map of the eSIS record for the student identified by studentStateId
     */
    public Map<String, ?> getStudentData(String studentStateId) {
        String query = createStudentQuery(null, null, null, studentStateId, null, -1);
        List<Map> students = queryStudentData(query);
        return (students.size() > 0) ? (Map<String, ?>) students.get(0) : null;
    }

    /**
     * Searches for a student in the database whose name, gender, birthdate and/or student ID
     * matches the parameters
     * passed to this method.
     *
     * @author X2 Development Corporation
     * @param firstName search param. Not used if null
     * @param lastName search param. Not used if null
     * @param gender search param. Not used if null
     * @param studentID search param. Not used if null
     * @param birthDate search param. Not used if null
     * @param dateRange search param. Not used if null
     * @return A map containing a list of records for each unique student id in the database. Each
     *         student may
     *         have multiple records, one for each subject, which will be stored in a map and added
     *         to a
     *         list
     *         for that student. The key for the map returned in this method is the student ID and
     *         the value
     *         is
     *         the list of maps, each of which represents on record from the database.
     *
     * @see mapResult(ResultSetMetaDataa, ResultSet)
     */
    public Map<String, List<Object>> getStudentData(String firstName,
                                                    String lastName,
                                                    String gender,
                                                    String studentID,
                                                    Calendar birthDate,
                                                    int dateRange) {
        String query = createStudentQuery(firstName, lastName, gender, studentID, birthDate, dateRange);
        List<Map> students = queryStudentData(query);
        return listToMap(students);
    }

    /**
     * Looks in the studentsMap for a student's ID and, if found, returns that list. If not found,
     * this method creates a new list and adds it to the studentsMap before returning the new list
     * to the caller.
     *
     * @author X2 Development Corporation
     * @param studentsMap Map<String,List<Object>>
     * @param studentID String
     * @return List
     */
    public List<Object> getStudentMapList(Map<String, List<Object>> studentsMap, String studentID) {
        List<Object> studentMapList = studentsMap.get(studentID);
        if (studentMapList == null) {
            studentMapList = new ArrayList<Object>();
            studentsMap.put(studentID, studentMapList);
        }
        return studentMapList;
    }

    /**
     * calls getStudentData(studentStateId) to obtain the student record for the student
     * identified by studentStateId. If found, this method gets the value for the school's
     * ministry number identified in the map of the student's record by SCHOOL_ID_KEY.
     *
     * @param studentStateId String
     * @return a string representing the state id of the student's school
     */
    public String getStudentSchoolID(String studentStateId) {
        Map<String, ?> esisStudentMap = getStudentData(studentStateId);
        String esisSchool = null;
        if (esisStudentMap != null) {
            esisSchool = (String) esisStudentMap.get(SCHOOL_ID_KEY);
        }
        return esisSchool;
    }

    /**
     * Converts unordered list of records into a map containing one entry for each student id. The
     * student id
     * is the key for the entry. The value is a list containing one or more maps, each representing
     * a result
     * from the database for this student id.
     *
     * @author X2 Development Corporation
     * @param students the unordered list of student records returned from the database.
     * @return Map
     */
    public Map<String, List<Object>> listToMap(List<Map> students) {
        Map<String, List<Object>> studentsMap = new HashMap<String, List<Object>>();
        for (Object student : students) {
            // One student/course record is represented by a map whose keys are the column names
            // from the database
            // The Map of all students contains each student's student/course record, keyed to
            // Student ID. Each student
            // will be represented in the map by a list containing one or more student/course maps
            // for that student.
            getStudentMapList(studentsMap, (String) ((Map<String, ?>) student).get(KEY_COLUMN_NAME)).add(student);
        }
        return studentsMap;
    }

    /**
     * Adds the condition to the query if the param is not null.
     *
     * @author X2 Development Corporation
     * @param queryBuffer the buffer to add the query to.
     * @param columnName the name of the column to search for this parameter
     * @param value the value to search for in the column identified by columnName
     * @return the buffer containing the query string with the condition added to
     *         if the value for this parameter is not null
     */
    private StringBuffer addCondition(StringBuffer queryBuffer, String columnName, String value) {
        if (value != null) {
            String columnNameLowerCase = new StringBuffer("LOWER(").append(columnName).append(")").toString();
            prefaceCondition(queryBuffer);
            queryBuffer = queryBuffer.append(columnNameLowerCase).append(EQUALS_CONDITION_VALUE);
            queryBuffer = appendQuoted(queryBuffer, value.toLowerCase());
        }
        return queryBuffer;
    }

    /**
     * Adds the date condition to the query if the dateValue is not null. Permits searching on a
     * range of dates
     * specified by dateRange.
     *
     * @author X2 Development Corporation
     * @param queryBuffer the buffer to containing the query to add the date condition to
     * @param columnName the name of the date column to be searched
     * @param dateValue the date to search for
     * @param dateRange indicates the number of days before or after the date specified to search
     *        for
     * @return the buffer containing the query with the date range condition added if the dateValue
     *         is not null
     */
    private StringBuffer addDateRangeCondition(StringBuffer queryBuffer,
                                               String columnName,
                                               Calendar dateValue,
                                               int dateRange) {
        if (dateValue != null) {
            SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");
            Date startDate = addDays((Calendar) dateValue.clone(), -1 * dateRange);
            Date endDate = addDays((Calendar) dateValue.clone(), dateRange);
            prefaceCondition(queryBuffer);
            queryBuffer.append(columnName + " BETWEEN ");
            if (m_isMySQL) {
                queryBuffer = appendQuoted(queryBuffer, sdf.format(startDate));
                queryBuffer = queryBuffer.append(AND_STRING);
                queryBuffer = appendQuoted(queryBuffer, sdf.format(endDate));
            } else {
                queryBuffer = queryBuffer.append(" TO_DATE(");
                queryBuffer = appendQuoted(queryBuffer, sdf.format(startDate)).append(", ");
                queryBuffer = appendQuoted(queryBuffer, "YYYY-MM-DD").append(") ");
                queryBuffer = queryBuffer.append(AND_STRING);
                queryBuffer = queryBuffer.append(" TO_DATE(");
                queryBuffer = appendQuoted(queryBuffer, sdf.format(endDate)).append(", ");
                queryBuffer = appendQuoted(queryBuffer, "YYYY-MM-DD").append(") ");
            }
        }
        return queryBuffer;
    }

    /**
     * A utility method that adds a number of days to a calendar date and returns a date value
     * representing this new date.
     *
     * @author X2 Development Corporation
     * @param cal the calendar date to add days to
     * @param days the number of days to add
     * @return the newly calculated date
     */
    private Date addDays(Calendar cal, int days) {
        cal.add(Calendar.DATE, days);
        Date newDate = new Date(cal.getTime().getTime());
        return newDate;
    }

    /**
     * appends a string value wrapped with quotation marks appropriate to the target database to a
     * StringBuffer.
     * The StringBuffer consists of the base query and any additional constraints that have already
     * been
     * appended.
     *
     * @author X2 Development Corporation
     * @param buffer buffer containing the query string
     * @param value a string representing the value to constrain the query.
     * @return StringBuffer
     */
    private StringBuffer appendQuoted(StringBuffer buffer, String value) {
        buffer.append(" ").append(m_quotes).append(value).append(m_quotes).append(" ");
        return buffer;
    }

    /**
     * This method creates a query for a prepared statement based on the search parameters passed to
     * the
     * {@link #getStudentData(String, String, String, String, Calendar, int)}.
     *
     * @author X2 Development Corporation
     * @param firstName search param. Not used if null
     * @param lastName search param. Not used if null
     * @param gender search param. Not used if null
     * @param studentID search param. Not used if null
     * @param birthDate search param. Not used if null
     * @param dateRange range of birthdates seached for is birthDate is not null.
     * @return the query formed from the search parameters of the request.
     */
    private String createStudentQuery(String firstName,
                                      String lastName,
                                      String gender,
                                      String studentID,
                                      Calendar birthDate,
                                      int dateRange) {
        // if parameter is null, addCondition() will skip that parameter.
        StringBuffer queryBuffer = new StringBuffer(SELECT_STATEMENT);
        addCondition(queryBuffer, FIRST_NAME_COLUMN, firstName);
        addCondition(queryBuffer, LAST_NAME_COLUMN, lastName);
        addCondition(queryBuffer, GENDER_COLUMN, gender);
        addCondition(queryBuffer, STUDENT_ID_COLUMN, studentID);
        addDateRangeCondition(queryBuffer, BIRTHDATE_COLUMN, birthDate, dateRange);
        return makeOpenQuery(queryBuffer.toString());
    }


    /**
     * Make open query.
     *
     * @param query String
     * @return String
     */
    private String makeOpenQuery(String query) {
        String newQuery = query;
        if (!m_isMySQL) {
            StringBuffer openQueryBuffer = new StringBuffer(SELECT_OPENQUERY_PREFIX);
            openQueryBuffer =
                    openQueryBuffer.append(m_dbServerName).append(", '").append(query).append(SELECT_OPENQUERY_SUFFIX);
            newQuery = openQueryBuffer.toString();
        }
        return newQuery;
    }

    /**
     * Ensures that the logger is not null when logging messages.
     *
     * @return a reference to m_logger, a Logger instance.
     */
    private Logger getLogger() {
        return (m_logger == null) ? AppGlobals.getLog() : m_logger;
    }

    /**
     * Creates a Map containing the fields from the current record of the result set. The metadata
     * object is obtained
     * from the ResultSet and is used to determine the column name to use as the key to add the
     * column value to the Map.
     *
     * @author X2 Development Corporation
     * @param meta the metadata object that contains the column names for the result set
     * @param result the result set containing the current record
     * @return the Map containing the column names and values of the current record
     */
    private Map<String, Object> mapResult(ResultSetMetaData meta, ResultSet result) {
        Map<String, Object> map = new HashMap<String, Object>();
        try {
            for (int i = 1; i <= meta.getColumnCount(); i++) {
                Object obj = result.getObject(i);
                map.put(meta.getColumnName(i).toUpperCase(), obj);
            }
        } catch (SQLException e) {
            e.printStackTrace();
        }
        return map;
    }

    /**
     * Prepends with <b>WHERE</b> or <b>AND</b> the query condition depending on whether this is the
     * first condition
     * added or a subsequent condition respectively.
     *
     * @author X2 Development Corporation
     * @param queryBuffer the buffer containing the query to that the condition prefix needs to be
     *        added to.
     * @return the buffer containing the query with the condition prefix added
     */
    private StringBuffer prefaceCondition(StringBuffer queryBuffer) {
        if (queryBuffer.length() == SELECT_STATEMENT.length()) {
            queryBuffer = queryBuffer.append(WHERE_STRING);
        } else {
            queryBuffer = queryBuffer.append(AND_STRING);
        }
        return queryBuffer;
    }

    /**
     * Common method used by {@link #getStudentData()} and {@link #getStudentData(String, String,
     * String, String, Calendar, int)}
     * to perform query against the database. This method passes the result set to {@link
     * mapResult(meta, result)} to convert the
     * result to a Map with the database column name as the key.
     *
     * @author X2 Development Corporation
     * @param query A query formatted for a PreparedStatement.
     * @return A list of maps, one for each result in the result set. The keyset for each map
     *         is the set of column names returned in the metadata of the resultset.
     *
     */
    private List<Map> queryStudentData(String query) {
        List<Map> students = new ArrayList<Map>();
        Connection conn = m_broker.borrowConnection();
        // Connection conn = (broker != null)
        // ? broker.borrowConnection()
        // : BaseDAO.getConnection();
        PreparedStatement statement = null;
        try {
            statement = conn.prepareStatement(query);
            ResultSet result = statement.executeQuery();
            ResultSetMetaData meta = result.getMetaData();
            while (result.next()) {
                Map<String, Object> resultMap = mapResult(meta, result);
                students.add(resultMap);
            }
        } catch (SQLException e) {
            getLogger().log(Level.SEVERE,
                    "\r\n        *******************************************************************************\r\n" +
                            "        **                                                                              \r\n"
                            +
                            "        ** StudentLocatorDAO experienced an exception attempting to access the database.\r\n"
                            +
                            "        ** This could mean that the database connection has be dropped. Alternatively, \r\n"
                            +
                            "        ** if there has been a change to the database, this could mean that application \r\n"
                            +
                            "        ** is not in sync with the database structure.\r\n" +
                            "        ** The query attempted was: \r\n" +
                            "        ** " + query + "\r\n" +
                            "        **                                                                              \r\n"
                            +
                            "        *******************************************************************************");
            e.printStackTrace();
        } catch (Exception e) {
            getLogger().log(Level.SEVERE,
                    "An unknown error occurred attempting to access the BCeSIS database. A stacktrace " +
                            "of the error follow.");
            getLogger().log(Level.WARNING, "The query attempted was: \r\n" + query);
            e.printStackTrace();
        } finally {
            if (conn != null) {
                m_broker.returnConnection();
            }
        }
        return students;
    }
}
