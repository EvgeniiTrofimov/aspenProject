/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2014 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.reports.bc;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.DatabaseOptimizer;
import com.follett.fsc.core.framework.persistence.DatabaseOptimizerFactory;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.Transcript;
import com.x2dev.sis.tools.reports.StudentContextReportHelper;
import com.x2dev.utils.DataGrid;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.DateAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import org.apache.commons.lang.exception.ExceptionUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Exports student course history (transcript) for BC's GDE.
 *
 * @author Follett Software Company
 */
public class StudentCourseHistoryExport extends GdeExportJavaSource {
    private static final long serialVersionUID = 1L;
    // Iput parameters
    private static final String PARAM_INCL_DL_INFO = "includeDLInfo";

    /**
     * The Enum EXPORT_FIELDS.
     */
    // Grid fields
    private enum EXPORT_FIELDS {
        FIELD_SCHOOL_ID("School Number", "Y", ""), FIELD_STUD_ID("Student Number", "Y", ""), FIELD_STUD_LAST_NAME(
                "Student Legal Last Name", "Y",
                ""), FIELD_STUD_FIRST_NAME("Student Legal First Name", "Y", ""), FIELD_COURSE_CODE("Course Code", "Y",
                        ""), FIELD_COURSE_DROP_DATE("Drop Date", "Y", ""), FIELD_TRN_HOW_TAKEN("How Taken Description",
                                "Y", ""), FIELD_COURSE_REPEAT_INDICATOR("Course Repeat Indicator", "Y",
                                        ""), FIELD_COURSE_FINAL_MARK("Final Mark", "Y", ""), FIELD_COMPLETED_DATE(
                                                "Completed Date", "Y", ""), FIELD_DL_START_DATE("DL Start Date", "N",
                                                        "DL"), FIELD_DL_COURSE_ACTIVE_DATE("DL Course Active Date", "N",
                                                                "DL"), FIELD_DL_COMPLETION_DATE("DL Completion Date",
                                                                        "N", "DL"), FIELD_SCHOOL_DISTRICT_NAME(
                                                                                "District Name", "Y",
                                                                                ""), FIELD_SCHOOL_NAME("School Name",
                                                                                        "Y", "");

        private String m_fieldName;
        private boolean m_required;
        private String m_fieldSetName;

        /**
         * Instantiates a new export fields.
         *
         * @param fieldName String
         * @param required String
         * @param setName String
         */
        private EXPORT_FIELDS(String fieldName, String required, String setName) {
            m_fieldName = fieldName;
            m_required = "Y".equals(required);
            m_fieldSetName = setName;
        }

        /**
         * Gets the field name.
         *
         * @return String
         */
        public String getFieldName() {
            return m_fieldName;
        }

        /**
         * Gets the required.
         *
         * @return boolean
         */
        public boolean getRequired() {
            return m_required;
        }

        /**
         * Checks if is DL field.
         *
         * @return true, if is DL field
         */
        public boolean isDLField() {
            return "DL".equals(m_fieldSetName);
        }
    }

    // Course aliases
    private static final String ALIAS_HOW_TAKEN_DESCR = "trn-how-taken";
    private static final String ALIAS_DL_START_DATE = "trn-start-date";
    private static final String ALIAS_DL_COURSE_COMPLETION_DATE = "trn-completion-date";
    private static final String ALIAS_COURSE_ACTIVE_DATE = "trn-course-active-date";
    private static final String ALIAS_DROP_DATE = "trn-drop-date";
    private static final String ALIAS_REPEAT_INDICATOR = "trn-course-repeat";

    private DateAsStringConverter m_dateConverter;
    private StudentContextReportHelper m_helper;
    private boolean m_IsIncludeDlInfo;
    private Map<String, Collection<String>> m_repeatedCourseMap;

    /**
     * Gather data.
     *
     * @return DataGrid
     * @throws Exception exception
     * @see com.x2dev.sis.tools.exports.ExportJavaSource#gatherData()
     */
    @Override
    protected DataGrid gatherData() throws Exception {
        DataGrid grid = new DataGrid() {
            private static final long serialVersionUID = 1L;

            @Override
            public void set(String column, Object value) {
                super.set(column, wrap((String) value));
            }
        };

        SimpleDateFormat formatter = new SimpleDateFormat("dd-MMM-yyyy");

        QueryByCriteria query = new QueryByCriteria(Transcript.class, buildCriteria());
        query.addOrderByAscending(Transcript.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_SCHOOL_ID);
        query.addOrderByAscending(Transcript.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_LOCAL_ID);
        query.addOrderByAscending(Transcript.COL_STUDENT_OID);

        QueryIterator transcripts = getBroker().getIteratorByQuery(query);
        try {
            int progressCount = 0;

            while (transcripts.hasNext()) {
                Transcript transcript = (Transcript) transcripts.next();

                boolean deleteRow = false;
                try {
                    SisStudent student = transcript.getStudent();
                    SisSchool school = transcript.getSchool();

                    if (++progressCount % 10000 == 0) {
                        AppGlobals.getLog()
                                .info("GDE Student Course History processed " + progressCount + " transcript records");
                    }

                    grid.append();
                    deleteRow = true;

                    // Fill grid data list with export information
                    grid.set(EXPORT_FIELDS.FIELD_SCHOOL_ID.getFieldName(), school.getSchoolId());
                    grid.set(EXPORT_FIELDS.FIELD_STUD_ID.getFieldName(), student.getLocalId());
                    grid.set(EXPORT_FIELDS.FIELD_STUD_LAST_NAME.getFieldName(), student.getPerson().getLastName());
                    grid.set(EXPORT_FIELDS.FIELD_STUD_FIRST_NAME.getFieldName(), student.getPerson().getFirstName());
                    grid.set(EXPORT_FIELDS.FIELD_COURSE_DROP_DATE.getFieldName(),
                            prepareDate((String) transcript.getFieldValueByAlias(ALIAS_DROP_DATE), formatter));
                    grid.set(EXPORT_FIELDS.FIELD_TRN_HOW_TAKEN.getFieldName(),
                            transcript.getFieldValueByAlias(ALIAS_HOW_TAKEN_DESCR));
                    grid.set(EXPORT_FIELDS.FIELD_COURSE_REPEAT_INDICATOR.getFieldName(),
                            transcript.getFieldValueByAlias(ALIAS_REPEAT_INDICATOR));
                    grid.set(EXPORT_FIELDS.FIELD_COURSE_FINAL_MARK.getFieldName(), transcript.getFinalGrade());
                    grid.set(EXPORT_FIELDS.FIELD_SCHOOL_DISTRICT_NAME.getFieldName(),
                            school.getParentOrganization().getName());
                    grid.set(EXPORT_FIELDS.FIELD_SCHOOL_NAME.getFieldName(), school.getName());

                    /*
                     * Set school course-based fields
                     */
                    SchoolCourse schoolCourse = transcript.getSchoolCourse();
                    if (schoolCourse != null) {
                        String courseNumber = schoolCourse.getCourse().getRootCourse().getNumber();

                        /*
                         * Get repeat indicator
                         */
                        String repeated = "N";

                        Collection<String> repeats = m_repeatedCourseMap.get(student.getOid());
                        if (repeats != null && repeats.contains(courseNumber)) {
                            repeated = "Y";
                        }

                        grid.set(EXPORT_FIELDS.FIELD_COURSE_CODE.getFieldName(), courseNumber);
                        grid.set(EXPORT_FIELDS.FIELD_COURSE_REPEAT_INDICATOR.getFieldName(), repeated);
                    }

                    /*
                     * Set completion date
                     */
                    String completionDate = prepareDate(
                            (String) transcript.getFieldValueByAlias(ALIAS_DL_COURSE_COMPLETION_DATE), formatter);
                    grid.set(EXPORT_FIELDS.FIELD_COMPLETED_DATE.getFieldName(), completionDate);

                    /*
                     * Optional DL fields
                     */
                    if (m_IsIncludeDlInfo) {
                        grid.set(EXPORT_FIELDS.FIELD_DL_START_DATE.getFieldName(),
                                prepareDate((String) transcript.getFieldValueByAlias(ALIAS_DL_START_DATE), formatter));
                        grid.set(EXPORT_FIELDS.FIELD_DL_COURSE_ACTIVE_DATE.getFieldName(), prepareDate(
                                (String) transcript.getFieldValueByAlias(ALIAS_COURSE_ACTIVE_DATE), formatter));
                        grid.set(EXPORT_FIELDS.FIELD_DL_COMPLETION_DATE.getFieldName(), completionDate);
                    }
                } catch (NullPointerException npe) {
                    StringBuilder strBldr = new StringBuilder();
                    strBldr.append("Unable to export ");
                    strBldr.append(query.getClass().getName());
                    strBldr.append(" with OID: [");
                    strBldr.append(transcript.getOid());
                    SisStudent student = transcript.getStudent();
                    if (student != null) {
                        strBldr.append("] for the Student with Local ID: [");
                        strBldr.append(student.getLocalId());
                        strBldr.append("].");
                    } else {
                        strBldr.append("] as it has no related Student.");
                    }


                    // deleteRow is true if an incomplete row has been added to the grid from
                    // grid.append()
                    if (!deleteRow) {
                        strBldr.append("Null encountered before adding to export.");
                    } else {
                        strBldr.append("Null encountered when setting Columns.");
                        grid.deleteRow(); // Delete the incomplete row that was appended to the
                                          // grid.
                    }

                    strBldr.append("\n\n\nNullPointerException: \n");
                    strBldr.append(ExceptionUtils.getStackTrace(npe));
                    logToolMessage(Level.WARNING, strBldr.toString(), false);
                }
            }
        } finally {
            transcripts.close();
        }

        grid.beforeTop();
        return grid;
    }

    /**
     * Gets the column names.
     *
     * @return List
     * @see com.x2dev.sis.tools.exports.ExportJavaSource#getColumnNames()
     */
    @Override
    protected List<?> getColumnNames() {
        return getColumnNamesList();
    }

    /**
     * Gets the column user names.
     *
     * @return List
     * @see com.x2dev.sis.tools.exports.ExportJavaSource#getColumnUserNames()
     */
    @Override
    protected List<?> getColumnUserNames() {
        return getColumnNamesList();
    }

    /**
     * Gets the comment.
     *
     * @return String
     * @see com.x2dev.sis.tools.exports.ExportJavaSource#getComment()
     */
    @Override
    protected String getComment() {
        return null;
    }

    /**
     * Gets the header.
     *
     * @return String
     * @see com.x2dev.sis.tools.exports.ExportJavaSource#getHeader()
     */
    @Override
    protected String getHeader() {
        return null;
    }

    /**
     * Initialize.
     *
     * @see com.x2dev.sis.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        super.initialize();
        m_helper = new StudentContextReportHelper(getOrganization(), getCurrentContext(), getBroker());

        m_IsIncludeDlInfo = ((Boolean) getParameter(PARAM_INCL_DL_INFO)).booleanValue();

        loadRepeats();

        m_dateConverter = (DateAsStringConverter) ConverterFactory.getConverterForClass(Converter.DATE_CONVERTER,
                getLocale(), true);
    }

    /**
     * Builds export criteria.
     * 
     * @return Criteria
     */
    private Criteria buildCriteria() {
        X2Criteria criteria = new X2Criteria();

        // Not sure if this is the right rel path, this criteria uses a helper
        criteria.addAndCriteria(getSchoolCriteria().copyWithAdjustedPath(
                Transcript.REL_STUDENT + PATH_DELIMITER + m_helper.getSchoolRelationship(),
                Transcript.REL_STUDENT + PATH_DELIMITER + m_helper.getSchoolOidField()));

        if (getSchool() != null) {
            criteria.addOrCriteria(StudentManager.getSecondaryStudentCriteria(
                    Transcript.REL_STUDENT + PATH_DELIMITER + SisStudent.REL_STUDENT_SCHOOLS +
                            PATH_DELIMITER,
                    getCurrentContext().getOid(), getSchool().getOid(), null, null,
                    getBroker().getPersistenceKey()));
        }

        return criteria;
    }

    /**
     * Generates the SQL for restricting the included student based on the user input. The intention
     * is to avoid the use
     * of IN clauses when an organization is selected.
     * 
     * @return String
     */
    private String generateStudentCriteria() {
        String clause = "";

        if (getSchool() != null) {
            clause = m_helper.getSchoolOidColumn() + " = '" + getSchool().getOid() + "' ";
        } else {
            String oids = getSelectedSchoolOids();
            if (!StringUtils.isEmpty(oids)) {
                Collection<String> oidList = StringUtils.convertDelimitedStringToList(oids, ',', true);
                clause = m_helper.getSchoolOidColumn() + " IN ("
                        + StringUtils.convertCollectionToDelimitedString(oidList, ",", "'") + ") ";
            } else {
                clause = "STD_ORG_OID_" + (getOrganization().getOrganizationDefinition().getLevel() + 1) + " = '"
                        + getOrganization().getOid() + "' ";
            }
        }

        return clause;
    }

    /**
     * Create list of Column names.
     *
     * @return List<String>
     */
    private List<String> getColumnNamesList() {
        List<String> columnNames = new ArrayList<String>(EXPORT_FIELDS.values().length);

        for (EXPORT_FIELDS field : EXPORT_FIELDS.values()) {
            if (field.getRequired()) {
                columnNames.add(field.getFieldName());
            } else {
                if (m_IsIncludeDlInfo && field.isDLField()) {
                    columnNames.add(field.getFieldName());
                }
            }
        }

        return columnNames;
    }


    /**
     * Loads the map of repeated course numbers by student. This is done via SQL due to the
     * complexity of joining to the
     * parent course to get the course number using SQL Server.
     */
    private void loadRepeats() {
        m_repeatedCourseMap = new HashMap<>(4096);

        DatabaseOptimizer optimizer = DatabaseOptimizerFactory.getDatabaseOptimizer(getBroker().getPersistenceKey());
        String ifClause = optimizer.sqlIf("parent.CRS_COURSE_NUMBER is null", "child.CRS_COURSE_NUMBER",
                "parent.CRS_COURSE_NUMBER");

        StringBuilder sql = new StringBuilder(500);
        sql.append("SELECT TRN_STD_OID, ").append(ifClause).append(" as course_number, COUNT(*) ");
        sql.append("  FROM STUDENT_TRANSCRIPT ");
        sql.append(" INNER JOIN STUDENT ON TRN_STD_OID = STD_OID ");
        sql.append(" INNER JOIN COURSE_SCHOOL ON TRN_CSK_OID = CSK_OID ");
        sql.append(" INNER JOIN COURSE child ON CSK_CRS_OID = child.CRS_OID ");

        if (m_helper.isContextOverride()) {
            sql.append(" INNER JOIN STUDENT_CONTEXT_ATTRIBUTES ON STD_SXA_OID_CURRENT = SXA_OID ");
        }

        sql.append(" LEFT OUTER JOIN COURSE parent ON child.CRS_CRS_OID_PARENT = parent.CRS_OID ");
        sql.append(" WHERE ").append(generateStudentCriteria());
        sql.append(" GROUP BY TRN_STD_OID, ").append(ifClause);
        sql.append(" HAVING COUNT(*) >= 2 ");

        Connection connection = getBroker().borrowConnection();
        Statement statement = null;
        ResultSet results = null;

        try {
            statement = connection.createStatement();
            results = statement.executeQuery(sql.toString());

            while (results.next()) {
                String studentOid = results.getString(1);
                String courseNumber = results.getString(2);

                Collection<String> repeats = m_repeatedCourseMap.get(studentOid);
                if (repeats == null) {
                    repeats = new LinkedList<>();
                    m_repeatedCourseMap.put(studentOid, repeats);
                }

                repeats.add(courseNumber);
            }
        } catch (SQLException sqle) {
            AppGlobals.getLog().severe("SQL Exception loading repeats: " + sqle.getMessage());
        } finally {
            try {
                if (results != null) {
                    results.close();
                }

                if (statement != null) {
                    statement.close();
                }
            } catch (SQLException sqle) {
                // Do nothing
            }

            getBroker().returnConnection();
        }
    }

    /**
     * Get String from UDF field convert it to date and format it.
     *
     * @param dateVal String
     * @param formatter SimpleDateFormat
     * @return String
     */
    private String prepareDate(String dateVal, SimpleDateFormat formatter) {
        String formattedDate = null;

        if (formatter != null && dateVal != null) {
            PlainDate date = (PlainDate) m_dateConverter.parseSystemString(dateVal);
            formattedDate = formatter.format(date);
        }

        return formattedDate;
    }
}
