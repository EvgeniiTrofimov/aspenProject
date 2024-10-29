/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2004 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

import com.follett.fsc.core.k12.beans.DataFieldConfig;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.sis.model.beans.GradeScale;
import com.x2dev.sis.model.beans.GradeScaleGradeDefinition;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.TranscriptColumnDefinition;
import com.x2dev.sis.model.business.GradesManager;
import com.x2dev.utils.StringUtils;
import java.math.BigDecimal;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares the data for the "Grade Distribution" report.
 *
 * @author X2 Development Corporation
 */
public class GradeDistributionData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Name for the enumerated "course select" input parameter. The value is an Integer.
     */
    public static final String CONTEXT_OID_PARAM = "contextOid";

    /**
     * Name for the enumerated "convert numeric" input parameter. The value is a Boolean.
     */
    public static final String CONVERT_PARAM = "convertNumeric";

    /**
     * Name for the enumerated "course type" input parameter. The value is an Integer.
     */
    public static final String COURSE_TYPE_PARAM = "courseType";

    /**
     * Name for the enumerated "sort" input parameter. The value is an Integer.
     */
    public static final String GROUP_PARAM = "groupBy";

    /**
     * Name for the enumerated "sort" input parameter. The value is an Integer.
     */
    public static final String STUDENT_PARAM = "studentSelect";

    /**
     * Name for the enumerated "sort" input parameter. The value is an Integer.
     */
    public static final String STUDENT_STRING_PARAM = "studentSelectString";

    /**
     * Name for the enumerated "grade select" input parameter. The value is an Integer.
     */
    public static final String TRANSCRIPT_COLUMN_PARAM = "transcriptColumnDefinitionOid";

    // Report parameters
    private static final String PARAMETER_CONTEXT = "yearContext";
    private static final String PARAMETER_GRADE_MAP = "gradeMap";
    private static final String PARAMETER_TRANSCRIPT_COLUMN = "transcriptColumn";

    // Grid fields
    private static final String FIELD_BREAK_BY = "breakBy";
    private static final String FIELD_CATEGORY = "category";
    private static final String FIELD_TOTAL = "total";

    private TranscriptColumnDefinition m_column;
    private Map m_gradeToNumber;
    private Map m_numberToGrade;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        ReportDataGrid grid = new ReportDataGrid(2000, 30);
        GradesManager gradesManager = new GradesManager(getBroker());

        boolean convertNumeric = ((Boolean) getParameter(CONVERT_PARAM)).booleanValue();

        /*
         * Year selection
         */
        String contextOid = (String) getParameter(CONTEXT_OID_PARAM);
        String yearOption = "AND TRN_CTX_OID = '" + contextOid + "' ";
        DistrictSchoolYearContext context =
                (DistrictSchoolYearContext) getBroker().getBeanByOid(DistrictSchoolYearContext.class, contextOid);

        /*
         * Grade selection
         */
        String transcriptColumnOid = (String) getParameter(TRANSCRIPT_COLUMN_PARAM);
        String gradeOption = "AND GTC_OID = '" + transcriptColumnOid + "' ";
        m_column = (TranscriptColumnDefinition) getBroker().getBeanByOid(TranscriptColumnDefinition.class,
                transcriptColumnOid);

        loadColumnMap();

        /*
         * Course type selection
         */
        String courseOption;
        int courseType = ((Integer) getParameter(COURSE_TYPE_PARAM)).intValue();
        switch (courseType) {
            case 1: // Major
                courseOption = "AND CSK_HONOR_ROLL_TYPE = 'Major' ";
                break;

            case 2: // Minor
                courseOption = "AND CSK_HONOR_ROLL_TYPE = 'Minor' ";
                break;

            default: // All
                courseOption = "";
                break;
        }

        /*
         * Student selection
         */
        String studentOption;
        int studentSelect = ((Integer) getParameter(STUDENT_PARAM)).intValue();
        switch (studentSelect) {
            case 1: // Yog
                studentOption = "AND STD_YOG = " + getParameter(STUDENT_STRING_PARAM) + " ";
                break;

            case 2: // Snapshot
                String subQuery = ReportUtils.getRecordSetSqlSubQuery(SisStudent.DICTIONARY_ID,
                        (String) getParameter(STUDENT_STRING_PARAM),
                        getUser(),
                        getSchool(),
                        getOrganization());
                studentOption = "AND STD_OID IN (" + subQuery + ") ";
                break;

            default: // All
                studentOption = "";
                break;
        }

        /*
         * Group by fields
         */
        String field1;
        String field2 = null;
        String additionalField = null;
        String queryFields = null;

        int groupBy = ((Integer) getParameter(GROUP_PARAM)).intValue();
        switch (groupBy) {
            case 1: // Department
                field1 = "CRS_DEPARTMENT_CODE";
                break;

            case 2: // Teacher
                field1 = "MST_STAFF_VIEW";
                break;

            case 3: // Teacher/Section
                field1 = "MST_STAFF_VIEW";
                field2 = "MST_COURSE_VIEW";
                additionalField = "MST_DESCRIPTION";
                break;

            case 4: // Course/Teacher
                field1 = "CSK_COURSE_NUMBER";
                field2 = "MST_STAFF_VIEW";
                additionalField = "MST_DESCRIPTION";
                break;

            default: // Overall
                field1 = "SKL_SCHOOL_ID";
                additionalField = "SKL_SCHOOL_NAME";
                break;
        }

        /*
         * Fields 1 & 2 are necessary for the grouping. Additional field provides additional
         * information for a helpful display.
         */
        queryFields = field1;
        if (!StringUtils.isEmpty(field2)) {
            queryFields += ", " + field2;
        } else {
            field2 = field1;
        }

        if (!StringUtils.isEmpty(additionalField)) {
            queryFields += ", " + additionalField;
        }

        /*
         * Build query
         */
        DataFieldConfig transcriptDataField = m_column.getDataFieldConfig();
        String transcriptColumn = transcriptDataField.getDataField().getDatabaseName();

        StringBuilder sql = new StringBuilder(128);
        sql.append("SELECT " + queryFields + ", " + transcriptColumn + ", ");
        sql.append("count(" + transcriptColumn + ") as total ");
        sql.append("FROM STUDENT_TRANSCRIPT ");

        sql.append(" INNER JOIN COURSE_SCHOOL ON TRN_CSK_OID = CSK_OID ");
        sql.append(" INNER JOIN COURSE ON CSK_CRS_OID = CRS_OID ");
        sql.append(" INNER JOIN SCHOOL ON TRN_SKL_OID = SKL_OID ");
        sql.append(" INNER JOIN SCHEDULE_MASTER ON TRN_MST_OID = MST_OID ");
        sql.append(" INNER JOIN GRADE_TRANS_COLUMN_DEFINITION ON GTC_GTD_OID = TRN_GTD_OID ");
        sql.append(" INNER JOIN STUDENT ON TRN_STD_OID = STD_OID ");

        sql.append("WHERE TRN_SKL_OID = ? ");
        sql.append("  AND TRN_GTD_OID = ? ");
        sql.append(yearOption);
        sql.append(courseOption);
        sql.append(gradeOption);
        sql.append(studentOption);
        sql.append("GROUP BY " + transcriptColumn + ", " + queryFields + " ");
        sql.append("ORDER BY " + queryFields + ", " + transcriptColumn);

        Connection connection = getBroker().borrowConnection();
        try {
            PreparedStatement statement = connection.prepareStatement(sql.toString());
            try {
                statement.setString(1, getSchool().getOid());
                statement.setString(2, m_column.getTranscriptDefinitionOid());

                ResultSet resultSet = statement.executeQuery();
                try {
                    String lastDetail = null;
                    String lastCategory = null;

                    while (resultSet.next()) {
                        String detail = resultSet.getString(field2);
                        String additionalDetail = "";
                        if (!StringUtils.isEmpty(additionalField)) {
                            additionalDetail = resultSet.getString(additionalField);
                        }

                        int currentTotal = resultSet.getInt("total");

                        /*
                         * If we run across a numeric grade and the option to convert is TRUE, we
                         * need
                         * to convert it to a letter grade.
                         */
                        String currentGrade = resultSet.getString(transcriptColumn);
                        if (StringUtils.isNumeric(currentGrade) && convertNumeric) {
                            GradeScale scale = m_column.getGradeScale();
                            BigDecimal grade = new BigDecimal(currentGrade);
                            currentGrade = gradesManager.getLetterValue(grade, scale, (SisSchool) getSchool(), null);
                        }

                        /*
                         * Determine category header.
                         */
                        String category;
                        switch (groupBy) {
                            case 1: // Department
                                category = "Department";
                                break;

                            case 2: // Teacher
                                category = "Teacher";
                                break;

                            case 3: // Teacher/Section
                                category = resultSet.getString(field1);
                                detail += " " + additionalDetail;
                                break;

                            case 4: // Course/Teacher
                                category = resultSet.getString(field1) + " " + additionalDetail;
                                break;

                            default: // Overall
                                category = "";
                                detail += " - " + additionalDetail;
                                break;
                        }

                        /*
                         * Check if we reach a new detail or category.
                         */
                        if ((lastDetail == null || detail == null || !detail.equals(lastDetail)) ||
                                (lastCategory == null || !category.equals(lastCategory))) {
                            grid.append();
                            grid.set(FIELD_BREAK_BY, detail);
                            grid.set(FIELD_TOTAL, Integer.valueOf(0));
                            grid.set(FIELD_CATEGORY, category);
                        }

                        // Always update total
                        int total = ((Integer) grid.get(FIELD_TOTAL)).intValue();
                        grid.set(FIELD_TOTAL, Integer.valueOf(total + currentTotal));

                        // Set grid for current grade
                        String gridEntry = (String) m_gradeToNumber.get(currentGrade);
                        if (!StringUtils.isEmpty(gridEntry)) {
                            Integer existingTotal = (Integer) grid.get(gridEntry);
                            if (existingTotal != null) {
                                grid.set(gridEntry, Integer.valueOf(currentTotal + existingTotal.intValue()));
                            } else {
                                grid.set(gridEntry, Integer.valueOf(currentTotal));
                            }
                        }

                        lastDetail = detail;
                        lastCategory = category;
                    }
                } finally {
                    resultSet.close();
                }
            } finally {
                statement.close();
            }
        } catch (Exception e) {
            String message = (e.getMessage() == null) ? "" : e.getMessage();
            AppGlobals.getLog().log(Level.WARNING, message, e);
        } finally {
            getBroker().returnConnection();
        }

        /*
         * Add report parameters
         */
        addParameter(PARAMETER_TRANSCRIPT_COLUMN, m_column.getGradeName());
        addParameter(PARAMETER_CONTEXT, String.valueOf(context.getSchoolYear()));

        grid.beforeTop();
        return grid;
    }

    /**
     * Sets up a map linking the grades to a numeric equivalent.
     */
    private void loadColumnMap() {
        m_gradeToNumber = new HashMap(32);
        m_numberToGrade = new HashMap(32);

        GradeScale scale = m_column.getGradeScale();

        Criteria criteria = new Criteria();
        criteria.addEqualTo(GradeScaleGradeDefinition.COL_GRADE_SCALE_OID, scale.getOid());

        QueryByCriteria query = new QueryByCriteria(GradeScaleGradeDefinition.class, criteria);
        query.addOrderByDescending(GradeScaleGradeDefinition.COL_GRADE_VALUE);
        QueryIterator scaleIterator = getBroker().getIteratorByQuery(query);
        try {
            int count = 0;
            while (scaleIterator.hasNext()) {
                GradeScaleGradeDefinition definition = (GradeScaleGradeDefinition) scaleIterator.next();
                m_gradeToNumber.put(definition.getGradeCode(), String.valueOf(count));
                m_numberToGrade.put(String.valueOf(count), definition.getGradeCode());
                count++;
            }
        } finally {
            scaleIterator.close();
        }

        addParameter(PARAMETER_GRADE_MAP, m_numberToGrade);
    }
}
