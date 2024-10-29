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

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.x2dev.sis.model.beans.Course;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.utils.DataGrid;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import org.apache.commons.lang.exception.ExceptionUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Class to export school course information for BC as part of their GDE.
 *
 * @author Follett Software Company
 */
public class CourseInformationExport extends GdeExportJavaSource {
    private static final long serialVersionUID = 1L;

    // Grid fields
    private static final String FIELD_SCHOOL_NUMBER = "School Number";
    private static final String FIELD_DEPARTMENT = "Department";
    private static final String FIELD_DEPARTMENT_CODE = "Standard Course Department Category Code";
    private static final String FIELD_DEPARTMENT_DESCRIPTION = "Standard Course Department Category Description";
    private static final String FIELD_CODE = "Course Code";
    private static final String FIELD_TYPE = "Course Type";
    private static final String FIELD_TITLE = "Title";
    private static final String FIELD_DESCRIPTION = "Course Description";
    private static final String FIELD_CREDIT = "Credit Value";
    private static final String FIELD_GRADE_LEVEL = "Grade Level";
    private static final String FIELD_CATALOGUE_DESCRIPTION = "Catalogue Description";
    private static final String FIELD_SHORT_NAME = "Short Name";
    private static final String FIELD_NUMBER = "Course Number";
    private static final String FIELD_FORMAT = "Format";
    private static final String FIELD_LENGTH = "Course Length";
    private static final String FIELD_ACADEMIC_LEVEL = "Academic Level Code";
    private static final String FIELD_HONOR_ROLL_TYPE = "Include in Honour Roll";
    private static final String FIELD_INCLUDE_IN_GPA = "Include in GPA";
    private static final String FIELD_NON_STANDARD = "Non Standard";
    private static final String FIELD_EXAM_REQUIRED = "Exam";
    private static final String FIELD_ON_REPORT_CARD_IND = "On Report Card Indicator";
    private static final String FIELD_CAREER_PREP_IND = "Career Prep Indicator";
    private static final String FIELD_VOCATIONAL_IND = "Vocational Indicator";

    private List<String> m_columns;
    private Map<String, ReferenceCode> m_courseTypeMap;
    private Map<String, ReferenceCode> m_departmentCodeMap;

    /**
     * Gather data.
     *
     * @return DataGrid
     * @throws Exception exception
     * @see com.x2dev.sis.tools.exports.ExportJavaSource#gatherData()
     */
    @Override
    protected DataGrid gatherData() throws Exception {
        DataGrid grid = new DataGrid();

        QueryByCriteria query = new QueryByCriteria(SchoolCourse.class, buildCriteria());
        query.addOrderByAscending(SchoolCourse.COL_SCHOOL_OID);
        query.addOrderByAscending(SchoolCourse.COL_NUMBER);

        QueryIterator courses = getBroker().getIteratorByQuery(query);
        try {
            while (courses.hasNext()) {
                SchoolCourse schoolCourse = (SchoolCourse) courses.next();
                boolean deleteRow = false;
                try {
                    Course course = schoolCourse.getCourse().getRootCourse();

                    grid.append();
                    deleteRow = true;

                    /*
                     * Fill grid data list with export information
                     */
                    grid.set(FIELD_SCHOOL_NUMBER, schoolCourse.getSchool().getSchoolId());
                    grid.set(FIELD_CODE, wrap(schoolCourse.getNumber()));
                    grid.set(FIELD_TITLE, wrap(schoolCourse.getDescription()));
                    grid.set(FIELD_GRADE_LEVEL, schoolCourse.getGradeLevel());
                    grid.set(FIELD_SHORT_NAME, wrap(schoolCourse.getShortDescription()));
                    grid.set(FIELD_NUMBER, wrap(schoolCourse.getFieldC001()));
                    grid.set(FIELD_ACADEMIC_LEVEL, course.getAcademicLevel());
                    grid.set(FIELD_INCLUDE_IN_GPA, schoolCourse.getGpaIndicator() ? "Y" : "N");
                    grid.set(FIELD_ON_REPORT_CARD_IND, schoolCourse.getHideReportCardIndicator() ? "Y" : "N");

                    /*
                     * Format credits
                     */
                    BigDecimal credits = schoolCourse.getCredit();
                    if (credits != null) {
                        grid.set(FIELD_CREDIT, String.valueOf(credits.intValue()));
                    }

                    /*
                     * Get the local code of the course type (if available)
                     */
                    String courseType = course.getFieldB002();
                    ReferenceCode code = m_courseTypeMap.get(courseType);

                    if (code != null) {
                        courseType = code.getLocalCode();
                    }

                    grid.set(FIELD_TYPE, courseType);

                    /*
                     * Lookup department reference code description, if it exists
                     */
                    String departmentCode = schoolCourse.getDepartmentCode();
                    code = m_departmentCodeMap.get(departmentCode);

                    if (code != null) {
                        departmentCode = code.getDescription();
                    }

                    grid.set(FIELD_DEPARTMENT, wrap(departmentCode));
                } catch (NullPointerException npe) {
                    StringBuilder strBldr = new StringBuilder();
                    strBldr.append("Unable to export ");
                    strBldr.append(query.getClass().getName());
                    strBldr.append(" with OID: [");
                    strBldr.append(schoolCourse.getOid());
                    strBldr.append("].");

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
            courses.close();
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
    protected List getColumnNames() {
        return m_columns;
    }

    /**
     * Gets the column user names.
     *
     * @return List
     * @see com.x2dev.sis.tools.exports.ExportJavaSource#getColumnUserNames()
     */
    @Override
    protected List getColumnUserNames() {
        return m_columns;
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

        // Set columns
        m_columns = new ArrayList<String>(23);
        m_columns.add(FIELD_SCHOOL_NUMBER);
        m_columns.add(FIELD_DEPARTMENT);
        m_columns.add(FIELD_DEPARTMENT_CODE);
        m_columns.add(FIELD_DEPARTMENT_DESCRIPTION);
        m_columns.add(FIELD_CODE);
        m_columns.add(FIELD_TYPE);
        m_columns.add(FIELD_TITLE);
        m_columns.add(FIELD_DESCRIPTION);
        m_columns.add(FIELD_CREDIT);
        m_columns.add(FIELD_GRADE_LEVEL);
        m_columns.add(FIELD_CATALOGUE_DESCRIPTION);
        m_columns.add(FIELD_SHORT_NAME);
        m_columns.add(FIELD_NUMBER);
        m_columns.add(FIELD_FORMAT);
        m_columns.add(FIELD_LENGTH);
        m_columns.add(FIELD_ACADEMIC_LEVEL);
        m_columns.add(FIELD_HONOR_ROLL_TYPE);
        m_columns.add(FIELD_INCLUDE_IN_GPA);
        m_columns.add(FIELD_NON_STANDARD);
        m_columns.add(FIELD_EXAM_REQUIRED);
        m_columns.add(FIELD_ON_REPORT_CARD_IND);
        m_columns.add(FIELD_CAREER_PREP_IND);
        m_columns.add(FIELD_VOCATIONAL_IND);

        // Fill reference code maps
        fillReferenceCodeMaps();
    }

    /**
     * Builds export criteria.
     *
     * @return Criteria
     */
    private Criteria buildCriteria() {
        X2Criteria criteria = new X2Criteria();

        criteria.addAndCriteria(
                getSchoolCriteria().copyWithAdjustedPath(SchoolCourse.REL_SCHOOL, SchoolCourse.COL_SCHOOL_OID));
        criteria.addEqualTo(SchoolCourse.REL_COURSE + ModelProperty.PATH_DELIMITER + Course.COL_DISTRICT_CONTEXT_OID,
                getCurrentContext().getOid());

        return criteria;
    }

    /**
     * Fills export reference code maps if reference tables exist.
     */
    private void fillReferenceCodeMaps() {
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());

        /*
         * Deparment
         */
        m_departmentCodeMap = new HashMap<String, ReferenceCode>();

        DataDictionaryField field =
                dictionary.findDataDictionaryField(SchoolCourse.class.getName(), SchoolCourse.COL_DEPARTMENT_CODE);
        if (field != null) {
            ReferenceTable referenceTable = field.getReferenceTable();
            if (referenceTable != null) {
                m_departmentCodeMap = referenceTable.getCodeMap(getBroker());
            }
        }

        /*
         * Course types
         */
        m_courseTypeMap = new HashMap<String, ReferenceCode>();

        field = dictionary.findDataDictionaryField(Course.class.getName(), Course.COL_FIELD_B002);
        if (field != null) {
            ReferenceTable referenceTable = field.getReferenceTable();
            if (referenceTable != null) {
                m_courseTypeMap = referenceTable.getCodeMap(getBroker());
            }
        }
    }
}
