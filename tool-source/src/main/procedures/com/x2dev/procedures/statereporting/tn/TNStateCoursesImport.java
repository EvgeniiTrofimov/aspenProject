/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2018 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */

package com.x2dev.procedures.statereporting.tn;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.imports.TextImportJavaSource;
import com.x2dev.sis.model.beans.Course;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.apache.commons.lang.StringUtils;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Procedure to import all TN state courses into the district course list.
 *
 * @author Follett Software
 */
public class TNStateCoursesImport extends TextImportJavaSource {
    private static final String NO_PERFMON4J_INSTRUMENTATION = "";

    /**
     * Aliases
     */
    private static final String ALIAS_CRS_ENDORSEMENT = "all-crs-Endorsement";
    private static final String ALIAS_CRS_GENERAL_NOTES = "all-crs-GeneralNotes";
    private static final String ALIAS_CRS_PREV_CODE = "all-crs-PreviousSDEStateCode";
    private static final String ALIAS_CRS_STATE_CODE = "DOE SDE COURSE CODE";
    private static final String ALIAS_FUNDING_YN = "DOE CTE FUNDING";
    private static final String ALIAS_VOC_CLASS = "DOE VOC CLASSIFICATION";

    /**
     * General Parameters.
     */
    private static final String INPUT_PARAM_ACTON = "action";
    private static final String INPUT_PARAM_COMMIT = "commit";
    private static final String INPUT_UPD_CTE_FUND = "updCteFund";
    private static final String INPUT_UPD_DESC = "updCrsDesc";
    private static final String INPUT_UPD_ENDORSEMENT = "updEndorsementCodes";
    private static final String INPUT_UPD_GENERAL_CODES = "updGeneralNotes";
    private static final String INPUT_UPD_GRADE = "updGrade";
    private static final String INPUT_UPD_SHORT_DESC = "updShortDesc";
    private static final String INPUT_UPD_STATE_CRS_CODE = "updStateCrsCode";
    private static final String INPUT_UPD_VOC_CLASS = "updVocClass";
    private static final String PARAM_CONTEXT_OID = "districtContextOid";
    private static final String PARAM_FIELD_COUNT = "fieldCount";
    private static final String PARAM_SKIP_FIRST_ROW = "skipFirstRow";

    /**
     * Indexes of the files as parameters.
     */
    private static final String INPUT_INDEX_CRS_CODE = "courseCode";
    private static final String INPUT_INDEX_CTE_FUNDING = "cteFundingYn";
    private static final String INPUT_INDEX_DESC = "crsDesc";
    private static final String INPUT_INDEX_ENDORSEMENT = "crsEndorsement";
    private static final String INPUT_INDEX_GENERAL_NOTES = "cteGeneralNotes";
    private static final String INPUT_INDEX_GRADE = "crsGrade";
    private static final String INPUT_INDEX_PREV_CODE = "prevCourseCode";
    private static final String INPUT_INDEX_SHORT_DESC = "crsShortDesc";
    private static final String INPUT_INDEX_VOC_CLASS = "crsVocClass";

    /**
     * Other constants.
     */
    private static final long serialVersionUID = 1L;

    /**
     * Class members
     */
    private boolean m_actionAddNew;
    private boolean m_commitChanges;
    private String m_contextOid;
    private Set<String> m_courseNumbers;
    private Map<String, DataDictionaryField> m_ddFieldsMap = new HashMap<String, DataDictionaryField>();
    private int m_fieldCount;
    private String m_fieldCrsEndorsement;
    private String m_fieldCrsGeneralNotes;
    private String m_fieldCrsStateCode;
    private String m_fieldCteFundingYn;
    private String m_fieldPrevCode;
    private String m_fieldVocClass;
    private int m_indexCourseCode;
    private int m_indexCteFundingYn;
    private int m_indexEndorsement;
    private int m_indexDesc;
    private int m_indexGeneralNotes;
    private int m_indexGrade;
    private int m_indexPrevStateCode;
    private int m_indexShortDesc;
    private int m_indexVocClass;
    private int m_setupErrors;
    private boolean m_skipFirstRow;
    private Map<String, Collection<Course>> m_stateCourses;
    private boolean m_updGeneralNotes;
    private boolean m_updGrade;
    private boolean m_updCrsDescr;
    private boolean m_updCteFund;
    private boolean m_updEndorsement;
    private boolean m_updShortDesc;
    private boolean m_updStateCrsCode;
    private boolean m_updVocClass;

    /**
     * Translates an alias into a Java bean path name.
     *
     * @param alias String
     * @return String
     */
    public String getJavaNameByAlias(String alias) {
        String javaName = null;
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(alias);
        if (field != null) {
            javaName = field.getJavaName();
            m_ddFieldsMap.put(javaName, field);
        } else {
            logInvalidRecord(0, "Field with alias " + alias + " doesn't exist.");
        }

        return javaName;
    }

    /**
     * Gets the field count.
     *
     * @return int
     * @see com.x2dev.sis.tools.imports.TextImportJavaSource#getFieldCount()
     */
    @Override
    protected int getFieldCount() {
        return m_fieldCount;
    }

    /**
     * Return StringBuilder which contain the text that would show to user when the import procedure
     * finished.
     *
     * @return StringBuilder
     */
    @Override
    protected StringBuilder getImportStatistics() {
        StringBuilder messageToReturn = new StringBuilder(312);
        String mode = m_commitChanges ? "Commit Mode." : "Review Mode.";
        messageToReturn.append(mode);
        messageToReturn.append('\n');
        messageToReturn.append('\n');
        StringBuilder bufferMessage = super.getImportStatistics();
        messageToReturn.append(bufferMessage);
        return messageToReturn;
    }

    /**
     * Import data.
     *
     * @param sourceFile File
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.imports.TextImportJavaSource#importData(java.io.File)
     */
    @Override
    protected void importData(File sourceFile) throws Exception {
        if (m_setupErrors == 0) {
            super.importData(sourceFile);
        }
    }

    /**
     * Import record.
     *
     * @param record List<String>
     * @param lineNumber int
     * @throws Exception exception
     * @see com.x2dev.sis.tools.imports.TextImportJavaSource#importRecord(java.util.List, int)
     */
    @Override
    protected void importRecord(List<String> record, int lineNumber) throws Exception {
        if ((m_skipFirstRow && lineNumber > 1) || (!m_skipFirstRow)) {
            String previousCode = record.get(m_indexPrevStateCode);
            String stateCrsNumberToSet = record.get(m_indexCourseCode);
            if (!StringUtils.isEmpty(previousCode) && !StringUtils.isEmpty(stateCrsNumberToSet)) {
                previousCode = StringUtils.leftPad(StringUtils.trim(previousCode), 5, "0");
                for (Course course : getCourse(previousCode)) {
                    boolean isUpdated = false;
                    String cteFundingYn = record.get(m_indexCteFundingYn);
                    if (m_actionAddNew && course.isNew()) {
                        course.setDistrictContextOid(m_contextOid);
                        course.setOrganization1Oid(getOrganization().getRootOrganization().getOid());
                        course.setMasterType(SchoolCourse.MASTER_TYPE_CLASS);
                        course.setNumber(previousCode);
                        course.setNumber(stateCrsNumberToSet);
                        course.setFieldValueByBeanPath(m_fieldCrsStateCode, stateCrsNumberToSet);
                        course.setDescription(getTruncatedValue(Course.COL_DESCRIPTION, record.get(m_indexDesc)));
                        course.setShortDescription(
                                getTruncatedValue(Course.COL_SHORT_DESCRIPTION, record.get(m_indexShortDesc)));
                        course.setFieldValueByBeanPath(m_fieldPrevCode, record.get(m_indexPrevStateCode));
                        course.setFieldValueByBeanPath(m_fieldVocClass,
                                getTruncatedValue(m_fieldVocClass, record.get(m_indexVocClass)));
                        course.setGradeLevel(record.get(m_indexGrade));
                        if ("Y".equals(cteFundingYn)) {
                            course.setFieldValueByBeanPath(m_fieldCteFundingYn, BooleanAsStringConverter.TRUE);
                        } else {
                            course.setFieldValueByBeanPath(m_fieldCteFundingYn, BooleanAsStringConverter.FALSE);
                        }
                        course.setFieldValueByBeanPath(m_fieldCrsEndorsement,
                                getTruncatedValue(m_fieldCrsEndorsement, record.get(m_indexEndorsement)));
                        course.setFieldValueByBeanPath(m_fieldCrsGeneralNotes,
                                getTruncatedValue(m_fieldCrsGeneralNotes, record.get(m_indexGeneralNotes)));
                    } else if (!m_actionAddNew && !course.isNew()) {
                        if (m_updStateCrsCode) {
                            isUpdated = setCourseValueByBeanPath(course, isUpdated, m_fieldCrsStateCode,
                                    stateCrsNumberToSet);
                        }
                        if (m_updCrsDescr) {
                            isUpdated = setCourseValueByBeanPath(course, isUpdated, Course.COL_DESCRIPTION,
                                    getTruncatedValue(Course.COL_DESCRIPTION, record.get(m_indexDesc)));
                        }
                        if (m_updShortDesc) {
                            course.setShortDescription(
                                    getTruncatedValue(Course.COL_SHORT_DESCRIPTION, record.get(m_indexShortDesc)));
                        }
                        isUpdated = setCourseValueByBeanPath(course, isUpdated, m_fieldPrevCode, previousCode);
                        if (m_updVocClass) {
                            isUpdated = setCourseValueByBeanPath(course, isUpdated, m_fieldVocClass,
                                    getTruncatedValue(m_fieldVocClass, record.get(m_indexVocClass)));
                        }
                        if (m_updGrade) {
                            isUpdated = setCourseValueByBeanPath(course, isUpdated, Course.COL_GRADE_LEVEL,
                                    record.get(m_indexGrade));
                        }
                        if (m_updCteFund) {
                            if ("Y".equals(cteFundingYn)) {
                                isUpdated = setCourseValueByBeanPath(course, isUpdated, m_fieldCteFundingYn,
                                        BooleanAsStringConverter.TRUE);
                            } else {
                                isUpdated = setCourseValueByBeanPath(course, isUpdated, m_fieldCteFundingYn,
                                        BooleanAsStringConverter.FALSE);
                            }
                        }
                        if (m_updEndorsement) {
                            isUpdated = setCourseValueByBeanPath(course, isUpdated, m_fieldCrsEndorsement,
                                    getTruncatedValue(m_fieldCrsEndorsement, record.get(m_indexEndorsement)));
                        }
                        if (m_updGeneralNotes) {
                            isUpdated = setCourseValueByBeanPath(course, isUpdated, m_fieldCrsGeneralNotes,
                                    getTruncatedValue(m_fieldCrsGeneralNotes, record.get(m_indexGeneralNotes)));
                        }
                    }
                    if (course.isNew()) {
                        if (m_courseNumbers.contains(course.getNumber())) {
                            incrementSkipCount();
                        } else {
                            m_courseNumbers.add(course.getNumber());
                            if (m_commitChanges && m_actionAddNew) {
                                getBroker().saveBeanForced(course);
                            }
                            if (m_actionAddNew) {
                                incrementInsertCount();
                            }
                        }
                    } else {
                        if (course.isDirty()) {
                            if (m_commitChanges && !m_actionAddNew) {
                                getBroker().saveBeanForced(course);
                            }
                            if (!m_actionAddNew) {
                                incrementUpdateCount();
                            }
                        }
                        incrementMatchCount();
                    }
                }
            }
        }
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.x2dev.sis.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();
        initializeFields();
        initializeInputs();
        initializeIndexes();
        if (m_setupErrors > 0) {
            return;
        }
        setValueWrappingMode(VALUE_WRAPPING_MODE.OPTIONAL);
        setUseValueDelimiters(true);
        setValueWrapper('"');
        // Gets the input parameters
        m_contextOid = (String) getParameter(PARAM_CONTEXT_OID);
        m_fieldCount = ((Integer) getParameter(PARAM_FIELD_COUNT)).intValue();
        m_skipFirstRow = ((Boolean) getParameter(PARAM_SKIP_FIRST_ROW)).booleanValue();
        loadStateCourses();
    }

    /**
     * Add fields with no aliases to the map.
     *
     * @param className
     * @param column
     */
    private void addDicitionaryFieldToMap(String className, String column) {
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField field = dictionary.findDataDictionaryField(className, column);
        if (field != null) {
            m_ddFieldsMap.put(column, field);
        }
    }

    /**
     * Lookup field aliases and paths.
     */
    private void initializeFields() {
        m_setupErrors = 0;
        m_fieldVocClass = getJavaNameByAlias(ALIAS_VOC_CLASS);
        m_fieldCrsStateCode = getJavaNameByAlias(ALIAS_CRS_STATE_CODE);
        m_fieldCteFundingYn = getJavaNameByAlias(ALIAS_FUNDING_YN);
        m_fieldCrsGeneralNotes = getJavaNameByAlias(ALIAS_CRS_GENERAL_NOTES);
        m_fieldCrsEndorsement = getJavaNameByAlias(ALIAS_CRS_ENDORSEMENT);
        m_fieldPrevCode = getJavaNameByAlias(ALIAS_CRS_PREV_CODE);
        addDicitionaryFieldToMap(Course.class.getName(), Course.COL_DESCRIPTION);
        addDicitionaryFieldToMap(Course.class.getName(), Course.COL_SHORT_DESCRIPTION);
    }

    /**
     * Initialize column numbers in CSV file.
     */
    private void initializeIndexes() {
        m_indexCourseCode = ((Integer) getParameter(INPUT_INDEX_CRS_CODE)).intValue();
        m_indexPrevStateCode = ((Integer) getParameter(INPUT_INDEX_PREV_CODE)).intValue();
        m_indexDesc = ((Integer) getParameter(INPUT_INDEX_DESC)).intValue();
        m_indexShortDesc = ((Integer) getParameter(INPUT_INDEX_SHORT_DESC)).intValue();
        m_indexGrade = ((Integer) getParameter(INPUT_INDEX_GRADE)).intValue();
        m_indexVocClass = ((Integer) getParameter(INPUT_INDEX_VOC_CLASS)).intValue();
        m_indexCteFundingYn = ((Integer) getParameter(INPUT_INDEX_CTE_FUNDING)).intValue();
        m_indexEndorsement = ((Integer) getParameter(INPUT_INDEX_ENDORSEMENT)).intValue();
        m_indexGeneralNotes = ((Integer) getParameter(INPUT_INDEX_GENERAL_NOTES)).intValue();
    }

    /**
     * Initialize values from input form.
     */
    private void initializeInputs() {
        Boolean commit = (Boolean) getParameter(INPUT_PARAM_COMMIT);
        m_commitChanges = commit != null ? commit.booleanValue() : false; // default: review
        Boolean actionAdd = (Boolean) getParameter(INPUT_PARAM_ACTON);
        m_actionAddNew = actionAdd != null ? actionAdd.booleanValue() : false; // update matched
        m_updStateCrsCode = getParameter(INPUT_UPD_STATE_CRS_CODE) != null
                ? ((Boolean) getParameter(INPUT_UPD_STATE_CRS_CODE)).booleanValue()
                : true;
        m_updCrsDescr = getParameter(INPUT_UPD_DESC) != null
                ? ((Boolean) getParameter(INPUT_UPD_DESC)).booleanValue()
                : false;
        m_updShortDesc = getParameter(INPUT_UPD_SHORT_DESC) != null
                ? ((Boolean) getParameter(INPUT_UPD_SHORT_DESC)).booleanValue()
                : false;
        m_updCteFund =
                getParameter(INPUT_UPD_CTE_FUND) != null ? ((Boolean) getParameter(INPUT_UPD_CTE_FUND)).booleanValue()
                        : false;
        m_updGrade = getParameter(INPUT_UPD_GRADE) != null ? ((Boolean) getParameter(INPUT_UPD_GRADE)).booleanValue()
                : false;
        m_updVocClass =
                getParameter(INPUT_UPD_VOC_CLASS) != null ? ((Boolean) getParameter(INPUT_UPD_VOC_CLASS)).booleanValue()
                        : false;
        m_updEndorsement =
                getParameter(INPUT_UPD_ENDORSEMENT) != null
                        ? ((Boolean) getParameter(INPUT_UPD_ENDORSEMENT)).booleanValue()
                        : false;
        m_updGeneralNotes =
                getParameter(INPUT_UPD_GENERAL_CODES) != null
                        ? ((Boolean) getParameter(INPUT_UPD_GENERAL_CODES)).booleanValue()
                        : false;
        if (m_fieldVocClass == null || m_fieldCrsStateCode == null || m_fieldCteFundingYn == null) {
            m_setupErrors += 1;
        }
    }

    /**
     * Returns an existing course record if it exists or creates a new one if one does not exist.
     *
     * @param courseNumber String
     * @return Course
     */
    private Collection<Course> getCourse(String courseNumber) {
        Collection<Course> courses = m_stateCourses.get(courseNumber);
        if (courses == null || courses.isEmpty()) {
            Course course = X2BaseBean.newInstance(Course.class, getBroker().getPersistenceKey());
            courses = new ArrayList(1);
            courses.add(course);
        }
        return courses;
    }

    /**
     * Truncate value based on database field length.
     *
     * @param key
     * @param baseValue
     * @return
     */
    private String getTruncatedValue(String key, String baseValue) {
        String value = !StringUtils.isEmpty(baseValue) ? baseValue : "";
        DataDictionaryField ddField = m_ddFieldsMap.get(key);
        if (ddField != null && ddField.getDatabaseLength() < value.length()) {
            value = value.substring(0, ddField.getDatabaseLength() - 1);
        }
        return value;
    }

    /**
     * Loads existing courses to memory keyed on year context + course number.
     */
    private void loadStateCourses() {
        X2Criteria crit = new X2Criteria();
        crit.addEqualTo(Course.COL_DISTRICT_CONTEXT_OID, m_contextOid);
        QueryByCriteria query = new QueryByCriteria(Course.class, crit);
        m_stateCourses = getBroker().getGroupedCollectionByQuery(query,
                m_actionAddNew ? m_fieldCrsStateCode : m_fieldPrevCode, 4);
        m_courseNumbers = new HashSet();
        for (Collection<Course> courses : m_stateCourses.values()) {
            for (Course course : courses) {
                m_courseNumbers.add(course.getNumber());
            }
        }
    }

    /**
     * @param course
     * @param isUpdated
     * @param m_fieldVocClass2
     * @param vocClass
     * @return
     */
    private boolean setCourseValueByBeanPath(Course course,
                                             boolean isUpdated,
                                             String beanPath,
                                             String value) {
        boolean updated = false;
        String testValue = (String) course.getFieldValueByBeanPath(beanPath);
        if (StringUtils.isEmpty(value)) {
            if (!StringUtils.isEmpty(testValue)) {
                updated = true;
            }
        } else if (!value.equals(testValue)) {
            if (m_commitChanges) {
                course.setFieldValueByBeanPath(beanPath, value);
            }
            updated = true;
        }
        if (updated && m_commitChanges && !StringUtils.isEmpty(beanPath)) {
            course.setFieldValueByBeanPath(beanPath, value);
        }
        return updated ? updated : isUpdated;
    }
}
