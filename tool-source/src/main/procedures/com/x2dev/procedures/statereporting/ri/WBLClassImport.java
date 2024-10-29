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
package com.x2dev.procedures.statereporting.ri;
/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2006 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.imports.TextImportJavaSource;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.StudentSchedule;
import com.x2dev.utils.StreamUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Tool for importing WBL data to Aspen.
 *
 * @author X2 Development Corporation
 */
public class WBLClassImport extends TextImportJavaSource {

    private static final String HEADING_COURSE = "Course";
    private static final String HEADING_NAME = "Name";
    private static final String HEADING_SASID = "SASID";
    private static final String HEADING_SCHOOL = "School";
    private static final String HEADING_SECTION = "Section";
    private static final String HEADING_WBL_HOURS = "all-ssc-WBLHours";
    private static final String HEADING_WBL_HOURS_PAID = "all-ssc-WBLHoursPaid";
    private static final String HEADING_WBL_PARTNER = "all-ssc-WBLPartner";
    private static final String HEADING_WBL_SECTOR = "all-ssc-WBLSector";
    private static final String HEADING_WBL_TYPE = "all-ssc-WBLType";
    private static final List<String> HEADINGS_REQUIRED =
            Arrays.asList(HEADING_SASID, HEADING_SCHOOL, HEADING_COURSE, HEADING_SECTION, HEADING_WBL_TYPE,
                    HEADING_WBL_SECTOR, HEADING_WBL_PARTNER, HEADING_WBL_HOURS, HEADING_WBL_HOURS_PAID);

    private static final String INPUT_PARAM_COMMIT = "commit";

    private static final int LENGTH_WBL_PARTNER = 50;
    private static final int LENGTH_WBL_HOURS = 6;

    private static final String MAP_KEY_DELIMITER = "_";

    private boolean m_commitChanges;
    private String m_fieldWBLType;
    private String m_fieldWBLSector;
    private String m_fieldWBLPartner;
    private String m_fieldWBLHours;
    private String m_fieldWBLHoursPaid;
    private Map<String, Integer> m_headingPosition = new HashMap();
    private Map<String, String> m_refCodesWBLSector = new HashMap();
    private Map<String, String> m_refCodesWBLType = new HashMap();
    private Map<String, List<StudentSchedule>> m_scheduleMap = new HashMap();
    private List<String> m_setupErrors = new LinkedList();
    private X2Criteria m_sscBaseCriteria;

    @Override
    protected void exportResults() throws X2BaseException {
        if (!m_setupErrors.isEmpty()) {
            exportList(m_setupErrors);
        }
        super.exportResults();
    }

    /**
     * Gets the field count.
     *
     * @see com.follett.fsc.core.k12.tools.imports.TextImportJavaSource#getFieldCount()
     */
    @Override
    protected int getFieldCount() {
        return 0;
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
        if (m_setupErrors.isEmpty()) {
            super.importData(sourceFile);
        }
    }

    /**
     * Import record.
     *
     * @param record List
     * @param lineNumber int
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.imports.TextImportJavaSource#importRecord(java.util.List,
     *      int)
     */
    @Override
    protected void importRecord(List record, int lineNumber) throws Exception {
        if (lineNumber == 1) {
            setIndexes(record);
        } else if (m_setupErrors.isEmpty()) {
            String wblType = m_refCodesWBLType.get(getField(record, HEADING_WBL_TYPE));
            String wblSector = m_refCodesWBLSector.get(getField(record, HEADING_WBL_SECTOR));
            if (StringUtils.isEmpty(wblType)) {
                logInvalidRecord(lineNumber, getRecordInfo(record, "WBL Type not valid.  Record not imported."));
                incrementSkipCount();
                return;
            }
            if (StringUtils.isEmpty(wblSector)) {
                logInvalidRecord(lineNumber, getRecordInfo(record, "WBL Sector not valid.  Record not imported."));
                incrementSkipCount();
                return;
            }
            StudentSchedule studentSchedule = getStudentSchedule(record);
            if (studentSchedule == null) {
                logInvalidRecord(lineNumber, getRecordInfo(record, "not found for active Course/Section combination."));
                incrementSkipCount();
                return;
            }
            String wblPartner = getFieldTruncated(record, HEADING_WBL_PARTNER, LENGTH_WBL_PARTNER);
            String wblHours = getFieldTruncated(record, HEADING_WBL_HOURS, LENGTH_WBL_HOURS);
            String wblHoursPaid = getFieldTruncated(record, HEADING_WBL_HOURS_PAID, LENGTH_WBL_HOURS);
            incrementMatchCount();
            if (m_commitChanges) {
                studentSchedule.setFieldValueByBeanPath(m_fieldWBLType, wblType);
                studentSchedule.setFieldValueByBeanPath(m_fieldWBLSector, wblSector);
                studentSchedule.setFieldValueByBeanPath(m_fieldWBLPartner, wblPartner);
                studentSchedule.setFieldValueByBeanPath(m_fieldWBLHours, wblHours);
                studentSchedule.setFieldValueByBeanPath(m_fieldWBLHoursPaid, wblHoursPaid);
                getBroker().saveBeanForced(studentSchedule);
                incrementUpdateCount();
            }
        }
    }

    /**
     * Initialize.
     *
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() {

        Boolean commit = (Boolean) getParameter(INPUT_PARAM_COMMIT);
        m_commitChanges = commit == null || commit.booleanValue();

        initializeRefCodes(m_refCodesWBLType, HEADING_WBL_TYPE);
        initializeRefCodes(m_refCodesWBLSector, HEADING_WBL_SECTOR);

        m_fieldWBLType = translateAliasToJavaName(HEADING_WBL_TYPE);
        m_fieldWBLSector = translateAliasToJavaName(HEADING_WBL_SECTOR);
        m_fieldWBLPartner = translateAliasToJavaName(HEADING_WBL_PARTNER);
        m_fieldWBLHours = translateAliasToJavaName(HEADING_WBL_HOURS);
        m_fieldWBLHoursPaid = translateAliasToJavaName(HEADING_WBL_HOURS_PAID);

    }

    /**
     * Write a list of strings to the results handler.
     *
     * @param list List<String>
     * @throws X2BaseException exception
     */
    private void exportList(List<?> list) throws X2BaseException {
        StringBuilder buffer = new StringBuilder(256);
        for (Object err : list) {
            buffer.append(err.toString());
            buffer.append('\n');
        }
        try {
            ByteArrayInputStream inputStream = new ByteArrayInputStream(buffer.toString().getBytes());
            try {
                StreamUtils.copyStream(inputStream, getResultHandler().getOutputStream());
            } finally {
                inputStream.close();
            }
        } catch (FileNotFoundException fnfe) {
            throw new X2BaseException(fnfe);
        } catch (IOException ioe) {
            throw new X2BaseException(ioe);
        }
    }

    /**
     * Gets DataDictionary
     *
     * @return DataDictionary
     */
    private DataDictionary getDataDictionary() {
        return DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
    }

    /**
     * Gets the field.
     *
     * @param record List<String>
     * @param fieldName String
     * @return String
     */
    private String getField(List<String> record, String fieldName) {
        String value = null;
        int index = m_headingPosition.get(fieldName);
        if (index >= 0) {
            value = record.get(index);
        }
        return value;
    }

    /**
     * Gets field and truncates it by given length
     *
     * @param record
     * @param headingWblType
     * @return
     */
    private String getFieldTruncated(List record, String heading, int length) {
        String value = getField(record, heading);
        if (value != null && length > 0 && value.length() > length) {
            value = value.substring(0, length);
        }
        return value;
    }

    /**
     * Gets the record as String.
     *
     * @param record List<String>
     * @param message String
     * @return String
     */
    private String getRecordInfo(List<String> record, String message) {
        StringBuilder value = new StringBuilder();
        value.append("Student ");
        value.append(getField(record, HEADING_NAME));
        value.append(", ");
        value.append(getField(record, HEADING_SASID));
        value.append(" ");
        value.append(message);
        return value.toString();
    }

    /**
     * Gets Student Schedule for given record.
     *
     * @param record List<String>
     * @return StudentSchedule
     */
    private StudentSchedule getStudentSchedule(List<String> record) {
        StudentSchedule studentSchedule = null;
        String school = stripLeadingZero(getField(record, HEADING_SCHOOL));
        String course = stripLeadingZero(getField(record, HEADING_COURSE));
        String section = stripLeadingZero(getField(record, HEADING_SECTION));
        String stateId = getField(record, HEADING_SASID);
        if (!StringUtils.isEmpty(school) && !StringUtils.isEmpty(course) && !StringUtils.isEmpty(section)
                && !StringUtils.isEmpty(stateId)) {
            String key = getStudentScheduleMapKey(school, course, section);
            List<StudentSchedule> studentScheduleList = m_scheduleMap.get(key);
            if (studentScheduleList == null) {
                X2Criteria sscCriteria = new X2Criteria();
                sscCriteria.addAndCriteria(getStudentScheduleBaseCriteria());
                sscCriteria.addEndsWith(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER + Schedule.REL_SCHOOL
                        + PATH_DELIMITER + School.COL_SCHOOL_ID, school);
                sscCriteria.addEndsWith(StudentSchedule.REL_SECTION + PATH_DELIMITER + MasterSchedule.REL_SCHOOL_COURSE
                        + PATH_DELIMITER + SchoolCourse.COL_NUMBER, course);
                sscCriteria.addEndsWith(
                        StudentSchedule.REL_SECTION + PATH_DELIMITER + MasterSchedule.COL_SECTION_NUMBER, section);
                QueryByCriteria studentScheduleQuery = new QueryByCriteria(StudentSchedule.class, sscCriteria);
                studentScheduleList = (List) getBroker().getCollectionByQuery(studentScheduleQuery);
                m_scheduleMap.put(key, studentScheduleList);
            }
            if (studentScheduleList != null) {
                studentSchedule = studentScheduleList.stream()
                        .filter(ssc -> ssc.getStudent().getStateId().equals(stateId)).findFirst().orElse(null);
            }
        }
        return studentSchedule;
    }

    /**
     * Construct an initial criteria for loading student schedule records.
     *
     * @return X2Criteria
     */
    private X2Criteria getStudentScheduleBaseCriteria() {
        if (m_sscBaseCriteria == null) {
            m_sscBaseCriteria = new X2Criteria();

            m_sscBaseCriteria.addEqualTo(StudentSchedule.REL_SECTION + PATH_DELIMITER +
                    MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                    SchoolCourse.COL_MASTER_TYPE, SchoolCourse.MASTER_TYPE_CLASS);

            m_sscBaseCriteria.addNotEqualTo(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
            m_sscBaseCriteria.addNotEqualTo(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);

            m_sscBaseCriteria.addEqualTo(
                    StudentSchedule.REL_SCHEDULE + PATH_DELIMITER + Schedule.COL_DISTRICT_CONTEXT_OID,
                    getCurrentContext().getOid());
        }

        return m_sscBaseCriteria;
    }

    /**
     * Gets Student Schedule.
     *
     * @param school String
     * @param course String
     * @param section String
     * @return String
     */
    private String getStudentScheduleMapKey(String school, String course, String section) {
        return school + MAP_KEY_DELIMITER + course + MAP_KEY_DELIMITER + section;
    }

    /**
     * Initialize ref code map.
     *
     * @param codesMap Map<String, String>
     * @param alias String
     */
    private void initializeRefCodes(Map<String, String> codesMap, String alias) {
        DataDictionary dictionary = getDataDictionary();
        DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(alias);
        if (field != null) {
            ReferenceTable refTable = field.getReferenceTable();
            if (refTable != null) {
                for (ReferenceCode code : refTable.getReferenceCodes()) {
                    if (!StringUtils.isEmpty(code.getStateCode())) {
                        codesMap.put(code.getCode(), code.getStateCode());
                    }
                }
            }
        }
    }


    /**
     * Set index by header name
     *
     * @param record List<String>
     */
    private void setIndexes(List<String> record) {
        HEADINGS_REQUIRED.stream().forEach(heading -> setIndexForHeader(record, heading, true));
        setIndexForHeader(record, HEADING_NAME, false);
    }

    /**
     * Set index by header name
     *
     * @param record List<String>
     * @param heading String
     * @param required boolean
     */
    private void setIndexForHeader(List<String> record, String heading, boolean required) {
        boolean indexIsSet = false;
        for (int i = 0; i < record.size(); i++) {
            if (heading.equals(record.get(i))) {
                m_headingPosition.put(heading, i);
                indexIsSet = true;
                break;
            }
        }
        if (!indexIsSet && required) {
            m_setupErrors.add("Headings on import file must include column " + heading);
        }
    }

    /**
     * Set index by header name
     *
     * @param text String
     * @return String
     */
    private String stripLeadingZero(String text) {
        return text.replaceAll("^0+", "");
    }

    /**
     * Translates an alias into a Java bean path name.
     *
     * @param alias String
     * @return String
     */
    private String translateAliasToJavaName(String alias) {
        String javaName = null;
        DataDictionaryField field = getDataDictionary().findDataDictionaryFieldByAlias(alias);
        if (field != null) {
            javaName = field.getJavaName();
        }
        return javaName;
    }
}
