/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2010 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.uk;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.tools.exports.ExportJavaSource;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.sis.model.beans.ExamEntry;
import com.x2dev.sis.model.beans.ExamEntryExport;
import com.x2dev.sis.model.beans.ExamOption;
import com.x2dev.sis.model.beans.ExamSeries;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.X2BaseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.commons.lang.CharSetUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * UK state report for exam entries and amendments export.
 *
 * @author Follett Software Company
 */
public class ExamEntries extends StateReportData {
    /**
     * Implementation of StateReportEntity to be used by the UK exam entries and amendments export.
     * This must be a public static inner class with a public no argument constructor so it can be
     * instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class ExamEntriesEntity extends StateReportEntity {
        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public ExamEntriesEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Return an identifying name for this entity for reporting purposes.
         *
         * @return String
         * @see com.x2dev.sis.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            SisStudent student = (SisStudent) getBean();

            String name = student.getNameView() +
                    " [" + student.getFieldValueByAlias(ALIAS_CANDIDATE_NUMBER) + "]";
            return name;
        }

        /**
         * Initialize.
         * Set number of rows needed so specify all the student's exam entries.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see com.x2dev.sis.tools.stateexports#initialize()
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            SisStudent student = (SisStudent) bean;
            ExamEntries entriesData = (ExamEntries) data;
            List<ExamEntry> entries = entriesData.m_entriesMap.get(student.getOid());
            if (entries == null || entries.size() == 0) {
                setRowCount(0);
            } else if (entries.size() <= MAX_ENTRIES_PER_ROW) {
                setRowCount(1);
            } else {
                setRowCount(2);
            }

            if (getRowCount() == 0 && entriesData.m_dataType.equals(ExamEntries.DATA_TYPE_AMENDMENTS)) {
                if (entriesData.m_studentSeriesList.contains(student.getOid())) {
                    setRowCount(1);
                }
            }

            entriesData.m_totalRowCount += getRowCount();
        }
    }

    /**
     * Retrieves the option entry code for the field. There are 12 option entry code fields per row.
     */
    protected class RetrieveOptionEntryCode implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();

            // Param identifying the field for this exam entry. There are 12 exam entry fields per
            // row and the param
            // contains a number from 1 to 12.
            String param = (String) field.getParameter();

            int row = entity.getCurrentRow();

            // Get the index of the exam entry in the list. It will be a number from 0 to 11 if this
            // is the first row
            // for this student. If this is the second row for this student (because he has more
            // than 12 entries), then
            // the index will be a number from 12 to 23.
            int index = (row * 12) + Integer.parseInt(param) - 1;

            ExamEntries entriesData = (ExamEntries) data;
            List<ExamEntry> entries = entriesData.m_entriesMap.get(student.getOid());

            // if there is an exam entry for this index, get it and return it.
            if (entries != null && index < entries.size()) {
                ExamEntry entry = entries.get(index);
                return entry.getOption().getOptionEntryCode();
            }

            return null;
        }
    }

    /**
     * Retrieves the data type code. The data type is either entry or amendment and will be the same
     * value
     * for every row in the file.
     */
    protected class RetrieveDataType implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            return m_dataType;
        }
    }

    /**
     * Retrieves the centre number which identifies this school.
     */
    protected class RetrieveCentreNumber implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            return getSchool().getFieldValueByAlias(ALIAS_CENTRE_NUMBER);
        }
    }

    /**
     * Retrieves the student's name and formats it properly. The name is also stripped of any
     * invalid chars.
     */
    protected class RetrieveCandidateName implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();

            String lastName = CharSetUtils.keep(student.getPerson().getLastName(), VALID_CHARS);
            String firstName = CharSetUtils.keep(student.getPerson().getFirstName(), VALID_CHARS);
            String middleName = CharSetUtils.keep(student.getPerson().getMiddleName(), VALID_CHARS);
            String nameSuffix = CharSetUtils.keep(student.getPerson().getNameSuffixCode(), VALID_CHARS);

            StringBuilder surname = new StringBuilder(80);
            StringBuilder forenames = new StringBuilder(100);

            StringBuilder fullName = new StringBuilder(190);

            boolean hasLastName = StringUtils.isNotBlank(lastName);
            boolean hasFirstName = StringUtils.isNotBlank(firstName);
            boolean hasMiddleName = StringUtils.isNotBlank(middleName);
            boolean hasNameSuffix = StringUtils.isNotBlank(nameSuffix);

            // build surname
            if (hasLastName) {
                surname.append(lastName);
            }

            // build forenames
            if (hasFirstName) {
                forenames.append(firstName);
            }

            if (hasMiddleName) {
                if (forenames.length() > 0) {
                    forenames.append(" ");
                }
                forenames.append(middleName);
            }

            if (hasNameSuffix) {
                if (forenames.length() > 0) {
                    forenames.append(" ");
                }
                forenames.append(nameSuffix);
            }

            // build full name from surname and forenames
            fullName.append(surname);

            if (surname.length() > 0 && forenames.length() > 0 && fullName.length() < field.getMaxLength() - 1) {
                fullName.append(":");
            }

            if (fullName.length() < field.getMaxLength() - 1) {
                fullName.append(forenames);
            }

            return StringUtils.substring(fullName.toString(), 0, field.getMaxLength());
        }
    }

    /*
     * Aliases
     */
    private static final String ALIAS_CENTRE_NUMBER = "DFE CENTRE NUMBER";
    private static final String ALIAS_CANDIDATE_NUMBER = "DFE CANDIDATE NUMBER";

    /*
     * Input parameters
     */
    private static final String PARAM_EXAM_SERIES = "examSeries";
    private static final String PARAM_DATA_TYPE = "dataType";
    private static final String PARAM_SEQUENCE_NUM = "sequenceNum";
    private static final String PARAM_QUERY_BY = "queryBy";
    private static final String QUERY_BY_CURRENT = "##current";

    /*
     * Constants
     */
    private static final String FILE_HEADER_RECORD_TYPE = "1";
    private static final String CENTRE_HEADER_RECORD_TYPE = "3";
    private static final String CENTRE_TRAILER_RECORD_TYPE = "7";
    private static final String FILE_TRAILER_RECORD_TYPE = "9";

    private static final String DATA_TYPE_AMENDMENTS = "A";

    private static final int MAX_ENTRIES_PER_ROW = 12;

    private static final int RECORD_COUNT_FIELD_LEN = 7;

    // list of valid chars for student name - A-Z, a-z, hyphen, apostrophe, space, round brackets
    // (open and close) and full stop
    protected final static String VALID_CHARS = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz-' ().";

    /*
     * Instance variables.
     */
    protected int m_totalRowCount = 0; // total number of data rows in the export

    protected ExamSeries m_series; // exam series
    protected String m_dataType; // data type code (entry or amendment)
    protected Integer m_sequenceNum; // sequence number
    protected Map<String, List<ExamEntry>> m_entriesMap; // map containing a list of option entry
                                                         // codes for each student
    // list of student OIDs of students who have ever been assigned an option in this series.
    protected Collection<String> m_studentSeriesList;

    /**
     * Gets the heading.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getHeading()
     */
    @Override
    public String getHeading() {
        StringBuilder headingLine1 = new StringBuilder(100);
        StringBuilder headingLine2 = new StringBuilder(100);

        if (getSetupErrors().isEmpty()) {
            String seriesAbIdentifier = m_series.getAbIdentifier();
            if (StringUtils.isEmpty(seriesAbIdentifier)) {
                seriesAbIdentifier = m_series.getAwardingBody();
            }

            // file header
            headingLine1.append(m_dataType); // E (Entries) or A (Amendments)
            headingLine1.append(FILE_HEADER_RECORD_TYPE);
            headingLine1.append(getSchool().getFieldValueByAlias(ALIAS_CENTRE_NUMBER));
            headingLine1.append(seriesAbIdentifier); // AB identifier
            headingLine1.append(m_series.getCode()); // examination series
            headingLine1.append(m_series.getYear()); // year
            headingLine1.append("S"); // distribution type - S = data to or from a centre
            headingLine1.append("Aspen  "); // software package
            headingLine1.append(AppGlobals.getVersion(false)); // software package version
            headingLine1.append("14"); // formats version for this export file
            headingLine1.append(StringUtils.rightPad(" ", 193 - headingLine1.length() - 1));
            headingLine1.append(ExportJavaSource.FORMAT_EOL_WINDOWS);

            // centre header
            headingLine2.append(m_dataType); // E (Entries) or A (Amendments)
            headingLine2.append(CENTRE_HEADER_RECORD_TYPE);
            headingLine2.append(getSchool().getFieldValueByAlias(ALIAS_CENTRE_NUMBER));
            headingLine2.append(seriesAbIdentifier); // AB identifier
            headingLine2.append(m_series.getCode()); // examination series
            headingLine2.append(m_series.getYear()); // year

            String seqNumStr = "";
            if (m_sequenceNum != null) {
                seqNumStr = m_sequenceNum.toString();
            } else {
                String sequenceNumbersString = PreferenceManager
                        .getPreferenceValue(getOrganization().getRootOrganization(), "exam.export.sequenceNumber");
                Collection<String> seriesSeqNumbers = null;
                if (StringUtils.isEmpty(sequenceNumbersString)) {
                    seriesSeqNumbers = new ArrayList<String>();
                } else {
                    seriesSeqNumbers =
                            com.x2dev.utils.StringUtils.convertDelimitedStringToList(sequenceNumbersString, ',');
                }

                int seqNumber = 0;
                for (String seriesSeqNum : seriesSeqNumbers) {
                    if (seriesSeqNum.startsWith(m_series.getSeasonOid() + ":" + m_series.getOid())) {
                        int indexSeparation = seriesSeqNum.lastIndexOf(":");
                        seqNumber = Integer.valueOf(seriesSeqNum.substring(indexSeparation + 1)).intValue();
                    }
                }

                seqNumber += 1;
                seqNumStr = String.valueOf(seqNumber);
            }

            seqNumStr = StringUtils.substring(seqNumStr, 0, 3);
            seqNumStr = StringUtils.leftPad(seqNumStr, 3, '0');
            headingLine2.append(seqNumStr); // sequence number
            String postCode = getSchool().getAddress() != null && getSchool().getAddress().getPostalCode() != null
                    ? getSchool().getAddress().getPostalCode() : "";
            headingLine2.append(StringUtils.rightPad(postCode, 8)); // post code
            headingLine2.append(StringUtils.rightPad(" ", 193 - headingLine2.length() - 1));
            headingLine2.append(ExportJavaSource.FORMAT_EOL_WINDOWS);
        }

        return headingLine1.toString() + headingLine2.toString();
    }

    /**
     * Gets the trailer.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getTrailer()
     */
    @Override
    public String getTrailer() {
        StringBuilder trailer1 = new StringBuilder(100);
        StringBuilder trailer2 = new StringBuilder(100);

        if (getSetupErrors().isEmpty()) {
            // centre trailer
            trailer1.append(m_dataType); // E (Entries) or A (Amendments)
            trailer1.append(CENTRE_TRAILER_RECORD_TYPE);
            trailer1.append(getSchool().getFieldValueByAlias(ALIAS_CENTRE_NUMBER));
            // num of records including centre header and trailer
            trailer1.append(StringUtils.leftPad(String.valueOf(m_totalRowCount + 2), RECORD_COUNT_FIELD_LEN, '0'));
            Date today = new Date();
            SimpleDateFormat formatter = new SimpleDateFormat("ddMMyy");
            trailer1.append(formatter.format(today)); // date file produced
            trailer1.append(StringUtils.rightPad(" ", 193 - trailer1.length() - 1));
            trailer1.append(ExportJavaSource.FORMAT_EOL_WINDOWS);

            // file trailer
            trailer2.append(m_dataType); // E (Entries) or A (Amendments)
            trailer2.append(FILE_TRAILER_RECORD_TYPE);
            trailer2.append(getSchool().getFieldValueByAlias(ALIAS_CENTRE_NUMBER));
            // num of records including file and centre, header and trailer
            trailer2.append(StringUtils.leftPad(String.valueOf(m_totalRowCount + 4), RECORD_COUNT_FIELD_LEN, '0'));
            trailer2.append(StringUtils.leftPad("1", RECORD_COUNT_FIELD_LEN, '0')); // num of
                                                                                    // centres
            trailer2.append(StringUtils.rightPad(" ", 193 - trailer2.length() - 1));
            trailer2.append(ExportJavaSource.FORMAT_EOL_WINDOWS);
        }

        return trailer1.toString() + trailer2.toString();
    }

    /**
     * Initialize.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    public void initialize() {
        /*
         * Job parameters.
         */
        String seriesOid = (String) getParameter(PARAM_EXAM_SERIES);
        m_series = (ExamSeries) getBroker().getBeanByOid(ExamSeries.class, seriesOid);
        m_dataType = (String) getParameter(PARAM_DATA_TYPE); // E (Entries) or A (Amendments)
        m_sequenceNum = (Integer) getParameter(PARAM_SEQUENCE_NUM);

        if (getSetupErrors().size() == 0) {
            Criteria studentCriteria = getStudentCriteria();
            applyInputCriteria(studentCriteria, true, null);

            /*
             * Build the criteria/query for the students to include in this export based on user
             * input.
             */
            QueryByCriteria studentQuery = new QueryByCriteria(SisStudent.class, studentCriteria);
            applyInputSort(studentQuery, null);

            // Set the query to be used for student selection.
            setQuery(studentQuery);
            setEntityClass(ExamEntriesEntity.class);

            // set entries map
            loadEntries(studentCriteria);

            // Build map of retriever functions
            HashMap<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
            calcs.put("option-entry-code", new RetrieveOptionEntryCode());
            calcs.put("data-type", new RetrieveDataType());
            calcs.put("centre-number", new RetrieveCentreNumber());
            calcs.put("candidate-name", new RetrieveCandidateName());
            super.addCalcs(calcs);
        }
    }

    /**
     * Compose and return a studentCriteria .
     *
     * @return studentCriteria
     */
    private Criteria getStudentCriteria() {
        X2Criteria studentCriteria = new X2Criteria();

        /*
         * Active students
         */
        studentCriteria.addAndCriteria(
                StudentManager.getActiveStudentStatusCriteria(getOrganization(), SisStudent.COL_ENROLLMENT_STATUS));

        /*
         * Check school selection user input parameter.
         */
        if (isSchoolContext()) {
            studentCriteria.addEqualTo(SisStudent.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            studentCriteria.addEqualTo(SisStudent.REL_SCHOOL + PATH_DELIMITER + School.COL_INACTIVE_INDICATOR,
                    Boolean.FALSE);
            studentCriteria.addEqualTo(SisStudent.REL_SCHOOL + PATH_DELIMITER + School.COL_ARCHIVE_INDICATOR,
                    Boolean.FALSE);
        }

        return studentCriteria;
    }

    /**
     * Load the entries using the composed studentCriteria.
     *
     * @param studentCriteria Criteria
     */
    private void loadEntries(Criteria studentCriteria) {
        String queryBy = (String) getParameter(PARAM_QUERY_BY);

        X2Criteria criteria = new X2Criteria();
        SubQuery studentsSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria);

        if (queryBy.equals(QUERY_BY_CURRENT)) {
            X2Criteria selectedEntriesCriteria = new X2Criteria();

            String entriesOids = (String) getParameter("entrieOids");
            if (!entriesOids.isEmpty()) {
                selectedEntriesCriteria.addIn(X2BaseBean.COL_OID,
                        com.x2dev.utils.StringUtils.convertDelimitedStringToList(entriesOids, ','));
            } else {
                selectedEntriesCriteria.addEmpty(X2BaseBean.COL_OID, getSchool().getPersistenceKey());
            }
            SubQuery selectedEntrieStudentQuery =
                    new SubQuery(ExamEntry.class, ExamEntry.COL_STUDENT_OID, selectedEntriesCriteria);
            SubQuery selectedYearQuery = new SubQuery(ExamEntry.class,
                    ExamEntry.REL_OPTION + "." + ExamOption.REL_SERIES + "." + ExamSeries.COL_SEASON_OID,
                    selectedEntriesCriteria);

            criteria.addIn(ExamEntry.COL_STUDENT_OID, selectedEntrieStudentQuery);
            criteria.addIn(ExamEntry.REL_OPTION + "." + ExamOption.REL_SERIES + "." + ExamSeries.COL_SEASON_OID,
                    selectedYearQuery);
            criteria.addEqualTo(ExamEntry.REL_OPTION + ModelProperty.PATH_DELIMITER + ExamOption.COL_SERIES_OID,
                    m_series.getOid());

            m_studentSeriesList = new ArrayList<String>();
        } else {
            // report query - student exam entries
            criteria.addIn(ExamEntry.COL_STUDENT_OID, studentsSubQuery);
            criteria.addEqualTo(ExamEntry.REL_OPTION + ModelProperty.PATH_DELIMITER + ExamOption.COL_SERIES_OID,
                    m_series.getOid());

            Criteria eeeCriteria = new Criteria();
            eeeCriteria.addIn(ExamEntryExport.COL_STUDENT_OID, studentsSubQuery);
            eeeCriteria.addEqualTo(ExamEntryExport.COL_SERIES_OID, m_series.getOid());
            QueryByCriteria eeeQuery = new QueryByCriteria(ExamEntryExport.class, eeeCriteria);
            Collection<ExamEntryExport> eeeCollection = getBroker().getCollectionByQuery(eeeQuery);
            m_studentSeriesList = new ArrayList<String>();
            for (ExamEntryExport eee : eeeCollection) {
                m_studentSeriesList.add(eee.getStudentOid());
            }
        }

        QueryByCriteria query = new QueryByCriteria(ExamEntry.class, criteria);
        query.addOrderBy(ExamEntry.COL_STUDENT_OID, true);
        query.addOrderBy(X2BaseBean.COL_LAST_MODIFIED_TIME, true);
        m_entriesMap = getBroker().getGroupedCollectionByQuery(query, ExamEntry.COL_STUDENT_OID, 100);

    }
}
