/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2002-2013 Follett Software Company.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from Follett Software Company.
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
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * UK state report for exam entry forecast export.
 *
 * @author Follett Software Company
 */
public class ExamEntryForecast extends StateReportData {
    /**
     * Implementation of StateReportEntity to be used by the UK exam entry forecast export.
     * This must be a public static inner class with a public no argument constructor so it can be
     * instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class ExamEntryForecastEntity extends StateReportEntity {
        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public ExamEntryForecastEntity() {
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
            ExamEntry entry = (ExamEntry) getBean();

            SisStudent student = entry.getStudent();

            String name = student.getNameView() + " - " + entry.getOption().getOptionEntryCode();

            return name;
        }

        /**
         * Initialize and set total row count.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see com.x2dev.sis.tools.stateexports#initialize()
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            ExamEntryForecast entriesData = (ExamEntryForecast) data;

            entriesData.m_totalRowCount += getRowCount();
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

    /*
     * Aliases
     */
    private static final String ALIAS_CENTRE_NUMBER = "DFE CENTRE NUMBER";

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

    private static final int RECORD_COUNT_FIELD_LEN = 7;

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
            headingLine1.append(m_dataType); // F (Forecast)
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
            headingLine2.append(m_dataType); // F (Forecast)
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
        m_dataType = (String) getParameter(PARAM_DATA_TYPE); // F (Forecast)
        m_sequenceNum = (Integer) getParameter(PARAM_SEQUENCE_NUM);

        if (getSetupErrors().size() == 0) {
            Criteria studentCriteria = getStudentCriteria();
            applyInputCriteria(studentCriteria, true, null);

            Criteria entryCriteria = getEntryCriteria();

            SubQuery studentSub = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria);
            entryCriteria.addIn(ExamEntry.COL_STUDENT_OID, studentSub);

            /*
             * Build the criteria/query for the entries to include in this export based on user
             * input.
             */
            QueryByCriteria entryQuery = new QueryByCriteria(ExamEntry.class, entryCriteria);
            applyInputSort(entryQuery, "student");

            // Set the query to be used for student selection.
            setQuery(entryQuery);
            setEntityClass(ExamEntryForecastEntity.class);

            // Build map of retriever functions
            HashMap<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
            calcs.put("data-type", new RetrieveDataType());
            calcs.put("centre-number", new RetrieveCentreNumber());
            super.addCalcs(calcs);
        }
    }

    /**
     * Builds and returns a entryCriteria.
     *
     * @return entryCriteria
     */
    private Criteria getEntryCriteria() {
        X2Criteria entryCriteria = new X2Criteria();

        String queryBy = (String) getParameter(PARAM_QUERY_BY);
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

            entryCriteria.addIn(ExamEntry.COL_STUDENT_OID, selectedEntrieStudentQuery);
            entryCriteria.addIn(ExamEntry.REL_OPTION + "." + ExamOption.REL_SERIES + "." + ExamSeries.COL_SEASON_OID,
                    selectedYearQuery);
        } else {
            // report query - student exam entries
            entryCriteria.addEqualTo(ExamEntry.REL_OPTION + ModelProperty.PATH_DELIMITER + ExamOption.COL_SERIES_OID,
                    m_series.getOid());
        }

        return entryCriteria;
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
}
