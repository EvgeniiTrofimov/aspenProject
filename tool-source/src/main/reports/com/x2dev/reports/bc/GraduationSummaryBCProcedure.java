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

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.x2dev.sis.model.beans.Transcript;
import com.x2dev.sis.model.beans.TranscriptColumnDefinition;
import com.x2dev.sis.model.business.GraduationSummaryProcedure;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

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

/**
 * This procedure facilitates displaying extra columns on the Graduation Summary and Progress
 * screens.
 * 
 * @author X2 Development Corporation
 */
public class GraduationSummaryBCProcedure extends GraduationSummaryProcedure {
    /**
     * Alias' for the grade column headers for these transcript columns.
     */
    private static final String TRANSCRIPT_EXAM_MARK_ALIAS = "trn-prov-exam-mark";
    private static final String TRANSCRIPT_BLENDED_MARK_ALIAS = "trn-blended-mark";
    private static final String TRANSCRIPT_COURSE_END_DATE_ALIAS = "trn-completion-date";

    /**
     * Alias for the trax reference table field for creating exams.
     */
    private static final String TRAX_EXTENDED_DATA_DICTIONARY_EXAM_CREATE_ALIAS = "rcd-exam-create";

    /**
     * Reference Table ID for the TRAX reference table.
     */
    // private static final String TRAX_EXTENDED_DICTIONARY_ID = "TRX-OVR-CRS-EXAM";
    // private static final String TRAX_REFERENCE_TABLE_USER_NAME = "TRAX Override Code";

    /**
     * Alias for the trax reference table field on the transcript table.
     */
    private static final String TRAX_TRANSRIPT_FIELD_ALIAS = "trn-trax-override";

    private Map<String, Boolean> m_referenceCodeMap;

    /**
     * Constructs an instance of the GraduationSummaryBCProcedure loading the Trax reference table.
     *
     * @param broker X2Broker
     */
    public GraduationSummaryBCProcedure(X2Broker broker) {
        super(broker);

        loadTraxOverrideReferenceCodes();
    }

    /**
     * Retrieves values from the TRAX reference table and determines if extra grades need to be
     * added or if the grades are N/A.
     *
     * @param transcript Transcript
     * @param expectedColumnHeaders List<String>
     * @param gradeColumnsToValues Map<String,String>
     * @see
     *      com.x2dev.sis.model.business.GraduationSummaryProcedure#addColumnHeadersFromTranscript(com.
     *      x2dev.sis.model.beans.Transcript, java.util.List, java.util.Map)
     */
    @Override
    public void addColumnHeadersFromTranscript(Transcript transcript,
                                               List<String> expectedColumnHeaders,
                                               Map<String, String> gradeColumnsToValues) {
        super.addColumnHeadersFromTranscript(transcript, expectedColumnHeaders, gradeColumnsToValues);

        // Will only load the student assessments the first time around.

        DataDictionary dictionary = DataDictionary.getDistrictDictionary(transcript.getTranscriptDefinition(),
                transcript.getPersistenceKey());
        String code = (String) transcript.getFieldValueByAlias(TRAX_TRANSRIPT_FIELD_ALIAS, dictionary);

        boolean examRequired = false;

        if (StringUtils.isEmpty(code)) {
            if (transcript.getSchoolCourse() != null && transcript.getSchoolCourse().getCourse() != null) {
                examRequired = transcript.getSchoolCourse().getCourse().getExamRequiredIndicator();
            }
        } else {
            examRequired = m_referenceCodeMap.get(code) == null ? false : m_referenceCodeMap.get(code).booleanValue();
        }

        if (examRequired) {
            String examMark = (String) transcript.getFieldValueByAlias(TRANSCRIPT_EXAM_MARK_ALIAS, dictionary);
            String blendedMark = (String) transcript.getFieldValueByAlias(TRANSCRIPT_BLENDED_MARK_ALIAS, dictionary);
            gradeColumnsToValues.put(TRANSCRIPT_EXAM_MARK_ALIAS, examMark == null ? "" : examMark);
            gradeColumnsToValues.put(TRANSCRIPT_BLENDED_MARK_ALIAS, blendedMark == null ? "" : blendedMark);
        } else {
            gradeColumnsToValues.put(TRANSCRIPT_EXAM_MARK_ALIAS, "N/A");
            gradeColumnsToValues.put(TRANSCRIPT_BLENDED_MARK_ALIAS, "N/A");
        }

        String completionDateColumnAlias = determineCompletionDateColumn(transcript);
        if (completionDateColumnAlias != null) {
            String completionDate = (String) transcript.getFieldValueByAlias(completionDateColumnAlias, dictionary);
            gradeColumnsToValues.put(TRANSCRIPT_COURSE_END_DATE_ALIAS, completionDate == null ? "" : completionDate);
        } else {
            gradeColumnsToValues.put(TRANSCRIPT_COURSE_END_DATE_ALIAS, "");
        }
    }

    /**
     * Figures out the completion date alias column on the transcript.
     *
     * @param transcript Transcript
     * @return String - The completion date alias.
     */
    private String determineCompletionDateColumn(Transcript transcript) {
        String completionDateColumnAlias = null;

        Collection<TranscriptColumnDefinition> columns =
                transcript.getTranscriptDefinition().getTranscriptColumnDefinitions(getBroker());
        for (TranscriptColumnDefinition column : columns) {
            if (TranscriptColumnDefinition.COLUMN_TYPE_DATE == column.getColumnTypeCode()
                    && TranscriptColumnDefinition.DATE_TYPE_COURSE_END_DATE == column.getDateType()) {
                completionDateColumnAlias = column.getAlias();
                break;
            }
        }

        return completionDateColumnAlias;
    }

    /**
     * Creates a map of the trax reference Codes to their extended data dictionary column value of
     * create exam.
     */
    private void loadTraxOverrideReferenceCodes() {
        m_referenceCodeMap = new HashMap<String, Boolean>();

        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField dataDictionaryField = dictionary.findDataDictionaryFieldByAlias(TRAX_TRANSRIPT_FIELD_ALIAS);
        ReferenceTable referenceTable = dataDictionaryField.getReferenceTable();
        ExtendedDataDictionary extendedDataDictionary = referenceTable.getExtendedDataDictionary();

        /*
         * If the extended dictionary is null, it is not necessary to load the reference code values
         * as they are
         * on the extended dictionary.
         */
        if (extendedDataDictionary != null) {
            DataDictionary dataDictionary =
                    DataDictionary.getDistrictDictionary(extendedDataDictionary, getBroker().getPersistenceKey());

            if (dataDictionary != null) {
                Criteria referenceCodeCriteria = new Criteria();
                referenceCodeCriteria.addEqualTo(
                        ReferenceCode.REL_REFERENCE_TABLE + PATH_DELIMITER + ReferenceTable.COL_USER_NAME,
                        referenceTable.getUserName());

                DataDictionaryField dictionaryField =
                        dataDictionary.findDataDictionaryFieldByAlias(TRAX_EXTENDED_DATA_DICTIONARY_EXAM_CREATE_ALIAS);

                if (dictionaryField != null) {
                    String courseExamJavaName = dictionaryField.getJavaName();
                    String[] attributes = new String[] {ReferenceCode.COL_CODE, courseExamJavaName};

                    ReportQueryByCriteria query =
                            new ReportQueryByCriteria(ReferenceCode.class, attributes, referenceCodeCriteria);
                    ReportQueryIterator results = getBroker().getReportQueryIteratorByQuery(query);
                    try {
                        while (results.hasNext()) {
                            Object[] row = (Object[]) results.next();

                            String code = (String) row[0];
                            String createExam = (String) row[1];

                            m_referenceCodeMap.put(code, "1".equals(createExam) ? Boolean.TRUE : Boolean.FALSE);
                        }
                    } finally {
                        results.close();
                    }
                }
            }
        }
    }
}
