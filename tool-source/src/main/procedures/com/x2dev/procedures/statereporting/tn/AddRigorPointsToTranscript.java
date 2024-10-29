/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2006 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
/* DEBUG */
package com.x2dev.procedures.statereporting.tn;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.Transcript;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.X2RuntimeException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang3.math.NumberUtils;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Adds rigor points to all transcript grades for AP and Honors courses.
 *
 * NOTE: This differs from the other Add Rigor Points to Transcript procedure in that this one adds
 * Rigor points to all
 * grades rather than just the Final Grade as the other procedure does.
 *
 * @author Follett Software Company
 */
public class AddRigorPointsToTranscript extends ProcedureJavaSource {
    private static final long serialVersionUID = 1L;

    private static final String ALIAS_RIGOR_ADJUST_PREFIX = "all-trn-rigor-adjust-";
    private static final String ALIAS_RIGOR_ADJUST_TEST_ONLY_PREFIX = "all-trn-rigor-adjust-test-only-";
    private static final String ALIAS_RIGOR_ORIGINAL_PREFIX = "all-trn-rigor-original-";
    private static final String ALIAS_TAKING_EXAM = "all-trn-taking-exam";

    private static final String DECIMAL_FORMAT = "#0.####";

    // Input parameters
    public static final String PARAM_CONTEXT_OID = "districtContextOid";
    public static final String PARAM_COURSE_RIGOR_POINTS_ALIAS = "courseRigorPointsAlias";
    public static final String PARAM_EXCEED_100_POINTS = "exceed100Points";
    public static final String PARAM_SECTION_RIGOR_POINTS_OVERRIDE_ALIAS = "sectionRigorPointsOverrideAlias";

    private String m_aliasCourseRigorPoints;
    private String m_aliasSectionRigorPointsOverride;

    private List<DataDictionaryField> m_adjustFields;
    private List<DataDictionaryField> m_adjustTestOnlyFields;
    private String m_contextOid;
    private boolean m_exceed100Points;
    private List<DataDictionaryField> m_originalFields;
    private int m_updateCount;

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        QueryByCriteria query = new QueryByCriteria(Transcript.class, buildCriteria());
        query.addOrderByAscending(Transcript.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_SCHOOL_OID);

        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            SisSchool lastSchool = null;

            while (iterator.hasNext()) {
                Transcript transcript = (Transcript) iterator.next();
                SisStudent student = transcript.getStudent();
                SisSchool school = student.getSchool();
                String schoolOid = student.getSchoolOid();

                if (student != null) {
                    if ((lastSchool != null && !lastSchool.getOid().equals(schoolOid))) {
                        logUpdateCount(lastSchool);
                        m_updateCount = 0;
                    }

                    for (DataDictionaryField field : m_adjustFields) {
                        String key = ALIAS_RIGOR_ORIGINAL_PREFIX
                                + field.getAlias().substring(ALIAS_RIGOR_ADJUST_PREFIX.length());
                        Optional<DataDictionaryField> originalField = m_originalFields.stream()
                                .filter(fld -> fld.getAlias().equals(key)).findFirst();
                        if (originalField.isPresent()) {
                            addRigorPointsToField(transcript, originalField.get().getJavaName(), field.getJavaName());
                        } else {
                            throw new X2RuntimeException(
                                    new UnsupportedOperationException("Required alias " + key + " not found."));
                        }
                    }

                    boolean isTakingExam =
                            BooleanAsStringConverter.TRUE.equals(getFieldValueByAlias(transcript, ALIAS_TAKING_EXAM));
                    if (isTakingExam) {
                        for (DataDictionaryField field : m_adjustTestOnlyFields) {
                            String key = ALIAS_RIGOR_ORIGINAL_PREFIX
                                    + field.getAlias().substring(ALIAS_RIGOR_ADJUST_TEST_ONLY_PREFIX.length());
                            Optional<DataDictionaryField> originalField = m_originalFields.stream()
                                    .filter(fld -> fld.getAlias().equals(key)).findFirst();
                            if (originalField.isPresent()) {
                                addRigorPointsToField(transcript, originalField.get().getJavaName(),
                                        field.getJavaName());
                            } else {
                                throw new X2RuntimeException(
                                        new UnsupportedOperationException("Required alias " + key + " not found."));
                            }
                        }
                    }
                    boolean success = saveRecord(transcript);

                    if (!success) {
                        throw new Exception("Adding rigor points failed for student: " +
                                student.getNameView() +
                                " course: " + transcript.getCourseDescription() + " Please contact Aspen " +
                                "Support.");
                    }

                    lastSchool = school;
                }
            }

            logUpdateCount(lastSchool);
        } finally {
            iterator.close();
        }
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        m_aliasSectionRigorPointsOverride = (String) getParameter(PARAM_SECTION_RIGOR_POINTS_OVERRIDE_ALIAS);
        m_aliasCourseRigorPoints = (String) getParameter(PARAM_COURSE_RIGOR_POINTS_ALIAS);

        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());

        m_adjustFields = dictionary.getFieldsForContext(Transcript.class.getName()).stream()
                .filter(fld -> fld.getAlias() != null && fld.getAlias().matches(ALIAS_RIGOR_ADJUST_PREFIX + "\\d+"))
                .collect(Collectors.toList());
        m_adjustTestOnlyFields = dictionary.getFieldsForContext(Transcript.class.getName()).stream()
                .filter(fld -> fld.getAlias() != null
                        && fld.getAlias().matches(ALIAS_RIGOR_ADJUST_TEST_ONLY_PREFIX + "\\d+"))
                .collect(Collectors.toList());
        m_originalFields = dictionary.getFieldsForContext(Transcript.class.getName()).stream()
                .filter(fld -> fld.getAlias() != null && fld.getAlias().matches(ALIAS_RIGOR_ORIGINAL_PREFIX + "\\d+"))
                .collect(Collectors.toList());

        m_contextOid = (String) getParameter(PARAM_CONTEXT_OID);
        m_exceed100Points = ((Boolean) getParameter(PARAM_EXCEED_100_POINTS)).booleanValue();
        m_updateCount = 0;
    }

    /**
     * Adds rigor points to the Transcript scores.
     *
     * @param transcript Transcript
     * @param termName String
     */
    private void addRigorPointsToField(Transcript transcript,
                                       String originalGradeBeanPath,
                                       String transcriptGradeBeanPath) {
        if (transcript.getSchoolCourse() != null && transcript.getSchoolCourse().getCourse() != null) {
            BigDecimal rigorPoints = getRigorPoints(transcript);

            // If rigor points >= 0
            if (rigorPoints.compareTo(BigDecimal.valueOf(0)) >= 0) {

                BigDecimal originalGrade = new BigDecimal(0);
                BigDecimal adjustedGrade = new BigDecimal(0);

                BigDecimal transcriptGrade = getBigDecimalFromString(getFieldValue(transcript,
                        transcriptGradeBeanPath));
                originalGrade = getOriginalGrade(transcript, originalGradeBeanPath, transcriptGrade);
                adjustedGrade = addPoints(rigorPoints, originalGrade);

                DecimalFormat deciamlFormat = new DecimalFormat(DECIMAL_FORMAT);
                String originalGradeText =
                        originalGrade.compareTo(BigDecimal.ZERO) == 1 ? deciamlFormat.format(originalGrade) : null;
                String adjustedGradeText =
                        adjustedGrade.compareTo(BigDecimal.ZERO) == 1 ? deciamlFormat.format(adjustedGrade) : null;

                setFieldValue(transcript, originalGradeBeanPath, originalGradeText);
                setFieldValue(transcript, transcriptGradeBeanPath, adjustedGradeText);
            }
        }
    }

    /**
     * Adds the rigor points to original grade.
     *
     * @param rigorPoints BigDecimal
     * @param originalGrade BigDecimal
     * @return BigDecimal
     */
    private BigDecimal addPoints(BigDecimal rigorPoints, BigDecimal originalGrade) {
        BigDecimal transcriptGrade = new BigDecimal(0);

        // Adds the rigor points to the transcript score if originalGrade > 0
        if (originalGrade.compareTo(BigDecimal.valueOf(0)) == 1) {
            transcriptGrade = originalGrade.add(rigorPoints);

            // Cap grades at 100 if the input checkbox is not checked allowing grades to exceed 100
            if (!m_exceed100Points) {
                if (transcriptGrade.compareTo(BigDecimal.valueOf(100)) == 1) {
                    transcriptGrade = new BigDecimal(100);
                }
            }
        }

        return transcriptGrade;
    }

    /**
     * Builds the criteria.
     *
     * @return criteria
     */
    private X2Criteria buildCriteria() {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(Transcript.COL_DISTRICT_CONTEXT_OID, m_contextOid);

        if (isSchoolContext()) {
            criteria.addEqualTo(Transcript.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_SCHOOL_OID,
                    getSchool().getOid());
        }
        return criteria;
    }

    /**
     * Converts strings to BigDecimal. Non-numeric strings will return 0.
     *
     * @param rawVal String
     * @return BigDecimal
     */
    private BigDecimal getBigDecimalFromString(String rawVal) {
        BigDecimal value = BigDecimal.valueOf(0);

        if (!StringUtils.isEmpty(rawVal) && NumberUtils.isNumber(rawVal)) {
            value = new BigDecimal(rawVal);
        }

        return value;
    }

    /**
     * Gets the selected field from the passed bean.
     *
     * @param bean X2BaseBean
     * @param path String
     * @return String
     */
    private String getFieldValue(X2BaseBean bean, String path) {
        String value = "";
        if (bean != null && path != null) {
            value = (String) bean.getFieldValueByBeanPath(path);
        }

        return value;
    }

    /**
     * Gets the selected field from the passed bean using its alias.
     *
     * @param bean X2BaseBean
     * @param alias String
     * @return String
     */
    private String getFieldValueByAlias(X2BaseBean bean, String alias) {
        String value = "";
        if (bean != null && alias != null) {
            value = (String) bean.getFieldValueByAlias(alias);
        }

        return value;
    }

    /**
     * Returns the original grade prior to rigor points being added. If 0, then sets original grade
     * equal to the
     * current grade value since that hasn't yet had rigor points added to it.
     *
     * @param bean X2BaseBean
     * @param originalGradePath String
     * @param transcriptGrade BigDecimal
     * @return BigDecimal
     */
    private BigDecimal getOriginalGrade(X2BaseBean bean, String originalGradePath, BigDecimal transcriptGrade) {
        BigDecimal originalGrade = getBigDecimalFromString(getFieldValue(bean, originalGradePath));

        // If original grade is zero and transcript grade > 0, set the original grade to the
        // transcript grade
        // since this must be the first pass.
        if (originalGrade.equals(BigDecimal.valueOf(0)) && transcriptGrade.compareTo(BigDecimal.valueOf(0)) == 1) {
            originalGrade = transcriptGrade;
        }

        return originalGrade;
    }

    /**
     * Gets the appropriate rigor points value using the override field when populated.
     *
     * @param transcript Transcript
     * @return BigDeciaml
     */
    private BigDecimal getRigorPoints(Transcript transcript) {
        BigDecimal rigor = BigDecimal.valueOf(-1);
        BigDecimal rigorPoints = BigDecimal.valueOf(-1);
        BigDecimal rigorPointsOverride = BigDecimal.valueOf(-1);

        if (transcript.getSchoolCourse() != null && transcript.getSchoolCourse().getCourse() != null) {
            String points = getFieldValueByAlias(transcript.getSchoolCourse().getCourse(),
                    m_aliasCourseRigorPoints);

            if (!com.x2dev.utils.StringUtils.isEmpty(points)) {
                rigorPoints = getBigDecimalFromString(points);
            }
        }

        if (transcript.getMasterSchedule() != null) {
            String points = getFieldValueByAlias(transcript.getMasterSchedule(),
                    m_aliasSectionRigorPointsOverride);

            if (!com.x2dev.utils.StringUtils.isEmpty(points)) {
                rigorPoints = getBigDecimalFromString(points);
            }
        }

        if (rigorPointsOverride.compareTo(BigDecimal.valueOf(0)) > 0) {
            rigor = rigorPointsOverride;
        } else {
            rigor = rigorPoints;
        }

        return rigor;
    }

    /**
     * Prints a log message displaying the number of records that were updated for each school.
     *
     * @param lastSchool SisSchool
     */
    private void logUpdateCount(SisSchool lastSchool) {
        String messageEnding = "";

        if (lastSchool != null && lastSchool.getName() != null) {
            messageEnding = " at " + lastSchool.getName() + ".";
        } else {
            messageEnding = ".";
        }

        logMessage(m_updateCount + " transcript record(s) updated" + messageEnding);
    }

    /**
     * Saves the Transcript record.
     *
     * @param transcript Transcript
     * @return boolean
     */
    private boolean saveRecord(Transcript transcript) {
        boolean success = true;

        if (transcript != null && transcript.isDirty()) {
            try {
                m_updateCount++;
                getBroker().saveBeanForced(transcript);
                logMessage("Rigor Points Added for Student: " + transcript.getStudent().getNameView() +
                        " Course: " + transcript.getCourseDescription());
            } catch (Exception ex) {
                logMessage("Error saving changes to Transcript record while running Jefferson's Add Rigor Points " +
                        "procedure. Please contact Aspen Support.");
                success = false;
                AppGlobals.getLog().warning("Error saving changes to Transcript record.");
            }
        }

        return success;
    }

    /**
     * Sets the selected field with the passed value on the passed bean.
     *
     * @param bean X2BaseBean
     * @param path String
     * @param value String
     */
    private void setFieldValue(X2BaseBean bean, String path, String value) {
        if (bean != null && path != null) {
            bean.setFieldValueByBeanPath(path, value);
        }
    }

}
