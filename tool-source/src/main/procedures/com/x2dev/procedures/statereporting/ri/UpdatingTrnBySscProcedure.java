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

package com.x2dev.procedures.statereporting.ri;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.GradeScale;
import com.x2dev.sis.model.beans.GradeScaleGradeDefinition;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.SchoolScheduleContext;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.StudentSchedule;
import com.x2dev.sis.model.beans.Transcript;
import com.x2dev.sis.model.beans.TranscriptColumnDefinition;
import com.x2dev.sis.model.beans.TranscriptDefinition;
import com.x2dev.sis.model.business.GradesManager;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.math.BigDecimal;
import java.util.Collection;
import java.util.HashSet;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class UpdatingTrnBySscProcedure.
 *
 * @author Follett Software Company
 *
 *         Procedure which will process daily to create TRN records from SCC records.
 *         This procedure will create/update TRN records based on the information in the SCC alias
 *         fields.
 */
public class UpdatingTrnBySscProcedure extends ProcedureJavaSource {

    /**
     * Aliases for StudentSchedule
     */
    protected static final String ALIAS_SSC_ENR_TYPE = "all-ssc-CourseEnrollmentType";
    protected static final String ALIAS_SSC_COURSE_CREDITS = "all-ssc-ProviderCourseCredits";
    protected static final String ALIAS_SSC_CRS_EXIT_RESON = "all-ssc-CourseExitReason";
    protected static final String ALIAS_SSC_CRS_NETWORK = "all-ssc-AdvancedCourseNetwork";
    protected static final String ALIAS_SSC_CRS_TERM = "all-ssc-CourseTermPSorOther";
    protected static final String ALIAS_SSC_PROVIDER_ID = "all-ssc-ProviderID";
    protected static final String ALIAS_SSC_PROVIDER_CRS_ID = "all-ssc-ProviderCourseID";
    protected static final String ALIAS_SSC_PROVIDER_CRS_NAME = "all-ssc-ProviderCourseName";

    /**
     * Aliases for Transcripts
     */
    protected static final String ALIAS_TRN_ENR_TYPE = "all-trn-CourseEnrollmentType";
    protected static final String ALIAS_TRN_COURSE_CREDITS = "all-trn-ProviderCourseCredits";
    protected static final String ALIAS_TRN_CRS_EXIT_RESON = "all-trn-CourseExitReason";
    protected static final String ALIAS_TRN_CRS_NETWORK = "all-trn-AdvancedCourseNetwork";
    protected static final String ALIAS_TRN_CRS_TERM = "all-trn-CourseTermPSorOther";
    protected static final String ALIAS_TRN_PROVIDER_ID = "all-trn-ProviderID";
    protected static final String ALIAS_TRN_PROVIDER_CR_EARNED = "all-trn-ProviderCreditEarned";
    protected static final String ALIAS_TRN_PROVIDER_CRS_ID = "all-trn-ProviderCourseID";
    protected static final String ALIAS_TRN_PROVIDER_CRS_NAME = "all-trn-ProviderCourseName";

    /**
     * Input parameters
     */
    protected static final String INPUT_PARAM_HIDE_TRN_FG = "hideTrnUntilFG";
    protected static final String INPUT_PARAM_UPD_PS_CREDITS = "updPSCredits";

    /**
     * Member fields
     */
    protected GradesManager m_gradesManager;
    protected boolean m_hideTrnUntilFg;
    protected String m_sscCrsNetwork;
    protected String m_sscCrsTerm;
    protected String m_sscExReason;
    protected String m_sscEnrollType;
    protected String m_sscProviderCrsCredits;
    protected String m_sscProviderId;
    protected String m_sscProviderCrsId;
    protected String m_sscProviderCrsName;
    protected String m_trnCreditsEarned;
    protected String m_trnCrsNetwork;
    protected String m_trnCrsTerm;
    protected String m_trnEnrollType;
    protected String m_trnExReason;
    protected String m_trnProviderCrsCredits;
    protected String m_trnProviderId;
    protected String m_trnProviderCrsId;
    protected String m_trnProviderCrsName;
    protected boolean m_updPSCredits;

    /**
     * Supporting instance variables.
     */
    private DataDictionary m_dictionary;
    private boolean m_initError = false;
    private Map<String, Collection<StudentSchedule>> m_scheduleMap;
    private Map<String, Collection<Transcript>> m_transcriptMap;

    /**
     * Translates an alias into a Java bean path name. An initialization error will be logged
     * if the alias does not exist.
     *
     * @param alias String
     * @param required boolean
     * @return String
     */
    public String translateAliasToJavaName(String alias, boolean required) {
        String javaName = null;

        DataDictionaryField field = m_dictionary.findDataDictionaryFieldByAlias(alias);
        if (field != null) {
            javaName = field.getJavaName();
        } else if (required) {
            m_initError = true;
            logMessage("Alias = " + alias + " is not found in Data Dictionary.");
        }

        return javaName;
    }

    /**
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        if (!m_initError) {
            for (Entry<String, Collection<StudentSchedule>> entry : m_scheduleMap.entrySet()) {
                String stdOid = entry.getKey();
                Collection<StudentSchedule> sscs = entry.getValue();
                Collection<Transcript> trns = m_transcriptMap.get(stdOid);

                for (StudentSchedule ssc : sscs) {
                    String mstOid = ssc.getSectionOid();
                    boolean existTrn = false;
                    Transcript trnToUpdate = null;
                    if (trns != null) {
                        for (Transcript trn : trns) {
                            if (mstOid.equals(trn.getMasterScheduleOid())) {
                                existTrn = true;
                                trnToUpdate = trn;
                                break;
                            }
                        }
                    }

                    if (!existTrn) {
                        trnCreateBySchedule(ssc);
                    } else {
                        trnUpdateBySchedule(ssc, trnToUpdate);
                    }
                }
            }
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
        super.initialize();

        initializeFields();
        if (!m_initError) {
            loadStudentSchedules();
            loadTranscripts();
        }
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.
     *      UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException {
        super.saveState(userData);
    }

    /**
     * Returns the grade definition for the passed grade and column. Null is returned if one cannot
     * be determined.
     *
     * @param grade String
     * @param column TranscriptColumnDefinition
     *
     * @return GradeScaleGradeDefinition
     */
    private GradeScaleGradeDefinition getGradeDefinition(String grade, TranscriptColumnDefinition column) {
        GradeScaleGradeDefinition gradeDefinition = null;
        GradeScale gradeScale = column.getGradeScale();
        if (gradeScale != null) {
            if (StringUtils.isNumeric(grade)) {
                gradeDefinition = m_gradesManager.getGradeDefinition(new BigDecimal(grade), gradeScale, null, null);
            } else {
                gradeDefinition = m_gradesManager.getGradeDefinition(grade, gradeScale, null, null);
            }
        }

        return gradeDefinition;
    }

    /**
     * Gets a list of the master schedules that are used for post-secondary credit.
     *
     * @return Sets the
     */
    private Set<String> getMasterSchedules() {
        Set<String> values = new HashSet();
        for (Entry<String, Collection<StudentSchedule>> entry : m_scheduleMap.entrySet()) {
            for (StudentSchedule item : entry.getValue()) {
                values.add(item.getSectionOid());
            }
        }
        return values;
    }

    /**
     * Initialize fields.
     */
    private void initializeFields() {
        m_dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        m_gradesManager = new GradesManager(getBroker());
        m_updPSCredits = ((Boolean) getParameter(INPUT_PARAM_UPD_PS_CREDITS)).booleanValue();
        m_hideTrnUntilFg = ((Boolean) getParameter(INPUT_PARAM_HIDE_TRN_FG)).booleanValue();
        m_sscCrsNetwork = translateAliasToJavaName(ALIAS_SSC_CRS_NETWORK, true);
        m_sscCrsTerm = translateAliasToJavaName(ALIAS_SSC_CRS_TERM, true);
        m_sscEnrollType = translateAliasToJavaName(ALIAS_SSC_ENR_TYPE, true);
        m_sscExReason = translateAliasToJavaName(ALIAS_SSC_CRS_EXIT_RESON, true);
        m_sscProviderCrsCredits = translateAliasToJavaName(ALIAS_SSC_COURSE_CREDITS, true);
        m_sscProviderId = translateAliasToJavaName(ALIAS_SSC_PROVIDER_ID, true);
        m_sscProviderCrsId = translateAliasToJavaName(ALIAS_SSC_PROVIDER_CRS_ID, true);
        m_sscProviderCrsName = translateAliasToJavaName(ALIAS_SSC_PROVIDER_CRS_NAME, true);

        m_trnCreditsEarned = translateAliasToJavaName(ALIAS_TRN_PROVIDER_CR_EARNED, true);
        m_trnCrsNetwork = translateAliasToJavaName(ALIAS_TRN_CRS_NETWORK, true);
        m_trnCrsTerm = translateAliasToJavaName(ALIAS_TRN_CRS_TERM, true);
        m_trnEnrollType = translateAliasToJavaName(ALIAS_TRN_ENR_TYPE, true);
        m_trnExReason = translateAliasToJavaName(ALIAS_TRN_CRS_EXIT_RESON, true);
        m_trnProviderCrsCredits = translateAliasToJavaName(ALIAS_TRN_COURSE_CREDITS, true);
        m_trnProviderId = translateAliasToJavaName(ALIAS_TRN_PROVIDER_ID, true);
        m_trnProviderCrsId = translateAliasToJavaName(ALIAS_TRN_PROVIDER_CRS_ID, true);
        m_trnProviderCrsName = translateAliasToJavaName(ALIAS_TRN_PROVIDER_CRS_NAME, true);
    }

    /**
     * Loads a map of students' schedules keyed on student oid.
     */
    private void loadStudentSchedules() {
        X2Criteria scheduleCriteria = new X2Criteria();

        // From active Schedule
        scheduleCriteria.addEqualToField(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.REL_SCHOOL + PATH_DELIMITER +
                SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER +
                SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID,
                StudentSchedule.COL_SCHEDULE_OID);

        scheduleCriteria.addEqualTo(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.REL_SCHOOL + PATH_DELIMITER +
                SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER +
                SchoolScheduleContext.COL_DISTRICT_CONTEXT_OID,
                getCurrentContext().getOid());

        scheduleCriteria.addNotEqualTo(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.REL_SCHOOL + PATH_DELIMITER +
                SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
        scheduleCriteria.addNotEqualTo(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.REL_SCHOOL + PATH_DELIMITER +
                SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);

        scheduleCriteria.addNotEmpty(m_sscEnrollType, getBroker().getPersistenceKey());

        QueryByCriteria query = new QueryByCriteria(StudentSchedule.class, scheduleCriteria);
        m_scheduleMap = getBroker().getGroupedCollectionByQuery(query, StudentSchedule.COL_STUDENT_OID, 500);
    }

    /**
     * Loads a map of students' transcripts keyed on student oid.
     *
     */
    private void loadTranscripts() {
        if (!m_scheduleMap.keySet().isEmpty()) {
            // load matching transcripts
            X2Criteria transcriptCriteria = new X2Criteria();
            transcriptCriteria.addIn(Transcript.COL_MASTER_SCHEDULE_OID, getMasterSchedules());
            transcriptCriteria.addIn(Transcript.COL_STUDENT_OID, m_scheduleMap.keySet());
            transcriptCriteria.addEqualTo(Transcript.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());

            QueryByCriteria query = new QueryByCriteria(Transcript.class, transcriptCriteria);
            query.addOrderBy(Transcript.COL_STUDENT_OID, true);
            m_transcriptMap = getBroker().getGroupedCollectionByQuery(query, Transcript.COL_STUDENT_OID, 500);
        }
    }

    /**
     * Creates a transcript from a student schedule object.
     *
     * @param studentSchedule StudentSchedule
     */
    private void trnCreateBySchedule(StudentSchedule studentSchedule) {
        Transcript transcript = new Transcript(getBroker().getPersistenceKey());

        transcript.setCourseDescription(studentSchedule.getSection().getDescription());
        transcript.setStudentOid(studentSchedule.getStudentOid());
        transcript.setDistrictContextOid(getCurrentContext().getOid());
        transcript.setFinalGrade("");

        if (m_hideTrnUntilFg) {
            transcript.setTranscriptHideInd(true);
        } else {
            transcript.setTranscriptHideInd(false);
        }

        transcript.setGradeLevel(studentSchedule.getStudent().getGradeLevel());
        transcript.setMasterScheduleOid(studentSchedule.getSectionOid());
        transcript.setSchoolCourseOid(studentSchedule.getSection().getSchoolCourseOid());
        transcript.setSchoolOid(studentSchedule.getSchedule().getSchoolOid());
        transcript.setTeacherOid(studentSchedule.getSection().getPrimaryStaffOid());
        transcript.setTermCode(studentSchedule.getTermView());
        transcript.setTotalCredit(null);
        transcript.setTranscriptDefinitionOid(
                studentSchedule.getSection().getSchoolCourse().getTranscriptDefinitionOid());

        transcript.setFieldValueByBeanPath(m_trnEnrollType,
                studentSchedule.getFieldValueByBeanPath(m_sscEnrollType));

        transcript.setFieldValueByBeanPath(m_trnProviderId,
                studentSchedule.getFieldValueByBeanPath(m_sscProviderId));

        transcript.setFieldValueByBeanPath(m_trnProviderCrsName,
                studentSchedule.getFieldValueByBeanPath(m_sscProviderCrsName));

        transcript.setFieldValueByBeanPath(m_trnProviderCrsId,
                studentSchedule.getFieldValueByBeanPath(m_sscProviderCrsId));

        transcript.setFieldValueByBeanPath(m_trnProviderCrsCredits,
                studentSchedule.getFieldValueByBeanPath(m_sscProviderCrsCredits));

        transcript.setFieldValueByBeanPath(m_trnCrsTerm,
                studentSchedule.getFieldValueByBeanPath(m_sscCrsTerm));

        transcript.setFieldValueByBeanPath(m_trnExReason,
                studentSchedule.getFieldValueByBeanPath(m_sscExReason));

        transcript.setFieldValueByBeanPath(m_trnCrsNetwork,
                studentSchedule.getFieldValueByBeanPath(m_sscCrsNetwork));

        logMessage("Transcript Was created for Std LASID = " + studentSchedule.getStudent().getLocalId()
                + ", Master Schedule = " + studentSchedule.getSection().getDescription());

        getBroker().saveBeanForced(transcript);
    }

    /**
     * Update given transcript from a student schedule object.
     *
     * @param ssc StudentSchedule
     * @param trn Transcript
     */
    private void trnUpdateBySchedule(StudentSchedule ssc, Transcript trn) {
        trn.setFieldValueByBeanPath(m_trnEnrollType, ssc.getFieldValueByBeanPath(m_sscEnrollType));
        trn.setFieldValueByBeanPath(m_trnProviderId, ssc.getFieldValueByBeanPath(m_sscProviderId));
        trn.setFieldValueByBeanPath(m_trnProviderCrsName, ssc.getFieldValueByBeanPath(m_sscProviderCrsName));
        trn.setFieldValueByBeanPath(m_trnProviderCrsId, ssc.getFieldValueByBeanPath(m_sscProviderCrsId));

        if (m_updPSCredits && !StringUtils.isEmpty(trn.getFinalGrade())) {
            TranscriptDefinition transcriptDefinition = trn.getTranscriptDefinition();
            if (transcriptDefinition != null) {
                TranscriptColumnDefinition column = transcriptDefinition.getFinalColumnDefinition();
                if (column != null) {
                    GradeScaleGradeDefinition gradeDefinition = getGradeDefinition(trn.getFinalGrade(), column);

                    if (gradeDefinition.getCreditIndicator()) {
                        trn.setFieldValueByBeanPath(m_trnCreditsEarned,
                                ssc.getFieldValueByBeanPath(m_sscProviderCrsCredits));
                    } else {
                        trn.setFieldValueByBeanPath(m_trnCreditsEarned, "0.00");
                    }
                }
            }
        }

        trn.setFieldValueByBeanPath(m_trnProviderCrsCredits, ssc.getFieldValueByBeanPath(m_sscProviderCrsCredits));
        trn.setFieldValueByBeanPath(m_trnCrsTerm, ssc.getFieldValueByBeanPath(m_sscCrsTerm));
        trn.setFieldValueByBeanPath(m_trnExReason, ssc.getFieldValueByBeanPath(m_sscExReason));
        trn.setFieldValueByBeanPath(m_trnCrsNetwork, ssc.getFieldValueByBeanPath(m_sscCrsNetwork));

        if (!StringUtils.isEmpty(trn.getFinalGrade()) || !m_hideTrnUntilFg) {
            trn.setTranscriptHideInd(false);
        } else if (m_hideTrnUntilFg) {
            trn.setTranscriptHideInd(true);
        }

        if (trn.isDirty()) {
            getBroker().saveBeanForced(trn);
            logMessage("Transcript was updated for Std LASID = " + trn.getStudent().getLocalId()
                    + ", Master Schedule = " + trn.getMasterSchedule().getCourseView());
        }
    }
}
