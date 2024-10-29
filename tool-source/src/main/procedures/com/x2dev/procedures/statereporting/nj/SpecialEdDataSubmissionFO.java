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
package com.x2dev.procedures.statereporting.nj;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.ibm.icu.text.SimpleDateFormat;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * New Jersey state procedure for SpecialEd export. This is a FailOver.
 *
 * @author X2 Development Corporation
 */

public class SpecialEdDataSubmissionFO extends StateReportData {
    /**
     * Entity class for Special Education Data Submission export.
     *
     * @author X2 Development Corporation
     */

    public static class SpecialEdDataEntity extends StateReportEntity {

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public SpecialEdDataEntity() {
            // no argument constructor
        }

        /**
         * Initialize the data module.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            SisStudent student = (SisStudent) bean;
            SpecialEdDataSubmissionFO spData = (SpecialEdDataSubmissionFO) data;
            StudentEnrollment activeEnr =
                    spData.m_helper.getEnrollmentForDate(student.getOid(), spData.m_snapshotDate, "E");
            if (activeEnr != null) {
                String spedCode = student.getSpedStatusCode();
                if (!StringUtils.isEmpty(spedCode)) {
                    spedCode = spData.lookupStateValue(SisStudent.class, SisStudent.COL_SPED_STATUS_CODE, spedCode);
                    if (!StringUtils.isEmpty(spedCode)) {
                        switch (spedCode) {
                            case "99":
                                if (student.getSpedExitDate() == null
                                        || !student.getSpedExitDate().after(spData.m_snapshotDatePrior)
                                        || student.getSpedExitDate().after(spData.m_snapshotDate)) {
                                    setRowCount(0);
                                }
                                break;
                            case "R":
                                if (student.getFieldValueByBeanPath(spData.m_fieldStdRefDate) == null
                                        || !spData.m_snapshotDatePrior.before(
                                                (PlainDate) student.getFieldValueByBeanPath(spData.m_fieldStdRefDate))
                                        || spData.m_snapshotDate.before((PlainDate) student
                                                .getFieldValueByBeanPath(spData.m_fieldStdRefDate))) {
                                    setRowCount(0);
                                }
                                break;
                            case "00":
                                if (student.getFieldValueByBeanPath(spData.m_fieldStdInitEligDate) == null
                                        || !spData.m_snapshotDatePrior.before((PlainDate) student
                                                .getFieldValueByBeanPath(spData.m_fieldStdInitEligDate))
                                        || spData.m_snapshotDate.before((Date) student
                                                .getFieldValueByBeanPath(spData.m_fieldStdInitEligDate))) {
                                    setRowCount(0);
                                }
                                break;
                            default:
                                break;
                        }
                    } else {
                        setRowCount(0);
                    }
                }
            } else {
                setRowCount(0);
            }
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            String name = ((SisStudent) this.getBean()).getNameView() +
                    " [LASID: " + ((SisStudent) this.getBean()).getLocalId() +
                    ", SASID: " + ((SisStudent) this.getBean()).getStateId() +
                    "] ";
            return name;
        }
    }

    /**
     * This retriever retrieves all the IEP related data from a student's IEP.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveSpEdData implements FieldRetriever {

        protected static final String CALC_ID = "SPED-RETRIEVE";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            String param = (String) field.getParameter();
            SisStudent student = (SisStudent) entity.getBean();
            Object value = null;

            if (student != null) {
                if (PARAM_CONSENT_OBTAINED.equals(param)) {
                    if (getValueInSnapshotRangeForStudent(student, m_fieldStdInitEligDate, false, false) != null) {
                        String iepStatusCode = student.getSpedStatusCode();
                        String notEligibleCode = PreferenceManager.getPreferenceValue(getOrganization(),
                                SisPreferenceConstants.SPED_INELIGIBLE_CODE);
                        if (!StringUtils.isEmpty(notEligibleCode) && !StringUtils.isEmpty(iepStatusCode)
                                && iepStatusCode.equalsIgnoreCase(notEligibleCode)) {
                            value = PARENTAL_CONSENT_REFUSED;
                        } else {
                            // The student is eligible
                            PlainDate parentalConsentToImplementDate = student.getSpedLastEvaluationDate();
                            if (parentalConsentToImplementDate != null) {
                                value = PARENTAL_CONSENT_OBTAINED;
                                if (m_snapshotDate.before(parentalConsentToImplementDate)) {
                                    value = PARENTAL_CONSENT_NOT_OBTAINED;
                                }
                            } else if (parentalConsentToImplementDate == null) {
                                value = PARENTAL_CONSENT_NOT_OBTAINED;
                            }
                        }
                    }
                }
            }
            return value;
        }
    }

    /**
     * This retriever retrieves all the IEP related data from a student's IEP.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveStudentData implements FieldRetriever {

        /**
         * Calculation ID
         */
        protected static final String CALC_ID = "STD-DATA";

        /**
         * Calculation Parameters
         */
        protected static final String CALC_PARAM_ANNUAL_REV_DATE = "ANNUAL_REV_DATE";
        protected static final String CALC_PARAM_CONSENT_EP_DATE = "CONSENT_DATE";
        protected static final String CALC_PARAM_DELAY_REASON = "DELAY_REASON";
        protected static final String CALC_PARAM_EP_MEETING_DATE = "EP_MEETING_DATE";
        protected static final String CALC_PARAM_IMPLEMENTATION_DATE = "IMPLEMENT_DATE";
        protected static final String CALC_PARAM_INIT_ELIG_DATE = "ELIG_DATE";
        protected static final String CALC_PARAM_REEVALUATION_DATE = "REEVALUATION_DATE";
        protected static final String CALC_PARAM_REF_DATE = "REF_DATE";
        protected static final String CALC_PARAM_TIME_IN_REG = "TIME_IN_REG";

        /**
         * Export Format Fields
         */
        protected static final String EXP_FIELD_120_IMPL_INIT_IEP = "ImplementInitialEP";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            String param = (String) field.getParameter();
            SisStudent student = (SisStudent) entity.getBean();
            Object value = null;
            if (student != null) {
                if (CALC_PARAM_REF_DATE.equals(param)) {
                    value = getValueInSnapshotRangeForStudent(student, m_fieldStdRefDate, false, false);
                } else if (CALC_PARAM_CONSENT_EP_DATE.equals(param)) {
                    value = getValueInSnapshotRangeForStudent(student, m_fieldStdConsentEpDate, false, false);
                } else if (CALC_PARAM_INIT_ELIG_DATE.equals(param)) {
                    value = getValueInSnapshotRangeForStudent(student, m_fieldStdInitEligDate, false, false);
                } else if (CALC_PARAM_EP_MEETING_DATE.equals(param)) {
                    value = getValueInSnapshotRangeForStudent(student, m_fieldStdEpMeetingDate, false, false);
                } else if (CALC_PARAM_IMPLEMENTATION_DATE.equals(param)) {
                    value = getValueInSnapshotRangeForStudent(student, m_fieldStdImplementaionDate, false, false);
                } else if (CALC_PARAM_ANNUAL_REV_DATE.equals(param)) {
                    value = getValueInSnapshotRangeForStudent(student, m_fieldStdAnnualMeetDate, true, false);
                } else if (CALC_PARAM_REEVALUATION_DATE.equals(param)) {
                    value = getValueInSnapshotRangeForStudent(student, m_fieldStdReevaluationDate, true, false);
                } else if (CALC_PARAM_DELAY_REASON.equals(param)
                        && !StringUtils.isEmpty(entity.getFieldValue(EXP_FIELD_120_IMPL_INIT_IEP))) {
                    String code = (String) student.getFieldValueByBeanPath(m_fieldStdDelayProcess);
                    if (!StringUtils.isEmpty(code)) {
                        value = lookupStateValue(SisStudent.class, m_fieldStdDelayProcess, code);
                    }
                } else if (CALC_PARAM_TIME_IN_REG.equals(param)) {
                    Object tempValue = getPropertyAsJavaType(student, m_fieldStdTimeInReg);
                    if (tempValue != null && tempValue instanceof Number) {
                        BigDecimal valueToReturn = (BigDecimal) tempValue;
                        if (valueToReturn.compareTo(BigDecimal.ZERO) > 0) {
                            value = new DecimalFormat("####").format(valueToReturn);
                        }
                    }
                }
            }
            return value;
        }
    }

    /**
     * Aliases
     */
    protected static final String ALIAS_EXCLUDE_SCHOOL = "DOE EXCLUDE SKL";
    protected static final String ALIAS_STD_ANNUAL_MEET_DATE = "DOE ANNUAL IEP REVIEW MEET DATE";
    protected static final String ALIAS_STD_DELAY_PROCESS = "DOE PROCESS DELAY REASON";
    protected static final String ALIAS_STD_EP_MEETING_DATE = "DOE EP MEETING DATE";
    protected static final String ALIAS_STD_IMPLEMENTATION_DATE = "DOE PARENT CONSENT TO IMP INITIAL IEP";
    protected static final String ALIAS_STD_INIT_ELIG_DATE = "DOE INITIAL ELIGIBILITY DATE";
    protected static final String ALIAS_STD_CONSENT_EP_DATE = "DOE PARENTAL CONSENT EP DATE";
    protected static final String ALIAS_STD_REEVALUATION = "DOE REEVALUATION DATE";
    protected static final String ALIAS_STD_REF_DATE = "DOE SPED REFERRAL DATE";
    protected static final String ALIAS_STD_TIME_IN_REG = "DOE TIME IN REGULAR PROGRAM";


    /**
     * Parameters
     */
    protected static final String PARAM_CONSENT_OBTAINED = "CONSENT_OBTAINED";
    protected static final String PARAM_SNAPSHOT_DATE = "snapshotDate";
    protected static final String PARAM_SNAPSHOT_DATE_PRIOR = "priorSnapshotDate";

    /**
     * Other Constants
     */
    protected static final SimpleDateFormat DATE_FORMAT = new SimpleDateFormat("yyyyMMdd");
    protected static final String PARENTAL_CONSENT_NOT_OBTAINED = "N";
    protected static final String PARENTAL_CONSENT_OBTAINED = "Y";
    protected static final String PARENTAL_CONSENT_REFUSED = "R";
    protected static final List<String> SPED_STATE_CODES = Arrays.asList("99", "R", "00", "A");

    /**
     * Supporting instance variables.
     * These are protected rather than private so they can be accessed by the inner classes.
     */
    /**
     * This is the end of school date for the district. It is retrieved using the current context of
     * the
     * current organization.
     */
    protected String m_excludeSchool;
    protected Map m_excludeSchoolMap;
    protected String m_fieldStdAnnualMeetDate;
    protected String m_fieldStdConsentEpDate;
    protected String m_fieldStdDelayProcess;
    protected String m_fieldStdEpMeetingDate;
    protected String m_fieldStdImplementaionDate;
    protected String m_fieldStdInitEligDate;
    protected String m_fieldStdReevaluationDate;
    protected String m_fieldStdRefDate;
    protected String m_fieldStdTimeInReg;
    /**
     * StudentHistoryHelper class instance.
     */
    protected StudentHistoryHelper m_helper;

    protected PlainDate m_snapshotDate;
    protected PlainDate m_snapshotDatePrior;

    /**
     * Checks if schoolOid given is a school to exclude. If that school is excluded this method will
     * return false ie - !included.
     *
     * @param schoolOid String
     * @return boolean
     */
    public boolean includeSchool(String schoolOid) {
        return (m_excludeSchoolMap == null) || !m_excludeSchoolMap.containsKey(schoolOid);
    }

    /**
     * Initialize the data module.
     */
    @Override
    public void initialize() {
        initializeFields();
        if (getSetupErrors().size() == 0) {
            loadSchoolExcludeMap();
            /*
             * Build helper object.
             */
            m_helper = new StudentHistoryHelper(this);
            m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_SNAPSHOT);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_snapshotDate);
            ArrayList<String> spedCodes = getSpedCodesByState();
            if (!spedCodes.isEmpty()) {
                m_helper.getStudentCriteria().addIn(SisStudent.COL_SPED_STATUS_CODE, spedCodes);
            } else {
                m_helper.getStudentCriteria().addEqualTo(SisStudent.COL_SPED_STATUS_CODE, "___dummy___");
            }
            QueryByCriteria activeStudentQuery = m_helper.getStudentQuery(false);

            // Set the query to be used for student selection.
            setQuery(activeStudentQuery);
            setEntityClass(SpecialEdDataEntity.class);

            // Build a map of calculations/retrievers
            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put(RetrieveSpEdData.CALC_ID, new RetrieveSpEdData());
            calcs.put(RetrieveStudentData.CALC_ID, new RetrieveStudentData());
            super.addCalcs(calcs);
        }
    }

    private void initializeFields() {
        // Get the IEP data dictionary
        m_snapshotDate = (PlainDate) getParameter(PARAM_SNAPSHOT_DATE);
        m_snapshotDatePrior = (PlainDate) getParameter(PARAM_SNAPSHOT_DATE_PRIOR);
        if (m_snapshotDatePrior.after(m_snapshotDate)) {
            addSetupError("Incorrect Input Dates", "\"Prior Snapshot Date\" should be before \"Snapshot Date\".");
            return;
        }
        m_excludeSchool = translateAliasToJavaName(ALIAS_EXCLUDE_SCHOOL, true);
        m_fieldStdConsentEpDate = translateAliasToJavaName(ALIAS_STD_CONSENT_EP_DATE, true);
        m_fieldStdEpMeetingDate = translateAliasToJavaName(ALIAS_STD_EP_MEETING_DATE, true);
        m_fieldStdInitEligDate = translateAliasToJavaName(ALIAS_STD_INIT_ELIG_DATE, true);
        m_fieldStdRefDate = translateAliasToJavaName(ALIAS_STD_REF_DATE, true);
        m_fieldStdReevaluationDate = translateAliasToJavaName(ALIAS_STD_REEVALUATION, true);
        m_fieldStdDelayProcess = translateAliasToJavaName(ALIAS_STD_DELAY_PROCESS, true);
        m_fieldStdImplementaionDate = translateAliasToJavaName(ALIAS_STD_IMPLEMENTATION_DATE, true);
        m_fieldStdAnnualMeetDate = translateAliasToJavaName(ALIAS_STD_ANNUAL_MEET_DATE, true);
        m_fieldStdTimeInReg = translateAliasToJavaName(ALIAS_STD_TIME_IN_REG, true);
    }

    /**
     * Loads a map of schools that have been selected to be excluded from state reporting. (exclude
     * from reporting on school table is selected)
     */
    private void loadSchoolExcludeMap() {
        X2Criteria schoolCriteria = new X2Criteria();
        schoolCriteria.addEqualTo(m_excludeSchool, BooleanAsStringConverter.TRUE);
        BeanQuery query = new BeanQuery(School.class, schoolCriteria);
        m_excludeSchoolMap = getBroker().getGroupedCollectionByQuery(query, X2BaseBean.COL_OID, 128);
    }

    /**
     * Return appropriate sped codes.
     *
     * @return
     */
    private ArrayList<String> getSpedCodesByState() {
        ArrayList<String> codesToReturn = new ArrayList<>();
        DataDictionaryField ddField = getDataDictionaryField(SisStudent.class, SisStudent.COL_SPED_STATUS_CODE);
        if (ddField != null && ddField.hasReferenceTable()) {
            Map<String, ReferenceCode> codes = ddField.getReferenceTable().getCodeMap();
            for (Entry<String, ReferenceCode> entry : codes.entrySet()) {
                if (SPED_STATE_CODES.contains(entry.getValue().getStateCode())) {
                    codesToReturn.add(entry.getKey());
                }
            }
        }
        return codesToReturn;
    }

    /**
     *
     * @param student
     * @param beanPath
     * @return
     * @throws X2BaseException
     */
    private Object getValueInSnapshotRangeForStudent(SisStudent student,
                                                     String beanPath,
                                                     boolean isOnlyLessThanSnapshot,
                                                     boolean isOnlyGreaterThanPriorSnapshot)
            throws X2BaseException {
        Object value = null;
        if (student.getFieldValueByBeanPath(beanPath) != null) {
            Object dateToOperate = getPropertyAsJavaType(student, beanPath);
            if (dateToOperate != null && dateToOperate instanceof Date) {
                if (!isOnlyLessThanSnapshot && !isOnlyGreaterThanPriorSnapshot) {
                    if (((PlainDate) dateToOperate).after(m_snapshotDatePrior)
                            && !((PlainDate) dateToOperate).after(m_snapshotDate)) {
                        value = DATE_FORMAT.format(dateToOperate);
                    }
                } else if (isOnlyLessThanSnapshot) {
                    if (!((PlainDate) dateToOperate).after(m_snapshotDate)) {
                        value = DATE_FORMAT.format(dateToOperate);
                    }
                } else if (((PlainDate) dateToOperate)
                        .after(m_snapshotDatePrior)) {
                    value = DATE_FORMAT.format(dateToOperate);
                }
            }
        }
        return value;
    }
}
