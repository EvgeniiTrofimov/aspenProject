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
package com.x2dev.procedures.statereporting;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.follett.fsc.core.k12.web.WebUtils;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.X2RuntimeException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * The Class CRDCReportData.
 */
public class CRDCReportData extends StateReportData {

    /**
     * The Enum AliasField.
     */
    public enum AliasField {
        AP_COURSE_CATEGORY("tblCourse", "CRDC AP Category", "CRDC AP Category", "all-crs-CRDCAPCourseCategory",
                "Character", "25", "CRDC AP Course Category"),

        AP_TEST_RESULT("tblStdTrans", "Took AP", "TookAP", "all-trn-CRDCAPTestResult", "Character", "2",
                "CRDC AP Test Results Code"),

        ASSESSMENT_CODE("tblAssessDef", "CRDC Assessment", "CRDCCode", "all-asd-CRDCAssessmentCode", "Character", "10",
                "CRDC SAT &amp; ACT"),

        CERTIFIED("tblSchMstTeach", "CRDC Certified Taught", "CRDC Certified", "all-mtc-CRDCCertified", "Logical", "1",
                ""),

        COURSE_CATEGORY("tblCourse", "CRDC Course Category", "CRDC Course Category", "all-crs-CRDCCourseCategory",
                "Character", "25", "CRDC Course Category"),

        CRDC_CODE("tblRefCode ", "CRDC Code", "CRDC Code", "all-rcd-CRDCCode", "Character", "50", ""),

        CREDIT_RECOVERY_SSC("tblStdSchedule", "CRDC Credit Recovery", "CRDC Credit Recovery",
                "all-ssc-CRDCCreditRecovery", "Logical", "", ""),

        CREDIT_RECOVERY_TRN("tblStdTrans", "Credit Recovery", "Credit Recovery", "all-trn-CRDCCreditRecovery",
                "Logical", "1", ""),

        DISTANCE_ED("tblCourse", "CRDC Distance Education", "CRDCDistEd", "all-crs-CRDCDistanceEd", "Logical", "1", ""),

        DUAL_ENROLLMENT("tblCourse", "CRDC Dual Enrollment", "CRDCDualEnroll", "all-crs-CRDCDualEnroll", "Logical", "1",
                ""),

        GED_PREP("tblCourse", "CRDC GED Prep", "CRDC GED Prep", "all-crs-CRDCGEDPrep", "Logical", "1", ""),

        GIFTED("tblCourse", "CRDC Gifted", "CRDC Gifted", "all-crs-CRDCGiftedTalented", "Logical", "1", ""),

        IB("tblCourse", "CRDC IB", "CRDC IB", "all-crs-CRDCIBCourse", "Logical", "1", ""),

        INIT_YEAR("tblStaff", "Year Began Teaching", "YearBeganTeaching", "all-stf-CRDCYearBeganTeaching", "Integer",
                "4", ""),

        NCES_ID("tblSchool", "NCES School ID", "NCES", "all-skl-NCESSchoolID", "Character", "12", ""),

        RI_DUAL_ENR_SSC("tblStdSchedule", "Post-Secondary Enrollment Type", "PSEnrollType", "all-ssc-PSEnrollmentType",
                "Character", "10", ""),

        RI_DUAL_ENR_TRN("tblStdTrans", "Post-Secondary Enrollment Type", "PSEnrollType", "all-trn-PSEnrollmentType",
                "Character", "10", ""),

        SINGLE_SEX_CRS("tblCourse", "Single Sex Course", "SingleSex", "all-crs-CRDCSingleSexCode", "Character", "10",
                "Gender Codes"),

        SINGLE_SEX_MST("tblSchMaster ", "Single Sex Course Override", "SingleSexOverride", "all-mst-CRDCSingleSexCode",
                "Character", "10", "Gender Codes");

        /**
         * Instantiates a new alias field.
         *
         * @param tableOid String
         * @param longName String
         * @param shortName String
         * @param alias String
         * @param userType String
         * @param userLength String
         * @param refTable String
         */
        private AliasField(String tableOid, String longName, String shortName, String alias, String userType,
                String userLength, String refTable) {
            m_tableOid = tableOid;
            m_longName = longName;
            m_shortName = shortName;
            m_alias = alias;
            m_userType = userType;
            m_userLength = userLength;
            m_refTable = refTable;
        }

        private String m_tableOid;
        private String m_longName;
        private String m_shortName;
        private String m_alias;
        private String m_userType;
        private String m_userLength;
        private String m_refTable;

        /**
         * Returns CalcParameter based on parameter name.
         *
         * @param alias String
         * @return CRDCDataHelper.CalcParameter
         */
        public static AliasField findAliasField(String alias) {
            AliasField match = null;

            for (AliasField field : AliasField.values()) {
                if (field.getAlias().equals(alias)) {
                    match = field;
                    break;
                }
            }

            return match;
        }

        /**
         * Gets the alias.
         *
         * @return String
         */
        public String getAlias() {
            return m_alias;
        }

        /**
         * Gets the error msg.
         *
         * @return String
         */
        public String getErrorMsg() {
            StringBuilder output = new StringBuilder();
            output.append("The alias could not be located. \nAlias characteristics: \n");
            append(output, "Alias", m_alias);
            append(output, "Table", m_tableOid);
            append(output, "Long Name", m_longName);
            append(output, "Short Name", m_shortName);
            append(output, "User Type", m_userType);
            append(output, "User Length", m_userLength);
            append(output, "Ref Table", m_refTable);
            return output.toString();
        }

        /**
         * Append.
         *
         * @param output StringBuilder
         * @param field String
         * @param value String
         */
        private void append(StringBuilder output, String field, String value) {
            output.append(field);
            output.append(": ");
            output.append(value);
            output.append("\n");
        }
    }

    /**
     * The Class RetrieverCRDCCode.
     */
    public class RetrieverCRDCCode implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            CRDCReportData crdcData = (CRDCReportData) data;
            String value = null;

            Object baseValue = null;
            // Look up the value through the bean.
            try {
                String beanPath = field.getBeanPath();
                if (!StringUtils.isEmpty(beanPath) && beanPath.charAt(0) != LABEL_PREFIX_CHAR) {
                    baseValue = WebUtils.getProperty(entity.getBean(), field.getBeanPath());
                }
            } catch (X2BaseException x2be) {
                // do nothing, must've been an empty bean path
            }

            value = crdcData.lookupCRDCCodeByBeanPath(entity.getBean().getClass(), field.getBeanPath(),
                    (String) baseValue);

            return value;
        }
    }

    /**
     * Retrieves a Boolean - true if values in the calc parameter contain the value in the bean
     * path, false otherwise.
     *
     * @author Follett Software Company
     */
    public class RetrieverIsEqual implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.
         *      core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Boolean result = Boolean.FALSE;
            String path = field.getBeanPath();

            try {
                Object propertyValue = WebUtils.getProperty(entity.getBean(), path);
                Collection<String> parameterValues = Arrays.asList(((String) field.getParameter()).split("[|]"));
                result = Boolean.valueOf(parameterValues.contains(propertyValue));
            } catch (X2BaseException e) {
                StateReportValidationError error = new StateReportValidationError(entity, field, e.getMessage(), path);
                entity.addRetrievalError(field.getFieldId(), error);
            }

            return result;
        }
    }
    /**
     * This class is created to work around issues operating this procedure with student history
     * helper
     * versions before 5.4.0.491 when context was supported for student schedules and student
     * schedule changes.
     *
     * THIS CLASS SHOULD BE REMOVED ONCE THESE VERSIONS ARE RETIRED FROM CLIENT SITES!!!!
     *
     * @author Follett Software Company
     */
    public class CRDCStudentHistoryHelper extends StudentHistoryHelper {
        /*
         * Standard aliases
         */
        private static final String ALIAS_EXCLUDE_CRS = "DOE EXCLUDE CRS";
        private static final String ALIAS_EXCLUDE_MST = "DOE EXCLUDE MST";
        private static final String ALIAS_EXCLUDE_STD = "DOE EXCLUDE STD";

        private PlainDate m_currentCRDCDate;
        private String m_fieldCRDCExcludeCrs;
        private String m_fieldCRDCExcludeMst;
        private String m_fieldCRDCExcludeStd;

        /**
         * Instantiates a new CRDC student history helper.
         *
         * @param data StateReportData
         */
        public CRDCStudentHistoryHelper(StateReportData data) {
            super(data);
            m_currentCRDCDate = new PlainDate(OrganizationManager.getTimeZone(getData().getOrganization()));

            m_fieldCRDCExcludeStd = getData().translateAliasToJavaName(ALIAS_EXCLUDE_STD, false);
            m_fieldCRDCExcludeCrs = getData().translateAliasToJavaName(ALIAS_EXCLUDE_CRS, false);
            m_fieldCRDCExcludeMst = getData().translateAliasToJavaName(ALIAS_EXCLUDE_MST, false);
        }

        /**
         * @see com.x2dev.sis.tools.stateexports.StudentHistoryHelper#buildStudentScheduleChangeCriteria()
         */
        @Override
        protected X2Criteria buildStudentScheduleChangeCriteria() {
            // Verify that we have enough information to start.
            if (getStudentSelectionMode() == null) {
                throw new X2RuntimeException();
            }

            // Identify parameters for student schedule selection.
            PlainDate endDate = (PlainDate) getCRDCStudentSelectionProperty(PROPERTY_END_DATE, PlainDate.class,
                    getData().getCurrentContext().getEndDate().before(m_currentCRDCDate)
                            ? getData().getCurrentContext().getEndDate()
                            : m_currentCRDCDate);
            Boolean applyExclude =
                    (Boolean) getCRDCStudentSelectionProperty(PROPERTY_APPLY_EXCLUDE, Boolean.class, Boolean.TRUE);
            Boolean excludeFutureSched = (Boolean) getCRDCStudentSelectionProperty(PROPERTY_EXCLUDE_FUTURE_SCHEDULES,
                    Boolean.class, Boolean.FALSE);
            Boolean applyInput =
                    (Boolean) getCRDCStudentSelectionProperty(PROPERTY_APPLY_INPUT, Boolean.class, Boolean.TRUE);
            Boolean applySchool =
                    (Boolean) getCRDCStudentSelectionProperty(PROPERTY_APPLY_SCHOOL, Boolean.class, Boolean.TRUE);

            X2Criteria studentScheduleChangeCriteria = new X2Criteria();

            studentScheduleChangeCriteria.addNotNull(StudentScheduleChange.COL_ACTION_DATE);

            // From Class type section
            studentScheduleChangeCriteria.addEqualTo(StudentScheduleChange.REL_MASTER_SCHEDULE + PATH_DELIMITER +
                    MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                    SchoolCourse.COL_MASTER_TYPE, SchoolCourse.MASTER_TYPE_CLASS);

            // From active Schedule
            studentScheduleChangeCriteria.addEqualTo(StudentScheduleChange.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.REL_ACTIVE_SCHOOL_SCHEDULE_CONTEXTS + PATH_DELIMITER +
                    SchoolScheduleContext.COL_DISTRICT_CONTEXT_OID,
                    getData().getCurrentContext().getOid());

            // section term started before report date.
            // Require section term to start before end/report date.
            if (excludeFutureSched.booleanValue()) {
                studentScheduleChangeCriteria
                        .addLessOrEqualThan(StudentScheduleChange.REL_MASTER_SCHEDULE + PATH_DELIMITER +
                                MasterSchedule.REL_SCHEDULE_TERM + PATH_DELIMITER +
                                ScheduleTerm.REL_SCHEDULE_TERM_DATES + PATH_DELIMITER +
                                ScheduleTermDate.COL_START_DATE, endDate);
            }

            // check school or organization selection.
            if (applySchool.booleanValue() && getData().isSchoolContext()) {
                studentScheduleChangeCriteria.addEqualTo(StudentScheduleChange.REL_SCHEDULE + PATH_DELIMITER +
                        Schedule.COL_SCHOOL_OID, getData().getSchool().getOid());
            } else {
                studentScheduleChangeCriteria.addNotEqualTo(StudentScheduleChange.REL_SCHEDULE + PATH_DELIMITER +
                        Schedule.REL_SCHOOL + PATH_DELIMITER +
                        SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
                studentScheduleChangeCriteria.addNotEqualTo(StudentScheduleChange.REL_SCHEDULE + PATH_DELIMITER +
                        Schedule.REL_SCHOOL + PATH_DELIMITER +
                        SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
            }

            // Check exclusion flags for student, section and student schedule.
            if (applyExclude.booleanValue() && !StringUtils.isEmpty(m_fieldCRDCExcludeStd)) {
                studentScheduleChangeCriteria.addNotEqualTo(StudentScheduleChange.REL_STUDENT + PATH_DELIMITER +
                        m_fieldCRDCExcludeStd,
                        BooleanAsStringConverter.TRUE);
            }

            // Check if the section exclusion custom field is present.
            if (applyExclude.booleanValue() && !StringUtils.isEmpty(m_fieldCRDCExcludeMst)) {
                studentScheduleChangeCriteria.addNotEqualTo(StudentScheduleChange.REL_MASTER_SCHEDULE + PATH_DELIMITER +
                        m_fieldCRDCExcludeMst,
                        BooleanAsStringConverter.TRUE);
            }

            // Check if the course exclusion custom field is present.
            if (applyExclude.booleanValue() && !StringUtils.isEmpty(m_fieldCRDCExcludeCrs)) {
                studentScheduleChangeCriteria.addNotEqualTo(StudentScheduleChange.REL_MASTER_SCHEDULE + PATH_DELIMITER +
                        MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                        SchoolCourse.REL_COURSE + PATH_DELIMITER +
                        m_fieldCRDCExcludeCrs,
                        BooleanAsStringConverter.TRUE);
            }

            // Check if the user criteria needs to be applied.
            if (applyInput.booleanValue()) {
                getData().applyInputCriteria(studentScheduleChangeCriteria, false, StudentScheduleChange.REL_STUDENT);
            }

            return studentScheduleChangeCriteria;
        }

        /**
         * @see com.x2dev.sis.tools.stateexports.StudentHistoryHelper#buildStudentScheduleCriteria()
         */
        @Override
        protected X2Criteria buildStudentScheduleCriteria() {
            // Verify that we have enough information to start.
            if (getStudentSelectionMode() == null) {
                throw new X2RuntimeException();
            }

            // Identify parameters for student schedule selection.
            PlainDate endDate = (PlainDate) getCRDCStudentSelectionProperty(PROPERTY_END_DATE, PlainDate.class,
                    getData().getCurrentContext().getEndDate().before(m_currentCRDCDate)
                            ? getData().getCurrentContext().getEndDate()
                            : m_currentCRDCDate);
            Boolean applyExclude =
                    (Boolean) getCRDCStudentSelectionProperty(PROPERTY_APPLY_EXCLUDE, Boolean.class, Boolean.TRUE);
            Boolean excludeFutureSched = (Boolean) getCRDCStudentSelectionProperty(PROPERTY_EXCLUDE_FUTURE_SCHEDULES,
                    Boolean.class, Boolean.FALSE);
            Boolean applyInput =
                    (Boolean) getCRDCStudentSelectionProperty(PROPERTY_APPLY_INPUT, Boolean.class, Boolean.TRUE);
            Boolean applySchool =
                    (Boolean) getCRDCStudentSelectionProperty(PROPERTY_APPLY_SCHOOL, Boolean.class, Boolean.TRUE);

            X2Criteria studentScheduleCriteria = new X2Criteria();

            // Master type Class
            studentScheduleCriteria.addEqualTo(StudentSchedule.REL_SECTION + PATH_DELIMITER +
                    MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                    SchoolCourse.COL_MASTER_TYPE, SchoolCourse.MASTER_TYPE_CLASS);

            // From active Schedule for the selected year.
            studentScheduleCriteria.addEqualTo(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.REL_ACTIVE_SCHOOL_SCHEDULE_CONTEXTS + PATH_DELIMITER +
                    SchoolScheduleContext.COL_DISTRICT_CONTEXT_OID,
                    getData().getCurrentContext().getOid());

            // Require section term to start before end/report date.
            if (excludeFutureSched.booleanValue()) {
                studentScheduleCriteria.addLessOrEqualThan(StudentSchedule.REL_SECTION + PATH_DELIMITER +
                        MasterSchedule.REL_SCHEDULE_TERM + PATH_DELIMITER +
                        ScheduleTerm.REL_SCHEDULE_TERM_DATES + PATH_DELIMITER +
                        ScheduleTermDate.COL_START_DATE, endDate);
            }

            // check school or organization selection.
            if (applySchool.booleanValue() && getData().isSchoolContext()) {
                studentScheduleCriteria.addEqualTo(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER +
                        Schedule.COL_SCHOOL_OID, getData().getSchool().getOid());
            } else {
                studentScheduleCriteria.addNotEqualTo(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER +
                        Schedule.REL_SCHOOL + PATH_DELIMITER +
                        SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
                studentScheduleCriteria.addNotEqualTo(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER +
                        Schedule.REL_SCHOOL + PATH_DELIMITER +
                        SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
            }

            // Check exclusion flags for student, section and student schedule.
            if (applyExclude.booleanValue() && !StringUtils.isEmpty(m_fieldCRDCExcludeStd)) {
                studentScheduleCriteria.addNotEqualTo(StudentSchedule.REL_STUDENT + PATH_DELIMITER +
                        m_fieldCRDCExcludeStd,
                        BooleanAsStringConverter.TRUE);
            }

            // Check if the section exclusion custom field is present.
            if (applyExclude.booleanValue() && !StringUtils.isEmpty(m_fieldCRDCExcludeMst)) {
                studentScheduleCriteria.addNotEqualTo(StudentSchedule.REL_SECTION + PATH_DELIMITER +
                        m_fieldCRDCExcludeMst,
                        BooleanAsStringConverter.TRUE);
            }

            // Check if the course exclusion custom field is present.
            if (applyExclude.booleanValue() && !StringUtils.isEmpty(m_fieldCRDCExcludeCrs)) {
                studentScheduleCriteria.addNotEqualTo(StudentSchedule.REL_SECTION + PATH_DELIMITER +
                        MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                        SchoolCourse.REL_COURSE + PATH_DELIMITER +
                        m_fieldCRDCExcludeCrs,
                        BooleanAsStringConverter.TRUE);
            }

            // Check if the user criteria needs to be applied.
            if (applyInput.booleanValue()) {
                getData().applyInputCriteria(studentScheduleCriteria, false, StudentSchedule.REL_STUDENT);
            }

            return studentScheduleCriteria;
        }

        /**
         * @see com.x2dev.sis.tools.stateexports.StudentHistoryHelper#buildStudentTranscriptCriteria()
         */
        @Override
        protected X2Criteria buildStudentTranscriptCriteria() {
            X2Criteria transcriptCriteria = new X2Criteria();
            transcriptCriteria.addEqualTo(Transcript.COL_DISTRICT_CONTEXT_OID, getData().getCurrentContext().getOid());

            // check school or organization selection.
            if (getData().isSchoolContext()) {
                transcriptCriteria.addEqualTo(Transcript.REL_SCHOOL,
                        getData().getSchool().getOid());
            } else {
                transcriptCriteria.addNotEqualTo(Transcript.REL_SCHOOL + PATH_DELIMITER +
                        SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
                transcriptCriteria.addNotEqualTo(Transcript.REL_SCHOOL + PATH_DELIMITER +
                        SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
            }

            return transcriptCriteria;
        }

        /**
         * Return the user value entered for a parameter. Verify that the value matches a specified
         * type.
         *
         * @param selectKey The key of the property to retrieve.
         * @param expectedClass The class type expected for the value.
         * @param defaultValue The value to return if the property is not present or is null.
         *
         * @return Object
         */
        Object getCRDCStudentSelectionProperty(String selectKey, Class expectedClass, Object defaultValue) {
            Object value = getSelectionProperty(selectKey);
            if (value != null) {
                if (!expectedClass.isInstance(value)) {
                    throw new ClassCastException("getCRDCStudentSeletionProperty(" + selectKey + "): Expected "
                            + expectedClass.getName() + ", found " + value.getClass().getName());
                }
            } else {
                value = defaultValue;
            }
            return value;
        }

    }

    public static final String ALIAS_CRDC_REF_CODE = "all-rcd-CRDCCode";
    protected static final String PARAM_EXCLUDE_ENR_SKL_NON_NCES = "excludeEnrSklNonNCES";
    protected static final String PARAM_PART_1_DATE = "reportDatePart1";
    protected static final String PARAM_USE_BLOCK_SCHEDULING = "useBlockScheduling";
    private static final String ERROR_ALIAS_LOOKUP = "error.state.report.alias";
    private static final String PATTERN_TABLE_PREFIX = "^[a-z]{2,3}-([a-z]{3})-.+";

    private String m_fieldCRDCCode;
    private Pattern m_patternTablePrefix;

    /**
     * Gets the codes for CRDC value.
     *
     * @param beanClass Class
     * @param columnName String
     * @param codes Collection<String>
     * @return Sets the
     */
    public Set<String> getCodesForCRDCValue(Class beanClass, String columnName, Collection<String> codes) {
        Set<String> value = new HashSet();
        // make sure at least one value is included.
        value.add("__dummy__");

        ModelProperty prop = new ModelProperty(beanClass, columnName, getBroker().getPersistenceKey());
        DataDictionaryField field = getDataDictionary().findDataDictionaryField(prop.getFieldId());
        DataDictionaryField aliasField =
                getDataDictionary().findDataDictionaryFieldByAlias(CRDCReportData.ALIAS_CRDC_REF_CODE);
        if (!StringUtils.isEmpty(field.getReferenceTableOid())) {
            Map<String, String> map = CRDCReportData.getCRDCCodeLookup(getBroker(), aliasField.getJavaName(),
                    field.getReferenceTableOid(), codes);
            value.addAll(map.keySet());
        }
        return value;
    }

    /**
     * Gets the CRDC code lookup.
     *
     * @param broker X2Broker
     * @param crdcCodeField String
     * @param referenceTableOid String
     * @param codes Collection<String>
     * @return Map
     */
    public static Map<String, String> getCRDCCodeLookup(X2Broker broker,
                                                        String crdcCodeField,
                                                        String referenceTableOid,
                                                        Collection<String> codes) {
        Set<String> upperCaseCodes = new HashSet();
        for (String code : codes) {
            upperCaseCodes.add(code.toUpperCase());
        }

        Map<String, String> crdcCodeLookup = new HashMap();
        X2Criteria criteria = new X2Criteria();
        criteria.addIn(crdcCodeField, codes);
        criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, referenceTableOid);

        String[] columns = new String[] {ReferenceCode.COL_CODE, crdcCodeField};

        ReportQueryByCriteria query = new ReportQueryByCriteria(ReferenceCode.class, columns, criteria);

        ReportQueryIterator iterator = broker.getReportQueryIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                Object[] record = (Object[]) iterator.next();
                String code = (String) record[0];
                String crdcCode = (String) record[1];
                if (!StringUtils.isEmpty(crdcCode) && upperCaseCodes.contains(crdcCode.toUpperCase())) {
                    crdcCodeLookup.put(code, crdcCode.toUpperCase());
                }
            }
        } finally {
            iterator.close();
        }
        return crdcCodeLookup;
    }

    /**
     * @return the m_fieldCRDCCode
     */
    public String getFieldCRDCCode() {
        return m_fieldCRDCCode;
    }

    /**
     * Returns the CRDC code for field value.
     * Look up based on bean path.
     *
     * @param alias String
     * @param value - the value to lookup and translate in the lookup table.
     * @return String - CRDC code for input value
     */
    public String lookupCRDCCodeByAlias(String alias, String value) {
        String crdcValue = null;
        DataDictionaryField dictionaryField = getDataDictionary().findDataDictionaryFieldByAlias(alias);
        if (dictionaryField != null && dictionaryField.hasReferenceTable()) {
            crdcValue = lookupCRDCCodeByRefTbl(dictionaryField.getReferenceTableOid(), value);
        }

        return crdcValue;
    }

    /**
     * Returns the CRDC lookup code for field value.
     * Look up based on bean path.
     *
     * @param beanClass - data dictionary table class to lookup in
     * @param beanPath - data dictionary field path from the passed table to lookup in
     * @param value - the value to lookup and translate in the lookup table.
     *
     * @return String - CRDC code for input value.
     */
    public String lookupCRDCCodeByBeanPath(Class beanClass, String beanPath, String value) {
        String crdcValue = null;
        DataDictionaryField dictionaryField = getDataDictionaryField(beanClass, beanPath);
        if (dictionaryField != null && dictionaryField.hasReferenceTable()) {
            crdcValue = lookupCRDCCodeByRefTbl(dictionaryField.getReferenceTableOid(), value);
        }

        return crdcValue;
    }

    /**
     * Returns the lookup code value for field value.
     * Look up based on the reference table.
     *
     * @param referenceTableOid String
     * @param value - the value to lookup and translate in the lookup table.
     * @return String - CRDC code lookup value for input value.
     */
    public String lookupCRDCCodeByRefTbl(String referenceTableOid, String value) {
        String returnValue = null;
        Map<String, ReferenceCode> refCodes = getReferenceCodes(referenceTableOid);
        ReferenceCode code = refCodes.get(value);
        if (code != null) {
            return value = (String) code.getFieldValueByBeanPath(m_fieldCRDCCode);
        }

        return returnValue;
    }

    /**
     * Translates an alias into a Java bean path name. An initialization error will be logged
     * if the alias does not exist.
     *
     * @param alias String
     * @param required boolean
     * @return String
     */
    @Override
    public String translateAliasToJavaName(String alias, boolean required) {
        String javaName = null;
        String tablePrefix = null;

        if (m_patternTablePrefix == null || true) {
            m_patternTablePrefix = Pattern.compile(PATTERN_TABLE_PREFIX);
        }
        Matcher matcher = m_patternTablePrefix.matcher(alias);
        if (matcher.matches()) {
            tablePrefix = matcher.group(1);
        }

        DataDictionaryField field = getDataDictionary().findDataDictionaryFieldByAlias(alias);
        if (field != null) {
            if (!field.isEnabled()) {
                addSetupError("Alias field is not enabled", alias);
            } else if (tablePrefix != null && !tablePrefix.equalsIgnoreCase(field.getTable().getObjectPrefix())) {
                addSetupError("Alias field is defined on the wrong table", alias);
            } else {
                javaName = field.getJavaName();
            }
        } else if (required) {
            AliasField aliasField = AliasField.findAliasField(alias);
            String aliasMsg =
                    LocalizationCache.getMessages(getBroker().getPersistenceKey()).getMessage(ERROR_ALIAS_LOOKUP);
            if (aliasField != null) {
                aliasMsg = aliasField.getErrorMsg();
            }
            addSetupError(aliasMsg, alias);
        }

        return javaName;
    }

    /**
     * Initialize CRDC field name.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();

        m_fieldCRDCCode = this.translateAliasToJavaName(ALIAS_CRDC_REF_CODE, true);

        Map<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
        calcs.put(CALC_TYPE_ISEQUAL, new RetrieverIsEqual());
        addCalcs(calcs);
    }

}
