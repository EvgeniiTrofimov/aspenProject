/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2012 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.procedures.statereporting.wa;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.HealthImmunizationDefinition;
import com.x2dev.sis.model.beans.HealthImmunizationDose;
import com.x2dev.sis.model.beans.HealthImmunizationGroup;
import com.x2dev.sis.model.beans.HealthImmunizationGroupOverride;
import com.x2dev.sis.model.beans.HealthImmunizationRuleAttributes;
import com.x2dev.sis.model.beans.HealthImmunizationRuleInstance;
import com.x2dev.sis.model.beans.HealthImmunizationSeries;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.business.GradeLevelHistory;
import com.x2dev.sis.model.business.health.ImmunizationRuleEngine;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.DateAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.*;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * This class implements a data extract that is used by WAImmunizationSummary to generate the
 * Washington immunization reports.
 *
 * @author Administrator
 */
public class WAImmunization extends StateReportData {

    /**
     * Entity class for WAImmunization data export.
     *
     * @author Administrator
     */
    public static class WAImmunizationEntity extends StateReportEntity {
        private static final String EFD_EXPDATA_WA_HIS_RPT = "RPT";

        private WAImmunization m_reportData;
        private Map<String, ImmunizationStatus> m_mapStatus;
        private String m_isCISFormSubmitted;

        /**
         * Gets the current format definition id.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#
         *      getCurrentFormatDefinitionId()
         */
        @Override
        public String getCurrentFormatDefinitionId() {
            String currentFormatDefinition = null;

            // Only fields of this export if parameter was not set.
            if (StringUtils.isEmpty(m_reportData.m_format)) {
                currentFormatDefinition = null;
            }
            // All fields including needed for CVS reports.
            else {
                currentFormatDefinition = EFD_EXPDATA_WA_HIS_RPT;
            }

            return currentFormatDefinition;
        }

        /**
         * Create list of immunization codes for a particular student
         * A state reference code column for an immunization can contain multiple
         * comma separated codes, allowing a single immunization to provide protection
         * for multiple state immunizations.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            m_reportData = (WAImmunization) data;
            SisStudent student = (SisStudent) bean;

            m_mapStatus = new HashMap<String, ImmunizationStatus>();

            for (HealthImmunizationRuleAttributes ruleAttributes : m_reportData.m_definitions) {
                // Get state code - skip series that are not on state list
                String code = getImmunizationCode(ruleAttributes);
                if (StringUtils.isEmpty(code) || StringUtils.isEmpty(ruleAttributes.getRuleDefinition())) {
                    continue;
                }
                // get series and doses
                HealthImmunizationRuleInstance ruleInstance = m_reportData.getRuleInstance(ruleAttributes, student);
                m_isCISFormSubmitted = getConditOrNonComplStatus(ruleInstance);
                List<HealthImmunizationDose> doses = m_reportData.getDoses(ruleAttributes, student);

                String exemptReason = ruleInstance == null ? null : getExemptReason(ruleInstance);

                // Get Waived status (series and doses) and waiver reason.
                boolean isWaived = ruleInstance == null ? false : ruleInstance.getWaivedIndicator();
                for (HealthImmunizationDose dose : doses) {
                    isWaived = isWaived || dose.getWaivedIndicator();
                    if (dose.getWaivedIndicator() && StringUtils.isEmpty(exemptReason)) {
                        exemptReason = getExemptReason(dose, m_reportData.m_fieldImmunizationDoseExempt);
                    }
                }

                // Get compliance status.
                ImmunizationRuleEngine engine =
                        new ImmunizationRuleEngine(ruleAttributes, m_reportData.m_history, m_reportData.getBroker());
                boolean isCompliant = engine.evaluateCompliance(student, doses) ||
                        (ruleInstance != null && ruleInstance.getComplianceOverrideIndicator());


                String status = getImmunizationStatus(isCompliant, isWaived, m_isCISFormSubmitted, exemptReason);
                String codes[] = code.split(",");
                for (String stateCode : codes) {
                    ImmunizationStatus currentStatus = m_mapStatus.get(stateCode);
                    if (updateStatus(status, currentStatus == null ? null : currentStatus.getStatus())) {
                        m_mapStatus.put(stateCode, m_reportData.new ImmunizationStatus(stateCode,
                                doses.size(), status));
                    }
                }
            }
        }

        /**
         * Returns the conditional immunization indicator for the entity.
         *
         * @return String
         */
        protected String getConditionalOrNonCompliant() {
            return m_isCISFormSubmitted;
        }

        /**
         * Returns the immunization code for a particular immunization for this entity.
         *
         * @param code String
         * @return Immunization status
         */
        protected ImmunizationStatus getStatus(String code) {
            return m_mapStatus.get(code);
        }

        /**
         * Returns the conditional or Non Compliant status for this immunization.
         *
         * @param ruleInstance HealthImmunizationRuleInstance
         * @return String
         */
        private String getConditOrNonComplStatus(HealthImmunizationRuleInstance ruleInstance) {
            String date = "";
            String status = null;
            String conditionalStatus = null;
            PlainDate expireDate = null;
            if (ruleInstance instanceof HealthImmunizationSeries) {
                conditionalStatus = (String) ((HealthImmunizationSeries) ruleInstance)
                        .getFieldValueByBeanPath(m_reportData.m_fieldConditionalStatus);
                date = (String) ((HealthImmunizationSeries) ruleInstance)
                        .getFieldValueByBeanPath(m_reportData.m_fieldConditionalExpire);
            } else if (ruleInstance instanceof HealthImmunizationGroupOverride) {
                conditionalStatus = (String) ((HealthImmunizationGroupOverride) ruleInstance)
                        .getFieldValueByBeanPath(m_reportData.m_fieldConditionalStatusGrp);
                date = (String) ((HealthImmunizationGroupOverride) ruleInstance)
                        .getFieldValueByBeanPath(m_reportData.m_fieldConditionalExpireGrp);
            }

            if (BooleanAsStringConverter.TRUE.equals(conditionalStatus)) {
                expireDate = (PlainDate) m_reportData.m_dConv.parseSystemString(date);
                if (expireDate != null && !m_reportData.m_reportDate.before(expireDate)) {
                    status = STATUS_NON_COMPLIANT;
                } else if (expireDate != null && m_reportData.m_reportDate.before(expireDate)
                        || (date != null && date.isEmpty()) || date == null) {
                    status = STATUS_CONDITIONAL;
                }
            }
            return status;
        }

        /**
         * Returns the reason this entity is exempt for a particular immunization series or dose.
         *
         * @param seriesDose X2BaseBean
         * @param field String
         * @return String
         */
        private String getExemptReason(X2BaseBean seriesDose, String field) {
            String value = (String) seriesDose.getFieldValueByBeanPath(field);
            value = m_reportData.lookupReferenceCodeByBeanPath(HealthImmunizationSeries.class, field, value,
                    ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
            return value;
        }

        /**
         * Returns the reason this entity is exempt for a particular immunization series or dose.
         *
         * @param ruleInstance HealthImmunizationRuleInstance
         * @return String
         */
        private String getExemptReason(HealthImmunizationRuleInstance ruleInstance) {
            String value = null;
            if (ruleInstance instanceof HealthImmunizationSeries) {
                value = (String) ((HealthImmunizationSeries) ruleInstance)
                        .getFieldValueByBeanPath(m_reportData.m_fieldImmunizationExempt);
                value = m_reportData.lookupReferenceCodeByBeanPath(HealthImmunizationSeries.class,
                        m_reportData.m_fieldImmunizationExempt, value,
                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
            } else if (ruleInstance instanceof HealthImmunizationGroupOverride) {
                value = (String) ((HealthImmunizationGroupOverride) ruleInstance)
                        .getFieldValueByBeanPath(m_reportData.m_fieldImmunizationExemptGrp);
                value = m_reportData.lookupReferenceCodeByBeanPath(HealthImmunizationGroupOverride.class,
                        m_reportData.m_fieldImmunizationExemptGrp, value,
                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
            }
            return value;
        }

        /**
         * Returns the code for a particular immunization for this entity.
         *
         * @param ruleInstance HealthImmunizationRuleAttributes
         * @return String
         */
        private String getImmunizationCode(HealthImmunizationRuleAttributes ruleInstance) {
            String oid = ruleInstance.getOid();
            String code = m_reportData.m_immunizationCodeMap.get(oid);

            if (code == null) {
                if (ruleInstance instanceof HealthImmunizationDefinition) {
                    HealthImmunizationDefinition defn = (HealthImmunizationDefinition) ruleInstance;
                    code = defn.getSeriesId();
                    if (!StringUtils.isEmpty(code)) {
                        code = m_reportData.lookupReferenceCodeByBeanPath(HealthImmunizationDefinition.class,
                                HealthImmunizationDefinition.COL_SERIES_ID, code,
                                ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                    }
                } else if (ruleInstance instanceof HealthImmunizationGroup) {
                    HealthImmunizationGroup grp = (HealthImmunizationGroup) ruleInstance;
                    code = grp.getGroupId();
                    if (!StringUtils.isEmpty(code)) {
                        code = m_reportData.lookupReferenceCodeByBeanPath(HealthImmunizationGroup.class,
                                HealthImmunizationGroup.COL_GROUP_ID, code,
                                ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                    }
                }
                m_reportData.m_immunizationCodeMap.put(oid, code == null ? "" : code);
            }
            return code;
        }

        /**
         * returns the immunization status.
         *
         * @param isCompliant - Indicates that the student is compliant based on immunization rules
         * @param isWaived - Indicates that the student is waived
         * @param conditionalOrNonCompliantStatus - Indicates that the student is conditional status
         *        or non compliant.
         * @param exemptReason - The reason the student is exempt for this immunization.
         * @return String
         */
        private String getImmunizationStatus(boolean isCompliant,
                                             boolean isWaived,
                                             String conditionalOrNonCompliantStatus,
                                             String exemptReason) {
            String status = STATUS_NON_COMPLIANT;
            if (isWaived) {
                if (CODE_MEDICAL.equals(exemptReason)) {
                    status = STATUS_MEDICAL_EXEMPT;
                } else if (CODE_RELIGIOUS.equals(exemptReason)) {
                    status = STATUS_RELIGIOUS_EXEMPT;
                } else if (CODE_RELIGIOUS_MEMBER.equals(exemptReason)) {
                    status = STATUS_RELIGIOUS_MEMBERSHIP_EXEMPT;
                } else // if (CODE_PERSONAL.equals(exemptReason)) // default to personal of not
                       // indicated.
                {
                    status = STATUS_PERSONAL_EXEMPT;
                }
            } else if (conditionalOrNonCompliantStatus != null) {
                status = conditionalOrNonCompliantStatus;
            } else if (isCompliant) {
                status = STATUS_COMPLIANT;
            }
            return status;
        }

        /**
         * Tests to determine if the new status should be used to replace the status determined
         * for a different immunization definition.
         * Returns true if the new status should replace the current status
         *
         * @param status String
         * @param previous String
         * @return true, if successful
         */
        private boolean updateStatus(String status, String previous) {
            if (previous != null) {
                if (STATUS_COMPLIANT.equals(previous)) {
                    return false;
                }
                if ((STATUS_PERSONAL_EXEMPT.equals(previous) ||
                        STATUS_MEDICAL_EXEMPT.equals(previous) ||
                        STATUS_RELIGIOUS_EXEMPT.equals(previous) ||
                        STATUS_RELIGIOUS_MEMBERSHIP_EXEMPT.equals(previous)) && !STATUS_COMPLIANT.equals(status)) {
                    return false;
                }
                if (STATUS_CONDITIONAL.equals(previous) && STATUS_NON_COMPLIANT.equals(status)) {
                    return false;
                }
            }
            return true;
        }

    }

    /**
     * A class used to maintain the status characteristics for a particular immunization.
     *
     * @author Administrator
     */
    protected class ImmunizationStatus {
        String m_code;
        int m_numDoses;
        String m_status;

        /**
         * Instantiates a new immunization status.
         *
         * @param code String
         * @param numDoses int
         * @param status String
         */
        protected ImmunizationStatus(String code, int numDoses, String status) {
            m_code = code;
            m_numDoses = numDoses;
            m_status = status;
        }

        /**
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            return "Code:" + m_code + ",numDoses:" + m_numDoses + ",Status:" + m_status;
        }

        /**
         * Gets the code.
         *
         * @return the code
         */
        protected String getCode() {
            return m_code;
        }

        /**
         * Gets the num doses.
         *
         * @return the numDoses
         */
        protected int getNumDoses() {
            return m_numDoses;
        }

        /**
         * Gets the status.
         *
         * @return the status
         */
        protected String getStatus() {
            return m_status;
        }

    }

    /**
     * A field retriever used to lookup and return the immunization status for this entity.
     *
     * @author Administrator
     */
    protected class RetrieveInfo implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String code = (String) field.getParameter();
            String statusValue = ((WAImmunizationEntity) entity).getConditionalOrNonCompliant();
            ImmunizationStatus status = ((WAImmunizationEntity) entity).getStatus(code);
            if (status != null) {
                statusValue = status.m_status;
            }
            return statusValue;
        }
    }

    /**
     * A field retriever used to lookup and return the needed for CSV export school info for this
     * entity.
     *
     * @author Administrator
     */
    protected class RetrieveSklInfo implements FieldRetriever {
        private static final String FIELD_SCHOOL_CITY_STATE = "School City/State";

        private static final String PARAM_CITY = "CITY";
        private static final String PARAM_HIGH_GRADE = "HIGH_GRADE";
        private static final String PARAM_LOW_GRADE = "LOW_GRADE";
        private static final String PARAM_ZIP = "ZIP";

        private static final String STRING_COMMA = ",";
        private static final String STRING_SPACE = " ";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            WAImmunization hisData = (WAImmunization) data;
            SisSchool school = ((SisStudent) entity.getBean()).getSchool();
            String param = (String) field.getParameter();
            String cityStateZip = entity.getFieldValue(FIELD_SCHOOL_CITY_STATE);

            String value = null;

            if (PARAM_CITY.equals(param) && !StringUtils.isEmpty(cityStateZip)) {
                value = cityStateZip.substring(0, cityStateZip.indexOf(STRING_COMMA));
            } else if (PARAM_ZIP.equals(param) && !StringUtils.isEmpty(cityStateZip)) {
                value = cityStateZip.substring(cityStateZip.lastIndexOf(STRING_SPACE) + 1, cityStateZip.length());
            } else if (PARAM_LOW_GRADE.equals(param)) {
                value = String.valueOf(school.getStartGrade());
                value = hisData.m_numericStateGradeMap.get(value);
            } else if (PARAM_HIGH_GRADE.equals(param)) {
                value = String.valueOf(school.getStartGrade() + school.getNumberOfGrades());
            }
            return value;
        }
    }


    protected static final String ALIAS_CIS_FORM_SUBMITTED = "DOE CIS FORM SUBMITTED";
    protected static final String ALIAS_CONDITIONAL_EXPIRE = "DOE CONDITIONAL EXPIRE ";
    protected static final String ALIAS_CONDITIONAL_EXPIRE_GRP = "DOE CONDITIONAL EXPIRE GRP";
    protected static final String ALIAS_CONDITIONAL_STATUS = "DOE CONDITIONAL STATUS";
    protected static final String ALIAS_CONDITIONAL_STATUS_GRP = "DOE CONDITIONAL STATUS GRP";
    protected static final String ALIAS_EXCLUDE_SCHOOL = "DOE EXCLUDE SKL";
    protected static final String ALIAS_IMMUNIZATION = "DOE IMMUNIZATION";
    protected static final String ALIAS_IMMUNIZATION_EXEMPT = "DOE IMMUN EXEMPT";
    protected static final String ALIAS_IMMUNIZATION_EXEMPT_GRP = "DOE IMMUN EXEMPT GRP";
    protected static final String ALIAS_IMMUNIZATION_DOSE_EXEMPT = "DOE IMMUN DOSE EXEMPT";
    protected static final String ALIAS_NUMERIC_GRADE = "NumericGradeLevel";


    protected static String CALC_PARAM_DIPTHERIA_TETANUS = "DIPTHERIA/TETANUS";
    protected static String CALC_PARAM_HIB = "HIB";
    protected static String CALC_PARAM_HEP_B = "HEP B";
    protected static String CALC_PARAM_MEASLES_1 = "MEASLES_1";
    protected static String CALC_PARAM_MEASLES_2 = "MEASLES_2";
    protected static String CALC_PARAM_MUMPS_1 = "MUMPS_1";
    protected static String CALC_PARAM_MUMPS_2 = "MUMPS_2";
    protected static String CALC_PARAM_PCV = "PCV";
    protected static String CALC_PARAM_PERTUSSIS = "PERTUSSIS";
    protected static String CALC_PARAM_POLIO = "POLIO";
    protected static String CALC_PARAM_RUBELLA_1 = "RUB_1";
    protected static String CALC_PARAM_RUBELLA_2 = "RUB_2";
    protected static String CALC_PARAM_VARICELLA_1 = "VARICELLA_1";
    protected static String CALC_PARAM_VARICELLA_2 = "VARICELLA_2";

    protected static String CODE_DIPTHERIA_TETANUS = "DT";
    protected static String CODE_HEP_B = "HEP B";
    protected static String CODE_HIB = "HIB";
    protected static String CODE_MEASLES = "MEASELS";
    protected static String CODE_MEDICAL = "Medical";
    protected static String CODE_MUMPS = "MUMPS";
    protected static String CODE_PCV = "PCV";
    protected static String CODE_PERSONAL = "Personal";
    protected static String CODE_PERTUSSIS = "P";
    protected static String CODE_POLIO = "POLIO";
    protected static String CODE_RELIGIOUS = "Religious";
    protected static String CODE_RELIGIOUS_MEMBER = "Religious Member";
    protected static String CODE_RUBELLA = "RUB";
    protected static String CODE_VARICELLA = "VARICELLA";

    protected static String GRADE_PK = "PK";
    protected static String gradesK12[] =
            {"K1", "K2", "1", "01", "2", "02", "3", "03", "4", "04", "5", "05", "6", "06", "7", "07", "8", "08", "9",
                    "09", "10", "11", "12"};

    protected static String STATUS_COMPLIANT = "C";
    protected static String STATUS_CONDITIONAL = "D";
    protected static String STATUS_MEDICAL_EXEMPT = "M";
    protected static String STATUS_NON_COMPLIANT = "N";
    protected static String STATUS_PERSONAL_EXEMPT = "P";
    protected static String STATUS_RELIGIOUS_EXEMPT = "R";
    protected static String STATUS_RELIGIOUS_MEMBERSHIP_EXEMPT = "G";

    private static final String EXTENDED_DICTIONARY_ID_GRADE_NUMERIC = "REF-GRADE-LEVELS";

    private static final String PARAM_ALL_SCHOOLS = "allSchools";
    private static final String PARAM_EXCLUDE_SCHOOL = "excludeSchool";
    private static final String PARAM_FORMAT = "format";
    private static final String PARAM_K12 = "k12";
    private static final String PARAM_PREK = "preK";
    private static final String PARAM_REPORT_DATE = "reportDate";
    private static final String PARAM_SCHOOLS = "schoolOids";


    protected DateAsStringConverter m_dConv;
    protected String m_fieldCISFormSubmitted;
    protected String m_fieldConditionalExpire;
    protected String m_fieldConditionalExpireGrp;
    protected String m_fieldConditionalStatus;
    protected String m_fieldConditionalStatusGrp;
    protected String m_fieldExcludeSchool;
    protected String m_fieldImmunizationExempt;
    protected String m_fieldImmunizationExemptGrp;
    protected String m_fieldImmunizationDoseExempt;

    // This field is used by reports to produce CVS export from reports.
    protected String m_format;

    protected GradeLevelHistory m_history;
    protected StudentHistoryHelper m_helper;
    protected Map<String, String> m_immunizationCodeMap;
    protected Map<String, String> m_numericStateGradeMap;
    protected PlainDate m_reportDate;

    protected Map<String, Collection<HealthImmunizationDefinition>> m_definitionCache;
    protected Collection<HealthImmunizationRuleAttributes> m_definitions;
    protected Map<String, Map<String, List<HealthImmunizationDose>>> m_doses;
    protected Map m_excludeSchoolMap;
    protected Map<String, Map<String, HealthImmunizationRuleInstance>> m_series;

    /**
     * Checks if schoolId given is a school to exclude. If that school is excluded this method will
     * return false ie - !included.
     *
     * @param schoolOid String
     * @return boolean
     */
    public boolean includeSchool(String schoolOid) {
        return (m_excludeSchoolMap == null) || !m_excludeSchoolMap.containsKey(schoolOid);
    }

    /**
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    /*
     * Initialize the data module.
     * Initialize necessary working resources.
     * Define query for students to load.
     * Define list of field definitions for the export.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(java.util.Map,
     * com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    public void initialize() {
        initGrades();
        initializeFields();

        if (((Boolean) getParameter(PARAM_EXCLUDE_SCHOOL)).booleanValue()) {
            loadSchoolExcludeMap();
        }

        m_immunizationCodeMap = new HashMap<String, String>();

        m_dConv = (DateAsStringConverter) ConverterFactory.getConverterForClass(Converter.DATE_CONVERTER,
                LocalizationCache.getPrimarySystemLocale(getBroker().getPersistenceKey()),
                true);
        m_helper = new StudentHistoryHelper(this);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_SNAPSHOT);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_SCHOOL, Boolean.FALSE);

        try {
            m_format = (String) getParameter(PARAM_FORMAT);
        } catch (ClassCastException exp) {
            // Do nothing. Format will be empty and we run original export.
        }

        m_reportDate = (PlainDate) getParameter(PARAM_REPORT_DATE);
        if (m_reportDate == null) {
            m_reportDate = new PlainDate();
        }

        X2Criteria studentCriteria = m_helper.getStudentCriteria();

        Collection<String> grades = new ArrayList<String>();

        if (getParameter(PARAM_PREK) != null) {
            grades.add(GRADE_PK);
        }
        if (getParameter(PARAM_K12) != null) {
            grades.addAll(Arrays.asList(gradesK12));
        }

        if (!grades.isEmpty()) {
            addGradeCriteria(studentCriteria, grades);
        }

        String schoolOids = (String) getParameter(PARAM_SCHOOLS);
        Boolean allSchools = (Boolean) getParameter(PARAM_ALL_SCHOOLS);
        if (allSchools != null && allSchools.booleanValue()) {
            studentCriteria.addNotEqualTo(SisStudent.REL_SCHOOL + ModelProperty.PATH_DELIMITER +
                    School.COL_INACTIVE_INDICATOR, Boolean.TRUE);
            studentCriteria.addNotEqualTo(SisStudent.REL_SCHOOL + ModelProperty.PATH_DELIMITER +
                    School.COL_ARCHIVE_INDICATOR, Boolean.TRUE);

            if (m_excludeSchoolMap != null && !m_excludeSchoolMap.isEmpty()) {
                studentCriteria.addNotIn(SisStudent.COL_SCHOOL_OID, m_excludeSchoolMap.keySet());
            }
        } else if (schoolOids != null) {
            Set<String> setSchoolOids = new HashSet<String>();
            setSchoolOids.addAll(Arrays.asList(schoolOids.split(",")));
            Iterator<String> schoolIterator = setSchoolOids.iterator();
            while (schoolIterator.hasNext()) {
                String sklOid = schoolIterator.next();
                if (!includeSchool(sklOid)) {
                    schoolIterator.remove();
                }
            }

            studentCriteria.addIn(SisStudent.COL_SCHOOL_OID, setSchoolOids);
        } else if (isSchoolContext()) {
            studentCriteria.addEqualTo(SisStudent.COL_SCHOOL_OID,
                    includeSchool(getSchool().getOid()) ? getSchool().getOid() : "__dummy__");
        } else {
            studentCriteria.addNotEqualTo(SisStudent.REL_SCHOOL + ModelProperty.PATH_DELIMITER +
                    School.COL_INACTIVE_INDICATOR, Boolean.TRUE);
            studentCriteria.addNotEqualTo(SisStudent.REL_SCHOOL + ModelProperty.PATH_DELIMITER +
                    School.COL_ARCHIVE_INDICATOR, Boolean.TRUE);

            if (m_excludeSchoolMap != null && !m_excludeSchoolMap.isEmpty()) {
                studentCriteria.addNotIn(SisStudent.COL_SCHOOL_OID, m_excludeSchoolMap.keySet());
            }

        }


        if (getSetupErrors().size() == 0) {
            m_history = new GradeLevelHistory(studentCriteria,
                    20,
                    OrganizationManager.getRootOrganization(getOrganization()),
                    getBroker());
            initImmunizationCollections(studentCriteria);
            // Set the query to be used for student selection.
            setQuery(m_helper.getStudentQuery(false));

            setEntityClass(WAImmunizationEntity.class);

            // Build a map of calculations/retrievers
            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put("HIS-CALC", new RetrieveInfo());
            calcs.put("HIS-SKL-CALC", new RetrieveSklInfo());
            super.addCalcs(calcs);
        }
    }

    /**
     * Returns a list of doses for the given series and student. Empty list if none exist or passed
     * series was null.
     *
     * @param ruleAttributes HealthImmunizationRuleAttributes
     * @param student SisStudent
     * @return List<HealthImmunizationDose>
     */
    protected List<HealthImmunizationDose> getDoses(HealthImmunizationRuleAttributes ruleAttributes,
                                                    SisStudent student) {
        List<HealthImmunizationDose> dosesList = new LinkedList<>();

        if (ruleAttributes != null) {
            for (HealthImmunizationDefinition definition : getImmunizationDefinitions(ruleAttributes, getBroker())) {
                Map<String, List<HealthImmunizationDose>> doseMap = m_doses.get(definition.getOid());
                if (doseMap != null) {
                    List<HealthImmunizationDose> dosesForStudent = doseMap.get(student.getOid());
                    if (dosesForStudent != null) {
                        dosesList.addAll(dosesForStudent);
                    }
                }
            }
        }

        // The doses list must be sorted by date before passing into the compliance engine
        Comparator comparator = new Comparator<HealthImmunizationDose>() {
            @Override
            public int compare(HealthImmunizationDose o1, HealthImmunizationDose o2) {
                if (o1.getDate() == null) {
                    return -1;
                } else if (o2.getDate() == null) {
                    return 1;
                } else {
                    return o1.getDate().compareTo(o2.getDate());
                }
            }
        };

        Collections.sort(dosesList, comparator);

        return dosesList;
    }

    /**
     * Returns a series for a given definition and student.
     *
     * @param def HealthImmunizationRuleAttributes
     * @param student SisStudent
     * @return HealthImmunizationSeries
     */
    protected HealthImmunizationRuleInstance getRuleInstance(HealthImmunizationRuleAttributes def, SisStudent student) {
        HealthImmunizationRuleInstance ruleInstance = null;

        Map<String, HealthImmunizationRuleInstance> ruleInstanceMap = m_series.get(def.getOid());
        if (ruleInstanceMap != null) {
            ruleInstance = ruleInstanceMap.get(student.getOid());
        }

        return ruleInstance;
    }

    /**
     * This method adds grades criteria to the student selection.
     *
     * @param studentCriteria X2Criteria
     * @param selectedGrades Collection<String>
     */
    private void addGradeCriteria(X2Criteria studentCriteria, Collection<String> selectedGrades) {
        DataDictionaryField dictionaryField = getDataDictionaryField(SisStudent.class, SisStudent.COL_GRADE_LEVEL);
        if (dictionaryField != null && dictionaryField.hasReferenceTable()) {
            Collection<String> grades = new LinkedList<String>();
            ReferenceTable refTable = (ReferenceTable) getBroker().getBeanByOid(ReferenceTable.class,
                    dictionaryField.getReferenceTableOid());
            if (refTable != null) {
                Collection<ReferenceCode> codes = refTable.getReferenceCodes(getBroker());
                for (ReferenceCode code : codes) {
                    if (selectedGrades.contains(code.getStateCode())) {
                        grades.add(code.getCode());
                    }
                }
            }
            studentCriteria.addIn(SisStudent.COL_GRADE_LEVEL, grades);
        }
    }

    /**
     * Get the cached immunization definitions for this rule.
     *
     * @param ruleAttributes HealthImmunizationRuleAttributes
     * @param broker X2Broker
     * @return Collection
     */
    private Collection<HealthImmunizationDefinition> getImmunizationDefinitions(HealthImmunizationRuleAttributes ruleAttributes,
                                                                                X2Broker broker) {
        if (m_definitionCache == null) {
            m_definitionCache = new HashMap();
        }
        Collection<HealthImmunizationDefinition> defns = m_definitionCache.get(ruleAttributes.getOid());
        if (defns == null) {
            defns = ruleAttributes.getImmunizationDefinitions(getBroker());
            m_definitionCache.put(ruleAttributes.getOid(), defns);
        }
        return defns;
    }

    /**
     * Lookup field aliases and paths.
     */
    private void initializeFields() {
        m_fieldConditionalExpire = translateAliasToJavaName(ALIAS_CONDITIONAL_EXPIRE, true);
        m_fieldConditionalExpireGrp = translateAliasToJavaName(ALIAS_CONDITIONAL_EXPIRE_GRP, true);
        m_fieldConditionalStatus = translateAliasToJavaName(ALIAS_CONDITIONAL_STATUS, true);
        m_fieldConditionalStatusGrp = translateAliasToJavaName(ALIAS_CONDITIONAL_STATUS_GRP, true);

        m_fieldCISFormSubmitted = translateAliasToJavaName(ALIAS_CIS_FORM_SUBMITTED, true);
        m_fieldImmunizationExempt = translateAliasToJavaName(ALIAS_IMMUNIZATION_EXEMPT, true);
        m_fieldImmunizationExemptGrp = translateAliasToJavaName(ALIAS_IMMUNIZATION_EXEMPT_GRP, true);
        m_fieldImmunizationDoseExempt = translateAliasToJavaName(ALIAS_IMMUNIZATION_DOSE_EXEMPT, true);
        m_fieldExcludeSchool = translateAliasToJavaName(ALIAS_EXCLUDE_SCHOOL, true);
    }

    /**
     * Load map with grade state codes keyed by numeric grade value.
     */
    private void initGrades() {
        m_numericStateGradeMap = new HashMap<String, String>();

        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ExtendedDataDictionary.COL_ID, EXTENDED_DICTIONARY_ID_GRADE_NUMERIC);
        ExtendedDataDictionary extendedDictionary = (ExtendedDataDictionary) getBroker()
                .getBeanByQuery(new QueryByCriteria(ExtendedDataDictionary.class, criteria));

        DataDictionary dictionary =
                DataDictionary.getDistrictDictionary(extendedDictionary, getBroker().getPersistenceKey());
        DataDictionaryField gradeLevelField = dictionary.findDataDictionaryField(Student.class.getName(),
                Student.COL_GRADE_LEVEL);

        DataDictionaryField numericGradeLevelField = dictionary.findDataDictionaryFieldByAlias(ALIAS_NUMERIC_GRADE);
        if (gradeLevelField.hasReferenceTable()) {
            for (ReferenceCode code : gradeLevelField.getReferenceTable().getReferenceCodes(getBroker())) {
                String numericLevel = (String) code.getFieldValueByBeanPath(numericGradeLevelField.getJavaName());

                if (!StringUtils.isEmpty(numericLevel)) {
                    m_numericStateGradeMap.put(numericLevel, code.getStateCode());
                }
            }
        }
    }

    /**
     * Generates immunization collections for definition, series and doses
     * used to determine the immunization status for each student / immunization.
     *
     * @param studentCriteria X2Criteria
     */
    private void initImmunizationCollections(X2Criteria studentCriteria) {
        SubQuery studentSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria);

        // definitions - all HealthImmunizationGroup + all HealthImmunizationDefinition not
        // associated with HealthImmunizationGroup
        QueryByCriteria definitionQuery = new QueryByCriteria(HealthImmunizationGroup.class, new Criteria());
        m_definitions = getBroker().getCollectionByQuery(definitionQuery);

        for (Object obj : getBroker()
                .getCollectionByQuery(new QueryByCriteria(HealthImmunizationDefinition.class, new Criteria()))) {
            HealthImmunizationDefinition defn = (HealthImmunizationDefinition) obj;
            if (defn.getImmunizationGroupMembers().isEmpty()) {
                m_definitions.add(defn);
            }
        }

        // series
        X2Criteria seriesMapCriteria = new X2Criteria();
        seriesMapCriteria.addIn(HealthImmunizationSeries.COL_STUDENT_OID, studentSubQuery);
        QueryByCriteria seriesQuery = new QueryByCriteria(HealthImmunizationSeries.class, seriesMapCriteria);
        seriesQuery.addOrderBy(HealthImmunizationSeries.COL_IMMUNIZATION_DEFINITION_OID, true);
        seriesQuery.addOrderBy(HealthImmunizationSeries.COL_STUDENT_OID, true);
        m_series = getBroker().getNestedMapByQuery(seriesQuery,
                HealthImmunizationSeries.COL_IMMUNIZATION_DEFINITION_OID,
                HealthImmunizationSeries.COL_STUDENT_OID,
                16,
                128);
        // group immunizations
        Criteria overridesCriteria = new Criteria();
        overridesCriteria.addIn(HealthImmunizationGroupOverride.COL_STUDENT_OID, studentSubQuery);
        QueryByCriteria overridesQuery = new QueryByCriteria(HealthImmunizationGroupOverride.class, overridesCriteria);

        m_series.putAll(getBroker().getNestedMapByQuery(overridesQuery,
                HealthImmunizationGroupOverride.COL_IMMUNIZATION_GROUP_OID,
                HealthImmunizationGroupOverride.COL_STUDENT_OID,
                16,
                128));

        // Add the dose data
        Criteria doseCriteria = new Criteria();
        doseCriteria.addIn(HealthImmunizationDose.COL_STUDENT_OID, studentSubQuery);

        QueryByCriteria doseQuery =
                new QueryByCriteria(HealthImmunizationDose.class, doseCriteria);
        doseQuery.addOrderByAscending(HealthImmunizationDose.COL_DATE);

        String[] columns = new String[] {HealthImmunizationDose.REL_IMMUNIZATION_SERIES + "."
                + HealthImmunizationSeries.COL_IMMUNIZATION_DEFINITION_OID,
                HealthImmunizationSeries.COL_STUDENT_OID};
        int[] sizes = new int[] {16, 128};

        m_doses = getBroker().getGroupedCollectionByQuery(doseQuery, columns, sizes);
    }

    /**
     * Loads a map of schools that have been selected to be excluded from state reporting. (exclude
     * from reporting on school table is selected)
     */
    private void loadSchoolExcludeMap() {
        X2Criteria schoolCriteria = new X2Criteria();
        schoolCriteria.addEqualTo(m_fieldExcludeSchool, BooleanAsStringConverter.TRUE);
        BeanQuery query = new BeanQuery(School.class, schoolCriteria);
        m_excludeSchoolMap = getBroker().getGroupedCollectionByQuery(query, X2BaseBean.COL_OID, 128);
    }

}
