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
package com.x2dev.procedures.statereporting.ga;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.Race;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.x2dev.procedures.statereporting.common.CriteriaHelper;
import com.x2dev.procedures.statereporting.common.CriteriaHelper.EnrollmentSpanCriteria;
import com.x2dev.procedures.statereporting.common.FilterableFactory;
import com.x2dev.procedures.statereporting.common.ToolBean;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolRace;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolSchool;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolStaff;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolStudent;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolStudentSchedule;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldDefinition;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldRetriever;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportData;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportEntity;
import com.x2dev.sis.model.beans.path.SisBeanPaths;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * This class implements the data export for GA GKIDS export.
 *
 * @author X2 Development Corporation
 */
public class GAGKIDSData extends StateReportData {

    /**
     * Implementation of StateReportEntity to be used by the VA Student Record Collection export.
     * This must be a public
     * static inner class with a public no argument constructor so it can be instantiated through
     * reflection.
     *
     * @author X2 Development Corporation
     */
    public static class GAGKIDSEntity extends ToolsSharedContainer.StateReportEntity {

        /**
         * Return an identifying name for this entity for reporting purposes.
         *
         * @return entity name
         * @see com.x2dev.sis.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            GAStudent student = (GAStudent) getBean();
            String name = STRING_EMPTY;
            name +=
                    student.getNameView() + " [Local ID: " + student.getLocalId() + ", State ID: "
                            + student.getStateId() + "] ";
            return name;
        }

        /**
         * Initialize.
         *
         * Create map of StudentScheduleSpan objects for each student.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see com.x2dev.sis.tools.stateexports.StateReportEntity#intitialize(com.x2dev.sis.tools.stateexports.StateReportData,
         *      com.x2dev.sis.model.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, ToolBean bean) throws X2BaseException {
            super.intitialize(data, bean);
        }
    }

    /**
     * The Class Organization.
     */
    public static class GAOrganization extends ToolBean.ToolOrganization {
        private static final String ALIAS_DOE_DISTRICT = "DOE District";

        public static final ToolBeanColumn FIELD_DOE_DISTRICT =
                new ToolBeanColumn(SisBeanPaths.ORGANIZATION, ALIAS_DOE_DISTRICT);

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION =
                ToolOrganization.FULL_DEFINITION.expand(FIELD_DOE_DISTRICT);

        /**
         * Instantiates a new Organization.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public GAOrganization(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the district.
         *
         * @return String
         */
        public String getDoeDistrict() {
            return getValueString(FIELD_DOE_DISTRICT);
        }
    }

    /**
     * The Class School.
     */
    public static class GASchool extends ToolSchool {
        private static final String ALIAS_DOE_SCHOOL = "DOE School";
        private static final String ALIAS_DOE_SKL_PER_ATT = "DOE PERIOD ATTENDANCE SCHOOL";

        public static final ToolBeanColumn FIELD_PERIOD_ATTENDANCE_INDICATOR =
                new ToolBeanColumn(SisBeanPaths.SCHOOL, ALIAS_DOE_SKL_PER_ATT);
        public static final ToolBeanColumn FIELD_SCHOOL_CODE =
                new ToolBeanColumn(SisBeanPaths.SCHOOL, ALIAS_DOE_SCHOOL);

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolSchool.FULL_DEFINITION
                .expand(FIELD_PERIOD_ATTENDANCE_INDICATOR,
                        FIELD_SCHOOL_CODE);

        /**
         * Instantiates a new School.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public GASchool(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the organization 1.
         *
         * @param broker X2Broker
         * @return GA Organization Tool Bean
         */
        @Override
        public GAOrganization getOrganization1(X2Broker broker) {
            String orgOid = getValueString(FIELD_ORGANIZATION_1_OID);
            return getBeanByOid(broker, GAOrganization.class, orgOid, true);
        }

        /**
         * Gets the period attendance indicator.
         *
         * @return boolean
         */
        public boolean getPeriodAttendanceIndicator() {
            return getValueLogical(FIELD_PERIOD_ATTENDANCE_INDICATOR);
        }

        /**
         * Gets the school code.
         *
         * @return String
         */
        public String getSchoolCode() {
            return getValueString(FIELD_SCHOOL_CODE);
        }
    }

    /**
     * The Class Student.
     */
    public static class GAStudent extends ToolStudent {
        private static final String ALIAS_STD_DOE_ELL = "DOE ELL";
        private static final String ALIAS_STD_DOE_MIGRANT = "DOE Migrant";
        private static final String ALIAS_STD_EXCLUDE_FROM_REPORTING = "DOE EXCLUDE STD";
        private static final String ALIAS_STD_GTID = "GTID";
        private static final String ALIAS_STD_PRIM_EXCEPT = "DOE Primary Exceptionality";

        // Fields
        public static final ToolBeanColumn FIELD_504_STATUS =
                new ToolBeanColumn(SisBeanPaths.STUDENT.section504StatusCode());
        public static final ToolBeanColumn FIELD_ELL =
                new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_STD_DOE_ELL);
        public static final ToolBeanColumn FIELD_EXCLUDE_FROM_REPORTING =
                new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_STD_EXCLUDE_FROM_REPORTING);
        public static final ToolBeanColumn FIELD_GENDER =
                new ToolBeanColumn(SisBeanPaths.STUDENT.person().genderCode());
        public static final ToolBeanColumn FIELD_GTID = new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_STD_GTID);
        public static final ToolBeanColumn FIELD_HISP_IND =
                new ToolBeanColumn(SisBeanPaths.STUDENT.person().hispanicLatinoIndicator());
        public static final ToolBeanColumn FIELD_HOMEROOM_TEACHER =
                new ToolBeanColumn(SisBeanPaths.STUDENT.homeroomTeacher());
        public static final ToolBeanColumn FIELD_MIGRANT =
                new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_STD_DOE_MIGRANT);
        public static final ToolBeanColumn FIELD_PRIMARY_EXCEPTIONALITY =
                new ToolBeanColumn(SisBeanPaths.STUDENT,
                        new ToolBeanColumn.AliasDefinition(ALIAS_STD_PRIM_EXCEPT, null, false));

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolStudent.FULL_DEFINITION
                .expand(FIELD_504_STATUS,
                        FIELD_ELL,
                        FIELD_EXCLUDE_FROM_REPORTING,
                        FIELD_GENDER,
                        FIELD_GTID,
                        FIELD_HISP_IND,
                        FIELD_HOMEROOM_TEACHER,
                        FIELD_MIGRANT,
                        FIELD_PRIMARY_EXCEPTIONALITY);

        /**
         * Instantiates a new Student.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public GAStudent(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        public String get504StatusCode() {
            return getValueString(FIELD_504_STATUS);
        }

        /**
         * Gets override school code.
         *
         * @return String
         */
        public String getEllCode() {
            return getValueString(FIELD_ELL);
        }

        /**
         * Gets the primary exceptionality.
         *
         * @return String
         */
        public String getPrimaryExceptionality() {
            return getValueString(FIELD_PRIMARY_EXCEPTIONALITY);
        }
    }

    /**
     * The Class Retrieve504.
     *
     * @author Follett Software Company
     */
    protected class Retrieve504 implements FieldRetriever {
        private static final String CALC_ID = "GA-GKIDS-504";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            GAStudent std = (GAStudent) entity.getBean();
            return m_activeCode.equalsIgnoreCase(std.get504StatusCode()) ? "Y" : "N";
        }
    }

    /**
     * Returns grade from the transcript record associated with this section or class.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveGradeLevel implements FieldRetriever {
        private static final String CALC_ID = "GRADE-LEVEL";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldRetriever#getFieldValue(com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportData,
         *      com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportEntity,
         *      com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String returnObject = null;
            GAStudent std = (GAStudent) entity.getBean();
            String grade = std.getGradeLevel();
            if (!StringUtils.isEmpty(grade)) {
                returnObject = data.getDictionaryExtractor().lookupStateValue(ToolStudent.FIELD_GRADE_LEVEL, grade);
            }
            return returnObject;
        }
    }

    protected class RetrieveStaffData implements FieldRetriever {
        private static final String CALC_ID = "GA-STF-DATA";
        private static final String CALC_PARAM_EMAIL = "EMAIL";
        private static final String CALC_PARAM_FIRST = "FIRST";
        private static final String CALC_PARAM_LAST = "LAST";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldRetriever#getFieldValue(com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportData,
         *      com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportEntity,
         *      com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String value = null;
            GAStudent std = (GAStudent) entity.getBean();
            ToolStudentSchedule ssc = std.getStudentSchedules(getBroker()).stream().findAny().orElse(null);
            if (ssc != null) {
                String param = (String) field.getParameter();
                ToolStaff primStaff = ssc.getSection(getBroker()).getPrimaryStaff(getBroker());
                if (primStaff != null) {
                    if (CALC_PARAM_EMAIL.equals(param)) {
                        value = primStaff.getEmail01();
                    } else if (CALC_PARAM_LAST.equals(param)) {
                        value = getCleanName(primStaff.getLastName(), field.getMaxLength());
                    } else if (CALC_PARAM_FIRST.equals(param)) {
                        value = getCleanName(primStaff.getFirstName(), field.getMaxLength());
                    }
                }
            }
            return value;
        }

        /**
         * Gets the clean name.
         *
         * @param valueToClean String
         * @param fieldLength int
         * @return String
         */
        private String getCleanName(String valueToClean, int fieldLength) {
            String cleanValue = null;
            if (!StringUtils.isEmpty(valueToClean)) {
                Matcher matcher = m_illegalNameCharacters.matcher(valueToClean);
                cleanValue = matcher.replaceAll(STRING_EMPTY);
                if (cleanValue.length() > fieldLength) {
                    cleanValue = cleanValue.substring(0, fieldLength);
                }
            } else {
                cleanValue = STRING_EMPTY;
            }
            return cleanValue;
        }
    }

    /**
     * Returns an indicator if the person is included in the specified race.
     * This implementation looks up all race codes for a person and finds
     * the race entry for the specified race. It returns a true/false value
     * indicating the presence of the race code record.
     *
     * The calculation parameter should be a string with three characters:
     * character 1:
     * The character to return if the requested race code is present.
     * character 2:
     * The character to return if the requested race code is not present.
     * character(s) 3+:
     * The reference code state code value in the reference table for race codes.
     * In GA, this is:
     * "W" - White
     * "B" - Black
     * "S" - Asian
     * "I" - Indian/Native/Alaskan
     * "P" - Pacific
     *
     * Ex: "SNS" searches for the Asian code, returns "S" if present, "N" otherwise.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveRace implements FieldRetriever {

        private static final String CALC_ID = "GA-GKIDS-RACE";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String param = (String) field.getParameter();
            String trueChar = param.substring(0, 1);
            String falseChar = param.substring(1, 2);
            String requestCode = param.substring(2);
            String raceCode = falseChar;
            GAStudent std = (GAStudent) entity.getBean();
            Collection<ToolRace> races = std.getPersonRaces(data.getBroker());
            // Find the reference code that we are looking for.
            ReferenceCode refCode = m_raceCodes.get(requestCode);
            if (refCode != null && races != null) {
                ToolRace foundRace = races.stream().filter(race -> refCode.getCode().equals(race.getRaceCode()))
                        .findAny().orElse(null);
                if (foundRace != null) {
                    raceCode = trueChar;
                }
            }
            return raceCode;
        }
    }

    /**
     * Retrieve a bean property and strip off invalid characters.
     * Useful for cleaning names for reporting.
     * Also trim the name to maximum field length to avoid validation warnings.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveStripNameChar implements FieldRetriever {
        private static final String CALC_ID = "GA-STD-CLEAN";
        private static final String CALC_PARAM_MID_INITIAL = "MID-INITIAL";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldRetriever#getFieldValue(com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportData,
         *      com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportEntity,
         *      com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String cleanValue = null;
            String param = (String) field.getParameter();
            String nameValue = (String) entity.getBean().getFieldValueByColumnName(field.getBeanPath());
            int max = field.getMaxLength();
            if (!StringUtils.isEmpty(nameValue)) {
                Matcher matcher = m_illegalNameCharacters.matcher(nameValue);
                cleanValue = matcher.replaceAll(STRING_EMPTY);
                if (cleanValue.length() > max) {
                    cleanValue = cleanValue.substring(0, max);
                }
            } else {
                cleanValue = STRING_EMPTY;
            }
            if (!StringUtils.isEmpty(cleanValue) && !StringUtils.isEmpty(param)
                    && CALC_PARAM_MID_INITIAL.equals(field.getParameter())) {
                cleanValue = String.valueOf(cleanValue.charAt(0));
            }
            return cleanValue;
        }
    }

    /**
     * The Class RetrieveSystemCodes.
     */
    protected class RetrieveSystemCodes implements FieldRetriever {
        private static final String CALC_ID = "GA-GKIDS-SYS";
        private static final String CALC_PARAM_SCH_CODE = "SCH-CODE";
        private static final String CALC_PARAM_SYS_CODE = "SYS-CODE";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String param = (String) field.getParameter();
            GAStudent std = (GAStudent) entity.getBean();
            String value = null;
            if (CALC_PARAM_SYS_CODE.equals(param)) {
                GAOrganization org = (GAOrganization) std.getSchool(getBroker()).getOrganization1(getBroker());
                if (org != null) {
                    value = org.getDoeDistrict();
                }
            } else if (CALC_PARAM_SCH_CODE.equals(param)) {
                GASchool skl = (GASchool) std.getSchool(getBroker());
                if (skl != null) {
                    value = skl.getSchoolCode();
                }
            }
            return value;
        }
    }

    /*
     * General Constants
     */
    private static final String ILLEGAL_NAME_CHARACTERS = "[_\\W&&[^-'.\\s]]";
    private static final String KK_COURSE_NUMBER = "00.0000000";
    private static final String STRING_EMPTY = "";

    /*
     * Member variables
     */
    protected String m_activeCode;
    protected Pattern m_illegalNameCharacters = Pattern.compile(ILLEGAL_NAME_CHARACTERS);
    protected Map<String, ReferenceCode> m_raceCodes;

    /**
     * @see com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportData#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();

        // Initialize fields+
        initializeFields();

        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {
            ToolBean.setDictionaryExtractor(getDictionaryExtractor());
            ToolBean.registerClass(GASchool.class);
            ToolBean.registerClass(ToolStaff.class);
            ToolBean.registerClass(GAStudent.class);
            ToolBean.registerClass(GAOrganization.class);
            ToolBean.registerClass(ToolRace.class);
            ToolBean.registerClass(ToolStudentSchedule.class);

            EnrollmentSpanCriteria spanCriteria = new EnrollmentSpanCriteria()
                    .setCurrentContext(m_currentContext)
                    .setEndDate(m_currentContext.getEndDate())
                    .setIncludeSecondarySpans(false)
                    .setExcludeStudent(GAStudent.FIELD_EXCLUDE_FROM_REPORTING);
            if (isSchoolContext()) {
                spanCriteria.setSchoolOids(Arrays.asList(getSchool().getOid()));
            }
            X2Criteria inputCriteria = new X2Criteria();
            applyInputCriteria(inputCriteria, false, null);
            if (!inputCriteria.isEmpty()) {
                spanCriteria.setStudentLimitingCriteria(inputCriteria);
            }

            // Preloads
            X2Criteria sscCriteria = CriteriaHelper.buildStudentScheduleCriteria(spanCriteria);
            sscCriteria.addEqualTo(SisBeanPaths.STUDENT_SCHEDULE.section().schoolCourse().course().number().getPath(),
                    KK_COURSE_NUMBER);
            ToolBean.addAndCriteria(getBroker(), ToolStudentSchedule.class, sscCriteria);
            ToolBean.load(getBroker(), getDictionaryExtractor(), ToolStudentSchedule.class);

            List<String> studentOids = ToolBean.getCachedToolBeans(ToolStudentSchedule.class).stream()
                    .map(ssc -> ssc.getStudentOid()).distinct().collect(Collectors.toList());

            X2Criteria candidateCriteria = CriteriaHelper.getStudentCandidateCriteria(spanCriteria, getBroker());
            candidateCriteria.addEqualTo(ToolStudent.FIELD_GRADE_LEVEL.resolve(getDictionaryExtractor()), "KK");
            candidateCriteria.addIn(ToolBean.FIELD_OID.resolve(getDictionaryExtractor()), studentOids);
            // Check user selection criteria.
            setEntityClass(GAGKIDSEntity.class);
            setFilterable(FilterableFactory.create(getBroker(), GAStudent.class, candidateCriteria, null));

            DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
            DataDictionaryField field = dictionary.findDataDictionaryField(Race.class.getName(), Race.COL_RACE_CODE);

            // Get race code reference codes for use in the race retriever.
            Criteria raceCriteria = new Criteria();
            raceCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, field.getReferenceTableOid());
            QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, raceCriteria);
            m_raceCodes = getBroker().getMapByQuery(query, ReferenceCode.COL_STATE_CODE, 5);
            ToolBean.preload(getBroker(), getDictionaryExtractor(), null, ToolStudent.CHILD_PERSON_RACES);

            // Add any retrievers
            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put(RetrieveStripNameChar.CALC_ID, new RetrieveStripNameChar());
            calcs.put(RetrieveGradeLevel.CALC_ID, new RetrieveGradeLevel());
            calcs.put(Retrieve504.CALC_ID, new Retrieve504());
            calcs.put(RetrieveRace.CALC_ID, new RetrieveRace());
            calcs.put(RetrieveSystemCodes.CALC_ID, new RetrieveSystemCodes());
            calcs.put(RetrieveStaffData.CALC_ID, new RetrieveStaffData());
            super.addCalcs(calcs);
        }
    }

    /**
     * Initialize fields. Translate aliases to java names.
     */
    private void initializeFields() {
        if (m_currentContext == null) {
            Organization organization = OrganizationManager.getRootOrganization(getBroker());
            m_currentContext = organization.getCurrentContext();
        }
        m_activeCode =
                PreferenceManager.getPreferenceValue(getOrganization(), SystemPreferenceDefinition.STUDENT_ACTIVE_CODE);

    }
}
