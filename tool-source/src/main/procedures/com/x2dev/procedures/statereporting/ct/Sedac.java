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
package com.x2dev.procedures.statereporting.ct;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.*;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepService;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Connecticut SPED state report for SEDAC export.
 * This class implements the data export for SEDAC export.
 *
 * @author X2 Development Corporation
 */
public class Sedac extends StateReportData {
    /**
     * Implementation of StateReportEntity to be used by the SEDAC export.
     * This must be a public static inner class with a public no argument
     * constructor so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */

    public static class SedacEntity extends StateReportEntity {
        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public SedacEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Return an identifying name for this entity for reporting purposes.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            IepData iepData = (IepData) getBean();
            SisStudent student = iepData.getStudent();

            String name = student.getNameView() +
                    " [LASID: " + student.getLocalId() +
                    ", SASID: " + student.getStateId() + "]";
            return name;
        }

        /**
         * initialize.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc
         *      .core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            IepData iepData = (IepData) bean;
            Sedac sedac = (Sedac) data;
            Date mostRecentIep = sedac.m_studentRecentIep.get(iepData.getStudentOid());
            if (iepData.getStartDate() == null) {
                setRowCount(0);
            } else if (!mostRecentIep.equals(iepData.getStartDate())) {
                setRowCount(0);
            }
        }
    }

    // on student, the facility a student is placed at
    public static final String SPECIAL_EDUCATION_FACILITY_ALIAS = "Special Education Facility";

    // on school
    public static final String SPECIAL_EDUCATION_SCHOOL_ALIAS = "Special Education School";

    // on student program participation
    public static final String ELL_CODE_ALIAS = "ELL_CODE";
    private static final String ENGLISH_LANGUAGE_LEARNER_PROGRAM = "ESL";

    /*
     * Alias
     */
    protected String ALIAS_EXTENDED_SERVICES = "iep-extended-service";
    protected String ALIAS_HOME_FACILITY_CODE = "";
    protected String ALIAS_PLACEMENT = "iep-ec-placement";
    protected String ALIAS_STATE_ID = "StateId";

    /*
     * Parameter
     */
    protected String PARAM_ADDRESS_01 = "Address 01";
    protected String PARAM_ADDRESS_02 = "Address 02";
    protected String PARAM_CITY = "City";
    protected String PARAM_STATE = "State";
    protected String PARAM_ZIP_CODE = "Zip Code";
    protected String PARAM_RELATED = "Related";
    protected String PARAM_NOT_REQUIRED = "Not Required";
    protected String PARAM_DISABILITY_REF_TABLE = "rtbCTSpedDisab";
    protected String PARAM_PRE_K_THREE = "P3";
    protected String PARAM_PRE_K_FOUR = "PK";

    /*
     * Instance variables.
     */
    protected DataDictionary m_ctIepDictionary;
    protected FormDefinition m_ctIepFormDef;
    protected HashMap<String, Date> m_studentRecentIep;
    protected Map<String, ReferenceCode> m_specialEducationFacilities;
    protected Map<String, StudentProgramParticipation> m_currentYearEnglishLanguageLearnerPrograms;

    /**
     * Class RetrieveAcademicProgram is the FieldRetriever used to retrieve
     * Academic Program value. If the student is enrolled in an academic
     * program after primary education, "01" is returned. Otherwise "02" is
     * returned.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveAcademicProgram implements FieldRetriever {

        /**
         * Returns the corresponding alias value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         *
         *         TODO check all three values in IEP. if any are true, then academicProgram="02"
         * @throws X2BaseException exception
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            String academicProgram = "01";
            String alias = (String) field.getParameter();
            DataDictionaryField ddField = m_ctIepDictionary.findDataDictionaryFieldByAlias(alias);
            IepData iepData = (IepData) ((SedacEntity) entity).getBean();
            Object furtherEducation = data.getProperty(iepData, ddField.getJavaName());
            if ("1".equals(furtherEducation)) {
                academicProgram = "02";
            }
            return academicProgram;
        }
    }

    /**
     * Class RetrieveAliasValue is the FieldRetriever used to retrieve alias values.
     * If the alias is associated with a reference table, that value is retrieved,
     * otherwise the field value itself is retrieved.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveAliasValue implements FieldRetriever {

        /**
         * Returns the corresponding alias value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            Object value = null;
            String alias = (String) field.getParameter();
            boolean transRef = false;
            if (alias.startsWith("t:")) {
                transRef = true;
                alias = alias.substring(2);
            }
            DataDictionaryField ddField = m_ctIepDictionary.findDataDictionaryFieldByAlias(alias);
            IepData iepData = (IepData) ((SedacEntity) entity).getBean();
            value = data.getPropertyAsJavaType(iepData, ddField.getJavaName());
            if (value != null && value instanceof String && transRef) {
                int referenceMap = ExportFormatField.ReferenceMapTypeCode.STATE.ordinal();
                String referenceTableOid = ddField.getReferenceTableOid();
                value = lookupReferenceCodeByRefTbl(referenceTableOid, (String) value, referenceMap);
            }
            return value;
        }
    }

    /**
     * Class RetrieveContactDetails is the FieldRetriever used to retrieve
     * Parent or Guardian contact information.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveContactDetails implements FieldRetriever {

        /**
         * Returns the corresponding parent/guardian contact value, depending upon
         * parameter passed in.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            String contactDetail = null;
            IepData iepData = (IepData) ((SedacEntity) entity).getBean();
            Person primaryContact = iepData.getStudent().getPrimaryContact().getPerson();
            Address contactAddress = primaryContact.getPhysicalAddress();

            /*
             * SisStudent student = iepData.getStudent();
             * student.getFieldValueByBeanPath(beanPath)
             */

            if (contactAddress != null && PARAM_ADDRESS_01.equals(field.getParameter())) {
                contactDetail = contactAddress.getAddressLine01();
            } else if (contactAddress != null && PARAM_ADDRESS_02.equals(field.getParameter())) {
                contactDetail = contactAddress.getAddressLine02();
            } else if (contactAddress != null && PARAM_CITY.equals(field.getParameter())) {
                contactDetail = contactAddress.getCity();
            } else if (contactAddress != null && PARAM_STATE.equals(field.getParameter())) {
                contactDetail = contactAddress.getState();
            } else if (contactAddress != null && PARAM_ZIP_CODE.equals(field.getParameter())) {
                contactDetail = contactAddress.getPostalCode();
            }
            return contactDetail;
        }
    }

    /**
     * Class RetrieveExtendedServices is the FieldRetriever used to retrieve
     * whether extended school year services are required for the student.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveExtendedServices implements FieldRetriever {

        /**
         * Returns the corresponding alias value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            Boolean requiresExtendedServices = Boolean.TRUE;
            DataDictionaryField ddField = m_ctIepDictionary.findDataDictionaryFieldByAlias(ALIAS_EXTENDED_SERVICES);
            IepData iepData = (IepData) ((SedacEntity) entity).getBean();
            String value = (String) data.getPropertyAsJavaType(iepData, ddField.getJavaName());
            if (value == null) {
                requiresExtendedServices = Boolean.FALSE;
            } else if (value.equalsIgnoreCase(PARAM_NOT_REQUIRED)) {
                requiresExtendedServices = Boolean.FALSE;
            }
            return requiresExtendedServices;
        }
    }

    /**
     * Class RetrievePrimaryDisability is the FieldRetriever used to
     * retrieve the student's primary disability.
     *
     * @author X2 Development Corporation
     */
    protected class RetrievePrimaryDisability implements FieldRetriever {

        /**
         * Returns student's primary disability, if one exists, null otherwise.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {

            Object primaryDisability = null;

            IepData iepData = (IepData) ((SedacEntity) entity).getBean();
            if (iepData.getPrimaryDisability() != null) {
                primaryDisability = iepData.getPrimaryDisability().getDisabilityCode();
            }

            return primaryDisability;
        }
    }

    /**
     * Class RetrieveRelatedServices is the FieldRetriever used to retrieve
     * an IEP's related services as well as determine whether the service
     * is used by this IEP.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveRelatedServices implements FieldRetriever {

        /**
         * Returns true if the related service is used, false otherwise.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            Object hasRequestedService = Boolean.FALSE;

            String requestedStateValue = (String) field.getParameter();
            IepData iepData = (IepData) ((SedacEntity) entity).getBean();
            Collection<IepService> services = iepData.getIepServices();
            for (IepService service : services) {
                if (service.getServiceMode().equals(PARAM_RELATED)) {
                    String serviceStateValue =
                            lookupReferenceCodeByBeanPath(IepService.class, IepService.COL_SERVICE_TYPE,
                                    service.getServiceType(), ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                    if (serviceStateValue != null && serviceStateValue.equals(requestedStateValue)) {
                        hasRequestedService = Boolean.TRUE;
                    }
                }
            }

            return hasRequestedService;
        }
    }

    /**
     * Retrieve the school Id from the membership record.
     * This is the school for the Student Enrollment record.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveSchoolCode implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String schoolId = null;

            IepData iepData = (IepData) ((SedacEntity) entity).getBean();
            SisStudent student = iepData.getStudent();
            SisSchool school = student.getSchool();

            String outplacementFacility = (String) student.getFieldValueByAlias("Outside Placement Facility");

            if (StringUtils.isEmpty(outplacementFacility)) {
                schoolId = (String) school.getFieldValueByAlias(ALIAS_STATE_ID);
                String temp = (String) school.getFieldValueByAlias(SPECIAL_EDUCATION_SCHOOL_ALIAS);
                if (BooleanAsStringConverter.TRUE.equalsIgnoreCase(temp)) {
                    schoolId = (String) student.getFieldValueByAlias(SPECIAL_EDUCATION_FACILITY_ALIAS);
                    if (m_specialEducationFacilities.containsKey(schoolId)) {
                        ReferenceCode refCode = m_specialEducationFacilities.get(schoolId);
                        schoolId = refCode.getStateCode();
                    }
                }
            } else {
                schoolId = outplacementFacility;
            }

            return schoolId;
        }
    }

    /**
     * Class RetrieveAliasValue is the FieldRetriever used to retrieve alias values.
     * If the alias is prefixed with "t:", the reference table state code is retrieved.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveAge3to5Placement implements FieldRetriever {

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
            String alias = ALIAS_PLACEMENT;
            String aliasPath = data.translateAliasToJavaName(alias, true);
            String placementValue = (String) data.getProperty(entity.getBean(), aliasPath);
            placementValue = lookupReferenceCodeByAlias(alias, placementValue,
                    ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());


            IepData iepData = (IepData) ((SedacEntity) entity).getBean();
            SisStudent student = iepData.getStudent();

            String gradeLevel = student.getGradeLevel();
            gradeLevel = lookupReferenceCodeByBeanPath(Student.class, Student.COL_GRADE_LEVEL, gradeLevel,
                    ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());

            if (placementValue == null && gradeLevel != null &&
                    !gradeLevel.equals(PARAM_PRE_K_THREE) && !gradeLevel.equals(PARAM_PRE_K_FOUR)) {
                placementValue = "00";
            }

            return placementValue;
        }
    }

    /**
     * The parameter is the name of the program code on student program participation that is the
     * ELL program code for this district.
     */
    protected class RetrieveEnglishLanguageLearner implements FieldRetriever {
        private static final String NO = "N";
        private static final String YES = "Y";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String ellCode = (String) field.getParameter();

            String ellStateCode = NO;

            IepData iepData = (IepData) entity.getBean();
            SisStudent student = iepData.getStudent();

            if (m_currentYearEnglishLanguageLearnerPrograms.containsKey(student.getOid())) {
                StudentProgramParticipation programParticipation =
                        m_currentYearEnglishLanguageLearnerPrograms.get(student.getOid());
                String programCode = programParticipation.getProgramCode();
                if (!StringUtils.isEmpty(programCode)) {
                    if (programCode.equalsIgnoreCase(ellCode)) {
                        String ellCodeValue = (String) programParticipation.getFieldValueByAlias(ELL_CODE_ALIAS);
                        if ("Y".equalsIgnoreCase(ellCodeValue)) {
                            ellStateCode = YES;
                        }
                    }
                }
            }

            return ellStateCode;

        }

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
        if (getSetupErrors().size() == 0) {
            /*
             * Build the criteria/query for the staff to include in this export based on user input.
             */
            Criteria iepCriteria = getIepCriteria();
            QueryByCriteria iepQuery = new QueryByCriteria(IepData.class, iepCriteria);
            applyInputSort(iepQuery, IepData.REL_STUDENT);

            /*
             * Set the query to be used for IepData selection.
             */
            setQuery(iepQuery);
            setEntityClass(SedacEntity.class);

            /*
             * Retrieve formDefinition, then dictionary for "fmdCtIep" -- used to complete date of
             * parental consent
             */
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(ExtendedDataDictionary.COL_ID, "SPED-CT-IEP");
            BeanQuery ct_iep_query = new BeanQuery(ExtendedDataDictionary.class, criteria);
            ExtendedDataDictionary ctIepExtendDictionary =
                    (ExtendedDataDictionary) getBroker().getBeanByQuery(ct_iep_query);
            m_ctIepDictionary =
                    DataDictionary.getDistrictDictionary(ctIepExtendDictionary, getBroker().getPersistenceKey());
            setDataDictionary(m_ctIepDictionary);

            /*
             * Load studentRecentIep HashMap
             */
            loadIepMap();
            loadCurrentYearEnglishLanguagePrograms();

            m_specialEducationFacilities = getReferenceCodesForAlias(SPECIAL_EDUCATION_FACILITY_ALIAS);

            /*
             * Build maps of retriever functions
             */
            HashMap<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
            calcs.put("SEDAC-CONTACT", new RetrieveContactDetails());
            calcs.put("SEDAC-ALIAS-VALUES", new RetrieveAliasValue());
            calcs.put("SEDAC-EXTEND-SERVICE", new RetrieveExtendedServices());
            calcs.put("SEDAC-RELATED-SERVIC", new RetrieveRelatedServices());
            calcs.put("SEDAC-ACAD-PGM", new RetrieveAcademicProgram());
            calcs.put("SEDAC-SCHOOL-CODE", new RetrieveSchoolCode());
            calcs.put("SEDAC-PRIMARY-DISAB", new RetrievePrimaryDisability());
            calcs.put("SEDAC-ELL", new RetrieveEnglishLanguageLearner());
            calcs.put("SEDAC-AGE3TO5-PLACE", new RetrieveAge3to5Placement());


            super.addCalcs(calcs);
        }
    }

    /**
     * Compose and return an iepCriteria .
     *
     * @return studentCriteria
     *
     *         TODO may need to add report date if that becomes important
     */
    private Criteria getIepCriteria() {
        X2Criteria iepCriteria = new X2Criteria();
        X2Criteria iepActiveCriteria = new X2Criteria();
        X2Criteria iepExitedCriteria = new X2Criteria();
        X2Criteria iepDiscardedCriteria = new X2Criteria();

        /*
         * Active iep, exited this year, or discarded this year.
         */

        /*
         * Active this year
         */
        iepActiveCriteria.addEqualTo(IepData.COL_STATUS_CODE, Integer.valueOf(1));

        /*
         * Exited this year
         */
        iepExitedCriteria.addGreaterOrEqualThan(IepData.REL_STUDENT + PATH_DELIMITER + Student.COL_SPED_EXIT_DATE,
                getCurrentContext().getStartDate());
        iepExitedCriteria.addEqualTo(IepData.COL_STATUS_CODE, Integer.valueOf(2));

        /*
         * Active or Exited this year
         */


        iepActiveCriteria.addOrCriteria(iepExitedCriteria);

        iepDiscardedCriteria.addGreaterOrEqualThan(IepData.COL_LAST_EVALUATION_DATE,
                getCurrentContext().getStartDate());
        iepDiscardedCriteria.addEqualTo(IepData.COL_STATUS_CODE, Integer.valueOf(7));

        iepActiveCriteria.addOrCriteria(iepDiscardedCriteria);

        /*
         * And either active or enrolled Students
         */
        X2Criteria activeStudentCriteria = new X2Criteria();
        activeStudentCriteria.addAndCriteria(StudentManager.getActiveStudentStatusCriteria(getOrganization(),
                IepData.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_ENROLLMENT_STATUS));
        X2Criteria enrolledStudentCriteria = new X2Criteria();
        X2Criteria membershipCriteria = new X2Criteria();

        membershipCriteria.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE,
                getCurrentContext().getStartDate());
        SubQuery membershipSubQuery =
                new SubQuery(StudentEnrollment.class, StudentEnrollment.COL_STUDENT_OID, membershipCriteria);
        enrolledStudentCriteria.addIn(IepData.COL_STUDENT_OID, membershipSubQuery);
        activeStudentCriteria.addOrCriteria(enrolledStudentCriteria);

        iepCriteria.addAndCriteria(activeStudentCriteria);

        /*
         * Check school selection user input parameter.
         */

        if (isSchoolContext()) {
            iepCriteria.addEqualTo(IepData.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_SCHOOL_OID,
                    getSchool().getOid());
        } else {
            iepCriteria.addEqualTo(IepData.REL_STUDENT + PATH_DELIMITER + SisStudent.REL_SCHOOL + PATH_DELIMITER
                    + School.COL_INACTIVE_INDICATOR, Boolean.FALSE);
            iepCriteria.addEqualTo(IepData.REL_STUDENT + PATH_DELIMITER + SisStudent.REL_SCHOOL + PATH_DELIMITER
                    + School.COL_ARCHIVE_INDICATOR, Boolean.FALSE);
        }


        applyInputCriteria(iepCriteria, false, IepData.REL_STUDENT);

        return iepCriteria;
    }

    /**
     * Return a map of reference codes for the field identified by the alias.
     *
     * @param alias String
     * @return Map<code, ReferenceCode>
     */
    private Map<String, ReferenceCode> getReferenceCodesForAlias(String alias) {
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField dictionaryField =
                dictionary.findDataDictionaryFieldByAlias(alias);

        Map<String, ReferenceCode> refCodes = new HashMap<String, ReferenceCode>();

        if (dictionaryField == null) {
            addSetupError("data dictionary alias is missing",
                    "alias: " + alias + " is missing from the data dictionary");
        } else {
            ReferenceTable refTable = dictionaryField.getReferenceTable();
            if (refTable == null) {
                addSetupError("missing reference table", "reference table not found for the " + alias + " alias ");
            } else {
                refCodes = refTable.getCodeMap(getBroker());
            }

        }

        return refCodes;
    }

    /**
     * loads existing Student Program Participation records for the CATE program.
     */
    private void loadCurrentYearEnglishLanguagePrograms() {
        String eslCodeColumnName = translateAliasToColumnName(ELL_CODE_ALIAS, false);
        if (!StringUtils.isEmpty(eslCodeColumnName)) {
            Criteria criteria = new X2Criteria();
            criteria.addEqualTo(StudentProgramParticipation.COL_PROGRAM_CODE, ENGLISH_LANGUAGE_LEARNER_PROGRAM);
            criteria.addEqualTo(eslCodeColumnName, "Y");
            QueryByCriteria query = new QueryByCriteria(StudentProgramParticipation.class, criteria);

            m_currentYearEnglishLanguageLearnerPrograms = getBroker().getMapByQuery(query,
                    StudentProgramParticipation.COL_STUDENT_OID, getBroker().getCount(query));
        } else {
            m_currentYearEnglishLanguageLearnerPrograms = new HashMap<String, StudentProgramParticipation>();
        }
    }

    /**
     * Translate alias to column name.
     *
     * @param alias String
     * @param required boolean
     * @return String
     */
    private String translateAliasToColumnName(String alias, boolean required) {
        String columnName = null;

        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(alias);
        if (field != null) {
            columnName = field.getDatabaseName();
        } else if (required) {
            String aliasMsg =
                    LocalizationCache.getMessages(getBroker().getPersistenceKey()).getMessage(ERROR_ALIAS_LOOKUP);
            addSetupError(aliasMsg, alias);
        }

        return columnName;
    }

    private static final String ERROR_ALIAS_LOOKUP = "error.state.report.alias";

    /**
     * Load a map of from studentOid to most recent Iep Oid.
     */
    private void loadIepMap() {
        X2Criteria criteria = new X2Criteria();

        if (isSchoolContext()) {
            criteria.addEqualTo(IepData.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            criteria.addEqualTo(IepData.REL_STUDENT + PATH_DELIMITER + SisStudent.REL_SCHOOL + PATH_DELIMITER
                    + School.COL_INACTIVE_INDICATOR, Boolean.FALSE);
            criteria.addEqualTo(IepData.REL_STUDENT + PATH_DELIMITER + SisStudent.REL_SCHOOL + PATH_DELIMITER
                    + School.COL_ARCHIVE_INDICATOR, Boolean.FALSE);
        }

        applyInputCriteria(criteria, false, IepData.REL_STUDENT);
        ReportQueryByCriteria query = new ReportQueryByCriteria(IepData.class,
                new String[] {IepData.COL_STUDENT_OID, "MAX(IEP_START_DATE)"}, criteria);
        query.addGroupBy(IepData.COL_STUDENT_OID);

        ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);
        m_studentRecentIep = new HashMap<String, Date>();
        try {
            while (iterator.hasNext()) {
                Object[] values = (Object[]) iterator.next();
                if (values[1] != null) {
                    m_studentRecentIep.put((String) values[0], (Date) values[1]);
                }
            }
        } finally {
            iterator.close();
        }
    }
}
