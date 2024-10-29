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
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.FormDefinition;
import com.follett.fsc.core.k12.beans.FormInstance;
import com.follett.fsc.core.k12.beans.GenericFormData;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.HashMap;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Connecticut SPED state report for Evaluation Timelines export.
 * This class implements the data export for Evaluation Timelines export.
 *
 * @author X2 Development Corporation
 */
public class EvaluationTimelines extends StateReportData {
    /**
     * Implementation of StateReportEntity to be used by the Evaluation Timelines export.
     * This must be a public static inner class with a public no argument
     * constructor so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class EvalTimelinesEntity extends StateReportEntity {
        /*
         * Member Variables
         */
        private Map<String, Object> m_enrollmentValues = new HashMap<String, Object>();

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public EvalTimelinesEntity() {
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
            IepData iepData = (IepData) getBean();
            SisStudent student = iepData.getStudent();

            String name = student.getNameView() +
                    " [LASID: " + student.getLocalId() +
                    ", SASID: " + student.getStateId() + "]";
            return name;
        }

        /**
         * Initialize.
         * If there are enrollments, setHomeServiceSchool() and setMayData() records.
         * Whether there are enrollments or not setTbeTpiData()
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see com.x2dev.sis.tools.stateexports#initialize()
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);
        }

        /**
         * Returns values from the m_enrollmentValues, given a key.
         *
         * @param key String
         * @return Object
         */
        public Object getEnrollmentValues(String key) {
            return m_enrollmentValues.get(key);
        }
    }

    /*
     * Aliases
     */

    protected String ALIAS_DISTRICT_ID = "DOE DISTRICT ID";
    protected String ALIAS_CT_IEP = "SPED-CT-IEP";
    protected String ALIAS_CT_ED625 = "ED625";
    protected String FORM_DEFINITION_CT_ED625 = "ED625";
    protected String ALIAS_CONSENT_DATE = "ed625-consent-date";
    protected String ALIAS_CONSENT = "ed625-consent";

    /*
     * Parameters
     */
    protected String PARAM_CONSENT_GIVEN = "do";

    /*
     * Instance variables.
     */
    protected DataDictionary m_ed625Dictionary;
    protected FormDefinition m_ed625FormDef;
    protected DataDictionary m_ctIepDictionary;
    protected FormDefinition m_ctIepFormDef;

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
            if (alias != null && alias.startsWith("t:")) {
                transRef = true;
                alias = alias.substring(2);
            }
            DataDictionaryField ddField = m_ctIepDictionary.findDataDictionaryFieldByAlias(alias);
            IepData iepData = (IepData) ((EvalTimelinesEntity) entity).getBean();
            value = data.getPropertyAsJavaType(iepData, ddField.getJavaName());
            if (value != null && value instanceof String && transRef) {
                value = lookupReferenceCodeByRefTbl(ddField.getReferenceTableOid(), (String) value,
                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
            }
            return value;
        }
    }

    /**
     * Class RetrieveConsentDate is the FieldRetriever for retrieving date of parental
     * consent for special education services.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveConsentDate implements FieldRetriever {

        /**
         * Returns the value of parental consent.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return PlainDate
         * @throws X2BaseException exception
         */
        @Override
        public PlainDate getFieldValue(StateReportData data,
                                       StateReportEntity entity,
                                       FieldDefinition field)
                throws X2BaseException {
            EvaluationTimelines evalTimelines = (EvaluationTimelines) data;

            PlainDate parentalConsent = null;

            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(FormInstance.COL_FORM_DEFINITION_OID, evalTimelines.m_ed625FormDef.getOid());
            IepData iepData = (IepData) ((EvalTimelinesEntity) entity).getBean();
            criteria.addEqualTo(FormInstance.COL_OWNER_OBJECT_OID, iepData.getOid());

            BeanQuery ed625entityQuery = new BeanQuery(FormInstance.class, criteria);
            FormInstance ed625FormInstance = (FormInstance) getBroker().getBeanByQuery(ed625entityQuery);
            if (ed625FormInstance != null) {
                GenericFormData gfd = (GenericFormData) ed625FormInstance.getStorageObject(getBroker());
                if (gfd != null) {
                    SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");
                    String dateString =
                            (String) gfd.getFieldValueByAlias(ALIAS_CONSENT_DATE, evalTimelines.m_ed625Dictionary);
                    try {
                        parentalConsent = new PlainDate(sdf.parse(dateString));
                    } catch (ParseException e) {
                        e.printStackTrace();
                    }
                }
            }
            return parentalConsent;
        }
    }

    /**
     * Class RetrieveEligibility is the FieldRetriever used to retrieve special education
     * eligibility code. This code may be "M" for moved, "Y" for yes, is eligible,
     * or "N" for no, is not eligible.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveEligibility implements FieldRetriever {

        /**
         * Returns the value for the eligibility code.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         */
        @Override
        public String getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            String eligibility = null;
            IepData iepData = (IepData) ((EvalTimelinesEntity) entity).getBean();
            String exitReason = iepData.getExitReason();

            if (exitReason != null && exitReason.contains("Transferred")) {
                eligibility = "M";
            } else // student has not moved, and is either eligible or not.
            {
                String value = null;
                // String alias = (String) field.getParameter();

                value = iepData.getStudent().getSpedStatusCode();

                // DataDictionaryField ddField =
                // m_ctIepDictionary.findDataDictionaryFieldByAlias(alias);
                // value = data.getPropertyAsJavaType(iepData, ddField.getJavaName());

                int referenceMap = ExportFormatField.ReferenceMapTypeCode.LOCAL.ordinal();
                // String referenceTableOid = ddField.getReferenceTableOid();
                eligibility =
                        lookupReferenceCodeByBeanPath(Student.class, Student.COL_SPED_STATUS_CODE, value, referenceMap);
            }
            return eligibility;
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
             * Retrieve formDefinition, then dictionary for "fmdCtIep" -- used to complete date of
             * parental consent
             */
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(ExtendedDataDictionary.COL_ID, ALIAS_CT_IEP);
            BeanQuery ct_iep_query = new BeanQuery(ExtendedDataDictionary.class, criteria);
            ExtendedDataDictionary ctIepExtendDictionary =
                    (ExtendedDataDictionary) getBroker().getBeanByQuery(ct_iep_query);
            m_ctIepDictionary =
                    DataDictionary.getDistrictDictionary(ctIepExtendDictionary, getBroker().getPersistenceKey());
            setDataDictionary(m_ctIepDictionary);

            /*
             * Retrieve formDefinition, then dictionary for "ED625" -- used to complete date of
             * parental consent
             */
            criteria = new X2Criteria();
            criteria.addEqualTo(FormDefinition.COL_ID, ALIAS_CT_ED625);
            BeanQuery ed625_query = new BeanQuery(FormDefinition.class, criteria);
            m_ed625FormDef = (FormDefinition) getBroker().getBeanByQuery(ed625_query);
            ExtendedDataDictionary ed625ExtendDictionary = m_ed625FormDef.getExtendedDataDictionary();
            m_ed625Dictionary =
                    DataDictionary.getDistrictDictionary(ed625ExtendDictionary, getBroker().getPersistenceKey());
            /*
             * Build the criteria/query for the students to include in this export.
             */
            Criteria iepCriteria = getIepCriteria();
            iepCriteria.addAndCriteria(getED625Criteria());
            iepCriteria.addAndCriteria(getUserCriteria());

            applyInputCriteria(iepCriteria, false, IepData.REL_STUDENT);
            QueryByCriteria iepQuery = new QueryByCriteria(IepData.class, iepCriteria);
            applyInputSort(iepQuery, IepData.REL_STUDENT);

            /*
             * Set the query to be used for IepData selection.
             */
            setQuery(iepQuery);
            setEntityClass(EvalTimelinesEntity.class);

            /*
             * Build maps of retriever functions
             */
            HashMap<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
            calcs.put("ET-PARENT-CONSENT", new RetrieveConsentDate());
            calcs.put("ET-ELIGIBILITY", new RetrieveEligibility());
            calcs.put("ET-REASON-DELAY", new RetrieveAliasValue());
            super.addCalcs(calcs);
        }
    }

    /**
     * Compose and return an iepCriteria .
     *
     * @return iepCriteria
     */
    private Criteria getIepCriteria() {
        X2Criteria iepCriteria = new X2Criteria();
        X2Criteria iepActiveCriteria = new X2Criteria();
        X2Criteria iepExitedCriteria = new X2Criteria();
        X2Criteria iepDiscardedCriteria = new X2Criteria();

        PlainDate schoolStartDate = getCurrentContext().getStartDate();

        /*
         * Active iep, exited this year, or discarded this year.
         */
        iepActiveCriteria.addEqualTo(IepData.COL_STATUS_CODE, Integer.valueOf(1));

        iepExitedCriteria.addGreaterOrEqualThan(IepData.REL_STUDENT + PATH_DELIMITER + Student.COL_SPED_EXIT_DATE,
                schoolStartDate);
        iepExitedCriteria.addEqualTo(IepData.COL_STATUS_CODE, Integer.valueOf(2));

        iepActiveCriteria.addOrCriteria(iepExitedCriteria);

        iepDiscardedCriteria.addGreaterOrEqualThan(IepData.COL_LAST_EVALUATION_DATE, schoolStartDate);
        iepDiscardedCriteria.addEqualTo(IepData.COL_STATUS_CODE, Integer.valueOf(7));

        iepActiveCriteria.addOrCriteria(iepDiscardedCriteria);
        iepCriteria.addAndCriteria(iepActiveCriteria);

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

        return iepCriteria;
    }

    /**
     * Compose and return an ED625Criteria .
     *
     * @return iepCriteria
     */
    private Criteria getED625Criteria() {
        /*
         * IEP evaluation consent form signed by parents
         * between July 1 and June 30 of this academic year
         */
        X2Criteria dateOfConsentCriteria = new X2Criteria();
        X2Criteria formInstanceCriteria = new X2Criteria();
        X2Criteria genericFormDataCriteria = new X2Criteria();
        X2Criteria getIepOidsCriteria = new X2Criteria();

        // create + set calendar
        Calendar calendar = Calendar.getInstance();
        PlainDate schoolStartDate = getCurrentContext().getStartDate();
        calendar.setTime(schoolStartDate);

        // set start date marker- july1StartOfYear
        calendar.set(Calendar.MONTH, Calendar.JULY);
        calendar.set(Calendar.DAY_OF_MONTH, 1);
        PlainDate july1StartOfYear = new PlainDate(calendar.getTime());

        // set end date marker - july1StartOfNextYear
        calendar.add(Calendar.YEAR, 1);
        PlainDate july1StartOfNextYear = new PlainDate(calendar.getTime());

        // get form instance for ED625, Parental Consent
        formInstanceCriteria.addEqualTo(FormInstance.REL_FORM_DEFINITION + "." + FormDefinition.COL_ID,
                FORM_DEFINITION_CT_ED625);
        SubQuery formInstanceSubQuery =
                new SubQuery(FormInstance.class, FormInstance.COL_STORAGE_OBJECT_OID, formInstanceCriteria);

        // get consentDateField and consentField from ED625 dictionary
        DataDictionaryField consentDateField = m_ed625Dictionary.findDataDictionaryFieldByAlias(ALIAS_CONSENT_DATE);
        DataDictionaryField consentField = m_ed625Dictionary.findDataDictionaryFieldByAlias(ALIAS_CONSENT);

        // query all Generic Form Data within date range, and having consent given
        genericFormDataCriteria.addIn(X2BaseBean.COL_OID, formInstanceSubQuery);
        genericFormDataCriteria.addGreaterOrEqualThan(consentDateField.getJavaName(), july1StartOfYear);
        genericFormDataCriteria.addLessThan(consentDateField.getJavaName(), july1StartOfNextYear);
        genericFormDataCriteria.addEqualTo(consentField.getJavaName(), PARAM_CONSENT_GIVEN);
        SubQuery genericFormDataSubQuery =
                new SubQuery(GenericFormData.class, X2BaseBean.COL_OID, genericFormDataCriteria);

        // query all IEP oid's from queried Generic Form Data, then apply to IepData.
        getIepOidsCriteria.addIn(FormInstance.COL_STORAGE_OBJECT_OID, genericFormDataSubQuery);
        SubQuery getIepOidsSubQuery =
                new SubQuery(FormInstance.class, FormInstance.COL_OWNER_OBJECT_OID, getIepOidsCriteria);
        dateOfConsentCriteria.addIn(X2BaseBean.COL_OID, getIepOidsSubQuery);

        return dateOfConsentCriteria;

    }

    /**
     * Compose and return an ED625Criteria .
     *
     * @return iepCriteria
     */
    private Criteria getUserCriteria() {
        /*
         * Check school selection user input parameter.
         */

        X2Criteria userCriteria = new X2Criteria();

        if (isSchoolContext()) {
            userCriteria.addEqualTo(IepData.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_SCHOOL_OID,
                    getSchool().getOid());
        } else {
            userCriteria.addEqualTo(IepData.REL_STUDENT + PATH_DELIMITER + SisStudent.REL_SCHOOL + PATH_DELIMITER
                    + School.COL_INACTIVE_INDICATOR, Boolean.FALSE);
            userCriteria.addEqualTo(IepData.REL_STUDENT + PATH_DELIMITER + SisStudent.REL_SCHOOL + PATH_DELIMITER
                    + School.COL_ARCHIVE_INDICATOR, Boolean.FALSE);
        }

        return userCriteria;
    }
}
