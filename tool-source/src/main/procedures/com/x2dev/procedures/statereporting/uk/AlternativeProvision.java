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
package com.x2dev.procedures.statereporting.uk;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.BeanValidator;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportModel;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.follett.fsc.core.k12.tools.stateexports.XMLStateReportData;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.jdom.Element;

/**
 * The Class AlternativeProvision.
 */
public class AlternativeProvision extends XMLStateReportData {
    /**
     * UK CBDS model
     */
    private StateReportModel m_model;
    protected PlainDate m_censusDate;
    protected Set m_UPNList;

    protected Map<String, Collection<StudentProgramParticipation>> m_fsmMap;
    protected final String FRL_CODE = "FRL";
    protected final String SE_CODE = "SE";

    private static final String VAR_CENSUS_DATE = "censusDate";

    /**
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        m_model = loadModel("CBDS");
        m_model.set("report", "AlternativeProvision");

        m_censusDate = (PlainDate) getParameter(VAR_CENSUS_DATE);
        setElements();

        Map<String, FieldValidator> validatorMap = m_model.getValidators();

        m_UPNList = new HashSet<String>();

        /* Add validation for fields in export */
        validatorMap.put("UPN", new ValidateUPN());
        validatorMap.put("DOB", new ValidateDOB());
        validatorMap.put("SCHCEN-SVCINED", new ValidateServiceChild());
        validatorMap.put("ADDRESSLINEONE", new ValidateAddressLineOne());
        validatorMap.put("ALTPROVISION", new ValidateAltProvision());

        super.addValidators(validatorMap);

        StudentHistoryHelper helper = new StudentHistoryHelper(this);
        helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_SNAPSHOT);
        helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_censusDate);

        // modify the student criteria to limit to students with alternative provision programs
        X2Criteria studentCriteria = helper.getStudentCriteria();
        X2Criteria apCriteria = loadAP(studentCriteria);

        SubQuery apQuery = new SubQuery(StudentProgramParticipation.class, StudentProgramParticipation.COL_STUDENT_OID,
                apCriteria);
        studentCriteria.addIn(X2BaseBean.COL_OID, apQuery);

        QueryByCriteria studentQuery = new QueryByCriteria(SisStudent.class, studentCriteria);// helper.getStudentQuery(false);
        Collection<SisStudent> students = getBroker().getCollectionByQuery(studentQuery);
        Collection<SisStudent> keepStudents = new HashSet<SisStudent>();

        String activeCode =
                PreferenceManager.getPreferenceValue(getOrganization(), SystemPreferenceDefinition.STAFF_ACTIVE_CODE);

        /* Check that Student was enrolled at time of census and remove them if not */
        Iterator studentIterator = students.iterator();
        while (studentIterator.hasNext()) {
            SisStudent student = (SisStudent) studentIterator.next();
            StudentEnrollment censusEnrollment = helper.getEnrollmentForDate(student.getOid(), m_censusDate, "EWS");

            /* Student has an active enrollment record */
            if (censusEnrollment != null && (activeCode.equals(censusEnrollment.getStatusCode()))) {
                keepStudents.add(student);
            }
        }

        setCollectionToQuery("pupils", keepStudents);
        m_fsmMap = loadFreeMeals(studentCriteria);

        Map<String, Collection<StudentProgramParticipation>> senMap = loadSens(studentCriteria);

        if (getSetupErrors().isEmpty()) {
            m_model.initializeRetriever("SYSTEM");
            m_model.initializeRetriever("SENPROVISION", senMap);
            m_model.initializeRetriever("ALTPROVISION", studentCriteria);
            super.addCalcs(m_model.getRetrievers());
        }
    }

    /**
     * Set new elements on template.
     */
    public void setElements() {
        Element referenceDate = new Element("ReferenceDate");

        referenceDate.setText(m_censusDate.toString());
        setElement("ReferenceDateId", referenceDate);
    }

    /**
     * Pre-loads the 'free meals' programs for each student in this report, if they have any.
     *
     * @param studentCriteria X2Criteria
     * @return Map
     */
    private Map loadFreeMeals(X2Criteria studentCriteria) {
        Map<String, Collection<StudentProgramParticipation>> fsmMap = null;

        X2Criteria criteria = null;
        DataDictionaryField field =
                getDataDictionaryField(StudentProgramParticipation.class, StudentProgramParticipation.COL_PROGRAM_CODE);
        String referenceTableOid = field.getReferenceTableOid();
        Collection<String> reportableCodes = null;
        if (!StringUtils.isEmpty(referenceTableOid)) {
            criteria = new X2Criteria();
            criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, referenceTableOid);
            criteria.addEqualTo(ReferenceCode.COL_LOCAL_CODE, FRL_CODE);
            SubQuery query = new SubQuery(ReferenceCode.class, ReferenceCode.COL_CODE, criteria);
            reportableCodes = getBroker().getSubQueryCollectionByQuery(query);
        }

        if (reportableCodes != null && reportableCodes.size() > 0) {
            SubQuery studentSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria.copy());

            X2Criteria freeMealsCriteria = new X2Criteria();
            freeMealsCriteria.addIn(StudentProgramParticipation.COL_STUDENT_OID, studentSubQuery); // reporting
                                                                                                   // students
            X2Criteria endDateCriteria = new X2Criteria();
            endDateCriteria.addGreaterThan(StudentProgramParticipation.COL_END_DATE, m_censusDate);
            X2Criteria endDateNullCriteria = new X2Criteria();
            endDateNullCriteria.addIsNull(StudentProgramParticipation.COL_END_DATE);
            endDateCriteria.addOrCriteria(endDateNullCriteria);
            freeMealsCriteria.addAndCriteria(endDateCriteria);/*
                                                               * Current fsm records for students in
                                                               * student query
                                                               */
            freeMealsCriteria.addIn(StudentProgramParticipation.COL_PROGRAM_CODE, reportableCodes); // meals

            BeanQuery freeMealsQuery = new BeanQuery(StudentProgramParticipation.class, freeMealsCriteria);
            freeMealsQuery.addOrderByAscending(StudentProgramParticipation.COL_START_DATE); // most
                                                                                            // recent
            freeMealsQuery.addOrderByAscending(X2BaseBean.COL_LAST_MODIFIED_TIME);

            fsmMap = getBroker().getGroupedCollectionByQuery(freeMealsQuery,
                    StudentProgramParticipation.COL_STUDENT_OID, 64);
        }
        setGroupedCollectionToQuery("fsm", fsmMap);
        return fsmMap;
    }

    /**
     * Pre-loads students' special education needs (a.k.a. "IEP" in the US)
     *
     * @param studentCriteria X2Criteria
     * @return Map
     */
    private Map<String, Collection<StudentProgramParticipation>> loadSens(X2Criteria studentCriteria) {
        Map<String, Collection<StudentProgramParticipation>> senMap = null;

        X2Criteria criteria = null;
        DataDictionaryField field =
                getDataDictionaryField(StudentProgramParticipation.class, StudentProgramParticipation.COL_PROGRAM_CODE);
        String referenceTableOid = field.getReferenceTableOid();
        Collection<String> reportableCodes = null;
        if (!StringUtils.isEmpty(referenceTableOid)) {
            criteria = new X2Criteria();
            criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, referenceTableOid);
            criteria.addEqualTo(ReferenceCode.COL_LOCAL_CODE, SE_CODE);
            SubQuery query = new SubQuery(ReferenceCode.class, ReferenceCode.COL_CODE, criteria);
            reportableCodes = getBroker().getSubQueryCollectionByQuery(query);
        }

        if (reportableCodes != null && reportableCodes.size() > 0) {
            SubQuery studentSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria.copy());

            String senProvisionBeanPath = translateAliasToJavaName("DFE SEN PROVISION", true);
            DataDictionaryField senProvisionField =
                    getDataDictionaryField(StudentProgramParticipation.class, senProvisionBeanPath);
            List<String> senCodes = new ArrayList<String>();
            if (senProvisionField != null && !StringUtils.isEmpty(senProvisionField.getReferenceTableOid())) {
                Map<String, ReferenceCode> senCodesMap = getReferenceCodes(senProvisionField.getReferenceTableOid());
                for (ReferenceCode referenceCode : senCodesMap.values()) {
                    String code = referenceCode.getCode();
                    String stateCode = referenceCode.getStateCode();
                    if (!"N".equals(stateCode)) {
                        senCodes.add(code);
                    }
                }
            }

            X2Criteria senCriteria = new X2Criteria();
            senCriteria.addIn(StudentProgramParticipation.COL_STUDENT_OID, studentSubQuery);
            senCriteria.addIn(senProvisionBeanPath, senCodes);
            senCriteria.addIn(StudentProgramParticipation.COL_PROGRAM_CODE, reportableCodes);
            BeanQuery senQuery = new BeanQuery(StudentProgramParticipation.class, senCriteria);
            senMap = getBroker().getGroupedCollectionByQuery(senQuery, StudentProgramParticipation.COL_STUDENT_OID,
                    128);
        }

        return senMap;
    }

    /**
     * Return criteria for students with AP records.
     *
     * @param studentCriteria X2Criteria
     * @return X2Criteria
     */
    private X2Criteria loadAP(X2Criteria studentCriteria) {
        X2Criteria apCriteria = new X2Criteria();

        X2Criteria criteria = null;
        DataDictionaryField field =
                getDataDictionaryField(StudentProgramParticipation.class, StudentProgramParticipation.COL_PROGRAM_CODE);
        String referenceTableOid = field.getReferenceTableOid();
        Collection<String> reportableCodes = null;

        if (!StringUtils.isEmpty(referenceTableOid)) {
            criteria = new X2Criteria();
            criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, referenceTableOid);
            criteria.addEqualTo(ReferenceCode.COL_LOCAL_CODE, SE_CODE);
            SubQuery query = new SubQuery(ReferenceCode.class, ReferenceCode.COL_CODE, criteria);
            reportableCodes = getBroker().getSubQueryCollectionByQuery(query);
        }
        if (reportableCodes != null && reportableCodes.size() > 0) {
            SubQuery studentSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria.copy());
            String alternativeProvisionBean = translateAliasToJavaName("DFE AP TYPE", true);

            /* Students are allowed to have a pgm end date before the census date or null */
            X2Criteria endDateCriteria = new X2Criteria();
            endDateCriteria.addGreaterThan(StudentProgramParticipation.COL_END_DATE, m_censusDate);
            X2Criteria endDateNullCriteria = new X2Criteria();
            endDateNullCriteria.addIsNull(StudentProgramParticipation.COL_END_DATE);
            endDateCriteria.addOrCriteria(endDateNullCriteria);

            apCriteria.addIn(StudentProgramParticipation.COL_STUDENT_OID, studentSubQuery);
            apCriteria.addIn(StudentProgramParticipation.COL_PROGRAM_CODE, reportableCodes);
            apCriteria.addNotEmpty(alternativeProvisionBean, getBroker().getPersistenceKey());
            apCriteria.addNotNull(alternativeProvisionBean);
            apCriteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, m_censusDate);
            apCriteria.addAndCriteria(endDateCriteria);/*
                                                        * Make sure record meets end date criteria
                                                        */
        }

        return apCriteria;
    }

    /**
     * Check census validations for students UPN number.
     */
    protected class ValidateUPN extends BeanValidator {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.BeanValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             X2BaseBean bean,
                                             FieldDefinition field,
                                             String value) {

            ArrayList<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            String upn = value;
            String studentName = ((SisStudent) bean).getNameView();

            if (m_UPNList.contains(upn)) {
                errors.add(new StateReportValidationError(studentName, field.getFieldId(),
                        "Error 1520 More than one pupil record with the same UPN.", value));
            } else {
                m_UPNList.add(upn);
            }

            return errors;
        }
    }

    /**
     * Check census validations for students age.
     */
    protected class ValidateDOB extends BeanValidator {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.BeanValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             X2BaseBean bean,
                                             FieldDefinition field,
                                             String value) {

            ArrayList<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            SisStudent student = (SisStudent) bean;
            int ageAsOfCensusDate = student.getPerson().getAgeAsOfDate(m_censusDate);
            String senProvision = data.getFieldValue(bean, "100472");

            if (ageAsOfCensusDate < 3) {
                errors.add(new StateReportValidationError(student.getNameView(), field.getFieldId(),
                        "Query 8300Q Please check: Pupil is younger than 3 years old. Pupils should only be on the AP Census if they are not attending a PVI "
                                +
                                "(Private, Voluntary and Independent) Early Years Setting or maintained nursery",
                        value));
            } else if (ageAsOfCensusDate > 18) {
                errors.add(new StateReportValidationError(student.getNameView(), field.getFieldId(),
                        "Query 8305Q Please check: Pupil is over 18 years old.", value));
            } else if (ageAsOfCensusDate <= 2) {
                if ("N".equals(senProvision)) {
                    errors.add(new StateReportValidationError(student.getNameView(), field.getFieldId(),
                            "Error 8330 Pupil aged 2 with no Special Needs", value));
                }
            }

            return errors;
        }
    }

    /**
     * Check census validations for student's ethnicity.
     */
    protected class ValidateEthnicity extends BeanValidator {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.BeanValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             X2BaseBean bean,
                                             FieldDefinition field,
                                             String value) {

            ArrayList<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            String ethnicity = value;
            SisStudent student = (SisStudent) bean;
            int ageAsOfCensusDate = student.getPerson().getAgeAsOfDate(m_censusDate);

            if (ageAsOfCensusDate >= 5) {
                if (StringUtils.isEmpty(ethnicity)) {
                    errors.add(new StateReportValidationError(student.getNameView(), field.getFieldId(),
                            "Error 1630 Pupil aged 5 or over with ethnic group missing or invalid.", value));
                }
            }

            return errors;
        }
    }

    /**
     * Check if the students has a parent in the service.
     */
    protected class ValidateServiceChild extends BeanValidator {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.BeanValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             X2BaseBean bean,
                                             FieldDefinition field,
                                             String value) {

            ArrayList<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            String serviceChild = value;
            SisStudent student = (SisStudent) bean;

            if (serviceChild.equals("Y")) {
                /* Student has current fsm record */
                if (m_fsmMap.containsKey(student.getOid())) {
                    errors.add(new StateReportValidationError(student.getNameView(), field.getFieldId(),
                            "Error 1741 Based on Ministry of Defence criteria, Service Children " +
                                    "(i.e. parents designated as personnel category 1 or 2) are not eligible for Free School Meals.",
                            value));
                }
            }

            return errors;
        }
    }

    /**
     * Check for valid Alternative Provisions.
     */
    protected class ValidateAltProvision extends BeanValidator {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.BeanValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             X2BaseBean bean,
                                             FieldDefinition field,
                                             String value) {
            ArrayList<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            SisStudent student = (SisStudent) bean;
            String senProvision = data.getFieldValue(bean, "100472");

            if ("N".equals(senProvision)) {
                if (value.equals("NMS")) {
                    errors.add(new StateReportValidationError(student.getNameView(), field.getFieldId(),
                            "Query 8320Q Please check: Pupil in non-maintained special school shown with no Special Needs.",
                            value));
                }
                if (value.equals("ACD")) {
                    errors.add(new StateReportValidationError(student.getNameView(), field.getFieldId(),
                            "Error 8325 Please check: Pupil in an Academy shown with no Special Needs.", value));
                }
            }

            return errors;
        }
    }

    /**
     * Validate student's address information.
     */
    protected class ValidateAddressLineOne extends BeanValidator {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.BeanValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             X2BaseBean bean,
                                             FieldDefinition field,
                                             String value) {

            ArrayList<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            String studentName = ((SisStudent) bean).getNameView();

            String line1 = value;
            String line2 = data.getFieldValue(bean, "100129");
            String line3 = data.getFieldValue(bean, "100130");
            String line4 = data.getFieldValue(bean, "100131");
            String line5 = data.getFieldValue(bean, "100132");

            String remainderAddress = line2 + line3 + line4 + line5;

            /*
             * If the student had an address line one they must also have
             * information in one of the other address lines
             */
            if (!StringUtils.isEmpty(line1)) {
                if (StringUtils.isEmpty(remainderAddress)) {
                    errors.add(new StateReportValidationError(studentName, field.getFieldId(),
                            "Query 2400Q Where first line present, at least one other address line should also be present.",
                            "Address is invalid."));
                }
            }

            return errors;
        }
    }

}
