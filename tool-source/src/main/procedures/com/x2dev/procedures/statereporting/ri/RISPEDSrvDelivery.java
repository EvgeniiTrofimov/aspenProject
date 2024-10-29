/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2002-2011 Follett Software Company.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.ri;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepDisability;
import com.x2dev.sis.model.beans.IepService;
import com.x2dev.sis.model.beans.IepServiceLog;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.DateAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * State report data module for the RI Special Education Services census. The base class for this
 * state report is IEP_SERVICE. Service records delivered between the start of the school year
 * and the report date are included in this report. Only students included in the Student Census
 * are included, based on the "In Last State Report" boolean alias on the IEP record. Therefore,
 * it is required that the Student Census be run prior to running this report.
 * <p>
 * For the students included in the student census, all services delivered between the start of the
 * school year (of the student's primary school) and the report date are included. This includes
 * services from previous or amended IEPs. Within this selection, services are eliminated and dates
 * are adjusted automatically if the "adjustServiceDates" parameter has been set. Adjustments are
 * made into the "service-adjusted-start-date" and "service-adjusted-end-date" aliases on the
 * service record. If the entire service is eliminated, the "service-in-last-state report" boolean
 * alias is set to false. The reason for automatic adjustment is saved in the
 * "service-adjustment-reason" alias. This allows the adjustments to be viewed on the IEP_SERVICE
 * table after running this report. The "Service Census Audit" report can also be run for a single
 * student and provides a convenient format for viewing this information.
 * <p>
 * The following automatic adjustments are made:
 * <ul>
 * <li>Overlapping date ranges between services in previous and active IEPs are adjusted. Services
 * in the older IEPs are set to end one day before the start date of the newer IEP.
 * <li>Overlapping date ranges between the same service in an amended and new IEP are eliminated.
 * The amended IEP service is adjusted so it does not conflict with the new IEP service. Note that
 * this requires the service to be paired between the amended and the new IEP. If this cannot be
 * done accurately, a validation error is generated. (This is the case if two services of the same
 * type exist, for example.)
 * <li>Services whose dates fall outside of the school year are adjusted to fit within the
 * boundaries of the year.
 * <li>Future service end dates are reported as blank.
 * </ul>
 * <p>
 * A number of custom field retrievers and validators are used; see javadoc on the corresponding
 * inner classes for details. If the bypassStateRules input parameter is set, custom validators
 * are bypassed.
 *
 * @author mmastrangelo
 */
public class RISPEDSrvDelivery extends RIStateReportData {

    /**
     * Entity Class
     * 
     * @author Follett Software Company
     * @copyright 2020
     */
    public static class RISPEDSrvDeliveryEntity extends StateReportEntity {

        /**
         * Instantiates a new compu claim entity.
         */
        public RISPEDSrvDeliveryEntity() {
            // Used for dynamic instantiation.
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            IepServiceLog isl = (IepServiceLog) bean;
            if (isl.getDuration() == 0) {
                setRowCount(0);
            }
        }
    }

    /**
     * Constants
     */
    private static final String ALIAS_SERVICE_RELATED = "service-related";
    private static final String IN_LAST_STATE_REPORT_ALIAS = "sped-in-last-state-report";
    private static final String MEMBERSHIP_PLACEMENT_LOCATION_CODE_ALIAS = "OP Location Code";
    private static final String OUTSIDE_PLACEMENT_SCHOOL_ALIAS = "Outside Placement School";
    private static final String PARAM_END_DATE = "endDate";
    private static final String PARAM_START_DATE = "startDate";
    private static final String PARAM_UPLOAD_DATE = "uploadDate";
    private static final String SERVICE_SCHOOL_CODE_OVERRIDE_ALIAS = "service-school-code";
    private static final String SERVICE_UPLOAD_DATE_ALIAS = "service-upload-date";
    private static final String SPED_RI_IEP_DDX_ID = "SPED-RI-IEP";
    private static final String STATE_VALIDATION_R2876 =
            "R2876 - if ServiceMode='Related Services' then Related_Service is required";

    /**
     * Class members
     */
    private DateAsStringConverter m_dateStringConverter;
    private DataDictionary m_dictionary;
    private PlainDate m_endDate;
    private Map<String, StudentEnrollment> m_latestEntries;
    private String m_relatedServiceFeild;
    private PlainDate m_reportDate;
    private PlainDate m_startDate;
    private boolean m_uploadDate;

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();

        m_reportDate = new PlainDate();

        m_dateStringConverter = (DateAsStringConverter) ConverterFactory.getConverterForClass(Converter.DATE_CONVERTER,
                null, true);

        m_dictionary = DataDictionary.getDistrictDictionary(getExtendedDataDictionaryById(SPED_RI_IEP_DDX_ID),
                getBroker().getPersistenceKey());

        m_relatedServiceFeild = translateAliasToJavaName(ALIAS_SERVICE_RELATED, true);
        // loadCalendarInfo();
        if (getSetupErrors().size() != 0) {
            return;
        }

        m_startDate = (PlainDate) getParameter(PARAM_START_DATE);
        m_endDate = (PlainDate) getParameter(PARAM_END_DATE);
        m_uploadDate = (boolean) getParameter(PARAM_UPLOAD_DATE);

        Criteria serviceLogCriteria = buildServiceLogsCriteria();

        setQuery(new QueryByCriteria(IepServiceLog.class, serviceLogCriteria));

        setEntityClass(RISPEDSrvDeliveryEntity.class);

        /*
         * Load a map of the latest entry enrollment records on or before the report date
         */
        loadLatestEntriesLookup();

        /*
         * Add field retrievers to support calculated columns
         */
        addCustomCalcs();

        /*
         * Add field validators to support custom validation rules
         */
        addCustomValidators();
    }

    /**
     * Convenience method used to determine if the passed code equals one of the codes provided in
     * the list of codes.
     *
     * @param code String
     * @param codes String[]
     * @return boolean
     */
    protected boolean isInList(String code, String... codes) {
        boolean inList = false;

        for (String aCode : codes) {
            if (code.equals(aCode)) {
                inList = true;
                break;
            }
        }

        return inList;
    }

    /**
     * Add custom field retrievers.
     */
    private void addCustomCalcs() {
        HashMap<String, FieldRetriever> calcRetrievers = new HashMap<String, FieldRetriever>();
        calcRetrievers.put("risped-log-srv", new ServiceLogRetriever());
        addCalcs(calcRetrievers);
    }

    /**
     * Add custom validators.
     */
    private void addCustomValidators() {

        HashMap<String, FieldValidator> validators = new HashMap<String, FieldValidator>();

        validators.put("risped-RelatedSvc", new RelatedServiceValidator());
        // validators.put("risped-RelatedSvcOth", new RelatedServiceOtherValidator());

        addValidators(validators);

    }

    /**
     * Returns the criteria for the IEPs to include. All services for students included in the
     * student census, based on the IN_LAST_STATE_REPORT_ALIAS on the IEP are included. Services
     * falling within the school year starting before the report date are included.
     *
     * @return Criteria - IEPs with the IN_LAST_STATE_REPORT_ALIAS field set to true
     */
    private X2Criteria buildServiceLogsCriteria() {
        X2Criteria iepServiceLogsCriteria = new X2Criteria();

        iepServiceLogsCriteria.addGreaterOrEqualThan(IepServiceLog.COL_DATE, m_startDate);
        iepServiceLogsCriteria.addLessOrEqualThan(IepServiceLog.COL_DATE, m_endDate);
        iepServiceLogsCriteria.addNotEmpty(
                IepServiceLog.REL_IEP_SERVICE + ModelProperty.PATH_DELIMITER + X2BaseBean.COL_OID,
                getBroker().getPersistenceKey());

        return iepServiceLogsCriteria;
    }

    /**
     * Gets the extended data dictionary by id.
     *
     * @param ddxId String
     * @return Extended data dictionary
     */
    private ExtendedDataDictionary getExtendedDataDictionaryById(String ddxId) {
        ExtendedDataDictionary extendedDataDictionary = null;
        X2Criteria ddxCriteria = new X2Criteria();

        ddxCriteria.addEqualTo(ExtendedDataDictionary.COL_ID, ddxId);

        QueryByCriteria ddxQuery = new QueryByCriteria(ExtendedDataDictionary.class, ddxCriteria);
        extendedDataDictionary =
                (ExtendedDataDictionary) getBroker().getBeanByQuery(ddxQuery);
        return extendedDataDictionary;
    }

    /**
     * Loads the enrollment lookup map - latest 'E' records on or prior to the report date.
     */
    private void loadLatestEntriesLookup() {
        Criteria iepCriteria = new Criteria();
        iepCriteria.addEqualTo(translateAliasToJavaName(IN_LAST_STATE_REPORT_ALIAS, true),
                BooleanAsStringConverter.TRUE);

        Criteria enrollmentCriteria = new Criteria();
        enrollmentCriteria.addEqualTo(StudentEnrollment.COL_ENROLLMENT_TYPE, StudentEnrollment.ENTRY);
        enrollmentCriteria.addLessOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, m_reportDate);
        enrollmentCriteria.addIn(StudentEnrollment.COL_STUDENT_OID,
                new SubQuery(IepData.class, IepData.COL_STUDENT_OID, iepCriteria));

        BeanQuery enrollmentQuery = new BeanQuery(StudentEnrollment.class, enrollmentCriteria);
        enrollmentQuery.addOrderByAscending(StudentEnrollment.COL_ENROLLMENT_DATE); // ensures that
                                                                                    // the record
                                                                                    // with the max
                                                                                    // date will end
                                                                                    // up in the map

        m_latestEntries = getBroker().getMapByQuery(enrollmentQuery, StudentEnrollment.COL_STUDENT_OID, 100);
    }



    /* ----------------------------------------------------------------------------------------- */
    /*
     * RETRIEVERS
     * /* -----------------------------------------------------------------------------------------
     */



    /**
     * Returns the school code to use for each service record. For outplaced students, this is the
     * outside placement location
     * pulled from the most recent enrollment record. Outplaced students are identified by being in
     * an Aspen school with the
     * outside placement flag set, or a school with a state code ending in "90" (per RIDE
     * conventions).
     *
     * @author mmastrangelo
     */
    protected class ServiceLogRetriever implements FieldRetriever {

        private static final String CALC_PARAM_RELATED_SERVICE_CODE = "relatedServiceCode";
        private static final String CALC_PARAM_UPLD_DATE = "upldDate";
        private static final String CALC_PARAM_UPLOAD_DATE = "uploadDate";
        private static final String CALC_PARAM_DIAGNOSIS_CODE = "diagnosisCode";
        private static final String CALC_PARAM_SCHOOL_CODE = "schoolCode";

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
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            IepServiceLog serviceLog = (IepServiceLog) entity.getBean();
            IepService service = serviceLog.getIepService();
            String param = (String) field.getParameter();
            Object returnValue = null;

            if (param.equals(CALC_PARAM_SCHOOL_CODE)) {
                String outsidePlacementCode = (String) service.getFieldValueByAlias(SERVICE_SCHOOL_CODE_OVERRIDE_ALIAS);
                if (StringUtils.isEmpty(outsidePlacementCode)) {
                    Student student = service.getIepData().getStudent();
                    School school = student.getSchool();

                    String outsidePlacementSchoolFlag =
                            (String) school.getFieldValueByAlias(OUTSIDE_PLACEMENT_SCHOOL_ALIAS);
                    String stateSchoolId = (String) school.getFieldValueByBeanPath(m_sklIdField);

                    outsidePlacementCode = stateSchoolId;

                    if ("1".equals(outsidePlacementSchoolFlag)
                            || (stateSchoolId != null && stateSchoolId.endsWith("90"))) {
                        StudentEnrollment enrollment = m_latestEntries.get(student.getOid());
                        if (enrollment != null) {
                            String outsidePlacementUserCode =
                                    (String) enrollment.getFieldValueByAlias(MEMBERSHIP_PLACEMENT_LOCATION_CODE_ALIAS);
                            outsidePlacementCode = lookupReferenceCodeByAlias(MEMBERSHIP_PLACEMENT_LOCATION_CODE_ALIAS,
                                    outsidePlacementUserCode, ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                        }
                    }
                }
                returnValue = outsidePlacementCode;
            } else if (param.equals(CALC_PARAM_DIAGNOSIS_CODE)) {
                for (IepDisability disability : service.getIepData().getIepDisability()) {
                    if (disability.getPrimaryIndicator()) {
                        String disabilityCOde = disability.getDisabilityCode();
                        returnValue =
                                lookupReferenceCodeByBeanPath(IepDisability.class, IepDisability.COL_DISABILITY_CODE,
                                        disabilityCOde, ExportFormatField.ReferenceMapTypeCode.LOCAL.ordinal());
                        break;
                    }
                }
            } else if (param.equals(CALC_PARAM_UPLD_DATE)) {
                returnValue = m_reportDate;
            } else if (param.equals(CALC_PARAM_UPLOAD_DATE) && m_uploadDate) {
                returnValue = m_reportDate;
                service.setFieldValueByAlias(SERVICE_UPLOAD_DATE_ALIAS,
                        m_dateStringConverter.getSystemString(m_reportDate), m_dictionary);
                getBroker().saveBeanForced(service);
            } else if (param.equals(CALC_PARAM_RELATED_SERVICE_CODE)) {

                String relatedService =
                        (String) service.getFieldValueByAlias(ALIAS_SERVICE_RELATED, getDataDictionary());

                // use this block if need show description when related service has "other" value
                /*
                 * if (!StringUtils.isEmpty(relatedService))
                 * {
                 * String stateCode = lookupStateValue(IepService.class, m_relatedServiceFeild,
                 * relatedService);
                 * /*if (RELATED_SERVICE_OTHER_CODE.equalsIgnoreCase(stateCode))
                 * {
                 * returnValue = service.getFieldValueByAlias(SERVICE_DESCRIPTION_ALIAS,
                 * getDataDictionary());
                 * }
                 * else
                 * {
                 * returnValue = relatedService;
                 * }
                 * }
                 */
                returnValue = relatedService;

            }

            return returnValue;
        }
    }


    /* ----------------------------------------------------------------------------------------- */
    /*
     * VALIDATORS
     * /* -----------------------------------------------------------------------------------------
     */


    /**
     * 2876.
     *
     * @author Follett Software Company
     */
    protected class RelatedServiceValidator implements FieldValidator {

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett
         *      .fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            LinkedList<StateReportValidationError> errors = new LinkedList<StateReportValidationError>();

            IepServiceLog iepServiceLog = (IepServiceLog) entity.getBean();
            String serviceMode = iepServiceLog.getIepService().getServiceMode();
            String relatedService = entity.getFieldValue("RelatedServiceCode");

            if ("Related Services".equals(serviceMode) && StringUtils.isEmpty(relatedService)) {
                errors.add(new StateReportValidationError(entity, field, STATE_VALIDATION_R2876, ""));
            }

            return errors;
        }
    }

    /**
     * 2877
     *
     * @author Follett Software Company
     */
    // not used because this export han't RelatedServiceOther field. But maybe it is will need.
    // current export has RelatedService field and they are related
    /*
     * protected class RelatedServiceOtherValidator implements FieldValidator
     * {
     *//**
        * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
        *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
        *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
        *//*
           * public Collection getFieldValidation(StateReportData data, StateReportEntity entity,
           * FieldDefinition field, String value)
           * {
           * LinkedList<StateReportValidationError> errors = new
           * LinkedList<StateReportValidationError>();
           *
           * String serviceType = entity.getFieldValue("Service_Type");
           * String relatedService = entity.getFieldValue("Related_Service");
           * String relatedServiceOther = entity.getFieldValue("Related_Service_Othe");
           *
           * if ("RelServ".equals(serviceType) && "Q".equals(relatedService) &&
           * StringUtils.isEmpty(relatedServiceOther))
           * {
           * errors.add(new StateReportValidationError(entity, field, STATE_VALIDATION_R2877, ""));
           * }
           *
           * return errors;
           * }
           * }
           */
}
