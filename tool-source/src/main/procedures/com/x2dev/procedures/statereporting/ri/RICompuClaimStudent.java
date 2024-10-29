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
package com.x2dev.procedures.statereporting.ri;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepDisability;
import com.x2dev.sis.model.beans.IepService;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.SystemStringConverter;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.Criteria;

/**
 * The Class RICompuClaimStudent.
 */
public class RICompuClaimStudent extends RIStateReportData {

    /**
     * The Class CompuClaimEntity.
     */
    public static class CompuClaimEntity extends StateReportEntity {
        private static final String NO_PERFMON4J_INSTRUMENTATION = "";

        /**
         * Instantiates a new compu claim entity.
         */
        public CompuClaimEntity() {
            // Used for dynamic instantiation.
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            RICompuClaimStudent ccData = (RICompuClaimStudent) data;

            SisStudent student = ((IepData) bean).getStudent();

            if (ccData.m_aliasField != null) {
                String lastTimeExportedString = (String) student.getFieldValueByBeanPath(ccData.m_aliasField);
                if (!StringUtils.isEmpty(lastTimeExportedString)) {
                    Timestamp lastTimeExported =
                            (Timestamp) ccData.m_timestampConverter.parseSystemString(lastTimeExportedString);
                    if (lastTimeExported != null && student.getLastModifiedTime() < lastTimeExported.getTime()) {
                        setRowCount(0);
                        return;
                    }
                }
            }

            if (ccData.m_updStudents && ccData.m_aliasField != null) {
                long currentTime = (new Date()).getTime() + 60000; // add 60 seconds to time to
                                                                   // exceed new last update time
                String value = ccData.m_timestampConverter.getSystemString(new Timestamp(currentTime));
                student.setFieldValueByBeanPath(ccData.m_aliasField, value);
                ccData.getBroker().saveBeanForced(student);
            }
        }
    }

    /**
     * Retrieves the student's primary disability. If only one disability exists, that disability
     * is used. If more than one exists, the one flagged as primary is used. If more than one
     * exists,
     * and one is not flagged as primary, or more than one is flagged as primary, a validation
     * error is generated.
     *
     * @author Follett Software Company
     */
    protected class PrimaryDisabilityRetriever implements FieldRetriever {

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
            IepData iep = (IepData) entity.getBean();
            IepDisability primaryDisability = null;

            int primaries = 0;
            int unflagged = 0;

            Collection<IepDisability> disabilities = m_disabilityMap.get(iep.getOid());

            if (disabilities != null && !disabilities.isEmpty()) {
                primaryDisability = disabilities.iterator().next();

                for (IepDisability disability : disabilities) {
                    if (disability.getPrimaryIndicator()) {
                        primaryDisability = disability;
                        primaries++;
                    } else {
                        unflagged++;
                    }
                }
            }

            String stateCode = null;
            if (primaryDisability == null) {
                entity.addRetrievalError(field.getFieldId(),
                        new StateReportValidationError(entity, field, "No disability",
                                "No disability information found"));
            } else {
                stateCode = lookupStateValue(IepDisability.class, IepDisability.COL_DISABILITY_CODE,
                        primaryDisability.getDisabilityCode());

                if (primaries > 1 || (unflagged > 1 && primaries == 0)) {
                    entity.addRetrievalError(field.getFieldId(),
                            new StateReportValidationError(entity, field, "Multipe Disabilities",
                                    "Unable to determine primary disability; using '"
                                            + primaryDisability.getDisabilityCode() + "' [" + stateCode + "]"));
                }
            }

            return stateCode;
        }
    }

    /**
     * Returns the school code to use for each student record. For outplaced students, this is the
     * outside placement location
     * pulled from the most recent enrollment record. Outplaced students are identified by being in
     * an Aspen school with the
     * outside placement flag set, or a school with a state code ending in "90" (per RIDE
     * conventions).
     *
     * @author mmastrangelo
     */
    protected class SchoolCodeRetriever implements FieldRetriever {

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
            IepData iep = (IepData) entity.getBean();
            Student student = iep.getStudent();
            School school = student.getSchool();

            String outsidePlacementSchoolFlag = (String) school.getFieldValueByAlias(ALIAS_OUTSIDE_PLACEMENT_SCHOOL);
            String stateSchoolId = (String) school.getFieldValueByBeanPath(m_sklIdField);

            String outsidePlacementCode = stateSchoolId;

            if ("1".equals(outsidePlacementSchoolFlag) || (stateSchoolId != null && stateSchoolId.endsWith("90"))) {
                StudentEnrollment enrollment = m_latestEntries.get(student.getOid());
                if (enrollment != null) {
                    String outsidePlacementUserCode =
                            (String) enrollment.getFieldValueByAlias(ALIAS_MEMBERSHIP_PLACEMENT_LOCATION_CODE);
                    outsidePlacementCode = lookupReferenceCodeByAlias(ALIAS_MEMBERSHIP_PLACEMENT_LOCATION_CODE,
                            outsidePlacementUserCode, ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                }
            }

            return outsidePlacementCode;
        }
    }

    /**
     * Retrieves "Y" if the IEP contains transportation services.
     *
     * risped-Transport
     *
     * @author Follett Software Company
     */
    protected class TransportationRetriever implements FieldRetriever {

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
            return Boolean.valueOf(m_iepsWithTransportation.contains(entity.getBean().getOid()));
        }
    }

    protected String m_aliasField;
    protected Map<String, Collection<IepDisability>> m_disabilityMap;
    protected Set<String> m_iepsWithTransportation;
    protected Map<String, StudentEnrollment> m_latestEntries;
    protected SystemStringConverter m_timestampConverter;
    protected boolean m_updStudents;

    private static final String ALIAS_COMPUCLAIM_EXPORT_DATE = "compuclaim-export-date";
    private static final String ALIAS_MEMBERSHIP_PLACEMENT_LOCATION_CODE = "OP Location Code";
    private static final String ALIAS_OUTSIDE_PLACEMENT_SCHOOL = "Outside Placement School";

    private static final String PARAM_UPD_STUDENTS = "updateStudents";

    /**
     * @see com.x2dev.procedures.statereporting.ri.RIStateReportData#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();

        initializeFields();

        m_timestampConverter = (SystemStringConverter) ConverterFactory.getConverterForClass(
                Converter.TIMESTAMP_CONVERTER, Locale.getDefault(), true);

        m_updStudents = ((Boolean) getParameter(PARAM_UPD_STUDENTS)).booleanValue();

        Criteria reportCriteria = getReportCriteria();

        loadLatestEntriesLookup(reportCriteria);
        loadDisabilityLookup(reportCriteria);
        loadTransportationLookup(reportCriteria);

        setQuery(new BeanQuery(IepData.class, reportCriteria));
        setEntityClass(CompuClaimEntity.class);

        addCustomCalcs();
    }

    /**
     * Add custom field retrievers.
     */
    private void addCustomCalcs() {
        HashMap<String, FieldRetriever> calcRetrievers = new HashMap<String, FieldRetriever>();

        calcRetrievers.put("risped-Disability", new PrimaryDisabilityRetriever());
        calcRetrievers.put("risped-Transport", new TransportationRetriever());
        calcRetrievers.put("risped-SchoolCode", new SchoolCodeRetriever());
        addCalcs(calcRetrievers);
    }

    /**
     * Returns the criteria for the IEPs to include.
     *
     * @return Criteria - active IEPs of students with Active Sped Status within the School Year
     */
    private Criteria getReportCriteria() {
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField field =
                dictionary.findDataDictionaryField(SisStudent.class.getName(), SisStudent.COL_SPED_STATUS_CODE);

        ArrayList<String> codesActive = new ArrayList<String>();
        if (field != null) {
            ReferenceTable refTable = field.getReferenceTable();
            if (refTable != null) {
                for (ReferenceCode code : refTable.getReferenceCodes()) {
                    if ("Y".equals(code.getStateCode())) {
                        codesActive.add(code.getCode());
                    }
                }
            }
        }

        X2Criteria reportCriteria = new X2Criteria();
        reportCriteria.addAndCriteria(
                StudentManager.getActiveStudentStatusCriteria(getOrganization(), Student.COL_ENROLLMENT_STATUS));

        if (getSchool() != null) {
            reportCriteria.addEqualTo(IepData.REL_STUDENT + ModelProperty.PATH_DELIMITER +
                    SisStudent.COL_SCHOOL_OID, getSchool().getOid());
        }

        // produce records for those students with an Active Sped Status
        if (codesActive.isEmpty()) {
            reportCriteria.addEqualTo(X2BaseBean.COL_OID, "__DUMMY__");
        } else {
            reportCriteria.addIn(IepData.REL_STUDENT + ModelProperty.PATH_DELIMITER +
                    SisStudent.COL_SPED_STATUS_CODE, codesActive);
        }
        // and Active IEP
        reportCriteria.addEqualTo(IepData.COL_STATUS_CODE, Integer.valueOf(IepData.StatusCode.ACTIVE.ordinal()));
        // within the School Year
        reportCriteria.addLessOrEqualThan(IepData.COL_START_DATE, getCurrentContext().getEndDate());
        X2Criteria orCriteria = new X2Criteria();
        X2Criteria criteria1 = new X2Criteria();
        criteria1.addGreaterOrEqualThan(IepData.COL_END_DATE, getCurrentContext().getStartDate());
        orCriteria.addOrCriteria(criteria1);
        X2Criteria criteria2 = new X2Criteria();
        criteria2.addEmpty(IepData.COL_END_DATE, getBroker().getPersistenceKey());
        orCriteria.addOrCriteria(criteria2);
        reportCriteria.addAndCriteria(orCriteria);

        return reportCriteria;
    }

    /**
     * Initialize fields.
     */
    private void initializeFields() {
        m_aliasField = translateAliasToJavaName(ALIAS_COMPUCLAIM_EXPORT_DATE, false);
    }

    /**
     * Loads the disability lookup map.
     *
     * @param reportCriteria Criteria
     */
    private void loadDisabilityLookup(Criteria reportCriteria) {
        Criteria disabilityCriteria = new Criteria();
        disabilityCriteria.addIn(IepDisability.COL_IEP_DATA_OID,
                new SubQuery(IepData.class, X2BaseBean.COL_OID, reportCriteria));

        BeanQuery disabilityQuery = new BeanQuery(IepDisability.class, disabilityCriteria);
        m_disabilityMap = getBroker().getGroupedCollectionByQuery(disabilityQuery, IepDisability.COL_IEP_DATA_OID, 100);
    }

    /**
     * Loads the enrollment lookup map - latest 'E' records on or prior to the report date.
     *
     * @param reportCriteria Criteria
     */
    private void loadLatestEntriesLookup(Criteria reportCriteria) {
        Criteria enrollmentCriteria = new Criteria();
        enrollmentCriteria.addEqualTo(StudentEnrollment.COL_ENROLLMENT_TYPE, StudentEnrollment.ENTRY);
        enrollmentCriteria.addLessOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, new Date());
        enrollmentCriteria.addIn(StudentEnrollment.COL_STUDENT_OID,
                new SubQuery(IepData.class, IepData.COL_STUDENT_OID, reportCriteria));

        BeanQuery enrollmentQuery = new BeanQuery(StudentEnrollment.class, enrollmentCriteria);
        enrollmentQuery.addOrderByAscending(StudentEnrollment.COL_ENROLLMENT_DATE); // ensures that
                                                                                    // the record
                                                                                    // with the max
                                                                                    // date will end
                                                                                    // up in the map

        m_latestEntries = getBroker().getMapByQuery(enrollmentQuery, StudentEnrollment.COL_STUDENT_OID, 100);
    }

    /**
     * Loads the set of IEP OIDs with transportation services.
     *
     * @param reportCriteria Criteria
     */
    private void loadTransportationLookup(Criteria reportCriteria) {
        m_iepsWithTransportation = new HashSet<String>(1000);

        Criteria serviceCriteria = new Criteria();
        serviceCriteria.addEqualTo(IepService.COL_SERVICE_TYPE, "Transportation");
        serviceCriteria.addEqualTo(IepService.COL_SERVICE_MODE, "Related Services");
        serviceCriteria.addIn(IepService.COL_IEP_DATA_OID,
                new SubQuery(IepData.class, X2BaseBean.COL_OID, reportCriteria));

        SubQuery serviceQuery = new SubQuery(IepService.class, IepService.COL_IEP_DATA_OID, serviceCriteria);
        m_iepsWithTransportation.addAll(getBroker().getSubQueryCollectionByQuery(serviceQuery));
    }
}
