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
package com.x2dev.procedures.statereporting.tn;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNStudentHistoryHelper;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentTransportation;
import com.x2dev.sis.model.beans.UserDefinedTableD;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Class for student transportation export.
 */
public class TNStudentTransportationData extends TNStateReportData {
    private static final String NO_PERFMON4J_INSTRUMENTATION = "";

    /**
     * Entity class for student transportation export.
     */
    public static class TNStudentTransportationEntity extends TNStateReportEntity {
        private List<TransInfo> m_actualTrans = null;
        private TNStudentTransportationData m_trnData;
        private SisStudent m_student;

        /**
         * Instantiates a new TN student transportation entity.
         */
        public TNStudentTransportationEntity() {}

        /**
         * Gets the actual transportation record.
         *
         * @return the m_actualTrans
         */
        public TransInfo getTransInfo() {
            return m_actualTrans.get(getCurrentRow());
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            SisStudent student = (SisStudent) getBean();
            return student.getNameView()
                    + " [LASID: " + student.getLocalId()
                    + ", SASID: " + student.getStateId()
                    + "] ";
        }

        /**
         * Intitialize.
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

            m_trnData = (TNStudentTransportationData) data;
            m_student = (SisStudent) bean;
            String stdOid = m_student.getOid();
            List<StudentRecordHelper> studentRecords = m_trnData.getStudentHelperMap().get(stdOid);
            List<StudentTransportation> transRecords = m_trnData.m_strMap.get(stdOid);

            m_actualTrans = new ArrayList();

            if (studentRecords != null && !studentRecords.isEmpty() && transRecords != null
                    && !transRecords.isEmpty()) {
                for (StudentRecordHelper studentRecord : studentRecords) {
                    for (StudentTransportation transRecord : transRecords) {
                        if (checkTransRecord(studentRecord, transRecord)) {
                            TransInfo info = m_trnData.new TransInfo(studentRecord, transRecord);
                            BigDecimal estMiles = info.getEstMiles();
                            if (estMiles != null && estMiles.compareTo(BigDecimal.ZERO) > 0) {
                                m_actualTrans.add(info);
                            }
                        }
                    }
                }

                setRowCount(m_actualTrans.size());

                m_trnData.addEntityRowsCount(getRowCount());

            } else {
                setRowCount(0);
            }
        }

        /**
         * Check trans record.
         *
         * @param studentRecord StudentRecordHelper
         * @param transRecord StudentTransportation
         * @return true, if successful
         */
        private boolean checkTransRecord(StudentRecordHelper studentRecord, StudentTransportation transRecord) {
            boolean value = false;
            String studentSchoolId = studentRecord.getSchoolId();
            String transSchoolId = transRecord.getSchool() == null ? null
                    : (String) transRecord.getSchool().getFieldValueByAlias(ALIAS_STATE_SCHOOL_ID);
            if (!StringUtils.isEmpty(transSchoolId) && !StringUtils.isEmpty(studentSchoolId)
                    && transSchoolId.equals(studentSchoolId)
                    && (studentRecord.getExitDate() == null
                            || transRecord.getStartDate().before(studentRecord.getExitDate()))
                    && (transRecord.getEndDate() == null
                            || transRecord.getEndDate().after(studentRecord.getEnrollDate()))) {
                value = true;
            }
            String amBus = (String) transRecord.getFieldValueByBeanPath(m_trnData.m_fieldAmBus);
            String pmBus = (String) transRecord.getFieldValueByBeanPath(m_trnData.m_fieldPmBus);

            // If export run from tab Bus apply input options of tab Bus to this export.
            if (m_trnData.m_fromTabBus && !m_trnData.m_allBuses.booleanValue() && value) {
                String busStateCodeAm =
                        m_trnData.lookupReferenceCodeByBeanPath(StudentTransportation.class, m_trnData.m_fieldAmBus,
                                amBus, ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                String busStateCodePm =
                        m_trnData.lookupReferenceCodeByBeanPath(StudentTransportation.class, m_trnData.m_fieldPmBus,
                                pmBus, ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());

                if (!m_trnData.getSelectedBuses().contains(busStateCodeAm)
                        && !m_trnData.getSelectedBuses().contains(busStateCodePm)) {
                    value = false;
                }
            }

            if (!m_trnData.m_buses.keySet().contains(amBus) && !m_trnData.m_buses.keySet().contains(pmBus) &&
                    (!StringUtils.isEmpty(amBus) && !StringUtils.isEmpty(pmBus))) {
                value = false;
            }
            return value;
        }

    }

    /**
     * Field retriever for RecordType, Instr program and SchoolDate fields.
     */
    protected class FieldRetrieverOther implements FieldRetriever {
        protected static final String STR_CALC_ID = "STR_CALC_OTHER";
        protected static final String CALC_PARAM_AMBUS = "STS_AM_BUS";
        protected static final String CALC_PARAM_AM_BEGIN_DATE = "STR_AM_BEGIN_DATE";
        protected static final String CALC_PARAM_AM_END_DATE = "STR_AM_END_DATE";
        protected static final String CALC_PARAM_PM_BEGIN_DATE = "STR_PM_BEGIN_DATE";
        protected static final String CALC_PARAM_PM_END_DATE = "STR_PM_END_DATE";

        protected static final String CALC_PARAM_ESTMILES = "STR_EST_MILES";
        protected static final String CALC_PARAM_PMBUS = "STS_PM_BUS";
        protected static final String CALC_PARAM_SCHOOL = "STR_SCHOOL";
        protected static final String CALC_PARAM_SCHOOLYEAR = "STR_SCHOOLYEAR";

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
            TNStudentTransportationEntity seEntity = (TNStudentTransportationEntity) entity;
            TNStudentTransportationData seData = (TNStudentTransportationData) data;
            TransInfo info = seEntity.getTransInfo();
            String param = (String) field.getParameter();
            Object value = null;
            if (FieldRetrieverOther.CALC_PARAM_SCHOOLYEAR.equalsIgnoreCase(param)) {
                value = seData.m_schoolYear;
            } else if (FieldRetrieverOther.CALC_PARAM_SCHOOL.equalsIgnoreCase(param)) {
                value = info.getSchoolId();
            } else if (FieldRetrieverOther.CALC_PARAM_ESTMILES.equalsIgnoreCase(param)) {
                value = info.getEstMiles();
            } else if (FieldRetrieverOther.CALC_PARAM_AMBUS.equalsIgnoreCase(param)) {
                String amBus = (String) info.getStudentTransport().getFieldValueByBeanPath(seData.m_fieldAmBus);
                if (seData.m_buses.keySet().contains(amBus)) {
                    value = seData.lookupReferenceCodeByBeanPath(StudentTransportation.class, seData.m_fieldAmBus,
                            amBus, ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                }
            } else if (FieldRetrieverOther.CALC_PARAM_PMBUS.equalsIgnoreCase(param)) {
                String pmBus = (String) info.getStudentTransport().getFieldValueByBeanPath(seData.m_fieldPmBus);
                if (seData.m_buses.keySet().contains(pmBus)) {
                    value = seData.lookupReferenceCodeByBeanPath(StudentTransportation.class, seData.m_fieldPmBus,
                            pmBus, ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                }
            } else if (FieldRetrieverOther.CALC_PARAM_AM_BEGIN_DATE.equalsIgnoreCase(param)) {
                value = !StringUtils.isEmpty(entity.getFieldValue(EXPORT_FIELD_AM_NUM)) ? info.getBeginDate() : null;
            } else if (FieldRetrieverOther.CALC_PARAM_AM_END_DATE.equalsIgnoreCase(param)) {
                value = !StringUtils.isEmpty(entity.getFieldValue(EXPORT_FIELD_AM_NUM)) ? info.getEndDate() : null;
            } else if (FieldRetrieverOther.CALC_PARAM_PM_BEGIN_DATE.equalsIgnoreCase(param)) {
                value = !StringUtils.isEmpty(entity.getFieldValue(EXPORT_FIELD_PM_NUM)) ? info.getBeginDate() : null;
            } else if (FieldRetrieverOther.CALC_PARAM_PM_END_DATE.equalsIgnoreCase(param)) {
                value = !StringUtils.isEmpty(entity.getFieldValue(EXPORT_FIELD_PM_NUM)) ? info.getEndDate() : null;
            }
            return value;
        }
    }

    /**
     *
     * Field retriever for SSN.
     * This retriever normalize SSN value to format 999999999 (nine digits without any other
     * characters)
     *
     */
    protected class FieldRetrieverSSN implements FieldRetriever {
        protected static final String STR_CALC_ID = "STR_CALC_SSN";

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
            TNStudentTransportationEntity seEntity = (TNStudentTransportationEntity) entity;

            SisStudent student = (SisStudent) seEntity.getBean();

            if (student == null) {
                return "";
            }

            Person psn = student.getPerson();

            if (psn == null) {
                return "";
            }

            String ssn = psn.getPersonId();
            if (StringUtils.isEmpty(ssn)) {
                return "";
            }

            return ssn.replaceAll("([^\\d]?)", "");
        }

    }

    /**
     * The Class FieldValidatorBus.
     */
    protected class FieldValidatorBus implements FieldValidator {
        protected static final String STR_VAL_ID = "STR_VAL_BUS";

        private static final String FIELD_NAME_AM_BUS_NUMBER = "AM BUS NUMBER";

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            TNStudentTransportationEntity seEntity = (TNStudentTransportationEntity) entity;
            TNStudentTransportationData seData = (TNStudentTransportationData) data;
            TransInfo transInfo = seEntity.getTransInfo();
            String param = (String) field.getParameter();
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            if (FieldRetrieverOther.CALC_PARAM_AMBUS.equalsIgnoreCase(param)
                    && transInfo.getStudentTransport() != null) {
                String amBus = (String) transInfo.getStudentTransport().getFieldValueByBeanPath(seData.m_fieldAmBus);
                checkBusIsOut(errors, amBus, transInfo, field, seEntity, seData);
            } else if (FieldRetrieverOther.CALC_PARAM_PMBUS.equalsIgnoreCase(param)
                    && transInfo.getStudentTransport() != null) {
                String pmBus = (String) transInfo.getStudentTransport().getFieldValueByBeanPath(seData.m_fieldPmBus);
                checkBusIsOut(errors, pmBus, transInfo, field, seEntity, seData);
                // Add validation error if both AM and PM buses are empty
                if (StringUtils.isEmpty(pmBus)
                        && StringUtils.isEmpty(seEntity.getFieldValue(FIELD_NAME_AM_BUS_NUMBER))) {
                    errors.add(new StateReportValidationError(seEntity, field,
                            "AM and PM are empty in student transportation.",
                            "Student transportation should contain AM or/and PM bus number."));
                }
            }
            return errors;
        }

        /**
         * Adds retrieval error if bus is out of service for student transportation record.
         *
         * @param errors
         *
         * @param busNum String
         * @param transInfo StudentTransportation
         * @param field FieldDefinition
         * @param seEntity TNStudentTransportationEntity
         * @param seData TNStudentTransportationData
         */
        private void checkBusIsOut(Collection<StateReportValidationError> errors,
                                   String busNum,
                                   TransInfo transInfo,
                                   FieldDefinition field,
                                   TNStudentTransportationEntity seEntity,
                                   TNStudentTransportationData seData) {
            UserDefinedTableD bus = seData.m_buses.get(busNum);
            if (bus != null) {
                Pair<Date, Date> busDates = seData.getServiceDates(bus);
                Pair<Date, Date> strDates =
                        Pair.of(transInfo.getBeginDate(), transInfo.getEndDate());
                if (!rangeIncludesRange(busDates, strDates)) {
                    errors.add(new StateReportValidationError(seEntity, field,
                            "Bus is out of service for student transportation record.",
                            "Bus code " + busNum + ". "
                                    + ("\r\nService dates: "
                                            + (busDates.getLeft() != null ? m_format.format(busDates.getLeft()) : ""))
                                    + " - " + (busDates.getRight() != null ? m_format.format(busDates.getRight()) : "")
                                    +
                                    ("\r\nTransportation dates: "
                                            + (strDates.getLeft() != null ? m_format.format(strDates.getLeft()) : ""))
                                    + " - " +
                                    (strDates.getRight() != null ? m_format.format(strDates.getRight()) : "")));
                }
            } else {
                errors.add(new StateReportValidationError(seEntity, field, "Invalid bus code.",
                        "Bus code " + busNum + " is not found."));
            }
        }

        /**
         * Returns true if range1 includes range2, otherwise false.
         *
         * @param range1 Pair<Date,Date>
         * @param range2 Pair<Date,Date>
         * @return boolean
         */
        private boolean rangeIncludesRange(Pair<Date, Date> range1, Pair<Date, Date> range2) {
            boolean afterOrOnStartDate = false;
            if (range1.getLeft() == null || (range2.getLeft() != null && !range2.getLeft().before(range1.getLeft()))) {
                afterOrOnStartDate = true;
            }
            boolean beforeOrOnEndDate = false;
            if (range1.getRight() == null
                    || (range2.getRight() != null && !range2.getRight().after(range1.getRight()))) {
                beforeOrOnEndDate = true;
            }

            return afterOrOnStartDate && beforeOrOnEndDate;
        }
    }

    /**
     * Validates SSN against non valid formats
     * Valid ssn format: 999999999.
     */
    protected class FieldValidatorSSN implements FieldValidator {
        protected static final String STR_VAL_ID = "STR_VAL_SSN";
        private static final String patternSSN = "^[0-9]{9}$|^$";

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            if (value != null && !value.matches(patternSSN)) {
                errors.add(new StateReportValidationError(entity, field, "Invalid format",
                        "SSN value must be 9 digits long"));
            }

            return errors;
        }

    }

    /**
     * The Class TransInfo.
     */
    public class TransInfo {
        private StudentRecordHelper m_studentRecord;
        private StudentTransportation m_transRecord;

        /**
         * Instantiates a new trans info.
         *
         * @param studentRecord StudentRecordHelper
         * @param transRecord StudentTransportation
         */
        public TransInfo(StudentRecordHelper studentRecord, StudentTransportation transRecord) {
            m_studentRecord = studentRecord;
            m_transRecord = transRecord;
        }

        /**
         * Gets the begin date.
         *
         * @return Plain date
         */
        public PlainDate getBeginDate() {
            PlainDate date = m_studentRecord.getEnrollDate();
            if (m_transRecord.getStartDate() != null && m_transRecord.getStartDate().after(date)) {
                date = m_transRecord.getStartDate();
            }
            return date;
        }

        public BigDecimal getEstMiles() throws X2BaseException {
            Object value = TNStudentTransportationData.this.getPropertyAsJavaType(m_transRecord, m_fieldEstMiles);
            if (value != null && value instanceof BigDecimal) {
                return ((BigDecimal) value).multiply(BIG_DECIMAL_100);
            }
            return null;
        }

        /**
         * Gets the end date.
         *
         * @return Plain date
         */
        public PlainDate getEndDate() {
            PlainDate date = m_studentRecord.getExitDate();
            if ((date == null && m_transRecord.getEndDate() != null) ||
                    (date != null && m_transRecord.getEndDate() != null && date.after(m_transRecord.getEndDate()))) {
                Calendar cal = Calendar.getInstance();
                cal.setTime(m_transRecord.getEndDate());
                cal.add(Calendar.DAY_OF_YEAR, -1);
                date = new PlainDate(cal.getTime());
            }
            return date;
        }

        /**
         * Gets the school id.
         *
         * @return String
         */
        public String getSchoolId() {
            return m_studentRecord.getSchoolId();
        }

        /**
         * Gets the student transport.
         *
         * @return Student transportation
         */
        public StudentTransportation getStudentTransport() {
            return m_transRecord;
        }

    }

    /**
     * Constants: Aliases, Fields, IDs, Parameters
     */
    protected static final String ALIAS_AM_BUS = "DOE AM BUS";
    protected static final String ALIAS_BUS_NUM = "DOE BUS NUM";
    protected static final String ALIAS_BUS_SERVICE_BEGIN_DATE = "all-rcd-BusServiceBeginDate";
    protected static final String ALIAS_BUS_SERVICE_END_DATE = "all-rcd-BusServiceEndDate";
    protected static final String ALIAS_EST_MILES = "DOE EST MILES TRANSPORTED";
    protected static final String ALIAS_EXCLUDE_STUDENT = "DOE EXCLUDE STD";
    protected static final String ALIAS_PM_BUS = "DOE PM BUS";

    protected static final BigDecimal BIG_DECIMAL_100 = new BigDecimal(100);

    // delete!

    private static final String DATE_FORMAT_SERVICE = "yyyy-MM-dd";
    // delete!

    private static final String EXTENDED_DICTIONARY_ID_BUS_RCD = "REF-BUS";
    private static final String EXTENDED_DICTIONARY_ID_BUS_UDD = "TN-UDD-BUS";
    private static final String EXPORT_FIELD_AM_NUM = "AM BUS NUMBER";
    private static final String EXPORT_FIELD_PM_NUM = "PM BUS NUMBER";
    /**
     * Other constants
     */
    protected static final String FIELD_ID_MILES_TRNSPRT = "EST MILES TRNSPRT";
    protected static final String PARAM_ALL_BUSES = "allBuses"; // NOTE: used by TN-RESTAGING. Do
                                                                // not delete!
    protected static final String PARAM_BUSES = "buses"; // NOTE: used by TN-RESTAGING. Do not
                                                         // delete!
    protected static final String PARAM_FROM_TAB = "fromTab"; // NOTE: used by TN-RESTAGING. Do not
                                                              // delete!

    protected static final String TAB_BUS = "tabStdBus"; // NOTE: used by TN-RESTAGING. Do not
                                                         // delete!

    protected static final String ZERO_STRING = "0";

    protected Map<String, UserDefinedTableD> m_buses = new HashMap<String, UserDefinedTableD>();
    protected PlainDate m_contextEndDate;
    protected PlainDate m_contextStartDate;
    protected String m_fieldAmBus;
    protected String m_fieldEstMiles;
    protected String m_fieldExcludeStudent;
    protected String m_fieldInstructionalProgram;
    protected String m_fieldPmBus;

    protected Boolean m_allBuses; // used by TN Restaging Procedure
    // Used by TN Restaging Procedure to determine if export was run from tab Bus.
    protected boolean m_fromTabBus;
    /**
     * A map of reference codes for instructional programs, for use in the instrpgm retriever.
     */
    protected SimpleDateFormat m_format = new SimpleDateFormat(DATE_FORMAT_SERVICE);
    protected HashMap<String, String> m_instructionalPrograms;
    protected String m_schoolYear;
    protected TNStudentHistoryHelper m_studentHelper;
    protected Map<String, List<StudentTransportation>> m_strMap;

    private Map<String, String> m_busBeanPaths = new HashMap<>();
    private Set<String> m_selectedBuses; // used by TN Restaging Procedure
    private X2Criteria m_transportationCriteria;

    /**
     * Initialize the data module.
     * Initialize necessary working resources.
     * Define query for students to load.
     * Define list of field definitions for the export.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(java.util.Map,
     *      com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    public void initialize() throws X2BaseException {
        super.initialize();

        initializeFields();

        initBuses();

        getCalendarsForContextOid(m_contextOid);

        if (getSetupErrors().size() != 0) {
            return;
        }

        if (getCurrentContext().getSchoolYear() > 2017) {
            setExportVersion(2);
        }

        initializeCriteriaAndQuery();

        QueryByCriteria transporationQuery = new QueryByCriteria(StudentTransportation.class, getSTRCriteria());
        m_strMap =
                getBroker().getGroupedCollectionByQuery(transporationQuery, StudentTransportation.COL_STUDENT_OID, 100);

        registerFieldRetrievers();
        registerFieldValidators();
    }

    /**
     * Used by TN Restaging Procedure to determine selected on tab Bus numbers of buses.
     *
     * @return Collection
     */
    protected Collection<String> getSelectedBuses() {
        if (m_selectedBuses == null) {
            Collection<String> codeOids = new ArrayList<String>();
            String selectedBusesOids = (String) getParameter(PARAM_BUSES);
            splitPicklistBeanOids(codeOids, selectedBusesOids);

            X2Criteria criteria = new X2Criteria();
            criteria.addIn(X2BaseBean.COL_OID, codeOids);
            QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, criteria);
            Collection<ReferenceCode> codes = getBroker().getCollectionByQuery(query);

            m_selectedBuses = new HashSet<String>();
            for (ReferenceCode code : codes) {
                if (!StringUtils.isEmpty(code.getStateCode())) {
                    m_selectedBuses.add(code.getStateCode());
                }
            }
        }

        return m_selectedBuses;
    }

    /**
     * Return service dates.
     *
     * @param busCode ReferenceCode
     * @return Pair
     */
    protected Pair<Date, Date> getServiceDates(UserDefinedTableD bus) {
        String startDateDB = (String) bus.getFieldValueByBeanPath(m_busBeanPaths.get(ALIAS_BUS_SERVICE_BEGIN_DATE));
        String endDateDB = (String) bus.getFieldValueByBeanPath(m_busBeanPaths.get(ALIAS_BUS_SERVICE_END_DATE));
        Date serviceStartDate = null;
        Date serviceEndDate = null;
        try {
            if (!StringUtils.isEmpty(startDateDB)) {
                serviceStartDate = m_format.parse(startDateDB);
            }
            if (!StringUtils.isEmpty(endDateDB)) {
                serviceEndDate = m_format.parse(endDateDB);
            }
        } catch (ParseException e) {
            e.printStackTrace();
        }
        Pair<Date, Date> dates = Pair.of(serviceStartDate, serviceEndDate);

        return dates;
    }

    /**
     * Returns true if dates of bus service overlap with dates of school year, otherwise false.
     *
     * @param code ReferenceCode
     * @return boolean
     */
    private boolean datesOverlapWithYear(UserDefinedTableD bus) {
        boolean datesOverlapWithYear = false;

        Pair<Date, Date> serviceDates = getServiceDates(bus);

        PlainDate contextStartDate = getCurrentContext().getStartDate();
        PlainDate contextEndDate = getCurrentContext().getEndDate();

        if (rangesCorrectAndOverlap(serviceDates.getLeft(), serviceDates.getRight(), contextStartDate,
                contextEndDate)) {
            datesOverlapWithYear = true;
        }

        return datesOverlapWithYear;
    }

    /**
     * Method for implementing business rule for schoolYear.
     *
     * @return string representation of school year = (CTX_SCHOOL_YEAR - 1)
     */
    private String getCurentSchoolYear() {
        return Integer.toString(getCurrentContext().getSchoolYear() - 1);
    }

    /**
     * Function for building custom Student criteria.
     *
     * @return criteria for query for list of active students
     *         limited by reportDate, school and not excluded students
     */
    private X2Criteria getSTRCriteria() {
        if (m_transportationCriteria == null) {
            X2Criteria strCriteria = new X2Criteria();
            strCriteria.addNotNull(StudentTransportation.COL_START_DATE);
            strCriteria.addLessOrEqualThan(StudentTransportation.COL_START_DATE, m_contextEndDate);

            X2Criteria endDateCriteria = new X2Criteria();
            endDateCriteria.addIsNull(StudentTransportation.COL_END_DATE);
            X2Criteria orEndDateCriteria = new X2Criteria();
            orEndDateCriteria.addGreaterOrEqualThan(StudentTransportation.COL_END_DATE, m_contextStartDate);
            endDateCriteria.addOrCriteria(orEndDateCriteria);

            X2Criteria contextCriteria = new X2Criteria();
            contextCriteria.addEqualTo(StudentTransportation.COL_DISTRICT_CONTEXT_OID, m_contextOid);

            strCriteria.addAndCriteria(endDateCriteria);
            strCriteria.addOrCriteria(contextCriteria);

            m_transportationCriteria = strCriteria;
        }
        return m_transportationCriteria;
    }

    /**
     * Load paths to bus properties by aliases
     *
     * @param extendedDictionary
     */
    private void initBusBeanPaths(ExtendedDataDictionary extendedDictionary) {
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(
                extendedDictionary, getBroker().getPersistenceKey());

        for (String alias : Arrays.asList(ALIAS_BUS_NUM, ALIAS_BUS_SERVICE_BEGIN_DATE,
                ALIAS_BUS_SERVICE_END_DATE)) {
            DataDictionaryField dataField = dictionary.findDataDictionaryFieldByAlias(alias);
            if (dataField != null) {
                m_busBeanPaths.put(alias, dataField.getJavaName());
            }
        }
    }

    /**
     * Inits the buses.
     */
    private void initBuses() {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ExtendedDataDictionary.COL_ID, EXTENDED_DICTIONARY_ID_BUS_UDD);
        ExtendedDataDictionary extendedDictionary = getBroker()
                .getBeanByQuery(new QueryByCriteria(ExtendedDataDictionary.class, criteria));

        initBusBeanPaths(extendedDictionary);

        X2Criteria uddCriteria = new X2Criteria();
        uddCriteria.addEqualTo(UserDefinedTableD.COL_EXTENDED_DATA_DICTIONARY_OID, extendedDictionary.getOid());
        QueryByCriteria uddQuery = new QueryByCriteria(UserDefinedTableD.class, uddCriteria);
        for (Object item : getBroker().getCollectionByQuery(uddQuery)) {
            UserDefinedTableD bus = (UserDefinedTableD) item;
            String busNum = (String) bus.getFieldValueByBeanPath(m_busBeanPaths.get(ALIAS_BUS_NUM));
            if (datesOverlapWithYear(bus) && !StringUtils.isEmpty(busNum)) {
                m_buses.put(busNum, bus);
            }
        }

        initBusesFromReferenceCodes();
    }

    /**
     * If state code != code for buses stored in ReferenceCodes, we need to add them to the bus map
     * as well
     */
    private void initBusesFromReferenceCodes() {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ExtendedDataDictionary.COL_ID, EXTENDED_DICTIONARY_ID_BUS_RCD);
        ExtendedDataDictionary extendedDictionary = getBroker()
                .getBeanByQuery(new QueryByCriteria(ExtendedDataDictionary.class, criteria));

        for (ReferenceCode rcd : extendedDictionary.getReferenceCodes()) {
            String code = rcd.getCode();
            String statCode = rcd.getStateCode();
            if (!code.equals(statCode) && m_buses.containsKey(statCode)) {
                m_buses.put(code, m_buses.get(statCode));
            }
        }
    }

    /**
     * Initialize criteria and query.
     */
    private void initializeCriteriaAndQuery() {
        SubQuery subQuery =
                new SubQuery(StudentTransportation.class, StudentTransportation.COL_STUDENT_OID, getSTRCriteria());

        m_studentHelper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);

        X2Criteria studentCriteria = m_studentHelper.getStudentCriteria();
        studentCriteria.addIn(X2BaseBean.COL_OID, subQuery);

        QueryByCriteria query = new QueryByCriteria(Student.class, studentCriteria);
        applyInputSort(query, null);
        setQuery(query);

        setEntityClass(TNStudentTransportationEntity.class);

        initStudentHelperMap(m_studentHelper, query);

    }

    /**
     * Lookup field aliases and paths
     * Get data from input definition.
     */
    private void initializeFields() {
        if (getParameter(PARAM_REPORT_DATE) != null) {
            m_reportDate = (PlainDate) getParameter(PARAM_REPORT_DATE);
        } else {
            m_reportDate = new PlainDate(OrganizationManager.getTimeZone(getOrganization()));
        }
        m_fieldExcludeStudent = translateAliasToJavaName(ALIAS_EXCLUDE_STUDENT, true);
        m_fieldInstructionalProgram = translateAliasToJavaName(ALIAS_INSTR_PGM, true);
        m_fieldEstMiles = translateAliasToJavaName(ALIAS_EST_MILES, true);
        m_fieldAmBus = translateAliasToJavaName(ALIAS_AM_BUS, true);
        m_fieldPmBus = translateAliasToJavaName(ALIAS_PM_BUS, true);

        m_fromTabBus = TAB_BUS.equals(getParameter(PARAM_FROM_TAB));
        m_allBuses = (Boolean) getParameter(PARAM_ALL_BUSES);

        TNEnrollmentHelper helper = new TNEnrollmentHelper(this);
        m_studentHelper = helper.getStudentHistoryHelper();

        m_contextStartDate = getCurrentContext().getStartDate();
        m_contextEndDate = getCurrentContext().getEndDate();

        m_schoolYear = getCurentSchoolYear();
    }

    /**
     * Return true if left date pair overlaps with right date pair.
     * Null is considering as unlimited past/future.
     *
     * @param leftStartDate Date
     * @param leftEndDate Date
     * @param rightStartDate Date
     * @param rightEndDate Date
     * @return boolean
     */
    private boolean rangesCorrectAndOverlap(Date leftStartDate,
                                            Date leftEndDate,
                                            Date rightStartDate,
                                            Date rightEndDate) {
        boolean rangesOverlap = false;

        boolean correctDates = ((leftStartDate == null || leftEndDate == null) || !leftStartDate.after(leftEndDate)) &&
                ((rightStartDate == null || rightEndDate == null) || !rightStartDate.after(rightEndDate));

        // Both ranges have unlimited past
        if (correctDates &&
                ((leftStartDate == null && rightStartDate == null) ||
                // both ranges have unlimited future
                        (leftEndDate == null && rightEndDate == null) ||
                        // left range has unlimited past and future
                        (leftStartDate == null && leftEndDate == null) ||
                        // right range has unlimited past and future
                        (rightStartDate == null && rightEndDate == null) ||

                        // to this point if range has unlimited past (null), it has limited future
                        // and second range has limited past (not nulls),
                        // and if range has unlimited future (null), it has limited past and second
                        // range has limited future (not nulls), so
                        // we can check if ranges are overlap
                        (leftEndDate == null && !leftStartDate.after(rightEndDate)) ||
                        (rightEndDate == null && !rightStartDate.after(leftEndDate)) ||
                        (leftStartDate == null && !leftEndDate.before(rightStartDate)) ||
                        (rightStartDate == null && !rightEndDate.before(leftStartDate)) ||

                        (leftStartDate != null && leftEndDate != null &&
                                rightStartDate != null && rightEndDate != null &&
                                !leftStartDate.after(rightEndDate) && !leftEndDate.before(rightStartDate)))) {
            rangesOverlap = true;
        }

        return rangesOverlap;
    }

    /**
     * Register custom field retrievers.
     */
    private void registerFieldRetrievers() {
        HashMap<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
        calcs.put(FieldRetrieverOther.STR_CALC_ID, new FieldRetrieverOther());
        calcs.put(FieldRetrieverSSN.STR_CALC_ID, new FieldRetrieverSSN());
        calcs.put(RetrieveInstProgramStdBean.TN_CALC_INSTPGM_ID, new RetrieveInstProgramStdBean());
        super.addCalcs(calcs);
    }

    /**
     * Register field validators.
     */
    private void registerFieldValidators() {
        HashMap<String, FieldValidator> validators = new HashMap<String, FieldValidator>();
        validators.put(FieldValidatorBus.STR_VAL_ID, new FieldValidatorBus());
        validators.put(FieldValidatorSSN.STR_VAL_ID, new FieldValidatorSSN());
        super.addValidators(validators);
    }

    /**
     * Splits parameter from picklist to initialize collection of Oids of selected entities.
     *
     * @param collectionOids - collection that will be splitted.
     * @param oidsString - parameter of multiselect picklist for splitting.
     */
    private void splitPicklistBeanOids(Collection<String> collectionOids, String oidsString) {
        if (!StringUtils.isEmpty(oidsString) && collectionOids != null) {
            String[] oids = oidsString.split(",");
            collectionOids.addAll(Arrays.asList(oids));
        }
    }
}
