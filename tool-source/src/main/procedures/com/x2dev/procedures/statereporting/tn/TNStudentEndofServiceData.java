/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2006 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.procedures.statereporting.tn;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatField.ReferenceMapTypeCode;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.SchoolCalendar;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNMultiYearHelper.Strategy;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNStudentEnrollmentSpan;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNStudentHistoryHelper;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNStudentMultiYearHelper;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisSchoolCalendarDate;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.business.CalendarManager;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Class for student extract export.
 */
public class TNStudentEndofServiceData extends TNStateReportData {
    private static final String NO_PERFMON4J_INSTRUMENTATION = ""; 

    /**
     * Helper class to store information about reported row .
     */
    class EndOfServiceRow {
        private String m_endOfServiceAction = null;
        private PlainDate m_endOfServiceDate = null;

        /**
         * Instantiates a new end of service row.
         *
         * @param action String
         * @param date PlainDate
         */
        public EndOfServiceRow(String action, PlainDate date) {
            m_endOfServiceAction = action;
            m_endOfServiceDate = date;
        }

        /**
         * Gets the end of service action.
         *
         * @return the m_endOfServiceAction
         */
        public String getEndOfServiceAction() {
            return m_endOfServiceAction;
        }

        /**
         * Gets the end of service date.
         *
         * @return the m_endOfServiceDate
         */
        public PlainDate getEndOfServiceDate() {
            return m_endOfServiceDate;
        }
    }

    /**
     * Entity class for student extract export.
     *
     */
    public static class TNStudentEndofServiceEntity extends TNStateReportEntity {
        private ArrayList<EndOfServiceRow> m_rows = new ArrayList<EndOfServiceRow>();
        TNStudentEndofServiceData m_tnData = null;

        /**
         * Instantiates a new TN student endof service entity.
         */
        public TNStudentEndofServiceEntity() {
            // Public no argument constructor for dynamic instantiation.
        }

        /**
         * Gets the current data.
         *
         * @return End of service row
         */
        public EndOfServiceRow getCurrentData() {
            return m_rows.get(getCurrentRow());
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
            String name = student.getNameView() +
                    " [LASID: " + student.getLocalId() +
                    ", SASID: " + student.getStateId() +
                    "] ";

            return name;
        }

        /**
         * Gets the field value by bean path.
         *
         * @param student SisStudent
         * @param beanPath String
         * @return Object
         * @see TNEnrollmentHelper#getStudentValueByBeanPath(com.follett.fsc.core.k12.beans.Student,
         *      String)
         */
        public Object getFieldValueByBeanPath(SisStudent student, String beanPath) {
            return m_tnData.m_enrHelper.getStudentValueByBeanPath(student, beanPath);
        }

        /**
         * Gets the student property as java type.
         *
         * @param student SisStudent
         * @param beanPath String
         * @return Object
         * @see TNEnrollmentHelper#getStudentPropertyAsJavaType(SisStudent, String)
         */
        public Object getStudentPropertyAsJavaType(SisStudent student, String beanPath) {
            return m_tnData.m_enrHelper.getStudentPropertyAsJavaType(student, beanPath);
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
            m_tnData = (TNStudentEndofServiceData) data;
            SisStudent student = (SisStudent) getBean();

            // Should include records that have a date within the school year context in the
            // following two fields
            PlainDate endOfServiceActDate =
                    (PlainDate) getStudentPropertyAsJavaType(student, m_tnData.m_fieldEndOfServiceActDate);
            if (endOfServiceActDate != null && !endOfServiceActDate.after(m_tnData.getCurrentContext().getEndDate())
                    && !endOfServiceActDate.before(m_tnData.getCurrentContext().getStartDate())) {
                TNStudentEnrollmentSpan actualSpan = null;
                List<TNStudentEnrollmentSpan> spans =
                        m_tnData.m_studentHelper.getTNStudentEnrollmentSpans(student, true);
                for (TNStudentEnrollmentSpan span : spans) {
                    if (getSchool(student).equals(span.getSchool()) &&
                            span.getFirstActiveEnrollment() != null &&
                            span.getFirstActiveEnrollment().getEnrollmentDate() != null) {
                        if (actualSpan == null ||
                                actualSpan.getFirstActiveEnrollment().getEnrollmentDate()
                                        .before(span.getFirstActiveEnrollment().getEnrollmentDate())) {
                            actualSpan = span;
                        }
                    }
                }

                if (actualSpan != null && actualSpan.getFirstActiveEnrollment() != null) {
                    String instServiceType = (String) actualSpan.getFirstActiveEnrollment()
                            .getFieldValueByBeanPath(m_tnData.m_fieldInstrServiceType);
                    String code = data.lookupReferenceCodeByBeanPath(StudentEnrollment.class,
                            m_tnData.m_fieldInstrServiceType, instServiceType, ReferenceMapTypeCode.STATE.ordinal());

                    if (StringUtils.isEmpty(instServiceType) || INSTR_SERVICE_PRIMARY.equals(code)) {
                        String servAction = (String) getFieldValueByBeanPath(student, m_tnData.m_fieldEndOfServiceAct);
                        String servActioncode = data.lookupReferenceCodeByBeanPath(SisStudent.class,
                                m_tnData.m_fieldEndOfServiceAct, servAction, ReferenceMapTypeCode.STATE.ordinal());

                        EndOfServiceRow row = m_tnData.new EndOfServiceRow(servActioncode, endOfServiceActDate);
                        m_rows.add(row);
                    }
                }
            }

            Collection<StudentEnrollment> additionalEnrs = m_tnData.m_qualEnrsMap.get(student.getOid());

            if (additionalEnrs != null) {
                for (StudentEnrollment enr : additionalEnrs) {
                    PlainDate eosDate = null;
                    PlainDate enrDate = enr.getEnrollmentDate();

                    List<PlainDate> inSessionDates = new ArrayList<PlainDate>(CalendarManager.getInSessionDates(
                            m_tnData.getCurrentContext().getStartDate(), enrDate, student, m_tnData.getBroker()));
                    int index = inSessionDates.indexOf(enr.getEnrollmentDate()) - 1;

                    if (index > -1) {
                        eosDate = inSessionDates.get(index);
                    } else {
                        eosDate = enrDate;
                    }
                    String servActioncode = m_tnData.m_qualCodesMap.get(enr.getReasonCode()).getStateCode();
                    EndOfServiceRow row = m_tnData.new EndOfServiceRow(servActioncode, eosDate);
                    m_rows.add(row);
                }
            }

            setRowCount(m_rows.size());
            m_tnData.addEntityRowsCount(getRowCount());
        }

        /**
         * Gets the school.
         *
         * @param student SisStudent
         * @return Sis school
         */
        private SisSchool getSchool(SisStudent student) {
            return m_tnData.getSchool(student);
        }


    }

    /**
     *
     * Field retriever for SSN.
     * This retriever normalizes SSN value to format 999999999 (nine digits without any other
     * characters)
     *
     */
    protected class FieldRetrieverSSN implements FieldRetriever {
        protected static final String STDE_CALC_ID = "STDE_CALC_SSN";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            TNStudentEndofServiceEntity seEntity = (TNStudentEndofServiceEntity) entity;
            SisStudent student = (SisStudent) seEntity.getBean();
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
     * Retrieve can change fields path depend on context<br>
     * it used if bean has mirror for context - beanContext<br>
     * example SisStudent/StudentContextAttributes.
     */
    protected class RetrieveContextPath implements FieldRetriever {
        public static final String CALC_ID = "STD_CTX_PTH";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            return m_enrHelper.getStudentMultiYearHelper().getFieldValueByBeanPath(entity.getBean(),
                    field.getBeanPath());
        }
    }

    /**
     * Retriever for instructional program.
     */
    protected class RetrieveInstrProgram implements FieldRetriever {
        public static final String TN_CALC_INSTPGM_ID = "TN_INSTRPGM_STD";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            TNStudentEndofServiceData tnData = (TNStudentEndofServiceData) data;
            TNStudentEndofServiceEntity seEntity = (TNStudentEndofServiceEntity) entity;
            SisStudent student = (SisStudent) entity.getBean();
            PlainDate docDate = (PlainDate) seEntity.getStudentPropertyAsJavaType(student, tnData.m_fieldCompDocDate);

            if (docDate == null) {
                docDate = seEntity.getCurrentData().getEndOfServiceDate();
            }

            String instrProgram = null;

            List<StudentRecordHelper> records = getStudentHelperMap().get(student.getOid());

            if (records != null && docDate != null) {
                // 1.Try to get record on document completion date.
                for (StudentRecordHelper curRecord : records) {
                    if (!docDate.before(curRecord.getEnrollDate()) &&
                            (curRecord.getExitDate() == null || !docDate.after(curRecord.getExitDate()))) {
                        instrProgram = curRecord.getInstrProgram();
                        break;
                    }
                }
                // 2.Try to get record on the latest date prior to the document completion date.
                if (StringUtils.isEmpty(instrProgram)) {
                    StudentRecordHelper latestRecord = null;
                    for (StudentRecordHelper curRecord : records) {
                        if (curRecord.getExitDate() != null && (!docDate.before(curRecord.getEnrollDate()) &&
                                (docDate.after(curRecord.getExitDate()))) &&
                                (latestRecord == null || latestRecord.getExitDate().before(curRecord.getExitDate()))) {
                            latestRecord = curRecord;
                        }
                    }
                    if (latestRecord != null) {
                        instrProgram = latestRecord.getInstrProgram();
                    }
                }
            }
            // 3.Use the default calendar based on the student calendar code.
            if (StringUtils.isEmpty(instrProgram)) {
                String programKey = makeCalendarLookupKey(m_contextOid, tnData.getSchoolOid(student),
                        (String) seEntity.getFieldValueByBeanPath(student, SisStudent.COL_CALENDAR_CODE));
                instrProgram = m_calendarOids.get(programKey);
            }
            return instrProgram;
        }

    }

    /**
     * Retriever for service information. Currently most info takes from student bean, except End of
     * service date field.
     */
    protected class RetrieveServiceInfo implements FieldRetriever {
        public static final String CALC_ID = "STDE_CALC_SERVICE";

        private static final String PARAM_EOS_ACTION = "EOS_ACTION";
        private static final String PARAM_EOS_DATE = "EOS_DATE";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String parameter = (String) field.getParameter();

            TNStudentEndofServiceEntity tnEntity = (TNStudentEndofServiceEntity) entity;
            if (PARAM_EOS_DATE.equals(parameter)) {
                return tnEntity.getCurrentData().getEndOfServiceDate();
            }
            if (PARAM_EOS_ACTION.equals(parameter)) {
                return tnEntity.getCurrentData().getEndOfServiceAction();
            }

            return null;
        }
    }

    /**
     * Retriever for service information. Currently most info takes from student bean, except End of
     * service date field.
     */
    protected class ValidateServiceInfo implements FieldValidator {
        public static final String VAL_ID = "VAL_SERVICE_DATE";

        private static final String FIELD_ID_ACTION = "EOS ACTION";
        private static final String FIELD_ID_DATE = "EOS DATE";
        private static final String ERROR_ID = "Specify all fields or leave all empty";
        private static final String PARAM_EOS_DATE = "EOS_DATE";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            String parameter = (String) field.getParameter();
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            String action = null;

            if (PARAM_EOS_DATE.equals(parameter)) {
                action = entity.getFieldValue(FIELD_ID_ACTION);

                if ((!StringUtils.isEmpty(value) && StringUtils.isEmpty(action))
                        || (StringUtils.isEmpty(value) && !StringUtils.isEmpty(action))) {
                    String message = "Both fields " + FIELD_ID_DATE + ", " + FIELD_ID_ACTION
                            + " should be empty or not empty at the same time.";
                    StateReportValidationError error = new StateReportValidationError(entity, field, ERROR_ID, message);
                    errors.add(error);
                }
            }

            return errors;
        }

    }

    /**
     * Business rule: For three Completion Document fields. If one is populated all three must be
     * (application validation rule).
     */
    protected class ValidateCompletionDocumentValues implements FieldValidator {
        public static final String FIELD_ID_DATE = "COMPL DOC DATE";
        public static final String VAL_ID = "VAL_COMPL_DOC";

        private static final String ERROR_ID = "All three Completion Document fields must be specified";

        private static final String PARAM_DATE = "COMPL_DOC_DATE";
        private static final String PARAM_TYPE = "COMPL_DOC_TYPE";
        private static final String PARAM_PER = "COMPL_DOC_PER";

        private static final String FIELD_ID_TYPE = "COMPL DOC TYPE";
        private static final String FIELD_ID_PER = "COMPL DOC TYPE";

        /**
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
            String parameter = (String) field.getParameter();
            String date = null;
            String type = null;
            String per = null;
            boolean errorExist = false;
            if (PARAM_DATE.equals(parameter)) {
                if (!StringUtils.isEmpty(value)) {
                    SimpleDateFormat format = new SimpleDateFormat("yyyyMMdd");
                    Date docDate = null;
                    try {
                        docDate = format.parse(value);
                    } catch (ParseException e) {
                        // do nothing for exception
                    }
                    if (docDate == null || docDate.before(data.getCurrentContext().getStartDate())
                            || docDate.after(data.getCurrentContext().getEndDate())) {
                        String message = "Completion document date " + value + " is not between " +
                                data.getCurrentContext().getStartDate() + " and "
                                + data.getCurrentContext().getEndDate();
                        StateReportValidationError error = new StateReportValidationError(entity, field,
                                "Completion document date must be within the current year", message);
                        errors.add(error);
                    }
                }
                date = value;
                type = entity.getFieldValue(FIELD_ID_TYPE);
                per = entity.getFieldValue(FIELD_ID_PER);
                if (!StringUtils.isEmpty(value) && (StringUtils.isEmpty(type) || StringUtils.isEmpty(per))) {
                    errorExist = true;
                }
            } else if (PARAM_TYPE.equals(parameter)) {
                date = entity.getFieldValue(FIELD_ID_DATE);
                type = value;
                per = entity.getFieldValue(FIELD_ID_PER);
                if (!StringUtils.isEmpty(value) && (StringUtils.isEmpty(date) || StringUtils.isEmpty(per))) {
                    errorExist = true;
                }
            } else if (PARAM_PER.equals(parameter)) {
                date = entity.getFieldValue(FIELD_ID_DATE);
                type = entity.getFieldValue(FIELD_ID_TYPE);
                per = value;
                if (!StringUtils.isEmpty(value) && (StringUtils.isEmpty(date) || StringUtils.isEmpty(type))) {
                    errorExist = true;
                }
            }
            if (errorExist) {
                String message = FIELD_ID_DATE + ": " + (StringUtils.isEmpty(date) ? "is empty" : date) + "; " +
                        FIELD_ID_TYPE + ": " + (StringUtils.isEmpty(type) ? "is empty" : type) + "; " +
                        FIELD_ID_TYPE + ": " + (StringUtils.isEmpty(per) ? "is empty" : per);
                StateReportValidationError error = new StateReportValidationError(entity, field, ERROR_ID, message);
                errors.add(error);
            }
            return errors;
        }

    }

    /**
     * Field validator for SSN.
     * Validates SSN against non valid formats
     * Valid ssn format: 999999999
     */
    protected class FiledValidatorSSN implements FieldValidator {
        protected static final String STDE_VAL_ID = "STDE_VAL_SSN";
        private static final String patternSSN = "^[0-9]{9}$|^$";

        /**
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
     * Field retriever for School Year field.
     */
    protected class RetrieveSchoolYear implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            TNStudentEndofServiceData seData = (TNStudentEndofServiceData) data;
            return seData.m_schoolYear;
        }

    }

    /**
     * Constants: Aliases, Fields, IDs, Parameters
     * 
     */
    protected static final String ALIAS_COMP_DOC_TYPE = "DOE COMPLETION DOCUMENT TYPE";
    protected static final String ALIAS_COMP_DOC_PER = "DOE COMPLETION DOCUMENT PER";
    protected static final String ALIAS_COMP_DOC_DATE = "DOE COMPLETION DOCUMENT DATE";
    protected static final String ALIAS_END_SERVICE_ACT = "DOE END SERVICE ACTION";
    protected static final String ALIAS_END_SERVICE_ACT_DATE = "DOE END SERVICE ACTION DATE";
    protected static final String ALIAS_INSTR_SERVICE_TYPE = "DOE INSTR SERVICE TYPE";

    protected static final String ENR_QUAL_CODE = "Y";
    protected static final String INSTR_SERVICE_PRIMARY = "P";

    protected static final String[] VALID_QUAL_STATE_CODES = new String[] {"D", "R", "P"};

    private static final String CALC_ID_SCHOOLYEAR = "STDE_CALC_SCHOOLYEAR";
    private static final String DATE_FORMAT = "yyyy-MM-dd";

    /**
     * Instance variables.
     *
     */
    protected String m_fieldCompDocType;
    protected String m_fieldCompDocPer;
    protected String m_fieldCompDocDate;
    protected String m_fieldEndOfServiceAct;
    protected String m_fieldEndOfServiceActDate;
    protected String m_fieldInstrServiceType;
    protected SimpleDateFormat m_format;
    protected String m_schoolYear;
    protected TNStudentHistoryHelper m_studentHelper;
    protected TNEnrollmentHelper m_enrHelper;

    /**
     * Instance variables (maps)
     */
    protected Map<String, PlainDate> m_lastInSessionDates;
    protected Map<String, ReferenceCode> m_qualCodesMap;
    protected Map<String, Collection<StudentEnrollment>> m_qualEnrsMap;


    /**
     * @see com.x2dev.procedures.statereporting.tn.TNStateReportData#initialize()
     */
    /*
     * Initialize the data module.
     * Initialize necessary working resources.
     * Define query to load.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(java.util.Map,
     * com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    public void initialize() throws X2BaseException {
        super.initialize();
        // insure that all aliases and any other resources needed to operate the
        // report are available in the database.
        // setup error is created if the alias is not found.
        initializeFields();

        getCalendarsForContextOid(m_contextOid);

        getValidQualifyingReasons();

        if (getSetupErrors().size() != 0) {
            return;
        }

        if (getCurrentContext().getSchoolYear() >= 2018) {
            setExportVersion(2);
        }

        initializeCriteriaAndQuery();
        setOnFirstDayWithdrew(true);
        initStudentHelperMap(m_studentHelper, m_studentHelper.getStudentQuery(false));

        registerFieldRetrievers();
        registerFieldValidators();
    }

    /**
     * Gets the last in session date.
     *
     * @param school SisSchool
     * @param calendarCode String
     * @return Plain date
     */
    public PlainDate getLastInSessionDate(SisSchool school, String calendarCode) {
        calendarCode = !StringUtils.isEmpty(calendarCode) ? calendarCode : CALENDAR_ID_STANDARD;
        String key = school.getOid() + calendarCode;
        if (!m_lastInSessionDates.containsKey(key)) {
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(SchoolCalendar.COL_DISTRICT_CONTEXT_OID, m_contextOid);
            criteria.addEqualTo(SchoolCalendar.COL_CALENDAR_ID, calendarCode);
            criteria.addEqualTo(SchoolCalendar.COL_SCHOOL_OID, school.getOid());
            BeanQuery query = new BeanQuery(SchoolCalendar.class, criteria);

            SchoolCalendar schoolCalendar = (SchoolCalendar) getBroker().getBeanByQuery(query);
            if (schoolCalendar != null) {
                SisSchoolCalendarDate lastInSessionDate =
                        CalendarManager.getLastInSessionDate(schoolCalendar, getBroker());
                if (lastInSessionDate != null) {
                    m_lastInSessionDates.put(key, lastInSessionDate.getDate());
                }
            }
        }
        return m_lastInSessionDates.get(key);
    }

    /**
     * Gets the school.
     *
     * @param student SisStudent
     * @return Sis school
     */
    SisSchool getSchool(SisStudent student) {
        return m_enrHelper.getStudentMultiYearHelper().getSchool(student);
    }

    /**
     * Gets the school oid.
     *
     * @param student SisStudent
     * @return String
     */
    String getSchoolOid(SisStudent student) {
        String oid = null;
        SisSchool school = m_enrHelper.getStudentMultiYearHelper().getSchool(student);
        if (school != null) {
            oid = school.getOid();
        }
        return oid == null ? "" : oid;
    }

    /**
     * Method for implementing business rule for schoolYear
     * (CTX_SCHOOL_YEAR - 1) where reporting date falls within `CTX_START_DATE` and `CTX_END_DATE`.
     * else CTX_SCHOOL_YEAR
     * 
     * @return string representation of school year
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
    private X2Criteria getStudentCriteria() {
        m_studentHelper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
        m_studentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);
        m_studentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_INPUT, Boolean.TRUE);
        m_studentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_EXCLUDE, Boolean.TRUE);

        X2Criteria criteria = new X2Criteria();
        X2Criteria criteriaSet1 = new X2Criteria();
        X2Criteria criteriaSet2 = new X2Criteria();
        TNStudentMultiYearHelper multyHelper = m_enrHelper.getStudentMultiYearHelper();
        multyHelper.adjustCriteria(criteriaSet1, Strategy.BETWEEN, m_fieldCompDocDate,
                new Object[] {getCurrentContext().getStartDate(), getCurrentContext().getEndDate()});
        multyHelper.adjustCriteria(criteriaSet2, Strategy.BETWEEN, m_fieldEndOfServiceActDate,
                getCurrentContext().getStartDate(), getCurrentContext().getEndDate());
        criteria.addOrCriteria(criteriaSet1);
        criteria.addOrCriteria(criteriaSet2);

        X2Criteria enrCriteria = null;
        Collection<String> validCodes = m_qualCodesMap.keySet();
        if (validCodes != null && !validCodes.isEmpty()) {
            X2Criteria criteriaSet3 = new X2Criteria();
            enrCriteria = new X2Criteria();
            enrCriteria.addEqualTo(StudentEnrollment.COL_ENROLLMENT_TYPE, ENR_QUAL_CODE);
            enrCriteria.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE,
                    getCurrentContext().getStartDate());
            enrCriteria.addLessOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, getCurrentContext().getEndDate());
            enrCriteria.addIn(StudentEnrollment.COL_REASON_CODE, validCodes);

            QueryByCriteria qualQuery = new QueryByCriteria(StudentEnrollment.class, enrCriteria);

            m_qualEnrsMap = getBroker().getGroupedCollectionByQuery(qualQuery, StudentEnrollment.COL_STUDENT_OID, 1024);

            criteriaSet3.addIn(X2BaseBean.COL_OID, m_qualEnrsMap.keySet());
            criteria.addOrCriteria(criteriaSet3);
        }

        X2Criteria studentCriteria = m_studentHelper.getStudentCriteria();
        studentCriteria.addAndCriteria(m_enrHelper.getStudentMultiYearHelper().getWithAttributesCriteria());
        studentCriteria.addAndCriteria(criteria);
        return studentCriteria;
    }

    /**
     * Returns all possible codes of StudentEnrollment.COL_REASON_CODE
     *
     * @return void
     */
    private void getValidQualifyingReasons() {
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField reasonField = dictionary.findDataDictionaryField(StudentEnrollment.class.getName(),
                StudentEnrollment.COL_REASON_CODE);
        String refTableOid = reasonField.getReferenceTableOid();
        if (StringUtils.isEmpty(refTableOid)) {
            addSetupError("Student Enrollment Configuration Error",
                    "There must be a reference table on the Student Enrollment Reason Code");
        } else {
            X2Criteria codesCriteria = new X2Criteria();
            codesCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, refTableOid);
            codesCriteria.addIn(ReferenceCode.COL_STATE_CODE, Arrays.asList(VALID_QUAL_STATE_CODES));

            QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, codesCriteria);

            m_qualCodesMap = getBroker().getMapByQuery(query, ReferenceCode.COL_CODE, 256);
            if (m_qualCodesMap.isEmpty()) {
                addSetupError("Student Enrollment Configuration Error",
                        "There reference table for Student Enrollment Reason Code must contain some codes with state values in {D, R, P}");
            }
        }


    }

    /**
     * Initialize criteria and query.
     */
    private void initializeCriteriaAndQuery() {
        X2Criteria criteria = getStudentCriteria();
        QueryByCriteria query = new QueryByCriteria(SisStudent.class, criteria);

        applyInputSort(query, null);
        setQuery(query);

        setEntityClass(TNStudentEndofServiceEntity.class);
    }

    /**
     * Lookup field aliases and paths.
     *
     */
    private void initializeFields() {
        m_fieldCompDocType = translateAliasToJavaName(ALIAS_COMP_DOC_TYPE, true);
        m_fieldCompDocPer = translateAliasToJavaName(ALIAS_COMP_DOC_PER, true);
        m_fieldCompDocDate = translateAliasToJavaName(ALIAS_COMP_DOC_DATE, true);
        m_fieldEndOfServiceAct = translateAliasToJavaName(ALIAS_END_SERVICE_ACT, true);
        m_fieldEndOfServiceActDate = translateAliasToJavaName(ALIAS_END_SERVICE_ACT_DATE, true);
        m_fieldInstrServiceType = translateAliasToJavaName(ALIAS_INSTR_SERVICE_TYPE, true);

        m_enrHelper = new TNEnrollmentHelper(this);
        m_studentHelper = m_enrHelper.getStudentHistoryHelper();
        m_reportDate = (PlainDate) getParameter(PARAM_REPORT_DATE);
        m_schoolYear = getCurentSchoolYear();
        m_lastInSessionDates = new HashMap<String, PlainDate>();

        m_format = new SimpleDateFormat(DATE_FORMAT);
    }

    /**
     * Register custom field Retrievers.
     */
    private void registerFieldRetrievers() {
        HashMap calcs = new HashMap<String, FieldRetriever>();
        calcs.put(RetrieveInstrProgram.TN_CALC_INSTPGM_ID, new RetrieveInstrProgram());
        calcs.put(CALC_ID_SCHOOLYEAR, new RetrieveSchoolYear());
        calcs.put(FieldRetrieverSSN.STDE_CALC_ID, new FieldRetrieverSSN());
        calcs.put(RetrieveServiceInfo.CALC_ID, new RetrieveServiceInfo());
        calcs.put(RetrieveStudentContextAttribute.CALC_ID, new RetrieveStudentContextAttribute(m_enrHelper));
        super.addCalcs(calcs);
    }

    /**
     * Register field validators.
     */
    private void registerFieldValidators() {
        HashMap<String, FieldValidator> validators = new HashMap<String, FieldValidator>();
        validators.put(FiledValidatorSSN.STDE_VAL_ID, new FiledValidatorSSN());
        validators.put(ValidateCompletionDocumentValues.VAL_ID, new ValidateCompletionDocumentValues());
        validators.put(ValidateServiceInfo.VAL_ID, new ValidateServiceInfo());
        super.addValidators(validators);
    }
}
