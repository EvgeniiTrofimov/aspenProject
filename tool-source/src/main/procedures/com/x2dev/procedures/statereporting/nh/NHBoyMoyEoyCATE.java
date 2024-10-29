/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2009 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.procedures.statereporting.nh;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.SchoolCalendarDate;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.procedures.statereporting.nh.NHCateHelper.CateEnrollmentSpan;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.SisDistrictSchoolYearContext;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAttendance;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.sis.model.business.EnrollmentManager;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.lang.reflect.InvocationTargetException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.apache.commons.beanutils.PropertyUtils;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * This class implements the data export for New Hampshire's i4see Enrollment export.
 *
 * @author X2 Development Corporation
 */
public class NHBoyMoyEoyCATE extends StateReportData {
    /**
     * Implementation of StateReportEntity to be used by the I4SeeEnrollment export.
     * This must be a public static inner class with a public no argument
     * constructor so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class NHBoyMoyEoyCATEEntity extends StateReportEntity {
        private NHBoyMoyEoyCATE m_data;

        /**
         * List of StudentProgramParticipation that reflect this students participation in CATE.
         */
        private List<CateEnrollmentSpan> m_spans;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public NHBoyMoyEoyCATEEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Check enrollment membership count and membership days parameter to determine if the
         * student should be reported.
         *
         * @return StateReportValidationError
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#filterEntity()
         */
        @Override
        public StateReportValidationError filterEntity() {
            StateReportValidationError error = null;

            FieldDefinition field = m_data.getFieldDefinition(NAME_ATTENDANCE_DAYS_S1);

            /*
             * Get membership days parameter
             */
            double membershipCountAsDouble = 0;

            /*
             * Get membership count
             */
            String membershipCount =
                    m_data.getMembershipDays(this, SEMESTER.FY, false, this.getCurrentSpan().getProgram());

            if (!StringUtils.isEmpty(membershipCount)) {
                try {
                    membershipCountAsDouble = Double.parseDouble(membershipCount);
                } catch (NumberFormatException nfe) {
                    // invalid format, will be reported elsewhere.
                }
            }

            if (m_requireMemberDay && !(Double.compare(membershipCountAsDouble, 0.0) > 0)) {
                // Student filtered.
                error = new StateReportValidationError(this, field, "0 member days - excluded from export", "");
            }

            return error;
        }

        /**
         * Returns the StudentProgramParticipation that represents the current report row.
         *
         * @return StudentProgramParticipation
         */
        public CateEnrollmentSpan getCurrentSpan() {
            CateEnrollmentSpan span = null;
            int index = getCurrentRow();
            if (index >= 0 && index < m_spans.size()) {
                span = m_spans.get(index);
            }
            return span;
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
            String name =
                    student.getNameView() + " [LASID: " + student.getLocalId() + ", SASID: " + student.getStateId()
                            + (m_data.isSchoolContext() ? ", SCHOOL: " + student.getSchool().getName() : "") + "]";

            return name;
        }

        /**
         * Intitialize.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            SisStudent student = (SisStudent) bean;
            m_data = (NHBoyMoyEoyCATE) data;
            m_spans = m_data.m_cateHelper.getCateEnrollmentSpans(student);

            setRowCount(m_spans.size());
        }

        /**
         * If update calculated fields is set, save new values into the bean.
         *
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#postProcess()
         */
        @Override
        public void postProcess() {
            /*
             * If the update flag is set, update calculated values into the student records.
             */
            if (m_updateRecords) {
                try {

                    PropertyUtils.setProperty(getBean(), m_data.m_studentDaysInAttendanceS1,
                            getFieldValue(NAME_ATTENDANCE_DAYS_S1));
                    PropertyUtils.setProperty(getBean(), m_data.m_studentDaysInAttendanceS2,
                            getFieldValue(NAME_ATTENDANCE_DAYS_S2));

                    if (getBean().isDirty()) {
                        m_data.getBroker().saveBeanForced(getBean());
                    }
                } catch (IllegalAccessException e) {
                    // conversion errors. Cannot save student.
                } catch (InvocationTargetException e) {
                    // conversion errors. Cannot save student.
                } catch (NoSuchMethodException e) {
                    // conversion errors. Cannot save student.
                }
            }
        }

        /**
         * Override toString to return identifying information.
         *
         * @return String
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            return getEntityName();
        }
    }

    /**
     * The Enum SEMESTER.
     */
    public enum SEMESTER {
        FY, S1, S2
    }

    /**
     * Returns the entry code for the student.
     */
    protected class RetrieveCateData implements FieldRetriever {
        private static final String CALC_ID = "CATE-DATA";

        private static final String PARAM_ATT_S1 = "ATT_S1";
        private static final String PARAM_ATT_S2 = "ATT_S2";
        private static final String PARAM_CATE_ENR_STATUS = "CATE_ENR_STATUS";
        private static final String PARAM_ENTRY_CODE = "ENTRY_CODE";
        private static final String PARAM_ENTRY_DATE = "ENTRY_DATE";
        private static final String PARAM_EXIT_DATE = "EXIT_DATE";
        private static final String PARAM_EXIT_CODE = "EXIT_CODE";
        private static final String PARAM_SKL_YEAR = "SKL_YEAR";
        private static final String PARAM_STD_DISPL_HOME = "DISP_HOME";
        private static final String PARAM_STD_SINGLE_PARENT = "SINGLE_PARENT";
        private static final String PARAM_STD_WORK = "STD_WORK";

        private final List fields = Arrays.asList(CALC_PARAM_NH_SAU, // 0
                CALC_PARAM_DISTR_NBR, // 1
                CALC_PARAM_SND_SKL, // 2
                CALC_PARAM_SAU_RCV, // 3
                CALC_PARAM_DIST_NBR_RCV, // 4
                CALC_PARAM_SKL_NBR_RCV // 5
        );

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
            NHBoyMoyEoyCATEEntity nhEntity = (NHBoyMoyEoyCATEEntity) entity;
            CateEnrollmentSpan cateSpan = nhEntity.getCurrentSpan();
            StudentProgramParticipation program = cateSpan.getProgram();
            Object parameter = field.getParameter();

            Object value = null;
            Object pgmValue = null;
            if (cateSpan != null) {
                if (PARAM_ENTRY_DATE.equals(parameter)) {
                    value = cateSpan.getEntryDate();
                } else if (PARAM_EXIT_DATE.equals(parameter)) {
                    value = cateSpan.getExitDate();
                } else if (PARAM_ENTRY_CODE.equals(parameter)) {
                    value = cateSpan.getEntryCode();
                } else if (PARAM_EXIT_CODE.equals(parameter)) {
                    value = cateSpan.getExitCode();
                } else if (PARAM_CATE_ENR_STATUS.equals(parameter)) {
                    value = cateSpan.getCateEnrStatus();
                } else if (CALC_PARAM_NH_SAU.equals(parameter)) {
                    pgmValue = program.getFieldValueByBeanPath(m_pgmSauSend);
                } else if (CALC_PARAM_DISTR_NBR.equals(parameter)) {
                    pgmValue = program.getFieldValueByBeanPath(m_pgmDistrSend);
                } else if (CALC_PARAM_SND_SKL.equals(parameter)) {
                    pgmValue = program.getFieldValueByBeanPath(m_pgmSklSend);
                } else if (CALC_PARAM_SAU_RCV.equals(parameter)) {
                    pgmValue = program.getFieldValueByBeanPath(m_pgmSauRec);
                } else if (PARAM_STD_DISPL_HOME.equals(parameter)) {
                    value = BooleanAsStringConverter.TRUE
                            .equals(program.getStudent().getFieldValueByBeanPath(m_stdDisplacedHomemaker)) ? "Y" : "N";
                } else if (PARAM_STD_WORK.equals(parameter)) {
                    value = "0";
                    Object valueByPath = null;
                    if (m_stdWork != null) {
                        valueByPath = program.getStudent().getFieldValueByBeanPath(m_stdWork);
                        if (valueByPath != null) {
                            value = lookupStateValue(SisStudent.class, m_stdWork, (String) valueByPath);
                        }
                    }
                    if (value == null && m_stdWork2 != null) {
                        valueByPath = program.getStudent().getFieldValueByBeanPath(m_stdWork2);
                        if (valueByPath != null) {
                            value = lookupStateValue(SisStudent.class, m_stdWork2, (String) valueByPath);
                        }
                    }
                } else if (PARAM_STD_SINGLE_PARENT.equals(parameter)) {
                    value = BooleanAsStringConverter.TRUE
                            .equals(program.getStudent().getFieldValueByBeanPath(m_stdSingleParent)) ? "Y" : "N";
                } else if (CALC_PARAM_DIST_NBR_RCV.equals(parameter)) {
                    pgmValue = program.getFieldValueByBeanPath(m_pgmDistrRec);

                    if (pgmValue == null && !StringUtils.isEmpty(m_respDistrict)) {
                        pgmValue = m_respDistrict;
                    }
                } else if (CALC_PARAM_SKL_NBR_RCV.equals(parameter)) {
                    pgmValue = program.getFieldValueByBeanPath(m_pgmSklRec);
                    if (StringUtils.isEmpty((String) pgmValue)) {
                        pgmValue = m_receivingSchoolNumber;
                    }
                    if (pgmValue == null && !StringUtils.isEmpty(m_respTown)) {
                        pgmValue = m_respTown;
                    }
                } else if (CALC_PARAM_CIP_CODE.equals(parameter)) {
                    String cipCode = (String) program.getFieldValueByBeanPath(m_pgmCipCode);
                    if (!StringUtils.isEmpty(cipCode)) {
                        if (cipCode.length() < 6) {
                            value = StringUtils.padLeft(cipCode, 6, '0');
                        } else {
                            value = cipCode;
                        }
                    }
                } else if (CALC_PARAM_PGM_COMPLETER.equals(parameter)) {
                    value = data.getPropertyAsJavaType(program, m_pgmCompleter);
                } else if (CALC_PARAM_TRAN_MODE.equals(parameter)) {
                    value = program.getFieldValueByBeanPath(m_pgmTranMode);
                } else if (CALC_PARAM_PGM_PRIM_FLAG.equals(parameter)) {
                    value = program.getFieldValueByBeanPath(m_pgmPrimIndFlag);
                } else if (CALC_PARAM_TSA_FLAG.equals(parameter)) {
                    value = program.getFieldValueByAlias(m_pgmTsaFlag);
                } else if (PARAM_ATT_S1.equals(parameter)) {
                    value = getMembershipDays(nhEntity, SEMESTER.S1, true, program);
                } else if (PARAM_ATT_S2.equals(parameter)) {
                    value = getMembershipDays(nhEntity, SEMESTER.S2, true, program);
                } else if (PARAM_SKL_YEAR.equals(parameter)) {
                    PlainDate entryDate = cateSpan.getEntryDate();
                    if (entryDate != null) {
                        for (SisDistrictSchoolYearContext ctx : m_schoolYears) {
                            if (!ctx.getStartDate().after(entryDate) && !ctx.getEndDate().before(entryDate)) {
                                value = String.valueOf(ctx.getSchoolYear());
                                break;
                            }
                        }
                    }
                }

                if (fields.contains(parameter)) {
                    if (pgmValue == null) {
                        pgmValue = getValueFromPrimarySchool(entity, field);
                    }
                    value = pgmValue;
                }
            }

            return value;
        }

        /**
         * Gets the value from primary school.
         *
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return String
         * @throws X2BaseException exception
         */
        private String getValueFromPrimarySchool(StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String value = null;
            SisStudent student = (SisStudent) entity.getBean();
            SisSchool school = student.getSchool();
            String parameter = (String) field.getParameter();

            if (parameter != null) {
                Organization organization = school.getOrganization1();

                switch (fields.indexOf(parameter)) {
                    case 0: // 1500_SAU_NUMBER_SEND
                    case 3: // 1600_SAU_NUMBER_RCV
                        value = (String) organization.getFieldValueByBeanPath(m_orgSauNumberField);
                        break;

                    case 1: // 1510_DISTRICT_NUMBER_SEND
                    case 4: // 1610_DISTRICT_NUMBER_RCV
                        value = (String) organization.getFieldValueByBeanPath(m_orgDistrictNumberField);
                        break;

                    case 2: // 1520_SCHOOL_NUMBER_SEND
                    case 5: // 1520_SCHOOL_NUMBER_SEND
                        value = (String) school.getFieldValueByBeanPath(m_schoolIdField);
                        break;
                    default:
                        break;
                }
            }

            return value;
        }
    }

    /**
     * The Class RetrieveRegexReplaceSpace.
     */
    protected class RetrieveRegexReplaceSpace implements FieldRetriever {
        private static final String CALC_ID = "REGEX-REPLACE-SPACE";

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
            String value = (String) getProperty(entity.getBean(), field.getBeanPath());
            if (!StringUtils.isEmpty(value)) {
                value = value.replaceAll((String) field.getParameter(), "");
            }
            return value;
        }

    }
    /**
     * Validate entry code.
     *
     */
    protected class ValidateEntryCode implements FieldValidator {
        private static final String VAL_ID = "VAL_ENTRY_CODE";

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
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>(2);

            if (!VALID_ENTRY_STATUS_CODES.contains(value)) {
                errors.add(new StateReportValidationError(entity, field, field.getFieldId() + " is not valid", field
                        .getFieldId() + " = " + value));
            }
            return errors;
        }
    }

    /**
     * Validate exit code.
     */
    protected class ValidateExitCode implements FieldValidator {
        protected static final String VAL_ID = "VAL_EXIT_CODE";

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
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>(1);

            String exitDate = entity.getFieldValue(NAME_EXIT_DATE);

            // Exit code is required for EOY report
            if (StringUtils.isEmpty(value) && m_reportType.intValue() == 1) {
                String errorId = "Exit Date is required";
                String errorMessage = "Exit Date = " + exitDate;
                errors.add(new StateReportValidationError(entity, field, errorId, errorMessage));
            }

            if (!StringUtils.isEmpty(value) && !VALID_EXIT_CODE_VALUES.contains(value)) {
                String errorId = "Exit Code is not valid";
                String errorMessage = "Exit Code = " + value;
                errors.add(new StateReportValidationError(entity, field, errorId, errorMessage));
            }

            return errors;
        }
    }

    /**
     * Validate CATE Enrollment Status.
     */
    protected class ValidateCATEEnrollmentStatus implements FieldValidator {
        private static final String VAL_ID = "VAL_CATE_ENR_STATUS";

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
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>(1);

            if (!VALID_ENROLLMENT_STATUS_VALUES.contains(value)) {
                errors.add(new StateReportValidationError(entity, field, field.getFieldId() + " is not a valid code.",
                        field.getFieldId() + " = " + value));

            }

            return errors;
        }
    }

    /**
     * Validate that the field is not blank for EOY collection only.
     */
    protected class ValidateEOYNotBlank implements FieldValidator {
        protected static final String VAL_ID = "EOYNOTBLANK";

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
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>(1);

            if (StringUtils.isEmpty(value) && m_reportType.intValue() == 1) {
                errors.add(new StateReportValidationError(entity, field, field.getFieldId() + " is required", field
                        .getFieldId() + " = " + value));
            }

            return errors;
        }
    }

    /**
     * Validate that the Telephone value is either blank or 7-14 characters.
     */
    protected class ValidateTelephoneLength implements FieldValidator {
        protected static final String VAL_ID = "TEL-LENGTH";

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
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>(2);
            StateReportValidationError filterError = entity.filterEntity();
            if (filterError != null) {
                errors.add(filterError);
            }
            if (!StringUtils.isEmpty(value) && (value.length() < 7 || value.length() > 14)) {
                errors.add(new StateReportValidationError(entity, field, field.getFieldId()
                        + " must be blank or between 7-14 characters", field.getFieldId() + " = " + value));
            }
            return errors;
        }
    }

    /**
     * Validate Tran Mode.
     */
    protected class ValidateTranMode implements FieldValidator {
        protected static final String VAL_ID = "VAL-TRAN";

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
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>(1);

            if (!VALID_TRAN_MODE_VALUES.contains(value)) {
                errors.add(new StateReportValidationError(entity, field, field.getFieldId() + " is not a valid code",
                        field.getFieldId() + " = " + value));
            }

            return errors;
        }
    }

    /**
     * Input Definition Parameters
     */
    public static final String DEFAULT_I4SEE_220_PARAM = "defaultI4see220";
    public static final String DEFAULT_I4SEE_225_PARAM = "defaultI4see225";
    public static final String INCLUDE_INACTIVE_STUDENTS_PARAM = "includeInactive";
    public static final String INCLUDE_STUDENT_NAMES_PARAM = "includeStudentName";
    public static final String MOY_DATE_PARAM = "moyDate";
    public static final String PARAM_STATUS_NOT_REPORT = "Do not report";
    public static final String PARAM_RECEIVING_SCHOOL_NUMBER = "receivingSchoolNumber";
    public static final String REPORT_DATE_PARAM = "reportDate";
    public static final String REPORT_TYPE_PARAM = "reportType";
    public static final String REQUIRE_MEMBER_DAY_PARAM = "requireMemberDay";
    public static final String REQUIRE_REPORT_STATUS_PARAM = "requireReportStatus";
    public static final String UPDATE_RECORDS_PARAM = "updateRecords";


    /**
     * Retriever Parameters
     */
    protected static final String CALC_PARAM_CIP_CODE = "CIP_CODE";
    protected static final String CALC_PARAM_DISTR_NBR = "DISTR_NBR";
    protected static final String CALC_PARAM_DIST_NBR_RCV = "DIST_NBR_RCV";
    protected static final String CALC_PARAM_NH_SAU = "NH_SAU";
    protected static final String CALC_PARAM_PGM_COMPLETER = "PGM_COMPLETER";
    protected static final String CALC_PARAM_PGM_PRIM_FLAG = "PGM_PRIM_FLAG";
    protected static final String CALC_PARAM_SND_SKL = "SND_SKL";
    protected static final String CALC_PARAM_SAU_RCV = "SAU_NBR_RCV";
    protected static final String CALC_PARAM_SKL_NBR_RCV = "SKL_NBR_RCV";
    protected static final String CALC_PARAM_TRAN_MODE = "TRAN_MODE";
    protected static final String CALC_PARAM_TSA_FLAG = "TSA_FLAG";

    /*
     * Aliases
     */
    // ORGANIZATION
    private static final String ALIAS_040_DISTRICT_NUMBER = "i4see 040";
    private static final String ALIAS_030_SAU_NUMBER = "i4see 030";

    // SCHOOL
    private static final String ALIAS_050_SCHOOL_ID = "i4see 050";

    // STUDENT
    private static final String ALIAS_CATE_ENR_STATUS = "all-std-CATEEnrollmentStatus";
    private static final String ALIAS_CATE_STATUS = "i4see CATE Status";
    private static final String ALIAS_STUDENT_EXCLUDE = "I4SEE EXCLUDE";
    private static final String ALIAS_PRIMARY_PROGRAM_FLAG = "all-pgm-PrimaryProgramIndicator";
    private static final String ALIAS_TSA_FLAG = "all-pgm-TSA";
    private static final String ALIAS_ATTENDANCE_S1 = "i4see 1764";
    private static final String ALIAS_ATTENDANCE_S2 = "i4see 1765";
    private static final String ALIAS_STATUS = "i4see Status";
    private static final String ALIAS_STD_DISP_HOME = "all-std-DisplacedHomemaker";
    private static final String ALIAS_STD_SINGLE_PARENT = "all-std-SingleParent";
    private static final String ALIAS_STD_WORK = "all-std-Work";
    private static final String ALIAS_STD_WORK_2 = "i4see 141";

    // STUDENT_PROGRAM_PARTICIPATION
    private static final String ALIAS_PRIMARY_PROGRAM_ID = "all-pgm-CIPCode";
    private static final String ALIAS_CATE_CONTEXT_ID = "i4see CATE CONTEXT";
    private static final String ALIAS_CATE_ENTRY_CODE = "all-pgm-EntryCode";
    private static final String ALIAS_CATE_ENTRY_DATE = "all-pgm-EntryDate";
    private static final String ALIAS_CATE_EXIT_CODE = "all-pgm-ExitCode";
    private static final String ALIAS_CATE_EXIT_DATE = "all-pgm-ExitDate";
    private static final String ALIAS_DISTRICT_NUMBER_RCV = "all-pgm-ReceivingDistrict";
    private static final String ALIAS_DISTRICT_NUMBER_SEND = "all-pgm-SendingDistrict";
    private static final String ALIAS_PROGRAM_COMPLETER = "all-pgm-ProgramCompleter";
    private static final String ALIAS_SAU_NUMBER_RCV = "all-pgm-ReceivingSchoolSAU";
    private static final String ALIAS_SAU_NUMBER_SEND = "all-pgm-SendingSchoolSAU";
    private static final String ALIAS_SCHOOL_NUMBER_SEND = "all-pgm-SendingSchool";
    private static final String ALIAS_SKL_NUMBER_RCV = "all-pgm-ReceivingSchool";
    private static final String ALIAS_TRAN_MODE = "all-pgm-TransportationMode";

    /**
     * Field Names
     */
    private static final String NAME_ATTENDANCE_DAYS_S1 = "CATEDaysInAttS1";
    private static final String NAME_ATTENDANCE_DAYS_S2 = "CATEDaysInAttS2";
    private static final String NAME_EXIT_DATE = "ExitDate";

    /*
     * Other internal constants
     */
    private static final String DATE_FORMAT = "MM/dd/yyyy";
    private static final String CODE_CATE_PROGRAM_CODE = "CATE";
    private static final String I4SEE_STATUS_FIELD_REPORT_CODE = "Report";
    private static final String OFF_TRACK_CODE = "OFTR";
    private static final String STUDENT_NAME = "name view";

    /**
     * Collections
     */
    protected static final Collection VALID_ENROLLMENT_STATUS_VALUES = Arrays.asList("1", "4", "9", "18", "22");
    protected static final Collection VALID_ENTRY_STATUS_CODES = Arrays.asList("V1", "V2", "V3", "V4");
    protected static final Collection VALID_TRAN_MODE_VALUES = Arrays.asList("2", "3", "6", "7", "8");
    protected static final Collection VALID_EXIT_CODE_VALUES = Arrays.asList("W6", "W10", "W31", "W32", "W33", "W34",
            "W35", "W36", "W37", "W38", "W39", "W40");


    /**
     * Supporting instance variables.
     * These are protected rather than private so they can be accessed by the inner classes.
     */

    protected NHCateHelper m_cateHelper;
    protected SimpleDateFormat m_dateFormat;
    protected PlainDate m_endOfYearDate;
    protected EnrollmentManager m_enrollManager;
    protected Set m_firstDayMembers;
    protected boolean m_includeInactiveStudents = false;
    protected boolean m_includeStudentNames = false;
    protected HashMap<String, HashMap<String, PlainDate>> m_lastInSessionDateMap;
    protected PlainDate m_middleOfYearDate;
    protected String m_orgDistrictNumberField;
    protected String m_orgSauNumberField;
    protected String m_pgmCipCode;
    protected String m_pgmCompleter;
    protected String m_pgmDistrRec;
    protected String m_pgmDistrSend;
    protected String m_pgmPrimIndFlag;
    protected String m_pgmSauRec;
    protected String m_pgmSauSend;
    protected String m_pgmSklRec;
    protected String m_pgmSklSend;
    protected String m_pgmTranMode;
    protected String m_pgmTsaFlag;
    protected String m_progCateContextIdField;
    protected String m_progCateEntryCodeField;
    protected String m_progCateExitCodeField;
    protected String m_receivingSchoolNumber;
    protected static boolean m_requireMemberDay = false;
    protected PlainDate m_reportDate;
    protected Integer m_reportType;
    protected String m_respDistrict;
    protected String m_respTown;
    protected Map<String, Schedule> m_scheduleMap = new HashMap();
    protected Map<String, SchoolCalendarDate> m_schoolCalendarDateMap = new HashMap();
    protected String m_schoolIdField;
    protected Map<String, SisSchool> m_schoolMap;
    protected HashMap m_schoolsToCalendars = new HashMap();
    protected Collection<SisDistrictSchoolYearContext> m_schoolYears;
    protected PlainDate m_schoolYearStartDate;
    protected String m_stdDisplacedHomemaker;
    protected String m_stdSingleParent;
    protected String m_stdWork;
    protected String m_stdWork2;
    protected String m_studentCateEnrollmentStatusField;
    protected String m_studentCateStatusField;
    protected String m_studentDaysInAttendanceS1;
    protected String m_studentDaysInAttendanceS2;
    protected String m_studentExcludeField;
    protected String m_studentReportStatusField;
    protected static boolean m_updateRecords = false;

    /**
     * Returns the class of the base bean used for this export.
     *
     * @return Class
     */
    @Override
    public Class getBeanClass() {
        return SisStudent.class;
    }

    /**
     * Returns the title of the report for report headings.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getExportTitle()
     */
    @Override
    public String getExportTitle() {
        return "NH CATE BOY, MOY and EOY Export";
    }

    /**
     * Sets the header row display mode in the export.
     *
     * @return boolean
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getIncludeHeaderRow()
     */
    @Override
    public boolean getIncludeHeaderRow() {
        return false;
    }

    /**
     * Initialize the data module.
     * Initialize necessary working resources.
     * Define query for students to load.
     * Define list of field definitions for the export.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(java.util.Map,
     *      com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    public void initialize() {
        initializeFields();

        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {
            /*
             * Initialize CATE helper
             */
            m_cateHelper = new NHCateHelper(this, getBroker(), CODE_CATE_PROGRAM_CODE, m_progCateContextIdField);

            /*
             * Build query object that will be used to retrieve export students.
             */
            m_cateHelper.getStdHelper().setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_INPUT, Boolean.TRUE);
            m_cateHelper.getStdHelper().setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
            m_cateHelper.getStdHelper().setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE,
                    m_schoolYearStartDate);
            m_cateHelper.getStdHelper().setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);
            m_cateHelper.getStdHelper().setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_SCHOOL,
                    isSchoolContext() ? Boolean.TRUE : Boolean.FALSE);

            if (m_includeInactiveStudents) {
                m_cateHelper.getStdHelper().setSelectionProperty(StudentHistoryHelper.PROPERTY_INCLUDE_INACTIVE,
                        Boolean.TRUE);
            }

            X2Criteria inclusionCriteria1 = new X2Criteria();
            inclusionCriteria1.addNotEqualTo(m_studentExcludeField, BooleanAsStringConverter.TRUE);
            inclusionCriteria1.addNotEqualTo(m_studentReportStatusField, PARAM_STATUS_NOT_REPORT);

            X2Criteria inclusionCriteria2 = new X2Criteria();
            inclusionCriteria2.addNotIn(m_studentCateStatusField,
                    m_cateHelper.getCodesByStateAndAlias("2", ALIAS_CATE_STATUS));

            X2Criteria inclusionCriteria = new X2Criteria();
            inclusionCriteria.addOrCriteria(inclusionCriteria1);
            inclusionCriteria.addOrCriteria(inclusionCriteria2);
            m_cateHelper.getStdHelper().getStudentCriteria().addAndCriteria(inclusionCriteria);

            Boolean requireReportStatus = (Boolean) getParameter(REQUIRE_REPORT_STATUS_PARAM);
            if (requireReportStatus.booleanValue()) {
                m_cateHelper.getStdHelper().getStudentCriteria()
                        .addEqualTo(m_studentCateStatusField, I4SEE_STATUS_FIELD_REPORT_CODE);
            }

            getBroker().getCount(m_cateHelper.getStdHelper().getStudentQuery(true));
            loadSchools();

            loadActiveSchedules();
            loadSchoolYears();
            /*
             * Add the Student Name field if requested
             */
            if (m_includeStudentNames) {
                getFieldDefinitions().add(0, getName());
            }

            /*
             * Set up Retrievers and Validators
             */
            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put(RetrieveCateData.CALC_ID, new RetrieveCateData());
            calcs.put(RetrieveRegexReplaceSpace.CALC_ID, new RetrieveRegexReplaceSpace());
            super.addCalcs(calcs);

            HashMap validators = new HashMap<String, FieldRetriever>();
            validators.put(ValidateCATEEnrollmentStatus.VAL_ID, new ValidateCATEEnrollmentStatus());
            validators.put(ValidateEntryCode.VAL_ID, new ValidateEntryCode());
            validators.put(ValidateExitCode.VAL_ID, new ValidateExitCode());
            validators.put(ValidateTranMode.VAL_ID, new ValidateTranMode());
            validators.put(ValidateEOYNotBlank.VAL_ID, new ValidateEOYNotBlank());
            validators.put(ValidateTelephoneLength.VAL_ID, new ValidateTelephoneLength());
            super.addValidators(validators);

            setQuery(m_cateHelper.getStdHelper().getStudentQuery(false));
            setEntityClass(NHBoyMoyEoyCATEEntity.class);
        }
    }

    /**
     * Returns the number of days the student has been a member during a particular semester
     * from the start of school to the report date.
     *
     * @param i4see NHBoyMoyEoyCATEEntity
     * @param semester - S1, S2, or FY
     * @param attendance boolean
     * @param program StudentProgramParticipation
     * @return String
     */
    protected String getMembershipDays(NHBoyMoyEoyCATEEntity i4see,
                                       SEMESTER semester,
                                       boolean attendance,
                                       StudentProgramParticipation program) {
        String count = null;
        SisStudent student = (SisStudent) i4see.getBean();

        // Check the active schedule for the school.
        SisSchool school = m_schoolMap.get(student.getSchoolOid());
        Schedule schedule = null;

        if (school != null) {
            schedule = m_scheduleMap.get(school.getOid());
            if (schedule != null) {
                try {
                    PlainDate startDate = m_scheduleMap.get(student.getSchoolOid()).getStartDate();
                    PlainDate endDate;

                    X2Criteria criteria = new X2Criteria();
                    criteria.addEqualTo(StudentAttendance.COL_STUDENT_OID, student.getOid());
                    criteria.addEqualTo(StudentAttendance.COL_OTHER_CODE, OFF_TRACK_CODE);

                    X2Criteria attCriteria = new X2Criteria();
                    attCriteria.addEqualTo(StudentAttendance.COL_STUDENT_OID, student.getOid());
                    attCriteria.addNotEqualTo(StudentAttendance.COL_OTHER_CODE, OFF_TRACK_CODE);
                    attCriteria.addEqualTo(StudentAttendance.COL_ABSENT_INDICATOR, Boolean.TRUE);

                    /*
                     * Set the starting date of the date range
                     */

                    if (program != null && program.getStartDate().after(startDate)) {
                        startDate = program.getStartDate();
                    }

                    // If the value being fetched is the number of days for the
                    // 2nd semester
                    // then set the startDate to the midterm date.
                    // MOY date is the first date in the second semester.
                    if (semester.equals(SEMESTER.S2) && m_middleOfYearDate.after(startDate)) {
                        startDate = m_middleOfYearDate;
                    }

                    criteria.addGreaterOrEqualThan(StudentAttendance.COL_DATE, startDate);
                    attCriteria.addGreaterOrEqualThan(StudentAttendance.COL_DATE, startDate);

                    /*
                     * Set the ending date of the date range
                     */

                    if (program != null && program.getEndDate() != null) {
                        endDate = program.getEndDate();
                    } else {
                        endDate = m_reportDate;
                    }

                    // If the value being fetched is the number of days for the
                    // 1st semester
                    // then set the endDate to the midterm date.
                    // MOY date is the first date in the second semester.
                    if (semester.equals(SEMESTER.S1) && m_middleOfYearDate != null
                            && !m_middleOfYearDate.after(endDate)) {
                        endDate = DateUtils.add(m_middleOfYearDate, -1);
                    }

                    if (endDate.after(m_reportDate)) {
                        endDate = m_reportDate;
                    }

                    criteria.addLessOrEqualThan(StudentAttendance.COL_DATE, endDate);
                    attCriteria.addLessOrEqualThan(StudentAttendance.COL_DATE, endDate);

                    /*
                     * Query student attendance
                     */

                    QueryByCriteria query = new QueryByCriteria(StudentAttendance.class, criteria);
                    int offTrackDays = getBroker().getCount(query);

                    float membership =
                            m_enrollManager
                                    .getMembershipTotal(
                                            student,
                                            getCalendarDays(m_schoolMap.get(student.getSchoolOid()),
                                                    student.getCalendarCode()),
                                            true, startDate, endDate, null)
                                    - offTrackDays;

                    // If requesting attendance instead of membership, subtract
                    // out absent total.
                    if (attendance) {
                        ReportQueryByCriteria reportQuery =
                                new ReportQueryByCriteria(StudentAttendance.class,
                                        new String[] {"sum(ATT_PORTION_ABSENT)"}, attCriteria);
                        ReportQueryIterator reportIterator = getBroker().getReportQueryIteratorByQuery(reportQuery);
                        try {
                            if (reportIterator.hasNext()) {
                                Object[] results = (Object[]) reportIterator.next();
                                Number absenses = (Number) results[0];
                                if (absenses != null) {
                                    membership -= absenses.floatValue();
                                }
                            }
                        } finally {
                            reportIterator.close();
                        }
                    }

                    count = Integer.toString((int) membership);
                } catch (Exception e) {
                    addSetupError("Could not calculate membership", "Student: " + student.getNameView()
                            + ", Exception\n\t" + e.getMessage());
                }
            }
        }
        return count;
    }

    /**
     * Returns the days-in-session for the given school and calendar combination.
     * If a schedule does not yet exist, null is returned.
     *
     * @param school SisSchool
     * @param calendar String
     * @return Set of PlainDate objects
     */
    private Set<PlainDate> getCalendarDays(SisSchool school, String calendar) {
        Set<PlainDate> calendarDays = null;

        if (!m_schoolsToCalendars.containsKey(school.getOid())) {
            // If a schedule has been defined...
            if (m_scheduleMap.get(school.getOid()) != null) {
                PlainDate startDate = getSchoolScheduleStartDate(school.getOid());
                Map calendarData = m_enrollManager.getCalendarLookup(school, startDate, m_reportDate);
                m_schoolsToCalendars.put(school.getOid(), calendarData);
            } else {
                m_schoolsToCalendars.put(school.getOid(), new HashMap<String, PlainDate>());
            }
        }

        calendarDays = (Set) ((Map) m_schoolsToCalendars.get(school.getOid())).get(calendar);

        return calendarDays;
    }

    /**
     * Build Field definition for the student name view.
     *
     * @return a FieldDefinition
     */
    private FieldDefinition getName() {
        FieldDefinition field =
                new FieldDefinition(STUDENT_NAME, SisStudent.COL_NAME_VIEW, null, false, 1, 32, null, null, null, null,
                        null);

        return field;
    }

    /**
     * Get the School Schedule Start Date.
     *
     * @param schoolOid String
     * @return PlainDate
     */
    private PlainDate getSchoolScheduleStartDate(String schoolOid) {
        PlainDate startDate = null;

        if (m_scheduleMap.containsKey(schoolOid)) {
            startDate = m_scheduleMap.get(schoolOid).getStartDate();
        }

        return startDate;
    }

    /**
     * Sets the Java names (bean paths) for all the exported fields.
     */
    private void initializeFields() {
        // System Parameters
        m_dateFormat = new SimpleDateFormat(DATE_FORMAT);
        m_schoolYearStartDate = getOrganization().getCurrentContext().getStartDate();
        m_endOfYearDate = getOrganization().getCurrentContext().getEndDate();
        m_enrollManager = new EnrollmentManager(getBroker(), getPrivilegeSet(), getOrganization());
        m_firstDayMembers = m_enrollManager.getMembershipAsOf(m_schoolYearStartDate, getOrganization());
        m_lastInSessionDateMap = new HashMap<String, HashMap<String, PlainDate>>();

        // Load Parameters
        m_includeInactiveStudents = false;
        if (getParameter(INCLUDE_INACTIVE_STUDENTS_PARAM) != null) {
            m_includeInactiveStudents = ((Boolean) getParameter(INCLUDE_INACTIVE_STUDENTS_PARAM)).booleanValue();
        }

        if (getParameter(DEFAULT_I4SEE_220_PARAM) != null) {
            m_respTown = (String) getParameter(DEFAULT_I4SEE_220_PARAM);
        }

        if (getParameter(DEFAULT_I4SEE_225_PARAM) != null) {
            m_respDistrict = (String) getParameter(DEFAULT_I4SEE_225_PARAM);
        }

        m_receivingSchoolNumber = (String) getParameter(PARAM_RECEIVING_SCHOOL_NUMBER);
        m_includeStudentNames = false;
        if (getParameter(INCLUDE_STUDENT_NAMES_PARAM) != null) {
            m_includeStudentNames = ((Boolean) getParameter(INCLUDE_STUDENT_NAMES_PARAM)).booleanValue();
        }
        if (getParameter(REPORT_DATE_PARAM) != null) {
            m_reportDate = (PlainDate) getParameter(REPORT_DATE_PARAM);
        }
        if (getParameter(REPORT_TYPE_PARAM) != null) {
            m_reportType = (Integer) getParameter(REPORT_TYPE_PARAM);
        }
        m_requireMemberDay = false;
        if (getParameter(REQUIRE_MEMBER_DAY_PARAM) != null) {
            m_requireMemberDay = ((Boolean) getParameter(REQUIRE_MEMBER_DAY_PARAM)).booleanValue();
        }
        m_updateRecords = false;
        if (getParameter(UPDATE_RECORDS_PARAM) != null) {
            m_updateRecords = ((Boolean) getParameter(UPDATE_RECORDS_PARAM)).booleanValue();
        }
        if (getParameter(MOY_DATE_PARAM) != null) {
            m_middleOfYearDate = (PlainDate) getParameter(MOY_DATE_PARAM);
        }

        // Load Alias database field Names
        m_orgDistrictNumberField = translateAliasToJavaName(ALIAS_040_DISTRICT_NUMBER, true);
        m_orgSauNumberField = translateAliasToJavaName(ALIAS_030_SAU_NUMBER, true);
        m_progCateContextIdField = translateAliasToJavaName(ALIAS_CATE_CONTEXT_ID, true);
        m_progCateEntryCodeField = translateAliasToJavaName(ALIAS_CATE_ENTRY_CODE, true);
        m_progCateExitCodeField = translateAliasToJavaName(ALIAS_CATE_EXIT_CODE, true);
        m_schoolIdField = translateAliasToJavaName(ALIAS_050_SCHOOL_ID, true);
        m_studentExcludeField = translateAliasToJavaName(ALIAS_STUDENT_EXCLUDE, true);
        m_studentReportStatusField = translateAliasToJavaName(ALIAS_STATUS, true);
        m_studentCateStatusField = translateAliasToJavaName(ALIAS_CATE_STATUS, true);
        m_studentCateEnrollmentStatusField = translateAliasToJavaName(ALIAS_CATE_ENR_STATUS, true);
        m_studentDaysInAttendanceS1 = translateAliasToJavaName(ALIAS_ATTENDANCE_S1, true);
        m_studentDaysInAttendanceS2 = translateAliasToJavaName(ALIAS_ATTENDANCE_S2, true);
        m_pgmDistrRec = translateAliasToJavaName(ALIAS_DISTRICT_NUMBER_RCV, true);
        m_pgmDistrSend = translateAliasToJavaName(ALIAS_DISTRICT_NUMBER_SEND, true);
        m_pgmSauRec = translateAliasToJavaName(ALIAS_SAU_NUMBER_RCV, true);
        m_pgmSauSend = translateAliasToJavaName(ALIAS_SAU_NUMBER_SEND, true);
        m_pgmSklSend = translateAliasToJavaName(ALIAS_SCHOOL_NUMBER_SEND, true);
        m_pgmSklRec = translateAliasToJavaName(ALIAS_SKL_NUMBER_RCV, true);
        m_pgmTranMode = translateAliasToJavaName(ALIAS_TRAN_MODE, true);
        m_pgmCompleter = translateAliasToJavaName(ALIAS_PROGRAM_COMPLETER, true);
        m_pgmPrimIndFlag = translateAliasToJavaName(ALIAS_PRIMARY_PROGRAM_FLAG, true);
        m_pgmCipCode = translateAliasToJavaName(ALIAS_PRIMARY_PROGRAM_ID, true);
        m_pgmTsaFlag = translateAliasToJavaName(ALIAS_TSA_FLAG, true);
        m_stdWork = translateAliasToJavaName(ALIAS_STD_WORK, false);
        m_stdWork2 = translateAliasToJavaName(ALIAS_STD_WORK_2, false);
        m_stdSingleParent = translateAliasToJavaName(ALIAS_STD_SINGLE_PARENT, true);
        m_stdDisplacedHomemaker = translateAliasToJavaName(ALIAS_STD_DISP_HOME, true);
        translateAliasToJavaName(ALIAS_CATE_ENTRY_DATE, true);
        translateAliasToJavaName(ALIAS_CATE_EXIT_DATE, true);

    }

    /**
     * Loads the active schedule for each school.
     */
    private void loadActiveSchedules() {
        Collection<SisSchool> schools = m_schoolMap.values();

        for (SisSchool school : schools) {
            m_scheduleMap.put(school.getOid(), school.getActiveSchedule());
        }
    }

    /**
     * Loads all school years.
     */
    private void loadSchoolYears() {

        QueryByCriteria ctxQuery = new QueryByCriteria(SisDistrictSchoolYearContext.class);
        ctxQuery.addOrderByDescending(SisDistrictSchoolYearContext.COL_END_DATE);

        m_schoolYears = getBroker().getCollectionByQuery(ctxQuery);
    }

    /**
     * Loads all the school beans in the district.
     */
    private void loadSchools() {
        X2Criteria schoolCriteria = new X2Criteria();
        QueryByCriteria schoolQuery = new QueryByCriteria(SisSchool.class, schoolCriteria);

        m_schoolMap = getBroker().getMapByQuery(schoolQuery, X2BaseBean.COL_OID, getBroker().getCount(schoolQuery));
    }

}
