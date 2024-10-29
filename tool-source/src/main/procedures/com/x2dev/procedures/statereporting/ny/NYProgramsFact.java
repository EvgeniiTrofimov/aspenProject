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
package com.x2dev.procedures.statereporting.ny;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.k12.beans.Procedure;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.beans.ToolSourceCode;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.ToolManager;
import com.follett.fsc.core.k12.tools.Tool;
import com.follett.fsc.core.k12.tools.ToolRunException;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.lang.reflect.InvocationTargetException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * New York state procedure for Programs Fact export.
 *
 * @author X2 Development Corporation
 */

public class NYProgramsFact extends StateReportData {
    /**
     * Entity class for Program Fact export.
     *
     * @author X2 Development Corporation
     */

    public static class NYProgramsFactEntity extends StateReportEntity {

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public NYProgramsFactEntity() {
            // no argument constructor
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            NYProgramsFact m_data = (NYProgramsFact) data;

            // This is unintentional excluding some StudentPrograms that it shouldn't. Hold for now.
            /*
             * NYProgramsFact pfData = (NYProgramsFact) data;
             * if (pfData.m_excludeSchoolIndicator)
             * {
             * StudentProgramParticipation program = (StudentProgramParticipation) bean;
             * StudentEnrollment enrollment =
             * pfData.m_helper.getEnrollmentForDate(program.getStudentOid(), program.getStartDate(),
             * StudentEnrollment.ENTRY);
             * if (enrollment != null)
             * {
             * if (BooleanAsStringConverter.TRUE.equals(enrollment.getSchool().getFieldValueByAlias(
             * ALIAS_EXCLUDE_SCHOOL)))
             * {
             * setRowCount(0);
             * }
             * }
             * }
             */

            StudentProgramParticipation program = (StudentProgramParticipation) bean;

            try {
                SisStudent student = program.getStudent();

                StateReportEntity stdLiteEntity =
                        NYStudentLite.NYStudentLiteEntity.class.getDeclaredConstructor().newInstance();
                stdLiteEntity.intitialize(m_data.m_dataStdLite, student);

                StateReportEntity stdEntryExit =
                        NYStudentEntryExit.EntryExitEntity.class.getDeclaredConstructor().newInstance();
                stdEntryExit.intitialize(m_data.m_dataEe, student);

                if (stdLiteEntity.getRowCount() == 0 || stdEntryExit.getRowCount() == 0) {
                    setRowCount(0);
                }
            } catch (InstantiationException | IllegalAccessException | IllegalArgumentException
                    | InvocationTargetException | NoSuchMethodException | SecurityException e) {
                throw new X2BaseException(e);
            }

            String stateCode = m_data.lookupStateValue(StudentProgramParticipation.class,
                    StudentProgramParticipation.COL_PROGRAM_CODE, program.getProgramCode());
            if (StringUtils.isEmpty(stateCode)) {
                setRowCount(0);
            }
        }
    }

    /**
     * Retrieves the location code for the attendance record.
     */
    protected class RetrieveLocationCode implements FieldRetriever {
        public static final String CAL_ID = "PGMFACT-LOCATION";

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
            String value = null;
            StudentProgramParticipation program = (StudentProgramParticipation) entity.getBean();
            SisStudent std = program.getStudent();
            StudentEnrollment stdEnrByDate = m_helper.getEnrollmentForDate(std.getOid(), m_reportDate,
                    StudentEnrollment.ENTRY + StudentEnrollment.YOG_CHANGE + StudentEnrollment.STATUS_CHANGE);
            // First check the enrollment for an override.
            if (stdEnrByDate != null) {
                value = (String) stdEnrByDate.getFieldValueByBeanPath(m_fieldEnrLocOverride);

                // If we don't have a value check the school of the enrollment, and check
                // that location code
                if (StringUtils.isEmpty(value)) {
                    value = (String) stdEnrByDate.getSchool().getFieldValueByBeanPath(m_fieldSklLocationCode);
                }
            }
            // If we still don't have a value get the code from the students school
            if (StringUtils.isEmpty(value)) {
                value = (String) std.getSchool().getFieldValueByBeanPath(m_fieldSklLocationCode);
            }
            return value;
        }
    }

    /**
     * Returns calculated values for the program from the entity.
     * <p>
     * Param:
     * <br>
     * SCHOOL: The school id (LocationId)
     * <br>
     * EXIT_DATE: The program exit date if present,
     * or the last enrollment date if the student is inactive,
     * or null if the student is still active.
     * <br>
     * QUALIFICIATION_CODE: Retrieve the exit reason. If the program is a meals program, retrieve an
     * alternate exit reason field for meals.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveProgram implements FieldRetriever {

        public static final String CAL_ID = "PGM-INFO";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object value = null;
            String parameter = (String) field.getParameter();
            StudentProgramParticipation program = (StudentProgramParticipation) entity.getBean();
            PlainDate districtStartDate = getOrganization().getCurrentContext().getStartDate();

            if (PARAM_START_DATE.equals(parameter)) {
                PlainDate startDate = program.getStartDate();
                if (startDate == null || startDate.before(districtStartDate)) {
                    startDate = districtStartDate;
                }
                value = startDate;
            } else if (PARAM_END_DATE.equals(parameter) ||
                    PARAM_PGM_EXIT_REASON_1.equals(parameter) ||
                    PARAM_PGM_EXIT_REASON_2.equals(parameter) ||
                    PARAM_PGM_EXIT_REASON_3.equals(parameter)) {
                PlainDate endDate = program.getEndDate();
                if (endDate != null && endDate.before(m_reportDate)) {
                    if (endDate.before(districtStartDate)) {
                        endDate = districtStartDate;
                    }
                    StudentProgramParticipation stdPgm = (StudentProgramParticipation) entity.getBean();
                    if (PARAM_PGM_EXIT_REASON_1.equals(parameter) && m_fieldPgmExitReason1 != null) {
                        value = data.lookupStateValue(m_beanClass, m_fieldPgmExitReason1,
                                (String) stdPgm.getFieldValueByBeanPath(m_fieldPgmExitReason1));
                    } else if (PARAM_PGM_EXIT_REASON_2.equals(parameter) && m_fieldPgmExitReason2 != null) {
                        value = data.lookupStateValue(m_beanClass, m_fieldPgmExitReason2,
                                (String) stdPgm.getFieldValueByBeanPath(m_fieldPgmExitReason2));
                    } else if (PARAM_PGM_EXIT_REASON_3.equals(parameter) && m_fieldPgmExitReason3 != null) {
                        value = data.lookupStateValue(m_beanClass, m_fieldPgmExitReason3,
                                (String) stdPgm.getFieldValueByBeanPath(m_fieldPgmExitReason3));
                    } else if (PARAM_END_DATE.equals(parameter)) {
                        value = endDate;
                    }
                }
            } else if (PARAM_PGM_DURATION.equals(parameter)
                    && program.getFieldValueByBeanPath(m_pgmDurationPath) != null) {
                Integer pgmDuration = Integer.valueOf((String) program.getFieldValueByBeanPath(m_pgmDurationPath));

                value = pgmDuration;
            }

            return value;
        }
    }

    /**
     * Validate the Student Program Participation.
     */
    protected class ValidateProgram implements FieldValidator {

        /**
         * The Class CrossRecordsValidator.
         */
        private class CrossRecordsValidator {

            /**
             * Class provide method to determine if period crosses another period.
             *
             * @author Follett Software Company
             */
            private class CrossPeriod {
                private Date m_end;
                private Date m_start;

                /**
                 * Instantiates a new cross period.
                 *
                 * @param start Date
                 * @param end Date
                 */
                public CrossPeriod(Date start, Date end) {
                    m_start = start;
                    m_end = end;
                }

                /**
                 * Returns end date.
                 *
                 * @return PlainDate
                 */
                public Date getEnd() {
                    return m_end;
                }

                /**
                 * Returns start date.
                 *
                 * @return PlainDate
                 */
                public Date getStart() {
                    return m_start;
                }

                /**
                 * @see java.lang.Object#equals(java.lang.Object)
                 */
                @Override
                public boolean equals(Object obj) {
                    if (!(obj instanceof CrossPeriod)) {
                        return false;
                    }
                    if (obj == this) {
                        return true;
                    }

                    CrossPeriod rhs = (CrossPeriod) obj;
                    return new EqualsBuilder().append(m_start.toString(), rhs.m_start.toString())
                            .append(m_end.toString(), rhs.m_end.toString()).isEquals();
                }

                /**
                 * Returns true if passed instance is overlapped with current instance, otherwise
                 * false.
                 *
                 * @param otherPeriod CrossPeriod
                 * @return boolean
                 */
                public boolean isCross(CrossPeriod otherPeriod) {
                    boolean returnValue = false;

                    if (otherPeriod.getEnd() == null && otherPeriod.getStart() == null) {
                        returnValue = true;
                    } else if (m_start != null && otherPeriod.getStart() != null) {
                        if (m_end != null && otherPeriod.getEnd() != null) {
                            if (!m_start.after(otherPeriod.getEnd()) && !m_end.before(otherPeriod.getStart())) {
                                returnValue = true;
                            }
                        } else if (m_end != null) {
                            if (!otherPeriod.getStart().after(m_end)) {
                                returnValue = true;
                            }
                        } else if (otherPeriod.getEnd() != null) {
                            if (!m_start.after(otherPeriod.getEnd())) {
                                returnValue = true;
                            }
                        } else {
                            returnValue = true;
                        }
                    }

                    return returnValue;
                }

                /**
                 * @see java.lang.Object#toString()
                 */
                @Override
                public String toString() {
                    return "CrossPeriod class " + m_start + " " + m_end;
                }

                /**
                 * @see java.lang.Object#hashCode()
                 */
                @Override
                public int hashCode() {
                    return new HashCodeBuilder(17, 31).append(m_start.toString()).append(m_end.toString()).toHashCode();
                }
            }

            Map<String, Map<String, Collection<CrossPeriod>>> m_studentPgmCrossPeriodsMap;

            /**
             * Instantiates a new cross records validator.
             */
            public CrossRecordsValidator() {
                m_studentPgmCrossPeriodsMap = new HashMap<String, Map<String, Collection<CrossPeriod>>>();
            }

            /**
             * Adds the pgm period record.
             *
             * @param stdOid String
             * @param pgmCode String
             * @param start Date
             * @param end Date
             */
            public void addPgmPeriodRecord(String stdOid, String pgmCode, Date start, Date end) {
                Map<String, Collection<CrossPeriod>> pgmCrossPeriods = m_studentPgmCrossPeriodsMap.get(stdOid);
                if (pgmCrossPeriods == null) {
                    pgmCrossPeriods = new HashMap<String, Collection<CrossPeriod>>();
                    m_studentPgmCrossPeriodsMap.put(stdOid, pgmCrossPeriods);
                }

                Collection<CrossPeriod> crossPeriods = pgmCrossPeriods.get(pgmCode);
                if (crossPeriods == null) {
                    crossPeriods = new ArrayList<CrossPeriod>();
                    pgmCrossPeriods.put(pgmCode, crossPeriods);
                }
                CrossPeriod newCrossPeriod = new CrossPeriod(start, end);
                crossPeriods.add(newCrossPeriod);
            }

            /**
             * Validate all records.
             *
             * @param stdOid String
             * @return Collection
             */
            public Collection<String> validateAllRecords(String stdOid) {
                Set<String> errorMessages = new HashSet<String>();

                Map<String, Collection<CrossPeriod>> programCrossPeriodsMap = m_studentPgmCrossPeriodsMap.get(stdOid);
                Collection<String> programCodes = programCrossPeriodsMap.keySet();

                if (!Collections.disjoint(programCodes, m_pgmCodesEllEligibility)) {
                    int numOfEllEligibRecords = 0;
                    for (Object pgmEllEligCode : m_pgmCodesEllEligibility) {
                        Collection<CrossPeriod> crossPeriods = programCrossPeriodsMap.get(pgmEllEligCode);
                        if (crossPeriods != null) {
                            numOfEllEligibRecords += crossPeriods.size();
                        }
                    }
                    if (numOfEllEligibRecords > 1) {
                        String errorMessage = "Multiple records with code of " + m_pgmCodesEllEligibility + " found";
                        errorMessages.add(errorMessage);
                    }
                }

                if (!Collections.disjoint(programCodes, m_pgmCodesEll)
                        && !Collections.disjoint(programCodes, m_pgmCodesEllEligibility)) {
                    ArrayList<CrossPeriod> ellPeriods = new ArrayList();
                    for (Object ellCode : m_pgmCodesEll) {
                        Collection<CrossPeriod> curEllCrossPeriods = programCrossPeriodsMap.get(ellCode);
                        if (curEllCrossPeriods != null) {
                            ellPeriods.addAll(curEllCrossPeriods);
                        }
                    }

                    ArrayList<CrossPeriod> eligibilityPeriods = new ArrayList();
                    for (Object eligibilityCode : m_pgmCodesEllEligibility) {

                        Collection<CrossPeriod> eligibilityCrossPeriods = programCrossPeriodsMap.get(eligibilityCode);
                        if (eligibilityCrossPeriods != null) {
                            eligibilityPeriods.addAll(eligibilityCrossPeriods);
                        }
                    }

                    for (CrossPeriod ellCrossPeriod : ellPeriods) {
                        for (CrossPeriod eligibilityPeriod : eligibilityPeriods) {
                            if (ellCrossPeriod.isCross(eligibilityPeriod)) {
                                String errorMessage = "ELL Eligibility start date is later than ELL Program start date";
                                errorMessages.add(errorMessage);
                            }
                        }
                    }
                }

                errorMessages
                        .addAll(getRequiredRecordErrors(stdOid, m_pgmCodesWithEligibility, m_pgmCodesEllEligibility));
                errorMessages
                        .addAll(getRequiredRecordErrors(stdOid, Arrays.asList("5720", "5731"), Arrays.asList("0231")));
                errorMessages.addAll(getRequiredRecordErrors(stdOid, Arrays.asList("5720", "5731"),
                        Arrays.asList("5709", "5676", "5687")));
                errorMessages.addAll(getRequiredRecordErrors(stdOid, m_pgmCodesSafetyNet, Arrays.asList("0264")));
                errorMessages
                        .addAll(getRequiredRecordErrors(stdOid, Arrays.asList("5731", "5742"), Arrays.asList("8282")));
                errorMessages
                        .addAll(getRequiredRecordErrors(stdOid, Arrays.asList("5817", "5806"), Arrays.asList("0198")));
                errorMessages.addAll(
                        getRequiredRecordErrors(stdOid, Arrays.asList("5687", "5676", "5742"), Arrays.asList("0231")));

                errorMessages.addAll(getConflictPeriodsErrors(stdOid, m_pgmCodesSafetyNet, Arrays.asList("0220")));
                errorMessages.addAll(getConflictPeriodsErrors(stdOid, m_pgmCodesSafetyNet, Arrays.asList("5775")));
                errorMessages.addAll(getConflictPeriodsErrors(stdOid, Arrays.asList("5817"), Arrays.asList("5806")));
                errorMessages.addAll(getConflictPeriodsErrors(stdOid, m_pgmCodesCte, m_pgmCodesCte));

                Collection<String> pgmCodes = m_studentPgmCrossPeriodsMap.get(stdOid).keySet();

                for (String pgmCode : pgmCodes) {
                    errorMessages
                            .addAll(getConflictPeriodsErrors(stdOid, Arrays.asList(pgmCode), Arrays.asList(pgmCode)));
                }

                return errorMessages;
            }

            /**
             * Do cross periods conflict.
             *
             * @param leftPeriods Collection<CrossPeriod>
             * @param rightPeriods Collection<CrossPeriod>
             * @return true, if successful
             */
            private boolean doCrossPeriodsConflict(Collection<CrossPeriod> leftPeriods,
                                                   Collection<CrossPeriod> rightPeriods) {
                boolean doCrossPeriodsConflict = false;

                for (CrossPeriod leftPeriod : leftPeriods) {
                    for (CrossPeriod rightPeriod : rightPeriods) {
                        if (leftPeriod.isCross(rightPeriod)) {
                            doCrossPeriodsConflict = true;
                        }
                    }
                }

                return doCrossPeriodsConflict;
            }

            /**
             * Gets the conflict periods errors.
             *
             * @param stdOid String
             * @param codesLeft Collection<String>
             * @param codesRight Collection<String>
             * @return Collection
             */
            private Collection<String> getConflictPeriodsErrors(String stdOid,
                                                                Collection<String> codesLeft,
                                                                Collection<String> codesRight) {
                Collection<String> errorsMessages = new ArrayList<String>();

                Map<String, Collection<CrossPeriod>> pgmCrossPeriodsMap = m_studentPgmCrossPeriodsMap.get(stdOid);
                Collection<String> studentProgramCodes = pgmCrossPeriodsMap.keySet();

                if (!Collections.disjoint(studentProgramCodes, codesLeft)
                        && !Collections.disjoint(studentProgramCodes, codesRight)) {
                    Collection<CrossPeriod> leftCrossPeriods = getCrossPeriodsOfPrograms(pgmCrossPeriodsMap, codesLeft);
                    Collection<CrossPeriod> rightCrossPeriods =
                            getCrossPeriodsOfPrograms(pgmCrossPeriodsMap, codesRight);

                    if (doCrossPeriodsConflict(leftCrossPeriods, rightCrossPeriods)) {
                        String errorMessage = "A student cannot have a program " + codesLeft +
                                " records and " + codesRight + " records during the same time period";
                        errorsMessages.add(errorMessage);
                    }
                }

                return errorsMessages;
            }

            /**
             * Gets the cross periods of programs.
             *
             * @param pgmPeriodsMap Map<String,Collection<CrossPeriod>>
             * @param pgmCodes Collection<String>
             * @return Collection
             */
            private Collection<CrossPeriod> getCrossPeriodsOfPrograms(Map<String, Collection<CrossPeriod>> pgmPeriodsMap,
                                                                      Collection<String> pgmCodes) {
                Collection<CrossPeriod> crossPeriods = new ArrayList<CrossPeriod>();
                Collection<String> studentPgmCodes = pgmPeriodsMap.keySet();
                for (String currentPgmCode : pgmCodes) {
                    if (studentPgmCodes.contains(currentPgmCode)) {
                        crossPeriods.addAll(pgmPeriodsMap.get(currentPgmCode));
                    }
                }
                return crossPeriods;
            }

            /**
             * Gets the required record errors.
             *
             * @param stdOid String
             * @param leftCodes Collection<String>
             * @param rightCodes Collection<String>
             * @return Collection
             */
            private Collection<String> getRequiredRecordErrors(String stdOid,
                                                               Collection<String> leftCodes,
                                                               Collection<String> rightCodes) {
                Collection<String> errorMessages = new ArrayList<String>();

                Map<String, Collection<CrossPeriod>> pgmCrossPeriods = m_studentPgmCrossPeriodsMap.get(stdOid);
                Collection<String> studentPrograms = pgmCrossPeriods.keySet();

                if (!Collections.disjoint(studentPrograms, leftCodes)
                        && Collections.disjoint(studentPrograms, studentPrograms)) {
                    String errorMessage = "Program codes " + leftCodes + " require programs " + rightCodes + " record";
                    errorMessages.add(errorMessage);
                }

                return errorMessages;
            }
        }

        /**
         * The Class ValidationHelper.
         */
        private class ValidationHelper {
            StateReportEntity m_entity;
            Set<StateReportValidationError> m_errors;
            FieldDefinition m_field;
            String m_value;

            /**
             * Instantiates a new validation helper.
             *
             * @param entity StateReportEntity
             * @param field FieldDefinition
             * @param value String
             */
            public ValidationHelper(StateReportEntity entity,
                    FieldDefinition field,
                    String value) {
                m_entity = entity;
                m_errors = new HashSet<StateReportValidationError>();
                m_field = field;
                m_value = value;
            }

            /**
             * Adds the beginning date errors.
             *
             * @throws ParseException exception
             */
            void addBeginningDateErrors() throws ParseException {
                addSchoolDateError();
            }

            /**
             * Adds the duration errors.
             */
            void addDurationErrors() {
                if (!StringUtils.isEmpty(m_value) && Integer.valueOf(m_value).intValue() > 6) {
                    String errorMessage = "The " + FIELD_PGM_DURATION
                            + " for Student Program Participation record should not be greater than 6";
                    String nonValidValues = m_field.getFieldId() + " = " + STYLE_BOLD + m_value + STYLE_END;
                    addError(errorMessage, nonValidValues);
                }

                String pgmCode = m_entity.getFieldValue(FIELD_PGM_CODE);
                if (StringUtils.isEmpty(m_value) && m_pgmCodesPTech.contains(pgmCode)) {
                    String errorMessage =
                            m_field.getFieldId() + " cannot be empty if " + FIELD_PGM_CODE + " is " + m_pgmCodesPTech;
                    String nonValidValues = m_field.getFieldId() + " = " + STYLE_BOLD + m_value + STYLE_END + ", " +
                            FIELD_PGM_CODE + " = " + STYLE_BOLD + pgmCode + STYLE_END;
                    addError(errorMessage, nonValidValues);
                }
            }

            /**
             * Adds the ending date errors.
             *
             * @throws ParseException exception
             */
            void addEndingDateErrors() throws ParseException {
                addSchoolDateError();

                Date endingDate = m_exportDateFormat.parse(m_value);

                String beginningDateExportValue = m_entity.getFieldValue(FIELD_PGM_BEGINNING_DATE);
                Date beginningDate = m_exportDateFormat.parse(beginningDateExportValue);
                if (endingDate.before(beginningDate)) {
                    String errorMessage = m_field.getFieldId() + " is earlier than " + FIELD_PGM_BEGINNING_DATE;
                    String nonValidValues = m_field.getFieldId() + " = " + STYLE_BOLD + m_value + STYLE_END + ", " +
                            FIELD_PGM_BEGINNING_DATE + " = " + STYLE_BOLD + beginningDateExportValue + STYLE_END;
                    addError(errorMessage, nonValidValues);
                }

                if (endingDate.after(m_reportDate)) {
                    String errorMessage = m_field.getFieldId() + " cannot be a future date.";
                    String nonValidValues = m_field.getFieldId() + " = " + STYLE_BOLD + m_value + STYLE_END + ", " +
                            "Report Date = " + STYLE_BOLD + m_exportDateFormat.format(m_reportDate) + STYLE_END;
                    addError(errorMessage, nonValidValues);
                }
            }

            /**
             * Adds the error.
             *
             * @param errorMessage String
             * @param nonValidValues String
             */
            void addError(String errorMessage, String nonValidValues) {
                m_errors.add(new StateReportValidationError(m_entity, m_field, errorMessage, nonValidValues));
            }

            /**
             * Adds the exit reason code errors.
             *
             * @throws ParseException exception
             */
            void addExitReasonCodeErrors() throws ParseException {
                addNonValidCodeError(m_exitReasonsCodes);

                String pgmCode = m_entity.getFieldValue(FIELD_PGM_CODE);
                addExitReasonCodeErrorForPgmCodes(pgmCode, m_pgmCodesCte, m_exitReasonsCodesCte);
                addExitReasonCodeErrorForPgmCodes(pgmCode, m_pgmCodesEllEligibility, m_exitReasonsCodesEllEligibility);
                addExitReasonCodeErrorForPgmCodes(pgmCode, m_pgmCodesDisability, m_exitReasonsCodesDisability);

                if (!StringUtils.isEmpty(m_value)) {
                    String endingDateExportValue = m_entity.getFieldValue(FIELD_PGM_ENDING_DATE);
                    if (m_pgmCodesWithRequiredExitDate.contains(pgmCode)) {
                        if (StringUtils.isEmpty(endingDateExportValue)) {
                            String errorMessage =
                                    FIELD_PGM_ENDING_DATE + " expected with non empty " + m_field.getFieldId() +
                                            " and " + FIELD_PGM_CODE + " " + m_pgmCodesWithRequiredExitDate;
                            String nonValidValues = FIELD_PGM_ENDING_DATE + " = " + STYLE_BOLD + endingDateExportValue
                                    + STYLE_END + ", " +
                                    FIELD_PGM_CODE + " = " + STYLE_BOLD + pgmCode + STYLE_END + ", " +
                                    m_field.getFieldId() + " = " + STYLE_BOLD + m_value + STYLE_END;
                            addError(errorMessage, nonValidValues);
                        }
                    }

                    if ("3011".equals(m_value)) {
                        if (StringUtils.isEmpty(endingDateExportValue) ||
                                !m_exportDateFormat.parse(endingDateExportValue).equals(m_june30)) {
                            String errorMessage =
                                    m_field.getFieldId() + " requires an " + FIELD_PGM_ENDING_DATE + " of " +
                                            m_exportDateFormat.format(m_june30);
                            String nonValidValues =
                                    m_field.getFieldId() + " = " + STYLE_BOLD + m_value + STYLE_END + ", " +
                                            FIELD_PGM_ENDING_DATE + " = " + STYLE_BOLD + endingDateExportValue
                                            + STYLE_END;
                            addError(errorMessage, nonValidValues);
                        }
                    }

                    if (m_pgmCodesCdos.contains(pgmCode) && !m_exitReasonsCodesCdos.contains(m_value)) {
                        String errorMessage = m_field.getFieldId() + " equal to " + m_exitReasonsCodesCdos +
                                " required for ended CDOS " + m_pgmCodesCdos + " program record";
                        String nonValidValues = m_field.getFieldId() + " = " + STYLE_BOLD + m_value + STYLE_END + ", " +
                                FIELD_PGM_CODE + " = " + STYLE_BOLD + pgmCode + STYLE_END;
                        addError(errorMessage, nonValidValues);
                    }

                    if (m_pgmCodesParentInArmedForces.contains(pgmCode)
                            && !m_exitReasonsCodesParentInArmedForces.contains(m_value)) {
                        String errorMessage =
                                m_field.getFieldId() + " equal to " + m_exitReasonsCodesParentInArmedForces +
                                        " required for ended Armed Forces " + m_pgmCodesParentInArmedForces
                                        + " program record";
                        String nonValidValues = m_field.getFieldId() + " = " + STYLE_BOLD + m_value + STYLE_END + ", " +
                                FIELD_PGM_CODE + " = " + STYLE_BOLD + pgmCode + STYLE_END;
                        addError(errorMessage, nonValidValues);
                    }

                    if ("646".equals(m_value)) {
                        String pgmIntensity = m_entity.getFieldValue(FIELD_PGM_INTENSITY);
                        if (!"Concentrator".equalsIgnoreCase(pgmIntensity)) {
                            String errorMessage =
                                    m_field.getFieldId() + " 646 requires a " + FIELD_PGM_INTENSITY + " = Concentrator";
                            String nonValidValues =
                                    m_field.getFieldId() + " = " + STYLE_BOLD + m_value + STYLE_END + ", " +
                                            FIELD_PGM_INTENSITY + " = " + STYLE_BOLD + pgmIntensity + STYLE_END;
                            addError(errorMessage, nonValidValues);
                        }
                    }
                }
            }

            /**
             * Adds the exit reason code simple errors.
             */
            void addExitReasonCodeSimpleErrors() {
                addNonValidCodeError(m_exitReasonsCodes);
            }

            /**
             * Adds the part info errors.
             */
            void addPartInfoErrors() {
                String pgmCode = m_entity.getFieldValue(FIELD_PGM_CODE);
                if (m_pgmCodesCte.contains(pgmCode) && StringUtils.isEmpty(m_value)) {
                    String errorMessage = m_field.getFieldId() + " required for CTE " + m_pgmCodesCte + " programs";
                    String nonValidValues = m_field.getFieldId() + " = " + STYLE_BOLD + m_value + STYLE_END + ", " +
                            FIELD_PGM_CODE + " = " + STYLE_BOLD + pgmCode + STYLE_END;
                    addError(errorMessage, nonValidValues);
                }

                if (!StringUtils.isEmpty(m_value) && !m_pgmCodesCte.contains(pgmCode)) {
                    String errorMessage = "No " + m_field.getFieldId() + " expected for a non-CTE programs. "
                            + "CTE program codes: " + m_pgmCodesCte;
                    String nonValidValues = m_field.getFieldId() + " = " + STYLE_BOLD + m_value + STYLE_END + ", " +
                            FIELD_PGM_CODE + " = " + STYLE_BOLD + pgmCode + STYLE_END;
                    addError(errorMessage, nonValidValues);
                }
            }

            /**
             * Adds the pgm code errors.
             */
            void addPgmCodeErrors() {
                addNonValidCodeError(m_pgmCodes);

                if (m_pgmCodesCte.contains(m_value)) {
                    String pgmIntensity = m_entity.getFieldValue(FIELD_PGM_INTENSITY);
                    if (!"concentrator".equalsIgnoreCase(pgmIntensity)) {
                        String errorMessage = m_field.getFieldId() + " " + m_pgmCodesCte + " with " +
                                FIELD_PGM_INTENSITY + " = concentrator is required";
                        String nonValidValues = m_field.getFieldId() + " = " + STYLE_BOLD + m_value + STYLE_END + ", " +
                                FIELD_PGM_INTENSITY + " = " + STYLE_BOLD + pgmIntensity + STYLE_END;
                        addError(errorMessage, nonValidValues);
                    }
                }
            }

            /**
             * Adds the pgm code errors.
             */
            void addPgmElCodeErrors() {
                if (StringUtils.isEmpty(m_value)) {
                    String prgmCode = m_entity.getFieldValue(FIELD_PGM_CODE);
                    if (m_pgmFRPLCodes.contains(prgmCode)) {
                        String errorMessage =
                                m_field.getFieldId() + " field, each FRPL (Code" + m_pgmFRPLCodes
                                        + " ) program code must have at least one Eligibility code ";
                        String nonValidValues = m_field.getFieldId() + " = " + STYLE_BOLD + m_value + STYLE_END + ", " +
                                FIELD_PGM_CODE + " = " + STYLE_BOLD + prgmCode + STYLE_END;
                        addError(errorMessage, nonValidValues);

                    }
                }
            }

            /**
             * Adds the pgm intencity errors.
             */
            void addPgmIntencityErrors() {
                String intensityPgmCode = m_entity.getFieldValue(FIELD_PGM_INTENSITY);
                String pgmCode = m_entity.getFieldValue(FIELD_PGM_CODE);

                if (StringUtils.isEmpty(m_value) && m_pgmCodesCte.contains(intensityPgmCode)) {
                    String errorMessage =
                            m_field.getFieldId() + " is required if " + FIELD_PGM_CODE + " = " + m_pgmCodesCte;
                    String nonValidValues = m_field.getFieldId() + " = " + STYLE_BOLD + m_value + STYLE_END + ", " +
                            FIELD_PGM_CODE + " = " + STYLE_BOLD + intensityPgmCode + STYLE_END;
                    addError(errorMessage, nonValidValues);
                }


                if (m_pgmCodesCte.contains(pgmCode) && StringUtils.isEmpty(m_value)) {
                    String errorMessage = m_field.getFieldId() + " required for CTE " + m_pgmCodesCte + " programs";
                    String nonValidValues = m_field.getFieldId() + " = " + STYLE_BOLD + m_value + STYLE_END + ", " +
                            FIELD_PGM_CODE + " = " + STYLE_BOLD + pgmCode + STYLE_END;
                    addError(errorMessage, nonValidValues);
                }

                if (m_pgmELLServiceLevelCodes.contains(m_value) && !m_pgmCodesEllEligibility.contains(pgmCode)) {
                    String errorMessage =
                            m_field.getFieldId()
                                    + " Students must have an 0231 (ELL Eligible) Program Service Code to report ELL service level";
                    String nonValidValues = m_field.getFieldId() + " = " + STYLE_BOLD + m_value + STYLE_END + ", " +
                            FIELD_PGM_CODE + " = " + STYLE_BOLD + pgmCode + STYLE_END;
                    addError(errorMessage, nonValidValues);
                }
            }

            /**
             * Adds the pgm intencity errors.
             */
            void addPgmProviderTypeErrors() {
                String pgmCode = m_entity.getFieldValue(FIELD_PGM_CODE);
                if (m_pgmCodesHNR.contains(pgmCode) && StringUtils.isEmpty(m_value)) {
                    String errorMessage =
                            m_field.getFieldId() + " required for Student Program 8262 " + m_pgmCodesHNR + " programs";
                    String nonValidValues = m_field.getFieldId() + " = " + STYLE_BOLD + m_value + STYLE_END + ", " +
                            FIELD_PGM_CODE + " = " + STYLE_BOLD + pgmCode + STYLE_END;
                    addError(errorMessage, nonValidValues);
                }
            }

            /**
             * Adds the non valid code error.
             *
             * @param codes Collection<String>
             */
            private void addNonValidCodeError(Collection<String> codes) {
                if (!codes.contains(m_value)) {
                    String errorMessage = m_field.getFieldId() + " must be a valid state code " + codes;
                    String nonValidValues = m_field.getFieldId() + " = " + STYLE_BOLD + m_value + STYLE_END;
                    addError(errorMessage, nonValidValues);
                }
            }

            /**
             * Adds the school date error.
             *
             * @throws ParseException exception
             */
            private void addSchoolDateError() throws ParseException {
                Date date = m_exportDateFormat.parse(m_value);
                if (date.before(m_july01) || date.after(m_june30)) {
                    String errorMessage =
                            m_field.getFieldId() + " must fall between " + m_exportDateFormat.format(m_july01) +
                                    " and " + m_exportDateFormat.format(m_june30);
                    String nonValidValues = m_field.getFieldId() + " = " + STYLE_BOLD + m_value + STYLE_END;
                    addError(errorMessage, nonValidValues);
                }
            }

            /**
             * Adds the exit reason code error for pgm codes.
             *
             * @param pgmCode String
             * @param pgmCodes Collection<String>
             * @param exitReasonCodes Collection<String>
             */
            private void addExitReasonCodeErrorForPgmCodes(String pgmCode,
                                                           Collection<String> pgmCodes,
                                                           Collection<String> exitReasonCodes) {
                if (pgmCodes.contains(pgmCode) && !exitReasonCodes.contains(m_value)) {
                    m_errors.add(new StateReportValidationError(m_entity, m_field,
                            m_field.getFieldId() + " must be a valid state code " + m_exitReasonsCodesCte +
                                    " if " + FIELD_PGM_CODE + " is " + pgmCodes,
                            FIELD_PGM_CODE + " = " + STYLE_BOLD + pgmCode + STYLE_END + ", " +
                                    m_field.getFieldId() + " = " + STYLE_BOLD + m_value + STYLE_END));
                }
            }
        }

        PlainDate m_july01;
        PlainDate m_june30;

        Collection m_exitReasonsCodes;

        Collection m_exitReasonsCodesCdos;
        Collection m_exitReasonsCodesCte;
        Collection m_exitReasonsCodesDisability;
        Collection m_exitReasonsCodesEllEligibility;
        Collection m_exitReasonsCodesParentInArmedForces;

        Collection<String> m_pgmCodes;

        Collection<String> m_pgmCodesCdos;
        Collection<String> m_pgmCodesCte;
        Collection<String> m_pgmCodesDisability;
        Collection<String> m_pgmCodesEll;
        Collection<String> m_pgmCodesEllEligibility;
        Collection<String> m_pgmCodesHNR;
        Collection<String> m_pgmELLServiceLevelCodes;
        Collection<String> m_pgmCodesParentInArmedForces;
        Collection<String> m_pgmCodesPTech;
        Collection<String> m_pgmCodesSafetyNet;
        Collection<String> m_pgmFRPLCodes;

        Collection m_pgmCodesWithEligibility;
        Collection m_pgmCodesWithRequiredExitDate;

        private boolean m_addCrossRecord;
        private CrossRecordsValidator m_crossRecordsValidator;
        private int m_previousRecordNum;

        /**
         * Instantiates a new validate program.
         */
        public ValidateProgram() {
            Calendar julyCal = Calendar.getInstance();
            julyCal.set(Calendar.DATE, 1);
            julyCal.set(Calendar.MONTH, 6);
            julyCal.set(Calendar.YEAR, getCurrentContext().getSchoolYear() - 1);
            m_july01 = new PlainDate(julyCal.getTime());

            Calendar juneCal = Calendar.getInstance();
            juneCal.set(Calendar.DATE, 30);
            juneCal.set(Calendar.MONTH, 5);
            juneCal.set(Calendar.YEAR, getCurrentContext().getSchoolYear());
            m_june30 = new PlainDate(juneCal.getTime());

            m_exitReasonsCodes = new ArrayList();
            m_exitReasonsCodesCdos = Arrays.asList("700", "701");
            m_exitReasonsCodesCte = Arrays.asList("646", "663", "680");
            m_exitReasonsCodesDisability = Arrays.asList("901", "912");
            m_exitReasonsCodesEllEligibility = Arrays.asList("3011", "3022", "3045");
            m_exitReasonsCodesParentInArmedForces = Arrays.asList("4000");
            m_exitReasonsCodes.addAll(m_exitReasonsCodesCte);
            m_exitReasonsCodes.addAll(m_exitReasonsCodesDisability);
            m_exitReasonsCodes.addAll(m_exitReasonsCodesEllEligibility);
            m_exitReasonsCodes.addAll(m_exitReasonsCodesCdos);
            m_exitReasonsCodes.addAll(m_exitReasonsCodesParentInArmedForces);
            m_pgmELLServiceLevelCodes = new ArrayList(Arrays.asList("Full", "Partial", "None"));

            m_pgmCodes = new ArrayList<String>(Arrays.asList("5533", "5577", "0286", "0411", "0330", "0187", "8327",
                    "5742", "5566", "5872",
                    "5883", "7022", "7033", "902", "990", "1309", "1320", "1331", "1342",
                    "1353", "1364", "1375", "1386", "1397", "1408", "1419", "0803", "0814", "0825", "0836",
                    "0847", "0858", "0869",
                    "0880", "0891", "4004", "4015", "4037", "0198", "0220", "0242", "0264", "1232", "2618",
                    "5753",
                    "5817", "5806", "8272", "8282", "8300", "8312", "0803", "0814", "0825", "0836", "0847",
                    "0858", "0869", "0880",
                    "0891", "1309", "1320", "1331", "1342", "1353", "1364", "1375", "1386", "1397", "1408",
                    "1419", "2751", "2752",
                    "2753", "2754", "2755", "2756", "2757", "2758", "2759", "2760", "2761", "2861", "2862",
                    "2863", "2864", "2865",
                    "2866", "2867", "2868", "2869", "2870", "2871", "8262"));
            m_pgmCodesCdos = new ArrayList<String>(Arrays.asList("8271"));
            m_pgmCodesCte = new ArrayList<String>(Arrays.asList("8261"));
            m_pgmCodesHNR = new ArrayList<String>(Arrays.asList("8262"));
            m_pgmCodesDisability = new ArrayList<String>(
                    Arrays.asList("5786", "0352", "0363", "0385", "0396", "0407", "0418", "0429", "0440",
                            "0451", "0462", "0473", "0484", "0495"));
            m_pgmCodesEll = new ArrayList<String>(Arrays.asList("5709", "5676", "5687", "8239", "5720", "5731"));
            m_pgmCodesEllEligibility = new ArrayList<String>(Arrays.asList("0231"));
            m_pgmCodesParentInArmedForces = new ArrayList<String>(Arrays.asList("8292"));
            m_pgmCodesPTech = new ArrayList<String>(Arrays.asList("4026", "4027"));
            m_pgmCodesSafetyNet = new ArrayList<String>(Arrays.asList("0550", "0572", "0583", "0594", "0605", "5775"));
            m_pgmFRPLCodes = new ArrayList<String>(Arrays.asList("5817", "5806"));
            m_pgmCodes.addAll(m_pgmCodesCdos);
            m_pgmCodes.addAll(m_pgmCodesCte);
            m_pgmCodes.addAll(m_pgmCodesDisability);
            m_pgmCodes.addAll(m_pgmCodesEllEligibility);
            m_pgmCodes.addAll(m_pgmCodesParentInArmedForces);
            m_pgmCodes.addAll(m_pgmCodesPTech);
            m_pgmCodes.addAll(m_pgmCodesEll);

            m_pgmCodesWithRequiredExitDate = new ArrayList<String>();
            m_pgmCodesWithRequiredExitDate.addAll(m_pgmCodesCdos);
            m_pgmCodesWithRequiredExitDate.addAll(m_pgmCodesCte);
            m_pgmCodesWithRequiredExitDate.addAll(m_pgmCodesDisability);
            m_pgmCodesWithRequiredExitDate.addAll(m_pgmCodesEllEligibility);
            m_pgmCodesWithRequiredExitDate.addAll(m_pgmCodesParentInArmedForces);

            m_pgmCodesWithEligibility = Arrays.asList("5709", "8239", "1232");

            m_crossRecordsValidator = new CrossRecordsValidator();

            m_previousRecordNum = -1;
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection<StateReportValidationError> getFieldValidation(StateReportData data,
                                                                         StateReportEntity entity,
                                                                         FieldDefinition field,
                                                                         String value) {
            ValidationHelper validationHelper = new ValidationHelper(entity, field, value);

            m_addCrossRecord = m_previousRecordNum != entity.getCurrentRow();

            if (m_addCrossRecord) {
                String stdOid = ((StudentProgramParticipation) entity.getBean()).getStudentOid();
                String pgmCode = entity.getFieldValue(FIELD_PGM_CODE);

                String startDateExportValue = entity.getFieldValue(FIELD_PGM_BEGINNING_DATE);
                Date start = null;
                if (!StringUtils.isEmpty(startDateExportValue)) {
                    try {
                        start = m_exportDateFormat.parse(startDateExportValue);
                    } catch (ParseException e) {
                        e.printStackTrace();
                    }
                }

                String endDateExportValue = entity.getFieldValue(FIELD_PGM_ENDING_DATE);
                Date end = null;
                if (!StringUtils.isEmpty(endDateExportValue)) {
                    try {
                        end = m_exportDateFormat.parse(endDateExportValue);
                    } catch (ParseException e) {
                        e.printStackTrace();
                    }
                }

                m_crossRecordsValidator.addPgmPeriodRecord(stdOid, pgmCode, start, end);
            }

            if (FIELD_PGM_CODE.equals(field.getFieldId())) {
                validationHelper.addPgmCodeErrors();
            }

            if (FIELD_PGM_ELGB_CODE1.equals(field.getFieldId())) {
                validationHelper.addPgmElCodeErrors();
            }


            if (FIELD_PGM_BEGINNING_DATE.equals(field.getFieldId())) {
                try {
                    validationHelper.addBeginningDateErrors();
                } catch (ParseException e) {
                    e.printStackTrace();
                }
            }

            if (FIELD_PGM_ENDING_DATE.equals(field.getFieldId())) {
                if (!StringUtils.isEmpty(value)) {
                    try {
                        validationHelper.addEndingDateErrors();
                    } catch (ParseException e) {
                        e.printStackTrace();
                    }
                }
            }

            if (FIELD_PGM_EXIT_REASON_CODE_1.equals(field.getFieldId())) {
                if (!StringUtils.isEmpty(value)) {
                    try {
                        validationHelper.addExitReasonCodeErrors();
                    } catch (ParseException e) {
                        e.printStackTrace();
                    }
                }
            }

            if (FIELD_PGM_EXIT_REASON_CODE_2.equals(field.getFieldId())) {
                validationHelper.addExitReasonCodeSimpleErrors();
            }

            if (FIELD_PGM_EXIT_REASON_CODE_3.equals(field.getFieldId())) {
                validationHelper.addExitReasonCodeSimpleErrors();
            }

            if (FIELD_PGM_DURATION.equals(field.getFieldId())) {
                validationHelper.addDurationErrors();
            }

            if (FIELD_PGM_PART_INFO_CODE.equals(field.getFieldId())) {
                validationHelper.addPartInfoErrors();
            }

            if (FIELD_PGM_INTENSITY.equals(field.getFieldId())) {
                validationHelper.addPgmIntencityErrors();
            }

            if (FIELD_PGM_PROVIDER_TYPE.equals(field.getFieldId())) {
                validationHelper.addPgmProviderTypeErrors();
            }

            if (entity.getCurrentRow() + 1 == entity.getRowCount()) {
                String stdOid = ((StudentProgramParticipation) entity.getBean()).getStudentOid();
                m_crossRecordsValidator.validateAllRecords(stdOid);
            }

            return validationHelper.m_errors;
        }
    }


    public class EligibilityCodeRetriever implements FieldRetriever {

        private static final String CAL_ID = "PGM-ELIG";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.getFieldValue()
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field) {

            String param = (String) field.getParameter();
            Object value = null;
            X2BaseBean bean = entity.getBean();
            String beanValue = (String) bean.getFieldValueByBeanPath(field.getBeanPath());
            if (!StringUtils.isEmpty(param) && !StringUtils.isEmpty(beanValue) && StringUtils.isNumeric(param)) {
                int index = Integer.valueOf(param.trim()).intValue();
                String[] elegigilityValues = beanValue.split(",");
                if (elegigilityValues.length >= index + 1) {
                    value = elegigilityValues[index];
                }
            }

            return value;
        }
    }

    /**
     * Aliases
     */
    protected static final String ALIAS_ENR_LOC_OVERRIDE = "DOE LOCATION OVERRIDE";
    protected static final String ALIAS_EXCLUDE_SCHOOL = "DOE EXCLUDE SKL";
    protected static final String ALIAS_PGM_DURATION = "all-pgm-ProgramDuration";
    protected static final String ALIAS_PGM_EXIT_REASON_1 = "DOE PGM EXIT REASON 1";
    protected static final String ALIAS_PGM_EXIT_REASON_2 = "DOE PGM EXIT REASON 2";
    protected static final String ALIAS_PGM_EXIT_REASON_3 = "DOE PGM EXIT REASON 3";
    protected static final String ALIAS_SKL_LOCATION_CODE = "LOCATION CODE";

    /**
     * Other constants
     */
    protected static final String FIELD_PGM_BEGINNING_DATE = "BEGINNING DATE";
    protected static final String FIELD_PGM_CODE = "PROGRAM CODE";
    protected static final String FIELD_PGM_DURATION = "PROGRAM DURATION";
    protected static final String FIELD_PGM_ELGB_CODE1 = "ProgramElgbCode1";
    protected static final String FIELD_PGM_ENDING_DATE = "ENDING DATE";
    protected static final String FIELD_PGM_EXIT_REASON_CODE_1 = "ENTRY REASON CODE 1";
    protected static final String FIELD_PGM_EXIT_REASON_CODE_2 = "ENTRY REASON CODE 2";
    protected static final String FIELD_PGM_EXIT_REASON_CODE_3 = "ENTRY REASON CODE 3";
    protected static final String FIELD_PGM_INTENSITY = "PROGRAM INTENSITY";
    protected static final String FIELD_PGM_PART_INFO_CODE = "PARTICIPATION INFO";
    protected static final String FIELD_PGM_PROVIDER_TYPE = "PGM PROVIDER TYPE";

    /**
     * Constants for reporting information.
     */
    protected static final String PARAM_END_DATE = "END";
    protected static final String PARAM_EXCLUDE_SCHOOL = "excludeSchool";
    protected static final String PARAM_PGM_DURATION = "PGM_DURATION";
    protected static final String PARAM_PGM_EXIT_REASON_1 = "EXIT_REASON_1";
    protected static final String PARAM_PGM_EXIT_REASON_2 = "EXIT_REASON_2";
    protected static final String PARAM_PGM_EXIT_REASON_3 = "EXIT_REASON_3";
    protected static final String PARAM_REMOVE_HEADER = "removeHeader";
    protected static final String PARAM_REPORT_DATE = "reportDate";
    protected static final String PARAM_START_DATE = "START";

    private static final String PROCEDURE_ID_NY_EE = "EXPDATA-NY-EE";
    private static final String PROCEDURE_ID_NY_STDLITE = "EXPDATA-NY-STDLITE";

    /**
     * Local variables for reporting information.
     */
    protected String m_activeCode;
    protected StateReportData m_dataEe;
    protected StateReportData m_dataStdLite;
    protected String m_excludeSchool;
    protected boolean m_excludeSchoolIndicator = false;
    protected SimpleDateFormat m_exportDateFormat;
    protected String m_fieldEnrLocOverride;
    protected String m_fieldPgmExitReason1;
    protected String m_fieldPgmExitReason2;
    protected String m_fieldPgmExitReason3;
    protected String m_fieldSklLocationCode;
    protected StudentHistoryHelper m_helper;
    protected String m_pgmDurationPath;
    protected boolean m_removeHeaderIndicator;
    protected PlainDate m_reportDate;

    /**
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getHeading()
     */
    @Override
    public String getHeading() {
        if (m_removeHeaderIndicator) {
            return null;
        }

        return super.getHeading();
    }

    /**
     * Initializes the data module.
     *
     * @throws X2BaseException exception
     */
    @Override
    public void initialize() throws X2BaseException {
        initializeFields();

        /*
         * Build helper object.
         */
        m_helper = new StudentHistoryHelper(this);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);

        m_exportDateFormat = new SimpleDateFormat("yyyy-MM-dd");

        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {
            m_parameters.put(PARAM_REPORT_DATE, getParameter(PARAM_REPORT_DATE));
            m_parameters.put(PARAM_EXCLUDE_SCHOOL, getParameter(PARAM_EXCLUDE_SCHOOL));

            m_dataStdLite = new NYStudentLite();
            initReportData(PROCEDURE_ID_NY_STDLITE, m_dataStdLite);

            m_dataEe = new NYStudentEntryExit();
            initReportData(PROCEDURE_ID_NY_EE, m_dataEe);

            // Set the query to be used for student selection.
            Criteria studentCriteria = m_helper.getStudentCriteria();
            SubQuery studentSubQuery = new SubQuery(Student.class, X2BaseBean.COL_OID, studentCriteria);
            Criteria programCriteria = getProgramCriteria();
            programCriteria.addIn(StudentProgramParticipation.COL_STUDENT_OID, studentSubQuery);
            PlainDate today = new PlainDate(OrganizationManager.getTimeZone(getOrganization()));
            programCriteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, today);
            if (!isSchoolContext()) {
                programCriteria.addNotEqualTo(StudentProgramParticipation.REL_STUDENT + ModelProperty.PATH_DELIMITER
                        + Student.REL_SCHOOL + ModelProperty.PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR,
                        BooleanAsStringConverter.TRUE);
                programCriteria.addNotEqualTo(StudentProgramParticipation.REL_STUDENT + ModelProperty.PATH_DELIMITER
                        + Student.REL_SCHOOL + ModelProperty.PATH_DELIMITER + SisSchool.COL_ARCHIVE_INDICATOR,
                        BooleanAsStringConverter.TRUE);
            }
            programCriteria.addNotEqualTo(StudentProgramParticipation.REL_STUDENT + ModelProperty.PATH_DELIMITER
                    + Student.REL_SCHOOL + ModelProperty.PATH_DELIMITER + m_excludeSchool,
                    BooleanAsStringConverter.TRUE);
            QueryByCriteria query = new QueryByCriteria(StudentProgramParticipation.class, programCriteria);
            applyInputSort(query, null);
            setQuery(query);
            setEntityClass(NYProgramsFactEntity.class);

            // Build a map of calculations/retrievers
            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put(RetrieveProgram.CAL_ID, new RetrieveProgram());
            calcs.put(EligibilityCodeRetriever.CAL_ID, new EligibilityCodeRetriever());
            calcs.put(RetrieveLocationCode.CAL_ID, new RetrieveLocationCode());
            super.addCalcs(calcs);

            HashMap validators = new HashMap<String, FieldValidator>();
            validators.put("PGM-VALIDATE", new ValidateProgram());
            super.addValidators(validators);
        }
    }

    /**
     * Returns the criteria that retrieves all programs that should be included in the export.
     *
     * @return Criteria
     */
    private Criteria getProgramCriteria() {
        PlainDate schoolYearStartDate = getOrganization().getCurrentContext().getStartDate();

        Criteria criteria = new Criteria();
        criteria.addNotNull(StudentProgramParticipation.COL_PROGRAM_CODE);
        criteria.addNotNull(StudentProgramParticipation.COL_START_DATE);

        Criteria endDate1Criteria = new Criteria();
        endDate1Criteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_END_DATE, schoolYearStartDate);

        Criteria endDate2Criteria = new Criteria();
        endDate2Criteria.addIsNull(StudentProgramParticipation.COL_END_DATE);

        endDate1Criteria.addOrCriteria(endDate2Criteria);

        criteria.addAndCriteria(endDate1Criteria);

        return criteria;
    }

    /**
     * Initialize Fields.
     */
    private void initializeFields() {
        m_removeHeaderIndicator = false;
        if (getParameter(PARAM_REMOVE_HEADER) != null) {
            m_removeHeaderIndicator = ((Boolean) getParameter(PARAM_REMOVE_HEADER)).booleanValue();
        }
        m_excludeSchoolIndicator = ((Boolean) getParameter(PARAM_EXCLUDE_SCHOOL)).booleanValue();
        m_reportDate = (PlainDate) getParameter(PARAM_REPORT_DATE);
        if (m_reportDate == null) {
            m_reportDate = new PlainDate();
        }

        m_excludeSchool = translateAliasToJavaName(ALIAS_EXCLUDE_SCHOOL, true);
        m_pgmDurationPath = translateAliasToJavaName(ALIAS_PGM_DURATION, true);
        m_activeCode =
                PreferenceManager.getPreferenceValue(getOrganization(), SystemPreferenceDefinition.STUDENT_ACTIVE_CODE);
        m_fieldPgmExitReason1 = translateAliasToJavaName(ALIAS_PGM_EXIT_REASON_1, false);
        m_fieldPgmExitReason2 = translateAliasToJavaName(ALIAS_PGM_EXIT_REASON_2, false);
        m_fieldPgmExitReason3 = translateAliasToJavaName(ALIAS_PGM_EXIT_REASON_3, false);
        m_fieldEnrLocOverride = translateAliasToJavaName(ALIAS_ENR_LOC_OVERRIDE, true);
        m_fieldSklLocationCode = translateAliasToJavaName(ALIAS_SKL_LOCATION_CODE, true);
    }

    /**
     * Inits the report data.
     *
     * @param procedureId String
     * @param stateReportData StateReportData
     * @throws X2BaseException exception
     */
    private void initReportData(String procedureId, StateReportData stateReportData) throws X2BaseException {
        ArrayList<StateReportValidationError> m_initErrors = new ArrayList<StateReportValidationError>();
        Procedure procedure = (Procedure) ToolManager.getToolForId(Tool.TYPE_PROCEDURE, procedureId, getBroker());
        if (procedure == null) {
            throw new ToolRunException("Procedure with ID = " + procedureId + " could not be found and is required");
        }
        /*
         * Since we are using the tool loader, we cannot compile the procedure separately from the
         * class compiled with the report
         * The new operator must be used to create the instance.
         */
        ToolSourceCode sourceCode = procedure.getSourceCode();
        if (sourceCode != null) {
            stateReportData.setInputDefinition(sourceCode.getInputDefinition());
        }
        stateReportData.setProcedureId(procedureId);
        m_initErrors.addAll(stateReportData.loadDefinitions(procedureId, getBroker()));

        // Initialize the report data object.
        stateReportData.setBroker(getBroker());
        stateReportData.setCurrentContext(getCurrentContext());
        stateReportData.setOrganization(getOrganization());
        stateReportData.setPrivilegeSet(getPrivilegeSet());
        stateReportData.setSchoolContext(isSchoolContext());
        stateReportData.setSchool(getSchool());
        stateReportData.setParameters(m_parameters);
        stateReportData.setUser(getUser());
        stateReportData.initializeExport();
    }
}
