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

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.Race;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.SchoolCalendar;
import com.follett.fsc.core.k12.beans.SchoolCalendarDate;
import com.follett.fsc.core.k12.beans.Staff;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StaffDegree;
import com.x2dev.sis.model.beans.StaffPosition;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * New York state procedure for Staff Snapshot Export.
 *
 * @author X2 Development Corporation
 */

public class NYStaffSnapshot extends StateReportData {
    /**
     * Entity class for Staff Snapshot Export.
     *
     * @author X2 Development Corporation
     */

    public static class NYStaffSnapshotEntity extends StateReportEntity {
        /**
         * Instantiates a new NY staff snapshot entity.
         */
        public NYStaffSnapshotEntity() {
            // no argument constructor
        }

        /**
         * Initialize the data module.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);
        }

        /**
         * Returns the Race collection for the student in this entity.
         *
         * @return Collection<Race>
         */
        public Collection<Race> getRaces() {
            SisStaff staff = (SisStaff) getBean();
            Collection<Race> races = null;
            SisPerson person = staff.getPerson();
            if (person != null) {
                races = person.getRaces();
            }

            return races;
        }

        /**
         * Returns the current staff.
         *
         * @return SisStaff
         */
        public SisStaff getStaff() {
            return (SisStaff) getBean();
        }
    }

    /**
     * Retrieves primary location.</br>
     * Primary Location should reflect '0000' if staff teaches at multiple schools throughout the
     * school year,
     * otherwise current school location for staff.
     *
     */
    protected class RetrievePrimaryLocation implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            Object value = field.getDefaultValue();
            SisStaff staff = (SisStaff) entity.getBean();
            String stfOid = staff.getOid();
            NYStaffSnapshot sftData = (NYStaffSnapshot) data;

            Collection<StaffPosition> sfps = sftData.m_staffPositionMap.get(stfOid);

            if (sfps != null && !sfps.isEmpty()) {
                SisSchool tempSchool = null;
                int schoolsCount = 1;

                for (StaffPosition sfp : sfps) {
                    if (tempSchool == null) {
                        tempSchool = sfp.getSchool();
                    }

                    if (!sfp.getSchoolOid().equals(tempSchool.getOid())) {
                        tempSchool = sfp.getSchool();
                        schoolsCount += 1;
                        break;
                    }
                }

                if (schoolsCount == 1 && tempSchool != null) {
                    value = tempSchool.getFieldValueByAlias(ALIAS_SKL_LOC_CODE);
                }
            }

            return value;
        }
    }

    /**
     * Retrieves the Race field in order of importance as input for the StudentLiteEntity.
     */
    protected class RetrieveRace implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String param = (String) field.getParameter();
            Collection<Race> races = ((NYStaffSnapshotEntity) entity).getRaces();
            Race found = null;
            Object value = null;
            int count = 0;
            // Find the reference code that we are looking for.
            if (param != null) {
                for (Race race : races) {
                    count++;
                    if (Integer.toString(count).equals(param)) {
                        found = race;
                        break;
                    }
                }
            }
            if (found != null) {
                value = data.lookupReferenceCodeByBeanPath(Race.class, Race.COL_RACE_CODE, found.getRaceCode(),
                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
            }

            return value;
        }
    }

    /**
     * Retrieves the status for the given StaffSnapshotEntity.
     */
    protected class RetrieveStaffStatus implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            Object value = null;
            NYStaffSnapshotEntity staffSnapshotEntity = ((NYStaffSnapshotEntity) entity);
            SisStaff staff = staffSnapshotEntity.getStaff();
            String param = (String) field.getParameter();
            String staffOid = staff.getOid();

            if (CALC_PARAM_STATUS.equals(param)) {
                if (m_staffPositionMap != null && m_staffPositionMap.containsKey(staffOid)) {
                    // We will populate this field as A or I depending on if the staff has a
                    // position the last day of the school year
                    StaffPosition sfp = getLatestStaffPosition(staffOid);
                    PlainDate sfpEndDate = (PlainDate) sfp.getFieldValueByBeanPath(m_fieldSfpEndDate);
                    value = CODE_STAFF_STATUS_INACTIVE;

                    if (sfpEndDate == null || getCurrentContext().getEndDate().equals(sfpEndDate)) {
                        value = CODE_STAFF_STATUS_ACTIVE;
                    }
                }
            } else if (CALC_PARAM_EXIT_DATE.equalsIgnoreCase(param)) {
                StaffPosition sfp = getLatestStaffPosition(staffOid);
                if (sfp != null
                        && BooleanAsStringConverter.TRUE.equals(sfp.getFieldValueByBeanPath(m_fieldSfpExitDate))) {
                    value = sfp.getFieldValueByBeanPath(m_fieldSfpEndDate);
                }
            }

            return value;
        }
    }

    /**
     * Staff table, type on the export there are two fields one if for teacher, one is for principal
     * so, if staff type in staff table= teacher, export teacher in field 80 job description [DOE
     * Position Type] otherwise leave blank
     *
     * If staff type=principal on the staff table, type field, export Principal in the field 1050
     * Second position field
     *
     * iF staff type =principal export hire date in second hire date position.
     */
    protected class RetrieveStaffPrincipalData implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData rootData, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object value = null;
            NYStaffSnapshot data = (NYStaffSnapshot) rootData;
            NYStaffSnapshotEntity staffSnapshotEntity = ((NYStaffSnapshotEntity) entity);
            SisStaff staff = staffSnapshotEntity.getStaff();
            String param = (String) field.getParameter();
            String staffType = staff.getStaffType();
            String staffOid = staff.getOid();
            boolean principal = false;
            boolean teacher = false;

            if (staffType != null) {
                principal = CODE_STAFF_TYPE_PRINCIPAL.equalsIgnoreCase(staffType)
                        || CODE_STAFF_TYPE_ADMINISTRATOR.equalsIgnoreCase(staffType);
                teacher = CODE_STAFF_TYPE_TEACHER.equalsIgnoreCase(staffType);
            }

            if (CALC_PARAM_JOB_DESCRIPTION.equals(param)) {
                if (teacher) {
                    value = CODE_STAFF_TYPE_TEACHER;
                }
            } else if (CALC_PARAM_ITINERANT_TEACHER.equals(param)) {
                if (teacher) {
                    if (m_staffPositionMap != null && m_staffPositionMap.containsKey(staffOid)) {
                        Collection<StaffPosition> staffPositions = m_staffPositionMap.get(staffOid);
                        PlainDate sfpDate = null;
                        StaffPosition sfpToRetrieveValue = null;
                        int countPrimaryPos = 0;
                        for (StaffPosition staffPosition : staffPositions) {
                            Boolean primaryPos =
                                    (Boolean) data.getPropertyAsJavaType(staffPosition, m_fieldPrimaryPositon);

                            if (primaryPos != null && primaryPos.booleanValue()) {
                                countPrimaryPos += 1;

                                if (sfpDate == null || sfpDate.before(staffPosition.getStartDate())) {
                                    sfpDate = staffPosition.getStartDate();
                                    sfpToRetrieveValue = staffPosition;
                                }
                            }
                        }

                        if (countPrimaryPos > 1) {
                            entity.addRetrievalError(field.getFieldId(),
                                    new StateReportValidationError(entity, field, "Primary Position",
                                            "More than one primary positions for the given staff with id = "
                                                    + staff.getLocalId()));

                        }

                        value = "N";

                        if (sfpToRetrieveValue != null && BooleanAsStringConverter.TRUE
                                .equals(sfpToRetrieveValue.getFieldValueByBeanPath(m_fieldSfpItinerantTeacher))) {
                            value = "Y";
                        }
                    }
                }
            } else if (CALC_PARAM_HIRE_DATE.equals(param)) {
                if (teacher) {
                    value = staff.getHireDate();
                }
            } else if (CALC_PARAM_SECOND_POSITION.equals(param)) {
                if (principal) {
                    if (m_staffPositionMap != null && m_staffPositionMap.containsKey(staffOid)) {
                        Collection<StaffPosition> staffPositions = m_staffPositionMap.get(staffOid);
                        for (StaffPosition staffPosition : staffPositions) {
                            if (CODE_STAFF_TYPE_PRINCIPAL.equalsIgnoreCase(staffPosition.getJobCode())) {
                                value = CODE_STAFF_TYPE_PRINCIPAL;
                            }
                        }
                    }
                }
            } else if (CALC_PARAM_SECOND_HIRE_DATE.equals(param)) {
                if (principal) {
                    if (m_staffPositionMap != null && m_staffPositionMap.containsKey(staffOid)) {
                        Collection<StaffPosition> staffPositions = m_staffPositionMap.get(staffOid);
                        PlainDate earliestDate = null;
                        for (StaffPosition staffPosition : staffPositions) {
                            if (CODE_STAFF_TYPE_PRINCIPAL.equalsIgnoreCase(staffPosition.getJobCode())) {
                                if (earliestDate == null || earliestDate.after(staffPosition.getStartDate())) {
                                    earliestDate = staffPosition.getStartDate();
                                }
                            }
                        }

                        value = earliestDate;
                    }
                }
            } else if (CALC_PARAM_PRIMARY_LOCATION.equals(param)) {
                if (m_staffPositionMap != null && m_staffPositionMap.containsKey(staffOid)) {
                    PlainDate startDate = getCurrentContext().getStartDate();

                    Collection<StaffPosition> staffPositions = m_staffPositionMap.get(staffOid);
                    Collection<String> schools = new ArrayList<String>();
                    for (StaffPosition staffPosition : staffPositions) {
                        String positionSchoolOid = staffPosition.getSchoolOid();

                        PlainDate staffStartDate = staffPosition.getStartDate();
                        if (staffPosition.getStartDate() != null &&
                                (startDate.before(staffStartDate) || startDate.equals(staffStartDate))) {
                            schools.add(positionSchoolOid);
                        }

                        if (!schools.isEmpty() &&
                                !schools.contains(positionSchoolOid)) {
                            value = "0000";
                        }
                    }
                }
            } else if (CALC_PARAM_DISTRICT_EXPERIENCE.equals(param)) {
                if (staff.getHireDate() != null) {
                    int years = data.getNumberOfContextYears(staff.getHireDate(), new PlainDate());
                    Object priorYears = data.getPropertyAsJavaType(staff, m_fieldStfPriorExperience);
                    if (priorYears != null) {
                        years += (int) Double.parseDouble(priorYears.toString());
                    }
                    value = Integer.valueOf(years);
                }
            } else if (CALC_PARAM_HISPANIC_INDICATOR.equals(param)) {
                value = staff.getPerson().getHispanicLatinoIndicator() ? "Y" : "N";
            } else if (CALC_PARAM_EDUCATION_LEVEL.equals(param)) {
                Collection<StaffDegree> staffDegrees = staff.getDegrees();
                if (staffDegrees != null) {
                    Integer previousStateValue = null;
                    for (StaffDegree staffDegree : staffDegrees) {
                        String degree = staffDegree.getType();
                        if (!StringUtils.isEmpty(degree)) {
                            Object stateValueObject = lookupReferenceCodeByBeanPath(StaffDegree.class,
                                    StaffDegree.COL_TYPE, degree,
                                    ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                            Integer stateValue = Integer.valueOf((String) stateValueObject);
                            if ((stateValue != null && previousStateValue == null) ||
                                    (stateValue != null && previousStateValue != null &&
                                            (stateValue.intValue() > previousStateValue.intValue()))) {
                                value = stateValueObject;
                            }
                            previousStateValue = stateValue;
                        }
                    }
                }
            } else if (CALC_PARAM_CONTRACT_MONTHS.equals(param)) {
                if (m_staffPositionMap != null && m_staffPositionMap.containsKey(staffOid)) {
                    List<Integer> monthsForPrimarySfp = new ArrayList<Integer>();
                    Collection<StaffPosition> staffPositions = m_staffPositionMap.get(staffOid);
                    for (StaffPosition staffPosition : staffPositions) {
                        Boolean primaryPos = (Boolean) data.getPropertyAsJavaType(staffPosition, m_fieldPrimaryPositon);
                        if (primaryPos != null && primaryPos.booleanValue()) {
                            Object months = data.getPropertyAsJavaType(staffPosition, m_fieldContractMonths);
                            if (months != null) {
                                monthsForPrimarySfp.add(Integer.valueOf((int) Double.parseDouble(months.toString())));
                            }
                        }
                    }

                    if (monthsForPrimarySfp.size() > 1) {
                        entity.addRetrievalError(field.getFieldId(),
                                new StateReportValidationError(entity, field, "Primary Position",
                                        "More than one primary positions for the given staff"));

                        Collections.sort(monthsForPrimarySfp, Collections.reverseOrder());
                    }

                    value = monthsForPrimarySfp.size() > 0 ? monthsForPrimarySfp.get(0) : "";

                }
            } else if (CALC_PARAM_STF_CONTRACT_DAYS.equals(param)) {
                if (m_staffPositionMap != null && m_staffPositionMap.containsKey(staffOid)) {
                    List<Integer> daysForPrimarySfp = new ArrayList<Integer>();
                    Collection<StaffPosition> staffPositions = m_staffPositionMap.get(staffOid);
                    for (StaffPosition staffPosition : staffPositions) {
                        Boolean primaryPos = (Boolean) data.getPropertyAsJavaType(staffPosition, m_fieldPrimaryPositon);
                        if (primaryPos != null && primaryPos.booleanValue()) {
                            Object days = data.getPropertyAsJavaType(staffPosition, m_fieldSfpContractDays);
                            if (days != null) {
                                daysForPrimarySfp.add(Integer.valueOf((int) Double.parseDouble(days.toString())));
                            }
                        }
                    }

                    if (daysForPrimarySfp.size() > 1) {
                        entity.addRetrievalError(field.getFieldId(),
                                new StateReportValidationError(entity, field, "Primary Position",
                                        "More than one primary positions for the given staff"));

                        Collections.sort(daysForPrimarySfp, Collections.reverseOrder());
                    }

                    value = daysForPrimarySfp.size() > 0 ? daysForPrimarySfp.get(0) : "";

                }
            } else if (CALC_PARAM_STF_SALARY.equals(param)) {
                if (m_staffPositionMap != null && m_staffPositionMap.containsKey(staffOid)) {
                    List<Double> salaryForPrimarySfp = new ArrayList<Double>();
                    Collection<StaffPosition> staffPositions = m_staffPositionMap.get(staffOid);
                    for (StaffPosition staffPosition : staffPositions) {
                        Boolean primaryPos = (Boolean) data.getPropertyAsJavaType(staffPosition, m_fieldPrimaryPositon);
                        if (primaryPos != null && primaryPos.booleanValue()) {
                            Object salary = data.getPropertyAsJavaType(staffPosition, m_fieldSfpAnnualSalary);
                            if (salary != null) {
                                salaryForPrimarySfp.add(Double.valueOf(salary.toString()));
                            }
                        }
                    }

                    if (salaryForPrimarySfp.size() > 1) {
                        entity.addRetrievalError(field.getFieldId(),
                                new StateReportValidationError(entity, field, "Primary Position",
                                        "More than one primary positions for the given staff"));

                        Collections.sort(salaryForPrimarySfp, Collections.reverseOrder());
                    }

                    value = salaryForPrimarySfp.size() > 0 ? salaryForPrimarySfp.get(0) : "";

                }
            } else if (CALC_PARAM_SEPARATION_REASON.equalsIgnoreCase(param)) {
                StaffPosition sfp = getLatestStaffPosition(staffOid);
                if (sfp != null
                        && BooleanAsStringConverter.TRUE.equals(sfp.getFieldValueByBeanPath(m_fieldSfpExitDate))) {
                    value = lookupReferenceCodeByBeanPath(StaffPosition.class,
                            m_fieldSfpSeparationReason,
                            (String) sfp.getFieldValueByBeanPath(m_fieldSfpSeparationReason),
                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                }
            } else if (CALC_PARAM_STF_TEACH_EXP.equalsIgnoreCase(param)) {
                StaffPosition sfp = getLatestStaffPosition(staffOid);
                if (sfp != null) {
                    int years = data.getNumberOfContextYears(sfp.getStartDate(), new PlainDate());
                    Object priorYears = data.getPropertyAsJavaType(sfp, m_fieldSfpPriorYearsExperience);
                    if (priorYears != null) {
                        years += (int) Double.parseDouble(priorYears.toString());
                    }
                    value = Integer.valueOf(years);
                }
            } else if (CALC_PARAM_STF_PROF_DEV_IND.equalsIgnoreCase(param)) {
                StaffPosition sfp = getLatestStaffPosition(staffOid);
                Object code = null;

                if (sfp != null) {
                    code = lookupReferenceCodeByBeanPath(StaffPosition.class,
                            m_fieldSfpProfInd,
                            (String) sfp.getFieldValueByBeanPath(m_fieldSfpProfInd),
                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                }

                value = code != null ? code : "N";
            } else if (CALC_PARAM_STF_CERT_EXEMPT.equalsIgnoreCase(param)) {
                StaffPosition sfp = getLatestStaffPosition(staffOid);
                value = "N";

                if (sfp != null) {
                    value = BooleanAsStringConverter.TRUE.equals(sfp.getFieldValueByBeanPath(m_fieldSfpCertExempt))
                            ? "Y"
                            : "N";
                }
            } else if (CALC_PARAM_STF_EMPL_BASIS.equalsIgnoreCase(param)) {
                Collection<StaffPosition> staffPositions = m_staffPositionMap.get(staffOid);
                double time = 0.0;
                for (StaffPosition sfp : staffPositions) {
                    String calendarCode = m_mostCommonCalendars.get(sfp.getSchoolOid());
                    Map<String, Collection<SchoolCalendarDate>> datesByCalCode = null;
                    if (sfp != null && !StringUtils.isEmpty(calendarCode) &&
                            (datesByCalCode = m_sklCalDates.get(sfp.getSchoolOid())) != null) {
                        Collection<SchoolCalendarDate> dates = datesByCalCode.get(calendarCode);
                        int allInSession = dates != null ? dates.size() : 0;
                        int inSessionForSfp = getNumberOfInSessionForDateRange(dates,
                                (PlainDate) sfp.getFieldValueByBeanPath(m_fieldSfpStartDate),
                                (PlainDate) sfp.getFieldValueByBeanPath(m_fieldSfpEndDate));

                        double fte = sfp.getFte() != null ? sfp.getFte().doubleValue() : 1.0;

                        if (allInSession != 0 && inSessionForSfp != 0) {
                            time += (double) inSessionForSfp / allInSession * fte;
                        }
                    }
                }

                value = Double.valueOf(time);
            }

            return value;
        }
    }

    /**
     * Implementation of Staff Snapshot Data Validation and Errors according to Level 0 and Level 0
     * Historical (L0H) Validation checks and
     * Error Messages, Version 12.01a
     *
     * @author Follett Software Company
     */
    class ValidateStaff implements FieldValidator {
        /**
         * Needed to clean up and simplify errors adding, using the same error message template: to
         * add error message you need only pass
         * error message and non valid values to addError method.
         *
         * @author Follett Software Company
         */
        class ValidationHelper {
            private StateReportEntity m_entity;
            private Collection<StateReportValidationError> m_errors;
            private FieldDefinition m_field;
            private ArrayList<String> m_races;
            private String m_value;

            /**
             * @param entity
             * @param field
             * @param value
             */
            public ValidationHelper(StateReportEntity entity, FieldDefinition field, String value) {
                m_entity = entity;
                m_field = field;
                m_value = value;

                m_errors = new ArrayList<StateReportValidationError>();

                m_alternateStaffIds = new HashSet<String>();

                m_races = new ArrayList<String>();
            }

            /**
             * Checks and adds if needed errors SN7351
             */
            public void addAnnualContractWorkMonthErrors() {
                String itinerantStaff = m_entity.getFieldValue(FIELD_STF_ITINERANT);
                if (LOGICAL_N_VALUE.equals(itinerantStaff)) {
                    boolean outsideValidRange = false;

                    if (StringUtils.isEmpty(m_value) || Float.parseFloat(m_value) < 1
                            || Float.parseFloat(m_value) > 12) {
                        outsideValidRange = true;
                    }

                    if (outsideValidRange) {
                        String errorMessage =
                                m_field.getFieldId() + " value outside of valid range (1-12 and whole number)";
                        String nonValidValues = m_field.getFieldId() + " = " + STYLE_BOLD + m_value + STYLE_END + ", " +
                                FIELD_STF_ITINERANT + " = " + STYLE_BOLD + itinerantStaff + STYLE_END;
                        addError(errorMessage, nonValidValues);
                    }
                }
            }

            /**
             * Checks and adds if needed errors SN7343
             */
            public void addAnnualSalaryErrors() {
                String itinerantStaff = m_entity.getFieldValue(FIELD_STF_ITINERANT);
                if (LOGICAL_N_VALUE.equals(itinerantStaff)) {
                    boolean outsideValidRange = false;

                    if (StringUtils.isEmpty(m_value) || Float.parseFloat(m_value) < 0
                            || Float.parseFloat(m_value) > 500000) {
                        outsideValidRange = true;
                    }

                    if (outsideValidRange) {
                        String errorMessage =
                                m_field.getFieldId() + " value outside of valid range (0-500000 and whole number)";
                        String nonValidValues = m_field.getFieldId() + " = " + STYLE_BOLD + m_value + STYLE_END + ", " +
                                FIELD_STF_ITINERANT + " = " + STYLE_BOLD + itinerantStaff + STYLE_END;
                        addError(errorMessage, nonValidValues);
                    }
                }
            }

            /**
             * Checks and adds if needed errors SN7340, SN7341, SN7361
             *
             * @throws ParseException
             */
            public void addBirthDateErrors() throws ParseException {
                if (!StringUtils.isEmpty(m_value)) {
                    Date dob = m_exportDateFormat.parse(m_value);
                    if (dob.after(m_currentDate)) {
                        String errorMessage = m_field.getFieldId() + " exceeds current date";
                        String nonValidValues = m_field.getFieldId() + " = " + STYLE_BOLD + m_value + STYLE_END;
                        addError(errorMessage, nonValidValues);
                    }

                    if (getYearsDiff(getCurrentContext().getStartDate(), dob) < 18) {
                        String errorMessage =
                                m_field.getFieldId() + " cannot be less than 18 years prior to school year";
                        String nonValidValues = m_field.getFieldId() + " = " + STYLE_BOLD + m_value + STYLE_END + ", " +
                                "school years start date = " + STYLE_BOLD + getCurrentContext().getStartDate()
                                + STYLE_END;
                        addError(errorMessage, nonValidValues);
                    }

                    if (getYearsDiff(getCurrentContext().getStartDate(), dob) > 100) {
                        String errorMessage = m_field.getFieldId() + " cannot be 100 years prior to school year";
                        String nonValidValues = m_field.getFieldId() + " = " + STYLE_BOLD + m_value + STYLE_END + ", " +
                                "school years start date = " + STYLE_BOLD + getCurrentContext().getStartDate()
                                + STYLE_END;
                        addError(errorMessage, nonValidValues);
                    }
                }
            }

            /**
             * Checks and adds if needed errors SN7363, SN7364
             */
            public void addContractWorkDaysErrors() {
                String itinerantStaff = m_entity.getFieldValue(FIELD_STF_ITINERANT);
                String positionTitle = m_entity.getFieldValue(FIELD_STF_POS_TITLE);
                if (STAFF_POSITION_TEACHER.equalsIgnoreCase(positionTitle) && LOGICAL_N_VALUE.equals(itinerantStaff)) {
                    boolean outsideValidRange = false;

                    if (StringUtils.isEmpty(m_value) || Float.parseFloat(m_value) < 1
                            || Float.parseFloat(m_value) > 260) {
                        outsideValidRange = true;
                    }

                    if (outsideValidRange) {
                        String errorMessage = "Teacher " + m_field.getFieldId()
                                + " value outside of valid range (1-260 and whole number)";
                        String nonValidValues = m_field.getFieldId() + " = " + STYLE_BOLD + m_value + STYLE_END + ", " +
                                FIELD_STF_ITINERANT + " = " + STYLE_BOLD + itinerantStaff + STYLE_END + ", " +
                                FIELD_STF_POS_TITLE + " = " + STYLE_BOLD + positionTitle + STYLE_END;
                        addError(errorMessage, nonValidValues);
                    }
                } else if (!STAFF_POSITION_TEACHER.equalsIgnoreCase(positionTitle)) {
                    boolean outsideValidRange = false;

                    if (!StringUtils.isEmpty(m_value) && (Float.parseFloat(m_value) < 1
                            || Float.parseFloat(m_value) > 366)) {
                        outsideValidRange = true;
                    }

                    if (outsideValidRange) {
                        String errorMessage = "Non-teachers " + m_field.getFieldId()
                                + " value outside of valid range (1-366 and whole number)";
                        String nonValidValues = m_field.getFieldId() + " = " + STYLE_BOLD + m_value + STYLE_END + ", " +
                                FIELD_STF_POS_TITLE + " = " + STYLE_BOLD + positionTitle + STYLE_END;
                        addError(errorMessage, nonValidValues);
                    }
                }
            }

            /**
             * Checks and adds if needed errors SN7365
             */
            public void addEmailAdressErrors() {
                Matcher matcher = VALID_EMAIL_ADDRESS_REGEX.matcher(m_value);
                if (!matcher.find()) {
                    String errorMessage = "Missing/invalid " + m_field.getFieldId();
                    String nonValidValues = m_field.getFieldId() + " = " + STYLE_BOLD + m_value + STYLE_END;
                    addError(errorMessage, nonValidValues);
                }
            }

            /**
             * Checks and adds if needed errors SN7345
             */
            public void addEmplymentBasisErrors() {
                String itinerantStaff = m_entity.getFieldValue(FIELD_STF_ITINERANT);
                if (LOGICAL_N_VALUE.equals(itinerantStaff)) {
                    if (StringUtils.isEmpty(m_value)
                            || (Double.parseDouble(m_value) < 0.0d || Double.parseDouble(m_value) > 1.000d)) {
                        String errorMessage =
                                m_field.getFieldId() + " must be greater than 0.0, but not greater than 1.000";
                        String nonValidValues = m_field.getFieldId() + " = " + STYLE_BOLD + m_value + STYLE_END + ", " +
                                FIELD_STF_ITINERANT + " = " + STYLE_BOLD + itinerantStaff + STYLE_END;
                        addError(errorMessage, nonValidValues);
                    }
                }
            }

            /**
             * Checks and adds if needed errors SN7356
             *
             * @throws ParseException
             */
            public void addEmplymentSeparationErrors() throws ParseException {
                String exitDateExportValue = m_entity.getFieldValue(FIELD_STF_EXIT_DATE);
                if (StringUtils.isEmpty(exitDateExportValue) && !StringUtils.isEmpty(m_value)) {
                    String errorMessage =
                            FIELD_STF_EXIT_DATE + " expected when " + m_field.getFieldId() + " is provided";
                    String nonValidValues = m_field.getFieldId() + " = " + STYLE_BOLD + m_value + STYLE_END + ", " +
                            FIELD_STF_EXIT_DATE + " = " + STYLE_BOLD + exitDateExportValue + STYLE_END;
                    addError(errorMessage, nonValidValues);
                }
            }

            /**
             * Checks and adds if needed errors SN7320, SN7321, SN7360
             *
             * @throws ParseException
             */
            public void addExitDateErrors() throws ParseException {
                if (!StringUtils.isEmpty(m_value)) {
                    Date exitDate = m_exportDateFormat.parse(m_value);
                    if (exitDate.after(m_currentDate)) {
                        String errorMessage = m_field.getFieldId() + " cannot be a future date";
                        String nonValidValues = m_field.getFieldId() + " = " + STYLE_BOLD + m_value + STYLE_END + ", " +
                                " current date " + " = " + STYLE_BOLD + m_exportDateFormat.format(m_currentDate)
                                + STYLE_END;
                        addError(errorMessage, nonValidValues);
                    }

                    String tchHireDateExportValue = m_entity.getFieldValue(FIELD_STF_TEACHER_HIRE_DATE);
                    if (!StringUtils.isEmpty(tchHireDateExportValue)) {
                        Date hireDate = m_exportDateFormat.parse(tchHireDateExportValue);
                        if (exitDate.before(hireDate)) {
                            String errorMessage =
                                    m_field.getFieldId() + " is earlier than " + FIELD_STF_TEACHER_HIRE_DATE;
                            String nonValidValues =
                                    m_field.getFieldId() + " = " + STYLE_BOLD + m_value + STYLE_END + ", " +
                                            FIELD_STF_TEACHER_HIRE_DATE + " = " + STYLE_BOLD + tchHireDateExportValue
                                            + STYLE_END;
                            addError(errorMessage, nonValidValues);
                        }
                    }

                    String prncplHireDateExportValue = m_entity.getFieldValue(FIELD_STF_PRNCPL_HIRE_DATE);
                    if (!StringUtils.isEmpty(prncplHireDateExportValue)) {
                        Date hireDate = m_exportDateFormat.parse(prncplHireDateExportValue);
                        if (exitDate.before(hireDate)) {
                            String errorMessage =
                                    m_field.getFieldId() + " is earlier than " + FIELD_STF_PRNCPL_HIRE_DATE;
                            String nonValidValues =
                                    m_field.getFieldId() + " = " + STYLE_BOLD + m_value + STYLE_END + ", " +
                                            FIELD_STF_PRNCPL_HIRE_DATE + " = " + STYLE_BOLD + prncplHireDateExportValue
                                            + STYLE_END;
                            addError(errorMessage, nonValidValues);
                        }
                    }

                    String sepReasonCode = m_entity.getFieldValue(FIELD_STF_SEP_REASON);
                    if (StringUtils.isEmpty(sepReasonCode)) {
                        String errorMessage =
                                FIELD_STF_SEP_REASON + " expected when " + m_field.getFieldId() + " is provided";
                        String nonValidValues = m_field.getFieldId() + " = " + STYLE_BOLD + m_value + STYLE_END + ", " +
                                FIELD_STF_SEP_REASON + " = " + STYLE_BOLD + sepReasonCode + STYLE_END;
                        addError(errorMessage, nonValidValues);
                    }
                }
            }

            /**
             * Checks and adds if needed errors SN7359
             */
            public void addHispanicEthnIndicatorErrors() {
                if (StringUtils.isEmpty(m_value)) {
                    String race1Code = m_entity.getFieldValue(FIELD_STF_RACE_1_CODE);
                    if (!StringUtils.isEmpty(race1Code)) {
                        String errorMessage =
                                m_field.getFieldId() + " required when " + FIELD_STF_RACE_1_CODE + " is provided";
                        String nonValidValues = m_field.getFieldId() + " = " + STYLE_BOLD + m_value + STYLE_END + ", " +
                                FIELD_STF_RACE_1_CODE + " = " + STYLE_BOLD + race1Code + STYLE_END;
                        addError(errorMessage, nonValidValues);
                    }
                }
            }

            /**
             * Checks and adds if needed errors SN7314, SN7315, SN7335
             *
             * @throws ParseException
             */
            public void addOriginalHireDateErrors() throws ParseException {
                String teacherTitle = m_entity.getFieldValue(FIELD_STF_POS_TITLE);
                String itinerantStaff = m_entity.getFieldValue(FIELD_STF_ITINERANT);
                if (!StringUtils.isEmpty(teacherTitle) && LOGICAL_N_VALUE.equals(itinerantStaff)) {
                    if (StringUtils.isEmpty(m_value)) {
                        String errorMessage =
                                "If the " + FIELD_STF_POS_TITLE + " is provided and " + FIELD_STF_ITINERANT +
                                        " is equal to N, then the " + m_field.getFieldId() + " must have data";
                        String nonValidValues =
                                FIELD_STF_POS_TITLE + " = " + STYLE_BOLD + teacherTitle + STYLE_END + ", " +
                                        FIELD_STF_ITINERANT + " = " + STYLE_BOLD + itinerantStaff + STYLE_END + ", " +
                                        m_field.getFieldId() + " = " + STYLE_BOLD + m_value + STYLE_END;
                        addError(errorMessage, nonValidValues);
                    } else {
                        Date teacherHireDate = m_exportDateFormat.parse(m_value);

                        if (m_earliestHireDate.after(teacherHireDate)) {
                            String errorMessage = m_field.getFieldId() + " must be later or equal to "
                                    + m_exportDateFormat.format(m_earliestHireDate);
                            String nonValidValues = m_field.getFieldId() + " = " + STYLE_BOLD + m_value + STYLE_END;
                            addError(errorMessage, nonValidValues);
                        }
                    }
                }

                if (!StringUtils.isEmpty(m_value)) {
                    Date teacherHireDate = m_exportDateFormat.parse(m_value);
                    if (teacherHireDate.after(m_currentDate)) {
                        String errorMessage = m_field.getFieldId() + " cannot be future date";
                        String nonValidValues = m_field.getFieldId() + " = " + STYLE_BOLD + m_value + STYLE_END + ", " +
                                "current date = " + STYLE_BOLD + m_currentDate + STYLE_END;
                        addError(errorMessage, nonValidValues);
                    }

                    if (!STAFF_POSITION_TEACHER.equalsIgnoreCase(teacherTitle)) {
                        String errorMessage = m_field.getFieldId() + " is not expected when " + FIELD_STF_POS_TITLE
                                + " is not provided";
                        String nonValidValues = m_field.getFieldId() + " = " + STYLE_BOLD + m_value + STYLE_END + ", " +
                                FIELD_STF_POS_TITLE + " = " + STYLE_BOLD + teacherTitle + STYLE_END;
                        addError(errorMessage, nonValidValues);
                    }
                }
            }

            /**
             * Checks and adds if needed errors SN7352
             */
            public void addProfDevIndErrors() {
                String teacherTitle = m_entity.getFieldValue(FIELD_STF_POS_TITLE);
                String itinerantStaff = m_entity.getFieldValue(FIELD_STF_ITINERANT);
                if (STAFF_POSITION_TEACHER.equalsIgnoreCase(teacherTitle) &&
                        LOGICAL_N_VALUE.equals(itinerantStaff)) {
                    if (StringUtils.isEmpty(m_value)) {
                        String errorMessage = m_field.getFieldId() + " must be provided where " + FIELD_STF_POS_TITLE +
                                " = " + teacherTitle + " and " + FIELD_STF_ITINERANT + " = " + itinerantStaff;
                        String nonValidValues = m_field.getFieldId() + " = " + STYLE_BOLD + m_value + STYLE_END + ", " +
                                FIELD_STF_POS_TITLE + " = " + STYLE_BOLD + teacherTitle + STYLE_END + ", " +
                                FIELD_STF_ITINERANT + " = " + STYLE_BOLD + itinerantStaff + STYLE_END;
                        addError(errorMessage, nonValidValues);
                    }
                }
            }

            /**
             * Checks and adds if needed errors SN7358, SN7354, SN7355
             */
            public void addRaceCodeErrors() {
                if (FIELD_STF_RACE_1_CODE.equals(m_field.getFieldId())) {
                    if (StringUtils.isEmpty(m_value)) {
                        String hispanicInd = m_entity.getFieldValue(FIELD_STF_HISPANIC_IND);
                        if (LOGICAL_N_VALUE.equals(hispanicInd)) {
                            String errorMessage =
                                    m_field.getFieldId() + " required when " + FIELD_STF_HISPANIC_IND + " is "
                                            + LOGICAL_N_VALUE;
                            String nonValidValues =
                                    m_field.getFieldId() + " = " + STYLE_BOLD + m_value + STYLE_END + ", " +
                                            FIELD_STF_HISPANIC_IND + " = " + STYLE_BOLD + hispanicInd + STYLE_END;
                            addError(errorMessage, nonValidValues);
                        }
                    }
                }

                if (m_races.size() > 0) {
                    if (StringUtils.isEmpty(m_races.get(m_races.size() - 1)) &&
                            !StringUtils.isEmpty(m_value)) {
                        String errorMessage = "Race Codes must be populated in sequence (" +
                                FIELD_STF_RACE_1_CODE + ", " + FIELD_STF_RACE_2_CODE + ", " + FIELD_STF_RACE_3_CODE
                                + ", etc)";
                        addError(errorMessage, "");
                    }
                }

                if (m_races.contains(m_value)) {
                    String errorMessage = "Each Race Code must be unique";
                    addError(errorMessage, "");
                }

                m_races.add(m_value);
            }

            /**
             * Checks and adds if needed errors SN7325, SN7326, SN7336
             *
             * @throws ParseException
             */
            public void addSecondPosHireDateErrors() throws ParseException {
                String principalTitle = m_entity.getFieldValue(FIELD_STF_SEC_POS_TITLE);
                String itinerantStaff = m_entity.getFieldValue(FIELD_STF_ITINERANT);
                if (!StringUtils.isEmpty(principalTitle) && LOGICAL_N_VALUE.equals(itinerantStaff)) {
                    if (StringUtils.isEmpty(m_value)) {
                        String errorMessage =
                                "If the " + FIELD_STF_SEC_POS_TITLE + " is provided and " + FIELD_STF_ITINERANT +
                                        " is equal to N, then the " + m_field.getFieldId() + " must have data";
                        String nonValidValues =
                                FIELD_STF_SEC_POS_TITLE + " = " + STYLE_BOLD + principalTitle + STYLE_END + ", " +
                                        FIELD_STF_ITINERANT + " = " + STYLE_BOLD + itinerantStaff + STYLE_END + ", " +
                                        m_field.getFieldId() + " = " + STYLE_BOLD + m_value + STYLE_END;
                        addError(errorMessage, nonValidValues);
                    } else {
                        Date teacherHireDate = m_exportDateFormat.parse(m_value);

                        if (m_earliestHireDate.after(teacherHireDate)) {
                            String errorMessage = m_field.getFieldId() + " must be later or equal to "
                                    + m_exportDateFormat.format(m_earliestHireDate);
                            String nonValidValues = m_field.getFieldId() + " = " + STYLE_BOLD + m_value + STYLE_END;
                            addError(errorMessage, nonValidValues);
                        }
                    }
                }

                if (!StringUtils.isEmpty(m_value)) {
                    Date teacherHireDate = m_exportDateFormat.parse(m_value);
                    if (teacherHireDate.after(m_currentDate)) {
                        String errorMessage = m_field.getFieldId() + " cannot be future date";
                        String nonValidValues = m_field.getFieldId() + " = " + STYLE_BOLD + m_value + STYLE_END + ", " +
                                "current date = " + STYLE_BOLD + m_currentDate + STYLE_END;
                        addError(errorMessage, nonValidValues);
                    }

                    if (!STAFF_POSITION_TEACHER.equalsIgnoreCase(principalTitle)) {
                        String errorMessage = m_field.getFieldId() + " is not expected when " + FIELD_STF_SEC_POS_TITLE
                                + " is not provided";
                        String nonValidValues = m_field.getFieldId() + " = " + STYLE_BOLD + m_value + STYLE_END + ", " +
                                FIELD_STF_SEC_POS_TITLE + " = " + STYLE_BOLD + principalTitle + STYLE_END;
                        addError(errorMessage, nonValidValues);
                    }
                }
            }

            /**
             * Checks and adds if needed errors SN7334
             */
            public void addSecondPosTitleErrors() {
                if (!StringUtils.isEmpty(m_value)) {
                    if (!"PRINCIPAL".equalsIgnoreCase(m_value)) {
                        String errorMessage = m_field.getFieldId() + " must be PRINCIPAL, if provided";
                        String nonValidValues = m_field.getFieldId() + " = " + STYLE_BOLD + m_value + STYLE_END;
                        addError(errorMessage, nonValidValues);
                    }
                }
            }

            /**
             * Checks and adds if needed errors SN7350
             */
            public void addStaffEdLvlErrors() {
                String itinerantStaff = m_entity.getFieldValue(FIELD_STF_ITINERANT);
                if (!StringUtils.isEmpty(itinerantStaff) && StringUtils.isEmpty(m_value)) {
                    String errorMessage =
                            "When " + FIELD_STF_ITINERANT + " = 'N', a " + m_field.getFieldId() + " must be provided";
                    String nonValidValues = m_field.getFieldId() + " = " + STYLE_BOLD + m_value + STYLE_END + ", " +
                            FIELD_STF_ITINERANT + " = " + STYLE_BOLD + itinerantStaff + STYLE_END;
                    addError(errorMessage, nonValidValues);
                }
            }

            /**
             * Checks and adds if needed errors SN7362, SN7324
             */
            public void addStaffIdErrors() {
                if (!StringUtils.isEmpty(m_value)) {
                    String altStaffId = m_entity.getFieldValue(FIELD_STF_ID_ALT);
                    if (!m_value.equals(altStaffId)) {
                        String errorMessage = m_field.getFieldId() + " must match " + FIELD_STF_ID_ALT;
                        String nonValidValues = m_field.getFieldId() + " = " + STYLE_BOLD + m_value + STYLE_END + ", " +
                                FIELD_STF_ID_ALT + " = " + STYLE_BOLD + altStaffId + STYLE_END;
                        addError(errorMessage, nonValidValues);
                    }

                    if (!m_alternateStaffIds.add(m_value)) {
                        String errorMessage = "Please review Alternate Staff ID for accuracy (duplicated value)";
                        addError(errorMessage, "");
                    }
                }
            }

            /**
             * Checks and adds if needed errors SN7342
             */
            public void addYearsExperienceInDistrErrors() {
                String itinerantStaff = m_entity.getFieldValue(FIELD_STF_ITINERANT);
                if (LOGICAL_N_VALUE.equals(itinerantStaff)) {
                    if (StringUtils.isEmpty(m_value)
                            || (Float.parseFloat(m_value) < 1 || Float.parseFloat(m_value) > 60)) {
                        String errorMessage = "When " + FIELD_STF_ITINERANT + " = 'N', " + m_field.getFieldId()
                                + " must be provided " +
                                "and cannot be less than 1 and more than 60";
                        String nonValidValues = m_field.getFieldId() + " = " + STYLE_BOLD + m_value + STYLE_END + ", " +
                                FIELD_STF_ITINERANT + " = " + STYLE_BOLD + itinerantStaff + STYLE_END;
                        addError(errorMessage, nonValidValues);
                    }
                }
            }

            /**
             * Checks and adds if needed errors SN7348, SN7349
             */
            public void addYearsTeachingExperienceErrors() {
                String itinerantStaff = m_entity.getFieldValue(FIELD_STF_ITINERANT);
                if (LOGICAL_N_VALUE.equals(itinerantStaff)) {
                    if (StringUtils.isEmpty(m_value)
                            || (Float.parseFloat(m_value) < 1 || Float.parseFloat(m_value) > 70)) {
                        String errorMessage = "When " + FIELD_STF_ITINERANT + " = 'N', " + m_field.getFieldId()
                                + " must be provided " +
                                "and cannot be less than 1 and more than 60";
                        String nonValidValues = m_field.getFieldId() + " = " + STYLE_BOLD + m_value + STYLE_END + ", " +
                                FIELD_STF_ITINERANT + " = " + STYLE_BOLD + itinerantStaff + STYLE_END;
                        addError(errorMessage, nonValidValues);
                    }

                    String distExp = m_entity.getFieldValue(FIELD_STF_DISTRICT_EXP);
                    if (!StringUtils.isEmpty(distExp) && !StringUtils.isEmpty(m_value)) {
                        if (Float.parseFloat(m_value) < Float.parseFloat(distExp)) {
                            String errorMessage =
                                    m_field.getFieldId() + " cannot be less than " + FIELD_STF_DISTRICT_EXP;
                            String nonValidValues =
                                    m_field.getFieldId() + " = " + STYLE_BOLD + m_value + STYLE_END + ", " +
                                            FIELD_STF_DISTRICT_EXP + " = " + STYLE_BOLD + distExp + STYLE_END;
                            addError(errorMessage, nonValidValues);
                        }
                    }
                }
            }

            /**
             * Returns added errors
             *
             * @return Collection<StateReportValidationError>
             */
            public Collection<StateReportValidationError> getErrors() {
                return m_errors;
            }

            /**
             * Add new error with passed error message and non valid values
             *
             * @param errorMessage
             * @param nonValidValues
             */
            private void addError(String errorMessage, String nonValidValues) {
                m_errors.add(new StateReportValidationError(m_entity, m_field, errorMessage, nonValidValues));
            }
        }

        public final Pattern VALID_EMAIL_ADDRESS_REGEX =
                Pattern.compile("^[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,6}$", Pattern.CASE_INSENSITIVE);
        private static final String LOGICAL_N_VALUE = "N";
        private static final String STAFF_POSITION_TEACHER = "TEACHER";

        protected Set<String> m_alternateStaffIds;
        protected Date m_currentDate;
        protected Date m_earliestHireDate;

        /**
         * Constructor
         */
        public ValidateStaff() {
            Calendar calendar = Calendar.getInstance();
            calendar.set(1950, 0, 1);
            m_earliestHireDate = calendar.getTime();

            m_currentDate = new Date();

            m_alternateStaffIds = new HashSet<String>();
        }

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
            ValidationHelper validationHelper = new ValidationHelper(entity, field, value);

            String fieldId = field.getFieldId();

            if (fieldId.equals(FIELD_STF_ID)) {
                validationHelper.addStaffIdErrors();
            }
            if (fieldId.equals(FIELD_STF_RACE_1_CODE)) {
                validationHelper.addRaceCodeErrors();
            }
            if (fieldId.equals(FIELD_STF_TEACHER_HIRE_DATE)) {
                try {
                    validationHelper.addOriginalHireDateErrors();
                } catch (ParseException e) {
                    e.printStackTrace();
                }
            }
            if (fieldId.equals(FIELD_STF_EXIT_DATE)) {
                try {
                    validationHelper.addExitDateErrors();
                } catch (ParseException e) {
                    e.printStackTrace();
                }
            }
            if (fieldId.equals(FIELD_STF_BIRTH_DATE)) {
                try {
                    validationHelper.addBirthDateErrors();
                } catch (ParseException e) {
                    e.printStackTrace();
                }
            }
            if (fieldId.equals(FIELD_STF_DISTRICT_EXP)) {
                validationHelper.addYearsExperienceInDistrErrors();
            }
            if (fieldId.equals(FIELD_STF_ANNUAL_SALARY)) {
                validationHelper.addAnnualSalaryErrors();
            }
            if (fieldId.equals(FIELD_STF_ANNUAL_CONTR_WORK)) {
                validationHelper.addAnnualContractWorkMonthErrors();
            }
            if (fieldId.equals(FIELD_STF_SEP_REASON)) {
                try {
                    validationHelper.addEmplymentSeparationErrors();
                } catch (ParseException e) {
                    e.printStackTrace();
                }
            }
            if (fieldId.equals(FIELD_STF_EMPL_BASIS)) {
                validationHelper.addEmplymentBasisErrors();
            }
            if (fieldId.equals(FIELD_STF_HISPANIC_IND)) {
                validationHelper.addHispanicEthnIndicatorErrors();
            }
            if (fieldId.equals(FIELD_STF_EMAIL_ADDRESS)) {
                validationHelper.addEmailAdressErrors();
            }
            if (fieldId.equals(FIELD_STF_YRS_TEACH_EXP)) {
                validationHelper.addYearsTeachingExperienceErrors();
            }
            if (fieldId.equals(FIELD_STF_SEC_POS_TITLE)) {
                validationHelper.addSecondPosTitleErrors();
            }
            if (fieldId.equals(FIELD_STF_SEC_HIRE_DATE)) {
                try {
                    validationHelper.addSecondPosHireDateErrors();
                } catch (ParseException e) {
                    e.printStackTrace();
                }
            }
            if (fieldId.equals(FIELD_STF_ED_LEVEL)) {
                validationHelper.addStaffEdLvlErrors();
            }
            if (fieldId.equals(FIELD_STF_CONTRACT_WORK_DAYS)) {
                validationHelper.addContractWorkDaysErrors();
            }
            if (fieldId.equals(FIELD_STF_PROF_DEV_IND)) {
                validationHelper.addProfDevIndErrors();
            }

            return validationHelper.getErrors();
        }

        /**
         * Returns difference in years between two dates.
         *
         * @param firstDate
         * @param secondDate
         * @return int
         */
        protected int getYearsDiff(Date firstDate, Date secondDate) {
            Calendar cal1 = Calendar.getInstance();
            cal1.setTime(firstDate);

            Calendar cal2 = Calendar.getInstance();
            cal2.setTime(secondDate);

            return cal2.get(Calendar.YEAR) - cal1.get(Calendar.YEAR);
        }
    }

    /**
     * Input Definition Parameters
     */
    protected static final String PARAM_REMOVE_HEADER = "removeHeader";
    protected static final String PARAM_SCHOOL_YEAR_CONTEXT = "schoolYearContext";

    /**
     * Retriever Parameters
     */
    protected static final String CALC_ID_PRIMARY_LOC = "PRIMARY-LOC";
    protected static final String CALC_ID_STDLITE_RACE = "STFSNAP-RACE";
    protected static final String CALC_ID_STAFF_STATUS = "STFSNAP-STFSTATUS";
    protected static final String CALC_ID_STAFF_TYPE = "STFSNAP-STFTYPE";

    protected static final String CALC_PARAM_CONTRACT_MONTHS = "CONTRACT-MONTHS";
    protected static final String CALC_PARAM_DISTRICT_EXPERIENCE = "DIST-EXPERIENCE";
    protected static final String CALC_PARAM_EDUCATION_LEVEL = "DEGREE-TYPE";
    protected static final String CALC_PARAM_EXIT_DATE = "EXIT-DATE";
    protected static final String CALC_PARAM_JOB_DESCRIPTION = "JOB-DESCRIPTION";
    protected static final String CALC_PARAM_SECOND_POSITION = "SECOND-POSITION";
    protected static final String CALC_PARAM_HIRE_DATE = "HIRE-DATE";
    protected static final String CALC_PARAM_HISPANIC_INDICATOR = "HISP-IND";
    protected static final String CALC_PARAM_ITINERANT_TEACHER = "ITINERANT-TEACHER";
    protected static final String CALC_PARAM_PRIMARY_LOCATION = "PRIMARY-LOC";
    protected static final String CALC_PARAM_SECOND_HIRE_DATE = "SECOND-HIRE";
    protected static final String CALC_PARAM_SEPARATION_REASON = "STF-SEPARATION-REASON";
    protected static final String CALC_PARAM_STATUS = "STF-STATUS";
    protected static final String CALC_PARAM_STF_CERT_EXEMPT = "STF-CERT-EXEMPT";
    protected static final String CALC_PARAM_STF_CONTRACT_DAYS = "STF-CONTRACT-DAYS";
    protected static final String CALC_PARAM_STF_EMPL_BASIS = "EMPL-BASIS";
    protected static final String CALC_PARAM_STF_PROF_DEV_IND = "STF-PROF-DEV-IND";
    protected static final String CALC_PARAM_STF_SALARY = "STF-SALARY";
    protected static final String CALC_PARAM_STF_TEACH_EXP = "STF-TEACH-EXP";

    /**
     * Aliases
     */
    protected static final String ALIAS_CONTRACT_MONTHS = "all-sfp-ContractMonths";
    protected static final String ALIAS_EXCLUDE_STAFF = "DOE EXCLUDE STF";
    protected static final String ALIAS_PRIMARY_POSITION = "all-sfp-PrimaryPosition";
    protected static final String ALIAS_SFP_ANNUAL_SALARY = "all-sfp-AnnualSalary";
    protected static final String ALIAS_SFP_CERT_EXEMPT = "all-sfp-CertificationExempt";
    protected static final String ALIAS_SFP_CONTRACT_DAYS = "all-sfp-ContractDays";
    protected static final String ALIAS_SFP_EXIT_DATE = "all-sfp-ExitDate";
    protected static final String ALIAS_SFP_END_DATE = "DOE POS END DATE";
    protected static final String ALIAS_SFP_ITINERANT_TEACHER = "all-sfp-ItinerantTeacher";
    protected static final String ALIAS_SFP_PROF_DEV = "all-sfp-ProfessionalDevelopment";
    protected static final String ALIAS_SFP_START_DATE = "DOE POS START DATE";
    protected static final String ALIAS_SFP_TEACH_EXP = "all-sfp-PriorYearsExperience";
    protected static final String ALIAS_SFP_TERMINATION_REASON = "all-sfp-TerminationReason";
    protected static final String ALIAS_SKL_EXCLUDE_SCHOOL = "DOE EXCLUDE SKL";
    protected static final String ALIAS_SKL_LOC_CODE = "LOCATION CODE";
    protected static final String ALIAS_STF_PRIOR_EXP = "all-stf-PriorDistrictExperience";
    protected static final String ALIAS_TEACH_ID = "DOE TEACH ID";

    /**
     * Constants for reporting information.
     */
    protected static final String CODE_STAFF_STATUS_ACTIVE = "A";
    protected static final String CODE_STAFF_STATUS_INACTIVE = "I";

    protected static final String CODE_STAFF_TYPE_ADMINISTRATOR = "ADMINISTRATOR";
    protected static final String CODE_STAFF_TYPE_PRINCIPAL = "PRINCIPAL";
    protected static final String CODE_STAFF_TYPE_TEACHER = "TEACHER";

    protected static final String FIELD_STF_ANNUAL_CONTR_WORK = "AnnualContWorkMonths";
    protected static final String FIELD_STF_ANNUAL_SALARY = "ANNUAL SALARY";
    protected static final String FIELD_STF_BIRTH_DATE = "BIRTH DATE";
    protected static final String FIELD_STF_CONTRACT_WORK_DAYS = "CONTRACT WORK DAYS";
    protected static final String FIELD_STF_DISTRICT_EXP = "DISTRICT EXPERIENCE";
    protected static final String FIELD_STF_ED_LEVEL = "Staff Education Lvl";
    protected static final String FIELD_STF_EMAIL_ADDRESS = "EMAIL ADDRESS";
    protected static final String FIELD_STF_EMPL_BASIS = "EMPLOYMENT BASIS";
    protected static final String FIELD_STF_EXIT_DATE = "EXIT DATE";
    protected static final String FIELD_STF_HISPANIC_IND = "HISPANIC INDICATOR";
    protected static final String FIELD_STF_ID = "STAFF ID";
    protected static final String FIELD_STF_ID_ALT = "ALTERNATE STAFF ID";
    protected static final String FIELD_STF_ITINERANT = "ITINERANT TEACHER";
    protected static final String FIELD_STF_POS_TITLE = "JOB DESCRIPTION";
    protected static final String FIELD_STF_PRNCPL_HIRE_DATE = "Second Position";
    protected static final String FIELD_STF_PROF_DEV_IND = "Professional Dev Ind";
    protected static final String FIELD_STF_RACE_1_CODE = "RACE 1 CODE";
    protected static final String FIELD_STF_RACE_2_CODE = "RACE 2 CODE";
    protected static final String FIELD_STF_RACE_3_CODE = "RACE 3 CODE";
    protected static final String FIELD_STF_SEC_HIRE_DATE = "Second Position";
    protected static final String FIELD_STF_SEC_POS_TITLE = "Second Position";
    protected static final String FIELD_STF_SEP_REASON = "SEPARATION REASON";
    protected static final String FIELD_STF_TEACHER_HIRE_DATE = "ORIGINAL HIRE DATE";
    protected static final String FIELD_STF_YRS_TEACH_EXP = "YRS OF TEACHING EXP.";

    /**
     * Instance variables
     */
    protected SimpleDateFormat m_exportDateFormat;
    protected String m_fieldPrimaryPositon;
    protected String m_fieldSfpAnnualSalary;
    protected String m_fieldContractMonths;
    protected String m_fieldSfpCertExempt;
    protected String m_fieldSfpContractDays;
    protected String m_fieldSfpEndDate;
    protected String m_fieldSfpExitDate;
    protected String m_fieldSfpItinerantTeacher;
    protected String m_fieldSfpPriorYearsExperience;
    protected String m_fieldSfpProfInd;
    protected String m_fieldSfpSeparationReason;
    protected String m_fieldSfpStartDate;
    protected String m_fieldSklExcludeSchool;
    protected String m_fieldStfPriorExperience;
    protected String m_fieldTeachId;
    protected boolean m_removeHeaderIndicator;
    protected Map<String, String> m_mostCommonCalendars;
    protected Map<String, Map<String, Collection<SchoolCalendarDate>>> m_sklCalDates;
    protected String m_staffExcludeIndicatorField;
    protected Map<String, Collection<StaffPosition>> m_staffPositionMap =
            new HashMap<String, Collection<StaffPosition>>();

    private DistrictSchoolYearContext m_context;
    private String m_contextOid;
    private List<DistrictSchoolYearContext> m_contextYears;

    /**
     * Gets the current context.
     *
     * @return District school year context
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getCurrentContext()
     */
    @Override
    public DistrictSchoolYearContext getCurrentContext() {
        if (m_context == null) {
            if (m_contextOid != null) {
                m_context = (DistrictSchoolYearContext) getBroker().getBeanByOid(DistrictSchoolYearContext.class,
                        m_contextOid);
            } else {
                m_context = super.getCurrentContext();
                m_contextOid = m_context.getOid();
            }
        }
        return m_context;
    }

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
     * Initialize the data module.
     */
    @Override
    public void initialize() {
        initializeFields();
        loadStaffPosition();
        initializeMostCommonCalendars();
        initializeInSessionDates();

        Criteria criteria = getStaffCriteria();

        QueryByCriteria query = new QueryByCriteria(Staff.class, criteria);

        if (getSetupErrors().size() == 0) {
            m_exportDateFormat = new SimpleDateFormat("yyyy-MM-dd");

            setQuery(query);
            setEntityClass(NYStaffSnapshotEntity.class);

            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put(CALC_ID_PRIMARY_LOC, new RetrievePrimaryLocation());
            calcs.put(CALC_ID_STDLITE_RACE, new RetrieveRace());
            calcs.put(CALC_ID_STAFF_STATUS, new RetrieveStaffStatus());
            calcs.put(CALC_ID_STAFF_TYPE, new RetrieveStaffPrincipalData());

            HashMap validators = new HashMap<String, FieldRetriever>();
            validators.put("VAL-STAFF", new ValidateStaff());
            super.addCalcs(calcs);
            super.addValidators(validators);
        }
    }

    /**
     * Returns most recent staff position.
     *
     * @param staffOid String
     * @return Staff position
     */
    protected StaffPosition getLatestStaffPosition(String staffOid) {
        Collection<StaffPosition> staffPositions = m_staffPositionMap.get(staffOid);
        StaffPosition operatedSfp = null;
        PlainDate operatedDate = null;
        for (StaffPosition staffPosition : staffPositions) {
            PlainDate sfpEndDate = (PlainDate) staffPosition.getFieldValueByBeanPath(m_fieldSfpEndDate);

            if (sfpEndDate == null) {
                sfpEndDate = getCurrentContext().getEndDate();
            }

            if (operatedDate == null) {
                operatedDate = sfpEndDate;
                operatedSfp = staffPosition;
            }

            if (sfpEndDate.after(operatedDate)) {
                operatedSfp = staffPosition;
                operatedDate = sfpEndDate;
            }
        }

        return operatedSfp;
    }

    /**
     * Gets the number of context years.
     *
     * @param beginDate PlainDate
     * @param endDate PlainDate
     * @return int
     */
    protected int getNumberOfContextYears(PlainDate beginDate, PlainDate endDate) {
        int value = 0;
        if (m_contextYears == null) {
            m_contextYears = (List<DistrictSchoolYearContext>) getBroker()
                    .getCollectionByQuery(new QueryByCriteria(DistrictSchoolYearContext.class, null));
        }
        if (beginDate != null && endDate != null) {
            for (DistrictSchoolYearContext year : m_contextYears) {
                if (!beginDate.after(year.getEndDate()) && !endDate.before(year.getStartDate())) {
                    ++value;
                }
            }
        }
        return value;
    }

    /**
     * Calculate number of in session days for the given date range among given collection .
     *
     * @param dates Collection<SchoolCalendarDate>
     * @param startDate PlainDate
     * @param endDate PlainDate
     * @return int
     */
    protected int getNumberOfInSessionForDateRange(Collection<SchoolCalendarDate> dates,
                                                   PlainDate startDate,
                                                   PlainDate endDate) {
        int value = 0;

        if (endDate == null) {
            endDate = getCurrentContext().getEndDate();
        }

        if (dates != null && startDate != null) {
            for (SchoolCalendarDate date : dates) {
                if ((date.getDate().after(startDate) || date.getDate().equals(startDate))
                        && (date.getDate().before(endDate) || date.getDate().equals(endDate))) {
                    value += 1;
                }
            }
        }

        return value;
    }

    /**
     * Sets the Java names (bean paths) for all the exported fields.
     */
    private void initializeFields() {
        m_removeHeaderIndicator = false;
        if (getParameter(PARAM_REMOVE_HEADER) != null) {
            m_removeHeaderIndicator = ((Boolean) getParameter(PARAM_REMOVE_HEADER)).booleanValue();
        }

        m_staffExcludeIndicatorField = translateAliasToJavaName(ALIAS_EXCLUDE_STAFF, true);
        m_fieldSfpEndDate = translateAliasToJavaName(ALIAS_SFP_END_DATE, true);
        m_fieldSfpExitDate = translateAliasToJavaName(ALIAS_SFP_EXIT_DATE, true);
        m_fieldSfpStartDate = translateAliasToJavaName(ALIAS_SFP_START_DATE, true);
        m_fieldPrimaryPositon = translateAliasToJavaName(ALIAS_PRIMARY_POSITION, true);
        m_fieldSfpAnnualSalary = translateAliasToJavaName(ALIAS_SFP_ANNUAL_SALARY, true);
        m_fieldContractMonths = translateAliasToJavaName(ALIAS_CONTRACT_MONTHS, true);
        m_fieldTeachId = translateAliasToJavaName(ALIAS_TEACH_ID, true);
        m_contextOid = (String) getParameter(PARAM_SCHOOL_YEAR_CONTEXT);
        m_fieldSfpContractDays = translateAliasToJavaName(ALIAS_SFP_CONTRACT_DAYS, true);
        m_fieldSfpSeparationReason = translateAliasToJavaName(ALIAS_SFP_TERMINATION_REASON, true);
        m_fieldSfpProfInd = translateAliasToJavaName(ALIAS_SFP_PROF_DEV, true);
        m_fieldSfpCertExempt = translateAliasToJavaName(ALIAS_SFP_CERT_EXEMPT, true);
        m_fieldStfPriorExperience = translateAliasToJavaName(ALIAS_STF_PRIOR_EXP, true);
        m_fieldSfpPriorYearsExperience = translateAliasToJavaName(ALIAS_SFP_TEACH_EXP, true);
        m_fieldSfpItinerantTeacher = translateAliasToJavaName(ALIAS_SFP_ITINERANT_TEACHER, true);
        m_fieldSklExcludeSchool = translateAliasToJavaName(ALIAS_SKL_EXCLUDE_SCHOOL, true);

    }

    /**
     * Build the list of most Common Calendars for schools.
     */
    private void initializeMostCommonCalendars() {
        Map<String, Map<String, Collection<SchoolCalendar>>> mapSchoolCalendars = getSchoolCalendars();

        m_mostCommonCalendars = new HashMap();

        String activeStatus = PreferenceManager.getPreferenceValue(getOrganization(),
                SystemPreferenceDefinition.STUDENT_ACTIVE_CODE);

        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(SisStudent.COL_ENROLLMENT_STATUS, activeStatus);

        String[] columns = new String[] {SisStudent.COL_SCHOOL_OID, SisStudent.COL_CALENDAR_CODE, "count(*)"};

        ReportQueryByCriteria query = new ReportQueryByCriteria(SisStudent.class, columns, criteria);
        query.addGroupBy(SisStudent.COL_SCHOOL_OID);
        query.addGroupBy(SisStudent.COL_CALENDAR_CODE);
        query.addOrderByDescending("count(*)");

        ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                Object[] row = (Object[]) iterator.next();
                String schoolOid = (String) row[0];
                String calendarCode = (String) row[1];

                if (!m_mostCommonCalendars.containsKey(schoolOid)) {
                    Map<String, Collection<SchoolCalendar>> mapCalendars = mapSchoolCalendars.get(schoolOid);
                    if (mapCalendars != null && mapCalendars.containsKey(calendarCode)) {
                        SchoolCalendar schoolCalendar = mapCalendars.get(calendarCode).iterator().next();
                        m_mostCommonCalendars.put(schoolOid, schoolCalendar.getCalendarId());
                    }
                }
            }
        } finally {
            iterator.close();
        }

        // Add schools without students - any calendar will do
        for (String oid : mapSchoolCalendars.keySet()) {
            if (!m_mostCommonCalendars.containsKey(oid)) {
                Map<String, Collection<SchoolCalendar>> mapCalendars = mapSchoolCalendars.get(oid);
                SchoolCalendar schoolCalendar = mapCalendars.values().iterator().next().iterator().next();
                m_mostCommonCalendars.put(oid, schoolCalendar.getCalendarId());
            }
        }
    }

    /**
     * Initialize map of map of school calendar dates. Inner map is keyed on calendar code, outer is
     * keyd on school oid.
     */
    private void initializeInSessionDates() {
        if (m_mostCommonCalendars == null) {
            initializeMostCommonCalendars();
        }

        Criteria criteria = new Criteria();

        if (m_mostCommonCalendars.keySet().isEmpty()) {
            criteria.addEqualTo(SchoolCalendarDate.REL_SCHOOL_CALENDAR + "." + SchoolCalendar.COL_SCHOOL_OID,
                    "__dummy__");
        } else {
            criteria.addIn(SchoolCalendarDate.REL_SCHOOL_CALENDAR + "." + SchoolCalendar.COL_SCHOOL_OID,
                    m_mostCommonCalendars.keySet());
        }

        if (m_mostCommonCalendars.values().isEmpty()) {
            criteria.addEqualTo(SchoolCalendarDate.REL_SCHOOL_CALENDAR + "." + SchoolCalendar.COL_CALENDAR_ID,
                    "__dummy__");
        } else {
            criteria.addIn(SchoolCalendarDate.REL_SCHOOL_CALENDAR + "." + SchoolCalendar.COL_CALENDAR_ID,
                    m_mostCommonCalendars.values());
        }

        criteria.addEqualTo(SchoolCalendarDate.COL_IN_SESSION_INDICATOR, Boolean.TRUE);
        criteria.addLessOrEqualThan(SchoolCalendarDate.COL_DATE, getCurrentContext().getEndDate());
        criteria.addGreaterOrEqualThan(SchoolCalendarDate.COL_DATE, getCurrentContext().getStartDate());

        QueryByCriteria query = new QueryByCriteria(SchoolCalendarDate.class, criteria);
        query.addOrderByAscending(SchoolCalendarDate.COL_DATE);
        String[] fields = new String[] {SchoolCalendarDate.REL_SCHOOL_CALENDAR + "." + SchoolCalendar.COL_SCHOOL_OID,
                SchoolCalendarDate.REL_SCHOOL_CALENDAR + "." + SchoolCalendar.COL_CALENDAR_ID};
        int[] sizes = new int[] {1024, 1024};

        m_sklCalDates = getBroker().getGroupedCollectionByQuery(query, fields, sizes);
    }

    /**
     * Build map of maps of SchoolCalendars keyed on school oid and school calendar id.
     *
     * @return Map<String, Map<String, Collection<SchoolCalendar>>>
     */
    private Map<String, Map<String, Collection<SchoolCalendar>>> getSchoolCalendars() {
        X2Criteria casCriteria = new X2Criteria();

        casCriteria.addEqualTo(SchoolCalendar.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());
        // Filter to eliminate unused schools.
        casCriteria.addNotEqualTo(SchoolCalendar.REL_SCHOOL + PATH_DELIMITER +
                SisSchool.COL_INACTIVE_INDICATOR,
                Boolean.TRUE);
        casCriteria.addNotEqualTo(SchoolCalendar.REL_SCHOOL + PATH_DELIMITER +
                SisSchool.COL_ARCHIVE_INDICATOR,
                Boolean.TRUE);
        casCriteria.addNotEqualTo(SchoolCalendar.REL_SCHOOL + PATH_DELIMITER +
                m_fieldSklExcludeSchool,
                Boolean.TRUE);

        QueryByCriteria query = new QueryByCriteria(SchoolCalendar.class, casCriteria);

        return getBroker().getGroupedCollectionByQuery(query, new String[] {SchoolCalendar.COL_SCHOOL_OID,
                SchoolCalendar.COL_CALENDAR_ID}, new int[] {100, 5});
    }

    /**
     * Returns the criteria that retrieves all staff that should be included in the export.
     *
     * @return Criteria
     */
    private Criteria getStaffCriteria() {
        Criteria staffCriteria = new Criteria();
        staffCriteria.addNotNull(X2BaseBean.COL_OID);

        applyInputCriteria(staffCriteria, true, null);
        if (!isSchoolContext()) {
            staffCriteria.addNotEqualTo(
                    Staff.REL_SCHOOL + ModelProperty.PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR,
                    BooleanAsStringConverter.TRUE);
            staffCriteria.addNotEqualTo(
                    Staff.REL_SCHOOL + ModelProperty.PATH_DELIMITER + SisSchool.COL_ARCHIVE_INDICATOR,
                    BooleanAsStringConverter.TRUE);
        }
        staffCriteria.addNotEqualTo(
                Staff.REL_SCHOOL + ModelProperty.PATH_DELIMITER + m_fieldSklExcludeSchool,
                BooleanAsStringConverter.TRUE);

        // Check exclude staff from state reporting flag if present.
        if (!StringUtils.isEmpty(m_staffExcludeIndicatorField)) {
            X2Criteria excludeCriteria = new X2Criteria();
            excludeCriteria.addNotEqualTo(m_staffExcludeIndicatorField, BooleanAsStringConverter.TRUE);

            X2Criteria isNullCriteria = new X2Criteria();
            isNullCriteria.addIsNull(m_staffExcludeIndicatorField);

            excludeCriteria.addOrCriteria(isNullCriteria);

            staffCriteria.addAndCriteria(excludeCriteria);
        }

        if (m_staffPositionMap.keySet().isEmpty()) {
            staffCriteria.addEqualTo(X2BaseBean.COL_OID, "__dummy__");
        } else {
            staffCriteria.addIn(X2BaseBean.COL_OID, m_staffPositionMap.keySet());
        }
        staffCriteria.addNotNull(m_fieldTeachId);

        return staffCriteria;
    }

    /**
     * Load StaffPosition table.
     */
    private void loadStaffPosition() {
        m_staffPositionMap = new HashMap();

        X2Criteria sfpCriteria = new X2Criteria();

        sfpCriteria.addLessThan(m_fieldSfpStartDate, getCurrentContext().getEndDate());

        X2Criteria endDateCriteria = new X2Criteria();
        X2Criteria orEndDateCriteria = new X2Criteria();

        endDateCriteria.addGreaterThan(m_fieldSfpEndDate, getCurrentContext().getStartDate());
        orEndDateCriteria.addIsNull(m_fieldSfpEndDate);

        endDateCriteria.addOrCriteria(orEndDateCriteria);

        sfpCriteria.addAndCriteria(endDateCriteria);

        QueryByCriteria staffPositionQuery = new QueryByCriteria(StaffPosition.class, sfpCriteria);

        m_staffPositionMap =
                getBroker().getGroupedCollectionByQuery(staffPositionQuery, StaffPosition.COL_STAFF_OID, 100);
    }
}
