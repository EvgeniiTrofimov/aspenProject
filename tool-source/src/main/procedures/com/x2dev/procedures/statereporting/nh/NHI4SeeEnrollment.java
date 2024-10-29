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

package com.x2dev.procedures.statereporting.nh;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import static com.x2dev.sis.tools.stateexports.StudentHistoryHelper.CALENDAR_ANY;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.Race;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.PreferenceSet;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.follett.fsc.core.k12.web.WebUtils;
import com.x2dev.sis.model.beans.ConductAction;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAttendance;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.business.EnrollmentManager;
import com.x2dev.sis.model.business.EnrollmentSnapshot;
import com.x2dev.sis.tools.stateexports.StudentEnrollmentSpan;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.CollectionUtils;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.types.PlainDate;
import java.lang.reflect.InvocationTargetException;
import java.math.BigDecimal;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;
import org.apache.commons.beanutils.PropertyUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * This class implements the data export for New Hampshire's i4see Enrollment export.
 *
 * @author X2 Development Corporation
 */
public class NHI4SeeEnrollment extends StateReportData {

    /**
     * Implementation of StateReportEntity to be used by the I4SeeEnrollment export.
     * This must be a public static inner class with a public no argument
     * constructor so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class I4SeeEntity extends StateReportEntity {
        /**
         * include summer withdrawals input param name
         */
        private static final String PARAM_INCLUDE_SUMMER_WITHDRAWALS = "includeSummerWithdrawals";

        /**
         * Cached values for retrievers to share.
         */
        private EnrollmentSnapshot m_snapshot;

        /**
         * Place holders for calculated unmapped fields. These can be written back to the database
         * in postProcess if update flag is set. Also, holds some calculated values that have
         * been overridden with default or related values.
         *
         * Map key should be field alias constant.
         */
        private Map<String, Object> m_updateValues = null;

        /*
         * List to hold the entry and withdrawal enrollment records
         */
        List<StudentEnrollmentSpan> m_enrollmentSpans = new ArrayList<StudentEnrollmentSpan>();

        private NHI4SeeEnrollment m_data;

        private Map<String, String> m_enrollmentStatusMap = new HashMap<String, String>();

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public I4SeeEntity() {
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

            FieldDefinition field = m_data.getFieldDefinition(ALIAS_I4SEE_300_DAYS_IN_ATTENDANCE);
            // SisStudent student = (SisStudent) getBean();
            // String daysInAttendance = (String)
            // student.getFieldValueByAlias(ALIAS_I4SEE_300_DAYS_IN_ATTENDANCE);


            // TODO: Remove EOY cludge

            if (REPORT_TYPE_EOY.equals(m_data.m_reportType)) {
                String entryDate = getFieldValue("Entry Date");
                if (StringUtils.isEmpty(entryDate)) {
                    String errorMessage = null;
                    errorMessage = "Record invalid for EOY";
                    error = new StateReportValidationError(getEntityName(), "", errorMessage, "");
                }
            }

            if (field != null) {
                /*
                 * Get membership days parameter
                 */
                double membershipCountAsDouble = 0;

                String membershipCount = m_data.getMembershipDays(this);

                if (!StringUtils.isEmpty(membershipCount) && StringUtils.isNumeric(membershipCount)) {
                    membershipCountAsDouble = Double.parseDouble(membershipCount);
                }

                String errorMessage = null;
                // check enrollment count and membership days parameter.
                if ((m_data.m_requireMemberDay && (membershipCountAsDouble > 0)) || !m_data.m_requireMemberDay) {
                    // No filtering.
                } else {
                    // Student filtered.
                    errorMessage = "0 member days - excluded from export";
                    error = new StateReportValidationError(this, field, errorMessage, "");
                }

                // if this is a W2 during the summer, don't report this entity
                StudentEnrollment withdrawlEnrollment = getWithdrawal();
                if ((error == null) && (withdrawlEnrollment != null) &&
                        (CODE_ENROLLMENT_W2.equals(withdrawlEnrollment.getEnrollmentCode()) ||
                                CODE_ENROLLMENT_W4.equals(withdrawlEnrollment.getEnrollmentCode()))) {
                    PlainDate startDate = m_data.m_reportDate;
                    EnrollmentSnapshot snapshot = getSnapshot(startDate, field);
                    PlainDate firstDay = m_data.findFirstEnrollmentDay(snapshot);

                    if (!withdrawlEnrollment.getEnrollmentDate().after(firstDay)) {
                        // Student filtered.
                        errorMessage = "summer withdrawal - excluded from export";
                        error = new StateReportValidationError(this, field, errorMessage, "");
                    }
                }

                StudentEnrollment entryEnrollment = getWithdrawal();
                if ((error == null) && (entryEnrollment != null) &&
                        (entryEnrollment.getStatusCode() != null) &&
                        entryEnrollment.getStatusCode().equals(CODE_ENROLLMENT_STATUS_PREREG)) {
                    errorMessage = "Enrollment is a Pre Registration - excluded from export";
                    error = new StateReportValidationError(this, field, errorMessage, "");
                }
            }

            return error;
        }

        /**
         * Returns the entry enrollment record for the current student.
         *
         * return StudentEnrollment
         *
         * @return Student enrollment span
         */
        public StudentEnrollmentSpan getCurrentSpan() {
            StudentEnrollmentSpan span = null;
            int index = getCurrentRow();

            if ((m_enrollmentSpans != null) && (index >= 0) && (index < m_enrollmentSpans.size())) {
                span = m_enrollmentSpans.get(index);
            }

            return span;
        }

        /**
         * Calculate and return enrollment status for current record.
         *
         * @return String
         */
        public String getEnrollmentStatus() {
            return getEnrollmentStatus(getCurrentSpan());
        }

        /**
         * Calculate and return enrollment status for input span.
         *
         * @param span StudentEnrollmentSpan
         * @return String
         */
        public String getEnrollmentStatus(StudentEnrollmentSpan span) {

            String key = getSpanKey(span);
            String enrollmentStatus = m_enrollmentStatusMap.get(key);
            if (enrollmentStatus == null) {
                StudentEnrollment entry = getLastActiveEnrollment(span);
                StudentEnrollment withdrawal = getWithdrawal(span);


                StudentEnrollment enrollment = entry;

                /*
                 * Check to see if the enrollment status should be taken from the entry record or
                 * the withdrawal record (which ever is the latest). W12 and W1 withdrawals
                 * cause us to use the entry's enrollment status, not the withdrawals
                 */
                if ((entry != null) && (withdrawal != null)
                        && StudentEnrollment.WITHDRAWAL.equals(withdrawal.getEnrollmentType())
                        && withdrawal.getEnrollmentDate().after(entry.getEnrollmentDate())
                        && !CODE_ENROLLMENT_W12.equals(withdrawal.getEnrollmentCode())
                        && !CODE_ENROLLMENT_W1.equals(withdrawal.getEnrollmentCode())) {
                    enrollment = withdrawal;
                }

                if (enrollment != null) {
                    enrollmentStatus = (String) enrollment.getFieldValueByAlias(ALIAS_I4SEE_210_ENROLLMENT_STATUS);
                }

                /*
                 * If the enrollmentStatus is empty, see if the status is present on the paired
                 * enrollment record
                 */
                if (StringUtils.isEmpty(enrollmentStatus)) {
                    /*
                     * status empty, try the 'other' paired record
                     */
                    if ((enrollment == withdrawal) && (entry != null)) {
                        enrollmentStatus = (String) entry.getFieldValueByAlias(ALIAS_I4SEE_210_ENROLLMENT_STATUS);
                        // enrollmentStatus = (String) WebUtils.getProperty(entry, javaName);
                    } else if (withdrawal != null) {
                        enrollmentStatus = (String) withdrawal.getFieldValueByAlias(ALIAS_I4SEE_210_ENROLLMENT_STATUS);
                        // enrollmentStatus = (String) WebUtils.getProperty(withdrawal, javaName);
                    }

                }

                if (StringUtils.isEmpty(enrollmentStatus)) {
                    // use previous not empty enrollment status code.
                    List<StudentEnrollment> enrollments = m_data.m_helper.getStudentEnrollments(entry.getStudent());
                    if (enrollments != null) {
                        boolean beforeEntry = false;
                        for (StudentEnrollment item : enrollments) {
                            if (beforeEntry) {
                                if ("EYW".contains(item.getEnrollmentType())) {
                                    enrollmentStatus =
                                            (String) item.getFieldValueByAlias(ALIAS_I4SEE_210_ENROLLMENT_STATUS);
                                    if (!StringUtils.isEmpty(enrollmentStatus)) {
                                        break;
                                    }
                                }
                            }
                            if (item.equals(entry)) {
                                beforeEntry = true;
                            }
                        }
                    }
                }

                if (StringUtils.isEmpty(enrollmentStatus) && (withdrawal == null)) {
                    enrollmentStatus = CODE_ENROLLMENT_STATUS_1;
                }

                // if this is a W2 during the summer, don't report this entity
                EnrollmentSnapshot snapshot =
                        getSnapshot(m_data.m_reportDate, m_data.getFieldDefinition(I4SEE_210_ENROLLMENT_STATUS));
                PlainDate firstDay = m_data.findFirstEnrollmentDay(snapshot);

                if ((withdrawal != null) && !withdrawal.getEnrollmentDate().after(firstDay)) {
                    String enrollmentCode = withdrawal.getEnrollmentCode();
                    if (REPORT_TYPE_BOY.equals(m_data.m_reportType)
                            && (!StringUtils.isEmpty(enrollmentCode))
                            && enrollmentCode.contains(StudentEnrollment.WITHDRAWAL)) {
                        if (!(CODE_ENROLLMENT_W2.equals(enrollmentCode) || CODE_ENROLLMENT_W4.equals(enrollmentCode))) {
                            enrollmentStatus = CODE_ENROLLMENT_STATUS_7;
                        }
                    }
                }

                m_enrollmentStatusMap.put(key, enrollmentStatus);
            }

            return enrollmentStatus;
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
                    (getData().isSchoolContext() ? ", SCHOOL: " + student.getSchool().getName() : "") +
                    "]";

            return name;
        }


        /**
         * Gets the entry.
         *
         * @param span StudentEnrollmentSpan
         * @return Student enrollment
         */
        public StudentEnrollment getEntry(StudentEnrollmentSpan span) {
            StudentEnrollment entry = null;
            entry = span.getFirstActiveEnrollment();
            for (StudentEnrollment enrollment : span.getEnrollments()) {
                if (StudentEnrollment.ENTRY.equals(enrollment.getEnrollmentType()) &&
                        !entry.getEnrollmentDate().after(enrollment.getEnrollmentDate()) &&
                        m_data.m_reportDate.after(enrollment.getEnrollmentDate())) {
                    entry = enrollment;
                }
            }
            return entry;
        }

        /**
         * Returns the entry enrollment record for the current student.
         *
         * return StudentEnrollment
         *
         * @return Student enrollment
         */
        public StudentEnrollment getEntry() {

            return getEntry(getCurrentSpan());
        }

        /**
         * Gets the entry date.
         *
         * @return Plain date
         */
        public PlainDate getEntryDate() {
            return getEntryDate(getCurrentSpan());
        }

        /**
         * Gets the entry date.
         *
         * @param span StudentEnrollmentSpan
         * @return Plain date
         */
        public PlainDate getEntryDate(StudentEnrollmentSpan span) {
            StudentEnrollment entryEnrollment = getEntry(span);
            PlainDate firstDay = null;
            SisSchool school = entryEnrollment == null ? null : entryEnrollment.getSchool();

            if (school != null) {
                firstDay = m_data.findFirstEnrollmentDay(school, (SisStudent) this.getBean());
            }

            PlainDate entryDate = null;

            StudentEnrollment withdrawalEnrollment = getWithdrawal(span);
            PlainDate withdrawlDate = null;

            if (entryEnrollment != null) {
                entryDate = entryEnrollment.getEnrollmentDate();

                if ((entryDate != null)
                        && (firstDay != null)
                        && entryDate.before(firstDay)) {
                    entryDate = firstDay;
                }
            } else {
                entryDate = firstDay;
            }

            // if the student has withdrawn, return blank.
            if (m_data.hasStudentWithdrawn(entryEnrollment, withdrawalEnrollment) && entryDate != null
                    && entryDate.before(entryEnrollment.getEnrollmentDate())) {
                entryDate = null;
            }

            // Student withdrawn in the summer, report only the WD date and code.
            if (withdrawalEnrollment != null) {
                withdrawlDate = withdrawalEnrollment.getEnrollmentDate();
                if (withdrawlDate != null && entryDate != null && withdrawlDate.before(entryDate)) {
                    entryDate = null;
                }
            }

            // When student Enrollment Code state value = "7" - Entry date must not be null /
            // must be the first day of the school year.
            String enrollmentCode = getEnrollmentStatus(span);
            if (CODE_ENROLLMENT_STATUS_7.equals(enrollmentCode)) {
                entryDate = firstDay;
            }
            return entryDate;
        }

        /**
         * Gets the exit date.
         *
         * @return Plain date
         */
        public PlainDate getExitDate() {
            StudentEnrollmentSpan nextSpan = null;
            int index = getCurrentRow() + 1;
            if ((m_enrollmentSpans != null)
                    && (index < m_enrollmentSpans.size())) {
                nextSpan = m_enrollmentSpans.get(index);
            }
            return getExitDate(getCurrentSpan(), nextSpan);
        }

        /**
         * Gets the exit date.
         *
         * @param span StudentEnrollmentSpan
         * @param nextSpan StudentEnrollmentSpan
         * @return Plain date
         */
        public PlainDate getExitDate(StudentEnrollmentSpan span, StudentEnrollmentSpan nextSpan) {
            StudentEnrollment withdrawalEnrollment = getWithdrawal(span);

            PlainDate exitDate = null;
            Date entryDate = null;

            StudentEnrollment entryEnrollment = getEntry(span);
            if (entryEnrollment != null) {
                entryEnrollment = span.getFirstActiveEnrollment();
            }

            StudentEnrollment nextEntryEnrollment = null;
            if (nextSpan != null) {
                nextEntryEnrollment = nextSpan.getFirstActiveEnrollment();
            }

            if (withdrawalEnrollment != null) {
                exitDate = withdrawalEnrollment.getEnrollmentDate();
                entryDate = entryEnrollment.getEnrollmentDate();
                // entryDate = m_dateFormat.parse(entity.getFieldValue(I4SEE_230_ENTRY_DATE));

                if (!m_data.m_enrollManager.getWithdrawalIsMemberDay()
                        || ((nextEntryEnrollment != null)
                                && (nextEntryEnrollment.getEnrollmentDate().equals(exitDate))
                                && m_data.m_enrollManager.getWithdrawalIsMemberDay()
                                && m_data.m_enrollManager.getEntryIsMemberDay())) {
                    /*
                     * The exit date must be equal to or greater than the entry date on this line,
                     * so don't subtract a day if this line's entry exists and is the same day as
                     * exit
                     */
                    if ((entryEnrollment == null) ||
                            ((entryEnrollment != null)
                                    && (entryDate.compareTo(exitDate) < 0))) {
                        exitDate = DateUtils.add(exitDate, -1);
                    }
                }
            }

            return exitDate;
        }

        /**
         * Gets the grade level.
         *
         * @return String
         */
        public String getGradeLevel() {
            ReferenceCode gradeLevelCode = m_data.getGradeCode(((SisStudent) getBean()).getGradeLevel());
            StudentEnrollmentSpan span = getCurrentSpan();
            if (span != null && span.getFirstActiveEnrollment() != null) {
                int yogEnr = span.getFirstActiveEnrollment().getYog();
                int yogStd = ((SisStudent) getBean()).getYog();
                if (yogEnr > 0 && yogEnr != yogStd) {
                    ReferenceCode code = m_data.getGradeCode(yogEnr);
                    if (code != null) {
                        gradeLevelCode = code;
                    }
                }
            }
            return gradeLevelCode == null ? "" : gradeLevelCode.getStateCode();
        }

        /**
         * returns a boolean indicating if the student is a non-graduating senior.
         *
         * @return boolean
         */
        public boolean getIsNonGraduatingSenior() {
            boolean isNonGraduatingSenior = false;

            String promotedInd = getFieldValue(ALIAS_I4SEE_510_PROMOTED_IND);
            String studentGradeLevel = getFieldValue(I4SEE_400_GRADE);

            // boolean isNonGraduatingSenior = "12".equals(studentGradeLevel) &&
            // ("1".equals(promotedInd) || "2".equals(promotedInd));
            if (CODE_GRADE_LEVEL_12.equals(studentGradeLevel)
                    && (CODE_PROMOTED_IND_SAME_GRADE.equals(promotedInd)
                            || CODE_PROMOTED_IND_PROMOTED.equals(promotedInd))) {
                isNonGraduatingSenior = true;
            }

            return isNonGraduatingSenior;
        }

        /**
         * Returns last active enrollment record for the input span.
         *
         * @param span StudentEnrollmentSpan
         * @return Student enrollment
         */
        public StudentEnrollment getLastActiveEnrollment(StudentEnrollmentSpan span) {
            StudentEnrollment lastEnrollment = getEntry(span);

            Iterator<StudentEnrollment> iterator = span.getEnrollments().iterator();
            while (iterator.hasNext()) {
                StudentEnrollment enrollment = iterator.next();
                if (iterator.hasNext()) { // skip last enrollment
                    if ((lastEnrollment == null) ||
                            (!enrollment.getEnrollmentType().equals(StudentEnrollment.WITHDRAWAL) &&
                                    lastEnrollment.getEnrollmentDate().before(enrollment.getEnrollmentDate()) &&
                                    (!StringUtils.isEmpty(
                                            (String) enrollment.getFieldValueByAlias(ALIAS_I4SEE_220_TOWN_RESPONSIBLE))
                                            ||
                                            !StringUtils.isEmpty((String) enrollment
                                                    .getFieldValueByAlias(ALIAS_I4SEE_225_DISTRICT_RESPONSIBLE))
                                            ||
                                            !StringUtils.isEmpty((String) enrollment
                                                    .getFieldValueByAlias(ALIAS_I4SEE_210_ENROLLMENT_STATUS))))) {
                        lastEnrollment = enrollment;
                    }
                }
            }
            return lastEnrollment;
        }

        /**
         * Returns last active enrollment record for the current span.
         *
         * return StudentEnrollment
         *
         * @return Student enrollment
         */
        public StudentEnrollment getLastActiveEnrollment() {

            return getLastActiveEnrollment(getCurrentSpan());
        }

        /**
         * Return the enrollment snapshot that is used by some fieldRetrievers to get enrollment
         * data.
         *
         * @param reportDate PlainDate
         * @param field FieldDefinition
         * @return the EnrollmentSnapshot for the student.
         */
        public EnrollmentSnapshot getSnapshot(PlainDate reportDate, FieldDefinition field) {
            if (m_snapshot == null) {
                m_snapshot = getSnapshot((SisStudent) getBean(), reportDate, field);
            }

            return m_snapshot;
        }


        /**
         * Gets the withdrawal.
         *
         * @param span StudentEnrollmentSpan
         * @return Student enrollment
         */
        public StudentEnrollment getWithdrawal(StudentEnrollmentSpan span) {
            StudentEnrollment withdrawal = span.getFirstInactiveEnrollment();
            if (withdrawal != null && withdrawal.getSchool() != null) {
                PlainDate lastSchoolDate = m_data.findLastEnrollmentDay(withdrawal.getSchool(),
                        ((SisStudent) getBean()).getCalendarCode());
                // skip withdrawals after school year end
                if (lastSchoolDate != null && lastSchoolDate.before(withdrawal.getEnrollmentDate())) {
                    withdrawal = null;
                }
            }
            return withdrawal;
        }

        /**
         * Returns the withdrawal enrollment record for the current student.
         *
         * return StudentEnrollment
         *
         * @return Student enrollment
         */
        public StudentEnrollment getWithdrawal() {
            return getWithdrawal(getCurrentSpan());
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

            m_data = (NHI4SeeEnrollment) getData();

            SisStudent student = (SisStudent) bean;

            // set false and make filter separate because it is more difficult logic
            List<StudentEnrollmentSpan> allEnrollmentSpans = null;

            Boolean includeSummerWithdrawals = (Boolean) m_data.getParameter(PARAM_INCLUDE_SUMMER_WITHDRAWALS);


            if (includeSummerWithdrawals != null && includeSummerWithdrawals.booleanValue()) {
                // summer withdrawals mode enable
                allEnrollmentSpans = getStudentEnrollmentSpans((SisStudent) bean, false);
                PlainDate reportStartDate = m_data.m_summerStartDate.before(m_data.getCurrentContext().getStartDate())
                        ? m_data.m_summerStartDate
                        : m_data.getCurrentContext().getStartDate();
                PlainDate reportEndDate = m_data.m_reportDate;

                PlainDate lastSummerWithdrawalDate = findLastSummerWithdrawalDate(allEnrollmentSpans);
                PlainDate beginDate = reportStartDate;
                PlainDate endDate = null;
                // if summer w records is not found - use standard report period for limit data
                if (lastSummerWithdrawalDate == null) {
                    endDate = reportEndDate;
                } else {
                    if (!m_data.isSchoolContext()) {
                        // if summer mode + summer record exist + chosen all schools - just use more
                        // latest date for limit records
                        endDate =
                                lastSummerWithdrawalDate.after(reportEndDate) ? lastSummerWithdrawalDate
                                        : reportEndDate;
                    } else {

                        endDate = lastSummerWithdrawalDate;
                        // SchoolContext. we can use report End date for limit records if it is
                        // after summer W date
                        // and on the report date student attend selected school
                        StudentEnrollmentSpan lastEnrSpan = findLastEnrollmentSpanInReportPeriod(allEnrollmentSpans);
                        if (lastSummerWithdrawalDate.before(reportEndDate) && lastEnrSpan != null
                                && lastEnrSpan.getSchool().getOid().equals(m_data.getSchool().getOid())) {
                            endDate = reportEndDate;
                        }
                    }
                }
                limitSpanByDates(allEnrollmentSpans, beginDate, endDate);


            } else {
                // summer withdrawals mode disable
                allEnrollmentSpans = getStudentEnrollmentSpans((SisStudent) bean, true);
            }

            if (m_data.isSchoolContext() && !allEnrollmentSpans.isEmpty()) {
                // if school context - last enrollment/or withdrawal should be in selected school
                StudentEnrollmentSpan lastStdEnrSpan = allEnrollmentSpans.get(allEnrollmentSpans.size() - 1);
                if (!lastStdEnrSpan.getSchool().getOid().equals(m_data.getSchool().getOid())) {
                    allEnrollmentSpans = new ArrayList<StudentEnrollmentSpan>();
                }

            }

            // Add spans that overlap with school year
            for (StudentEnrollmentSpan span : allEnrollmentSpans) {
                if (includeSummerWithdrawals != null && includeSummerWithdrawals.booleanValue()) {
                    m_enrollmentSpans.add(span);
                } else if (span.getFirstInactiveEnrollment() == null) {
                    m_enrollmentSpans.add(span);
                } else {
                    PlainDate lastDate = span.getFirstInactiveEnrollment().getEnrollmentDate();
                    PlainDate startDate =
                            m_data.findFirstEnrollmentDay(span.getFirstInactiveEnrollment().getSchool(), student);
                    if (lastDate != null && !lastDate.before(startDate)) {
                        m_enrollmentSpans.add(span);
                    }
                }
            }

            // if no span overlaps with school year, add last span
            if (m_enrollmentSpans.isEmpty() && !allEnrollmentSpans.isEmpty()) {
                m_enrollmentSpans.add(allEnrollmentSpans.get(allEnrollmentSpans.size() - 1));
            }

            // run fixPreferenceMemberOnWithdrawalIssue after adding any enrollment
            fixPreferenceMemberOnWithdrawalIssue();
            if (REPORT_TYPE_EOY.equals(m_data.m_reportType)) {
                deleteSummerRecords();
                removeSpansWithSameEntryAndExitDate();
            }
            removeSpansWithNotReportedSchool();

            setRowCount(m_enrollmentSpans.size());
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
            if (m_data.m_updateRecords) {
                try {
                    Converter integerConverter = ConverterFactory.getConverterForClass(Converter.INTEGER_CONVERTER,
                            Locale.getDefault(), true);

                    FieldDefinition field = m_data.getFieldDefinition(I4SEE_300_DAYS_IN_ATTENDANCE);
                    PropertyUtils.setProperty(getBean(), field.getBeanPath(),
                            integerConverter.stringToJava(getFieldValue(I4SEE_300_DAYS_IN_ATTENDANCE)));

                    field = m_data.getFieldDefinition(I4SEE_310_DAYS_ABSENT);
                    PropertyUtils.setProperty(getBean(), field.getBeanPath(),
                            integerConverter.stringToJava(getFieldValue(I4SEE_310_DAYS_ABSENT)));

                    field = m_data.getFieldDefinition(I4SEE_520_SUSPENSIONS_IN);
                    PropertyUtils.setProperty(getBean(), field.getBeanPath(),
                            integerConverter.stringToJava(getFieldValue(I4SEE_520_SUSPENSIONS_IN)));

                    field = m_data.getFieldDefinition(I4SEE_530_SUSPENSIONS_OUT);
                    PropertyUtils.setProperty(getBean(), field.getBeanPath(),
                            integerConverter.stringToJava(getFieldValue(I4SEE_530_SUSPENSIONS_OUT)));

                    field = m_data.getFieldDefinition(I4SEE_660_ORIGINAL_YOG);
                    PropertyUtils.setProperty(getBean(), field.getBeanPath(), getFieldValue(I4SEE_660_ORIGINAL_YOG));

                    field = m_data.getFieldDefinition(I4SEE_420_RACE);
                    PropertyUtils.setProperty(getBean(), field.getBeanPath(), getFieldValue(I4SEE_420_RACE));

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
         * Sets a field value before mapping.
         *
         * Certain calculated data fields (Sped valued) can be stored
         * and retrieved before reference code mapping by DOE field constant.
         *
         * @param doeId String
         * @param value Object
         */
        public void setUpdateValue(String doeId, Object value) {
            if (m_updateValues == null) {
                m_updateValues = new HashMap<String, Object>();
            }

            m_updateValues.put(doeId, value);
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


        /**
         * delete summer records. It is record where
         * Enrollment Code Field Position 40 = "7"
         * Days in Half Days Field Position 160 = "0"
         * Days Half Days Absent Field Position 170 = "0"
         *
         * @throws X2BaseException exception
         */
        private void deleteSummerRecords() throws X2BaseException {
            Iterator<StudentEnrollmentSpan> iterator = m_enrollmentSpans.iterator();

            while (iterator.hasNext()) {
                StudentEnrollmentSpan span = iterator.next();
                String enrCode = getEnrollmentStatus(span);
                // 160 field it is membershipdays(*1 or *2) - 170 field
                // if 160 < 0 - error. it mean that 170 we can don not calculate there
                int membershipdays = span.getMembershipDays();

                if (CODE_ENROLLMENT_STATUS_7.equals(enrCode) && membershipdays == 0) {
                    iterator.remove();
                }
            }
        }


        /**
         * try find last enrollment from allEnrollmentSpans limited by reports date
         * start date - current context start date
         * end date - report date.
         *
         * @param allEnrollmentSpans List<StudentEnrollmentSpan>
         * @return StudentEnrollmentSpan
         */
        private StudentEnrollmentSpan findLastEnrollmentSpanInReportPeriod(List<StudentEnrollmentSpan> allEnrollmentSpans) {
            PlainDate reportStrarDate = m_data.getCurrentContext().getStartDate();
            PlainDate reportEndDate = m_data.m_reportDate;
            StudentEnrollmentSpan lastEnrSpan = null;
            for (StudentEnrollmentSpan stdEnrSpan : allEnrollmentSpans) {

                PlainDate firstActiveDate = stdEnrSpan.getFirstActiveDate();
                if (firstActiveDate != null && !firstActiveDate.after(reportEndDate)
                        && !firstActiveDate.before(reportStrarDate)) {
                    if (lastEnrSpan == null
                            || (lastEnrSpan.getFirstActiveDate() != null && lastEnrSpan.getFirstActiveDate()
                                    .before(firstActiveDate))) {
                        lastEnrSpan = stdEnrSpan;

                    }
                }
            }

            if (lastEnrSpan == null && !allEnrollmentSpans.isEmpty()) {
                lastEnrSpan = allEnrollmentSpans.get(allEnrollmentSpans.size() - 1);
            }
            return lastEnrSpan;
        }

        /**
         * try find latest enrollment date for W record limited by summer start and end dates and
         * selected school.
         *
         * @param allEnrollmentSpans List<StudentEnrollmentSpan>
         * @return PlainDate
         */
        private PlainDate findLastSummerWithdrawalDate(List<StudentEnrollmentSpan> allEnrollmentSpans) {
            PlainDate summerStartDate = m_data.m_summerStartDate;
            PlainDate summerEndDate = m_data.m_summerEndDate;

            PlainDate lastSummerWithdrawalDate = null;
            for (StudentEnrollmentSpan stdEnrSpan : allEnrollmentSpans) {
                StudentEnrollment firstInactive = stdEnrSpan.getFirstInactiveEnrollment();
                PlainDate wDate = firstInactive == null ? null : firstInactive.getEnrollmentDate();
                if (wDate != null && !wDate.after(summerEndDate) && !wDate.before(summerStartDate)) {
                    if (!m_data.isSchoolContext()
                            || stdEnrSpan.getSchool().getOid().equals(m_data.getSchool().getOid())) {
                        if (lastSummerWithdrawalDate == null || lastSummerWithdrawalDate.before(wDate)) {
                            lastSummerWithdrawalDate = wDate;
                        }
                    }

                }
            }
            return lastSummerWithdrawalDate;
        }

        /**
         * helper has issue FOR m_preferenceMemberOnWithdrawal = false case
         * if w date - it is first in session date -
         * helper try find previous in session date but fail
         * because w it is ALREADY first in session date and it use this date like LAST ACTIVE.
         * like result appear issue with getMembershipDays method - span count one attendance
         * date.
         * Next E record will has first in session date the same (if student continue study)
         * and logic count first in session date like membership date for both spans
         * delete span for m_preferenceMemberOnWithdrawal = false and where first inactive
         * enrollment date = first in session date
         * if first in session date not found - using schedule start date, if not found - context
         * start date
         */
        private void fixPreferenceMemberOnWithdrawalIssue() {
            if (!m_data.m_preferenceMemberOnWithdrawal) {
                Iterator<StudentEnrollmentSpan> iterator = m_enrollmentSpans.iterator();
                while (iterator.hasNext()) {
                    StudentEnrollmentSpan span = iterator.next();
                    if (span.getFirstInactiveEnrollment() != null) {
                        SisStudent student = span.getFirstInactiveEnrollment().getStudent();
                        PlainDate lastDate = span.getFirstInactiveEnrollment().getEnrollmentDate();
                        PlainDate startDate =
                                m_data.findFirstEnrollmentDay(span.getFirstInactiveEnrollment().getSchool(), student);
                        if (lastDate != null && lastDate.equals(startDate)) {
                            iterator.remove();
                        }

                    }
                }
            }

        }

        /**
         * Returns the enrollment snapshot for the student as of the report date.
         *
         * @param student SisStudent
         * @param reportDate PlainDate
         * @param field FieldDefinition
         * @return EnrollmentSnapshot
         */
        private EnrollmentSnapshot getSnapshot(SisStudent student, PlainDate reportDate, FieldDefinition field) {
            EnrollmentSnapshot snapshot = new EnrollmentSnapshot(student, reportDate, getData().getBroker());

            if (!SUPPRESS_ENROLLMENT_WARNINGS) {
                if (!snapshot.isPrecise()) {
                    String errorMessage =
                            "WARNING: Enrollment information (enrollment status, school, and/or YOG) is not precise";
                    addRetrievalError(I4SEE_210_ENROLLMENT_STATUS,
                            new StateReportValidationError(this, field, errorMessage, ""));
                }
            }

            return snapshot;
        }

        /**
         * get key for input span.
         *
         * @param span StudentEnrollmentSpan
         * @return String
         */
        private String getSpanKey(StudentEnrollmentSpan span) {
            StringBuilder key = new StringBuilder();
            for (StudentEnrollment enrollment : span.getEnrollments()) {
                key.append(enrollment.getOid());
            }
            return key.toString();
        }

        /**
         * Gets the student enrollment spans.
         *
         * @param student SisStudent
         * @param limit boolean
         * @return List
         */
        private List<StudentEnrollmentSpan> getStudentEnrollmentSpans(SisStudent student, boolean limit) {
            List<StudentEnrollmentSpan> spans = m_data.m_helper.getStudentEnrollmentSpans(student, limit);

            // remove spans with null first inactive date that are not the last span
            Iterator<StudentEnrollmentSpan> iterator = spans.iterator();
            while (iterator.hasNext()) {
                StudentEnrollmentSpan span = iterator.next();
                if (iterator.hasNext() && span.getLastActiveDate() == null) {
                    iterator.remove();
                }
            }
            return spans;
        }

        /**
         * limit input enrollmentSpans by input beginDate and endDate dates.
         *
         * @param enrollmentSpans List<StudentEnrollmentSpan>
         * @param beginDate PlainDate
         * @param endDate PlainDate
         */
        private void limitSpanByDates(List<StudentEnrollmentSpan> enrollmentSpans,
                                      PlainDate beginDate,
                                      PlainDate endDate) {
            Iterator<StudentEnrollmentSpan> iterator = enrollmentSpans.iterator();
            while (iterator.hasNext()) {
                StudentEnrollmentSpan span = iterator.next();
                if (span.getFirstActiveDate() == null) {
                    iterator.remove();
                    continue;
                }

                if (endDate.before(span.getFirstActiveDate())) {
                    iterator.remove();
                    continue;
                }
                if (span.getLastActiveDate() != null &&
                        beginDate.after(span.getLastActiveDate())) {
                    iterator.remove();
                    continue;
                }
            }
        }

        /**
         * Removes the spans with not reported school.
         */
        private void removeSpansWithNotReportedSchool() {
            Iterator<StudentEnrollmentSpan> iterator = m_enrollmentSpans.iterator();
            while (iterator.hasNext()) {
                StudentEnrollmentSpan span = iterator.next();
                if (span.getSchool() != null && m_data.m_excludedSchoolMap.containsKey(span.getSchool().getOid())) {
                    iterator.remove();
                }
            }
        }

        /**
         * Removes the spans with same entry and exit date.
         */
        private void removeSpansWithSameEntryAndExitDate() {
            // Create next span map
            Map<StudentEnrollmentSpan, StudentEnrollmentSpan> mapNextSpan = new HashMap();
            Iterator<StudentEnrollmentSpan> iterator = m_enrollmentSpans.iterator();
            StudentEnrollmentSpan previousSpan = null;
            while (iterator.hasNext()) {
                StudentEnrollmentSpan span = iterator.next();
                if (previousSpan != null) {
                    mapNextSpan.put(previousSpan, span);
                }
                previousSpan = span;
            }

            iterator = m_enrollmentSpans.iterator();
            while (iterator.hasNext()) {
                StudentEnrollmentSpan span = iterator.next();
                PlainDate entryDate = getEntryDate(span);
                PlainDate exitDate = getExitDate(span, mapNextSpan.get(span));
                if (entryDate != null && exitDate != null && !entryDate.before(exitDate)) {
                    if (CODE_ENROLLMENT_STATUS_7.equals(getEnrollmentStatus(span))) {
                        iterator.remove();
                    }
                }
            }
        }
    }

    /**
     * The Class NHI4SeeStudentHistoryHelper.
     */
    public class NHI4SeeStudentHistoryHelper extends StudentHistoryHelper {

        /**
         * Instantiates a new NHI 4 see student history helper.
         *
         * @param data StateReportData
         */
        public NHI4SeeStudentHistoryHelper(StateReportData data) {
            super(data);
        }

        /**
         * Builds the enrollment criteria.
         *
         * @param studentSubQuery SubQuery
         * @return X2Criteria
         * @see com.x2dev.sis.tools.stateexports.StudentHistoryHelper#buildEnrollmentCriteria(com.follett.fsc.core.framework.persistence.SubQuery)
         */
        @Override
        protected X2Criteria buildEnrollmentCriteria(SubQuery studentSubQuery) {
            X2Criteria criteriaEW = new X2Criteria();
            criteriaEW.addIn(StudentEnrollment.COL_ENROLLMENT_TYPE,
                    Arrays.asList(StudentEnrollment.ENTRY, StudentEnrollment.WITHDRAWAL));

            X2Criteria criteriaS = new X2Criteria();
            criteriaS.addEqualTo(StudentEnrollment.COL_ENROLLMENT_TYPE, StudentEnrollment.STATUS_CHANGE);
            criteriaS.addIn(StudentEnrollment.COL_REASON_CODE, Arrays.asList("W1/R1", "W12/R12"));

            X2Criteria criteriaY = new X2Criteria();
            criteriaY.addEqualTo(StudentEnrollment.COL_ENROLLMENT_TYPE, StudentEnrollment.YOG_CHANGE);
            criteriaY.addIn(StudentEnrollment.COL_REASON_CODE, Arrays.asList("W1/R1 - Promote", "W1/R1 - Demote"));

            X2Criteria criteriaType = new X2Criteria();
            criteriaType.addOrCriteria(criteriaEW);
            criteriaType.addOrCriteria(criteriaS);
            criteriaType.addOrCriteria(criteriaY);

            X2Criteria criteria = new X2Criteria();
            criteria.addIn(StudentEnrollment.COL_STUDENT_OID, studentSubQuery);
            criteria.addAndCriteria(criteriaType);

            return criteria;
        }

    }
    /**
     * Returns the school number for the given student.
     * <p>
     * For non-archived students we use the snapshot. For archived students (i.e., summer
     * withdrawals) we use the school from the most recent enrollment record during summer vacation
     * (if we didn't do this then the export would show the history school's school number).
     */
    protected class Retrieve050SchoolNumber implements FieldRetriever {

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
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();
            String schoolNumber = null;
            I4SeeEntity i4SeeEntity = (I4SeeEntity) entity;

            // String adjustedSchoolNumber = (String) WebUtils.getProperty(student,
            // m_adjustedSchoolCode);
            String adjustedSchoolNumber = (String) student.getFieldValueByAlias(ALIAS_ADJUSTED_SCHOOL_NUMBER);

            if (StringUtils.isEmpty(adjustedSchoolNumber)) {
                if (m_schoolMap.get(student.getSchoolOid()).getArchiveIndicator()) {
                    List enrollments =
                            m_enrollManager.getOrderedEnrollment(student, m_summerStartDate, m_summerEndDate,
                                    null, false);

                    if (!enrollments.isEmpty()) {
                        StudentEnrollment withdrawalEnrollment = (StudentEnrollment) enrollments.get(0);
                        SisSchool school = withdrawalEnrollment.getSchool();
                        schoolNumber = (String) school.getFieldValueByAlias(ALIAS_I4SEE_050_SCHOOL_NUMBER);
                    }
                } else {
                    StudentEnrollment entryEnrollment = i4SeeEntity.getEntry();
                    StudentEnrollment withdrawalEnrollment = i4SeeEntity.getWithdrawal();
                    StudentEnrollment activeRecord = null;

                    if (hasStudentWithdrawn(entryEnrollment, withdrawalEnrollment)) {
                        if (withdrawalEnrollment != null) {
                            activeRecord = withdrawalEnrollment;
                        }
                    } else {
                        if (entryEnrollment != null) {
                            activeRecord = entryEnrollment;
                        }
                    }

                    if (activeRecord != null) {
                        SisSchool school = m_schoolMap.get(activeRecord.getSchoolOid());
                        if (school == null) {
                            school = m_schoolMap.get(student.getSchoolOid());
                        }

                        schoolNumber = (String) school.getFieldValueByAlias(ALIAS_I4SEE_050_SCHOOL_NUMBER);
                    }
                }
            } else {
                schoolNumber = adjustedSchoolNumber;
            }

            return schoolNumber;
        }
    }

    /**
     * Returns the enrollment status for the student.
     */
    protected class Retrieve210EnrollmentStatus implements FieldRetriever {
        /*
         * Enrollment Status: This should be pulled from the Student Enrollment Table > Enrollment
         * Code (alias i4see 210). Export seems to be populated all with a code of '1'. BOY and
         * EOY should follow this logic:
         * 1. Check Enrollment Code and if there is one, report it. If there is no enrollment code
         * in latest record, check all records and use code entered in the latest enrollment record
         * 2. If Enrollment Code is null, check for Withdrawal record. If withdrawal occurred during
         * summer, then Enrollment Code= 7
         * 3. If Enrollment Code is null, and there is no Withdrawal record, then populate with 1.
         * 4. For EOY, do not report any records where Enrollment Code = 7.
         */

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
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            I4SeeEntity i4SeeEntity = (I4SeeEntity) entity;

            return i4SeeEntity.getEnrollmentStatus();
        }
    }

    /**
     * Returns the town responsible for a given student, this is dependent on their enrollment at
     * the time.
     */
    protected class Retrieve220TownResponsible implements FieldRetriever {

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
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            I4SeeEntity i4SeeEntity = (I4SeeEntity) entity;
            StudentEnrollment enrollment = i4SeeEntity.getLastActiveEnrollment();
            String townResponsible = null;

            if (enrollment != null) {
                townResponsible = (String) enrollment.getFieldValueByAlias(ALIAS_I4SEE_220_TOWN_RESPONSIBLE);
            }

            if (StringUtils.isEmpty(townResponsible)) {
                // get Organization table > District Number field
                townResponsible = (String) getOrganization().getFieldValueByAlias(ALIAS_I4SEE_040_DISTRICT_NUMBER);
            }

            return townResponsible;
        }
    }

    /**
     * Returns the district responsible for a given student, this is dependent on their enrollment
     * at the time.
     */
    protected class Retrieve225DistrictResponsible implements FieldRetriever {

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
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            I4SeeEntity i4SeeEntity = (I4SeeEntity) entity;
            StudentEnrollment enrollment = i4SeeEntity.getLastActiveEnrollment();
            String districtResponsible = null;

            if (enrollment != null) {
                districtResponsible = (String) enrollment.getFieldValueByAlias(ALIAS_I4SEE_225_DISTRICT_RESPONSIBLE);
            }
            if (StringUtils.isEmpty(districtResponsible)) {
                districtResponsible = (String) getOrganization().getFieldValueByAlias(ALIAS_I4SEE_040_DISTRICT_NUMBER);
            }

            return districtResponsible;
        }
    }

    /**
     * Returns the entry date for the student.
     */
    protected class Retrieve230EntryDate implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.
         *      core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            I4SeeEntity i4SeeEntity = (I4SeeEntity) entity;
            return i4SeeEntity.getEntryDate();
        }

    }

    /**
     * This is a check to see if in the current enrollment span the student has left. Basically if
     * we return true, then use the withdrawl record, otherwise use the entry record
     *
     * @param entry StudentEnrollment
     * @param withdrawal StudentEnrollment
     * @return true, if successful
     */
    protected boolean hasStudentWithdrawn(StudentEnrollment entry, StudentEnrollment withdrawal) {
        /*
         * The hack here is for students with
         * an enrollment and withdrawal record on the same day. We use the timestamp to
         * figure out the order in which they happened. This is fine unless someone edits
         * the records, modifying the timestamp.
         */
        return (withdrawal != null) && (entry != null) &&
                (withdrawal.getEnrollmentDate().after(entry.getEnrollmentDate()) ||
                        (withdrawal.getEnrollmentDate().equals(entry.getEnrollmentDate()) && // hack
                                (withdrawal.getTimestamp() > entry.getTimestamp())));
    }

    /**
     * Returns the entry code for the student.
     */
    protected class Retrieve240EntryCode implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.
         *      core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            I4SeeEntity i4SeeEntity = (I4SeeEntity) entity;
            StudentEnrollment entryEnrollment = i4SeeEntity.getEntry();

            String stateCode = null;
            String entryCode = null;

            if (entryEnrollment != null && field != null) {
                String code = entryEnrollment.getEnrollmentCode();
                if (!StringUtils.isEmpty(code)) {
                    // entryCode = getStateCode(I4SEE_240_ENTRY_CODE, entity, field, code,
                    // field.getParameter().toString());
                    stateCode = lookupStateValue(StudentEnrollment.class, StudentEnrollment.COL_ENROLLMENT_CODE, code);

                    if (StringUtils.isEmpty(stateCode)) {
                        stateCode = code;
                    }
                    entryCode = stateCode;
                }

                // Case when span starts from not entry enrollment
                if ((entryCode == null)
                        && StudentEnrollment.YOG_CHANGE.equals(entryEnrollment.getEnrollmentType())) {
                    entryCode = entity.getCurrentRow() == 0 ? CODE_ENROLLMENT_E1 : CODE_ENROLLMENT_R1;
                }

                // If an entry code is before the start of the school year then it needs to be
                // reported as "E1"
                if (!StringUtils.isEmpty(entryCode)
                        && entryEnrollment.getEnrollmentDate().before(m_schoolYearStartDate)) {
                    entryCode = CODE_ENROLLMENT_E1; // because blanks are allowed
                }

                // if the entry date is blank, so is the code
                // String entryDate = entity.getFieldValue(I4SEE_230_ENTRY_DATE);
                PlainDate entryDate = entryEnrollment.getEnrollmentDate();
                if (entryDate == null) {
                    entryCode = null;
                } else {
                    if (entryCode == null) {
                        entryCode = entity.getCurrentRow() == 0 ? CODE_ENROLLMENT_E1 : CODE_ENROLLMENT_R1;
                    }
                }
            }

            return entryCode;
        }
    }

    /**
     * Returns the exit date for the student.
     *
     * @author X2 Development Corporation
     */
    protected class Retrieve250ExitDate implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.
         *      core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            I4SeeEntity i4SeeEntity = (I4SeeEntity) entity;
            PlainDate date = i4SeeEntity.getExitDate();
            return (date != null && !date.after(m_reportDate)) ? date : null;
        }
    }

    /**
     * Returns the exit date for the student.
     *
     * @author X2 Development Corporation
     */
    protected class Retrieve260ExitCode implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.
         *      core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String stateCode = null;

            I4SeeEntity i4SeeEntity = (I4SeeEntity) entity;
            PlainDate date = i4SeeEntity.getExitDate();
            if (date != null && !date.after(m_reportDate)) {
                StudentEnrollment withdrawEnrollment = i4SeeEntity.getWithdrawal();

                if (withdrawEnrollment != null) {
                    String code = withdrawEnrollment.getEnrollmentCode();
                    if (!StringUtils.isEmpty(code)) {
                        // exitCode = getStateCode(I4SEE_260_EXIT_CODE, entity, field, code,
                        // field.getParameter().toString());
                        stateCode =
                                lookupStateValue(StudentEnrollment.class, StudentEnrollment.COL_ENROLLMENT_CODE, code);
                    }
                }

                if ((stateCode == null) &&
                        (withdrawEnrollment != null) &&
                        (StudentEnrollment.YOG_CHANGE.equals(withdrawEnrollment.getEnrollmentType()) ||
                                StudentEnrollment.STATUS_CHANGE.equals(withdrawEnrollment.getEnrollmentType()))) {
                    stateCode = CODE_ENROLLMENT_W1;
                }
            }

            return stateCode;
        }
    }

    /**
     * Retrieve days in attendance for the student. If the count is zero, use the 555 placeholder.
     *
     * @author X2 Development Corporation
     */
    protected class Retrieve300DaysInAttendance implements FieldRetriever {
        Set<String> skipCodes = new HashSet();

        /**
         * Load the attendance skip codes
         */
        public Retrieve300DaysInAttendance() {
            skipCodes.add("---NoMatch---");
            ModelProperty prop = new ModelProperty(StudentAttendance.class, StudentAttendance.COL_CODE_VIEW,
                    getBroker().getPersistenceKey());
            DataDictionaryField dictionaryField = getDataDictionary().findDataDictionaryField(prop.getFieldId());
            if (dictionaryField != null && dictionaryField.hasReferenceTable()) {
                Map<String, ReferenceCode> codeMap = dictionaryField.getReferenceTable().getCodeMap(getBroker());
                for (ReferenceCode code : codeMap.values()) {
                    if ("Exclude".equals(code.getStateCode())) {
                        skipCodes.add(code.getCode());
                    }
                }
            }
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.
         *      core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String attendanceCount = null;

            String membershipCount = getMembershipDays((I4SeeEntity) entity);

            String daysAbsent = entity.getFieldValue(I4SEE_310_DAYS_ABSENT);

            int membershipCountAsInt = 0;

            if (membershipCount != null) {
                try {
                    membershipCountAsInt = Integer.parseInt(membershipCount);
                } catch (NumberFormatException nfe) {
                    // Do nothing, this error has already been logged.
                }
            }


            if (membershipCountAsInt != 0) {
                membershipCountAsInt -= getSkipDays((I4SeeEntity) entity);
                membershipCountAsInt = membershipCountAsInt * 2;
                /*
                 * Check student's Full Day Percent value (i4see 405). If the value is
                 * not empty and less than 50, consider these as half-days
                 */
                String fullDayPercent = entity.getFieldValue(I4SEE_405_FULL_DAY_PERCENT);
                if (StringUtils.isNumeric(fullDayPercent)) {
                    double fdPercentValue = (new BigDecimal(fullDayPercent)).doubleValue();
                    if (fdPercentValue <= 50) {
                        membershipCountAsInt = membershipCountAsInt / 2;
                    }
                }

                int attendance = membershipCountAsInt - Integer.parseInt(daysAbsent);
                attendanceCount = String.valueOf(attendance);

                if (attendance < 0) {
                    entity.addRetrievalError(field.getFieldId(),
                            new StateReportValidationError(entity,
                                    field,
                                    "ERROR",
                                    VALIDATION_INVALID_VALUE));
                }
            } else {
                attendanceCount = "0";
            }

            /*
             * Save the unmapped value in the entity so it can be written back to the student
             * record in postProcess.
             */
            ((I4SeeEntity) entity).setUpdateValue(field.getFieldId(), attendanceCount);

            return attendanceCount;
        }

        /**
         * Gets the skip days.
         *
         * @param i4SeeEntity I4SeeEntity
         * @return int
         */
        private int getSkipDays(I4SeeEntity i4SeeEntity) {
            int skipDays = 0;

            SisStudent student = (SisStudent) i4SeeEntity.getBean();
            if (i4SeeEntity.getRowCount() == 1) {
                List<StudentAttendance> studentAttendances = m_helper.getStudentAttendances(student.getOid());

                if (studentAttendances != null && studentAttendances.size() > 0) {
                    for (StudentAttendance studentAttendance : studentAttendances) {
                        if (skipCodes.contains(studentAttendance.getCodeView())) {
                            skipDays++;
                        }
                    }
                }
            } else {
                StudentEnrollment entryEnrollment = i4SeeEntity.getEntry();
                StudentEnrollment withdrawalEnrollment = i4SeeEntity.getWithdrawal();

                PlainDate startDate = null;
                if (m_scheduleMap.get(student.getSchoolOid()) != null) {
                    startDate = m_scheduleMap.get(student.getSchoolOid()).getStartDate();
                }
                if (startDate != null && (entryEnrollment == null || entryEnrollment.getEnrollmentDate() != null)) {
                    Criteria criteria = new Criteria();
                    criteria.addEqualTo(StudentAttendance.COL_STUDENT_OID, student.getOid());
                    criteria.addIn(StudentAttendance.COL_CODE_VIEW, skipCodes);

                    if ((entryEnrollment != null)
                            && (entryEnrollment.getEnrollmentDate() != null)
                            && (startDate != null)
                            && entryEnrollment.getEnrollmentDate().after(startDate)) {
                        criteria.addGreaterOrEqualThan(StudentAttendance.COL_DATE,
                                entryEnrollment.getEnrollmentDate());
                    } else {
                        criteria.addGreaterOrEqualThan(StudentAttendance.COL_DATE, startDate);
                    }

                    if ((withdrawalEnrollment != null)
                            && (withdrawalEnrollment.getEnrollmentDate() != null)) {
                        criteria.addLessThan(StudentAttendance.COL_DATE, withdrawalEnrollment.getEnrollmentDate());
                    } else {
                        criteria.addLessOrEqualThan(StudentAttendance.COL_DATE, m_reportDate);
                    }

                    QueryByCriteria query = new QueryByCriteria(StudentAttendance.class, criteria);

                    skipDays = getBroker().getCount(query);
                }
            }
            return skipDays;
        }
    }

    /**
     * Retrieve days absent for a student. If the count is zero, use the 555 placeholder.
     *
     * @author X2 Development Corporation
     */
    protected class Retrieve310DaysAbsent implements FieldRetriever {

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
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            I4SeeEntity i4SeeEntity = (I4SeeEntity) entity;

            // String javaName = field.getBeanPath().substring(field.getBeanPath().lastIndexOf('.')
            // + 1);
            String absentCount = "0";

            int absenses = 0;
            SisStudent student = (SisStudent) entity.getBean();

            if (m_calculateTotals) {
                if (entity.getRowCount() == 1) {
                    absenses = getAbsentDays(student.getOid());
                } else {
                    StudentEnrollment entryEnrollment = i4SeeEntity.getEntry();
                    StudentEnrollment withdrawalEnrollment = i4SeeEntity.getWithdrawal();

                    PlainDate startDate = null;
                    if (m_scheduleMap.get(student.getSchoolOid()) != null) {
                        startDate = m_scheduleMap.get(student.getSchoolOid()).getStartDate();
                    }
                    if ((startDate == null)
                            && ((entryEnrollment != null)
                                    && (entryEnrollment.getEnrollmentDate() == null))) {
                        String errorId = "Could not calculate absences";
                        String errorMessage = "Both entryDate and school schedule startDate missing";
                        entity.addRetrievalError(field.getFieldId(),
                                new StateReportValidationError(entity, field, errorId, errorMessage));
                    } else {
                        Criteria criteria = new Criteria();
                        criteria.addEqualTo(StudentAttendance.COL_STUDENT_OID, student.getOid());
                        criteria.addEqualTo(StudentAttendance.COL_ABSENT_INDICATOR, Boolean.TRUE);

                        if ((entryEnrollment != null)
                                && (entryEnrollment.getEnrollmentDate() != null)
                                && (startDate != null)
                                && entryEnrollment.getEnrollmentDate().after(startDate)) {
                            criteria.addGreaterOrEqualThan(StudentAttendance.COL_DATE,
                                    entryEnrollment.getEnrollmentDate());
                        } else {
                            criteria.addGreaterOrEqualThan(StudentAttendance.COL_DATE, startDate);
                        }

                        if ((withdrawalEnrollment != null)
                                && (withdrawalEnrollment.getEnrollmentDate() != null)) {
                            criteria.addLessThan(StudentAttendance.COL_DATE, withdrawalEnrollment.getEnrollmentDate());
                        } else {
                            criteria.addLessOrEqualThan(StudentAttendance.COL_DATE, m_reportDate);
                        }

                        QueryByCriteria query = new QueryByCriteria(StudentAttendance.class, criteria);

                        absenses = getBroker().getCount(query);
                    }
                }

                absenses = absenses * 2;

                /*
                 * Check student's Full Day Percent value (i4see 405). If the value is
                 * not empty and less than 50, consider these as half-days
                 */
                // String fullDayPercent = entity.getFieldValue(I4SEE_405_FULL_DAY_PERCENT);
                String fullDayPercent = (String) student.getFieldValueByAlias(ALIAS_I4SEE_405_FULL_DAY_PERCENT);

                if (!StringUtils.isEmpty(fullDayPercent) && StringUtils.isNumeric(fullDayPercent)) {
                    double fdPercentValue = (new BigDecimal(fullDayPercent)).doubleValue();
                    if (fdPercentValue <= 50) {
                        absenses = absenses / 2;
                    }
                }

                absentCount = String.valueOf(absenses);
            } else {
                // absentCount = m_integerConverter.javaToString(WebUtils.getProperty(student,
                // javaName));
                String daysAbsent = (String) student.getFieldValueByAlias(ALIAS_I4SEE_310_HALF_DAYS_ABSENT);
                if (!StringUtils.isEmpty(daysAbsent) && StringUtils.isNumeric(daysAbsent)) {
                    absentCount = daysAbsent;
                }
            }

            if (StringUtils.isEmpty(absentCount)) {
                String errorId = "ERROR: Missing value";
                String errorMessage = VALIDATION_MISSING_VALUE;
                entity.addRetrievalError(field.getFieldId(),
                        new StateReportValidationError(entity, field, errorId, errorMessage));
            }

            /*
             * Save the unmapped value in the entity so it can be written back to the student
             * record in postProcess.
             */
            i4SeeEntity.setUpdateValue(field.getFieldId(), absentCount);

            return absentCount;
        }
    }

    /**
     * Returns the grade level of a student.
     *
     * @author X2 Development Corporation
     */
    protected class Retrieve400GradeLevel implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.
         *      core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            return ((I4SeeEntity) entity).getGradeLevel();
        }
    }

    /**
     * Returns the full day percent date for the student.
     *
     * @author X2 Development Corporation
     */
    protected class Retrieve405FullDayPercent implements FieldRetriever {

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
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();

            String fullDayPercent = (String) student.getFieldValueByAlias(ALIAS_I4SEE_405_FULL_DAY_PERCENT);

            int fullDay = 0;
            String fullDayString = "";
            List<String> preSchoolCodes = Arrays.asList("13", "PK1", "PK2", "PK3", "PK4", "14", "K");

            if ((fullDayPercent != null) && StringUtils.isNumeric(fullDayPercent) &&
                    (preSchoolCodes.contains(
                            lookupStateValue(Student.class, Student.COL_GRADE_LEVEL, student.getGradeLevel())))) {
                Double percent = Double.valueOf(0);

                try {
                    percent = Double.valueOf(fullDayPercent);
                    fullDay = percent.intValue();
                    fullDayString = String.valueOf(fullDay);
                } catch (NumberFormatException e) {
                    // ignore bad format in student record field
                }
            }

            return fullDayString;
        }
    }

    /**
     * Returns the DOE race code for a student.
     *
     * @author X2 Development Corporation
     */
    protected class Retrieve420Race implements FieldRetriever {

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
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String raceCode = "";
            SisStudent student = (SisStudent) entity.getBean();
            SisPerson person = student.getPerson();

            if (m_calculateTotals) {
                /*
                 * Calculate the local (X2) bitmap value and then translate it to the corresponding
                 * state code.
                 */
                Collection<Race> races = m_raceCodeMap.get(student.getPersonOid());

                if (races != null) {
                    /*
                     * Calculate the local (X2) bitmap value and then translate it to the
                     * corresponding DOE
                     * code.
                     */
                    int stateRaceCode = 0;
                    Collection<Race> raceBeans = m_raceCodeMap.get(student.getPersonOid());

                    if (!CollectionUtils.isEmpty(raceBeans)) {
                        int localRaceCode = 0;

                        for (Race raceBean : raceBeans) {
                            String stateCode = lookupStateValue(Race.class, Race.COL_RACE_CODE, raceBean.getRaceCode());

                            if (StringUtils.isInteger(stateCode)) {
                                localRaceCode |= NH_TO_X2_RACE_CODES[Integer.parseInt(stateCode)];
                            }
                        }

                        /*
                         * Check Hispanic indicator PSN_HISPANIC_LATINO_IND
                         */
                        // SisPerson person = (SisPerson) getBroker().getBeanByOid(SisPerson.class,
                        // student.getPersonOid());
                        if (person != null) {
                            if (person.getHispanicLatinoIndicator()) {
                                localRaceCode |= HISPANIC_RACE_IND;
                            }
                        }

                        if ((localRaceCode >= 0) && (localRaceCode <= NH_STATE_RACE_CODES.length)) {
                            stateRaceCode = NH_STATE_RACE_CODES[localRaceCode];
                        }

                        raceCode = StringUtils.padLeft(String.valueOf(stateRaceCode), 2, '0');
                    }

                }
            } else {
                raceCode = (String) WebUtils.getProperty(student, field.getBeanPath());
            }

            return raceCode;
        }
    }

    /**
     * Returns the homeless indicator.
     */
    protected class RetrieveHomeCodes implements FieldRetriever {

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
            String param = (String) field.getParameter();
            SisStudent student = (SisStudent) entity.getBean();

            String value = (String) student.getFieldValueByAlias(param);

            // We get the state value of the code. If they have any errors
            // data then we just leave it as the default of '1' -
            // "does not apply"
            value = lookupStateValue(SisStudent.class, m_homelessCD, value);

            if (StringUtils.isEmpty(value)) {
                value = CODE_HOME_LESS_DOES_NOT_APPLY;
            }

            return value;
        }
    }

    /**
     * Returns the promoted indicator.
     */
    protected class Retrieve510PromotedInd implements FieldRetriever {

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
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            I4SeeEntity i4seeEntity = ((I4SeeEntity) entity);
            SisStudent student = (SisStudent) entity.getBean();

            StudentEnrollment withdrawalEnrollment = i4seeEntity.getWithdrawal();

            String promotedInd = null;
            String withdrawalCode = null;

            if (withdrawalEnrollment != null) {
                withdrawalCode = withdrawalEnrollment.getEnrollmentCode();
            }

            if ((entity.getCurrentRow() == (entity.getRowCount() - 1))
                    && ((withdrawalCode == null)
                            || CODE_ENROLLMENT_W11.equals(withdrawalCode))) {
                promotedInd = (String) student.getFieldValueByAlias(ALIAS_I4SEE_510_PROMOTED_IND);
                promotedInd = lookupReferenceCodeByAlias(ALIAS_I4SEE_510_PROMOTED_IND, promotedInd,
                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                promotedInd = promotedInd == null ? "" : promotedInd;
            }

            return promotedInd;
        }
    }

    /**
     * Returns the number of in-school suspensions for the given student from the start of school to
     * the report date.
     *
     * @author X2 Development Corporation
     */
    protected class Retrieve520and530ConductActionTotals implements FieldRetriever {

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
            I4SeeEntity i4SeeEntity = (I4SeeEntity) entity;
            SisStudent student = (SisStudent) entity.getBean();

            String count = "0";

            /*
             * Days Suspended not used in B-O-Y report
             */
            if (REPORT_TYPE_EOY.equals(m_reportType)) {
                StudentEnrollment entryEnrollment = i4SeeEntity.getEntry();
                StudentEnrollment withdrawalEnrollment = i4SeeEntity.getWithdrawal();

                if (m_calculateTotals) {
                    // Collection<String> codes = ("520".equals(field.getParameter().toString())) ?
                    // m_suspensionInCodes : m_suspensionOutCodes;
                    if (CODE_SUSPENSION_IN_CODE.equals(field.getParameter().toString())) {
                        count = getConductActionTotal(student, m_suspensionInCodes, entryEnrollment,
                                withdrawalEnrollment);
                    } else {
                        count = getConductActionTotal(student, m_suspensionOutCodes, entryEnrollment,
                                withdrawalEnrollment);
                    }
                } else {
                    String daysSuspensionIn = (String) student.getFieldValueByAlias(ALIAS_I4SEE_520_SUSPENSIONS_IN);
                    if (!StringUtils.isEmpty(daysSuspensionIn) && StringUtils.isNumeric(daysSuspensionIn)) {
                        count = daysSuspensionIn;
                    }
                }

                /*
                 * Save the unmapped value in the entity so it can be written back to the student
                 * record in postProcess.
                 */
                // i4SeeEntity.setUpdateValue(field.getFieldId(), count);
                student.setFieldValueByAlias(ALIAS_I4SEE_520_SUSPENSIONS_IN, count);
            }

            return count;
        }

        /**
         * Returns the total number of conduct actions for the current year (up to the report date)
         * for the given action codes.
         *
         * @param student SisStudent
         * @param codes Collection
         * @param entryEnrollment StudentEnrollment
         * @param withdrawalEnrollment StudentEnrollment
         * @return String
         * @throws X2BaseException exception
         */
        private String getConductActionTotal(SisStudent student,
                                             Collection codes,
                                             StudentEnrollment entryEnrollment,
                                             StudentEnrollment withdrawalEnrollment)
                throws X2BaseException {
            int intCount = 0;

            if ((codes != null) && !codes.isEmpty()) {
                /*
                 * Don't bother logging an error if the school doesn't have an active schedule. This
                 * will be reported elsewhere. (If we did log it here then we would flood the log
                 * with
                 * messages since this error would be reported twice for every student - once for
                 * ISS,
                 * and again for OSS.)
                 */
                if (m_scheduleMap.get(student.getSchoolOid()) != null) {
                    Criteria criteria = new Criteria();
                    criteria.addEqualTo(ConductAction.COL_STUDENT_OID, student.getOid());
                    criteria.addIn(ConductAction.COL_ACTION_CODE, codes);

                    if (entryEnrollment != null) {
                        criteria.addGreaterOrEqualThan(ConductAction.COL_ACTION_START_DATE,
                                entryEnrollment.getEnrollmentDate());
                    }
                    criteria.addGreaterOrEqualThan(ConductAction.COL_ACTION_START_DATE, m_schoolYearStartDate);


                    if (withdrawalEnrollment != null) {
                        criteria.addLessOrEqualThan(ConductAction.COL_ACTION_START_DATE,
                                withdrawalEnrollment.getEnrollmentDate());
                    }
                    criteria.addLessOrEqualThan(ConductAction.COL_ACTION_START_DATE, m_reportDate);


                    QueryByCriteria query = new QueryByCriteria(ConductAction.class, criteria);

                    Collection<ConductAction> conductActions = getBroker().getCollectionByQuery(query);

                    for (ConductAction conductAction : conductActions) {
                        BigDecimal suspensionLength =
                                (BigDecimal) conductAction.getFieldValueByAlias(ALIAS_SUSPENSION_LENGTH);

                        BigDecimal bdCount = null;
                        if (suspensionLength != null) {
                            bdCount = suspensionLength;
                        }
                        if (bdCount != null) {
                            intCount += bdCount.intValue();
                        }
                    }
                }
            }

            return Integer.toString(intCount);
        }
    }

    /**
     * returns a student's post graduate plans.
     */
    protected class Retrieve610PostGraduatePlans implements FieldRetriever {

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
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            I4SeeEntity i4SeeEntity = ((I4SeeEntity) entity);
            SisStudent student = (SisStudent) entity.getBean();

            String postGradPlans = null;
            String withdrawalCode = null;

            StudentEnrollment withdrawalEnrollment = i4SeeEntity.getWithdrawal();
            if (withdrawalEnrollment != null) {
                withdrawalCode = withdrawalEnrollment.getEnrollmentCode();
            }

            if ((entity.getCurrentRow() == (entity.getRowCount() - 1))
                    && ((withdrawalCode == null)
                            || CODE_ENROLLMENT_W11.equals(withdrawalCode))
                    && !i4SeeEntity.getIsNonGraduatingSenior()) {
                postGradPlans = (String) student.getFieldValueByBeanPath(m_postGrad);
                postGradPlans = data.lookupStateValue(SisStudent.class, m_postGrad, postGradPlans);
            }

            return postGradPlans;
        }
    }

    /**
     * Retrieves the student's diploma type.
     */
    protected class Retrieve620DiplomaType implements FieldRetriever {

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
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            I4SeeEntity i4SeeEntity = ((I4SeeEntity) entity);
            SisStudent student = (SisStudent) entity.getBean();

            StudentEnrollment withdrawalEnrollment = i4SeeEntity.getWithdrawal();

            String diplomaType = null;
            String withdrawalCode = null;

            if (withdrawalEnrollment != null) {
                withdrawalCode = withdrawalEnrollment.getEnrollmentCode();
            }
            if ((entity.getCurrentRow() == (entity.getRowCount() - 1))
                    && ((withdrawalCode == null)
                            || CODE_ENROLLMENT_W11.equals(withdrawalCode))
                    && !i4SeeEntity.getIsNonGraduatingSenior()) {
                // diplomaType = WebUtils.getProperty(entity.getBean(), m_diplomaType);
                diplomaType = (String) student.getFieldValueByAlias(ALIAS_I4SEE_620_DIPLOMA_TYPE);
                String stateValue = lookupStateValue(SisStudent.class, m_diplomaType, diplomaType);

                if (!StringUtils.isEmpty(stateValue)) {
                    diplomaType = stateValue;
                }
            }

            return diplomaType;
        }
    }

    /**
     * Validate enrollment status.
     */
    protected class Validate210EnrollmentStatus implements FieldValidator {

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

            if (CODE_ENROLLMENT_STATUS_1.equals(value)
                    || CODE_ENROLLMENT_STATUS_2.equals(value)) {
                String exitDate = entity.getFieldValue(I4SEE_250_EXIT_DATE);

                try {
                    if (!StringUtils.isEmpty(exitDate)
                            && m_dateFormat.parse(exitDate).before(m_schoolYearStartDate)) {
                        String errorId =
                                "I4SEE 210 Enrollment Status " + value + " not valid for exit date " + exitDate;
                        String errorMessage = "I4SEE 210 = " + value;
                        errors.add(new StateReportValidationError(entity, field, errorId, errorMessage));
                    }
                } catch (Exception e) {
                    // This should be handled by the date formatter already.
                }
            }
            if (CODE_ENROLLMENT_STATUS_2.equals(value)) {
                String distResp = entity.getFieldValue(I4SEE_225_RESPONSIBLE_DISTRICT);

                if (CODE_DISTRICT_930.equals(distResp)) {
                    String errorId =
                            "I4SEE 210 Enrollment Status " + value + " not valid for District Responsible " + distResp;
                    String errorMessage = "I4SEE 225 = " + value;
                    errors.add(new StateReportValidationError(entity, field, errorId, errorMessage));
                }
            }
            if (CODE_ENROLLMENT_STATUS_5.equals(value)) {
                String schoolNo = entity.getFieldValue(I4SEE_050_SCHOOL_NUMBER);
                if (StringUtils.isEmpty(schoolNo)
                        || !(schoolNo.equals(CODE_SCHOOL_15010) || schoolNo.equals(CODE_SCHOOL_15020) ||
                                schoolNo.equals(CODE_SCHOOL_15030) || schoolNo.equals(CODE_SCHOOL_15040))) {
                    String errorId =
                            "I4SEE 210 Enrollment Status " + value + " not valid for School Number " + schoolNo;
                    String errorMessage = "I4SEE 050 = " + value;
                    errors.add(new StateReportValidationError(entity, field, errorId, errorMessage));
                }
            }
            if (CODE_ENROLLMENT_STATUS_7.equals(value)) {
                String exitDate = entity.getFieldValue(I4SEE_250_EXIT_DATE);

                try {
                    if (StringUtils.isEmpty(exitDate)
                            || !m_dateFormat.parse(exitDate).before(m_schoolYearStartDate)) {
                        String errorId =
                                "I4SEE 210 Enrollment Status " + value + " not valid for exit date " + exitDate;
                        String errorMessage = "I4SEE 210 = " + value;
                        errors.add(new StateReportValidationError(entity, field, errorId, errorMessage));
                    }
                } catch (Exception e) {
                    // This should be handled by the date formatter already.
                }
            }
            if (CODE_ENROLLMENT_STATUS_8.equals(value)) {
                String gradeLevel = entity.getFieldValue(I4SEE_400_GRADE);

                if (StringUtils.isEmpty(gradeLevel)
                        || !(gradeLevel.equals(CODE_GRADE_LEVEL_9) || gradeLevel.equals(CODE_GRADE_LEVEL_10) ||
                                gradeLevel.equals(CODE_GRADE_LEVEL_11) || gradeLevel.equals(CODE_GRADE_LEVEL_12))) {
                    String errorId =
                            "I4SEE 210 Enrollment Status " + value + " not valid for Grade Level " + gradeLevel;
                    String errorMessage = "I4SEE 050 = " + value;
                    errors.add(new StateReportValidationError(entity, field, errorId, errorMessage));
                }
            }

            return errors;
        }
    }

    /**
     * Validate entry code.
     *
     */
    protected class Validate240EntryCode implements FieldValidator {

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
            SisStudent student = (SisStudent) entity.getBean();

            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>(1);

            String entryDate = entity.getFieldValue(I4SEE_230_ENTRY_DATE);

            if ((!StringUtils.isEmpty(entryDate) && StringUtils.isEmpty(value))
                    || (StringUtils.isEmpty(entryDate) && !StringUtils.isEmpty(value))) {
                String errorId =
                        "I4SEE 240 Entry Code and I4See 230 Entry Date must either both be empty, or both be filled in";
                String errorMessage = "I4SEE 230 = " + entryDate + ", I4SEE 240 = " + value;
                errors.add(new StateReportValidationError(entity, field, errorId, errorMessage));
            } else if (!StringUtils.isEmpty(entryDate) && !StringUtils.isEmpty(value)) {
                // If both empty nothing to validate, and this not an error as
                // blank value is allowed
                try {
                    Date entDate = m_dateFormat.parse(entryDate);
                    Date startDate = null;

                    if (m_scheduleMap.get(student.getSchoolOid()) != null) {
                        startDate = m_scheduleMap.get(student.getSchoolOid()).getStartDate();
                    }

                    if (startDate != null) {
                        if (((entity.getRowCount() == 1)
                                && !CODE_ENROLLMENT_E1.equals(value) && entDate.equals(startDate))
                                || (CODE_ENROLLMENT_E2.equals(value) && entDate.before(startDate))) {
                            String errorId = "I4SEE 240 Entry Code " + value + " not valid for Entry Date " + entryDate;
                            String errorMessage = "I4SEE 240 = " + value;
                            errors.add(new StateReportValidationError(entity, field, errorId, errorMessage));
                        }
                    }
                } catch (ParseException pe) {
                    String errorId = VALIDATION_INVALID_VALUE;
                    String errorMessage = "Entry date: " + entryDate;
                    errors.add(new StateReportValidationError(entity, field, errorId, errorMessage));
                }
            }

            if (REPORT_TYPE_EOY.equals(m_reportType) && BOY_NOT_USED_FIELDS_LENGTHS.containsKey(field.getFieldId())) {
                Integer integer = BOY_NOT_USED_FIELDS_LENGTHS.get(field.getFieldId());
                if (entity.getFieldValue(field.getFieldId()).length() < integer.intValue()) {
                    String errorId = " length does not meet or exceed minimum length of: " + integer;
                    String errorMessage = field.getFieldId() + " : " + value;
                    errors.add(new StateReportValidationError(entity, field, errorId, errorMessage));
                }
            }

            return errors;
        }
    }

    /**
     * Validate exit code.
     *
     */
    protected class Validate260ExitCode implements FieldValidator {

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

            String exitDate = entity.getFieldValue(I4SEE_250_EXIT_DATE);

            if ((!StringUtils.isEmpty(exitDate) && StringUtils.isEmpty(value)) ||
                    (StringUtils.isEmpty(exitDate) && !StringUtils.isEmpty(value))) {
                String errorId =
                        "I4SEE 260 Exit Code and I4See 250 Exit Date must either both be empty, or both be filled in";
                String errorMessage = "I4SEE 250 = " + exitDate + ", I4SEE 260 = " + value;
                errors.add(new StateReportValidationError(entity, field, errorId, errorMessage));
            }

            if (CODE_ENROLLMENT_W11.equals(value)) {
                String promotedInd = entity.getFieldValue(ALIAS_I4SEE_510_PROMOTED_IND);

                if (!CODE_PROMOTED_IND_COMPLETED.equals(promotedInd)) {
                    String errorId =
                            "I4SEE 260 Exit Code " + value + " not valid for Promoted Indicator " + promotedInd;
                    String errorMessage = "I4SEE 260 = " + value;
                    errors.add(new StateReportValidationError(entity, field, errorId, errorMessage));
                }

                String gradeLevel = entity.getFieldValue(I4SEE_400_GRADE);

                if (!(CODE_GRADE_LEVEL_11.equals(gradeLevel) || CODE_GRADE_LEVEL_12.equals(gradeLevel))) {
                    String errorId = "I4SEE 260 Exit Code " + value + " not valid for Grade Level " + gradeLevel;
                    String errorMessage = "I4SEE 260 = " + value;
                    errors.add(new StateReportValidationError(entity, field, errorId, errorMessage));
                }
            }

            if ((value != null) && value.matches("W2[0123456789]")) {
                String gradeLevel = entity.getFieldValue(I4SEE_400_GRADE);

                if (!(CODE_GRADE_LEVEL_9.equals(gradeLevel) || CODE_GRADE_LEVEL_10.equals(gradeLevel) ||
                        CODE_GRADE_LEVEL_11.equals(gradeLevel) || CODE_GRADE_LEVEL_12.equals(gradeLevel))) {
                    String errorId = "I4SEE 260 Exit Code " + value + " not valid for Grade Level " + gradeLevel;
                    String errorMessage = "I4SEE 260 = " + value;
                    errors.add(new StateReportValidationError(entity, field, errorId, errorMessage));
                }
            }

            return errors;
        }
    }

    /**
     * Validates race codes (code 09 is no longer an option).
     */
    protected class Validate420RaceCode implements FieldValidator {

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

            String raceCode = entity.getFieldValue(I4SEE_420_RACE);

            if ("9".equals(raceCode)) {
                String errorId = "I4SEE 420 Racial and Ethnic code 09 is no longer used.";
                String errorMessage = "I4SEE 420 = " + value;
                errors.add(new StateReportValidationError(entity, field, errorId, errorMessage));
            }

            return errors;
        }
    }

    /**
     * Validates post graduation plans.
     */
    protected class Validate610PostGraduatePlans implements FieldValidator {

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

            SisStudent student = (SisStudent) entity.getBean();

            // String promotedInd = entity.getFieldValue(ALIAS_I4SEE_510_PROMOTED_IND);
            String promotedInd = (String) student.getFieldValueByAlias(ALIAS_I4SEE_510_PROMOTED_IND);

            if (CODE_PROMOTED_IND_COMPLETED.equals(promotedInd)
                    && StringUtils.isEmpty(value)) {
                String errorId = "I4SEE 610 Post Graduation Plans cannot be blank if I4See 510 Promoted indicator = 3";
                String errorMessage = "I4SEE 610 = " + value;
                errors.add(new StateReportValidationError(entity, field, errorId, errorMessage));
            }

            return errors;
        }
    }

    /**
     * Validate diploma type.
     */
    protected class Validate620DiplomaType implements FieldValidator {

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

            SisStudent student = (SisStudent) entity.getBean();

            if (!StringUtils.isEmpty(value)) {
                // String promotedInd = entity.getFieldValue(ALIAS_I4SEE_510_PROMOTED_IND);
                String promotedInd = (String) student.getFieldValueByAlias(ALIAS_I4SEE_510_PROMOTED_IND);

                if (!CODE_PROMOTED_IND_COMPLETED.equals(promotedInd)) {
                    String errorId =
                            "I4SEE 620 Diploma Type " + value + " not valid for Promoted Indicator " + promotedInd;
                    String errorMessage = "I4SEE 620 = " + value;
                    entity.addRetrievalError(field.getFieldId(),
                            new StateReportValidationError(entity, field, errorId, errorMessage));

                    errors.add(new StateReportValidationError(entity, field, errorId, errorMessage));
                }
            }

            return errors;
        }
    }

    /**
     * The Class Validate660OrigGradYear.
     */
    protected class Validate660OrigGradYear implements FieldValidator {

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

            String grade = entity.getFieldValue(I4SEE_400_GRADE);
            int gradeNum = 0;
            try {
                gradeNum = Integer.parseInt(grade);
            } catch (NumberFormatException e) {
                // use grade zero
            }
            if ((gradeNum < 9 || gradeNum > 12) && gradeNum != 15 && !StringUtils.isEmpty(value)) {
                String errorId =
                        "I4SEE 660 Original Graduation Year " + value
                                + " not valid for students with grade level less than 9";
                String errorMessage = "I4SEE 660 = " + value;
                entity.addRetrievalError(field.getFieldId(),
                        new StateReportValidationError(entity, field, errorId, errorMessage));

                errors.add(new StateReportValidationError(entity, field, errorId, errorMessage));
            } else if (gradeNum > 8 && gradeNum < 13) {
                int year = 0;
                try {
                    year = Integer.parseInt(StringUtils.unNullify(value));
                } catch (NumberFormatException e) {
                    // use year zero
                }
                int currentYear = data.getCurrentContext().getSchoolYear();
                if (year > currentYear + 6 || year < currentYear - 3) {
                    String errorId =
                            "I4SEE 660 Original Graduation Year " + value
                                    + " not valid for students with grade level greater than 8";
                    String errorMessage = "I4SEE 660 = " + value;
                    entity.addRetrievalError(field.getFieldId(),
                            new StateReportValidationError(entity, field, errorId, errorMessage));

                    errors.add(new StateReportValidationError(entity, field, errorId, errorMessage));
                }
            }

            return errors;
        }

    }

    /**
     * Validates all fields meet minimum length requirements.
     */
    protected class ValidateMinimumFieldLength implements FieldValidator {

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

            // This check needs only for EOY report
            if (REPORT_TYPE_EOY.equals(m_reportType)
                    && BOY_NOT_USED_FIELDS_LENGTHS.containsKey(field.getFieldId())) {
                Integer integer = BOY_NOT_USED_FIELDS_LENGTHS.get(field.getFieldId());

                if (entity.getFieldValue(field.getFieldId()).length() < integer.intValue()) {
                    String errorId = " length does not meet or exceed minimum length of: " + integer;
                    String errorMessage = field.getFieldId() + " : " + value;
                    errors.add(new StateReportValidationError(entity, field, errorId, errorMessage));
                }
            }

            return errors;
        }
    }

    /**
     * Input Definition Parameters
     */
    public static final String CALCULATE_TOTALS_PARAM = "calculateTotals";
    public static final String CALENDAR_CODE_STANDARD = "Standard";
    public static final String INCLUDE_INACTIVE_STUDENTS_PARAM = "includeInactive";
    public static final String INCLUDE_STUDENT_NAMES_PARAM = "includeStudentName";
    public static final String INCLUDE_SUMMER_WITHDRAWALS_PARAM = "includeSummerWithdrawals";
    public static final String QUERY_BY_PARAM = "queryBy";
    public static final String QUERY_STRING_PARAM = "queryString";
    public static final String REPORT_DATE_PARAM = "reportDate";
    public static final String REPORT_TYPE_PARAM = "reportType";
    public static final String REQUIRE_MEMBER_DAY_PARAM = "requireMemberDay";
    public static final String REQUIRE_REPORT_STATUS_PARAM = "requireReportStatus";
    public static final String SORT_PARAM = "sort";
    public static final String SUMMER_END_DATE_PARAM = "summerEndDate";
    public static final String SUMMER_START_DATE_PARAM = "summerStartDate";
    public static final String UPDATE_RECORDS_PARAM = "updateRecords";

    /**
     * Retriever Parameters
     */
    protected static final String CALC_ID_I4SEE_050_SCHOOL_NUMBER = "I4SEE050";
    protected static final String CALC_ID_I4SEE_210_ENROLLMENT_STATUS = "I4SEE210";
    protected static final String CALC_ID_I4SEE_220_TOWN_RESPONSIBLE = "I4SEE220";
    protected static final String CALC_ID_I4SEE_225_DISTRICT_RESPONSIBLE = "I4SEE225";
    protected static final String CALC_ID_I4SEE_230_ENTRY_DATE = "I4SEE230";
    protected static final String CALC_ID_I4SEE_240_ENTRY_CODE = "I4SEE240";
    protected static final String CALC_ID_I4SEE_250_EXIT_DATE = "I4SEE250";
    protected static final String CALC_ID_I4SEE_260_EXIT_CODE = "I4SEE260";
    protected static final String CALC_ID_I4SEE_300_DAYS_IN_ATTENDANCE = "I4SEE300";
    protected static final String CALC_ID_I4SEE_310_DAYS_ABSENT = "I4SEE310";
    protected static final String CALC_ID_I4SEE_400_GRADE = "I4SEE400";
    protected static final String CALC_ID_I4SEE_405_FULL_DAY_PERCENT = "I4SEE405";
    protected static final String CALC_ID_I4SEE_420_RACE = "I4SEE420";
    protected static final String CALC_ID_I4SEE_510_PROMOTED_IND = "I4SEE510";
    protected static final String CALC_ID_I4SEE_520_530_CONDUCT_ACTION_TOTALS = "I4SEE520";
    protected static final String CALC_ID_I4SEE_610_POST_GRADUATE_PLANS = "I4SEE610";
    protected static final String CALC_ID_I4SEE_620_DIPLOMA_TYPE = "I4SEE620";
    protected static final String CALC_ID_I4SEE_HOME_CODES = "I4SEEHOME";

    protected static final String VAL_ID_I4SEE_420_RACE_CODE = "I4SEE420";
    protected static final String VAL_ID_I4SEE_210_ENROLLMENT_STATUS = "I4SEE210";
    protected static final String VAL_ID_I4SEE_240_ENTRY_CODE = "I4SEE240";
    protected static final String VAL_ID_I4SEE_260_EXIT_CODE = "I4SEE260";
    protected static final String VAL_ID_I4SEE_620_DIPLOMA_TYPE = "I4SEE620";
    protected static final String VAL_ID_I4SEE_610_POST_GRADUATE_PLANS = "I4SEE610";
    protected static final String VAL_ID_I4SEE_660_ORIG_GRAD_YEAR = "I4SEE660";
    protected static final String VAL_ID_MINIMUM_LENGTH = "MINLENGTH";

    /**
     * Alias constants
     */
    private static final String I4SEE_050_SCHOOL_NUMBER = "School Number";
    private static final String I4SEE_210_ENROLLMENT_STATUS = "Enrollment Status";
    private static final String I4SEE_225_RESPONSIBLE_DISTRICT = "District Reponsible";
    private static final String I4SEE_230_ENTRY_DATE = "Entry Date";
    private static final String I4SEE_240_ENTRY_CODE = "Entry Code";
    private static final String I4SEE_250_EXIT_DATE = "Exit Date";
    private static final String I4SEE_510_PROMOTED_IND = "i4see 510";
    // private static final String I4SEE_260_EXIT_CODE = "Exit Code";
    private static final String I4SEE_300_DAYS_IN_ATTENDANCE = "Half Days";
    private static final String I4SEE_310_DAYS_ABSENT = "Half Days Absent";
    private static final String I4SEE_400_GRADE = "Grade";
    private static final String I4SEE_405_FULL_DAY_PERCENT = "Full Day Percent";
    private static final String I4SEE_420_RACE = "Race";
    private static final String I4SEE_520_SUSPENSIONS_IN = "Days Suspended IS";
    private static final String I4SEE_530_SUSPENSIONS_OUT = "Days Suspended OS";
    // private static final String I4SEE_580_RESIDENTIAL_HOME_FIELD_NAME = "Residential Home";
    // private static final String I4SEE_610_POST_GRAD_PLANS = "Post Grad Plans";
    // private static final String I4SEE_620_DIPLOMA_TYPE = "Diploma Type";
    private static final String I4SEE_660_ORIGINAL_YOG = "Orig Grad Year";
    private static final String I4SEE_STATUS_DO_NOT_REPORT = "Do not report";
    private static final String I4SEE_STATUS_FIELD_REPORT_CODE = "Report";
    private static final String STUDENT_NAME = "name view";

    /*
     * Aliases
     */
    // STUDENT
    // private static final String ALIAS_EXCLUDE_STD = "DOE EXCLUDE STD";
    private static final String ALIAS_ADJUSTED_SCHOOL_NUMBER = "i4see Adj School Number";
    private static final String ALIAS_EXCLUDE_STUDENT = "I4SEE EXCLUDE";
    private static final String ALIAS_I4SEE_300_DAYS_IN_ATTENDANCE = "i4see 300";
    private static final String ALIAS_I4SEE_310_HALF_DAYS_ABSENT = "i4see 310";
    private static final String ALIAS_I4SEE_405_FULL_DAY_PERCENT = "i4see 405";
    private static final String ALIAS_I4SEE_500_HOMELESS_CD = "i4see 500";
    private static final String ALIAS_I4SEE_510_PROMOTED_IND = "i4see 510";
    private static final String ALIAS_I4SEE_520_SUSPENSIONS_IN = "i4see 520";
    private static final String ALIAS_I4SEE_580_RESIDENTIAL_HOME = "i4see 580";
    private static final String ALIAS_I4SEE_610_PORT_GRAD = "i4see 610";
    private static final String ALIAS_I4SEE_620_DIPLOMA_TYPE = "i4see 620";
    private static final String ALIAS_I4SEE_STATUS = "i4see Status";

    // STUDENT_ENROLLMENT
    private static final String ALIAS_I4SEE_210_ENROLLMENT_STATUS = "i4see 210";
    private static final String ALIAS_I4SEE_220_TOWN_RESPONSIBLE = "i4see 220";
    private static final String ALIAS_I4SEE_225_DISTRICT_RESPONSIBLE = "i4see 225";

    // STUDENT_CONDUCT_ACTION
    private static final String ALIAS_SUSPENSION_LENGTH = "i4see Susp Length";

    // SCHOOL
    private static final String ALIAS_I4SEE_050_SCHOOL_NUMBER = "i4see 050";
    private static final String ALIAS_EXCLUDE_SCHOOL = "i4see EXCLUDE SCHOOL";

    // ORGANIZATION
    private static final String ALIAS_I4SEE_040_DISTRICT_NUMBER = "i4see 040";

    // COURSE
    // private static final String ALIAS_EXCLUDE_CRS = "DOE EXCLUDE CRS";

    // MASTER_SCHEDULE
    // private static final String ALIAS_EXCLUDE_MST = "DOE EXCLUDE MST";

    /*
     * Standard aliases
     */

    /*
     * List of fields NOT USED or BlanksAllowed(if N/A) by the B-O-Y report
     */
    /*
     * private static final List<String> BOY_NOT_USED_FIELDS = Arrays.asList(
     * I4SEE_405_FULL_DAY_PERCENT, // not
     * // used
     * // I4SEE_300_DAYS_IN_ATTENDANCE,
     * // //
     * // not
     * // used
     * // I4SEE_310_DAYS_ABSENT,
     * // //
     * // not
     * // used
     * I4SEE_520_SUSPENSIONS_IN, // not
     * // used
     * I4SEE_530_SUSPENSIONS_OUT, // not
     * // used
     * ALIAS_I4SEE_580_RESIDENTIAL_HOME, // not
     * // used
     * I4SEE_230_ENTRY_DATE, // BlanksAllowed(if
     * // NA)
     * I4SEE_240_ENTRY_CODE, // BlanksAllowed(if
     * // NA)
     * I4SEE_250_EXIT_DATE, // BlanksAllowed(if
     * // NA)
     * I4SEE_260_EXIT_CODE, // BlanksAllowed(if
     * // NA)
     * ALIAS_I4SEE_510_PROMOTED_IND, // BlanksAllowed(if
     * // NA)
     * I4SEE_620_DIPLOMA_TYPE, // BlanksAllowed(if
     * // NA)
     * I4SEE_610_POST_GRAD_PLANS, // BlanksAllowed(if
     * // NA)
     * I4SEE_660_ORIGINAL_YOG // BlanksAllowed(if
     * // NA)
     * );
     */
    public static final HashMap<String, Integer> BOY_NOT_USED_FIELDS_LENGTHS = new HashMap<String, Integer>() {
        {
            put(I4SEE_230_ENTRY_DATE, Integer.valueOf(8));
            put(I4SEE_240_ENTRY_CODE, Integer.valueOf(2));
            put(I4SEE_510_PROMOTED_IND, Integer.valueOf(1));
            put(I4SEE_520_SUSPENSIONS_IN, Integer.valueOf(1));
            put(I4SEE_530_SUSPENSIONS_OUT, Integer.valueOf(1));
            put(I4SEE_660_ORIGINAL_YOG, Integer.valueOf(4));
        }
    };



    /*
     * Other internal constants
     */
    private static final boolean SUPPRESS_ENROLLMENT_WARNINGS = true;
    private static final String DATE_FORMAT = "MM/dd/yyyy";

    // private static final String REGEX_NUMERIC = "[0-9]*";
    private static final String VALIDATION_INVALID_VALUE = "Invalid value";
    private static final String VALIDATION_MISSING_VALUE = "Missing value";
    private final static double HASHMAP_CAPACITY_MULTIPLIER = 1.5;

    public static final Integer REPORT_TYPE_BOY = Integer.valueOf(0);
    public static final Integer REPORT_TYPE_EOY = Integer.valueOf(1);

    private static final String CODE_SUSPENSION_IN = "1";
    private static final String CODE_SUSPENSION_OUT = "2";
    private static final String CODE_SUSPENSION_IN_CODE = "520";

    private static final String CODE_GRADE_LEVEL_11 = "11";
    private static final String CODE_GRADE_LEVEL_12 = "12";
    private static final String CODE_GRADE_LEVEL_10 = "10";
    private static final String CODE_GRADE_LEVEL_9 = "09";

    private static final String CODE_PROMOTED_IND_SAME_GRADE = "1";
    private static final String CODE_PROMOTED_IND_PROMOTED = "2";
    private static final String CODE_PROMOTED_IND_COMPLETED = "3";

    private static final String CODE_ENROLLMENT_E1 = "E1";
    private static final String CODE_ENROLLMENT_E2 = "E2";
    private static final String CODE_ENROLLMENT_R1 = "R1";
    private static final String CODE_ENROLLMENT_W1 = "W1";
    private static final String CODE_ENROLLMENT_W11 = "W11";
    private static final String CODE_ENROLLMENT_W12 = "W12";
    private static final String CODE_ENROLLMENT_W2 = "W2";
    private static final String CODE_ENROLLMENT_W4 = "W4";

    private static final String CODE_ENROLLMENT_STATUS_PREREG = "PreReg";
    private static final String CODE_ENROLLMENT_STATUS_1 = "1";
    private static final String CODE_ENROLLMENT_STATUS_2 = "2";
    private static final String CODE_ENROLLMENT_STATUS_5 = "5";
    private static final String CODE_ENROLLMENT_STATUS_7 = "7";
    private static final String CODE_ENROLLMENT_STATUS_8 = "8";

    private static final String CODE_HOME_LESS_DOES_NOT_APPLY = "1";

    private static final String CODE_DISTRICT_930 = "930";

    private static final String CODE_SCHOOL_15010 = "15010";
    private static final String CODE_SCHOOL_15020 = "15020";
    private static final String CODE_SCHOOL_15030 = "15030";
    private static final String CODE_SCHOOL_15040 = "15040";

    private static final String DEFAULT_CALENDAR_ID = "Standard";

    private static final int WHITE_RACE_IND = 1;
    private static final int BLACK_RACE_IND = 2;
    private static final int ASIAN_RACE_IND = 4;
    private static final int AMERICAN_INDIAN_RACE_IND = 8;
    private static final int PACIFIC_ISLANDER_RACE_IND = 16;
    private static final int HISPANIC_RACE_IND = 32;

    /**
     * Map of NH Race Reference codes to a bit-value for mapping into
     * the NH_STATE_RACE_CODES table below.
     * The Hispanic indicator is a remnant from when it was considered a race. Having a record
     * with the hispanic race code is the same has having the HispanicIndicator on the PERSON record
     * set to true.
     */
    static final int[] NH_TO_X2_RACE_CODES = new int[] {0, // not
            // used
            AMERICAN_INDIAN_RACE_IND, // 1 - American Indian
            ASIAN_RACE_IND, // 2 - Asian
            HISPANIC_RACE_IND, // 3 - Hispanic
            BLACK_RACE_IND, // 4 - Black
            WHITE_RACE_IND, // 5 - White
            PACIFIC_ISLANDER_RACE_IND, // 6 - Pacific Islander
            0, // not used
            0, // not used
            0 // not
              // used
              // (nor
              // is
              // it
              // valid)
    };

    /**
     * The DOE uses an arbitrary code for its combinations of races/ethnicities. X2 instead uses
     * a bitmap with the following values:
     *
     * <pre>
     * - WHITE: 1
     * - BLACK: 2
     * - ASIAN: 4
     * - AMERICAN INDIAN: 8
     * - PACIFIC ISLANDER: 16
     * - HISPANIC: 32   NOTE:  this is not a race, but an attribute to a race
     * </pre>
     *
     * The X2 bitmap is translated to a state code using the following array.
     *
     * (NH values from http://www.ed.state.nh.us/education/doe/racecodes.asp)
     */
    protected static final int[] NH_STATE_RACE_CODES = new int[] {0, // X2
            // value
            // Hispanic
            // Hawaiian
            // American
            // Asian
            // Black
            // White
            // Pacific
            // Indian/Alaska
            5, // 1 x
            4, // 2 x
            15, // 3 x x
            2, // 4 x
            13, // 5 x x
            12, // 6 x x
            15, // 7 x x x
            1, // 8 x
            10, // 9 x x
            8, // 10 x x
            21, // 11 x x x
            7, // 12 x x
            19, // 13 x x x
            18, // 14 x x x
            28, // 15 x x x x
            6, // 16 x
            17, // 17 x x
            16, // 18 x x
            27, // 19 x x x
            14, // 20 x x
            29, // 21 x x x
            31, // 22 x x x
            32, // 23 x x x x
            11, // 24 x x
            23, // 25 x x x
            22, // 26 x x x
            30, // 27 x x x x
            20, // 28 x x x
            29, // 29 x x x x
            31, // 30 x x x x
            33, // 31 x x x x x
            3, // 32 x
            3, // 33 x x
            36, // 34 x x
            45, // 35 x x x
            35, // 36 x x
            43, // 37 x x x
            42, // 38 x x x
            45, // 39 x x x x
            34, // 40 x x
            40, // 41 x x x
            39, // 42 x x x
            51, // 43 x x x x
            38, // 44 x x x
            49, // 45 x x x x
            48, // 46 x x x x
            28, // 47 x x x x x
            58, // 48 x x
            47, // 49 x x x
            46, // 50 x x x
            57, // 51 x x x x
            44, // 52 x x x
            59, // 53 x x x x
            61, // 54 x x x x
            62, // 55 x x x x x
            41, // 56 x x x
            53, // 57 x x x x
            52, // 58 x x x x
            60, // 59 x x x x x
            50, // 60 x x x x
            59, // 61 x x x x x
            61, // 62 x x x x x
            63}; // 63

    /**
     * Supporting instance variables.
     * These are protected rather than private so they can be accessed by the inner classes.
     */
    protected boolean m_calculateTotals = false;
    protected boolean m_includeInactiveStudents = false;
    protected boolean m_includeStudentNames = false;
    protected boolean m_requireMemberDay = false;
    protected Collection m_suspensionInCodes = new LinkedList();
    protected Collection m_suspensionOutCodes = new LinkedList();
    // protected Converter m_integerConverter;
    protected EnrollmentManager m_enrollManager;
    protected HashMap m_schoolsToCalendars = new HashMap();
    protected HashMap<String, Map<String, String>> m_fieldToRefTable;
    protected Map<String, Collection<Race>> m_raceCodeMap;
    protected Map<String, Integer> m_absences = new HashMap<String, Integer>();
    protected Map<String, Schedule> m_scheduleMap = new HashMap();
    protected Map<String, SisSchool> m_schoolMap;
    protected Map<String, SisSchool> m_excludedSchoolMap;
    protected PlainDate m_reportDate;
    protected PlainDate m_schoolYearStartDate;
    protected PlainDate m_summerEndDate;
    protected PlainDate m_summerStartDate;
    protected SimpleDateFormat m_dateFormat;
    protected boolean m_updateRecords = false;
    protected Integer m_reportType;
    protected String m_adjustedSchoolCode;
    protected String m_diplomaType;
    protected String m_excludeSchool;
    protected String m_excludeStudent;
    protected String m_homelessCD;
    protected String m_postGrad;
    protected String m_promotedInd;
    protected Map<String, ReferenceCode> m_referenceGradeCodeMap;
    protected String m_reportStatusField;
    protected String m_residentialHome;
    protected String m_suspensionLength;
    protected NHI4SeeStudentHistoryHelper m_helper;
    protected TreeMap m_gradeLevelMap;
    private boolean m_preferenceMemberOnWithdrawal;

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
        return "I4SEE ENROLLMENT";
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
     * Returns the number of days the student has been a member from the start of school to the
     * report date.
     *
     * @param i4SeeEntity I4SeeEntity
     * @return String
     */
    public String getMembershipDays(I4SeeEntity i4SeeEntity) {
        return i4SeeEntity.getCurrentSpan().getMembershipDays() + "";
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
        /*
         * Load initialization data
         */
        initializeFields();

        /*
         * Load state codes for reference tables
         */
        loadStateReferenceCodes();

        /*
         * Load conduct codes
         */
        loadConductCodes();

        /*
         * Load Grade Level Map
         */
        loadGradeLevelMap();

        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {
            /*
             * Build query object that will be used to retrieve export students.
             */
            m_helper = new NHI4SeeStudentHistoryHelper(this);
            m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);

            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_SPAN_BREAK_ON_STATUS, Boolean.TRUE);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_SPAN_BREAK_ON_YOG, Boolean.TRUE);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_SCHOOL,
                    isSchoolContext() ? Boolean.TRUE : Boolean.FALSE);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_LOAD_ALL_ATTENDANCE, Boolean.TRUE);
            if (m_includeInactiveStudents) {
                m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_INCLUDE_INACTIVE, Boolean.TRUE);
            }

            m_helper.getStudentCriteria().addNotEqualTo(m_excludeStudent, BooleanAsStringConverter.TRUE);

            Boolean requireReportStatus = (Boolean) getParameter(REQUIRE_REPORT_STATUS_PARAM);
            if (requireReportStatus != null && requireReportStatus.booleanValue()) {
                m_helper.getStudentCriteria().addEqualTo(m_reportStatusField, I4SEE_STATUS_FIELD_REPORT_CODE);
            } else {
                m_helper.getStudentCriteria().addNotEqualTo(m_reportStatusField, I4SEE_STATUS_DO_NOT_REPORT);
            }
            m_helper.getStudentCriteria().addNotEqualTo(Student.REL_SCHOOL + PATH_DELIMITER + m_excludeSchool,
                    BooleanAsStringConverter.TRUE);


            /*
             * Load Schools
             */
            loadSchools();
            loadExcludedSchools();

            /*
             * Load Active Schedules
             */
            loadActiveSchedules();

            /*
             * Load the race codes for all students included in the export.
             */
            loadRaceRecords();

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
            calcs.put(CALC_ID_I4SEE_050_SCHOOL_NUMBER, new Retrieve050SchoolNumber());
            calcs.put(CALC_ID_I4SEE_210_ENROLLMENT_STATUS, new Retrieve210EnrollmentStatus());
            calcs.put(CALC_ID_I4SEE_220_TOWN_RESPONSIBLE, new Retrieve220TownResponsible());
            calcs.put(CALC_ID_I4SEE_225_DISTRICT_RESPONSIBLE, new Retrieve225DistrictResponsible());
            calcs.put(CALC_ID_I4SEE_230_ENTRY_DATE, new Retrieve230EntryDate());
            calcs.put(CALC_ID_I4SEE_240_ENTRY_CODE, new Retrieve240EntryCode());
            calcs.put(CALC_ID_I4SEE_250_EXIT_DATE, new Retrieve250ExitDate());
            calcs.put(CALC_ID_I4SEE_260_EXIT_CODE, new Retrieve260ExitCode());
            calcs.put(CALC_ID_I4SEE_300_DAYS_IN_ATTENDANCE, new Retrieve300DaysInAttendance());
            calcs.put(CALC_ID_I4SEE_310_DAYS_ABSENT, new Retrieve310DaysAbsent());
            calcs.put(CALC_ID_I4SEE_400_GRADE, new Retrieve400GradeLevel());
            calcs.put(CALC_ID_I4SEE_405_FULL_DAY_PERCENT, new Retrieve405FullDayPercent());
            calcs.put(CALC_ID_I4SEE_420_RACE, new Retrieve420Race());
            calcs.put(CALC_ID_I4SEE_510_PROMOTED_IND, new Retrieve510PromotedInd());
            // calcs.put(CALC_ID_I4SEE_520_530_CONDUCT_ACTION_TOTALS, new
            // Retrieve520and530ConductActionTotals(m_suspensionLength));
            calcs.put(CALC_ID_I4SEE_520_530_CONDUCT_ACTION_TOTALS, new Retrieve520and530ConductActionTotals());
            calcs.put(CALC_ID_I4SEE_610_POST_GRADUATE_PLANS, new Retrieve610PostGraduatePlans());
            calcs.put(CALC_ID_I4SEE_620_DIPLOMA_TYPE, new Retrieve620DiplomaType());
            calcs.put(CALC_ID_I4SEE_HOME_CODES, new RetrieveHomeCodes());
            super.addCalcs(calcs);

            HashMap validators = new HashMap<String, FieldRetriever>();
            validators.put(VAL_ID_I4SEE_210_ENROLLMENT_STATUS, new Validate210EnrollmentStatus());
            validators.put(VAL_ID_I4SEE_240_ENTRY_CODE, new Validate240EntryCode());
            validators.put(VAL_ID_I4SEE_260_EXIT_CODE, new Validate260ExitCode());
            validators.put(VAL_ID_I4SEE_420_RACE_CODE, new Validate420RaceCode());
            validators.put(VAL_ID_I4SEE_610_POST_GRADUATE_PLANS, new Validate610PostGraduatePlans());
            validators.put(VAL_ID_I4SEE_620_DIPLOMA_TYPE, new Validate620DiplomaType());
            validators.put(VAL_ID_I4SEE_660_ORIG_GRAD_YEAR, new Validate660OrigGradYear());
            validators.put(VAL_ID_MINIMUM_LENGTH, new ValidateMinimumFieldLength());
            super.addValidators(validators);

            setQuery(m_helper.getStudentQuery(false));
            setEntityClass(I4SeeEntity.class);
        }
    }

    /**
     * Looks up the first in-session date for the student's calendar. If one does not exists, use
     * the first day date (start date of district school year).
     *
     * @param snapshot EnrollmentSnapshot
     * @return PlainDate
     */
    protected PlainDate findFirstEnrollmentDay(EnrollmentSnapshot snapshot) {
        PlainDate date = null;
        SisSchool school = snapshot.getSchool();
        if (school == null) {
            school = m_schoolMap.get(snapshot.getStudent().getSchoolOid());
        }

        if (school != null) {
            date = findFirstEnrollmentDay(school, snapshot.getStudent());
        }
        return date;
    }

    /**
     * Looks up the first in-session date for the student's calendar. If one does not exists, use
     * the first day date (start date of district school year).
     *
     * @param school SisSchool
     * @param calendarCode String
     * @return PlainDate
     */
    protected PlainDate findFirstEnrollmentDay(SisSchool school, String calendarCode) {
        PlainDate date = null;

        Set<PlainDate> days = findInsessionDates(school, calendarCode);

        if (days != null) {
            for (PlainDate day : days) {
                if ((date == null) || day.before(date)) {
                    date = day;
                }
            }
        }

        if (date == null) {
            date = getSchoolScheduleStartDate(school.getOid());
        }

        if (date == null) {
            date = m_schoolYearStartDate;
        }

        return date;
    }

    /**
     * Looks up the first in-session date for the student's calendar. If one does not exists, use
     * the first day date (start date of district school year).
     *
     * @param school SisSchool
     * @param student Student
     * @return PlainDate
     */
    protected PlainDate findFirstEnrollmentDay(SisSchool school, Student student) {

        return findFirstEnrollmentDay(school, student.getCalendarCode());
    }

    /**
     * Find last enrollment day.
     *
     * @param school SisSchool
     * @param calendarCode String
     * @return PlainDate
     */
    protected PlainDate findLastEnrollmentDay(SisSchool school, String calendarCode) {
        PlainDate date = null;

        Set<PlainDate> days = findInsessionDates(school, calendarCode);

        if (days != null) {
            for (PlainDate day : days) {
                if ((date == null) || day.after(date)) {
                    date = day;
                }
            }
        }

        if (date == null) {
            date = getSchoolScheduleEndDate(school.getOid());
        }

        if (date == null) {
            date = getCurrentContext().getEndDate();
        }

        return date;
    }

    /**
     * Return number of days absent.
     *
     * @param studentOid String
     * @return absentDays
     */
    protected int getAbsentDays(String studentOid) {
        int absentDays = 0;
        List<StudentAttendance> studentAttendances = m_helper.getStudentAttendances(studentOid);

        if (studentAttendances != null && studentAttendances.size() > 0) {
            for (StudentAttendance studentAttendance : studentAttendances) {
                if (studentAttendance.getAbsentIndicator()) {
                    absentDays++;
                }
            }
        }

        return absentDays;
    }

    /**
     * Gets the grade code.
     *
     * @param yog int
     * @return Reference code
     */
    protected ReferenceCode getGradeCode(int yog) {
        ReferenceCode gradeCode = null;

        int maxGradeLevel = StudentManager.getMaxGradeLevel(getBroker());
        if (yog >= getCurrentContext().getSchoolYear()
                && yog <= getCurrentContext().getSchoolYear() + maxGradeLevel + 3) {
            ModelBroker broker = new ModelBroker(getPrivilegeSet());
            TreeMap sortedGradeLevels = StudentManager.buildGradeLevelMap(broker);
            List<String> matchingGradeLevels = StudentManager.getMatchingGradeLevels(maxGradeLevel, yog,
                    getCurrentContext().getSchoolYear(), sortedGradeLevels);
            for (String matchingGradeLevel : matchingGradeLevels) {
                gradeCode = getGradeCode(matchingGradeLevel);
                if (gradeCode != null) {
                    break;
                }
            }
        }
        return gradeCode;
    }

    /**
     * Gets the grade code.
     *
     * @param gradeLevel String
     * @return Reference code
     */
    protected ReferenceCode getGradeCode(String gradeLevel) {
        if (m_referenceGradeCodeMap == null) {
            m_referenceGradeCodeMap = Collections.EMPTY_MAP;
            DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
            ModelProperty prop =
                    new ModelProperty(SisStudent.class, SisStudent.COL_GRADE_LEVEL, getBroker().getPersistenceKey());
            DataDictionaryField field = dictionary.findDataDictionaryField(prop.getFieldId());
            if (field != null) {
                ReferenceTable referenceTable = field.getReferenceTable();
                m_referenceGradeCodeMap = referenceTable.getCodeMap();
            }
        }
        return m_referenceGradeCodeMap.get(gradeLevel);
    }

    /**
     * Build Field definition for the student name view.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getName() {
        FieldDefinition field = new FieldDefinition(STUDENT_NAME, SisStudent.COL_NAME_VIEW,
                null, false, 1, 32, null,
                null, null, null, null);

        return field;
    }

    /**
     * Find in session date in next priority
     * 1 input calendar
     * 2 default calendar
     * 3 any calendar.
     *
     * @param school SisSchool
     * @param calendar String
     * @return Set
     */
    private Set<PlainDate> findInsessionDates(SisSchool school, String calendar) {
        Set<PlainDate> insessionDates = m_helper.getCalendarDays(school, calendar);
        if (insessionDates == null && !DEFAULT_CALENDAR_ID.equals(calendar)) {
            insessionDates = m_helper.getCalendarDays(school, DEFAULT_CALENDAR_ID);
        }
        if (insessionDates == null && !CALENDAR_ANY.equals(calendar)) {
            insessionDates = m_helper.getCalendarDays(school, CALENDAR_ANY);
        }
        return insessionDates == null ? new TreeSet<PlainDate>() : insessionDates;
    }

    /**
     * Gets the school schedule end date.
     *
     * @param schoolOid String
     * @return Plain date
     */
    private PlainDate getSchoolScheduleEndDate(String schoolOid) {
        PlainDate startDate = null;

        if (m_scheduleMap.containsKey(schoolOid)) {
            startDate = m_scheduleMap.get(schoolOid).getEndDate();
        }

        return startDate;
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
     * Returns the reference table's state code for a field's reference code.
     *
     * @param propertyName String
     * @return String
     */
    /*
     * protected String getStateCode(String fieldID, StateReportEntity entity, FieldDefinition
     * field, String enrollmentCode,
     * String javaname)
     * {
     * String returnCode = null;
     * // Enrollment Code can be empty that not an Error
     * if (!StringUtils.isEmpty(enrollmentCode))
     * {
     * //returnCode = m_fieldToRefTable.get(javaname).get(enrollmentCode);
     * Map<String, String> refCodes = m_fieldToRefTable.get(javaname);
     * String errorMessage = "";
     * if (refCodes == null)
     * {
     * // Missing state code
     * errorMessage = "MISSING REFERENCE CODE FOR: ";
     * entity.addRetrievalError(fieldID, new StateReportValidationError(entity, field, errorMessage
     * + javaname, ""));
     * }
     * else
     * {
     * returnCode = refCodes.get(enrollmentCode);
     *
     * if (returnCode == null)
     * {
     * // Missing state code
     * errorMessage = "MISSING STATE CODE FOR: ";
     * entity.addRetrievalError(fieldID, new StateReportValidationError(entity, field, errorMessage
     * + enrollmentCode, ""));
     * }
     * }
     * }
     *
     * return returnCode;
     * }
     */
    /**
     * Returns a map of base reference codes to their state reference code equivalents for the
     * reference table used by the given property. If the property doesn't use a
     * reference table then an empty map is returned.
     *
     * @param propertyName
     *
     * @return Map
     */
    private Map getStateCodeReferenceMap(String propertyName) {
        HashMap baseToStateCodes = null;

        ModelProperty property =
                new ModelProperty(SisStudent.class.getName(), propertyName, getBroker().getPersistenceKey());
        DataDictionaryField field = property.getField();
        if (field.hasReferenceTable()) {
            /*
             * Collection codes = field.getReferenceTable().getReferenceCodes(getBroker());
             * baseToStateCodes = new HashMap((int) (codes.size() * HASHMAP_CAPACITY_MULTIPLIER));
             * Iterator codeIterator = codes.iterator();
             * while (codeIterator.hasNext())
             * {
             * ReferenceCode code = (ReferenceCode) codeIterator.next();
             * baseToStateCodes.put(code.getCode(), code.getStateCode());
             * }
             */

            Collection<ReferenceCode> referenceCodes = field.getReferenceTable().getReferenceCodes(getBroker());
            baseToStateCodes = new HashMap((int) (referenceCodes.size() * HASHMAP_CAPACITY_MULTIPLIER));

            if (referenceCodes != null) {
                for (ReferenceCode referenceCode : referenceCodes) {
                    baseToStateCodes.put(referenceCode.getCode(), referenceCode.getStateCode());
                }
            }
        } else {
            /*
             * If the propertyName is for the student enrollment codes, these need to be pulled
             * from reference table via the codes specified in the system preferences
             */
            if (propertyName.equals(SisStudent.REL_ENROLLMENTS + "." + StudentEnrollment.COL_ENROLLMENT_CODE)) {
                /*
                 * get the preferences for the organization and the two OIDs for withdrawal codes
                 * and entry codes
                 */
                PreferenceSet preferenceSet = PreferenceManager.getPreferenceSet(getOrganization());
                String[] sysPrefDefs = {
                        SisPreferenceConstants.ENROLLMENT_ENTRY_CODES,
                        SisPreferenceConstants.ENROLLMENT_WITHDRAWAL_CODES
                };

                baseToStateCodes = new HashMap();
                for (String sysPrefDef : sysPrefDefs) {
                    String refTableOid = preferenceSet.getPreferenceValue(sysPrefDef);
                    ReferenceTable refTable =
                            (ReferenceTable) getBroker().getBeanByOid(ReferenceTable.class, refTableOid);
                    Collection<ReferenceCode> referenceCodes = refTable.getReferenceCodes(getBroker());

                    if (referenceCodes != null) {
                        for (ReferenceCode referenceCode : referenceCodes) {
                            baseToStateCodes.put(referenceCode.getCode(), referenceCode.getStateCode());
                        }
                    }

                    // Collection codes = refTable.getReferenceCodes(getBroker());
                    // Iterator codeIterator = codes.iterator();
                    /*
                     * while (codeIterator.hasNext())
                     * {
                     * ReferenceCode code = (ReferenceCode) codeIterator.next();
                     * baseToStateCodes.put(code.getCode(), code.getStateCode());
                     * }
                     */}
            }
        }

        return baseToStateCodes;
    }

    /**
     * Sets the Java names (bean paths) for all the exported fields.
     */
    private void initializeFields() {
        // System Parameters
        m_dateFormat = new SimpleDateFormat(DATE_FORMAT);
        m_schoolYearStartDate = getCurrentContext().getStartDate();
        m_enrollManager = new EnrollmentManager(getBroker(), getPrivilegeSet(), getOrganization());

        // Load Parameters
        m_calculateTotals = false;
        if (getParameter(CALCULATE_TOTALS_PARAM) != null) {
            m_calculateTotals = ((Boolean) getParameter(CALCULATE_TOTALS_PARAM)).booleanValue();
        }
        m_includeInactiveStudents = false;
        if (getParameter(INCLUDE_INACTIVE_STUDENTS_PARAM) != null) {
            m_includeInactiveStudents = ((Boolean) getParameter(INCLUDE_INACTIVE_STUDENTS_PARAM)).booleanValue();
        }
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
        if (getParameter(SUMMER_START_DATE_PARAM) != null) {
            m_summerStartDate = (PlainDate) getParameter(SUMMER_START_DATE_PARAM);
        }
        if (getParameter(SUMMER_END_DATE_PARAM) != null) {
            m_summerEndDate = (PlainDate) getParameter(SUMMER_END_DATE_PARAM);
        }
        m_updateRecords = false;
        if (getParameter(UPDATE_RECORDS_PARAM) != null) {
            m_updateRecords = ((Boolean) getParameter(UPDATE_RECORDS_PARAM)).booleanValue();
        }

        m_preferenceMemberOnWithdrawal = Boolean.valueOf(PreferenceManager.getPreferenceValue(getOrganization(),
                SisPreferenceConstants.STUDENT_MEMBER_ON_WITHDRAWAL_DATE)).booleanValue();

        // Load Alias database field Names
        m_adjustedSchoolCode = translateAliasToJavaName(ALIAS_ADJUSTED_SCHOOL_NUMBER, true);
        m_diplomaType = translateAliasToJavaName(ALIAS_I4SEE_620_DIPLOMA_TYPE, true);
        m_excludeSchool = translateAliasToJavaName(ALIAS_EXCLUDE_SCHOOL, true);
        m_excludeStudent = translateAliasToJavaName(ALIAS_EXCLUDE_STUDENT, true);
        m_homelessCD = translateAliasToJavaName(ALIAS_I4SEE_500_HOMELESS_CD, true);
        m_postGrad = translateAliasToJavaName(ALIAS_I4SEE_610_PORT_GRAD, true);
        m_promotedInd = translateAliasToJavaName(ALIAS_I4SEE_510_PROMOTED_IND, true);
        m_reportStatusField = translateAliasToJavaName(ALIAS_I4SEE_STATUS, true);
        m_suspensionLength = translateAliasToJavaName(ALIAS_SUSPENSION_LENGTH, true);
        m_residentialHome = translateAliasToJavaName(ALIAS_I4SEE_580_RESIDENTIAL_HOME, true);
    }

    /**
     * Loads the active schedule for each school.
     */
    private void loadActiveSchedules() {
        Collection<SisSchool> schools = m_schoolMap.values();

        for (SisSchool school : schools) {
            if (school.getActiveSchedule() != null) {
                m_scheduleMap.put(school.getOid(), school.getActiveSchedule());
            }
        }
    }

    /**
     * Loads all the basic codes that correspond to the state codes for in-school suspensions and
     * out-of-school suspensions. If there are no basic codes for the state code then the
     * collection(s) will be empty.
     */
    private void loadConductCodes() {
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField actionCode =
                dictionary.findDataDictionaryField(ConductAction.class.getName(), ConductAction.COL_ACTION_CODE);

        String actionReferenceTable = actionCode.getDataFieldConfig().getReferenceTableOid();

        // In-school
        Criteria criteria = new Criteria();
        criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, actionReferenceTable);
        criteria.addEqualTo(ReferenceCode.COL_STATE_CODE, CODE_SUSPENSION_IN);

        QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, criteria);

        Collection<ReferenceCode> referenceCodes = getBroker().getCollectionByQuery(query);
        if (referenceCodes != null) {
            for (ReferenceCode referenceCode : referenceCodes) {
                m_suspensionInCodes.add(referenceCode.getCode());
            }
        }
        // QueryIterator referenceCodes = getBroker().getIteratorByQuery(query);
        /*
         * try
         * {
         * while (referenceCodes.hasNext())
         * {
         * ReferenceCode referenceCode = (ReferenceCode) referenceCodes.next();
         * m_suspensionInCodes.add(referenceCode.getCode());
         * }
         * }
         * finally
         * {
         * referenceCodes.close();
         * }
         */

        // Out-of-school
        criteria = new Criteria();
        criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, actionReferenceTable);
        criteria.addEqualTo(ReferenceCode.COL_STATE_CODE, CODE_SUSPENSION_OUT);

        query = new QueryByCriteria(ReferenceCode.class, criteria);
        /*
         * referenceCodes = getBroker().getIteratorByQuery(query);
         * try
         * {
         * while (referenceCodes.hasNext())
         * {
         * ReferenceCode referenceCode = (ReferenceCode) referenceCodes.next();
         * m_suspensionOutCodes.add(referenceCode.getCode());
         * }
         * }
         * finally
         * {
         * referenceCodes.close();
         * }
         */

        referenceCodes = getBroker().getCollectionByQuery(query);
        if (referenceCodes != null) {
            for (ReferenceCode referenceCode : referenceCodes) {
                m_suspensionOutCodes.add(referenceCode.getCode());
            }
        }
    }

    /**
     * Loads all the school beans in the district.
     */
    private void loadExcludedSchools() {
        Criteria schoolCriteria = new Criteria();
        schoolCriteria.addEqualTo(m_excludeSchool, BooleanAsStringConverter.TRUE);

        QueryByCriteria schoolQuery = new QueryByCriteria(SisSchool.class, schoolCriteria);

        m_excludedSchoolMap =
                getBroker().getMapByQuery(schoolQuery, X2BaseBean.COL_OID, getBroker().getCount(schoolQuery));
    }

    /**
     * Loads Grade Level Map.
     */
    private void loadGradeLevelMap() {
        m_gradeLevelMap = StudentManager.buildGradeLevelMap(getBroker());
    }

    /**
     * Loads the race records for all students included in the export into a Map keyed to the
     * Person OID.
     */
    private void loadRaceRecords() {
        SubQuery personSubQuery =
                new SubQuery(SisStudent.class, SisStudent.COL_PERSON_OID, m_helper.getStudentCriteria());
        QueryByCriteria studentQuery = m_helper.getStudentQuery(false);

        Criteria raceCriteria = new Criteria();
        raceCriteria.addIn(Race.COL_PERSON_OID, personSubQuery);

        QueryByCriteria raceQuery = new QueryByCriteria(Race.class, raceCriteria);
        int count = getBroker().getCount(studentQuery);

        m_raceCodeMap = getBroker().getGroupedCollectionByQuery(raceQuery, Race.COL_PERSON_OID, count);
    }

    /**
     * Loads all the school beans in the district.
     */
    private void loadSchools() {
        Criteria schoolCriteria = new Criteria();
        // schoolCriteria.addNotEqualTo(m_excludeSchool, BooleanAsStringConverter.TRUE);

        QueryByCriteria schoolQuery = new QueryByCriteria(SisSchool.class, schoolCriteria);

        m_schoolMap = getBroker().getMapByQuery(schoolQuery, X2BaseBean.COL_OID, getBroker().getCount(schoolQuery));
    }

    /**
     * Load State Reference Codes.
     */
    private void loadStateReferenceCodes() {
        String[] beanPaths = {SisStudent.REL_ENROLLMENTS + "." + StudentEnrollment.COL_ENROLLMENT_CODE, m_promotedInd};

        m_fieldToRefTable =
                new HashMap<String, Map<String, String>>((int) (beanPaths.length * HASHMAP_CAPACITY_MULTIPLIER));

        for (String beanPath : beanPaths) {
            m_fieldToRefTable.put(beanPath, getStateCodeReferenceMap(beanPath));
        }
    }

}