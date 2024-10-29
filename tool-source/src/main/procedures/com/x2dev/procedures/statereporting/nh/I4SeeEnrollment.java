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

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.*;
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
import com.x2dev.sis.model.beans.SchoolScheduleContext;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAttendance;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.business.EnrollmentManager;
import com.x2dev.sis.model.business.EnrollmentSnapshot;
import com.x2dev.utils.CollectionUtils;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.KeyValuePair;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
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
import org.apache.ojb.broker.query.Query;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * This class implements the data export for New Hampshire's i4see Enrollment export.
 *
 * @author X2 Development Corporation
 */
public class I4SeeEnrollment extends StateReportData {
    /**
     * Implementation of StateReportEntity to be used by the I4SeeEnrollment export.
     * This must be a public static inner class with a public no argument
     * constructor so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class I4SeeEntity extends StateReportEntity {
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
        List<KeyValuePair<StudentEnrollment, StudentEnrollment>> m_enrollments;

        /*
         * Enrollment record constants
         */
        private static final String ENROLLMENT_H1_CODE = "H1";
        private static final String ENROLLMENT_W2_CODE = "W2";
        private static final String ENROLLMENT_W4_CODE = "W4";

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

            FieldDefinition field = getData().getFieldDefinition(FIELD_300_DAYS_IN_ATTENDANCE);

            boolean requireMemberDay = ((Boolean) getData().getParameter(REQUIRE_MEMBER_DAY_PARAM)).booleanValue();

            /*
             * Get membership days parameter
             */
            double membershipCountAsDouble = 0;

            /*
             * Get membership count
             */
            I4SeeEnrollment i4seeData = (I4SeeEnrollment) getData();
            String membershipCount = i4seeData.getMembershipDays(this);

            if (membershipCount != null) {
                try {
                    membershipCountAsDouble = Double.parseDouble(membershipCount);
                } catch (NumberFormatException nfe) {
                    // invalid format, will be reported elsewhere.
                }
            }

            // check enrollment count and membership days parameter.
            if ((requireMemberDay && membershipCountAsDouble > 0) || !requireMemberDay) {
                // No filtering.
            } else {
                // Student filtered.
                error = new StateReportValidationError(this, field, "0 member days - excluded from export", "");
            }

            // if this is a W2 during the summer, don't report this entity
            if (error == null && getWithdrawal() != null &&
                    (getWithdrawal().getEnrollmentCode().equals(ENROLLMENT_W2_CODE) ||
                            getWithdrawal().getEnrollmentCode().equals(ENROLLMENT_W4_CODE))) {
                PlainDate startDate = (PlainDate) getData().getParameter(REPORT_DATE_PARAM);
                EnrollmentSnapshot snapshot = getSnapshot(startDate, field);
                PlainDate firstDay = ((I4SeeEnrollment) getData()).findFirstEnrollmentDay(snapshot);
                if (getWithdrawal().getEnrollmentDate().before(firstDay)) {
                    // Student filtered.
                    error = new StateReportValidationError(this, field, "summer withdrawal - excluded from export", "");
                }
            }

            return error;
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
         * Returns the entry enrollment record for the current student.
         *
         * return StudentEnrollment
         *
         * @return Student enrollment
         */
        public StudentEnrollment getEntry() {
            StudentEnrollment entry = null;
            int index = getCurrentRow();

            if (m_enrollments != null && index >= 0 && index < m_enrollments.size()) {
                KeyValuePair<StudentEnrollment, StudentEnrollment> pair = m_enrollments.get(index);
                entry = pair.getKey();
            }

            return entry;
        }

        /**
         * returns a boolean indicating if the student is a non-graduating senior.
         *
         * @return boolean
         */
        public boolean getIsNonGraduatingSenior() {
            String promotedInd = getFieldValue(FIELD_510_PROMOTED_IND);
            return ("12".equals(getFieldValue(FIELD_400_GRADE)) &&
                    ("1".equals(promotedInd) || "2".equals(promotedInd)));
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
         * Returns the withdrawal enrollment record for the current student.
         *
         * return StudentEnrollment
         *
         * @return Student enrollment
         */
        public StudentEnrollment getWithdrawal() {
            StudentEnrollment withdrawal = null;
            int index = getCurrentRow();

            if (m_enrollments != null && index >= 0 && index < m_enrollments.size()) {
                KeyValuePair<StudentEnrollment, StudentEnrollment> pair = m_enrollments.get(index);
                withdrawal = pair.getValue();
            }

            return withdrawal;
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

            SisStudent student = (SisStudent) bean;

            /*
             * A student can appear multiple times on the report if they withdrew and re-entered
             * the district
             */
            m_enrollments = new LinkedList<KeyValuePair<StudentEnrollment, StudentEnrollment>>();

            Collection<StudentEnrollment> enrollments = getEnrollments(student, false);

            StudentEnrollment entry = null;
            StudentEnrollment withdrawal = null;

            for (StudentEnrollment enrollment : enrollments) {
                if (enrollment.getEnrollmentCode() != null && enrollment.getEnrollmentCode().startsWith("X2")) {
                    continue;
                }
                if (StudentEnrollment.ENTRY.equals(enrollment.getEnrollmentType()) &&
                        entry == null) {
                    entry = enrollment;
                }

                if (StudentEnrollment.WITHDRAWAL.equals(enrollment.getEnrollmentType()) &&
                        withdrawal == null) {
                    withdrawal = enrollment;
                }

                /*
                 * Special case! For students with certain status changes, there need to be
                 * faux E and W records for the state report.
                 *
                 * A type=='S' with a Reason containing "W12R12", two rows of data need to
                 * be added to the report:
                 * "E1", "last E date", "W12", "date"
                 * "R12", "date", "", ""
                 *
                 * A type=='Y' with a Reason containing "W1R1", two rows of data need to
                 * be added to the report:
                 * "E1", "last E date", "W1", "date"
                 * "R1", "date", "", ""
                 *
                 * The "last E date" should be the 'startDate' or the student's last Entry date.
                 */
                if (StudentEnrollment.STATUS_CHANGE.equals(enrollment.getEnrollmentType()) ||
                        StudentEnrollment.YOG_CHANGE.equals(enrollment.getEnrollmentType())) {
                    String enrollmentCode = null;
                    String withdrawalCode = null;
                    String reentryCode = null;

                    if (!StringUtils.isEmpty(enrollment.getReasonCode())) {
                        if (enrollment.getReasonCode().startsWith(W1R1_KEY)) {
                            enrollmentCode = "E1";
                            withdrawalCode = "W1";
                            reentryCode = "R1";
                        } else if (enrollment.getReasonCode().startsWith(W12R12_KEY)) {
                            enrollmentCode = "E1";
                            withdrawalCode = "W12";
                            reentryCode = "R12";
                        }
                    }
                    /*
                     * if this status change is a special case (e.g. Reason contains W12R12 or W1R1)
                     * create the entry/withdrawal records needed
                     */
                    if (enrollmentCode != null) {
                        /*
                         * if the first record is a withdrawal, create an entry record
                         */
                        if (entry == null) {
                            entry = getMostRecentEnrollment();
                        }

                        /*
                         * use the current entry as the entry record, or create a dummy one
                         */
                        if (entry == null) {
                            entry = enrollment.clone();
                            // entry.setEnrollmentCode(enrollmentCode);
                            entry.setEnrollmentDate(data.getCurrentContext().getStartDate());
                            entry.setEnrollmentType(StudentEnrollment.ENTRY);
                        }
                        withdrawal = enrollment.clone();
                        withdrawal.setEnrollmentCode(withdrawalCode);
                        withdrawal.setEnrollmentType(StudentEnrollment.WITHDRAWAL);

                        addEnrollmentPair(entry, withdrawal);

                        /*
                         * set up the next entry record for the next pair (or final entry)
                         */
                        withdrawal = null;
                        entry = enrollment.clone();
                        entry.setEnrollmentCode(reentryCode);
                    }

                }

                /*
                 * Add the entry, withdrawal pair
                 */
                if (withdrawal != null) {
                    addEnrollmentPair(entry, withdrawal);

                    entry = null;
                    withdrawal = null;
                }
            }

            /*
             * If enrollments is empty, student may be a summer withdrawal
             */
            Boolean includeSummerWithdrawals = (Boolean) data.getParameter(INCLUDE_SUMMER_WITHDRAWALS_PARAM);

            if (enrollments.isEmpty() && includeSummerWithdrawals != null && includeSummerWithdrawals.booleanValue()) {
                enrollments = getEnrollments(student, true);
                for (StudentEnrollment enrollment : enrollments) {
                    if (StudentEnrollment.WITHDRAWAL.equals(enrollment.getEnrollmentType())) {
                        withdrawal = enrollment;
                    }
                }

                if (withdrawal != null) {
                    addEnrollmentPair(entry, withdrawal);
                }

                entry = null;
                withdrawal = null;
            }

            /*
             * This call covers the case if the student does not have any enrollment records or if
             * the student has a withdrawal followed by an entry
             */
            if (enrollments.isEmpty() ||
                    (!enrollments.isEmpty() && withdrawal == null && entry != null)) {
                addEnrollmentPair(entry, withdrawal);
            }

            if (m_enrollments.size() == 0 || m_enrollments.get(0).getKey() == null) {
                addMostRecentEnrollment(m_enrollments);
            }

            setRowCount(m_enrollments.size());
        }

        /**
         * If update calculated fields is set, save new values into the bean.
         *
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#postProcess()
         */
        @Override
        public void postProcess() {
            Boolean updateRecords = (Boolean) getData().getParameter(UPDATE_RECORDS_PARAM);

            /*
             * If the update flag is set, update calculated values into the student records.
             */
            if (updateRecords != null && updateRecords.booleanValue()) {
                try {
                    Converter integerConverter = ConverterFactory.getConverterForClass(Converter.INTEGER_CONVERTER,
                            Locale.getDefault(), true);

                    FieldDefinition field = getData().getFieldDefinition(FIELD_300_DAYS_IN_ATTENDANCE);
                    PropertyUtils.setProperty(getBean(), field.getBeanPath(),
                            integerConverter.stringToJava(getFieldValue(FIELD_300_DAYS_IN_ATTENDANCE)));

                    field = getData().getFieldDefinition(FIELD_310_DAYS_ABSENT);
                    PropertyUtils.setProperty(getBean(), field.getBeanPath(),
                            integerConverter.stringToJava(getFieldValue(FIELD_310_DAYS_ABSENT)));

                    field = getData().getFieldDefinition(FIELD_520_SUSPENSIONS_IN);
                    PropertyUtils.setProperty(getBean(), field.getBeanPath(),
                            integerConverter.stringToJava(getFieldValue(FIELD_520_SUSPENSIONS_IN)));

                    field = getData().getFieldDefinition(FIELD_530_SUSPENSIONS_OUT);
                    PropertyUtils.setProperty(getBean(), field.getBeanPath(),
                            integerConverter.stringToJava(getFieldValue(FIELD_530_SUSPENSIONS_OUT)));

                    field = getData().getFieldDefinition(FIELD_660_ORIGINAL_YOG);
                    PropertyUtils.setProperty(getBean(), field.getBeanPath(), getFieldValue(FIELD_660_ORIGINAL_YOG));

                    field = getData().getFieldDefinition(FIELD_420_RACE);
                    PropertyUtils.setProperty(getBean(), field.getBeanPath(), getFieldValue(FIELD_420_RACE));

                    if (getBean().isDirty()) {
                        getData().getBroker().saveBeanForced(getBean());
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
         * Adds an entry/withdrawal pair of StudentEnrollment records to the m_enrollments list.
         *
         * @param entry StudentEnrollment
         * @param withdrawal StudentEnrollment
         */
        private void addEnrollmentPair(StudentEnrollment entry, StudentEnrollment withdrawal) {
            KeyValuePair<StudentEnrollment, StudentEnrollment> enrollmentPair =
                    new KeyValuePair<StudentEnrollment, StudentEnrollment>(entry, withdrawal);

            m_enrollments.add(enrollmentPair);
        }

        /**
         * Adds the most recent enrollment prior to the start of the school year.
         *
         * @param enrollments List<KeyValuePair<StudentEnrollment,StudentEnrollment>>
         */
        private void addMostRecentEnrollment(List<KeyValuePair<StudentEnrollment, StudentEnrollment>> enrollments) {
            List<StudentEnrollment> orderedEnrollments = getOrderedEnrollment((SisStudent) getBean(), null,
                    DateUtils.add(getData().getCurrentContext().getStartDate(), -1), null, false);
            Object value = null;
            while (orderedEnrollments.size() > 0) {
                if ((orderedEnrollments.get(0).getEnrollmentType().equals(StudentEnrollment.ENTRY)
                        && (orderedEnrollments.get(0).getEnrollmentCode() == null
                                || !orderedEnrollments.get(0).getEnrollmentCode().startsWith("X2")))
                        || (orderedEnrollments.get(0).getEnrollmentType().equals(StudentEnrollment.STATUS_CHANGE)
                                && (orderedEnrollments.get(0).getEnrollmentCode() == null
                                        || !orderedEnrollments.get(0).getEnrollmentCode().startsWith("W1")))) {
                    if (enrollments.size() > 0) {
                        value = enrollments.get(0).getValue();
                        enrollments.remove(0);
                    }
                    enrollments.add(0, new KeyValuePair(orderedEnrollments.get(0), value));
                    break;
                }
                orderedEnrollments.remove(0);
            }
        }

        /**
         * Returns the list of ENTRY, STATUS CHANGE and WITHDRAWAL enrollment beans, ordered by date
         * and timestamp, for
         * the given student from the start date to the end date, inclusive. If either the start or
         * end
         * date parameters is null then there will be no lower or upper date boundary, respectively.
         * If
         * the school parameter is not null then the results will be filtered by that school.
         *
         * @param student the student whose enrollment records will be queried
         * @param startDate if not null then all records returned will be on or after this date
         * @param endDate if not null then all records returned will be on or before this date
         * @param school if not null then all records returned will be for this school
         * @param ascending if true then the beans will be listed in ascending order (oldest to
         *        newest),
         *        otherwise the beans will be reverse ordered (newest to oldest)
         *
         * @return A List of StudentEnrollment beans, this list may be empty if no enrollment
         *         records
         *         meet the specified criteria
         */
        private List getOrderedEnrollment(Student student,
                                          PlainDate startDate,
                                          PlainDate endDate,
                                          School school,
                                          boolean ascending) {
            ArrayList typeCodes = new ArrayList(2);
            typeCodes.add(StudentEnrollment.ENTRY);
            typeCodes.add(StudentEnrollment.WITHDRAWAL);
            typeCodes.add(StudentEnrollment.STATUS_CHANGE);

            Criteria criteria = new Criteria();
            criteria.addEqualTo(StudentEnrollment.COL_STUDENT_OID, student.getOid());
            criteria.addIn(StudentEnrollment.COL_ENROLLMENT_TYPE, typeCodes);

            if (startDate != null) {
                criteria.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, startDate);
            }

            if (endDate != null) {
                criteria.addLessOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, endDate);
            }

            if (school != null) {
                criteria.addEqualTo(StudentEnrollment.COL_SCHOOL_OID, school.getOid());
            }

            QueryByCriteria query = new QueryByCriteria(StudentEnrollment.class, criteria);
            query.addOrderBy(StudentEnrollment.COL_ENROLLMENT_DATE, ascending);
            query.addOrderBy(StudentEnrollment.COL_TIMESTAMP, ascending);

            ArrayList enrollments = new ArrayList();
            enrollments.addAll(getData().getBroker().getCollectionByQuery(query));

            return enrollments;
        }

        /**
         * Gets the most recent enrollment prior to the start of the school year.
         *
         * @return Student enrollment
         */
        private StudentEnrollment getMostRecentEnrollment() {
            StudentEnrollment entry = null;
            List<StudentEnrollment> orderedEnrollments = getOrderedEnrollment((SisStudent) getBean(), null,
                    DateUtils.add(getData().getCurrentContext().getStartDate(), -1), null, false);
            while (orderedEnrollments.size() > 0) {
                if ((orderedEnrollments.get(0).getEnrollmentType().equals(StudentEnrollment.ENTRY)
                        && (orderedEnrollments.get(0).getEnrollmentCode() == null
                                || !orderedEnrollments.get(0).getEnrollmentCode().startsWith("X2")))
                        || (orderedEnrollments.get(0).getEnrollmentType().equals(StudentEnrollment.STATUS_CHANGE)
                                && (orderedEnrollments.get(0).getEnrollmentCode() == null
                                        || !orderedEnrollments.get(0).getEnrollmentCode().startsWith("W1")))) {
                    entry = orderedEnrollments.get(0);
                    break;
                }
                orderedEnrollments.remove(0);
            }
            return entry;
        }

        /**
         * Returns a Collection of Student Enrollment beans for the passed student since the first
         * day
         * of the school year.
         *
         * @param student SisStudent
         * @param summerDates - true if collecting summer information
         *        NOT USED for B-O-Y report.
         * @return Collection of StudentEnrollment records for any date on or after the start of
         *         school
         */
        private Collection<StudentEnrollment> getEnrollments(SisStudent student, boolean summerDates) {
            Collection<StudentEnrollment> enrollments = new LinkedList<StudentEnrollment>();

            PlainDate startDate = null;
            PlainDate endDate = null;

            /*
             * B-O-Y report includes all dates from start of summer up to the report date
             */

            if (REPORT_TYPE_BOY.equals(((I4SeeEnrollment) getData()).m_reportType)) {
                startDate = (PlainDate) getData().getParameter(SUMMER_START_DATE_PARAM);
                endDate = (PlainDate) getData().getParameter(REPORT_DATE_PARAM);
            } else if (summerDates) {
                startDate = (PlainDate) getData().getParameter(SUMMER_START_DATE_PARAM);
                endDate = (PlainDate) getData().getParameter(SUMMER_END_DATE_PARAM);
            } else {
                startDate = ((I4SeeEnrollment) getData()).m_firstDayDate;
                endDate = ((I4SeeEnrollment) getData()).m_reportDate;
            }

            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(StudentEnrollment.COL_STUDENT_OID, student.getOid());
            criteria.addNotEqualTo(StudentEnrollment.COL_ENROLLMENT_CODE, ENROLLMENT_H1_CODE);
            criteria.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, startDate);
            criteria.addLessOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, endDate);

            /*
             * Only include schools that are not CATE (Career And Technical Education)
             */

            String javaName = getData().translateAliasToJavaName(ALIAS_SKL_CATE_INDICATOR, false);
            if (!StringUtils.isEmpty(javaName)) {
                criteria.addNotEqualTo(StudentEnrollment.REL_SCHOOL + "." + javaName, Boolean.valueOf(true));
            }

            QueryByCriteria query = new QueryByCriteria(StudentEnrollment.class, criteria);
            query.addOrderByAscending(StudentEnrollment.COL_ENROLLMENT_DATE);
            query.addOrderByAscending(StudentEnrollment.COL_TIMESTAMP);

            enrollments = getData().getBroker().getCollectionByQuery(query);

            if (!summerDates) {
                /*
                 * Go through records and remove any that should not be reported
                 *
                 * If a student has and E1 and a W2 on 8/26/08 along with an R2 at another
                 * school on 8/26/08, then the E1 and W2 should be ignored and only the
                 * active R2 (which will have to be corrected to an E1) should be reported
                 *
                 * If a student has an E1 and a W-anything on 8/26/08, and is NOT active at
                 * any other school, then only the withdrawal should be reported ? not the
                 * entry code.
                 *
                 * Translation: Do not report on any E records on the first date. A lone withdrawal
                 * record will generate a E record in the export for the first date. If a W is
                 * encountered, only report it if it is not followed by an E record on the same
                 * date.
                 */
                Collection<StudentEnrollment> recordsToRemove = new LinkedList<StudentEnrollment>();
                StudentEnrollment lastEnrollment = null;

                for (StudentEnrollment enrollment : enrollments) {
                    if (enrollment.getEnrollmentDate().equals(startDate)) {
                        if (StudentEnrollment.WITHDRAWAL.equals(enrollment.getEnrollmentType())) {
                            lastEnrollment = enrollment;
                        } else {
                            // Check to see if the last record was an enrollment
                            if (lastEnrollment != null &&
                                    StudentEnrollment.WITHDRAWAL.equals(lastEnrollment.getEnrollmentType())) {
                                // Ignore the last withdrawal
                                recordsToRemove.add(lastEnrollment);
                                lastEnrollment = null;
                            }
                        }
                    }
                }

                enrollments.removeAll(recordsToRemove);
            }

            return enrollments;
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
                    addRetrievalError(field.getFieldId(),
                            new StateReportValidationError(this, field,
                                    "WARNING: Enrollment information (enrollment status, school, and/or YOG) is not precise",
                                    ""));
                }
            }

            return snapshot;
        }
    }

    /**
     * Returns the sau or district number for the given student.
     * <p>
     * For non-archived students we use the snapshot. For archived students (i.e., summer
     * withdrawals) we use the school from the most recent enrollment record during summer vacation
     * (if we didn't do this then the export would show the history school's district SAU number).
     */
    protected class Retrieve030_040_Sau_DistrictNumber implements FieldRetriever {

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
            String javaName = field.getBeanPath().substring(field.getBeanPath().lastIndexOf('.') + 1);
            SisStudent student = (SisStudent) entity.getBean();
            String number = null;

            if (m_schoolMap.get(student.getSchoolOid()).getArchiveIndicator()) {
                List enrollments = m_enrollmentManager.getOrderedEnrollment(student,
                        (PlainDate) getParameter(SUMMER_START_DATE_PARAM),
                        (PlainDate) getParameter(SUMMER_END_DATE_PARAM),
                        null,
                        false);

                if (!enrollments.isEmpty()) {
                    StudentEnrollment withdrawal = (StudentEnrollment) enrollments.get(0);
                    number = (String) WebUtils.getProperty(withdrawal.getSchool().getOrganization1(), javaName);
                }
            } else {
                number = (String) WebUtils.getProperty(getOrganization(), javaName);
            }

            return number;
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
            String javaName = field.getBeanPath().substring(field.getBeanPath().lastIndexOf('.') + 1);
            SisStudent student = (SisStudent) entity.getBean();
            String schoolNumber = null;

            String adjustedSchoolNumber = (String) WebUtils.getProperty(student, m_adjustedSchoolCode);
            if (StringUtils.isEmpty(adjustedSchoolNumber)) {
                if (m_schoolMap.get(student.getSchoolOid()).getArchiveIndicator()) {
                    List enrollments = m_enrollmentManager.getOrderedEnrollment(student,
                            (PlainDate) getParameter(SUMMER_START_DATE_PARAM),
                            (PlainDate) getParameter(SUMMER_END_DATE_PARAM),
                            null,
                            false);
                    if (!enrollments.isEmpty()) {
                        StudentEnrollment withdrawal = (StudentEnrollment) enrollments.get(0);
                        schoolNumber = (String) WebUtils.getProperty(withdrawal.getSchool(), javaName);
                    }
                } else {
                    StudentEnrollment entry = ((I4SeeEntity) entity).getEntry();
                    if (entry != null) {
                        SisSchool school = m_schoolMap.get(entry.getSchoolOid());
                        if (school == null) {
                            school = m_schoolMap.get(student.getSchoolOid());
                        }
                        schoolNumber = (String) WebUtils.getProperty(school, javaName);
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
            String enrollmentStatus = field.getDefaultValue();

            StudentEnrollment entry = ((I4SeeEntity) entity).getEntry();
            StudentEnrollment withdrawal = ((I4SeeEntity) entity).getWithdrawal();
            StudentEnrollment enrollment = entry;

            /*
             * Check to see if the enrollment status should be taken from the entry record or
             * the withdrawal record (which ever is the latest). W12 and W1 withdrawals
             * cause us to use the entry's enrollment status, not the withdrawals
             */
            if (entry != null && withdrawal != null
                    && withdrawal.getEnrollmentDate().after(entry.getEnrollmentDate())
                    && !withdrawal.getEnrollmentCode().equals("W12")
                    && !withdrawal.getEnrollmentCode().equals("W1")) {
                enrollment = withdrawal;
            }

            if (enrollment != null) {
                enrollmentStatus = (String) WebUtils.getProperty(enrollment, m_enr210Field);
            }

            /*
             * If the enrollmentStatus is empty, see if the status is present on the paired
             * enrollment record
             */
            if (StringUtils.isEmpty(enrollmentStatus)) {
                /*
                 * status empty, try the 'other' paired record
                 */
                if (enrollment == withdrawal && entry != null) {
                    enrollmentStatus = (String) WebUtils.getProperty(entry, m_enr210Field);
                } else if (withdrawal != null) {
                    enrollmentStatus = (String) WebUtils.getProperty(withdrawal, m_enr210Field);
                }
            }

            return enrollmentStatus;
        }
    }
    /**
     * Returns the town responsible for a given student, this is dependent on their enrollment at
     * the time.
     */
    protected class Retrieve220TownResponsible implements FieldRetriever {
        protected static final String DEFAULT_I4SEE_220_PARAM = "defaultI4see220";


        private String m_default;

        /**
         * Instantiates a new retrieve 220 town responsible.
         */
        public Retrieve220TownResponsible() {
            super();
            m_default = (String) getParameter(DEFAULT_I4SEE_220_PARAM);
        }

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
            String townResponsible = null;
            StudentEnrollment enrollment = ((I4SeeEntity) entity).getEntry() != null ? ((I4SeeEntity) entity).getEntry()
                    : ((I4SeeEntity) entity).getWithdrawal();
            if (enrollment != null) {
                townResponsible = (String) getProperty(enrollment, m_enr220Field);
                if (townResponsible != null) {
                    townResponsible = townResponsible.trim();
                }
            }
            if (StringUtils.isEmpty(townResponsible)) {
                townResponsible = StringUtils.isEmpty(m_default) ? "" : m_default;
            }

            return townResponsible;
        }
    }
    /**
     * Returns the district responsible for a given student, this is dependent on their enrollment
     * at the time.
     */
    protected class Retrieve225DistrictResponsible implements FieldRetriever {
        protected static final String DEFAULT_I4SEE_225_PARAM = "defaultI4see225";

        private String m_default;

        /**
         * Instantiates a new retrieve 225 district responsible.
         */
        public Retrieve225DistrictResponsible() {
            super();
            m_default = (String) getParameter(DEFAULT_I4SEE_225_PARAM);
        }

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
            String districtResponsible = null;
            StudentEnrollment enrollment = ((I4SeeEntity) entity).getEntry() != null ? ((I4SeeEntity) entity).getEntry()
                    : ((I4SeeEntity) entity).getWithdrawal();
            if (enrollment != null) {
                districtResponsible = (String) getProperty(enrollment, m_enr225Field);
                if (districtResponsible != null) {
                    districtResponsible = districtResponsible.trim();
                }
            }
            if (StringUtils.isEmpty(districtResponsible)) {
                districtResponsible = StringUtils.isEmpty(m_default) ? "" : m_default;
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
            PlainDate entryDate = null;

            EnrollmentSnapshot snapshot = ((I4SeeEntity) entity).getSnapshot(m_reportDate, field);

            PlainDate firstDay = findFirstEnrollmentDay(snapshot);

            StudentEnrollment entry = ((I4SeeEntity) entity).getEntry();
            StudentEnrollment withdrawal = ((I4SeeEntity) entity).getWithdrawal();

            if (entry != null) {
                entryDate = entry.getEnrollmentDate();
                if (entryDate != null && firstDay != null && entryDate.before(firstDay)) {
                    entryDate = firstDay;
                }
            } else {
                entryDate = firstDay;
            }

            /*
             * if the student has withdrawn, return blank. The hack here is for students with
             * an enrollment and withdrawal record on the same day. We use the timestamp to
             * figure out the order in which they happened. This is fine unless someone edits
             * the records, modifying the timestamp.
             */
            if (withdrawal != null && entry != null && entryDate.before(entry.getEnrollmentDate()) &&
                    (withdrawal.getEnrollmentDate().after(entry.getEnrollmentDate()) ||
                            (withdrawal.getEnrollmentDate().equals(entry.getEnrollmentDate()) && // hack
                                    withdrawal.getTimestamp() > entry.getTimestamp()))) {
                entryDate = null;
            }

            return entryDate;
        }

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
            String entryCode = null;

            StudentEnrollment entry = ((I4SeeEntity) entity).getEntry();

            if (entry != null) {
                entryCode = lookupStateValue(StudentEnrollment.class, StudentEnrollment.COL_ENROLLMENT_CODE,
                        entry.getEnrollmentCode());

                /*
                 * If an entry code is before the start of the school year
                 * then it needs to be reported as "E1"
                 */
                if (!StringUtils.isEmpty(entryCode)
                        // && entryCode.charAt(0) == 'R'
                        && entry.getEnrollmentDate().before(m_firstDayDate)) {
                    entryCode = "E1"; // can't use entry.getDefaultValue() because blanks are
                                      // allowed
                }
            }

            /*
             * if the entry date is blank, so is the code
             */
            String entryDate = entity.getFieldValue(FIELD_230_ENTRY_DATE);
            if (StringUtils.isEmpty(entryDate)) {
                entryCode = null;
            } else {
                if (entryCode == null) {
                    entryCode = "E1";
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
            PlainDate exitDate = null;
            Date entryDate = null;

            I4SeeEntity i4See = (I4SeeEntity) entity;
            StudentEnrollment withdrawal = i4See.getWithdrawal();
            StudentEnrollment entry = i4See.getEntry();
            int index = entity.getCurrentRow() + 1;
            StudentEnrollment nextEntry = null;
            if (i4See.m_enrollments != null && index < i4See.m_enrollments.size()) {
                KeyValuePair<StudentEnrollment, StudentEnrollment> pair = i4See.m_enrollments.get(index);
                nextEntry = pair.getKey();
            }

            if (withdrawal != null) {
                exitDate = withdrawal.getEnrollmentDate();
                try {
                    entryDate = m_dateFormat.parse(entity.getFieldValue(FIELD_230_ENTRY_DATE));

                    if (!m_enrollmentManager.getWithdrawalIsMemberDay() ||
                            (nextEntry != null
                                    && nextEntry.getEnrollmentDate().compareTo(withdrawal.getEnrollmentDate()) == 0
                                    && m_enrollmentManager.getWithdrawalIsMemberDay()
                                    && m_enrollmentManager.getEntryIsMemberDay())) {
                        /*
                         * The exit date must be equal to or greater than the entry date on this
                         * line,
                         * so don't subtract a day if this line's entry exists and is the same day
                         * as exit
                         */
                        if (entry == null
                                || (entry != null && entryDate.compareTo(withdrawal.getEnrollmentDate()) < 0)) {
                            exitDate = DateUtils.add(exitDate, -1);
                        }
                    }
                } catch (ParseException e) {
                    // Handled by date formatter
                }
            }

            return exitDate;
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
            String exitCode = null;

            StudentEnrollment withdrawal = ((I4SeeEntity) entity).getWithdrawal();

            if (withdrawal != null) {
                exitCode = lookupStateValue(StudentEnrollment.class, StudentEnrollment.COL_ENROLLMENT_CODE,
                        withdrawal.getEnrollmentCode());
            }

            return exitCode;
        }
    }
    /**
     * Retrieve days in attendance for the student. If the count is zero, use the 555 placeholder.
     *
     * @author X2 Development Corporation
     */
    protected class Retrieve300DaysInAttendance implements FieldRetriever {

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

            String daysAbsent = entity.getFieldValue(FIELD_310_DAYS_ABSENT);

            int membershipCountAsInt = 0;

            if (membershipCount != null) {
                try {
                    membershipCountAsInt = Integer.parseInt(membershipCount);
                } catch (NumberFormatException nfe) {
                    // Do nothing, this error has already been logged.
                }
            }


            if (membershipCountAsInt != 0) {
                membershipCountAsInt = membershipCountAsInt * 2;
                /*
                 * Check student's Full Day Percent value (i4see 405). If the value is
                 * not empty and less than 50, consider these as half-days
                 */
                String fullDayPercent = entity.getFieldValue(FIELD_405_FULL_DAY_PERCENT);
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
            String absentCount = null;

            int absenses = 0;
            SisStudent student = (SisStudent) entity.getBean();

            if (m_calculateTotals) {
                if (entity.getRowCount() == 1) {
                    Integer absenceCount = m_absences.get(student.getOid());
                    if (absenceCount != null) {
                        absenses = absenceCount.intValue();
                    }
                } else {
                    try {
                        StudentEnrollment entry = ((I4SeeEntity) entity).getEntry();
                        StudentEnrollment withdrawal = ((I4SeeEntity) entity).getWithdrawal();

                        PlainDate startDate = m_scheduleMap.get(student.getSchoolOid()).getStartDate();
                        PlainDate endDate = m_reportDate;

                        if (entry != null && entry.getEnrollmentDate().after(startDate)) {
                            startDate = entry.getEnrollmentDate();
                        }

                        if (withdrawal != null) {
                            endDate = withdrawal.getEnrollmentDate();
                            if (!m_enrollmentManager.getWithdrawalIsMemberDay() && (entry == null || (entry != null
                                    && entry.getEnrollmentDate().compareTo(withdrawal.getEnrollmentDate()) < 0))) {
                                endDate = DateUtils.add(endDate, -1);
                            }
                        }

                        Collection<StudentAttendance> allStdAtt = m_absencesMap.get(student.getOid());

                        if (allStdAtt != null && !allStdAtt.isEmpty()) {
                            for (StudentAttendance att : allStdAtt) {
                                if ((startDate.before(att.getDate()) || startDate.equals(att.getDate())) &&
                                        (endDate.after(att.getDate()) || endDate.equals(att.getDate()))) {
                                    absenses += 1;
                                }
                            }
                        }
                    } catch (Exception e) {
                        entity.addRetrievalError(field.getFieldId(),
                                new StateReportValidationError(entity,
                                        field,
                                        "ERROR",
                                        "Could not calculate absences"));
                    }
                }

                absenses = absenses * 2;

                /*
                 * Check student's Full Day Percent value (i4see 405). If the value is
                 * not empty and less than 50, consider these as half-days
                 */
                String fullDayPercent = entity.getFieldValue(FIELD_405_FULL_DAY_PERCENT);
                if (StringUtils.isNumeric(fullDayPercent)) {
                    double fdPercentValue = (new BigDecimal(fullDayPercent)).doubleValue();
                    if (fdPercentValue <= 50) {
                        absenses = absenses / 2;
                    }
                }

                absentCount = String.valueOf(absenses);
            } else {
                absentCount = m_integerConverter.javaToString(WebUtils.getProperty(student, m_std310Field));
            }

            if (StringUtils.isEmpty(absentCount)) {
                entity.addRetrievalError(field.getFieldId(),
                        new StateReportValidationError(entity,
                                field,
                                "ERROR",
                                VALIDATION_MISSING_VALUE));
            }

            /*
             * Save the unmapped value in the entity so it can be written back to the student
             * record in postProcess.
             */
            ((I4SeeEntity) entity).setUpdateValue(field.getFieldId(), absentCount);

            return absentCount;
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
            String fullDayPercent = (String) WebUtils.getProperty(entity.getBean(), m_std405Field);
            int fullDay = 0;
            String fullDayString = "";

            // field not used for B-O-Y report
            if (REPORT_TYPE_EOY.equals(m_reportType) &&
                    fullDayPercent != null && StringUtils.isNumeric(fullDayPercent)) {
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
                        SisPerson person =
                                (SisPerson) getBroker().getBeanByOid(SisPerson.class, student.getPersonOid());
                        if (person != null) {
                            if (person.getHispanicLatinoIndicator()) {
                                localRaceCode |= HISPANIC_RACE_IND;
                            }
                        }

                        if (localRaceCode >= 0 && localRaceCode <= NH_STATE_RACE_CODES.length) {
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
            String promotedInd = null;
            String withdrawalCode = null;
            I4SeeEntity i4see = ((I4SeeEntity) entity);
            StudentEnrollment withdrawal = i4see.getWithdrawal();
            String stateCode = "";
            if (withdrawal != null) {
                withdrawalCode = withdrawal.getEnrollmentCode();
            }
            if (entity.getCurrentRow() == entity.getRowCount() - 1
                    && (withdrawalCode == null
                            || EARLY_GRADUATION_CODE.equals(withdrawalCode))) {
                promotedInd = (String) WebUtils.getProperty(entity.getBean(), m_std510Field);
                if (!StringUtils.isEmpty(promotedInd)) {
                    promotedInd = lookupStateValue(SisStudent.class, m_std510Field, promotedInd);
                }

                /*
                 * in the BOY report, only 3's get reported
                 */
                if (!StringUtils.isEmpty(promotedInd) &&
                        !stateCode.startsWith("3") &&
                        REPORT_TYPE_BOY.equals(m_reportType)) {
                    promotedInd = "";
                }
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
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String count = null;

            /*
             * Days Suspended not used in B-O-Y report
             */
            if (REPORT_TYPE_EOY.equals(m_reportType)) {
                SisStudent student = (SisStudent) entity.getBean();
                StudentEnrollment entry = ((I4SeeEntity) entity).getEntry();
                StudentEnrollment withdrawal = ((I4SeeEntity) entity).getWithdrawal();

                if (m_calculateTotals) {
                    boolean isInSusp = "IN SUSP".equals(field.getParameter());
                    count = getConductActionTotal(student, entry, withdrawal, isInSusp);
                } else {
                    count = m_integerConverter.javaToString(WebUtils.getProperty(student, field.getBeanPath()));
                }

                /*
                 * Save the unmapped value in the entity so it can be written back to the student
                 * record in postProcess.
                 */
                ((I4SeeEntity) entity).setUpdateValue(field.getFieldId(), count);
            }

            return count;
        }

        /**
         * Returns the total number of conduct actions for the current year (up to the report date)
         * for the given action codes.
         *
         * @param student SisStudent
         * @param entry StudentEnrollment
         * @param withdrawal StudentEnrollment
         * @param isInSusp boolean
         * @return String
         * @throws X2BaseException exception
         */
        private String getConductActionTotal(SisStudent student,
                                             StudentEnrollment entry,
                                             StudentEnrollment withdrawal,
                                             boolean isInSusp)
                throws X2BaseException {
            int intCount = 0;

            Collection<ConductAction> acts = isInSusp ? m_actInSuspMapByStd.get(student.getOid())
                    : m_actOutSuspMapByStd.get(student.getOid());
            /*
             * Don't bother logging an error if the school doesn't have an active schedule. This
             * will be reported elsewhere. (If we did log it here then we would flood the log with
             * messages since this error would be reported twice for every student - once for ISS,
             * and again for OSS.)
             */
            if (acts != null && m_scheduleMap.get(student.getSchoolOid()) != null) {

                PlainDate startDate = m_firstDayDate;
                PlainDate endDate = m_reportDate;
                if (entry != null) {
                    startDate =
                            isInCtxDateRange(entry.getEnrollmentDate()) ? entry.getEnrollmentDate() : m_firstDayDate;
                }

                if (withdrawal != null) {
                    endDate = isInCtxDateRange(withdrawal.getEnrollmentDate()) ? withdrawal.getEnrollmentDate()
                            : m_lastDayDate;
                }

                for (ConductAction act : acts) {
                    if ((startDate.before(act.getActionStartDate()) || startDate.equals(act.getActionStartDate())) &&
                            (endDate.after(act.getActionStartDate()) || endDate.equals(act.getActionStartDate()))) {
                        BigDecimal bdCount = ((BigDecimal) WebUtils.getProperty(act,
                                translateAliasToJavaName("i4see Susp Length",
                                        true)));
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
            String postGradPlans = null;
            String withdrawalCode = null;
            I4SeeEntity i4see = ((I4SeeEntity) entity);
            StudentEnrollment withdrawal = i4see.getWithdrawal();
            if (withdrawal != null) {
                withdrawalCode = withdrawal.getEnrollmentCode();
            }
            if (entity.getCurrentRow() == entity.getRowCount() - 1
                    && (withdrawalCode == null
                            || EARLY_GRADUATION_CODE.equals(withdrawalCode))
                    && !i4see.getIsNonGraduatingSenior()) {
                postGradPlans = (String) WebUtils.getProperty(entity.getBean(), m_std610Field);
                if (!StringUtils.isEmpty(postGradPlans)) {
                    postGradPlans = lookupStateValue(SisStudent.class, m_std610Field, postGradPlans);
                }
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
            String diplomaType = null;
            String withdrawalCode = null;
            I4SeeEntity i4see = ((I4SeeEntity) entity);
            StudentEnrollment withdrawal = i4see.getWithdrawal();
            if (withdrawal != null) {
                withdrawalCode = withdrawal.getEnrollmentCode();
            }
            if (entity.getCurrentRow() == entity.getRowCount() - 1
                    && (withdrawalCode == null
                            || EARLY_GRADUATION_CODE.equals(withdrawalCode))
                    && !i4see.getIsNonGraduatingSenior()) {
                diplomaType = (String) WebUtils.getProperty(entity.getBean(), m_std620Field);
                if (!StringUtils.isEmpty(diplomaType)) {
                    diplomaType = lookupStateValue(SisStudent.class, m_std620Field, diplomaType);
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
            if ("1".equals(value) || "2".equals(value)) {
                String exitDate = entity.getFieldValue(FIELD_250_EXIT_DATE);
                try {
                    if (!StringUtils.isEmpty(exitDate) && m_dateFormat.parse(exitDate).before(m_firstDayDate)) {
                        errors.add(new StateReportValidationError(entity, field,
                                "I4SEE 210 Enrollment Status " + value + " not valid for exit date "
                                        + exitDate,
                                "I4SEE 210 = " + value));
                    }
                } catch (Exception e) {
                    // This should be handled by the date formatter already.
                }
            }
            if ("2".equals(value)) {
                String distResp = entity.getFieldValue(FIELD_225_RESPONSIBLE_DISTR);
                if ("930".equals(distResp)) {
                    errors.add(new StateReportValidationError(entity, field,
                            "I4SEE 210 Enrollment Status " + value + " not valid for District Responsible "
                                    + distResp,
                            "I4SEE 225 = " + value));
                }
            }
            if ("5".equals(value)) {
                String schoolNo = entity.getFieldValue(FIELD_050_SCHOOL_NUMBER);
                if (StringUtils.isEmpty(schoolNo) || !(schoolNo.equals("15010") || schoolNo.equals("15020") ||
                        schoolNo.equals("15030") || schoolNo.equals("15040"))) {
                    errors.add(new StateReportValidationError(entity, field,
                            "I4SEE 210 Enrollment Status " + value + " not valid for School Number "
                                    + schoolNo,
                            "I4SEE 050 = " + value));
                }
            }
            if ("7".equals(value)) {
                String exitDate = entity.getFieldValue(FIELD_250_EXIT_DATE);
                try {
                    if (StringUtils.isEmpty(exitDate) || !m_dateFormat.parse(exitDate).before(m_firstDayDate)) {
                        errors.add(new StateReportValidationError(entity, field,
                                "I4SEE 210 Enrollment Status " + value + " not valid for exit date "
                                        + exitDate,
                                "I4SEE 210 = " + value));
                    }
                } catch (Exception e) {
                    // This should be handled by the date formatter already.
                }
            }
            if ("8".equals(value)) {
                String gradeLvl = entity.getFieldValue(FIELD_400_GRADE);
                if (StringUtils.isEmpty(gradeLvl) || !(gradeLvl.equals("9") || gradeLvl.equals("10") ||
                        gradeLvl.equals("11") || gradeLvl.equals("12"))) {
                    errors.add(new StateReportValidationError(entity, field,
                            "I4SEE 210 Enrollment Status " + value + " not valid for Grade Level "
                                    + gradeLvl,
                            "I4SEE 050 = " + value));
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
            String entryDate = entity.getFieldValue(FIELD_230_ENTRY_DATE);

            if ((!StringUtils.isEmpty(entryDate) && StringUtils.isEmpty(value)) ||
                    (StringUtils.isEmpty(entryDate) && !StringUtils.isEmpty(value))) {
                errors.add(new StateReportValidationError(entity, field,
                        "I4SEE 240 Entry Code and I4See 230 Entry Date must either both be empty, or both be filled in",
                        "I4SEE 230 = " + entryDate + ", I4SEE 240 = " + value));
            }

            try {
                SisStudent student = (SisStudent) entity.getBean();
                Date entDate = m_dateFormat.parse(entryDate);
                Date startDate = m_scheduleMap.get(student.getSchoolOid()).getStartDate();
                if ((entity.getRowCount() == 1 && startDate.compareTo(entDate) == 0 && !"E1".equals(value))
                        || ("E2".equals(value) && entDate.before(startDate))) {
                    errors.add(new StateReportValidationError(entity, field,
                            "I4SEE 240 Entry Code " + value + " not valid for Entry Date "
                                    + entryDate,
                            "I4SEE 240 = " + value));
                }
            } catch (Exception e) {
                // This should be handled by the date formatter already.
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
            String exitDate = entity.getFieldValue(FIELD_250_EXIT_DATE);
            if ((!StringUtils.isEmpty(exitDate) && StringUtils.isEmpty(value)) ||
                    (StringUtils.isEmpty(exitDate) && !StringUtils.isEmpty(value))) {
                errors.add(new StateReportValidationError(entity, field,
                        "I4SEE 260 Exit Code and I4See 250 Exit Date must either both be empty, or both be filled in",
                        "I4SEE 250 = " + exitDate + ", I4SEE 260 = " + value));
            }

            if ("W11".equals(value)) {
                String promotedInd = entity.getFieldValue(FIELD_510_PROMOTED_IND);
                if (!"3".equals(promotedInd)) {
                    errors.add(new StateReportValidationError(entity, field,
                            "I4SEE 260 Exit Code " + value + " not valid for Promoted Indicator "
                                    + promotedInd,
                            "I4SEE 260 = " + value));
                }
                String gradeLvl = entity.getFieldValue(FIELD_400_GRADE);
                if (!("11".equals(gradeLvl) || "12".equals(gradeLvl))) {
                    errors.add(new StateReportValidationError(entity, field,
                            "I4SEE 260 Exit Code " + value + " not valid for Grade Level "
                                    + gradeLvl,
                            "I4SEE 260 = " + value));
                }
            }

            if (value != null && value.matches("W2[0123456789]")) {
                String gradeLvl = entity.getFieldValue(FIELD_400_GRADE);
                if (!("9".equals(gradeLvl) || "10".equals(gradeLvl) ||
                        "11".equals(gradeLvl) || "12".equals(gradeLvl))) {
                    errors.add(new StateReportValidationError(entity, field,
                            "I4SEE 260 Exit Code " + value + " not valid for Grade Level "
                                    + gradeLvl,
                            "I4SEE 260 = " + value));
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
            String raceCode = entity.getFieldValue(FIELD_420_RACE);
            if ("9".equals(raceCode)) {
                errors.add(new StateReportValidationError(entity, field,
                        "I4SEE 420 Racial and Ethnic code 09 is no longer used.",
                        "I4SEE 420 = " + value));
            }
            return errors;
        }
    }

    /**
     * Validates post graduation plans.
     */
    protected class Validate610PostGraduatePlans implements FieldValidator {

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
            String promotedInd = entity.getFieldValue(FIELD_510_PROMOTED_IND);
            if ("3".equals(promotedInd) && StringUtils.isEmpty(value)) {
                errors.add(new StateReportValidationError(entity, field,
                        "I4SEE 610 Post Graduation Plans cannot be blank if I4See 510 Promoted indicator = 3",
                        "I4SEE 610 = " + value));
            }
            return errors;
        }
    }
    /**
     * Validate diploma type.
     */
    protected class Validate620DiplomaType implements FieldValidator {

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
            if (!StringUtils.isEmpty(value)) {
                String promotedInd = entity.getFieldValue(FIELD_510_PROMOTED_IND);
                if (!"3".equals(promotedInd)) {
                    errors.add(new StateReportValidationError(entity, field,
                            "I4SEE 620 Diploma Type " + value + " not valid for Promoted Indicator "
                                    + promotedInd,
                            "I4SEE 620 = " + value));
                }
            }
            return errors;
        }
    }


    /**
     * The Class Validate660OriginalGraduationYear.
     */
    protected class Validate660OriginalGraduationYear implements FieldValidator {

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
            String grade = entity.getFieldValue("Grade");
            int intGrade = 0;
            try {
                intGrade = Integer.parseInt(grade);
            } catch (NumberFormatException e) {
                // intGrade remains 0
            }
            if (intGrade > 8 && intGrade < 13) {
                int currentYear = data.getCurrentContext().getSchoolYear();
                int year = 0;
                try {
                    year = Integer.parseInt(value);
                } catch (NumberFormatException e) {
                    // year remains 0
                }
                if (year < currentYear - 6 || year > currentYear + 3) {
                    errors.add(new StateReportValidationError(entity, field,
                            "I4SEE 660 Original Graduation Year must be in the range from " + (currentYear - 6) + " to "
                                    + (currentYear + 3),
                            "I4SEE 660 = " + value));
                }
            } else {
                if (!StringUtils.isEmpty(value)) {
                    errors.add(new StateReportValidationError(entity, field,
                            "I4SEE 660 Original Graduation Year must be empty if grade is less than 09.",
                            "I4SEE 660 = " + value));
                }
            }
            return errors;
        }

    }

    /*
     * Alias constants
     */
    private static final String ALIAS_ENR_220_RESP_TOWN = "i4see 220";
    private static final String ALIAS_ENR_225_RESP_DISTR = "i4see 225";
    private static final String ALIAS_STD_210_ENROLLMENT_STATUS = "i4see 210";
    private static final String ALIAS_STD_310_DAYS_ABSENT = "i4see 310";
    private static final String ALIAS_STD_405_FULL_DAY_PERCENT = "i4see 405";
    private static final String ALIAS_STD_510_PROMOTED_IND = "i4see 510";
    private static final String ALIAS_STD_610_POST_GRAD_PLANS = "i4see 610";
    private static final String ALIAS_STD_620_DIPLOMA_TYPE = "i4see 620";
    private static final String ALIAS_SKL_CATE_INDICATOR = "i4see CATE";

    /**
     * Fields' ids
     */
    private static final String FIELD_050_SCHOOL_NUMBER = "School Number";
    private static final String FIELD_225_RESPONSIBLE_DISTR = "District Reponsible";
    private static final String FIELD_230_ENTRY_DATE = "Entry Date";
    private static final String FIELD_240_ENTRY_CODE = "Entry Code";
    private static final String FIELD_250_EXIT_DATE = "Exit Date";
    private static final String FIELD_260_EXIT_CODE = "Exit Code";
    private static final String FIELD_300_DAYS_IN_ATTENDANCE = "Half Days";
    private static final String FIELD_310_DAYS_ABSENT = "Half Days Absent";
    private static final String FIELD_400_GRADE = "Grade";
    private static final String FIELD_405_FULL_DAY_PERCENT = "Full Day Percent";
    private static final String FIELD_420_RACE = "Race";
    private static final String FIELD_510_PROMOTED_IND = "Promoted Indicator";
    private static final String FIELD_520_SUSPENSIONS_IN = "Days Suspended IS";
    private static final String FIELD_530_SUSPENSIONS_OUT = "Days Suspended OS";
    private static final String FIELD_580_RESIDENTIAL_HOME = "Residential Home";
    private static final String FIELD_610_POST_GRAD_PLANS = "Post Grad Plans";
    private static final String FIELD_620_DIPLOMA_TYPE = "Diploma Type";
    private static final String FIELD_660_ORIGINAL_YOG = "Orig Grad Year";

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
    protected static final String CALC_ID_I4SEE_405_FULL_DAY_PERCENT = "I4SEE405";
    protected static final String CALC_ID_I4SEE_420_RACE = "I4SEE420";
    protected static final String CALC_ID_I4SEE_510_PROMOTED_IND = "I4SEE510";
    protected static final String CALC_ID_I4SEE_520_530_CONDUCT_ACTION_TOTALS = "I4SEE520";
    protected static final String CALC_ID_I4SEE_610_POST_GRADUATE_PLANS = "I4SEE610";
    protected static final String CALC_ID_I4SEE_620_DIPLOMA_TYPE = "I4SEE620";
    protected static final String CALC_ID_I4SEE_HOME_CODES = "I4SEEHOME";

    /**
     * Validators' ids
     */
    protected static final String VAL_ID_I4SEE_210_ENROLLMENT_STATUS = "I4SEE210";
    protected static final String VAL_ID_I4SEE_240_ENTRY_CODE = "I4SEE240";
    protected static final String VAL_ID_I4SEE_260_EXIT_CODE = "I4SEE260";
    protected static final String VAL_ID_I4SEE_420_RACE_CODE = "I4SEE420";
    protected static final String VAL_ID_I4SEE_610_POST_GRADUATE_PLANS = "I4SEE610";
    protected static final String VAL_ID_I4SEE_620_DIPLOMA_TYPE = "I4SEE620";
    protected static final String VAL_ID_I4SEE_660_ORIG_GRAD_YR = "I4SEE660";
    protected static final String VAL_ID_MINIMUM_LENGTH = "MINLENGTH";

    /*
     * Field aliases for "adjusted" fields (these fields may have manually entered values that
     * override/adjust calculated values).
     *
     * Note: The alias for the adjusted status is optional (is does not have to be defined in the
     * Data Dictionary).
     */
    private static final String ADJUSTED_SCHOOL_NUMBER_FIELD = "i4see Adj School Number";
    /*
     * Field alias for the adjusted district code on the SCHOOL table. This alias is optional.
     */
    private static final String ADJUSTED_DISTRICT_CODE_FIELD = "DOE ADJUSTED DISTRICT";
    /*
     * Field alias/field value for querying options on the export
     */
    private static final String I4SEE_STATUS_FIELD = "i4see Status";
    private static final String I4SEE_STATUS_FIELD_REPORT_CODE = "Report";
    /*
     * List of fields NOT USED or BlanksAllowed(if N/A) by the B-O-Y report
     */

    @SuppressWarnings("unused")
    private static final List<String> BOY_NOT_USED_FIELDS = Arrays.asList(
            FIELD_405_FULL_DAY_PERCENT, // not used
            // I4SEE_300_DAYS_IN_ATTENDANCE, // not used
            // I4SEE_310_DAYS_ABSENT, // not used
            FIELD_520_SUSPENSIONS_IN, // not used
            FIELD_530_SUSPENSIONS_OUT, // not used
            FIELD_580_RESIDENTIAL_HOME, // not used
            FIELD_230_ENTRY_DATE, // BlanksAllowed(if NA)
            FIELD_240_ENTRY_CODE, // BlanksAllowed(if NA)
            FIELD_250_EXIT_DATE, // BlanksAllowed(if NA)
            FIELD_260_EXIT_CODE, // BlanksAllowed(if NA)
            FIELD_510_PROMOTED_IND, // BlanksAllowed(if NA)
            FIELD_620_DIPLOMA_TYPE, // BlanksAllowed(if NA)
            FIELD_610_POST_GRAD_PLANS, // BlanksAllowed(if NA)
            FIELD_660_ORIGINAL_YOG // BlanksAllowed(if NA)
    );

    /*
     * Conduct codes
     */
    private static final String SUSPENSION_IN_CODE = "1";
    private static final String SUSPENSION_OUT_CODE = "2";
    /*
     * Conduct Field for length of suspension
     */
    /**
     * Name for the "calculate totals" parameter. The value is a Boolean.
     */
    public static final String CALCULATE_TOTALS_PARAM = "calculateTotals";

    /**
     * Name for the enumerated "selection" parameter. The value is an Integer.
     */
    public static final String QUERY_BY_PARAM = "queryBy";
    /**
     * Name for the "selection value" parameter. The value is an String.
     */
    public static final String QUERY_STRING_PARAM = "queryString";

    /**
     * Name for the enumerated "sort" parameter. The value is an Integer.
     */
    public static final String SORT_PARAM = "sort";
    /**
     * Name for the "Include names as 1st column" parameter. The value is a Boolean.
     */
    public static final String INCLUDE_STUDENT_NAMES_PARAM = "includeStudentName";
    /**
     * Name for the "include summer withdrawals" parameter. The value is a Boolean.
     */
    public static final String INCLUDE_SUMMER_WITHDRAWALS_PARAM = "includeSummerWithdrawals";
    /**
     * Name for the report date parameter. The corresponding values is a PlainDate object.
     */
    public static final String REPORT_DATE_PARAM = "reportDate";
    /**
     * Name for the report type parameter. The corresponding value is an integer.
     */
    public static final String REPORT_TYPE = "reportType";
    /**
     * Value for REPORT_TYPE parameter indicating Beginning of Year report
     */
    public static final Integer REPORT_TYPE_BOY = Integer.valueOf(0);
    /**
     * Value for REPORT_TYPE parameter indicating End of Year report
     */
    public static final Integer REPORT_TYPE_EOY = Integer.valueOf(1);
    /**
     * Name for the "require report status" parameter. The value is a Boolean.
     */
    public static final String REQUIRE_REPORT_STATUS_PARAM = "requireReportStatus";
    /**
     * Name for the "require at lease one member day" parameter. The value is a Boolean.
     */
    public static final String REQUIRE_MEMBER_DAY_PARAM = "requireMemberDay";
    /**
     * Name for the "schoolYearContext" parameter. The value is a String.
     */
    public static final String PARAM_SCHOOL_YEAR_CONTEXT = "schoolYearContext";
    /**
     * Name for the field definition.
     */
    public static final String STUDENT_NAME = "name view";
    /**
     * Name for the "summer end date" parameter. The corresponding values is a java.sql.Date object.
     */
    public static final String SUMMER_END_DATE_PARAM = "summerEndDate";
    /**
     * Name for the "summer start date" parameter. The corresponding values is a java.sql.Date
     * object.
     */
    public static final String SUMMER_START_DATE_PARAM = "summerStartDate";
    /**
     * Name for the "update student records" parameter. The value is a Boolean.
     */
    public static final String UPDATE_RECORDS_PARAM = "updateRecords";
    /**
     * Hash map capacity multiplier
     */
    private final static double HASHMAP_CAPACITY_MULTIPLIER = 1.5;
    /**
     * String for keying off special Reason Code
     */
    private final static String W12R12_KEY = "W12/R12";
    /**
     * String for keying off special Reason Code
     */
    private final static String W1R1_KEY = "W1/R1";

    /*
     * Other internal constants
     */
    private static final String DATE_FORMAT = "MM/dd/yyyy";
    private static final boolean SUPPRESS_ENROLLMENT_WARNINGS = true;
    private static final String VALIDATION_INVALID_VALUE = "Invalid value";
    private static final String VALIDATION_MISSING_VALUE = "Missing value";
    private static final String OFF_TRACK_CODE = "OFTR";
    private static final String EARLY_GRADUATION_CODE = "W11";

    /**
     * bit values to <code>OR</code> together to index the <code>NH_STATE_RACE_CODES</code> table
     */
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
    static final int[] NH_TO_X2_RACE_CODES = new int[] {0, // not used
            AMERICAN_INDIAN_RACE_IND, // 1 - American Indian
            ASIAN_RACE_IND, // 2 - Asian
            HISPANIC_RACE_IND, // 3 - Hispanic
            BLACK_RACE_IND, // 4 - Black
            WHITE_RACE_IND, // 5 - White
            PACIFIC_ISLANDER_RACE_IND, // 6 - Pacific Islander
            0, // not used
            0, // not used
            0 // not used (nor is it valid)
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
    protected static final int[] NH_STATE_RACE_CODES = new int[] {0, // X2 value Hispanic Hawaiian
                                                                     // American Asian Black White
                                                                     // Pacific Indian/Alaska
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
            63}; // 63 x x x x x x
    private static final Object CALC_ID_I4SEE_SAU_DISTR = "I4SEESAU";

    /**
     * Supporting instance variables.
     * These are protected rather than private so they can be accessed by the inner classes.
     */
    protected Map<String, Integer> m_absences;
    protected Map<String, Collection<StudentAttendance>> m_absencesMap;
    protected String m_adjustedSchoolCode;
    protected boolean m_calculateTotals;
    protected DistrictSchoolYearContext m_context;
    protected String m_contextOid;
    protected Map<String, Collection<ConductAction>> m_actOutSuspMapByStd;
    protected Map<String, Collection<ConductAction>> m_actInSuspMapByStd;
    protected SimpleDateFormat m_dateFormat;
    protected String m_enr210Field;
    protected String m_enr220Field;
    protected String m_enr225Field;
    protected EnrollmentManager m_enrollmentManager;
    protected HashMap<String, Map<String, String>> m_fieldToRefTable;
    protected PlainDate m_firstDayDate;
    protected Set m_firstDayMembers;
    protected TreeMap m_gradeLevelMap;
    protected boolean m_includeStudentNames;
    protected Converter m_integerConverter;
    protected PlainDate m_lastDayDate;
    protected Map<String, Collection<StudentAttendance>> m_offTrackAbsenceForStd;
    protected Map<String, Collection<Race>> m_raceCodeMap;
    protected PlainDate m_reportDate;
    protected String m_reportStatusField;
    protected Integer m_reportType;
    protected Map<String, Schedule> m_scheduleMap;
    protected Map<String, SisSchool> m_schoolMap;
    protected HashMap m_schoolsToCalendars;
    protected String m_std405Field;
    protected String m_std310Field;
    protected String m_std510Field;
    protected String m_std620Field;
    protected String m_std610Field;
    protected Collection m_suspensionInCodes;
    protected Collection m_suspensionOutCodes;

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
     * @param i4see I4SeeEntity
     * @return String
     */
    public String getMembershipDays(I4SeeEntity i4see) {
        String count = null;
        SisStudent student = (SisStudent) i4see.getBean();

        String adjustedCount = "";
        if (!StringUtils.isEmpty(adjustedCount)) {
            count = adjustedCount;
        } else {
            // Check the active schedule for the school.
            SisSchool school = m_schoolMap.get(student.getSchoolOid());
            Schedule schedule = null;

            if (school != null) {
                schedule = m_scheduleMap.get(school.getOid());
                if (schedule != null) {
                    try {
                        StudentEnrollment entry = i4see.getEntry();
                        StudentEnrollment withdrawal = i4see.getWithdrawal();

                        PlainDate startDate = m_scheduleMap.get(student.getSchoolOid()).getStartDate();
                        PlainDate exitDate = m_reportDate;

                        if (entry != null && entry.getEnrollmentDate().after(startDate)) {
                            startDate = entry.getEnrollmentDate();
                        }
                        if (withdrawal != null) {
                            exitDate = new PlainDate(m_dateFormat.parse(i4see.getFieldValue(FIELD_250_EXIT_DATE)));
                        }

                        Collection<StudentAttendance> atts = m_offTrackAbsenceForStd.get(student.getOid());

                        int offTrackDays = 0;
                        if (atts != null && !atts.isEmpty()) {
                            for (StudentAttendance att : atts) {
                                if ((startDate.before(att.getDate()) || startDate.equals(att.getDate())) &&
                                        (exitDate.after(att.getDate()) || exitDate.equals(att.getDate()))) {
                                    offTrackDays += 1;
                                }
                            }
                        }

                        count = String.valueOf(
                                m_enrollmentManager.getMembershipTotal(
                                        student,
                                        getCalendarDays(m_schoolMap.get(student.getSchoolOid()),
                                                student.getCalendarCode()),
                                        true,
                                        startDate,
                                        exitDate,
                                        null) - offTrackDays);
                    } catch (Exception e) {
                        addSetupError("Membership days",
                                "Could not calculate membership: exception\n\t" + e.getMessage());
                    }
                }
            }
        }

        return count;
    }

    /**
     * Checks if given date in the current context date range.
     *
     * @param dateToCheck PlainDate
     * @return true, if is in ctx date range
     */
    public boolean isInCtxDateRange(PlainDate dateToCheck) {

        boolean inDateRange = false;

        if (dateToCheck != null && ((dateToCheck.after(m_firstDayDate) && (dateToCheck.before(m_lastDayDate)))
                || dateToCheck.equals(m_firstDayDate) || dateToCheck.equals(m_lastDayDate))) {
            inDateRange = true;
        }

        return inDateRange;
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
        m_contextOid = (String) getParameter(PARAM_SCHOOL_YEAR_CONTEXT);
        m_context = getCurrentContext();

        /*
         * Load initialization data
         */
        initializeFields();

        /*
         * Load state codes for reference tables
         */
        String javaNamePromotedInd = translateAliasToJavaName(ALIAS_STD_510_PROMOTED_IND, true);
        String[] fields = {SisStudent.REL_ENROLLMENTS + "." + StudentEnrollment.COL_ENROLLMENT_CODE,
                javaNamePromotedInd
        };
        loadStateReferenceCodes(fields);

        /*
         * Load conduct codes
         */
        loadConductCodes();

        /*
         * Load Schools
         */
        loadSchools();

        /*
         * Load Schedules for the Selected CTX
         */
        loadSchedules();

        /*
         * Get core parameters
         */
        m_calculateTotals = ((Boolean) getParameter(CALCULATE_TOTALS_PARAM)).booleanValue();
        m_reportType = (Integer) getParameter(REPORT_TYPE);

        m_dateFormat = new SimpleDateFormat(DATE_FORMAT);

        /*
         * Set up converters, formatters, reference lookup tables, and other database-intense
         * operations. We do this once outside the student loop to improve performance.
         */
        m_gradeLevelMap = StudentManager.buildGradeLevelMap(getBroker());
        m_integerConverter =
                ConverterFactory.getConverterForClass(Converter.INTEGER_CONVERTER, Locale.getDefault(), true);

        /*
         * Set the field definition array
         */
        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {
            /*
             * Build query object that will be used to retrieve export students.
             */
            Criteria studentCriteria = getStudentCriteria();
            QueryByCriteria studentQuery = new QueryByCriteria(SisStudent.class, studentCriteria);

            int sort = ((Integer) getParameter(SORT_PARAM)).intValue();
            switch (sort) {
                case 0: // Name
                    studentQuery.addOrderByAscending(SisStudent.COL_NAME_VIEW);
                    break;

                case 1: // YOG
                    studentQuery.addOrderByAscending(SisStudent.COL_YOG);
                    studentQuery.addOrderByAscending(SisStudent.COL_NAME_VIEW);
                    break;

                case 2: // School
                    studentQuery.addOrderByAscending(SisStudent.REL_SCHOOL + PATH_DELIMITER +
                            translateAliasToJavaName(FIELD_050_SCHOOL_NUMBER, true));
                    studentQuery.addOrderByAscending(SisStudent.COL_NAME_VIEW);
                    break;

                case 3: // LASID
                    studentQuery.addOrderByAscending(SisStudent.COL_LOCAL_ID);
                    break;

                case 4: // SASID
                    studentQuery.addOrderByAscending(SisStudent.COL_STATE_ID);
                    break;

                default:
                    studentQuery.addOrderByAscending(SisStudent.COL_NAME_VIEW);
                    break;
            }

            /*
             * Add the Student Name field if requested
             */
            if (m_includeStudentNames) {
                getFieldDefinitions().add(0, getName());
            }

            initializeRetrievers();
            initializeValidators();

            // Set the query to be used for student selection.
            setQuery(studentQuery);
            setEntityClass(I4SeeEntity.class);

            /*
             * Load absence days for all students included in the export.
             */
            loadAbsenceDaysMaps(studentCriteria);

            /*
             * Load conduct action map.
             */
            loadActInSuspMap(studentCriteria);
            loadActOutSuspMap(studentCriteria);
            /*
             * Load the race codes for all students included in the export.
             */
            SubQuery subQuery = new SubQuery(SisStudent.class, SisStudent.COL_PERSON_OID, studentCriteria);
            loadRaceRecords(studentQuery, subQuery);
        }
    }

    /**
     * Looks up the first in-session date for the student's calendar. If one does not exists, use
     * the first day date (start date of district school year).
     *
     * @param snapshot EnrollmentSnapshot
     * @return PlainDate
     */
    PlainDate findFirstEnrollmentDay(EnrollmentSnapshot snapshot) {
        PlainDate date = null;

        Set<PlainDate> days = getCalendarDays(
                snapshot.getSchool() == null ? m_schoolMap.get(snapshot.getStudent().getSchoolOid())
                        : snapshot.getSchool(),
                snapshot.getStudent().getCalendarCode());
        if (days != null) {
            for (PlainDate day : days) {
                if (date == null || day.before(date)) {
                    date = day;
                }
            }
        }

        if (date == null) {
            date = m_firstDayDate;
        }

        return date;
    }

    /**
     * Adds criteria to filter the export selection by record set.
     *
     * @param criteria Criteria
     * @param recordSetName String
     */
    private void addRecordSetCriteria(Criteria criteria, String recordSetName) {
        Criteria recordSetCriteria = new Criteria();
        recordSetCriteria.addEqualTo(RecordSetKey.REL_RECORD_SET + "." + RecordSet.COL_NAME,
                recordSetName);

        criteria.addIn(X2BaseBean.COL_OID, new SubQuery(RecordSetKey.class,
                RecordSetKey.COL_OBJECT_OID, recordSetCriteria));
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
        if (!m_schoolsToCalendars.containsKey(school.getOid())) {
            // If a schedule has been defined...
            if (m_scheduleMap.get(school.getOid()) != null) {
                PlainDate startDate = m_scheduleMap.get(school.getOid()).getStartDate();
                Map calendarData = m_enrollmentManager.getCalendarLookup(school, startDate, m_reportDate);
                m_schoolsToCalendars.put(school.getOid(), calendarData);
            } else {
                m_schoolsToCalendars.put(school.getOid(), new HashMap<String, PlainDate>());
            }
        }

        return (Set) ((Map) m_schoolsToCalendars.get(school.getOid())).get(calendar);
    }

    /**
     * Build Field definition for the student name view.
     *
     * @return a FieldDefinition
     */
    private FieldDefinition getName() {
        FieldDefinition field = new FieldDefinition(STUDENT_NAME, SisStudent.COL_NAME_VIEW,
                null, false, 1, 32, null,
                null, null, null, null);

        return field;
    }

    /**
     * Builds a criteria for the students that should be reported (not considering user input
     * parameters).
     *
     * @return Criteria
     */
    private Criteria getReportingCriteria() {
        /*
         * Who should be included? Primary students and, optionally, summer withdrawals.
         *
         * The students that belong to each group depend upon how this export is being run:
         *
         * Case 1: The export is being run for either (A) the entire district or (B) a single school
         * without "nested districts" (99% of all cases)
         *
         * Case 2: The export is being run for a single school with "nested districts"
         *
         * ----------------------------------------------------------------------------------------
         *
         * Q: What are "nested districts" anyway?
         *
         * A: A single X2 district could really represent multiple districts as far as the DOE is
         * concerned. For example, Nauset is a single X2 district but only the middle and high
         * schools are in the Nauset Regional School District. All the elementary schools belong
         * to their own districts according to the DOE. These "nested districts" can be
         * represented in X2 by setting different Adjusted District Code values on the school
         * records. If "nested districts" are used then ALL schools should have an Adjusted
         * District Code (even the archive school).
         *
         * ----------------------------------------------------------------------------------------
         *
         * Primary students, case 1:
         *
         * Students in an active, non-archived school in the district
         *
         * ----------------------------------------------------------------------------------------
         *
         * Primary students, case 2:
         *
         * Students in the select school as well as students who have withdrawn from the selected
         * school between the start of the school year and the report date (both dates inclusive)
         * and are now in a school with a different adjusted district code
         *
         * ----------------------------------------------------------------------------------------
         *
         * Summer withdrawals, case 1:
         *
         * Students who withdrew during the summer (start/end dates inclusive) and are now in the
         * archive school
         *
         * ----------------------------------------------------------------------------------------
         *
         * Summer withdrawals, case 2:
         *
         * Students who withdrew from the selected school during the summer (start/end dates
         * inclusive) and are now either in the archive school or a school with a different
         * adjusted district code
         *
         * ----------------------------------------------------------------------------------------
         *
         */
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(ADJUSTED_DISTRICT_CODE_FIELD);

        String adjustedDistrictCode = null;
        if (isSchoolContext() && field != null) {
            adjustedDistrictCode = (String) getSchool().getFieldValueByBeanPath(field.getJavaName());
        }

        boolean useNestedDistricts = !StringUtils.isEmpty(adjustedDistrictCode);

        /*
         * Primary students
         */
        Criteria primaryCriteria = new Criteria();
        if (isSchoolContext()) {
            primaryCriteria.addEqualTo(SisStudent.COL_SCHOOL_OID, getSchool().getOid());

            if (useNestedDistricts) {
                PlainDate startDate = m_scheduleMap.get(getSchool().getOid()).getStartDate();

                Criteria withdrawalsCriteria = new Criteria();
                withdrawalsCriteria.addIn(X2BaseBean.COL_OID, getStudentWithdrawalQuery(startDate, m_reportDate));
                withdrawalsCriteria.addNotEqualTo(
                        SisStudent.REL_SCHOOL + ModelProperty.PATH_DELIMITER + field.getJavaName(),
                        adjustedDistrictCode);

                primaryCriteria.addOrCriteria(withdrawalsCriteria);
            }
        } else {
            primaryCriteria.addEqualTo(
                    SisStudent.REL_SCHOOL + ModelProperty.PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR,
                    Boolean.FALSE);
            primaryCriteria.addEqualTo(
                    SisStudent.REL_SCHOOL + ModelProperty.PATH_DELIMITER + SisSchool.COL_ARCHIVE_INDICATOR,
                    Boolean.FALSE);
        }

        X2Criteria reportingCriteria = new X2Criteria();
        reportingCriteria.addAndCriteria(primaryCriteria);

        /*
         * Summer withdrawals (optional)
         */
        Boolean includeSummerWithdrawals = (Boolean) getParameter(INCLUDE_SUMMER_WITHDRAWALS_PARAM);
        if (includeSummerWithdrawals != null && includeSummerWithdrawals.booleanValue()) {
            X2Criteria schoolCriteria = new X2Criteria();

            if (useNestedDistricts) {
                schoolCriteria.addNotEqualTo(SisStudent.REL_SCHOOL + ModelProperty.PATH_DELIMITER + field.getJavaName(),
                        adjustedDistrictCode);
            }

            schoolCriteria.addOrEqualTo(
                    SisStudent.REL_SCHOOL + ModelProperty.PATH_DELIMITER + SisSchool.COL_ARCHIVE_INDICATOR,
                    Boolean.TRUE);

            X2Criteria summerCriteria = new X2Criteria();
            summerCriteria.addEqualTo(SisStudent.COL_ORGANIZATION1_OID, getOrganization().getOid());
            summerCriteria.addAndCriteria(schoolCriteria);

            PlainDate summerEndDate = (PlainDate) getParameter(SUMMER_END_DATE_PARAM);
            PlainDate summerStartDate = (PlainDate) getParameter(SUMMER_START_DATE_PARAM);

            summerCriteria.addIn(X2BaseBean.COL_OID, getStudentWithdrawalQuery(summerStartDate, summerEndDate));

            reportingCriteria.addOrCriteria(summerCriteria);
        }

        return reportingCriteria;
    }

    /**
     * Returns the criteria that retrieves all students that should be included in the export.
     *
     * @return Criteria
     */
    private Criteria getStudentCriteria() {
        /*
         * First build the criteria based on the user's input
         */
        X2Criteria userCriteria = new X2Criteria();

        Boolean requireReportStatus = (Boolean) getParameter(REQUIRE_REPORT_STATUS_PARAM);
        if (requireReportStatus.booleanValue()) {
            userCriteria.addEqualTo(m_reportStatusField, I4SEE_STATUS_FIELD_REPORT_CODE);
        }

        String queryString = (String) getParameter(QUERY_STRING_PARAM);
        int queryBy = ((Integer) getParameter(QUERY_BY_PARAM)).intValue();
        switch (queryBy) {
            case 1: // YOG
                userCriteria.addEqualTo(SisStudent.COL_YOG, queryString);
                break;

            case 2: // LASID
                userCriteria.addEqualTo(SisStudent.COL_LOCAL_ID, queryString);
                break;

            case 3: // SASID
                userCriteria.addEqualTo(SisStudent.COL_STATE_ID, queryString);
                break;

            case 4: // Snapshot
                addRecordSetCriteria(userCriteria, queryString);
                break;

            default:
                // Take all students in the district
                break;
        }

        /*
         * Then combine the user criteria with the general reporting criteria
         */
        Criteria studentCriteria = new Criteria();
        studentCriteria.addAndCriteria(userCriteria);
        studentCriteria.addAndCriteria(getReportingCriteria());

        return studentCriteria;
    }

    /**
     * Returns a query that finds the students who withdrew during the given date range (filtered
     * by school as appropriate).
     *
     * @param startDate PlainDate
     * @param endDate PlainDate
     * @return A SubQuery that returns the student OID from StudentEnrollment records
     */
    private SubQuery getStudentWithdrawalQuery(PlainDate startDate, PlainDate endDate) {
        Criteria enrollmentCriteria = new Criteria();
        enrollmentCriteria.addEqualTo(StudentEnrollment.COL_ENROLLMENT_TYPE, StudentEnrollment.WITHDRAWAL);
        enrollmentCriteria.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, startDate);
        enrollmentCriteria.addLessOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, endDate);

        if (isSchoolContext()) {
            enrollmentCriteria.addEqualTo(StudentEnrollment.COL_SCHOOL_OID, getSchool().getOid());
        }

        return new SubQuery(StudentEnrollment.class, StudentEnrollment.COL_STUDENT_OID, enrollmentCriteria);
    }

    /**
     * Sets the Java names (bean paths) for all the exported fields.
     */
    private void initializeFields() {
        /*
         * Resolve some support aliases, log an error and stop the export if an alias is not found.
         */
        m_adjustedSchoolCode = translateAliasToJavaName(ADJUSTED_SCHOOL_NUMBER_FIELD, true);

        m_includeStudentNames = false;
        if (getParameter(INCLUDE_STUDENT_NAMES_PARAM) != null) {
            m_includeStudentNames = ((Boolean) getParameter(INCLUDE_STUDENT_NAMES_PARAM)).booleanValue();
        }

        m_enrollmentManager = new EnrollmentManager(getBroker(), getPrivilegeSet(), getOrganization());
        m_firstDayDate = getCurrentContext().getStartDate();
        m_lastDayDate = getCurrentContext().getEndDate();

        m_reportDate = (PlainDate) getParameter(REPORT_DATE_PARAM);

        if (m_reportDate.before(m_firstDayDate) || m_reportDate.after(m_lastDayDate)) {
            m_reportDate = m_lastDayDate;
        }
        m_firstDayMembers = m_enrollmentManager.getMembershipAsOf(m_firstDayDate, getOrganization());
        m_reportStatusField = translateAliasToJavaName(I4SEE_STATUS_FIELD, true);
        m_enr220Field = translateAliasToJavaName(ALIAS_ENR_220_RESP_TOWN, true);
        m_enr225Field = translateAliasToJavaName(ALIAS_ENR_225_RESP_DISTR, true);
        m_enr210Field = translateAliasToJavaName(ALIAS_STD_210_ENROLLMENT_STATUS, true);
        m_std310Field = translateAliasToJavaName(ALIAS_STD_310_DAYS_ABSENT, true);
        m_std405Field = translateAliasToJavaName(ALIAS_STD_405_FULL_DAY_PERCENT, true);
        m_std510Field = translateAliasToJavaName(ALIAS_STD_510_PROMOTED_IND, true);
        m_std620Field = translateAliasToJavaName(ALIAS_STD_620_DIPLOMA_TYPE, true);
        m_std610Field = translateAliasToJavaName(ALIAS_STD_610_POST_GRAD_PLANS, true);
        m_schoolsToCalendars = new HashMap();
    }

    /**
     * Initialize field retrievers.
     */
    private void initializeRetrievers() {
        /**
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
        calcs.put(CALC_ID_I4SEE_405_FULL_DAY_PERCENT, new Retrieve405FullDayPercent());
        calcs.put(CALC_ID_I4SEE_420_RACE, new Retrieve420Race());
        calcs.put(CALC_ID_I4SEE_510_PROMOTED_IND, new Retrieve510PromotedInd());
        calcs.put(CALC_ID_I4SEE_520_530_CONDUCT_ACTION_TOTALS, new Retrieve520and530ConductActionTotals());
        calcs.put(CALC_ID_I4SEE_610_POST_GRADUATE_PLANS, new Retrieve610PostGraduatePlans());
        calcs.put(CALC_ID_I4SEE_620_DIPLOMA_TYPE, new Retrieve620DiplomaType());
        calcs.put(CALC_ID_I4SEE_SAU_DISTR, new Retrieve030_040_Sau_DistrictNumber());
        super.addCalcs(calcs);

    }

    /**
     * Initialize field validators.
     */
    private void initializeValidators() {
        HashMap validators = new HashMap<String, FieldValidator>();
        validators.put(VAL_ID_I4SEE_210_ENROLLMENT_STATUS, new Validate210EnrollmentStatus());
        validators.put(VAL_ID_I4SEE_240_ENTRY_CODE, new Validate240EntryCode());
        validators.put(VAL_ID_I4SEE_260_EXIT_CODE, new Validate260ExitCode());
        validators.put(VAL_ID_I4SEE_420_RACE_CODE, new Validate420RaceCode());
        validators.put(VAL_ID_I4SEE_610_POST_GRADUATE_PLANS, new Validate610PostGraduatePlans());
        validators.put(VAL_ID_I4SEE_620_DIPLOMA_TYPE, new Validate620DiplomaType());
        validators.put(VAL_ID_I4SEE_660_ORIG_GRAD_YR, new Validate660OriginalGraduationYear());
        super.addValidators(validators);

    }

    /**
     * Loads a map by student of absence days for that student.
     *
     * @param studentCriteria Criteria
     */
    private void loadAbsenceDaysMaps(Criteria studentCriteria) {
        /*
         * Part I. Absence days from attendance.
         */
        // subQuery - Students.
        SubQuery studentsSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria);

        // Main report query - students and Absence.
        Criteria criteria = new Criteria();
        criteria.addIn(StudentAttendance.COL_STUDENT_OID, studentsSubQuery);
        criteria.addGreaterOrEqualThan(StudentAttendance.COL_DATE, m_firstDayDate);
        criteria.addLessOrEqualThan(StudentAttendance.COL_DATE, m_reportDate);
        criteria.addEqualTo(StudentAttendance.COL_ABSENT_INDICATOR, Boolean.TRUE);

        QueryByCriteria allAttQuery = new QueryByCriteria(StudentAttendance.class, criteria);
        m_absencesMap = getBroker().getGroupedCollectionByQuery(allAttQuery, StudentAttendance.COL_STUDENT_OID, 1024);

        ReportQueryByCriteria reportQuery = new ReportQueryByCriteria(StudentAttendance.class,
                new String[] {StudentAttendance.COL_STUDENT_OID, "COUNT(*)"}, criteria);
        reportQuery.addGroupBy(StudentAttendance.COL_STUDENT_OID);

        // Build the map of student to courses.
        m_absences = new HashMap<String, Integer>();

        ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(reportQuery);
        try {
            while (iterator.hasNext()) {
                Object[] row = (Object[]) iterator.next();
                String studentOid = (String) row[0];
                Integer absencesCount = Integer.valueOf(row[1].toString());
                m_absences.put(studentOid, absencesCount);
            }
        } finally {
            iterator.close();
        }

        criteria = new Criteria();
        criteria.addIn(StudentAttendance.COL_STUDENT_OID, studentsSubQuery);
        criteria.addGreaterOrEqualThan(StudentAttendance.COL_DATE, m_firstDayDate);
        criteria.addLessOrEqualThan(StudentAttendance.COL_DATE, m_reportDate);
        criteria.addEqualTo(StudentAttendance.COL_OTHER_CODE, OFF_TRACK_CODE);
        QueryByCriteria offTrackQuery = new QueryByCriteria(StudentAttendance.class, criteria);
        m_offTrackAbsenceForStd =
                getBroker().getGroupedCollectionByQuery(offTrackQuery, StudentAttendance.COL_STUDENT_OID, 1024);
    }

    /**
     * Load map of IN suspension Conduct Action map keyed on STD OID.
     *
     * @param studentCriteria Criteria
     */
    private void loadActInSuspMap(Criteria studentCriteria) {
        // subQuery - Students.
        SubQuery studentsSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria);

        // Main report query - students and Absence.
        Criteria criteria = new Criteria();
        criteria.addIn(ConductAction.COL_STUDENT_OID, studentsSubQuery);
        criteria.addGreaterOrEqualThan(ConductAction.COL_ACTION_START_DATE, m_firstDayDate);
        criteria.addLessOrEqualThan(ConductAction.COL_ACTION_END_DATE, m_reportDate);

        if (m_suspensionInCodes != null && !m_suspensionInCodes.isEmpty()) {
            criteria.addIn(ConductAction.COL_ACTION_CODE, m_suspensionInCodes);
        } else {
            criteria.addEqualTo(ConductAction.COL_ACTION_CODE, "__dummy__");
        }

        QueryByCriteria query = new QueryByCriteria(ConductAction.class, criteria);

        m_actInSuspMapByStd = getBroker().getGroupedCollectionByQuery(query, ConductAction.COL_STUDENT_OID, 1024);
    }

    /**
     * Load map of OUT suspension Conduct Action map keyed on STD OID.
     *
     * @param studentCriteria Criteria
     */
    private void loadActOutSuspMap(Criteria studentCriteria) {
        // subQuery - Students.
        SubQuery studentsSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria);

        // Main report query - students and Absence.
        Criteria criteria = new Criteria();
        criteria.addIn(ConductAction.COL_STUDENT_OID, studentsSubQuery);
        criteria.addGreaterOrEqualThan(ConductAction.COL_ACTION_START_DATE, m_firstDayDate);
        criteria.addLessOrEqualThan(ConductAction.COL_ACTION_END_DATE, m_reportDate);

        if (m_suspensionOutCodes != null && !m_suspensionOutCodes.isEmpty()) {
            criteria.addIn(ConductAction.COL_ACTION_CODE, m_suspensionOutCodes);
        } else {
            criteria.addEqualTo(ConductAction.COL_ACTION_CODE, "__dummy__");
        }

        QueryByCriteria query = new QueryByCriteria(ConductAction.class, criteria);

        m_actOutSuspMapByStd = getBroker().getGroupedCollectionByQuery(query, ConductAction.COL_STUDENT_OID, 1024);

    }

    /**
     * Loads all the basic codes that correspond to the state codes for in-school suspensions and
     * out-of-school suspensions. If there are no basic codes for the state code then the
     * collection(s) will be empty.
     */
    private void loadConductCodes() {
        m_suspensionInCodes = new LinkedList();
        m_suspensionOutCodes = new LinkedList();

        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());

        /*
         * Suspensions are ConductAction codes
         */
        DataDictionaryField actionCode = dictionary.findDataDictionaryField(
                ConductAction.class.getName(), ConductAction.COL_ACTION_CODE);
        String actionReferenceTable = actionCode.getDataFieldConfig().getReferenceTableOid();

        // In-school
        Criteria criteria = new Criteria();
        criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, actionReferenceTable);
        criteria.addEqualTo(ReferenceCode.COL_STATE_CODE, SUSPENSION_IN_CODE);

        QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, criteria);
        QueryIterator referenceCodes = getBroker().getIteratorByQuery(query);
        try {
            while (referenceCodes.hasNext()) {
                ReferenceCode referenceCode = (ReferenceCode) referenceCodes.next();
                m_suspensionInCodes.add(referenceCode.getCode());
            }
        } finally {
            referenceCodes.close();
        }

        // Out-of-school
        criteria = new Criteria();
        criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, actionReferenceTable);
        criteria.addEqualTo(ReferenceCode.COL_STATE_CODE, SUSPENSION_OUT_CODE);

        query = new QueryByCriteria(ReferenceCode.class, criteria);
        referenceCodes = getBroker().getIteratorByQuery(query);
        try {
            while (referenceCodes.hasNext()) {
                ReferenceCode referenceCode = (ReferenceCode) referenceCodes.next();
                m_suspensionOutCodes.add(referenceCode.getCode());
            }
        } finally {
            referenceCodes.close();
        }
    }

    /**
     * Loads the race records for all students included in the export into a Map keyed to the
     * Person OID.
     *
     * @param studentQuery Query
     * @param personSubQuery SubQuery
     */
    private void loadRaceRecords(Query studentQuery, SubQuery personSubQuery) {
        Criteria raceCriteria = new Criteria();
        raceCriteria.addIn(Race.COL_PERSON_OID, personSubQuery);

        QueryByCriteria raceQuery = new QueryByCriteria(Race.class, raceCriteria);
        int count = getBroker().getCount(studentQuery);
        m_raceCodeMap = getBroker().getGroupedCollectionByQuery(raceQuery, Race.COL_PERSON_OID, count);
    }

    /**
     * Loads the active schedule for each school.
     */
    private void loadSchedules() {
        X2Criteria schCriteria = new X2Criteria();
        if (m_schoolMap != null) {
            schCriteria.addIn(Schedule.COL_SCHOOL_OID, m_schoolMap.keySet());
        } else {
            schCriteria.addEqualTo(Schedule.COL_SCHOOL_OID, "__dummy__");
        }

        schCriteria.addEqualTo(Schedule.REL_ACTIVE_SCHOOL_SCHEDULE_CONTEXTS + PATH_DELIMITER
                + SchoolScheduleContext.COL_DISTRICT_CONTEXT_OID, m_contextOid);

        QueryByCriteria schQuery = new QueryByCriteria(Schedule.class, schCriteria);
        m_scheduleMap = getBroker().getMapByQuery(schQuery, Schedule.COL_SCHOOL_OID, 1024);
    }

    /**
     * Loads all the school beans in the district.
     */
    private void loadSchools() {
        QueryByCriteria schoolQuery = new QueryByCriteria(SisSchool.class, new Criteria());
        m_schoolMap = getBroker().getMapByQuery(schoolQuery, X2BaseBean.COL_OID, getBroker().getCount(schoolQuery));
    }

    /**
     * Returns a map of base reference codes to their state reference code equivalents for the
     * reference table used by the given property. If the property doesn't use a
     * reference table then an empty map is returned.
     *
     * @param propertyName String
     * @return A Map of String keys to String values
     */
    private Map getStateCodeReferenceMap(String propertyName) {
        HashMap baseToStateCodes = null;

        ModelProperty property =
                new ModelProperty(SisStudent.class.getName(), propertyName, getBroker().getPersistenceKey());
        DataDictionaryField field = property.getField();
        if (field.hasReferenceTable()) {
            Collection codes = field.getReferenceTable().getReferenceCodes(getBroker());
            baseToStateCodes = new HashMap((int) (codes.size() * HASHMAP_CAPACITY_MULTIPLIER));
            Iterator codeIterator = codes.iterator();
            while (codeIterator.hasNext()) {
                ReferenceCode code = (ReferenceCode) codeIterator.next();
                baseToStateCodes.put(code.getCode(), code.getStateCode());
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
                String[] sysPrefDefs = {SisPreferenceConstants.ENROLLMENT_ENTRY_CODES,
                        SisPreferenceConstants.ENROLLMENT_WITHDRAWAL_CODES};

                baseToStateCodes = new HashMap();
                for (String sysPrefDef : sysPrefDefs) {
                    String refTableOid = preferenceSet.getPreferenceValue(sysPrefDef);
                    ReferenceTable refTable =
                            (ReferenceTable) getBroker().getBeanByOid(ReferenceTable.class, refTableOid);
                    Collection codes = refTable.getReferenceCodes(getBroker());
                    Iterator codeIterator = codes.iterator();
                    while (codeIterator.hasNext()) {
                        ReferenceCode code = (ReferenceCode) codeIterator.next();
                        baseToStateCodes.put(code.getCode(), code.getStateCode());
                    }
                }
            }
        }

        return baseToStateCodes;
    }

    /**
     * *********************************************************************
     *
     * COMMON STATE REPORTING CODE THAT NEEDS TO BE PUT IN A COMMON PLACE
     *
     * *********************************************************************.
     *
     * @param beanPaths String[]
     */
    private void loadStateReferenceCodes(String[] beanPaths) {
        m_fieldToRefTable =
                new HashMap<String, Map<String, String>>((int) (beanPaths.length * HASHMAP_CAPACITY_MULTIPLIER));

        for (String beanPath : beanPaths) {
            m_fieldToRefTable.put(beanPath, getStateCodeReferenceMap(beanPath));
        }
    }

}
