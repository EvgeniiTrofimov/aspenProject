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

package com.x2dev.procedures.statereporting.wa;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.StudentSchool;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAttendance;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.tools.stateexports.StudentEnrollmentSpan;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Export Data Module for WA School Student.
 *
 * @author X2 Development Corporation
 */
public class WASchoolStudent extends StateReportData {
    /**
     * Entity class for WA School Student export.
     *
     */
    public static class WASchoolStudentEntity extends StateReportEntity {
        /**
         * Entity instance variables.
         */
        WASchoolStudent m_sdData;
        List<Object> m_schoolStudentEntry;

        boolean m_memberOnEntryDate;
        boolean m_memberOnWithdrawalDate;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public WASchoolStudentEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Returns the StudentEnrollmentSpan or StudentSchool record for the current index.
         *
         * @return Object The student school entry
         */
        public Object getStudentSchoolEntry() {
            return m_schoolStudentEntry.get(getCurrentRow());
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
            Object entry = getStudentSchoolEntry();
            if (entry instanceof StudentEnrollmentSpan) {
                StudentEnrollmentSpan span = (StudentEnrollmentSpan) entry;
                if (span.getSchool() != null) {
                    name += span.getSchool().getName();
                }
            } else if (entry instanceof StudentSchool) {
                StudentSchool ss = (StudentSchool) entry;
                if (ss.getSchool() != null) {
                    name += ss.getSchool().getName();
                }
            }

            return name;
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

            m_sdData = (WASchoolStudent) data;
            List<StudentEnrollmentSpan> enrollmentSpans =
                    m_sdData.m_helper.getStudentEnrollmentSpans((Student) bean, true);

            m_memberOnEntryDate = Boolean.valueOf(PreferenceManager.getPreferenceValue(m_sdData.getOrganization(),
                    SisPreferenceConstants.STUDENT_MEMBER_ON_ENTRY_DATE)).booleanValue();
            m_memberOnWithdrawalDate = Boolean.valueOf(PreferenceManager.getPreferenceValue(m_sdData.getOrganization(),
                    SisPreferenceConstants.STUDENT_MEMBER_ON_WITHDRAWAL_DATE)).booleanValue();

            filterEnrollmentSpans(enrollmentSpans);
            addSchoolStudentEntries(enrollmentSpans, getSecondarySchools(bean, m_sdData.m_studentSchoolMap));
            setRowCount(m_schoolStudentEntry.size());
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
         * Adds the enrollment spans that relate to the primary school to the school student
         * entries.
         *
         * @param enrollmentSpans The enrollment spans
         */
        private void addPrimarySchoolEntries(List<StudentEnrollmentSpan> enrollmentSpans) {
            if (enrollmentSpans != null) {
                m_schoolStudentEntry.addAll(enrollmentSpans);
            }
        }

        /**
         * Adds the entries.
         *
         * @param enrollmentSpans The enrollment spans for the primary schools
         * @param secondarySchools The secondary schools
         */
        private void addSchoolStudentEntries(List<StudentEnrollmentSpan> enrollmentSpans,
                                             List<StudentSchool> secondarySchools) {
            m_schoolStudentEntry = new ArrayList<Object>(getSize(enrollmentSpans, secondarySchools));

            addPrimarySchoolEntries(enrollmentSpans);
            addSecondarySchoolEntries(secondarySchools);
        }

        /**
         * Adds the secondary schools.
         *
         * @param secondarySchools The secondary schools
         */
        private void addSecondarySchoolEntries(List<StudentSchool> secondarySchools) {
            if (secondarySchools != null) {
                m_schoolStudentEntry.addAll(secondarySchools);
            }
        }

        /**
         * Processes the enrollment spans.
         *
         * @param enrollmentSpans List<StudentEnrollmentSpan>
         */
        private void filterEnrollmentSpans(List<StudentEnrollmentSpan> enrollmentSpans) {
            Iterator<StudentEnrollmentSpan> spanIterator = enrollmentSpans.iterator();
            while (spanIterator.hasNext()) {
                StudentEnrollmentSpan span = spanIterator.next();

                if (!isMember(span)) {
                    spanIterator.remove();
                } else if (isDistrictServe(span)) {
                    spanIterator.remove();
                } else if (isNoShow(span)) {
                    spanIterator.remove();
                } else if (isExcludedSchool(span, getSchoolOid(m_sdData.getSchool()))) {
                    spanIterator.remove();
                }
            }
        }

        /**
         * Returns the first active date.
         *
         * @param span The span
         * @return PlainDate The first active date
         */
        private PlainDate getFirstActiveDate(StudentEnrollmentSpan span) {
            PlainDate firstActiveDate = null;

            if (span != null && span.getFirstActiveEnrollment() != null) {
                firstActiveDate = span.getFirstActiveEnrollment().getEnrollmentDate();
            }

            return firstActiveDate;
        }

        /**
         * Returns the last active date.
         *
         * @param span The span
         * @return PlainDate The last active date
         */
        private PlainDate getLastActiveDate(StudentEnrollmentSpan span) {
            PlainDate lastActiveDate = null;

            if (span != null && span.getFirstInactiveEnrollment() != null) {
                lastActiveDate = span.getFirstInactiveEnrollment().getEnrollmentDate();
            }

            return lastActiveDate;
        }

        /**
         * Returns the school oid.
         *
         * @param school The school
         * @return String The school oid
         */
        private String getSchoolOid(School school) {
            String schoolOid = null;

            if (school != null) {
                schoolOid = school.getOid();
            }

            return schoolOid;
        }

        /**
         * Returns the secondary schools.
         *
         * @param bean The bean
         * @param studentSchoolMap The student school map
         * @return List The secondary schools
         */
        private List<StudentSchool> getSecondarySchools(X2BaseBean bean,
                                                        Map<String, List<StudentSchool>> studentSchoolMap) {
            List<StudentSchool> secondarySchools = null;
            if (studentSchoolMap.get(bean.getOid()) != null) {
                secondarySchools = removeExcludedSecondarySchools(studentSchoolMap.get(bean.getOid()));
            }

            return secondarySchools;
        }

        /**
         * Returns the total number of entries of all added parameters.
         *
         * @param enrollmentSpans The enrollment spans
         * @param secondarySchools The secondary schools
         * @return int The size
         */
        private int getSize(List<StudentEnrollmentSpan> enrollmentSpans, List<StudentSchool> secondarySchools) {
            int size = enrollmentSpans.size();

            if (secondarySchools != null) {
                size += secondarySchools.size();
            }

            return size;
        }

        /**
         * Returns true if any enrollments of a district student have service in a different
         * district.
         *
         * @param span The span
         * @return boolean True if the district student has service in a different district
         */
        private boolean isDistrictServe(StudentEnrollmentSpan span) {
            boolean removeDistrictServe = false;

            String districtServe =
                    (String) span.getFirstActiveEnrollment().getFieldValueByBeanPath(m_sdData.m_fieldDistrictServe);
            if (!StringUtils.isEmpty(districtServe)) {
                districtServe =
                        m_sdData.lookupReferenceCodeByBeanPath(StudentEnrollment.class, m_sdData.m_fieldDistrictServe,
                                districtServe, ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
            }
            if (!StringUtils.isEmpty(districtServe) && !districtServe.equals(m_sdData.m_districtCode)) {
                removeDistrictServe = true;
            }

            return removeDistrictServe;
        }

        /**
         * Returns true if the span relates to an excluded school.
         *
         * @param span The span
         * @param schoolOid The school oid
         * @return boolean True if the span should be excluded
         */
        private boolean isExcludedSchool(StudentEnrollmentSpan span, String schoolOid) {
            boolean exludedSchool = false;

            StudentEnrollment enrollment = span.getFirstActiveEnrollment();
            if (enrollment != null) {
                String tempSchoolOid = schoolOid == null ? enrollment.getSchoolOid() : schoolOid;
                exludedSchool = isExcluded(tempSchoolOid);
            }

            return exludedSchool;
        }

        /**
         * Returns true if the span is a member.
         *
         * @param span The span
         *
         * @return boolean True if the span is a member
         */
        private boolean isMember(StudentEnrollmentSpan span) {
            PlainDate firstActiveDate = getFirstActiveDate(span);
            PlainDate lastActiveDate = getLastActiveDate(span);
            boolean isMember = true;

            if (firstActiveDate != null && lastActiveDate != null && firstActiveDate.equals(lastActiveDate)) {
                if (!m_memberOnEntryDate && !m_memberOnWithdrawalDate) {
                    isMember = false;
                } else if (m_memberOnEntryDate != m_memberOnWithdrawalDate) {
                    isMember = false;
                }
            }

            return isMember;
        }

        /**
         * Returns true if the span is a no show.
         *
         * @param span The span
         * @return boolean True if no show
         */
        private boolean isNoShow(StudentEnrollmentSpan span) {
            boolean removeDistrictServe = false;

            /*
             * Check if the span is a no-show span. Do not include no-show spans.
             * A no-show span represents an enrollment where the student never showed up.
             * It is identified by a withdrawal code that has NS in the local code of the reference
             * table.
             */
            StudentEnrollment enrollment = span.getFirstInactiveEnrollment();
            if (enrollment != null) {
                String withdrawalCode = enrollment.getEnrollmentCode();
                withdrawalCode =
                        getData().lookupReferenceCodeByRefTbl(m_sdData.m_refTableWithdrawalCode, withdrawalCode,
                                ExportFormatField.ReferenceMapTypeCode.LOCAL.ordinal());
                if (CODE_NO_SHOW.equals(withdrawalCode)) {
                    removeDistrictServe = true;
                }
            }

            return removeDistrictServe;
        }

        /**
         * Removes the excluded secondary schools.
         *
         * @param allSecondarySchools The list of secondary schools
         * @return List The filtered list of secondary schools after exclusions have been filtered
         *         out
         */
        private List<StudentSchool> removeExcludedSecondarySchools(List<StudentSchool> allSecondarySchools) {
            List<StudentSchool> secondarySchools = new ArrayList<StudentSchool>();
            for (StudentSchool secondarySchool : allSecondarySchools) {
                String schoolOId = secondarySchool.getSchoolOid();
                if (!isExcluded(schoolOId)) {
                    secondarySchools.add(secondarySchool);
                }
            }

            return secondarySchools;
        }
    }

    /**
     * class provide method for determine is period cross another period.
     *
     * @author Follett Software Company
     */
    private class CrossPeriod {

        private PlainDate m_end;
        private PlainDate m_start;

        /**
         * Instantiates a new cross period.
         *
         * @param start PlainDate
         * @param end PlainDate
         */
        public CrossPeriod(PlainDate start, PlainDate end) {
            m_start = start;
            m_end = end;
        }

        /**
         * Gets the end.
         *
         * @return the m_end
         */
        public PlainDate getEnd() {
            return m_end;
        }

        /**
         * Gets the string end.
         *
         * @return the m_end
         */
        public String getStringEnd() {
            return m_end == null ? "" : m_end.toString();
        }


        /**
         * Gets the start.
         *
         * @return the m_start
         */
        public PlainDate getStart() {
            return m_start;
        }

        /**
         * Gets the string start.
         *
         * @return the m_start
         */
        public String getStringStart() {
            return m_start == null ? "" : m_start.toString();
        }



        /**
         * @see java.lang.Object#equals(java.lang.Object)
         */
        /*
         *
         */
        @Override
        public boolean equals(Object obj) {
            if (this == obj) {
                return true;
            }
            if (obj == null) {
                return false;
            }
            if (getClass() != obj.getClass()) {
                return false;
            }
            CrossPeriod other = (CrossPeriod) obj;
            if (!getOuterType().equals(other.getOuterType())) {
                return false;
            }
            if (m_end == null) {
                if (other.m_end != null) {
                    return false;
                }
            } else if (!m_end.equals(other.m_end)) {
                return false;
            }
            if (m_start == null) {
                if (other.m_start != null) {
                    return false;
                }
            } else if (!m_start.equals(other.m_start)) {
                return false;
            }
            return true;
        }

        /**
         * method for determine period cross another period.
         *
         * @param period CrossPeriod
         * @return true, if is cross
         */
        public boolean isCross(CrossPeriod period) {
            boolean returnValue = false;
            PlainDate end = period.getEnd();
            PlainDate start = period.getStart();
            if (end == null && m_end == null) {
                returnValue = true;
            } else if (m_start != null && start != null) {
                if (m_end != null && end != null) {
                    if (!m_start.after(end) && !m_end.before(start)) {
                        returnValue = true;
                    }
                }
                // end == null
                else if (m_end != null) {
                    if (!start.after(m_end) && !start.before(m_start)) {
                        returnValue = true;
                    }
                }
                // end != null m_end == null
                else {
                    if (!m_start.after(end) && !m_start.before(start)) {
                        returnValue = true;
                    }
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
        /*
         *
         */
        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + getOuterType().hashCode();
            result = prime * result + ((m_end == null) ? 0 : m_end.hashCode());
            result = prime * result + ((m_start == null) ? 0 : m_start.hashCode());
            return result;
        }

        /**
         * Gets the outer type.
         *
         * @return WA school student
         */
        private WASchoolStudent getOuterType() {
            return WASchoolStudent.this;
        }
    }

    /**
     * Returns the enrollment information from the current enrollment span.
     */
    protected class RetrieveEnrollment implements FieldRetriever {
        private final String PARAM_CODE_WITHDRAW = "CODE_WITHDRAW";
        private final String PARAM_DATE_ENROLLED = "DATE_ENROLLED";
        private final String PARAM_DATE_EXITED = "DATE_EXITED";
        private final String PARAM_DAYS_PRESENT = "DAYS_PRESENT";
        private final String PARAM_SCHOOL_CHOICE = "SCHOOL_CHOICE";
        private final String PARAM_SCHOOL_ID = "SCHOOL_ID";
        private final String PARAM_SCHOOL_RESP = "SCHOOL_RESP";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            WASchoolStudent ssData = (WASchoolStudent) data;
            String parameter = (String) field.getParameter();
            Object entry = ((WASchoolStudentEntity) entity).getStudentSchoolEntry();
            Object value = null;

            if (entry instanceof StudentEnrollmentSpan) {
                StudentEnrollmentSpan span = (StudentEnrollmentSpan) entry;
                if (PARAM_CODE_WITHDRAW.equals(parameter)) {
                    StudentEnrollment enrollment = span.getFirstInactiveEnrollment();
                    if (enrollment != null &&
                            (enrollment.getEnrollmentDate() != null || span.getLastActiveDate() != null)) {
                        value = enrollment.getEnrollmentCode();
                        value = data.lookupReferenceCodeByRefTbl(m_refTableWithdrawalCode, (String) value,
                                ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                    }
                } else if (PARAM_DATE_ENROLLED.equals(parameter)) {
                    PlainDate districtStartDate = data.getOrganization().getCurrentContext().getStartDate();
                    if (getParameter(PARAM_OVERRIDE_DISTRICT_START) != null) {
                        value = districtStartDate;
                    }
                    PlainDate enrollmentDate = span.getFirstActiveEnrollment().getEnrollmentDate();
                    if (value == null || enrollmentDate.after(districtStartDate)) {
                        value = enrollmentDate;
                    }
                } else if (PARAM_DATE_EXITED.equals(parameter)) {
                    StudentEnrollment enrollment = span.getFirstInactiveEnrollment();
                    if (enrollment != null) {
                        value = enrollment.getEnrollmentDate();
                    } else {
                        value = span.getLastActiveDate();
                    }
                } else if (PARAM_DAYS_PRESENT.equals(parameter)) {
                    PlainDate beginDate = m_beginIntervalDate;
                    PlainDate endDate = m_endIntervalDate;
                    if (beginDate.before(span.getFirstActiveDate())) {
                        beginDate = span.getFirstActiveDate();
                    }
                    if (span.getLastActiveDate() != null && span.getLastActiveDate().before(endDate)) {
                        endDate = span.getLastActiveDate();
                    }

                    int cntMembershipDays = span.getMembershipDays();
                    double absentDays = 0;

                    List<StudentAttendance> attendenceRecords = span.getStudentAttendance();

                    if (attendenceRecords != null) {
                        for (StudentAttendance sa : attendenceRecords) {
                            if (sa.getPortionAbsent() != null) {
                                absentDays = absentDays + sa.getPortionAbsent().doubleValue();
                            }
                        }
                    }
                    value = Integer.valueOf(cntMembershipDays - ((int) absentDays));
                } else if (PARAM_SCHOOL_CHOICE.equals(parameter)) {
                    value = span.getFirstActiveEnrollment().getFieldValueByBeanPath(m_fieldSchoolChoice);
                    if (!StringUtils.isEmpty((String) value)) {
                        value = data.lookupReferenceCodeByBeanPath(StudentEnrollment.class, m_fieldSchoolChoice,
                                (String) value,
                                ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                    }
                } else if (PARAM_SCHOOL_ID.equals(parameter)) {
                    value = span.getFirstActiveEnrollment().getFieldValueByBeanPath(m_fieldLocationOveride);
                    if (value == null) {
                        value = span.getFirstActiveEnrollment().getSchool() == null ? null
                                : span.getFirstActiveEnrollment().getSchool().getSchoolId();
                    }
                } else if (PARAM_SCHOOL_RESP.equals(parameter)) {
                    String nonPrimary =
                            (String) span.getFirstActiveEnrollment().getFieldValueByBeanPath(m_fieldSchNonPrimary);
                    if (BooleanAsStringConverter.TRUE.equals(nonPrimary)) {
                        value = Boolean.FALSE;
                    } else {
                        value = Boolean.TRUE;
                    }
                }
            } else if (entry instanceof StudentSchool) {
                StudentSchool ss = (StudentSchool) entry;
                if (PARAM_CODE_WITHDRAW.equals(parameter)) {
                    if (!ss.getEndDate().after(m_reportDate) && ss.getEndDate() != null) {
                        value = ss.getFieldValueByBeanPath(ssData.m_fieldSchoolExitCode);
                        value = data.lookupReferenceCodeByBeanPath(StudentSchool.class, ssData.m_fieldSchoolExitCode,
                                (String) value, ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                    }
                } else if (PARAM_DATE_ENROLLED.equals(parameter)) {
                    value = ss.getStartDate();
                } else if (PARAM_DATE_EXITED.equals(parameter)) {
                    if (!ss.getEndDate().after(m_reportDate)) {
                        value = ss.getEndDate();
                    }
                } else if (PARAM_DAYS_PRESENT.equals(parameter)) {
                    SisStudent student = (SisStudent) entity.getBean();

                    PlainDate beginDate = m_beginIntervalDate;
                    PlainDate endDate = m_endIntervalDate;

                    if (beginDate.before(ss.getStartDate())) {
                        beginDate = ss.getStartDate();
                    }

                    if (ss.getEndDate() != null && ss.getEndDate().before(endDate)) {
                        endDate = ss.getEndDate();
                    }

                    Set<PlainDate> insessionDates = m_helper.getCalendarDays((SisSchool) ss.getSchool(),
                            student.getCalendarCode());
                    if (insessionDates == null && !"Standard".equals(student.getCalendarCode())) {
                        insessionDates = m_helper.getCalendarDays((SisSchool) ss.getSchool(), "Standard");
                    }

                    int cntMembershipDays = 0;
                    double absentDays = 0;
                    if (insessionDates != null) {
                        for (PlainDate date : insessionDates) {
                            if (!date.before(beginDate) && !date.after(endDate)) {
                                cntMembershipDays++;
                            }
                        }
                    }

                    List<StudentAttendance> attendenceRecords = ssData.m_helper.getStudentAttendances(student.getOid());

                    if (attendenceRecords != null) {
                        for (StudentAttendance sa : attendenceRecords) {
                            if (!beginDate.after(sa.getDate()) &&
                                    !endDate.before(sa.getDate()) &&
                                    sa.getPortionAbsent() != null) {
                                absentDays = absentDays + sa.getPortionAbsent().doubleValue();
                            }
                        }
                    }
                    value = Integer.valueOf(cntMembershipDays - ((int) absentDays));
                } else if (PARAM_SCHOOL_CHOICE.equals(parameter)) {
                    value = "0"; // "0" = Not Applicable
                } else if (PARAM_SCHOOL_ID.equals(parameter)) {
                    value = ss.getSchool() == null ? null : ss.getSchool().getSchoolId();
                } else if (PARAM_SCHOOL_RESP.equals(parameter)) {
                    value = Boolean.FALSE; // Not primary school
                }
            }

            return value;
        }
    }


    /**
     * Validate the exit.
     */
    protected class ValidateExit implements FieldValidator {
        private static final String FIELD_SCHOOL_ENROLLMENT_DATE = "SchoolEnrollmentDate";
        private static final String VAL_ID = "EXIT-VAL";
        private static final String CAL_PARAM_DATE = "DATE_EXITED";
        private static final String FIELD_NAME_EXIT_DATE = "SchoolExitDate";
        private static final String FIELD_NAME_EXIT_CODE = "SchoolWithdrawalCode";
        private List<String> m_wCodees = new ArrayList<String>(Arrays.asList("G0", "GA", "GB", "C1", "C2", "T0", "T1",
                "T2", "T3", "D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9", "D0", "U1", "U2", "U3", "ZZ"));

        private static final String CAL_PARAM_CODE = "CODE_WITHDRAW";

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
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            // Element C08
            // Element C09
            // if c08 provided - c09 should be provided too and conversely
            String neighborFieldValue = null;
            String parameter = (String) field.getParameter();
            if (!StringUtils.isEmpty(value)) {

                String neighborName = null;
                if (CAL_PARAM_DATE.equals(parameter)) {
                    neighborName = FIELD_NAME_EXIT_CODE;
                    neighborFieldValue = entity.getFieldValue(FIELD_NAME_EXIT_CODE);
                } else if (CAL_PARAM_CODE.equals(parameter)) {
                    neighborName = FIELD_NAME_EXIT_DATE;
                    neighborFieldValue = entity.getFieldValue(FIELD_NAME_EXIT_DATE);
                }

                if (StringUtils.isEmpty(neighborFieldValue)) {
                    errors.add(new StateReportValidationError(entity, field,
                            "If " + field.getFieldId() + " is not empty -" + neighborName + " is require.",
                            field.getFieldId() + " - " + STYLE_BOLD + value + STYLE_END + "." + neighborName + " - "
                                    + STYLE_BOLD + neighborFieldValue + STYLE_END));


                }

                // Element C08
                // C08 should be >= C06
                if (CAL_PARAM_DATE.equals(parameter)) {
                    // C08
                    PlainDate exitDate = formatDate(value);
                    String schoolEnrDateString = entity.getFieldValue(FIELD_SCHOOL_ENROLLMENT_DATE);
                    // C06
                    PlainDate schoolEnrDate = formatDate(schoolEnrDateString);
                    if (exitDate != null && schoolEnrDate != null && schoolEnrDate.after(exitDate)) {
                        errors.add(new StateReportValidationError(entity, field,
                                field.getFieldId() + " must be equal to or greater than the date in -"
                                        + FIELD_SCHOOL_ENROLLMENT_DATE,
                                field.getFieldId() + " - " + STYLE_BOLD + value + STYLE_END + "." + "Sch Enr Date"
                                        + " - " + STYLE_BOLD + schoolEnrDateString + STYLE_END));
                    }
                }
            }

            // Element C09 (Use the numeric character 0 for zero, not the letter O.)
            if (CAL_PARAM_CODE.equals(parameter) && !StringUtils.isEmpty(value) && !m_wCodees.contains(value)) {
                errors.add(new StateReportValidationError(entity, field,
                        field.getFieldId()
                                + " must have valid value. Use the numeric character 0 for zero, not the letter O",
                        field.getFieldId() + " - " + STYLE_BOLD + value + STYLE_END + "." + "list of valid values:"
                                + " - " + STYLE_BOLD + m_wCodees + STYLE_END));
            }

            return errors;
        }
    }

    /**
     * validate DaysPresent field.
     *
     * @author Follett Software Company
     */
    protected class ValidatePresentDatets implements FieldValidator {
        private static final String VAL_ID = "PRESENT-VAL";

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
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            if (!StringUtils.isEmpty(value) && StringUtils.isNumeric(value)) {
                int presentDates = Integer.parseInt(value);
                if (!(presentDates >= 0 && presentDates <= 366)) {
                    errors.add(new StateReportValidationError(entity, field,
                            field.getFieldId() + " should be 0 - 366",
                            field.getFieldId() + " - " + STYLE_BOLD + value + STYLE_END));
                }
            }

            return errors;
        }
    }

    /**
     * Validate the primary school.
     */
    protected class ValidatePrimary implements FieldValidator {
        private Map<String, List<CrossPeriod>> m_beansUniquePrimaryPeriods = new HashMap<String, List<CrossPeriod>>();
        private static final String VAL_ID = "PRIMARY-VAL";
        private static final String FIELD_NAME_EXIT_DATE = "SchoolExitDate";
        private static final String FIELD_NAME_START_DATE = "SchoolEnrollmentDate";



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
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            String studentOid = entity.getBean().getOid();
            List<CrossPeriod> uniquePrimaryPeriods = m_beansUniquePrimaryPeriods.get(studentOid);
            if (uniquePrimaryPeriods == null) {
                uniquePrimaryPeriods = new ArrayList<CrossPeriod>();
                m_beansUniquePrimaryPeriods.put(studentOid, uniquePrimaryPeriods);
            }

            // Element C10
            // Span should be crossed (Span - element C6 and C 8)
            if (!StringUtils.isEmpty(value) && "Y".equals(value)) {
                String entryField = entity.getFieldValue(FIELD_NAME_START_DATE);
                String exitField = entity.getFieldValue(FIELD_NAME_EXIT_DATE);

                PlainDate entryDate = formatDate(entryField);
                PlainDate exitdate = formatDate(exitField);


                CrossPeriod period = new CrossPeriod(entryDate, exitdate);
                for (CrossPeriod oldPeriod : uniquePrimaryPeriods) {
                    if (oldPeriod.isCross(period)) {
                        errors.add(new StateReportValidationError(entity, field,
                                "Student must have only one primary school during the academic school year. If student move from one school to another school - entry and exit dates should not overlap",
                                "current period = " + STYLE_BOLD + entryDate + " - " + exitdate + STYLE_END
                                        + " overlap preiod - " + STYLE_BOLD + oldPeriod.getStringStart() + " - "
                                        + oldPeriod.getStringEnd() + STYLE_END));
                    }
                }
                uniquePrimaryPeriods.add(period);


            }



            return errors;
        }

    }


    /*
     * Constants: Aliases, Parameters
     */
    protected static final String ALIAS_DISTRICT_ID = "DOE DISTRICT ID";
    protected static final String ALIAS_DISTRICT_SERVE = "DOE DISTRICT SERVE";
    protected static final String ALIAS_NON_PRIMARY = "DOE NON PRIMARY";
    protected static final String ALIAS_SCHOOL_CHOICE = "DOE SCHOOL CHOICE";
    protected static final String ALIAS_SCHOOL_CODE = "DOE SCHOOL ID";
    protected static final String ALIAS_SCHOOL_EXIT_CODE = "DOE SCHOOL EXIT CODE";
    protected static final String ALIAS_EXCLUDE_SCHOOL = "DOE EXCLUDE SKL";
    protected static final String ALIAS_LOCATION_OVERRIDE = "DOE LOC OVERRIDE";
    protected static final String CODE_NO_SHOW = "NS";
    protected static final String DATE_FORMAT = "MM/dd/yyyy";
    protected static final String PARAM_REPORT_DATE = "reportDate";
    protected static final String PARAM_EXCLUDE_SCHOOL = "excludeSchool";
    protected static final String PARAM_OVERRIDE_DISTRICT_START = "overrideDistrictStart";

    /*
     * Instance variables
     */
    protected SimpleDateFormat m_dateFormatt = new SimpleDateFormat(DATE_FORMAT);
    protected PlainDate m_beginIntervalDate;
    protected String m_districtCode;
    protected PlainDate m_endIntervalDate;
    protected String m_fieldDistrictId;
    protected String m_fieldDistrictServe;
    protected String m_fieldSchNonPrimary;
    protected String m_fieldSchoolChoice;
    protected String m_fieldSchoolCode;
    protected String m_fieldSchoolExitCode;
    protected StudentHistoryHelper m_helper;
    protected String m_refTableWithdrawalCode;
    protected String m_fieldLocationOveride;
    protected PlainDate m_reportDate;
    protected Map<String, List<StudentSchool>> m_studentSchoolMap;
    protected String m_excludeSchool;
    protected static Map m_excludeSchoolMap;

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
        m_reportDate = (PlainDate) getParameter(PARAM_REPORT_DATE);
        m_beginIntervalDate = getOrganization().getCurrentContext().getStartDate();
        m_endIntervalDate = m_reportDate;
        if (getOrganization().getCurrentContext().getEndDate().before(m_endIntervalDate)) {
            m_endIntervalDate = getOrganization().getCurrentContext().getEndDate();
        }

        initializeFields();

        // had to initialize to null as it is made static for allowing it to be accessed for
        // removing secondary schools.
        m_excludeSchoolMap = null;

        if (((Boolean) getParameter(PARAM_EXCLUDE_SCHOOL)).booleanValue()) {
            loadSchoolExcludeMap();
        }

        /*
         * Build helper object.
         */
        m_helper = new StudentHistoryHelper(this);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_endIntervalDate);

        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {
            // Set the query to be used for student selection.
            setQuery(m_helper.getStudentQuery(false));

            setEntityClass(WASchoolStudentEntity.class);

            loadStudentSchools();

            // Build a map of calculations/retrievers
            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put("SS-ENROLLMENT", new RetrieveEnrollment());
            super.addCalcs(calcs);

            HashMap<String, FieldValidator> validators = new HashMap<String, FieldValidator>();
            validators.put(ValidateExit.VAL_ID, new ValidateExit());
            validators.put(ValidatePrimary.VAL_ID, new ValidatePrimary());
            validators.put(ValidatePresentDatets.VAL_ID, new ValidatePresentDatets());

            super.addValidators(validators);
        }
    }

    /**
     * Checks if is excluded.
     *
     * @param schoolOid String
     * @return true, if is excluded
     */
    public static boolean isExcluded(String schoolOid) {
        boolean returnValue = false;
        if (m_excludeSchoolMap != null) {
            returnValue = m_excludeSchoolMap.containsKey(schoolOid);
        }

        return returnValue;
    }

    /**
     * format date from string to plainDate.
     *
     * @param date String
     * @return PlainDate
     */
    PlainDate formatDate(String date) {
        PlainDate returnDate = null;
        if (!StringUtils.isEmpty(date)) {
            try {
                returnDate = new PlainDate(m_dateFormatt.parse(date));
            } catch (ParseException e) {
                // Nothing to do

            }
        }
        return returnDate;
    }

    /**
     * Lookup field aliases and paths.
     */
    private void initializeFields() {
        m_fieldDistrictId = translateAliasToJavaName(ALIAS_DISTRICT_ID, true);
        m_fieldDistrictServe = translateAliasToJavaName(ALIAS_DISTRICT_SERVE, true);
        m_fieldSchNonPrimary = translateAliasToJavaName(ALIAS_NON_PRIMARY, true);
        m_fieldSchoolCode = translateAliasToJavaName(ALIAS_SCHOOL_CODE, true);
        m_fieldSchoolChoice = translateAliasToJavaName(ALIAS_SCHOOL_CHOICE, true);
        m_fieldSchoolExitCode = translateAliasToJavaName(ALIAS_SCHOOL_EXIT_CODE, true);
        m_excludeSchool = translateAliasToJavaName(ALIAS_EXCLUDE_SCHOOL, true);
        m_fieldLocationOveride = translateAliasToJavaName(ALIAS_LOCATION_OVERRIDE, true);

        m_districtCode = (String) getOrganization().getFieldValueByBeanPath(m_fieldDistrictId);

        m_refTableWithdrawalCode = PreferenceManager.getPreferenceValue(getOrganization(),
                SisPreferenceConstants.ENROLLMENT_WITHDRAWAL_CODES);
    }

    /**
     * Load a map of student secondary school associations.
     */
    private void loadStudentSchools() {
        X2Criteria studentCriteria = m_helper.getStudentCriteria();
        SubQuery studentSubquery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria);
        X2Criteria criteria = new X2Criteria();
        criteria.addIn(StudentSchool.COL_STUDENT_OID, studentSubquery);
        criteria.addEqualTo(StudentSchool.COL_DISTRICT_CONTEXT_OID, getOrganization().getCurrentContextOid());
        criteria.addEqualTo(StudentSchool.COL_TYPE, Integer.valueOf(StudentSchool.SECONDARY));
        criteria.addLessOrEqualThan(StudentSchool.COL_START_DATE, m_reportDate);
        criteria.addNotEmpty(StudentSchool.REL_SCHOOL + ModelProperty.PATH_DELIMITER + m_fieldSchoolCode,
                getBroker().getPersistenceKey());

        QueryByCriteria query = new QueryByCriteria(StudentSchool.class, criteria);
        query.addOrderBy(StudentSchool.COL_STUDENT_OID, true);
        query.addOrderBy(StudentSchool.COL_START_DATE, true);
        m_studentSchoolMap = getBroker().getGroupedCollectionByQuery(query, StudentSchool.COL_STUDENT_OID, 100);
    }

    /**
     * Load school exclude map.
     */
    private void loadSchoolExcludeMap() {
        X2Criteria schoolCriteria = new X2Criteria();
        schoolCriteria.addEqualTo(m_excludeSchool, BooleanAsStringConverter.TRUE);
        BeanQuery query = new BeanQuery(School.class, schoolCriteria);
        m_excludeSchoolMap = getBroker().getGroupedCollectionByQuery(query, X2BaseBean.COL_OID, 128);
    }
}
