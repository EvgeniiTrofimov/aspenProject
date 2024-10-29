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

package com.x2dev.procedures.statereporting.nh;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.Race;
import com.follett.fsc.core.k12.beans.RecordSet;
import com.follett.fsc.core.k12.beans.RecordSetKey;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.InvalidDictionaryIdException;
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
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.SchoolScheduleContext;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAttendance;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.sis.model.business.EnrollmentManager;
import com.x2dev.sis.model.business.EnrollmentSnapshot;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.types.PlainDate;
import java.lang.reflect.InvocationTargetException;
import java.text.SimpleDateFormat;
import java.util.*;
import org.apache.commons.beanutils.PropertyUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * This class implements the data export for New Hampshire's i4see Enrollment export.
 *
 * @author X2 Development Corporation
 */
public class I4SeeEnrollmentCATE extends StateReportData {
    /**
     * Implementation of StateReportEntity to be used by the I4SeeEnrollment export.
     * This must be a public static inner class with a public no argument
     * constructor so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class I4SeeEntity extends StateReportEntity {
        /*
         * Enrollment record constants
         */
        // private static final String ENROLLMENT_H1_CODE = "H1";

        // /*
        // * List to hold the entry and withdrawal enrollment records
        // */
        // List<KeyValuePair<StudentEnrollment, StudentEnrollment>> m_enrollments;

        /**
         * Cached values for retrievers to share.
         */
        private EnrollmentSnapshot m_snapshot;

        /**
         * List of StudentProgramParticipation that reflect this students participation in CATE.
         */
        private List<StudentProgramParticipation> m_programs;

        /**
         * Placeholders for calculated unmapped fields. These can be written back to the database
         * in postProcess if update flag is set. Also, holds some calculated values that have
         * been overridden with default or related values.
         *
         * Map key should be field alias constant.
         */
        private Map<String, Object> m_updateValues = null;

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

            FieldDefinition field = getData().getFieldDefinition(I4SEE_1764_CATE_DAYS_IN_ATTEND_S1);
            // FieldDefinition field2 =
            // getData().getFieldDefinition(I4SEE_1765_CATE_DAYS_IN_ATTEND_S2);

            boolean requireMemberDay = ((Boolean) getData().getParameter(REQUIRE_MEMBER_DAY_PARAM)).booleanValue();

            /*
             * Get membership days parameter
             */
            double membershipCountAsDouble = 0;

            /*
             * Get membership count
             */
            I4SeeEnrollmentCATE i4seeData = (I4SeeEnrollmentCATE) getData();
            String membershipCount = i4seeData.getMembershipDays(this, SEMESTER.FY, false);

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

            return error;
        }

        /**
         * Returns the StudentProgramParticipation that represents the current report row.
         *
         * @return StudentProgramParticipation
         */
        public StudentProgramParticipation getCurrentProgram() {
            StudentProgramParticipation program = null;
            int index = getCurrentRow();
            if (index >= 0 && index < m_programs.size()) {
                program = m_programs.get(index);
            }
            return program;
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
         * @return boolean
         */
        // public StudentEnrollment getEntry()
        // {
        // StudentEnrollment entry = null;
        // int index = getCurrentRow();
        //
        // if (m_enrollments != null && index >= 0 && index < m_enrollments.size())
        // {
        // KeyValuePair<StudentEnrollment, StudentEnrollment> pair = m_enrollments.get(index);
        // entry = pair.getKey();
        // }
        //
        // return entry;
        // }

        /**
         * returns a boolean indicating if the student is a non-graduating senior
         * 
         * @return boolean
         */
        public boolean getIsNonGraduatingSenior() {
            String promotedInd = getFieldValue(I4SEE_510_PROMOTED_IND);
            return ("12".equals(getFieldValue(I4SEE_400_GRADE)) &&
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
            I4SeeEnrollmentCATE i4seeData = (I4SeeEnrollmentCATE) data;
            m_programs = i4seeData.getProgram(student.getOid());
            setRowCount(m_programs.size());
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
                    // Converter integerConverter =
                    // ConverterFactory.getConverterForClass(Converter.INTEGER_CONVERTER,
                    // Locale.getDefault(), true);

                    FieldDefinition field;
                    field = getData().getFieldDefinition(I4SEE_1764_CATE_DAYS_IN_ATTEND_S1);
                    PropertyUtils.setProperty(getBean(), field.getBeanPath(),
                            getFieldValue(I4SEE_1764_CATE_DAYS_IN_ATTEND_S1));

                    field = getData().getFieldDefinition(I4SEE_1765_CATE_DAYS_IN_ATTEND_S2);
                    PropertyUtils.setProperty(getBean(), field.getBeanPath(),
                            getFieldValue(I4SEE_1765_CATE_DAYS_IN_ATTEND_S2));

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
         * Returns a Collection of Student Program Participation records.
         *
         * @param student SisStudent
         * @param reportDate PlainDate
         * @param field FieldDefinition
         * @return Collection of StudentEnrollment records for any date on or after the start of
         *         school
         */
        // private LinkedList<StudentProgramParticipation>
        // getCATEStudentProgramParticipation(Student student)
        // {
        // LinkedList<StudentProgramParticipation> programs = new
        // LinkedList<StudentProgramParticipation>();
        //
        // Criteria criteria = new Criteria();
        // criteria.addEqualTo(StudentProgramParticipation.COL_STUDENT_OID, student.getOid());
        // criteria.addEqualTo(StudentProgramParticipation.COL_PROGRAM_CODE,
        // I4SEE_CATE_PROGRAM_IDENTIFIER);
        //
        // QueryByCriteria query = new QueryByCriteria(StudentProgramParticipation.class, criteria);
        // query.addOrderByDescending(StudentProgramParticipation.COL_START_DATE);
        //
        // programs.addAll(getData().getBroker().getCollectionByQuery(query));
        //
        // return programs;
        // }

        /**
         * Adds the most recent enrollment prior to the start of the school year
         *
         * @param enrollments
         */
        // private void
        // addMostRecentEnrollment(List<KeyValuePair<StudentEnrollment,StudentEnrollment>>
        // enrollments)
        // {
        // EnrollmentManager manager = new EnrollmentManager(getData().getBroker(),
        // getData().getPrivilegeSet(), getData().getOrganization());
        // List<StudentEnrollment> orderedEnrollments = manager.getOrderedEnrollment((Student)
        // getBean(), null,
        // DateUtils.add(getData().getOrganization().getCurrentContext().getStartDate(),-1), null ,
        // false);
        // Object value = null;
        // while(orderedEnrollments.size() > 0)
        // {
        // if (orderedEnrollments.get(0).getEnrollmentType().equals(StudentEnrollment.ENTRY)
        // && (orderedEnrollments.get(0).getEnrollmentCode() == null ||
        // !orderedEnrollments.get(0).getEnrollmentCode().startsWith("X2")))
        // {
        // if (enrollments.size() > 0)
        // {
        // value = enrollments.get(0).getValue();
        // enrollments.remove(0);
        // }
        // enrollments.add(0,new KeyValuePair(orderedEnrollments.get(0), value));
        // break;
        // }
        // orderedEnrollments.remove(0);
        // }
        // }

        /**
         * Returns a Collection of Student Enrollment beans for the passed student since the first
         * day
         * of the school year.
         *
         * @param student
         * @param summerDates - true if collecting summer information
         *        NOT USED for B-O-Y report.
         *
         * @return Collection of StudentEnrollment records for any date on or after the start of
         *         school
         */
        // private Collection<StudentEnrollment> getEnrollments(Student student, boolean
        // summerDates)
        // {
        // Collection<StudentEnrollment> enrollments = new LinkedList<StudentEnrollment>();
        //
        // PlainDate startDate = null;
        // PlainDate endDate = null;
        //
        // /*
        // * B-O-Y report includes all dates from start of summer up to the report date
        // */
        // if ((Integer) getData().getParameter(REPORT_TYPE)== REPORT_TYPE_BOY)
        // {
        // startDate = (PlainDate) getData().getParameter(SUMMER_START_DATE_PARAM);
        // endDate = (PlainDate) getData().getParameter(REPORT_DATE_PARAM);
        // }
        // if (summerDates)
        // {
        // startDate = (PlainDate) getData().getParameter(SUMMER_START_DATE_PARAM);
        // endDate = (PlainDate) getData().getParameter(SUMMER_END_DATE_PARAM);
        // }
        // else
        // {
        // startDate = getData().getOrganization().getCurrentContext().getStartDate();
        // endDate = (PlainDate) getData().getParameter(REPORT_DATE_PARAM);
        // }
        //
        // Criteria criteria = new Criteria();
        // criteria.addEqualTo(StudentEnrollment.COL_STUDENT_OID, student.getOid());
        // criteria.addNotEqualTo(StudentEnrollment.COL_ENROLLMENT_CODE, ENROLLMENT_H1_CODE);
        // criteria.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, startDate);
        // criteria.addLessOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, endDate);
        //
        // /*
        // * Only include schools that are CATE (Career And Technical Education)
        // */
        // String javaName = translateAliasToJavaName(I4SEE_CATE_INDICATOR);
        // if (!StringUtils.isEmpty(javaName))
        // {
        // criteria.addEqualTo(StudentEnrollment.REL_SCHOOL+PATH_DELIMITER+javaName, true);
        // }
        //
        // QueryByCriteria query = new QueryByCriteria(StudentEnrollment.class, criteria);
        // query.addOrderByAscending(StudentEnrollment.COL_ENROLLMENT_DATE);
        // query.addOrderByAscending(StudentEnrollment.COL_TIMESTAMP);
        //
        // enrollments = getData().getBroker().getCollectionByQuery(query);
        //
        // if (!summerDates)
        // {
        // /*
        // * Go through records and remove any that should not be reported
        // *
        // * If a student has and E1 and a W2 on 8/26/08 along with an R2 at another
        // * school on 8/26/08, then the E1 and W2 should be ignored and only the
        // * active R2 (which will have to be corrected to an E1) should be reported
        // *
        // * If a student has an E1 and a W-anything on 8/26/08, and is NOT active at
        // * any other school, then only the withdrawal should be reported – not the
        // * entry code.
        // *
        // * Translation: Do not report on any E records on the first date. A lone withdrawal
        // * record will generate a E record in the export for the first date. If a W is
        // * encountered, only report it if it is not followed by an E record on the same date.
        // */
        // Collection<StudentEnrollment> recordsToRemove = new LinkedList<StudentEnrollment>();
        // StudentEnrollment lastEnrollment = null;
        //
        // for (StudentEnrollment enrollment : enrollments)
        // {
        // if (enrollment.getEnrollmentDate().equals(startDate))
        // {
        // if (StudentEnrollment.WITHDRAWAL.equals(enrollment.getEnrollmentType()))
        // {
        // lastEnrollment = enrollment;
        // }
        // else
        // {
        // // Check to see if the last record was an enrollment
        // if (lastEnrollment != null &&
        // StudentEnrollment.WITHDRAWAL.equals(lastEnrollment.getEnrollmentType()))
        // {
        // // Ignore the last withdrawal
        // recordsToRemove.add(lastEnrollment);
        // lastEnrollment = null;
        // }
        // }
        // }
        // }
        //
        // enrollments.removeAll(recordsToRemove);
        // }
        //
        // return enrollments;
        // }

        /**
         * Returns the enrollment snapshot for the student as of the report date.
         *
         * @param student
         *
         * @return EnrollmentSnapshot
         */
        private EnrollmentSnapshot getSnapshot(SisStudent student, PlainDate reportDate, FieldDefinition field) {
            EnrollmentSnapshot snapshot = new EnrollmentSnapshot(student, reportDate, getData().getBroker());

            if (!SUPPRESS_ENROLLMENT_WARNINGS) {
                if (!snapshot.isPrecise()) {
                    addRetrievalError(I4SEE_1700_CATE_ENROLLMENT_STATUS,
                            new StateReportValidationError(this, field,
                                    "WARNING: Enrollment information (enrollment status, school, and/or YOG) is not precise",
                                    ""));
                }
            }

            return snapshot;
        }

        /**
         * Translates an alias into a Java bean path name.
         *
         * @param alias
         *
         * @return String
         */
        // private String translateAliasToJavaName(String alias)
        // {
        // String javaName = null;
        //
        // DataDictionary dictionary = DataDictionary.getDistrictDictionary();
        // DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(alias);
        // if (field != null)
        // {
        // javaName = field.getJavaName();
        // }
        //
        // return javaName;
        // }
    }

    /**
     * The Enum SEMESTER.
     */
    public enum SEMESTER {
        FY, S1, S2
    }

    /**
     * The Class Retrieve150SSN.
     */
    protected class Retrieve150SSN implements FieldRetriever {
        private String m_javaName = null;

        /**
         * Constructor.
         *
         * @param javaName String
         */
        public Retrieve150SSN(String javaName) {
            m_javaName = javaName;
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
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();
            String socialSecurityNumber = m_integerConverter.javaToString(WebUtils.getProperty(student, m_javaName));

            // if the social security field is not set (e.g. 11 characters), then use the default
            if (StringUtils.isEmpty(socialSecurityNumber) ||
                    (socialSecurityNumber != null && socialSecurityNumber.length() != 11)) {
                socialSecurityNumber = "000-00-0000";
            }

            return socialSecurityNumber;
        }

    }

    /**
     * Returns fields from the newest CATE Student Program Participation record.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveProgramParticipationFields implements FieldRetriever {
        private final List fields = Arrays.asList(
                I4SEE_1500_SAU_NUMBER_SEND, // 0
                I4SEE_1510_DISTRICT_NUMBER_SEND, // 1
                I4SEE_1520_SCHOOL_NUMBER_SEND, // 2
                I4SEE_1600_SAU_NUMBER_RCV, // 3
                I4SEE_1610_DISTRICT_NUMBER_RCV // 4
        );

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
            I4SeeEntity i4seeEntity = (I4SeeEntity) entity;
            String javaName = field.getBeanPath().substring(field.getBeanPath().lastIndexOf('.') + 1);
            String value = null;

            StudentProgramParticipation program = i4seeEntity.getCurrentProgram();

            if (program != null) {
                value = (String) WebUtils.getProperty(program, javaName);
            }

            /*
             * If the data wasn't found in the Program record, go to the primary school and get it
             * there
             */
            if (value == null) {
                value = getValueFromPrimarySchool(entity, field);
            }

            return value;
        }

        /**
         * Gets the value from primary school.
         *
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return String
         */
        private String getValueFromPrimarySchool(StateReportEntity entity,
                                                 FieldDefinition field) {
            String value = null;
            String javaName = null;
            SisStudent student = (SisStudent) entity.getBean();
            SisSchool school = student.getSchool();

            if (field.getParameter() != null && fields.contains(field.getParameter())) {
                try {
                    switch (fields.indexOf(field.getParameter())) {
                        case 0: // I4SEE_1500_SAU_NUMBER_SEND
                        case 3: // I4SEE_1600_SAU_NUMBER_RCV
                            javaName = translateAliasToJavaName(I4SEE_030_SAU_NUMBER, true);
                            value = (String) WebUtils.getProperty(school.getOrganization1(), javaName);
                            break;

                        case 1: // I4SEE_1510_DISTRICT_NUMBER_SEND
                        case 4: // I4SEE_1610_DISTRICT_NUMBER_RCV
                            javaName = translateAliasToJavaName(I4SEE_040_DISTRICT_NUMBER, true);
                            value = (String) WebUtils.getProperty(school.getOrganization1(), javaName);
                            break;

                        case 2: // I4SEE_1520_SCHOOL_NUMBER_SEND
                            javaName = translateAliasToJavaName(I4SEE_050_SCHOOL_ID, true);
                            value = (String) WebUtils.getProperty(school, javaName);
                            break;
                        default:
                            break;
                    }
                } catch (X2BaseException e) {
                    entity.addRetrievalError(field.getFieldId(),
                            new StateReportValidationError(entity,
                                    field,
                                    (String) field.getParameter(),
                                    VALIDATION_INVALID_VALUE));
                }
            }

            return value;
        }
    }

    // protected class Retrieve1620ReceivingSchoolNumber implements FieldRetriever
    // {
    // public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition
    // field)
    // {
    // return getParameter("receivingSchoolNumber");
    // }
    // }


    /**
     * Retrieve days in S1 attendance for the student. If the count is zero, use the 555
     * placeholder.
     *
     * @author X2 Development Corporation
     */
    protected class Retrieve1764DaysInAttendanceS1 implements FieldRetriever {

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

            // field not used for B-O-Y report
            // if ( m_reportType.equals(REPORT_TYPE_EOY))
            {
                attendanceCount = getMembershipDays((I4SeeEntity) entity, SEMESTER.S1, true);

                // String membershipCount = getMembershipDays((I4SeeEntity)entity, SEMESTER.S1);
                // attendanceCount = retrieveDaysInAttendance(entity, field, membershipCount);
            }
            return attendanceCount;
        }
    }
    /**
     * Retrieve days in S2 attendance for the student. If the count is zero, use the 555
     * placeholder.
     *
     * @author X2 Development Corporation
     */
    protected class Retrieve1765DaysInAttendanceS2 implements FieldRetriever {

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

            // field not used for B-O-Y report
            // if ( m_reportType.equals(REPORT_TYPE_EOY))
            {
                attendanceCount = getMembershipDays((I4SeeEntity) entity, SEMESTER.S2, true);

                // String membershipCount = getMembershipDays((I4SeeEntity)entity, SEMESTER.S2);
                // attendanceCount = retrieveDaysInAttendance(entity, field, membershipCount);
            }
            return attendanceCount;
        }
    }

    /**
     * Returns the entry code for the student.
     */
    protected class Retrieve240_260_EntryExitCode implements FieldRetriever {
        private String m_javaName;

        /**
         * Instantiates a new retrieve 240 260 entry exit code.
         *
         * @param javaName String
         */
        Retrieve240_260_EntryExitCode(String javaName) {
            m_javaName = javaName;
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
            String entryExitCode = null;

            I4SeeEntity i4seeEntity = (I4SeeEntity) entity;
            StudentProgramParticipation program = i4seeEntity.getCurrentProgram();

            if (program != null) {
                String code = null;
                try {
                    code = (String) WebUtils.getProperty(program, m_javaName);
                } catch (X2BaseException e) {
                    e.printStackTrace();
                }
                if (code != null) {
                    entryExitCode = getStateCode(field.getFieldId(), code);
                }
            }

            return entryExitCode;
        }
    }


    /**
     * Returns the entry code for the student.
     */
    protected class Retrieve230_250_EntryExitDate implements FieldRetriever {
        private String m_javaName;

        /**
         * Instantiates a new retrieve 230 250 entry exit date.
         *
         * @param javaName String
         */
        Retrieve230_250_EntryExitDate(String javaName) {
            m_javaName = javaName;
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
            Date entryExitDate = null;

            I4SeeEntity i4seeEntity = (I4SeeEntity) entity;
            StudentProgramParticipation program = i4seeEntity.getCurrentProgram();

            if (program != null) {
                PlainDate date = null;
                try {
                    date = (PlainDate) WebUtils.getProperty(program, m_javaName);
                } catch (X2BaseException e) {
                    e.printStackTrace();
                }
                entryExitDate = date;
            }

            return entryExitDate;
        }
    }

    /**
     * Returns the sau number for the given student.
     * <p>
     * For non-archived students we use the snapshot. For archived students (i.e., summer
     * withdrawals) we use the school from the most recent enrollment record during summer vacation
     * (if we didn't do this then the export would show the history school's district SAU number).
     */
    // protected class Retrieve030SauNumber implements FieldRetriever
    // {
    // private String m_javaName = null;
    //
    // /**
    // * Constructor
    // *
    // * @param javaName
    // */
    // public Retrieve030SauNumber(String javaName)
    // {
    // m_javaName = javaName;
    // }
    //
    // /**
    // * @see
    // com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
    // com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
    // com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
    // */
    // public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition
    // field) throws X2BaseException
    // {
    // Student student = (Student) entity.getBean();
    // String sauNumber = null;
    //
    // if (m_schoolMap.get(student.getSchoolOid()).getArchiveIndicator())
    // {
    // List enrollments = m_enrollmentManager.getOrderedEnrollment(student,
    // (PlainDate) getParameter(SUMMER_START_DATE_PARAM),
    // (PlainDate) getParameter(SUMMER_END_DATE_PARAM),
    // null,
    // false);
    //
    // if (!enrollments.isEmpty())
    // {
    // StudentEnrollment withdrawal = (StudentEnrollment) enrollments.get(0);
    // sauNumber = (String) WebUtils.getProperty(withdrawal.getSchool().getOrganization1(),
    // m_javaName);
    // }
    // }
    // else
    // {
    // sauNumber = (String) WebUtils.getProperty(getOrganization(), m_javaName);
    // }
    //
    // return sauNumber;
    // }
    // }

    /**
     * Returns the district number for the given student.
     * <p>
     * For non-archived students we use the snapshot. For archived students (i.e., summer
     * withdrawals) we use the school from the most recent enrollment record during summer vacation
     * (if we didn't do this then the export would show the history school's district district
     * number).
     *
     * @throws X2BaseException
     */
    protected class Retrieve1610DistrictNumber implements FieldRetriever {

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
            String districtNumber = null;
            String javaName = field.getBeanPath().substring(field.getBeanPath().lastIndexOf('.') + 1);

            if (m_schoolMap.get(student.getSchoolOid()).getArchiveIndicator()) {
                List enrollments = m_enrollmentManager.getOrderedEnrollment(student,
                        (PlainDate) getParameter(SUMMER_START_DATE_PARAM),
                        (PlainDate) getParameter(SUMMER_END_DATE_PARAM),
                        null,
                        false);

                if (!enrollments.isEmpty()) {
                    StudentEnrollment withdrawal = (StudentEnrollment) enrollments.get(0);
                    districtNumber = (String) WebUtils.getProperty(withdrawal.getSchool().getOrganization1(), javaName);
                }
            } else {
                districtNumber = (String) WebUtils.getProperty(getOrganization(), javaName);
            }

            return districtNumber;
        }
    }

    /**
     * Returns the full day percent date for the student.
     *
     * @author X2 Development Corporation
     */
    protected class Retrieve405FullDayPercent implements FieldRetriever {
        private String m_javaName = null;

        /**
         * Constructor.
         *
         * @param javaName String
         */
        public Retrieve405FullDayPercent(String javaName) {
            m_javaName = javaName;
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
            String fullDayPercent = (String) WebUtils.getProperty(entity.getBean(), m_javaName);
            int fullDay = 0;

            // field not used for B-O-Y report
            if (m_reportType == REPORT_TYPE_EOY &&
                    fullDayPercent != null && StringUtils.isNumeric(fullDayPercent)) {
                Double percent = Double.valueOf(fullDayPercent);
                fullDay = percent.intValue();
            }

            return String.valueOf(fullDay);
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
            SisStudent student = (SisStudent) entity.getBean();
            String gradeLevel = student.getGradeLevel();
            String stateCode = lookupStateValue(SisStudent.class, SisStudent.COL_GRADE_LEVEL, gradeLevel);
            if (StringUtils.isEmpty(stateCode)) {
                entity.addRetrievalError(field.getFieldId(),
                        new StateReportValidationError(entity,
                                field,
                                I4SEE_400_GRADE,
                                VALIDATION_INVALID_VALUE));
            }

            return stateCode;
        }
    }

    /**
     * The Class Validate1700CATEEnrollmentStatus.
     */
    protected class Validate1700CATEEnrollmentStatus implements FieldValidator {

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
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>(1);

            try {
                if (!VALID_1700_ENROLLMENT_STATUS_VALUES.contains(value)) {
                    errors.add(new StateReportValidationError(entity, field,
                            "I4SEE 1700 Enrollment Status " + value + " not a valid Enrollment Status",
                            "I4SEE 1700 = " + value));
                }
            } catch (Exception e) {
                // This should be handled by the date formatter already.
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
            String entryDate = entity.getFieldValue(I4SEE_230_CATE_ENTRY_DATE);

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
            String exitDate = entity.getFieldValue(I4SEE_250_CATE_EXIT_DATE);
            if ((!StringUtils.isEmpty(exitDate) && StringUtils.isEmpty(value)) ||
                    (StringUtils.isEmpty(exitDate) && !StringUtils.isEmpty(value))) {
                errors.add(new StateReportValidationError(entity, field,
                        "I4SEE 260 Exit Code and I4See 250 Exit Date must either both be empty, or both be filled in",
                        "I4SEE 250 = " + exitDate + ", I4SEE 260 = " + value));
            }

            if ("W11".equals(value)) {
                String promotedInd = entity.getFieldValue(I4SEE_510_PROMOTED_IND);
                if (!"3".equals(promotedInd)) {
                    errors.add(new StateReportValidationError(entity, field,
                            "I4SEE 260 Exit Code " + value + " not valid for Promoted Indicator "
                                    + promotedInd,
                            "I4SEE 260 = " + value));
                }
                String gradeLvl = entity.getFieldValue(I4SEE_400_GRADE);
                if (!("11".equals(gradeLvl) || "12".equals(gradeLvl))) {
                    errors.add(new StateReportValidationError(entity, field,
                            "I4SEE 260 Exit Code " + value + " not valid for Grade Level "
                                    + gradeLvl,
                            "I4SEE 260 = " + value));
                }
            }

            if (value != null && value.matches("W2[0123456789]")) {
                String gradeLvl = entity.getFieldValue(I4SEE_400_GRADE);
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

    /*
     * Field alias for the adjusted district code on the SCHOOL table. This alias is optional.
     */
    private static final String ADJUSTED_DISTRICT_CODE_FIELD = "DOE ADJUSTED DISTRICT";
    /*
     * Field aliases for "adjusted" fields (these fields may have manually entered values that
     * override/adjust calculated values).
     *
     * Note: The alias for the adjusted status is optional (is does not have to be defined in the
     * Data Dictionary).
     */
    private static final String ADJUSTED_SCHOOL_NUMBER_FIELD = "i4see Adj School Number";

    /**
     * Name for the "calculate totals" parameter. The value is a Boolean.
     */
    public static final String CALCULATE_TOTALS_PARAM = "calculateTotals";
    /*
     * Other internal constants
     */
    private static final String DATE_FORMAT = "MM/dd/yyyy";

    private static final String I4SEE_010_SASID = "i4see 010";
    private static final String I4SEE_030_SAU_NUMBER = "i4see 030";
    private static final String I4SEE_040_DISTRICT_NUMBER = "i4see 040";
    private static final String I4SEE_050_SCHOOL_ID = "i4see 050";
    private static final String I4SEE_100_DOB = "i4see 100";
    private static final String I4SEE_130_ADDRESS = "i4see 130";
    private static final String I4SEE_131_TOWN = "i4see 131";
    private static final String I4SEE_132_STATE = "i4see 132";
    private static final String I4SEE_133_POSTAL_CODE = "i4see 133";
    private static final String I4SEE_134_ADDRESS_2 = "i4see 134";
    private static final String I4SEE_140_TELEPHONE = "i4see 140";
    private static final String I4SEE_141_WORK = "i4see 141";
    private static final String I4SEE_1420_SCHOOL_YEAR = "i4see 1420";
    private static final String I4SEE_150_SSN = "i4see 150";
    private static final String I4SEE_160_DISPLACED_HOMEMAKER = "i4see 160";
    private static final String I4SEE_161_SINGLE_PARENT = "i4see 161";
    private static final String I4SEE_162_PRIMARY_PROGRAM = "i4see 162";
    private static final String I4SEE_163_TSA_FLAG = "i4see 163";
    private static final String I4SEE_230_CATE_ENTRY_DATE = "i4see 230 CATE";
    private static final String I4SEE_240_CATE_ENTRY_CODE = "i4see 240 CATE";
    private static final String I4SEE_250_CATE_EXIT_DATE = "i4see 250 CATE";
    private static final String I4SEE_260_CATE_EXIT_CODE = "i4see 260 CATE";
    private static final String I4SEE_400_GRADE = "i4see 400";
    private static final String I4SEE_405_FULL_DAY_PERCENT = "i4see 405";
    private static final String I4SEE_510_PROMOTED_IND = "i4see 510";
    private static final String I4SEE_680_GRADUATED_FLAG = "i4see 680";
    private static final String I4SEE_1500_SAU_NUMBER_SEND = "i4see 1500";
    private static final String I4SEE_1510_DISTRICT_NUMBER_SEND = "i4see 1510";
    private static final String I4SEE_1520_SCHOOL_NUMBER_SEND = "i4see 1520";
    private static final String I4SEE_1600_SAU_NUMBER_RCV = "i4see 1600";
    private static final String I4SEE_1610_DISTRICT_NUMBER_RCV = "i4see 1610";
    private static final String I4SEE_1620_SCHOOL_NUMBER_RCV = "i4see 1620";
    private static final String I4SEE_1700_CATE_ENROLLMENT_STATUS = "i4see 1700";
    private static final String I4SEE_1710_PRIMARY_PROGRAM_ID = "i4see 1710";
    private static final String I4SEE_1720_PROGRAM_COMPLETER = "i4see 1720";
    private static final String I4SEE_1730_TRAN_MODE = "i4see 1730";
    private static final String I4SEE_1764_CATE_DAYS_IN_ATTEND_S1 = "i4see 1764";
    private static final String I4SEE_1765_CATE_DAYS_IN_ATTEND_S2 = "i4see 1765";
    // private static final String I4SEE_CATE_INDICATOR = "i4see CATE";
    private static final String I4SEE_CATE_CONTEXT = "i4see CATE CONTEXT";

    /**
     * Student Program Participation identifier for CATE
     *
     * see StudentProgramParticipation.COL_PROGRAM_CODE
     *
     */
    private static final String I4SEE_CATE_PROGRAM_CODE = "CATE";
    /**
     * Reference Table OID for CATE Entry codes
     */
    public static final String CATE_ENTRY_CODES_REF_OID = "i4see CATE Entry Codes";

    /**
     * Reference Table OID for CATE Exit codes
     */
    public static final String CATE_EXIT_CODES_REF_OID = "i4see CATE Exit Codes";

    /*
     * Field alias/field value for querying options on the export
     */
    private static final String I4SEE_CATE_STATUS_FIELD = "i4see CATE Status";
    private static final String I4SEE_STATUS_FIELD_REPORT_CODE = "Report";
    /**
     * Name for the "include student names" parameter. The value is a Boolean.
     */
    public static final String INCLUDE_STUDENT_NAMES_PARAM = "includeStudentName";
    /**
     * Name for the "include summer withdrawals" parameter. The value is a Boolean.
     */
    public static final String INCLUDE_SUMMER_WITHDRAWALS_PARAM = "includeSummerWithdrawals";
    /**
     * Name for the middle of year date parameter. The corresponding value is a PlainDate object.
     */
    public static final String MOY_DATE_PARAM = "moyDate";

    private static final String OFF_TRACK_CODE = "OFTR";
    /**
     * Name for the enumerated "selection" parameter. The value is an Integer.
     */
    public static final String QUERY_BY_PARAM = "queryBy";
    /**
     * Name for the "selection value" parameter. The value is an String.
     */
    public static final String QUERY_STRING_PARAM = "queryString";

    private static final String REGEX_NUMERIC = "[0-9]*";

    private static final String REGEX_YN = "[YN]?";

    /**
     * Social Security pattern - value must be in the format "000-00-0000"
     */
    private static final String REGEX_SSN = "\\d{3}-\\d{2}-\\d{4}";
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
    public static final int REPORT_TYPE_BOY = 0;
    /**
     * Value for REPORT_TYPE parameter indicating End of Year report
     */
    public static final int REPORT_TYPE_EOY = 1;
    /**
     * Name for the "require at lease one member day" parameter. The value is a Boolean.
     */
    public static final String REQUIRE_MEMBER_DAY_PARAM = "requireMemberDay";
    /**
     * Name for the "require report status" parameter. The value is a Boolean.
     */
    public static final String REQUIRE_REPORT_STATUS_PARAM = "requireReportStatus";
    /**
     * Name for the enumerated "sort" parameter. The value is an Integer.
     */
    public static final String SORT_PARAM = "sort";
    /*
     * Alias constants
     */
    private static final String STUDENT_NAME = "name view";
    /**
     * Name for the "summer end date" parameter. The corresponding values is a java.sql.Date object.
     */
    public static final String SUMMER_END_DATE_PARAM = "summerEndDate";
    /**
     * Name for the "summer start date" parameter. The corresponding values is a java.sql.Date
     * object.
     */
    public static final String SUMMER_START_DATE_PARAM = "summerStartDate";

    private static final boolean SUPPRESS_ENROLLMENT_WARNINGS = true;

    /**
     * Hash map capacity multiplier
     */
    private final static double HASHMAP_CAPACITY_MULTIPLIER = 1.5;

    /**
     * Name for the "update student records" parameter. The value is a Boolean.
     */
    public static final String UPDATE_RECORDS_PARAM = "updateRecords";

    private static final String VALIDATION_INVALID_VALUE = "Invalid value";

    static final Collection VALID_1700_ENROLLMENT_STATUS_VALUES = Arrays.asList("1", "4", "9", "18", "22");

    /*
     * List of fields where BlanksAllowed(if N/A) by the CATE B-O-Y report
     */
    private static final List<String> BOY_BLANKS_ALLOWED = Arrays.asList(
            I4SEE_250_CATE_EXIT_DATE, // BlanksAllowed(if NA)
            I4SEE_260_CATE_EXIT_CODE, // BlanksAllowed(if NA)
            I4SEE_1710_PRIMARY_PROGRAM_ID, // BlanksAllowed(if NA)
            I4SEE_1720_PROGRAM_COMPLETER, // BlanksAllowed(if NA)
            I4SEE_130_ADDRESS, // BlanksAllowed(if NA)
            I4SEE_134_ADDRESS_2, // BlanksAllowed(if NA)
            I4SEE_131_TOWN, // BlanksAllowed(if NA)
            I4SEE_132_STATE, // BlanksAllowed(if NA)
            I4SEE_133_POSTAL_CODE, // BlanksAllowed(if NA)
            I4SEE_140_TELEPHONE, // BlanksAllowed(if NA)
            I4SEE_141_WORK, // BlanksAllowed(if NA)
            I4SEE_680_GRADUATED_FLAG // BlanksAllowed(if NA)
    );

    /**
     * Supporting instance variables.
     * These are protected rather than private so they can be accessed by the inner classes.
     */
    protected Map<String, Integer> m_absences;

    protected String m_adjustedSchoolCode;

    protected boolean m_calculateTotals;

    protected SimpleDateFormat m_dateFormat;

    protected EnrollmentManager m_enrollmentManager;

    private HashMap<String, Map<String, String>> m_fieldToRefTable;

    protected PlainDate m_firstDayDate;

    protected TreeMap m_gradeLevelMap;

    protected boolean m_includeStudentNames;

    protected Converter m_integerConverter;

    protected PlainDate m_middleOfYearDate;

    protected Map<String, Race> m_raceCodeMap;

    protected PlainDate m_reportDate;

    protected String m_reportStatusField;

    protected int m_reportType;

    protected Map<String, Schedule> m_scheduleMap;

    protected Map<String, SisSchool> m_schoolMap;

    protected HashMap m_schoolsToCalendars;

    protected Collection m_suspensionInCodes;

    protected Collection m_suspensionOutCodes;

    protected Map<String, List<StudentProgramParticipation>> m_existingPrograms;

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
        return "I4SEE CATE ENROLLMENT";
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
     * Returns the number of days the student has been a member during a particular semester
     * from the start of school to the report date.
     *
     * @param i4see I4SeeEntity
     * @param semester - S1, S2, or FY
     * @param attendance boolean
     * @return String
     */
    public String getMembershipDays(I4SeeEntity i4see, SEMESTER semester, boolean attendance) {
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
                        StudentProgramParticipation program = i4see.getCurrentProgram();
                        PlainDate startDate = m_scheduleMap.get(student.getSchoolOid()).getStartDate();
                        PlainDate endDate;

                        Criteria criteria = new Criteria();
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

                        // If the value being fetched is the number of days for the 2nd semester
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

                        // If the value being fetched is the number of days for the 1st semester
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

                        float membership = m_enrollmentManager.getMembershipTotal(student,
                                getCalendarDays(m_schoolMap.get(student.getSchoolOid()), student.getCalendarCode()),
                                true,
                                startDate,
                                endDate,
                                null) - offTrackDays;

                        // If requesting attendance instead of membership, subtract out absent
                        // total.
                        if (attendance) {
                            ReportQueryByCriteria reportQuery = new ReportQueryByCriteria(StudentAttendance.class,
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

                        count = Integer.toString((int) membership); // String.valueOf(membership);
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
        String[] fields = {I4SEE_240_CATE_ENTRY_CODE, I4SEE_260_CATE_EXIT_CODE
        };
        loadStateReferenceCodes(fields);

        loadCATEStudentProgramParticipation();

        /*
         * Get core parameters
         */
        m_calculateTotals = ((Boolean) getParameter(CALCULATE_TOTALS_PARAM)).booleanValue();
        m_includeStudentNames = ((Boolean) getParameter(INCLUDE_STUDENT_NAMES_PARAM)).booleanValue();
        m_reportDate = (PlainDate) getParameter(REPORT_DATE_PARAM);
        m_middleOfYearDate = (PlainDate) getParameter(MOY_DATE_PARAM);
        m_reportType = ((Integer) getParameter(REPORT_TYPE)).intValue();

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
        ArrayList<FieldDefinition> fieldDefinitions = new ArrayList<FieldDefinition>(m_includeStudentNames ? 26 : 25);

        if (m_includeStudentNames) {
            fieldDefinitions.add(getName());
        }

        fieldDefinitions.add(getSasid());
        fieldDefinitions.add(getDob());

        fieldDefinitions.add(getI4see1500_SendingSchoolAdminUnitNumber());
        fieldDefinitions.add(getI4see1510_SendingSchoolDistrictNumber());
        fieldDefinitions.add(getI4see1520_SendingSchoolNumber());
        fieldDefinitions.add(getI4see1600_ReceivingSchoolAdminUnitNumber());
        fieldDefinitions.add(getI4see1610_ReceivingSchoolDistrictNumber());
        fieldDefinitions.add(getI4see1620_ReceivingSchoolNumber());
        fieldDefinitions.add(getI4see1420_SchoolYear());
        fieldDefinitions.add(getI4see400_GradeLevel());

        fieldDefinitions.add(getI4see1700_EnrollmentStatus());

        fieldDefinitions.add(getI4see230_EntryDate());
        fieldDefinitions.add(getI4see240_EntryCode());
        fieldDefinitions.add(getI4see250_ExitDate());
        fieldDefinitions.add(getI4see260_ExitCode());

        fieldDefinitions.add(getI4see1710_PrimaryProgramId());
        fieldDefinitions.add(getI4see1720_ProgramCompleter());
        fieldDefinitions.add(getI4see130_Address());
        fieldDefinitions.add(getI4see134_Address2());
        fieldDefinitions.add(getI4see131_Town());
        fieldDefinitions.add(getI4see132_State());
        fieldDefinitions.add(getI4see133_PostalCode());
        fieldDefinitions.add(getI4see140_Telephone());
        fieldDefinitions.add(getI4see141_Work());
        fieldDefinitions.add(getI4see150_SSN());

        fieldDefinitions.add(getI4see680_GraduatedFlag());
        fieldDefinitions.add(getI4see1730_TranMode());
        fieldDefinitions.add(getI4see1764_DaysInAttendanceS1());
        fieldDefinitions.add(getI4see1765_DaysInAttendanceS2());
        fieldDefinitions.add(getI4see160_DisplacedHomemaker());
        fieldDefinitions.add(getI4see161_SingleParent());
        fieldDefinitions.add(getI4see162_PrimaryProgram());
        fieldDefinitions.add(getI4see163_TSAFlag());

        setFieldDefinitions(fieldDefinitions);

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
                            translateAliasToJavaName(I4SEE_1520_SCHOOL_NUMBER_SEND, true));
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

            // Set the query to be used for student selection.
            setQuery(studentQuery);
            setEntityClass(I4SeeEntity.class);

            /*
             * Load absence days for all students included in the export.
             */
            loadAbsenceDaysMaps(studentCriteria);

            /*
             * Load Schools
             */
            loadSchools();

            /*
             * Load Active Schedules
             */
            loadActiveSchedules();
        }
    }

    /**
     * Adds criteria to filter the export selection by record set.
     *
     * @param criteria Criteria
     * @param recordSetName String
     */
    private void addRecordSetCriteria(Criteria criteria, String recordSetName) {
        Criteria recordSetCriteria = new Criteria();
        recordSetCriteria.addEqualTo(RecordSetKey.REL_RECORD_SET + PATH_DELIMITER + RecordSet.COL_NAME,
                recordSetName);

        criteria.addIn(X2BaseBean.COL_OID, new SubQuery(RecordSetKey.class,
                RecordSetKey.COL_OBJECT_OID, recordSetCriteria));
    }

    /**
     * Returns the days-in-session for the given school and calendar combination.
     *
     * @param school SisSchool
     * @param calendar String
     * @return Set of PlainDate objects
     */
    private Set getCalendarDays(SisSchool school, String calendar) {
        if (!m_schoolsToCalendars.containsKey(school.getOid())) {
            PlainDate startDate = m_scheduleMap.get(school.getOid()).getStartDate();
            Map calendarData = m_enrollmentManager.getCalendarLookup(school, startDate, m_reportDate);
            m_schoolsToCalendars.put(school.getOid(), calendarData);
        }

        return (Set) ((Map) m_schoolsToCalendars.get(school.getOid())).get(calendar);
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
                PlainDate startDate = ((SisSchool) getSchool()).getActiveSchedule().getStartDate();

                Criteria withdrawalsCriteria = new Criteria();
                withdrawalsCriteria.addIn(X2BaseBean.COL_OID, getStudentWithdrawalQuery(startDate, m_reportDate));
                withdrawalsCriteria.addNotEqualTo(SisStudent.REL_SCHOOL + PATH_DELIMITER + field.getJavaName(),
                        adjustedDistrictCode);

                primaryCriteria.addOrCriteria(withdrawalsCriteria);
            }
        } else {
            primaryCriteria.addEqualTo(SisStudent.COL_ORGANIZATION1_OID, getOrganization().getOid());
            primaryCriteria.addEqualTo(SisStudent.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR,
                    Boolean.FALSE);
            primaryCriteria.addEqualTo(SisStudent.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_ARCHIVE_INDICATOR,
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
                schoolCriteria.addNotEqualTo(SisStudent.REL_SCHOOL + PATH_DELIMITER + field.getJavaName(),
                        adjustedDistrictCode);
            }

            schoolCriteria.addOrEqualTo(SisStudent.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_ARCHIVE_INDICATOR,
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

        m_enrollmentManager = new EnrollmentManager(getBroker(), getPrivilegeSet(), getOrganization());
        m_firstDayDate = getCurrentContext().getStartDate();
        m_reportStatusField = translateAliasToJavaName(I4SEE_CATE_STATUS_FIELD, true);
        m_schoolsToCalendars = new HashMap();
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
        criteria.addGreaterOrEqualThanField(StudentAttendance.COL_DATE,
                StudentAttendance.REL_SCHOOL + PATH_DELIMITER +
                        SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER +
                        SchoolScheduleContext.REL_ACTIVE_SCHEDULE + PATH_DELIMITER +
                        Schedule.COL_START_DATE);
        criteria.addLessOrEqualThan(StudentAttendance.COL_DATE, m_reportDate);
        criteria.addEqualTo(StudentAttendance.COL_ABSENT_INDICATOR, Boolean.TRUE);

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
    }

    /**
     * Loads the active schedule for each school.
     */
    private void loadActiveSchedules() {
        m_scheduleMap = new HashMap();
        Collection<SisSchool> schools = m_schoolMap.values();
        for (SisSchool school : schools) {
            m_scheduleMap.put(school.getOid(), school.getActiveSchedule());
        }
    }

    /**
     * Loads all the school beans in the district.
     */
    private void loadSchools() {
        QueryByCriteria schoolQuery = new QueryByCriteria(SisSchool.class, new Criteria());
        m_schoolMap = getBroker().getMapByQuery(schoolQuery, X2BaseBean.COL_OID, getBroker().getCount(schoolQuery));
    }

    /**
     * Returns the minimum valid length for this field. If the field is not used or may be
     * blank, then a length of 0 is returned.
     *
     * @param fieldId String
     * @param minReqLength int
     * @return int
     */
    private int minValidLength(String fieldId, int minReqLength) {
        int minLength = minReqLength;

        if (m_reportType == REPORT_TYPE_BOY
                && BOY_BLANKS_ALLOWED.contains(fieldId)) {
            minLength = 0;
        }
        return minLength;
    }

    /**
     * I4SEE_100_DOB Build Field definition for the student DOB (i4see 100).
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDob() {
        FieldDefinition field = new FieldDefinition(I4SEE_100_DOB,
                SisStudent.REL_PERSON + PATH_DELIMITER + Person.COL_DOB,
                null,
                false,
                8,
                10,
                null,
                m_dateFormat,
                null,
                null,
                null);
        return field;
    }

    /**
     * I4SEE_400_GRADE Build Field definition for the student grade level.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getI4see400_GradeLevel() {
        FieldDefinition field = new FieldDefinition(I4SEE_400_GRADE,
                SisStudent.COL_GRADE_LEVEL,
                null,
                false,
                1,
                2,
                null,
                null,
                new Retrieve400GradeLevel(),
                null,
                null);
        return field;
    }

    /**
     * I4SEE_130_ADDRESS.
     *
     * @return FieldDefinition
     */
    protected FieldDefinition getI4see130_Address() {
        String javaName = translateAliasToJavaName(I4SEE_130_ADDRESS, true);
        FieldDefinition field = new FieldDefinition(I4SEE_130_ADDRESS,
                SisStudent.REL_PERSON + PATH_DELIMITER + SisPerson.REL_PHYSICAL_ADDRESS + PATH_DELIMITER + javaName,
                null,
                false,
                0,
                50,
                null,
                null,
                null,
                null,
                null);
        return field;
    }

    /**
     * I4SEE_131_TOWN.
     *
     * @return FieldDefinition
     */
    protected FieldDefinition getI4see131_Town() {
        String javaName = translateAliasToJavaName(I4SEE_131_TOWN, true);
        FieldDefinition field = new FieldDefinition(I4SEE_131_TOWN,
                SisStudent.REL_PERSON + PATH_DELIMITER + SisPerson.REL_PHYSICAL_ADDRESS + PATH_DELIMITER + javaName,
                null,
                false,
                0,
                50,
                null,
                null,
                null,
                null,
                null);
        return field;
    }

    /**
     * I4SEE_132_STATE.
     *
     * @return FieldDefinition
     */
    protected FieldDefinition getI4see132_State() {
        String javaName = translateAliasToJavaName(I4SEE_132_STATE, true);
        FieldDefinition field = new FieldDefinition(I4SEE_132_STATE,
                SisStudent.REL_PERSON + PATH_DELIMITER + SisPerson.REL_PHYSICAL_ADDRESS + PATH_DELIMITER + javaName,
                null,
                false,
                0,
                2,
                null,
                null,
                null,
                null,
                null);
        return field;
    }

    /**
     * I4SEE_133_POSTAL_CODE.
     *
     * @return FieldDefinition
     */
    protected FieldDefinition getI4see133_PostalCode() {
        String javaName = translateAliasToJavaName(I4SEE_133_POSTAL_CODE, true);
        FieldDefinition field = new FieldDefinition(I4SEE_133_POSTAL_CODE,
                SisStudent.REL_PERSON + PATH_DELIMITER + SisPerson.REL_PHYSICAL_ADDRESS + PATH_DELIMITER + javaName,
                null,
                false,
                0,
                12,
                null,
                null,
                null,
                null,
                null);
        return field;
    }

    /**
     * I4SEE_134_ADDRESS_2.
     *
     * @return FieldDefinition
     */
    protected FieldDefinition getI4see134_Address2() {
        String javaName = translateAliasToJavaName(I4SEE_134_ADDRESS_2, true);
        FieldDefinition field = new FieldDefinition(I4SEE_134_ADDRESS_2,
                SisStudent.REL_PERSON + PATH_DELIMITER + SisPerson.REL_PHYSICAL_ADDRESS + PATH_DELIMITER + javaName,
                null,
                false,
                0,
                50,
                null,
                null,
                null,
                null,
                null);
        return field;
    }

    /**
     * I4SEE_140_TELEPHONE.
     *
     * @return FieldDefinition
     */
    protected FieldDefinition getI4see140_Telephone() {
        String javaName = translateAliasToJavaName(I4SEE_140_TELEPHONE, true);
        FieldDefinition field = new FieldDefinition(I4SEE_140_TELEPHONE,
                SisStudent.REL_PERSON + PATH_DELIMITER + javaName,
                null,
                false,
                0,
                20,
                null,
                null,
                null,
                null,
                null);
        return field;
    }

    /**
     * I4SEE_141_WORK.
     *
     * @return FieldDefinition
     */
    protected FieldDefinition getI4see141_Work() {
        String javaName = translateAliasToJavaName(I4SEE_141_WORK, true);
        FieldDefinition field = new FieldDefinition(I4SEE_141_WORK,
                javaName,
                null,
                false,
                0,
                1,
                null,
                null,
                null,
                null,
                null);
        return field;
    }

    /**
     * I4SEE_150_SSN.
     *
     * @return FieldDefinition
     */
    protected FieldDefinition getI4see150_SSN() {
        String javaName = translateAliasToJavaName(I4SEE_150_SSN, true);
        FieldDefinition field = new FieldDefinition(I4SEE_150_SSN,
                javaName,
                null,
                false,
                11,
                11,
                REGEX_SSN,
                null,
                new Retrieve150SSN(javaName),
                null,
                null);
        return field;
    }

    /**
     * I4SEE_160_DISPLACED_HOMEMAKER.
     *
     * @return FieldDefinition
     */
    protected FieldDefinition getI4see160_DisplacedHomemaker() {
        String javaName = translateAliasToJavaName(I4SEE_160_DISPLACED_HOMEMAKER, true);
        FieldDefinition field = new FieldDefinition(I4SEE_160_DISPLACED_HOMEMAKER,
                javaName,
                "N",
                false,
                0,
                1,
                null,
                null,
                null,
                null,
                null);

        return field;
    }

    /**
     * I4SEE_161_SINGLE_PARENT.
     *
     * @return FieldDefinition
     */
    protected FieldDefinition getI4see161_SingleParent() {
        String javaName = translateAliasToJavaName(I4SEE_161_SINGLE_PARENT, true);
        FieldDefinition field = new FieldDefinition(I4SEE_161_SINGLE_PARENT,
                javaName,
                "N",
                false,
                0,
                1,
                null,
                null,
                null,
                null,
                null);

        return field;
    }

    /**
     * I4SEE_162_PRIMARY_PROGRAM.
     *
     * @return FieldDefinition
     */
    protected FieldDefinition getI4see162_PrimaryProgram() {
        String javaName = translateAliasToJavaName(I4SEE_162_PRIMARY_PROGRAM, true);
        FieldDefinition field = new FieldDefinition(I4SEE_162_PRIMARY_PROGRAM,
                javaName,
                "N",
                false,
                0,
                1,
                null,
                null,
                new RetrieveProgramParticipationFields(),
                null,
                null);

        return field;
    }

    /**
     * I4SEE_163_TSA_FLAG.
     *
     * @return FieldDefinition
     */
    protected FieldDefinition getI4see163_TSAFlag() {
        String javaName = translateAliasToJavaName(I4SEE_163_TSA_FLAG, true);
        FieldDefinition field = new FieldDefinition(I4SEE_163_TSA_FLAG,
                "programParticipation." + javaName,
                null,
                true,
                0,
                1,
                null,
                null,
                new RetrieveProgramParticipationFields(),
                null,
                null);

        return field;
    }

    /**
     * I4SEE_1500_SAU_NUMBER_SEND.
     *
     * @return FieldDefinition
     */
    protected FieldDefinition getI4see1500_SendingSchoolAdminUnitNumber() {
        String javaName = translateAliasToJavaName(I4SEE_1500_SAU_NUMBER_SEND, true);
        FieldDefinition field = new FieldDefinition(I4SEE_1500_SAU_NUMBER_SEND,
                SisStudent.REL_PROGRAM_PARTICIPATION + PATH_DELIMITER + javaName,
                null,
                false,
                1,
                3,
                REGEX_NUMERIC,
                null,
                new RetrieveProgramParticipationFields(),
                null,
                I4SEE_1500_SAU_NUMBER_SEND);

        return field;
    }

    /**
     * I4SEE_1510_DISTRICT_NUMBER_SEND.
     *
     * @return FieldDefinition
     */
    protected FieldDefinition getI4see1510_SendingSchoolDistrictNumber() {
        String javaName = translateAliasToJavaName(I4SEE_1510_DISTRICT_NUMBER_SEND, true);
        FieldDefinition field = new FieldDefinition(I4SEE_1510_DISTRICT_NUMBER_SEND,
                SisStudent.REL_PROGRAM_PARTICIPATION + PATH_DELIMITER + javaName,
                null,
                false,
                1,
                3,
                REGEX_NUMERIC,
                null,
                new RetrieveProgramParticipationFields(),
                null,
                I4SEE_1510_DISTRICT_NUMBER_SEND);

        return field;
    }

    /**
     * Build a Field Definition for i4see 1520 (sending school number).
     *
     * @return FieldDefinition
     */
    protected FieldDefinition getI4see1520_SendingSchoolNumber() {
        String javaName = translateAliasToJavaName(I4SEE_1520_SCHOOL_NUMBER_SEND, true);
        FieldDefinition field = new FieldDefinition(I4SEE_1520_SCHOOL_NUMBER_SEND,
                SisStudent.REL_PROGRAM_PARTICIPATION + PATH_DELIMITER + javaName,
                null,
                false,
                5,
                5,
                REGEX_NUMERIC,
                null,
                new RetrieveProgramParticipationFields(),
                null,
                I4SEE_1520_SCHOOL_NUMBER_SEND);

        return field;
    }

    /**
     * Gets the i 4 see 1600 receiving school admin unit number.
     *
     * @return FieldDefinition
     */
    protected FieldDefinition getI4see1600_ReceivingSchoolAdminUnitNumber() {
        String javaName = translateAliasToJavaName(I4SEE_1600_SAU_NUMBER_RCV, true);
        FieldDefinition field = new FieldDefinition(I4SEE_1600_SAU_NUMBER_RCV,
                SisStudent.REL_PROGRAM_PARTICIPATION + PATH_DELIMITER + javaName,
                null,
                false,
                1,
                3,
                REGEX_NUMERIC,
                null,
                new RetrieveProgramParticipationFields(),
                null,
                I4SEE_1600_SAU_NUMBER_RCV);

        return field;
    }

    /**
     * Gets the i 4 see 1610 receiving school district number.
     *
     * @return FieldDefinition
     */
    protected FieldDefinition getI4see1610_ReceivingSchoolDistrictNumber() {
        String javaName = translateAliasToJavaName(I4SEE_1610_DISTRICT_NUMBER_RCV, true);
        FieldDefinition field = new FieldDefinition(I4SEE_1610_DISTRICT_NUMBER_RCV,
                SisStudent.REL_PROGRAM_PARTICIPATION + PATH_DELIMITER + javaName,
                null,
                false,
                1,
                3,
                REGEX_NUMERIC,
                null,
                new RetrieveProgramParticipationFields(),
                null,
                I4SEE_1610_DISTRICT_NUMBER_RCV);

        return field;
    }

    /**
     * Gets the i 4 see 1620 receiving school number.
     *
     * @return FieldDefinition
     */
    protected FieldDefinition getI4see1620_ReceivingSchoolNumber() {
        String javaName = translateAliasToJavaName(I4SEE_1620_SCHOOL_NUMBER_RCV, true);
        FieldDefinition field = new FieldDefinition(I4SEE_1620_SCHOOL_NUMBER_RCV,
                LABEL_PREFIX_CHAR + SisStudent.REL_PROGRAM_PARTICIPATION + PATH_DELIMITER + javaName,
                (String) getParameter("receivingSchoolNumber"),
                false,
                5,
                5,
                REGEX_NUMERIC,
                null,
                null,
                null,
                null);

        return field;
    }

    /**
     * Gets the i 4 see 1420 school year.
     *
     * @return FieldDefinition
     */
    protected FieldDefinition getI4see1420_SchoolYear() {
        String javaName = translateAliasToJavaName(I4SEE_1420_SCHOOL_YEAR, true);
        FieldDefinition field = new FieldDefinition(I4SEE_1420_SCHOOL_YEAR,
                SisStudent.REL_SCHOOL + PATH_DELIMITER + SisSchool.REL_ORGANIZATION1 + PATH_DELIMITER
                        + Organization.REL_CURRENT_CONTEXT + PATH_DELIMITER + javaName,
                null,
                false,
                4,
                4,
                REGEX_NUMERIC,
                null,
                null,
                null,
                null);

        return field;
    }


    /**
     * Gets the i 4 see 1700 enrollment status.
     *
     * @return FieldDefinition
     */
    protected FieldDefinition getI4see1700_EnrollmentStatus() {
        String javaName = translateAliasToJavaName(I4SEE_1700_CATE_ENROLLMENT_STATUS, true);
        FieldDefinition field = new FieldDefinition(I4SEE_1700_CATE_ENROLLMENT_STATUS,
                javaName,
                null,
                false,
                0,
                2,
                REGEX_NUMERIC,
                null,
                null,
                new Validate1700CATEEnrollmentStatus(),
                null);

        return field;
    }

    /**
     * Gets the i 4 see 1710 primary program id.
     *
     * @return FieldDefinition
     */
    protected FieldDefinition getI4see1710_PrimaryProgramId() {
        String javaName = translateAliasToJavaName(I4SEE_1710_PRIMARY_PROGRAM_ID, true);
        FieldDefinition field = new FieldDefinition(I4SEE_1710_PRIMARY_PROGRAM_ID,
                javaName,
                null,
                false,
                0,
                10,
                REGEX_NUMERIC,
                null,
                new RetrieveProgramParticipationFields(),
                null,
                null);

        return field;
    }

    /**
     * Gets the i 4 see 1720 program completer.
     *
     * @return FieldDefinition
     */
    protected FieldDefinition getI4see1720_ProgramCompleter() {
        String javaName = translateAliasToJavaName(I4SEE_1720_PROGRAM_COMPLETER, true);
        FieldDefinition field = null;
        if (m_reportType == REPORT_TYPE_BOY) {
            field = new FieldDefinition(I4SEE_1720_PROGRAM_COMPLETER,
                    LABEL_PREFIX_CHAR + javaName,
                    "",
                    false,
                    0,
                    1,
                    null,
                    null,
                    null,
                    null,
                    null);
        } else {
            field = new FieldDefinition(I4SEE_1720_PROGRAM_COMPLETER,
                    javaName,
                    null,
                    false,
                    0,
                    1,
                    null,
                    null,
                    new RetrieveProgramParticipationFields(),
                    null,
                    null);
        }
        return field;
    }

    /**
     * Gets the i 4 see 1730 tran mode.
     *
     * @return FieldDefinition
     */
    protected FieldDefinition getI4see1730_TranMode() {
        String javaName = translateAliasToJavaName(I4SEE_1730_TRAN_MODE, true);
        FieldDefinition field = new FieldDefinition(I4SEE_1730_TRAN_MODE,
                LABEL_PREFIX_CHAR + I4SEE_1730_TRAN_MODE + PATH_DELIMITER + javaName,
                null,
                false,
                1,
                1,
                REGEX_NUMERIC,
                null,
                new RetrieveProgramParticipationFields(),
                null,
                null);

        return field;
    }

    /**
     * Build Field definition for I4see 1764 (days in attendance S1).
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getI4see1764_DaysInAttendanceS1() {
        FieldDefinition field = new FieldDefinition(I4SEE_1764_CATE_DAYS_IN_ATTEND_S1,
                translateAliasToJavaName(I4SEE_1764_CATE_DAYS_IN_ATTEND_S1, true),
                null,
                false,
                minValidLength(I4SEE_1764_CATE_DAYS_IN_ATTEND_S1, 1),
                3,
                REGEX_NUMERIC,
                null,
                new Retrieve1764DaysInAttendanceS1(),
                null,
                null);
        return field;
    }

    /**
     * Build Field definition for I4see 1765 (days in attendance S2).
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getI4see1765_DaysInAttendanceS2() {
        FieldDefinition field = new FieldDefinition(I4SEE_1765_CATE_DAYS_IN_ATTEND_S2,
                translateAliasToJavaName(I4SEE_1765_CATE_DAYS_IN_ATTEND_S2, true),
                null,
                false,
                minValidLength(I4SEE_1765_CATE_DAYS_IN_ATTEND_S2, 1),
                3,
                REGEX_NUMERIC,
                null,
                new Retrieve1765DaysInAttendanceS2(),
                null,
                null);
        return field;
    }

    /**
     * Build a Field Definition for i4see 230 (entry date).
     *
     * @return FieldDefinition
     */
    protected FieldDefinition getI4see230_EntryDate() {
        String javaName = translateAliasToJavaName(I4SEE_230_CATE_ENTRY_DATE, true);
        FieldDefinition field = new FieldDefinition(I4SEE_230_CATE_ENTRY_DATE,
                StudentProgramParticipation.COL_START_DATE,
                null,
                false,
                minValidLength(I4SEE_230_CATE_ENTRY_DATE, 8),
                10,
                null,
                m_dateFormat,
                new Retrieve230_250_EntryExitDate(javaName),
                null,
                null);

        return field;
    }

    /**
     * Build a Field Definition for i4see 240 (entry code).
     *
     * @return FieldDefinition
     */
    protected FieldDefinition getI4see240_EntryCode() {
        String javaName = translateAliasToJavaName(I4SEE_240_CATE_ENTRY_CODE, true);
        FieldDefinition field = new FieldDefinition(I4SEE_240_CATE_ENTRY_CODE,
                I4SEE_240_CATE_ENTRY_CODE,
                null,
                false,
                minValidLength(I4SEE_240_CATE_ENTRY_CODE, 2),
                3,
                null,
                null,
                new Retrieve240_260_EntryExitCode(javaName),
                new Validate240EntryCode(),
                null);

        return field;
    }

    /**
     * Build a Field Definition for i4see 250 (exit date).
     *
     * @return FieldDefinition
     */
    protected FieldDefinition getI4see250_ExitDate() {
        String javaName = translateAliasToJavaName(I4SEE_250_CATE_EXIT_DATE, true);
        FieldDefinition field = new FieldDefinition(I4SEE_250_CATE_EXIT_DATE,
                I4SEE_250_CATE_EXIT_DATE,
                null,
                false,
                0,
                10,
                null,
                m_dateFormat,
                new Retrieve230_250_EntryExitDate(javaName),
                null,
                null);

        return field;
    }

    /**
     * Build a Field Definition for i4see 260 (exit code).
     *
     * @return FieldDefinition
     */
    protected FieldDefinition getI4see260_ExitCode() {
        String javaName = translateAliasToJavaName(I4SEE_260_CATE_EXIT_CODE, true);
        FieldDefinition field = new FieldDefinition(I4SEE_260_CATE_EXIT_CODE,
                SisStudent.REL_ENROLLMENTS + PATH_DELIMITER + StudentEnrollment.COL_ENROLLMENT_CODE,
                null,
                false,
                0,
                3,
                null,
                null,
                new Retrieve240_260_EntryExitCode(javaName),
                new Validate260ExitCode(),
                null);

        return field;
    }

    /**
     * Build a Field Definition for i4see 405 (full day percent).
     *
     * @return FieldDefinition
     */
    protected FieldDefinition getI4see405_FullDayPercent() {
        String javaName = translateAliasToJavaName(I4SEE_405_FULL_DAY_PERCENT, true);
        FieldDefinition field = new FieldDefinition(I4SEE_405_FULL_DAY_PERCENT,
                javaName,
                null,
                false,
                0,
                3,
                REGEX_NUMERIC,
                null,
                new Retrieve405FullDayPercent(javaName),
                null,
                null);

        return field;
    }

    /**
     * Build a Field Definition for i4see 660 (original YOG).
     *
     * @return FieldDefinition
     */
    protected FieldDefinition getI4see680_GraduatedFlag() {
        FieldDefinition field = new FieldDefinition(I4SEE_680_GRADUATED_FLAG,
                translateAliasToJavaName(I4SEE_680_GRADUATED_FLAG, true),
                null,
                false,
                minValidLength(I4SEE_680_GRADUATED_FLAG, 1),
                1,
                REGEX_YN,
                null,
                null,
                null,
                null);

        return field;
    }

    /**
     * Build Field definition for the student name view.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getName() {
        FieldDefinition field = new FieldDefinition(STUDENT_NAME,
                SisStudent.COL_NAME_VIEW,
                null,
                false,
                1,
                32,
                null,
                null,
                null,
                null,
                null);
        return field;
    }


    /**
     * Build Field definition for the student SASID (i4see 010).
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getSasid() {
        FieldDefinition field = new FieldDefinition(I4SEE_010_SASID,
                SisStudent.COL_STATE_ID,
                null,
                false,
                1,
                32,
                null,
                null,
                null,
                null,
                null);
        return field;
    }

    /**
     * Returns the number of days in attendance based on the membershipCount.
     *
     * @param entity StateReportEntity
     * @param field FieldDefinition
     * @param membershipCount String
     * @return String
     */
    String retrieveDaysInAttendance(StateReportEntity entity,
                                    FieldDefinition field,
                                    String membershipCount) {
        String attendanceCount;
        int daysAbsent = getDaysAbsent((SisStudent) entity.getBean());

        int membershipCountAsInt = 0;

        if (membershipCount != null) {
            try {
                membershipCountAsInt = Integer.parseInt(membershipCount);
            } catch (NumberFormatException nfe) {
                // Do nothing, this error has already been logged.
            }
        }


        if (membershipCountAsInt != 0) {
            int attendance = membershipCountAsInt;
            attendance -= daysAbsent;
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
     * Gets the days absent.
     *
     * @param student SisStudent
     * @return int
     */
    private int getDaysAbsent(SisStudent student) {

        int absenses = 0;

        Integer absenceCount = m_absences.get(student.getOid());
        if (absenceCount != null) {
            absenses = absenceCount.intValue();
        }



        return absenses;
    }

    /**
     * Returns a list of student program participation, ordered newest to oldest.
     *
     * @param studentOid String
     * @return LinkedList&lt;StudentProgramParticipation&gt;
     */
    protected List<StudentProgramParticipation> getProgram(String studentOid) {
        List<StudentProgramParticipation> programs = m_existingPrograms.get(studentOid);

        if (programs == null) {
            programs = new ArrayList<StudentProgramParticipation>();
        }
        return programs;
    }

    /**
     * loads existing Student Program Participation records for the CATE program.
     */
    private void loadCATEStudentProgramParticipation() {
        String cateContext = translateAliasToJavaName(I4SEE_CATE_CONTEXT, true);
        Criteria criteria = new Criteria();
        criteria.addEqualTo(StudentProgramParticipation.COL_PROGRAM_CODE, I4SEE_CATE_PROGRAM_CODE);
        criteria.addEqualTo(cateContext, getCurrentContext().getContextId());
        QueryByCriteria query = new QueryByCriteria(StudentProgramParticipation.class, criteria);

        m_existingPrograms = getBroker().getGroupedCollectionByQuery(query, StudentProgramParticipation.COL_STUDENT_OID,
                getBroker().getCount(query));
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

    /**
     * Returns the reference table's state code for a field's reference code.
     *
     * @param beanPath String
     * @param enrollmentCode String
     * @return String
     */
    protected String getStateCode(String beanPath, String enrollmentCode) {
        return m_fieldToRefTable.get(beanPath).get(enrollmentCode);
    }

    /**
     * Returns a map of base reference codes to their state reference code equivalents for the
     * reference table used by the given student property. If the student property doesn't use a
     * reference table then an empty map is returned.
     *
     * @param propertyName String
     * @return A Map of String keys to String values
     */
    private Map getStateCodeReferenceMap(String propertyName) {
        HashMap baseToStateCodes = null;
        DataDictionaryField field = null;

        try {
            ModelProperty property =
                    new ModelProperty(SisStudent.class.getName(), propertyName, getBroker().getPersistenceKey());
            field = property.getField();
        } catch (InvalidDictionaryIdException e) {
            // not a valid field
        }

        if (field != null && field.hasReferenceTable()) {
            Collection codes = field.getReferenceTable().getReferenceCodes(getBroker());
            baseToStateCodes = new HashMap((int) (codes.size() * HASHMAP_CAPACITY_MULTIPLIER));
            Iterator codeIterator = codes.iterator();
            while (codeIterator.hasNext()) {
                ReferenceCode code = (ReferenceCode) codeIterator.next();
                baseToStateCodes.put(code.getCode(), code.getStateCode());
            }
        } else {
            baseToStateCodes = new HashMap();
            /*
             * If the propertyName is for the student enrollment codes, these need to be pulled
             * from reference table via the codes specified in the system preferences
             */
            if (propertyName.equals(StudentEnrollment.COL_ENROLLMENT_CODE)) {
                /*
                 * get the preferences for the organization and the two OIDs for withdrawal codes
                 * and entry codes
                 */
                PreferenceSet preferenceSet = PreferenceManager.getPreferenceSet(getOrganization());
                String[] sysPrefDefs = {SisPreferenceConstants.ENROLLMENT_ENTRY_CODES,
                        SisPreferenceConstants.ENROLLMENT_WITHDRAWAL_CODES};

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
            } else if (propertyName.equals(I4SEE_240_CATE_ENTRY_CODE) ||
                    propertyName.equals(I4SEE_260_CATE_EXIT_CODE)) {
                String refTableName = null;
                if (propertyName.equals(I4SEE_240_CATE_ENTRY_CODE)) {
                    refTableName = CATE_ENTRY_CODES_REF_OID;
                }
                if (propertyName.equals(I4SEE_260_CATE_EXIT_CODE)) {
                    refTableName = CATE_EXIT_CODES_REF_OID;
                }

                X2Criteria criteria = new X2Criteria();
                criteria.addEqualTo(ReferenceTable.COL_USER_NAME, refTableName);
                QueryByCriteria query = new QueryByCriteria(ReferenceTable.class, criteria);
                ReferenceTable refTable = (ReferenceTable) getBroker().getBeanByQuery(query);

                if (refTable != null) {
                    Collection codes = refTable.getReferenceCodes(getBroker());
                    Iterator codeIterator = codes.iterator();
                    while (codeIterator.hasNext()) {
                        ReferenceCode code = (ReferenceCode) codeIterator.next();
                        baseToStateCodes.put(code.getCode(), code.getStateCode());
                    }
                } else {
                    this.addSetupError("Reference Codes", "Could not find the reference table: " + refTableName);
                }
            }
        }

        return baseToStateCodes;
    }
}
