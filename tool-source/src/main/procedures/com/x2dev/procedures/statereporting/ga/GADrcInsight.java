/*
 * ====================================================================
 *
 * Follett School Solutions
 *
 * Copyright (c) 2020 Follett School Solutions
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */


package com.x2dev.procedures.statereporting.ga;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.RecordSet;
import com.follett.fsc.core.k12.beans.RecordSetKey;
import com.follett.fsc.core.k12.beans.Staff;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.Course;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.SchoolScheduleContext;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentSchedule;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import java.util.HashMap;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * This class implements the data export for DRC Insight export.
 *
 * @author X2 Development Corporation
 */
public class GADrcInsight extends StateReportData {

    /*
     * Parameters
     */
    private static final String PARAM_SORT = "sort";
    private static final String PARAM_QUERY_BY = "queryBy";
    private static final String PARAM_QUERY_STRING = "queryString";

    /*
     * Field aliases
     */
    protected static final String SCHOOL_EXCLUDE_CODE = "DOE EXCLUDE SKL";
    protected static final String STUDENT_EXCLUDE_CODE = "DOE EXCLUDE STD";
    protected static final String STUDENT_ID_CODE = "GTID";

    protected static Map<String, String> courseNumbers = new HashMap<String, String>();

    /**
     * Class to retrieve value of the primary guardian's first name.
     */
    protected class GroupNameRetriever implements FieldRetriever {

        protected static final String RETRIEVER_ID = "STUDENT_GROUP_NAME";

        /**
         * Instantiates a new MA retriever.
         */
        public GroupNameRetriever() {
            super();
        }

        /**
         * Gets the field value.
         *
         * @param dataDefault StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever
         *      #getFieldValue(StateReportData, StateReportEntity, FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData dataDefault, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            StudentSchedule ssc = (StudentSchedule) entity.getBean();
            Person teacher = ssc.getSection().getPrimaryStaff().getPerson();
            Course course = ssc.getSection().getSchoolCourse().getCourse();
            return teacher.getLastName() + " " + teacher.getFirstName().substring(0, 1) + " - "
                    + courseNumbers.get(course.getNumber().substring(0, 2));
        }
    }

    /**
     * Initialize.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    public void initialize() {
        courseNumbers.put("23", "ELA");
        courseNumbers.put("27", "Mathematics");
        courseNumbers.put("55", "ESOL");

        if (getSetupErrors().size() == 0) {
            /*
             * Build the criteria/query for the staff to include in this export based on user input.
             */
            Criteria sscCriteria = getSscCriteria();
            QueryByCriteria sscQuery = new QueryByCriteria(StudentSchedule.class, sscCriteria);
            int sort = ((Integer) getParameter(PARAM_SORT)).intValue();

            switch (sort) {
                case 0: // Student group name: Primary Teacher Last Name +
                        // Teacher First Name Initial + hyphen + Content Area
                    sscQuery.addOrderByAscending(
                            StudentSchedule.REL_SECTION + PATH_DELIMITER + MasterSchedule.REL_PRIMARY_STAFF
                                    + PATH_DELIMITER + Staff.REL_PERSON + PATH_DELIMITER + Person.COL_LAST_NAME);
                    sscQuery.addOrderByAscending(
                            StudentSchedule.REL_SECTION + PATH_DELIMITER + MasterSchedule.REL_PRIMARY_STAFF
                                    + PATH_DELIMITER + Staff.REL_PERSON + PATH_DELIMITER + Person.COL_FIRST_NAME);
                    sscQuery.addOrderByAscending(StudentSchedule.REL_STUDENT + PATH_DELIMITER + Student.COL_NAME_VIEW);
                    break;

                case 1: // Name
                    sscQuery.addOrderByAscending(
                            StudentSchedule.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_NAME_VIEW);
                    break;

                case 2: // LASID
                    sscQuery.addOrderByAscending(
                            StudentSchedule.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_LOCAL_ID);
                    break;

                case 3: // GTID
                    sscQuery.addOrderByAscending(
                            StudentSchedule.REL_STUDENT + PATH_DELIMITER
                                    + translateAliasToJavaName(STUDENT_ID_CODE, true));
                    break;

                default:
                    sscQuery.addOrderByAscending(
                            StudentSchedule.REL_SECTION + PATH_DELIMITER + MasterSchedule.REL_PRIMARY_STAFF
                                    + PATH_DELIMITER + Staff.REL_PERSON + PATH_DELIMITER + Person.COL_LAST_NAME);
                    sscQuery.addOrderByAscending(
                            StudentSchedule.REL_SECTION + PATH_DELIMITER + MasterSchedule.REL_PRIMARY_STAFF
                                    + PATH_DELIMITER + Staff.REL_PERSON + PATH_DELIMITER + Person.COL_FIRST_NAME);
                    sscQuery.addOrderByAscending(StudentSchedule.REL_STUDENT + PATH_DELIMITER + Student.COL_NAME_VIEW);
                    break;
            }

            // Set the query to be used for student selection.
            setQuery(sscQuery);


            // Add any retrievers or validators.
            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put(GroupNameRetriever.RETRIEVER_ID, new GroupNameRetriever());
            super.addCalcs(calcs);
        }
    }

    /**
     * Implementing the select student filter based on parameters.
     *
     * @param studentCriteria X2Criteria
     */
    private void filterStudentCriteria(X2Criteria studentCriteria) {
        String queryString = (String) getParameter(PARAM_QUERY_STRING);
        int queryBy = ((Integer) getParameter(PARAM_QUERY_BY)).intValue();
        switch (queryBy) {
            case 1: // YOG
                studentCriteria.addEqualTo(StudentSchedule.REL_STUDENT + PATH_DELIMITER + Student.COL_YOG,
                        queryString);
                break;

            case 2: // LASID
                studentCriteria.addEqualTo(StudentSchedule.REL_STUDENT + PATH_DELIMITER + Student.COL_LOCAL_ID,
                        queryString);
                break;

            case 3: // GTID
                studentCriteria.addEqualTo(
                        StudentSchedule.REL_STUDENT + PATH_DELIMITER + translateAliasToJavaName(STUDENT_ID_CODE, true),
                        queryString);
                break;

            case 4: // Snapshot
                addRecordSetCriteria(studentCriteria, queryString);
                break;

            default:
                // Take all students in the district
                break;
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

        criteria.addIn(StudentSchedule.REL_STUDENT + PATH_DELIMITER + X2BaseBean.COL_OID,
                new SubQuery(RecordSetKey.class, RecordSetKey.COL_OBJECT_OID, recordSetCriteria));
    }

    /**
     * Returns the criteria that retrieves all students that should be included in the export.
     *
     * @param testId String
     * @return Criteria
     */
    private Criteria getSscCriteria() {
        X2Criteria sscCriteria = new X2Criteria();

        /*
         * Active students
         */
        String activeCode =
                PreferenceManager.getPreferenceValue(getOrganization(), SystemPreferenceDefinition.STUDENT_ACTIVE_CODE);
        sscCriteria.addEqualTo(StudentSchedule.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_ENROLLMENT_STATUS,
                activeCode);
        sscCriteria.addNotEqualTo(
                StudentSchedule.REL_STUDENT + PATH_DELIMITER + translateAliasToJavaName(STUDENT_EXCLUDE_CODE, true),
                BooleanAsStringConverter.TRUE);
        sscCriteria.addNotEmpty(
                StudentSchedule.REL_STUDENT + PATH_DELIMITER + translateAliasToJavaName(STUDENT_ID_CODE, true),
                getBroker().getPersistenceKey());
        filterStudentCriteria(sscCriteria);

        /*
         * Active schools
         */
        if (isSchoolContext()) {
            sscCriteria.addEqualTo(StudentSchedule.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_SCHOOL_OID,
                    getSchool().getOid());
        }
        sscCriteria.addEqualTo(StudentSchedule.REL_STUDENT + PATH_DELIMITER + SisStudent.REL_SCHOOL + PATH_DELIMITER
                + SisSchool.COL_INACTIVE_INDICATOR, Boolean.FALSE);
        sscCriteria.addEqualTo(StudentSchedule.REL_STUDENT + PATH_DELIMITER + SisStudent.REL_SCHOOL + PATH_DELIMITER
                + SisSchool.COL_ARCHIVE_INDICATOR, Boolean.FALSE);
        sscCriteria.addNotEqualTo(StudentSchedule.REL_STUDENT + PATH_DELIMITER + SisStudent.REL_SCHOOL + PATH_DELIMITER
                + translateAliasToJavaName(SCHOOL_EXCLUDE_CODE, true), BooleanAsStringConverter.TRUE);

        /*
         * Active schedule
         */
        sscCriteria.addEqualToField(StudentSchedule.REL_SCHEDULE + ModelProperty.PATH_DELIMITER +
                Schedule.REL_SCHOOL + ModelProperty.PATH_DELIMITER +
                SisSchool.REL_ACTIVE_SCHOOL_SCHED + ModelProperty.PATH_DELIMITER +
                SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID,
                StudentSchedule.COL_SCHEDULE_OID);

        /*
         * Specific course numbers
         */
        X2Criteria coursesCriteria = new X2Criteria();
        for (String crsNum : courseNumbers.keySet()) {
            X2Criteria crsCriteria = new X2Criteria();
            crsCriteria.addBeginsWith(StudentSchedule.REL_SECTION + PATH_DELIMITER + MasterSchedule.REL_SCHOOL_COURSE
                    + PATH_DELIMITER + SchoolCourse.REL_COURSE + PATH_DELIMITER + Course.COL_NUMBER, crsNum);
            coursesCriteria.addOrCriteria(crsCriteria);
        }
        sscCriteria.addAndCriteria(coursesCriteria);

        sscCriteria.addNotEmpty(
                StudentSchedule.REL_SECTION + PATH_DELIMITER + MasterSchedule.REL_PRIMARY_STAFF + PATH_DELIMITER
                        + Staff.REL_PERSON + PATH_DELIMITER + Person.COL_EMAIL01,
                getBroker().getPersistenceKey());

        return sscCriteria;
    }

}
