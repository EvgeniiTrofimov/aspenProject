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
package com.x2dev.procedures.statereporting.ri;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.OrganizationDefinition;
import com.follett.fsc.core.k12.beans.RecordSet;
import com.follett.fsc.core.k12.beans.RecordSetKey;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.Transcript;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * RI state report for Class Roster export. This class implements the data
 * export for the RI Class Roster export.
 *
 * @author X2 Development Corporation
 */
public class Ricats extends StateReportData {
    /**
     * Implementation of StateReportEntity to be used by the Class Roster export. This
     * must be a public static inner class with a public no argument constructor
     * so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class RicatsEntity extends StateReportEntity {
        private static final String NO_PERFMON4J_INSTRUMENTATION = "";
        Ricats m_ricats;
        Transcript m_transcript;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public RicatsEntity() {
            // Empty constructor
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean)
                throws X2BaseException {
            super.intitialize(data, bean);

            m_ricats = (Ricats) data;
            m_transcript = (Transcript) bean;


        }

        /**
         * Gets the transcript.
         *
         * @return Transcript
         */
        public Transcript getTranscript() {
            return m_transcript;
        }
    }

    /**
     * Alias on the COURSE table for CTE CIP code.
     */
    public static final String ALIAS_CIP = "DOE CIP";

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
     * Input parameter: Select organization level.
     */
    public static final String ORGANIZATION_PARAM = "orgOid";

    /**
     * Name for the active students only parameter. The value is a boolean.
     */
    public static final String SASID_STUDENTS_ONLY_PARAM = "sasidStudentsOnly";

    private static final String EXCLUDE_ALIAS_STD = "DOE EXCLUDE STD";

    /**
     * Local variables.
     */
    protected String m_excludeStdField;
    protected boolean m_sasidStudentsOnly;
    private String m_fieldCip = null;
    private String m_orgFieldStr = null;
    private String m_orgOid = null;

    /**
     * Initialize the data module.
     * Initialize necessary working resources.
     * Define query for students to load.
     * Define list of field definitions for the export.
     * 
     * @see com.x2dev.sis.tools.stateexports.StateReportData#initialize(java.util.Map,
     *      com.x2dev.sis.model.business.X2Broker)
     */
    @Override
    public void initialize() {
        m_fieldCip = translateAliasToJavaName(ALIAS_CIP, true);
        m_excludeStdField = translateAliasToJavaName(EXCLUDE_ALIAS_STD, false);

        m_sasidStudentsOnly = true;
        Boolean sasidStudentsOnly = (Boolean) getParameter(SASID_STUDENTS_ONLY_PARAM);
        if (sasidStudentsOnly != null) {
            m_sasidStudentsOnly = sasidStudentsOnly.booleanValue();
        }

        // Get organization Oid and level and field.
        m_orgOid = (String) getParameter(ORGANIZATION_PARAM);
        Organization organization = null;
        OrganizationDefinition def = null;
        if (!StringUtils.isEmpty(m_orgOid)) {
            organization = (Organization) getBroker().getBeanByOid(Organization.class, m_orgOid);
            if (organization != null) {
                def = organization.getOrganizationDefinition();
                m_orgFieldStr = "organization" + Integer.toString(def.getLevel() + 1) + "Oid";
            }
        }

        if (getSetupErrors().size() == 0) {
            Criteria studentTranscriptCriteria = getStudentScheduleCriteria();
            QueryByCriteria studentTranscriptQuery = new QueryByCriteria(Transcript.class,
                    studentTranscriptCriteria);

            int sort = ((Integer) getParameter(SORT_PARAM)).intValue();
            switch (sort) {
                case 0: // Name
                    studentTranscriptQuery
                            .addOrderByAscending(Transcript.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_NAME_VIEW);
                    break;

                case 1: // YOG
                    studentTranscriptQuery
                            .addOrderByAscending(Transcript.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_YOG);
                    studentTranscriptQuery
                            .addOrderByAscending(Transcript.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_NAME_VIEW);
                    break;

                case 2: // School
                    studentTranscriptQuery
                            .addOrderByAscending(Transcript.REL_STUDENT + PATH_DELIMITER + SisStudent.REL_SCHOOL +
                                    +PATH_DELIMITER + SisSchool.COL_NAME);
                    studentTranscriptQuery
                            .addOrderByAscending(Transcript.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_NAME_VIEW);
                    break;

                case 3: // LASID
                    studentTranscriptQuery
                            .addOrderByAscending(Transcript.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_LOCAL_ID);
                    break;

                case 4: // SASID
                    studentTranscriptQuery
                            .addOrderByAscending(Transcript.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_STATE_ID);

                    break;

                default:
                    studentTranscriptQuery
                            .addOrderByAscending(Transcript.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_NAME_VIEW);
                    break;
            }

            // Set the query to be used for student selection.
            setQuery(studentTranscriptQuery);
            setEntityClass(RicatsEntity.class);
        }
    }

    /**
     * Retrieve the final grade and performs rounding if the grade is numeric
     * Final grade can be the letter grade of numeric.
     */
    protected class RetrieveFinalGrade implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field) {
            RicatsEntity ricats = ((RicatsEntity) entity);
            Transcript transcript = (Transcript) ricats.getBean();

            String finalGrade = transcript.getFinalGrade();

            String returnFinalGrade = "";

            try {
                float grade = Float.parseFloat(finalGrade);
                int roundedGrade = Math.round(grade);

                returnFinalGrade = String.valueOf(roundedGrade);
            } catch (NumberFormatException nfe) {
                // lettergrade
                returnFinalGrade = finalGrade;
            }

            return returnFinalGrade;
        }
    }

    /**
     * Returns the criteria that retrieves all student schedules that should be included in the
     * export.
     *
     * @return Criteria
     */
    private Criteria getStudentScheduleCriteria() {
        X2Criteria userCriteria = new X2Criteria();
        userCriteria.addAndCriteria(getReportingCriteria());

        String queryString = (String) getParameter(QUERY_STRING_PARAM);
        int queryBy = ((Integer) getParameter(QUERY_BY_PARAM)).intValue();
        switch (queryBy) {
            case 1: // YOG
                userCriteria.addEqualTo(Transcript.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_YOG, queryString);
                break;

            case 2: // LASID
                userCriteria.addEqualTo(Transcript.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_LOCAL_ID, queryString);
                break;

            case 3: // SASID
                userCriteria.addEqualTo(Transcript.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_STATE_ID, queryString);
                break;

            case 4: // Snapshot
                addRecordSetCriteria(userCriteria, queryString);
                break;

            default:
                // Take all students in the district
                break;
        }

        return userCriteria;
    }

    /**
     * Returns the criteria that retrieves current year transcript records that should be included
     * in the export.
     *
     * @return Criteria
     */
    private Criteria getReportingCriteria() {
        DistrictSchoolYearContext currentSchoolYearContext = getCurrentContext();

        X2Criteria currentYearTranscriptCriteria = new X2Criteria();
        currentYearTranscriptCriteria.addEqualTo(Transcript.COL_DISTRICT_CONTEXT_OID,
                currentSchoolYearContext.getOid());
        currentYearTranscriptCriteria.addNotEmpty(Transcript.REL_SCHOOL_COURSE + PATH_DELIMITER +
                SchoolCourse.REL_COURSE + PATH_DELIMITER +
                m_fieldCip,
                getBroker().getPersistenceKey());

        if (isSchoolContext()) {
            currentYearTranscriptCriteria.addEqualTo(Transcript.COL_SCHOOL_OID,
                    getSchool().getOid());
        } else {
            if (m_orgFieldStr != null && !StringUtils.isEmpty(m_orgOid)) {
                currentYearTranscriptCriteria.addEqualTo(Transcript.REL_SCHOOL + PATH_DELIMITER +
                        m_orgFieldStr,
                        m_orgOid);
            }
            currentYearTranscriptCriteria.addNotEqualTo(Transcript.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_INACTIVE_INDICATOR,
                    Boolean.TRUE);
            currentYearTranscriptCriteria.addNotEqualTo(Transcript.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_ARCHIVE_INDICATOR,
                    Boolean.TRUE);
        }
        // Check exclude student from state reporting flag if present.
        if (!StringUtils.isEmpty(m_excludeStdField)) {
            currentYearTranscriptCriteria.addNotEqualTo(Transcript.REL_STUDENT + PATH_DELIMITER +
                    m_excludeStdField,
                    BooleanAsStringConverter.TRUE);
        }

        // If "Student must have SASID" is checked, get only students with non-empty state ids
        if (m_sasidStudentsOnly) {
            currentYearTranscriptCriteria.addNotEmpty(Transcript.REL_STUDENT + PATH_DELIMITER +
                    SisStudent.COL_STATE_ID, getBroker().getPersistenceKey());
        }

        return currentYearTranscriptCriteria;
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

        criteria.addIn(Transcript.COL_STUDENT_OID, new SubQuery(RecordSetKey.class,
                RecordSetKey.COL_OBJECT_OID, recordSetCriteria));
    }
}
