/*
 * ====================================================================
 *
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
package com.x2dev.procedures.statereporting.ga;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.Race;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.StudentContact;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentSchedule;
import com.x2dev.sis.model.beans.StudentScheduleChange;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.sis.tools.stateexports.StudentScheduleSpan;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Georgia state report for GA GKIDS Roster export.
 * This class implements the data export GKIDS Roster export.
 *
 * @author X2 Development Corporation
 */
public class PandaRoster extends StateReportData {

    /**
     * Implementation of StateReportEntity to be used by the GKIDS Roster export.
     * This must be a public static inner class with a public no argument
     * constructor so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class GKIDSRosterEntity extends StateReportEntity {

        List<StudentScheduleSpan> m_scheduleSpans;
        PandaRoster m_data;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public GKIDSRosterEntity() {
            // public no argument constructor for dynamic instantiation.
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
                    ", GTID: " + this.getFieldValue("GTID") +
                    (getData().isSchoolContext() ? ", SCHOOL: " + student.getSchool().getName() : "") +
                    "]";
            return name;
        }

        /**
         * Retrieves and returns a student's schedule span, based on requested student
         * and schedule.
         *
         * @return StudentScheduleSpan for the current row
         */
        public StudentScheduleSpan getScheduleSpan() {
            StudentScheduleSpan currentSpan = null;

            if (m_scheduleSpans != null && getCurrentRow() < m_scheduleSpans.size() && getCurrentRow() >= 0) {
                currentSpan = m_scheduleSpans.get(getCurrentRow());
            }

            return currentSpan;
        }

        /**
         * Initialize.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc
         *      .core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            /*
             * Filter:
             * The export shall return all student enrolled in a class where scheduleMaster.DOE
             * PANDA CLASS # is not null/blank AND student
             * Begin Date (in class ) is = > ""Start Date"" (input parameter) and Begin Date is = <
             * ""End Date"" (input parameter).
             *
             * Not all students will have a ""Start Date"" in their schedule history record. In this
             * case we use the term start date based on the term code assigned to the class."
             */


            m_data = (PandaRoster) data;
            SisStudent student = (SisStudent) bean;
            m_scheduleSpans = new ArrayList<StudentScheduleSpan>();

            // This will run once for each Student
            List<StudentScheduleSpan> studentScheduleSpans = null;
            studentScheduleSpans = m_data.m_helper.getStudentScheduleSpans(student);
            if (studentScheduleSpans != null && studentScheduleSpans.size() > 0) {
                for (StudentScheduleSpan scheduleSpan : studentScheduleSpans) {
                    if (scheduleSpan.getEntryChange() != null
                            && scheduleSpan.getExitChange() != null
                            && scheduleSpan.getEntryChange().getEffectiveDate()
                                    .equals(scheduleSpan.getExitChange().getEffectiveDate())) {
                        continue;
                    }
                    MasterSchedule section = scheduleSpan.getSection();
                    PlainDate scheduleStartDate = scheduleSpan.getEntryDate();

                    if (section != null) {
                        if (!scheduleStartDate.after(m_data.m_startDate)
                                && !scheduleStartDate.after(m_data.m_endDate)) {
                            m_scheduleSpans.add(scheduleSpan);
                        }
                    }
                }
            }
            // Individual records are a single Student Schedule Span which
            // represent one section that a student was in.
            setRowCount(m_scheduleSpans.size());
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
     * Get Student Begin Date.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveBeginDate implements FieldRetriever {

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

            GKIDSRosterEntity scEntity = (GKIDSRosterEntity) entity;
            PlainDate entryDate = scEntity.getScheduleSpan().getEntryDate();
            if (scEntity.getScheduleSpan().getEntryChange() != null
                    && scEntity.getScheduleSpan().getEntryChange().getEffectiveDate()
                            .after(entryDate)
                    && scEntity.getScheduleSpan().getEntryChange().getChangeTypeCode()
                            .equals(StudentScheduleChange.CODE_ADD)) {
                entryDate = scEntity.getScheduleSpan().getEntryChange().getEffectiveDate();
            }
            return entryDate;
        }
    }

    /**
     * Get Student CLASS#.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveClass implements FieldRetriever {

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
            GKIDSRosterEntity scEntity = (GKIDSRosterEntity) entity;
            StudentScheduleSpan currentSpan = scEntity.getScheduleSpan();
            MasterSchedule currentSection = currentSpan.getSection();
            String param = (String) currentSection.getFieldValueByBeanPath(m_pathClass);
            return param;
        }
    }

    /**
     * Get Student Dual Language Learner.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveELL implements FieldRetriever {

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
            SisStudent student = (SisStudent) entity.getBean();
            String param = (String) student.getFieldValueByBeanPath(m_pathEll);
            return param == null ? "N" : "Y".equals(param) ? "Y" : "N";
        }
    }

    /**
     * Get Student End Date.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveEndDate implements FieldRetriever {

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

            GKIDSRosterEntity scEntity = (GKIDSRosterEntity) entity;
            PlainDate exitDate = null;
            if (scEntity.getScheduleSpan().getExitChange() != null && scEntity.getScheduleSpan().getExitChange()
                    .getChangeTypeCode().equals(StudentScheduleChange.CODE_DROP)) {
                exitDate = scEntity.getScheduleSpan().getExitChange().getEffectiveDate();
            }
            return exitDate;
        }
    }

    /**
     * Get Student Funding.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveFunding implements FieldRetriever {

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
            GKIDSRosterEntity scEntity = (GKIDSRosterEntity) entity;
            StudentScheduleSpan currentSpan = scEntity.getScheduleSpan();
            MasterSchedule currentSection = currentSpan.getSection();
            String param = (String) currentSection.getFieldValueByBeanPath(m_pathFunding);

            return param;
        }

    }


    /**
     * Get Student Parent/Guardian Relationship.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveGuardianCode implements FieldRetriever {

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
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            String rel = null;
            SisStudent student = (SisStudent) entity.getBean();
            StudentContact primaryContact = student.getPrimaryContact();

            if (primaryContact != null) {
                rel = data.lookupStateValue(StudentContact.class, StudentContact.COL_RELATIONSHIP_CODE,
                        primaryContact.getRelationshipCode());
            }

            return rel;
        }
    }

    /**
     * Get Student Guardian Name.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveGuardianName implements FieldRetriever {

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
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            String param = (String) field.getParameter();
            String guardianName = null;
            SisStudent student = (SisStudent) entity.getBean();
            StudentContact primaryContact = student.getPrimaryContact();
            if (primaryContact != null) {
                String relCode = data.lookupStateValue(StudentContact.class, StudentContact.COL_RELATIONSHIP_CODE,
                        primaryContact.getRelationshipCode());
                if (!StringUtils.isEmpty(relCode)) {
                    Person primaryContactPerson = primaryContact.getPerson();
                    if (primaryContactPerson != null) {
                        guardianName = param.equals("1") ? primaryContactPerson.getFirstName()
                                : primaryContactPerson.getLastName();
                        if (guardianName.length() > 60) {
                            guardianName = guardianName.substring(0, 60);
                        }
                    }
                }
            }

            return guardianName;
        }
    }

    /**
     * Get Student IEP.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveIEP implements FieldRetriever {

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
            SisStudent student = (SisStudent) entity.getBean();
            if (student.getSpedStatusCode() != null
                    && "S".equals(lookupStateValue(SisStudent.class, m_pathSpedStatus, student.getSpedStatusCode()))) {
                return "Y";
            }

            return "N";
        }

    }

    /**
     * Get Student IEP.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveMiddleName implements FieldRetriever {

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
            SisStudent student = (SisStudent) entity.getBean();
            SisPerson person = student.getPerson();
            if (StringUtils.isEmpty(person.getMiddleName())
                    && ((String) person.getFieldValueByBeanPath(m_pathMidName)).equals("3")) {
                return "NMN";
            }

            return person.getMiddleName();
        }

    }

    /**
     * Returns an indicator if the person is included in the specified race.
     * This implementation looks up all race codes for a person and finds
     * the race entry for the specified race. It returns a true/false value
     * indicating the presence of the race code record.
     *
     * The calculation parameter should be a string with three characters:
     * character 1:
     * The character to return if the requested race code is present.
     * character 2:
     * The character to return if the requested race code is not present.
     * character(s) 3+:
     * The reference code state code value in the reference table for race codes.
     * In GA, this is:
     * "W" - White
     * "B" - Black
     * "S" - Asian
     * "I" - Indian/Native/Alaskan
     * "P" - Pacific
     *
     * Ex: "SNS" searches for the Asian code, returns "S" if present, "N" otherwise.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveRace implements FieldRetriever {
        private Map<String, ReferenceCode> m_raceCodes;
        private Map<String, Collection<Race>> m_raceCodeMap;


        /**
         * Instantiates a new retrieve race.
         */
        public RetrieveRace() {
            // Get race code reference codes for use in the race retriever.
            Criteria criteria = new Criteria();
            criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, REF_CODES_RACE);
            QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, criteria);
            m_raceCodes = getBroker().getMapByQuery(query, ReferenceCode.COL_STATE_CODE, 5);

            // Load the race codes for all students included in the export.
            SubQuery studentQuery =
                    new SubQuery(SisStudent.class, SisStudent.COL_PERSON_OID, m_helper.getStudentCriteria());
            criteria = new Criteria();
            criteria.addIn(Race.COL_PERSON_OID, studentQuery);
            QueryByCriteria raceQuery = new QueryByCriteria(Race.class, criteria);
            m_raceCodeMap = getBroker().getGroupedCollectionByQuery(raceQuery, Race.COL_PERSON_OID, 100);
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String param = (String) field.getParameter();
            String trueChar = param.substring(0, 1);
            String falseChar = param.substring(1, 2);
            String requestCode = param.substring(2);

            String raceCode = falseChar;

            SisStudent student = (SisStudent) entity.getBean();
            Collection<Race> races = m_raceCodeMap.get(student.getPersonOid());

            // Find the reference code that we are looking for.
            ReferenceCode refCode = m_raceCodes.get(requestCode);
            if (refCode != null && races != null) {
                for (Race race : races) {
                    if (refCode.getCode().equals(race.getRaceCode())) {
                        raceCode = trueChar;
                        break;
                    }
                }
            }

            return raceCode;
        }
    }



    /**
     * Get SSN.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveSSN implements FieldRetriever {

        private static final String PREFIX_BLANK = "999";

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
            SisStudent student = (SisStudent) entity.getBean();
            String param = (String) student.getPerson().getFieldValueByBeanPath(m_pathSSN);
            if (param == null || param.startsWith(PREFIX_BLANK)) {
                return null;
            }
            return param;
        }
    }

    /**
     * The Class ValidateWReasonDetail.
     */
    class ValidateWReasonDetail implements FieldValidator {
        public static final String VAL_ID = "VAL-WRSN-DET";

        private static final String CODE_REQUIRED_DETAIL = "5";
        private static final String FIELD_ID_W_REASON_CODE = "Waiver Reason Code";
        private static final String FIELD_ID_W_REASON_DETAIL = "Waiver Reason Detail";
        private static final String MESSAGE =
                FIELD_ID_W_REASON_DETAIL + " required when " + FIELD_ID_W_REASON_CODE + " = "
                        + CODE_REQUIRED_DETAIL;

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
            String reasonCode = entity.getFieldValue(FIELD_ID_W_REASON_CODE);
            if (CODE_REQUIRED_DETAIL.equals(reasonCode) && StringUtils.isEmpty(value)) {
                StateReportValidationError error =
                        new StateReportValidationError(entity, field, FIELD_ID_W_REASON_DETAIL + " missing", MESSAGE);
                errors.add(error);
            }
            return errors;
        }

    }

    /*
     * Aliases
     */
    private static final String ALIAS_CLASS = "DOE PANDA CLASS";
    private static final String ALIAS_ELL = "DOE ELL";
    private static final String ALIAS_FUNDING = "DOE PK FUNDING";
    private static final String ALIAS_MIDDLE_NAME = "DOE MN Verification";
    private static final String ALIAS_SPED_STATUS = "DOE SPED Type";
    private static final String ALIAS_SSN = "DOE SSN";

    /*
     * Calculation Ids
     */
    private static final String GID_BDATE = "GA-GKIDS-BDATE";
    private static final String GID_CLASS = "GA-GKIDS-CLASS";
    private static final String GID_ELL = "GA-GKIDS-ELL";
    private static final String GID_EDATE = "GA-GKIDS-EDATE";
    private static final String GID_FUNDING = "GA-GKIDS-FUNDING";
    private static final String GID_GUARDIAN_NAME = "GID-GUARDIAN-NAME";
    private static final String GID_GUARDIAN_CODE = "GID-GUARDIAN-CODE";
    private static final String GID_IEP = "GA-GKIDS-IEP";
    private static final String GID_MNAME = "GA-GKIDS-MNAME";
    private static final String GID_RACE = "GID-RACE";
    private static final String GID_SSN = "GA-GKIDS-SSN";

    /*
     * Parameters
     */
    private static final String PARAM_START_DATE = "startDate";
    private static final String PARAM_END_DATE = "endDate";

    /*
     * Reference Table
     */
    private static final String REF_CODES_RACE = "rtbRaceCodes";

    /*
     * Member variables
     */
    private Date m_endDate;
    private StudentHistoryHelper m_helper;
    private String m_pathClass;
    private String m_pathEll;
    private String m_pathFunding;
    private String m_pathMidName;
    private String m_pathSpedStatus;
    private String m_pathSSN;
    private Date m_startDate;

    /**
     * Initialize.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    public void initialize() {
        m_startDate = (Date) getParameter(PARAM_START_DATE);
        if (m_startDate == null) {
            m_startDate = getCurrentContext().getStartDate();
        }
        m_endDate = (Date) getParameter(PARAM_END_DATE);

        if (m_endDate == null) {
            m_endDate = new PlainDate();
        }

        m_helper = new StudentHistoryHelper(this);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_SCHEDULE_SPANS);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_INPUT, Boolean.TRUE);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_SCHOOL, Boolean.TRUE);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_EXCLUDE_FUTURE_SCHEDULES, Boolean.TRUE);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_endDate);
        initializeFields();

        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {

            m_helper.getStudentScheduleCriteria().addNotEmpty(
                    StudentSchedule.REL_SECTION + ModelProperty.PATH_DELIMITER + m_pathClass,
                    getBroker().getPersistenceKey());
            m_helper.getStudentScheduleChangeCriteria().addNotEmpty(
                    StudentScheduleChange.REL_MASTER_SCHEDULE + ModelProperty.PATH_DELIMITER + m_pathClass,
                    getBroker().getPersistenceKey());

            setQuery(m_helper.getStudentQuery(false));

            // Set the entity class
            setEntityClass(GKIDSRosterEntity.class);

            // Add any retrievers.
            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put(GID_SSN, new RetrieveSSN());
            calcs.put(GID_ELL, new RetrieveELL());
            calcs.put(GID_IEP, new RetrieveIEP());
            calcs.put(GID_BDATE, new RetrieveBeginDate());
            calcs.put(GID_CLASS, new RetrieveClass());
            calcs.put(GID_FUNDING, new RetrieveFunding());
            calcs.put(GID_EDATE, new RetrieveEndDate());
            calcs.put(GID_MNAME, new RetrieveMiddleName());
            calcs.put(GID_RACE, new RetrieveRace());
            calcs.put(GID_GUARDIAN_NAME, new RetrieveGuardianName());
            calcs.put(GID_GUARDIAN_CODE, new RetrieveGuardianCode());

            HashMap vals = new HashMap<String, FieldValidator>();
            vals.put(ValidateWReasonDetail.VAL_ID, new ValidateWReasonDetail());

            super.addCalcs(calcs);
            super.addValidators(vals);
        }
    }

    /**
     * Initialize fields.
     */
    private void initializeFields() {
        m_pathClass = translateAliasToJavaName(ALIAS_CLASS, true);
        m_pathMidName = translateAliasToJavaName(ALIAS_MIDDLE_NAME, true);
        m_pathSSN = translateAliasToJavaName(ALIAS_SSN, true);
        m_pathEll = translateAliasToJavaName(ALIAS_ELL, true);
        m_pathFunding = translateAliasToJavaName(ALIAS_FUNDING, true);
        m_pathSpedStatus = translateAliasToJavaName(ALIAS_SPED_STATUS, true);
    }
}
