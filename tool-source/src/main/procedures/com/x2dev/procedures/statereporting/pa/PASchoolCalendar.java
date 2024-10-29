/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2018 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.pa;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.SchoolCalendar;
import com.follett.fsc.core.k12.beans.SchoolCalendarDate;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Map;
import java.util.Set;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * The Class PASchoolCalendar.
 */
/*
 * Export procedure Code Template
 */
public class PASchoolCalendar extends StateReportData {
    /**
     * Implementation of StateReportEntity to be used by the PASchoolCalendar
     * export. This must be a public static inner class with a public no
     * argument constructor so it can be instantiated through reflection.
     */
    public static class PASchoolCalendarEntity extends StateReportEntity {
        /**
         * Entity instance variables.
         */
        PASchoolCalendar m_exportData;
        protected PlainDate m_inSessionEnd;
        protected PlainDate m_inSessionStart;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public PASchoolCalendarEntity() {} // Empty constructor.

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            SchoolCalendar schoolCalendar = (SchoolCalendar) getBean();
            String name = schoolCalendar.getSchool().getName() + "[" + schoolCalendar.getCalendarId() + "]";

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
            m_exportData = (PASchoolCalendar) data;
            LinkedList<SchoolCalendarDate> calDates = m_exportData.m_calendarDays.get(bean.getOid());
            if (calDates != null && calDates.size() > 0) {
                m_inSessionStart = calDates.get(0).getDate();
                m_inSessionEnd = calDates.get(calDates.size() > 1 ? calDates.size() - 1 : 0).getDate();
            }
        }
    }

    /**
     *
     * Retriever for calendar id.
     */
    public class RetrieveCalendarId implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            SchoolCalendar calendar = (SchoolCalendar) entity.getBean();
            StringBuilder result = new StringBuilder();
            result.append(calendar.getSchool().getSchoolId())
                    .append("-")
                    .append(calendar.getCalendarId());
            return result.toString();
        }
    }

    /**
     *
     * Retriever for in session start date.
     */
    protected class RetrieveISD implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity reportEntity, FieldDefinition field) {
            PASchoolCalendarEntity entity = (PASchoolCalendarEntity) reportEntity;
            return entity.m_inSessionStart;
        }
    }

    /**
     *
     * Retriever for in session end date.
     */
    protected class RetrieveIED implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity reportEntity, FieldDefinition field) {
            PASchoolCalendarEntity entity = (PASchoolCalendarEntity) reportEntity;
            return entity.m_inSessionEnd;
        }
    }

    /**
     * Validator for Act 80 Group. Required if field LOST DUE TO ACT 80 greater than zero.
     */
    protected class ValidateAct80Group implements FieldValidator {

        static final String EXPORT_FIELD_LOST_TO_80_ACT = "LOST DUE TO ACT 80";
        static final String VAL_ID = "CSD_VAL_ACT_80";

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
            if (!StringUtils.isEmpty(entity.getFieldValue(EXPORT_FIELD_LOST_TO_80_ACT))) {
                Integer lostTo80Act = Integer.valueOf(entity.getFieldValue(EXPORT_FIELD_LOST_TO_80_ACT));
                if (lostTo80Act != null && lostTo80Act.intValue() > 0 && StringUtils.isEmpty(value)) {
                    errors.add(new StateReportValidationError(entity, field, "Value Required",
                            "This field is required if Field 17 - TOTAL DAYS IN SESSION LOST DUE TO ACT 80 is greater than zero."));
                }
            }
            return errors;
        }
    }

    /**
     *
     * Validator for calendar id.
     */
    protected class ValidateCalId implements FieldValidator {

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
            SchoolCalendar cas = (SchoolCalendar) entity.getBean();
            Collection<SchoolCalendar> calendars = null;
            Map<String, Collection<SchoolCalendar>> calendarsMap = m_calendarIdMap.get(cas.getSchoolOid());
            if (calendarsMap != null) {
                calendars = calendarsMap.get(cas.getCalendarId());
            }
            if (calendars == null || calendars.size() > 1 || calendars.isEmpty()) {
                errors.add(new StateReportValidationError(entity, field, "Value in not unique",
                        "This field must contain a unique value"));
            }
            if (StringUtils.isEmpty(value)) {
                errors.add(new StateReportValidationError(entity, field, "Value Required",
                        "This field must contain a value"));
            }
            return errors;
        }
    }

    /**
     *
     * Validator for calendar description.
     */
    protected class ValidateCalendarDescription implements FieldValidator {

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
            if (StringUtils.isEmpty(value)) {
                errors.add(new StateReportValidationError(entity, field, "Value Required",
                        "This field must contain a value"));
            }
            return errors;
        }
    }

    /**
     *
     * Validator for graduation field required.
     */
    protected class ValidateGraduation implements FieldValidator {

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
            SchoolCalendar cal = (SchoolCalendar) entity.getBean();
            if (m_graduationSklOids.contains(cal.getSchool().getOid())) {
                if (StringUtils.isEmpty(value)) {
                    errors.add(new StateReportValidationError(entity, field, "Value Required",
                            "This field must contain a value for schools containing grade 12"));
                }
            }
            return errors;
        }
    }


    /**
     * Constants: Aliases, Fields, IDs, Parameters
     */
    static final String ALIAS_GRADUATION = "DOE GRADUATION";
    static final String ALIAS_SKL_EXCLUDE_FROM_REP = "DOE EXCLUDE SKL";
    static final String CALC_ID_CALID = "CSD_CALC_CAL_ID";
    static final String CALC_ID_IED = "CSD_CALC_IED";
    static final String CALC_ID_ISD = "CSD_CALC_ISD";
    static final String CALC_ID_LOST = "CSD_CALC_LOST";
    static final String GRADELEVEL12 = "012";
    static final String PARAM_CONTEXT_OID = "contextOid";
    static final String VAL_ID_CALID = "CSD_VAL_CAL_ID";
    static final String VAL_ID_CAL_DESCR = "CSD_VAL_CAL_DESCR";
    static final String VAL_ID_GRADUATION = "CSD_VAL_GRAD_DATE";

    /**
     * Instance variables.
     */
    protected Map<String, LinkedList<SchoolCalendarDate>> m_calendarDays;
    protected Map<String, Map<String, Collection<SchoolCalendar>>> m_calendarIdMap;
    protected String m_fieldGraduation;
    protected String m_fieldSklExclude;
    protected Set<String> m_graduationSklOids;
    protected PlainDate m_reportDate;
    protected Set<String> m_setGradeLevel12;
    protected StudentHistoryHelper m_studentHelper;

    /**
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    /*
     * Initialize the data module.
     * Initialize necessary working resources.
     * Define query to load.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(java.util.Map,
     * com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    public void initialize() {
        // insure that all aliases and any other resources needed to operate the report are
        // available in the database.
        // setup error is created if the alias is not found.
        initializeFields();
        if (getSetupErrors().size() != 0) {
            return;
        }
        m_calendarDays = new HashMap<String, LinkedList<SchoolCalendarDate>>();
        /*
         * Build criteria, query and retrievers maps.
         */
        X2Criteria schoolCalendarCriteria = getSchoolCalendarCriteria();
        QueryByCriteria schoolCalendarQuery = new QueryByCriteria(SchoolCalendar.class, schoolCalendarCriteria, true);
        setEntityClass(PASchoolCalendarEntity.class);
        getSchoolCalendarDates(schoolCalendarCriteria);
        m_calendarIdMap = getBroker().getGroupedCollectionByQuery(schoolCalendarQuery,
                new String[] {SchoolCalendar.COL_SCHOOL_OID, SchoolCalendar.COL_CALENDAR_ID},
                new int[] {50, 10});

        // generate a list of schools (sklOid) from the students.
        getGradeLevel12TypeCodes();
        getGradeLevel12SchoolOids();
        setQuery(schoolCalendarQuery);
        // Add any necessary FieldRetrievers an Field Validators
        initFieldValidators();
        initFieldRetrievers();
    }

    /**
     * Load the grade levels that correspond to grade level 12.
     *
     * @return void
     */
    private void getGradeLevel12TypeCodes() {
        m_setGradeLevel12 = new HashSet<String>();
        DataDictionaryField fld = getDataDictionaryField(SisStudent.class, SisStudent.COL_GRADE_LEVEL);
        ReferenceTable refTable = fld.getReferenceTable();
        if (refTable != null) {
            Collection<ReferenceCode> codes = refTable.getReferenceCodes(getBroker());
            for (ReferenceCode code : codes) {
                if (GRADELEVEL12.equals(code.getStateCode())) {
                    m_setGradeLevel12.add(code.getCode());
                }
            }
        }
    }

    /**
     * Load the schools that contain grade level 12 students.
     *
     * @return void
     */
    private void getGradeLevel12SchoolOids() {
        m_graduationSklOids = new HashSet<String>();
        m_reportDate = getOrganization().getCurrentContext().getEndDate();
        m_studentHelper = new StudentHistoryHelper(this);
        m_studentHelper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
        m_studentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);
        X2Criteria criteria = m_studentHelper.getStudentCriteria();
        criteria.addIn(SisStudent.COL_GRADE_LEVEL, m_setGradeLevel12);
        ReportQueryByCriteria schoolIdQuery = new ReportQueryByCriteria(SisStudent.class,
                new String[] {SisStudent.COL_SCHOOL_OID},
                criteria, true);
        ReportQueryIterator schoolIdIterator = null;
        try {
            schoolIdIterator = getBroker().getReportQueryIteratorByQuery(schoolIdQuery);
            while (schoolIdIterator.hasNext()) {
                Object[] row = (Object[]) schoolIdIterator.next();
                m_graduationSklOids.add((String) row[0]);
            }
        } finally {
            if (schoolIdIterator != null) {
                schoolIdIterator.close();
            }
        }
    }

    /**
     * get the criteria for the school calendars.
     *
     * @return X 2 criteria
     */
    private X2Criteria getSchoolCalendarCriteria() {
        X2Criteria schoolCritera = new X2Criteria();
        String contextOid = (String) getParameter(PARAM_CONTEXT_OID);
        if (StringUtils.isEmpty(contextOid)) {
            schoolCritera.addEqualTo(SchoolCalendar.COL_DISTRICT_CONTEXT_OID, getOrganization().getCurrentContextOid());
        } else {
            schoolCritera.addEqualTo(SchoolCalendar.COL_DISTRICT_CONTEXT_OID, contextOid);
        }
        // Filter by selected school or eliminate unused schools.
        if (isSchoolContext()) {
            schoolCritera.addEqualTo(SchoolCalendar.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            schoolCritera.addNotEqualTo(SchoolCalendar.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_INACTIVE_INDICATOR,
                    Boolean.TRUE);
            schoolCritera.addNotEqualTo(SchoolCalendar.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_ARCHIVE_INDICATOR,
                    Boolean.TRUE);
        }
        schoolCritera.addNotEqualTo(SchoolCalendar.REL_SCHOOL + PATH_DELIMITER + m_fieldSklExclude, Boolean.TRUE);
        applyInputCriteria(schoolCritera, false, null);
        return schoolCritera;
    }

    /**
     * Load the school calendar dates map.
     *
     * @param schoolCalendarCriteria X2Criteria
     * @return void
     */
    private void getSchoolCalendarDates(X2Criteria schoolCalendarCriteria) {
        X2Criteria criteria = new X2Criteria();
        criteria.addIn(SchoolCalendarDate.COL_SCHOOL_CALENDAR_OID,
                new SubQuery(SchoolCalendar.class, X2BaseBean.COL_OID, schoolCalendarCriteria));
        criteria.addEqualTo(SchoolCalendarDate.COL_IN_SESSION_INDICATOR, BooleanAsStringConverter.TRUE);
        QueryByCriteria csdQuery = new QueryByCriteria(SchoolCalendarDate.class, criteria);
        csdQuery.addOrderByAscending(SchoolCalendarDate.COL_DATE);
        m_calendarDays =
                getBroker().getGroupedCollectionByQuery(csdQuery, SchoolCalendarDate.COL_SCHOOL_CALENDAR_OID, 200);
    }

    /**
     * Lookup field aliases and paths.
     */
    private void initializeFields() {
        m_fieldGraduation = translateAliasToJavaName(ALIAS_GRADUATION, true);
        m_fieldSklExclude = translateAliasToJavaName(ALIAS_SKL_EXCLUDE_FROM_REP, true);
    }

    /**
     * Initialize fiedl retrievers.
     */
    private void initFieldRetrievers() {
        HashMap calcs = new HashMap<String, FieldRetriever>();
        calcs.put(CALC_ID_CALID, new RetrieveCalendarId());
        calcs.put(CALC_ID_IED, new RetrieveIED());
        calcs.put(CALC_ID_ISD, new RetrieveISD());
        super.addCalcs(calcs);
    }

    /**
     * Initialize field validators.
     */
    private void initFieldValidators() {
        HashMap validators = new HashMap<String, FieldRetriever>();
        validators.put(VAL_ID_CALID, new ValidateCalId());
        validators.put(VAL_ID_CAL_DESCR, new ValidateCalendarDescription());
        validators.put(VAL_ID_GRADUATION, new ValidateGraduation());
        validators.put(ValidateAct80Group.VAL_ID, new ValidateAct80Group());
        addValidators(validators);
    }
}
