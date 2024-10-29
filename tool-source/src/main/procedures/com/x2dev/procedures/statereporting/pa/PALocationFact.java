/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2013 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.procedures.statereporting.pa;

import static com.follett.fsc.core.k12.business.ModelProperty.*;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.Race;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.ConductAction;
import com.x2dev.sis.model.beans.ConductIncident;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.*;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Location Fact Export data module.
 */
public class PALocationFact extends StateReportData {

    /**
     * Entity class .
     */
    public static class PALocationFactEntity extends StateReportEntity {
        PALocationFact m_exportData;

        /**
         * Instantiates a new PA location fact entity.
         */
        public PALocationFactEntity() {}

        /**
         * Gets the row.
         *
         * @return Counter result
         */
        public CounterResult getRow() {
            return m_exportData.m_aggregateResults.get(getCurrentRow());
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
            m_exportData = (PALocationFact) data;
            super.intitialize(data, bean);
            setRowCount(m_exportData.m_aggregateResults.size());
        }
    }

    /**
     * CounterAggResult is a list of CounterResult objects
     * each CounterResult represent counter of group of values.
     */
    class CounterAggResult extends ArrayList<CounterResult> {

        /**
         * Adds incident .
         *
         * @param category1 String
         * @param incident ConductIncident
         */
        public void addIncident(String category1, ConductIncident incident) {
            String districtCode = incident.getSchool().getOrganization1().getId();
            String schoolCode = incident.getSchool().getSchoolId();

            String genderCode = incident.getStudent().getPerson().getGenderCode();
            String ethnicityCode = getRaceCode(incident.getStudent().getPerson());
            String gradeLevel = lookupReferenceCodeByBeanPath(SisStudent.class,
                    SisStudent.COL_GRADE_LEVEL,
                    incident.getStudent().getGradeLevel(),
                    ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());

            String category4 = "CONDUCT";

            addIncident(districtCode, schoolCode, category1, "GRADE", gradeLevel, category4);
            addIncident(districtCode, schoolCode, category1, "ETHNICITY", ethnicityCode, category4);
            addIncident(districtCode, schoolCode, category1, "GENDER", genderCode, category4);
        }

        /**
         * Adds truancy .
         *
         * @param student SisStudent
         */
        public void addTruancy(SisStudent student) {
            String districtCode = student.getSchool().getOrganization1().getId();
            String schoolCode = student.getSchool().getSchoolId();

            String genderCode = student.getPerson().getGenderCode();
            String ethnicityCode = getRaceCode(student.getPerson());
            String gradeLevel = lookupReferenceCodeByBeanPath(SisStudent.class,
                    SisStudent.COL_GRADE_LEVEL,
                    student.getGradeLevel(),
                    ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());

            addIncident(districtCode, schoolCode, "TRUANCY", "GRADE", gradeLevel, "");
            addIncident(districtCode, schoolCode, "TRUANCY", "ETHNICITY", ethnicityCode, "");
            addIncident(districtCode, schoolCode, "TRUANCY", "GENDER", genderCode, "");
        }

        /**
         * Adds counts of incident .
         *
         * @param districtCode String
         * @param schoolCode String
         * @param category1 String
         * @param category2 String
         * @param category3 String
         * @param category4 String
         */
        private void addIncident(String districtCode,
                                 String schoolCode,
                                 String category1,
                                 String category2,
                                 String category3,
                                 String category4) {
            CounterResult counter =
                    new CounterResult(districtCode, schoolCode, category1, category2, category3, category4);
            int i = this.indexOf(counter);
            if (i < 0) {
                this.add(counter);
            } else {
                counter = this.get(i);
            }
            counter.increment();
        }

    }

    /**
     * CounterResult used to count groups of values similar to SQL count(*) with GROUP BY
     * It's overriden equals() method does not include 'count' field to compare.
     */
    class CounterResult {
        final String category1;
        final String category2;
        final String category3;
        final String category4;
        Integer count;
        final String districtCode;
        final String locationCode;

        /**
         * Instantiates a new counter result.
         *
         * @param districtCode String
         * @param locationCode String
         * @param category1 String
         * @param category2 String
         * @param category3 String
         * @param category4 String
         */
        CounterResult(String districtCode, String locationCode, String category1, String category2, String category3,
                String category4) {
            this.districtCode = districtCode;
            this.locationCode = locationCode;
            this.category1 = category1;
            this.category2 = category2;
            this.category3 = category3;
            this.category4 = category4;
            this.count = Integer.valueOf(0);
        }

        /**
         * @see java.lang.Object#equals(java.lang.Object)
         */
        /*
         * (non-Javadoc)
         *
         * @see java.lang.Object#equals(java.lang.Object)
         */
        @Override
        public boolean equals(Object obj) {
            boolean result = false;
            if (null == obj || !(obj instanceof CounterResult)) {
                result = super.equals(obj);
            } else {
                CounterResult other = (CounterResult) obj;
                result = equals(districtCode, other.districtCode)
                        && equals(locationCode, other.locationCode)
                        && equals(category1, other.category1)
                        && equals(category2, other.category2)
                        && equals(category3, other.category3)
                        && equals(category4, other.category4);
            }
            return result;
        }

        /**
         * Increments count .
         */
        public void increment() {
            count = Integer.valueOf(count.intValue() + 1);
        }

        /**
         * Sets count .
         *
         * @param value int
         */
        public void set(int value) {
            count = Integer.valueOf(value);
        }

        /**
         * Checks equality two strings .
         *
         * @param str1 String
         * @param str2 String
         * @return true, if successful
         */
        private boolean equals(String str1, String str2) {
            if (str1 == null && str2 == null) {
                return true;
            }
            return StringUtils.equals(str1, str2);
        }
    }

    /**
     * Field retriever for all fields not derived from organization.
     */
    class DefaultFieldRetriever implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity exportEntity, FieldDefinition field)
                throws X2BaseException {
            PALocationFactEntity entity = (PALocationFactEntity) exportEntity;

            CounterResult result = entity.getRow();
            Object value = "##empty";

            String calcParam = (String) field.getParameter();
            if (CALC_PARAM_SCHOOL_NUMBER.equals(calcParam)) {
                value = result.locationCode;
            } else if (CALC_PARAM_CATEGORY_1.equals(calcParam)) {
                value = result.category1;
            } else if (CALC_PARAM_CATEGORY_2.equals(calcParam)) {
                value = result.category2;
            } else if (CALC_PARAM_CATEGORY_3.equals(calcParam)) {
                value = result.category3;
            } else if (CALC_PARAM_CATEGORY_4.equals(calcParam)) {
                value = result.category4;
            } else if (CALC_PARAM_COUNT.equals(calcParam)) {
                value = result.count;
            }

            return value;
        }
    }

    /**
     * The Class RetrieveMeasureType.
     */
    class RetrieveMeasureType implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            return "COUNT";
        }
    }

    static final String ALIAS_TRUANT = "DOE HABITUAL TRUANT";
    static final String ALIAS_TRUANT_DATE = "DOE TRUANT DATE";

    static final String CALC_ID_CND_CALC_DEFAULT = "LOC_CALC_DEFAULT";
    static final String CALC_ID_MEASURE_TYPE = "MEASURE TYPE";

    static final String CALC_PARAM_CATEGORY_1 = "CATEGORY 1";
    static final String CALC_PARAM_CATEGORY_2 = "CATEGORY 2";
    static final String CALC_PARAM_CATEGORY_3 = "CATEGORY 3";
    static final String CALC_PARAM_CATEGORY_4 = "CATEGORY 4";
    static final String CALC_PARAM_COUNT = "COUNT";
    static final String CALC_PARAM_REPORTING_DATE = "REPORTING DATE";
    static final String CALC_PARAM_SCHOOL_NUMBER = "SCHOOL NUMBER";

    // Export input parameters
    static final String PARAM_END_DATE = "endDate";
    static final String PARAM_START_DATE = "startDate";

    CounterAggResult m_aggregateResults = new CounterAggResult();

    String m_fieldTruant;
    String m_fieldTruantDate;

    /**
     * Gets race code.
     *
     * @param person SisPerson
     * @return race code
     */
    String getRaceCode(SisPerson person) {
        String result = null;
        Collection<Race> races = person.getRaces();
        if (races == null) {
            return null;
        }
        if (person.getHispanicLatinoIndicator()) {
            result = "4";
        } else {
            if (races.size() > 1) {
                result = "6";
            } else if (races.size() == 1) {
                result = lookupReferenceCodeByBeanPath(Race.class,
                        Race.COL_RACE_CODE,
                        races.iterator().next().getRaceCode(),
                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                if (result == null) {
                    result = "##unknown##";
                }
            }
        }

        return result;
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        initializeFields();

        if (getSetupErrors().size() > 0) {
            return;
        }

        countHabitualTruancy();
        countIncidents("EXPULSION", new String[] {"S5", "S6", "S7"});
        countIncidents("SUSPENSION", new String[] {"S4"});

        // Sort by category 3.
        Collections.sort(m_aggregateResults, new Comparator<CounterResult>() {
            @Override
            public int compare(CounterResult result1, CounterResult result2) {
                return result1.category3.compareTo(result2.category3);
            }
        });

        // Sort by category 2.
        Collections.sort(m_aggregateResults, new Comparator<CounterResult>() {
            @Override
            public int compare(CounterResult result1, CounterResult result2) {
                if (result1.category2 == "GRADE" && result2.category2 == "GRADE") {
                    return 0;
                } else if (result1.category2 == "GRADE" && result2.category2 != "GRADE") {
                    return -1;
                } else if (result1.category2 != "GRADE" && result2.category2 == "GRADE") {
                    return 1;
                }

                return result1.category2.compareTo(result2.category2);
            }
        });

        // Sort by category 3.
        Collections.sort(m_aggregateResults, new Comparator<CounterResult>() {
            @Override
            public int compare(CounterResult result1, CounterResult result2) {
                if (result1.category1 == "TRUANCY" && result2.category1 == "TRUANCY") {
                    return 0;
                } else if (result1.category1 == "TRUANCY" && result2.category1 != "TRUANCY") {
                    return -1;
                } else if (result1.category1 != "TRUANCY" && result2.category1 == "TRUANCY") {
                    return 1;
                }

                return result1.category1.compareTo(result2.category1);
            }
        });

        setEntityClass(PALocationFactEntity.class);
    }

    /**
     * Used to count and populate truancy.
     */
    private void countHabitualTruancy() {
        Map<String, SisSchool> schools = getSchoolsMap();

        PlainDate endDate = (PlainDate) getParameter(PARAM_END_DATE);
        StudentHistoryHelper helper = new StudentHistoryHelper(this);
        helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
        helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, endDate);
        X2Criteria studentCriteria = helper.getStudentCriteria();
        studentCriteria.addEqualTo(m_fieldTruant, BooleanAsStringConverter.TRUE);

        studentCriteria.addGreaterOrEqualThan(m_fieldTruantDate, getOrganization().getCurrentContext().getStartDate());
        studentCriteria.addLessOrEqualThan(m_fieldTruantDate, getOrganization().getCurrentContext().getEndDate());

        QueryByCriteria studentQuery = helper.getStudentQuery(false);
        QueryIterator studentIterator = getBroker().getIteratorByQuery(studentQuery);
        try {
            while (studentIterator.hasNext()) {
                SisStudent student = (SisStudent) studentIterator.next();
                m_aggregateResults.addTruancy(student);
                if (schools.containsKey(student.getSchoolOid())) {
                    schools.remove(student.getSchoolOid());
                }
            }
        } finally {
            studentIterator.close();
        }
    }

    /**
     * Used to count and populate Expulsions and Suspensions.
     *
     * @param type - EXPULSION or SUSPENSION
     * @param actionCodes included action codes
     */
    private void countIncidents(String type, String[] actionCodes) {
        Map<String, SisSchool> schools = getSchoolsMap();

        // Expulsions and Out of School Suspensions
        X2Criteria incidentCriteria = getIncidentCriteria();
        Collection<String> codes = getStateCodes(ConductAction.class, ConductAction.COL_ACTION_CODE, actionCodes);
        if (codes.size() > 0) {
            incidentCriteria.addIn(ConductIncident.REL_CONDUCT_ACTIONS + PATH_DELIMITER + ConductAction.COL_ACTION_CODE,
                    codes);
        } else {
            incidentCriteria.addEqualTo(X2BaseBean.COL_OID, "_*_dummyoid_*_");
        }

        QueryByCriteria incidentQuery = new QueryByCriteria(ConductIncident.class, incidentCriteria, true);
        QueryIterator incidentIterator = getBroker().getIteratorByQuery(incidentQuery);
        try {
            while (incidentIterator.hasNext()) {
                ConductIncident incident = (ConductIncident) incidentIterator.next();
                m_aggregateResults.addIncident(type, incident);
                if (schools.containsKey(incident.getSchoolOid())) {
                    schools.remove(incident.getSchoolOid());
                }
            }
        } finally {
            incidentIterator.close();
        }
    }

    /**
     * Create the criteria for selection ConductIncident.
     *
     * @return ConductIncident criteria
     */
    private X2Criteria getIncidentCriteria() {
        X2Criteria incidentCriteria = new X2Criteria();

        incidentCriteria.addEqualTo(ConductIncident.REL_SCHOOL + ModelProperty.PATH_DELIMITER +
                SisSchool.COL_ORGANIZATION1_OID, super.getOrganization().getOid());

        if (isSchoolContext()) {
            incidentCriteria.addEqualTo(ConductIncident.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            incidentCriteria.addNotEqualTo(ConductIncident.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_INACTIVE_INDICATOR,
                    Boolean.TRUE);
            incidentCriteria.addNotEqualTo(ConductIncident.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_ARCHIVE_INDICATOR,
                    Boolean.TRUE);
        }

        Date startDate = (Date) getParameter(PARAM_START_DATE);
        if (startDate != null) {
            incidentCriteria.addGreaterOrEqualThan(ConductIncident.COL_INCIDENT_DATE, startDate);
        }

        Date endDate = (Date) getParameter(PARAM_END_DATE);
        if (endDate != null) {
            incidentCriteria.addLessOrEqualThan(ConductIncident.COL_INCIDENT_DATE, endDate);
        }

        return incidentCriteria;
    }

    /**
     * Provides a map of schools.
     *
     * @return Map<sklOid, SisSchool> for the schools included in the export.
     */
    private Map<String, SisSchool> getSchoolsMap() {
        X2Criteria sklCriteria = new X2Criteria();
        if (isSchoolContext()) {
            sklCriteria.addEqualTo(X2BaseBean.COL_OID, getSchool().getOid());
        } else {
            sklCriteria.addNotEqualTo(SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
            sklCriteria.addNotEqualTo(SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
        }
        return getBroker().getMapByQuery(new QueryByCriteria(SisSchool.class, sklCriteria), X2BaseBean.COL_OID, 10);
    }

    /**
     * Provides a list of reference codes mapped to a particular set of state codes.
     *
     * @param cls Bean Class
     * @param path Field
     * @param stateCodes State Codes
     * @return Collection
     */
    private Collection<String> getStateCodes(Class cls, String path, String[] stateCodes) {
        Set<String> codes = new HashSet<String>();
        DataDictionaryField field = getDataDictionaryField(cls, path);
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, field.getReferenceTableOid());
        criteria.addIn(ReferenceCode.COL_STATE_CODE, Arrays.asList(stateCodes));

        String[] columns = new String[] {ReferenceCode.COL_CODE};
        ReportQueryByCriteria query = new ReportQueryByCriteria(ReferenceCode.class, columns, criteria);

        ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                Object[] record = (Object[]) iterator.next();
                String code = (String) record[0];
                codes.add(code);
            }
        } finally {
            iterator.close();
        }
        return codes;
    }

    /**
     * Lookup field aliases and paths.
     */
    private void initializeFields() {
        m_fieldTruant = translateAliasToJavaName(ALIAS_TRUANT, true);
        m_fieldTruantDate = translateAliasToJavaName(ALIAS_TRUANT_DATE, true);
        // Add retrievers
        HashMap<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
        calcs.put(CALC_ID_CND_CALC_DEFAULT, new DefaultFieldRetriever());
        calcs.put(CALC_ID_MEASURE_TYPE, new RetrieveMeasureType());
        addCalcs(calcs);
    }
}
