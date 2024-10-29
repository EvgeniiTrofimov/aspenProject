/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2014 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.procedures.statereporting.ca;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Address;
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
import com.x2dev.sis.model.beans.HealthImmunizationDefinition;
import com.x2dev.sis.model.beans.HealthImmunizationDose;
import com.x2dev.sis.model.beans.HealthImmunizationSeries;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * This class implements a data extract that is used by CASchoolImmunizationRecord to generate the
 * California School Immunization Record reports.
 *
 * @author X2 Development Corporation
 */
public class CASchoolImmunization extends StateReportData {

    /**
     * Entity class for CASchoolImmunization data export.
     */

    public static class CASchoolImmunizationEntity extends StateReportEntity {
        private CASchoolImmunization m_reportData;
        private SisStudent m_student;
        private HealthImmunizationDose m_tdapDose;

        /**
         * Intitialize.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity
         *      #intitialize(StateReportData, X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            m_reportData = (CASchoolImmunization) data;
            m_student = (SisStudent) bean;
        }

        /**
         * Returns dates of given doses which included in the category.
         *
         * @param category String
         * @return List
         */
        protected List<PlainDate> getDosesDates(String category) {
            List<PlainDate> dosesDates = new ArrayList<PlainDate>();

            for (HealthImmunizationDefinition defn : m_reportData.getDefinitionsByCategory(category)) {
                HealthImmunizationSeries series = m_reportData.getSeries(defn, m_student);
                if (series != null) {
                    Collection<HealthImmunizationDose> doses = series.getImmunizationDoses();
                    for (HealthImmunizationDose dose : doses) {
                        if (dose.getDate() != null) {
                            if (CATEGORY_DTP.equals(category) && SERIES_TDAP.equals(defn.getSeriesName())) {
                                m_tdapDose = dose;
                                continue;
                            }
                            dosesDates.add(dose.getDate());
                        }
                    }
                }
            }
            Collections.sort(dosesDates);
            return dosesDates;
        }

        /**
         * Returns student.
         *
         * @return Sis student
         */
        protected SisStudent getStudent() {
            return m_student;
        }

        /**
         * Returns Tdap (pertussis booster) date.
         *
         * @return Health immunization dose
         */
        protected HealthImmunizationDose getTdap() {
            return m_tdapDose;
        }
    }

    /**
     * Returns address line 1 + line 2 for student.
     */
    protected class RetrieveAddress implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            SisStudent student = (SisStudent) entity.getBean();
            return m_studentAddresses.get(student.getOid());
        }
    }

    /**
     * Retrieves the birthplace for the student.
     */
    protected class RetrieveBirthPlace implements FieldRetriever {
        private static final String ALIAS_BIRTH_COUNTRY = "DOE BIRTH COUNTRY";
        private static final String ALIAS_BIRTH_STATE = "DOE BIRTH STATE";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            SisStudent student = (SisStudent) entity.getBean();
            SisPerson person = student.getPerson();
            String country = (String) person.getFieldValueByAlias(ALIAS_BIRTH_COUNTRY);
            String state = (String) person.getFieldValueByAlias(ALIAS_BIRTH_STATE);
            return StringUtils.isEmpty(state) ? StringUtils.unNullify(country)
                    : state + "/" + StringUtils.unNullify(country);
        }
    }

    /**
     * 1)Takes collection of doses for vaccine from parameter;
     * 2)Returns date of dose or null.
     */
    protected class RetrieveDosesDates implements FieldRetriever {
        private final static String CALC_PARAM_PREFIX_DTP = "dtp-date-";
        private final static String CALC_PARAM_PREFIX_HEPA = "hepatitisa-date-";
        private final static String CALC_PARAM_PREFIX_HEPB = "hepatitisb-date-";
        private final static String CALC_PARAM_PREFIX_HIB = "hib-date-";
        private final static String CALC_PARAM_PREFIX_MMR = "mmr-date-";
        private final static String CALC_PARAM_PREFIX_POLIO = "polio-date-";
        private final static String CALC_PARAM_PREFIX_VAR = "varicella-date-";
        private final static String CALC_PARAM_TDAP = "tdap";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            Object value = null;
            CASchoolImmunization siData = (CASchoolImmunization) data;
            String parameter = (String) field.getParameter();

            CASchoolImmunizationEntity schoolImmunizationEntity = (CASchoolImmunizationEntity) entity;

            Collection<PlainDate> dosesDates = new ArrayList<PlainDate>();

            if (parameter.startsWith(CALC_PARAM_PREFIX_POLIO)) {
                dosesDates = schoolImmunizationEntity.getDosesDates(CATEGORY_POLIO);
                value = getDoseDate(dosesDates, CALC_PARAM_PREFIX_POLIO, parameter);
            } else if (parameter.startsWith(CALC_PARAM_PREFIX_DTP)) {
                dosesDates = schoolImmunizationEntity.getDosesDates(CATEGORY_DTP);
                value = getDoseDate(dosesDates, CALC_PARAM_PREFIX_DTP, parameter);
            } else if (parameter.startsWith(CALC_PARAM_PREFIX_MMR)) {
                dosesDates = schoolImmunizationEntity.getDosesDates(CATEGORY_MMR);
                value = getDoseDate(dosesDates, CALC_PARAM_PREFIX_MMR, parameter);
            } else if (parameter.startsWith(CALC_PARAM_PREFIX_HIB)) {
                dosesDates = schoolImmunizationEntity.getDosesDates(CATEGORY_HIB);
                value = getDoseDate(dosesDates, CALC_PARAM_PREFIX_HIB, parameter);
            } else if (parameter.startsWith(CALC_PARAM_PREFIX_HEPB)) {
                dosesDates = schoolImmunizationEntity.getDosesDates(CATEGORY_HEPB);
                value = getDoseDate(dosesDates, CALC_PARAM_PREFIX_HEPB, parameter);
            } else if (parameter.startsWith(CALC_PARAM_PREFIX_VAR)) {
                dosesDates = schoolImmunizationEntity.getDosesDates(CATEGORY_VAR);
                value = getDoseDate(dosesDates, CALC_PARAM_PREFIX_VAR, parameter);
            } else if (parameter.startsWith(CALC_PARAM_PREFIX_HEPA)) {
                dosesDates = schoolImmunizationEntity.getDosesDates(CATEGORY_HEPA);
                value = getDoseDate(dosesDates, CALC_PARAM_PREFIX_HEPA, parameter);
            } else if (parameter.equals(CALC_PARAM_TDAP)) {
                HealthImmunizationDose tdapDose = null;
                tdapDose = schoolImmunizationEntity.getTdap();
                if (tdapDose != null) {
                    value = siData.m_dateFormat.format(tdapDose.getDate());
                }
            }
            return value;
        }

        // Return date of dose if prefix + account of dose matches with parameter.
        /**
         * Gets the dose date.
         *
         * @param dosesDates Collection<PlainDate>
         * @param prefix String
         * @param calc_param String
         * @return Plain date
         */
        // If there is no matches then return null.
        private PlainDate getDoseDate(Collection<PlainDate> dosesDates, String prefix, String calc_param) {
            int doseAccount = 1;

            for (PlainDate date : dosesDates) {
                if ((prefix + doseAccount).equals(calc_param)) {
                    return date;
                }
                doseAccount++;
            }
            return null;
        }
    }

    /**
     * Returns address line 1 + line 2 for student.
     */
    protected class RetrieveRace implements FieldRetriever {
        private static final String RACE_BLACK = "Black";
        private static final String RACE_HISPANIC = "Hispanic";
        private static final String RACE_UNKNOWN = "Unknown";
        private static final String RACE_WHITE = "White";

        private static final String STATE_CODE_WHITE = "700";
        private static final String STATE_CODE_BLACK = "600";

        private Set<String> blackCodes = new HashSet();
        private Set<String> whiteCodes = new HashSet();
        private Map<String, String> m_mapDescription = new HashMap();

        /**
         * Standard constructor.
         */
        public RetrieveRace() {
            DataDictionaryField field =
                    getDataDictionary().findDataDictionaryField(Race.class.getName(), Race.COL_RACE_CODE);
            if (field != null && field.getReferenceTableOid() != null) {
                Map<String, ReferenceCode> codes = getReferenceCodes(field.getReferenceTableOid());

                for (ReferenceCode code : codes.values()) {
                    if (STATE_CODE_WHITE.equals(code.getStateCode())) {
                        whiteCodes.add(code.getCode());
                    } else if (STATE_CODE_BLACK.equals(code.getStateCode())) {
                        blackCodes.add(code.getCode());
                    } else {
                        m_mapDescription.put(code.getCode(), code.getDescription());
                    }
                }
            }
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String value = RACE_UNKNOWN;
            SisStudent student = (SisStudent) entity.getBean();
            if (student.getPerson().getHispanicLatinoIndicator()) {
                value = RACE_HISPANIC;
            } else {
                Collection<Race> races = m_races.get(student.getPersonOid());
                if (races != null && !races.isEmpty()) {
                    for (Race race : races) {
                        if (whiteCodes.contains(race.getRaceCode())) {
                            value = RACE_WHITE;
                        }
                    }
                    if (RACE_UNKNOWN.equals(value)) {
                        for (Race race : races) {
                            if (blackCodes.contains(race.getRaceCode())) {
                                value = RACE_BLACK;
                            }
                        }
                    }
                    if (RACE_UNKNOWN.equals(value)) {
                        for (Race race : races) {
                            String description = m_mapDescription.get(race.getRaceCode());
                            if (!StringUtils.isEmpty(description)) {
                                value = description;
                                break;
                            }
                        }

                    }
                }
            }
            return value;
        }
    }

    /**
     * Returns Tdap indicators values.
     */
    protected class RetrieveTdapIndicators implements FieldRetriever {
        private final static String CALC_PARAM_TDAP_7 = "tdap7years";
        private final static String CALC_PARAM_REASON = "tdapReason";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object value = null;
            String parameter = (String) field.getParameter();

            CASchoolImmunizationEntity schoolImmunizationEntity = (CASchoolImmunizationEntity) entity;

            HealthImmunizationDose tdapDose = null;
            tdapDose = schoolImmunizationEntity.getTdap();

            if (parameter.equals(CALC_PARAM_TDAP_7)) {
                value = BooleanAsStringConverter.FALSE;

                if (tdapDose != null) {
                    int diff = getDiffYears(schoolImmunizationEntity.getStudent().getPerson().getDob(),
                            tdapDose.getDate());
                    if (diff >= 7) {
                        value = BooleanAsStringConverter.TRUE;
                    }
                }
            } else if (parameter.equals(CALC_PARAM_REASON)) {
                if (tdapDose != null) {
                    value = tdapDose.getImmunizationSeries().getFieldA002();
                }
            }

            return value;
        }
    }



    private static final String CATEGORY_DTP = "Diphtheria Tetanus Pertussis";
    private static final String CATEGORY_HEPA = "Hepatitis A";
    private static final String CATEGORY_HEPB = "Hepatitis B";
    private static final String CATEGORY_HIB = "Haemophilus influenzae type b";
    private static final String CATEGORY_MMR = "Measles Mumps Rubella";
    private static final String CATEGORY_POLIO = "Polio";
    private static final String CATEGORY_VAR = "Varicella";
    private static final String PARAM_CURRENT_STUDENTS = "Current students";
    private static final String RETRIEVER_SIR_ADDR_CALC = "SIR-ADDR-CALC";
    private static final String RETRIEVER_SIR_BIRTHPLACE = "SIR-BIRTHPLACE";
    private static final String RETRIEVER_SIR_CALC = "SIR-CALC";
    private static final String RETRIEVER_SIR_RACE = "SIR-RACE";
    private static final String RETRIEVER_TDAP_IND = "SIR-TDAP-IND";
    private static final String SERIES_TDAP = "Tdap";

    protected Map<String, Collection<HealthImmunizationDefinition>> m_categories;
    protected SimpleDateFormat m_dateFormat;
    protected Collection<HealthImmunizationDefinition> m_definitions;
    protected Map<String, Collection<Race>> m_races;
    protected Map<String, String> m_studentAddresses;

    private Map<String, List<HealthImmunizationDose>> m_seriesImmunizationDoseMap;
    private X2Criteria m_studentCriteria = null;
    private Map<String, Map<String, HealthImmunizationSeries>> m_studentImmunizationSeriesMap;

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
        m_dateFormat = new SimpleDateFormat("MM/dd/yyyy");

        List<String> students = (List<String>) getParameter(PARAM_CURRENT_STUDENTS);
        m_studentCriteria = new X2Criteria();

        if (students != null) {
            m_studentCriteria.addIn(X2BaseBean.COL_OID, students);
        } else {
            applyInputCriteria(m_studentCriteria, true, null);
        }

        if (getSetupErrors().size() == 0) {
            initImmunizationCollections(m_studentCriteria);

            // Set the query to be used for student selection.
            setQuery(new QueryByCriteria(SisStudent.class, m_studentCriteria));

            setEntityClass(CASchoolImmunizationEntity.class);
            loadAddresses();
            loadRaces();
            // Build a map of calculations/retrievers
            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put(RETRIEVER_SIR_CALC, new RetrieveDosesDates());
            calcs.put(RETRIEVER_SIR_RACE, new RetrieveRace());
            calcs.put(RETRIEVER_SIR_ADDR_CALC, new RetrieveAddress());
            calcs.put(RETRIEVER_SIR_BIRTHPLACE, new RetrieveBirthPlace());
            calcs.put(RETRIEVER_TDAP_IND, new RetrieveTdapIndicators());

            super.addCalcs(calcs);
        }
    }

    /**
     * Returns definitions by category.
     *
     * @param category String
     * @return Collection
     */
    protected Collection<HealthImmunizationDefinition> getDefinitionsByCategory(String category) {
        Collection<HealthImmunizationDefinition> defs = m_categories.get(category);
        return defs;
    }

    /**
     * Returns number of years between two dates.
     * argument 'date2' should be greater than 'date1' to avoid negative result
     *
     * @param date1 PlainDate
     * @param date2 PlainDate
     * @return int
     */
    protected int getDiffYears(PlainDate date1, PlainDate date2) {
        Calendar cal1 = Calendar.getInstance();
        cal1.setTime(date1);

        Calendar cal2 = Calendar.getInstance();
        cal2.setTime(date2);

        int diff = cal2.get(Calendar.YEAR) - cal1.get(Calendar.YEAR);
        if (cal2.get(Calendar.MONTH) > cal1.get(Calendar.MONTH) ||
                (cal2.get(Calendar.MONTH) == cal1.get(Calendar.MONTH) &&
                        cal2.get(Calendar.DATE) > cal1.get(Calendar.DATE))) {
            diff--;
        }
        return diff;
    }

    /**
     * Returns series by definition and student.
     *
     * @param defn HealthImmunizationDefinition
     * @param student SisStudent
     * @return Health immunization series
     */
    protected HealthImmunizationSeries getSeries(HealthImmunizationDefinition defn, SisStudent student) {
        Map<String, HealthImmunizationSeries> seriesMap = m_studentImmunizationSeriesMap.get(defn.getOid());
        if (seriesMap == null) {
            seriesMap = new HashMap<String, HealthImmunizationSeries>(0);
        }
        return seriesMap.get(student.getOid());
    }

    /**
     * Returns immunization dose by series.
     *
     * @param series HealthImmunizationSeries
     * @return List
     */
    protected List<HealthImmunizationDose> getSeriesImmunizationDose(HealthImmunizationSeries series) {
        return m_seriesImmunizationDoseMap.get(series.getOid());
    }

    /**
     * Returns student immunization series by student.
     *
     * @param student SisStudent
     * @return Map
     */
    protected Map<String, HealthImmunizationSeries> getStudentImmunizationSeries(SisStudent student) {
        return m_studentImmunizationSeriesMap.get(student.getOid());
    }

    /**
     * Generates immunization collections for definition, categories, series and doses
     * used to determine given doses.
     *
     * @param studentCriteria X2Criteria
     */
    private void initImmunizationCollections(X2Criteria studentCriteria) {
        SubQuery studentSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria);

        // definitions
        QueryByCriteria definitionQuery = new QueryByCriteria(HealthImmunizationDefinition.class, new Criteria());
        m_definitions = getBroker().getCollectionByQuery(definitionQuery);

        // categories
        QueryByCriteria categoryQuery = new QueryByCriteria(HealthImmunizationDefinition.class, new Criteria());
        m_categories =
                getBroker().getGroupedCollectionByQuery(categoryQuery, HealthImmunizationDefinition.COL_CATEGORIES, 20);

        // series
        X2Criteria seriesMapCriteria = new X2Criteria();
        seriesMapCriteria.addIn(HealthImmunizationSeries.COL_STUDENT_OID, studentSubQuery);
        QueryByCriteria seriesQuery = new QueryByCriteria(HealthImmunizationSeries.class, seriesMapCriteria);
        seriesQuery.addOrderBy(HealthImmunizationSeries.COL_IMMUNIZATION_DEFINITION_OID, true);
        seriesQuery.addOrderBy(HealthImmunizationSeries.COL_STUDENT_OID, true);
        m_studentImmunizationSeriesMap = getBroker().getNestedMapByQuery(seriesQuery,
                HealthImmunizationSeries.COL_IMMUNIZATION_DEFINITION_OID,
                HealthImmunizationSeries.COL_STUDENT_OID,
                16,
                128);

        // doses
        SubQuery seriesSubQuery = new SubQuery(HealthImmunizationSeries.class, X2BaseBean.COL_OID, seriesMapCriteria);
        X2Criteria doseMapCriteria = new X2Criteria();
        doseMapCriteria.addIn(HealthImmunizationDose.COL_IMMUNIZATION_SERIES_OID, seriesSubQuery);
        QueryByCriteria doseQuery = new QueryByCriteria(HealthImmunizationDose.class, seriesMapCriteria);
        doseQuery.addOrderBy(HealthImmunizationDose.COL_IMMUNIZATION_SERIES_OID, true);
        doseQuery.addOrderBy(HealthImmunizationDose.COL_DATE, true);
        m_seriesImmunizationDoseMap = getBroker().getGroupedCollectionByQuery(doseQuery,
                HealthImmunizationDose.COL_IMMUNIZATION_SERIES_OID, 500);

    }

    /**
     * Loads addresses line 1 + line 2 for students.
     */
    private void loadAddresses() {
        m_studentAddresses = new HashMap<String, String>();

        final String SQL_ADDRESS_LINE_1 =
                SisStudent.REL_PERSON + ModelProperty.PATH_DELIMITER + SisPerson.REL_PHYSICAL_ADDRESS +
                        ModelProperty.PATH_DELIMITER + Address.COL_ADDRESS_LINE01;
        final String SQL_ADDRESS_LINE_2 =
                SisStudent.REL_PERSON + ModelProperty.PATH_DELIMITER + SisPerson.REL_PHYSICAL_ADDRESS +
                        ModelProperty.PATH_DELIMITER + Address.COL_ADDRESS_LINE02;

        ReportQueryByCriteria query = new ReportQueryByCriteria(SisStudent.class, new String[] {X2BaseBean.COL_OID,
                SQL_ADDRESS_LINE_1,
                SQL_ADDRESS_LINE_2},
                m_studentCriteria);

        ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);

        final int STUDENT_OID = 0;
        final int ADDRESS_LINE_1 = 1;
        final int ADDRESS_LINE_2 = 2;

        while (iterator.hasNext()) {
            Object[] rows = (Object[]) iterator.next();
            if (rows[ADDRESS_LINE_2] != null && !((String) rows[ADDRESS_LINE_2]).isEmpty()) {
                m_studentAddresses.put((String) rows[STUDENT_OID], rows[ADDRESS_LINE_1] + ", " + rows[ADDRESS_LINE_2]);
            } else {
                m_studentAddresses.put((String) rows[STUDENT_OID], (String) rows[ADDRESS_LINE_1]);
            }
        }
    }

    /**
     * Loads races.
     */
    private void loadRaces() {
        SubQuery raceSubquery = new SubQuery(SisStudent.class, SisStudent.COL_PERSON_OID, m_studentCriteria);
        X2Criteria raceCriteria = new X2Criteria();
        raceCriteria.addIn(Race.COL_PERSON_OID, raceSubquery);
        BeanQuery raceQuery = new BeanQuery(Race.class, raceCriteria);
        raceQuery.addOrderBy(Race.COL_PERSON_OID, true);
        m_races = getBroker().getGroupedCollectionByQuery(raceQuery, Race.COL_PERSON_OID, 1000);

    }
}
