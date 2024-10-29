/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2014 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.reports.statereporting.ca;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Race;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.ContextList;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.HealthImmunizationDefinition;
import com.x2dev.sis.model.beans.HealthImmunizationDose;
import com.x2dev.sis.model.beans.HealthImmunizationSeries;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares the data for the "California school immunization record" report.
 *
 * @author X2 Development Corporation
 */
public class CASchoolImmunizationRecordData extends ReportJavaSourceNet {

    private static final String NO_PERFMON4J_INSTRUMENTATION = "";

    /**
     * Constants
     */
    private static final String ALIAS_HIS_EXEMPT_REASON = "all-immExemptReason";
    private static final String ALIAS_HIS_FOLLOW_UP_REASON = "all-immFollowUpReason";
    private static final String ALIAS_HIS_FOLLOW_UP_DATE = "all-immFollowUpDate";
    private static final String ALIAS_SKL_HOME_HOSPITAL = "home-hospital";
    private static final String ALIAS_SKL_INDEP_STUD = "independent-studies";
    private static final String ALIAS_STD_IEP_STATUS = "cust-STD-SpEdIEP-status";
    private static final String CATEGORY_DTP = "Diphtheria Tetanus Pertussis";
    private static final String CATEGORY_HEPA = "Hepatitis A";
    private static final String CATEGORY_HEPB = "Hepatitis B";
    private static final String CATEGORY_HIB = "Haemophilus influenzae type b";
    private static final String CATEGORY_MMR = "Measles Mumps Rubella";
    private static final String CATEGORY_POLIO = "Polio";
    private static final String CATEGORY_VAR = "Varicella";
    private static final String CODE_EXEMPTION_P = "Medical P";
    private static final SimpleDateFormat DATE_FORMAT = new SimpleDateFormat("MM/dd/yyyy");
    private static final String POSTFIX_PME = "PME";
    private static final String REPORT_FIELD_ALL_7 = "all7";
    private static final String REPORT_FIELD_ALL_PK = "allPK";
    private static final String REPORT_FIELD_ALL_TK = "allTK";
    private static final String REPORT_FIELD_DOB = "dob";
    private static final String REPORT_FIELD_DTP = "dtp";
    private static final String REPORT_FIELD_DTP_AGE = "dtpAge";
    private static final String REPORT_FIELD_FOLLOW_DATE_7 = "followDate7";
    private static final String REPORT_FIELD_FOLLOW_DATE_PK = "followDatePK";
    private static final String REPORT_FIELD_FOLLOW_DATE_TK = "followDateTK";
    private static final String REPORT_FIELD_FOLLOW_UP_7 = "followUp7";
    private static final String REPORT_FIELD_FOLLOW_UP_PK = "followPK";
    private static final String REPORT_FIELD_FOLLOW_UP_COND_TK = "followConditionalTK";
    private static final String REPORT_FIELD_FOLLOW_UP_NEED_TK = "followNeedTK";
    private static final String REPORT_FIELD_FOLLOW_UP_TEMP_TK = "followTempTK";
    private static final String REPORT_FIELD_HEPB = "hepb";
    private static final String REPORT_FIELD_HIB = "hib";
    private static final String REPORT_FIELD_HISPANIC_INDICATOR = "hispanic";
    private static final String REPORT_FIELD_HOME_7 = "home7";
    private static final String REPORT_FIELD_HOME_TK = "homeTK";
    private static final String REPORT_FIELD_IEP_7 = "iep7";
    private static final String REPORT_FIELD_IEP_PK = "iepPK";
    private static final String REPORT_FIELD_IEP_TK = "iepTK";
    private static final String REPORT_FIELD_INDEPEND_7 = "independ7";
    private static final String REPORT_FIELD_INDEPEND_TK = "independTK";
    private static final String REPORT_FIELD_MET_7 = "met7";
    private static final String REPORT_FIELD_MET_PK = "metPK";
    private static final String REPORT_FIELD_MET_TK = "metTK";
    private static final String REPORT_FIELD_MMR = "mmr";
    private static final String REPORT_FIELD_MMR_AGE = "mmrAge";
    private static final String REPORT_FIELD_PARENT_FIRST_NAME = "parentFirstName";
    private static final String REPORT_FIELD_PARENT_LAST_NAME = "parentLastName";
    private static final String REPORT_FIELD_POLIO = "polio";
    private static final String REPORT_FIELD_POLIO_AGE = "polioAge";
    private static final String REPORT_FIELD_RACE = "race";
    private static final String REPORT_FIELD_SASID = "sasid";
    private static final String REPORT_FIELD_LASID = "lasid";
    private static final String REPORT_FIELD_SEX = "sex";
    private static final String REPORT_FIELD_STUDENT_ID = "studentID";
    private static final String REPORT_FIELD_STUDENT_FIRST_NAME = "studentFirstName";
    private static final String REPORT_FIELD_STUDENT_GRADE = "grade";
    private static final String REPORT_FIELD_STUDENT_LAST_NAME = "studentLastName";
    private static final String REPORT_FIELD_STUDENT_MIDDLE_NAME = "studentMiddleName";
    private static final String REPORT_FIELD_TDAP = "tdap";
    private static final String REPORT_FIELD_TDAP_MEET = "tdapMeet";
    private static final String REPORT_FIELD_TDAP_REASON = "tdapReason";
    private static final String REPORT_FIELD_TDAP_YEARS = "tdapYears";
    private static final String REPORT_FIELD_VAR = "var";
    private static final String REPORT_PARAM_REPORT_DATE = "reportDate";
    private static final String RACE_BLACK = "Black";
    private static final String RACE_UNKNOWN = "Unknown";
    private static final String RACE_WHITE = "White";
    private static final String RTB_RACE_CODES_NAME = "Race Codes";
    private static final String SERIES_TDAP = "Tdap";
    private static final String STATE_CODE_WHITE = "700";
    private static final String STATE_CODE_BLACK = "600";
    private static final String QUERY_BY_PARAM = "queryBy1";
    private static final String QUERY_STRING_PARAM = "queryString1";

    /**
     * Class members.
     */
    private Set<String> m_blackCodes = new HashSet();
    protected Map<String, Collection<HealthImmunizationDefinition>> m_categories;
    private SimpleDateFormat m_dateFormat = new SimpleDateFormat("MM/dd/yyyy");
    protected Collection<HealthImmunizationDefinition> m_definitions;
    private Map<String, String> m_mapDescription = new HashMap();
    protected Map<String, Collection<Race>> m_races;
    private HealthImmunizationSeries m_seriesDtp;
    private Map<String, List<HealthImmunizationDose>> m_seriesImmunizationDoseMap;
    private HealthImmunizationSeries m_seriesHepb;
    private HealthImmunizationSeries m_seriesHib;
    private HealthImmunizationSeries m_seriesMmr;
    private HealthImmunizationSeries m_seriesPolio;
    private HealthImmunizationSeries m_seriesTdap7;
    private HealthImmunizationSeries m_seriesVar;
    private Map<String, SisStudent> m_stdByOids;
    private Map<String, Map<String, HealthImmunizationSeries>> m_studentImmunizationSeriesMap;
    private HealthImmunizationDose m_tdapDose;
    private Set<String> m_whiteCodes = new HashSet();
    private String m_fieldHisExemptReason;

    /**
     * Provides the report data source. The input data source is delivered
     * by the CAImmunization class and export format
     *
     * @return Object
     * @throws Exception exception
     */
    @Override
    protected Object gatherData() throws Exception {
        initImmunizationCollections();
        loadRaces();
        ReportDataGrid dataGrid = new ReportDataGrid();
        for (String stdOid : m_stdByOids.keySet()) {
            m_seriesHib = null;
            m_seriesTdap7 = null;
            m_seriesDtp = null;
            m_seriesHepb = null;
            m_seriesPolio = null;
            m_seriesVar = null;
            m_seriesMmr = null;
            m_tdapDose = null;
            SisStudent std = m_stdByOids.get(stdOid);
            dataGrid.append();
            dataGrid.set(REPORT_FIELD_STUDENT_FIRST_NAME, std.getPerson().getFirstName());
            dataGrid.set(REPORT_FIELD_STUDENT_GRADE, std.getGradeLevel());
            dataGrid.set(REPORT_FIELD_STUDENT_LAST_NAME, std.getPerson().getLastName());
            dataGrid.set(REPORT_FIELD_STUDENT_MIDDLE_NAME, std.getPerson().getMiddleName());
            dataGrid.set(REPORT_FIELD_RACE, getRace(std));
            dataGrid.set(REPORT_FIELD_PARENT_FIRST_NAME,
                    std.getContact1() != null ? std.getContact1().getPerson().getFirstName() : "");
            dataGrid.set(REPORT_FIELD_PARENT_LAST_NAME,
                    std.getContact1() != null ? std.getContact1().getPerson().getLastName() : "");
            dataGrid.set(REPORT_FIELD_SEX, std.getPerson().getGenderCode());
            dataGrid.set(REPORT_FIELD_DOB, m_dateFormat.format(std.getPerson().getDob()));
            if (std.getStateId() == null) {
                dataGrid.set(REPORT_FIELD_SASID, "");
            } else {
                dataGrid.set(REPORT_FIELD_SASID, std.getStateId());
            }
            dataGrid.set(REPORT_FIELD_LASID, std.getLocalId());
            dataGrid.set(REPORT_FIELD_HISPANIC_INDICATOR,
                    Boolean.valueOf(std.getPerson().getHispanicLatinoIndicator()));
            List<PlainDate> polioDates = getDosesDates(dataGrid, CATEGORY_POLIO, REPORT_FIELD_POLIO, std.getOid());
            populateGridWithDose(dataGrid, std, polioDates, Integer.valueOf(4), Integer.valueOf(3), null,
                    REPORT_FIELD_POLIO, REPORT_FIELD_POLIO_AGE);
            List<PlainDate> dtpDates = getDosesDates(dataGrid, CATEGORY_DTP, REPORT_FIELD_DTP, std.getOid());
            if (m_tdapDose != null) {
                if (dtpDates == null) {
                    dtpDates = new ArrayList<>();
                }
                dtpDates.add(m_tdapDose.getDate());
                Collections.sort(dtpDates);
            }
            populateGridWithDose(dataGrid, std, dtpDates, Integer.valueOf(5), Integer.valueOf(3),
                    Integer.valueOf(4), REPORT_FIELD_DTP, REPORT_FIELD_DTP_AGE);
            List<PlainDate> mmrDates = getDosesDates(dataGrid, CATEGORY_MMR, REPORT_FIELD_MMR,
                    std.getOid());
            populateGridWithDose(dataGrid, std, mmrDates, Integer.valueOf(2), Integer.valueOf(1),
                    null, REPORT_FIELD_MMR, REPORT_FIELD_MMR_AGE);
            List<PlainDate> hibDates = getDosesDates(dataGrid, CATEGORY_HIB, REPORT_FIELD_HIB, std.getOid());
            populateGridWithDose(dataGrid, std, hibDates, Integer.valueOf(4), null, null, REPORT_FIELD_HIB, null);
            List<PlainDate> hepbDates = getDosesDates(dataGrid, CATEGORY_HEPB, REPORT_FIELD_HEPB, std.getOid());
            populateGridWithDose(dataGrid, std, hepbDates, Integer.valueOf(3), null, null, REPORT_FIELD_HEPB, null);
            List<PlainDate> varDates = getDosesDates(dataGrid, CATEGORY_VAR, REPORT_FIELD_VAR, std.getOid());
            populateGridWithDose(dataGrid, std, varDates, Integer.valueOf(2), null, null, REPORT_FIELD_VAR, null);
            for (HealthImmunizationDefinition defn : getDefinitionsByCategory(CATEGORY_VAR)) {
                HealthImmunizationSeries series = getSeries(defn, stdOid);
                if (series != null && series.getHistoryOfDiseaseIndicator()) {
                    dataGrid.set(REPORT_FIELD_VAR + "hx", series.getComment());
                    break;
                }
            }
            if ("PK".equals(std.getGradeLevel())) {
                populateSectionForPK(dataGrid, hibDates, std);
            } else if ("07".equals(std.getGradeLevel())) {
                populateSectionForTDAP(dataGrid, dtpDates, std);
            } else {
                List<PlainDate> datesForMetTK = new ArrayList<PlainDate>();
                if (polioDates != null && polioDates.size() > 0) {
                    datesForMetTK.add(polioDates.get(polioDates.size() - 1));
                }
                if (dtpDates != null && dtpDates.size() > 0) {
                    datesForMetTK.add(dtpDates.get(dtpDates.size() - 1));
                }
                if (hepbDates != null && hepbDates.size() > 0) {
                    datesForMetTK.add(hepbDates.get(hepbDates.size() - 1));
                }
                if (varDates != null && varDates.size() > 0) {
                    datesForMetTK.add(varDates.get(varDates.size() - 1));
                }
                Collections.sort(datesForMetTK);
                if (!datesForMetTK.isEmpty()) {
                    dataGrid.set(REPORT_FIELD_MET_TK, m_dateFormat.format(datesForMetTK.get(datesForMetTK.size() - 1)));
                }
                populateSectionForTK(dataGrid, std);
            }
            if (m_tdapDose != null) {
                dataGrid.set(REPORT_FIELD_TDAP, m_dateFormat.format(m_tdapDose.getDate()));
                dataGrid.set(REPORT_FIELD_TDAP_YEARS, getAge(std.getPerson().getDob(), m_tdapDose.getDate()));
                int ageInMonth = (int) DateUtils.getAgeInMonths(std.getPerson().getDob(), m_tdapDose.getDate());
                dataGrid.set(REPORT_FIELD_TDAP_MEET, Boolean.valueOf(((int) Math.floor(ageInMonth / 12)) >= 7));
            }
        }
        dataGrid.sort(REPORT_FIELD_STUDENT_LAST_NAME, true);
        dataGrid.beforeTop();
        return dataGrid;
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
     * Returns dates of given doses which included in the category.
     *
     * @param category String
     * @return List
     */
    protected List<PlainDate> getDosesDates(ReportDataGrid dataGrid,
                                            String category,
                                            String reportField,
                                            String stdOid) {
        List<PlainDate> dosesDates = new ArrayList<PlainDate>();
        Collection<HealthImmunizationDefinition> defs = getDefinitionsByCategory(category);
        if (defs != null) {
            for (HealthImmunizationDefinition defn : defs) {
                HealthImmunizationSeries series = getSeries(defn, stdOid);
                if (series != null) {
                    if (CATEGORY_HIB.equals(category)) {
                        m_seriesHib = series;
                    }
                    if (CATEGORY_DTP.equals(category)) {
                        m_seriesDtp = series;
                    }
                    if (!StringUtils.isEmpty(m_fieldHisExemptReason)) {
                        String exemptionCode = (String) series.getFieldValueByBeanPath(m_fieldHisExemptReason);
                        if (!StringUtils.isEmpty(exemptionCode)) {
                            dataGrid.set(reportField + POSTFIX_PME,
                                    Boolean.valueOf(CODE_EXEMPTION_P.equals(exemptionCode)));
                        }
                    }
                    Collection<HealthImmunizationDose> doses = series.getImmunizationDoses();
                    for (HealthImmunizationDose dose : doses) {
                        if (dose.getDate() != null) {
                            if (CATEGORY_DTP.equals(category) && SERIES_TDAP.equals(defn.getSeriesName())) {
                                m_tdapDose = dose;
                                m_seriesTdap7 = series;
                                continue;
                            }
                            dosesDates.add(dose.getDate());
                        }
                    }
                }
            }
            Collections.sort(dosesDates);
        }
        return dosesDates;
    }

    /**
     * Returns series by definition and student.
     *
     * @param defn HealthImmunizationDefinition
     * @param student SisStudent
     * @return Health immunization series
     */
    protected HealthImmunizationSeries getSeries(HealthImmunizationDefinition defn, String stdOid) {
        Map<String, HealthImmunizationSeries> seriesMap = m_studentImmunizationSeriesMap.get(defn.getOid());
        if (seriesMap == null) {
            seriesMap = new HashMap<String, HealthImmunizationSeries>(0);
        }
        return seriesMap.get(stdOid);
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
     * Adds parameters from the input to the report.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getUser().getPersistenceKey());
        DataDictionaryField ddField = dictionary.findDataDictionaryFieldByAlias(ALIAS_HIS_EXEMPT_REASON);
        if (ddField != null) {
            m_fieldHisExemptReason = ddField.getJavaName();
        }
    }

    /**
     * Remember the currently selected student if this report is being run from the student module.
     *
     * @param userDate UserDataContainer
     * @see com.x2dev.sis.tools.ToolJavaSource#saveState(com.x2dev.sis.web.UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userDate) {
        X2Criteria studentCriteria = new X2Criteria();
        @SuppressWarnings("cast")
        SisStudent stdCurrent = userDate.getCurrentRecord(SisStudent.class);
        if (stdCurrent != null) {
            studentCriteria.addEqualTo(X2BaseBean.COL_OID, stdCurrent.getOid());
        } else {
            ContextList currentList = userDate.getCurrentList();
            if (currentList != null && currentList.getDataClass().equals(SisStudent.class)) {
                studentCriteria.addAndCriteria(getCurrentCriteria());
            }
        }
        String queryBy = (String) getParameter(QUERY_BY_PARAM);
        String queryString = (String) getParameter(QUERY_STRING_PARAM);
        if (queryBy != null && queryString != null && stdCurrent == null) {
            addUserCriteria(studentCriteria, queryBy, queryString, null, null);
        }
        m_stdByOids = getBroker().getMapByQuery(new QueryByCriteria(SisStudent.class, studentCriteria),
                X2BaseBean.COL_OID, 1028);
    }

    /**
     * Calculate race for the given Student.
     *
     * @param student
     * @return
     */
    private String getRace(SisStudent student) {
        String value = RACE_UNKNOWN;
        Collection<Race> races = m_races.get(student.getPersonOid());
        if (races != null && !races.isEmpty()) {
            for (Race race : races) {
                if (m_whiteCodes.contains(race.getRaceCode())) {
                    value = RACE_WHITE;
                }
            }
            if (RACE_UNKNOWN.equals(value)) {
                for (Race race : races) {
                    if (m_blackCodes.contains(race.getRaceCode())) {
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
        return value;
    }

    /**
     * Returns number of years between two dates.
     * argument 'date2' should be greater than 'date1' to avoid negative result
     *
     * @param date1 PlainDate
     * @param date2 PlainDate
     * @return int
     */
    private String getAge(PlainDate dob, PlainDate doseDate) {
        String ageToReturn = null;
        int ageInMonth = (int) DateUtils.getAgeInMonths(dob, doseDate);
        if (ageInMonth > 0) {
            int years = (int) Math.floor(ageInMonth / 12);
            int month = ageInMonth % 12;
            ageToReturn = String.valueOf(years) + " yrs " + String.valueOf(month) + " mos";
        }
        return ageToReturn;
    }

    /**
     * Generates immunization collections for definition, categories, series and doses
     * used to determine given doses.
     *
     */
    private void initImmunizationCollections() {
        // definitions
        QueryByCriteria definitionQuery = new QueryByCriteria(HealthImmunizationDefinition.class, new Criteria());
        m_definitions = getBroker().getCollectionByQuery(definitionQuery);
        // categories
        QueryByCriteria categoryQuery = new QueryByCriteria(HealthImmunizationDefinition.class, new Criteria());
        m_categories =
                getBroker().getGroupedCollectionByQuery(categoryQuery, HealthImmunizationDefinition.COL_CATEGORIES, 20);
        // series
        X2Criteria seriesMapCriteria = new X2Criteria();
        seriesMapCriteria.addIn(HealthImmunizationSeries.COL_STUDENT_OID, m_stdByOids.keySet());
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
     * Loads races.
     */
    private void loadRaces() {
        X2Criteria stdCriteria = new X2Criteria();
        stdCriteria.addIn(X2BaseBean.COL_OID, m_stdByOids.keySet());
        SubQuery raceSubquery = new SubQuery(SisStudent.class, SisStudent.COL_PERSON_OID, stdCriteria);
        X2Criteria raceCriteria = new X2Criteria();
        raceCriteria.addIn(Race.COL_PERSON_OID, raceSubquery);
        BeanQuery raceQuery = new BeanQuery(Race.class, raceCriteria);
        raceQuery.addOrderBy(Race.COL_PERSON_OID, true);
        m_races = getBroker().getGroupedCollectionByQuery(raceQuery, Race.COL_PERSON_OID, 1000);
        X2Criteria rtbCriteria = new X2Criteria();
        rtbCriteria.addEqualTo(ReferenceTable.COL_USER_NAME, RTB_RACE_CODES_NAME);
        /**
         * Change is done for 5.8 compatibility.
         * remove when 5.9 and higher.
         */
        ReferenceTable rtbRaceCode =
                (ReferenceTable) getBroker().getBeanByQuery(new QueryByCriteria(ReferenceTable.class, rtbCriteria));
        if (rtbRaceCode != null) {
            Map<String, ReferenceCode> codes = rtbRaceCode.getCodeMap();
            for (ReferenceCode code : codes.values()) {
                if (STATE_CODE_WHITE.equals(code.getStateCode())) {
                    m_whiteCodes.add(code.getCode());
                } else if (STATE_CODE_BLACK.equals(code.getStateCode())) {
                    m_blackCodes.add(code.getCode());
                } else {
                    m_mapDescription.put(code.getCode(), code.getDescription());
                }
            }
        }
    }

    /**
     * Populate grid with dates and ages.
     *
     * @param dataGrid
     * @param std
     * @param doseDates
     * @param numDoses
     * @param numAge1
     * @param numAge2
     * @param reportField
     * @param reportFieldAge
     * @param month
     */
    private void populateGridWithDose(ReportDataGrid dataGrid,
                                      SisStudent std,
                                      List<PlainDate> doseDates,
                                      Integer numDoses,
                                      Integer numAge1,
                                      Integer numAge2,
                                      String reportField,
                                      String reportFieldAge) {
        int iter = 0;
        if (doseDates != null && doseDates.size() > 0) {
            iter += doseDates.size() > numDoses ? numDoses : doseDates.size();
        }
        for (int i = 0; i < iter; i++) {
            dataGrid.set(reportField + (i + 1), m_dateFormat.format(doseDates.get(i)));
            if (numAge1 != null && i == numAge1.intValue() - 1) {
                dataGrid.set(reportFieldAge + (i + 1), getAge(std.getPerson().getDob(), doseDates.get(i)));
            }
            if (numAge2 != null && i == numAge2.intValue() - 1) {
                dataGrid.set(reportFieldAge + (i + 1), getAge(std.getPerson().getDob(), doseDates.get(i)));
            }
        }
    }

    /**
     * Populate status section for PK.
     *
     * @param dataGrid
     * @param hibDates
     * @param dob
     */
    private void populateSectionForPK(ReportDataGrid dataGrid, List<PlainDate> hibDates, SisStudent std) {
        if (BooleanAsStringConverter.TRUE.equals(std.getFieldValueByAlias(ALIAS_STD_IEP_STATUS))) {
            dataGrid.set(REPORT_FIELD_IEP_PK, Boolean.TRUE);
        }
        if (m_seriesHib != null && m_seriesHib.getFieldValueByAlias(ALIAS_HIS_FOLLOW_UP_REASON) != null) {
            dataGrid.set(REPORT_FIELD_FOLLOW_UP_PK, m_seriesHib.getFieldValueByAlias(ALIAS_HIS_FOLLOW_UP_REASON));
            if (m_seriesHib.getFieldValueByAlias(ALIAS_HIS_FOLLOW_UP_DATE) != null) {
                dataGrid.set(REPORT_FIELD_FOLLOW_DATE_PK,
                        m_dateFormat.format((PlainDate) m_seriesHib.getFieldValueByAlias(ALIAS_HIS_FOLLOW_UP_DATE)));
            }
        }
        if (hibDates != null && hibDates.size() >= 4) {
            for (PlainDate doseDate : hibDates) {
                int ageInMonth = (int) DateUtils.getAgeInMonths(std.getPerson().getDob(), doseDate);
                if (((int) Math.floor(ageInMonth / 12)) >= 1) {
                    dataGrid.set(REPORT_FIELD_ALL_PK, Boolean.valueOf(true));
                    dataGrid.set(REPORT_FIELD_MET_PK, m_dateFormat.format(hibDates.get(hibDates.size() - 1)));
                    break;
                }
            }
        }
    }

    /**
     * Populate status section for TDAP.
     *
     * @param dataGrid
     * @param hibDates
     * @param std
     */
    private void populateSectionForTDAP(ReportDataGrid dataGrid, List<PlainDate> dtpDates, SisStudent std) {
        if (BooleanAsStringConverter.TRUE.equals(std.getFieldValueByAlias(ALIAS_STD_IEP_STATUS))) {
            dataGrid.set(REPORT_FIELD_IEP_7, Boolean.TRUE);
        }
        if (BooleanAsStringConverter.TRUE.equals(std.getSchool().getFieldValueByAlias(ALIAS_SKL_HOME_HOSPITAL))) {
            dataGrid.set(REPORT_FIELD_HOME_7, Boolean.TRUE);
        }
        if (BooleanAsStringConverter.TRUE.equals(std.getSchool().getFieldValueByAlias(ALIAS_SKL_INDEP_STUD))) {
            dataGrid.set(REPORT_FIELD_INDEPEND_7, Boolean.TRUE);
        }
        if (m_seriesDtp != null && m_seriesDtp.getFieldValueByAlias(ALIAS_HIS_FOLLOW_UP_REASON) != null) {
            dataGrid.set(REPORT_FIELD_FOLLOW_UP_7, m_seriesDtp.getFieldValueByAlias(ALIAS_HIS_FOLLOW_UP_REASON));
            if (m_seriesDtp.getFieldValueByAlias(ALIAS_HIS_FOLLOW_UP_DATE) != null) {
                dataGrid.set(REPORT_FIELD_FOLLOW_DATE_7,
                        m_dateFormat.format((PlainDate) m_seriesDtp.getFieldValueByAlias(ALIAS_HIS_FOLLOW_UP_DATE)));
            }
        }
        if (dtpDates != null) {
            for (PlainDate doseDate : dtpDates) {
                int ageInMonth = (int) DateUtils.getAgeInMonths(std.getPerson().getDob(), doseDate);
                if (((int) Math.floor(ageInMonth / 12)) >= 7) {
                    dataGrid.set(REPORT_FIELD_ALL_7, Boolean.valueOf(true));
                    dataGrid.set(REPORT_FIELD_MET_7, m_dateFormat.format(dtpDates.get(dtpDates.size() - 1)));
                    break;
                }
            }
        }
    }

    /**
     * Populate status section for TK.
     *
     * @param dataGrid
     * @param hibDates
     * @param std
     */
    private void populateSectionForTK(ReportDataGrid dataGrid, SisStudent std) {
        if (BooleanAsStringConverter.TRUE.equals(std.getFieldValueByAlias(ALIAS_STD_IEP_STATUS))) {
            dataGrid.set(REPORT_FIELD_IEP_TK, Boolean.TRUE);
        }
        if (BooleanAsStringConverter.TRUE.equals(std.getSchool().getFieldValueByAlias(ALIAS_SKL_HOME_HOSPITAL))) {
            dataGrid.set(REPORT_FIELD_HOME_TK, Boolean.TRUE);
        }
        if (BooleanAsStringConverter.TRUE.equals(std.getSchool().getFieldValueByAlias(ALIAS_SKL_INDEP_STUD))) {
            dataGrid.set(REPORT_FIELD_INDEPEND_TK, Boolean.TRUE);
        }
        List<PlainDate> dates = new ArrayList<>();
        List<String> reasons = new ArrayList<>();
        if (m_seriesDtp != null && m_seriesDtp.getFieldValueByAlias(ALIAS_HIS_FOLLOW_UP_REASON) != null) {
            reasons.add((String) m_seriesDtp.getFieldValueByAlias(ALIAS_HIS_FOLLOW_UP_REASON));
            if (m_seriesDtp.getFieldValueByAlias(ALIAS_HIS_FOLLOW_UP_DATE) != null) {
                dates.add((PlainDate) m_seriesDtp.getFieldValueByAlias(ALIAS_HIS_FOLLOW_UP_DATE));
            }
        }
        if (m_seriesHepb != null && m_seriesHepb.getFieldValueByAlias(ALIAS_HIS_FOLLOW_UP_REASON) != null) {
            reasons.add((String) m_seriesHepb.getFieldValueByAlias(ALIAS_HIS_FOLLOW_UP_REASON));
            if (m_seriesHepb.getFieldValueByAlias(ALIAS_HIS_FOLLOW_UP_DATE) != null) {
                dates.add((PlainDate) m_seriesHepb.getFieldValueByAlias(ALIAS_HIS_FOLLOW_UP_DATE));
            }
        }
        if (m_seriesPolio != null && m_seriesPolio.getFieldValueByAlias(ALIAS_HIS_FOLLOW_UP_REASON) != null) {
            reasons.add((String) m_seriesPolio.getFieldValueByAlias(ALIAS_HIS_FOLLOW_UP_REASON));
            if (m_seriesPolio.getFieldValueByAlias(ALIAS_HIS_FOLLOW_UP_DATE) != null) {
                dates.add((PlainDate) m_seriesPolio.getFieldValueByAlias(ALIAS_HIS_FOLLOW_UP_DATE));
            }
        }
        if (m_seriesVar != null && m_seriesVar.getFieldValueByAlias(ALIAS_HIS_FOLLOW_UP_REASON) != null) {
            reasons.add((String) m_seriesVar.getFieldValueByAlias(ALIAS_HIS_FOLLOW_UP_REASON));
            if (m_seriesVar.getFieldValueByAlias(ALIAS_HIS_FOLLOW_UP_DATE) != null) {
                dates.add((PlainDate) m_seriesVar.getFieldValueByAlias(ALIAS_HIS_FOLLOW_UP_DATE));
            }
        }
        if (m_seriesMmr != null && m_seriesMmr.getFieldValueByAlias(ALIAS_HIS_FOLLOW_UP_REASON) != null) {
            reasons.add((String) m_seriesMmr.getFieldValueByAlias(ALIAS_HIS_FOLLOW_UP_REASON));
            if (m_seriesMmr.getFieldValueByAlias(ALIAS_HIS_FOLLOW_UP_DATE) != null) {
                dates.add((PlainDate) m_seriesMmr.getFieldValueByAlias(ALIAS_HIS_FOLLOW_UP_DATE));
            }
        }
        Collections.sort(dates);
        if (!dates.isEmpty()) {
            dataGrid.set(REPORT_FIELD_FOLLOW_DATE_TK, m_dateFormat.format(dates.get(0)));
        }
        if (!reasons.isEmpty()) {
            for (String reason : reasons) {
                if ("Temporary".equals(reason)) {
                    dataGrid.set(REPORT_FIELD_FOLLOW_UP_TEMP_TK, Boolean.TRUE);

                } else if ("Conditional".equals(reason)) {
                    dataGrid.set(REPORT_FIELD_FOLLOW_UP_COND_TK, Boolean.TRUE);

                } else if ("Needs doses now".equals(reason)) {
                    dataGrid.set(REPORT_FIELD_FOLLOW_UP_NEED_TK, Boolean.TRUE);
                }
            }
        }
    }
}
