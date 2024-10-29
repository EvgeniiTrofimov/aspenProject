/*
 * ====================================================================
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
package com.x2dev.reports.statereporting.ny;

/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2011 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.x2dev.sis.model.beans.ConductIncident;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Class for REPORT OF INCIDENTS CONCERNING SCHOOL SAFETY AND THE EDUCATIONAL CLIMATE.
 */
public class NYVadirPart2DignityData extends ReportJavaSourceNet {

    /**
     * The Class SchoolCounter.
     */
    public static class SchoolCounter {
        public static final String COLUMN_NATURE_A = "A"; // "Race";
        public static final String COLUMN_NATURE_B = "B"; // "Ethnic Group";
        public static final String COLUMN_NATURE_C = "C"; // "National Origin";
        public static final String COLUMN_NATURE_D = "D"; // "Color";
        public static final String COLUMN_NATURE_E = "E"; // "Religion";
        public static final String COLUMN_NATURE_F = "F"; // "Religious Practice";
        public static final String COLUMN_NATURE_G = "G"; // "Disability";
        public static final String COLUMN_NATURE_H = "H"; // "Gender";
        public static final String COLUMN_NATURE_I = "I"; // "Sexual Orientation";
        public static final String COLUMN_NATURE_J = "J"; // "Sex";
        public static final String COLUMN_NATURE_K = "K"; // "Weight";
        public static final String COLUMN_NATURE_M = "M"; // "Other";
        public static final String COLUMN_NATURE_TOTAL = "Total"; // "Total";

        public static final String INCIDENT_GROUP_HARASSMENT = "Discrimination and/or Harassment";
        public static final String INCIDENT_GROUP_CYBERBULLYING = "Cyberbullying";

        public static final String ROW_TYPE_1A = "1.a";
        public static final String ROW_TYPE_1B = "1.b";
        public static final String ROW_TYPE_2A = "2.a";
        public static final String ROW_TYPE_2B = "2.b";
        public static final String ROW_TYPE_2C = "2.c";
        public static final String ROW_TYPE_2D = "2.d";
        public static final String ROW_TYPE_3A = "3.a";
        public static final String ROW_TYPE_3B = "3.b";
        public static final String ROW_TYPE_3C = "3.c";
        public static final String ROW_TYPE_TOTAL = "Total";

        private static Map<String, List<String>> s_rows;
        private static List<String> s_columns;
        private static Map<String, String> s_comments;
        private static List<String> s_groups;
        private static Map<String, String> s_incidentTypeGroup;

        private HashMap<String, HashMap<String, Integer>> m_schoolCounter;
        private SisSchool m_school;

        static {
            s_groups = Arrays.asList(INCIDENT_GROUP_HARASSMENT, INCIDENT_GROUP_CYBERBULLYING);

            s_incidentTypeGroup = new HashMap<String, String>();
            s_incidentTypeGroup.put(INCIDENT_GROUP_HARASSMENT + "1", "1. Count of Incidents by Location");
            s_incidentTypeGroup.put(INCIDENT_GROUP_HARASSMENT + "2",
                    "2. Count of Incidents by Type of Discrimination/Harassment");
            s_incidentTypeGroup.put(INCIDENT_GROUP_HARASSMENT + "3", "3. Count of Incidents by Offender Type");
            s_incidentTypeGroup.put(INCIDENT_GROUP_CYBERBULLYING + "1",
                    "1. Count of Incidents by Type of Cyberbullying");
            s_incidentTypeGroup.put(INCIDENT_GROUP_CYBERBULLYING + "2", "2. Count of Incidents by Offender Type");

            s_rows = new HashMap<String, List<String>>();
            s_rows.put(INCIDENT_GROUP_HARASSMENT,
                    Arrays.asList(ROW_TYPE_1A, ROW_TYPE_1B, ROW_TYPE_2A, ROW_TYPE_2B, ROW_TYPE_2C,
                            ROW_TYPE_2D, ROW_TYPE_3A, ROW_TYPE_3B, ROW_TYPE_3C, ROW_TYPE_TOTAL));
            s_rows.put(INCIDENT_GROUP_CYBERBULLYING,
                    Arrays.asList(ROW_TYPE_1A, ROW_TYPE_1B, ROW_TYPE_2A, ROW_TYPE_2B, ROW_TYPE_2C, ROW_TYPE_TOTAL));

            s_columns = Arrays.asList(COLUMN_NATURE_A, COLUMN_NATURE_B, COLUMN_NATURE_C, COLUMN_NATURE_D,
                    COLUMN_NATURE_E, COLUMN_NATURE_F, COLUMN_NATURE_G, COLUMN_NATURE_H,
                    COLUMN_NATURE_I, COLUMN_NATURE_J, COLUMN_NATURE_K, COLUMN_NATURE_M,
                    COLUMN_NATURE_TOTAL);

            s_comments = new HashMap<String, String>();
            s_comments.put(INCIDENT_GROUP_HARASSMENT + ROW_TYPE_1A, "Incidents occurring on school property");
            s_comments.put(INCIDENT_GROUP_HARASSMENT + ROW_TYPE_1B,
                    "Incidents occurring at school-sponsored function off school grounds");
            s_comments.put(INCIDENT_GROUP_HARASSMENT + ROW_TYPE_2A,
                    "Incidents involving intimidation or abuse but no verbal threat or physical contact");
            s_comments.put(INCIDENT_GROUP_HARASSMENT + ROW_TYPE_2B,
                    "Incidents involving verbal threat but no physical contact");
            s_comments.put(INCIDENT_GROUP_HARASSMENT + ROW_TYPE_2C,
                    "Incidents involving physical contact but no verbal threat");
            s_comments.put(INCIDENT_GROUP_HARASSMENT + ROW_TYPE_2D,
                    "Incidents involving both verbal threat and physical contact");
            s_comments.put(INCIDENT_GROUP_HARASSMENT + ROW_TYPE_3A, "Incidents involving only student offenders");
            s_comments.put(INCIDENT_GROUP_HARASSMENT + ROW_TYPE_3B, "Incidents involving only employee offenders");
            s_comments.put(INCIDENT_GROUP_HARASSMENT + ROW_TYPE_3C,
                    "Incidents involving both student and employee offenders");
            s_comments.put(INCIDENT_GROUP_CYBERBULLYING + ROW_TYPE_1A,
                    "Incidents involving intimidation or abuse but no threat(s)");
            s_comments.put(INCIDENT_GROUP_CYBERBULLYING + ROW_TYPE_1B, "Incidents involving threat(s)");
            s_comments.put(INCIDENT_GROUP_CYBERBULLYING + ROW_TYPE_2A, "Incidents involving only student offenders");
            s_comments.put(INCIDENT_GROUP_CYBERBULLYING + ROW_TYPE_2B, "Incidents involving only employee offenders");
            s_comments.put(INCIDENT_GROUP_CYBERBULLYING + ROW_TYPE_2C,
                    "Incidents involving both student and employee offenders");
        }

        /**
         * Instantiates a new school counter.
         *
         * @param school SisSchool
         */
        public SchoolCounter(SisSchool school) {
            m_school = school;
            m_schoolCounter = new HashMap<String, HashMap<String, Integer>>();
            for (String incidentGroup : s_groups) {
                for (String rowType : s_rows.get(incidentGroup)) {
                    HashMap<String, Integer> typeCounter = new HashMap<String, Integer>();
                    m_schoolCounter.put(incidentGroup + rowType, typeCounter);
                    for (String column : s_columns) {
                        typeCounter.put(column, Integer.valueOf(0));
                    }
                }
            }
        }

        /**
         * Gets the incident groups.
         *
         * @return List
         */
        public static List<String> getIncidentGroups() {
            return s_groups;
        }

        /**
         * Gets the rows.
         *
         * @param incidentGroup String
         * @return List
         */
        public static List<String> getRows(String incidentGroup) {
            return s_rows.get(incidentGroup);
        }

        /**
         * Gets the school.
         *
         * @return Sis school
         */
        public SisSchool getSchool() {
            return m_school;
        }

        /**
         * Gets the type counter.
         *
         * @param incidentGroup String
         * @param row String
         * @return Map
         */
        public Map getTypeCounter(String incidentGroup, String row) {
            return m_schoolCounter.get(incidentGroup + row);
        }

        /**
         * Increase value.
         *
         * @param incidentGroup String
         * @param incidentType String
         * @param column String
         * @param countTotal boolean
         */
        public void increaseValue(String incidentGroup, String incidentType, String column, boolean countTotal) {
            String key = incidentGroup + incidentType;
            int prevValue = m_schoolCounter.get(key).get(column).intValue();
            m_schoolCounter.get(key).put(column, Integer.valueOf(prevValue + 1));
            // increase totals
            if (countTotal) {
                increaseValue(incidentGroup, incidentType, SchoolCounter.COLUMN_NATURE_TOTAL, false);
            }
        }

        /**
         * Gets the comment.
         *
         * @param incidentGroup String
         * @param row String
         * @return String
         */
        public String getComment(String incidentGroup, String row) {
            return s_comments.get(incidentGroup + row);
        }

        /**
         * Gets the incident type group.
         *
         * @param incidentGroup String
         * @param row String
         * @return String
         */
        public String getIncidentTypeGroup(String incidentGroup, String row) {
            return s_incidentTypeGroup.get(incidentGroup + row.substring(0, 1));
        }
    }

    private static final String ALIAS_BEDS_CODE = "BEDS CODE";
    private static final String ALIAS_CYBER_BULLYING = "DOE CYBER BULLYING";
    private static final String ALIAS_CYBER_BULLYING_THREAT = "DOE CYBER BULLYING THREAT";
    private static final String ALIAS_DASA_REPORTABLE = "DOE DASA REPORT OVERRIDE";

    private static final String ALIAS_HAR_RACE = "DOE HAR RACE";
    private static final String ALIAS_HAR_ETHNIC = "DOE HAR ETHNIC GROUP";
    private static final String ALIAS_HAR_NATIONAL = "DOE HAR NATIONAL ORIGIN";
    private static final String ALIAS_HAR_COLOR = "DOE HAR COLOR";
    private static final String ALIAS_HAR_RELIGION = "DOE HAR RELIGION";
    private static final String ALIAS_HAR_RELIGIOUS = "DOE HAR RELIGIOUS PRACTICE";
    private static final String ALIAS_HAR_DISABILITY = "DOE HAR DISABLITY";
    private static final String ALIAS_HAR_GENDER = "DOE HAR GENDER";
    private static final String ALIAS_HAR_SEXUAL = "DOE HAR SEXUAL ORIENTATION";
    private static final String ALIAS_HAR_SEX = "DOE HAR SEX";
    private static final String ALIAS_HAR_WEIGHT = "DOE HAR WEIGHT";
    private static final String ALIAS_HAR_OTHER = "DOE HAR OTHER";

    private static final String ALIAS_VERBAL_THREAT = "DOE VERBAL THREAT";
    private static final String ALIAS_PHYSICAL_CONTACT = "DOE PHYSICAL CONTACT";

    private static final String FIELD_SCHOOL = "school";
    private static final String FIELD_SCHOOL_NAME = "schoolName";
    private static final String FIELD_BEDS_CODE = "bedsCode";
    private static final String FIELD_INCIDENT_GROUP = "incidentGroup";
    private static final String FIELD_INCIDENT_GROUP_NUMBER = "incidentGroupNumber";
    private static final String FIELD_INCIDENT_TYPE_GROUP = "incidentTypeGroup";
    private static final String FIELD_INCIDENT_TYPE = "incidentType";
    private static final String FIELD_INCIDENT_TYPE_COMMENT = "incidentTypeComment";
    private static final String FIELD_TYPE_COUNTER = "incidentTypeCounter";

    private static final String PARAM_CONTEXT = "context";
    private static final String IPARAM_CONTEXT_OID = "contextOid";

    private static final String REFERENCE_TABLE_CND_LOCATION = "rtbCndLocation";

    // private static final String STATE_CODE_EVENT = "EVENT";
    private static final String STATE_CODE_INTIMIDATION = "10A";
    private static final String STATE_CODE_OFFSCHOOL = "OFFSCHOOL";
    // private static final String STATE_CODE_TRANSPORTATION = "TRANS";

    private DistrictSchoolYearContext m_context;
    private String m_dasaReportable;
    private DataDictionary m_dict;
    private HashMap<String, String> m_naturesMapping;
    private List<String> m_natures;

    /**
     * Find all incidents that include subIncident with harassment code OR with check box DASA
     * Reportable
     * (DOE DASA REPORT OVERRIDE) checked off.
     *
     * @return QueryByCriteria
     */
    protected QueryByCriteria buildQuery() {
        X2Criteria incidentCriteria = new X2Criteria();
        incidentCriteria.addGreaterOrEqualThan(ConductIncident.COL_INCIDENT_DATE, m_context.getStartDate());
        incidentCriteria.addLessOrEqualThan(ConductIncident.COL_INCIDENT_DATE, m_context.getEndDate());
        if (isSchoolContext()) {
            incidentCriteria.addEqualTo(ConductIncident.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            incidentCriteria.addNotEqualTo(
                    ConductIncident.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_ARCHIVE_INDICATOR,
                    BooleanAsStringConverter.TRUE);
            incidentCriteria.addNotEqualTo(
                    ConductIncident.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR,
                    BooleanAsStringConverter.TRUE);
        }
        HashSet incidentsCodes = new HashSet();
        // // Place a non-matching value in the list to insure criteria operates
        // // properly when there are no matching codes
        // incidentsCodes.add("##dummy##");
        DataDictionaryField field =
                m_dict.findDataDictionaryField(ConductIncident.class.getName(), ConductIncident.COL_INCIDENT_CODE);
        ReferenceTable refTable = field.getReferenceTable();
        if (refTable != null) {
            Collection<ReferenceCode> codes = refTable.getReferenceCodes(getBroker());
            for (ReferenceCode code : codes) {
                if (STATE_CODE_INTIMIDATION.equals(code.getStateCode())) {
                    incidentsCodes.add(code.getCode());
                }
            }
        }
        if (m_dasaReportable == null) {
            incidentCriteria.addIn(ConductIncident.COL_INCIDENT_CODE, incidentsCodes);
        } else {
            X2Criteria harassOrDasaCriteria = new X2Criteria();
            harassOrDasaCriteria.addIn(ConductIncident.COL_INCIDENT_CODE, incidentsCodes);
            X2Criteria dasaCriteria = new X2Criteria();
            dasaCriteria.addEqualTo(m_dasaReportable, BooleanAsStringConverter.TRUE);
            harassOrDasaCriteria.addOrCriteria(dasaCriteria);

            incidentCriteria.addAndCriteria(harassOrDasaCriteria);
        }

        return new QueryByCriteria(ConductIncident.class, incidentCriteria);
    }

    /**
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected ReportDataGrid gatherData() throws Exception {
        ReportDataGrid dataGrid = new ReportDataGrid();
        QueryIterator incidentIterator = getBroker().getIteratorByQuery(buildQuery());
        Map<String, SchoolCounter> counters = new HashMap<String, SchoolCounter>();

        try {
            while (incidentIterator.hasNext()) {
                ConductIncident incident = (ConductIncident) incidentIterator.next();
                SisSchool school = incident.getSchool();

                SchoolCounter counter = counters.get(school.getName());
                if (counter == null) {
                    counter = new SchoolCounter(school);
                    counters.put(school.getName(), counter);
                }

                Set<String> natures = determineNatures(incident);

                /** Harassment group **/
                // Section 1. Incident place
                // if Incident Location state codes in {OFFSCHOOL}
                // then incident type 1B
                String stateLocationCode = m_dict.findStateReferenceCode(REFERENCE_TABLE_CND_LOCATION,
                        incident.getIncidentLocation());
                if (STATE_CODE_OFFSCHOOL.equals(stateLocationCode)) {
                    increaseNatureValues(natures, counter, SchoolCounter.INCIDENT_GROUP_HARASSMENT,
                            SchoolCounter.ROW_TYPE_1B);
                } else {
                    increaseNatureValues(natures, counter, SchoolCounter.INCIDENT_GROUP_HARASSMENT,
                            SchoolCounter.ROW_TYPE_1A);
                }

                // Section 2. Verbal threat or physical contact
                boolean isVerbalThreat = false;
                String doeVerbalThreat = (String) incident.getFieldValueByAlias(ALIAS_VERBAL_THREAT);
                if (BooleanAsStringConverter.TRUE.equals(doeVerbalThreat)) {
                    isVerbalThreat = true;
                }

                boolean isPhysicalContact = false;
                String doePhysicalContact = (String) incident.getFieldValueByAlias(ALIAS_PHYSICAL_CONTACT);
                if (BooleanAsStringConverter.TRUE.equals(doePhysicalContact)) {
                    isPhysicalContact = true;
                }

                if (isVerbalThreat && isPhysicalContact) {
                    increaseNatureValues(natures, counter, SchoolCounter.INCIDENT_GROUP_HARASSMENT,
                            SchoolCounter.ROW_TYPE_2D);
                } else if (isPhysicalContact) {
                    increaseNatureValues(natures, counter, SchoolCounter.INCIDENT_GROUP_HARASSMENT,
                            SchoolCounter.ROW_TYPE_2C);
                } else if (isVerbalThreat) {
                    increaseNatureValues(natures, counter, SchoolCounter.INCIDENT_GROUP_HARASSMENT,
                            SchoolCounter.ROW_TYPE_2B);
                } else {
                    increaseNatureValues(natures, counter, SchoolCounter.INCIDENT_GROUP_HARASSMENT,
                            SchoolCounter.ROW_TYPE_2A);
                }

                // Section 3. Offenders
                boolean isStudentOffender = false;
                if (!StringUtils.isEmpty(incident.getStudentOid())) {
                    isStudentOffender = true;
                }
                boolean isStaffOffender = false;
                // For now way of determining staff offenders not represent.
                if (isStudentOffender && isStaffOffender) {
                    increaseNatureValues(natures, counter, SchoolCounter.INCIDENT_GROUP_HARASSMENT,
                            SchoolCounter.ROW_TYPE_3C);
                } else if (isStaffOffender) {
                    increaseNatureValues(natures, counter, SchoolCounter.INCIDENT_GROUP_HARASSMENT,
                            SchoolCounter.ROW_TYPE_3B);
                } else {
                    increaseNatureValues(natures, counter, SchoolCounter.INCIDENT_GROUP_HARASSMENT,
                            SchoolCounter.ROW_TYPE_3A);
                }

                // Section 4. Total
                for (String nature : natures) {
                    counter.increaseValue(SchoolCounter.INCIDENT_GROUP_HARASSMENT, SchoolCounter.ROW_TYPE_TOTAL,
                            nature, true);
                }

                boolean isCyberBullying =
                        BooleanAsStringConverter.TRUE.equals(incident.getFieldValueByAlias(ALIAS_CYBER_BULLYING));
                boolean isCyberBullyingThreat = BooleanAsStringConverter.TRUE
                        .equals(incident.getFieldValueByAlias(ALIAS_CYBER_BULLYING_THREAT));

                /** Cyberbullying **/
                // TODO need additional information:
                // If the incident is classified as "Cyberbyllying" it is
                // considered in both sections, or only in the second?
                // For now assuming that count in both
                if (isCyberBullying || isCyberBullyingThreat) {
                    // Section 1. Cyberbullying Type
                    if (isCyberBullying) {
                        increaseNatureValues(natures, counter, SchoolCounter.INCIDENT_GROUP_CYBERBULLYING,
                                SchoolCounter.ROW_TYPE_1A);
                    } else {
                        increaseNatureValues(natures, counter, SchoolCounter.INCIDENT_GROUP_CYBERBULLYING,
                                SchoolCounter.ROW_TYPE_1B);
                    }

                    // Section 2. Offenders
                    // For now way of determining staff offenders not represent.
                    if (isStudentOffender && isStaffOffender) {
                        increaseNatureValues(natures, counter, SchoolCounter.INCIDENT_GROUP_CYBERBULLYING,
                                SchoolCounter.ROW_TYPE_2C);
                    } else if (isStaffOffender) {
                        increaseNatureValues(natures, counter, SchoolCounter.INCIDENT_GROUP_CYBERBULLYING,
                                SchoolCounter.ROW_TYPE_2B);
                    } else {
                        increaseNatureValues(natures, counter, SchoolCounter.INCIDENT_GROUP_CYBERBULLYING,
                                SchoolCounter.ROW_TYPE_2A);
                    }

                    // Section 3. Total
                    for (String nature : natures) {
                        counter.increaseValue(SchoolCounter.INCIDENT_GROUP_CYBERBULLYING, SchoolCounter.ROW_TYPE_TOTAL,
                                nature, true);
                    }
                }
            }
        } finally {
            incidentIterator.close();
        }

        prepareDataGrid(dataGrid, counters);
        return dataGrid;
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {

        String contextOid = (String) getParameter(IPARAM_CONTEXT_OID);
        m_context = (DistrictSchoolYearContext) getBroker().getBeanByOid(DistrictSchoolYearContext.class, contextOid);
        addParameter(PARAM_CONTEXT, m_context);

        m_dict = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());

        initializeJavaNames();

        m_naturesMapping = new HashMap<String, String>();
        m_naturesMapping.put(ALIAS_HAR_RACE, SchoolCounter.COLUMN_NATURE_A);
        m_naturesMapping.put(ALIAS_HAR_ETHNIC, SchoolCounter.COLUMN_NATURE_B);
        m_naturesMapping.put(ALIAS_HAR_NATIONAL, SchoolCounter.COLUMN_NATURE_C);
        m_naturesMapping.put(ALIAS_HAR_COLOR, SchoolCounter.COLUMN_NATURE_D);
        m_naturesMapping.put(ALIAS_HAR_RELIGION, SchoolCounter.COLUMN_NATURE_E);
        m_naturesMapping.put(ALIAS_HAR_RELIGIOUS, SchoolCounter.COLUMN_NATURE_F);
        m_naturesMapping.put(ALIAS_HAR_DISABILITY, SchoolCounter.COLUMN_NATURE_G);
        m_naturesMapping.put(ALIAS_HAR_GENDER, SchoolCounter.COLUMN_NATURE_H);
        m_naturesMapping.put(ALIAS_HAR_SEXUAL, SchoolCounter.COLUMN_NATURE_I);
        m_naturesMapping.put(ALIAS_HAR_SEX, SchoolCounter.COLUMN_NATURE_J);
        m_naturesMapping.put(ALIAS_HAR_WEIGHT, SchoolCounter.COLUMN_NATURE_K);
        m_naturesMapping.put(ALIAS_HAR_OTHER, SchoolCounter.COLUMN_NATURE_M);

        m_natures = Arrays.asList(ALIAS_HAR_RACE, ALIAS_HAR_ETHNIC, ALIAS_HAR_NATIONAL, ALIAS_HAR_COLOR,
                ALIAS_HAR_RELIGION, ALIAS_HAR_RELIGIOUS, ALIAS_HAR_DISABILITY,
                ALIAS_HAR_GENDER, ALIAS_HAR_SEXUAL, ALIAS_HAR_SEX, ALIAS_HAR_WEIGHT,
                ALIAS_HAR_OTHER);
    }

    /**
     * Initialize java names.
     */
    private void initializeJavaNames() {
        m_dasaReportable = translateAliasToJavaName(ALIAS_DASA_REPORTABLE);
    }

    /**
     * Determine natures.
     *
     * @param incident ConductIncident
     * @return Set
     */
    private Set<String> determineNatures(ConductIncident incident) {
        Set<String> natures = new HashSet<String>();

        for (String nature : m_natures) {
            String doeNature = (String) incident.getFieldValueByAlias(nature);
            if (BooleanAsStringConverter.TRUE.equals(doeNature)) {
                natures.add(m_naturesMapping.get(nature));
            }
        }
        return natures;
    }

    /**
     * Increase nature values.
     *
     * @param natures Set<String>
     * @param counter SchoolCounter
     * @param incidentGroup String
     * @param incidentType String
     */
    private void increaseNatureValues(Set<String> natures,
                                      SchoolCounter counter,
                                      String incidentGroup,
                                      String incidentType) {
        for (String nature : natures) {
            counter.increaseValue(incidentGroup, incidentType, nature, true);
        }
    }

    /**
     * Prepare data grid.
     *
     * @param dataGrid ReportDataGrid
     * @param counters Map<String,SchoolCounter>
     */
    private void prepareDataGrid(ReportDataGrid dataGrid, Map<String, SchoolCounter> counters) {
        TreeSet<String> schoolKeys = new TreeSet(counters.keySet());
        for (String schoolKey : schoolKeys) {
            SchoolCounter counter = counters.get(schoolKey);
            SisSchool school = counter.getSchool();
            String bedsCode = (String) school.getFieldValueByAlias(ALIAS_BEDS_CODE);

            List<String> incidentGroups = SchoolCounter.getIncidentGroups();
            for (String incidentGroup : incidentGroups) {
                for (String row : SchoolCounter.getRows(incidentGroup)) {
                    dataGrid.append();
                    dataGrid.set(FIELD_SCHOOL_NAME, schoolKey);
                    dataGrid.set(FIELD_SCHOOL, school);
                    dataGrid.set(FIELD_BEDS_CODE, bedsCode);
                    dataGrid.set(FIELD_INCIDENT_GROUP, incidentGroup);
                    dataGrid.set(FIELD_INCIDENT_GROUP_NUMBER,
                            String.valueOf(incidentGroups.indexOf(incidentGroup) + 1));
                    dataGrid.set(FIELD_INCIDENT_TYPE_GROUP, counter.getIncidentTypeGroup(incidentGroup, row));
                    dataGrid.set(FIELD_INCIDENT_TYPE, row);
                    dataGrid.set(FIELD_INCIDENT_TYPE_COMMENT, counter.getComment(incidentGroup, row));
                    dataGrid.set(FIELD_TYPE_COUNTER, counter.getTypeCounter(incidentGroup, row));
                }
            }
        }
        dataGrid.beforeTop();
    }

    /**
     * Translates an alias into a Java bean path name.
     *
     * @param alias String
     * @return String
     */
    private String translateAliasToJavaName(String alias) {
        String javaName = null;

        DataDictionaryField field = m_dict.findDataDictionaryFieldByAlias(alias);
        if (field != null) {
            javaName = field.getJavaName();
        }

        return javaName;
    }
}
