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
package com.x2dev.procedures.statereporting.ga;
/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2015 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.KeyValuePair;
import com.x2dev.utils.StreamUtils;
import com.x2dev.utils.X2BaseException;
import java.io.ByteArrayInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Clears data for GA FTE based on user inputs.
 *
 * @author X2 Development Corporation
 */
public class FTEClearData extends ProcedureJavaSource {
    /**
     * Name for the Boolean "clearCrsViewsData" parameter. The value is an Boolean.
     */
    public static final String CLEAR_CRS_VIEWS_DATA = "clearCrsViewsData";

    /**
     * Name for the Boolean "clearFteSegmentData" parameter. The value is an Boolean.
     */
    public static final String CLEAR_FTE_SEGMENT_DATA = "clearFteSegmentData";

    /**
     * Name for the Boolean "clearSEHoursData" parameter. The value is an Boolean.
     */
    public static final String CLEAR_SE_HOURS_DATA = "clearSEHoursData";

    /**
     * Name for the Boolean "clearMiscAllIEP" parameter. The value is an Boolean.
     */
    public static final String CLEAR_MISC_ALL_IEP = "clearMiscAllIEP";

    /**
     * Name for the Boolean "clearMiscReportSchool" parameter. The value is an Boolean.
     */
    public static final String CLEAR_MISC_REPORT_SCHOOL = "clearMiscReportSchool";

    /**
     * Name for the Boolean "clearMiscResidEnv" parameter. The value is an Boolean.
     */
    public static final String CLEAR_MISC_RESID_ENV = "clearMiscResidEnv";

    /**
     * Name for the Boolean "clearMiscSpedEnv" parameter. The value is an Boolean.
     */
    public static final String CLEAR_MISC_SPED_ENV = "clearMiscSpedEnv";

    /**
     * Name for the Boolean "clearMiscPrimExc" parameter. The value is an Boolean.
     */
    public static final String CLEAR_MISC_PRIM_EXC = "clearMiscPrimExc";

    /**
     * Name for the Boolean "clearMiscAreaServed" parameter. The value is an Boolean.
     */
    public static final String CLEAR_MISC_AREA_SERVED = "clearMiscAreaServed";

    /**
     * Name for the Boolean "clearMiscEsolItinerant" parameter. The value is an Boolean.
     */
    public static final String CLEAR_MISC_ESOL_SEGMENTS = "clearMiscEsolSegments";

    // Input Parameters
    /**
     * Name for the enumerated "selection" parameter. The value is an Integer.
     */
    public static final String INPUT_PARAM_ENR_STATUS = "enrStatus";

    // Constants
    private static final String ALIAS_RELATED_SERV = "DOE Related Ser";
    private static final String ALIAS_SERVICES = "DOE Services";
    private static final String ALIAS_STD_FTE_CALC_CRS_STATE_CODE = "FTE CALCULATED STATE CRS CODE";
    private static final String ALL_IEP_SERVICES_ALIAS = "DOE ALL IEP";
    private static final String AREA_SERVED_ALIAS = "DOE Area Served";
    private static final String CRS_VIEW_MAIN_ALIAS = "DOE Section View";
    private static final String ESOL_SEGMENTS = "all-std-ESOLSegments";
    private static final String INCLUSION_ALIAS = "DOE Inclusion";
    private static final String ITINERANT_ALIAS = "DOE Itinerant";
    private static final String LOCATION_ALIAS = "DOE Location";
    private static final String PRIMARY_EXCEPTIONALITY_ALIAS = "DOE Primary Exceptionality";
    private static final String PROGRAM_ALIAS = "DOE Program Code";
    private static final String PROGRAM_ORIG_ALIAS = "DOE Orig Prog Code";
    private static final String REPORT_SCHOOL_ALIAS = "DOE Override School Code";
    private static final String RESIDENTIAL_ENVIRONMENT_ALIAS = "DOE Residential Environment";
    private static final String SPACE = " ";
    private static final String SPED_ENVIRONMENT_ALIAS = "DOE SPED Environment";
    private static final String SPEECH_ALIAS = "DOE Speech";
    private static final String TRANSPORT_ALIAS = "DOE Transport";
    private static final String QUERY_BY_PARAM = "queryBy1";
    private static final String QUERY_STRING_PARAM = "queryString1";

    private static final String FTE_ALIAS_LIST[] =
            {PROGRAM_ALIAS, PROGRAM_ORIG_ALIAS, LOCATION_ALIAS, TRANSPORT_ALIAS, SPEECH_ALIAS,
                    ITINERANT_ALIAS, INCLUSION_ALIAS, ALIAS_STD_FTE_CALC_CRS_STATE_CODE};

    String[] doeRelatedSerSuffixes = {"A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "3"};
    String[] doeServicesSuffixes =
            {"P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z", "1", "2", "3", "6", "7", "8"};

    // Used for storing java path and default values for miscellaneous fields
    private String doeSegmentsDefaultValue = null;
    private String doeSegmentsJavaPath = null;
    private String doeAllIEPJavaPath = null;
    private String doeAllIEPDefaultValue = null;
    private String doeAreaServedJavaPath = null;
    private String doeAreaServedDefaultValue = null;
    private String doePrimExcJavaPath = null;
    private String doePrimExcDefaultValue = null;
    private String doeReportSchoolJavaPath = null;
    private String doeReportSchoolDefaultValue = null;
    private String doeResidEnvJavaPath = null;
    private String doeResidEnvDefaultValue = null;
    private String doeSpedEnvJavaPath = null;
    private String doeSpedEnvDefaultValue = null;

    /**
     * Instance variables.
     */
    private Boolean m_clearCrsViewsData;
    private Boolean m_clearFTESegmentData;
    private Boolean m_clearSEHoursData;
    private Boolean m_clearMiscAllIEP;
    private Boolean m_clearMiscReportSchool;
    private Boolean m_clearMiscResidEnv;
    private Boolean m_clearMiscSpedEnv;
    private Boolean m_clearMiscPrimExc;
    private Boolean m_clearMiscAreaServed;
    private Boolean m_clearMiscEsolSegments;

    // Map contains alias prefix and a KeyValuePair of the javaName and defaultValue
    private Map<String, KeyValuePair<String, String>>[] m_crsViewsAliasMap;

    // Map contains alias prefix (used in an array) and a KeyValuePair of the javaName and
    // defaultValue
    private Map<String, KeyValuePair<String, String>>[] m_FTESegmentAliasMap;

    // Map contains alias and KeyValuePair of the javaName and defaultValue
    private Map<String, KeyValuePair<String, String>> m_servicesAliasMap;

    /**
     * Prepare alias lookups, data maps.
     */
    @Override
    protected void initialize() {
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField field = null;

        field = dictionary.findDataDictionaryFieldByAlias(ESOL_SEGMENTS);
        doeSegmentsJavaPath = field.getJavaName();
        doeSegmentsDefaultValue = field.getDefaultValue();

        field = dictionary.findDataDictionaryFieldByAlias(ALL_IEP_SERVICES_ALIAS);
        doeAllIEPJavaPath = field.getJavaName();
        doeAllIEPDefaultValue = field.getDefaultValue();

        field = dictionary.findDataDictionaryFieldByAlias(AREA_SERVED_ALIAS);
        doeAreaServedJavaPath = field.getJavaName();
        doeAreaServedDefaultValue = field.getDefaultValue();

        field = dictionary.findDataDictionaryFieldByAlias(PRIMARY_EXCEPTIONALITY_ALIAS);
        doePrimExcJavaPath = field.getJavaName();
        doePrimExcDefaultValue = field.getDefaultValue();

        field = dictionary.findDataDictionaryFieldByAlias(REPORT_SCHOOL_ALIAS);
        doeReportSchoolJavaPath = field.getJavaName();
        doeReportSchoolDefaultValue = field.getDefaultValue();

        field = dictionary.findDataDictionaryFieldByAlias(RESIDENTIAL_ENVIRONMENT_ALIAS);
        doeResidEnvJavaPath = field.getJavaName();
        doeResidEnvDefaultValue = field.getDefaultValue();

        field = dictionary.findDataDictionaryFieldByAlias(SPED_ENVIRONMENT_ALIAS);
        doeSpedEnvJavaPath = field.getJavaName();
        doeSpedEnvDefaultValue = field.getDefaultValue();

        /*
         * Prepare the FTE alias maps and populate java paths and default values.
         */
        m_crsViewsAliasMap = new Map[6];
        field = null;
        for (int postfix = 0; postfix < 6; postfix++) {
            m_crsViewsAliasMap[postfix] = new HashMap<String, KeyValuePair<String, String>>();

            field = dictionary.findDataDictionaryFieldByAlias(CRS_VIEW_MAIN_ALIAS + SPACE + (postfix + 1));
            if (field != null) {
                m_crsViewsAliasMap[postfix].put(CRS_VIEW_MAIN_ALIAS + SPACE + (postfix + 1),
                        new KeyValuePair<String, String>(field.getJavaName(), field.getDefaultValue()));
            }
        }

        /*
         * Prepare the FTE alias maps and populate java paths and default values.
         */
        m_FTESegmentAliasMap = new Map[6];
        field = null;
        for (int i = 0; i < 6; i++) {
            m_FTESegmentAliasMap[i] = new HashMap<String, KeyValuePair<String, String>>();
            String postFix = SPACE + Integer.toString(i + 1).trim();

            for (int a = 0; a < FTE_ALIAS_LIST.length; a++) {
                String aliasRoot = FTE_ALIAS_LIST[a];
                field = dictionary.findDataDictionaryFieldByAlias(aliasRoot + postFix);
                if (field != null) {
                    m_FTESegmentAliasMap[i].put(aliasRoot,
                            new KeyValuePair<String, String>(field.getJavaName(), field.getDefaultValue()));
                }
            }
        }

        // Populates the doeHoursSuffixes array and stores the java paths and default values in a
        // Map/KeyValuePair.
        m_servicesAliasMap = new HashMap();
        for (int j = 0; j < doeRelatedSerSuffixes.length; j++) {
            String currentAlias = ALIAS_RELATED_SERV + SPACE + doeRelatedSerSuffixes[j];
            DataDictionaryField currentField = dictionary.findDataDictionaryFieldByAlias(currentAlias);
            if (currentField != null) {
                String currentAliasJavaName = currentField.getJavaName();
                String fieldDefaultValue = currentField.getDefaultValue();
                m_servicesAliasMap.put(currentAlias,
                        new KeyValuePair<String, String>(currentAliasJavaName, fieldDefaultValue));
            }
        }
        for (int j = 0; j < doeServicesSuffixes.length; j++) {
            String currentAlias = ALIAS_SERVICES + SPACE + doeServicesSuffixes[j];
            DataDictionaryField currentField = dictionary.findDataDictionaryFieldByAlias(currentAlias);
            if (currentField != null) {
                String currentAliasJavaName = currentField.getJavaName();
                String fieldDefaultValue = currentField.getDefaultValue();
                m_servicesAliasMap.put(currentAlias,
                        new KeyValuePair<String, String>(currentAliasJavaName, fieldDefaultValue));
            }
        }
    }

    /**
     * Executes the procedure.
     */
    @Override
    protected void execute() {
        // get input parameters.
        m_clearCrsViewsData = (Boolean) getParameter(CLEAR_CRS_VIEWS_DATA);
        if (m_clearCrsViewsData == null) {
            m_clearCrsViewsData = Boolean.FALSE;
        }

        m_clearFTESegmentData = (Boolean) getParameter(CLEAR_FTE_SEGMENT_DATA);
        if (m_clearFTESegmentData == null) {
            m_clearFTESegmentData = Boolean.FALSE;
        }

        m_clearSEHoursData = (Boolean) getParameter(CLEAR_SE_HOURS_DATA);
        if (m_clearSEHoursData == null) {
            m_clearSEHoursData = Boolean.FALSE;
        }

        m_clearMiscAllIEP = (Boolean) getParameter(CLEAR_MISC_ALL_IEP);
        if (m_clearMiscAllIEP == null) {
            m_clearMiscAllIEP = Boolean.FALSE;
        }

        m_clearMiscReportSchool = (Boolean) getParameter(CLEAR_MISC_REPORT_SCHOOL);
        if (m_clearMiscReportSchool == null) {
            m_clearMiscReportSchool = Boolean.FALSE;
        }

        m_clearMiscResidEnv = (Boolean) getParameter(CLEAR_MISC_RESID_ENV);
        if (m_clearMiscResidEnv == null) {
            m_clearMiscResidEnv = Boolean.FALSE;
        }

        m_clearMiscSpedEnv = (Boolean) getParameter(CLEAR_MISC_SPED_ENV);
        if (m_clearMiscSpedEnv == null) {
            m_clearMiscSpedEnv = Boolean.FALSE;
        }

        m_clearMiscPrimExc = (Boolean) getParameter(CLEAR_MISC_PRIM_EXC);
        if (m_clearMiscPrimExc == null) {
            m_clearMiscPrimExc = Boolean.FALSE;
        }

        m_clearMiscAreaServed = (Boolean) getParameter(CLEAR_MISC_AREA_SERVED);
        if (m_clearMiscAreaServed == null) {
            m_clearMiscAreaServed = Boolean.FALSE;
        }

        m_clearMiscEsolSegments = (Boolean) getParameter(CLEAR_MISC_ESOL_SEGMENTS);
        if (m_clearMiscEsolSegments == null) {
            m_clearMiscEsolSegments = Boolean.FALSE;
        }
        Collection<SisStudent> studentsToUpdate = queryStudents();
        for (SisStudent student : studentsToUpdate) {
            clearStudentData(student);
        }
    }

    /**
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#exportResults()
     */
    @Override
    protected void exportResults() throws X2BaseException {
        StringBuilder buffer = new StringBuilder(256);
        buffer.append("Process Complete");

        try {
            ByteArrayInputStream inputStream = new ByteArrayInputStream(buffer.toString().getBytes());
            try {
                StreamUtils.copyStream(inputStream, getResultHandler().getOutputStream());
            } finally {
                inputStream.close();
            }
        } catch (FileNotFoundException fnfe) {
            throw new X2BaseException(fnfe);
        } catch (IOException ioe) {
            throw new X2BaseException(ioe);
        }

    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.
     *      UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException {
        super.saveState(userData);
    }

    /**
     * Update the student record with the default values for the FTE Segment data.
     *
     * @param student SisStudent
     */
    private void clearStudentData(SisStudent student) {
        if (m_clearCrsViewsData.booleanValue()) {
            int count = 0;
            for (count = 0; count < 6; count++) {
                {
                    student.setFieldValueByBeanPath(
                            m_crsViewsAliasMap[count].get(CRS_VIEW_MAIN_ALIAS + SPACE + (count + 1)).getKey(),
                            m_crsViewsAliasMap[count].get(CRS_VIEW_MAIN_ALIAS + SPACE + (count + 1)).getValue());
                }
            }
        }
        if (m_clearFTESegmentData.booleanValue()) {
            int count = 0;
            for (count = 0; count < 6; count++) {
                {
                    student.setFieldValueByBeanPath(m_FTESegmentAliasMap[count].get(PROGRAM_ALIAS).getKey(),
                            m_FTESegmentAliasMap[count].get(PROGRAM_ALIAS).getValue());
                    student.setFieldValueByBeanPath(m_FTESegmentAliasMap[count].get(PROGRAM_ORIG_ALIAS).getKey(),
                            m_FTESegmentAliasMap[count].get(PROGRAM_ORIG_ALIAS).getValue());
                    student.setFieldValueByBeanPath(m_FTESegmentAliasMap[count].get(LOCATION_ALIAS).getKey(),
                            m_FTESegmentAliasMap[count].get(LOCATION_ALIAS).getValue());
                    student.setFieldValueByBeanPath(m_FTESegmentAliasMap[count].get(TRANSPORT_ALIAS).getKey(),
                            m_FTESegmentAliasMap[count].get(TRANSPORT_ALIAS).getValue());
                    student.setFieldValueByBeanPath(m_FTESegmentAliasMap[count].get(SPEECH_ALIAS).getKey(),
                            m_FTESegmentAliasMap[count].get(SPEECH_ALIAS).getValue());
                    student.setFieldValueByBeanPath(m_FTESegmentAliasMap[count].get(ITINERANT_ALIAS).getKey(),
                            m_FTESegmentAliasMap[count].get(ITINERANT_ALIAS).getValue());
                    student.setFieldValueByBeanPath(m_FTESegmentAliasMap[count].get(INCLUSION_ALIAS).getKey(),
                            m_FTESegmentAliasMap[count].get(INCLUSION_ALIAS).getValue());
                    student.setFieldValueByBeanPath(
                            m_FTESegmentAliasMap[count].get(ALIAS_STD_FTE_CALC_CRS_STATE_CODE).getKey(),
                            m_FTESegmentAliasMap[count].get(ALIAS_STD_FTE_CALC_CRS_STATE_CODE).getValue());
                }
            }
        }
        if (m_clearSEHoursData.booleanValue()) {
            for (int j = 0; j < doeRelatedSerSuffixes.length; j++) {
                student.setFieldValueByBeanPath(
                        m_servicesAliasMap.get(ALIAS_RELATED_SERV + SPACE + doeRelatedSerSuffixes[j]).getKey(),
                        m_servicesAliasMap.get(ALIAS_RELATED_SERV + SPACE + doeRelatedSerSuffixes[j]).getValue());
            }
            for (int j = 0; j < doeServicesSuffixes.length; j++) {
                student.setFieldValueByBeanPath(
                        m_servicesAliasMap.get(ALIAS_SERVICES + SPACE + doeServicesSuffixes[j]).getKey(),
                        m_servicesAliasMap.get(ALIAS_SERVICES + SPACE + doeServicesSuffixes[j]).getValue());
            }
        }
        if (m_clearMiscAllIEP.booleanValue()) {
            student.setFieldValueByBeanPath(doeAllIEPJavaPath, doeAllIEPDefaultValue);
        }
        if (m_clearMiscAreaServed.booleanValue()) {
            student.setFieldValueByBeanPath(doeAreaServedJavaPath, doeAreaServedDefaultValue);
        }
        if (m_clearMiscPrimExc.booleanValue()) {
            student.setFieldValueByBeanPath(doePrimExcJavaPath, doePrimExcDefaultValue);
        }
        if (m_clearMiscReportSchool.booleanValue()) {
            student.setFieldValueByBeanPath(doeReportSchoolJavaPath, doeReportSchoolDefaultValue);
        }
        if (m_clearMiscResidEnv.booleanValue()) {
            student.setFieldValueByBeanPath(doeResidEnvJavaPath, doeResidEnvDefaultValue);
        }
        if (m_clearMiscSpedEnv.booleanValue()) {
            student.setFieldValueByBeanPath(doeSpedEnvJavaPath, doeSpedEnvDefaultValue);
        }
        if (m_clearMiscEsolSegments.booleanValue()) {
            student.setFieldValueByBeanPath(doeSegmentsJavaPath, doeSegmentsDefaultValue);
        }
        getBroker().saveBeanForced(student);
    }

    /**
     * Return Collection of students to clear data based on input criteria.
     *
     * @return Collection
     */
    private Collection queryStudents() {

        String activeCode =
                PreferenceManager.getPreferenceValue(getOrganization(), SystemPreferenceDefinition.STUDENT_ACTIVE_CODE);
        // Identify students to restore defaults
        X2Criteria criteria = new X2Criteria();
        int enrStatus = ((Integer) getParameter(INPUT_PARAM_ENR_STATUS)).intValue();
        switch (enrStatus) {
            case 1: // Active
                criteria.addEqualTo(SisStudent.COL_ENROLLMENT_STATUS, activeCode);
                break;

            case 2: // Not Active (Any student who is not Active)
                criteria.addNotEqualTo(SisStudent.COL_ENROLLMENT_STATUS, activeCode);
                break;

            default:
                // No additional criteria (this is the case for "All")
                break;
        }
        String queryBy = (String) getParameter(QUERY_BY_PARAM);
        String queryString = (String) getParameter(QUERY_STRING_PARAM);
        if (queryBy != null && queryString != null) {
            addUserCriteria(criteria, queryBy, queryString, SisStudent.class, X2BaseBean.COL_OID, X2BaseBean.COL_OID);
        }
        QueryByCriteria query = new QueryByCriteria(SisStudent.class, criteria);
        return getBroker().getCollectionByQuery(query);
    }
}
