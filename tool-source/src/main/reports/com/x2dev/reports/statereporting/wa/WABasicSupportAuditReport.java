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
package com.x2dev.reports.statereporting.wa;

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

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatDefinition;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.ExportFormatResult;
import com.follett.fsc.core.k12.beans.ExportFormatRow;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.Course;
import com.x2dev.sis.model.beans.Section;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.text.DateFormat;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class WABasicSupportAuditReport.
 */
public class WABasicSupportAuditReport extends ReportJavaSourceNet {
    private static final String ALIAS_EXCLUDE_SCHOOL = "DOE EXCLUDE SKL";

    private static final String AUDIT_COURSE = "course";
    private static final String AUDIT_DISTRICT_ID = "districtId";
    private static final String AUDIT_FTE_CRS = "fteCrs";
    private static final String AUDIT_FTE_MAX = "fteMax";
    private static final String AUDIT_FTE_OVER = "fteOver";
    private static final String AUDIT_NON_VOC_RS = "nonVocRs";
    private static final String AUDIT_PERIOD = "period";
    private static final String AUDIT_SCHOOL = "school";
    private static final String AUDIT_SKILLS_CENTER = "skillsCenter";
    private static final String AUDIT_VOC = "vocational";

    private static final String DATA_FIELD_BILINGUAL_PROGRAM = "bilingualProgram";
    private static final String DATA_FIELD_COURSE_DESC = "courseDesc";
    private static final String DATA_FIELD_COURSE_NUMBER = "courseNumber";
    private static final String DATA_FIELD_DISTRICT_HOME = "districtHome";
    private static final String DATA_FIELD_DISTRICT_ID = "districtId";
    private static final String DATA_FIELD_FTE_CRS = "auditFteCrs";
    private static final String DATA_FIELD_FTE_OVER = "auditFteOver";
    private static final String DATA_FIELD_FTE_MAX = "auditFteMax";
    private static final String DATA_FIELD_GRADE_LEVEL = "gradeLevel";
    private static final String DATA_FIELD_K12_AND_SC = "K12+SC";
    private static final String DATA_FIELD_NON_VOC_RS = "auditNonVocRs";
    private static final String DATA_FIELD_PERIOD = "auditPeriod";
    private static final String DATA_FIELD_SKILLS_CENTER = "skillsProgram";
    private static final String DATA_FIELD_STUDENT_ID = "localId";
    private static final String DATA_FIELD_STUDENT_NAME = "nameView";
    private static final String DATA_FIELD_STUDENT_YOG = "yog";
    private static final String DATA_FIELD_RESIDENT = "resident";
    private static final String DATA_FIELD_SCHOOL_ID = "schoolId";
    private static final String DATA_FIELD_SCHOOL_NAME = "school.name";
    private static final String DATA_FIELD_VOCATIONAL_PROGRAM = "auditVocProgram";
    private static final String FORMAT_PATTERN = "##0.00";
    private static final String GROUP_BY_DISTRICT_RESIDENCE = "districtResidence";
    private static final String GROUP_BY_SCHOOL = "school";
    private static final String GROUP_BY_SKL_AND_DISTRICT = "schoolAndDistrictResidence";


    private static final String ENTITY_METHOD_GET_AUDIT_INFO = "getAuditInfo";

    private static final String PARAM_AUDIT_PROCEDURE_ID = "auditProcedureId";
    private static final String PARAM_CURRENT_DATE = "currentDate";
    private static final String PARAM_DATE_TIME_FORMATTER = "dateTimeFormat";
    private static final String PARAM_DISTRICT_ID = "districtId";
    private static final String PARAM_DISTRICT_NAME = "districtName";
    private static final String PARAM_EXCLUDE_SKL = "excludeSchool";
    private static final String PARAM_EXPORT_RESULT_OID = "efrOid";
    private static final String PARAM_FILE_DATE = "fileDate";
    private static final String PARAM_FROM_EXPORT_RESULT = "fromExportResult";
    private static final String PARAM_FTE_RANGE = "fteRange";
    private static final String PARAM_PROCEDURE_ID = "procedureId";
    private static final String PARAM_REPORT_DATE = "reportDate";

    /*
     * Constants for query criteria parameters from user input template.
     */
    private static final String PARAM_DECIMAL_FORMATTER = "decimalFormat";
    private static final String PARAM_GROUP_BY_DISTR = "District";
    private static final String PARAM_GROUP_BY_FIELDS = "groupBy";
    private static final String PARAM_GROUP_BY_SKL = "School";
    private static final String PARAM_QUERY_BY_FIELD = "queryBy";
    private static final String PARAM_QUERY_BY_CRITERIA = "queryString";
    private static final String PARAM_SCHOOL_NAME = "school.name";
    private static final String PARAM_SORT_BY_FIELDS = "sortBy";

    private final HashMap<String, ExportFormatField> m_auditMapping = new HashMap<String, ExportFormatField>();
    private Map<String, SisSchool> m_excludedSchools;
    private String m_fteRange = null;
    private Collection<StateReportValidationError> m_initErrors = null;
    private StateReportData m_reportData = null;

    // Entity field positions
    private int m_bilingualPosition = -1;
    private int m_districtHomePosition = -1;
    private int m_k12AndSCPosition = -1;
    private int m_sklNamePosition = -1;


    /**
     * Check if we need to include school in report.
     *
     * @param schoolName String
     * @return true, if successful
     */
    public boolean includeSchool(String schoolName) {
        return (m_excludedSchools == null) || !m_excludedSchools.containsKey(schoolName);
    }

    /**
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        if (getParameter(PARAM_EXCLUDE_SKL) != null && ((Boolean) getParameter(PARAM_EXCLUDE_SKL)).booleanValue()) {
            loadSchoolExcludeMap();
        }

        ReportDataGrid dataGrid = new ReportDataGrid();
        m_initErrors = new ArrayList<StateReportValidationError>();
        Organization currentOrg = ((Organization) getParameter(ORGANIZATION_KEY));
        Boolean useExportResult = (Boolean) getParameter(PARAM_FROM_EXPORT_RESULT);

        String exportResultOid = (String) getParameter(PARAM_EXPORT_RESULT_OID);

        if (StringUtils.isEmpty(exportResultOid)) {
            exportResultOid = getLastExportResultOid();
        }

        Date currentDate = new Date(System.currentTimeMillis());

        if ((useExportResult != null) && useExportResult.booleanValue() && (exportResultOid != null)) {
            ExportFormatResult result =
                    (ExportFormatResult) getBroker().getBeanByOid(ExportFormatResult.class, exportResultOid);
            addParameter(PARAM_FILE_DATE, new Date(result.getLastModifiedTime()));

            String auditProcedureId = (String) getParameter(PARAM_AUDIT_PROCEDURE_ID);
            loadAuditMapping(auditProcedureId);
            dataGrid = getDataFromExportResults(exportResultOid);
        } else {
            dataGrid = getDataFromLiveDB();
            addParameter(PARAM_FILE_DATE, currentDate);
        }
        String sortBy = (String) getParameter(PARAM_SORT_BY_FIELDS);
        addParameter(PARAM_SORT_BY_FIELDS, sortBy);
        addParameter(PARAM_REPORT_DATE, new PlainDate());
        addParameter(PARAM_DISTRICT_ID, currentOrg.getId());
        addParameter(PARAM_DISTRICT_NAME, currentOrg.getName());
        addParameter(PARAM_DATE_TIME_FORMATTER, DateFormat.getDateTimeInstance());
        addParameter(PARAM_CURRENT_DATE, currentDate);

        DecimalFormat decimalFormatter = new DecimalFormat(FORMAT_PATTERN);
        addParameter(PARAM_DECIMAL_FORMATTER, decimalFormatter);

        String groupBy = (String) getParameter(PARAM_GROUP_BY_FIELDS);
        if (groupBy != null) {
            switch (groupBy) {
                case GROUP_BY_DISTRICT_RESIDENCE:
                    dataGrid.sort(DATA_FIELD_DISTRICT_ID, true);
                    addParameter(PARAM_GROUP_BY_FIELDS, PARAM_GROUP_BY_DISTR);
                    break;

                case GROUP_BY_SCHOOL:
                    dataGrid.sort(DATA_FIELD_SCHOOL_ID, true);
                    addParameter(PARAM_GROUP_BY_FIELDS, PARAM_GROUP_BY_SKL);
                    break;

                case GROUP_BY_SKL_AND_DISTRICT:
                    dataGrid.sort(DATA_FIELD_DISTRICT_ID, true);
                    dataGrid.sort(DATA_FIELD_SCHOOL_ID, true);
                    addParameter(PARAM_GROUP_BY_FIELDS, PARAM_GROUP_BY_SKL + " / " + PARAM_GROUP_BY_DISTR);
                    break;

                default:
                    break;
            }
        }


        dataGrid.beforeTop();
        return dataGrid;
    }

    /**
     * Return false if FTE(K12 + SC) does not fall within FTE range.
     *
     * @param k12AndSC String
     * @return true, if successful
     */
    private boolean checkFteRange(String k12AndSC) {
        if (m_fteRange == null) {
            m_fteRange = (String) getParameter(PARAM_FTE_RANGE);
        }
        if (!"all".equals(m_fteRange)) {
            Double k12AndSCNumber = Double.valueOf(k12AndSC);
            if ("0".equals(m_fteRange) && k12AndSCNumber.doubleValue() > 1.0 ||
                    "1".equals(m_fteRange) && k12AndSCNumber.doubleValue() < 1.0) {
                return false;
            }
        }
        return true;
    }

    /**
     * Load ExportFormatRows for passed exportResultOid.
     * Populate Grid with values stored in rows
     *
     * @param exportResultOid String
     * @return Report data grid
     */
    private ReportDataGrid getDataFromExportResults(String exportResultOid) {
        ReportDataGrid dataGrid = new ReportDataGrid(10, 20);

        String queryBy = (String) getParameter(PARAM_QUERY_BY_FIELD + Integer.toString(1));
        String queryString = (String) getParameter(PARAM_QUERY_BY_CRITERIA + Integer.toString(1));

        X2Criteria resultRowCriteria = new X2Criteria();
        resultRowCriteria.addEqualTo(ExportFormatRow.COL_RESULT_OID, exportResultOid);
        if (!"##all".equals(queryBy)) {
            ExportFormatField field = m_auditMapping.get(queryBy);
            if (field != null) {
                resultRowCriteria.addEqualTo(field.getFieldPath(), queryString);
            }
        }

        if (isSchoolContext()) {
            ExportFormatField field = m_auditMapping.get(PARAM_SCHOOL_NAME);
            if (field != null) {
                resultRowCriteria.addEqualTo(field.getFieldPath(), getSchool().getName());
            }
        }

        String sortBy = (String) getParameter(PARAM_SORT_BY_FIELDS);
        List<String> sortParameters = StringUtils.convertDelimitedStringToList(sortBy, ',', true);


        QueryByCriteria query = new QueryByCriteria(ExportFormatRow.class, resultRowCriteria);
        for (String sortParameter : sortParameters) {
            ExportFormatField field = m_auditMapping.get(sortParameter);
            if (field != null) {
                query.addOrderByAscending(field.getFieldPath());
            }
        }

        QueryIterator i_rows = getBroker().getIteratorByQuery(query);
        try {
            ExportFormatField k12AndScField = m_auditMapping.get(DATA_FIELD_K12_AND_SC);
            ExportFormatField sklNameField = m_auditMapping.get(DATA_FIELD_SCHOOL_NAME);

            while (i_rows.hasNext()) {
                ExportFormatRow row = (ExportFormatRow) i_rows.next();

                // Drop student with fte does not fall within the range
                String k12AndSC = (String) row.getFieldValueByBeanPath(k12AndScField.getFieldPath());
                String sklName = (String) row.getFieldValueByBeanPath(sklNameField.getFieldPath());
                if (checkFteRange(k12AndSC) && includeSchool(sklName)) {
                    dataGrid.append();
                    for (Entry<String, ExportFormatField> entry : m_auditMapping.entrySet()) {
                        String dataFieldName = entry.getKey();
                        ExportFormatField field = entry.getValue();
                        Object value = row.getFieldValueByBeanPath(field.getFieldPath());
                        if (value != null) {
                            dataGrid.set(dataFieldName, value.toString());
                        }
                    }
                }
            }
        } finally {
            i_rows.close();
        }

        return dataGrid;
    }

    /**
     * Gets the data from live DB.
     *
     * @return Report data grid
     * @throws X2BaseException exception
     * @throws NoSuchMethodException exception
     * @throws IllegalAccessException exception
     * @throws InvocationTargetException exception
     */
    private ReportDataGrid getDataFromLiveDB() throws X2BaseException,
            NoSuchMethodException, IllegalAccessException, InvocationTargetException {
        // Lookup State report source data procedure
        String procedureId = (String) getParameter(PARAM_PROCEDURE_ID);
        m_reportData = StateReportData.getReportDataFromProcedure(procedureId, getBroker(), m_initErrors);

        if ((m_reportData != null) && (m_initErrors.size() == 0)) {
            try {
                // Initialize the report data object.
                m_reportData.setBroker(getBroker());
                m_reportData.setCurrentContext(getCurrentContext());
                m_reportData.setOrganization(getOrganization());
                m_reportData.setPrivilegeSet(getPrivilegeSet());
                m_reportData.setSchoolContext(isSchoolContext());
                m_reportData.setSchool(getSchool());
                m_reportData.setParameters(getParameters());
                m_reportData.setUser(getUser());
                m_reportData.initializeExport();
            } catch (X2BaseException x2be) {
                String init_msg = "Failure initializing data structure in WABasicSupport";
                m_initErrors.add(new StateReportValidationError(init_msg, init_msg, init_msg, x2be.getMessage()));

                throw x2be;
            }

            m_initErrors.addAll(m_reportData.getSetupErrors());
        }
        ReportDataGrid dataGrid = new ReportDataGrid(10, 20);

        if ((m_reportData != null) && m_reportData.open() && initializeFieldPositions()) {
            try {
                StateReportEntity entity = null;
                // Here need to use reflection because simple
                // downcasting entity class to WABasicSupportEntity
                // don't work on customer side.
                Method getAuditInfoMethod = null;
                while ((entity = m_reportData.next()) != null) {
                    entity.preProcess();

                    // Drop student with fte does not fall within the range
                    String k12AndSC = entity.getFieldValue(m_k12AndSCPosition);
                    String sklName = entity.getFieldValue(m_k12AndSCPosition);
                    if (checkFteRange(k12AndSC) && includeSchool(sklName)) {
                        Student student = (Student) entity.getBean();
                        String grade = student.getGradeLevel();
                        grade = StringUtils.padLeft(grade, 2, '0');

                        String districtHome = entity.getFieldValue(m_districtHomePosition);
                        Boolean resident =
                                ((districtHome != null) && (getOrganization().getName().equals(districtHome)))
                                        ? Boolean.TRUE
                                        : Boolean.FALSE;

                        Boolean isBilingual = Boolean.valueOf(
                                BooleanAsStringConverter.TRUE.equals(entity.getFieldValue(m_bilingualPosition)));
                        if (getAuditInfoMethod == null) {
                            getAuditInfoMethod = entity.getClass().getMethod(ENTITY_METHOD_GET_AUDIT_INFO);
                        }
                        HashMap<Section, HashMap<String, Object>> auditInfo =
                                (HashMap<Section, HashMap<String, Object>>) getAuditInfoMethod.invoke(entity);
                        for (HashMap<String, Object> audit : auditInfo.values()) {
                            dataGrid.append();
                            dataGrid.set(DATA_FIELD_STUDENT_ID, student.getLocalId());
                            dataGrid.set(DATA_FIELD_STUDENT_NAME, student.getNameView());
                            dataGrid.set(DATA_FIELD_STUDENT_YOG, Integer.toString(student.getYog()));
                            dataGrid.set(DATA_FIELD_COURSE_NUMBER, ((Course) audit.get(AUDIT_COURSE)).getNumber());
                            dataGrid.set(DATA_FIELD_COURSE_DESC, ((Course) audit.get(AUDIT_COURSE)).getDescription());
                            dataGrid.set(DATA_FIELD_SCHOOL_ID, ((SisSchool) audit.get(AUDIT_SCHOOL)).getSchoolId());
                            dataGrid.set(DATA_FIELD_SCHOOL_NAME, ((SisSchool) audit.get(AUDIT_SCHOOL)).getName());
                            dataGrid.set(DATA_FIELD_DISTRICT_ID, valueToString(audit.get(AUDIT_DISTRICT_ID)));
                            dataGrid.set(DATA_FIELD_GRADE_LEVEL, valueToString(grade));
                            dataGrid.set(DATA_FIELD_K12_AND_SC, k12AndSC);
                            dataGrid.set(DATA_FIELD_PERIOD, valueToString(audit.get(AUDIT_PERIOD)));
                            dataGrid.set(DATA_FIELD_FTE_CRS, valueToString(audit.get(AUDIT_FTE_CRS)));
                            dataGrid.set(DATA_FIELD_FTE_OVER, valueToString(audit.get(AUDIT_FTE_OVER)));
                            dataGrid.set(DATA_FIELD_FTE_MAX, valueToString(audit.get(AUDIT_FTE_MAX)));
                            dataGrid.set(DATA_FIELD_NON_VOC_RS, valueToString(audit.get(AUDIT_NON_VOC_RS)));
                            dataGrid.set(DATA_FIELD_BILINGUAL_PROGRAM, valueToString(isBilingual));
                            dataGrid.set(DATA_FIELD_SKILLS_CENTER, valueToString(audit.get(AUDIT_SKILLS_CENTER)));
                            dataGrid.set(DATA_FIELD_VOCATIONAL_PROGRAM, valueToString(audit.get(AUDIT_VOC)));
                            dataGrid.set(DATA_FIELD_RESIDENT, valueToString(resident));
                        }
                        entity.postProcess();
                    }
                }
            } finally {
                m_reportData.close();
            }
        }
        return dataGrid;
    }

    /**
     * If last export format result was not selected looking for the last from DB.
     * 
     * @return lastEfrOid
     */
    private String getLastExportResultOid() {
        String lastEfrOid = null;
        String procedureId = (String) getParameter(PARAM_AUDIT_PROCEDURE_ID);
        if (!StringUtils.isEmpty(procedureId)) {
            X2Criteria efrCriteria = new X2Criteria();
            efrCriteria.addEqualTo(
                    ExportFormatResult.REL_DEFINITION + PATH_DELIMITER + ExportFormatDefinition.COL_PROCEDURE_ID,
                    procedureId);

            QueryByCriteria efrQuery = new QueryByCriteria(ExportFormatResult.class, efrCriteria);
            efrQuery.addOrderByDescending(ExportFormatResult.COL_RUN_DATE);

            Collection<ExportFormatResult> efrs = getBroker().getCollectionByQuery(efrQuery);

            if (!efrs.isEmpty()) {
                lastEfrOid = ((ExportFormatResult) efrs.toArray()[0]).getOid();
            }
        }
        return lastEfrOid;
    }

    /**
     * Need for case then audit data was saved in ExportFormatRow's.
     * Audit Report need to now in what field what value stores.
     *
     * @param auditProcedureId String
     */
    private void loadAuditMapping(String auditProcedureId) {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ExportFormatField.REL_DEFINITION + PATH_DELIMITER + ExportFormatDefinition.COL_PROCEDURE_ID,
                auditProcedureId);
        QueryByCriteria query = new QueryByCriteria(ExportFormatField.class, criteria);
        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                ExportFormatField field = (ExportFormatField) iterator.next();
                m_auditMapping.put(field.getName(), field);
            }
        } finally {
            iterator.close();
        }

    }

    /**
     * Loads schools with excluded indicator set to true.
     */
    private void loadSchoolExcludeMap() {
        String schoolExclude = null;
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(ALIAS_EXCLUDE_SCHOOL);
        if (field != null) {
            schoolExclude = field.getJavaName();
        }

        if (!StringUtils.isEmpty(schoolExclude)) {
            X2Criteria schoolCriteria = new X2Criteria();
            schoolCriteria.addEqualTo(schoolExclude, BooleanAsStringConverter.TRUE);
            BeanQuery query = new BeanQuery(School.class, schoolCriteria);
            m_excludedSchools = getBroker().getGroupedCollectionByQuery(query, SisSchool.COL_NAME, 128);

        }
    }

    /**
     * Initializes the field position members for the data source.
     *
     * @return true, if successful
     */
    private boolean initializeFieldPositions() {
        for (int pos = 0; pos < m_reportData.getFieldCount(); pos++) {
            FieldDefinition field = m_reportData.getFieldDefinition(pos);
            String fieldName = field.getFieldId();

            if (DATA_FIELD_DISTRICT_HOME.equals(fieldName)) {
                m_districtHomePosition = pos;
            } else if (DATA_FIELD_BILINGUAL_PROGRAM.equals(fieldName)) {
                m_bilingualPosition = pos;
            } else if (DATA_FIELD_K12_AND_SC.equals(fieldName)) {
                m_k12AndSCPosition = pos;
            } else if (DATA_FIELD_SCHOOL_NAME.equals(fieldName)) {
                m_sklNamePosition = pos;
            }

        }
        if ((m_districtHomePosition < 0) ||
                (m_bilingualPosition < 0) ||
                (m_k12AndSCPosition < 0) ||
                (m_sklNamePosition < 0)) {
            return false;
        }
        return true;
    }

    /**
     * Helper for checking obj before call .toString()
     *
     * @param obj Object
     * @return String
     */
    private String valueToString(Object obj) {
        String value = null;
        if (obj != null) {
            value = obj.toString();
        }
        return value;
    }
}
