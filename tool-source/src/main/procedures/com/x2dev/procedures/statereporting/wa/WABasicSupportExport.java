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
package com.x2dev.procedures.statereporting.wa;

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
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatDefinition;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.ExportFormatResult;
import com.follett.fsc.core.k12.beans.ExportFormatRow;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.exports.ExportJavaSource;
import com.follett.fsc.core.k12.tools.stateexports.ExportFormatManager;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.follett.fsc.core.k12.web.WebUtils;
import com.x2dev.sis.model.beans.Course;
import com.x2dev.sis.model.beans.Section;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.utils.DataGrid;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import java.lang.reflect.Method;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.logging.Level;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * This is a report class that performs standardized data export for
 * the state report infrastructure.
 * This class will identify a procedure that contains a state report definition.
 * It will use that definition to find all data definitions and produce
 * an export file.
 *
 * @author X2 Development Corporation
 */
public class WABasicSupportExport extends ExportJavaSource {
    private static final String FIELD_AUDIT_NON_VOC_RS = "auditNonVocRs";
    private static final String FIELD_AUDIT_FTE_MAX = "auditFteMax";
    private static final String FIELD_AUDIT_FTE_OVER = "auditFteOver";
    private static final String FIELD_AUDIT_FTE_CRS = "auditFteCrs";
    private static final String FIELD_AUDIT_VOC = "auditVocProgram";
    private static final String FIELD_AUDIT_SKILLS = "skillsProgram";
    private static final String FIELD_AUDIT_BILINGUAL = "bilingualProgram";
    private static final String FIELD_AUDIT_PERIOD = "auditPeriod";
    private static final String FIELD_AUDIT_RESIDENT = "resident";
    private static final String FIELD_STUDENT_NAME_VIEW = "nameView";
    private static final String FIELD_STUDENT_ID = "localId";
    private static final String FIELD_STUDENT_STATE_ID = "stateId";
    private static final String FIELD_STUDENT_YOG = "yog";
    private static final String FIELD_DISTRICT_ID = "districtId";
    private static final String FIELD_SCHOOL_NAME = "school.name";
    private static final String FIELD_SCHOOL_ID = "schoolId";
    private static final String FIELD_COURSE_DESCRIPTION = "courseDesc";
    private static final String FIELD_COURSE_NUMBER = "courseNumber";
    private static final String FIELD_GRADE_LEVEL = "gradeLevel";
    private static final String DATA_FIELD_K12_AND_SC = "K12+SC";
    private static final String DATA_FIELD_DISTRICT_HOME = "districtHome";
    private static final String DATA_FIELD_BILINGUAL_PROGRAM = "bilingualProgram";

    private static final String COMMENT_BEGINING = "Tool was run by ";

    /**
     *
     */
    private static final long serialVersionUID = 1L;

    private static final String INITIALIZE_KEY = "label.state.report.initialize";

    private static final String PARAM_AUDIT_PROCEDURE_ID = "auditProcedureId";
    private static final String PARAM_PROCEDURE_ID = "procedureId";
    private static final String PARAM_SAVE_RESULTS = "saveResults";
    private static final String PARAM_SAVE_AUDIT_INFO = "saveAuditInfo";

    private static final String ENTITY_METHOD_GET_AUDIT_INFO = "getAuditInfo";

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

    private static final String LOG_EMPTY_VALUES = "Audit information is empty";
    private static final String LOG_SAVE_ERROR = "Audit information cannot be saved";

    private Collection<StateReportValidationError> m_initErrors = null;
    private StateReportData m_reportData = null;
    private final HashMap<String, ExportFormatField> m_auditMapping = new HashMap<String, ExportFormatField>();

    /**
     * A parameter that indicates the export results should be saved in the CustomExportResults
     * table.
     */
    private boolean m_saveResults = false;
    private boolean m_saveAuditInfo = false;
    private String m_auditProcedureId = null;
    private int m_bilingualPosition = -1;
    private int m_districtHomePosition = -1;
    private int m_k12AndSCPosition = -1;

    /**
     * Gather data.
     *
     * @return DataGrid
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#gatherData()
     */
    @Override
    protected DataGrid gatherData() throws Exception {
        ExportFormatResult saveResult = null;
        ExportFormatRow saveRow = null;
        ExportFormatResult saveAuditResult = null;
        ExportFormatRow saveAuditRow = null;
        long rowNumber = 0;
        long rowAuditNumber = 0;
        int rowcount = 1;
        if (m_reportData != null) {
            rowcount = m_reportData.getFieldCount();
        }

        DataGrid dataGrid = new DataGrid(rowcount);
        if (m_initErrors.size() == 0) {
            initializeFieldPositions();
            if (m_saveResults) {
                saveResult = X2BaseBean.newInstance(ExportFormatResult.class,
                        getBroker().getPersistenceKey());
                saveResult.setOrganization1Oid(m_reportData.getOrganization().getOid());
                saveResult.setRunDate(System.currentTimeMillis());
                saveResult.setName(m_reportData.getExportTitle());
                saveResult.setDefinitionOid(m_reportData.getEfdOid());
                saveResult.setInputParams(getJob().getTool().getFullInputDefinition());
                saveResult.setComment(COMMENT_BEGINING + getUser().getNameView());
                getBroker().saveBeanForced(saveResult);
            }

            if (m_saveAuditInfo) {
                saveAuditResult = X2BaseBean.newInstance(ExportFormatResult.class,
                        getBroker().getPersistenceKey());
                saveAuditResult.setOrganization1Oid(m_reportData.getOrganization().getOid());
                saveAuditResult.setRunDate(System.currentTimeMillis());
                saveAuditResult.setName(m_reportData.getExportTitle() + " Audit info");
                saveAuditResult.setDefinitionOid(m_auditProcedureId);
                saveAuditResult.setInputParams(getJob().getTool().getFullInputDefinition());
                saveAuditResult.setComment(COMMENT_BEGINING + getUser().getNameView());
                getBroker().saveBeanForced(saveAuditResult);
            }

            if (m_reportData.open()) {
                try {
                    StateReportEntity entity = null;

                    Method getAuditInfoMethod = null;
                    while ((entity = m_reportData.next()) != null) {
                        StateReportValidationError err = entity.filterEntity();
                        if (err == null) {
                            entity.preProcess();
                            dataGrid.append();

                            if (m_saveResults) {
                                rowNumber++;
                                saveRow = X2BaseBean.newInstance(ExportFormatRow.class,
                                        getBroker().getPersistenceKey());
                                saveRow.setResultOid(saveResult.getOid());
                                saveRow.setDefinitionOid(m_reportData.getEfdOid());
                                saveRow.setSortOrder(new BigDecimal(rowNumber));
                                saveRow.setSourceOid(entity.getBean().getOid());
                                String rowName = entity.getEntityName();
                                if ((rowName != null) && (rowName.length() > 50)) {
                                    rowName = rowName.substring(0, 50);
                                }
                                saveRow.setDescription(rowName);
                            }

                            /*
                             * Add all fields
                             */
                            for (int pos = 0; pos < m_reportData.getFieldCount(); pos++) {
                                FieldDefinition field = m_reportData.getFieldDefinition(pos);
                                String fieldValue = entity.getFieldValue(pos);

                                if (m_saveResults) {
                                    /*
                                     * If a value has a specified maximum length, then the field
                                     * that it is
                                     * being saved into also has the specified maximum length,
                                     * So we must trim the value to that maximum length before
                                     * saving.
                                     *
                                     * Ex: Middle name is specified as 10 chars and is assigned to a
                                     * FieldA.
                                     * The value is 12 chars.
                                     * Must trim to 10 prior to saving so it will fit into the
                                     * field.
                                     *
                                     * The case that this might lose data would be in a CSV where
                                     * the length is not
                                     * absolute as it would be in a column width report. The export
                                     * might still
                                     * contain the excessive length but the saved value would not.
                                     * 
                                     * In those cases, the field would generate a validation error
                                     * anyway.
                                     *
                                     * Save happens before padding so pad values do not also get
                                     * saved.
                                     */
                                    String saveFieldValue = ExportFormatManager.doPadding(fieldValue,
                                            ExportFormatField.PaddingDirectionCode.TRUNCATE_ONLY.ordinal(),
                                            null, field.getExportLength());

                                    /*
                                     * Save field value into CustomExportRow before
                                     * padding/trimming.
                                     */
                                    String saveField = field.getSaveBeanPath();
                                    if (!StringUtils.isEmpty(saveField)) {
                                        try {
                                            WebUtils.setProperty(saveRow, saveField, saveFieldValue);
                                        } catch (RuntimeException re) {
                                            // Ignore: the value was not saved,
                                            // probably an invalid field name.
                                        }
                                    }
                                }

                                /*
                                 * If the value requires padding, pad it and trim it to field max
                                 * length.
                                 */
                                fieldValue = ExportFormatManager.doPadding(fieldValue,
                                        (field.getResizeMode() == null
                                                ? ExportFormatField.PaddingDirectionCode.NONE.ordinal()
                                                : field.getResizeMode().ordinal()),
                                        field.getPaddingChar(),
                                        field.getExportLength());

                                // Set the final value.
                                dataGrid.set(field.getFieldId(), fieldValue);

                            }

                            entity.postProcess();

                            if (m_saveResults) {
                                getBroker().saveBean(saveRow);
                            }

                            if (m_saveAuditInfo) {

                                String rowName = entity.getEntityName();
                                if ((rowName != null) && (rowName.length() > 50)) {
                                    rowName = rowName.substring(0, 50);
                                }

                                Student student = (Student) entity.getBean();
                                String grade = student.getGradeLevel();
                                grade = StringUtils.padLeft(grade, 2, '0');

                                String districtHome = entity.getFieldValue(m_districtHomePosition);
                                Boolean resident =
                                        ((districtHome != null) && (getOrganization().getName().equals(districtHome)))
                                                ? Boolean.TRUE
                                                : Boolean.FALSE;

                                Boolean isBilingual = Boolean.valueOf(BooleanAsStringConverter.TRUE
                                        .equals(entity.getFieldValue(m_bilingualPosition)));
                                if (getAuditInfoMethod == null) {
                                    getAuditInfoMethod = entity.getClass().getMethod(ENTITY_METHOD_GET_AUDIT_INFO);
                                }

                                String k12AndSC = entity.getFieldValue(m_k12AndSCPosition);
                                HashMap<Section, HashMap<String, Object>> auditInfo =
                                        (HashMap<Section, HashMap<String, Object>>) getAuditInfoMethod.invoke(entity);
                                if (auditInfo.isEmpty()) {
                                    AppGlobals.getLog().log(Level.WARNING,
                                            LOG_EMPTY_VALUES + " for " + entity.getEntityName());
                                }

                                for (HashMap<String, Object> audit : auditInfo.values()) {
                                    rowAuditNumber++;
                                    saveAuditRow = X2BaseBean.newInstance(ExportFormatRow.class,
                                            getBroker().getPersistenceKey());
                                    saveAuditRow.setResultOid(saveAuditResult.getOid());
                                    saveAuditRow.setDefinitionOid(m_auditProcedureId);
                                    saveAuditRow.setSortOrder(new BigDecimal(rowAuditNumber));
                                    saveAuditRow.setSourceOid(entity.getBean().getOid());
                                    saveAuditRow.setDescription(rowName);

                                    addValue(saveAuditRow, student.getLocalId(), FIELD_STUDENT_ID);
                                    addValue(saveAuditRow, student.getNameView(), FIELD_STUDENT_NAME_VIEW);
                                    addValue(saveAuditRow, student.getStateId(), FIELD_STUDENT_STATE_ID);
                                    addValue(saveAuditRow, Integer.toString(student.getYog()), FIELD_STUDENT_YOG);

                                    Course course = (Course) audit.get(AUDIT_COURSE);
                                    addValue(saveAuditRow, course.getNumber(), FIELD_COURSE_NUMBER);
                                    addValue(saveAuditRow, course.getDescription(), FIELD_COURSE_DESCRIPTION);

                                    SisSchool school = (SisSchool) audit.get(AUDIT_SCHOOL);
                                    addValue(saveAuditRow, school.getSchoolId(), FIELD_SCHOOL_ID);
                                    addValue(saveAuditRow, school.getName(), FIELD_SCHOOL_NAME);
                                    addValue(saveAuditRow, audit.get(AUDIT_DISTRICT_ID), FIELD_DISTRICT_ID);
                                    addValue(saveAuditRow, grade, FIELD_GRADE_LEVEL);
                                    addValue(saveAuditRow, audit.get(AUDIT_PERIOD), FIELD_AUDIT_PERIOD);
                                    addValue(saveAuditRow, audit.get(AUDIT_FTE_CRS), FIELD_AUDIT_FTE_CRS);
                                    addValue(saveAuditRow, audit.get(AUDIT_FTE_OVER), FIELD_AUDIT_FTE_OVER);
                                    addValue(saveAuditRow, audit.get(AUDIT_FTE_MAX), FIELD_AUDIT_FTE_MAX);
                                    addValue(saveAuditRow, audit.get(AUDIT_NON_VOC_RS), FIELD_AUDIT_NON_VOC_RS);
                                    addValue(saveAuditRow, isBilingual, FIELD_AUDIT_BILINGUAL);
                                    addValue(saveAuditRow, audit.get(AUDIT_SKILLS_CENTER), FIELD_AUDIT_SKILLS);
                                    addValue(saveAuditRow, audit.get(AUDIT_VOC), FIELD_AUDIT_VOC);
                                    addValue(saveAuditRow, resident, FIELD_AUDIT_RESIDENT);
                                    addValue(saveAuditRow, k12AndSC, DATA_FIELD_K12_AND_SC);

                                    try {
                                        getBroker().saveBean(saveAuditRow);
                                    } catch (Exception e) {
                                        AppGlobals.getLog().log(Level.WARNING,
                                                LOG_SAVE_ERROR + " for " + entity.getEntityName());
                                    }
                                }
                            }

                        } else {
                            m_initErrors.add(err);
                        }
                    }
                } finally {
                    m_reportData.close();
                }
            }

            // If the report has a heading or trailer, save it to the parent
            // record.
            if (m_saveResults &&
                    (!StringUtils.isEmpty(m_reportData.getHeading())
                            || !StringUtils.isEmpty(m_reportData.getTrailer()))) {
                saveResult.setHeading(m_reportData.getHeading());
                saveResult.setTrailer(m_reportData.getTrailer());
                getBroker().saveBeanForced(saveResult);
            }

        } else {
            for (StateReportValidationError error : m_initErrors) {
                dataGrid.append();
                dataGrid.set("Entity name", error.getEntityName());
                dataGrid.set("Error ID", error.getErrorId());
                dataGrid.set("Field Name", error.getFieldName());
                dataGrid.set("Error message", error.getErrorMessage());
            }
        }
        dataGrid.beforeTop();
        return dataGrid;
    }

    /**
     * Returns a list of export field names.
     *
     * @return List
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getColumnNames()
     */
    @Override
    protected List getColumnNames() {
        List fields = null;
        if (m_reportData != null) {
            fields = new ArrayList(m_reportData.getFieldCount());
            for (int pos = 0; pos < m_reportData.getFieldCount(); pos++) {
                fields.add(m_reportData.getFieldDefinition(pos) == null ? ""
                        : m_reportData.getFieldDefinition(pos).getFieldId());
            }

        }
        return fields;
    }

    /**
     * Gets the column user names.
     *
     * @return List
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getColumnUserNames()
     */
    @Override
    protected List getColumnUserNames() {
        return getColumnNames();
    }

    /**
     * Gets the comment.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getComment()
     */
    @Override
    protected String getComment() {
        StringBuilder comment = new StringBuilder();
        String lastName = "";
        if ((m_initErrors != null) && (m_initErrors.size() > 0)) {
            for (StateReportValidationError err : m_initErrors) {
                String thisName = err.getEntityName();
                if (!lastName.equals(thisName)) {
                    comment.append(err.getEntityName());
                    comment.append("\n");
                    lastName = thisName;
                }
                comment.append("    ");
                comment.append(err.getFieldName());
                comment.append("   ");
                comment.append(err.getErrorId());
                comment.append("   ");
                comment.append(err.getErrorMessage());
                comment.append("\n");
            }
        }
        return comment.toString();
    }

    /**
     * Gets the header.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getHeader()
     */
    @Override
    protected String getHeader() {
        String header = null;
        if (m_reportData != null) {
            header = m_reportData.getHeading();
        }
        return header;
    }

    /**
     * Gets the trailer.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getTrailer()
     */
    @Override
    protected String getTrailer() {
        String trailer = null;
        if (m_reportData != null) {
            trailer = m_reportData.getTrailer();
        }
        return trailer;
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        // Set exports to use MS/windows end of line character for all exports.
        setLineSeparator(FORMAT_EOL_WINDOWS);

        // Determine if the results should be saved in the StateReport results
        // tables.
        Boolean saveResults = (Boolean) getParameter(PARAM_SAVE_RESULTS);
        if (saveResults != null) {
            m_saveResults = saveResults.booleanValue();
        }
        Boolean saveAuditInfo = (Boolean) getParameter(PARAM_SAVE_AUDIT_INFO);
        if (saveAuditInfo != null) {
            m_saveAuditInfo = saveAuditInfo.booleanValue();
        }

        String procedureId = (String) getParameter(PARAM_PROCEDURE_ID);
        m_auditProcedureId = (String) getParameter(PARAM_AUDIT_PROCEDURE_ID);
        if (saveAuditInfo.booleanValue() && (m_auditProcedureId != null)) {
            loadAuditMapping();
        }
        m_initErrors = new ArrayList<StateReportValidationError>();

        // Lookup State report source data procedure
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

                // Set export parameters from the report data object.
                setEscapeCharacter(m_reportData.getEscapeCharacter());
                setIncludeHeaderRow(m_reportData.getIncludeHeaderRow());
                setUseEscapes(m_reportData.getUseEscapes());
                setUseValueDelimiters(m_reportData.getUseValueDelimiters());
                setUseValueWrappers(m_reportData.getUseValueWrappers());
                setValueDelimiter(m_reportData.getValueDelimiter());
                setValueWrapper(m_reportData.getValueWrapper());
            } catch (X2BaseException x2be) {
                String init_msg = LocalizationCache.getMessages(getBroker().getPersistenceKey(),
                        getLocale()).getMessage(INITIALIZE_KEY);
                // String init_msg = "Initialize";
                m_initErrors.add(new StateReportValidationError(init_msg, init_msg, init_msg, x2be.getMessage()));

                throw x2be;
            }

            m_initErrors.addAll(m_reportData.getSetupErrors());
        }
    }

    /**
     * Add value in row. Position defines by previously loaded m_auditMapping
     *
     * @param row ExportFormatRow
     * @param value Object
     * @param fieldName String
     */
    private void addValue(ExportFormatRow row, Object value, String fieldName) {
        ExportFormatField field = m_auditMapping.get(fieldName);
        if (value instanceof Boolean) {
            if (((Boolean) value).booleanValue()) {
                value = BooleanAsStringConverter.TRUE;
            } else {
                value = BooleanAsStringConverter.FALSE;
            }
        }
        if (value != null) {
            row.setFieldValueByBeanPath(field.getFieldPath(), value.toString());
        }
    }

    /**
     * Need for case then audit data saves in ExportFormatRow's.
     * Audit Report will need the way to now in what field what value store.
     */
    private void loadAuditMapping() {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ExportFormatField.REL_DEFINITION + PATH_DELIMITER + ExportFormatDefinition.COL_PROCEDURE_ID,
                m_auditProcedureId);
        QueryByCriteria query = new QueryByCriteria(ExportFormatField.class, criteria);
        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                ExportFormatField field = (ExportFormatField) iterator.next();
                m_auditProcedureId = field.getDefinitionOid();
                m_auditMapping.put(field.getName(), field);
            }
        } finally {
            iterator.close();
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
            }
        }
        if ((m_districtHomePosition < 0) ||
                (m_bilingualPosition < 0) ||
                (m_k12AndSCPosition < 0)) {
            return false;
        }
        return true;
    }
}
