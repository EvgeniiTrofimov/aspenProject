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

package com.x2dev.reports.bc;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.x2dev.sis.model.beans.HealthLog;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.DataGrid;
import com.x2dev.utils.StringUtils;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import org.apache.commons.lang.exception.ExceptionUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Class to export accident information (health log) for BC as part of their GDE.
 *
 * @author Follett Software Company
 */
public class AccidentInformationExport extends GdeExportJavaSource {
    /*
     * Input parameters
     */
    private static final String PARAM_ACCIDENT_TYPE = "accidentType";

    // Grid fields
    private static final String FIELD_STD_LOCAL_ID = "Student Number";
    private static final String FIELD_STD_LAST_NAME = "Student Legal Last Name";
    private static final String FIELD_STD_FIRST_NAME = "Student Legal First Name";
    private static final String FIELD_STD_GRADE = "Grade";
    private static final String FIELD_STD_HOMEROOM_CODE = "Homeroom";
    private static final String FIELD_INCIDENT_DATE = "Date of Incident";
    private static final String FIELD_INJURY_DESCRIPTION = "Injury description";
    private static final String FIELD_INCIDENT_TYPE_DESCRIPTION = "Incident Type Description";
    private static final String FIELD_INCIDENT_CAUSE_DESCRIPTION = "Cause Description";
    private static final String FIELD_INCIDENT_LOCATION_DESCRIPTION = "Location Description";

    // Health Log aliases
    private static final String ALIAS_INJURY_DESCRIPTION = "hlg-accident-description";
    private static final String ALIAS_INCIDENT_TYPE_DESCRIPTION = "hlg-accident-type";
    private static final String ALIAS_INCIDENT_CAUSE_DESCRIPTION = "hlg-accident-cause";

    // Other constants
    private static final int FIELD_COUNT = 10;

    private List<String> m_columns;

    private Map<String, ReferenceCode> m_accidentCodesMap;
    private Map<String, ReferenceCode> m_locationCodesMap;

    /**
     * Gather data.
     *
     * @return DataGrid
     * @throws Exception exception
     * @see com.x2dev.sis.tools.exports.ExportJavaSource#gatherData()
     */
    @Override
    protected DataGrid gatherData() throws Exception {
        SimpleDateFormat formatter = new SimpleDateFormat("MM/dd/yyyy");
        DataGrid grid = new DataGrid() {
            private static final long serialVersionUID = 1L;

            @Override
            public void set(String column, Object value) {
                super.set(column, wrap((String) value));
            }

        };

        Criteria criteria = buildCriteria();
        QueryByCriteria query = new QueryByCriteria(HealthLog.class, criteria);
        QueryIterator logs = getBroker().getIteratorByQuery(query);
        try {
            while (logs.hasNext()) {
                HealthLog log = (HealthLog) logs.next();
                boolean deleteRow = false;
                try {
                    grid.append();
                    deleteRow = true;

                    SisStudent student = log.getPerson().getStudent(getBroker());

                    String gradeLevel = student.getGradeLevel(getCurrentContext().getOid(), getBroker());
                    String subGradeLevel = (String) student.getFieldValueByAlias("std-sub-grade");

                    // Fill grid data list with export information
                    grid.set(FIELD_STD_LOCAL_ID, student.getLocalId());
                    grid.set(FIELD_STD_LAST_NAME, log.getPerson().getLastName());
                    grid.set(FIELD_STD_FIRST_NAME, log.getPerson().getFirstName());
                    grid.set(FIELD_STD_GRADE, StringUtils.isEmpty(subGradeLevel) ? gradeLevel : subGradeLevel);
                    grid.set(FIELD_STD_HOMEROOM_CODE, student.getHomeroom());
                    grid.set(FIELD_INCIDENT_DATE, formatter.format(log.getDate()));
                    grid.set(FIELD_INJURY_DESCRIPTION, log.getFieldValueByAlias(ALIAS_INJURY_DESCRIPTION));
                    grid.set(FIELD_INCIDENT_CAUSE_DESCRIPTION,
                            log.getFieldValueByAlias(ALIAS_INCIDENT_CAUSE_DESCRIPTION));

                    /*
                     * Lookup description of incident type
                     */
                    String incidentTypeDesc = (String) log.getFieldValueByAlias(ALIAS_INCIDENT_TYPE_DESCRIPTION);
                    if (m_accidentCodesMap != null && m_accidentCodesMap.containsKey(incidentTypeDesc)) {
                        ReferenceCode code = m_accidentCodesMap.get(incidentTypeDesc);
                        incidentTypeDesc = code.getDescription();
                    }
                    grid.set(FIELD_INCIDENT_TYPE_DESCRIPTION, incidentTypeDesc);

                    /*
                     * Lookup description of location code
                     */
                    String location = log.getLocationCode();
                    if (m_locationCodesMap != null && m_locationCodesMap.containsKey(location)) {
                        ReferenceCode code = m_locationCodesMap.get(location);
                        location = code.getDescription();
                    }
                    grid.set(FIELD_INCIDENT_LOCATION_DESCRIPTION, location);
                } catch (NullPointerException npe) {
                    StringBuilder strBldr = new StringBuilder();
                    strBldr.append("Unable to export ");
                    strBldr.append(query.getClass().getName());
                    strBldr.append(" with OID: [");
                    strBldr.append(log.getOid());
                    SisStudent student = log.getPerson().getStudent();
                    if (student != null) {
                        strBldr.append("] for the Student with Local ID: [");
                        strBldr.append(student.getLocalId());
                        strBldr.append("].");
                    } else {
                        strBldr.append("] as it has no related Student.");
                    }

                    // deleteRow is true if an incomplete row has been added to the grid from
                    // grid.append()
                    if (!deleteRow) {
                        strBldr.append("Null encountered before adding to export.");
                    } else {
                        strBldr.append("Null encountered when setting Columns.");
                        grid.deleteRow(); // Delete the incomplete row that was appended to the
                                          // grid.
                    }

                    strBldr.append("\n\n\nNullPointerException: \n");
                    strBldr.append(ExceptionUtils.getStackTrace(npe));
                    logToolMessage(Level.WARNING, strBldr.toString(), false);

                }
            }
        } finally {
            logs.close();
        }

        grid.beforeTop();
        return grid;
    }

    /**
     * Gets the column names.
     *
     * @return List
     * @see com.x2dev.sis.tools.exports.ExportJavaSource#getColumnNames()
     */
    @Override
    protected List getColumnNames() {
        return m_columns;
    }

    /**
     * Gets the column user names.
     *
     * @return List
     * @see com.x2dev.sis.tools.exports.ExportJavaSource#getColumnUserNames()
     */
    @Override
    protected List getColumnUserNames() {
        return m_columns;
    }

    /**
     * Initialize.
     *
     * @see com.x2dev.sis.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        super.initialize();

        // Set columns
        m_columns = new ArrayList<String>(FIELD_COUNT);
        m_columns.add(FIELD_STD_LOCAL_ID);
        m_columns.add(FIELD_STD_LAST_NAME);
        m_columns.add(FIELD_STD_FIRST_NAME);
        m_columns.add(FIELD_STD_GRADE);
        m_columns.add(FIELD_STD_HOMEROOM_CODE);
        m_columns.add(FIELD_INCIDENT_DATE);
        m_columns.add(FIELD_INJURY_DESCRIPTION);
        m_columns.add(FIELD_INCIDENT_TYPE_DESCRIPTION);
        m_columns.add(FIELD_INCIDENT_CAUSE_DESCRIPTION);
        m_columns.add(FIELD_INCIDENT_LOCATION_DESCRIPTION);

        // Fill accident types and location codes reference code map if reference table exists
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());

        DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(ALIAS_INCIDENT_TYPE_DESCRIPTION);
        if (field != null) {
            ReferenceTable referenceTable = field.getReferenceTable();
            if (referenceTable != null) {
                m_accidentCodesMap = referenceTable.getCodeMap(getBroker());
            }
        }

        field = dictionary.findDataDictionaryField(HealthLog.class.getName(), HealthLog.COL_LOCATION_CODE);
        if (field != null) {
            ReferenceTable referenceTable = field.getReferenceTable();
            if (referenceTable != null) {
                m_locationCodesMap = referenceTable.getCodeMap(getBroker());
            }
        }
    }

    /**
     * Builds export criteria.
     *
     * @return Criteria
     */
    private Criteria buildCriteria() {
        X2Criteria criteria = new X2Criteria();
        criteria.addAndCriteria(
                getSchoolCriteria().copyWithAdjustedPath(HealthLog.REL_SCHOOL, HealthLog.COL_SCHOOL_OID));
        criteria.addEqualTo(HealthLog.REL_PERSON + ModelProperty.PATH_DELIMITER + Person.COL_STUDENT_INDICATOR,
                Boolean.TRUE);

        /*
         * Restrict by visit type
         */
        String visitType = (String) getParameter(PARAM_ACCIDENT_TYPE);
        if (!StringUtils.isEmpty(visitType)) {
            criteria.addEqualTo(HealthLog.COL_VISIT_TYPE, visitType);
        }

        /*
         * Restrict by date range
         */
        DistrictSchoolYearContext context = getCurrentContext();
        criteria.addGreaterOrEqualThan(HealthLog.COL_DATE, context.getStartDate());
        criteria.addLessOrEqualThan(HealthLog.COL_DATE, context.getEndDate());

        return criteria;
    }
}
