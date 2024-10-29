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
package com.x2dev.reports.statereporting.ma;

import static com.follett.fsc.core.k12.tools.reports.ReportConstants.FIELD_DATA_SOURCE;
import static com.follett.fsc.core.k12.tools.reports.ReportConstants.FIELD_FORMAT;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.follett.fsc.core.k12.tools.reports.SimpleBeanDataSource;
import com.follett.fsc.core.k12.web.ContextList;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.procedures.statereporting.ma.MaPhysicalRestraint;
import com.x2dev.sis.model.beans.ConductAction;
import com.x2dev.sis.model.beans.ConductIncident;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.StringUtils;
import java.io.ByteArrayInputStream;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeSet;
import java.util.regex.Pattern;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares the data for the physical restraint report.
 *
 * @author X2 Development Corporation
 */
public class MaPhysicalRestraintReport extends ReportJavaSourceNet {

    private static final String PARAM_VALUE_CURRENT = "##current";
    private static final String PARAM_QUERY_BY1 = "queryBy1";
    private static final String ALIAS_DOE_RESTRAINT_CONTACT = "DOE RESTRAINT SCHOOL CONTACT";
    private static final String ALIAS_DOE_RESTRAINT_LOCATION = "DOE RESTRAINT LOCATION";

    private static final String EMTY = "";

    private static final String FIELD_SPED_STATUS = "spedStatus";
    private static final String FIELD_GENDER = "gender";
    private static final String FIELD_SASID = "sasid";
    private static final String FIELD_SCHOOL_NAME = "SchoolName";
    private static final String FIELD_STUDENT_NAME = "StudentName";
    private static final String FIELD_PARAMETERS_MAP = "PARAMETERS_MAP";

    private static final String PARAM_AFORMATTER = "aformatter";
    private static final String PARAM_HFORMATTER = "hformatter";
    private static final String PARAM_MFORMATTER = "mformatter";
    private static final String PARAM_PERSON_CONTACT = "personContact";
    private static final String PARAM_SIMPLE_SUBREPORT = "simpleSubreport";
    private static final String PARAM_TIME_PATTERN = "timePattern";
    private static final String PARAM_TIME_FORMATTER = "timeFormatter";
    private static final String PARAM_CURRENT_STUDENTS = "Current students";
    private static final String PARAM_UPDATE_PERSON_CONTACT = "updatePersonContact";

    private static final String PARAM_PROC_ID = "procedureId";


    private static final String TIME_FORMAT_A = "a";
    private static final String TIME_FORMAT_HH = "hh";
    private static final String TIME_FORMAT_HH_MM_A = "hh:mm a";
    private static final String TIME_FORMAT_MM = "mm";
    private static final String TIME_PATTERN_HH_MM_A = "(1[012]|[1-9]):[0-5][0-9](\\s)?(?i)(am|pm)";

    private Collection<String> m_studentsOids = null;


    /**
     * Report gather data phase.
     *
     * @return Object
     * @throws Exception exception
     */
    @Override
    protected Object gatherData() throws Exception {
        MaPhysicalRestraint export = new MaPhysicalRestraint();
        export.loadDefinitions((String) getParameter(PARAM_PROC_ID), getBroker());
        QueryIterator iterator = null;

        export.setBroker(getBroker());
        export.setCurrentContext(getCurrentContext());
        export.setOrganization(getOrganization());
        export.setPrivilegeSet(getPrivilegeSet());
        export.setSchoolContext(isSchoolContext());
        export.setSchool(getSchool());

        export.setUser(getUser());
        String queryParam = (String) getParameters().get(PARAM_QUERY_BY1);
        if (!StringUtils.isEmpty(queryParam) && queryParam.equals(PARAM_VALUE_CURRENT)) {
            getParameters().put(PARAM_CURRENT_STUDENTS, m_studentsOids);
        }
        export.setParameters(getParameters());

        QueryByCriteria reportQuery = export.getSortedExportQuery();
        iterator = getBroker().getIteratorByQuery(reportQuery);


        Set<String> subreportIds = getParamValuesByPrefix(PARAM_SIMPLE_SUBREPORT);
        ReportDataGrid grid = new ReportDataGrid();
        addParameter(PARAM_HFORMATTER, new SimpleDateFormat(TIME_FORMAT_HH));
        addParameter(PARAM_MFORMATTER, new SimpleDateFormat(TIME_FORMAT_MM));
        addParameter(PARAM_AFORMATTER, new SimpleDateFormat(TIME_FORMAT_A));
        addParameter(PARAM_TIME_FORMATTER, new SimpleDateFormat(TIME_FORMAT_HH_MM_A));
        addParameter(PARAM_TIME_PATTERN, Pattern.compile(TIME_PATTERN_HH_MM_A));

        Boolean updatePersonContact = (Boolean) getParameter(PARAM_UPDATE_PERSON_CONTACT);
        String personContact = (String) getParameter(PARAM_PERSON_CONTACT);
        try {
            while (iterator.hasNext()) {
                ConductAction action = (ConductAction) iterator.next();

                if (updatePersonContact.booleanValue()) {
                    DataDictionary ddx = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
                    DataDictionaryField field = ddx.findDataDictionaryFieldByAlias(ALIAS_DOE_RESTRAINT_CONTACT);
                    int maxSize = field.getUserLength();
                    if (!StringUtils.isEmpty(personContact) && personContact.length() > maxSize) {
                        personContact = personContact.substring(0, maxSize);
                    }
                    action.setFieldValueByAlias(ALIAS_DOE_RESTRAINT_CONTACT, personContact);
                    if (action.isDirty()) {
                        getBroker().saveBeanForced(action);

                    }
                }
                Map<String, Object> params = new HashMap<String, Object>();
                params.putAll(getParameters());
                ConductIncident incident = action.getIncident();
                params.put(ALIAS_DOE_RESTRAINT_LOCATION,
                        incident == null ? EMTY : incident.getFieldValueByAlias(ALIAS_DOE_RESTRAINT_LOCATION));
                for (String subreportId : subreportIds) {

                    grid.append();
                    DataDictionary datadictionary =
                            DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
                    JRDataSource dataSource = new SimpleBeanDataSource(action, datadictionary, getLocale());
                    Report subreport = ReportUtils.getReport(subreportId, getBroker());
                    ByteArrayInputStream format = new ByteArrayInputStream(subreport.getCompiledFormat());
                    grid.set(FIELD_DATA_SOURCE, dataSource);
                    grid.set(FIELD_FORMAT, format);
                    grid.set(FIELD_PARAMETERS_MAP, params);
                    grid.set(FIELD_STUDENT_NAME, action.getStudent().getNameView());
                    grid.set(FIELD_SCHOOL_NAME, action.getSchool().getName());
                    grid.set(FIELD_SASID, action.getStudent().getStateId());
                    grid.set(FIELD_GENDER, action.getStudent().getPerson().getGenderCode());
                    grid.set(FIELD_SPED_STATUS, action.getStudent().getSpedStatusCode());
                }

            }
        } finally {
            iterator.close();
        }


        grid.beforeTop();

        return grid;
    }

    /**
     * Remember the currently selected student if this report is being run from the student module.
     *
     * @param userDate UserDataContainer
     * @see com.x2dev.sis.tools.ToolJavaSource#saveState(com.x2dev.sis.web.UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userDate) {

        m_studentsOids = new ArrayList<String>();
        ConductIncident incident = userDate.getCurrentRecord(ConductIncident.class);
        SisStudent stdCurrent = userDate.getCurrentRecord(SisStudent.class);
        ConductAction action = userDate.getCurrentRecord(ConductAction.class);
        if (incident != null) {
            m_studentsOids.add(incident.getStudentOid());
        } else if (stdCurrent != null) {
            m_studentsOids.add(stdCurrent.getOid());
        } else if (action != null) {
            m_studentsOids.add(action.getStudentOid());
        } else {
            ContextList currentList = userDate.getCurrentList();
            if (currentList != null && currentList.getDataClass().equals(SisStudent.class)) {
                SubQuery stdSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, getCurrentCriteria());
                m_studentsOids = getBroker().getSubQueryCollectionByQuery(stdSubQuery);
            }
            if (currentList != null && currentList.getDataClass().equals(ConductIncident.class)) {
                SubQuery stdSubQuery =
                        new SubQuery(ConductIncident.class, ConductIncident.COL_STUDENT_OID, getCurrentCriteria());
                m_studentsOids = getBroker().getSubQueryCollectionByQuery(stdSubQuery);
            }
            if (currentList != null && currentList.getDataClass().equals(ConductAction.class)) {
                SubQuery stdSubQuery =
                        new SubQuery(ConductAction.class, ConductAction.COL_STUDENT_OID, getCurrentCriteria());
                m_studentsOids = getBroker().getSubQueryCollectionByQuery(stdSubQuery);
            }

        }
    }

    /**
     * Gets the param values by prefix.
     *
     * @param prefix String
     * @return Sets the
     */
    protected Set<String> getParamValuesByPrefix(String prefix) {
        Set<String> values = new TreeSet<String>();
        Map<String, Object> map = getParameters();
        for (Entry<String, Object> entry : map.entrySet()) {
            String key = entry.getKey();
            if (key.startsWith(prefix)) {
                String value = (String) entry.getValue();
                if (StringUtils.isEmpty(value)) {
                    value = key.replaceAll(prefix, EMTY);
                }
                values.add(value);
            }
        }
        return values;
    }



}
