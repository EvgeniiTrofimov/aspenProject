/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2015 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.reports.statereporting.ma;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.GenericFormChildData;
import com.follett.fsc.core.k12.beans.GenericFormData;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.Staff;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.beans.StaffPdPlan;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import net.sf.jasperreports.engine.JRDataSource;

/**
 * The Class PdPerformSelfAssessmentData.
 *
 * @author X2 Development Corporation
 */
public class PdPerformSelfAssessmentData extends BaseFormReportJavaSource {
    private static final String ALIAS_STAFF_TEAM_NAME = "toe-team-name";
    private static final String ALIAS_STAFF_TEAM_LIST = "toe-team-list";
    private static final String ALIAS_ASSESSMENT_STANDARDS = "assessment-standards";
    private static final String ALIAS_ASSESSMENT_ASSESSMENT = "assessment-assessment";

    private static final String REPORT_PARAM_TEAM_MEMBERS = "teamMembers";
    private static final String REPORT_PARAM_TEAM_NAME = "teamName";
    private static final String REPORT_PARAM_ASSESSMENTS = "assessments";
    private static final String REPORT_PARAMETER_PRIMARY_EVALUATOR = "primaryEvaluator";

    private static final String STORAGE_ALIAS_PRIMARY_EVALUATOR_OID = "stf-oid-primary";

    private static final Map<String, String> ASSESSMENTS_REPLACEMENT_MAP = new HashMap<String, String>() {
        {
            put("&(?!amp;)", "&amp;");
        }
    };

    private String m_assessmentField;
    private String m_assessments;
    private String m_assessmentStandardsField;
    private Map<String, ReferenceCode> m_staffTeamCodes;
    private String m_staffTeamListField;
    private String m_staffTeamNameField;
    private List<String> m_teamMembers;
    private String m_teamName = null;
    private DataDictionary m_dataDictionary;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        loadAssessments();
        loadTeamCodes();
        loadTeam();

        String primaryEvaluatorId =
                (String) getFormStorage().getFieldValueByAlias(STORAGE_ALIAS_PRIMARY_EVALUATOR_OID, getDictionary());
        String primaryEvaluatorName = null;
        if (primaryEvaluatorId != null && !primaryEvaluatorId.isEmpty()) {
            SisStaff primaryEvaluator = (SisStaff) getBroker().getBeanByOid(Staff.class, primaryEvaluatorId);
            if (primaryEvaluator != null) {
                primaryEvaluatorName = primaryEvaluator.getNameView();
            }
        }

        addParameter(REPORT_PARAMETER_PRIMARY_EVALUATOR, primaryEvaluatorName);
        addParameter(REPORT_PARAM_TEAM_NAME, m_teamName);
        addParameter(REPORT_PARAM_TEAM_MEMBERS, m_teamMembers);
        addParameter(REPORT_PARAM_ASSESSMENTS, m_assessments);

        return new SimpleFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale());
    }

    /**
     * Initialize.
     *
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#initialize()
     */
    @Override
    protected void initialize() {
        super.initialize();
        m_dataDictionary = DataDictionary.getDistrictDictionary(getFormDefinition().getExtendedDataDictionary(),
                getBroker().getPersistenceKey());
        m_assessmentField = m_dataDictionary.findDataDictionaryFieldByAlias(ALIAS_ASSESSMENT_STANDARDS).getJavaName();
        m_assessmentStandardsField =
                m_dataDictionary.findDataDictionaryFieldByAlias(ALIAS_ASSESSMENT_ASSESSMENT).getJavaName();
        m_staffTeamNameField = m_dataDictionary.findDataDictionaryFieldByAlias(ALIAS_STAFF_TEAM_NAME).getJavaName();
        m_staffTeamListField = m_dataDictionary.findDataDictionaryFieldByAlias(ALIAS_STAFF_TEAM_LIST).getJavaName();
    }

    /**
     * Gathers assessments in a formatted string.
     */
    private void loadAssessments() {
        StringBuilder assessmentBuilder = new StringBuilder();
        GenericFormData genericFormData = (GenericFormData) getFormStorage();
        Collection<GenericFormChildData> children = genericFormData.getGenericFormDataChildren(getBroker());
        for (GenericFormChildData childData : children) {
            {
                assessmentBuilder.append("<style isBold=\"true\" pdfFontName=\"Helvetica-Bold\">")
                        .append(childData.getFieldValueByBeanPath(m_assessmentStandardsField))
                        .append("</style>\n")
                        .append(childData.getFieldValueByBeanPath(m_assessmentField))
                        .append("\n");
            }
        }

        m_assessments = assessmentBuilder.toString();
        for (Entry<String, String> replacement : ASSESSMENTS_REPLACEMENT_MAP.entrySet()) {
            m_assessments = m_assessments.replaceAll(replacement.getKey(), replacement.getValue());
        }
    }

    /**
     * Loads toe-team-name field for the form owner and
     * list of persons which contain that team in toe-team-list field.
     */
    private void loadTeam() {
        X2Criteria ownerCriteria = new X2Criteria();
        ownerCriteria.addEqualTo(X2BaseBean.COL_OID, getFormOwner().getOid());
        SubQuery ownerSubQuery = new SubQuery(StaffPdPlan.class, StaffPdPlan.COL_STAFF_OID, ownerCriteria);

        X2Criteria ownerTeamCriteria = new X2Criteria();
        ownerTeamCriteria.addIn(X2BaseBean.COL_OID, ownerSubQuery);
        SubQuery ownerTeamSubQuery = new SubQuery(SisStaff.class, m_staffTeamNameField, ownerTeamCriteria);
        Collection<String> teamNames = getBroker().getSubQueryCollectionByQuery(ownerTeamSubQuery);

        m_teamMembers = new ArrayList<String>();
        if (teamNames.size() > 0) {
            String teamNameCode = teamNames.iterator().next();

            m_teamName = null;
            if (m_staffTeamCodes.containsKey(teamNameCode)) {
                m_teamName = m_staffTeamCodes.get(teamNameCode).getDescription();
            }

            X2Criteria teamCriteria = new X2Criteria();
            teamCriteria.addLike(m_staffTeamListField, "%" + teamNameCode + "%");
            BeanQuery teamQuery = new BeanQuery(SisStaff.class, teamCriteria);
            Collection<SisStaff> teamCollection = getBroker().getCollectionByQuery(teamQuery);
            Iterator<SisStaff> iterator = teamCollection.iterator();
            while (m_teamMembers.size() <= 8 && iterator.hasNext()) {
                m_teamMembers.add(iterator.next().getNameView());
            }
        }
    }

    /**
     * Creates the "TO&E Teams" reference codes map.
     */
    private void loadTeamCodes() {
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField teamCodeField =
                dictionary.findDataDictionaryField(SisStaff.class.getName(), m_staffTeamNameField);

        ReferenceTable refTable = teamCodeField.getReferenceTable();
        if (refTable == null) {
            m_staffTeamCodes = new HashMap<String, ReferenceCode>();
        } else {
            m_staffTeamCodes = refTable.getCodeMap(getBroker());
        }
    }

}
