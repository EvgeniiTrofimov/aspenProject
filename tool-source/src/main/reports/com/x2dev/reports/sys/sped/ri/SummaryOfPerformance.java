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
package com.x2dev.reports.sys.sped.ri;

import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.GenericFormChildData;
import com.follett.fsc.core.k12.beans.GenericFormData;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepService;
import com.x2dev.sis.model.beans.IepTeamMember;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.beans.SisStudent;
import java.io.ByteArrayInputStream;
import java.sql.Date;
import java.util.Collection;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * RI SPED Summary Of Performance Report java source.
 *
 * @author X2 Development Corporation
 */
public class SummaryOfPerformance extends BaseFormReportJavaSource {
    /*
     * Parameter names
     */
    private final String PARAM_ACCOMMODATIONS_GRID = "accommodationsGrid";
    private final String PARAM_ACCOMMODATIONS_FORMAT_PARAM = "accommodationsFormat";
    private final String PARAM_ACCOMMODATIONS_SUBREPORT_ID_PARAM = "RI-SPED-SOP-A";
    private final String PARAM_ACCOMMODATION_REASON_NEEDED_TEXT = "reasonText";
    private final String PARAM_ACCOMMODATION_TEXT = "accomText";
    private final String PARAM_CASE_MANAGER = "caseManager";
    private final String PARAM_IEP_DATA = "iepData";
    private final String PARAM_ORGANIZATION = "organization";
    private final String PARAM_PRIMARY_CONDITION = "primaryCondition";
    private final String PARAM_SECONDARY_CONDITION = "secondaryCondition";
    private final String PARAM_SERVICES_DESCRIPTIONS = "servicesDescriptions";
    private final String PARAM_SOP_DATE = "sopDate";
    private final String PARAM_STUDENT = "student";

    /*
     * Iep Team Member field names
     */
    private final String FIELD_NAME = "name";
    private final String FIELD_TITLE = "title";

    /*
     * Accommodation field names
     */
    private final String FIELD_ACCOMMODATION_NAME = "accommodationName";
    private final String FIELD_ACCOMMODATION_REASON_NEEDED = "accommodationNeededReason";

    /*
     * Data Dictionary Aliases
     */
    private final String ACCOMMODATION_NAME_ALIAS = "sop-accom-name";
    private final String ACCOMMODATION_REASON_NEEDED_ALIAS = "sop-accom-reason-needed";
    private final String ACCOMMODATION_REASON_NEEDED_TEXT_ALIAS = "sop-reason-text";
    private final String ACCOMMODATION_TEXT_ALIAS = "sop-accom-text";
    private final String SERVICE_DESCRIPTION = "service-description";
    private final String SUMMARY_OF_PERFORMANCE_DATE_ALIAS = "sop-date";

    private final String SERVICE_MODE_SUPPLEM_AIDS = "Supplementary Aids";

    private final String FORM_IEP_RI_ID = "SPED-RI-IEP";

    private Collection<IepService> m_services = null;

    /**
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        DataDictionary dictionary =
                DataDictionary.getDistrictDictionary(getFormDefinition().getExtendedDataDictionary(),
                        getBroker().getPersistenceKey());

        GenericFormData formData = (GenericFormData) getFormStorage();
        Collection<GenericFormChildData> accommodations = null;

        if (formData != null) {
            accommodations = formData.getGenericFormDataChildren();
        }

        ReportDataGrid teamMemberGrid = new ReportDataGrid();

        addParameter(PARAM_ORGANIZATION, getOrganization());

        IepData iepData = (IepData) getFormOwner();
        SisStudent student = iepData.getStudent();
        SisStaff caseManager = iepData.getStaff();

        addParameter(PARAM_IEP_DATA, iepData);
        addParameter(PARAM_STUDENT, student);
        addParameter(PARAM_CASE_MANAGER, caseManager);

        String sopDate = "2000-01-01";
        if (formData != null) {
            if (formData.getFieldValueByAlias(SUMMARY_OF_PERFORMANCE_DATE_ALIAS, dictionary) != null) {
                sopDate = (String) formData.getFieldValueByAlias(SUMMARY_OF_PERFORMANCE_DATE_ALIAS, dictionary);
            }
        }
        addParameter(PARAM_SOP_DATE, Date.valueOf(sopDate));
        /*
         * Retrieve and set the primaryCondition and secondaryCondition from the IepData
         */
        if (iepData.getPrimaryDisability() != null) {
            addParameter(PARAM_PRIMARY_CONDITION, iepData.getPrimaryDisability().getDisabilityCode());
        }
        addParameter(PARAM_SECONDARY_CONDITION, iepData.getSecondaryDisabilitiesView());

        String accomReasonNeededText = (String) formData.getFieldValueByAlias(ACCOMMODATION_REASON_NEEDED_TEXT_ALIAS,
                dictionary);
        addParameter(PARAM_ACCOMMODATION_REASON_NEEDED_TEXT, accomReasonNeededText);

        String accomText = (String) formData.getFieldValueByAlias(ACCOMMODATION_TEXT_ALIAS, dictionary);
        addParameter(PARAM_ACCOMMODATION_TEXT, accomText);

        /*
         * Add supplementary aids and services descriptions
         */
        loadServices();

        String supplAidsDescriptions = StringUtils.EMPTY;

        for (IepService service : m_services) {
            supplAidsDescriptions = supplAidsDescriptions.concat(service.getFieldValueByAlias(SERVICE_DESCRIPTION,
                    getDictionaryByExtendedDictionaryId(FORM_IEP_RI_ID)) + "\n");
        }

        addParameter(PARAM_SERVICES_DESCRIPTIONS, supplAidsDescriptions);

        /*
         * Create the Team Members details band of the report
         */
        Collection<IepTeamMember> teamMembers = iepData.getTeamMembers();
        if (teamMembers != null) {
            for (IepTeamMember teamMember : teamMembers) {
                if ("1".equals(teamMember.getFieldA001())) {
                    teamMemberGrid.append();
                    teamMemberGrid.set(FIELD_NAME, teamMember.getNameView());
                    teamMemberGrid.set(FIELD_TITLE, teamMember.getMemberRoleCode());
                }
            }
        }

        if (teamMemberGrid.isEmpty()) {
            teamMemberGrid.append();
            teamMemberGrid.set(FIELD_NAME, "");
            teamMemberGrid.set(FIELD_TITLE, "");
        }

        /*
         * Create the sub report grid for the accommodations section of the report
         */
        ReportDataGrid accommodationsGrid = new ReportDataGrid();

        if (accommodations != null) {
            for (GenericFormChildData accommodation : accommodations) {
                accommodationsGrid.append();
                accommodationsGrid.set(FIELD_ACCOMMODATION_NAME, accommodation.getFieldValueByAlias(
                        ACCOMMODATION_NAME_ALIAS, dictionary));
                accommodationsGrid.set(FIELD_ACCOMMODATION_REASON_NEEDED, accommodation.getFieldValueByAlias(
                        ACCOMMODATION_REASON_NEEDED_ALIAS, dictionary));
            }
        }

        accommodationsGrid.beforeTop();
        addParameter(PARAM_ACCOMMODATIONS_GRID, accommodationsGrid);
        Report accommodationsReport = ReportUtils.getReport(PARAM_ACCOMMODATIONS_SUBREPORT_ID_PARAM, getBroker());
        addParameter(PARAM_ACCOMMODATIONS_FORMAT_PARAM,
                new ByteArrayInputStream(accommodationsReport.getCompiledFormat()));

        teamMemberGrid.beforeTop();
        return teamMemberGrid;
    }

    /**
     * Returns extended data dictionary of another form by ID.
     *
     * @param extendedDataDictionaryID String
     * @return DataDictinary
     */
    private DataDictionary getDictionaryByExtendedDictionaryId(String extendedDataDictionaryID) {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(ExtendedDataDictionary.COL_ID, extendedDataDictionaryID);
        QueryByCriteria byCriteria = new QueryByCriteria(ExtendedDataDictionary.class, criteria);
        ExtendedDataDictionary extendedDataDictionary = (ExtendedDataDictionary) getBroker().getBeanByQuery(byCriteria);
        return DataDictionary.getDistrictDictionary(extendedDataDictionary, getBroker().getPersistenceKey());
    }

    /**
     * Returns the current IEP. If a blank form is being printed, a new (unsaved) IEP is created and
     * returned.
     *
     * @return IepData
     */
    private IepData getIep() {
        IepData iep = null;

        if (isBlank()) {
            IepData ownerIep = (IepData) getFormOwner();

            iep = new IepData(getBroker().getPersistenceKey());
            iep.setStudentOid(ownerIep.getStudentOid());
            iep.setStaffOid(ownerIep.getStaffOid());
        } else {
            iep = (IepData) getFormOwner();
        }

        return iep;
    }

    /**
     * Loads Supplementary Aids into collection.
     */
    private void loadServices() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(IepService.COL_IEP_DATA_OID, getIep().getOid());
        criteria.addEqualTo(IepService.COL_SERVICE_MODE, SERVICE_MODE_SUPPLEM_AIDS);

        QueryByCriteria servicesQuery = new QueryByCriteria(IepService.class, criteria);

        servicesQuery.addOrderByAscending(IepService.COL_GOAL_VIEW); // Focus on goal number

        m_services = getBroker().getCollectionByQuery(servicesQuery);
    }
}
