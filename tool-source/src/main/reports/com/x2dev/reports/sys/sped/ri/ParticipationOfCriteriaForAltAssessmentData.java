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
package com.x2dev.reports.sys.sped.ri;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.FormInstance;
import com.follett.fsc.core.k12.beans.GenericFormData;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.types.PlainDate;
import java.util.Date;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class ParticipationOfCriteriaForAltAssessmentData.
 */
public class ParticipationOfCriteriaForAltAssessmentData extends BaseFormReportJavaSource {
    /**
     *
     */
    private static final String PARAM_CREATED_IN_YEARS = "createdInYears";
    /*
     * Parameters
     */
    private final String PARAM_IEPDATA = "iepData";
    private final String PARAM_STUDENT = "student";

    /*
     * Aliases
     */
    private final String ALIAS_DECISION1 = "decision-1";
    private final String ALIAS_DECISION2 = "decision-2";
    private final String ALIAS_DECISION3 = "decision-3";
    private final String ALIAS_DOCUMENT_DESCRIPTION1 = "doc-desc-1";
    private final String ALIAS_DOCUMENT_DESCRIPTION2 = "doc-desc-2";
    private final String ALIAS_DOCUMENT_DESCRIPTION3 = "doc-desc-3";

    /*
     * Fields
     */
    private final String FIELD_DECISION1 = "decision1";
    private final String FIELD_DECISION2 = "decision2";
    private final String FIELD_DECISION3 = "decision3";
    private final String FIELD_DOCUMENT_DESCRIPTION1 = "documentDescription1";
    private final String FIELD_DOCUMENT_DESCRIPTION2 = "documentDescription2";
    private final String FIELD_DOCUMENT_DESCRIPTION3 = "documentDescription3";

    /**
     * Crideria descriptions sub-report parameters
     */
    private static final String DATASOURCE_DEF_DESCRIPTION = "datasource_desc";
    private static final String FORMAT_DEF_DESCRIPTION = "format_desc";
    private static final String PARTICIPATION_CRITERIA_DESCRIPTION_SUB = "RI-SPED-PRCR-DESC";

    /**
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        DataDictionary dictionary =
                DataDictionary.getDistrictDictionary(getFormDefinition().getExtendedDataDictionary(),
                        getBroker().getPersistenceKey());

        GenericFormData formData = (GenericFormData) getFormStorage();
        IepData iepData = (IepData) getFormOwner();
        SisStudent student = iepData.getStudent();

        ReportDataGrid grid = new ReportDataGrid();
        addParameter(PARAM_IEPDATA, iepData);
        addParameter(PARAM_STUDENT, student);
        addParameter(PARAM_CREATED_IN_YEARS, getContextYearsBYFormDate());

        addParameter(FIELD_DECISION1, StringUtils.coalesce((String) formData.getFieldValueByAlias(
                ALIAS_DECISION1, dictionary), ""));
        addParameter(FIELD_DECISION2, StringUtils.coalesce((String) formData.getFieldValueByAlias(
                ALIAS_DECISION2, dictionary), ""));
        addParameter(FIELD_DECISION3, StringUtils.coalesce((String) formData.getFieldValueByAlias(
                ALIAS_DECISION3, dictionary), ""));

        grid.append();
        grid.set(FIELD_DOCUMENT_DESCRIPTION1, StringUtils.coalesce((String) formData.getFieldValueByAlias(
                ALIAS_DOCUMENT_DESCRIPTION1, dictionary), ""));
        grid.set(FIELD_DOCUMENT_DESCRIPTION2, StringUtils.coalesce((String) formData.getFieldValueByAlias(
                ALIAS_DOCUMENT_DESCRIPTION2, dictionary), ""));
        grid.set(FIELD_DOCUMENT_DESCRIPTION3, StringUtils.coalesce((String) formData.getFieldValueByAlias(
                ALIAS_DOCUMENT_DESCRIPTION3, dictionary), ""));

        /**
         * Prepares parameters for 'Criteria Descriptions' sub-report
         */
        JRDataSource dataSource =
                new SimpleFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale());

        Report evalSubreport = ReportUtils.getReport(PARTICIPATION_CRITERIA_DESCRIPTION_SUB, getBroker());
        addParameter(FORMAT_DEF_DESCRIPTION, evalSubreport.getCompiledFormat());
        addParameter(DATASOURCE_DEF_DESCRIPTION, dataSource);

        grid.beforeTop();
        return grid;
    }

    private String getContextYearsBYFormDate() {
        FormInstance fi = getFormInstance();
        DistrictSchoolYearContext context = null;
        if (fi != null) {
            PlainDate date = new PlainDate(new Date(fi.getCreatedTime()));
            Criteria criteria = new X2Criteria();
            criteria.addLessOrEqualThan(DistrictSchoolYearContext.COL_START_DATE, date);
            QueryByCriteria byCriteria = new QueryByCriteria(DistrictSchoolYearContext.class, criteria);
            byCriteria.addOrderBy(DistrictSchoolYearContext.COL_START_DATE, false);
            context = (DistrictSchoolYearContext) getBroker().getBeanByQuery(byCriteria);
        }
        if (context == null) {
            context = getCurrentContext();
        }
        return String.valueOf(context.getSchoolYear() - 1) + "-" +
                String.valueOf(context.getSchoolYear());
    }
}
