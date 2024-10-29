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
package com.x2dev.reports.sys.sped.ma;

import static com.follett.fsc.core.k12.tools.reports.ReportConstants.FIELD_DATA_SOURCE;
import static com.follett.fsc.core.k12.tools.reports.ReportConstants.FIELD_FORMAT;
import static com.follett.fsc.core.k12.tools.reports.ReportConstants.FIELD_OVERFLOW;
import static com.follett.fsc.core.k12.tools.reports.ReportConstants.FIELD_OVERFLOW_TITLE;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.tools.reports.MultiPageFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.x2dev.procedures.sys.sped.ma.MaSpedAttribHelper;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepDisability;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;

import net.sf.jasperreports.engine.JRDataSource;

/**
 * Java source for the Eligibility Determination form. This class provides access to the following
 * information on the format:
 * <ul>
 * <li>The form storage and owner objects as provided by <code>SimpleBeanDataSource</code>
 * <li>Each of the student's disabilities as report parameters in the following format: <code>
 * disability0, disability1, disability2...</code>; if a primary disability is flagged, it is
 * provided as <code>disability0</code>.
 * </ul>
 *
 * @author X2 Development Corporation
 */
public class EligibilityDeterminationFormData extends MultiPageFormReportJavaSource {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    private static final String DISABILITY_PARAM_PREFIX = "disability";

    public static final String PAGE_1_FORMAT_ID = "SYS-SPED-MA-ED1-1";
    public static final String OVERFLOW_FORMAT_ID = "SYS-SPED-MA-OVERFLOW";

    private MaSpedAttribHelper m_attribHelper;

    @Override
    protected Object gatherData() throws Exception {
        m_attribHelper = new MaSpedAttribHelper(getBroker(), true);
        return super.gatherData();
    }

    /**
     * Prepare page.
     *
     * @param grid ReportDataGrid
     * @see
     *      com.follett.fsc.core.k12.tools.reports.MultiPageFormReportJavaSource#preparePage(com.follett.
     *      fsc.core.k12.tools.reports.ReportDataGrid)
     */
    @Override
    protected void preparePage(ReportDataGrid grid) {
        String overflowFormatId = getOverflowFormatId();

        JRDataSource dataSource = null;
        if (overflowFormatId != null) {
            byte[] overflowFormat = getSubreportFormat(overflowFormatId);
            dataSource =
                    m_attribHelper.getMaSpedDataSource(getFormStorage(), getFormOwner(), grid, overflowFormat, null,
                            getDictionary(), getLocale());
        } else {
            dataSource =
                    m_attribHelper.getMaSpedDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale());
        }

        prepareCurrentPage(grid, dataSource);

        IepData iep = null;
        if (getCurrentPageNumber() == 1 && !isBlank() && getFormOwner() != null) {
            iep = (IepData) getFormOwner();
            int count = 0;

            IepDisability primaryDisability = iep.getPrimaryDisability(getBroker());
            if (primaryDisability != null) {
                addParameter(DISABILITY_PARAM_PREFIX + count, primaryDisability.getDisabilityCode());
                count++;
            }

            for (IepDisability disability : iep.getIepDisability(getBroker())) {
                if (!disability.equals(primaryDisability)) {
                    addParameter(DISABILITY_PARAM_PREFIX + count, disability.getDisabilityCode());
                    count++;
                }
            }
        }
        createOverflowPage(grid, iep);
    }

    /**
     * Gets the format ids.
     *
     * @return String[]
     * @see com.follett.fsc.core.k12.tools.reports.MultiPageFormReportJavaSource#getFormatIds()
     */
    @Override
    protected String[] getFormatIds() {
        return new String[] {PAGE_1_FORMAT_ID};
    }

    /**
     * Gets the overflow format id.
     *
     * @return String
     * @see
     *      com.follett.fsc.core.k12.tools.reports.MultiPageFormReportJavaSource#getOverflowFormatId()
     */
    @Override
    protected String getOverflowFormatId() {
        return OVERFLOW_FORMAT_ID;
    }
    
    /**
     * Create a custom overflow page to show the evaluation findings.
     * (Similar to hopw the TextOverflowHandler would do it.)
     */
    protected void createOverflowPage(ReportDataGrid grid, IepData iep) {
        X2BaseBean bean = getFormStorage();
        String findings = (String) bean.getFieldValueByAlias("ed1-evaluation-findings", getDictionary());
        String formDate = (String) bean.getFieldValueByAlias("ed1-date", getDictionary());
		if (!StringUtils.isEmpty(findings)) {
        	String heading = "Special Education Eligibility/Initial and Reevaluation Determination\n";  
        	if (iep != null) {
        		Converter converter = ConverterFactory.getConverterForClass(Converter.DATE_CONVERTER, getLocale(), false);
        		Converter asConverter = ConverterFactory.getConverterForClass(Converter.DATE_CONVERTER, getLocale(), true);
        		Student student = iep.getStudent();
    			if (formDate != null) {
    				formDate = asConverter.javaToString(formDate);
    			}
        		heading += "Student Name: " + student.getNameView() + "        DOB: " + converter.javaToString(student.getPerson().getDob()) + "        ID#: " + student.getStateId() + "        Date: " + formDate  + "\n";
        	}		
        			
        	heading += "Key Evaluation Findings and/or Next Steps";
            ReportDataGrid overflowGrid = new ReportDataGrid();
            overflowGrid.append();
            overflowGrid.set(FIELD_OVERFLOW, findings);
            overflowGrid.set(FIELD_OVERFLOW_TITLE, heading);
            overflowGrid.set("hideContinued", Boolean.TRUE);
            overflowGrid.beforeTop();

            String overflowFormatId = getOverflowFormatId();
            byte[] overflowFormat = getSubreportFormat(overflowFormatId);
            grid.append();
            grid.set(FIELD_DATA_SOURCE, overflowGrid);
            grid.set(FIELD_FORMAT, overflowFormat);
        }
    }
}
