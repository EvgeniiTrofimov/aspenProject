/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2002-2016 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.reports.sys.sped.il;

import static com.follett.fsc.core.k12.tools.reports.ReportConstants.FIELD_DATA_SOURCE;
import static com.follett.fsc.core.k12.tools.reports.ReportConstants.FIELD_FORMAT;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import com.x2dev.utils.StringUtils;
import java.io.ByteArrayInputStream;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeSet;
import net.sf.jasperreports.engine.JRDataSource;

/**
 * Java source for a form-based report. All fields and aliases present on the form storage and owner
 * objects are available for use in the format. The storage and owner objects are retrieved and made
 * available by the superclass - <code>BaseFormReportJavaSource</code>.
 * <p>
 * In the report format, fields and aliases from the storage object can be accessed directly. To
 * retrieve values from the owner object, the prefix <b>"owner"</b> must be present on the field
 * or alias. See <code>SimpleFormDataSource</code> for more information.
 *
 * @author X2 Development Corporation
 */
public class ILSimpleSubreports extends BeanReport {

    private static final String FIELD_PARAMETERS_MAP = "PARAMETERS_MAP";
    private static final String PARAM_SIMPLE_SUBREPORT = "simpleSubreport";

    private ReportDataGrid m_grid;

    private static final long serialVersionUID = 1L;


    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.x2dev.reports.sys.sped.il.BeanReport#fillGrid(Map, Locale)
     */
    @Override
    protected JRDataSource gatherData() {
        Set<String> subreportIds = getParamValuesByPrefix(PARAM_SIMPLE_SUBREPORT);
        m_grid = new ReportDataGrid();
        for (String subreportId : subreportIds) {
            m_grid.append();
            JRDataSource dataSource =
                    new SimpleFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale());
            Report subreport = ReportUtils.getReport(subreportId, getBroker());
            ByteArrayInputStream format = new ByteArrayInputStream(subreport.getCompiledFormat());
            m_grid.set(FIELD_DATA_SOURCE, dataSource);
            m_grid.set(FIELD_FORMAT, format);
            m_grid.set(FIELD_PARAMETERS_MAP, getParameters());

        }
        m_grid.beforeTop();
        return m_grid;
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
                    value = key.replaceAll(prefix, "");
                }
                values.add(value);
            }
        }
        return values;
    }
}
