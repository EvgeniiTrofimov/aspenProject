/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2003 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.reports.sys.sped.dodea;

import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.DateAsStringConverter;
import net.sf.jasperreports.engine.JRDataSource;

/**
 * Java source for a form-based report. All fields and aliases present on the form storage and owner
 * objects are available for use in the format. The storage and owner objects are retrieved and made
 * available by the superclass - <code>BaseFormReportJavaSource</code>.
 * <p>
 * In the report format, fields and aliases from the storage object can be accessed directly. To
 * retrieve values from the owner object, the prefix <b>"owner"</b> must be present on the field
 * or alias. See <code>SimpleFormDataSource</code> for more information.
 * <p>
 * This form for DoDEA is the "Transfer of Rights" form. Users have the option to print 3 copies
 * of the report.
 *
 * @author X2 Development Corporation
 */
public class TransferOfRightsData extends BaseFormReportJavaSource {
    /**
     * Name for the "original copy" report parameter. The value is a Boolean.
     */
    public static final String FORM_COPY_1_PARAM = "copy1";

    /**
     * Name for the "copy to student" report parameter. The value is a Boolean.
     */
    public static final String FORM_COPY_2_PARAM = "copy2";

    /**
     * Name for the "copy to parent/guardian" report parameter. The value is a Boolean.
     */
    public static final String FORM_COPY_3_PARAM = "copy3";

    /*
     * Grid fields
     */
    private static final String FIELD_COPY = "copy";
    private static final String FIELD_INFORMED_DATE = "informedDate";
    private static final String FIELD_OWNER = "owner";
    private static final String FIELD_SIGNATURE_DATE = "signatureDate";

    /*
     * Dictionary aliases
     */
    private static final String ALIAS_INFORMED_DATE = "csc-informed-date";
    private static final String ALIAS_SIGNATURE_DATE = "csc-csmgr-sig-date";

    /**
     * @see com.x2dev.sis.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        ReportDataGrid grid = new ReportDataGrid();

        DateAsStringConverter converter = (DateAsStringConverter) ConverterFactory
                .getConverterForClass(Converter.DATE_CONVERTER, getLocale(), true);

        X2BaseBean storage = getFormStorage();
        String informedDate = (String) storage.getFieldValueByAlias(ALIAS_INFORMED_DATE, getDictionary());
        String signatureDate = (String) storage.getFieldValueByAlias(ALIAS_SIGNATURE_DATE, getDictionary());

        boolean copy1 = (Boolean) getParameter(FORM_COPY_1_PARAM);
        if (copy1) {
            grid.append();
            grid.set(FIELD_COPY, 1);
            grid.set(FIELD_INFORMED_DATE, converter.javaToString(informedDate));
            grid.set(FIELD_OWNER, getFormOwner());
            grid.set(FIELD_SIGNATURE_DATE, converter.javaToString(signatureDate));
        }

        boolean copy2 = (Boolean) getParameter(FORM_COPY_2_PARAM);
        if (copy2) {
            grid.append();
            grid.set(FIELD_COPY, 2);
            grid.set(FIELD_INFORMED_DATE, converter.javaToString(informedDate));
            grid.set(FIELD_OWNER, getFormOwner());
            grid.set(FIELD_SIGNATURE_DATE, converter.javaToString(signatureDate));
        }

        boolean copy3 = (Boolean) getParameter(FORM_COPY_3_PARAM);
        if (copy3) {
            grid.append();
            grid.set(FIELD_COPY, 3);
            grid.set(FIELD_INFORMED_DATE, converter.javaToString(informedDate));
            grid.set(FIELD_OWNER, getFormOwner());
            grid.set(FIELD_SIGNATURE_DATE, converter.javaToString(signatureDate));
        }

        grid.beforeTop();

        return grid;
    }
}
