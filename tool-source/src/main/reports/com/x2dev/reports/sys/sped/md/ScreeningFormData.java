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
package com.x2dev.reports.sys.sped.md;

import com.follett.fsc.core.k12.beans.GenericFormData;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.business.sped.SpedUtils;
import net.sf.jasperreports.engine.JRDataSource;

/**
 * Java source for the Wicomico County special education screening form.
 *
 * @author X2 Development Corporation
 */
public class ScreeningFormData extends BaseFormReportJavaSource {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    public static final String PARAM_IEP_DICTIONARY = "iepDictionary";

    public static final String COL_FORM = "form";
    public static final String COL_STUDENT = "student";
    public static final String COL_RESPONSE = "response";

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        X2BaseBean formStorage = getFormStorage();
        DataDictionary dictionary = getDictionary();

        ReportDataGrid grid = new ReportDataGrid();

        /*
         * Append a row to the response grid for each unlimited text response. This approach allows
         * us to display the questions and responses using a band-based format that can grow with
         * the size of the text entered.
         */
        for (int i = 0; i < 11; i++) {
            String alias = null;

            switch (i) {
                case 0:
                    alias = "screen-hearing";
                    break;

                case 1:
                    alias = "screen-vision";
                    break;

                case 2:
                    alias = "screen-health";
                    break;

                case 3:
                    alias = "screen-academic";
                    break;

                case 4:
                    alias = "screen-general-intel";
                    break;

                case 5:
                    alias = "screen-communication";
                    break;

                case 6:
                    alias = "screen-motor";
                    break;

                case 7:
                    alias = "screen-social-emotional";
                    break;

                case 8:
                    alias = "screen-intervention-strategies";
                    break;

                case 9:
                    alias = "screen-additional-info";
                    break;
            }

            SisStudent student = ((IepData) getFormOwner()).getStudent();

            grid.append();

            if (isBlank()) {
                grid.set(COL_FORM, X2BaseBean.newInstance(GenericFormData.class, getBroker().getPersistenceKey()));
                grid.set(COL_RESPONSE, "");
            } else {
                grid.set(COL_FORM, formStorage);

                String responseBytes = (String) formStorage.getFieldValueByAlias(alias, dictionary);
                if (responseBytes != null) {
                    grid.set(COL_RESPONSE, responseBytes);
                }
            }
            grid.set(COL_STUDENT, student);
        }

        grid.beforeTop();

        addParameter(PARAM_IEP_DICTIONARY,
                DataDictionary.getDistrictDictionary(SpedUtils.getIepDictionary(getOrganization(), getBroker()),
                        getBroker().getPersistenceKey()));

        return grid;
    }
}
