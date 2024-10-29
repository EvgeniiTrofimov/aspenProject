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

import static com.x2dev.sis.model.business.sped.MassachusettsAliases.CH688_FORM_ID;
import static com.x2dev.sis.model.business.sped.MassachusettsAliases.CH688_REFERRAL_DATE_COMPLETED;
import static com.x2dev.sis.model.business.sped.MassachusettsAliases.IEP_ANTICIPATED_GRADUATION_DATE;
import com.follett.fsc.core.k12.beans.FormDefinition;
import com.follett.fsc.core.k12.beans.FormInstance;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.ExtendedDictionaryAttributes;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.business.sped.SpedUtils;
import com.x2dev.utils.types.PlainDate;
import java.util.Collection;

/**
 * Java source for the transition planning form. This class is responsible for retrieving the
 * following two dates:
 * <ul>
 * <li>Anticipated graduation date - from the student's IEP
 * <li>Anticipated date of 688 referral - from the most recent chapter 688 referral, if one exists
 * </ul>
 * These must be retrieved in the Java source because they are from foreign extended dictionaries.
 * They are passed in as parameters.
 *
 * @author X2 Development Corporation
 */
public class TransitionPlanningFormData extends MaMultiPageBeanReport {
    private static final long serialVersionUID = 1L;

    /**
     * ID of the page 1 report.
     */
    public static final String PAGE_1_FORMAT_ID = "SYS-SPED-MA-28M9-1";

    /**
     * ID of the page 2 report.
     */
    public static final String PAGE_2_FORMAT_ID = "SYS-SPED-MA-28M9-2";

    /**
     * Report parameter containing the student's age at the start of IEP
     */
    public static final String PARAM_AGE = "age";

    /**
     * Report parameter containing the date of the most recent chapter 688 referral, if one exists
     */
    public static final String PARAM_CH688_DATE = "ch688Date";

    /**
     * Report parameter containing the student's anticipated graduation date from the IEP
     */
    public static final String PARAM_GRADUATION_DATE = "graduationDate";

    /**
     * Additional footer note that should be printed on page
     */
    public static final String FOOTER_NOTE = "footerNote";

    /**
     * Gather data.
     *
     * @return Object
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        IepData iep = (IepData) getFormOwner();

        if (iep != null) {
            /*
             * Anticipated graduation date
             */
            ExtendedDictionaryAttributes iepExtendedDictionary =
                    SpedUtils.getIepDictionary(getOrganization(), getBroker());
            DataDictionary iepDictionary =
                    DataDictionary.getDistrictDictionary(iepExtendedDictionary, getBroker().getPersistenceKey());

            Object gradDateRaw = iep.getFieldValueByAlias(IEP_ANTICIPATED_GRADUATION_DATE, iepDictionary);
            String gradDateString =
                    ReportUtils.getStringValueByAlias(gradDateRaw, IEP_ANTICIPATED_GRADUATION_DATE, iepDictionary,
                            getLocale());

            addParameter(PARAM_GRADUATION_DATE, gradDateString);
            // get age as of iep start date.
            if (iep.getStartDate() != null) {
                PlainDate startDate = iep.getStartDate();
                int ageOfStudent = iep.getStudent().getPerson().getAgeAsOfDate(startDate);
                addParameter(PARAM_AGE, String.valueOf(ageOfStudent));
            }
            /*
             * 688 referral date
             */
            FormDefinition ch688FormDef = FormDefinition.getById(CH688_FORM_ID, getBroker());
            Collection<FormInstance> instances = ch688FormDef.getInstancesForOwner(iep.getOid(), getBroker());

            if (!instances.isEmpty()) {
                FormInstance instance = instances.iterator().next();
                X2BaseBean storage = instance.getStorageObject(getBroker());

                if (storage != null) {
                    DataDictionary ch688Dictionary = DataDictionary.getDistrictDictionary(
                            ch688FormDef.getExtendedDataDictionary(), getBroker().getPersistenceKey());

                    Object referralDateRaw =
                            storage.getFieldValueByAlias(CH688_REFERRAL_DATE_COMPLETED, ch688Dictionary);
                    String referralDateString =
                            ReportUtils.getStringValueByAlias(referralDateRaw, CH688_REFERRAL_DATE_COMPLETED,
                                    ch688Dictionary, getLocale());

                    addParameter(PARAM_CH688_DATE, referralDateString);
                }
            }
        }

        return super.gatherData();
    }

    /**
     * Gets the format ids.
     *
     * @return String[]
     * @see com.follett.fsc.core.k12.tools.reports.MultiPageFormReportJavaSource#getFormatIds()
     */
    @Override
    protected String[] getFormatIds() {
        return new String[] {PAGE_1_FORMAT_ID, PAGE_2_FORMAT_ID};
    }

    /**
     * @see com.follett.fsc.core.k12.tools.reports.MultiPageFormReportJavaSource#preparePage(com.follett.fsc.core.k12.tools.reports.ReportDataGrid)
     */
    @Override
    protected void preparePage(ReportDataGrid grid) {
        super.preparePage(grid);

        grid.set(FOOTER_NOTE, "Mandated Form 28M/9");
    }

    /**
     * Show page numbers.
     *
     * @return true, if successful
     * @see com.follett.fsc.core.k12.tools.reports.MultiPageFormReportJavaSource#showPageNumbers()
     */
    @Override
    protected boolean showPageNumbers() {
        return true;
    }
}
