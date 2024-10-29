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
package com.x2dev.reports.sys.sped.il;

import com.follett.fsc.core.k12.beans.GenericFormData;
import com.follett.fsc.core.k12.beans.StudentSchool;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.utils.types.PlainDate;
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
public class NotificationOfConference extends BaseFormReportJavaSource {

    private static final String KEY_RESIDENT_SCHOOL = "residentSchool";
    private static final String KEY_SERVING_SCHOOL = "servingSchool";

    private IlSpedHelper m_ilSpedHelper = new IlSpedHelper();
    private static final long serialVersionUID = 1L;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        GenericFormData formData = (GenericFormData) getFormStorage();
        if (!(formData == null || formData.getOid() == null)) {
            IepData iepData = (IepData) getFormOwner();
            m_ilSpedHelper.initializeHelper(getBroker(), getDictionary());
            /*
             * PlainDate date = m_ilSpedHelper.getPlainDateByBeenAlias(formData, ALIAS_NOTIF_DATE);
             * 
             * String schoolYearContextOid = m_ilSpedHelper.getSchoolYearContextOidByDate(date);
             */
            PlainDate date = new PlainDate(getFormInstance().getCreatedTime());
            StudentSchool lastOutplacement = m_ilSpedHelper.getLastOutplacement(iepData.getStudent(), null, date);
            StudentEnrollment stdEnrollment = m_ilSpedHelper.getLastStudentEnrollment(iepData.getStudent());
            addParameter(KEY_RESIDENT_SCHOOL, m_ilSpedHelper.getResidentSchool(stdEnrollment));
            addParameter(KEY_SERVING_SCHOOL, m_ilSpedHelper.getServingSchool(lastOutplacement, stdEnrollment));

        }
        return new SimpleFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale());
    }
}
