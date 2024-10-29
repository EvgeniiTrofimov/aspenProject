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
package com.x2dev.reports.sys.sped.nh;

import com.follett.fsc.core.framework.persistence.IterableQuery;
import com.follett.fsc.core.k12.beans.StudentContact;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepDisability;
import com.x2dev.sis.model.beans.IepMeeting;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Java source for the Administrative Data Sheet. This class provides access to the following
 * information on the format:
 * <ul>
 * <li>The form storage and owner objects as provided by <code>SimpleBeanDataSource</code>
 * <li>The student's first two contacts in ascending priority order as report parameters
 * <li>The student's disabilities (up to three) as report parameters
 * <li>The student's latest IEP meeting as a report parameter
 * </ul>
 *
 * @author X2 Development Corporation
 */
public class AdministrativeDataSheetData_Nh extends BaseFormReportJavaSource {
    /**
     *
     */
    private static final long serialVersionUID = 1L;


    /**
     * Report parameter IDs
     * TODO: create separate comments for each param
     */
    public static final String PARAM_CONTACT_0 = "contact0";


    public static final String PARAM_CONTACT_1 = "contact1";
    public static final String PARAM_DISABILITY_0 = "disability0";
    public static final String PARAM_DISABILITY_1 = "disability1";
    public static final String PARAM_DISABILITY_2 = "disability2";
    public static final String PARAM_MEETING = "meeting";

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        IepData iep = null;

        if (!isBlank()) {
            iep = (IepData) getFormOwner();
            prepareContacts(iep);
            prepareMeeting(iep);
            prepareDisabilities(iep);
        } else {
            iep = new IepData(getBroker().getPersistenceKey());
        }

        return new SimpleFormDataSource(iep, getFormOwner(), getDictionary(), getLocale());
    }

    /**
     * Retrieves the student's disabilities, and sets them as report parameters.
     *
     * @param sourceIep IepData
     */
    private void prepareDisabilities(IepData sourceIep) {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(IepDisability.COL_IEP_DATA_OID, sourceIep.getOid());

        @SuppressWarnings("resource")
        IterableQuery<IepDisability> query =
                new IterableQuery<IepDisability>(IepDisability.class, criteria, getBroker());
        query.addOrderByDescending(IepDisability.COL_PRIMARY_INDICATOR);

        int count = 0;
        for (IepDisability disability : query) {
            if (disability != null) {
                if (count == 0) {
                    addParameter(PARAM_DISABILITY_0, disability);
                } else if (count == 1) {
                    addParameter(PARAM_DISABILITY_1, disability);
                } else if (count == 2) {
                    addParameter(PARAM_DISABILITY_2, disability);
                } else {
                    break;
                }
                count++;
            }
        }
    }

    /**
     * Retrieves two of the student's contacts, and stores them as report parameters.
     *
     * @param sourceIep IepData
     */
    private void prepareContacts(IepData sourceIep) {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(StudentContact.COL_STUDENT_OID, sourceIep.getStudentOid());

        @SuppressWarnings("resource")
        IterableQuery<StudentContact> query =
                new IterableQuery<StudentContact>(StudentContact.class, criteria, getBroker());

        query.addOrderByAscending(StudentContact.COL_EMERGENCY_PRIORITY);

        int count = 0;
        for (StudentContact contact : query) {
            if (count == 0) {
                addParameter(PARAM_CONTACT_0, contact);
            } else if (count == 1) {
                addParameter(PARAM_CONTACT_1, contact);
            } else {
                break;
            }

            count++;
        }
    }

    /**
     * Retrieves the IEP's latest meeting, and stores it as a report parameter.
     *
     * @param sourceIep IepData
     */
    private void prepareMeeting(IepData sourceIep) {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(IepMeeting.COL_IEP_DATA_OID, sourceIep.getOid());

        QueryByCriteria query = new QueryByCriteria(IepMeeting.class, criteria);
        query.addOrderByDescending(IepMeeting.COL_DATE);

        addParameter(PARAM_MEETING, getBroker().getBeanByQuery(query));
    }
}
