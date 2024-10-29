/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2011 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.reports.sys.sped.ri;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.FormInstance;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.StudentContact;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.SisAddress;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.business.sped.SpedUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.types.PlainDate;
import java.util.Date;
import java.util.List;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Java source for the RI SPED Permission to Evaluate report.
 *
 * @author X2 Development Corporation
 */
public class RiGenericSpedFormsData extends BaseFormReportJavaSource {
    // Report parameters
    public static final String PARAM_AS_OF_GRADE = "asOfGrade";
    public static final String PARAM_AS_OF_SCHOOL = "asOfSchool";
    public static final String PARAM_AS_OF_SCHOOL_ADR1 = "asOfSchoolAdr1";
    public static final String PARAM_AS_OF_SCHOOL_ADR2 = "asOfSchoolAdr2";
    public static final String PARAM_AS_OF_SCHOOL_ADR3 = "asOfSchoolAdr3";
    public static final String PARAM_AS_OF_SCHOOL_PHONE = "asOfSchoolPhn";
    public static final String PARAM_FORM_DATE = "formDate";
    public static final String PARAM_RECIPIENT_KEY = "recipient";
    public static final String PARAM_RECIPIENT_VAL = "Parent/Guardian/Adult Student";

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws Exception exception
     * @see com.x2dev.sis.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        return new SimpleFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale());
    }

    /**
     * Initialize.
     *
     * @see com.x2dev.sis.tools.reports.BaseFormReportJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        super.initialize();

        IepData iep = (IepData) getFormOwner();
        PlainDate startDate = (getFormInstance() != null && getFormInstance().getCreatedTime() > 0
                ? new PlainDate(getFormInstance().getCreatedTime())
                : new PlainDate());
        SisStudent student = iep.getStudent();
        StudentEnrollment lastEnrollment = getStudentEnrollmentAsOf(startDate, student);
        DistrictSchoolYearContext asOfContext = getContextAsOf(startDate);

        List<String> gradeLevels = null;
        if (lastEnrollment != null &&
                asOfContext != null) {
            gradeLevels = StudentManager.getMatchingGradeLevels(StudentManager.getMaxGradeLevel(getBroker()),
                    lastEnrollment.getYog(),
                    asOfContext.getSchoolYear(),
                    StudentManager.buildGradeLevelMap(getBroker()));
        }

        if (gradeLevels != null && !gradeLevels.isEmpty()) {
            addParameter(PARAM_AS_OF_GRADE, gradeLevels.get(0));
        } else {
            addParameter(PARAM_AS_OF_GRADE, student.getGradeLevel());
        }

        if (lastEnrollment != null) {
            addParameter(PARAM_AS_OF_SCHOOL, lastEnrollment.getSchool().getName());
            SisAddress address = lastEnrollment.getSchool().getAddress();
            if (address != null) {
                addParameter(PARAM_AS_OF_SCHOOL_ADR1, address.getAddressLine01());
                String addressLine2 = StringUtils.coalesce(address.getAddressLine02(), "");
                addParameter(PARAM_AS_OF_SCHOOL_ADR2, addressLine2);
                addParameter(PARAM_AS_OF_SCHOOL_ADR3, address.getAddressLine03());
                addParameter(PARAM_AS_OF_SCHOOL_PHONE, StringUtils.coalesce(address.getPhone01(), ""));
            } else {
                addParameter(PARAM_AS_OF_SCHOOL_ADR1, "");
                addParameter(PARAM_AS_OF_SCHOOL_ADR2, "");
                addParameter(PARAM_AS_OF_SCHOOL_ADR3, "");
                addParameter(PARAM_AS_OF_SCHOOL_PHONE, "");
            }
        } else {
            addParameter(PARAM_AS_OF_SCHOOL, student.getSchool().getName());
            SisAddress address = student.getSchool().getAddress();
            if (address != null) {
                addParameter(PARAM_AS_OF_SCHOOL_ADR1, address.getAddressLine01());
                String addressLine2 = StringUtils.coalesce(address.getAddressLine02(), "");
                addParameter(PARAM_AS_OF_SCHOOL_ADR2, addressLine2);
                addParameter(PARAM_AS_OF_SCHOOL_ADR3, address.getAddressLine03());
                addParameter(PARAM_AS_OF_SCHOOL_PHONE, StringUtils.coalesce(address.getPhone01(), ""));
            } else {
                addParameter(PARAM_AS_OF_SCHOOL_ADR1, "");
                addParameter(PARAM_AS_OF_SCHOOL_ADR2, "");
                addParameter(PARAM_AS_OF_SCHOOL_ADR3, "");
                addParameter(PARAM_AS_OF_SCHOOL_PHONE, "");
            }
        }

        FormInstance formInstance = getFormInstance();
        long timestamp = 0;
        if (formInstance != null) {
            timestamp = formInstance.getCreatedTime();
        }
        Date formDate = new Date();
        if (timestamp > 0) {
            formDate = new Date(timestamp);
        }

        addParameter(PARAM_FORM_DATE, formDate);

        List<StudentContact> contacts = SpedUtils.getStudentContacts(iep, getBroker(), 1);
        if (!contacts.isEmpty()) {
            Person contact = contacts.get(0).getPerson();
            addParameter(PARAM_RECIPIENT_KEY, contact.getFirstName() + " " + contact.getLastName());
        } else {
            addParameter(PARAM_RECIPIENT_KEY, PARAM_RECIPIENT_VAL);
        }
    }

    /**
     * Gets the student enrollment as of.
     *
     * @param startDate PlainDate
     * @param student SisStudent
     * @return Student enrollment
     */
    private StudentEnrollment getStudentEnrollmentAsOf(PlainDate startDate, SisStudent student) {
        StudentEnrollment lastEnrollment = null;
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(StudentEnrollment.COL_STUDENT_OID, student.getOid());
        criteria.addLessOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, startDate);
        criteria.addEqualTo(StudentEnrollment.COL_ENROLLMENT_TYPE, "E");

        QueryByCriteria query = new QueryByCriteria(StudentEnrollment.class, criteria);
        query.addOrderByDescending(StudentEnrollment.COL_ENROLLMENT_DATE);

        lastEnrollment = (StudentEnrollment) getBroker().getBeanByQuery(query);

        if (lastEnrollment == null) {
            criteria.addEqualTo(StudentEnrollment.COL_STUDENT_OID, student.getOid());
            criteria.addEqualTo(StudentEnrollment.COL_ENROLLMENT_TYPE, "E");

            query = new QueryByCriteria(StudentEnrollment.class, criteria);
            query.addOrderByAscending(StudentEnrollment.COL_ENROLLMENT_DATE);

            lastEnrollment = (StudentEnrollment) getBroker().getBeanByQuery(query);
        }
        return lastEnrollment;
    }

    /**
     * Gets the context as of.
     *
     * @param startDate PlainDate
     * @return District school year context
     */
    private DistrictSchoolYearContext getContextAsOf(PlainDate startDate) {
        DistrictSchoolYearContext asOfContext = null;
        X2Criteria criteria = new X2Criteria();
        criteria.addLessOrEqualThan(DistrictSchoolYearContext.COL_START_DATE, startDate);
        criteria.addGreaterOrEqualThan(DistrictSchoolYearContext.COL_END_DATE, startDate);

        QueryByCriteria query = new QueryByCriteria(DistrictSchoolYearContext.class, criteria);

        asOfContext = (DistrictSchoolYearContext) getBroker().getBeanByQuery(query);
        return asOfContext;
    }
}
