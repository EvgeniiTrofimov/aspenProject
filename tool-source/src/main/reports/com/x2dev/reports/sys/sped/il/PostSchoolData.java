/*
 * ==================================================================== X2 Development Corporation
 * Copyright (c)
 * 2002-2003 X2 Development Corporation. All rights reserved. Redistribution and use in source and
 * binary forms, with or
 * without modification, is not permitted without express written agreement from X2 Development
 * Corporation.
 * ====================================================================
 */
package com.x2dev.reports.sys.sped.il;

import com.follett.fsc.core.k12.beans.GenericFormData;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.Race;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepTeamMember;
import com.x2dev.sis.model.beans.StudentEnrollment;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;

/**
 * Java source for a form-based report. All fields and aliases present on the form storage and owner
 * objects are
 * available for use in the format. The storage and owner objects are retrieved and made available
 * by the superclass -
 * <code>BaseFormReportJavaSource</code>.
 * <p>
 * In the report format, fields and aliases from the storage object can be accessed directly. To
 * retrieve values from
 * the owner object, the prefix <b>"owner"</b> must be present on the field or alias. See
 * <code>SimpleFormDataSource</code> for more information.
 *
 * @author X2 Development Corporation
 */
public class PostSchoolData extends BaseFormReportJavaSource {


    private static final String PARAM_RACES = "races";

    private static final String PARAM_HISPANIC = "hispanic";

    /*
     * Aliases
     */
    private static final String ALIAS_PARENT = "psdata-parent";
    private static final String ALIAS_STD_PROJ_GRAD_DATE = "stdProjGradDate";

    /*
     * Parameters
     */
    private static final String PARAM_DISABILITIES_MAP = "disabilitiesMap";
    private static final String PARAM_GRADUATED_DATE = "graduatedDate";
    private static final String PARAM_GRADUATED_DISTRICT = "graduatedDistrict";
    private static final String PARAM_GRADUATED_SCHOOL = "graduatedSchool";
    private static final String PARAM_PARENT_INFO = "parentInfo";
    private static final String PARAM_STUDENT_PHONES = "studentPhones";

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
        IepData iepData = (IepData) getFormOwner();

        if (iepData != null && !isBlank()) {
            GenericFormData formData = (GenericFormData) getFormStorage();
            m_ilSpedHelper.initializeHelper(getBroker(), getDictionary());
            fillStudentInformation(iepData, formData);
        }
        return new SimpleFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale());
    }

    /**
     * Fill student information.
     *
     * @param iepData IepData
     * @param formData GenericFormData
     */
    private void fillStudentInformation(IepData iepData, GenericFormData formData) {
        Map<String, String> disabilitiesMap = m_ilSpedHelper.getDisabilities(iepData, true);
        addParameter(PARAM_DISABILITIES_MAP, disabilitiesMap);
        Map<String, Object> parentInfo = new HashMap<String, Object>();

        IepTeamMember teamMember = (IepTeamMember) m_ilSpedHelper.getObjectByAlias(formData, ALIAS_PARENT);
        if (teamMember != null) {
            m_ilSpedHelper.fillParentInformation(teamMember, parentInfo);
        }
        Map<String, Object> studentPhones = new HashMap<String, Object>();
        m_ilSpedHelper.fillPhones(iepData.getStudent().getPerson(), studentPhones);
        addParameter(PARAM_STUDENT_PHONES, studentPhones);
        addParameter(PARAM_PARENT_INFO, parentInfo);
        fillStudentInformation(iepData);

    }

    /**
     * Fill student information.
     *
     * @param iepData IepData
     */
    private void fillStudentInformation(IepData iepData) {
        Student student = iepData.getStudent();
        Person person = student.getPerson();
        StudentEnrollment studentEnrollment = m_ilSpedHelper.getLastStudentEnrollment(student);
        String graduatedDate = m_ilSpedHelper.formatDate(student.getFieldValueByAlias(ALIAS_STD_PROJ_GRAD_DATE));

        addParameter(PARAM_GRADUATED_DATE, graduatedDate);
        addParameter(PARAM_GRADUATED_SCHOOL, m_ilSpedHelper.getResidentSchool(studentEnrollment));
        addParameter(PARAM_GRADUATED_DISTRICT, m_ilSpedHelper.getResidentDistrict(studentEnrollment));
        addParameter(PARAM_HISPANIC,
                person.getHispanicLatinoIndicator() ? Boolean.TRUE.toString() : Boolean.FALSE.toString());
        List<String> races = new ArrayList<String>();
        for (Race race : person.getRaces()) {
            races.add(race.getRaceCode());
        }
        addParameter(PARAM_RACES, races);
    }

}
