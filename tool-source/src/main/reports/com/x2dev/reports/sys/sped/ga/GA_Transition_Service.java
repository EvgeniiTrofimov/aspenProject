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
 */package com.x2dev.reports.sys.sped.ga;

import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.SisPerson;

/**
 * The Class GA_Transition_Service.
 */
public class GA_Transition_Service extends BaseFormReportJavaSource {
    String ALIAS_GRADUATION_DATE = "DOE GRADUATION DATE";

    String CONSTANT_SPACE = " ";

    String PARAM_GRADUATION_DATE = "graduationDate";
    String PARAM_STUDENT_NAME = "studentName";

    /**
     * TODO javadoc + get.
     *
     * @return Object
     * @throws Exception exception
     */
    @Override
    protected Object gatherData() throws Exception {
        IepData iepData = (IepData) getFormOwner();

        String graduationDate = (String) iepData.getStudent().getFieldValueByAlias(ALIAS_GRADUATION_DATE);
        addParameter(PARAM_GRADUATION_DATE, graduationDate);

        SisPerson person = iepData.getStudent().getPerson();
        if (person != null) {
            addParameter(PARAM_STUDENT_NAME, person.getFirstName() + CONSTANT_SPACE + person.getLastName());
        }

        SimpleFormDataSource dataSource =
                new SimpleFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale());
        return dataSource;
    }

}
