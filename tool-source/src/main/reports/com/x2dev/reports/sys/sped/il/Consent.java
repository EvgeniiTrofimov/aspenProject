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

import com.follett.fsc.core.k12.beans.Contact;
import com.follett.fsc.core.k12.beans.GenericFormChildData;
import com.follett.fsc.core.k12.beans.GenericFormData;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
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
public class Consent extends BeanReport {


    private static final String ALIAS_PARENT_NAME = "parent-name";
    private static final String ALIAS_DOMAIN_TYPES = "domain-types";

    private static final String ALIAS_IS_RELEVANT = "is-relevant";
    private static final String ALIAS_EXIST_INF = "exist-inf";
    private static final String ALIAS_ADD_EVDATA = "add-evdata";
    private static final String ALIAS_SOURCE = "source";

    private static final String ACADEMIC_ACHIEVEMENT = "Academic Achievement";
    private static final String COGNITIVE_FUNCTIONING = "Cognitive Functioning";
    private static final String COMMUNICATION_STATUS = "Communication Status";
    private static final String FUNCTIONAL_PERFORMANCE = "Functional Performance";
    private static final String HEALTH = "Health";
    private static final String HEARING_VISION = "Hearing/Vision";
    private static final String MOTOR_ABILITIES = "Motor Abilities";
    private static final String SOCIAL_EMOTIONAL_STATUS = "Social/Emotional Status";

    private static final String PARAM_ACADEMIC_ACHIEVEMENT = "academAchieve";
    private static final String PARAM_COGNITIVE_FUNCTIONING = "cognitFunc";
    private static final String PARAM_COMMUNICATION_STATUS = "commStatus";
    private static final String PARAM_FUNCTIONAL_PERFORMANCE = "funcPerform";
    private static final String PARAM_HEALTH = "health";
    private static final String PARAM_HEARING_VISION = "hearVision";
    private static final String PARAM_MOTOR_ABILITIES = "motorAbil";
    private static final String PARAM_SOCIAL_EMOTIONAL_STATUS = "socEmotStatus";

    private static final long serialVersionUID = 1L;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        ReportDataGrid grid = new ReportDataGrid();
        grid.append();
        GenericFormData gfd = (GenericFormData) getFormStorage();
        Collection<GenericFormChildData> collections = gfd.getGenericFormDataChildren();

        for (GenericFormChildData childData : collections) {
            List<String> indAssessments = new ArrayList<String>(4);
            String fieldindAssessments = (String) childData.getFieldValueByAlias(ALIAS_DOMAIN_TYPES, getDictionary());
            if (fieldindAssessments != null) {
                indAssessments.add(0, (String) childData.getFieldValueByAlias(ALIAS_IS_RELEVANT, getDictionary()));
                indAssessments.add(1, (String) childData.getFieldValueByAlias(ALIAS_EXIST_INF, getDictionary()));
                indAssessments.add(2, (String) childData.getFieldValueByAlias(ALIAS_ADD_EVDATA, getDictionary()));
                indAssessments.add(3, (String) childData.getFieldValueByAlias(ALIAS_SOURCE, getDictionary()));

                if (fieldindAssessments.equals(ACADEMIC_ACHIEVEMENT)) {
                    addParameter(PARAM_ACADEMIC_ACHIEVEMENT, indAssessments);
                } else if (fieldindAssessments.equals(FUNCTIONAL_PERFORMANCE)) {
                    addParameter(PARAM_FUNCTIONAL_PERFORMANCE, indAssessments);
                } else if (fieldindAssessments.equals(COGNITIVE_FUNCTIONING)) {
                    addParameter(PARAM_COGNITIVE_FUNCTIONING, indAssessments);
                } else if (fieldindAssessments.equals(COMMUNICATION_STATUS)) {
                    addParameter(PARAM_COMMUNICATION_STATUS, indAssessments);
                } else if (fieldindAssessments.equals(HEALTH)) {
                    addParameter(PARAM_HEALTH, indAssessments);
                } else if (fieldindAssessments.equals(HEARING_VISION)) {
                    addParameter(PARAM_HEARING_VISION, indAssessments);
                } else if (fieldindAssessments.equals(MOTOR_ABILITIES)) {
                    addParameter(PARAM_MOTOR_ABILITIES, indAssessments);
                } else if (fieldindAssessments.equals(SOCIAL_EMOTIONAL_STATUS)) {
                    addParameter(PARAM_SOCIAL_EMOTIONAL_STATUS, indAssessments);
                }
            }
        }
        GenericFormData data = (GenericFormData) getFormStorage();
        String parentID = (String) data.getFieldValueByAlias(ALIAS_PARENT_NAME, getDictionary());
        Contact stdContact = (Contact) getBroker().getBeanByOid(Contact.class, parentID);
        addParameter(ALIAS_PARENT_NAME, stdContact);

        return new SimpleFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale());
    }
}
