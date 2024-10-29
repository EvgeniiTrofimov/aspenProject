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
package com.x2dev.reports.sys.sped.ri;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.k12.beans.GenericFormData;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepMeeting;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.types.PlainDate;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Collection;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;

/**
 * RI SPED Summary Of Performance Report java source.
 *
 * @author X2 Development Corporation
 */
public class ESYDescisionMaking extends BaseFormReportJavaSource {
    /*
     * Factors
     */
    private final String FACTOR1_STRING = "Regression/Recoupment Analysis";
    private final String FACTOR2_STRING = "The nature and or severity of the child's disability.";
    private final String FACTOR3_STRING = "The child's rate of learning";
    private final String FACTOR4_STRING = "The degree of progress towards " +
            "(a) IEP goals and objectives or " +
            "(b) for children transitioning from part C " +
            "services (Early Intervention) to part B " +
            "services, consideration of the degree of " +
            "progress toward Individual Family Service " +
            "Plan (IFSP) outcomes.";
    private final String FACTOR5_STRING =
            "The child's stereotypic, ritualistic, aggressive or self injurious interfering behavior";
    private final String FACTOR6_STRING = "The physical needs of the child";
    private final String FACTOR7_STRING = "Emerging skills breakthrough opportunities";
    private final String FACTOR8_STRING = "Ability of the child to interact with typically developing peers";
    private final String FACTOR9_STRING = "Student's post school outcomes";
    private final String FACTOR10_STRING = "Other special circumstances as determined by the IEP team";

    /*
     * Criterias
     */
    private final String CRITERIA1_STRING = "Is there data that indicates to the IEP team that there is serious " +
            "potential for regression of academic achievement or functional skills " +
            "beyond a reasonable period of recoupment?";
    private final String CRITERIA2_STRING = "Is there information regarding the nature of severity of disability " +
            "of the child that indicates to the IEP Team that there is a need to " +
            "provide services in the identified goal area of concern?";
    private final String CRITERIA3_STRING = "Is there information that indicates the child's rate of learning " +
            "is such that child will not make sufficient progress toward IEP " +
            "annual goals";
    private final String CRITERIA4_STRING = "Is there data indicating the child's degree of progress towards meeting " +
            "(a) IEP goals and objectives or " +
            "(b) IFSP outcomes would jeopardize the child's access to " +
            "a Free and Appropriate Public Education (FAPE) during the " +
            "regular school year, if ESY services are not provided?";
    private final String CRITERIA5_STRING = "Does the child demonstrate behaviors that would jeopardize " +
            "the child's access to a Free and Appropriate Public Education " +
            "(FAPE) during the regular school year, if ESY services are not provided?";
    private final String CRITERIA6_STRING = "Is there data indicating that the child's physical needs " +
            "require ESY services?";
    private final String CRITERIA7_STRING = "Is there information that indicates the child is at a critical " +
            "stage of learning and/or a critical area of learning where failure " +
            "to provide a service beyond the normal school year will jeopardize " +
            "the child's capacity to acquire essential skills?";
    private final String CRITERIA8_STRING = "Is there information to indicate that a break in programming " +
            "will jeopardize the child's ability to interact with typically " +
            "developing peers?";
    private final String CRITERIA9_STRING = "Is there information to indicate that a break in programming " +
            "will jeopardize the student's attainment of his/her IEP goals " +
            "and/or provision of transition services, in the areas of education " +
            "and training, employment, and where appropriate independent living?";
    private final String CRITERIA10_STRING = "Are there any special circumstances to indicate that without ESY " +
            "services the child's access to FAPE during the regular school year " +
            "would be jeopardized?";

    /*
     * Parameters
     */
    private final String PARAM_DATE_FORMAT = "dateFormat";
    private final String PARAM_IEPDATA = "iepData";
    private final String PARAM_MEETINGDATES = "meetingDates";
    private final String PARAM_ORGANIZATION = "organization";
    private final String PARAM_STUDENT = "student";

    /*
     * Fields
     */
    private final String FACTOR_FIELD = "factor";
    private final String CRITERIA_FIELD = "criteria";
    private final String DATASOURCE_FIELD = "datasource";
    private final String YESNO_FIELD = "yesno";
    private final String GOALS_NUMBERS_FIELD = "goal";

    /*
     * Data Dictionary Aliases
     */
    private final String ESY_FACTOR1_DATASOURCE_ALIAS = "esy-factor1-ds";
    private final String ESY_FACTOR1_YESNO_ALIAS = "esy-factor1-yn";
    private final String ESY_FACTOR1_GOALS_ALIAS = "esy-factor1-goals";
    private final String ESY_FACTOR2_DATASOURCE_ALIAS = "esy-factor2-ds";
    private final String ESY_FACTOR2_YESNO_ALIAS = "esy-factor2-yn";
    private final String ESY_FACTOR2_GOALS_ALIAS = "esy-factor2-goals";
    private final String ESY_FACTOR3_DATASOURCE_ALIAS = "esy-factor3-ds";
    private final String ESY_FACTOR3_YESNO_ALIAS = "esy-factor3-yn";
    private final String ESY_FACTOR3_GOALS_ALIAS = "esy-factor3-goals";
    private final String ESY_FACTOR4_DATASOURCE_ALIAS = "esy-factor4-ds";
    private final String ESY_FACTOR4_YESNO_ALIAS = "esy-factor4-yn";
    private final String ESY_FACTOR4_GOALS_ALIAS = "esy-factor4-goals";
    private final String ESY_FACTOR5_DATASOURCE_ALIAS = "esy-factor5-ds";
    private final String ESY_FACTOR5_YESNO_ALIAS = "esy-factor5-yn";
    private final String ESY_FACTOR5_GOALS_ALIAS = "esy-factor5-goals";
    private final String ESY_FACTOR6_DATASOURCE_ALIAS = "esy-factor6-ds";
    private final String ESY_FACTOR6_YESNO_ALIAS = "esy-factor6-yn";
    private final String ESY_FACTOR6_GOALS_ALIAS = "esy-factor6-goals";
    private final String ESY_FACTOR7_DATASOURCE_ALIAS = "esy-factor7-ds";
    private final String ESY_FACTOR7_YESNO_ALIAS = "esy-factor7-yn";
    private final String ESY_FACTOR7_GOALS_ALIAS = "esy-factor7-goals";
    private final String ESY_FACTOR8_DATASOURCE_ALIAS = "esy-factor8-ds";
    private final String ESY_FACTOR8_YESNO_ALIAS = "esy-factor8-yn";
    private final String ESY_FACTOR8_GOALS_ALIAS = "esy-factor8-goals";
    private final String ESY_FACTOR9_DATASOURCE_ALIAS = "esy-factor9-ds";
    private final String ESY_FACTOR9_YESNO_ALIAS = "esy-factor9-yn";
    private final String ESY_FACTOR9_GOALS_ALIAS = "esy-factor9-goals";
    private final String ESY_FACTOR10_DATASOURCE_ALIAS = "esy-factor10-ds";
    private final String ESY_FACTOR10_YESNO_ALIAS = "esy-factor10-yn";
    private final String ESY_FACTOR10_GOALS_ALIAS = "esy-factor10-goals";

    /**
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        DataDictionary dictionary =
                DataDictionary.getDistrictDictionary(getFormDefinition().getExtendedDataDictionary(),
                        getBroker().getPersistenceKey());

        GenericFormData formData = (GenericFormData) getFormStorage();
        IepData iepData = (IepData) getFormOwner();
        SisStudent student = iepData.getStudent();

        ReportDataGrid esyGrid = new ReportDataGrid();

        addParameter(PARAM_DATE_FORMAT, new SimpleDateFormat("MM/dd/yyyy"));
        addParameter(PARAM_IEPDATA, iepData);
        addParameter(PARAM_MEETINGDATES, getMeetingDates(iepData));
        addParameter(PARAM_ORGANIZATION, getOrganization());
        addParameter(PARAM_STUDENT, student);

        esyGrid.append();
        esyGrid.set(FACTOR_FIELD, FACTOR1_STRING);
        esyGrid.set(CRITERIA_FIELD, CRITERIA1_STRING);
        esyGrid.set(DATASOURCE_FIELD, formData.getFieldValueByAlias(ESY_FACTOR1_DATASOURCE_ALIAS, dictionary));
        esyGrid.set(YESNO_FIELD, formData.getFieldValueByAlias(ESY_FACTOR1_YESNO_ALIAS, dictionary));
        esyGrid.set(GOALS_NUMBERS_FIELD, formData.getFieldValueByAlias(ESY_FACTOR1_GOALS_ALIAS, dictionary));

        esyGrid.append();
        esyGrid.set(FACTOR_FIELD, FACTOR2_STRING);
        esyGrid.set(CRITERIA_FIELD, CRITERIA2_STRING);
        esyGrid.set(DATASOURCE_FIELD, formData.getFieldValueByAlias(ESY_FACTOR2_DATASOURCE_ALIAS, dictionary));
        esyGrid.set(YESNO_FIELD, formData.getFieldValueByAlias(ESY_FACTOR2_YESNO_ALIAS, dictionary));
        esyGrid.set(GOALS_NUMBERS_FIELD, formData.getFieldValueByAlias(ESY_FACTOR2_GOALS_ALIAS, dictionary));

        esyGrid.append();
        esyGrid.set(FACTOR_FIELD, FACTOR3_STRING);
        esyGrid.set(CRITERIA_FIELD, CRITERIA3_STRING);
        esyGrid.set(DATASOURCE_FIELD, formData.getFieldValueByAlias(ESY_FACTOR3_DATASOURCE_ALIAS, dictionary));
        esyGrid.set(YESNO_FIELD, formData.getFieldValueByAlias(ESY_FACTOR3_YESNO_ALIAS, dictionary));
        esyGrid.set(GOALS_NUMBERS_FIELD, formData.getFieldValueByAlias(ESY_FACTOR3_GOALS_ALIAS, dictionary));

        esyGrid.append();
        esyGrid.set(FACTOR_FIELD, FACTOR4_STRING);
        esyGrid.set(CRITERIA_FIELD, CRITERIA4_STRING);
        esyGrid.set(DATASOURCE_FIELD, formData.getFieldValueByAlias(ESY_FACTOR4_DATASOURCE_ALIAS, dictionary));
        esyGrid.set(YESNO_FIELD, formData.getFieldValueByAlias(ESY_FACTOR4_YESNO_ALIAS, dictionary));
        esyGrid.set(GOALS_NUMBERS_FIELD, formData.getFieldValueByAlias(ESY_FACTOR4_GOALS_ALIAS, dictionary));

        esyGrid.append();
        esyGrid.set(FACTOR_FIELD, FACTOR5_STRING);
        esyGrid.set(CRITERIA_FIELD, CRITERIA5_STRING);
        esyGrid.set(DATASOURCE_FIELD, formData.getFieldValueByAlias(ESY_FACTOR5_DATASOURCE_ALIAS, dictionary));
        esyGrid.set(YESNO_FIELD, formData.getFieldValueByAlias(ESY_FACTOR5_YESNO_ALIAS, dictionary));
        esyGrid.set(GOALS_NUMBERS_FIELD, formData.getFieldValueByAlias(ESY_FACTOR5_GOALS_ALIAS, dictionary));

        esyGrid.append();
        esyGrid.set(FACTOR_FIELD, FACTOR6_STRING);
        esyGrid.set(CRITERIA_FIELD, CRITERIA6_STRING);
        esyGrid.set(DATASOURCE_FIELD, formData.getFieldValueByAlias(ESY_FACTOR6_DATASOURCE_ALIAS, dictionary));
        esyGrid.set(YESNO_FIELD, formData.getFieldValueByAlias(ESY_FACTOR6_YESNO_ALIAS, dictionary));
        esyGrid.set(GOALS_NUMBERS_FIELD, formData.getFieldValueByAlias(ESY_FACTOR6_GOALS_ALIAS, dictionary));

        esyGrid.append();
        esyGrid.set(FACTOR_FIELD, FACTOR7_STRING);
        esyGrid.set(CRITERIA_FIELD, CRITERIA7_STRING);
        esyGrid.set(DATASOURCE_FIELD, formData.getFieldValueByAlias(ESY_FACTOR7_DATASOURCE_ALIAS, dictionary));
        esyGrid.set(YESNO_FIELD, formData.getFieldValueByAlias(ESY_FACTOR7_YESNO_ALIAS, dictionary));
        esyGrid.set(GOALS_NUMBERS_FIELD, formData.getFieldValueByAlias(ESY_FACTOR7_GOALS_ALIAS, dictionary));

        esyGrid.append();
        esyGrid.set(FACTOR_FIELD, FACTOR8_STRING);
        esyGrid.set(CRITERIA_FIELD, CRITERIA8_STRING);
        esyGrid.set(DATASOURCE_FIELD, formData.getFieldValueByAlias(ESY_FACTOR8_DATASOURCE_ALIAS, dictionary));
        esyGrid.set(YESNO_FIELD, formData.getFieldValueByAlias(ESY_FACTOR8_YESNO_ALIAS, dictionary));
        esyGrid.set(GOALS_NUMBERS_FIELD, formData.getFieldValueByAlias(ESY_FACTOR8_GOALS_ALIAS, dictionary));

        esyGrid.append();
        esyGrid.set(FACTOR_FIELD, FACTOR9_STRING);
        esyGrid.set(CRITERIA_FIELD, CRITERIA9_STRING);
        esyGrid.set(DATASOURCE_FIELD, formData.getFieldValueByAlias(ESY_FACTOR9_DATASOURCE_ALIAS, dictionary));
        esyGrid.set(YESNO_FIELD, formData.getFieldValueByAlias(ESY_FACTOR9_YESNO_ALIAS, dictionary));
        esyGrid.set(GOALS_NUMBERS_FIELD, formData.getFieldValueByAlias(ESY_FACTOR9_GOALS_ALIAS, dictionary));

        esyGrid.append();
        esyGrid.set(FACTOR_FIELD, FACTOR10_STRING);
        esyGrid.set(CRITERIA_FIELD, CRITERIA10_STRING);
        esyGrid.set(DATASOURCE_FIELD, formData.getFieldValueByAlias(ESY_FACTOR10_DATASOURCE_ALIAS, dictionary));
        esyGrid.set(YESNO_FIELD, formData.getFieldValueByAlias(ESY_FACTOR10_YESNO_ALIAS, dictionary));
        esyGrid.set(GOALS_NUMBERS_FIELD, formData.getFieldValueByAlias(ESY_FACTOR10_GOALS_ALIAS, dictionary));

        esyGrid.beforeTop();
        return esyGrid;
    }

    /**
     * Returns a String representation of meeting dates for the passed iep.
     *
     * @param iep IepData
     * @return String
     */
    private String getMeetingDates(IepData iep) {
        String dates = "";
        Criteria criteria = new Criteria();
        criteria.addEqualTo(IepMeeting.COL_IEP_DATA_OID, iep.getOid());

        com.follett.fsc.core.framework.persistence.SubQuery query =
                new SubQuery(IepMeeting.class, IepMeeting.COL_DATE, criteria);

        DateFormat format = new SimpleDateFormat("MM/dd/yyyy");
        for (PlainDate date : (Collection<PlainDate>) getBroker().getSubQueryCollectionByQuery(query)) {
            dates += format.format(date) + ", ";
        }

        return dates.substring(0, Math.max(dates.length() - 2, 0));
    }
}
