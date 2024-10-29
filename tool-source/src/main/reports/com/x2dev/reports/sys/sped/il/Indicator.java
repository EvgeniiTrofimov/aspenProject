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

import com.follett.fsc.core.k12.beans.GenericFormChildData;
import com.follett.fsc.core.k12.beans.GenericFormData;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.StudentSchool;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
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
public class Indicator extends BaseFormReportJavaSource {
    private static final String ALIAS_BASED_AGE_APPR_ASMNT = "ch-ind-based-age-appr-asmnt";
    private static final String ALIAS_DOE_DISTRICT_HOME = "DOE DISTRICT HOME";
    private static final String ALIAS_DOE_DISTRICT_SERVICE = "DOE DISTRICT SERVICE";
    private static final String ALIAS_DOE_SCHOOL_HOME = "DOE SCHOOL HOME";
    private static final String ALIAS_DOE_SCHOOL_SERVICE = "DOE SCHOOL SERVICE";
    private static final String ALIAS_GFD_EMBEDDED_LIST_TYPE = "gfd-embedded-list-type";
    private static final String ALIAS_GFD_TR_SERVICE_NAME = "gfd-tr-service-name";
    /* private static final String ALIAS_IND_DATE = "ind-date"; */
    private static final String ALIAS_GOAL_REL_TO_TR_SRV = "ch-ild-goal-rel-to-tr-srv";
    private static final String ALIAS_GOAL_MEASURABLE = "ch-ild-goal-measurable";
    private static final String ALIAS_GOAL_SUB = "ch-ind-goal-sub";
    private static final String ALIAS_GOAL_UPDATE = "ch-ild-goal-update";
    private static final String ALIAS_TR_WILL_MEET_PS_GOAL = "ch-ild-tr-will-meet-ps-goal";

    private static final String GOAL_TYPE_SUBCATEGORY_INDICATOR = "GoalSubcategoryIndicator";

    private static final String KEY_RESIDENT_DISTRICT = "residentDistrict";
    private static final String KEY_RESIDENT_SCHOOL = "residentSchool";
    private static final String KEY_SERVING_DISTRICT = "servingDistrict";
    private static final String KEY_SERVING_SCHOOL = "servingSchool";

    private static final String PARAM_ETHNIC = "ethnic";
    private static final String PARAM_SUBCATEGORY_SERVICES_MAP = "subcategoryServicesMap";
    private static final String PARAM_SERVING_DISTRICT = "servingDistrict";
    private static final String PARAM_SERVING_SCHOOL = "servingSchool";
    private static final String PARAM_RESIDENT_DISTRICT = "residentDistrict";
    private static final String PARAM_RESIDENT_SCHOOL = "residentSchool";


    private static final String EMPTY = "";

    private static final long serialVersionUID = 1L;
    private IlSpedHelper m_ilSpedHelper = new IlSpedHelper();

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
        GenericFormData formData = (GenericFormData) getFormStorage();
        if (iepData != null && formData.getOid() != null) {
            m_ilSpedHelper.initializeHelper(getBroker(), getDictionary());
            for (StudentEnrollment enrollment : iepData.getStudent().getEnrollments()) {
                addParameter(PARAM_RESIDENT_DISTRICT, enrollment.getFieldValueByAlias(ALIAS_DOE_DISTRICT_HOME));
                addParameter(PARAM_SERVING_DISTRICT, enrollment.getFieldValueByAlias(ALIAS_DOE_DISTRICT_SERVICE));
                addParameter(PARAM_RESIDENT_SCHOOL, enrollment.getFieldValueByAlias(ALIAS_DOE_SCHOOL_HOME));
                addParameter(PARAM_SERVING_SCHOOL, enrollment.getFieldValueByAlias(ALIAS_DOE_SCHOOL_SERVICE));
                break;
            }

            GenericFormData genericFormData = (GenericFormData) getFormStorage();
            Map<String, Map<String, String>> subcategoryMap = new HashMap<String, Map<String, String>>();
            Map<String, List<String>> subcategoryServicesMap = new HashMap<String, List<String>>();
            for (GenericFormChildData childData : genericFormData.getGenericFormDataChildren()) {
                String childType =
                        (String) childData.getFieldValueByAlias(ALIAS_GFD_EMBEDDED_LIST_TYPE, getDictionary());
                childType = (childType == null ? EMPTY : childType);
                if (childType.equals(GOAL_TYPE_SUBCATEGORY_INDICATOR)) {
                    String subcategory = (String) childData.getFieldValueByAlias(ALIAS_GOAL_SUB, getDictionary());
                    if (subcategory != null && !subcategory.isEmpty()) {
                        Map<String, String> subcategoryValue = new HashMap<String, String>();
                        subcategoryValue.put(ALIAS_GOAL_MEASURABLE, getFieldByAlias(childData, ALIAS_GOAL_MEASURABLE));
                        subcategoryValue.put(ALIAS_GOAL_UPDATE, getFieldByAlias(childData, ALIAS_GOAL_UPDATE));
                        subcategoryValue.put(ALIAS_BASED_AGE_APPR_ASMNT,
                                getFieldByAlias(childData, ALIAS_BASED_AGE_APPR_ASMNT));
                        subcategoryValue.put(ALIAS_GOAL_REL_TO_TR_SRV,
                                getFieldByAlias(childData, ALIAS_GOAL_REL_TO_TR_SRV));
                        subcategoryValue.put(ALIAS_TR_WILL_MEET_PS_GOAL,
                                getFieldByAlias(childData, ALIAS_TR_WILL_MEET_PS_GOAL));
                        subcategoryMap.put(subcategory, subcategoryValue);
                    }
                } else {
                    ReferenceCode serviceNameCode =
                            (ReferenceCode) m_ilSpedHelper.getObjectByAlias(childData, ALIAS_GFD_TR_SERVICE_NAME);
                    String servuceName = serviceNameCode == null ? EMPTY : serviceNameCode.getCode();
                    List<String> services = subcategoryServicesMap.get(childType);
                    if (services == null) {
                        services = new ArrayList<String>(Arrays.asList(EMPTY, EMPTY, EMPTY));
                        subcategoryServicesMap.put(childType, services);
                    }
                    int lastIndex = services.indexOf(EMPTY);
                    if (lastIndex > -1) {
                        services.add(lastIndex, servuceName);
                    }
                }
            }
            addParameter(ALIAS_GOAL_SUB, subcategoryMap);

            /*
             * PlainDate indicatorDate = m_ilSpedHelper.getPlainDateByBeenAlias(genericFormData,
             * ALIAS_IND_DATE, getDictionary());
             * String schoolYearContextOid =
             * m_ilSpedHelper.getSchoolYearContextOidByDate(indicatorDate);
             */
            PlainDate date = new PlainDate(getFormInstance().getCreatedTime());
            StudentSchool lastOutplacement = m_ilSpedHelper.getLastOutplacement(iepData.getStudent(), null, date);

            StudentEnrollment stdEnrollment = m_ilSpedHelper.getLastStudentEnrollment(iepData.getStudent());
            addParameter(KEY_SERVING_DISTRICT, m_ilSpedHelper.getServingDistrict(lastOutplacement, stdEnrollment));
            addParameter(KEY_SERVING_SCHOOL, m_ilSpedHelper.getServingSchool(lastOutplacement, stdEnrollment));
            addParameter(KEY_RESIDENT_SCHOOL, m_ilSpedHelper.getResidentSchool(stdEnrollment));
            addParameter(KEY_RESIDENT_DISTRICT, m_ilSpedHelper.getResidentDistrict(stdEnrollment));
            String ethnic = m_ilSpedHelper.calculateEthnic(iepData.getStudent().getPerson());
            addParameter(PARAM_ETHNIC, ethnic);

            addParameter(PARAM_SUBCATEGORY_SERVICES_MAP, subcategoryServicesMap);

        }
        return new SimpleFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale());
    }

    /**
     * get Field By Alias From GenericFormChildData.
     *
     * @param childData GenericFormChildData
     * @param alias String
     * @return String
     */
    // helper has same method change in future
    private String getFieldByAlias(GenericFormChildData childData, String alias) {
        String value = (String) childData.getFieldValueByAlias(alias, getDictionary());
        return value == null ? EMPTY : value;
    }
}
