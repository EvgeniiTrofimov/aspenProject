/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2002-2012 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.reports.sys.sped.nj;

import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.beans.IepData.StatusCode;
import com.x2dev.sis.model.beans.IepMeeting.TypeCode;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import java.io.ByteArrayInputStream;
import java.math.BigDecimal;
import java.util.Collection;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;


/**
 * This class is used by the State of New Jersey for printing the IEP Reports.
 * 
 * @author Follett Software Company
 *
 */
public class TeamSignInSheetData extends BaseFormReportJavaSource {
    private IepData m_currentIep = null;
    private Boolean m_calcService;

    private static final String FAX = "FAX";

    /**
     * The variables below are used for setting the sub report streams.
     */
    private Report m_reportCoverPageNames = null;

    /**
     * The variables below are used for setting the grids of each sub report.
     */
    private ReportDataGrid m_coverPageNamesGrid = null;

    /**
     * Reference code maps
     */
    private Map<String, ReferenceCode> m_placementDecisionRefCodes = null;
    private Map<String, ReferenceCode> m_serviceSettingRefCodes = null;
    private Map<String, ReferenceCode> m_serviceCycleRefCodes = null;

    /**
     * The variables below are the alias variables that are used in the printed report.
     */
    private static final String ALIAS_DEPT_SUPERVISOR = "DOE DEPT SUPERVISOR";
    private static final String ALIAS_LIAISON = "iep-itm-liaison";
    private static final String ALIAS_PLACEMENT_DECISION = "iep-pl-categ";
    private static final String ALIAS_SCHOOL_FAX = "DOE SCHOOL FAX";
    private static final String ALIAS_SERVICE_CODE = "iep-nj-svc-service-code";

    /**
     * The variables below are used for setting the data source fields of the printed report..
     */
    private static final String DATASOURCE_COVERPAGE_NAMES = "DATASOURCE_COVERPAGE_NAMES";

    private static final String TEXT_LIAISON = "Liaison";

    private static final String FIELD_TITLE = "title";
    private static final String FIELD_NAME = "name";

    /**
     * The variables below are input parameters
     */
    private static final String PARAM_CALC_SERVICE = "calcServiceHours";

    /**
     * The variables below are used for setting the parameters of the printed report.
     */
    private static final String PARAM_CASE_MANAGER_NAME = "CASE_MANAGER_NAME";
    private static final String PARAM_CONTACT_NAME = "CONTACT_NAME";
    private static final String PARAM_LOCATION = "location";
    private static final String PARAM_MEETING_DATE = "meetingDate";
    private static final String PARAM_MEETING_REASON = "meetingReason";
    private static final String PARAM_MEETING_TYPE = "MEETING_TYPE";
    private static final String PARAM_MEETING_TYPE_INITIAL = "MEETING_TYPE_INITIAL";
    private static final String PARAM_PRIMARY_DISABILITY = "PRIMARY_DISABILITY";
    private static final String PARAM_SCHOOL_ADDRESS_LINE1 = "SKL_ADDRESS_LINE1";
    private static final String PARAM_SCHOOL_ADDRESS_LINE2 = "SKL_ADDRESS_LINE2";
    private static final String PARAM_SCHOOL_FAX_NO = "SKL_FAX_NO";
    private static final String PARAM_SCHOOL_NAME = "SCHOOL_NAME";
    private static final String PARAM_SCHOOL_PHONE_NO = "SKL_PHONE_NO";
    private static final String PARAM_SECONDARY_CONTACT_NAME = "SECONDARY_CONTACT_NAME";
    private static final String PARAM_SKL_ADMIN1 = "SKL_ADMIN1";
    private static final String PARAM_SKL_ADMIN2 = "SKL_ADMIN2";
    private static final String PARAM_STATUS_CODE = "STATUS_CODE";
    private static final String PARAM_STUDENT_NAME = "STUDENT_NAME";
    private static final String PARAM_SUBREPORT_COVERPAGE_NAMES = "SUB_REPORT_COVERPAGE_NAMES";
    private static final String PARAM_TIME = "time";

    private static final String REFCODE_PARENT = "Parent/Guardian";

    /**
     * The variables below represent the reference codes for the service setting.
     */
    private static final String REFCODE_SETTING_GEN_ED = "01";
    private static final String REFCODE_SETTING_SPED = "02";
    private static final String REFCODE_SETTING_PUB_SEP = "03";
    private static final String REFCODE_SETTING_PRIV = "04";
    private static final String REFCODE_SETTING_PRIV_RES = "05";
    private static final String REFCODE_SETTING_HOME = "06";
    private static final String REFCODE_SETTING_PUB_RES = "07";

    /**
     * The variables below represent the reference state codes for the placement decision.
     */
    private static final String REFCODE_PLACEMENT_EIGHTY = "09";
    private static final String REFCODE_PLACEMENT_FORTY = "10";
    private static final String REFCODE_PLACEMENT_SOME = "11";
    private static final String REFCODE_PLACEMENT_PUB_SEP = "12";
    private static final String REFCODE_PLACEMENT_PRIV_DAY = "13";
    private static final String REFCODE_PLACEMENT_PRIV_RES = "14";
    private static final String REFCODE_PLACEMENT_PUB_RES = "15";
    private static final String REFCODE_PLACEMENT_HOME_INST = "16";

    private static final String OID_REFTABLE_PLACEMENT_DECISION = "rtbNjPlCat";
    private static final String OID_REFTABLE_SERVICE_CYCLE = "rtbNjSvcCycle";
    private static final String OID_REFTABLE_SERVICE_SETTING = "rtbNJSetting";

    /**
     * The variables below represent the report ids of the default IEP sub reports.
     */
    private static final String REPORT_ID_COVERPAGE_NAMES = "SYS-SPED-NJ-IEPCOVER";

    /**
     * Codes used in report
     */
    private static final String CODE_SPED_MODE = "SpecialEd";

    /**
     * Prepares the data source that will be used by the Jasper design. This method is called after
     * <code>initialize(UserDataContainer)</code> and before <code>releaseResources()</code>.
     *
     * @return JRDataSource
     * @throws Exception exception
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        loadCoverPageNames();
        setCoverMeetingTypeAndStatusCode();

        SimpleFormDataSource simpleDataSource =
                new SimpleFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale());
        JRDataSource dataSource = simpleDataSource;

        // determine sped placement if requested
        m_calcService = (Boolean) getParameter(PARAM_CALC_SERVICE);
        if (m_calcService != null ? m_calcService.booleanValue() : false) {
            determinePlacement();
        }

        return dataSource;
    }

    /**
     * Determine placement.
     */
    private void determinePlacement() {
        double spedMinutes = 0.0;
        double peerMinutes = 0.0;
        double peerPercent = 0.0;
        double totalMinutes = 0.0;
        boolean placementDetermined = false;
        String placement = null;

        // populate reference code maps needed by method
        m_serviceSettingRefCodes = getIepRefCodes(OID_REFTABLE_SERVICE_SETTING, ReferenceCode.COL_CODE);
        m_placementDecisionRefCodes = getIepRefCodes(OID_REFTABLE_PLACEMENT_DECISION, ReferenceCode.COL_STATE_CODE);
        m_serviceCycleRefCodes = getIepRefCodes(OID_REFTABLE_SERVICE_CYCLE, ReferenceCode.COL_CODE);

        Collection<IepService> services = m_currentIep.getIepServices();

        for (IepService service : services) {
            String serviceCode = (String) service.getFieldValueByAlias(ALIAS_SERVICE_CODE, getDictionary());
            String settingMode = service.getServiceMode();

            // Include only special education indicated by having a SPED mode
            // Make sure it has a setting code.
            if (CODE_SPED_MODE.equals(settingMode) && !StringUtils.isEmpty(serviceCode)) {
                if (m_serviceSettingRefCodes.containsKey(serviceCode)) {
                    ReferenceCode refCode = m_serviceSettingRefCodes.get(serviceCode);
                    serviceCode = refCode.getStateCode();
                }
                if (REFCODE_SETTING_SPED.equals(serviceCode)) {
                    spedMinutes += calculateServiceMinutes(service);
                } else if (REFCODE_SETTING_GEN_ED.equals(serviceCode)) {
                    peerMinutes += calculateServiceMinutes(service);
                } else if (REFCODE_SETTING_PUB_SEP.equals(serviceCode)) {
                    placement = getServicePlacement(REFCODE_PLACEMENT_PUB_SEP);
                    placementDetermined = true;
                    break;
                } else if (REFCODE_SETTING_PRIV.equals(serviceCode)) {
                    placement = getServicePlacement(REFCODE_PLACEMENT_PRIV_DAY);
                    placementDetermined = true;
                    break;
                } else if (REFCODE_SETTING_PRIV_RES.equals(serviceCode)) {
                    placement = getServicePlacement(REFCODE_PLACEMENT_PRIV_RES);
                    placementDetermined = true;
                    break;
                } else if (REFCODE_SETTING_HOME.equals(serviceCode)) {
                    placement = getServicePlacement(REFCODE_PLACEMENT_HOME_INST);
                    placementDetermined = true;
                    break;
                } else if (REFCODE_SETTING_PUB_RES.equals(serviceCode)) {
                    placement = getServicePlacement(REFCODE_PLACEMENT_PUB_RES);
                    placementDetermined = true;
                    break;
                }
            }
        }

        // calculate total minutes
        totalMinutes = spedMinutes + peerMinutes;

        if (!placementDetermined && totalMinutes > 0.0) {
            final double eightyPercent = 0.8;
            final double fortyPercent = 0.4;

            peerPercent = peerMinutes / totalMinutes;

            if (peerPercent >= eightyPercent) {
                placement = getServicePlacement(REFCODE_PLACEMENT_EIGHTY);
                placementDetermined = true;
            } else if (peerPercent >= fortyPercent) {
                placement = getServicePlacement(REFCODE_PLACEMENT_FORTY);
                placementDetermined = true;
            } else {
                placement = getServicePlacement(REFCODE_PLACEMENT_SOME);
                placementDetermined = true;
            }

        }

        if (placementDetermined) {
            m_currentIep.setFieldValueByAlias(ALIAS_PLACEMENT_DECISION, placement, getDictionary());

            getBroker().saveBeanForced(m_currentIep, getDictionary());
        }
    }

    /**
     * Returns the corresponding service placement.
     *
     * @param key String
     * @return String
     */
    private String getServicePlacement(String key) {
        String servicePlacement = null;
        if (m_placementDecisionRefCodes != null &&
                m_placementDecisionRefCodes.containsKey(key)) {
            ReferenceCode refCode = m_placementDecisionRefCodes.get(key);
            servicePlacement = refCode.getCode();
        }

        return servicePlacement;
    }

    /**
     * Calculates service minutes.
     *
     * @param service IepService
     * @return double
     */
    private double calculateServiceMinutes(IepService service) {
        double serviceMinutes = 0.0;
        BigDecimal frequency = service.getFrequency();
        int duration = service.getDuration();

        // get the total minutes considered from the cycle code
        // the cycle code will have a state value corresponding to the number
        // of days in the cycle. this needs to be resolved into minutes.
        double totalMinutes = 0.0;
        String cycleCode = service.getCycle();
        if (m_serviceCycleRefCodes.containsKey(cycleCode)) {
            ReferenceCode refCode = m_serviceCycleRefCodes.get(cycleCode);
            cycleCode = refCode.getStateCode();
            totalMinutes = Double.parseDouble(cycleCode) * 24.0 * 60.0;
        }

        // calculate the service minutes using the frequency, duration and total minutes
        if (frequency != null) {
            serviceMinutes += frequency.doubleValue() * duration / totalMinutes;
        }

        return serviceMinutes;
    }

    /**
     * This method gets reference codes specified by refTableOid.
     *
     * @param refTableOid String
     * @param keyColumn String
     * @return Map
     */
    private Map<String, ReferenceCode> getIepRefCodes(String refTableOid, String keyColumn) {
        Map<String, ReferenceCode> iepRefCodeMap = null;

        Criteria refCodeCriteria = new Criteria();
        refCodeCriteria.addEqualTo(ReferenceCode.REL_REFERENCE_TABLE + "." + X2BaseBean.COL_OID, refTableOid);
        QueryByCriteria refCodeQuery = new QueryByCriteria(ReferenceCode.class, refCodeCriteria);

        iepRefCodeMap = getBroker().getMapByQuery(refCodeQuery, keyColumn, 32);

        return iepRefCodeMap;
    }

    /**
     * This method sets the meeting type and status code on the cover page of the printed IEP.
     */
    private void setCoverMeetingTypeAndStatusCode() {
        boolean isInitial = false;
        TypeCode typeCode = m_currentIep.getMeetingTypeCodeEnum();

        if (IepMeeting.TypeCode.INITIAL.equals(typeCode)) {
            isInitial = true;
        }

        addParameter(PARAM_MEETING_TYPE_INITIAL, Boolean.valueOf(isInitial));
        String meetingTypeCode = "";

        if (typeCode != null) {
            meetingTypeCode = typeCode.name();
        }

        addParameter(PARAM_MEETING_TYPE, meetingTypeCode);
        StatusCode statusCode = m_currentIep.getStatusCodeEnum();
        String status = "";

        if (statusCode != null) {
            status = statusCode.name();
        }

        addParameter(PARAM_STATUS_CODE, status);
    }

    /**
     * Load cover page names.
     */
    private void loadCoverPageNames() {
        getCoverPageNamesSubRptCriteria();
        getCoverPageNamesCriteria();
    }

    /**
     * This method sets the input stream for the cover page names sub report.
     *
     * @return void
     */
    private void getCoverPageNamesSubRptCriteria() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(Report.COL_ORGANIZATION1_OID, getOrganization().getOid());
        criteria.addEqualTo(Report.COL_ID, REPORT_ID_COVERPAGE_NAMES);
        QueryByCriteria query = new QueryByCriteria(Report.class, criteria);

        m_reportCoverPageNames = (Report) getBroker().getBeanByQuery(query);

        addParameter(PARAM_SUBREPORT_COVERPAGE_NAMES,
                new ByteArrayInputStream(m_reportCoverPageNames.getCompiledFormat()));
    }

    /**
     * This method sets parameters for the names of all people on the cover page
     * of the IEP report form.
     *
     * @return void
     */
    private void getCoverPageNamesCriteria() {
        String studentName = "";
        String caseManagerName = "";
        String schoolName = "";
        String primaryContactName = "";
        String secondaryContactName = "";
        String primaryDisability = "";
        String admin1 = "";
        IepTeamMember primaryContactTeamMember = null;
        IepTeamMember secondaryContactTeamMember = null;
        SisStudent student = m_currentIep.getStudent();
        SisSchool school = student.getSchool();

        if (student != null) {
            studentName = student.getPerson().getFirstName() + " " + student.getPerson().getLastName();
            schoolName = student.getSchool().getName();
            Collection<IepDisability> disabilities = m_currentIep.getIepDisability();
            for (IepDisability disability : disabilities) {
                if (disability.getPrimaryIndicator()) {
                    String disabilityRefCode = disability.getDisabilityCode();
                    if (!StringUtils.isEmpty(primaryDisability)) {
                        primaryDisability = primaryDisability + "; ";
                    }
                    primaryDisability = primaryDisability + disabilityRefCode;
                }
            }
        }

        SisStaff caseManager = m_currentIep.getStaff();
        if (caseManager != null) {
            caseManagerName = caseManager.getPerson().getFirstName() + " " + caseManager.getPerson().getLastName();
        }

        Collection<IepTeamMember> teamMembers = m_currentIep.getTeamMembers();
        m_coverPageNamesGrid = new ReportDataGrid();

        for (IepTeamMember member : teamMembers) {
            if (null != member) {

                String title = member.getMemberRoleCode();
                Person person = member.getPerson();
                String name = "";

                if (person != null) {
                    name = person.getFirstName() + " " + person.getLastName();
                }
                if (title != null && REFCODE_PARENT.equalsIgnoreCase(title)) {
                    if (primaryContactTeamMember == null) {
                        primaryContactTeamMember = member;
                        primaryContactName = name;
                    } else {
                        if ((primaryContactTeamMember.getFormPriority() == null && member.getFormPriority() != null) ||
                                (primaryContactTeamMember.getFormPriority() != null && member.getFormPriority() != null
                                        &&
                                        Integer.parseInt(primaryContactTeamMember.getFormPriority()) > Integer
                                                .parseInt(member.getFormPriority()))) {
                            secondaryContactTeamMember = primaryContactTeamMember;
                            secondaryContactName = primaryContactName;
                            primaryContactTeamMember = member;
                            primaryContactName = name;
                        } else if (secondaryContactTeamMember == null ||
                                (secondaryContactTeamMember.getFormPriority() != null
                                        && member.getFormPriority() != null &&
                                        Integer.parseInt(secondaryContactTeamMember.getFormPriority()) > Integer
                                                .parseInt(member.getFormPriority()))) {
                            secondaryContactTeamMember = member;
                            secondaryContactName = name;
                        }
                    }
                }
                String isLiaison = (String) member.getFieldValueByAlias(ALIAS_LIAISON, getDictionary());
                if (BooleanAsStringConverter.TRUE.equals(isLiaison)) {
                    title = title + ";" + " " + TEXT_LIAISON;
                }

                m_coverPageNamesGrid.append();
                m_coverPageNamesGrid.set(FIELD_TITLE, title);
                m_coverPageNamesGrid.set(FIELD_NAME, name);
            }
        }

        addParameter(PARAM_SCHOOL_PHONE_NO, "");
        addParameter(PARAM_SCHOOL_ADDRESS_LINE1, "");
        addParameter(PARAM_SCHOOL_ADDRESS_LINE2, "");
        String fax = (String) school.getFieldValueByAlias(ALIAS_SCHOOL_FAX);
        addParameter(PARAM_SCHOOL_FAX_NO, !StringUtils.isEmpty(fax) ? FAX + fax : "");

        SisAddress schoolAddress = school.getAddress();
        if (schoolAddress != null) {
            if (!StringUtils.isEmpty(schoolAddress.getPhone01()) ||
                    !StringUtils.isEmpty(schoolAddress.getPhone02())) {
                addParameter(PARAM_SCHOOL_PHONE_NO, (StringUtils.isEmpty(schoolAddress.getPhone01())
                        ? schoolAddress.getPhone02() : schoolAddress.getPhone01()));
            }
            addParameter(PARAM_SCHOOL_ADDRESS_LINE1, schoolAddress.getAddressLine01());
            addParameter(PARAM_SCHOOL_ADDRESS_LINE2, schoolAddress.getAddressLine03());
        }

        addParameter(PARAM_SKL_ADMIN1, "");
        addParameter(PARAM_SKL_ADMIN2, "");

        if (null != school.getAdministrator1()) {
            SisPerson adminPerson1 = school.getAdministrator1().getPerson();
            admin1 = adminPerson1.getFirstName() + " " + adminPerson1.getLastName();
            addParameter(PARAM_SKL_ADMIN1, admin1);
            String superintendent = (String) school.getAdministrator1().getFieldValueByAlias(ALIAS_DEPT_SUPERVISOR);

            if (null != superintendent) {
                String[] admin2 = superintendent.split(",");
                addParameter(PARAM_SKL_ADMIN2, admin2[1] + " " + admin2[0]);
            }
        }

        m_coverPageNamesGrid.beforeTop();
        addParameter(DATASOURCE_COVERPAGE_NAMES, m_coverPageNamesGrid);

        String formDate = String.valueOf(getParameter(PARAM_MEETING_DATE));
        String sFormatDate = "";

        if (formDate != null) {
            String delims = "-";
            String[] tokens = formDate.split(delims);

            String pYear = tokens[0];
            String pMonth = tokens[1];
            String pDay = tokens[2];

            sFormatDate = pMonth + "/" + pDay + "/" + pYear;
        }

        addParameter(PARAM_CASE_MANAGER_NAME, caseManagerName);
        addParameter(PARAM_CONTACT_NAME, primaryContactName);
        addParameter(PARAM_LOCATION, getParameter(PARAM_LOCATION));
        addParameter(PARAM_MEETING_DATE, sFormatDate);
        addParameter(PARAM_MEETING_REASON, getParameter(PARAM_MEETING_REASON));
        addParameter(PARAM_PRIMARY_DISABILITY, primaryDisability);
        addParameter(PARAM_SCHOOL_NAME, schoolName);
        addParameter(PARAM_SECONDARY_CONTACT_NAME, secondaryContactName);
        addParameter(PARAM_STUDENT_NAME, studentName);
        addParameter(PARAM_TIME, getParameter(PARAM_TIME));
    }

    /**
     * The following methods are called, in order, during the life-cycle of a ReportJavaSource
     * instance:
     * <ol>
     * <li><code>saveState(UserDataContainer)</code>
     * <li><code>initialize()</code>
     * <li><code>gatherData()</code>
     * <li><code>releaseResources()</code>
     * </ol>
     *
     * This method is provided as a convenient way for subclasses to initialize member variables
     * before the <code>run()</code> method is called. The default implementation does nothing.
     */
    @Override
    protected void initialize() {
        if (getFormInstance() != null || getFormDefinition() != null) {
            super.initialize();
        } else if (m_currentIep != null) {
            setFormOwner(m_currentIep);
            setFormStorage(m_currentIep);
            setDictionary(DataDictionary.getDistrictDictionary(m_currentIep.getExtendedDataDictionary(),
                    getBroker().getPersistenceKey()));
            addFormParameters();
        }
    }

    /**
     * This method is provided as a way for subclasses to save session state information. The
     * default implementation does nothing.
     *
     * @param userData UserDataContainer
     */
    @Override
    protected void saveState(UserDataContainer userData) {
        super.saveState(userData);
        m_currentIep = userData.getCurrentRecord(IepData.class);
    }
}
