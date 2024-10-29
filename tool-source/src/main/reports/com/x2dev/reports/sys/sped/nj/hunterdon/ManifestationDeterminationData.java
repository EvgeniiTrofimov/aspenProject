/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2017 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.reports.sys.sped.nj.hunterdon;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.GenericFormData;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.dictionary.ExtendedDictionaryAttributes;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.SisAddress;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.types.PlainDate;
import java.io.ByteArrayInputStream;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.HashMap;
import java.util.Locale;
import net.sf.jasperreports3.engine.data.JRBeanCollectionDataSource;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class ManifestationDeterminationData.
 */
public class ManifestationDeterminationData extends BaseFormReportJavaSource {
    // Aliases
    private static final String ALIAS_DEPT_SUPERVISOR = "DOE DEPT SUPERVISOR";
    private static final String ALIAS_SCHOOL_FAX = "'DOE SCHOOL FAX";
    private static final String ALIAS_STAFF_ROLE = "DOE STAFF ROLE";
    private static final String ALIAS_WORK_PHONE = "DOE STAFF WORK PHONE";

    private static final String COL_PARAMETER_MAP = "parameters";

    private static final String EMPTY_STRING = "";
    private static final String FAX = "FAX";

    private static final String FIELD_BLANK_TYPE = "blankType";
    private static final String FIELD_PROC_SAFE_DATA = "procSafeData";
    private static final String FIELD_RECIPIENT_NAME = "recipientName";
    private static final String FIELD_STUDENT_EIGHTEENTH = "studentEighteenth";
    private static final String FIELD_STUDENT_NAME = "studentName";
    // Report parameters
    private static final String PARAM_CASE_MANAGER = "caseManager";
    private static final String PARAM_CASE_MANAGER_WORKPHONE = "workPhone";
    private static final String PARAM_IEP_DATA = "iepData";
    private static final String PARAM_IS_REDETERMINE_ELIG = "isRedetermineEligibility";
    private static final String PARAM_MEETING = "meeting";
    private static final String PARAM_MEETING_DICTIONARY = "meetingDictionary";
    private static final String PARAM_PROC_SAFE_REPORT = "procSafeReport";
    private static final String PARAM_PROC_SAFE_SUBREPORT_ID = "procSafeSubreportId";
    private static final String PARAM_SCHOOL_ADDRESS_LINE1 = "SKL_ADDRESS_LINE1";
    private static final String PARAM_SCHOOL_ADDRESS_LINE2 = "SKL_ADDRESS_LINE2";
    private static final String PARAM_SCHOOL_FAX_NO = "SKL_FAX_NO";
    private static final String PARAM_SCHOOL_NAME = "SCHOOL_NAME";
    private static final String PARAM_SCHOOL_PHONE_NO = "SKL_PHONE_NO";
    private static final String PARAM_SKL_ADMIN1 = "SKL_ADMIN1";
    private static final String PARAM_SKL_ADMIN2 = "SKL_ADMIN2";
    private static final String PARAM_SKL_ADMIN3 = "SKL_ADMIN3";
    private static final String PARAM_SSS_STAFF = "sssStaff";

    // Member variables
    private IepData m_currentIep;
    private GenericFormData m_meeting;
    private DataDictionary m_meetingDictionary;
    private String m_sssStaffName;

    /**
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {

        String formDate = (String) (((GenericFormData) getFormStorage()).getFieldValueByAlias("manifest-meeting-date",
                getDictionary()));
        String sFormatDate = "";

        if (formDate != null) {
            String delims = "-";
            String[] tokens = formDate.split(delims);

            String pYear = tokens[0];
            String pMonth = tokens[1];
            String pDay = tokens[2];

            sFormatDate = pMonth + "/" + pDay + "/" + pYear;
        }

        formDate = sFormatDate;

        SimpleDateFormat formatter = new SimpleDateFormat("MM/dd/yyyy");
        SimpleDateFormat formatted = new SimpleDateFormat("MMMMM dd, yyyy");

        String dFormDate = formatted.format(formatter.parse(formDate));
        addParameter("FORM_DATE", dFormDate);

        String ia =
                (String) (((GenericFormData) getFormStorage()).getFieldValueByAlias("manifest-2ia", getDictionary()));
        String ib =
                (String) (((GenericFormData) getFormStorage()).getFieldValueByAlias("manifest-2ib", getDictionary()));
        String ic =
                (String) (((GenericFormData) getFormStorage()).getFieldValueByAlias("manifest-2ic", getDictionary()));
        String id =
                (String) (((GenericFormData) getFormStorage()).getFieldValueByAlias("manifest-2id", getDictionary()));
        String ie =
                (String) (((GenericFormData) getFormStorage()).getFieldValueByAlias("manifest-2ie", getDictionary()));
        String schoolfail = (String) (((GenericFormData) getFormStorage()).getFieldValueByAlias("manifest-schoolfail",
                getDictionary()));
        String conductCaused = (String) (((GenericFormData) getFormStorage())
                .getFieldValueByAlias("manifest-conduct-caused", getDictionary()));
        String conductRelationship = (String) (((GenericFormData) getFormStorage())
                .getFieldValueByAlias("manifest-conduct-relationsip", getDictionary()));
        String disabilityManif = (String) (((GenericFormData) getFormStorage())
                .getFieldValueByAlias("manifest-disability-manif", getDictionary()));
        String violation = (String) (((GenericFormData) getFormStorage()).getFieldValueByAlias("manifest-violation",
                getDictionary()));
        String infoSources = (String) (((GenericFormData) getFormStorage())
                .getFieldValueByAlias("manifest-info-sources", getDictionary()));
        String infoComments = (String) (((GenericFormData) getFormStorage())
                .getFieldValueByAlias("manifest-info-comments", getDictionary()));
        String conductComments = (String) (((GenericFormData) getFormStorage())
                .getFieldValueByAlias("manifest-conduct-comments", getDictionary()));
        String procFBA = (String) (((GenericFormData) getFormStorage()).getFieldValueByAlias("manifest-procedures-fba",
                getDictionary()));
        String discAction = (String) (((GenericFormData) getFormStorage())
                .getFieldValueByAlias("manifest-disciplinary-action", getDictionary()));
        String notCommentText = (String) (((GenericFormData) getFormStorage())
                .getFieldValueByAlias("manifest-notcomment", getDictionary()));

        addParameter("conductRelationship", conductRelationship);
        addParameter("conductCaused", conductCaused);
        addParameter("schoolfail", schoolfail);

        if (ia.equals("1")) {
            addParameter("twoia", "X");
        } else {
            addParameter("twoia", "");
        }

        if (ib.equals("1")) {
            addParameter("twoib", "X");
        } else {
            addParameter("twoib", "");
        }

        if (ic.equals("1")) {
            addParameter("twoic", "X");
        } else {
            addParameter("twoic", "");
        }

        if (id.equals("1")) {
            addParameter("twoid", "X");
        } else {
            addParameter("twoid", "");
        }

        if (ie.equals("1")) {
            addParameter("twoie", "X");
        } else {
            addParameter("twoie", "");
        }

        addParameter("violation", violation);
        addParameter("relevantComments", infoSources);
        addParameter("deficiencyRemedy", procFBA);
        addParameter("isComments", conductComments);
        addParameter("isNotComments", infoComments);
        addParameter("fbaProcedure", discAction);

        if (disabilityManif.equals("1")) {
            addParameter("mdMessage", "The behavior was a manifestation of the student's disability. " +
                    " (Therefore, the student may not be removed from their current placement beyond 10 days" +
                    " for disciplinary reasons. However, the student's placement may be changed through the"
                    + " regular IEP review process.)");
        } else {
            addParameter("mdMessage", "The behavior was not a manifestation of the student's disability. State" +
                    " the disciplinary action to be imposed and services to be provided: " + notCommentText);
        }

        ReportDataGrid grid = new ReportDataGrid();
        HashMap<String, Object> parameterMap = new HashMap<String, Object>();

        loadReportHeader();
        // this allows blank form to be produced if requested
        Collection dummy = new ArrayList<X2BaseBean>();
        dummy.add(getOrganization());
        if (getParameter(PARAM_BLANK).equals(Boolean.FALSE)) {
            addParameter(PARAM_IEP_DATA, m_currentIep);
            addParameter(PARAM_MEETING, m_meeting);
            addParameter(PARAM_MEETING_DICTIONARY, m_meetingDictionary);

            if (m_currentIep.getStaff() != null &&
                    m_currentIep.getStaff().getPerson() != null) {
                String caseManager = getFullName(m_currentIep.getStaff().getPerson());
                String caseManagerName = m_currentIep.getStaff().getPerson().getFirstName() + " "
                        + m_currentIep.getStaff().getPerson().getLastName();
                String caseManagerRole = m_currentIep.getStaff().getSpedRole();
                addParameter(PARAM_CASE_MANAGER, caseManager);
                addParameter("caseManagerName", caseManagerName);
                addParameter("caseManagerRole", caseManagerRole);
            }

            // print a notification for each parent
            // the report is completed in the detail band
            boolean hasGuardian = false;

            if (!hasGuardian) {
                grid.append();
                grid.set(FIELD_RECIPIENT_NAME, "*** No Guardian set for this Student's IEP ***");
                grid.set(FIELD_PROC_SAFE_DATA, new JRBeanCollectionDataSource(dummy));
                addMeetingData(grid);
            }
        }
        Report evalSubreport = ReportUtils.getReport((String) getParameter(PARAM_PROC_SAFE_SUBREPORT_ID), getBroker());
        addParameter(PARAM_PROC_SAFE_REPORT, new ByteArrayInputStream(evalSubreport.getCompiledFormat()));

        addParameter(PARAM_IS_REDETERMINE_ELIG,
                getFormDefinition().getId().equals("SPED-NJ-REDETERMINE") ? Boolean.TRUE : Boolean.FALSE);

        // if there are no recipients, use an empty grid to print a blank report
        if (grid.rowCount() == 0) {
            grid.append();
            grid.set(FIELD_BLANK_TYPE, Boolean.FALSE);
            grid.set(FIELD_PROC_SAFE_DATA, new JRBeanCollectionDataSource(dummy));
            grid.append();
            grid.set(FIELD_BLANK_TYPE, Boolean.TRUE);
            grid.set(FIELD_PROC_SAFE_DATA, new JRBeanCollectionDataSource(dummy));
        }
        IepData iepData = (IepData) getFormOwner();
        addParameter("caseManagerPhone", "(908) 284-" + iepData.getStaff().getFieldValueByAlias(ALIAS_WORK_PHONE));
        grid.set(PARAM_CASE_MANAGER, iepData.getStaff());
        grid.set(PARAM_CASE_MANAGER_WORKPHONE, iepData.getStaff() != null
                ? "(908) 284-" + iepData.getStaff().getFieldValueByAlias(ALIAS_WORK_PHONE) : "");
        grid.set(COL_PARAMETER_MAP, parameterMap);
        grid.beforeTop();

        return grid;
    }

    /**
     * Initialize report, m_currentIep (IepData), m_meeting (GenericFormData), and
     * m_meetingDictionary.
     */
    @Override
    protected void initialize() {
        super.initialize();
        m_currentIep = (IepData) getFormOwner();
        m_meeting = (GenericFormData) getFormStorage();
        ExtendedDictionaryAttributes extendDictionary = m_meeting.getExtendedDataDictionary();
        m_meetingDictionary = DataDictionary.getDistrictDictionary(extendDictionary, getBroker().getPersistenceKey());
        addParameter("dateAsStringConverter",
                ConverterFactory.getConverterForClass(PlainDate.class.getName(), Locale.getDefault(), true));
    }

    /**
     * Add meeting data to grid for display on report.
     *
     * @param grid ReportDataGrid
     */
    private void addMeetingData(ReportDataGrid grid) {
        if (m_currentIep.getStudent() != null &&
                m_currentIep.getStudent().getPerson() != null) {
            String studentName = getFullName(m_currentIep.getStudent().getPerson());
            grid.set(FIELD_STUDENT_NAME, studentName);
        }

        PlainDate eighteenthBirthDate = new PlainDate();
        if (m_currentIep.getStudent() != null) {
            Person person = m_currentIep.getStudent().getPerson();
            PlainDate birthDate = person.getDob();
            eighteenthBirthDate = DateUtils.add(birthDate, Calendar.YEAR, 18);
        }
        grid.set(FIELD_STUDENT_EIGHTEENTH, eighteenthBirthDate);
    }

    /**
     * Get full name from a person bean.
     *
     * @param person SisPerson
     * @return fullName
     */
    private String getFullName(SisPerson person) {
        String fullName = "";

        fullName = fullName + person.getFirstName();
        if (!StringUtils.isEmpty(fullName)) {
            fullName = fullName + " ";
        }
        fullName = fullName + person.getLastName();

        return fullName;
    }

    /**
     * Initialize Supervisor name.
     */
    private void initSssName() {
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(ALIAS_STAFF_ROLE);
        if (field != null) {
            String beanPath = field.getJavaName();
            ReferenceTable refTable = field.getReferenceTable();
            Collection<ReferenceCode> codes = refTable.getReferenceCodes();

            String sssCode = null;

            for (ReferenceCode code : codes) {
                String stateCode = code.getStateCode();
                if (!StringUtils.isEmpty(stateCode) &&
                        stateCode.equals("SSS")) {
                    sssCode = code.getCode();
                }
            }

            if (!StringUtils.isEmpty(sssCode)) {
                X2Criteria criteria = new X2Criteria();
                criteria.addEqualTo(beanPath, sssCode);
                QueryByCriteria query = new QueryByCriteria(SisStaff.class, criteria);
                SisStaff sssStaff = (SisStaff) getBroker().getBeanByQuery(query);
                m_sssStaffName = sssStaff.getPerson().getFirstName() + " " + sssStaff.getPerson().getLastName();
            }
        }
    }

    /**
     * Load report header.
     */
    private void loadReportHeader() {
        SisStudent student = m_currentIep.getStudent();
        String admin1 = "";
        addParameter(PARAM_SCHOOL_ADDRESS_LINE1, EMPTY_STRING);
        addParameter(PARAM_SCHOOL_ADDRESS_LINE2, EMPTY_STRING);
        addParameter(PARAM_SCHOOL_PHONE_NO, EMPTY_STRING);
        addParameter(PARAM_SKL_ADMIN1, EMPTY_STRING);
        addParameter(PARAM_SKL_ADMIN2, EMPTY_STRING);
        addParameter(PARAM_SKL_ADMIN3, EMPTY_STRING);
        SisSchool school = student.getSchool();
        String schoolName = school.getName();
        addParameter(PARAM_SCHOOL_NAME, schoolName);
        String fax = (String) school.getFieldValueByAlias(ALIAS_SCHOOL_FAX);
        addParameter(PARAM_SCHOOL_FAX_NO, !StringUtils.isEmpty(fax) ? FAX + fax : EMPTY_STRING);
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
        initSssName();
        addParameter(PARAM_SSS_STAFF, m_sssStaffName);
    }
}
