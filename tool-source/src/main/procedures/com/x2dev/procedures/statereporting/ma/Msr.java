/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2018 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.ma;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.X2BaseException;
import java.util.Collection;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * This class implements the data export for MSR export.
 *
 * @author X2 Development Corporation
 */
public class Msr extends StateReportData {

    private static final String ALIAS_ORG_DISTRICT_ID = "DOE District ID";
    private static final String ALIAS_SKL_DISTRICT_ID = "skl-sif-district-id";
    private static final String ALIAS_SKL_SCHOOL_ID = "DOE 15";
    private static final String ALIAS_STD_SPED_PLACEMENT = "DOE 34";
    private static final String ALIAS_STD_STATUS_FIELD = "DOE Status";

    private static final String DOE_STATUS_FIELD_REPORT_CODE = "Report";

    private static final String PARAM_EXCLUDE_PREREG_STUDENTS = "excludePreReg";

    /**
     * Name for the "require report status" parameter. The value is a Boolean.
     */
    private static final String PARAM_REQUIRE_REPORT_STATUS = "requireReportStatus";

    /**
     * Name for the "SPED only" parameter. The value is a Boolean.
     */
    private static final String PARAM_SPED_ONLY = "spedOnly";

    private static final String PATTERN_DISTRICT_ID = "__DISTRICT_ID__";
    private static final String PATTERN_SCHOOL_ID = "__SCHOOL_ID__";

    private static final String HEADER_DISTRICT_PATTERN = "DOEHEADER,DISTRICT=" + PATTERN_DISTRICT_ID + "\n" +
            "DOEHEADER,ELEMENTS=DOE001,DOE003,DOE004,DOE005,DOE999,DOE006,DOE008,DOE009,DOE014\n" +
            "DOEHEADER,DATEFORMAT=D10\n" +
            "DOEHEADER,GENDERFORMAT,M=Male,F=Female,N=Nonbinary" +
            "\n";
    private static final String HEADER_SCHOOL_PATTERN = "DOEHEADER,SCHOOL=" + PATTERN_SCHOOL_ID + "\n" +
            "DOEHEADER,ELEMENTS=DOE001,DOE003,DOE004,DOE005,DOE999,DOE006,DOE008,DOE009,DOE014\n" +
            "DOEHEADER,DATEFORMAT=XXX\n" +
            "DOEHEADER,GENDERFORMAT,M=Male,F=Female,N=Nonbinary" +
            "\n";

    /**
     * Gets the heading.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getHeading()
     */
    @Override
    public String getHeading() {
        if (isSchoolContext()) {
            String schoolId = (String) getSchool().getFieldValueByAlias(ALIAS_SKL_SCHOOL_ID);
            String schoolHeader = HEADER_SCHOOL_PATTERN.replace(PATTERN_SCHOOL_ID, schoolId);
            return schoolHeader;
        }
        String districtHeader =
                HEADER_DISTRICT_PATTERN.replace(PATTERN_DISTRICT_ID, getDistrictId() == null ? "" : getDistrictId());
        return districtHeader;
    }

    String m_districtId = null;
    String m_fieldOrgDistId = null;
    String m_fieldSklDistId = null;
    String m_fieldStdSpedPlacement = null;
    String m_fieldStdStatus = null;
    String m_preRegCode = null;

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();

        m_preRegCode =
                PreferenceManager.getPreferenceValue(getOrganization(), SisPreferenceConstants.STUDENT_PREREG_CODE);

        m_fieldOrgDistId = translateAliasToJavaName(ALIAS_ORG_DISTRICT_ID, true);
        m_fieldSklDistId = translateAliasToJavaName(ALIAS_SKL_DISTRICT_ID, true);
        m_fieldStdSpedPlacement = translateAliasToJavaName(ALIAS_STD_SPED_PLACEMENT, true);
        m_fieldStdStatus = translateAliasToJavaName(ALIAS_STD_STATUS_FIELD, true);

        StudentHistoryHelper helper = new StudentHistoryHelper(this);
        helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);

        X2Criteria studentCriteria = helper.getStudentCriteria();
        studentCriteria.addEmpty(SisStudent.COL_STATE_ID, getBroker().getPersistenceKey());

        Boolean spedOnly = (Boolean) getParameter(PARAM_SPED_ONLY);
        if (spedOnly != null && spedOnly.booleanValue()) {
            studentCriteria.addNotEmpty(m_fieldStdSpedPlacement, getBroker().getPersistenceKey());
            studentCriteria.addNotEqualTo(m_fieldStdSpedPlacement, "Not special ed");
        }

        Boolean requireReportStatus = (Boolean) getParameter(PARAM_REQUIRE_REPORT_STATUS);
        if (requireReportStatus != null && requireReportStatus.booleanValue()) {
            studentCriteria.addEqualTo(m_fieldStdStatus, DOE_STATUS_FIELD_REPORT_CODE);
        }

        Boolean excludePreRegStudents = (Boolean) getParameter(PARAM_EXCLUDE_PREREG_STUDENTS);
        if (excludePreRegStudents != null && excludePreRegStudents.booleanValue()) {
            studentCriteria.addNotEqualTo(SisStudent.COL_ENROLLMENT_STATUS, m_preRegCode);
        }

        setQuery(helper.getStudentQuery(false));

    }

    /**
     * Gets the district id.
     *
     * @return String
     */
    private String getDistrictId() {
        String districtId = null;

        if (isSchoolContext()) {
            districtId = (String) getSchool().getFieldValueByBeanPath(m_fieldSklDistId);
        } else {
            X2Criteria schoolsCriteria = new X2Criteria();
            if (m_fieldSklDistId != null) {
                schoolsCriteria.addNotEmpty(m_fieldSklDistId, getBroker().getPersistenceKey());
                QueryByCriteria schoolsQuery = new QueryByCriteria(SisSchool.class, schoolsCriteria);
                Collection<SisSchool> schools = getBroker().getCollectionByQuery(schoolsQuery);
                for (SisSchool school : schools) {
                    districtId = (String) school.getFieldValueByBeanPath(m_fieldSklDistId);
                }
            }
        }
        if (districtId == null && m_fieldOrgDistId != null) {
            districtId = (String) getOrganization().getFieldValueByBeanPath(m_fieldOrgDistId);
        }

        return districtId;
    }
}
