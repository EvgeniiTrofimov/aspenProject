/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2016 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.reports.statereporting;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatDefinition;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.x2dev.procedures.statereporting.GradesSumData.GradesSumReport;
import com.x2dev.sis.model.beans.HealthScreening;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares the data for the Health Screening Statistics.
 *
 * @author Follett Software Company
 */
public class HealthScreeningStatistics extends GradesSumReport {
    private static final String COMMA = ",";
    private static final String INPUT_PARAM_BEGIN_DATE = "beginDate";
    private static final String INPUT_PARAM_EFD_OIDS = "efdOids";
    private static final String INPUT_PARAM_END_DATE = "endDate";
    private static final String INPUT_PARAM_GRADES_OIDS = "gradesOids";
    private static final String INPUT_PARAM_SCHOOL_OIDS = "schoolOids";
    private static final String INPUT_PARAM_SCREENING_PERIODS = "screeningPeriods";

    private static final String PARAM_DISTRICT_SUMMARY = "districtSummary";

    private static final String TITLE = "Health Services Screening Statistics";

    private boolean m_allSchoolSelected;
    private boolean m_districtSummary;
    private Collection<ExportFormatDefinition> m_formatDefinitions;
    private boolean m_gradesSelected;
    private boolean m_screeningSelected;
    private BeanQuery m_query;
    private Collection<SisSchool> m_schools;
    private boolean m_schoolsSelected;
    private boolean m_screeningPeriodsSelected;
    private Collection<String> m_selectedGrades;
    private Collection<String> m_selectedSchools;

    /**
     * Gets the detail query.
     *
     * @return Bean query
     * @see com.x2dev.procedures.statereporting.GradesSumData.GradesSumReport#getDetailQuery()
     */
    @Override
    protected BeanQuery getDetailQuery() {
        if (m_query == null) {
            X2Criteria criteria = new X2Criteria();
            criteria.addGreaterOrEqualThan(HealthScreening.COL_DATE, getStartDate());
            criteria.addLessOrEqualThan(HealthScreening.COL_DATE, getEndDate());
            if (m_schoolsSelected) {
                criteria.addIn(HealthScreening.REL_STUDENT + ModelProperty.PATH_DELIMITER + SisStudent.COL_SCHOOL_OID,
                        m_selectedSchools);
            } else {
                criteria.addNotEqualTo(HealthScreening.REL_STUDENT + ModelProperty.PATH_DELIMITER +
                        SisStudent.REL_SCHOOL + ModelProperty.PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR,
                        Boolean.TRUE);
                criteria.addNotEqualTo(HealthScreening.REL_STUDENT + ModelProperty.PATH_DELIMITER +
                        SisStudent.REL_SCHOOL + ModelProperty.PATH_DELIMITER + SisSchool.COL_ARCHIVE_INDICATOR,
                        Boolean.TRUE);
                criteria.addAndCriteria(getOrganizationCriteria(HealthScreening.class));
            }

            criteria.addIn(HealthScreening.REL_STUDENT + ModelProperty.PATH_DELIMITER + SisStudent.COL_GRADE_LEVEL,
                    getGrades());
            criteria.addIn(HealthScreening.COL_SCREENING_PERIOD, getScreeningPeriods());
            m_query = new BeanQuery(HealthScreening.class, criteria);
        }
        return m_query;
    }

    /**
     * Gets the end date.
     *
     * @return Plain date
     * @see com.x2dev.procedures.statereporting.GradesSumData.GradesSumReport#getEndDate()
     */
    @Override
    protected PlainDate getEndDate() {
        return (PlainDate) getParameter(INPUT_PARAM_END_DATE);
    }

    /**
     * Gets the format definitions.
     *
     * @return Collection
     * @see com.x2dev.procedures.statereporting.GradesSumData.GradesSumReport#getFormatDefinitions()
     */
    @Override
    protected Collection<ExportFormatDefinition> getFormatDefinitions() {
        if (m_formatDefinitions == null) {
            X2Criteria efdCriteria = new X2Criteria();
            if (m_screeningSelected) {
                Collection<String> efdOids = Arrays.asList(((String) getParameter(INPUT_PARAM_EFD_OIDS)).split(COMMA));
                efdCriteria.addIn(X2BaseBean.COL_OID, efdOids);
            } else {
                efdCriteria.addBeginsWith(ExportFormatDefinition.COL_PROCEDURE_ID, "SR-GRD-SUM-HSC-");
            }
            QueryByCriteria efdQuery = new QueryByCriteria(ExportFormatDefinition.class, efdCriteria);
            m_formatDefinitions = getBroker().getCollectionByQuery(efdQuery);
        }
        return m_formatDefinitions;
    }

    /**
     * Gets the grades.
     *
     * @return Collection
     * @see com.x2dev.procedures.statereporting.GradesSumData.GradesSumReport#getGrades()
     */
    @Override
    protected Collection<String> getGrades() {
        if (m_selectedGrades == null) {
            m_selectedGrades = getReferenceCodes("rtbGradeLevel",
                    m_gradesSelected ? ((String) getParameter(INPUT_PARAM_GRADES_OIDS)) : null);

        }
        return m_selectedGrades;
    }

    /**
     * Gets the schools.
     *
     * @return Collection
     * @see com.x2dev.procedures.statereporting.GradesSumData.GradesSumReport#getSchools()
     */
    @Override
    protected Collection<SisSchool> getSchools() {
        if (m_schools == null) {
            X2Criteria schoolCriteria = new X2Criteria();
            if (m_schoolsSelected) {
                schoolCriteria.addIn(X2BaseBean.COL_OID, m_selectedSchools);
            } else {
                schoolCriteria.addNotEqualTo(SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
                schoolCriteria.addNotEqualTo(SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
                schoolCriteria.addAndCriteria(getOrganizationCriteria(SisSchool.class));
            }
            m_schools = getBroker().getCollectionByQuery(new QueryByCriteria(SisSchool.class, schoolCriteria));
        }
        return m_schools;
    }

    /**
     * Gets the start date.
     *
     * @return Plain date
     * @see com.x2dev.procedures.statereporting.GradesSumData.GradesSumReport#getStartDate()
     */
    @Override
    protected PlainDate getStartDate() {
        return (PlainDate) getParameter(INPUT_PARAM_BEGIN_DATE);
    }

    /**
     * Gets the title.
     *
     * @return String
     * @see com.x2dev.procedures.statereporting.GradesSumData.GradesSumReport#getTitle()
     */
    @Override
    protected String getTitle() {
        return TITLE;
    }

    /**
     * @see com.x2dev.procedures.statereporting.GradesSumData.GradesSumReport#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        m_schoolsSelected = !StringUtils.isEmpty((String) getParameter(INPUT_PARAM_SCHOOL_OIDS));

        if (m_schoolsSelected) {
            m_selectedSchools = Arrays.asList(((String) getParameter(INPUT_PARAM_SCHOOL_OIDS)).split(COMMA));
        }

        if (m_schoolsSelected) {
            X2Criteria activeSchoolsCriteria = new X2Criteria();
            activeSchoolsCriteria.addNotEqualTo(SisSchool.COL_INACTIVE_INDICATOR, Boolean.valueOf(true));
            activeSchoolsCriteria.addNotEqualTo(SisSchool.COL_ARCHIVE_INDICATOR, Boolean.valueOf(true));
            QueryByCriteria activeSchoolsQuery = new QueryByCriteria(SisSchool.class, activeSchoolsCriteria);
            Collection<SisSchool> activeSchools = getBroker().getCollectionByQuery(activeSchoolsQuery);
            if (activeSchools.size() == m_selectedSchools.size()) {
                m_allSchoolSelected = true;
            }
        } else {
            m_allSchoolSelected = true;
        }

        if (m_allSchoolSelected) {
            m_districtSummary = true;
        }

        addParameter(PARAM_DISTRICT_SUMMARY, Boolean.valueOf(m_districtSummary));

        m_gradesSelected = !StringUtils.isEmpty((String) getParameter(INPUT_PARAM_GRADES_OIDS));
        m_screeningSelected = !StringUtils.isEmpty((String) getParameter(INPUT_PARAM_EFD_OIDS));
        m_screeningPeriodsSelected = !StringUtils.isEmpty((String) getParameter(INPUT_PARAM_SCREENING_PERIODS));

        super.initialize();
    }

    /**
     * Return the collection of code values corresponding to a list of reference code oids.
     *
     * @param rtbOid String
     * @param oidsList String
     * @return Collection
     */
    private Collection<String> getReferenceCodes(String rtbOid, String oidsList) {
        Collection<String> codes = new HashSet<String>();

        X2Criteria criteria = new X2Criteria();
        if (oidsList != null) {
            Collection<String> refOids = Arrays.asList(oidsList.split(COMMA));
            criteria.addIn(X2BaseBean.COL_OID, refOids);
        }
        criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, rtbOid);
        QueryByCriteria gradesQuery = new QueryByCriteria(ReferenceCode.class, criteria);
        Collection<ReferenceCode> refCodes = getBroker().getCollectionByQuery(gradesQuery);

        for (ReferenceCode code : refCodes) {
            codes.add(code.getCode());
        }
        return codes;
    }

    /**
     * Return the collection of screening periods selected.
     *
     * @return Collection
     */
    private Collection<String> getScreeningPeriods() {
        return getReferenceCodes("rtbHthScrPrd",
                m_screeningPeriodsSelected ? ((String) getParameter(INPUT_PARAM_SCREENING_PERIODS)) : null);
    }
}
