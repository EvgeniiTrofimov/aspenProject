/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2014 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.reports.bc;

import static com.x2dev.reports.bc.StudentAchievementDataExtractData.LIMITED_GRADES;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.exports.ExportJavaSource;
import com.x2dev.reports.bc.StudentAchievementDataExtractData.EXPORT_FIELDS;
import com.x2dev.reports.bc.StudentAchievementDataExtractData.SADEReportDataGrid;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.tools.reports.StudentContextReportHelper;
import com.x2dev.utils.DataGrid;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Data source for the "SADE" export.
 *
 * @author X2 Development Corporation
 */
public class StudentAchievementDataExtractExport extends ExportJavaSource {
    /*
     * Input parameters
     */
    private static final String PARAM_PERIODS_INCLUDED = "includePeriod";
    private static final String PARAM_LIMIT_GRADES = "limitGrades";
    private static final String PARAM_SORT_ORDER = "sortOrder";
    private static final String PARAM_SCHOOL_OIDS = "schoolOids";

    /*
     * Members
     */
    private boolean m_includePreviousDates;
    private Boolean m_limitGrades = Boolean.FALSE;
    private Collection<SisSchool> m_schools = null;
    private String[] m_sortOrder = null;

    private String[] m_defaultSortOrder = new String[] {EXPORT_FIELDS.FIELD_PEN.getFieldId(),
            EXPORT_FIELDS.FIELD_COURSE_CODE.getFieldId(),
            EXPORT_FIELDS.FIELD_ACTIVE_DATE.getFieldId(),
            EXPORT_FIELDS.FIELD_CLASS_IDENTIFIER.getFieldId()};

    /**
     * Gather data.
     *
     * @return DataGrid
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#gatherData()
     */
    @Override
    protected DataGrid gatherData() throws Exception {
        StudentContextReportHelper helper =
                new StudentContextReportHelper(getOrganization(), getCurrentContext(), getBroker());
        SADEReportDataGrid sadeGrid = new StudentAchievementDataExtractData.SADEReportDataGrid(helper, getBroker());

        sadeGrid.initializeDateRanges(getOrganization().getRootOrganization());
        sadeGrid.setDateFormat("yyyMMdd");
        sadeGrid.setSchools(m_schools);
        sadeGrid.setIncludePrevious(m_includePreviousDates);
        sadeGrid.setLimitGrades(m_limitGrades);
        sadeGrid.setLimitedGradeList(Arrays.asList(LIMITED_GRADES));
        sadeGrid.evaluateGrid();
        sadeGrid.removeInvalidRows();

        for (EXPORT_FIELDS field : EXPORT_FIELDS.values()) {
            if (!field.getInExtract()) {
                sadeGrid.deleteColumn(field.getFieldId());
            }
        }

        sadeGrid.sort(Arrays.asList(m_sortOrder), false);

        sadeGrid.beforeTop();
        return sadeGrid;
    }

    /**
     * Gets the column names.
     *
     * @return List
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getColumnNames()
     */
    @Override
    protected List getColumnNames() {
        List<String> columnNames = new ArrayList(EXPORT_FIELDS.values().length);

        for (EXPORT_FIELDS field : EXPORT_FIELDS.values()) {
            if (field.getInExtract()) {
                columnNames.add(field.getFieldId());
            }
        }

        return columnNames;
    }

    /**
     * Gets the column user names.
     *
     * @return List
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getColumnUserNames()
     */
    @Override
    protected List getColumnUserNames() {
        List<String> columnUserNames = new ArrayList(EXPORT_FIELDS.values().length);

        for (EXPORT_FIELDS field : EXPORT_FIELDS.values()) {
            if (field.getInExtract()) {
                columnUserNames.add(field.getFieldName());
            }
        }

        return columnUserNames;
    }

    /**
     * Gets the comment.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getComment()
     */
    @Override
    protected String getComment() {
        return null;
    }

    /**
     * Gets the custom file name.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#getCustomFileName()
     */
    @Override
    public String getCustomFileName() {
        return getSchool() != null ? (getSchool().getSchoolId() + ".csv") : null;
    }

    /**
     * Gets the header.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getHeader()
     */
    @Override
    protected String getHeader() {
        return null;
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        setUseValueWrappers(false);
        setIncludeHeaderRow(false);
        setLineSeparator(FORMAT_EOL_WINDOWS);

        m_schools = getSchools();

        String includedPeriods = (String) getParameter(PARAM_PERIODS_INCLUDED);
        if ("previous".equals(includedPeriods)) {
            m_includePreviousDates = true;
        } else {
            m_includePreviousDates = false;
        }

        if (getParameter(PARAM_LIMIT_GRADES) != null) {
            m_limitGrades = (Boolean) getParameter(PARAM_LIMIT_GRADES);
        }

        if (getParameter(PARAM_SORT_ORDER) != null) {
            m_sortOrder = getUserSortOrderAsStringArray((String) getParameter(PARAM_SORT_ORDER));
        } else {
            m_sortOrder = m_defaultSortOrder;
        }
    }

    /**
     * Loads the schools used in the export.
     * 
     * @return Collection<SisSchool>
     */
    private Collection<SisSchool> getSchools() {
        X2Criteria criteria = new X2Criteria();

        if (getSchool() != null) {
            criteria.addEqualTo(X2BaseBean.COL_OID, getSchool().getOid());
        } else {
            String oids = (String) getParameter(PARAM_SCHOOL_OIDS);
            if (!StringUtils.isEmpty(oids)) {
                Collection<String> oidList = StringUtils.convertDelimitedStringToList(oids, ',', true);
                criteria.addIn(X2BaseBean.COL_OID, oidList);
            } else {
                criteria.addNotEqualTo(SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
                criteria.addNotEqualTo(SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
                criteria.addAndCriteria(getOrganizationCriteria(SisSchool.class));
            }
        }

        return getBroker().getCollectionByQuery(new QueryByCriteria(SisSchool.class, criteria));
    }
}
