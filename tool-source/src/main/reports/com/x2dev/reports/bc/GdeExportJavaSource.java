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
package com.x2dev.reports.bc;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.Selection;
import com.follett.fsc.core.k12.beans.SelectionObject;
import com.follett.fsc.core.k12.beans.Staff;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.exports.ExportJavaSource;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.utils.DataGrid;
import com.x2dev.utils.StringUtils;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;

/**
 * Base class for BC's GDE exports. The convention for building the school criteria, conditionally
 * wrapping the values,
 * and general export behavior are all handled via this class.
 *
 * @author Follett Software Company
 */
public class GdeExportJavaSource extends ExportJavaSource {
    private static final long serialVersionUID = 1L;

    /*
     * General input parameters
     */
    private static final String PARAM_INCL_HEADINGS = "includeHeadings";
    private static final String PARAM_SCHOOL_OIDS = "schoolOids";

    /*
     * Field constants
     */
    private static final String VALUE_WRAPPER = "\"";

    /*
     * Members
     */
    private X2Criteria m_schoolCriteria;
    private Map<String, Map<String, Staff>> m_schoolHomeroomToStaffMap;
    private String[] m_dateFormats;
    private List<SimpleDateFormat> m_dateFormatters;

    /*
     * Temporary objects
     */
    private Collection<X2BaseBean> m_temporaryBeans = null;

    /**
     * Gather data.
     *
     * @return DataGrid
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#gatherData()
     */
    @Override
    protected DataGrid gatherData() throws Exception {
        /*
         * This will be implemented by the child class
         */
        throw new UnsupportedOperationException("Subclass must implement this method");
    }

    /**
     * Gets the column names.
     *
     * @return List
     * @see com.x2dev.sis.tools.exports.ExportJavaSource#getColumnNames()
     */
    @Override
    protected List getColumnNames() {
        /*
         * This will be implemented by the child class
         */
        throw new UnsupportedOperationException("Subclass must implement this method");
    }

    /**
     * Gets the column user names.
     *
     * @return List
     * @see com.x2dev.sis.tools.exports.ExportJavaSource#getColumnUserNames()
     */
    @Override
    protected List getColumnUserNames() {
        /*
         * This will be implemented by the child class
         */
        throw new UnsupportedOperationException("Subclass must implement this method");
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
     * Returns homeroom to staff map.
     *
     * @param school School
     * @return Map
     */
    protected Map<String, Staff> getHomeroomToStaffMap(School school) {
        Map<String, Staff> staffMap = m_schoolHomeroomToStaffMap.get(school.getOid());

        if (staffMap == null) {
            staffMap = ReportUtils.buildHomeroomToStaffMap(getBroker(), getOrganization(), school);
            m_schoolHomeroomToStaffMap.put(school.getOid(), staffMap);
        }

        return staffMap;
    }

    /**
     * Returns the criteria built for returning schools.
     *
     * @return X 2 criteria
     */
    protected X2Criteria getSchoolCriteria() {
        return m_schoolCriteria;
    }

    /**
     * Returns the school OIDs inputted by the user.
     *
     * @return String
     */
    protected String getSelectedSchoolOids() {
        return (String) getParameter(PARAM_SCHOOL_OIDS);
    }

    /**
     * Returns the temporaryBeans.
     *
     * @return Collection<X2BaseBean>
     */
    protected Collection<X2BaseBean> getTemporaryBeans() {
        return m_temporaryBeans;
    }

    /**
     * Initialize.
     *
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        m_temporaryBeans = new ArrayList<X2BaseBean>();
        m_schoolHomeroomToStaffMap = new HashMap<>(2048);
        m_dateFormats = new String[] {"dd-MM-yyyy", "MM/dd/yyyy", "dd-MM-yyyy"};

        m_dateFormatters = new ArrayList<>();
        for (String format : m_dateFormats) {
            m_dateFormatters.add(new SimpleDateFormat(format));
        }

        // Set general export parameters
        setLineSeparator(FORMAT_EOL_WINDOWS);
        setUseValueWrappers(false);
        setUseEscapes(false);

        // Set include Header Row depending on export parameter
        Boolean includeHeadings = (Boolean) getParameter(PARAM_INCL_HEADINGS);
        setIncludeHeaderRow(includeHeadings != null ? includeHeadings.booleanValue() : false);

        buildSchoolCriteria();
    }

    /**
     * Removes the temporary beans used for school querying.
     *
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#releaseResources()
     */
    @Override
    protected void releaseResources() {
        /*
         * Cleanup temporary bean data
         */
        for (X2BaseBean bean : m_temporaryBeans) {
            getBroker().deleteBean(bean);
        }
    }

    /**
     * Wraps the value in double-quotes if it does not parse into a date from m_dateFormats.
     *
     * @param value String
     * @return String
     */
    protected String wrap(String value) {
        String formattedValue = value;

        if (!StringUtils.isEmpty(formattedValue)) {
            boolean isDate = false;
            if (formattedValue.length() <= 12) {
                for (SimpleDateFormat sdf : m_dateFormatters) {
                    try {
                        sdf.parse(value);
                        // Break out of loop if we get a valid parse to avoid extra logic
                        isDate = true;
                        break;
                    } catch (ParseException pE) {
                        // Exceptions are expected, checking if value is a valid date.
                        continue;
                    }
                }
            }

            // Wrap value if it could not be parsed as a date
            if (!isDate) {
                // Escape double-quotes with 2 double-quotes
                formattedValue = StringUtils.replaceAll(formattedValue, VALUE_WRAPPER, VALUE_WRAPPER + VALUE_WRAPPER);

                // Wrap value in quotes
                formattedValue = VALUE_WRAPPER + formattedValue + VALUE_WRAPPER;
            }
        }

        return formattedValue;
    }

    /**
     * Builds a criteria for the included schools based on user input.
     */
    private void buildSchoolCriteria() {
        m_schoolCriteria = new X2Criteria();

        if (getSchool() != null) {
            m_schoolCriteria.addEqualTo(X2BaseBean.COL_OID, getSchool().getOid());
        } else {
            String oids = (String) getParameter(PARAM_SCHOOL_OIDS);
            if (!StringUtils.isEmpty(oids)) {
                Collection<String> oidList = StringUtils.convertDelimitedStringToList(oids, ',', true);

                Selection selection = X2BaseBean.newInstance(Selection.class, getBroker().getPersistenceKey());
                for (String oid : oidList) {
                    SelectionObject object =
                            X2BaseBean.newInstance(SelectionObject.class, getBroker().getPersistenceKey());
                    object.setObjectOid(oid);
                    selection.addToSelectionObjects(object);
                }
                getBroker().saveBean(selection);

                m_temporaryBeans.add(selection);
                m_temporaryBeans.addAll(selection.getSelectionObjects());

                Criteria subCriteria = new Criteria();
                subCriteria.addEqualTo(SelectionObject.COL_SELECTION_OID, selection.getOid());

                m_schoolCriteria.addIn(X2BaseBean.COL_OID,
                        new SubQuery(SelectionObject.class, SelectionObject.COL_OBJECT_OID, subCriteria));
            } else {
                m_schoolCriteria.addNotEqualTo(SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
                m_schoolCriteria.addNotEqualTo(SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
                m_schoolCriteria.addAndCriteria(getOrganizationCriteria(SisSchool.class));
            }
        }
    }
}
