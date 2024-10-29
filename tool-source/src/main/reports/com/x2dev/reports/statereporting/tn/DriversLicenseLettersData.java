/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
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

package com.x2dev.reports.statereporting.tn;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Contact;
import com.follett.fsc.core.k12.beans.StudentContact;
import com.follett.fsc.core.k12.beans.StudentSchool;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNMultiYearHelper.Strategy;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNStudentMultiYearHelper;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.X2BaseException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Data source for the "Drivers License Letters" report.
 *
 * @author X2 Development Corporation
 */
public class DriversLicenseLettersData extends ReportJavaSourceNet {
    private static final String NO_PERFMON4J_INSTRUMENTATION = ""; 

    private static final String ALIAS_SKL_STATE_ID = "DOE SCHOOL STATE ID";

    /**
     * Name for the "active only" report parameter. The value is an Boolean.
     */
    public static final String ACTIVE_ONLY_PARAM = "activeOnly";

    public static final String INPUT_ALL_SCHOOLS = "allSchools";
    private static final String INPUT_SCHOOLS = "schoolOids";
    public static final String MARKER_SCHOOL_NAME = "${schoolName}";
    public static final String PARAM_NON_DISCRIMINATION = "nonDiscrimination";

    /**
     * Name for the "selection" report parameter. The value is a String.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the "selection value" report parameter. The value is a String.
     */
    public static final String QUERY_STRING_PARAM = "queryString";

    /**
     * Name for the "secondary students" report parameter. The value is a Boolean.
     */
    public static final String SECONDARY_STUDENT_PARAM = "secondaryStudent";

    /**
     * Name for the "sort" report parameter. The value is a String.
     */
    public static final String SORT_PARAM = "sort";

    /**
     * Report Fields
     */
    private static final String FIELD_CONTACT = "contact";
    private static final String FIELD_NON_DISCRIMINATION = "nonDiscrimination";
    private static final String FIELD_ORGANIZATION = "organization";
    private static final String FIELD_SCHOOL = "school";
    private static final String FIELD_STUDENT = "student";

    /**
     * Fields
     */
    private SisStudent m_currentStudent;
    private DataDictionary m_dictionary;
    private String m_nonDiscrimination;
    private Collection<SisSchool> m_schools;
    private Collection<String> m_schoolOids;
    private X2Criteria m_stdCriteria;
    private Map<String, Collection<SisStudent>> m_stdMap;
    private TNStudentMultiYearHelper m_multiYearHelper;

    /**
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        ReportDataGrid dataGrid = new ReportDataGrid();

        for (Map.Entry<String, Collection<SisStudent>> entry : m_stdMap.entrySet()) {
            SisSchool skl = (SisSchool) getBroker().getBeanByOid(SisSchool.class, entry.getKey());
            String nonDiscrimination = null;
            if (!StringUtils.isEmpty(m_nonDiscrimination)) {
                nonDiscrimination = m_nonDiscrimination.replace(MARKER_SCHOOL_NAME, skl.getName());
            }
            for (SisStudent std : entry.getValue()) {
                dataGrid.append();
                dataGrid.set(FIELD_ORGANIZATION, getOrganization());
                dataGrid.set(FIELD_SCHOOL, skl);
                dataGrid.set(FIELD_STUDENT, std);
                dataGrid.set(FIELD_CONTACT, getContact(std));
                dataGrid.set(FIELD_NON_DISCRIMINATION, nonDiscrimination);
            }
        }

        dataGrid.beforeTop();
        return dataGrid;
    }

    /**
     * Initialize the export.
     * Set up the student history helper.
     *
     * @throws X2BaseException exception
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();
        m_multiYearHelper = new TNStudentMultiYearHelper(getOrganization(), getCurrentContext(), getBroker());
        m_dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        m_schools = getSchools();
        m_schoolOids = new ArrayList<String>();
        for (SisSchool school : m_schools) {
            m_schoolOids.add(school.getOid());
        }
        buildStudentCriteria();
        buildStudentsMap();
        m_nonDiscrimination = (String) getParameter(PARAM_NON_DISCRIMINATION);
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.
     *      UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) {
        /*
         * If we're in the context of a single student, print the report for just that student
         */
        m_currentStudent = userData.getCurrentRecord(SisStudent.class);
    }

    /**
     * Adjust user bean path.
     *
     * @param originalPath String
     * @return String
     */
    private String adjustUserBeanPath(String originalPath) {
        if (!originalPath.startsWith(SELECTION_SPECIAL_CASE_PREFIX) && !originalPath.startsWith(ALIAS_PREFIX)) {
            return m_multiYearHelper.getAdjustedPath(originalPath);
        }
        return originalPath;
    }

    /**
     * Build student criteria based on input parameters.
     */
    private void buildStudentCriteria() {
        /*
         * Build the criteria based on user input
         */
        m_stdCriteria = new X2Criteria();

        if (m_currentStudent != null) {
            m_stdCriteria.addEqualTo(X2BaseBean.COL_OID, m_currentStudent.getOid());
        } else {
            String queryBy = (String) getParameter(QUERY_BY_PARAM);
            String queryByString = (String) getParameter(QUERY_STRING_PARAM);
            if (!StringUtils.isEmpty(queryByString)) {
                m_stdCriteria.addAndCriteria(m_multiYearHelper.getWithAttributesCriteria());
            }
            addUserCriteria(m_stdCriteria, adjustUserBeanPath(queryBy), queryByString, null, null);

            if (!queryBy.contains(CURRENT_KEY)) {
                m_multiYearHelper.adjustCriteria(m_stdCriteria, Strategy.IN, SisStudent.COL_SCHOOL_OID, m_schoolOids);

                /*
                 * Include secondary students of the school if needed.
                 */
                if (((Boolean) getParameter(SECONDARY_STUDENT_PARAM)).booleanValue()) {
                    Criteria orCriteria = new Criteria();

                    SubQuery secondarySub = new SubQuery(StudentSchool.class,
                            StudentSchool.COL_STUDENT_OID,
                            StudentManager.getSecondaryStudentCriteria(null, getCurrentContext().getOid(),
                                    getSchool().getOid(), null, null, getBroker().getPersistenceKey()));

                    orCriteria.addIn(X2BaseBean.COL_OID, secondarySub);

                    m_stdCriteria.addOrCriteria(orCriteria);
                }
            }

            boolean activeOnly = ((Boolean) getParameter(ACTIVE_ONLY_PARAM)).booleanValue();
            if (activeOnly) {
                m_stdCriteria.addAndCriteria(m_multiYearHelper.getActiveStudentCriteria());
            }
        }
        if (m_schoolOids.size() == 0) {
            m_stdCriteria = null;
        }
    }

    /**
     * Build student map keyed on school oids.
     */
    private void buildStudentsMap() {
        m_stdMap = new HashMap<String, Collection<SisStudent>>();

        if (m_stdCriteria != null) {
            QueryByCriteria query = new QueryByCriteria(SisStudent.class, m_stdCriteria);
            Collection<SisStudent> students = getBroker().getCollectionByQuery(query);

            m_stdMap = new HashMap<String, Collection<SisStudent>>();
            for (SisStudent student : students) {
                String schoolOid =
                        (String) m_multiYearHelper.getFieldValueByBeanPath(student, SisStudent.COL_SCHOOL_OID);
                Collection<SisStudent> currentStudents = m_stdMap.get(schoolOid);
                if (currentStudents == null) {
                    currentStudents = new ArrayList<SisStudent>();
                    m_stdMap.put(schoolOid, currentStudents);
                }
                currentStudents.add(student);
            }
        }
    }

    /**
     * Gets the contact.
     *
     * @param std SisStudent
     * @return Contact
     */
    private Contact getContact(SisStudent std) {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(StudentContact.REL_STUDENT + ModelProperty.PATH_DELIMITER + X2BaseBean.COL_OID,
                std.getOid());
        criteria.addEqualTo(StudentContact.COL_LIVES_WITH_INDICATOR, Boolean.TRUE);

        QueryByCriteria query = new QueryByCriteria(StudentContact.class, criteria);
        query.addOrderByAscending(StudentContact.COL_EMERGENCY_PRIORITY);

        StudentContact contact = (StudentContact) getBroker().getBeanByQuery(query);
        if (contact != null && contact.getContact() != null) {
            return contact.getContact();
        }
        return std.getPrimaryContact() != null ? std.getPrimaryContact().getContact() : null;
    }

    /**
     * Return collection of schools.
     *
     * @return Collection<SisSchool> collection of schools
     */
    private Collection<SisSchool> getSchools() {
        Collection<SisSchool> schools = null;
        Object objIsAllSchools = getParameter(INPUT_ALL_SCHOOLS);
        boolean isAllSchools = objIsAllSchools == null ? false : ((Boolean) objIsAllSchools).booleanValue();
        if (isAllSchools) {
            X2Criteria schoolCriteria = new X2Criteria();

            schoolCriteria.addNotEqualTo(SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
            schoolCriteria.addNotEqualTo(SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);

            DataDictionaryField aliasSklStateIDField =
                    m_dictionary.findDataDictionaryFieldByAlias(ALIAS_SKL_STATE_ID);
            schoolCriteria.addNotEmpty(aliasSklStateIDField.getJavaName(), getBroker().getPersistenceKey());

            QueryByCriteria schoolQuery = new QueryByCriteria(SisSchool.class, schoolCriteria);
            schoolQuery.addOrderByAscending(SisSchool.COL_NAME);
            schools = getBroker().getCollectionByQuery(schoolQuery);
        } else {
            schools = new LinkedList();
            Object objSchools = getParameter(INPUT_SCHOOLS);
            String schoolOids = objSchools == null ? "" : (String) objSchools;
            if (!StringUtils.isEmpty(schoolOids)) {
                List<String> oids = Arrays.asList(schoolOids.split(","));
                X2Criteria schoolCriteria = new X2Criteria();

                schoolCriteria.addIn(X2BaseBean.COL_OID, oids);

                QueryByCriteria schoolQuery = new QueryByCriteria(SisSchool.class, schoolCriteria);
                schoolQuery.addOrderByAscending(SisSchool.COL_NAME);
                schools = getBroker().getCollectionByQuery(schoolQuery);
            }
        }

        return schools;
    }
}
