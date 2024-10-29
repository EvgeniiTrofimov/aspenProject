/*
 * ====================================================================
 *
 * Follett School Solutions
 *
 * Copyright (c) 2020 Follett School Solutions
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett School Solutions.
 *
 * ====================================================================
 */

package com.x2dev.reports.statereporting.ct;


import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.framework.persistence.adjusters.ListSelectionAdjuster;
import com.follett.fsc.core.k12.beans.DataFieldConfig;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.Selection;
import com.follett.fsc.core.k12.beans.SelectionObject;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.PreferenceSet;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.WebUtils;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.X2RuntimeException;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Copy of SYS-ADM-011 with CT spec of DOE EXCLUDE STD input option.
 *
 * Prepares the data for the "Enrollment Breakdown" report. This report shows enrollment totals
 * for a school with a user-defined breakdown. The user has access to group the totals by any
 * enabled
 * field from the STUDENT, PERSON, or PERSON_ADDRESS tables.
 *
 * @author FSS
 */
public class CTEnrollmentBreakdownData extends ReportJavaSourceNet {
    private static final long serialVersionUID = 1L;

    /**
     * Name for the "end date" input parameter. This value is a PlainDate.
     */
    public static final String DATE_PARAM = "date";

    /**
     * Name for the "gender breakdown" input parameter. This value is a Boolean.
     */
    public static final String GENDER_PARAM = "gender";

    /**
     * Name for the "selection" input parameter. This value is an Integer.
     */
    public static final String GROUP_BY_PARAM = "dataFieldConfigOid";

    /**
     * Name for the "school breakdown" input parameter. This value is a Boolean.
     */
    public static final String GROUP_BY_SCHOOL_PARAM = "groupBySchool";

    /**
     * Name for the "Include DOE Excluded Students" input parameter. This value is a Boolean.
     */
    public static final String PARAM_INCLUDE_DOE_EXCLUDE_STUDENTS = "includeExcludeStudents";

    /**
     * Name for the "sort" input parameter. This value is an Integer.
     */
    public static final String SORT_PARAM = "sort";

    // Report parameters
    private static final String HEADER_COLUMNS_PARAM = "columnHeader";

    // Aliases
    private static final String ALIAS_DOE_EXCLUDE_STD = "DOE EXCLUDE STD";

    // Grid fields
    private static final String CODE_FIELD = "codeField";
    private static final String CODE_TOTAL = "codeTotal";
    private static final String SCHOOL_NAME_FIELD = "schoolName";

    private PlainDate m_date;
    private Map<String, String> m_enrollmentSchoolOidByStudentOid;
    private Map m_gradeLevelMap;
    private Collection<String> m_orderByClauses;
    private int m_sort;
    private boolean m_useGender;
    private boolean m_useSchool;

    private DataDictionary m_dictionary;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        m_date = (PlainDate) getParameter(DATE_PARAM);
        m_orderByClauses = new ArrayList<String>();
        m_sort = ((Integer) getParameter(SORT_PARAM)).intValue();
        m_useGender = ((Boolean) getParameter(GENDER_PARAM)).booleanValue();
        m_useSchool = ((Boolean) getParameter(GROUP_BY_SCHOOL_PARAM)).booleanValue();

        m_enrollmentSchoolOidByStudentOid = new HashMap<String, String>();

        String dataFieldConfigOid = (String) getParameter(GROUP_BY_PARAM);
        DataFieldConfig dataField =
                (DataFieldConfig) getBroker().getBeanByOid(DataFieldConfig.class, dataFieldConfigOid);

        m_dictionary = DataDictionary.getDistrictDictionary(getUser().getPersistenceKey());
        DataDictionaryField field = m_dictionary.findDataDictionaryField(dataField);

        /*
         * Build student criteria.
         */
        X2Criteria criteria = new X2Criteria();
        Selection selection = getSelection();

        Criteria subCriteria = new Criteria();
        subCriteria.addEqualTo(SelectionObject.COL_SELECTION_OID, selection.getOid());
        subCriteria.addEqualToField(SelectionObject.COL_OBJECT_OID, Criteria.PARENT_QUERY_PREFIX
                + X2BaseBean.COL_OID);

        criteria.addExists(new SubQuery(SelectionObject.class, X2BaseBean.COL_OID, subCriteria));
        /*
         * Generate query
         */
        BeanQuery query = new BeanQuery(SisStudent.class, criteria);

        /*
         * Add ordering by school if specified.
         */
        if (m_useSchool) {
            checkAndAddOrderByClause(query, SisStudent.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_NAME);
        }

        /*
         * Add ordering of grouping.
         */
        String path = "";
        if (!dataField.getDataField().getDataTable().getDatabaseName().equalsIgnoreCase("student")) {
            path = SisStudent.REL_PERSON + PATH_DELIMITER;

            if (dataField.getDataField().getDataTable().getDatabaseName().equalsIgnoreCase("person_address")) {
                path += SisPerson.REL_PHYSICAL_ADDRESS + PATH_DELIMITER;
            }
        }

        path += field.getSystemDataField().getJavaName();
        checkAndAddOrderByClause(query, path);

        /*
         * Add ordering by gender if specified.
         */
        if (m_useGender) {
            checkAndAddOrderByClause(query, SisStudent.REL_PERSON + PATH_DELIMITER + SisPerson.COL_GENDER_CODE);
        }

        /*
         * Add ordering of sort.
         */
        switch (m_sort) {
            case 0: // Grade level
                checkAndAddOrderByClause(query, SisStudent.COL_GRADE_LEVEL);
                break;

            case 1: // Age
                checkAndAddOrderByClause(query, SisStudent.REL_PERSON + PATH_DELIMITER + SisPerson.COL_DOB);
                break;

            default: // None selected
                break;
        }

        checkAndAddOrderByClause(query, X2BaseBean.COL_OID);

        loadGradeLevelMap();
        Map<String, String> schoolNameByOid = loadSchoolMap();

        /*
         * Iterate over results and tally.
         */
        ReportDataGrid grid = new ReportDataGrid(10000, 20);

        /*
         * Adjust the query
         */
        ListSelectionAdjuster selAdjuster = new ListSelectionAdjuster(getBroker().getPersistenceKey());
        query.addQueryAdjuster(selAdjuster);

        Map<String, Integer> gridIndex = new HashMap<String, Integer>();

        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                SisStudent student = (SisStudent) iterator.next();
                String group = getGroupName(path, student);

                String schoolOid = student.getSchoolOid();

                /*
                 * If report is run for a past/future date, use school from students'
                 * enrollment record.
                 */
                if (!m_date.equals(new PlainDate(getTimeZone()))) {
                    schoolOid = m_enrollmentSchoolOidByStudentOid.get(student.getOid());

                    // If student has no enrollment records, use students' current school
                    if (schoolOid == null) {
                        schoolOid = student.getSchoolOid();
                    }
                }

                String schoolName = null;
                if (schoolOid != null) {
                    schoolName = schoolNameByOid.get(schoolOid);
                }

                if (!StringUtils.isEmpty(schoolName)) {
                    String rowKey = schoolName + group;

                    /*
                     * Student's school may not necessarily be in sorted order anymore, since
                     * we use school from students' enrollment record. Maintain a map of
                     * grid rows and jump/append new row to grid, as necessary.
                     */
                    Integer rowIndex = gridIndex.get(rowKey);
                    if (rowIndex == null) {
                        grid.append();
                        grid.set(CODE_FIELD, group);
                        grid.set(CODE_TOTAL, Integer.valueOf(0));

                        if (m_useSchool || isSchoolContext()) {
                            grid.set(SCHOOL_NAME_FIELD, schoolName);
                        } else {
                            grid.set(SCHOOL_NAME_FIELD, getOrganization().getName());
                        }

                        gridIndex.put(rowKey, Integer.valueOf(grid.currentRowNumber()));
                    } else {
                        grid.gotoRow(rowIndex.intValue());
                    }

                    /*
                     * Always update the total.
                     */
                    int codeTotal = ((Integer) grid.get(CODE_TOTAL)).intValue();
                    grid.set(CODE_TOTAL, Integer.valueOf(codeTotal + 1));

                    /*
                     * Update the counts based on the sort option
                     */
                    Integer sortTotal;
                    String numericLevel;
                    if (m_sort == 0) {
                        String grade = student.getGradeLevel();
                        numericLevel = (String) m_gradeLevelMap.get(grade);
                        if (numericLevel == null) {
                            numericLevel = "22";
                        }
                        sortTotal = (Integer) grid.get(numericLevel);
                    } else {
                        SisPerson person = student.getPerson();
                        int age = person.getAge();

                        /*
                         * As a restriction due to space on report, lump together any student over
                         * the
                         * age of 21.
                         */
                        if (age > 21) {
                            age = 22;
                        }

                        numericLevel = String.valueOf(age);
                        sortTotal = (Integer) grid.get(numericLevel);
                    }

                    int total = 1;
                    if (sortTotal != null) {
                        total += sortTotal.intValue();
                    }

                    grid.set(numericLevel, Integer.valueOf(total));
                }
            }
        } finally {
            iterator.close();
        }

        List<String> sortColumns = new ArrayList<String>();
        sortColumns.add(SCHOOL_NAME_FIELD);
        sortColumns.add(CODE_FIELD);

        grid.sort(sortColumns, true);

        // add reportParameters
        addParameter(HEADER_COLUMNS_PARAM, getColumnHeaders());
        addParameter(DATE_PARAM, m_date);

        grid.beforeTop();
        return grid;
    }

    /**
     * Helper method to check whether to add the order by clause or not.
     * Duplicate clauses can cause issues.
     *
     * @param query BeanQuery
     * @param clause String
     */
    private void checkAndAddOrderByClause(BeanQuery query, String clause) {
        if (!m_orderByClauses.contains(clause)) {
            query.addOrderByAscending(clause);
            m_orderByClauses.add(clause);
        }
    }

    /**
     * Returns a Map of strings for the column headers on the format keyed to numeric strings.
     *
     * @return Map
     */
    private Map getColumnHeaders() {
        Map headers = new HashMap(32);

        switch (m_sort) {
            case 0: // Grade level
                Iterator keys = m_gradeLevelMap.keySet().iterator();
                while (keys.hasNext()) {
                    String key = (String) keys.next();
                    String value = (String) m_gradeLevelMap.get(key);

                    if (value.equals("0")) {
                        headers.put(value, "SP");
                    } else {
                        headers.put(value, key);
                    }
                }

                break;

            case 1: // Age
                headers.put("0", "NA");
                headers.put("22", ">21");

                for (int i = 1; i <= 21; i++) {
                    String age = String.valueOf(i);
                    headers.put(age, age);
                }

                break;
        }

        return headers;
    }

    /**
     * Returns the value of the student field that the report is grouping by.
     *
     * @param path String
     * @param student SisStudent
     * @return String
     */
    private String getGroupName(String path, SisStudent student) {
        String group = null;

        try {
            group = WebUtils.getPropertyAsString(student,
                    new ModelProperty(SisStudent.class, path, getBroker().getPersistenceKey()), getLocale());

            if (StringUtils.isEmpty(group)) {
                group = "BLANK";
            }

            if (m_useGender) {
                group += " - " + student.getPerson().getGenderCode();
            }
        } catch (X2BaseException xbe) {
            throw new X2RuntimeException(xbe);
        }

        return group;
    }

    /**
     * Calculates student membership as-of the passed date for the passed school. The return value
     * is a Set containing the student OID if the student was a member on the given date.
     *
     * @param date PlainDate
     * @param school SisSchool
     * @return Set&lt;String&gt;
     */
    private Set<String> getMembershipAsOf(PlainDate date, SisSchool school) {
        return getMembershipAsOf(date, StudentEnrollment.COL_SCHOOL_OID,
                SisStudent.COL_SCHOOL_OID, school.getOid(), false);
    }

    /**
     * Calculates student membership as-of the passed date for the passed district. The return value
     * is a Set containing the student OID if the student was a member on the given date.
     *
     * @param date PlainDate
     * @param organization Organization
     * @return Set&lt;String&gt;
     */
    private Set<String> getMembershipAsOf(PlainDate date, Organization organization) {
        return getMembershipAsOf(date, StudentEnrollment.REL_SCHOOL + "." + SisSchool.COL_ORGANIZATION1_OID,
                SisStudent.COL_ORGANIZATION1_OID, organization.getOid(), false);
    }

    /**
     * Calculates student membership as-of passed date.
     * <p>
     * Note: This method differs from <code>{@link #getMembershipAsOf(PlainDate, String, String,
     * String, boolean)}</code>
     * from EnrollmentManager in that it populates a map of school OID's keyed on student OID's
     * which denotes the school that student was in during given date.
     *
     * @param date PlainDate
     * @param enrollmentOwnerProperty String
     * @param studentOwnerProperty String
     * @param ownerOid String
     * @param checkStatus boolean
     * @return Set&lt;String&gt;
     */
    private Set<String> getMembershipAsOf(PlainDate date,
                                          String enrollmentOwnerProperty,
                                          String studentOwnerProperty,
                                          String ownerOid,
                                          boolean checkStatus) {
        Set<String> membership = new HashSet<String>(500);

        PreferenceSet preferenceSet = PreferenceManager.getPreferenceSet(getOrganization());

        boolean entryIsMemberDay = Boolean.valueOf(preferenceSet.getPreferenceValue(
                SisPreferenceConstants.STUDENT_MEMBER_ON_ENTRY_DATE)).booleanValue();

        boolean withdrawalIsMemberDay = Boolean.valueOf(preferenceSet.getPreferenceValue(
                SisPreferenceConstants.STUDENT_MEMBER_ON_WITHDRAWAL_DATE)).booleanValue();

        // Process enrollment records with one query
        List typesList = new ArrayList(3);
        typesList.add(StudentEnrollment.ENTRY);
        typesList.add(StudentEnrollment.WITHDRAWAL);
        if (checkStatus) {
            typesList.add(StudentEnrollment.STATUS_CHANGE);
        }

        X2Criteria enrollmentCriteria = new X2Criteria();
        enrollmentCriteria.addEqualTo(enrollmentOwnerProperty, ownerOid);
        enrollmentCriteria.addNotNull(StudentEnrollment.COL_STUDENT_OID);
        enrollmentCriteria.addIn(StudentEnrollment.COL_ENROLLMENT_TYPE, typesList);
        if (getParameter(PARAM_INCLUDE_DOE_EXCLUDE_STUDENTS) != null) {
            DataDictionaryField excludeField = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_DOE_EXCLUDE_STD);
            if (getParameter(PARAM_INCLUDE_DOE_EXCLUDE_STUDENTS) == Boolean.FALSE) {
                if (excludeField != null) {
                    enrollmentCriteria.addNotEqualTo(
                            StudentEnrollment.REL_STUDENT + PATH_DELIMITER + excludeField.getJavaName(), Boolean.TRUE);
                }
            } else {
                X2Criteria orIncludeCriteria = new X2Criteria();
                X2Criteria orNullCriteria = new X2Criteria();
                orNullCriteria.addIsNull(StudentEnrollment.REL_STUDENT + PATH_DELIMITER + excludeField.getJavaName());
                orIncludeCriteria.addOrCriteria(orNullCriteria);
                orIncludeCriteria.addEqualTo(
                        StudentEnrollment.REL_STUDENT + PATH_DELIMITER + excludeField.getJavaName(), Boolean.TRUE);
                enrollmentCriteria.addOrCriteria(orIncludeCriteria);
            }
        }

        QueryByCriteria enrollmentQuery =
                new QueryByCriteria(StudentEnrollment.class, enrollmentCriteria);
        enrollmentQuery.addOrderByAscending(StudentEnrollment.COL_STUDENT_OID);
        enrollmentQuery.addOrderByAscending(StudentEnrollment.COL_ENROLLMENT_DATE);
        enrollmentQuery.addOrderByAscending(StudentEnrollment.COL_TIMESTAMP);

        Set complete = new HashSet(500);
        Set recordsEvaluated = new HashSet(500);

        try (QueryIterator enrollmentIterator = getBroker().getIteratorByQuery(enrollmentQuery)) {
            while (enrollmentIterator.hasNext()) {
                StudentEnrollment enrollment = (StudentEnrollment) enrollmentIterator.next();
                String studentOid = enrollment.getStudentOid();
                String schoolOid = enrollment.getSchoolOid();

                if (studentOid != null &&
                        enrollment.getEnrollmentDate() != null &&
                        !complete.contains(studentOid)) {
                    // Determine how to check if this record indicates an active status:
                    // checkStatus == false -> ENTRY means active, all else is not.
                    // checkStatus == true -> ENTRY or STATUS_CHANGE must have "Active" or empty in
                    // status code.
                    boolean enrollmentIsActiveStatus = true;
                    if (checkStatus) {
                        enrollmentIsActiveStatus = (StudentEnrollment.ENTRY.equals(enrollment.getEnrollmentType()) ||
                                StudentEnrollment.STATUS_CHANGE.equals(enrollment.getEnrollmentType())) &&
                                (StudentManager.isActiveStudent(OrganizationManager.getRootOrganization(getBroker()),
                                        enrollment.getStatusCode()) ||
                                        StringUtils.isEmpty(enrollment.getStatusCode()));
                    } else {
                        enrollmentIsActiveStatus = StudentEnrollment.ENTRY.equals(enrollment.getEnrollmentType());
                    }

                    if (date.after(enrollment.getEnrollmentDate())) {
                        if (enrollmentIsActiveStatus) {
                            membership.add(enrollment.getStudentOid());

                            if (!StringUtils.isEmpty(schoolOid)) {
                                m_enrollmentSchoolOidByStudentOid.put(studentOid, schoolOid);
                            }
                        } else {
                            membership.remove(enrollment.getStudentOid());
                        }
                    } else if (date.before(enrollment.getEnrollmentDate())) {
                        // Only consider records after the as-of date if no records existed before
                        // it
                        if (!recordsEvaluated.contains(studentOid)) {
                            if (StudentEnrollment.ENTRY.equals(enrollment.getEnrollmentType())) {
                                membership.remove(enrollment.getStudentOid());
                            } else {
                                membership.add(enrollment.getStudentOid());

                                if (!StringUtils.isEmpty(schoolOid)) {
                                    m_enrollmentSchoolOidByStudentOid.put(studentOid, schoolOid);
                                }
                            }
                        }

                        complete.add(studentOid);
                    } else // Enrollment date occurs on asOf date
                    {
                        if (enrollmentIsActiveStatus) {
                            if (entryIsMemberDay) {
                                membership.add(enrollment.getStudentOid());

                                if (!StringUtils.isEmpty(schoolOid)) {
                                    m_enrollmentSchoolOidByStudentOid.put(studentOid, schoolOid);
                                }
                            } else {
                                if (!recordsEvaluated.contains(studentOid)) {
                                    membership.remove(enrollment.getStudentOid());
                                }
                            }
                        } else {
                            if (withdrawalIsMemberDay) {
                                if (!recordsEvaluated.contains(studentOid)) {
                                    membership.add(enrollment.getStudentOid());

                                    if (!StringUtils.isEmpty(schoolOid)) {
                                        m_enrollmentSchoolOidByStudentOid.put(studentOid, schoolOid);
                                    }
                                }
                            } else {
                                membership.remove(enrollment.getStudentOid());
                            }
                        }

                        complete.add(studentOid);
                    }

                    recordsEvaluated.add(studentOid);
                }
            }
        }

        complete.clear();
        complete = null;
        recordsEvaluated.clear();
        recordsEvaluated = null;

        // Add students active in the passed school with no enrollment records
        Criteria subqueryCriteria = new Criteria();
        subqueryCriteria.addEqualToField(StudentEnrollment.COL_STUDENT_OID,
                Criteria.PARENT_QUERY_PREFIX + X2BaseBean.COL_OID);
        subqueryCriteria.addEqualTo(enrollmentOwnerProperty, ownerOid);
        subqueryCriteria.addIn(StudentEnrollment.COL_ENROLLMENT_TYPE, typesList);

        Criteria studentCriteria = new Criteria();
        studentCriteria.addNotExists(
                new SubQuery(StudentEnrollment.class, X2BaseBean.COL_OID, subqueryCriteria));
        studentCriteria.addEqualTo(studentOwnerProperty, ownerOid);
        studentCriteria.addAndCriteria(StudentManager.getActiveStudentStatusCriteria(
                OrganizationManager.getRootOrganization(getBroker()), SisStudent.COL_ENROLLMENT_STATUS));
        SubQuery studentQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria);
        membership.addAll(getBroker().getSubQueryCollectionByQuery(studentQuery));

        return membership;
    }

    /**
     * Returns a Selection of the students that were members of the school on the report start date.
     *
     * @return Selection
     */
    private Selection getSelection() {
        Selection selection = X2BaseBean.newInstance(Selection.class, getBroker().getPersistenceKey());

        Collection<String> currentMembers = null;

        /*
         * If the report is run for today we look at active students only. Otherwise we use
         * enrollment records to determine the active students on the reporting date.
         */
        if (m_date.equals(new PlainDate(getTimeZone()))) {
            X2Criteria criteria = new X2Criteria();
            criteria.addAndCriteria(
                    StudentManager.getActiveStudentStatusCriteria(getOrganization(), SisStudent.COL_ENROLLMENT_STATUS));
            if (isSchoolContext()) {
                criteria.addEqualTo(SisStudent.COL_SCHOOL_OID, getSchool().getOid());
            } else {
                criteria.addAndCriteria(getOrganizationCriteria(SisStudent.class));
            }
            if (getParameter(PARAM_INCLUDE_DOE_EXCLUDE_STUDENTS) != null) {
                DataDictionaryField excludeField = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_DOE_EXCLUDE_STD);
                if (getParameter(PARAM_INCLUDE_DOE_EXCLUDE_STUDENTS) == Boolean.FALSE) {
                    if (excludeField != null) {
                        criteria.addNotEqualTo(excludeField.getJavaName(), Boolean.TRUE);
                    }
                } else {
                    X2Criteria orIncludeCriteria = new X2Criteria();
                    X2Criteria orNullCriteria = new X2Criteria();
                    orNullCriteria.addIsNull(excludeField.getJavaName());
                    orIncludeCriteria.addOrCriteria(orNullCriteria);
                    orIncludeCriteria.addEqualTo(excludeField.getJavaName(), Boolean.TRUE);
                    criteria.addOrCriteria(orIncludeCriteria);
                }
            }
            SubQuery subQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, criteria);
            currentMembers = getBroker().getSubQueryCollectionByQuery(subQuery);
        } else {
            if (isSchoolContext()) {
                currentMembers = getMembershipAsOf(m_date, (SisSchool) getSchool());
            } else {
                currentMembers = getMembershipAsOf(m_date, getOrganization());
            }
        }

        for (String oid : currentMembers) {
            SelectionObject selectedObject =
                    X2BaseBean.newInstance(SelectionObject.class, getBroker().getPersistenceKey());
            selectedObject.setObjectOid(oid);
            selection.addToSelectionObjects(selectedObject);
        }

        selection.setTimestamp(System.currentTimeMillis());
        getBroker().saveBeanForced(selection);

        return selection;
    }

    /**
     * Returns a map of the report columns keyed to the corresponding grade level.
     *
     * @return Map
     */
    private void loadGradeLevelMap() {
        m_gradeLevelMap = new HashMap(32);

        TreeMap gradeLevels = StudentManager.buildGradeLevelMap(getBroker());

        Iterator offsetIterator = gradeLevels.keySet().iterator();
        while (offsetIterator.hasNext()) {
            Integer offset = (Integer) offsetIterator.next();
            List grades = (List) gradeLevels.get(offset);

            Iterator gradeIterator = grades.iterator();
            while (gradeIterator.hasNext()) {
                String grade = (String) gradeIterator.next();

                if (offset.intValue() > 20 || offset.intValue() < -5) {
                    m_gradeLevelMap.put(grade, "0");
                } else {
                    m_gradeLevelMap.put(grade, String.valueOf(offset.intValue() + 3));
                }
            }
        }
    }

    /**
     * Loads and returns a map of school name keyed on school OID for all schools in the district.
     *
     * @return Map&lt;String, String&gt;
     */
    private Map<String, String> loadSchoolMap() {
        Map<String, String> schoolNameByOid = new HashMap<String, String>();

        String[] columns = {X2BaseBean.COL_OID, SisSchool.COL_NAME};

        // Include all schools (archived and non-active schools too)
        ReportQueryByCriteria schoolQuery = new ReportQueryByCriteria(SisSchool.class, columns, null);

        try (ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(schoolQuery)) {
            while (iterator.hasNext()) {
                Object[] row = (Object[]) iterator.next();
                schoolNameByOid.put(row[0].toString(), row[1].toString());
            }
        }

        return schoolNameByOid;
    }
}
