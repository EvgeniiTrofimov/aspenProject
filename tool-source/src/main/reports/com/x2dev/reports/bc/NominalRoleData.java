/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2014 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.reports.bc;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.BeanManager;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.StudentSchool;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.ReferenceDescriptionLookup;
import com.follett.fsc.core.k12.tools.ToolJavaSource;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.procedures.statereporting.bc.BcData1701;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepDisability;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.sis.model.beans.Transcript;
import com.x2dev.sis.model.business.EnrollmentManager;
import com.x2dev.utils.CollectionUtils;
import com.x2dev.utils.ObjectUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Data source for the Nominal Role report. This report lists students that are flagged as
 * aboriginal by school.
 *
 * @author X2 Development Corporation
 */
public class NominalRoleData extends ReportJavaSourceNet {
    /*
     * Report parameters
     */
    private static final String QUERY_BY_PARAM = "queryBy";
    private static final String QUERY_STRING_PARAM = "queryString";
    private static final String SCHOOL_LOOKUP_PARAM = "schoolLookup";
    private static final String SORT_PARAM = "sort";
    private static final String SCHOOL_OIDS_PARAM = "schoolOids";

    /*
     * Alias fields
     */
    private static final String ALIAS_ANCESTORY = "psn-indian-ancestry";
    private static final String ALIAS_DIPLOMA_DATE = "std-diploma-granted-date";

    /*
     * Citizenship code constants
     */
    private static final String CITIZENSHIP_ALIAS = "std-citizenship-status";
    private static final String INTERNATIONAL_NOT_ELIGIBLE = "Intl Funding Not Eligible";
    private static final String OUT_OF_PROV_NOT_ELIGIBLE = "Cdn Funding Not Eligible";

    /*
     * Grid fields
     */
    private static final String FIELD_DISABILITY = "disability";
    private static final String FIELD_PROGRAM = "program";
    private static final String FIELD_SCHOOL = "school";
    private static final String FIELD_STUDENT = "student";
    private static final String FIELD_FTE = "fte";

    /*
     * School lookup values
     */
    private static final String SCHOOL_LOOKUP_GRADE_RANGE = "Grade Range";
    private static final String SCHOOL_LOOKUP_INST_DAYS = "Instruction Days";
    private static final String SCHOOL_LOOKUP_PRINCIPAL = "Principal";
    private static final String SCHOOL_LOOKUP_PROF_DAYS = "ProfessionalDays"; // Currently undefined

    /*
     * Ancestry code contsants
     */
    private static final String LOCAL_CODE_STANTON = "STATON";
    private static final String ON_RESERVE_STATUS = "Status - On Reserve";
    private static final String STATON_CODES_PARAM = "statonCodes";

    /*
     * School categorization constants
     */
    private static final String ALTERNATE_FACILITY = "Alternate Program School";
    private static final String DL_SCHOOL_TYPE = "DL";
    private static final String INDEPENDENT_TYPE = "Independent";

    /*
     * Key grade levels
     */
    private static final String GRADE_KH = "KH";
    private static final String GRADE_KF = "KF";
    private static final String GRADE_EU = "EU";

    private PlainDate m_adultAsOfDate;
    private BcData1701 m_bcData1701;
    private SisStudent m_currentStudent;
    private EnrollmentManager m_enrollmentManager;
    private Collection<ReferenceCode> m_gradeRefCodes;
    private Collection<String> m_schoolOids;
    private PlainDate m_underageAsOfDate;

    /*
     * Student data maps
     */
    private Map<String, Map<String, BigDecimal>> m_countCourseMap;
    private Map<String, Map<String, BigDecimal>> m_countSupportMap;
    private Map<String, IepDisability> m_disabilityMap;
    private Map<String, StringBuilder> m_programMap;

    /**
     * Gather data.
     *
     * @return Object
     * @throws Exception exception
     * @see com.x2dev.sis.tools.ToolJavaSource#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        ReportDataGrid grid = new ReportDataGrid();

        Map<String, Map<String, String>> schoolLookup = new HashMap<String, Map<String, String>>();
        addParameter(SCHOOL_LOOKUP_PARAM, schoolLookup);

        /*
         * The report should include both primary and active students for each school. This means a
         * student can be
         * included twice - once for their primary and once for their secondary
         */
        QueryByCriteria secondaryQuery = new QueryByCriteria(StudentSchool.class, buildSecondaryCriteria());
        QueryIterator secondaryStudents = getBroker().getIteratorByQuery(secondaryQuery);

        QueryByCriteria query = createQueryByCriteria(SisStudent.class, buildStudentCriteria(SisStudent.class));
        QueryIterator students = getBroker().getIteratorByQuery(query);

        try {
            load1701Data();

            while (students.hasNext()) {
                SisStudent student = (SisStudent) students.next();
                SisSchool school = student.getSchool();

                setSchoolLookup(schoolLookup, school);

                grid.append();
                grid.set(FIELD_SCHOOL, school);
                grid.set(FIELD_STUDENT, student);
                grid.set(FIELD_DISABILITY, m_disabilityMap.get(student.getOid()));
                grid.set(FIELD_FTE, calculateFte(student, school, true));
                grid.set("courseCount", getCount(student, school, m_countCourseMap));
                grid.set("supportCount", getCount(student, school, m_countSupportMap));

                StringBuilder programDisplay = m_programMap.get(student.getOid());
                if (programDisplay != null) {
                    grid.set(FIELD_PROGRAM, programDisplay.toString());
                }
            }

            while (secondaryStudents.hasNext()) {
                StudentSchool studentSchool = (StudentSchool) secondaryStudents.next();
                Student student = studentSchool.getStudent();
                School school = studentSchool.getSchool();

                setSchoolLookup(schoolLookup, (SisSchool) school);

                grid.append();
                grid.set(FIELD_SCHOOL, school);
                grid.set(FIELD_STUDENT, student);
                grid.set(FIELD_DISABILITY, m_disabilityMap.get(studentSchool.getStudentOid()));
                grid.set(FIELD_FTE, calculateFte(student, school, true));
                grid.set("courseCount", getCount(student, school, m_countCourseMap));
                grid.set("supportCount", getCount(student, school, m_countSupportMap));

                StringBuilder programDisplay = m_programMap.get(student.getOid());
                if (programDisplay != null) {
                    grid.set(FIELD_PROGRAM, programDisplay.toString());
                }
            }
        } finally {
            students.close();
            secondaryStudents.close();
        }

        /*
         * Sort the results - first by school then by selected input option
         */
        List<String> sortColumns = new ArrayList<String>();
        sortColumns.add(FIELD_SCHOOL + PATH_DELIMITER + SisSchool.COL_NAME);
        sortColumns.add(FIELD_SCHOOL + PATH_DELIMITER + X2BaseBean.COL_OID);

        String[] sortInputOrder = getUserSortOrderAsStringArray((String) getParameter(SORT_PARAM));
        sortColumns.addAll(Arrays.asList(sortInputOrder));
        grid.sort(sortColumns, false);

        grid.beforeTop();
        return grid;
    }

    /**
     * Initialize.
     *
     * @see com.x2dev.sis.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());

        m_schoolOids = getSchoolOids();
        loadPrimaryDisabilities();
        loadProgramParticipation(dictionary);

        /*
         * Load the grade level codes
         */
        DataDictionaryField gradeLevelField =
                dictionary.findDataDictionaryField(Transcript.class.getName(), Transcript.COL_GRADE_LEVEL);
        ReferenceTable gradeLevels = gradeLevelField.getReferenceTable();
        m_gradeRefCodes = gradeLevels.getReferenceCodes(getBroker());

        /*
         * Load STATON codes
         */
        Collection<String> statonCodes = new LinkedList<String>();

        DataDictionaryField ancestryField = dictionary.findDataDictionaryFieldByAlias(ALIAS_ANCESTORY);
        if (ancestryField != null) {
            ReferenceTable ancestryTable = ancestryField.getReferenceTable();
            if (ancestryTable != null) {
                Criteria criteria = new Criteria();
                criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, ancestryTable.getOid());
                criteria.addEqualTo(ReferenceCode.COL_LOCAL_CODE, LOCAL_CODE_STANTON);

                statonCodes = getBroker().getSubQueryCollectionByQuery(
                        new SubQuery(ReferenceCode.class, ReferenceCode.COL_CODE, criteria));
            }
        }
        addParameter(STATON_CODES_PARAM, statonCodes);

        /*
         * Get adult-age-as-of-date (June 30 of current school year)
         */
        Calendar calendar = Calendar.getInstance();
        calendar.set(Calendar.YEAR, getCurrentContext().getSchoolYear());
        calendar.set(Calendar.MONTH, Calendar.JUNE);
        calendar.set(Calendar.DAY_OF_MONTH, 30);
        m_adultAsOfDate = new PlainDate(calendar.getTimeInMillis());

        /*
         * Get underage-as-of-date (Dec 31 of current school year)
         */
        calendar.set(Calendar.YEAR, getCurrentContext().getSchoolYear() - 1);
        calendar.set(Calendar.MONTH, Calendar.DECEMBER);
        calendar.set(Calendar.DAY_OF_MONTH, 31);
        m_underageAsOfDate = new PlainDate(calendar.getTimeInMillis());
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
        m_enrollmentManager = new EnrollmentManager(getBroker(), userData.getPrivilegeSet(), getOrganization());
        m_currentStudent = userData.getCurrentRecord(SisStudent.class);
    }

    /**
     * Adds student criteria restrictions to the passed criteria. This is used for both the student
     * and student
     * school queries in the report.
     *
     * @param criteria X2Criteria
     * @param criteriaClass Class
     * @param currentSelectionClass Class
     * @param studentPrefix String
     * @param addInCriteriaBeanPath String
     * @param schoolBeanPath String
     */
    private void addStudentCriteria(X2Criteria criteria,
                                    Class criteriaClass,
                                    Class currentSelectionClass,
                                    String studentPrefix,
                                    String addInCriteriaBeanPath,
                                    String schoolBeanPath) {
        if (m_currentStudent != null) {
            criteria.addEqualTo(studentPrefix + X2BaseBean.COL_OID, m_currentStudent.getOid());
        } else {
            /*
             * User criteria
             */
            String queryBy = (String) getParameter(QUERY_BY_PARAM);
            addUserCriteria(criteria, queryBy, (String) getParameter(QUERY_STRING_PARAM), criteriaClass,
                    currentSelectionClass, addInCriteriaBeanPath);

            /*
             * School restriction
             */
            if (!StringUtils.isEmpty(schoolBeanPath)) {
                criteria.addIn(schoolBeanPath, m_schoolOids);
            }

            /*
             * Aboriginal only (alias "psn-indian-ancestry" on student table)
             */
            DataDictionaryField field = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey())
                    .findDataDictionaryFieldByAlias(ALIAS_ANCESTORY);
            if (field != null && field.getDataTableOid()
                    .equals(BeanManager.getFullOid(Student.DICTIONARY_ID, getBroker().getPersistenceKey()))) {
                criteria.addNotEmpty(studentPrefix + field.getJavaName(),
                        getBroker().getPersistenceKey());
            } else {
                addNoMatchCriteria(criteria);
            }

            /*
             * Active only
             */
            criteria.addAndCriteria(StudentManager.getActiveStudentStatusCriteria(getOrganization(),
                    studentPrefix + SisStudent.COL_ENROLLMENT_STATUS));
        }
    }

    /**
     * Build query for Student Schools for the selected schools and current context. End and start
     * date of
     * secondary school should not be empty and current date should be in the range of these dates.
     *
     * @return Criteria
     */
    private Criteria buildSecondaryCriteria() {
        X2Criteria secondaryCriteria = new X2Criteria();
        secondaryCriteria.addIn(StudentSchool.COL_SCHOOL_OID, m_schoolOids);
        secondaryCriteria.addEqualTo(StudentSchool.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());
        secondaryCriteria.addEqualTo(StudentSchool.COL_TYPE, Integer.valueOf(StudentSchool.SECONDARY));

        /*
         * Start date
         */
        X2Criteria startDateCriteria = new X2Criteria();
        startDateCriteria.addIsNull(StudentSchool.COL_START_DATE);

        X2Criteria startDateCriteriaNotNull = new X2Criteria();
        startDateCriteriaNotNull.addLessOrEqualThan(StudentSchool.COL_START_DATE, getPlainDate());

        startDateCriteria.addOrCriteria(startDateCriteriaNotNull);
        secondaryCriteria.addAndCriteria(startDateCriteria);

        /*
         * End date
         */
        X2Criteria endDateCriteria = new X2Criteria();
        endDateCriteria.addIsNull(StudentSchool.COL_END_DATE);

        X2Criteria endDateCriteriaNotNull = new X2Criteria();
        endDateCriteriaNotNull.addGreaterOrEqualThan(StudentSchool.COL_END_DATE, getPlainDate());

        endDateCriteria.addOrCriteria(endDateCriteriaNotNull);
        secondaryCriteria.addAndCriteria(endDateCriteria);

        addStudentCriteria(secondaryCriteria,
                StudentSchool.class,
                SisStudent.class,
                StudentSchool.REL_STUDENT + PATH_DELIMITER,
                StudentSchool.COL_STUDENT_OID,
                StudentSchool.COL_SCHOOL_OID);

        return secondaryCriteria;
    }

    /**
     * Build the criteria for the student including only tribal students.
     *
     * @param criteriaClass Class
     * @return X2Criteria
     */
    private X2Criteria buildStudentCriteria(Class criteriaClass) {
        X2Criteria criteria = new X2Criteria();
        addStudentCriteria(criteria, criteriaClass, null, "", "", SisStudent.COL_SCHOOL_OID);

        return criteria;
    }

    /**
     * Calculates the ministry FTE value.
     *
     * @param student Student
     * @param school School
     * @param isPrimary boolean
     * @return BigDecimal
     */
    private BigDecimal calculateFte(Student student, School school, boolean isPrimary) {
        BigDecimal fte = new BigDecimal(99);

        Person person = student.getPerson();

        /*
         * Student information
         */
        String gradeLevel = student.getGradeLevel();
        int numericGrade = StringUtils.isInteger(gradeLevel) ? Integer.parseInt(gradeLevel) : 0;

        boolean isAdult = person.getAgeAsOfDate(m_adultAsOfDate) >= 19;
        boolean isGraduate = !StringUtils.isEmpty((String) student.getFieldValueByAlias(ALIAS_DIPLOMA_DATE));
        boolean isUnderage = person.getAgeAsOfDate(m_underageAsOfDate) < 5;
        boolean isSchoolAge = !isAdult && !isUnderage;

        String citizenshipStatus = (String) person.getFieldValueByAlias(CITIZENSHIP_ALIAS);
        boolean isIneligible = INTERNATIONAL_NOT_ELIGIBLE.equals(citizenshipStatus)
                || OUT_OF_PROV_NOT_ELIGIBLE.equals(citizenshipStatus);

        String ancestry = (String) student.getFieldValueByAlias(ALIAS_ANCESTORY);
        boolean onReserve = ON_RESERVE_STATUS.equals(ancestry);

        /*
         * Course counts
         */
        BigDecimal courseCount = null;
        Map<String, BigDecimal> schoolCounts = m_countCourseMap.get(school.getOid());
        if (schoolCounts != null) {
            courseCount = schoolCounts.get(student.getOid());
        }
        courseCount = courseCount == null ? new BigDecimal(0) : courseCount;

        BigDecimal supportCount = null;
        schoolCounts = m_countSupportMap.get(school.getOid());
        if (schoolCounts != null) {
            supportCount = schoolCounts.get(student.getOid());
        }
        supportCount = supportCount == null ? new BigDecimal(0) : supportCount;

        /*
         * School information
         */
        boolean alternateSchool = ALTERNATE_FACILITY.equals(school.getSchoolLevelCode());
        boolean dlSchool = DL_SCHOOL_TYPE.equals(school.getSchoolTypeCode());
        boolean independentSchool = INDEPENDENT_TYPE.equals(school.getSchoolTypeCode());
        boolean homeSchool = false;

        /*
         * Determine FTE
         */
        if (!isPrimary && numericGrade < 8) {
            fte = BigDecimal.ZERO;
        } else if (onReserve && independentSchool) {
            fte = BigDecimal.ZERO;
        } else if (isUnderage) {
            fte = BigDecimal.ZERO;
        } else if (isIneligible) {
            fte = BigDecimal.ZERO;
        } else if (homeSchool) {
            fte = BigDecimal.ZERO;
        } else if (GRADE_KH.equals(gradeLevel)) {
            fte = new BigDecimal(0.5);
        } else if (GRADE_KF.equals(gradeLevel)) {
            fte = BigDecimal.ONE;
        } else if (GRADE_EU.equals(gradeLevel) || numericGrade < 8) {
            fte = BigDecimal.ONE;
        } else if (!isAdult && numericGrade >= 8 && numericGrade < 10) {
            if (isSchoolAge && !isGraduate) {
                if (courseCount.intValue() == 0) {
                    fte = BigDecimal.ZERO;
                } else if (alternateSchool) {
                    fte = BigDecimal.ONE;
                } else {
                    double courseFte = 0.5 + (courseCount.doubleValue() * 0.125);

                    if (courseFte > 1) {
                        courseFte = 1;
                    }

                    fte = new BigDecimal(courseFte);
                }
            } else if (isSchoolAge && isGraduate) {
                fte = new BigDecimal(courseCount.doubleValue() * 0.125);
            }
        } else if (!isAdult && numericGrade >= 10 && numericGrade <= 12) {
            if (alternateSchool) {
                if (isSchoolAge && !isGraduate) {
                    if (courseCount.intValue() == 0) {
                        fte = BigDecimal.ZERO;
                    } else {
                        fte = BigDecimal.ONE;
                    }
                } else if (isSchoolAge && isGraduate) {
                    fte = new BigDecimal(courseCount.doubleValue() * 0.125);
                }
            } else if (dlSchool) {
                fte = new BigDecimal(courseCount.doubleValue() * 0.125);
            } else if (independentSchool) {
                double courseFte = courseCount.doubleValue() * 0.125;
                double supportFte =
                        isSchoolAge && !isGraduate && courseFte > 0 ? (supportCount.doubleValue() * 0.125) : 0;

                double totalFte = courseFte + supportFte;
                if (totalFte > 1) {
                    totalFte = 1;
                }

                fte = new BigDecimal(totalFte);
            } else {
                double courseFte = courseCount.doubleValue() * 0.125;
                double supportFte =
                        isSchoolAge && !isGraduate && courseFte > 0 ? (supportCount.doubleValue() * 0.125) : 0;

                if (courseFte < 1) {
                    courseFte = courseFte + supportFte;
                    if (courseFte > 1) {
                        courseFte = 1;
                    }
                }

                fte = new BigDecimal(courseFte);
            }
        } else if (isAdult) {
            if (independentSchool) {
                fte = BigDecimal.ZERO;
            } else {
                fte = new BigDecimal(courseCount.doubleValue() * 0.125);
            }
        }

        return fte;
    }

    /**
     * Returns the count based on the provided information.
     *
     * @param student Student
     * @param school School
     * @param countMap Map<String,Map<String,BigDecimal>>
     * @return BigDecimal
     */
    private BigDecimal getCount(Student student, School school, Map<String, Map<String, BigDecimal>> countMap) {
        BigDecimal count = null;

        Map<String, BigDecimal> schoolCounts = countMap.get(school.getOid());
        if (schoolCounts != null) {
            count = schoolCounts.get(student.getOid());
        }

        return count;
    }

    /**
     * Get a string value corresponding to a numeric grade.
     *
     * @param num int
     * @return String
     */
    private String getNumericGrade(int num) {
        String gradeLevel = null;

        String value = Integer.toString(num);

        for (ReferenceCode rcd : m_gradeRefCodes) {
            if (value.equals(rcd.getFieldA005())) {
                gradeLevel = rcd.getCode();
            }
        }

        return gradeLevel;
    }

    /**
     * Builds the display of the principal name for the passed school as Mr. David Jones
     *
     * @param school SisSchool
     * @return String
     */
    private String getPrincipalName(SisSchool school) {
        StringBuilder display = new StringBuilder(150);

        SisStaff staff = school.getAdministrator1();
        if (staff != null) {
            SisPerson person = staff.getPerson();

            if (!StringUtils.isEmpty(person.getNameTitleCode())) {
                display.append(person.getNameTitleCode());
                display.append(" ");
            }

            display.append(person.getFirstName());
            display.append(" ");
            display.append(person.getLastName());
        }

        return display.toString();
    }

    /**
     * Returns collection with export school oids.
     *
     * @return Collection<String>
     */
    private Collection<String> getSchoolOids() {
        X2Criteria criteria = new X2Criteria();

        if (getSchool() != null) {
            criteria.addEqualTo(X2BaseBean.COL_OID, getSchool().getOid());
        } else {
            String oids = (String) getParameter(SCHOOL_OIDS_PARAM);
            if (!StringUtils.isEmpty(oids)) {
                Collection<String> oidList = StringUtils.convertDelimitedStringToList(oids, ',', true);
                criteria.addIn(X2BaseBean.COL_OID, oidList);
            } else {
                criteria.addNotEqualTo(SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
                criteria.addNotEqualTo(SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
                criteria.addAndCriteria(getOrganizationCriteria(SisSchool.class));
            }
        }

        return getBroker().getSubQueryCollectionByQuery(new SubQuery(SisSchool.class, X2BaseBean.COL_OID, criteria));
    }

    /**
     * Loads the course count and support block counts from the 1701 extract into maps keyed to the
     * student OID.
     */
    private void load1701Data() {
        m_bcData1701 = new BcData1701(getBroker(), getOrganization(), getOrganizationCriteria(SisStudent.class));

        m_countCourseMap = new HashMap<String, Map<String, BigDecimal>>(m_schoolOids.size());
        m_countSupportMap = new HashMap<String, Map<String, BigDecimal>>(m_schoolOids.size());

        for (String oid : m_schoolOids) {
            Map<String, BigDecimal> courseMap = new HashMap<String, BigDecimal>(4096);
            Map<String, BigDecimal> supportMap = new HashMap<String, BigDecimal>(4096);

            m_countCourseMap.put(oid, courseMap);
            m_countSupportMap.put(oid, supportMap);

            Collection<BcData1701.Bc1701Student> bcStudents = m_bcData1701.get1701Students(oid);
            if (!CollectionUtils.isEmpty(bcStudents)) {
                for (BcData1701.Bc1701Student bcStudent : bcStudents) {
                    courseMap.put(bcStudent.getStudent().getOid(), new BigDecimal(bcStudent.getCourseCount()));
                    supportMap.put(bcStudent.getStudent().getOid(), new BigDecimal(bcStudent.getSupportBlockCount()));
                }
            }
        }
    }

    /**
     * Load the primary disabilities for any active IEPs of students included in the report.
     */
    private void loadPrimaryDisabilities() {
        m_disabilityMap = new HashMap<String, IepDisability>(2048);

        /*
         * Build the criteria for disabilities associated with student's active IEPs
         */
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(IepDisability.REL_IEP_DATA + PATH_DELIMITER + IepData.COL_STATUS_CODE,
                Integer.valueOf(IepData.StatusCode.ACTIVE.ordinal()));

        addStudentCriteria(criteria,
                IepDisability.class,
                SisStudent.class,
                IepDisability.REL_STUDENT + PATH_DELIMITER,
                IepDisability.COL_STUDENT_OID,
                null);

        /*
         * Build and execute query. The first disability (hopefully flagged as primary) is the one
         * included in the
         * report for the student.
         */
        QueryByCriteria query = new QueryByCriteria(IepDisability.class, criteria);
        query.addOrderByAscending(IepDisability.COL_STUDENT_OID);
        query.addOrderByDescending(IepDisability.COL_PRIMARY_INDICATOR);

        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            SisStudent lastStudent = null;
            while (iterator.hasNext()) {
                IepDisability disability = (IepDisability) iterator.next();
                SisStudent student = disability.getStudent();

                if (!ObjectUtils.match(student, lastStudent)) {
                    m_disabilityMap.put(student.getOid(), disability);
                }

                lastStudent = student;
            }
        } finally {
            iterator.close();
        }
    }

    /**
     * Loads a display of the students' current program participations.
     *
     * @param dictionary DataDictionary
     */
    private void loadProgramParticipation(DataDictionary dictionary) {
        m_programMap = new HashMap<String, StringBuilder>(2048);
        PlainDate today = getPlainDate();

        /*
         * Load reference code lookup
         */
        ReferenceDescriptionLookup referenceLookup =
                (ReferenceDescriptionLookup) getParameter(ToolJavaSource.REFERENCE_LOOKUP_KEY);
        String referenceTableOid = "noOidFound";
        DataDictionaryField field = dictionary.findDataDictionaryField(StudentProgramParticipation.class.getName(),
                StudentProgramParticipation.COL_PROGRAM_CODE);
        if (field != null && !StringUtils.isEmpty(field.getReferenceTableOid())) {
            referenceTableOid = field.getReferenceTableOid();
        }

        /*
         * Build the criteria for program participation in that spans the run date
         */
        X2Criteria criteria = new X2Criteria();
        criteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, today);
        addStudentCriteria(criteria,
                StudentProgramParticipation.class,
                SisStudent.class,
                StudentProgramParticipation.REL_STUDENT + PATH_DELIMITER,
                StudentProgramParticipation.COL_STUDENT_OID,
                null);

        /*
         * Program participation can either have an end date (after today) or no end date
         */
        X2Criteria endDateCriteria = new X2Criteria();
        endDateCriteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_END_DATE, today);

        X2Criteria emptyEndCriteria = new X2Criteria();
        emptyEndCriteria.addEmpty(StudentProgramParticipation.COL_END_DATE, getBroker().getPersistenceKey());

        endDateCriteria.addOrCriteria(emptyEndCriteria);
        criteria.addAndCriteria(endDateCriteria);

        /*
         * Build and execute query
         */
        QueryByCriteria query = new QueryByCriteria(StudentProgramParticipation.class, criteria);
        query.addOrderByAscending(StudentProgramParticipation.COL_STUDENT_OID);
        query.addOrderByAscending(StudentProgramParticipation.COL_START_DATE);

        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            SisStudent lastStudent = null;
            StringBuilder display = new StringBuilder(200);

            while (iterator.hasNext()) {
                StudentProgramParticipation program = (StudentProgramParticipation) iterator.next();
                SisStudent student = program.getStudent();

                if (!ObjectUtils.match(student, lastStudent)) {
                    display = new StringBuilder(200);
                    m_programMap.put(student.getOid(), display);
                }

                if (display.length() > 0) {
                    display.append("; ");
                }

                display.append(referenceLookup.getDescription(referenceTableOid, program.getProgramCode()));

                lastStudent = student;
            }
        } finally {
            iterator.close();
        }
    }

    /**
     * Sets the school lookup values fore this school.
     *
     * @param schoolLookup Map<String,Map<String,String>>
     * @param school SisSchool
     */
    private void setSchoolLookup(Map<String, Map<String, String>> schoolLookup, SisSchool school) {
        if (!schoolLookup.containsKey(school.getOid())) {
            Map<String, String> map = new HashMap<String, String>();
            schoolLookup.put(school.getOid(), map);

            /*
             * Set grade range
             */
            String beginGrade = getNumericGrade(school.getStartGrade());
            String endGrade = getNumericGrade(school.getStartGrade() + school.getNumberOfGrades() - 1);

            if (beginGrade != null && endGrade != null) {
                map.put(SCHOOL_LOOKUP_GRADE_RANGE, beginGrade + " - " + endGrade);
            }

            /*
             * Set principal
             */
            map.put(SCHOOL_LOOKUP_PRINCIPAL, getPrincipalName(school));

            /*
             * Set institution days
             */
            Map calendarMap = m_enrollmentManager.getCalendarLookup(school,
                    getCurrentContext().getStartDate(),
                    getCurrentContext().getEndDate(),
                    getCurrentContext().getOid());
            if (calendarMap != null && calendarMap.size() > 0) {
                Set set = ((Set) ((Map.Entry) calendarMap.entrySet().iterator().next()).getValue());
                if (set != null) {
                    map.put(SCHOOL_LOOKUP_INST_DAYS, Integer.toString(set.size()));
                }
            }

            /*
             * Set programs
             * TODO: Implement based on client specs
             */
            map.put(SCHOOL_LOOKUP_PROF_DAYS, "");
        }
    }
}
