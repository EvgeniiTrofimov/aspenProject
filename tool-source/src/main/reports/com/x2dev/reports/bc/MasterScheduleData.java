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

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.admin.ReferenceManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.QueryIteratorDataSource;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.follett.fsc.core.k12.web.ApplicationContext;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.Section;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.sis.model.beans.StudentSchedule;
import com.x2dev.sis.model.beans.StudentSection;
import com.x2dev.sis.tools.reports.ScheduleReportHelper;
import com.x2dev.sis.web.SisUserDataContainer;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2002-2015 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Prepares the data for the Master Schedule report. This report lists all the sections in the
 * active schedule for the selected school. The sections will be ordered according to the user
 * input.
 * <p>
 * This report supports both the MasterSchedule and BuildMasterSchedule objects.
 *
 * @author Follett Software Company
 */
public class MasterScheduleData extends ReportJavaSourceNet {
    private static final long serialVersionUID = 1L;

    /**
     * Active Codes for student enrollment status
     */
    private final String ACTIVE_KEY = SystemPreferenceDefinition.STUDENT_ACTIVE_CODE;
    private final String ACTIVE_NO_PRIMARY_KEY = SystemPreferenceDefinition.STUDENT_ACTIVE_CODE_NO_PRIMARY;

    /**
     * Name for the section-OID-to-male/female-count report parameter. The value is a Map of female
     * enrollment counts (Long objects) keyed on MasterSchedule OIDs (String objects).
     */
    public static final String FEMALE_INCL_COUNTS_MAP_PARAM = "femaleINCLCountsMap";
    public static final String MALE_INCL_COUNTS_MAP_PARAM = "maleINCLCountsMap";
    public static final String FEMALE_SPED_COUNTS_MAP_PARAM = "femaleSPEDCountsMap";
    public static final String MALE_SPED_COUNTS_MAP_PARAM = "maleSPEDCountsMap";
    public static final String FEMALE_ELL_COUNTS_MAP_PARAM = "femaleELLCountsMap";
    public static final String MALE_ELL_COUNTS_MAP_PARAM = "maleELLCountsMap";

    private static final String ALIAS_STUDENT_DESIGNATION = "std-sped-category";

    /**
     * Name for the build schedule scenario name
     *
     */
    public static final String BUILD_SCENARIO_NAME_PARAM = "buildSchedule";

    /**
     * Name for the "selection" report parameter. The value is a String.
     */
    public static final String QUERY_BY_PARAM = "queryBy";
    public static final String QUERY_STRING_PARAM = "queryString";

    /**
     * Name for the "sort" report parameter. The value is a String.
     */
    public static final String SORT_PARAM = "sort";

    /**
     * Prefix of the ELL code that should be included in the counts
     */
    private final String PREFIX_ELL_CODE = "17";

    /**
     * 'Gifted' student disability code for BC
     */
    private final String GIFTED_STUDENT_DISABILITY_CODE = "Gifted";

    /**
     * Female and Male genderCode
     */
    private final String FEMALE_GENDER_CODE = "F";
    private final String MALE_GENDER_CODE = "M";

    /**
     * Student Report Helper variable
     */
    private ScheduleReportHelper m_reportHelper;
    private List<String> m_activeCodes;
    private String m_buildSchedule;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        // Get the list of active codes for student enrollments
        m_activeCodes = Arrays.asList(PreferenceManager.getPreferenceValue(getOrganization(), ACTIVE_KEY),
                PreferenceManager.getPreferenceValue(getOrganization(), ACTIVE_NO_PRIMARY_KEY));

        Criteria criteria = new Criteria();

        /*
         * Build the schedule criteria based on the school's current schedule and the user's input.
         */
        String queryString = (String) getParameter(QUERY_STRING_PARAM);
        String queryBy = getParameter(QUERY_BY_PARAM).toString();

        switch (queryBy) {
            case "1": // Department
                criteria.addEqualTo(
                        MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.COL_DEPARTMENT_CODE,
                        queryString);
                break;

            case "2": // Schedule Expression (scheduleDisplay)
                criteria.addEqualTo(MasterSchedule.COL_SCHEDULE_DISPLAY, queryString);
                break;

            case "3": // Section
                criteria.addEqualTo(MasterSchedule.COL_SECTION_NUMBER, queryString);
                break;

            case "4": // Teacher
                criteria.addEqualTo(MasterSchedule.COL_STAFF_VIEW, queryString);
                break;

            default: // "Current Selection" or "All"
                addUserCriteria(criteria, queryBy, null, null, null);
                break;
        }

        criteria.addEqualTo(Section.COL_SCHEDULE_OID, m_reportHelper.getScheduleOid());

        QueryByCriteria query = createQueryByCriteria(m_reportHelper.getSectionClass(), criteria);
        query.setPathOuterJoin(MasterSchedule.REL_SECTION_CLASS);

        /*
         * Build the sort based on user input.
         */
        applyUserSort(query, (String) getParameter(SORT_PARAM));

        /*
         * First create a map to link each section to a inclusion female students counts. This map
         * will
         * become a report parameter.
         */
        addParameter(FEMALE_INCL_COUNTS_MAP_PARAM, getInclusionCounts(getBroker().getCount(query), FEMALE_GENDER_CODE));
        addParameter(MALE_INCL_COUNTS_MAP_PARAM, getInclusionCounts(getBroker().getCount(query), MALE_GENDER_CODE));
        addParameter(FEMALE_SPED_COUNTS_MAP_PARAM, getSPEDCounts(getBroker().getCount(query), FEMALE_GENDER_CODE));
        addParameter(MALE_SPED_COUNTS_MAP_PARAM, getSPEDCounts(getBroker().getCount(query), MALE_GENDER_CODE));
        addParameter(FEMALE_ELL_COUNTS_MAP_PARAM, getELLCounts(getBroker().getCount(query), FEMALE_GENDER_CODE));
        addParameter(MALE_ELL_COUNTS_MAP_PARAM, getELLCounts(getBroker().getCount(query), MALE_GENDER_CODE));

        /*
         * If the BuildSchedule name is not empty, add it to the parameter
         */
        if (!StringUtils.isEmpty(m_buildSchedule)) {
            addParameter(BUILD_SCENARIO_NAME_PARAM, m_buildSchedule);
        }

        /*
         * Execute the query
         */
        return new QueryIteratorDataSource(getBroker().getIteratorByQuery(query));
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.
     *      UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException {
        super.saveState(userData);

        /*
         * If it's build view, we want to add the name of the current scenario if possible
         */
        ApplicationContext appContext = userData.getSessionNavConfig().getApplicationContext();
        if (appContext != null && appContext == ApplicationContext.BUILD) {
            m_buildSchedule = ((SisUserDataContainer) userData).getBuildSchedule().getName();
        }

        m_reportHelper = new ScheduleReportHelper(userData);
    }

    /**
     * Returns a non-null map that relates all sections in the selected school's active schedule to
     * the number of enrolled 'ELL' female students.
     *
     * @param sectionCount the number of sections in the master schedule
     * @param genderCode M for Male, F for female.
     *
     * @return A Map of String keys to Long values
     */
    private Map getELLCounts(int sectionCount, String genderCode) {
        HashMap inclFemaleCounts = new HashMap((int) (sectionCount * 1.5 + 1));

        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(StudentSection.COL_SCHEDULE_OID, m_reportHelper.getScheduleOid());
        criteria.addIn(StudentSection.REL_STUDENT + PATH_DELIMITER + Student.COL_ENROLLMENT_STATUS, m_activeCodes);

        if (!StringUtils.isEmpty(genderCode)) {
            criteria.addBeginsWithIgnoreCase(StudentSection.REL_STUDENT + PATH_DELIMITER +
                    SisStudent.REL_PERSON + PATH_DELIMITER + SisPerson.COL_GENDER_CODE, genderCode);
        }

        criteria.addIn(
                StudentSection.REL_STUDENT + PATH_DELIMITER + SisStudent.REL_PROGRAM_PARTICIPATION + PATH_DELIMITER +
                        StudentProgramParticipation.COL_PROGRAM_CODE,
                getELLProgramTypesSubQuery());
        criteria.addLessOrEqualThan(
                StudentSection.REL_STUDENT + PATH_DELIMITER + SisStudent.REL_PROGRAM_PARTICIPATION + PATH_DELIMITER +
                        StudentProgramParticipation.COL_START_DATE,
                new PlainDate(getTimeZone()));

        /*
         * Program participation end date has to null OR the end date is after today
         */
        X2Criteria orCriteria = new X2Criteria();
        orCriteria.addGreaterThan(
                StudentSection.REL_STUDENT + PATH_DELIMITER + SisStudent.REL_PROGRAM_PARTICIPATION + PATH_DELIMITER +
                        StudentProgramParticipation.COL_END_DATE,
                new PlainDate(getTimeZone()));

        X2Criteria endDateCriteria = new X2Criteria();
        endDateCriteria.addIsNull(
                StudentSection.REL_STUDENT + PATH_DELIMITER + SisStudent.REL_PROGRAM_PARTICIPATION + PATH_DELIMITER +
                        StudentProgramParticipation.COL_END_DATE);
        endDateCriteria.addOrCriteria(orCriteria);
        criteria.addAndCriteria(endDateCriteria);

        String[] columns = new String[] {StudentSection.COL_SECTION_OID, "count(DISTINCT STD_OID)"};
        ReportQueryByCriteria query =
                new ReportQueryByCriteria(m_reportHelper.getStudentSectionClass(), columns, criteria);
        query.addGroupBy(StudentSection.COL_SECTION_OID);

        ReportQueryIterator results = null;
        try {
            results = getBroker().getReportQueryIteratorByQuery(query);
            while (results.hasNext()) {
                Object[] values = (Object[]) results.next();

                String countString = values[1].toString();
                Integer count = Integer.valueOf(countString);

                inclFemaleCounts.put(values[0], count);
            }
        } finally {
            if (results != null) {
                results.close();
            }
        }

        return inclFemaleCounts;
    }

    /**
     * Returns a non-null map that relates all sections in the selected school's active schedule to
     * the number of enrolled 'inclusion' students.
     *
     * @param sectionCount the number of sections in the master schedule
     * @param genderCode M for Male, F for female.
     *
     * @return A Map of String keys to Long values
     */
    private Map getInclusionCounts(int sectionCount, String genderCode) {
        HashMap inclCounts = new HashMap((int) (sectionCount * 1.5 + 1));

        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(StudentSection.COL_SCHEDULE_OID, m_reportHelper.getScheduleOid());
        criteria.addIn(StudentSection.REL_STUDENT + PATH_DELIMITER + Student.COL_ENROLLMENT_STATUS, m_activeCodes);

        if (!StringUtils.isEmpty(genderCode)) {
            criteria.addBeginsWithIgnoreCase(StudentSection.REL_STUDENT + PATH_DELIMITER +
                    SisStudent.REL_PERSON + PATH_DELIMITER + SisPerson.COL_GENDER_CODE, genderCode);
        }

        criteria.addEqualTo(StudentSchedule.COL_INCLUSION_INDICATOR, Boolean.TRUE);

        String[] columns = new String[] {StudentSection.COL_SECTION_OID, "count(DISTINCT STD_OID)"};
        ReportQueryByCriteria query =
                new ReportQueryByCriteria(m_reportHelper.getStudentSectionClass(), columns, criteria);
        query.addGroupBy(StudentSection.COL_SECTION_OID);

        ReportQueryIterator results = null;
        try {
            results = getBroker().getReportQueryIteratorByQuery(query);
            while (results.hasNext()) {
                Object[] values = (Object[]) results.next();

                String countString = values[1].toString();
                Integer count = Integer.valueOf(countString);

                inclCounts.put(values[0], count);
            }
        } finally {
            if (results != null) {
                results.close();
            }
        }

        return inclCounts;
    }

    /**
     * Get program type codes that starts with "17" as their state code.
     *
     * @return program type codes
     */
    private SubQuery getELLProgramTypesSubQuery() {
        DataDictionaryField field = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey())
                .findDataDictionaryField(StudentProgramParticipation.class.getName(),
                        StudentProgramParticipation.COL_PROGRAM_CODE);

        X2Criteria criteria = ReferenceManager.getCodesCriteria(field.getReferenceTableOid(),
                getOrganization(),
                getBroker().getPersistenceKey());
        criteria.addBeginsWith(ReferenceCode.COL_STATE_CODE, PREFIX_ELL_CODE);

        QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, criteria);
        Collection<ReferenceCode> codes = getBroker().getCollectionByQuery(query);
        if (codes.size() == 0) {
            AppGlobals.getLog().log(Level.SEVERE, "ERROR: There are no program types with state code that starts with '"
                    + PREFIX_ELL_CODE + "' (for ELL)");
        }

        SubQuery subQuery = new SubQuery(ReferenceCode.class, ReferenceCode.COL_CODE, criteria);
        return subQuery;
    }

    /**
     * Returns a non-null map that relates all sections in the selected school's active schedule to
     * the number of enrolled 'SPED' students.
     *
     * @param sectionCount the number of sections in the master schedule
     * @param genderCode M for Male, F for female.
     *
     * @return A Map of String keys to Long values
     */
    private Map getSPEDCounts(int sectionCount, String genderCode) {
        Map spedCounts = new HashMap((int) (sectionCount * 1.5 + 1));

        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField studentDesigField = dictionary.findDataDictionaryFieldByAlias(ALIAS_STUDENT_DESIGNATION);
        if (studentDesigField != null) {
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(StudentSection.COL_SCHEDULE_OID, m_reportHelper.getScheduleOid());
            criteria.addIn(StudentSection.REL_STUDENT + PATH_DELIMITER + Student.COL_ENROLLMENT_STATUS, m_activeCodes);

            if (!StringUtils.isEmpty(genderCode)) {
                criteria.addBeginsWithIgnoreCase(StudentSection.REL_STUDENT + PATH_DELIMITER +
                        SisStudent.REL_PERSON + PATH_DELIMITER + SisPerson.COL_GENDER_CODE, genderCode);
            }

            criteria.addNotEmpty(StudentSection.REL_STUDENT + PATH_DELIMITER +
                    studentDesigField.getJavaName(), getBroker().getPersistenceKey());
            criteria.addNotEqualTo(StudentSection.REL_STUDENT + PATH_DELIMITER +
                    studentDesigField.getJavaName(), GIFTED_STUDENT_DISABILITY_CODE);

            String[] columns = new String[] {StudentSection.COL_SECTION_OID, "count(DISTINCT STD_OID)"};
            ReportQueryByCriteria query =
                    new ReportQueryByCriteria(m_reportHelper.getStudentSectionClass(), columns, criteria);
            query.addGroupBy(StudentSection.COL_SECTION_OID);

            ReportQueryIterator results = null;
            try {
                results = getBroker().getReportQueryIteratorByQuery(query);
                while (results.hasNext()) {
                    Object[] values = (Object[]) results.next();

                    String countString = values[1].toString();
                    Integer count = Integer.valueOf(countString);

                    spedCounts.put(values[0], count);
                }
            } finally {
                if (results != null) {
                    results.close();
                }
            }

        }

        return spedCounts;
    }
}
