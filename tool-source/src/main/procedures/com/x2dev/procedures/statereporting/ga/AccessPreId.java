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
package com.x2dev.procedures.statereporting.ga;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Race;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepDisability;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.tools.stateexports.StudentEnrollmentSpan;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.*;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Georgia state report for GA ACCESS for Pre-ID (WIDA) export.
 * This class implements the data export for GA ACCESS for Pre-ID (WIDA) export.
 *
 * @author X2 Development Corporation
 */
public class AccessPreId extends StateReportData {
    /**
     * Implementation of StateReportEntity to be used by the GA ACCESS for Pre-ID (WIDA) export.
     * This must be a public static inner class with a public no argument
     * constructor so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class AccessPreIdEntity extends StateReportEntity {
        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public AccessPreIdEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Intitialize.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc
         *      .core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);
        }
    }

    /**
     * Retrieves 504 plan status.
     *
     * @author Follett Software Company
     */
    protected class Retrieve504Status implements FieldRetriever {
        static final String CALC_ID = "504_PLAN";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String status504 = null;

            SisStudent student = (SisStudent) entity.getBean();

            String activeCode = PreferenceManager.getPreferenceValue(student.getSchool(),
                    SisPreferenceConstants.SECTION_504_STUDENT_ACTIVE_CODE);
            if (activeCode != null && activeCode.equalsIgnoreCase(student.getSection504StatusCode())) {
                status504 = "Y";
            }

            return status504;
        }
    }

    /**
     * Retrieves admin mode.
     *
     * @author Follett Software Company
     */
    protected class RetrieveAdminMode implements FieldRetriever {
        static final String CALC_ID = "ADMIN_MODE";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            return m_adminMode;
        }
    }

    /**
     * Retrieves Hispanic/Latino indicator, If True, return Y, If False, return Blank.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveEthnicity implements FieldRetriever {
        static final String CALC_ID = "HISPANIC";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String hispLatinoInd = null;

            SisStudent student = (SisStudent) entity.getBean();

            if (student.getPerson().getHispanicLatinoIndicator()) {
                hispLatinoInd = "Y";
            }

            return hispLatinoInd;
        }
    }

    /**
     * Retrieves state code of grade level.
     *
     * @author Follett Software Company
     */
    protected class RetrieveGradeLevel implements FieldRetriever {
        static final String CALC_ID = "GRADE_LVL";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String gradeLevel = null;

            SisStudent student = (SisStudent) entity.getBean();

            ReferenceCode gradeCode = getGradeLevelByDates(student, getCurrentContext().getStartDate(), m_reportDate);

            if (gradeCode != null) {
                gradeLevel = gradeCode.getStateCode();
            }

            if (gradeLevel.equals("KK")) {
                gradeLevel = "00";
            }

            return gradeLevel;
        }
    }

    /**
     * Retrieves iep status.
     *
     * @author Follett Software Company
     */
    protected class RetrieveIepStatus implements FieldRetriever {
        static final String CALC_ID = "IEP_STATUS";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String iepStatus = null;

            SisStudent student = (SisStudent) entity.getBean();

            if (m_stdActiveIepMap.containsKey(student.getOid()) &&
                    (student.getSpedExitDate() == null || student.getSpedExitDate().after(m_reportDate))) {
                iepStatus = "Y";
            }

            return iepStatus;
        }
    }

    /**
     * Retrieves organization number.
     * The District Number MUST = 9 characters.
     * Two digit State Abbreviation + 0000 + org.[DOE District]
     *
     * E.g. GA0000781
     *
     * @author Follett Software Company
     */
    protected class RetrieveOrgNumber implements FieldRetriever {
        static final String CALC_ID = "ORG_NUMBER";

        static final String STATE_NUMBER_PREFIX = "GA0000";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            return STATE_NUMBER_PREFIX + getOrganization().getFieldValueByAlias("DOE District");
        }
    }

    /**
     * Retrieves primary disability.
     *
     * @author X2 Development Corporation
     */
    protected class RetrievePrimaryDisability implements FieldRetriever {
        static final String CALC_ID = "PRIM_DISABILITY";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String primaryDisability = null;

            SisStudent student = (SisStudent) entity.getBean();
            Collection<IepDisability> stdDisabilities = m_stdDisabilityMap.get(student.getOid());

            String stateCode = null;
            if (stdDisabilities != null) {
                IepDisability disability = stdDisabilities.iterator().next();
                String disCode = disability.getDisabilityCode();
                ReferenceCode refCode = m_disabilityCodes.get(disCode);
                if (refCode != null) {
                    stateCode = refCode.getStateCode();
                }
            }

            if (stateCode != null) {
                switch (stateCode) {
                    case "6":
                        primaryDisability = "AS";
                        break;
                    case "2":
                        primaryDisability = "BD";
                        break;
                    case "8":
                        primaryDisability = "DD";
                        break;
                    case "W":
                    case "X":
                        primaryDisability = "HI";
                        break;
                    case "P":
                    case "Q":
                    case "R":
                    case "S":
                        primaryDisability = "ID";
                        break;
                    case "V":
                        primaryDisability = "OI";
                        break;
                    case "Y":
                        primaryDisability = "OHI";
                        break;
                    case "T":
                        primaryDisability = "SED";
                        break;
                    case "U":
                        primaryDisability = "SLD";
                        break;
                    case "3":
                        primaryDisability = "SLI";
                        break;
                    case "7":
                        primaryDisability = "TBI";
                        break;
                    case "Z":
                    case "1":
                        primaryDisability = "VI";
                        break;

                    default:
                        break;
                }
            }

            return primaryDisability;
        }
    }

    /**
     * Returns an indicator if the person is included in the specified race.
     * This implementation looks up all race codes for a person and finds
     * the race entry for the specified race. It returns a true/false value
     * indicating the presence of the race code record.
     *
     * The calculation parameter should be a reference code state code value in the reference table
     * for race codes.
     * In GA, this is:
     * "W" - White
     * "B" - Black
     * "S" - Asian
     * "I" - Indian/Native/Alaskan
     * "P" - Pacific
     *
     * Ex: "S" searches for the Asian code, returns "S" if present, blank otherwise.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveRace implements FieldRetriever {
        static final String CALC_ID = "RACE";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String raceInd = null;

            String param = (String) field.getParameter();

            SisStudent student = (SisStudent) entity.getBean();
            Collection<Race> races = m_stdRaceCodeMap.get(student.getPersonOid());

            // Find the reference code that we are looking for.
            ReferenceCode refCode = m_raceCodes.get(param);
            if (refCode != null && races != null) {
                for (Race race : races) {
                    if (refCode.getCode().equals(race.getRaceCode())) {
                        raceInd = "Y";
                        break;
                    }
                }
            }

            return raceInd;
        }
    }

    /**
     * Retieve school info considering overridden school code (Student.[DOE Override School Code])
     *
     * @author Follett Software Company
     */
    protected class RetrieveSchool implements FieldRetriever {
        static final String CALC_ID = "SCHOOL";

        static final String CALC_PARAM_NAME = "SCHOOL_NAME";
        static final String CALC_PARAM_NUMBER = "SCHOOL_NUMBER";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String value = null;

            SisStudent student = (SisStudent) entity.getBean();
            String schoolCode = m_stdOverrideSklCodeMap.get(student.getOid());

            SisSchool school = null;

            if (!StringUtils.isEmpty(schoolCode)) {
                school = m_schoolCodeMap.get(schoolCode);
            } else {
                school = student.getSchool();
            }

            if (CALC_PARAM_NAME.equals(field.getParameter())) {
                value = school.getName();
            } else if (CALC_PARAM_NUMBER.equals(field.getParameter())) {
                value = (String) school.getFieldValueByBeanPath(m_fieldSklCode);
            }

            return value;
        }
    }

    /*
     * ALIASES
     */
    private static final String ALIAS_ELL = "DOE ELL";
    private static final String ALIAS_SCHOOL_CODE = "DOE School";
    private static final String ALIAS_STD_SCHOOL_CODE = "DOE Override School Code";

    /*
     * INPUT PARAMETERS
     */
    private static final String INPUT_PARAM_ADMIN_MODE = "testFormat";
    private static final String INPUT_PARAM_REPORT_DATE = "reportDate";

    String m_adminMode = null;
    Map<String, ReferenceCode> m_disabilityCodes = null;
    String m_fieldEll = null;
    String m_fieldStdSklCode = null;
    String m_fieldSklCode = null;
    Map<String, ReferenceCode> m_referenceGradeCodeMap;
    StudentHistoryHelper m_historyHelper = null;
    Map<String, ReferenceCode> m_raceCodes;
    PlainDate m_reportDate = null;
    Map<String, SisSchool> m_schoolCodeMap = null;
    Map<String, String> m_stdOverrideSklCodeMap = null;

    /*
     * A map of student's IEPs that are active on report date, for use in the IEP retriever
     */
    Map<String, IepData> m_stdActiveIepMap = new HashMap<String, IepData>();
    /*
     * A map of student's primary disabilities that are active on report date, for use in the
     * disability retriever
     */
    Map<String, Collection<IepDisability>> m_stdDisabilityMap;
    /*
     * A map of student's races, for use in the race retriever
     */
    Map<String, Collection<Race>> m_stdRaceCodeMap;

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    public void initialize() throws X2BaseException {
        super.initialize();

        initializeFields();

        m_historyHelper = new StudentHistoryHelper(this);
        m_historyHelper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_SNAPSHOT);
        m_reportDate = (PlainDate) getParameter(INPUT_PARAM_REPORT_DATE);
        m_historyHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);
        m_historyHelper.getStudentCriteria().addIn(m_fieldEll,
                getStateReportableCodes(SisStudent.class, m_fieldEll, "Y"));

        setQuery(m_historyHelper.getStudentQuery(true));
        setEntityClass(AccessPreIdEntity.class);

        // Map of IepData on report date with active status
        X2Criteria activeIepCriteria = new X2Criteria();

        X2Criteria endDateCriteria = new X2Criteria();
        endDateCriteria.addGreaterOrEqualThan(IepData.COL_END_DATE, m_reportDate);
        X2Criteria orCriteria = new X2Criteria();
        orCriteria.addEmpty(IepData.COL_END_DATE, getBroker().getPersistenceKey());
        endDateCriteria.addOrCriteria(orCriteria);

        activeIepCriteria.addLessOrEqualThan(IepData.COL_START_DATE, m_reportDate);
        activeIepCriteria.addAndCriteria(endDateCriteria);

        activeIepCriteria.addIn(IepData.COL_STATUS_CODE, Arrays.asList(
                Integer.valueOf(IepData.StatusCode.ACTIVE.ordinal()),
                Integer.valueOf(IepData.StatusCode.PREVIOUS.ordinal())));
        SubQuery studentSubQuery =
                new SubQuery(SisStudent.class, X2BaseBean.COL_OID, m_historyHelper.getStudentCriteria());
        activeIepCriteria.addIn(IepData.COL_STUDENT_OID, studentSubQuery);
        QueryByCriteria iepQuery = new QueryByCriteria(IepData.class, activeIepCriteria);
        m_stdActiveIepMap = getBroker().getMapByQuery(iepQuery, IepData.COL_STUDENT_OID, 200);

        // Get state race code reference codes for use in the race retriever.
        Criteria raceCriteria = new Criteria();
        raceCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, "rtbRaceCodes");
        QueryByCriteria raceCodeQuery = new QueryByCriteria(ReferenceCode.class, raceCriteria);
        m_raceCodes = getBroker().getMapByQuery(raceCodeQuery, ReferenceCode.COL_STATE_CODE, 5);

        /*
         * Load the race codes for all students included in the export.
         */
        SubQuery personSubQuery =
                new SubQuery(SisStudent.class, SisStudent.COL_PERSON_OID, m_historyHelper.getStudentCriteria());
        X2Criteria studentRaceCriteria = new X2Criteria();
        studentRaceCriteria.addIn(Race.COL_PERSON_OID, personSubQuery);
        QueryByCriteria raceQuery = new QueryByCriteria(Race.class, studentRaceCriteria);
        m_stdRaceCodeMap = getBroker().getGroupedCollectionByQuery(raceQuery, Race.COL_PERSON_OID, 100);

        // Get disability code reference codes for use in the race retriever.
        Criteria disabilityCriteria = new Criteria();
        disabilityCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, "rtbSpedDisab");
        QueryByCriteria disabilityCodeQuery = new QueryByCriteria(ReferenceCode.class, disabilityCriteria);
        m_disabilityCodes = getBroker().getMapByQuery(disabilityCodeQuery, ReferenceCode.COL_CODE, 5);

        /*
         * Load disability codes for all students included in the export.
         */
        X2Criteria studentDisabilityCriteria = new X2Criteria();

        studentDisabilityCriteria.addEqualTo(IepDisability.COL_PRIMARY_INDICATOR, BooleanAsStringConverter.TRUE);
        studentDisabilityCriteria.addIn(IepDisability.COL_STUDENT_OID, m_stdActiveIepMap.keySet());
        QueryByCriteria studentQuery = new QueryByCriteria(IepDisability.class, studentDisabilityCriteria);
        studentQuery.addOrderBy(X2BaseBean.COL_LAST_MODIFIED_TIME, false);
        m_stdDisabilityMap = getBroker().getGroupedCollectionByQuery(studentQuery, IepDisability.COL_STUDENT_OID, 100);

        /*
         * Load students with overridden school codes.
         */
        m_stdOverrideSklCodeMap = new HashMap<String, String>();

        X2Criteria stdWithSklCodesCriteria = new X2Criteria();
        stdWithSklCodesCriteria.addNotEmpty(m_fieldStdSklCode, getBroker().getPersistenceKey());
        stdWithSklCodesCriteria.addIn(X2BaseBean.COL_OID, studentSubQuery);

        String[] columns = {X2BaseBean.COL_OID, m_fieldStdSklCode};
        ReportQueryByCriteria stdWithSklCode =
                new ReportQueryByCriteria(SisStudent.class, columns, stdWithSklCodesCriteria);
        ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(stdWithSklCode);
        try {
            while (iterator.hasNext()) {
                Object[] items = (Object[]) iterator.next();
                String stdOid = (String) items[0];
                String stdSklCode = (String) items[1];

                m_stdOverrideSklCodeMap.put(stdOid, stdSklCode);
            }
        } finally {
            iterator.close();
        }

        /*
         * Load "school code-school" map
         */
        QueryByCriteria schoolQuery = new QueryByCriteria(SisSchool.class, new X2Criteria());
        m_schoolCodeMap = getBroker().getMapByQuery(schoolQuery, m_fieldSklCode, 200);

        Map<String, FieldRetriever> retrieversMap = new HashMap<String, FieldRetriever>();
        retrieversMap.put(RetrieveOrgNumber.CALC_ID, new RetrieveOrgNumber());
        retrieversMap.put(RetrieveGradeLevel.CALC_ID, new RetrieveGradeLevel());
        retrieversMap.put(RetrieveRace.CALC_ID, new RetrieveRace());
        retrieversMap.put(RetrieveEthnicity.CALC_ID, new RetrieveEthnicity());
        retrieversMap.put(RetrieveIepStatus.CALC_ID, new RetrieveIepStatus());
        retrieversMap.put(Retrieve504Status.CALC_ID, new Retrieve504Status());
        retrieversMap.put(RetrievePrimaryDisability.CALC_ID, new RetrievePrimaryDisability());
        retrieversMap.put(RetrieveAdminMode.CALC_ID, new RetrieveAdminMode());
        retrieversMap.put(RetrieveSchool.CALC_ID, new RetrieveSchool());

        addCalcs(retrieversMap);

        m_adminMode = (String) getParameter(INPUT_PARAM_ADMIN_MODE);
    }

    /**
     * Returns reference code of grade based on dates.
     *
     * @param student SisStudent
     * @param startDate PlainDate
     * @param endDate PlainDate
     * @return Reference code
     */
    ReferenceCode getGradeLevelByDates(SisStudent student, PlainDate startDate, PlainDate endDate) {
        if (m_referenceGradeCodeMap == null) {
            m_referenceGradeCodeMap = loadRefCodeMapByField(SisStudent.class, SisStudent.COL_GRADE_LEVEL);
        }
        List<StudentEnrollmentSpan> spans = m_historyHelper.getStudentEnrollmentSpans(student, true);
        Collections.sort(spans, new Comparator<StudentEnrollmentSpan>() {
            @Override
            public int compare(StudentEnrollmentSpan o1, StudentEnrollmentSpan o2) {
                return o1.getFirstActiveEnrollment().getEnrollmentDate()
                        .compareTo(o2.getFirstActiveEnrollment().getEnrollmentDate());
            }
        });

        int yog = student.getYog();
        for (StudentEnrollmentSpan span : spans) {
            if (!span.getFirstActiveEnrollment().getEnrollmentDate().after(endDate) &&
                    (span.getFirstInactiveEnrollment() == null
                            || !span.getFirstInactiveEnrollment().getEnrollmentDate().before(startDate))) {
                yog = getYogOfSpan(span);
            }
        }

        ReferenceCode gradeCode = null;
        if (yog == student.getYog()) {
            String gradeLevel = student.getGradeLevel();
            gradeCode = m_referenceGradeCodeMap.get(gradeLevel);
        } else {
            ModelBroker broker = new ModelBroker(getPrivilegeSet());
            TreeMap sortedGradeLevels = StudentManager.buildGradeLevelMap(broker);
            int maxGradeLevel = StudentManager.getMaxGradeLevel(getBroker());
            List<String> matchingGradeLevels = StudentManager.getMatchingGradeLevels(maxGradeLevel, yog,
                    getCurrentContext().getSchoolYear(), sortedGradeLevels);
            for (String matchingGradeLevel : matchingGradeLevels) {
                gradeCode = m_referenceGradeCodeMap.get(matchingGradeLevel);
                if (gradeCode != null) {
                    break;
                }
            }
        }
        return gradeCode;
    }

    /**
     * Returns state reportable codes.
     *
     * @param string
     * @param m_fieldEll2
     * @param class1
     *
     * @return Collection<String> codes
     */
    private Collection<String> getStateReportableCodes(Class<SisStudent> beanClass,
                                                       String beanPath,
                                                       String matchValue) {
        Set<String> codes = new HashSet<String>();
        DataDictionaryField field = getDataDictionaryField(beanClass, beanPath);
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, field.getReferenceTableOid());
        criteria.addEqualTo(ReferenceCode.COL_STATE_CODE, matchValue);

        String[] columns = new String[] {ReferenceCode.COL_CODE};
        ReportQueryByCriteria query = new ReportQueryByCriteria(ReferenceCode.class, columns, criteria);

        ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                Object[] record = (Object[]) iterator.next();
                String code = (String) record[0];
                codes.add(code);
            }
        } finally {
            iterator.close();
        }
        if (codes.isEmpty()) {
            codes.add("--No-Match--");
        }
        return codes;
    }

    /**
     * Gets the yog of span.
     *
     * @param span StudentEnrollmentSpan
     * @return int
     */
    /*
     * get the yog from the first active enrollment, unless there exists a YOG_CHANGE record
     * immediately following the first active enrollment on the same date for the same school.
     * In this case, use the yog from the YOG_CHANGE record.
     */
    private int getYogOfSpan(StudentEnrollmentSpan span) {
        int value = 0;
        if (span.getFirstActiveEnrollment() != null) {
            value = span.getFirstActiveEnrollment().getYog();

            Collection<StudentEnrollment> enrollments = span.getEnrollments();
            Iterator<StudentEnrollment> iterator = enrollments.iterator();
            while (iterator.hasNext()) {
                StudentEnrollment enrollment = iterator.next();
                if (enrollment == span.getFirstActiveEnrollment()) {
                    if (iterator.hasNext()) {
                        StudentEnrollment nextEnrollment = iterator.next();
                        if (yogEliminate(nextEnrollment, enrollment)) {
                            value = nextEnrollment.getYog();
                        }
                    }
                    break;
                }
            }
        }
        return value;
    }

    /**
     * Initializes fields.
     */
    private void initializeFields() {
        m_fieldEll = translateAliasToJavaName(ALIAS_ELL, true);
        m_fieldStdSklCode = translateAliasToJavaName(ALIAS_STD_SCHOOL_CODE, true);
        m_fieldSklCode = translateAliasToJavaName(ALIAS_SCHOOL_CODE, true);
    }

    /**
     * Load reference code map by field name.
     *
     * @param beanClass Class
     * @param fieldName String
     * @return Map<String, ReferenceCode>
     */
    private Map<String, ReferenceCode> loadRefCodeMapByField(Class beanClass, String fieldName) {
        Map<String, ReferenceCode> refCodeMap = null;
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        ModelProperty prop = new ModelProperty(beanClass, fieldName, getBroker().getPersistenceKey());
        DataDictionaryField field = dictionary.findDataDictionaryField(prop.getFieldId());
        if (field != null) {
            ReferenceTable referenceTable = field.getReferenceTable();
            refCodeMap = referenceTable.getCodeMap();
        }
        return refCodeMap;
    }

    /**
     * Yog eliminate.
     *
     * @param yogEnrollment StudentEnrollment
     * @param previousEnrollment StudentEnrollment
     * @return true, if successful
     */
    private boolean yogEliminate(StudentEnrollment yogEnrollment, StudentEnrollment previousEnrollment) {
        return (StudentEnrollment.YOG_CHANGE.equals(yogEnrollment.getEnrollmentType()) &&
                StudentEnrollment.ENTRY.equals(previousEnrollment.getEnrollmentType()) &&
                (yogEnrollment.getEnrollmentDate() != null && previousEnrollment.getEnrollmentDate() != null
                        && yogEnrollment.getEnrollmentDate().equals(previousEnrollment.getEnrollmentDate()))
                &&
                (yogEnrollment.getSchool() != null && previousEnrollment.getSchool() != null
                        && yogEnrollment.getSchool().equals(previousEnrollment.getSchool())));
    }
}
