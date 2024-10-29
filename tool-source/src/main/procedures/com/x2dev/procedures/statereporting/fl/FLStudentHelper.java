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
package com.x2dev.procedures.statereporting.fl;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import static com.x2dev.procedures.statereporting.fl.FLStateReportData.ALIAS_WDIS_AGE_INDICTOR;
import static com.x2dev.procedures.statereporting.fl.FLStateReportData.WDIS_MODE_ADULT_GENERAL_EDUCATION;
import static com.x2dev.procedures.statereporting.fl.FLStateReportData.WDIS_MODE_BOTH;
import static com.x2dev.procedures.statereporting.fl.FLStateReportData.WDIS_MODE_POST_SECONDARY_CTE;
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.*;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.x2dev.procedures.statereporting.fl.FLScheduleHelper.MasterScheduleInfo;
import com.x2dev.procedures.statereporting.fl.FLStateReportData.SchoolCalendarInfo;
import com.x2dev.procedures.statereporting.fl.FLStateReportData.SurveyPeriod;
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.StudentScheduleHelper.StudentScheduleInfo;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.business.GradesManager;
import com.x2dev.sis.tools.stateexports.StudentEnrollmentSpan;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.sis.tools.stateexports.StudentScheduleSpan;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.X2RuntimeException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.util.*;
import java.util.Map.Entry;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;
import jersey.repackaged.com.google.common.collect.ImmutableSet;

/**
 * Class for common student selection methods.
 *
 * @author Follett Software Company
 */
public class FLStudentHelper extends StudentHistoryHelper {

    /**
     * The Class StudentPgmCPCHelper.
     */
    public class CompletionPointHelper {

        /**
         * The Class CompletionPointCode.
         */
        public class CompletionPointCode {
            private String m_code;
            private PlainDate m_date;

            /**
             * Instantiates a new completion point code.
             *
             * @param code String
             * @param date PlainDate
             */
            public CompletionPointCode(String code, PlainDate date) {
                m_code = code;
                m_date = date;
            }

            /**
             * Gets the code.
             *
             * @return String
             */
            public String getCode() {
                return m_code;
            }

            /**
             * Gets the date.
             *
             * @return Plain date
             */
            public PlainDate getDate() {
                return m_date;
            }
        }

        private static final String ALIAS_CPC_DEFINITION_IND = "cpc-def-indicator";
        private static final String ALIAS_CPC_POINT = "cpc-point";
        private static final String ALIAS_CPC_PROGRAM = "cpc-program";

        private static final String CPC_DEF_IND = "cpcdefinition";

        private static final String DDX_CPC_DEF = "FL-UDA-CPC-DEF";

        private static final int INDEX_PROGRAM_CODE = 0;
        private static final int INDEX_CPC = 1;
        private static final int INDEX_CRS_OID = 2;

        private final Collection m_literacyPrograms =
                Arrays.asList("9900010", "9900099", "9900130", "9900300", "9900100", "S990001",
                        "9900000", "9900040", "9900050", "9900051", "9900090");

        private HashMap<String, HashMap<String, Collection<String>>> m_pgmCpcCrs = new HashMap<>();

        private StudentScheduleHelper m_studentScheduleHelper;

        /**
         * Instantiates a new student pgm CPC helper.
         *
         * @param date PlainDate
         */
        public CompletionPointHelper(PlainDate date) {
            FLScheduleHelper scheduleHelper = new FLScheduleHelper(getFLData(),
                    getFLData().getCurrentContext().getStartDate(), date);
            m_studentScheduleHelper =
                    new StudentScheduleHelper(scheduleHelper, getFLData().getCurrentContext().getStartDate(), date);

            X2Criteria ddxCriteria = new X2Criteria();
            ddxCriteria.addEqualTo(ExtendedDataDictionary.COL_ID, DDX_CPC_DEF);
            QueryByCriteria ddxQuery = new QueryByCriteria(ExtendedDataDictionary.class, ddxCriteria);
            ExtendedDataDictionary ddx = (ExtendedDataDictionary) getFLData().getBroker().getBeanByQuery(ddxQuery);

            X2Criteria cpcCriteria = new X2Criteria();
            DataDictionary dictionary =
                    DataDictionary.getDistrictDictionary(ddx, getFLData().getBroker().getPersistenceKey());
            DataDictionaryField fieldDefIndicator =
                    getFLData().translateAliasToDictionaryField(dictionary, ALIAS_CPC_DEFINITION_IND, true);
            if (fieldDefIndicator != null) {
                cpcCriteria.addEqualTo(
                        UserDefinedTableB.REL_USER_DEFINED_TABLE_A + PATH_DELIMITER + UserDefinedTableA.COL_FIELD_B001,
                        CPC_DEF_IND);
            }

            DataDictionaryField fieldPgmCode =
                    getFLData().translateAliasToDictionaryField(dictionary, ALIAS_CPC_PROGRAM, true);
            DataDictionaryField fieldPoint =
                    getFLData().translateAliasToDictionaryField(dictionary, ALIAS_CPC_POINT, true);

            String[] attributes =
                    {UserDefinedTableB.REL_USER_DEFINED_TABLE_A + PATH_DELIMITER + fieldPgmCode.getJavaName(),
                            UserDefinedTableB.REL_USER_DEFINED_TABLE_A + PATH_DELIMITER + fieldPoint.getJavaName(),
                            UserDefinedTableB.COL_COURSE_OID};

            ReportQueryByCriteria reportQuery =
                    new ReportQueryByCriteria(UserDefinedTableB.class, attributes, cpcCriteria);
            try (ReportQueryIterator iterator = getFLData().getBroker().getReportQueryIteratorByQuery(reportQuery)) {
                while (iterator.hasNext()) {
                    Object[] row = (Object[]) iterator.next();

                    String programCode = (String) row[INDEX_PROGRAM_CODE];
                    String cpc = (String) row[INDEX_CPC];
                    String crsOid = (String) row[INDEX_CRS_OID];

                    HashMap<String, Collection<String>> cpcCodeCrsOids = m_pgmCpcCrs.get(programCode);
                    if (cpcCodeCrsOids == null) {
                        cpcCodeCrsOids = new HashMap<>();
                        m_pgmCpcCrs.put(programCode, cpcCodeCrsOids);
                    }
                    Collection<String> crsOids = cpcCodeCrsOids.get(cpc);
                    if (crsOids == null) {
                        crsOids = new ArrayList<>();
                        cpcCodeCrsOids.put(cpc, crsOids);
                    }
                    crsOids.add(crsOid);
                }
            }
        }

        /**
         * Gets the completion point code.
         *
         * @param student SisStudent
         * @param programCode String
         * @return String
         */
        public String getCompletionPointCode(SisStudent student, String programCode) {
            List<CompletionPointCode> earnedCodes = getEarnedCompletionPointCodes(student, programCode);

            StringBuilder codes = new StringBuilder();
            for (CompletionPointCode code : earnedCodes) {
                codes.append(code.getCode());
            }
            return codes.toString();
        }

        /**
         * Gets the literacy point date.
         *
         * @param student SisStudent
         * @param programCode String
         * @return Plain date
         */
        public PlainDate getLiteracyPointDate(SisStudent student, String programCode) {
            PlainDate litPointDate = null;
            if (m_literacyPrograms.contains(programCode)) {
                litPointDate = getLatestCodeDate(student, programCode);
            }
            return litPointDate;
        }

        /**
         * Gets the completion point codes.
         *
         * @param student SisStudent
         * @param programCode String
         * @return Collection
         */
        private List<CompletionPointCode> getEarnedCompletionPointCodes(SisStudent student, String programCode) {
            Collection<StudentScheduleInfo> stdInfos = m_studentScheduleHelper.getStudentScheduleInfo(student);

            List<CompletionPointCode> earnedCodes = new ArrayList<>();

            HashMap<String, Collection<String>> cpcPointCrsOids = m_pgmCpcCrs.get(programCode);

            if (cpcPointCrsOids == null) {
                // CPC cannot be determined without assigned to the course program with CPC.
            } else {
                for (String cpc : cpcPointCrsOids.keySet()) {
                    Collection<String> coursesToEarnPoint = cpcPointCrsOids.get(cpc);

                    Collection<String> stdCourses = new ArrayList<>();
                    PlainDate latestExitDate = null;
                    // Count only student's courses with earned credits.
                    for (StudentScheduleInfo info : stdInfos) {
                        Transcript transcript = info.getTranscript();
                        BigDecimal creditBD = transcript.getTotalCredit();
                        String curCourseOid = info.getCourse().getOid();
                        if (coursesToEarnPoint.contains(curCourseOid) && creditBD.doubleValue() > 0) {
                            if (latestExitDate == null || latestExitDate.before(info.getExitDate())) {
                                latestExitDate = info.getExitDate();
                            }
                            stdCourses.add(info.getCourse().getOid());
                        }
                    }

                    // Check if student has credits for all needed courses to earn the point.
                    boolean isPointEarned = true;
                    for (String courseOid : coursesToEarnPoint) {
                        if (!stdCourses.contains(courseOid)) {
                            isPointEarned = false;
                        }
                    }

                    if (isPointEarned) {
                        earnedCodes.add(new CompletionPointCode(cpc, latestExitDate));
                    }
                }
            }

            Collections.sort(earnedCodes, new Comparator<CompletionPointCode>() {
                @Override
                public int compare(CompletionPointCode o1, CompletionPointCode o2) {
                    return o1.getDate().compareTo(o2.getDate());
                }
            });

            return earnedCodes;
        }

        /**
         * Gets the latest code date.
         *
         * @param student SisStudent
         * @param programCode String
         * @return Plain date
         */
        private PlainDate getLatestCodeDate(SisStudent student, String programCode) {
            PlainDate latestCodeDate = null;
            List<CompletionPointCode> earnedCodes = getEarnedCompletionPointCodes(student, programCode);
            if (earnedCodes.size() > 0) {
                latestCodeDate = earnedCodes.get(earnedCodes.size() - 1).getDate();
            }
            return latestCodeDate;
        }
    }

    /**
     * The Class FteCalculator.
     */
    public class FteCalculator {
        private static final String ALIAS_FTE_MULTIPLIER_PK_TO_03 = "fl-org-FteMultiplierPKto03";
        private static final String ALIAS_FTE_MULTIPLIER_04_TO_12 = "fl-org-FteMultiplier04to12";
        private static final String ALIAS_YEAR = "ora-survey-year";
        private static final String ORA_DDX_ID = "FL-ORA-COST-FACTOR";

        private final List<String> COST_FACTOR_ALIASES = Arrays.asList("ora-cost-factor-101", "ora-cost-factor-102",
                "ora-cost-factor-103", "ora-cost-factor-111", "ora-cost-factor-112", "ora-cost-factor-113",
                "ora-cost-factor-254", "ora-cost-factor-255", "ora-cost-factor-130", "ora-cost-factor-300");

        private double m_fteMultiplierPKto03 = 0.0;
        private double m_fteMultiplier04to12 = 0.0;
        private Map<String, BigDecimal> m_mapCostFactors = new HashMap();

        /**
         * Instantiates a new fte calculator.
         *
         * @throws X2BaseException exception
         */
        public FteCalculator() throws X2BaseException {
            FLStateReportData data = getFLData();
            DataDictionaryField fieldMultiplierPKto03 =
                    data.translateAliasToDictionaryField(ALIAS_FTE_MULTIPLIER_PK_TO_03, true);
            DataDictionaryField fieldMultiplier04to12 =
                    data.translateAliasToDictionaryField(ALIAS_FTE_MULTIPLIER_04_TO_12, true);

            if (fieldMultiplierPKto03 != null && fieldMultiplier04to12 != null) {
                Object m_objPKto03 = null;
                Object m_obj04to12 = null;
                try {
                    m_objPKto03 = data.getFieldValue(data.getOrganization(), fieldMultiplierPKto03);
                    m_obj04to12 = data.getFieldValue(data.getOrganization(), fieldMultiplier04to12);
                } catch (X2BaseException e) {
                    // Add setup error based on null result
                }
                if (m_objPKto03 != null && m_objPKto03 instanceof BigDecimal && m_obj04to12 != null
                        && m_obj04to12 instanceof BigDecimal) {
                    m_fteMultiplierPKto03 = ((BigDecimal) m_objPKto03).doubleValue();
                    m_fteMultiplier04to12 = ((BigDecimal) m_obj04to12).doubleValue();
                } else {
                    data.addSetupError("The necessary multipliers for FTE are not defined", "FTE multiplier Error");
                }
            }
            initCostFactors();
        }

        /**
         * Gets the fte value.
         *
         * @param gradeLevel the grade level
         * @param minutesPerWeek the minutes per week
         * @return the fte value
         */
        public int getFteValue(String gradeLevel, Integer minutesPerWeek) {
            double multiplier = 0.0;
            if (GRADE_LEVELS_PK_3.contains(gradeLevel)) {
                multiplier = m_fteMultiplierPKto03;
            } else if (GRADE_LEVELS_4_8.contains(gradeLevel) || GRADE_LEVELS_9_12.contains(gradeLevel)) {
                multiplier = m_fteMultiplier04to12;
            }

            return (int) Math.ceil(multiplier * minutesPerWeek.doubleValue() * 10000.0);
        }

        /**
         * Gets the fte value from credit.
         *
         * @param gradeLevel String
         * @param credit BigDecimal
         * @return int
         */
        public int getFteValueFromCredit(String gradeLevel, BigDecimal credit) {
            Integer minutes = Integer.valueOf((int) Math.ceil(500.0 * credit.doubleValue()));
            return getFteValue(gradeLevel, minutes);
        }

        /**
         * Gets the weighted fte value.
         *
         * @param gradeLevel String
         * @param fefpProgramCode String
         * @param minutesPerWeek Integer
         * @return int
         */
        public int getWeightedFteValue(String gradeLevel, String fefpProgramCode, Integer minutesPerWeek) {
            double multiplier = 0.0;
            if (GRADE_LEVELS_PK_3.contains(gradeLevel)) {
                multiplier = m_fteMultiplierPKto03;
            } else if (GRADE_LEVELS_4_8.contains(gradeLevel) || GRADE_LEVELS_9_12.contains(gradeLevel)) {
                multiplier = m_fteMultiplier04to12;
            }
            if (m_mapCostFactors.containsKey(fefpProgramCode)) {
                multiplier *= m_mapCostFactors.get(fefpProgramCode).doubleValue();
            } else {
                multiplier = 0.0;
            }

            return (int) Math.ceil(multiplier * minutesPerWeek.doubleValue() * 10000.0);
        }

        /**
         * Gets the weighted fte value.
         *
         * @param fefpProgramCode String
         * @param fte double
         * @return int
         */
        public int getWeightedFteValue(String fefpProgramCode, double fte) {
            double multiplier = 0.0;
            if (m_mapCostFactors.containsKey(fefpProgramCode)) {
                multiplier = m_mapCostFactors.get(fefpProgramCode).doubleValue();
            } else {
                multiplier = 0.0;
            }
            return (int) Math.ceil(multiplier * fte * 10000.0);
        }

        /**
         * Gets the weighted fte value from credit.
         *
         * @param gradeLevel String
         * @param fefpProgramCode String
         * @param credit BigDecimal
         * @return int
         */
        public int getWeightedFteValueFromCredit(String gradeLevel, String fefpProgramCode, BigDecimal credit) {
            Integer minutes = Integer.valueOf((int) Math.ceil(500.0 * credit.doubleValue()));
            return getWeightedFteValue(gradeLevel, fefpProgramCode, minutes);
        }

        /**
         * Alias not found.
         *
         * @param alias String
         * @return X2RuntimeException
         */
        private X2RuntimeException aliasNotFound(String alias) {
            return new X2RuntimeException(new UnsupportedOperationException(
                    "Extended data dictionary " + ORA_DDX_ID + " does not contain alias " + alias + "."));
        }

        /**
         * Inits the cost factors.
         *
         * @throws X2BaseException exception
         */
        private void initCostFactors() throws X2BaseException {
            X2Criteria ddxCriteria = new X2Criteria();

            ddxCriteria.addEqualTo(ExtendedDataDictionary.COL_ID, ORA_DDX_ID);

            QueryByCriteria ddxQuery = new QueryByCriteria(ExtendedDataDictionary.class, ddxCriteria);
            ExtendedDataDictionary ddx = (ExtendedDataDictionary) getFLData().getBroker().getBeanByQuery(ddxQuery);
            if (ddx == null) {
                throw new X2RuntimeException(new UnsupportedOperationException(
                        "Extended data dictionary " + ORA_DDX_ID + " was not found."));
            }
            DataDictionary dictionary =
                    DataDictionary.getDistrictDictionary(ddx, getFLData().getBroker().getPersistenceKey());
            DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(ALIAS_YEAR);
            if (field == null) {
                throw aliasNotFound(ALIAS_YEAR);
            }
            String fieldYear = field.getJavaName();
            X2Criteria criteria = new X2Criteria();

            String year = Integer.toString(getFLData().getCurrentContext().getSchoolYear());
            criteria.addEqualTo(OrganizationAttributes.COL_EXTENDED_DATA_DICTIONARY_OID, ddx.getOid());
            criteria.addEqualTo(fieldYear, year);

            QueryByCriteria query = new QueryByCriteria(OrganizationAttributes.class, criteria);
            OrganizationAttributes ora = (OrganizationAttributes) getFLData().getBroker().getBeanByQuery(query);
            if (ora == null) {
                throw new X2RuntimeException(new UnsupportedOperationException(
                        "Extended data dictionary bean " + ORA_DDX_ID + " was not found for year " + year + "."));
            }
            for (String alias : COST_FACTOR_ALIASES) {
                field = dictionary.findDataDictionaryFieldByAlias(alias);
                if (field == null) {
                    throw aliasNotFound(alias);
                }
                String costCode = alias.substring(alias.length() - 3);
                Object value = getFLData().getFieldValue(ora, field);
                m_mapCostFactors.put(costCode, (BigDecimal) value);
            }
        }
    }

    /**
     * The Class EnrollmentSnapshot.
     */
    public class EnrollmentSnapshot {
        private PlainDate m_date = null;
        private String m_enrollmentStatus = null;
        private boolean m_isPrecise = true;
        private SisSchool m_school = null;
        private SisStudent m_student = null;
        private int m_yog = 0;

        /**
         * Constructs an EnrollmentSnapshop for the student as of the given date. All parameters are
         * expected to have non-null values.
         *
         * @param student the student
         * @param date the date
         */
        public EnrollmentSnapshot(SisStudent student, PlainDate date) {
            m_student = student;
            m_date = date;

            if (student != null && date != null) {
                initialize();
            } else {
                m_isPrecise = false;
            }
        }

        /**
         * Returns the date represented by this snapshot.
         *
         * @return PlainDate
         */
        public PlainDate getDate() {
            return m_date;
        }

        /**
         * Returns the enrollment status for the student on the date represented by this snapshot.
         *
         * @return String
         */
        public String getEnrollmentStatus() {
            return m_enrollmentStatus;
        }

        /**
         * Returns the student represented by this snapshot.
         *
         * @return Student
         */
        public SisStudent getStudent() {
            return m_student;
        }

        /**
         * Returns the school for the student on the date represented by this snapshot.
         *
         * @return School
         */
        public SisSchool getSchool() {
            return m_school;
        }

        /**
         * Returns the year of graduation for the student on the date represented by this snapshot.
         *
         * @return int
         */
        public int getYog() {
            return m_yog;
        }

        /**
         * Returns true if the snapshot is an accurate representation of the student's properties on
         * the
         * given date, false otherwise. A snapshot is not precise if there is no enrollment record
         * on or
         * before the given date. This could occur in two situations:
         * <ol>
         * <li>The student's enrollment date is complete but the given date is prior to the
         * student's
         * first enrollment.
         * <li>The student's enrollment data is incomplete.
         * </ol>
         * When a snapshot is imprecise then the values either come from the first enrollment record
         * after the given date (if present) or directly from the student (if no enrollment records
         * exist for the student at all).
         *
         * @return boolean
         */
        public boolean isPrecise() {
            return m_isPrecise;
        }

        /**
         * Derives the student's enrollment snapshot (school, enrollment status, and YOG) from the
         * first
         * enrollment record after the report date (e1) and the last enrollment record on or before
         * the
         * report date (e0).
         *
         * <pre>
         *      time ---------+-------------@-----------+--------->
         *                    e0          report        e1
         *                                 date
         * </pre>
         * <p>
         * If e1 is null then there has been no changes to the student's snapshop since the report
         * date.
         * In this case we get all three values directly from propties on the student bean.
         * <p>
         * If e1 is not null then the values are based on the enrollment type according to the
         * following
         * table:
         *
         * <pre>
         *     +---------++--------+--------+-----+
         *     |         || ENROLL |        |     |
         *     | e1.type || STATUS | SCHOOL | YOG |
         *     +=========++========+========+=====+
         *     | Y       || e1     | e1     | e0* |
         *     +---------++--------+--------+-----+
         *     | S       || e0*    | e1     | e1  |
         *     +---------++--------+--------+-----+
         *     | W       || e0*    | e1     | e1  |
         *     +---------++--------+--------+-----+
         *     | E       || e0*    | e0*    | e0* |
         *     +---------++--------+--------+-----+
         * </pre>
         * <p>
         * * If e0 is null then there is no way to get accurate data for this property. In such a
         * case
         * we use the property from e1 which is the best we can do - it might be correct or it might
         * be wrong. The isPrecise flag will be set to false.
         */
        private void initialize() {
            StudentEnrollment e0 = null;
            StudentEnrollment e1 = null;

            List<StudentEnrollment> enrollmentList = FLStudentHelper.this.getStudentEnrollments(m_student);

            if (enrollmentList != null) {
                Iterator<StudentEnrollment> enrollments = enrollmentList.iterator();
                while (enrollments.hasNext()) {
                    StudentEnrollment enrollment = enrollments.next();
                    if (enrollment.getEnrollmentDate() != null) {
                        if (enrollment.getEnrollmentDate().after(getDate())) {
                            e1 = enrollment;
                            break;
                        }
                        e0 = enrollment;
                    }
                }
            }


            if (e1 == null) {
                m_enrollmentStatus = m_student.getEnrollmentStatus();
                m_school = m_student.getSchool();
                m_yog = m_student.getYog();
            } else {
                if (e1.getEnrollmentType().equals(StudentEnrollment.YOG_CHANGE)) {
                    m_enrollmentStatus = e1.getStatusCode();
                    m_school = e1.getSchool();
                    m_yog = (e0 != null ? e0.getYog() : e1.getYog());
                } else if (e1.getEnrollmentType().equals(StudentEnrollment.STATUS_CHANGE) ||
                        e1.getEnrollmentType().equals(StudentEnrollment.WITHDRAWAL)) {
                    m_enrollmentStatus = (e0 != null ? e0.getStatusCode() : e1.getStatusCode());
                    m_school = e1.getSchool();
                    m_yog = e1.getYog();
                } else if (e1.getEnrollmentType().equals(StudentEnrollment.ENTRY)) {
                    m_enrollmentStatus = (e0 != null ? e0.getStatusCode() : e1.getStatusCode());
                    m_school = (e0 != null ? e0.getSchool() : e1.getSchool());
                    m_yog = (e0 != null ? e0.getYog() : e1.getYog());
                }
            }

            m_isPrecise = (e0 != null);
        }
    }
    /**
     * The Class GradeMatcher.
     */
    public class GradeMatcher {
        private TreeMap sortedGradeLevels;
        private int maxGradeLevel;
        private Map<String, ReferenceCode> m_referenceGradeCodeMap;

        /**
         * Instantiates a new grade matcher.
         */
        public GradeMatcher() {
            DataDictionary dictionary = getFLData().getDataDictionary();
            ModelProperty prop = new ModelProperty(SisStudent.class, SisStudent.COL_GRADE_LEVEL,
                    getFLData().getBroker().getPersistenceKey());
            DataDictionaryField field = dictionary.findDataDictionaryField(prop.getFieldId());
            if (field != null) {
                ReferenceTable referenceTable = field.getReferenceTable();
                m_referenceGradeCodeMap = referenceTable.getCodeMap();
            } else {
                throw new IllegalStateException("Grade Code Reference table cannot be built");
            }

            List<String> removedCodes = new LinkedList();
            for (Entry<String, ReferenceCode> entry : m_referenceGradeCodeMap.entrySet()) {
                if (entry.getValue().getDisabledIndicator()) {
                    removedCodes.add(entry.getKey());
                }
            }
            for (String code : removedCodes) {
                m_referenceGradeCodeMap.remove(code);
            }


            ModelBroker broker = new ModelBroker(getFLData().getPrivilegeSet());
            sortedGradeLevels = StudentManager.buildGradeLevelMap(broker);
            maxGradeLevel = StudentManager.getMaxGradeLevel(broker);
        }

        /**
         * Gets the reference code.
         *
         * @param yog the yog
         * @return the reference code
         */
        public ReferenceCode getReferenceCode(int yog) {
            ReferenceCode gradeCode = null;
            List<String> matchingGradeLevels = StudentManager.getMatchingGradeLevels(maxGradeLevel, yog,
                    getFLData().getCurrentContext().getSchoolYear(), sortedGradeLevels);
            for (String matchingGradeLevel : matchingGradeLevels) {
                gradeCode = m_referenceGradeCodeMap.get(matchingGradeLevel);
                if (gradeCode != null) {
                    break;
                }
            }
            return gradeCode;
        }

        /**
         * Gets the reference code.
         *
         * @param code the code
         * @return the reference code
         */
        public ReferenceCode getReferenceCode(String code) {
            ReferenceCode gradeCode = m_referenceGradeCodeMap.get(code);
            return gradeCode;
        }

    }

    /**
     * The Class StudentAttendanceDataset.
     */
    public class StudentAttendanceDataset {
        private PlainDate m_endDate;
        private Map<String, List<StudentAttendance>> m_map = null;
        private PlainDate m_startDate;

        /**
         * Instantiates a new student attendance dataset.
         *
         * @param startDate the start date
         * @param endDate the end date
         */
        public StudentAttendanceDataset(PlainDate startDate, PlainDate endDate) {
            m_startDate = startDate;
            m_endDate = endDate;
        }

        /**
         * Equals.
         *
         * @param obj the obj
         * @return true, if successful
         * @see java.lang.Object#equals(java.lang.Object)
         */
        @Override
        public boolean equals(Object obj) {
            if (obj instanceof StudentAttendanceDataset) {
                StudentAttendanceDataset data = (StudentAttendanceDataset) obj;
                boolean value = safeEquals(m_startDate, data.m_startDate) && safeEquals(m_endDate, data.m_endDate);
                if (!value) {
                    System.out.println();
                }
                return value;
            }
            return false;
        }


        /**
         * Gets the attendances.
         *
         * @param oid the oid
         * @return the attendances
         */
        public List<StudentAttendance> getAttendances(String oid) {
            if (m_map == null) {
                initializeAttendances();
            }
            return m_map.get(oid);
        }

        /**
         * Hash code.
         *
         * @return the int
         * @see java.lang.Object#hashCode()
         */
        @Override
        public int hashCode() {
            return m_startDate.hashCode() + m_endDate.hashCode();
        }

        /**
         * Initialize attendances.
         */
        private void initializeAttendances() {
            Boolean applyExclude = (Boolean) getSelectProperty(PROPERTY_APPLY_EXCLUDE, Boolean.class, Boolean.TRUE);
            Boolean applyInput = (Boolean) getSelectProperty(PROPERTY_APPLY_INPUT, Boolean.class, Boolean.TRUE);
            Boolean applySchool = (Boolean) getSelectProperty(PROPERTY_APPLY_SCHOOL, Boolean.class, Boolean.TRUE);
            Boolean applyStudentCriteria = (Boolean) getSelectProperty(PROPERTY_APPLY_MATERIALIZED_STUDENT_SELECTION,
                    Boolean.class, Boolean.FALSE);
            String fieldExcludeStd = getFLData().translateAliasToJavaName(ALIAS_EXCLUDE_STD, false);

            X2Criteria studentAttendanceCriteria = new X2Criteria();
            studentAttendanceCriteria.addGreaterOrEqualThan(StudentAttendance.COL_DATE, m_startDate);
            studentAttendanceCriteria.addLessOrEqualThan(StudentAttendance.COL_DATE, m_endDate);

            // Apply school criteria.
            if (applySchool.booleanValue() && getFLData().isSchoolContext()) {
                studentAttendanceCriteria.addEqualTo(StudentAttendance.REL_SCHOOL,
                        getFLData().getSchool().getOid());
            }

            // Apply exclude criteria.
            if (applyExclude.booleanValue() && !StringUtils.isEmpty(fieldExcludeStd)) {
                studentAttendanceCriteria.addNotEqualTo(StudentAttendance.REL_STUDENT + PATH_DELIMITER +
                        fieldExcludeStd,
                        BooleanAsStringConverter.TRUE);
            }

            // Apply user input criteria.
            if (applyInput.booleanValue()) {
                getFLData().applyInputCriteria(studentAttendanceCriteria, false, StudentAttendance.REL_STUDENT);
            }

            BeanQuery studentAttendanceQuery;
            if (applyStudentCriteria.booleanValue()) {
                studentAttendanceQuery = getStudentSelectionQuery(StudentAttendance.class, studentAttendanceCriteria,
                        StudentAttendance.COL_STUDENT_OID);
            } else {
                studentAttendanceQuery = new BeanQuery(StudentAttendance.class, studentAttendanceCriteria);
            }

            studentAttendanceQuery.addOrderBy(StudentAttendance.COL_STUDENT_OID, true);
            studentAttendanceQuery.addOrderBy(StudentAttendance.COL_DATE, false);
            m_map = getFLData().getBroker().getGroupedCollectionByQuery(studentAttendanceQuery,
                    StudentAttendance.COL_STUDENT_OID, 500);
        }

        /**
         * Gets the select property.
         *
         * @param selectKey the select key
         * @param expectedClass the expected class
         * @param defaultValue the default value
         * @return the select property
         */
        private Object getSelectProperty(String selectKey, Class expectedClass, Object defaultValue) {
            Object value = getSelectionProperty(selectKey);
            if (value != null) {
                if (!expectedClass.isInstance(value)) {
                    throw new ClassCastException("getStudentSeletionProperty(" + selectKey + "): Expected "
                            + expectedClass.getName() + ", found " + value.getClass().getName());
                }
            } else {
                value = defaultValue;
            }
            return value;
        }

        /**
         * Safe equals.
         *
         * @param obj1 the obj 1
         * @param obj2 the obj 2
         * @return true, if successful
         */
        private boolean safeEquals(Object obj1, Object obj2) {
            return (obj1 == null && obj2 == null) || (obj1 != null && obj1.equals(obj2));
        }
    }

    /**
     * The Class StudentConductDataset.
     */
    public class StudentConductDataset {
        private static final String ALIAS_ARREST = "all-cnd-Arrested";

        private Map<String, List<ConductAction>> m_conductActionMap = null;
        private Map<String, List<ConductIncident>> m_conductIncidentMap = null;
        private Map<String, List<ConductIncident>> m_conductIncidentByIdMap = null;
        private StateReportData m_data;
        private PlainDate m_endDate;
        private DataDictionaryField m_fieldArrest;
        private Map<String, StudentVictim> m_mapVictims = null;
        private PlainDate m_startDate;

        /**
         * Instantiates a new student conduct dataset.
         *
         * @param stateReportData the state report data
         * @param startDate the start date
         * @param endDate the end date
         */
        public StudentConductDataset(StateReportData stateReportData, PlainDate startDate, PlainDate endDate) {
            m_data = stateReportData;
            m_startDate = startDate;
            m_endDate = endDate;
            m_fieldArrest = getFLData().translateAliasToDictionaryField(ALIAS_ARREST, true);
        }

        /**
         * Gets the action criteria.
         *
         * @return the action criteria
         */
        public X2Criteria getActionCriteria() {
            X2Criteria actionCriteria = new X2Criteria();
            // State reportable codes
            actionCriteria.addIn(ConductAction.COL_ACTION_CODE,
                    getFLData().getCodesForStateValue(ConductAction.class, ConductAction.COL_ACTION_CODE,
                            REPORTABLE_ACTION_CODES));

            // school selection
            if (getFLData().isSchoolContext()) {
                actionCriteria.addEqualTo(ConductAction.COL_SCHOOL_OID, getFLData().getSchool().getOid());
            } else {
                actionCriteria.addNotEqualTo(
                        ConductAction.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
                actionCriteria.addNotEqualTo(ConductAction.REL_SCHOOL + PATH_DELIMITER +
                        SisSchool.COL_ARCHIVE_INDICATOR,
                        Boolean.TRUE);
            }

            // Date range
            actionCriteria.addGreaterOrEqualThan(ConductAction.REL_INCIDENT + ModelProperty.PATH_DELIMITER +
                    ConductIncident.COL_INCIDENT_DATE, m_startDate);
            actionCriteria.addLessOrEqualThan(ConductAction.REL_INCIDENT + ModelProperty.PATH_DELIMITER +
                    ConductIncident.COL_INCIDENT_DATE, m_endDate);

            return actionCriteria;
        }

        /**
         * Gets the arrested indicator.
         *
         * @param studentOid the student oid
         * @return the arrested indicator
         * @throws X2BaseException the x 2 base exception
         */
        public boolean getArrestedIndicator(String studentOid) throws X2BaseException {
            boolean arrestedIndicator = false;
            List<ConductIncident> incidents = getConductIncidents(studentOid);
            if (incidents != null) {
                for (ConductIncident incident : incidents) {
                    Boolean arrested = (Boolean) getFLData().getFieldValue(incident, m_fieldArrest);
                    if (arrested != null && arrested.booleanValue()) {
                        arrestedIndicator = true;
                        break;
                    }
                }
            }
            return arrestedIndicator;
        }

        /**
         * Gets the conduct actions.
         *
         * @param oid the oid
         * @return the conduct actions
         */
        public List<ConductAction> getConductActions(String oid) {
            initializeConductActions();

            return m_conductActionMap.get(oid);
        }

        /**
         * Gets the conduct incidents.
         *
         * @param oid the oid
         * @return the conduct incidents
         */
        public List<ConductIncident> getConductIncidents(String oid) {
            initializeConductIncidents();

            return m_conductIncidentMap.get(oid);
        }

        /**
         * Gets the conduct incidents by id.
         *
         * @param id the id
         * @return the conduct incidents by id
         */
        public List<ConductIncident> getConductIncidentsById(String id) {
            initializeConductIncidentsById();

            return m_conductIncidentByIdMap.get(id);
        }

        /**
         * Gets the conduct incidents by id map.
         *
         * @return the conduct incidents by id map
         */
        public Map<String, List<ConductIncident>> getConductIncidentsByIdMap() {
            initializeConductIncidentsById();

            return m_conductIncidentByIdMap;
        }

        /**
         * Gets the incident criteria.
         *
         * @return the incident criteria
         */
        public X2Criteria getIncidentCriteria() {
            X2Criteria incidentCriteria = new X2Criteria();
            Collection<String> codes =
                    getFLData().getCodesForStateValue(ConductIncident.class, ConductIncident.COL_INCIDENT_CODE,
                            REPORTABLE_INCIDENT_CODES);
            incidentCriteria.addIn(ConductIncident.COL_INCIDENT_CODE, codes);

            if (getFLData().isSchoolContext()) {
                incidentCriteria.addEqualTo(ConductIncident.COL_SCHOOL_OID, getFLData().getSchool().getOid());
            } else {
                incidentCriteria.addNotEqualTo(
                        ConductIncident.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
                incidentCriteria.addNotEqualTo(ConductIncident.REL_SCHOOL + PATH_DELIMITER +
                        SisSchool.COL_ARCHIVE_INDICATOR,
                        Boolean.TRUE);
            }

            incidentCriteria.addGreaterOrEqualThan(ConductIncident.COL_INCIDENT_DATE, m_startDate);

            incidentCriteria.addLessOrEqualThan(ConductIncident.COL_INCIDENT_DATE, m_endDate);

            return incidentCriteria;
        }

        /**
         * get list of student programs ordered by start date.
         *
         * @param oid the oid
         * @return the victim incidents
         */
        public StudentVictim getVictimIncidents(String oid) {
            initializeVictims();

            return m_mapVictims.get(oid);
        }

        /**
         * Equals.
         *
         * @param obj the obj
         * @return true, if successful
         * @see java.lang.Object#equals(java.lang.Object)
         */
        @Override
        public boolean equals(Object obj) {
            if (obj instanceof StudentConductDataset) {
                StudentConductDataset data = (StudentConductDataset) obj;
                return safeEquals(m_startDate, data.m_startDate) && safeEquals(m_endDate, data.m_endDate);
            }
            return false;
        }

        /**
         * Hash code.
         *
         * @return the int
         * @see java.lang.Object#hashCode()
         */
        @Override
        public int hashCode() {
            return m_startDate.hashCode() + m_endDate.hashCode();
        }

        /**
         * Checks for conduct action.
         *
         * @param student the student
         * @return true, if successful
         */
        public boolean hasConductAction(SisStudent student) {
            Collection<ConductAction> actions = getConductActions(student.getOid());

            boolean res = false;
            if (actions != null && !actions.isEmpty()) {
                for (ConductAction action : actions) {
                    String actType = m_data.getDataDictionary().findStateReferenceCode(REFERENCE_TABLE_CND_ACTION,
                            action.getActionCode());
                    if (!StringUtils.isEmpty(actType)) {
                        res = true;
                        break;
                    }
                }
            }
            return res;
        }

        /**
         * Initialize victims.
         */
        private void initializeVictims() {
            if (m_mapVictims == null) {
                m_mapVictims = new HashMap();
                List<DataDictionaryField> fields = Arrays.asList(
                        getFLData().translateAliasToDictionaryField(ALIAS_HARASSED_DISABILITY, false),
                        getFLData().translateAliasToDictionaryField(ALIAS_HARASSED_ORIENTATION, false),
                        getFLData().translateAliasToDictionaryField(ALIAS_HARASSED_RACE, false),
                        getFLData().translateAliasToDictionaryField(ALIAS_HARASSED_RELIGION, false),
                        getFLData().translateAliasToDictionaryField(ALIAS_HARASSED_SEX, false));

                X2Criteria andCriteria = new X2Criteria();
                for (DataDictionaryField field : fields) {
                    if (field != null) {
                        X2Criteria orCriteria = new X2Criteria();
                        orCriteria.addEqualTo(field.getJavaName(), BooleanAsStringConverter.TRUE);
                        andCriteria.addOrCriteria(orCriteria);
                    }
                }

                PlainDate startDate = m_startDate;
                if (startDate == null) {
                    startDate = m_data.getCurrentContext().getStartDate();
                }

                PlainDate endDate = m_endDate;
                if (endDate == null) {
                    endDate = m_data.getCurrentContext().getEndDate();
                }
                X2Criteria criteria = new X2Criteria();

                // Date range criteria
                criteria.addLessOrEqualThan(ConductIncident.COL_INCIDENT_DATE, endDate);
                criteria.addGreaterOrEqualThan(ConductIncident.COL_INCIDENT_DATE, startDate);
                criteria.addAndCriteria(andCriteria);

                BeanQuery query =
                        getStudentSelectionQuery(ConductIncident.class, criteria, ConductIncident.COL_VICTIM_OID);
                QueryIterator iterator = m_data.getBroker().getIteratorByQuery(query);

                try {
                    while (iterator.hasNext()) {
                        ConductIncident incident = (ConductIncident) iterator.next();
                        String victimOid = incident.getVictimOid();
                        if (!StringUtils.isEmpty(victimOid)) {
                            StudentVictim victim = m_mapVictims.get(victimOid);
                            if (victim == null) {
                                victim = new StudentVictim();
                                m_mapVictims.put(victimOid, victim);
                            }
                            if (victimAliasSet(incident, ALIAS_HARASSED_DISABILITY)) {
                                victim.setDisabilityBased();
                            }
                            if (victimAliasSet(incident, ALIAS_HARASSED_ORIENTATION)) {
                                victim.setSexualOrientationBased();
                            }
                            if (victimAliasSet(incident, ALIAS_HARASSED_RACE)) {
                                victim.setRaceBased();
                            }
                            if (victimAliasSet(incident, ALIAS_HARASSED_RELIGION)) {
                                victim.setReligionBased();
                            }
                            if (victimAliasSet(incident, ALIAS_HARASSED_SEX)) {
                                victim.setSexBased();
                            }
                        }
                    }
                } finally {
                    iterator.close();
                }
            }
        }

        /**
         * Initialize conduct actions.
         */
        private void initializeConductActions() {
            if (m_conductActionMap == null) {
                X2Criteria criteria = getActionCriteria().copy();
                BeanQuery query =
                        getStudentSelectionQuery(ConductAction.class, criteria, ConductAction.COL_STUDENT_OID);

                m_conductActionMap = m_data.getBroker().getGroupedCollectionByQuery(query,
                        ConductAction.COL_STUDENT_OID, INITIAL_MAP_SIZE);
            }
        }

        /**
         * Initialize conduct incidents.
         */
        private void initializeConductIncidents() {
            if (m_conductIncidentMap == null) {
                X2Criteria criteria = getIncidentCriteria().copy();
                BeanQuery query =
                        getStudentSelectionQuery(ConductIncident.class, criteria, ConductIncident.COL_STUDENT_OID);

                m_conductIncidentMap = m_data.getBroker().getGroupedCollectionByQuery(query,
                        ConductIncident.COL_STUDENT_OID, INITIAL_MAP_SIZE);
            }
        }

        /**
         * Initialize conduct incidents by id.
         */
        private void initializeConductIncidentsById() {
            if (m_conductIncidentByIdMap == null) {
                X2Criteria criteria = getIncidentCriteria().copy();
                BeanQuery query =
                        getStudentSelectionQuery(ConductIncident.class, criteria, ConductIncident.COL_STUDENT_OID);

                m_conductIncidentByIdMap = m_data.getBroker().getGroupedCollectionByQuery(query,
                        ConductIncident.COL_INCIDENT_ID, INITIAL_MAP_SIZE);
            }
        }

        /**
         * Safe equals.
         *
         * @param obj1 the obj 1
         * @param obj2 the obj 2
         * @return true, if successful
         */
        private boolean safeEquals(Object obj1, Object obj2) {
            return (obj1 == null && obj2 == null) || (obj1 != null && obj1.equals(obj2));
        }

        /**
         * Victim alias set.
         *
         * @param incident the incident
         * @param alias the alias
         * @return true, if successful
         */
        private boolean victimAliasSet(ConductIncident incident, String alias) {
            boolean value = false;
            DataDictionaryField field = getFLData().translateAliasToDictionaryField(alias, false);
            if (field != null) {
                String fieldValue = (String) incident.getFieldValueByBeanPath(field.getJavaName());
                if (!StringUtils.isEmpty(fieldValue) && fieldValue.equals(BooleanAsStringConverter.TRUE)) {
                    value = true;
                }
            }
            return value;
        }
    }

    /**
     * The Class StudentEnrollmentSpanInfo.
     */
    public class StudentEnrollmentSpanInfo {
        private String m_gradeLevel;
        private StudentEnrollmentSpan m_span;
        private SisStudent m_student;
        private PlainDate m_startCurrentYear;
        private PlainDate m_endCurrentYear;
        private StudentEnrollment m_priorSchoolEnrollment;
        private boolean m_isSetCurrentYear;
        private SchoolCalendarInfo m_schoolCalendarInfo;
        private ImmutableSet<PlainDate> m_inSessionDates;

        /**
         * Instantiates a new student enrollment span info.
         *
         * @param student the student
         * @param span the span
         */
        public StudentEnrollmentSpanInfo(SisStudent student, StudentEnrollmentSpan span) {
            m_student = student;
            m_span = span;
        }

        /**
         * Gets the end of year withdrawal code.
         *
         * @return the end of year withdrawal code
         * @throws X2BaseException the x 2 base exception
         */
        public String getEndOfYearWithdrawalCode() throws X2BaseException {
            String value = null;
            StudentEnrollmentSpan span = getStudentEnrollmentSpan();
            if (getStudent().getNextSchoolOid() != null && span.getSchool() != null
                    && !span.getSchool().getOid().equals(getStudent().getSchoolOid())) {
                value = WITHDRAW_CODE_PROMOTED_IN_DISTRICT;
            } else {
                value = WITHDRAW_CODE_PROMOTED_IN_SCHOOL;
            }

            StudentEnrollment withdrawal = getWithdrawalEnrollment();
            if (withdrawal != null) {
                String withdrawalCode = (String) getFLData().getFieldValue(withdrawal, m_fieldEnrollmentCode);
                if (WITHDRAW_CODES.contains(withdrawalCode)) {
                    value = withdrawalCode;
                }
            }
            return value;
        }

        /**
         * Gets the grade level.
         *
         * @return the grade level
         */
        public String getGradeLevel() {
            if (m_gradeLevel == null) {
                int yog = m_span.getFirstActiveEnrollment().getYog();
                m_gradeLevel = FLStudentHelper.this.getGradeLevel(m_student, yog);
            }
            return m_gradeLevel;
        }

        /**
         * Gets the in session dates.
         *
         * @param context
         *
         * @return the in session dates
         */
        public ImmutableSet<PlainDate> getInSessionDates(DistrictSchoolYearContext context) {
            if (m_inSessionDates == null) {
                Set<PlainDate> dates = new TreeSet();
                setContextYear(context);
                if (m_startCurrentYear != null) {
                    SchoolCalendarInfo info = getSchoolCalendarInfo(context);
                    if (info != null) {
                        ImmutableSet<PlainDate> allDates = info.getInSessionDates();
                        if (allDates != null) {
                            for (PlainDate date : allDates) {
                                if (!date.before(m_startCurrentYear) &&
                                        (m_endCurrentYear == null || !date.after(m_endCurrentYear))) {
                                    dates.add(date);
                                }
                            }
                        }
                    }
                }
                m_inSessionDates = ImmutableSet.copyOf(dates);
            }
            return m_inSessionDates;
        }

        /**
         * Gets the prior district enrollment.
         *
         * @return the prior district enrollment
         */
        public StudentEnrollment getPriorDistrictEnrollment() {
            // TODO: complete implementation
            if (m_priorSchoolEnrollment == null) {
                // use the first inactive if a prior populated enrollment record cannot be found
                m_priorSchoolEnrollment = m_span.getFirstActiveEnrollment();
            }
            return m_priorSchoolEnrollment;
        }

        /**
         * Gets the student.
         *
         * @return the student
         */
        public SisStudent getStudent() {
            return m_student;
        }

        /**
         * Gets the student enrollment span.
         *
         * @return the student enrollment span
         */
        public StudentEnrollmentSpan getStudentEnrollmentSpan() {
            return m_span;
        }

        /**
         * Gets the withdrawal enrollment by finding the first inactive withdrawal record in the
         * span.
         *
         * @return the withdrawal enrollment
         */
        public StudentEnrollment getWithdrawalEnrollment() {
            StudentEnrollment value = null;
            StudentEnrollmentSpan span = getStudentEnrollmentSpan();
            if (span.getFirstInactiveEnrollment() != null) {
                Iterator<StudentEnrollment> iterator = span.getEnrollments().iterator();
                while (iterator.hasNext()) {
                    StudentEnrollment enrollment = iterator.next();
                    if (enrollment.equals(span.getFirstInactiveEnrollment())) {
                        do {
                            if (StudentEnrollment.WITHDRAWAL.equals(enrollment.getEnrollmentType())) {
                                value = enrollment;
                                enrollment = null;
                            } else {
                                if (iterator.hasNext()) {
                                    enrollment = iterator.next();
                                } else {
                                    enrollment = null;
                                }
                            }
                        } while (enrollment != null);
                        break;
                    }
                }
            }
            return value;
        }

        /**
         * Sets the current year.
         */
        private void setContextYear(DistrictSchoolYearContext context) {
            if (!m_isSetCurrentYear) {
                m_isSetCurrentYear = true;
                if (m_span.getFirstInactiveEnrollment() != null &&
                        m_span.getFirstInactiveEnrollment().getEnrollmentDate() != null &&
                        m_span.getFirstInactiveEnrollment().getEnrollmentDate()
                                .before(context.getStartDate())
                        &&
                        m_span.getLastActiveDate() != null
                        && m_span.getLastActiveDate().before(context.getStartDate())) {
                    // Span ends before year starts
                    return;
                }
                PlainDate firstInSessionDate = getFirstInSessionDate(context);
                if (firstInSessionDate != null && m_span.getFirstActiveEnrollment() != null &&
                        m_span.getFirstActiveEnrollment().getEnrollmentDate() != null) {
                    m_startCurrentYear = m_span.getFirstActiveEnrollment().getEnrollmentDate();
                    if (m_startCurrentYear.before(firstInSessionDate)) {
                        m_startCurrentYear = firstInSessionDate;
                    } else {
                        m_startCurrentYear = getNextInSessionDate(m_startCurrentYear, isMemberOnEntryDate(), context);
                    }

                    m_endCurrentYear = getLastInSessionDate(context);
                    if (m_endCurrentYear != null && m_span.getFirstInactiveEnrollment() != null &&
                            m_span.getFirstInactiveEnrollment().getEnrollmentDate() != null) {
                        m_endCurrentYear = getPreviousInSessionDate(
                                m_span.getFirstInactiveEnrollment().getEnrollmentDate(), isMemberOnWithdrawalDate(),
                                context);
                        if (m_endCurrentYear == null) {
                            // no in session dates before withdrawal
                            m_startCurrentYear = null;
                        }
                    }
                }
                m_isSetCurrentYear = true;
            }
        }

        /**
         * Gets the last in session date.
         *
         * @return the last in session date
         */
        private PlainDate getLastInSessionDate(DistrictSchoolYearContext context) {
            PlainDate value = null;
            SchoolCalendarInfo calendarInfo = getSchoolCalendarInfo(context);
            if (calendarInfo != null) {
                ImmutableSet<PlainDate> inSessionDates = calendarInfo.getInSessionDates();
                if (inSessionDates != null && !inSessionDates.isEmpty()) {
                    for (PlainDate date : inSessionDates) {
                        value = date;
                    }
                }
            }
            return value;
        }

        /**
         * Find the first insession date on or after date.
         *
         * @param testDate the test date
         * @param onOrAfter if true search date. If false, result must be after date
         * @return the next in session date
         */
        private PlainDate getNextInSessionDate(PlainDate testDate,
                                               boolean onOrAfter,
                                               DistrictSchoolYearContext context) {
            PlainDate value = null;
            SchoolCalendarInfo calendarInfo = getSchoolCalendarInfo(context);
            if (calendarInfo != null) {
                ImmutableSet<PlainDate> inSessionDates = calendarInfo.getInSessionDates();
                if (inSessionDates != null && !inSessionDates.isEmpty()) {
                    for (PlainDate date : inSessionDates) {
                        if (onOrAfter && !date.before(testDate)) {
                            value = date;
                            break;
                        } else if (!onOrAfter && date.after(testDate)) {
                            value = date;
                            break;
                        }
                    }
                }
            }
            return value;
        }

        /**
         * Gets the previous in session date.
         *
         * @param testDate the test date
         * @param onOrBefore the on or before
         * @return the previous in session date
         */
        private PlainDate getPreviousInSessionDate(PlainDate testDate,
                                                   boolean onOrBefore,
                                                   DistrictSchoolYearContext context) {
            PlainDate value = null;
            SchoolCalendarInfo calendarInfo = getSchoolCalendarInfo(context);
            if (calendarInfo != null) {
                ImmutableSet<PlainDate> inSessionDates = calendarInfo.getInSessionDates();
                if (inSessionDates != null && !inSessionDates.isEmpty()) {
                    for (PlainDate date : inSessionDates) {
                        if (!onOrBefore && !date.before(testDate)) {
                            break;
                        } else if (onOrBefore && date.after(testDate)) {
                            break;
                        }
                        value = date;
                    }
                }
            }
            return value;
        }

        /**
         * get the first insesssion date for this span.
         *
         * @return the first in session date
         */
        private PlainDate getFirstInSessionDate(DistrictSchoolYearContext context) {
            PlainDate value = null;
            SchoolCalendarInfo calendarInfo = getSchoolCalendarInfo(context);
            if (calendarInfo != null) {
                ImmutableSet<PlainDate> inSessionDates = calendarInfo.getInSessionDates();
                if (inSessionDates != null && !inSessionDates.isEmpty()) {
                    value = inSessionDates.iterator().next();
                }
            }
            return value;
        }

        /**
         * Gets the school calendar info.
         *
         * @return the school calendar info
         */
        private SchoolCalendarInfo getSchoolCalendarInfo(DistrictSchoolYearContext context) {
            if (m_schoolCalendarInfo == null) {
                m_schoolCalendarInfo = getFLData().getSchoolCalendarHelper().getSchoolCalendarInfo(m_span.getSchool(),
                        getCalendarCode(), context);
                if (!m_schoolCalendarInfo.isValid()) {
                    m_schoolCalendarInfo = null;
                    SchoolCalendar mostCommonCalendar =
                            getFLData().getSchoolCalendarHelper().getMostCommonCalendarCode(m_span.getSchool());
                    if (mostCommonCalendar != null) {
                        m_schoolCalendarInfo = getFLData().getSchoolCalendarHelper()
                                .getSchoolCalendarInfo(m_span.getSchool(), mostCommonCalendar.getCalendarId(), context);
                    }
                }
            }
            return m_schoolCalendarInfo;
        }

        /**
         * Gets the calendar code.
         *
         * @return the calendar code
         */
        private String getCalendarCode() {
            /*
             * for now just use the student calendar. If calendar tracking is implemented in
             * enrollment, this will need to be changed.
             */
            return m_student.getCalendarCode();
        }
    }

    /**
     * The Class StudentInfo.
     */
    public class StudentInfo {
        private Map<String, ImmutableSet<PlainDate>> m_absentDatesByContext = new HashMap<>();
        private String m_fefpProgramNumber;
        private Map<PlainDate, String> m_gradeLevels;
        private Map<String, ImmutableSet<PlainDate>> m_memberDatesByContext = new HashMap<>();
        private Boolean m_hasServices;
        private SisStudent m_student;
        private HashMap<PlainDate, SisSchool> m_schools;

        /**
         * Instantiates a new student info.
         *
         * @param student the student
         */
        public StudentInfo(SisStudent student) {
            m_student = student;
        }

        /**
         * Format student name according to FL standard
         * http://www.fldoe.org/core/fileparse.php/15229/urlt/1617-175425.pdf
         *
         * @return the string
         */
        public String formatStudentLegalName() {
            StringBuilder sb = new StringBuilder(StringUtils.padRight(m_student.getPerson().getLastName(), 17));
            String appendage = StringUtils.padRight(m_student.getPerson().getNameSuffixCode(), 3);
            sb.append(appendage);
            String fname = StringUtils.padRight(m_student.getPerson().getFirstName(), 12);
            sb.append(fname);
            String mname = StringUtils.padRight(m_student.getPerson().getMiddleName(), 10);
            sb.append(mname);
            return sb.length() <= MAX_LEGAL_NAME_LEN ? sb.toString() : sb.substring(0, MAX_LEGAL_NAME_LEN);
        }

        /**
         * Gets the absent dates.
         *
         * @param context
         *
         * @return the absent dates
         */
        public ImmutableSet<PlainDate> getAbsentDates(DistrictSchoolYearContext context) {
            if (m_absentDatesByContext.get(context.getOid()) == null) {
                StudentAttendanceDataset dataset = getStudentAttendanceDataset(
                        context.getStartDate(),
                        context.getEndDate());
                TreeSet<PlainDate> dates = new TreeSet();
                List<StudentAttendance> attendances = dataset.getAttendances(m_student.getOid());
                if (attendances != null && !attendances.isEmpty()) {
                    for (StudentAttendance attendance : attendances) {
                        if (attendance.getAbsentIndicator() && getMemberDates(context).contains(attendance.getDate())) {
                            dates.add(attendance.getDate());
                        }
                    }
                }
                m_absentDatesByContext.put(context.getOid(), ImmutableSet.copyOf(dates));
            }
            return m_absentDatesByContext.get(context.getOid());
        }

        /**
         * Gets the cte indicator.
         *
         * @return the cte indicator
         */
        public boolean getCteIndicator() {
            boolean indicator = false;
            List<StudentProgramParticipation> pgms =
                    getStudentPrograms(m_student.getOid(), "FL-PGM-CTE", getFLData().getSurveyPeriod());
            if (pgms != null && !pgms.isEmpty()) {
                indicator = true;
            }
            return indicator;
        }

        /**
         * Gets the esol indicator.
         *
         * @return the esol indicator
         */
        public boolean getEsolIndicator() {
            boolean indicator = false;
            List<StudentProgramParticipation> pgms =
                    getStudentPrograms(m_student.getOid(), "FL-PGM-ELL", getFLData().getSurveyPeriod());
            if (pgms != null && !pgms.isEmpty()) {
                Calendar startAfterDate = Calendar.getInstance();
                startAfterDate.setTime(getFLData().getSurveyPeriod().getSnapshotDate());
                startAfterDate.add(Calendar.YEAR, -6);
                for (StudentProgramParticipation pgm : pgms) {
                    if (pgm.getStartDate() != null
                            && pgm.getStartDate().after(new PlainDate(startAfterDate.getTime()))) {
                        indicator = true;
                    }
                }
            }
            return indicator;
        }

        /**
         * Gets the fefp program.
         *
         * @return the fefp program
         * @throws X2BaseException the x 2 base exception
         */
        public String getFefpProgram() throws X2BaseException {
            if (m_fefpProgramNumber == null) {
                String grade = getGradeLevel(getFLData().getSurveyPeriod().getDateCertain());
                String eseProgramNumber = getEseProgramNumber(grade);

                m_fefpProgramNumber = FEFP_OTHER;
                if (getEsolIndicator()) {
                    m_fefpProgramNumber = FEFP_ESOL;
                } else if (getCteIndicator()) {
                    m_fefpProgramNumber = FEFP_CAREER;
                } else if (GRADE_LEVELS_PK_3.contains(grade)) {
                    m_fefpProgramNumber = FEFP_BASIC_K_3;
                } else if (GRADE_LEVELS_4_8.contains(grade)) {
                    m_fefpProgramNumber = FEFP_BASIC_4_8;
                } else if (GRADE_LEVELS_9_12.contains(grade)) {
                    m_fefpProgramNumber = FEFP_BASIC_9_12;
                }
                if (eseProgramNumber != null && (m_fefpProgramNumber.equals(FEFP_OTHER)
                        || m_fefpProgramNumber.compareTo(eseProgramNumber) < 0)) {
                    m_fefpProgramNumber = eseProgramNumber;
                }

            }
            return m_fefpProgramNumber;
        }

        /**
         * Gets the grade level.
         *
         * @param date the date
         * @return the grade level
         */
        public String getGradeLevel(PlainDate date) {
            if (m_gradeLevels == null) {
                m_gradeLevels = new HashMap();
            }

            String gradeLevel = m_gradeLevels.get(date);
            if (gradeLevel == null) {
                EnrollmentSnapshot snapshot = new EnrollmentSnapshot(m_student, date);
                gradeLevel = FLStudentHelper.this.getGradeLevel(m_student, snapshot.getYog());
            }
            return gradeLevel;
        }

        /**
         * Gets the member dates.
         *
         * @param context
         *
         * @return the member dates
         */
        public ImmutableSet<PlainDate> getMemberDates(DistrictSchoolYearContext context) {
            if (m_memberDatesByContext.get(context.getOid()) == null) {
                TreeSet<PlainDate> dates = new TreeSet();
                List<StudentEnrollmentSpan> spans = getStudentEnrollmentSpans(m_student, false);
                for (StudentEnrollmentSpan span : spans) {
                    StudentEnrollmentSpanInfo info = new StudentEnrollmentSpanInfo(m_student, span);
                    Set<PlainDate> spanDates = info.getInSessionDates(context);
                    dates.addAll(spanDates);
                }
                m_memberDatesByContext.put(context.getOid(), ImmutableSet.copyOf(dates));
            }
            return m_memberDatesByContext.get(context.getOid());
        }

        /**
         * Gets the school.
         *
         * @param date the date
         * @return the school
         */
        public SisSchool getSchool(PlainDate date) {
            if (m_schools == null) {
                m_schools = new HashMap();
            }
            SisSchool school = m_schools.get(date);
            if (school == null) {
                StudentEnrollment enroll = getEnrollmentForDate(m_student.getOid(), date,
                        StudentEnrollment.ENTRY + StudentEnrollment.STATUS_CHANGE + StudentEnrollment.YOG_CHANGE);
                if (enroll != null) {
                    school = enroll.getSchool();
                } else {
                    school = m_student.getSchool();
                }
                m_schools.put(date, school);
            }
            return school;
        }

        /**
         * Gets the ssn.
         *
         * @return the ssn
         */
        public String getSSN() {
            String ssn = m_student.getPerson().getPersonId();
            if (!StringUtils.isEmpty(ssn)) {
                ssn = ssn.replaceAll("-", "");
            }
            return ssn;
        }

        /**
         * Gets the std enrollment span info.
         *
         * @return Student enrollment span info
         */
        public StudentEnrollmentSpanInfo getStdEnrollmentSpanInfo() {
            List<StudentEnrollmentSpan> spans = getStudentEnrollmentSpans(m_student, false);
            StudentEnrollmentSpan theLatestSpan = null;
            for (StudentEnrollmentSpan span : spans) {
                if (theLatestSpan == null || theLatestSpan.getFirstActiveDate().before(span.getFirstActiveDate())) {
                    theLatestSpan = span;
                }
            }
            if (theLatestSpan != null) {
                return new StudentEnrollmentSpanInfo(m_student, theLatestSpan);
            }
            return null;
        }

        /**
         * Gets the std grade point.
         *
         * @return the std grade point
         * @throws X2BaseException the x 2 base exception
         */

        public BigDecimal getStdGradePointAverage() throws X2BaseException {
            BigDecimal value = null;
            DataDictionaryField field = FLStudentHelper.this.getGpaField();
            if (field != null) {
                value = (BigDecimal) FLStudentHelper.this.getFLData().getFieldValue(m_student, field);
            }
            return value;
        }

        /**
         * Checks for services.
         *
         * @return true, if successful
         */
        public boolean hasServices() {
            if (m_hasServices == null) {
                m_hasServices = FLStudentHelper.this.hasServices(m_student) ? Boolean.TRUE : Boolean.FALSE;
            }
            return m_hasServices.booleanValue();
        }

        /**
         * Gets the ese program number.
         *
         * @param grade the grade
         * @return the ese program number
         * @throws X2BaseException the x 2 base exception
         */
        private String getEseProgramNumber(String grade) throws X2BaseException {
            String fefpProgramNumber = null;
            DataDictionaryField field =
                    getFLData().translateAliasToDictionaryField(ALIAS_SPED_FUNDING_TYPE, false);
            if (field != null) {
                String eseCode = (String) getFLData().getFieldValue(m_student, field);
                if (!StringUtils.isEmpty(eseCode)) {
                    String[] values = eseCode.split("\\|");
                    if (values.length > 1) {
                        if (GRADE_LEVELS_PK_3.contains(grade)) {
                            fefpProgramNumber = values[0];
                        } else if (GRADE_LEVELS_4_8.contains(grade)) {
                            fefpProgramNumber = values[1];
                        } else if (GRADE_LEVELS_9_12.contains(grade) && values.length > 2) {
                            fefpProgramNumber = values[2];
                        }
                    } else {
                        fefpProgramNumber = eseCode;
                    }
                }
            }
            return fefpProgramNumber;
        }

    }


    /**
     * The Class StudentScheduleHelper.
     */
    public class StudentScheduleHelper {

        /**
         * The Class StudentScheduleInfo.
         */
        public class StudentScheduleInfo {
            private final Integer INTEGER_ZERO = Integer.valueOf(0);

            private Course m_course;
            private String m_cteCourseSetting;
            private String m_dualEnrollmentCrsLocation;
            private String m_dualEnrollmentIndicator;
            private String m_dualEnrollmentType;
            private String m_ellInstructionalModel;
            private Integer m_fte;
            private Integer m_fteWeighted;
            private MasterScheduleInfo m_masterScheduleInfo;
            private SisOrganization m_organization;
            private String m_readingInterventionIndicator;
            private SisSchool m_school;
            private MasterSchedule m_section;
            private StudentScheduleSpan m_span;
            private SisStudent m_student;
            private StudentInfo m_studentInfo;
            private String m_wdisComplPoint;
            private Collection<String> m_wdisCourseStatuses;
            private PlainDate m_wdisLitComplPointDate;

            /**
             * Instantiates a new student schedule info.
             *
             * @param student the student
             * @param span the span
             */
            public StudentScheduleInfo(SisStudent student, StudentScheduleSpan span) {
                m_student = student;
                m_span = span;
            }

            /**
             * Gets the adult fee status first.
             *
             * @return String
             * @throws X2BaseException exception
             */
            public String getAdultFeeStatusFirst() throws X2BaseException {
                return getAdultFeeStatus(ALIAS_WDIS_ADULT_FEE_STATUS_FIRST_SSC, ALIAS_WDIS_ADULT_FEE_STATUS_FIRST_STD);
            }

            /**
             * Gets the adult fee status second.
             *
             * @return String
             * @throws X2BaseException exception
             */
            public String getAdultFeeStatusSecond() throws X2BaseException {
                return getAdultFeeStatus(ALIAS_WDIS_ADULT_FEE_STATUS_SECOND_SSC,
                        ALIAS_WDIS_ADULT_FEE_STATUS_SECOND_STD);
            }

            /**
             * Gets the adult edu func level init.
             *
             * @return String
             * @throws X2BaseException exception
             */
            public String getAdultEduFuncLevelInit() throws X2BaseException {
                return getFromSscOrScc(ALIAS_WDIS_AGE_FUNC_LEVEL_INIT_SSC,
                        ALIAS_WDIS_AGE_FUNC_LEVEL_INIT_SCC);
            }

            /**
             * Gets the basic skills exam.
             *
             * @return String
             * @throws X2BaseException exception
             */
            public String getBasicSkillsExam() throws X2BaseException {
                return getValueByAlias(m_student, ALIAS_WDIS_BASIC_SKILLS_EXAM_STD);
            }

            /**
             * Gets the cost reporting code.
             *
             * @return String
             * @throws X2BaseException exception
             */
            public String getCostReportingCode() throws X2BaseException {
                return getValueByAlias(m_span.getSchedule(), ALIAS_WDIS_COST_REPORTING_CODE);
            }

            /**
             * Gets the entry date.
             *
             * @return entry date for schedule span
             * @throws X2BaseException exception
             */
            public String getEnrNotStateFunded() throws X2BaseException {
                String value = getValueByAlias(m_student, ALIAS_WDIS_ENR_NOT_STATE_FUNDED_STD);
                if (StringUtils.isEmpty(value)) {
                    value = getValueByAlias(getCourse(), ALIAS_WDIS_ENR_NOT_STATE_FUNDED_CRS);
                }
                return value;
            }

            /**
             * Gets the entry date.
             *
             * @return entry date for schedule span
             */
            public PlainDate getEntryDate() {
                return m_span != null ? m_span.getEntryDate() : null;
            }

            /**
             * Gets the exit date.
             *
             * @return exit date for schedule span
             */
            public PlainDate getExitDate() {
                return m_span != null ? m_span.getExitDate() : null;
            }

            /**
             * Gets the scheduled monday.
             *
             * @return the scheduled monday
             */
            public boolean getScheduledMonday() {
                boolean value = false;

                return value;
            }

            /**
             * Gets the course.
             *
             * @return the course
             */
            public Course getCourse() {
                if (m_course == null) {
                    m_course = m_span.getSection().getSchoolCourse().getCourse();
                }
                return m_course;
            }

            /**
             * Gets the cte course setting.
             *
             * @return the cte course setting
             * @throws X2BaseException the x 2 base exception
             */
            public String getCteCourseSetting() throws X2BaseException {
                if (m_cteCourseSetting == null) {
                    m_cteCourseSetting = "";
                    StudentSchedule schedule = m_span.getSchedule();
                    if (schedule != null) {
                        DataDictionaryField field =
                                getFLData().translateAliasToDictionaryField(ALIAS_CTE_EXCEPTIONAL_SETTING_SSC,
                                        false);
                        if (field != null) {
                            m_cteCourseSetting = (String) getFLData().getFieldValue(schedule, field);
                        }
                    } else {
                        StudentScheduleChange change = m_span.getExitChange();
                        DataDictionaryField field =
                                getFLData().translateAliasToDictionaryField(ALIAS_CTE_EXCEPTIONAL_SETTING_SCC,
                                        false);
                        if (field != null) {
                            m_cteCourseSetting = (String) getFLData().getFieldValue(change, field);
                        }
                    }
                }
                return m_cteCourseSetting;
            }

            /**
             * Gets the dual enrollment crs location.
             *
             * @return String
             * @throws X2BaseException exception
             */
            public String getDualEnrollmentCrsLocation() throws X2BaseException {
                if (m_dualEnrollmentCrsLocation == null) {
                    m_dualEnrollmentCrsLocation = "";
                    StudentSchedule schedule = m_span.getSchedule();
                    if (schedule != null) {
                        DataDictionaryField field =
                                getFLData().translateAliasToDictionaryField(ALIAS_DUAL_ENROLLMENT_CRS_LOC_SSC, false);
                        if (field != null) {
                            m_dualEnrollmentCrsLocation = (String) getFLData().getFieldValue(schedule, field);
                        }
                    } else {
                        StudentScheduleChange change = m_span.getExitChange();
                        DataDictionaryField field =
                                getFLData().translateAliasToDictionaryField(ALIAS_DUAL_ENROLLMENT_CRS_LOC_SCC, false);
                        if (field != null) {
                            m_dualEnrollmentCrsLocation = (String) getFLData().getFieldValue(change, field);
                        }
                    }
                    if (StringUtils.isEmpty(m_dualEnrollmentCrsLocation)
                            || NOT_APPLICABLE_Z.contentEquals(m_dualEnrollmentCrsLocation)) {
                        m_dualEnrollmentCrsLocation = getMasterScheduleInfo().getDualEnrollmentCrsLocation();
                    }
                }
                return m_dualEnrollmentCrsLocation;
            }

            /**
             * Gets the dual enrollment indicator.
             *
             * @return the dual enrollment indicator
             * @throws X2BaseException the x 2 base exception
             */
            public String getDualEnrollmentIndicator() throws X2BaseException {
                if (m_dualEnrollmentIndicator == null) {
                    m_dualEnrollmentIndicator = "";
                    StudentSchedule schedule = m_span.getSchedule();
                    if (schedule != null) {
                        DataDictionaryField field =
                                getFLData().translateAliasToDictionaryField(ALIAS_DUAL_ENROLLMENT_SSC, false);
                        if (field != null) {
                            m_dualEnrollmentIndicator = (String) getFLData().getFieldValue(schedule, field);
                        }
                    } else {
                        StudentScheduleChange change = m_span.getExitChange();
                        DataDictionaryField field =
                                getFLData().translateAliasToDictionaryField(ALIAS_DUAL_ENROLLMENT_SCC, false);
                        if (field != null) {
                            m_dualEnrollmentIndicator = (String) getFLData().getFieldValue(change, field);
                        }
                    }
                    if (StringUtils.isEmpty(m_dualEnrollmentIndicator)
                            || NOT_APPLICABLE_Z.contentEquals(m_dualEnrollmentIndicator)) {
                        m_dualEnrollmentIndicator = getMasterScheduleInfo().getDualEnrollmentIndicator();
                    }
                }
                return m_dualEnrollmentIndicator;
            }

            /**
             * Gets the dual enrollment type.
             *
             * @return the dual enrollment type
             * @throws X2BaseException the x 2 base exception
             */
            public String getDualEnrollmentType() throws X2BaseException {
                if (m_dualEnrollmentType == null) {
                    m_dualEnrollmentType = "";
                    StudentSchedule schedule = m_span.getSchedule();
                    if (schedule != null) {
                        DataDictionaryField field =
                                getFLData().translateAliasToDictionaryField(ALIAS_DUAL_ENROLLMENT_TYPE_SSC, false);
                        if (field != null) {
                            m_dualEnrollmentType = (String) getFLData().getFieldValue(schedule, field);
                        }
                    }
                    if (StringUtils.isEmpty(m_dualEnrollmentType)) {
                        return NOT_APPLICABLE_Z;
                    }
                }
                return m_dualEnrollmentType;
            }

            /**
             * Gets the ell instructional model.
             *
             * @return the ell instructional model
             * @throws X2BaseException the x 2 base exception
             */
            public String getEllInstructionalModel() throws X2BaseException {
                if (m_ellInstructionalModel == null) {
                    m_ellInstructionalModel = "";
                    StudentSchedule schedule = m_span.getSchedule();
                    if (schedule != null) {
                        DataDictionaryField field =
                                getFLData().translateAliasToDictionaryField(ALIAS_ELL_MODEL_SSC, false);
                        if (field != null) {
                            m_ellInstructionalModel = (String) getFLData().getFieldValue(schedule, field);
                        }
                    } else {
                        StudentScheduleChange change = m_span.getExitChange();
                        DataDictionaryField field =
                                getFLData().translateAliasToDictionaryField(ALIAS_ELL_MODEL_SCC, false);
                        if (field != null) {
                            m_ellInstructionalModel = (String) getFLData().getFieldValue(change, field);
                        }
                    }
                    if (StringUtils.isEmpty(m_ellInstructionalModel)
                            || NOT_APPLICABLE_Z.equals(m_ellInstructionalModel)) {
                        m_ellInstructionalModel = getMasterScheduleInfo().getEllInstructionalModel();
                    }
                }
                return m_ellInstructionalModel;
            }

            /**
             * Gets the fin assistance received.
             *
             * @return String
             * @throws X2BaseException exception
             */
            public String getFinAssistanceReceived() throws X2BaseException {
                String codes = getValueByAlias(m_span.getSchedule(), ALIAS_WDIS_FINANCIAL_ASSISTANCE_REC);
                return codes == null ? null : codes.replace(",", "");
            }

            /**
             * Gets the fte.
             *
             * @return the fte
             * @throws X2BaseException exception
             */
            public Integer getFte() throws X2BaseException {
                if (m_fte == null) {
                    m_fte = INTEGER_ZERO;
                    String gradeLevel =
                            getStudentInfo().getGradeLevel(getFLData().getSurveyPeriod().getDateCertain());

                    if (this.getMasterScheduleInfo().isVirtualCourse()) {
                        BigDecimal credit = BigDecimal.ZERO;
                        if (m_span.getTranscript() != null) {
                            Transcript transcript = m_span.getTranscript();
                            credit = transcript.getTotalCredit() == null ? BigDecimal.ZERO
                                    : transcript.getTotalCredit();
                        }
                        m_fte = Integer.valueOf(getFteCalculator().getFteValueFromCredit(gradeLevel, credit));
                    } else {
                        if ((GRADE_LEVELS_PK_3.contains(gradeLevel) || GRADE_LEVELS_4_8.contains(gradeLevel)
                                || GRADE_LEVELS_9_12.contains(gradeLevel)) &&
                                studentIsMember() && studentAttendanceQualifies(getFLData().getCurrentContext())) {
                            m_fte = Integer.valueOf(getFteCalculator().getFteValue(gradeLevel,
                                    getMasterScheduleInfo().getMinutesPerWeek()));
                        }
                    }
                }
                return m_fte;
            }

            /**
             * Gets the fte weighted.
             *
             * @return Integer
             * @throws X2BaseException exception
             */
            public Integer getFteWeighted() throws X2BaseException {
                if (m_fteWeighted == null) {
                    m_fteWeighted = INTEGER_ZERO;
                    String gradeLevel =
                            getStudentInfo().getGradeLevel(getFLData().getSurveyPeriod().getDateCertain());

                    if (this.getMasterScheduleInfo().isVirtualCourse()) {
                        BigDecimal credit = BigDecimal.ZERO;
                        if (m_span.getTranscript() != null) {
                            Transcript transcript = m_span.getTranscript();
                            credit = transcript.getTotalCredit() == null ? BigDecimal.ZERO
                                    : transcript.getTotalCredit();
                        }
                        m_fteWeighted = Integer.valueOf(getFteCalculator().getWeightedFteValueFromCredit(gradeLevel,
                                getStudentInfo().getFefpProgram(), credit));
                    } else {
                        if ((GRADE_LEVELS_PK_3.contains(gradeLevel) || GRADE_LEVELS_4_8.contains(gradeLevel)
                                || GRADE_LEVELS_9_12.contains(gradeLevel)) &&
                                studentIsMember() && studentAttendanceQualifies(getFLData().getCurrentContext())) {
                            m_fteWeighted = Integer.valueOf(getFteCalculator().getWeightedFteValue(gradeLevel,
                                    getStudentInfo().getFefpProgram(), getMasterScheduleInfo().getMinutesPerWeek()));
                        }
                    }
                }
                return m_fteWeighted;
            }

            /**
             * Gets the full time indicator.
             *
             * @return String
             * @throws X2BaseException exception
             */
            public String getFullTimeIndicator() throws X2BaseException {
                String value = getValueByAlias(m_span.getSchedule(), ALIAS_WDIS_FULL_TIME_INDICATOR_SSC);
                if (StringUtils.isEmpty(value)) {
                    value = getValueByAlias(m_student, ALIAS_WDIS_FULL_TIME_INDICATOR_STD);
                }
                return value;
            }

            /**
             * Gets the master schedule info.
             *
             * @return the master schedule info
             */
            public MasterScheduleInfo getMasterScheduleInfo() {
                if (m_masterScheduleInfo == null) {
                    m_masterScheduleInfo = getScheduleHelper().getMasterScheduleInfo(getSection().getOid());
                }
                return m_masterScheduleInfo;
            }

            /**
             * Gets the organization.
             *
             * @return the organization
             */
            public SisOrganization getOrganization() {
                if (m_organization == null) {
                    m_organization = m_span.getSection().getSchedule().getSchool().getOrganization1();
                }
                return m_organization;
            }

            /**
             * Gets the post secondary school of enrollment.
             *
             * @return Sis school
             * @throws X2BaseException exception
             */
            public String getPostSecondarySchoolOfEnrollment() throws X2BaseException {
                return getValueByAlias(m_span.getSection().getSchoolCourse().getSchool(), ALIAS_WDIS_PS_SCHOOL_ENR);
            }

            /**
             * Gets the reading intervention indicator.
             *
             * @return the reading intervention indicator
             * @throws X2BaseException the x 2 base exception
             */
            public String getReadingInterventionIndicator() throws X2BaseException {
                if (m_readingInterventionIndicator == null) {
                    m_readingInterventionIndicator = "N";
                    StudentSchedule schedule = m_span.getSchedule();
                    if (schedule != null) {
                        DataDictionaryField field =
                                getFLData().translateAliasToDictionaryField(ALIAS_READING_INTERVENTION_SSC, false);
                        if (field != null) {
                            Boolean value = (Boolean) getFLData().getFieldValue(schedule, field);
                            if (value != null && value.booleanValue()) {
                                m_readingInterventionIndicator = "Y";
                            }
                        }
                    } else {
                        StudentScheduleChange change = m_span.getExitChange();
                        DataDictionaryField field =
                                getFLData().translateAliasToDictionaryField(ALIAS_READING_INTERVENTION_SCC, false);
                        if (field != null) {
                            Boolean value = (Boolean) getFLData().getFieldValue(change, field);
                            if (value != null && value.booleanValue()) {
                                m_readingInterventionIndicator = "Y";
                            }
                        }
                    }
                    if (StringUtils.isEmpty(m_readingInterventionIndicator)
                            || NOT_APPLICABLE_Z.equals(m_readingInterventionIndicator)) {
                        m_readingInterventionIndicator = getMasterScheduleInfo().getReadingInterventionIndicator();
                    }
                }
                return m_readingInterventionIndicator;
            }

            /**
             * Gets the residency tuition purposes.
             *
             * @return String
             * @throws X2BaseException exception
             */
            public String getResidencyTuitionPurposes() throws X2BaseException {
                String value = getValueByAlias(m_span.getExitChange(), ALIAS_RESIDENCY_TUITION_PURPOSES_SCC);
                if (value == null) {
                    value = getValueByAlias(m_span.getSchedule(), ALIAS_RESIDENCY_TUITION_PURPOSES_SSC);
                }
                if (value == null) {
                    value = getValueByAlias(m_span.getSection(), ALIAS_RESIDENCY_TUITION_PURPOSES_MST);
                }
                return value;
            }

            /**
             * Gets the school.
             *
             * @return the school
             */
            public SisSchool getSchool() {
                if (m_school == null) {
                    m_school = m_span.getSection().getSchedule().getSchool();
                }
                return m_school;
            }

            /**
             * Gets the section.
             *
             * @return the section
             */
            public MasterSchedule getSection() {
                if (m_section == null) {
                    m_section = m_span.getSection();
                }
                return m_section;
            }

            /**
             * Gets the student info.
             *
             * @return the student info
             */
            public StudentInfo getStudentInfo() {
                if (m_studentInfo == null) {
                    m_studentInfo = FLStudentHelper.this.getStudentInfo(m_student);
                }
                return m_studentInfo;
            }

            /**
             * Gets the transcript.
             *
             * @return Transcript
             */
            public Transcript getTranscript() {
                return m_span.getTranscript();
            }

            /**
             * Gets the transcript grade.
             *
             * @return String
             */
            public String getTranscriptGrade() {
                String value = null;
                if (m_span.getTranscript() != null) {
                    value = m_span.getTranscript().getFinalGrade();
                }
                return value;
            }

            /**
             * Gets the transcript grade letter.
             *
             * @return String
             */
            public String getTranscriptGradeLetter() {
                String value = null;
                if (m_span.getTranscript() != null) {
                    value = m_span.getTranscript().getFinalGrade();
                    if (!StringUtils.isEmpty(value)) {
                        if (m_gradeScales == null) {
                            loadGradeScales();
                        }
                        GradeScale scale = m_gradeScales.get(m_span.getTranscript().getTranscriptDefinitionOid());
                        if (StringUtils.isNumeric(value) && scale != null) {
                            // Try the final grade as a number.
                            BigDecimal gradeAsNumber = null;
                            try {
                                gradeAsNumber = new BigDecimal(value);
                            } catch (NumberFormatException nfe) {
                                // nothing. The grade is not numeric.
                            }

                            if (gradeAsNumber != null) {
                                value = m_gradesManager.getLetterValue(gradeAsNumber, scale,
                                        m_span.getTranscript().getSchool(),
                                        m_span.getTranscript().getSchoolCourseOid());
                            }
                        }
                    }
                }
                return value;
            }

            /**
             * Gets the wdis compl point.
             *
             * @param programCode
             *
             * @return String
             * @throws X2BaseException exception
             */
            public String getWdisComplPoint(String programCode) throws X2BaseException {
                if (m_wdisComplPoint == null) {
                    if (m_cpcHelper == null) {
                        m_cpcHelper = new CompletionPointHelper(getFLData().getSurveyPeriod().getEndDate());
                    }
                    m_wdisComplPoint = m_cpcHelper.getCompletionPointCode(m_student, programCode);
                }
                return m_wdisComplPoint;
            }

            /**
             * Gets the post secondary course status.
             *
             * @return String
             * @throws X2BaseException exception
             */
            public String getWdisCourseStatus() throws X2BaseException {
                String value = null;
                if (m_wdisCourseStatuses == null) {
                    X2Criteria criteria = new X2Criteria();
                    criteria.addEqualTo(ReferenceCode.REL_REFERENCE_TABLE + PATH_DELIMITER + X2BaseBean.COL_OID,
                            "rtbFlWdisPsSt");
                    SubQuery query = new SubQuery(ReferenceCode.class, ReferenceCode.COL_CODE, criteria);
                    m_wdisCourseStatuses = getFLData().getBroker().getSubQueryCollectionByQuery(query);
                }
                PlainDate exitDate = m_span.getExitDate();
                if (exitDate == null || exitDate.after(getFLData().getSurveyPeriod().getEndDate())) {
                    value = "C";
                } else {
                    value = getTranscriptGradeLetter();
                    if (value != null && !m_wdisCourseStatuses.contains(value)) {
                        if (value.matches("^[A-D][+-]{0,1}$")) {
                            value = "S";
                        } else if ("F".equals(value)) {
                            value = "U";
                        }
                    }
                }
                return value;
            }

            /**
             * Gets the wdis entry date.
             *
             * @return Plain date
             */
            public PlainDate getWdisEntryDate() {
                PlainDate surveyStartDate = getFLData().getSurveyPeriod().getStartDate();
                PlainDate entryDate = m_span.getEntryDate();
                if (entryDate == null || entryDate.before(surveyStartDate)) {
                    entryDate = surveyStartDate;
                }
                return entryDate;
            }

            /**
             * Gets the wdis exit date.
             *
             * @return Plain date
             */
            public PlainDate getWdisExitDate() {
                PlainDate surveyEndDate = getFLData().getSurveyPeriod().getEndDate();
                PlainDate exitDate = m_span.getExitDate();
                if (exitDate == null || exitDate.after(surveyEndDate)) {
                    exitDate = surveyEndDate;
                }
                return exitDate;
            }

            /**
             * Gets the wdis total hours earned to award.
             *
             * @return String
             * @throws X2BaseException exception
             */
            public String getWdisTotalHoursEarnedToAward() throws X2BaseException {
                String value = getValueByAlias(m_span.getSchedule(), ALIAS_WDIS_TOTAL_HRS_TO_AWARD_SSC);
                if (StringUtils.isEmpty(value)) {
                    value = getValueByAlias(getSection(), ALIAS_WDIS_TOTAL_HRS_TO_AWARD_MST);
                }
                return value;
            }

            /**
             * Gets the wdis literacy compl point date.
             *
             * @param programCode
             *
             * @return String
             * @throws X2BaseException exception
             */
            public PlainDate getWdisLitComplPointDate(String programCode) throws X2BaseException {
                if (m_wdisLitComplPointDate == null) {
                    if (m_cpcHelper == null) {
                        m_cpcHelper = new CompletionPointHelper(getFLData().getSurveyPeriod().getEndDate());
                    }
                    m_wdisLitComplPointDate = m_cpcHelper.getLiteracyPointDate(m_student, programCode);
                }
                return m_wdisLitComplPointDate;
            }

            /**
             * Gets the student instr hrs.
             *
             * @return String
             * @throws X2BaseException exception
             */
            public String getWdisStudentInstrHrs() throws X2BaseException {
                String value = getValueByAlias(m_span.getSchedule(), ALIAS_WDIS_STUDENT_INSTR_HRS_SSC);
                if (StringUtils.isEmpty(value)) {
                    value = getValueByAlias(getSection(), ALIAS_WDIS_STUDENT_INSTR_HRS_MST);
                }
                return value;
            }

            /**
             * A student is considered to have met the attendance requirement if the student has
             * been in attendance
             * at least one day of survey week or on one of the six scheduled school days
             * preceding the survey week when the school was in session.
             *
             * @return true, if successful
             */
            private boolean studentAttendanceQualifies(DistrictSchoolYearContext context) {
                Set<PlainDate> dates = new TreeSet();
                dates.addAll(getMasterScheduleInfo().getFteDates());
                StudentInfo studentInfo = getStudentInfo();
                dates.retainAll(studentInfo.getMemberDates(context));
                dates.removeAll(studentInfo.getAbsentDates(context));
                return !dates.isEmpty();
            }

            /**
             * If the student has at least one day of membership during survey week, the student
             * meets the membership
             * requirement and is eligible for reporting. The student is in membership when he
             * or
             * she is officially
             * assigned to a course or program by a school or district.
             *
             * @return true, if successful
             */
            private boolean studentIsMember() {
                return getStudentInfo().hasServices();
            }

            /**
             * Gets the from SSC or SCC.
             *
             * @param aliasSchedule String
             * @param aliasChange String
             * @return String
             * @throws X2BaseException exception
             */
            private String getFromSscOrScc(String aliasSchedule, String aliasChange)
                    throws X2BaseException {
                String value = null;
                StudentSchedule schedule = m_span.getSchedule();
                if (schedule != null) {
                    value = getValueByAlias(schedule, aliasSchedule);
                } else {
                    StudentScheduleChange change = m_span.getExitChange();
                    value = getValueByAlias(change, aliasChange);
                }
                return value;
            }

            /**
             * Gets the adult fee status.
             *
             * @param aliasSchedule String
             * @param aliasStudent String
             * @return String
             * @throws X2BaseException exception
             */
            private String getAdultFeeStatus(String aliasSchedule, String aliasStudent)
                    throws X2BaseException {
                String value = null;
                StudentSchedule schedule = m_span.getSchedule();
                if (schedule != null) {
                    value = getValueByAlias(schedule, aliasSchedule);
                }
                if (StringUtils.isEmpty(value)) {
                    value = getValueByAlias(m_student, aliasStudent);
                }
                return value;
            }

            /**
             * Gets the wdis value.
             *
             * @param bean X2BaseBean
             * @param alias String
             * @return String
             * @throws X2BaseException exception
             */
            private String getValueByAlias(X2BaseBean bean, String alias) throws X2BaseException {
                if (m_fieldsByAlias.get(alias) == null) {
                    DataDictionaryField field = getFLData().translateAliasToDictionaryField(alias, true);
                    m_fieldsByAlias.put(alias, field);
                }
                return (String) getFLData().getFieldValue(bean, m_fieldsByAlias.get(alias));
            }
        }

        private PlainDate m_startDate;
        private PlainDate m_endDate;
        private FLScheduleHelper m_scheduleHelper;

        /**
         * Instantiates a new student schedule helper.
         *
         * @param scheduleHelper the schedule helper
         * @param startDate the start date
         * @param endDate the end date
         */
        public StudentScheduleHelper(FLScheduleHelper scheduleHelper, PlainDate startDate, PlainDate endDate) {
            m_scheduleHelper = scheduleHelper;
            m_startDate = startDate;
            m_endDate = endDate;
            if (m_startDate == null || m_endDate == null) {
                throw new X2RuntimeException(
                        new UnsupportedOperationException(
                                "StudentScheduleHelper cannot be created with null dates"));
            }
        }

        /**
         * Gets the student schedule info.
         *
         * @param student the student
         * @return the student schedule info
         */
        public List<StudentScheduleInfo> getStudentScheduleInfo(SisStudent student) {
            List<StudentScheduleSpan> spans = FLStudentHelper.this.getStudentScheduleSpans(student);
            List<StudentScheduleInfo> infoSpans = new ArrayList(spans.size());

            for (StudentScheduleSpan span : spans) {
                if (span.getSection() != null &&
                        m_scheduleHelper.getMasterSchedule(span.getSection().getOid()) != null &&
                        span.getEntryDate() != null && span.getExitDate() != null &&
                        !span.getEntryDate().after(m_endDate) && !span.getExitDate().before(m_startDate)) {
                    infoSpans.add(new StudentScheduleInfo(student, span));
                }
            }
            return infoSpans;
        }

        /**
         * Gets the schedule helper.
         *
         * @return the schedule helper
         */
        public FLScheduleHelper getScheduleHelper() {
            return m_scheduleHelper;
        }
    }


    /**
     * The Class StudentProgramDataset.
     */
    public class StudentProgramDataset {
        private static final String ERROR_MSG_DDX = "Extended data dictionary was not found.";

        private X2Criteria m_criteria;
        private StateReportData m_data;
        private DataDictionary m_dictionary = null;
        private String m_ddxId;
        private PlainDate m_endDate;
        private Map<String, List<StudentProgramParticipation>> m_map = null;
        private PlainDate m_startDate;
        private SubQuery m_subQuery;

        /**
         * Instantiates a new student program dataset.
         *
         * @param stateReportData the state report data
         * @param ddxId the ddx id
         * @param startDate the start date
         * @param endDate the end date
         */
        public StudentProgramDataset(StateReportData stateReportData, String ddxId, PlainDate startDate,
                PlainDate endDate) {
            m_data = stateReportData;
            m_ddxId = ddxId;
            m_startDate = startDate;
            m_endDate = endDate;
        }

        /**
         * Equals.
         *
         * @param obj the obj
         * @return true, if successful
         * @see java.lang.Object#equals(java.lang.Object)
         */
        @Override
        public boolean equals(Object obj) {
            if (obj instanceof StudentProgramDataset) {
                StudentProgramDataset data = (StudentProgramDataset) obj;
                return safeEquals(m_ddxId, data.m_ddxId) && safeEquals(m_startDate, data.m_startDate)
                        && safeEquals(m_endDate, data.m_endDate);
            }
            return false;
        }

        /**
         * Gets the alias value.
         *
         * @param pgm the pgm
         * @param alias the alias
         * @param stateReferenceCode the state reference code
         * @return the alias value
         */
        public Object getAliasValue(StudentProgramParticipation pgm, String alias, boolean stateReferenceCode) {
            Object value = null;
            DataDictionaryField field = getDataDictionary().findDataDictionaryFieldByAlias(alias);
            if (field != null) {
                value = pgm.getFieldValueByBeanPath(field.getJavaName());
                if (stateReferenceCode && value != null && value instanceof String && field.hasReferenceTable()) {
                    value = m_data.lookupReferenceCodeByRefTbl(field.getReferenceTableOid(), (String) value,
                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                }
            }
            return value;
        }

        /**
         * Returns a local instance of a district data dictionary.
         *
         * @return DataDictionary.
         */
        public DataDictionary getDataDictionary() {
            if (m_dictionary == null) {
                X2Criteria ddxCriteria = new X2Criteria();

                ddxCriteria.addEqualTo(ExtendedDataDictionary.COL_ID, m_ddxId);

                QueryByCriteria ddxQuery = new QueryByCriteria(ExtendedDataDictionary.class, ddxCriteria);
                ExtendedDataDictionary ddx = (ExtendedDataDictionary) m_data.getBroker().getBeanByQuery(ddxQuery);
                if (ddx == null) {
                    // Use default data dictionary
                    m_data.addSetupError(ERROR_MSG_DDX, m_ddxId);
                    m_dictionary = m_data.getDataDictionary();
                } else {
                    m_dictionary =
                            DataDictionary.getDistrictDictionary(ddx, m_data.getBroker().getPersistenceKey());
                }
            }

            return m_dictionary;
        }

        /**
         * get list of student programs ordered by start date.
         *
         * @param oid the oid
         * @return the programs
         */
        public List<StudentProgramParticipation> getPrograms(String oid) {
            if (m_map == null) {
                initializePrograms();
            }
            return m_map.get(oid);
        }

        /**
         * Gets the student sub query.
         *
         * @return the student sub query
         */
        public SubQuery getStudentSubQuery() {
            if (m_subQuery == null) {
                m_subQuery = new SubQuery(StudentProgramParticipation.class,
                        StudentProgramParticipation.COL_STUDENT_OID, getCriteria());
            }
            return m_subQuery;
        }

        /**
         * Hash code.
         *
         * @return the int
         * @see java.lang.Object#hashCode()
         */
        @Override
        public int hashCode() {
            return m_ddxId.hashCode() + m_startDate.hashCode() + m_endDate.hashCode();
        }

        /**
         * Gets the criteria.
         *
         * @return the criteria
         */
        private X2Criteria getCriteria() {
            if (m_criteria == null) {
                PlainDate startDate = m_startDate;
                if (startDate == null) {
                    startDate = m_data.getCurrentContext().getStartDate();
                }
                PlainDate endDate = m_endDate;
                if (endDate == null) {
                    endDate = m_data.getCurrentContext().getEndDate();
                }

                m_criteria = new X2Criteria();
                // Date range criteria
                m_criteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, endDate);
                X2Criteria endDate1Criteria = new X2Criteria();
                endDate1Criteria.addEmpty(StudentProgramParticipation.COL_END_DATE,
                        m_data.getBroker().getPersistenceKey());
                X2Criteria endDate2Criteria = new X2Criteria();
                endDate2Criteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_END_DATE, startDate);
                endDate1Criteria.addOrCriteria(endDate2Criteria);
                m_criteria.addAndCriteria(endDate1Criteria);

                // Extended data dictionary
                m_criteria.addEqualTo(StudentProgramParticipation.REL_EXTENDED_DATA_DICTIONARY + PATH_DELIMITER
                        + ExtendedDataDictionary.COL_ID, m_ddxId);
            }
            return m_criteria;
        }

        /**
         * Initialize programs.
         */
        private void initializePrograms() {
            // use copy of criteria to avoid recursion issue if this criteria is already
            // included in
            // the student
            BeanQuery query = getStudentSelectionQuery(StudentProgramParticipation.class, getCriteria().copy(),
                    StudentProgramParticipation.COL_STUDENT_OID);
            query.addOrderBy(StudentProgramParticipation.COL_STUDENT_OID, true);
            query.addOrderBy(StudentProgramParticipation.COL_START_DATE, true);
            m_map = m_data.getBroker().getGroupedCollectionByQuery(query,
                    StudentProgramParticipation.COL_STUDENT_OID,
                    1024);
        }

        /**
         * Safe equals.
         *
         * @param obj1 the obj 1
         * @param obj2 the obj 2
         * @return true, if successful
         */
        private boolean safeEquals(Object obj1, Object obj2) {
            return (obj1 == null && obj2 == null) || (obj1 != null && obj1.equals(obj2));
        }

    }


    /**
     * The Class StudentVictim.
     */
    public class StudentVictim {
        private boolean m_isDisabilityBased;
        private boolean m_isRaceBased;
        private boolean m_isReligionBased;
        private boolean m_isSexBased;
        private boolean m_isSexualOrientationBased;

        /**
         * Checks if is disability based.
         *
         * @return true, if is disability based
         */
        public boolean isDisabilityBased() {
            return m_isDisabilityBased;
        }

        /**
         * Checks if is race based.
         *
         * @return true, if is race based
         */
        public boolean isRaceBased() {
            return m_isRaceBased;
        }

        /**
         * Checks if is religion based.
         *
         * @return true, if is religion based
         */
        public boolean isReligionBased() {
            return m_isReligionBased;
        }

        /**
         * Checks if is sex based.
         *
         * @return true, if is sex based
         */
        public boolean isSexBased() {
            return m_isSexBased;
        }

        /**
         * Checks if is sexual orientation based.
         *
         * @return true, if is sexual orientation based
         */
        public boolean isSexualOrientationBased() {
            return m_isSexualOrientationBased;
        }

        /**
         * Sets the disability based.
         */
        public void setDisabilityBased() {
            this.m_isDisabilityBased = true;
        }

        /**
         * Sets the race based.
         */
        public void setRaceBased() {
            this.m_isRaceBased = true;
        }

        /**
         * Sets the religion based.
         */
        public void setReligionBased() {
            this.m_isReligionBased = true;
        }

        /**
         * Sets the sex based.
         */
        public void setSexBased() {
            this.m_isSexBased = true;
        }

        /**
         * Sets the sexual orientation based.
         */
        public void setSexualOrientationBased() {
            this.m_isSexualOrientationBased = true;
        }
    }

    public static final String GRADE_LEVEL_KG = "KG";
    public static final String GRADE_LEVEL_PK = "PK";
    public static final List<String> GRADE_LEVELS_PK_3 = Arrays.asList("PK", "KG", "01", "02", "03");
    public static final List<String> GRADE_LEVELS_4_8 = Arrays.asList("04", "05", "06", "07", "08");
    public static final List<String> GRADE_LEVELS_9_12 = Arrays.asList("09", "10", "11", "12");

    protected static final String ALIAS_CTE_EXCEPTIONAL_SETTING_SCC = "all-scc-CTEExceptionalCourseSetting";
    protected static final String ALIAS_CTE_EXCEPTIONAL_SETTING_SSC = "all-ssc-CTEExceptionalCourseSetting";
    protected static final String ALIAS_DUAL_ENROLLMENT_SCC = "all-scc-DualEnrollmentIndicator";
    protected static final String ALIAS_DUAL_ENROLLMENT_SSC = "all-ssc-DualEnrollmentIndicator";
    protected static final String ALIAS_DUAL_ENROLLMENT_CRS_LOC_SCC = "all-scc-DualEnrollmentCrsLoc";
    protected static final String ALIAS_DUAL_ENROLLMENT_CRS_LOC_SSC = "all-ssc-DualEnrollmentCrsLoc";
    protected static final String ALIAS_DUAL_ENROLLMENT_TYPE_SSC = "all-ssc-DualEnrollmentInstitutionType";
    protected static final String ALIAS_FED_CONNECTED_INDICATOR = "all-std-FederallyConnectedIndicator";
    protected static final String ALIAS_ELL_MODEL_SCC = "all-scc-ELLInstructionalModel";
    protected static final String ALIAS_ELL_MODEL_SSC = "all-ssc-ELLInstructionalModel";
    protected static final String ALIAS_GPD_STATE_INDICATOR = "all-gpd-StateIndicator";
    protected static final String ALIAS_HARASSED_DISABILITY = "all-cnd-HarassmentBasedOnDisability";
    protected static final String ALIAS_HARASSED_ORIENTATION = "all-cnd-HarassmentBasedOnSexualOrientation";
    protected static final String ALIAS_HARASSED_RACE = "all-cnd-HarassmentBasedOnRace";
    protected static final String ALIAS_HARASSED_RELIGION = "all-cnd-HarassmentBasedOnReligion";
    protected static final String ALIAS_HARASSED_SEX = "all-cnd-HarassmentBasedOnSex";
    protected static final String ALIAS_READING_INTERVENTION_SCC = "all-scc-ReadingIntervention";
    protected static final String ALIAS_READING_INTERVENTION_SSC = "all-ssc-ReadingIntervention";
    protected static final String ALIAS_RESIDENCY_TUITION_PURPOSES_SCC = "all-scc-ResidTuitionPurposes";
    protected static final String ALIAS_RESIDENCY_TUITION_PURPOSES_SSC = "all-ssc-ResidTuitionPurposes";
    protected static final String ALIAS_RESIDENCY_TUITION_PURPOSES_MST = "all-mst-ResidTuitionPurposes";
    protected static final String ALIAS_SCHOOL_PMRN = "all-skl-ProgressMonitoringSchool";
    protected static final String ALIAS_SPED_FUNDING_TYPE = "all-std-SpecialEdFundingType";
    protected static final String ALIAS_US_ENTRY_DATE = "all-std-USEntryDate";
    protected static final String ALIAS_WDIS_ADULT_FEE_STATUS_FIRST_SSC = "all-ssc-WdisFeeStatusFirst";
    protected static final String ALIAS_WDIS_ADULT_FEE_STATUS_SECOND_SSC = "all-ssc-WdisFeeStatusSecond";
    protected static final String ALIAS_WDIS_ADULT_FEE_STATUS_FIRST_STD = "all-std-WdisFeeStatusFirst";
    protected static final String ALIAS_WDIS_ADULT_FEE_STATUS_SECOND_STD = "all-std-WdisFeeStatusSecond";
    protected static final String ALIAS_WDIS_AGE_FUNC_LEVEL_INIT_SCC = "all-scc-WdisFuncLvlInit";
    protected static final String ALIAS_WDIS_AGE_FUNC_LEVEL_INIT_SSC = "all-ssc-WdisFuncLvlInit";
    protected static final String ALIAS_WDIS_BASIC_SKILLS_EXAM_STD = "all-std-WdisCteBasicSkillsExam";
    protected static final String ALIAS_WDIS_COST_REPORTING_CODE = "all-ssc-WdisCostReportingCode";
    protected static final String ALIAS_WDIS_ENR_NOT_STATE_FUNDED_STD = "all-std-WdisStateFunded";
    protected static final String ALIAS_WDIS_ENR_NOT_STATE_FUNDED_CRS = "all-crs-WdisStateFunded";
    protected static final String ALIAS_WDIS_FINANCIAL_ASSISTANCE_REC = "all-ssc-WdisFinancialAssistance";
    protected static final String ALIAS_WDIS_FULL_TIME_INDICATOR_SSC = "all-ssc-WdisFullTimeStdInd";
    protected static final String ALIAS_WDIS_FULL_TIME_INDICATOR_STD = "all-std-WdisFullTimeStdInd";
    protected static final String ALIAS_WDIS_PS_COURSE_STATUS = "all-ssc-WdisPostSecCourseStatus";
    protected static final String ALIAS_WDIS_PS_SCHOOL_ENR = "all-skl-StateId";
    protected static final String ALIAS_WDIS_STUDENT_INSTR_HRS_SSC = "all-ssc-WdisStdInstrHrs";
    protected static final String ALIAS_WDIS_STUDENT_INSTR_HRS_MST = "all-mst-WdisStdInstrHrs";
    protected static final String ALIAS_WDIS_TOTAL_HRS_TO_AWARD_MST = "all-mst-WdisHrsToAward";
    protected static final String ALIAS_WDIS_TOTAL_HRS_TO_AWARD_SSC = "all-ssc-WdisHrsToAward";
    protected static final String NOT_APPLICABLE_Z = "Z";

    protected static final String FEFP_BASIC_K_3 = "101";
    protected static final String FEFP_BASIC_4_8 = "102";
    protected static final String FEFP_BASIC_9_12 = "103";
    protected static final String FEFP_ESOL = "130";
    protected static final String FEFP_CAREER = "300";
    protected static final String FEFP_OTHER = "999";

    protected static final List<String> REPORTABLE_ACTION_CODES =
            Arrays.asList("C", "E", "F", "H", "I", "L", "M", "O", "P", "R", "S", "U");
    protected static final List<String> REPORTABLE_INCIDENT_CODES = Arrays.asList(
            "ARS", "BRK", "BUL", "DOC", "FIT", "HAR", "HAZ", "OMC", "PHA",
            "ROB", "SXH", "BAT", "HOM", "KID", "SXB", "ALC", "DRD", "DRU",
            "STL", "SXA", "SXO", "TBC", "TRE", "TRS", "UBL", "UHR", "VAN", "WPO");


    protected static final String WITHDRAW_CODE_PROMOTED_IN_SCHOOL = "W01";
    protected static final String WITHDRAW_CODE_PROMOTED_IN_DISTRICT = "W";
    protected final List<String> WITHDRAW_CODES =
            Arrays.asList("DNE", "W01", "W02", "W3A", "W3B", "W04", "W05", "W12", "W13", "W15", "W18", "W21", "W22",
                    "W23", "W24", "W25", "W26", "WPO");

    private static final String ALIAS_EXCLUDE_STD = "DOE EXCLUDE STD";

    private static final List<String> GRADE_LEVELS_SURVEY_6 =
            Arrays.asList("KG", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12");
    private static final List<String> GRADE_LEVELS_SURVEY_8 =
            Arrays.asList("03", "04", "05", "06", "07", "08", "09", "10", "11", "12");

    private static final int IMMIGRATION_AGE = 21;
    private static final String IMMIGRATION_SCHOOL_ID = "9997";
    private static final int INITIAL_MAP_SIZE = 1000;
    private static final int MAX_LEGAL_NAME_LEN = 42;

    private static final String REFERENCE_TABLE_CND_ACTION = "rtbCndAction";

    private static final String STD_PROGRAM_NEGLECT = "FL-PGM-NEGLECT";
    private static final String STD_PROGRAM_SES = "FL-PGM-SES";

    private Map<String, Collection<StudentAssessment>> m_assessmentMap;
    private CompletionPointHelper m_cpcHelper;
    private String m_fedConnectedIndicator;
    private DataDictionaryField m_fieldEnrollmentCode;
    private DataDictionaryField m_fieldGpa;
    private DataDictionaryField m_fieldGpdSchoolIndicator;
    private DataDictionaryField m_fieldSchoolPmrn;
    private String m_fieldWdisAgeIndicator;
    private FteCalculator m_fteCalculator;
    private Collection<String> m_immigrantStudentOids;
    private GradesManager m_gradesManager;
    private GradeMatcher m_gradeMatcher;
    private Map<String, GradeScale> m_gradeScales;
    private Collection<GradePointAverageDefinition> m_gpaDefinitions;

    private Map<String, Collection<Race>> m_raceMap;
    private Map<StudentConductDataset, StudentConductDataset> m_studentConductDatasets = new HashMap();
    private Map<StudentAttendanceDataset, StudentAttendanceDataset> m_studentAttendanceDatasets = new HashMap();
    private Map<String, StudentInfo> m_studentInfoMap = new HashMap();
    private Map<StudentProgramDataset, StudentProgramDataset> m_studentProgramDatasets = new HashMap();

    private StudentConductDataset m_studentConductDataset;

    private Map<String, DataDictionaryField> m_fieldsByAlias = new HashMap<>();

    private String m_wdisMode;

    /**
     * Instantiates a new FL student helper.
     *
     * @param data the data
     * @throws X2BaseException the x 2 base exception
     */
    public FLStudentHelper(FLStateReportData data) throws X2BaseException {
        super(data);

        setStudentSelectionMode(MODE_STUDENT_ACTIVE_ANY_TIME);
        setSelectionProperty(PROPERTY_APPLY_SCHOOL, Boolean.FALSE);
        setSelectionProperty(PROPERTY_SCHEDULE_SPANS, Boolean.TRUE);
        setSelectionProperty(StudentHistoryHelper.PROPERTY_INCLUDE_SECONDARY, Boolean.TRUE);

        if (getFLData().getSurveyPeriod() != null) {
            setSelectionProperty(PROPERTY_BEGIN_DATE, getFLData().getSurveyPeriod().getStartDate());
            setSelectionProperty(PROPERTY_END_DATE, getFLData().getSurveyPeriod().getEndDate());
            m_studentConductDataset = this.getStudentConductDataset(data.getCurrentContext().getStartDate(),
                    getFLData().getSurveyPeriod().getEndDate());
        }

        ModelProperty prop = new ModelProperty(StudentEnrollment.class, StudentEnrollment.COL_ENROLLMENT_CODE,
                getFLData().getBroker().getPersistenceKey());
        m_fieldEnrollmentCode = getFLData().getDataDictionary().findDataDictionaryField(prop.getFieldId());
        m_fieldGpdSchoolIndicator = data.translateAliasToDictionaryField(ALIAS_GPD_STATE_INDICATOR, true);
        m_fieldSchoolPmrn = data.translateAliasToDictionaryField(ALIAS_SCHOOL_PMRN, true);

        m_fieldWdisAgeIndicator = data.translateAliasToJavaName(ALIAS_WDIS_AGE_INDICTOR, true);

        // generate setup errors for other aliases that are required
        data.translateAliasToDictionaryField(ALIAS_SCHOOL_PMRN, true);
        data.translateAliasToDictionaryField(ALIAS_HARASSED_SEX, true);
        data.translateAliasToDictionaryField(ALIAS_HARASSED_DISABILITY, true);
        data.translateAliasToDictionaryField(ALIAS_HARASSED_RACE, true);
        data.translateAliasToDictionaryField(ALIAS_HARASSED_RELIGION, true);
        data.translateAliasToDictionaryField(ALIAS_HARASSED_ORIENTATION, true);
    }

    /**
     * Gets the fed connected indicator.
     *
     * @param student the student
     *
     * @return the fed connected indicator
     * @throws X2BaseException the x 2 base exception
     */
    public String getFedConnectedIndicator(SisStudent student) throws X2BaseException {
        if (m_fedConnectedIndicator == null) {
            m_fedConnectedIndicator = "";

            DataDictionaryField field =
                    getFLData().translateAliasToDictionaryField(ALIAS_FED_CONNECTED_INDICATOR, false);
            if (field != null) {
                m_fedConnectedIndicator = (String) getFLData().getFieldValue(student, field);
            }

            if (StringUtils.isEmpty(m_fedConnectedIndicator)) {
                return NOT_APPLICABLE_Z;
            }
        }
        return m_fedConnectedIndicator;
    }

    /**
     * Gets the fte calculator.
     *
     * @return the fte calculator
     * @throws X2BaseException exception
     */
    public FteCalculator getFteCalculator() throws X2BaseException {
        if (m_fteCalculator == null) {
            m_fteCalculator = new FteCalculator();
        }
        return m_fteCalculator;
    }

    /**
     * Gets the grade level.
     *
     * @param student the student
     * @param yog the yog
     * @return the grade level
     */
    public String getGradeLevel(SisStudent student, int yog) {
        String gradeLevel = student.getGradeLevel();
        ReferenceCode gradeCode = null;

        if (m_gradeMatcher == null) {
            m_gradeMatcher = new GradeMatcher();
        }

        if (student.getYog() != yog) {
            gradeCode = m_gradeMatcher.getReferenceCode(yog);
            if (gradeCode != null) {
                gradeLevel = gradeCode.getCode();
            }
        } else {
            gradeCode = m_gradeMatcher.getReferenceCode(gradeLevel);
        }
        if (gradeCode != null) {
            gradeLevel = gradeCode.getStateCode();
        }

        return gradeLevel;
    }

    /**
     * Gets the student attendance dataset.
     *
     * @param surveyPeriod the survey period
     * @return the student attendance dataset
     */
    public StudentAttendanceDataset getStudentAttendanceDataset(SurveyPeriod surveyPeriod) {
        StudentAttendanceDataset dataset = getStudentAttendanceDataset(surveyPeriod.getStartDate(),
                surveyPeriod.getEndDate());
        return dataset;
    }

    /**
     * Gets the student attendance dataset.
     *
     * @param startDate the start date
     * @param endDate the end date
     * @return the student attendance dataset
     */
    public StudentAttendanceDataset getStudentAttendanceDataset(PlainDate startDate, PlainDate endDate) {
        StudentAttendanceDataset dataset = new StudentAttendanceDataset(startDate, endDate);
        StudentAttendanceDataset existing = m_studentAttendanceDatasets.get(dataset);
        if (existing == null) {
            m_studentAttendanceDatasets.put(dataset, dataset);
        }
        return existing == null ? dataset : existing;
    }

    /**
     * Gets the student conduct dataset.
     *
     * @param startDate the start date
     * @param endDate the end date
     * @return the student conduct dataset
     */
    public StudentConductDataset getStudentConductDataset(PlainDate startDate, PlainDate endDate) {
        StudentConductDataset dataset = new StudentConductDataset(getData(), startDate, endDate);
        StudentConductDataset existing = m_studentConductDatasets.get(dataset);
        if (existing == null) {
            m_studentConductDatasets.put(dataset, dataset);
        }
        return existing == null ? dataset : existing;
    }

    /**
     * Return the current student criteria. If it has not been defined, create it.
     *
     * @return X2Criteria
     */
    @Override
    public X2Criteria getStudentCriteria() {
        X2Criteria criteria = super.getStudentCriteria();

        String surveyPeriod = getFLData().getSurveyPeriodCode();
        if (FLStateReportData.SURVEY_PERIOD_2.equals(surveyPeriod)
                || FLStateReportData.SURVEY_PERIOD_3.equals(surveyPeriod)) {
            X2Criteria iCriteria = new X2Criteria();
            iCriteria.addEqualTo(StudentEnrollment.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_SCHOOL_ID,
                    IMMIGRATION_SCHOOL_ID);
            iCriteria.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE,
                    getData().getCurrentContext().getStartDate());
            iCriteria.addLessOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE,
                    getData().getCurrentContext().getEndDate());
            SubQuery iSubQuery =
                    new SubQuery(StudentEnrollment.class, StudentEnrollment.COL_STUDENT_OID, iCriteria);

            X2Criteria orCriteria = new X2Criteria();
            orCriteria.addIn(X2BaseBean.COL_OID, iSubQuery);
            criteria.addOrCriteria(orCriteria);
        }

        return criteria;
    }

    /**
     * For FL, remove all spans without a school.
     *
     * @param student the student
     * @param limit the limit
     * @return the student enrollment spans
     * @see com.x2dev.sis.tools.stateexports.StudentHistoryHelper#getStudentEnrollmentSpans(com.follett.fsc.core.k12.beans.Student,
     *      boolean)
     */
    @Override
    public List<StudentEnrollmentSpan> getStudentEnrollmentSpans(Student student, boolean limit) {
        List<StudentEnrollmentSpan> spans = super.getStudentEnrollmentSpans(student, limit);
        Iterator<StudentEnrollmentSpan> iterator = spans.iterator();
        while (iterator.hasNext()) {
            StudentEnrollmentSpan span = iterator.next();
            if (span.getSchool() == null || span.getFirstActiveEnrollment() == null
                    || span.getFirstActiveDate() == null) {
                iterator.remove();
            }
        }
        return spans;
    }

    /**
     * Gets the student schedule change criteria.
     *
     * @return X 2 criteria
     * @see com.x2dev.sis.tools.stateexports.StudentHistoryHelper#getStudentScheduleChangeCriteria()
     */
    @Override
    public X2Criteria getStudentScheduleChangeCriteria() {
        return getAdjustedCriteria(super.getStudentScheduleChangeCriteria(), StudentScheduleChange.REL_MASTER_SCHEDULE);
    }

    /**
     * Gets the student schedule criteria.
     *
     * @return X 2 criteria
     * @see com.x2dev.sis.tools.stateexports.StudentHistoryHelper#getStudentScheduleCriteria()
     */
    @Override
    public X2Criteria getStudentScheduleCriteria() {
        return getAdjustedCriteria(super.getStudentScheduleCriteria(), StudentSchedule.REL_SECTION);
    }

    /**
     * Gets the student transcript criteria.
     *
     * @return X 2 criteria
     * @see com.x2dev.sis.tools.stateexports.StudentHistoryHelper#getStudentTranscriptCriteria()
     */
    @Override
    public X2Criteria getStudentTranscriptCriteria() {
        return getAdjustedCriteria(super.getStudentTranscriptCriteria(), Transcript.REL_MASTER_SCHEDULE);
    }

    /**
     * Gets the std grade point.
     *
     * @return the m_stdGradePointMap
     */
    public DataDictionaryField getGpaField() {
        if (m_gpaDefinitions == null) {
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(m_fieldGpdSchoolIndicator.getJavaName(), BooleanAsStringConverter.TRUE);
            BeanQuery query = new BeanQuery(GradePointAverageDefinition.class, criteria);
            m_gpaDefinitions = getFLData().getBroker().getCollectionByQuery(query);

            if (m_gpaDefinitions.size() != 1) {
                throw new IllegalStateException(
                        "Exactly one grade point average definition must be selected as state definition");
            }
            GradePointAverageDefinition gpaDefinition = m_gpaDefinitions.iterator().next();
            if (gpaDefinition.getGpaDataFieldConfig() != null) {
                m_fieldGpa = getFLData().getDataDictionary()
                        .findDataDictionaryField(gpaDefinition.getGpaDataFieldConfig().getDataFieldOid());
            }
        }
        return m_fieldGpa;
    }

    /**
     * Finds the student's grade level on a particular date.
     *
     * @param student the student
     * @return the student info
     */
    public StudentInfo getStudentInfo(SisStudent student) {
        StudentInfo studentInfo = m_studentInfoMap.get(student.getOid());
        if (studentInfo == null) {
            studentInfo = new StudentInfo(student);
            m_studentInfoMap.put(student.getOid(), studentInfo);
        }
        return studentInfo;
    }

    /**
     * Gets the student program.
     *
     * @param oid the oid
     * @param ddxId the ddx id
     * @param surveyPeriod the survey period
     * @return the student program
     */
    public StudentProgramParticipation getStudentProgram(String oid, String ddxId, SurveyPeriod surveyPeriod) {
        StudentProgramParticipation value = null;
        StudentProgramDataset dataset = getStudentProgramDataset(ddxId, surveyPeriod.getStartDate(),
                surveyPeriod.getEndDate());
        List<StudentProgramParticipation> programs = dataset.getPrograms(oid);
        if (programs != null && !programs.isEmpty()) {
            value = programs.iterator().next();
        }
        return value;
    }


    /**
     * Gets the student programs.
     *
     * @param oid the oid
     * @param ddxId the ddx id
     * @param surveyPeriod the survey period
     * @return the student programs
     */
    public List<StudentProgramParticipation> getStudentPrograms(String oid,
                                                                String ddxId,
                                                                SurveyPeriod surveyPeriod) {
        StudentProgramDataset dataset = getStudentProgramDataset(ddxId, surveyPeriod.getStartDate(),
                surveyPeriod.getEndDate());
        return dataset.getPrograms(oid);
    }

    /**
     * Gets the student program dataset.
     *
     * @param ddxId the ddx id
     * @param surveyPeriod the survey period
     * @return the student program dataset
     */
    public StudentProgramDataset getStudentProgramDataset(String ddxId, SurveyPeriod surveyPeriod) {
        StudentProgramDataset dataset = getStudentProgramDataset(ddxId, surveyPeriod.getStartDate(),
                surveyPeriod.getEndDate());
        return dataset;
    }

    /**
     * Gets the student program dataset.
     *
     * @param ddxId the ddx id
     * @param startDate the start date
     * @param endDate the end date
     * @return the student program dataset
     */
    public StudentProgramDataset getStudentProgramDataset(String ddxId, PlainDate startDate, PlainDate endDate) {
        StudentProgramDataset dataset = new StudentProgramDataset(getData(), ddxId, startDate, endDate);
        StudentProgramDataset existing = m_studentProgramDatasets.get(dataset);
        if (existing == null) {
            m_studentProgramDatasets.put(dataset, dataset);
        }
        return existing == null ? dataset : existing;
    }

    /**
     * Gets the student race map.
     *
     * @return the student race map
     */
    public Map<String, Collection<Race>> getStudentRaceMap() {
        if (m_raceMap == null) {
            SubQuery studentSubQuery =
                    new SubQuery(SisStudent.class, SisStudent.COL_PERSON_OID, getStudentCriteria());
            X2Criteria raceCriteria = new X2Criteria();
            raceCriteria.addIn(Race.COL_PERSON_OID, studentSubQuery);

            QueryByCriteria raceQuery = new QueryByCriteria(Race.class, raceCriteria);
            m_raceMap =
                    getData().getBroker().getGroupedCollectionByQuery(raceQuery, Race.COL_PERSON_OID,
                            INITIAL_MAP_SIZE);
        }
        return m_raceMap;
    }

    /**
     * Checks if is student eligible.
     *
     * @param student the student
     * @return true, if is student eligible
     * @throws X2BaseException the x 2 base exception
     */
    public boolean isStudentEligible(SisStudent student) throws X2BaseException {
        String surveyPeriod = getFLData().getSurveyPeriodCode();
        if (FLStateReportData.SURVEY_PERIOD_1.equals(surveyPeriod)
                || FLStateReportData.SURVEY_PERIOD_4.equals(surveyPeriod)) {
            return isEligibleForPeriod_1_4(student);
        } else if (FLStateReportData.SURVEY_PERIOD_2.equals(surveyPeriod)
                || FLStateReportData.SURVEY_PERIOD_3.equals(surveyPeriod)) {
            return isEligibleForPeriod_2_3(student);
        } else if (FLStateReportData.SURVEY_PERIOD_5.equals(surveyPeriod)) {
            return isEligibleForPeriod_5(student);
        } else if (FLStateReportData.SURVEY_PERIOD_6.equals(surveyPeriod)) {
            return isEligibleForPeriod_6(student);
        } else if (FLStateReportData.SURVEY_PERIOD_8.equals(surveyPeriod)) {
            return isEligibleForPeriod_8(student);
        } else if (FLStateReportData.SURVEY_PERIOD_9.equals(surveyPeriod)) {
            return isEligibleForPeriod_9(student);
        }
        return true;
    }

    /**
     * Gets the assessments.
     *
     * @param student the student
     * @return the assessments
     */
    public List<StudentAssessment> getAssessments(SisStudent student) {
        loadAssessments();
        Collection<StudentAssessment> assessments = m_assessmentMap.get(student.getOid());

        List list = new ArrayList();
        if (assessments != null && !assessments.isEmpty()) {
            list = new ArrayList(assessments);
        }

        return list;
    }

    /**
     * Sets the wdis mode.
     *
     * @param mode void
     */
    public void setWdisMode(String mode) {
        if (WDIS_MODE_ADULT_GENERAL_EDUCATION.equals(mode) || WDIS_MODE_POST_SECONDARY_CTE.equals(mode)
                || WDIS_MODE_BOTH.equals(mode)) {
            m_wdisMode = mode;
        }
    }

    /**
     * Gets the FL data.
     *
     * @return the FL data
     */
    FLStateReportData getFLData() {
        return (FLStateReportData) super.getData();
    }

    /**
     * return true when the student is enrolled and scheduled for at least on class during the
     * range
     * of the survey period.
     *
     * @param student the student
     * @return true, if successful
     */
    boolean hasServices(SisStudent student) {
        boolean res = false;
        List<StudentEnrollmentSpan> espans = getStudentEnrollmentSpans(student, true);
        if (espans != null && espans.size() > 0) {
            List<StudentScheduleSpan> sspans = getStudentScheduleSpans(student);
            if (sspans != null && sspans.size() > 0) {
                PlainDate startDate = getFLData().getSurveyPeriod().getStartDate();
                PlainDate endDate = getFLData().getSurveyPeriod().getEndDate();
                for (StudentScheduleSpan span : sspans) {
                    if (span.getEntryDate() != null && !span.getEntryDate().after(endDate) &&
                            span.getEntryDate() != null && !span.getExitDate().before(startDate)) {
                        res = true;
                        break;
                    }
                }
            }
        }
        return res;
    }

    /**
     * Checks if is member on entry date.
     *
     * @return true, if is member on entry date
     */
    boolean isMemberOnEntryDate() {
        return Boolean.valueOf(PreferenceManager.getPreferenceValue(getFLData().getOrganization(),
                SisPreferenceConstants.STUDENT_MEMBER_ON_ENTRY_DATE)).booleanValue();
    }

    /**
     * Checks if is member on withdrawal date.
     *
     * @return true, if is member on withdrawal date
     */
    boolean isMemberOnWithdrawalDate() {
        return Boolean.valueOf(PreferenceManager.getPreferenceValue(getFLData().getOrganization(),
                SisPreferenceConstants.STUDENT_MEMBER_ON_WITHDRAWAL_DATE)).booleanValue();
    }

    /**
     * Adjust criteria.
     *
     * @param criteria X2Criteria
     * @param pathToMst String
     * @return X 2 criteria
     */
    private X2Criteria getAdjustedCriteria(X2Criteria criteria, String pathToMst) {
        if (!StringUtils.isEmpty(m_wdisMode) && !WDIS_MODE_BOTH.equals(m_wdisMode)) {
            criteria.addEqualTo(pathToMst + ModelProperty.PATH_DELIMITER + m_fieldWdisAgeIndicator, m_wdisMode);
        } else if (WDIS_MODE_BOTH.equals(m_wdisMode)) {
            criteria.addIn(pathToMst + ModelProperty.PATH_DELIMITER + m_fieldWdisAgeIndicator,
                    Arrays.asList(WDIS_MODE_ADULT_GENERAL_EDUCATION, WDIS_MODE_POST_SECONDARY_CTE));
        }

        return criteria;
    }

    /**
     * Inits the immigrants.
     */
    private void initImmigrants() {
        if (m_immigrantStudentOids == null) {
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(StudentEnrollment.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_SCHOOL_ID,
                    IMMIGRATION_SCHOOL_ID);
            criteria.addNotNull(
                    StudentEnrollment.REL_STUDENT + PATH_DELIMITER + SisStudent.REL_PERSON + PATH_DELIMITER
                            + SisPerson.COL_DOB);
            QueryByCriteria query = new QueryByCriteria(StudentEnrollment.class, criteria);
            m_immigrantStudentOids = new ArrayList<>();
            QueryIterator iterator = getData().getBroker().getIteratorByQuery(query);
            PlainDate begin = (PlainDate) getSelectionProperty(PROPERTY_BEGIN_DATE);
            while (iterator.hasNext()) {
                StudentEnrollment bean = (StudentEnrollment) iterator.next();
                if (Person.getAgeAsOfDate(bean.getStudent().getPerson().getDob(), begin) <= IMMIGRATION_AGE) {
                    m_immigrantStudentOids.add(bean.getStudentOid());
                }
            }
            iterator.close();
        }
    }

    /**
     * Checks if is eligible for period 1 4.
     *
     * @param student the student
     * @return true, if is eligible for period 1 4
     */
    private boolean isEligibleForPeriod_1_4(SisStudent student) {
        return hasServices(student) || hasProgram(student, STD_PROGRAM_SES)
                || m_studentConductDataset.hasConductAction(student)
                || hasAssessment(student) || hasFederalIndicator(student);
    }

    /**
     * Checks if is eligible for period 2 3.
     *
     * @param student the student
     * @return true, if is eligible for period 2 3
     * @throws X2BaseException the x 2 base exception
     */
    private boolean isEligibleForPeriod_2_3(SisStudent student) throws X2BaseException {
        return isEligibleForPeriod_1_4(student) || isImmigrant(student);
    }

    /**
     * Checks if is eligible for period 5.
     *
     * @param student the student
     * @return true, if is eligible for period 5
     */
    private boolean isEligibleForPeriod_5(SisStudent student) {
        // TODO: should be re-implemented later, see the S-32183 backlog item
        List<StudentEnrollment> senrollments = this.getStudentEnrollments(student);
        return senrollments != null && !senrollments.isEmpty();
    }

    /**
     * Checks if is eligible for period 6.
     *
     * @param student the student
     * @return true, if is eligible for period 6
     */
    private boolean isEligibleForPeriod_6(SisStudent student) {
        return hasGradeLevel(student, GRADE_LEVELS_SURVEY_6);
    }

    /**
     * Checks if is eligible for period 8.
     *
     * @param student the student
     * @return true, if is eligible for period 8
     * @throws X2BaseException the x 2 base exception
     */
    private boolean isEligibleForPeriod_8(SisStudent student) throws X2BaseException {
        boolean res = hasGradeLevel(student, GRADE_LEVEL_KG);
        if (!res && hasGradeLevel(student, GRADE_LEVELS_SURVEY_8)) {
            List<StudentEnrollment> enrollments = getStudentEnrollments(student);
            if (enrollments != null && !enrollments.isEmpty()) {
                ArrayList<String> schoolOids = new ArrayList<>(1);
                for (StudentEnrollment se : enrollments) {
                    if (!schoolOids.contains(se.getSchoolOid())) {
                        Boolean prmnSchool = (Boolean) getFLData().getFieldValue(se.getSchool(), m_fieldSchoolPmrn);
                        if (prmnSchool != null && prmnSchool.booleanValue()) {
                            res = true;
                            break;
                        }
                        schoolOids.add(se.getSchoolOid());
                    }
                }
            }
        }
        return res;
    }

    /**
     * Checks if is eligible for period 9.
     *
     * @param student the student
     * @return true, if is eligible for period 9
     */
    private boolean isEligibleForPeriod_9(SisStudent student) {
        return hasProgram(student, STD_PROGRAM_NEGLECT) || hasProgram(student, STD_PROGRAM_SES);
    }

    /**
     * Checks if is immigrant.
     *
     * @param student the student
     * @return true, if is immigrant
     * @throws X2BaseException the x 2 base exception
     */
    private boolean isImmigrant(SisStudent student) throws X2BaseException {
        FLStateReportData data = getFLData();

        DataDictionaryField fieldUSEntryDate =
                data.translateAliasToDictionaryField(ALIAS_US_ENTRY_DATE, true);
        Object m_USEntryDate = data.getFieldValue(student, fieldUSEntryDate);

        return m_USEntryDate != null;
    }

    /**
     * Checks for assessment.
     *
     * @param student the student
     * @return true, if successful
     */
    private boolean hasAssessment(SisStudent student) {
        loadAssessments();
        Collection<StudentAssessment> assessments = m_assessmentMap.get(student.getOid());
        return assessments != null && !assessments.isEmpty();
    }

    /**
     * Checks for federal indicator.
     *
     * @param student the student
     * @return true, if successful
     */
    private boolean hasFederalIndicator(SisStudent student) {
        ////////////////////////////////////////////////////
        // TODO: need specification
        ////////////////////////////////////////////////////
        return true;
    }

    /**
     * Checks for grade level.
     *
     * @param student the student
     * @param gradeLevel the grade level
     * @return true, if successful
     */
    private boolean hasGradeLevel(SisStudent student, String gradeLevel) {
        return hasGradeLevel(student, Arrays.asList(gradeLevel));
    }

    /**
     * Checks for grade level.
     *
     * @param student the student
     * @param gradeLevels the grade levels
     * @return true, if successful
     */
    private boolean hasGradeLevel(SisStudent student, List<String> gradeLevels) {
        StudentInfo info = getStudentInfo(student);
        String startGradeLevel = info.getGradeLevel(getFLData().getSurveyPeriod().getStartDate());
        boolean res = gradeLevels.contains(startGradeLevel);
        if (!res) {
            String endGradeLevel = info.getGradeLevel(getFLData().getSurveyPeriod().getEndDate());
            res = gradeLevels.contains(endGradeLevel);
        }
        return res;
    }

    /**
     * Checks for program.
     *
     * @param student the student
     * @param ddxId the ddx id
     * @return true, if successful
     */
    private boolean hasProgram(SisStudent student, String ddxId) {
        Collection<StudentProgramParticipation> programs = getStudentPrograms(student.getOid(), ddxId,
                getFLData().getSurveyPeriod());
        return programs != null && !programs.isEmpty();
    }

    /**
     * Load assessments.
     */
    private void loadAssessments() {
        if (m_assessmentMap == null) {
            X2Criteria stdCriteria = getStudentCriteria();
            SubQuery stdSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, stdCriteria);

            X2Criteria criteria = new X2Criteria();
            criteria.addIn(StudentAssessment.COL_STUDENT_OID, stdSubQuery);

            PlainDate begin = (PlainDate) getSelectionProperty(PROPERTY_BEGIN_DATE);
            PlainDate end = (PlainDate) getSelectionProperty(PROPERTY_END_DATE);
            if (begin != null && end != null) {
                criteria.addBetween(StudentAssessment.COL_DATE, begin, end);
            }

            QueryByCriteria query = new QueryByCriteria(StudentAssessment.class, criteria);
            m_assessmentMap = getData().getBroker().getGroupedCollectionByQuery(query,
                    StudentAssessment.COL_STUDENT_OID, INITIAL_MAP_SIZE);
        }
    }

    /**
     * Load grade scales.
     */
    private void loadGradeScales() {
        /*
         * map grade scales by transcript definition Oid for easier retrieval.
         */
        m_gradeScales = new HashMap<String, GradeScale>();
        m_gradesManager = new GradesManager(getData().getBroker());
        X2Criteria criteria = new X2Criteria();

        // Find the column definition that points to TRN_FINAL_GRADE
        criteria.addEqualTo(TranscriptColumnDefinition.COL_COLUMN_TYPE_CODE,
                Integer.valueOf(TranscriptColumnDefinition.COLUMN_TYPE_FINAL));
        QueryByCriteria query = new QueryByCriteria(TranscriptColumnDefinition.class, criteria);
        QueryIterator iterator = getData().getBroker().getIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                TranscriptColumnDefinition tcd = (TranscriptColumnDefinition) iterator.next();
                m_gradeScales.put(tcd.getTranscriptDefinitionOid(), tcd.getGradeScale());
            }
        } finally {
            iterator.close();
        }
    }
}
