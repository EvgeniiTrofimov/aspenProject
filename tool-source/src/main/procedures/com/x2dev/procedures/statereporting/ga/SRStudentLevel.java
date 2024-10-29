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
package com.x2dev.procedures.statereporting.ga;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.ColumnQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.*;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryUtils;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.business.CalendarManager;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.sis.tools.stateexports.StudentScheduleSpan;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * This class implements the data export for Student Record Student Level export.
 *
 * @author X2 Development Corporation
 */
public class SRStudentLevel extends StateReportData {
    /**
     * Entity class for Student Record Student Level export.
     *
     * @author X2 Development Corporation
     */
    public static class SRStudentLevelEntity extends StateReportEntity {
        private SRStudentLevel m_studentLevelData = null;
        private List<SchoolYear> m_schools = null;
        protected Boolean m_hasCipCourse = Boolean.FALSE;
        private String ALIAS_DOE_CIP_COURSE = "DOE CIP Course";

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public SRStudentLevelEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Return an identifying name for this entity for reporting purposes.
         *
         * @return String
         * @see com.x2dev.sis.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            Student student = (Student) getBean();

            String name = student.getNameView() +
                    " [Local ID: " + student.getLocalId() +
                    ", GTID: " + student.getFieldValueByAlias("GTID") +
                    "]";
            return name;
        }

        /**
         * Returns the current reporting school for this student.
         *
         * @return School
         */
        public School getSchool() {
            School school = null;
            if (getCurrentRow() >= 0 && (null != m_schools) && getCurrentRow() < m_schools.size()) {
                school = m_schools.get(getCurrentRow()).getSchool();
            }
            return school;
        }

        /**
         * Gets the yog.
         *
         * @return int
         */
        public int getYog() {
            int value = -1;
            if (getCurrentRow() >= 0 && (null != m_schools) && getCurrentRow() < m_schools.size()) {
                value = m_schools.get(getCurrentRow()).getYog();
            }
            return value;
        }

        /**
         * Initialize.
         * Get a list of all schools the student was active in during the year.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see com.x2dev.sis.tools.stateexports.StateReportEntity#intitialize(com.x2dev.sis.tools.
         *      stateexports.StateReportData, com.x2dev.sis.model.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            m_studentLevelData = (SRStudentLevel) data;
            SisStudent student = (SisStudent) bean;
            String excludeStudent = "";
            if (null != student) {
                excludeStudent = (String) student.getFieldValueByAlias(ALIAS_DOE_EXCLUDE_STD);
            }

            if (!StringUtils.isEmpty(excludeStudent) && excludeStudent.equals(BOOLEAN_YES)) {
                setRowCount(0);
            } else {
                // Iterate through current Schedule Spans to determine if any span (section) was a
                // CIP Course.
                List<StudentScheduleSpan> scheduleSpans = m_studentLevelData.m_helper.getStudentScheduleSpans(student);

                if (scheduleSpans != null && !scheduleSpans.isEmpty()) {
                    for (StudentScheduleSpan scheduleSpan : scheduleSpans) {
                        PlainDate exitDate = scheduleSpan.getExitDate();
                        PlainDate yearStartDate =
                                m_studentLevelData.getOrganization().getCurrentContext().getStartDate();
                        String hasCipCourse = BooleanAsStringConverter.FALSE;

                        MasterSchedule section = scheduleSpan.getSection();
                        if (section != null) {
                            SchoolCourse schoolCourse = section.getSchoolCourse();
                            if (schoolCourse != null) {
                                Course course = schoolCourse.getCourse();
                                if (course != null) {
                                    hasCipCourse = (String) course.getFieldValueByAlias(ALIAS_DOE_CIP_COURSE);
                                }
                            }
                        }

                        if ((exitDate == null || !exitDate.before(yearStartDate)) &&
                                BooleanAsStringConverter.TRUE.equals(hasCipCourse)) {
                            m_hasCipCourse = Boolean.TRUE;
                            break;
                        }
                    }
                }

                m_schools = new LinkedList<SchoolYear>();
                Set<String> schoolCodes = new HashSet<String>();

                Collection<StudentEnrollment> enrollments =
                        m_studentLevelData.m_studentEnrollements.get(student.getOid());

                // If the student is active, add current school.
                if (m_studentLevelData.m_activeCode.equals(student.getEnrollmentStatus())) {
                    m_schools.add(new SchoolYear(student.getSchool(), student.getYog()));
                    String schoolCode = null;
                    School studentSchool = student.getSchool();
                    schoolCode = (String) student.getFieldValueByBeanPath(m_studentLevelData.m_overrideSchoolCodeField);
                    if (StringUtils.isEmpty(schoolCode)) {
                        if (studentSchool != null) {
                            schoolCode = (String) studentSchool
                                    .getFieldValueByBeanPath(m_studentLevelData.m_schoolCodeField);
                        }
                    }
                    schoolCodes.add(schoolCode);
                }

                // Any enrollment activity, add the school. Start with latest enrollments, Latest
                // record for each school
                if (enrollments != null) {
                    for (StudentEnrollment enrollment : enrollments) {
                        String code = null;
                        School school = enrollment.getSchool();
                        code = (String) student.getFieldValueByBeanPath(m_studentLevelData.m_overrideSchoolCodeField);
                        if (StringUtils.isEmpty(code)) {
                            if (school != null) {
                                code = (String) school.getFieldValueByBeanPath(m_studentLevelData.m_schoolCodeField);
                            }
                        }
                        if (schoolCodes.add(code)) {
                            m_schools.add(new SchoolYear(enrollment.getSchool(), enrollment.getYog()));
                        }
                    }
                }

                setRowCount(schoolCodes.size());
            }
        }
    }


    /**
     * The Class SchoolYear.
     *
     * @author Follett Software Company
     */
    protected static class SchoolYear {
        private SisSchool m_school;
        private int m_yog;

        /**
         * Instantiates a new school year.
         *
         * @param school SisSchool
         * @param yog int
         */
        protected SchoolYear(SisSchool school, int yog) {
            m_school = school;
            m_yog = yog;
        }

        /**
         * Gets the school.
         *
         * @return Sis school
         */
        public SisSchool getSchool() {
            return m_school;
        }

        /**
         * Gets the yog.
         *
         * @return int
         */
        public int getYog() {
            return m_yog;
        }
    }

    /**
     * Aliases
     */
    private static final String ALIAS_ASM_SCORE = "all-asm-score";
    private static final String ALIAS_DOE_EXCLUDE_STD = "DOE EXCLUDE STD";
    private static final String ALIAS_DOE_DIPLOMA_TYPE = "DOE Diploma Type";
    private static final String ALIAS_DOE_PRIMARY_EXCEPTIONALITY = "DOE Primary Exceptionality";
    private static final String ALIAS_DOE_RETAINED = "DOE Retained";
    private static final String ALIAS_DOE_TITLE_3 = "DOE Title III Served";
    private static final String ALIAS_DOE_ELL = "DOE ELL";
    private static final String ALIAS_PGM_GE = "all-pgm-GiftedEligibility";
    private static final String ALIAS_PGM_GR = "all-pgm-GiftedReferral";
    private static final String ALIAS_STD_DDX_CC_SKL_CODE = "DOE CC School Code";
    private static final String ALIAS_STD_DDX_CC_YEAR = "DOE CC School Year";
    private static final String ALIAS_STD_GE = "DOE Gift Elig";
    private static final String ALIAS_STD_GR = "DOE Gift Referral";
    private static final String ALIAS_STD_PAR_LANG = "DOE Parent Communication Lang";
    private static final String ALIAS_STD_PRIM_LANG = "DOE Primary Lang";

    /**
     * Extended dictionaries
     */
    private static final String DDX_ID_SLDS = "STD-SLDS-CC";
    private static final String DDX_ID_STD_504_PLAN = "STD-504-PLAN";

    /**
     * Constants for reporting information.
     */
    protected final String FIELD_BIRTH_PLACE = "Place of Birth";
    protected final String FIELD_ELL = "ELL";
    protected final String FIELD_ESOL = "ESOL";
    protected final String FIELD_DIPLOMA_TYPE = "Diploma Type";
    protected final String FIELD_GIFT_ELIG = "Gifted Eligibility";
    protected final String FIELD_GRADE = "Grade level";
    protected final String FIELD_NON_ESOL = "Non-ESOL";
    protected final String FIELD_RES_ENV = "Residential Env";
    protected final String FIELD_PREK = "Pre-K Program Code";
    protected final String FIELD_PRIM_EXP = "Primary Exceptionali";
    protected final String FIELD_SKL_CODE = "School Code";

    private static final String ASD_ID_ASVAB = "ASVAB";
    private static final String DOE_OVERRIDE_SCHOOL = "DOE Override School Code";
    protected static final String ERROR_MESSAGE_ASSESS_DEF_NOT_LOADED =
            " Assessment Definition is not loaded in the Data Dictionary.";
    protected static final String ERROR_TYPE_WARNING = "Warning";
    private static final String ILLEGAL_NAME_CHARACTERS = "[_\\W&&[^-'.\\s]]";
    private static final String NON_ESOL_ALIAS = "DOE REASON FOR NON-ESOL";
    private static final String PARAM_QUERY_BY = "queryBy";
    private static final String PARAM_QUERY_STRING = "queryString";
    private static final String PARAM_RETAINED_SNAPSHOT = "retained";
    private static final String PARAM_SORT = "sort";
    private static final String SCHOOL_CODE_ALIAS = "DOE School";

    private static final String BOOLEAN_YES = "1";
    private static final String CONSTANT_NO = "N";
    private static final String CONSTANT_YES = "Y";
    private static final String ENR_TYPE_GRADUATE = "Graduate";
    private static final String CODE_GE_DEFAULT = "9";
    private static final String CODE_GR_DEFAULT = "9";
    private static final String PROGRAM_CODE_GIFTED = "GIFTED";
    private static final String SECTION_504_CODE_ACTIVE = "Active";

    /**
     * Local variables for reporting information.
     */
    protected DataDictionary m_asmDictASVAB;
    protected String m_activeCode;
    protected CalendarManager m_calMngr;
    protected Map<String, ReferenceCode> m_disabilityCodes;
    protected String m_doeDiplomaType;
    protected String m_doeEllField;
    protected String m_fieldAsmScore;
    protected String m_fieldStdTitle3;
    protected String m_fieldStdDdxSklCode;
    protected String m_fieldStdDdxSklCtx;
    protected String m_fieldStdGiftElig;
    protected String m_fieldStdGiftReferral;
    protected String m_fieldStdParLang;
    protected String m_fieldStdPrimLang;
    protected StudentHistoryHelper m_helper;
    protected Pattern m_illegalNameCharacters = Pattern.compile(ILLEGAL_NAME_CHARACTERS);
    protected String m_nonEsolField;
    protected String m_overrideSchoolCodeField;
    protected Map<String, ReferenceCode> m_raceCodes;
    protected Map<String, Collection<Race>> m_raceCodeMap;
    protected PlainDate m_reportDate;
    protected Collection<String> m_retained;
    protected String m_schoolCode;
    protected String m_schoolCodeField;
    protected DataDictionary m_std504PlanDDX;
    protected Map<String, Collection<StudentAssessment>> m_stdAsmASVABMap = new HashMap<>();
    protected Map<String, Collection<X2BaseBean>> m_stdSLDSMap;
    protected Map<String, Collection<StudentEnrollment>> m_studentEnrollements;
    protected Map<String, Collection<StudentProgramParticipation>> m_studentProgramParticipation;
    protected String eightGrade = "08";

    /**
     * The Class Retrieve504.
     *
     * @author Follett Software Company
     */
    protected class Retrieve504 implements FieldRetriever {
        private static final String CALC_ID = "GA-SR-504";
        private Set<String> m_activeSEPByStdMap = new HashSet();

        /**
         * Instantiates a new retrieve 504.
         *
         * @param studentCriteria X2Criteria
         */
        public Retrieve504(X2Criteria studentCriteria) {
            X2Criteria activeSEP = new X2Criteria();
            activeSEP.addEqualTo(StudentEdPlan.COL_EXTENDED_DATA_DICTIONARY_OID,
                    m_std504PlanDDX.getExtendedDictionaryOid());
            activeSEP.addEqualTo(StudentEdPlan.COL_STATUS_CODE,
                    Integer.valueOf(StudentEdPlan.StatusCode.ACTIVE.ordinal()));
            SubQuery studentSubQuery = new SubQuery(Student.class, X2BaseBean.COL_OID, studentCriteria);
            activeSEP.addIn(StudentEdPlan.COL_STUDENT_OID, studentSubQuery);

            String[] columns = new String[] {StudentEdPlan.COL_STUDENT_OID};
            ColumnQuery query = new ColumnQuery(StudentEdPlan.class, columns, activeSEP);
            try (QueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query)) {
                while (iterator.hasNext()) {
                    Object[] row = (Object[]) iterator.next();
                    m_activeSEPByStdMap.add((String) row[0]);
                }
            }
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String returnValue = CONSTANT_NO;
            SisStudent student = (SisStudent) entity.getBean();
            if (m_activeSEPByStdMap.contains(student.getOid())) {
                returnValue = CONSTANT_YES;
            }
            return returnValue;
        }
    }

    /**
     * The Class Retrieve504Datail.
     *
     * @author Follett Software Company
     */
    protected class Retrieve504Detail implements FieldRetriever {
        private static final String CALC_ID = "GA-SR-504-DETAIL";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();
            if (SECTION_504_CODE_ACTIVE.equals(student.getSection504StatusCode()) && m_disabilityCodes != null) {
                String disabilityStateCodeParam = (String) field.getParameter();
                Collection<IepDisability> studentDisability = student.getIepDisability();
                if (!StringUtils.isEmpty(disabilityStateCodeParam) && studentDisability != null) {
                    for (IepDisability disability : studentDisability) {
                        ReferenceCode disabilityCode = m_disabilityCodes.get(disability.getDisabilityCode());
                        if (disabilityCode != null && disabilityStateCodeParam.equals(disabilityCode.getStateCode())) {
                            return CONSTANT_YES;
                        }
                    }
                }
            }
            return CONSTANT_NO;
        }
    }

    /**
     * The Class RetrieveASVAB.
     */
    protected class RetrieveASVAB implements FieldRetriever {
        private static final String CALC_ID = "GA-SR-ASVAB";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String returnValue = "";
            SisStudent std = (SisStudent) entity.getBean();
            if (m_stdAsmASVABMap.containsKey(std.getOid())) {
                int scoreToReturnInt = 0;
                Collection<StudentAssessment> asvabAsms = m_stdAsmASVABMap.get(std.getOid());
                for (StudentAssessment asvab : asvabAsms) {
                    Object score = asvab.getFieldValueByBeanPath(m_fieldAsmScore);
                    if (score instanceof Number) {
                        int scoreToCompare = ((Integer) score).intValue();
                        if (scoreToReturnInt < scoreToCompare) {
                            scoreToReturnInt = scoreToCompare;
                        }
                    } else if (score instanceof String) {
                        String scoreSrt = (String) score;
                        if (scoreSrt.matches("\\d+")) {
                            int scoreToCompare = Integer.valueOf(scoreSrt).intValue();
                            if (scoreToReturnInt < scoreToCompare) {
                                scoreToReturnInt = scoreToCompare;
                            }
                        }
                    }
                    if (scoreToReturnInt > 0) {
                        returnValue = String.valueOf(scoreToReturnInt);
                    }
                }
            }
            return returnValue;
        }
    }

    /**
     * The Class RetrieveBiLitLanguage.
     */
    protected class RetrieveBiLitLanguage implements FieldRetriever {
        private static final String CALC_ID = "GA-SR-BI-LIT-LANG";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String returnValue = null;
            String biLitLang = (String) getProperty(entity.getBean(), field.getBeanPath());
            if (!StringUtils.isEmpty(biLitLang)) {
                biLitLang = lookupStateValue(entity.getBean().getClass(), field.getBeanPath(), biLitLang);
                if (!StringUtils.isEmpty(biLitLang)) {
                    int maxLength = field.getMaxLength();
                    returnValue = StringUtils.padLeft(biLitLang, maxLength, '0');
                }
            }
            return returnValue;
        }
    }

    /**
     * Retrieve student CTAE values by aliases.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveCTAEValues implements FieldRetriever {
        private static final String CALC_ID = "GA-SR-STD-CTAE";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String ctaeValueState = null;
            String diplomaType = entity.getFieldValue(FIELD_DIPLOMA_TYPE);
            if (!StringUtils.isEmpty(diplomaType)) {
                String beanPath = data.translateAliasToJavaName((String) field.getParameter(), false);
                if (!StringUtils.isEmpty(beanPath)) {
                    String ctaeValue = (String) entity.getBean().getFieldValueByBeanPath(beanPath);
                    if (!StringUtils.isEmpty(ctaeValue)) {
                        ctaeValueState = lookupStateValue(SisStudent.class, beanPath, ctaeValue);
                    }
                }
            }
            return ctaeValueState;
        }
    }

    /**
     * Retrieve the Doplima Type.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveDiplomaType implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String value = null;
            SisStudent student = (SisStudent) entity.getBean();
            String stdSchoolId = "";
            if (null != student && null != student.getSchool()) {
                stdSchoolId = student.getSchool().getSchoolId();
            }
            String entitySchoolId = "";
            School school = ((SRStudentLevelEntity) entity).getSchool();
            if (null != school) {
                entitySchoolId = school.getSchoolId();
            }
            if (entitySchoolId != null && entitySchoolId.equals(stdSchoolId)
                    && ENR_TYPE_GRADUATE.equals(student.getEnrollmentStatus())) {
                String diplomaType = (String) student.getFieldValueByBeanPath(m_doeDiplomaType);
                value = lookupReferenceCodeByAlias(ALIAS_DOE_DIPLOMA_TYPE, diplomaType,
                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
            }
            return value;
        }
    }


    /**
     * Return the student ELL status on report date.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveELL implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field) {
            Object value = null;
            SisStudent student = (SisStudent) entity.getBean();
            String ellCode = (String) student.getFieldValueByBeanPath(m_doeEllField);
            if (!StringUtils.isEmpty(ellCode)) {
                String ellCodeState = lookupStateValue(SisStudent.class, m_doeEllField, ellCode);
                if (!StringUtils.isEmpty(ellCodeState) && !"M".equals(ellCodeState)) {
                    value = ellCodeState;
                }
            }
            return value;
        }
    }


    /**
     * Return the student Gifted Eligibility State Code.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveGE implements FieldRetriever {
        private static final String CALC_ID = "GA-SR-GE";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field) {
            String value = null;
            SisStudent student = (SisStudent) entity.getBean();
            StudentProgramParticipation programGifted = null;
            if (m_studentProgramParticipation != null &&
                    m_studentProgramParticipation.containsKey(student.getOid())) {
                Collection<StudentProgramParticipation> studentProgramParticipation =
                        m_studentProgramParticipation.get(student.getOid());
                for (StudentProgramParticipation program : studentProgramParticipation) {
                    String programCode = program.getProgramCode();
                    if (!StringUtils.isEmpty(programCode)) {
                        programCode = lookupStateValue(StudentProgramParticipation.class,
                                StudentProgramParticipation.COL_PROGRAM_CODE, programCode);
                        if (PROGRAM_CODE_GIFTED.equals(programCode)) {
                            programGifted = program;
                            String giftElig = (String) programGifted.getFieldValueByAlias(ALIAS_PGM_GE);
                            if (!StringUtils.isEmpty(giftElig)) {
                                value = lookupReferenceCodeByAlias(ALIAS_PGM_GE, giftElig,
                                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                            }
                        }
                    }
                }
            }
            if (programGifted == null) {
                String giftElig = (String) student.getFieldValueByBeanPath(m_fieldStdGiftElig);
                if (!StringUtils.isEmpty(giftElig)) {
                    value = data.lookupStateValue(SisStudent.class, m_fieldStdGiftElig, giftElig);
                }
            }
            return StringUtils.isEmpty(value) ? CODE_GE_DEFAULT : value;
        }
    }


    /**
     * Return the student Gifted Referral State Code.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveGR implements FieldRetriever {
        private static final String CALC_ID = "GA-SR-GR";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field) {
            String value = null;
            SisStudent student = (SisStudent) entity.getBean();
            StudentProgramParticipation programGifted = null;
            if (m_studentProgramParticipation != null &&
                    m_studentProgramParticipation.containsKey(student.getOid())) {
                Collection<StudentProgramParticipation> studentProgramParticipation =
                        m_studentProgramParticipation.get(student.getOid());
                for (StudentProgramParticipation program : studentProgramParticipation) {
                    String programCode = program.getProgramCode();
                    if (!StringUtils.isEmpty(programCode)) {
                        programCode = lookupStateValue(StudentProgramParticipation.class,
                                StudentProgramParticipation.COL_PROGRAM_CODE, programCode);
                        if (PROGRAM_CODE_GIFTED.equals(programCode)) {
                            programGifted = program;
                            String giftReferal = (String) programGifted.getFieldValueByAlias(ALIAS_PGM_GR);
                            if (!StringUtils.isEmpty(giftReferal)) {
                                value = lookupReferenceCodeByAlias(ALIAS_PGM_GR, giftReferal,
                                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                            }
                        }
                    }
                }
            }
            if (programGifted == null) {
                String giftReferal = (String) student.getFieldValueByBeanPath(m_fieldStdGiftReferral);
                if (!StringUtils.isEmpty(giftReferal)) {
                    value = data.lookupStateValue(SisStudent.class, m_fieldStdGiftReferral, giftReferal);
                }
            }
            return StringUtils.isEmpty(value) ? CODE_GR_DEFAULT : value;
        }
    }

    /**
     * The Class RetrieveGradeLevel.
     *
     * @author Follett Software Company
     */
    protected class RetrieveGradeLevel implements FieldRetriever {
        private static final String CAL_ID = "GA-SR-GRADE-LVL";
        private Map<Integer, String> m_mapGradeLevel = new HashMap();
        private int m_maxGradeLevel;
        private Map<String, ReferenceCode> m_referenceGradeCodeMap;
        private TreeMap m_sortedGradeLevels;

        /**
         * Instantiates a new retrieve grade level.
         */
        protected RetrieveGradeLevel() {
            DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
            ModelProperty prop =
                    new ModelProperty(SisStudent.class, SisStudent.COL_GRADE_LEVEL, getBroker().getPersistenceKey());
            DataDictionaryField field = dictionary.findDataDictionaryField(prop.getFieldId());
            ReferenceTable referenceTable = field.getReferenceTable();
            m_referenceGradeCodeMap = referenceTable.getCodeMap();

            ModelBroker broker = new ModelBroker(getPrivilegeSet());
            m_sortedGradeLevels = StudentManager.buildGradeLevelMap(broker);
            m_maxGradeLevel = StudentManager.getMaxGradeLevel(getBroker());
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String value = null;
            SisStudent student = (SisStudent) entity.getBean();
            int yog = ((SRStudentLevelEntity) entity).getYog();
            if (yog > 0) {
                String gradeLevel = null;
                if (yog == student.getYog()) {
                    gradeLevel = student.getGradeLevel();
                    if (!StringUtils.isEmpty(gradeLevel)) {
                        value = data.lookupStateValue(SisStudent.class, SisStudent.COL_GRADE_LEVEL, gradeLevel);
                    }
                } else {
                    value = getGradeLevel(Integer.valueOf(yog));
                }
            }
            return value;
        }

        /**
         * Gets the grade level.
         *
         * @param yog Integer
         * @return String
         */
        private String getGradeLevel(Integer yog) {
            String value = null;
            if (m_mapGradeLevel.containsKey(yog)) {
                value = m_mapGradeLevel.get(yog);
            } else {
                List<String> matchingGradeLevels =
                        StudentManager.getMatchingGradeLevels(m_maxGradeLevel, yog.intValue(),
                                getCurrentContext().getSchoolYear(), m_sortedGradeLevels);
                for (String matchingGradeLevel : matchingGradeLevels) {
                    ReferenceCode gradeCode = m_referenceGradeCodeMap.get(matchingGradeLevel);
                    if (gradeCode != null && !StringUtils.isEmpty(gradeCode.getStateCode())) {
                        value = gradeCode.getStateCode();
                        break;
                    }
                }
                m_mapGradeLevel.put(yog, value);
            }
            return value;
        }

    }

    /**
     * The Class RetrieveParentLan.
     *
     * @author Follett Software Company
     */
    protected class RetrieveParentLang implements FieldRetriever {
        private static final String CALC_ID = "GA-SR-PAR-LANG";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String returnValue = null;
            SisStudent std = (SisStudent) entity.getBean();
            String parLang = (String) std.getFieldValueByBeanPath(m_fieldStdParLang);
            if (!StringUtils.isEmpty(parLang)) {
                returnValue = data.lookupStateValue(SisStudent.class, m_fieldStdParLang, parLang);
            }
            if (StringUtils.isEmpty(returnValue)) {
                String primLang = (String) std.getFieldValueByBeanPath(m_fieldStdPrimLang);
                if (!StringUtils.isEmpty(primLang)) {
                    returnValue = data.lookupStateValue(SisStudent.class, m_fieldStdPrimLang, primLang);
                }
            }
            if (!StringUtils.isEmpty(returnValue)) {
                returnValue = StringUtils.padLeft(returnValue, field.getMaxLength(), '0');
            }
            return StringUtils.isEmpty(returnValue) ? "008" : returnValue;
        }
    }

    /**
     * The Class RetrievePrimaryExceptionality.
     *
     * @author Follett Software Company
     */
    protected class RetrievePrimaryExceptionality implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String value = null;
            SisStudent student = (SisStudent) entity.getBean();
            PlainDate startDate = getOrganization().getCurrentContext().getStartDate();
            PlainDate spedExitDate = student.getSpedExitDate();
            if (null == spedExitDate || (null != spedExitDate && !(spedExitDate.before(startDate)))) {
                String primaryExceptionality = (String) student.getFieldValueByAlias(ALIAS_DOE_PRIMARY_EXCEPTIONALITY);
                value = lookupReferenceCodeByAlias(ALIAS_DOE_PRIMARY_EXCEPTIONALITY, primaryExceptionality,
                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
            }
            return value;
        }
    }

    /**
     * If the student is an English Learner and not enrolled in ESOL, this will return the reason
     * why the student is not enrolled in ESOL.
     *
     * @author X2 Development Corporation
     *
     */
    protected class RetrieveNonESOL implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String reason = null;

            SRStudentLevel studentLevelData = (SRStudentLevel) data;
            // SRStudentLevelEntity studentLevelEntity = (SRStudentLevelEntity) entity;
            SisStudent student = (SisStudent) entity.getBean();
            /*
             * String ell = studentLevelEntity.getFieldValue(studentLevelData.FIELD_ELL);
             * String esol = studentLevelEntity.getFieldValue(studentLevelData.FIELD_ESOL);
             *
             * if (CONSTANT_YES.equals(ell) && CONSTANT_NO.equals(esol))
             * {
             */
            reason = (String) student.getFieldValueByBeanPath(studentLevelData.m_nonEsolField);

            /*
             * if (!StringUtils.isEmpty(reason))
             * {
             */
            reason = lookupReferenceCodeByBeanPath(SisStudent.class, studentLevelData.m_nonEsolField, reason,
                    ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
            // reason = lookupStateValue(SisStudent.class, studentLevelData.m_nonEsolField, reason);
            // }
            // }

            return reason;
        }

    }

    /**
     * Retrieves the DOE PRE-K PROGRAM CODE, if the student is in grade level "PK" and if there
     * is a value in DOE PRE-K PROGRAM CODE return it.
     *
     * @author X2 Development Corporation
     *
     */
    protected class RetrievePreK implements FieldRetriever {
        private static final String ALIAS_PRE_K = "DOE PRE-K PROGRAM CODE";
        private static final String CODE_PRE_K = "PK";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String preKProgramCode = null;
            String preKProgramStateCode = null;
            SisStudent student = (SisStudent) entity.getBean();
            String stdStateGradeLevel =
                    lookupStateValue(SisStudent.class, SisStudent.COL_GRADE_LEVEL, student.getGradeLevel());
            if (CODE_PRE_K.equals(stdStateGradeLevel)) {
                preKProgramCode = (String) student.getFieldValueByAlias(ALIAS_PRE_K);
                if (!StringUtils.isEmpty(preKProgramCode)) {
                    preKProgramStateCode = lookupReferenceCodeByAlias(ALIAS_PRE_K, preKProgramCode,
                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                }
            }
            return preKProgramStateCode;
        }
    }

    /**
     * return value by fieldPath if "Place of Birth" field is not 1790 or 2310.
     *
     * @author X2 Development Corporation
     */
    protected class RetrievePrimaryLanguage implements FieldRetriever {
        private static final String CAL_ID = "GA-SR-PRL";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String returnValue = null;
            String primaryLanguage = (String) getProperty(entity.getBean(), field.getBeanPath());
            primaryLanguage = lookupStateValue(entity.getBean().getClass(), field.getBeanPath(), primaryLanguage);
            if (!StringUtils.isEmpty(primaryLanguage)) {
                int maxLength = field.getMaxLength();
                returnValue = StringUtils.padLeft(primaryLanguage, maxLength, '0');
            }
            return StringUtils.isEmpty(returnValue) ? "008" : returnValue;
        }
    }

    /**
     * Returns the count of student programs, in the student's program participation,
     * matching the requested parameter. The requested parameter will be the state value
     * for a type of program.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveProgramCount implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            int programCount = 0;
            SisStudent student = (SisStudent) entity.getBean();
            if (m_studentProgramParticipation != null &&
                    m_studentProgramParticipation.containsKey(student.getOid())) {
                Collection<StudentProgramParticipation> studentProgramParticipation =
                        m_studentProgramParticipation.get(student.getOid());
                String param = (String) field.getParameter();

                for (StudentProgramParticipation program : studentProgramParticipation) {
                    String programCode = program.getProgramCode();
                    if (!StringUtils.isEmpty(programCode)) {
                        programCode = lookupStateValue(StudentProgramParticipation.class,
                                StudentProgramParticipation.COL_PROGRAM_CODE, programCode);
                        if (param.equals(programCode)) {
                            programCount++;
                        }
                    }
                }
            }
            return Integer.valueOf(programCount);
        }

    }

    /**
     * Returns an indicator if the person is included in the specified race.
     * This implementation looks up all race codes for a person and finds
     * the race entry for the specified race. It returns a true/false value
     * indicating the presence of the race code record.
     *
     * The calculation parameter should be a string with three characters:
     * character 1:
     * The character to return if the requested race code is present.
     * character 2:
     * The character to return if the requested race code is not present.
     * character(s) 3+:
     * The reference code state code value in the reference table for race codes.
     * In GA, this is:
     * "W" - White
     * "B" - Black
     * "S" - Asian
     * "I" - Indian/Native/Alaskan
     * "P" - Pacific
     *
     * Ex: "SNS" searches for the Asian code, returns "S" if present, "N" otherwise.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveRace implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String param = (String) field.getParameter();
            String trueChar = param.substring(0, 1);
            String falseChar = param.substring(1, 2);
            String requestCode = param.substring(2);

            String raceCode = falseChar;

            Student student = (Student) entity.getBean();
            Collection<Race> races = m_raceCodeMap.get(student.getPersonOid());

            // Find the reference code that we are looking for.
            ReferenceCode refCode = m_raceCodes.get(requestCode);
            if (refCode != null && races != null) {
                for (Race race : races) {
                    if (refCode.getCode().equals(race.getRaceCode())) {
                        raceCode = trueChar;
                        break;
                    }
                }
            }

            return raceCode;
        }
    }

    /**
     * Retrieve a string indicating the student is in the
     * Year End retained.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveRetained implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            SisStudent student = (SisStudent) entity.getBean();
            String stdSchoolName = "";
            if (null != student && null != student.getSchool()) {
                stdSchoolName = student.getSchool().getName();
            }
            String entitySchoolName = "";
            School school = ((SRStudentLevelEntity) entity).getSchool();
            if (null != school) {
                entitySchoolName = school.getName();
            }
            String value = CONSTANT_NO;
            String retained = (String) student.getFieldValueByAlias(ALIAS_DOE_RETAINED);
            if (entitySchoolName.equals(stdSchoolName) && null != retained && retained.equals(BOOLEAN_YES)) {
                value = CONSTANT_YES;
            }
            return value;
        }
    }

    /**
     * Retrieve the school code from the school on the row. A student
     * can generate multiple schools, so retrieve the code from the current row school.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveSchoolCode implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            SisStudent student = (SisStudent) entity.getBean();
            School school = ((SRStudentLevelEntity) entity).getSchool();
            m_schoolCode = (String) student.getFieldValueByBeanPath(m_overrideSchoolCodeField);
            if (StringUtils.isEmpty(m_schoolCode)) {
                if (school != null) {
                    m_schoolCode = (String) school.getFieldValueByBeanPath(m_schoolCodeField);
                }
            }
            return m_schoolCode;
        }
    }

    /**
     * Retrieve student Y\N values by aliases.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveStdYNValues implements FieldRetriever {
        private static final String CALC_ID = "GA-SR-STD-YN";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();
            String value = null;
            String diplomaType = entity.getFieldValue(FIELD_DIPLOMA_TYPE);
            if (!StringUtils.isEmpty(diplomaType)) {
                String valueToCompare = (String) getProperty(student, field.getBeanPath());
                value = !StringUtils.isEmpty(valueToCompare) && BooleanAsStringConverter.TRUE.equals(valueToCompare)
                        ? CONSTANT_YES
                        : CONSTANT_NO;
            }
            return value;
        }
    }

    /**
     * Retrieve student Y\N values by aliases.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveStdYN implements FieldRetriever {
        private static final String CALC_ID = "GA-STD-YN";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();
            String valueToCompare = (String) getProperty(student, field.getBeanPath());
            return !StringUtils.isEmpty(valueToCompare) && BooleanAsStringConverter.TRUE.equals(valueToCompare)
                    ? CONSTANT_YES
                    : CONSTANT_NO;
        }
    }

    /**
     *
     * /**
     * Retrieve a bean property and strip off invalid characters.
     * Useful for cleaning names for reporting.
     * Also trim the name to maximum field length to avoid validation warnings.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveStripNameChar implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            String cleanValue = null;
            String nameValue = (String) getProperty(entity.getBean(), field.getBeanPath());
            int max = field.getMaxLength();
            if (!StringUtils.isEmpty(nameValue)) {
                Matcher matcher = m_illegalNameCharacters.matcher(nameValue);
                cleanValue = matcher.replaceAll("");
                if (cleanValue.length() > max) {
                    cleanValue = cleanValue.substring(0, max);
                }
            } else {
                cleanValue = "";
            }

            return cleanValue;
        }
    }

    /**
     * Returns Y or N if the student with ESOL="Y" has Title 3 Served.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveTitle3 implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            Object value = null;
            String esolValue = entity.getFieldValue(FIELD_ESOL);

            SisStudent student = (SisStudent) entity.getBean();

            if ("Y".equals(esolValue)) {
                value = BooleanAsStringConverter.TRUE.equals(student.getFieldValueByBeanPath(m_fieldStdTitle3))
                        ? CONSTANT_YES
                        : CONSTANT_NO;
            }

            return value;
        }
    }

    /**
     * Returns true if the student has been enrolled for less than three years.
     * Returns false otherwise.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveUSSchoolYears implements FieldRetriever {
        private final String ALIAS_US_ENTRY_DATE = "DOE Date Entry US";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            Boolean value = Boolean.FALSE;

            SisStudent student = (SisStudent) entity.getBean();
            String usEntryDateString = (String) student.getFieldValueByAlias(ALIAS_US_ENTRY_DATE);

            if (!StringUtils.isEmpty(usEntryDateString)) {
                SimpleDateFormat dateFormatter = new SimpleDateFormat("yyyy-MM-dd");
                PlainDate usEntryDate;
                try {
                    usEntryDate = new PlainDate(dateFormatter.parse(usEntryDateString));
                    Calendar threeYearsInUs = Calendar.getInstance();
                    threeYearsInUs.setTime(usEntryDate);
                    threeYearsInUs.add(Calendar.YEAR, 3);

                    Calendar today = Calendar.getInstance();

                    if (today.before(threeYearsInUs)) {
                        value = Boolean.TRUE;
                    }
                } catch (ParseException e) {
                    // do nothing not a date.
                }
            }

            return value;
        }
    }

    /**
     * Never required but must be blank unless Grade Level = 12.
     *
     * @author Follett Software Company
     *
     */
    protected class ValidateBlankForGrade12 implements FieldValidator {
        private final static String VAL_ID = "GA-SR-VAL-GRADE-12";

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            SRStudentLevel studentLevelData = (SRStudentLevel) data;
            SRStudentLevelEntity studentLevelEntity = (SRStudentLevelEntity) entity;
            String grade = studentLevelEntity.getFieldValue(studentLevelData.FIELD_GRADE);
            if (!StringUtils.isEmpty(grade) && "12".equals(grade) && !StringUtils.isEmpty(value)) {
                String error = field.getFieldId() + " invalid for grade level = " + grade + ".";
                String message = "";
                errors.add(new StateReportValidationError(entity, field, error, message));
            }
            return errors;
        }
    }

    /**
     * Required when gradel level = 09 - 12.
     *
     * @author Follett Software Company
     *
     */
    protected class ValidateCCAE implements FieldValidator {
        private final List<String> CONSTANT_GRADE_LEVELS = Arrays.asList("09", "10", "11", "12");
        private final static String VAL_ID = "GA-SR-VAL-CCAE";

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            SRStudentLevel studentLevelData = (SRStudentLevel) data;
            SRStudentLevelEntity studentLevelEntity = (SRStudentLevelEntity) entity;
            String grade = studentLevelEntity.getFieldValue(studentLevelData.FIELD_GRADE);

            if (!StringUtils.isEmpty(grade) && CONSTANT_GRADE_LEVELS.contains(grade)
                    && StringUtils.isEmpty(value)) {
                String error = "CCAE cannot be blank for grade levels 09 - 12.";
                String message = "";
                errors.add(new StateReportValidationError(entity, field, error, message));
            }
            return errors;
        }
    }

    /**
     * Field is required only for grade levels 06 and 07.
     *
     * @author Follett Software Company
     *
     */
    protected class ValidateCareerAwareness implements FieldValidator {
        private final List<String> REQUIRED_VALID_GRADES = Arrays.asList("01", "02", "03", "04", "05");
        private final static String VAL_ID = "GA-SR-VAL-CAW";

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            SRStudentLevel studentLevelData = (SRStudentLevel) data;
            SRStudentLevelEntity studentLevelEntity = (SRStudentLevelEntity) entity;
            String grade = studentLevelEntity.getFieldValue(studentLevelData.FIELD_GRADE);
            if (!StringUtils.isEmpty(grade) && REQUIRED_VALID_GRADES.contains(grade)
                    && StringUtils.isEmpty(value)) {
                String error = "Career Awareness Lessons is blank for student in grade 01-05.";
                String message = "";
                errors.add(new StateReportValidationError(entity, field, error, message));
            }
            return errors;
        }
    }

    /**
     * Required only when Grade Level = 09, 10, 11 or 12.
     *
     * Must be blank when Grade Level is not = 09, 10, 11 or 12.
     *
     * @author Follett Software Company
     *
     */
    protected class ValidateDate9thGrade implements FieldValidator {
        private final List<String> CONSTANT_GRADE_LEVELS = Arrays.asList("09", "10", "11", "12");
        private final static String VAL_ID = "GA-SR-VAL-D9GR";

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            SRStudentLevel studentLevelData = (SRStudentLevel) data;
            SRStudentLevelEntity studentLevelEntity = (SRStudentLevelEntity) entity;
            String grade = studentLevelEntity.getFieldValue(studentLevelData.FIELD_GRADE);
            if (!StringUtils.isEmpty(grade) && CONSTANT_GRADE_LEVELS.contains(grade)
                    && StringUtils.isEmpty(value)) {
                String error = "Date Entered 9th Grade missing.";
                String message = "";
                errors.add(new StateReportValidationError(entity, field, error, message));
            }
            if (!StringUtils.isEmpty(grade) && !CONSTANT_GRADE_LEVELS.contains(grade)
                    && !StringUtils.isEmpty(value)) {
                String error = "Date Entered 9th grade no valid for grade level < 09.";
                String message = "";
                errors.add(new StateReportValidationError(entity, field, error, message));
            }
            return errors;
        }
    }

    /**
     * Required when [Birth Place] not = 1790 or 2310.
     *
     * @author Follett Software Company
     */
    protected class ValidateDateUsEntry implements FieldValidator {
        private final List<String> CONSTANT_BIRTH_PLACE = Arrays.asList("1790", "2310");
        private final static String VAL_ID = "GA-SR-VAL-US-ENTRY";

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            SRStudentLevel studentLevelData = (SRStudentLevel) data;
            SRStudentLevelEntity studentLevelEntity = (SRStudentLevelEntity) entity;
            String birthPlace = studentLevelEntity.getFieldValue(studentLevelData.FIELD_BIRTH_PLACE);

            if (!StringUtils.isEmpty(birthPlace) && !CONSTANT_BIRTH_PLACE.contains(birthPlace)
                    && StringUtils.isEmpty(value)) {
                String error = "Date of Entry to U.S. School missing.";
                String message = "";
                errors.add(new StateReportValidationError(entity, field, error, message));
            }
            return errors;
        }
    }

    /**
     * Diploma Type required when Student.Status = Graduate.
     *
     * @author Follett Software Company
     *
     */
    protected class ValidateDiplomaType implements FieldValidator {
        private final static String VAL_ID = "GA-SR-VAL-DT";

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            SisStudent student = (SisStudent) entity.getBean();
            if (ENR_TYPE_GRADUATE.equals(student.getEnrollmentStatus()) && StringUtils.isEmpty(value)) {
                String error = "Diploma missing for graduated student.";
                String message = "";
                errors.add(new StateReportValidationError(entity, field, error, message));
            }
            if (ENR_TYPE_GRADUATE.equals(student.getEnrollmentStatus()) && !StringUtils.isEmpty(value)
                    && !Arrays.asList("S", "A", "G").contains(value)) {
                String error = "Diploma Type invalid value.";
                String message = "";
                errors.add(new StateReportValidationError(entity, field, error, message));
            }
            return errors;
        }
    }

    /**
     * If Y, Primary Excpectionality cannot = Blank, Z, X, 1, U, T or 3.
     *
     * @author Follett Software Company
     *
     */
    protected class ValidateGAA implements FieldValidator {
        private final List<String> NOT_VALID_VALUES = Arrays.asList("Z", "X", "U", "T", "3");
        private final static String VAL_ID = "GA-SR-VAL-GAA";

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            SRStudentLevel studentLevelData = (SRStudentLevel) data;
            SRStudentLevelEntity studentLevelEntity = (SRStudentLevelEntity) entity;
            String primExp = studentLevelEntity.getFieldValue(studentLevelData.FIELD_PRIM_EXP);
            if (!StringUtils.isEmpty(value) && "Y".equals(value)
                    && (StringUtils.isEmpty(primExp) || NOT_VALID_VALUES.contains(primExp))) {
                String error = "GAA Flag not valid for Primary Area of Exceptionality.";
                String message = "";
                errors.add(new StateReportValidationError(entity, field, error, message));
            }
            return errors;
        }
    }

    /**
     * Must be blank if Gifted Eligiblity Code = 4 or 9.
     *
     * @author Follett Software Company
     *
     */
    protected class ValidateGiftedService implements FieldValidator {
        private final List<String> NOT_VALID_GIFT_CODES = Arrays.asList("4", "9");
        private final static String VAL_ID = "GA-SR-VAL-GIFT-SERV";
        private final List<String> VALID_IF_EXIST = Arrays.asList("1", "2");

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            SRStudentLevel studentLevelData = (SRStudentLevel) data;
            SRStudentLevelEntity studentLevelEntity = (SRStudentLevelEntity) entity;
            String giftElig = studentLevelEntity.getFieldValue(studentLevelData.FIELD_GIFT_ELIG);
            if (!StringUtils.isEmpty(value) && !VALID_IF_EXIST.contains(value)) {
                String error = "Gifted Served not valid. Should be 1 or 2.";
                String message = "";
                errors.add(new StateReportValidationError(entity, field, error, message));
            }
            if (!StringUtils.isEmpty(giftElig) && NOT_VALID_GIFT_CODES.contains(giftElig)
                    && !StringUtils.isEmpty(value)) {
                String error = "Gifted Served not valid when Gifted Eligibility = 4 or 9.";
                String message = "";
                errors.add(new StateReportValidationError(entity, field, error, message));
            }
            return errors;
        }
    }

    /**
     * Required only when Residential Env = 3 or 4.
     * Must be blank when Residential Env = 1 or 2
     *
     * @author Follett Software Company
     *
     */
    protected class ValidateHomelessShelter implements FieldValidator {
        private final List<String> REQUIRED_WHEN = Arrays.asList("3", "4");
        private final List<String> BLANK_WHEN = Arrays.asList("1", "2");
        private final static String VAL_ID = "GA-SR-VAL-HS";

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            SRStudentLevel studentLevelData = (SRStudentLevel) data;
            SRStudentLevelEntity studentLevelEntity = (SRStudentLevelEntity) entity;
            String resEnv = studentLevelEntity.getFieldValue(studentLevelData.FIELD_RES_ENV);
            if (!StringUtils.isEmpty(resEnv) && REQUIRED_WHEN.contains(resEnv)
                    && StringUtils.isEmpty(value)) {
                String error = "Homeless Shelter Required when Residential Env = 3 or 4.";
                String message = "";
                errors.add(new StateReportValidationError(entity, field, error, message));
            }
            if (!StringUtils.isEmpty(resEnv) && BLANK_WHEN.contains(resEnv)
                    && !StringUtils.isEmpty(value)) {
                String error = "Homeless Shelter not valid when Residental Env = 1 or 2.";
                String message = "";
                errors.add(new StateReportValidationError(entity, field, error, message));
            }
            return errors;
        }
    }

    /**
     * Validate whether value for Non-ESOL is valid. If a student is an English Learner
     * and ESOL flag is "N" than the student should have a value for Reason for Non-ESOL
     *
     * @author Follett Software Company
     *
     */
    protected class ValidateNonEsol implements FieldValidator {
        private final List<String> CONSTANT_ELL = Arrays.asList("N", "M", "F");

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            SRStudentLevel studentLevelData = (SRStudentLevel) data;
            SRStudentLevelEntity studentLevelEntity = (SRStudentLevelEntity) entity;
            String ell = studentLevelEntity.getFieldValue(studentLevelData.FIELD_ELL);
            String nonEsol = studentLevelEntity.getFieldValue(studentLevelData.FIELD_NON_ESOL);
            if (!StringUtils.isEmpty(ell) && CONSTANT_ELL.contains(ell) && !StringUtils.isEmpty(nonEsol)) {
                String error = "Non-ESOL" + STYLE_END + " not valid when ELL = N, M or F.";
                String message = "";
                errors.add(new StateReportValidationError(entity, field, error, message));
            }
            return errors;
        }
    }


    /**
     * Validates that a student in grade level of "Pre-K" has a valid "Pre-K Program Code".
     *
     * @author Follett Software Company
     */
    protected class ValidatePreK implements FieldValidator {

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            String CODE_PRE_K = "PK";
            SRStudentLevel studentLevelData = (SRStudentLevel) data;
            SRStudentLevelEntity studentLevelEntity = (SRStudentLevelEntity) entity;
            SisStudent student = (SisStudent) entity.getBean();

            String preKValue = studentLevelEntity.getFieldValue(studentLevelData.FIELD_PREK);

            if (CODE_PRE_K.equals(student.getGradeLevel()) && StringUtils.isEmpty(preKValue)) {
                String error = "A student in grade level " + STYLE_BOLD + "PK" + STYLE_END +
                        " must have a " + STYLE_BOLD + " Pre-K Program Code" + STYLE_END;
                String message = "";
                errors.add(new StateReportValidationError(entity, field, error, message));
            }
            return errors;
        }
    }

    /**
     * Primary Exceptionality= '8' ONLY valid when grade level = PK, UK, KK, U1, 01, 02, 03, 04 or
     * 05.
     *
     * @author Follett Software Company
     *
     */
    protected class ValidatePrimExp implements FieldValidator {
        private final List<String> NOT_VALID_GRADES = Arrays.asList("PK", "UK", "KK", "01", "02", "03", "04", "05");
        private final static String VAL_ID = "GA-SR-VAL-PRIM-EXP";

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            SRStudentLevel studentLevelData = (SRStudentLevel) data;
            SRStudentLevelEntity studentLevelEntity = (SRStudentLevelEntity) entity;
            String grade = studentLevelEntity.getFieldValue(studentLevelData.FIELD_GRADE);
            if (!StringUtils.isEmpty(grade) && !StringUtils.isEmpty(value) && "8".equals(value)
                    && !NOT_VALID_GRADES.contains(grade)) {
                String error =
                        "Primary Exceptionality = 08 and Grade Level is NOT PK, UK, KK, U1, 01, 02, 03, 04 or 05.";
                String message = "";
                errors.add(new StateReportValidationError(entity, field, error, message));
            }
            return errors;
        }
    }

    /**
     * Cannot be blank when ELL = Y, 1 or 2.
     *
     * @author Follett Software Company
     */
    protected class ValidatePrimLang implements FieldValidator {
        private final List<String> NOT_VALID_ELL_CODES = Arrays.asList("Y", "1", "2");
        private final static String VAL_ID = "GA-SR-VAL-PRIM-LANG";

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            SRStudentLevel studentLevelData = (SRStudentLevel) data;
            SRStudentLevelEntity studentLevelEntity = (SRStudentLevelEntity) entity;
            String ell = studentLevelEntity.getFieldValue(studentLevelData.FIELD_ELL);
            if (!StringUtils.isEmpty(ell) && NOT_VALID_ELL_CODES.contains(ell)
                    && StringUtils.isEmpty(value)) {
                String error = "Primary Language required with EL = Y, 1 or 2";
                String message = "";
                errors.add(new StateReportValidationError(entity, field, error, message));
            }
            return errors;
        }
    }

    /**
     * This is not a required field, but if populated must = 1, 2, 3 or 4.
     *
     * @author Follett Software Company
     *
     */
    protected class ValidateResEnv implements FieldValidator {
        private final List<String> VALID_VALUES = Arrays.asList("1", "2", "3", "4");
        private final static String VAL_ID = "GA-SR-VAL-RESENV";

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            if (!StringUtils.isEmpty(value) && !VALID_VALUES.contains(value)) {
                String error = "Residential Enviornment code is not valid.";
                String message = "";
                errors.add(new StateReportValidationError(entity, field, error, message));
            }
            return errors;
        }
    }

    /**
     * Gets the student SLDS.
     *
     * @param oid String
     * @return X2BaseBean
     */
    protected Collection<X2BaseBean> getStudentSLDS(String oid) {
        if (m_stdSLDSMap == null) {
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(UserDefinedTableA.REL_EXTENDED_DATA_DICTIONARY + ModelProperty.PATH_DELIMITER
                    + ExtendedDataDictionary.COL_ID, DDX_ID_SLDS);
            BeanQuery query =
                    m_helper.getStudentSelectionQuery(m_beanClass, criteria, UserDefinedTableA.COL_STUDENT_OID);
            m_stdSLDSMap =
                    getBroker().getGroupedCollectionByQuery(query, UserDefinedTableA.COL_STUDENT_OID, 1024);
        }
        return m_stdSLDSMap.get(oid);
    }

    /**
     * Initialize the data module.
     */
    @Override
    protected void initialize() {
        // Set the report date.
        m_reportDate = new PlainDate(); // (PlainDate) getParameter(REPORT_DATE_PARAM);

        // Construct and initialize the Student History Helper.
        m_helper = new StudentHistoryHelper(this);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);

        // Lookup aliases
        m_schoolCodeField = translateAliasToJavaName(SCHOOL_CODE_ALIAS, true);
        m_overrideSchoolCodeField = translateAliasToJavaName(DOE_OVERRIDE_SCHOOL, true);
        m_nonEsolField = translateAliasToJavaName(NON_ESOL_ALIAS, true);
        m_activeCode =
                PreferenceManager.getPreferenceValue(getOrganization(), SystemPreferenceDefinition.STUDENT_ACTIVE_CODE);
        String preRegCode =
                PreferenceManager.getPreferenceValue(getOrganization(), SystemPreferenceDefinition.STUDENT_PRE_REG);

        m_doeDiplomaType = translateAliasToJavaName(ALIAS_DOE_DIPLOMA_TYPE, true);
        m_fieldStdTitle3 = translateAliasToJavaName(ALIAS_DOE_TITLE_3, true);
        m_doeEllField = translateAliasToJavaName(ALIAS_DOE_ELL, true);
        m_fieldStdParLang = translateAliasToJavaName(ALIAS_STD_PAR_LANG, true);
        m_fieldStdPrimLang = translateAliasToJavaName(ALIAS_STD_PRIM_LANG, true);
        m_fieldStdGiftElig = translateAliasToJavaName(ALIAS_STD_GE, true);
        m_fieldStdGiftReferral = translateAliasToJavaName(ALIAS_STD_GR, true);
        m_calMngr = new CalendarManager(getBroker());
        DataDictionary stdDictByDdx = DataDictionaryUtils.getExtendedDictionaryById(DDX_ID_SLDS, getBroker());
        if (stdDictByDdx != null) {
            DataDictionaryField dictField =
                    stdDictByDdx.findDataDictionaryFieldByAlias(ALIAS_STD_DDX_CC_YEAR);
            if (dictField != null) {
                m_fieldStdDdxSklCtx = dictField.getJavaName();
                if (m_beanClass == null) {
                    m_beanClass = dictField.getDataTable().getDataClass();
                }
            }
            dictField = stdDictByDdx.findDataDictionaryFieldByAlias(ALIAS_STD_DDX_CC_SKL_CODE);
            if (dictField != null) {
                m_fieldStdDdxSklCode = dictField.getJavaName();
                if (m_beanClass == null) {
                    m_beanClass = dictField.getDataTable().getDataClass();
                }
            }
        }
        String disabilityRefTableOid = null;
        m_std504PlanDDX = DataDictionaryUtils.getExtendedDictionaryById(DDX_ID_STD_504_PLAN, getBroker());
        if (m_std504PlanDDX != null) {
            DataDictionaryField dictField =
                    m_std504PlanDDX.findDataDictionaryField(IepDisability.class.getName(),
                            IepDisability.COL_DISABILITY_CODE);
            if (dictField != null) {
                disabilityRefTableOid = dictField.getReferenceTableOid();
            }
            if (disabilityRefTableOid != null) {
                Criteria disabilityRefCriteria = new Criteria();
                disabilityRefCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, disabilityRefTableOid);
                QueryByCriteria disabilityRefQuery = new QueryByCriteria(ReferenceCode.class, disabilityRefCriteria);
                m_disabilityCodes = getBroker().getMapByQuery(disabilityRefQuery, ReferenceCode.COL_CODE, 10);
            }
        }
        X2Criteria studentCriteria = getStudentCriteria();
        studentCriteria.addNotEqualTo(Student.COL_ENROLLMENT_STATUS, preRegCode);
        QueryByCriteria studentQuery = new QueryByCriteria(Student.class, studentCriteria);
        int sort = ((Integer) getParameter(PARAM_SORT)).intValue();
        switch (sort) {
            case 0: // Name
                studentQuery.addOrderByAscending(Student.COL_NAME_VIEW);
                break;

            case 1: // YOG
                studentQuery.addOrderByAscending(Student.COL_YOG);
                studentQuery.addOrderByAscending(Student.COL_NAME_VIEW);
                break;

            case 2: // School
                studentQuery.addOrderByAscending(Student.REL_SCHOOL + PATH_DELIMITER + School.COL_NAME);
                studentQuery.addOrderByAscending(Student.COL_NAME_VIEW);
                break;

            case 3: // LASID
                studentQuery.addOrderByAscending(Student.COL_LOCAL_ID);
                break;

            case 4: // SASID
                studentQuery.addOrderByAscending(Student.COL_STATE_ID);
                break;

            default:
                studentQuery.addOrderByAscending(Student.COL_NAME_VIEW);
                break;
        }

        // Set the query to be used for student selection.
        setQuery(studentQuery);
        setEntityClass(SRStudentLevelEntity.class);

        // Get race code reference codes for use in the race retriever.
        Criteria raceCriteria = new Criteria();
        raceCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, "rtbRaceCodes");
        QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, raceCriteria);
        m_raceCodes = getBroker().getMapByQuery(query, ReferenceCode.COL_STATE_CODE, 5);

        // Load the race codes for all students included in the export.
        SubQuery subQuery = new SubQuery(Student.class, Student.COL_PERSON_OID, studentCriteria);
        raceCriteria = new Criteria();
        raceCriteria.addIn(Race.COL_PERSON_OID, subQuery);
        QueryByCriteria raceQuery = new QueryByCriteria(Race.class, raceCriteria);
        m_raceCodeMap = getBroker().getGroupedCollectionByQuery(raceQuery, Race.COL_PERSON_OID, 100);

        // Load associated maps for student data.
        loadEnrollmentData(studentCriteria);

        // Load set of retained student Oids from snapshot.
        loadRetainedStudents();

        // Load current year's student program participation by student oid
        loadStudentProgramParticipation();

        // Load ASVAB assessments.
        loadAsmASVAB(studentCriteria);

        // Add any retrievers
        HashMap calcs = new HashMap<String, FieldRetriever>();
        calcs.put("GA-SR-NON-ESOL", new RetrieveNonESOL());
        calcs.put("GA-SR-SCHOOL-YEARS", new RetrieveUSSchoolYears());
        calcs.put("GA-SR-CLEAN", new RetrieveStripNameChar());
        calcs.put("GA-SR-RACE", new RetrieveRace());
        calcs.put("GA-SR-RETAINED", new RetrieveRetained());
        calcs.put("GA-SR-DIPLO-TYPE", new RetrieveDiplomaType());
        calcs.put("GA-SR-SCHOOL-CODE", new RetrieveSchoolCode());
        calcs.put("GA-SR-PREK", new RetrievePreK());
        calcs.put("GA-SR-PRI-EXC", new RetrievePrimaryExceptionality());
        calcs.put("GA-SR-TITLE-3", new RetrieveTitle3());
        calcs.put(RetrievePrimaryLanguage.CAL_ID, new RetrievePrimaryLanguage());
        calcs.put(RetrieveGradeLevel.CAL_ID, new RetrieveGradeLevel());
        calcs.put("FTE-ELL", new RetrieveELL());
        calcs.put(RetrieveParentLang.CALC_ID, new RetrieveParentLang());
        calcs.put(RetrieveCTAEValues.CALC_ID, new RetrieveCTAEValues());
        calcs.put(Retrieve504.CALC_ID, new Retrieve504(studentCriteria));
        calcs.put(Retrieve504Detail.CALC_ID, new Retrieve504Detail());
        calcs.put(RetrieveBiLitLanguage.CALC_ID, new RetrieveBiLitLanguage());
        calcs.put(RetrieveStdYNValues.CALC_ID, new RetrieveStdYNValues());
        calcs.put(RetrieveGE.CALC_ID, new RetrieveGE());
        calcs.put(RetrieveGR.CALC_ID, new RetrieveGR());
        calcs.put(RetrieveStdYN.CALC_ID, new RetrieveStdYN());
        calcs.put(RetrieveASVAB.CALC_ID, new RetrieveASVAB());
        super.addCalcs(calcs);

        // Add any validators
        HashMap vals = new HashMap<String, FieldValidator>();
        vals.put("GA-SR-VAL-NON-ESOL", new ValidateNonEsol());
        vals.put("GA-SR-VAL-PREK", new ValidatePreK());
        vals.put(ValidateDateUsEntry.VAL_ID, new ValidateDateUsEntry());
        vals.put(ValidateCCAE.VAL_ID, new ValidateCCAE());
        vals.put(ValidateDate9thGrade.VAL_ID, new ValidateDate9thGrade());
        vals.put(ValidateGiftedService.VAL_ID, new ValidateGiftedService());
        vals.put(ValidatePrimLang.VAL_ID, new ValidatePrimLang());
        vals.put(ValidatePrimExp.VAL_ID, new ValidatePrimExp());
        vals.put(ValidateHomelessShelter.VAL_ID, new ValidateHomelessShelter());
        vals.put(ValidateDiplomaType.VAL_ID, new ValidateDiplomaType());
        vals.put(ValidateGAA.VAL_ID, new ValidateGAA());
        vals.put(ValidateCareerAwareness.VAL_ID, new ValidateCareerAwareness());
        vals.put(ValidateBlankForGrade12.VAL_ID, new ValidateBlankForGrade12());
        vals.put(ValidateResEnv.VAL_ID, new ValidateResEnv());
        super.addValidators(vals);
    }

    /**
     * Adds criteria to filter the export selection by record set.
     *
     * @param criteria Criteria
     * @param recordSetName String
     */
    private void addRecordSetCriteria(Criteria criteria, String recordSetName) {
        Criteria recordSetCriteria = new Criteria();
        recordSetCriteria.addEqualTo(RecordSetKey.REL_RECORD_SET + "." + RecordSet.COL_NAME,
                recordSetName);

        criteria.addIn(X2BaseBean.COL_OID, new SubQuery(RecordSetKey.class,
                RecordSetKey.COL_OBJECT_OID, recordSetCriteria));
    }

    /**
     * Gets the assessment dictionary for ASVAB.
     *
     * @return Data dictionary
     */
    private DataDictionary getAssessmentDictionary() {
        if (m_asmDictASVAB == null) {
            X2Criteria assessmentDefinitonCriteria = new X2Criteria();
            assessmentDefinitonCriteria.addEqualTo(AssessmentDefinition.COL_ID, ASD_ID_ASVAB);

            QueryByCriteria assessmentDefinitonQuery =
                    new QueryByCriteria(AssessmentDefinition.class, assessmentDefinitonCriteria);

            AssessmentDefinition assessmentDefinition =
                    (AssessmentDefinition) getBroker().getBeanByQuery(assessmentDefinitonQuery);
            if (assessmentDefinition == null) {
                addSetupError(ERROR_TYPE_WARNING,
                        ASD_ID_ASVAB + ERROR_MESSAGE_ASSESS_DEF_NOT_LOADED);
            } else {
                m_asmDictASVAB =
                        DataDictionary.getDistrictDictionary(assessmentDefinition, getBroker().getPersistenceKey());
            }
        }
        return m_asmDictASVAB;
    }

    /**
     * Builds a criteria for the students that should be reported (not considering user input
     * parameters).
     *
     * @return Criteria
     */
    private Criteria getReportingCriteria() {
        /*
         * Students to include.
         *
         * 1. The student is active and in an active school.
         * or
         * 2. The student has (E,W) enrollment records within the school year.
         *
         */

        // Select students with enrollment activity (E,W) in the school this year.
        X2Criteria enrollCriteria = new X2Criteria();
        enrollCriteria.addEqualTo(StudentEnrollment.COL_ENROLLMENT_TYPE, StudentEnrollment.WITHDRAWAL);
        X2Criteria enrollCriteria2 = new X2Criteria();
        enrollCriteria2.addEqualTo(StudentEnrollment.COL_ENROLLMENT_TYPE, StudentEnrollment.ENTRY);
        enrollCriteria2.addOrCriteria(enrollCriteria);

        X2Criteria activityCriteria = new X2Criteria();
        PlainDate startDate = getOrganization().getCurrentContext().getStartDate();
        activityCriteria.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, startDate);
        activityCriteria.addLessOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, m_reportDate);
        activityCriteria.addAndCriteria(enrollCriteria2);

        if (isSchoolContext()) {
            activityCriteria.addEqualTo(StudentEnrollment.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            activityCriteria.addEqualTo(StudentEnrollment.REL_SCHOOL + PATH_DELIMITER + School.COL_INACTIVE_INDICATOR,
                    Boolean.FALSE);
            activityCriteria.addEqualTo(StudentEnrollment.REL_SCHOOL + PATH_DELIMITER + School.COL_ARCHIVE_INDICATOR,
                    Boolean.FALSE);
        }

        SubQuery enrQuery = new SubQuery(StudentEnrollment.class, StudentEnrollment.COL_STUDENT_OID, activityCriteria);

        // Get the students current primary school, Or in students with enrollment activity.
        enrollCriteria = new X2Criteria();
        enrollCriteria.addIn(X2BaseBean.COL_OID, enrQuery);

        X2Criteria primaryCriteria = new X2Criteria();
        primaryCriteria.addEqualTo(Student.COL_ENROLLMENT_STATUS, m_activeCode);

        primaryCriteria.addOrCriteria(enrollCriteria);

        return primaryCriteria;
    }

    /**
     * Returns the criteria that retrieves all students that should be included in the export.
     *
     * @return Criteria
     */
    private X2Criteria getStudentCriteria() {
        X2Criteria userCriteria = new X2Criteria();

        /*
         * Check school selection user input parameter.
         */
        if (isSchoolContext()) {
            userCriteria.addEqualTo(Student.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            userCriteria.addEqualTo(Student.REL_SCHOOL + PATH_DELIMITER + School.COL_INACTIVE_INDICATOR, Boolean.FALSE);
            userCriteria.addEqualTo(Student.REL_SCHOOL + PATH_DELIMITER + School.COL_ARCHIVE_INDICATOR, Boolean.FALSE);
        }

        /*
         * Check student selection criteria user input.
         */
        String queryString = (String) getParameter(PARAM_QUERY_STRING);
        int queryBy = ((Integer) getParameter(PARAM_QUERY_BY)).intValue();
        switch (queryBy) {
            case 1: // YOG
                userCriteria.addEqualTo(Student.COL_YOG, queryString);
                break;

            case 2: // LASID
                userCriteria.addEqualTo(Student.COL_LOCAL_ID, queryString);
                break;

            case 3: // SASID
                userCriteria.addEqualTo(Student.COL_STATE_ID, queryString);
                break;

            case 4: // Snapshot
                addRecordSetCriteria(userCriteria, queryString);
                break;

            default:
                // Take all students in the district
                break;
        }

        /*
         * Then combine the user criteria with the general reporting criteria
         */
        X2Criteria studentCriteria = new X2Criteria();
        studentCriteria.addAndCriteria(userCriteria);
        studentCriteria.addAndCriteria(getReportingCriteria());

        return studentCriteria;
    }

    /**
     * Load asm ASVAB.
     *
     * @param stdCriteria X2Criteria
     */
    private void loadAsmASVAB(X2Criteria stdCriteria) {
        DataDictionary dictionary = getAssessmentDictionary();
        DataDictionaryField dictField =
                dictionary.findDataDictionaryFieldByAlias(ALIAS_ASM_SCORE);
        if (dictField != null) {
            m_fieldAsmScore = dictField.getJavaName();
            if (!StringUtils.isBlank(m_fieldAsmScore)) {
                X2Criteria asvabCriteria = new X2Criteria();
                asvabCriteria.addEqualTo(StudentAssessment.REL_ASSESSMENT_DEFINITION + ModelProperty.PATH_DELIMITER
                        + AssessmentDefinition.COL_ID, ASD_ID_ASVAB);
                asvabCriteria.addNotEmpty(m_fieldAsmScore, getBroker().getPersistenceKey());
                SubQuery stdSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, stdCriteria);
                asvabCriteria.addIn(StudentAssessment.COL_STUDENT_OID, stdSubQuery);
                QueryByCriteria asmQuery = new QueryByCriteria(StudentAssessment.class, asvabCriteria, true);
                m_stdAsmASVABMap = getBroker().getGroupedCollectionByQuery(asmQuery, StudentAssessment.COL_STUDENT_OID);
            }
        }
    }

    /**
     * Loads the enrollment data required by this export.
     *
     * @param studentCriteria Criteria
     */
    private void loadEnrollmentData(Criteria studentCriteria) {
        ArrayList typeCodes = new ArrayList(2);
        typeCodes.add(StudentEnrollment.ENTRY);
        typeCodes.add(StudentEnrollment.WITHDRAWAL);
        typeCodes.add(StudentEnrollment.YOG_CHANGE);

        // subQuery - Students.
        SubQuery studentsSubQuery = new SubQuery(Student.class, X2BaseBean.COL_OID, studentCriteria);

        // Main report query - students and Absence.
        Criteria criteria = new Criteria();
        criteria.addIn(StudentEnrollment.COL_STUDENT_OID, studentsSubQuery);
        if (isSchoolContext()) {
            criteria.addEqualTo(StudentEnrollment.COL_SCHOOL_OID, getSchool().getOid());
        }
        criteria.addIn(StudentEnrollment.COL_ENROLLMENT_TYPE, typeCodes);
        criteria.addGreaterOrEqualThanField(StudentEnrollment.COL_ENROLLMENT_DATE,
                StudentEnrollment.REL_SCHOOL + PATH_DELIMITER +
                        SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER +
                        SchoolScheduleContext.REL_ACTIVE_SCHEDULE + PATH_DELIMITER +
                        Schedule.COL_START_DATE);

        QueryByCriteria query = new QueryByCriteria(StudentEnrollment.class, criteria);
        query.addOrderBy(StudentEnrollment.COL_ENROLLMENT_DATE, false);
        query.addOrderBy(StudentEnrollment.COL_TIMESTAMP, false);

        m_studentEnrollements = getBroker().getGroupedCollectionByQuery(query, StudentEnrollment.COL_STUDENT_OID, 1000);
    }

    /**
     * Load a set of student OID of students retained at the end of the school year.
     * This should use the retained snapshot from Year End Rollover.
     */
    private void loadRetainedStudents() {
        String snapshotName = (String) getParameter(PARAM_RETAINED_SNAPSHOT);
        if (!StringUtils.isEmpty(snapshotName)) {
            Criteria criteria = new Criteria();
            addRecordSetCriteria(criteria, snapshotName);
            ReportQueryByCriteria query = new ReportQueryByCriteria(Student.class,
                    new String[] {X2BaseBean.COL_OID}, criteria);
            m_retained = new HashSet<String>();
            ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);
            try {
                while (iterator.hasNext()) {
                    Object[] row = (Object[]) iterator.next();
                    m_retained.add((String) row[0]);
                }
            } finally {
                iterator.close();
            }
        }
    }

    /**
     * Load student program participation.
     */
    private void loadStudentProgramParticipation() {
        X2Criteria currentYearCriteria = new X2Criteria();

        PlainDate schoolStartDate = getOrganization().getCurrentContext().getStartDate();
        PlainDate schoolEndDate = getOrganization().getCurrentContext().getEndDate();

        X2Criteria endDateCriteria = new X2Criteria();
        X2Criteria endDateEmptyCriteria = new X2Criteria();
        X2Criteria endDateValidCriteria = new X2Criteria();

        currentYearCriteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, schoolEndDate);

        endDateEmptyCriteria.addEmpty(StudentProgramParticipation.COL_END_DATE, getOrganization().getPersistenceKey());
        endDateValidCriteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_END_DATE, schoolStartDate);
        endDateCriteria.addOrCriteria(endDateEmptyCriteria);
        endDateCriteria.addOrCriteria(endDateValidCriteria);
        currentYearCriteria.addAndCriteria(endDateCriteria);

        QueryByCriteria query = new QueryByCriteria(StudentProgramParticipation.class, currentYearCriteria);
        m_studentProgramParticipation =
                getBroker().getGroupedCollectionByQuery(query, StudentProgramParticipation.COL_STUDENT_OID, 32);
    }
}
