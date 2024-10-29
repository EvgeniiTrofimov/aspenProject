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
package com.x2dev.procedures.statereporting.tn;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.*;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNStudentEnrollmentSpan;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNStudentHistoryHelper;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNStudentMultiYearHelper;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNStudentScheduleSpan;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.ScheduleTermDate;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisSchoolCalendarDate;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.beans.StudentScheduleChange;
import com.x2dev.sis.model.business.schedule.ScheduleManager;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.DateAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.lang.reflect.Method;
import java.util.*;
import java.util.logging.Level;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class TNStateReportData.
 */
public class TNStateReportData extends StateReportData {

    /**
     * The Class Pair.
     *
     * @param <L> the generic type
     * @param <R> the generic type
     */
    public static class Pair<L, R> {
        /**
         * Of.
         *
         * @param <L> the generic type
         * @param <R> the generic type
         * @param left L
         * @param right R
         * @return Pair
         */
        public static <L, R> Pair<L, R> of(L left, R right) {
            return new Pair(left, right);
        }

        private final L m_left;
        private final R m_right;

        /**
         * Instantiates a new pair.
         *
         * @param left L
         * @param right R
         */
        private Pair(L left, R right) {
            m_left = left;
            m_right = right;
        }

        /**
         * Gets the left.
         *
         * @return l
         */
        public L getLeft() {
            return m_left;
        }

        /**
         * Gets the right.
         *
         * @return r
         */
        public R getRight() {
            return m_right;
        }
    }

    private static final String NO_PERFMON4J_INSTRUMENTATION = "";

    /**
     * Return the related bean based on the bean path. Return null if not found or empty
     *
     * @param bean X2BaseBean
     * @param beanPath String
     * @return X 2 base bean
     */
    static public X2BaseBean getRelatedBean(X2BaseBean bean, String beanPath) {
        X2BaseBean relatedBean = null;
        String methodName = "get" + beanPath.substring(0, 1).toUpperCase() + beanPath.substring(1);
        Method method;
        Class myClass = bean.getClass();
        while (myClass != Object.class) {
            try {
                method = myClass.getDeclaredMethod(methodName, new Class[0]);
                relatedBean = (X2BaseBean) method.invoke(bean, new Object[0]);
                break;
            } catch (Exception e) {
                myClass = myClass.getSuperclass();
            }
        }
        return relatedBean;
    }

    /**
     * The Interface HasStudentRecordHelper.
     */
    public interface HasStudentRecordHelper {

        /**
         * Gets the current record.
         *
         * @return Student record helper
         */
        public StudentRecordHelper getCurrentRecord();
    }

    /**
     * Wrapper class to store unique records.
     *
     * @author
     */
    public class EntityRowKeys {

        private String[] m_keys;

        /**
         * Instantiates a new entity row keys.
         *
         * @param keys String[]
         */
        public EntityRowKeys(String[] keys) {
            this.m_keys = keys;
        }

        /**
         * Equals.
         *
         * @param obj Object
         * @return true, if successful
         * @see java.lang.Object#equals(java.lang.Object)
         */
        @Override
        public boolean equals(Object obj) {
            boolean result = false;
            if (obj instanceof EntityRowKeys) {
                EntityRowKeys thatEntityRowKeys = (EntityRowKeys) obj;
                result = Arrays.equals(thatEntityRowKeys.m_keys, m_keys);
            }
            return result;
        }

        /**
         * Hash code.
         *
         * @return int
         * @see java.lang.Object#hashCode()
         */
        @Override
        public int hashCode() {
            return Arrays.hashCode(m_keys);
        }

        /**
         * To string.
         *
         * @return String
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            StringBuilder builder = new StringBuilder();
            builder.append("[");
            for (int i = 0; i < m_keys.length; ++i) {
                builder.append(m_keys[i].toString());
                if (i < m_keys.length - 1) {
                    builder.append(", ");
                }
            }
            builder.append("]");
            return builder.toString();
        }
    }

    /**
     * The Class RetrieveEnglishLangBG.
     */
    public class RetrieveEnglishLangBG extends RetrieveStudentContextAttribute {
        public static final String CALC_ID_BG = "STD_CONTEXT_BG";

        /**
         * Instantiates a new retrieve english lang BG.
         *
         * @param helper TNEnrollmentHelper
         */
        public RetrieveEnglishLangBG(TNEnrollmentHelper helper) {
            super(helper);
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.x2dev.procedures.statereporting.tn.TNStateReportData.RetrieveStudentContextAttribute#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String value = (String) super.getFieldValue(data, entity, field);

            if (!isReport() && StringUtils.isEmpty(value)) {
                value = "E";
            }
            return value;
        }
    }

    /**
     * Field retriever for Instructional program field.
     * Can be used only with SisStudent beans.
     */
    public class RetrieveInstProgramStdBean implements FieldRetriever {
        public static final String TN_CALC_INSTPGM_ID = "TN_INSTRPGM_STD";
        private static final String INSTPGM_EMPTY_ERROR = "Instructional Program is empty.";

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
            TNStateReportData tnData = (TNStateReportData) data;
            SisStudent student = (SisStudent) entity.getBean();

            String calendarCode = getStdCalendarId(student);
            SisSchool school = getStdSchool(student);

            Object value = null;
            String message = null;
            if (entity instanceof HasStudentRecordHelper) {
                StudentRecordHelper record = ((HasStudentRecordHelper) entity).getCurrentRecord();
                value = record.getInstrProgram();

                message = "Value not specified. Set to " + field.getDefaultValue() + " by default. Context:" +
                        tnData.getCurrentContext().getContextId() + ". School:" + school.getName() +
                        ". Exit date: " + record.getExitDate();
            } else {
                value = getInstProgramStdBean(calendarCode, school);

                message = "Value not specified. Set to " + field.getDefaultValue() + " by default. Context:" +
                        tnData.getCurrentContext().getContextId() + ". School:" + school.getName() +
                        ". Calendar code:" + calendarCode;
            }
            if (value == null) {
                StateReportValidationError error =
                        new StateReportValidationError(entity, field, INSTPGM_EMPTY_ERROR, message);
                entity.addRetrievalError(field.getFieldId(), error);
            }
            return value;
        }
    }

    /**
     * Field retriever for names. Truncate by field max length.
     *
     */
    public class RetrieveTruncatedNames implements FieldRetriever {
        public static final String TN_CALC_NAME = "TN_TRUNCATED_NAME";

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
            String value = (String) getProperty(entity.getBean(), field.getBeanPath());
            int maxLength = field.getMaxLength();
            if (!StringUtils.isEmpty(value) && value.length() > maxLength) {
                value = value.substring(0, maxLength);
            }
            return value;
        }
    }

    /**
     * The Class RetrieveStudentContextAttribute.
     */
    public class RetrieveStudentContextAttribute implements FieldRetriever {
        public static final String CALC_ID = "STD_CONTEXT_ATTR";

        private TNEnrollmentHelper m_helper;
        private Pattern m_aliasPattern;

        /**
         * Instantiates a new retrieve student context attribute.
         *
         * @param helper TNEnrollmentHelper
         */
        public RetrieveStudentContextAttribute(TNEnrollmentHelper helper) {
            super();
            this.m_helper = helper;
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
            Object returnValue = null;

            String[] beanPaths = replaceAlias(field.getBeanPath()).split("\\.");
            X2BaseBean bean = entity.getBean();
            if (beanPaths.length > 1) {
                for (int i = 0; i < beanPaths.length - 1 && bean != null; ++i) {
                    String beanPath = beanPaths[i];
                    if (Student.class.isAssignableFrom(bean.getClass())
                            && getStudentMultiYearHelper().isContextOverride()) {
                        bean = m_helper.getStudentRelationByBeanPath((Student) bean, beanPath);
                    } else {
                        bean = getRelatedBean(bean, beanPath);
                    }
                }
            }
            if (bean != null) {
                String beanPath = beanPaths[beanPaths.length - 1];
                if (Student.class.isAssignableFrom(bean.getClass())) {
                    returnValue = m_helper.getStudentValueByBeanPath((Student) bean, beanPath);
                } else {
                    returnValue = getProperty(bean, beanPath);
                }
            } else {
                returnValue = getProperty(bean, field.getBeanPath());
            }
            return returnValue;
        }

        /**
         * Replace alias.
         *
         * @param path String
         * @return String
         */
        private String replaceAlias(String path) {
            String value = path;
            if (m_aliasPattern == null) {
                m_aliasPattern = Pattern.compile("\\[.*\\]");
            }
            Matcher matcher = m_aliasPattern.matcher(path);
            String alias = matcher.find() ? matcher.group() : null;
            if (alias != null) {
                String beanPath = translateAliasToJavaName(alias.replace("[", "").replace("]", ""), false);
                if (beanPath != null) {
                    value = path.replace(alias, beanPath);
                }
            }
            return value;
        }

    }

    /**
     * This helper class is used to provide the extracts with information about students.
     */
    public class StudentRecordHelper {
        private String m_debug;
        private PlainDate m_enrollmentDate;
        private String m_enrollmentReason;
        private String m_withdrawReason;
        private PlainDate m_exitDate;
        private boolean m_isPrimary;
        private String m_instrProgram;
        private String m_schoolId;
        private String m_serviceDistrictId;
        private String m_serviceSchoolId;
        private int m_yog;

        /**
         * Instantiates a new student record helper.
         */
        public StudentRecordHelper() {
            m_isPrimary = true;
        }

        /**
         * Copy.
         *
         * @return StudentRecordHelper
         */
        public StudentRecordHelper copy() {
            StudentRecordHelper record = new StudentRecordHelper();

            record.m_debug = m_debug;
            record.m_enrollmentDate = m_enrollmentDate;
            record.m_enrollmentReason = m_enrollmentReason;
            record.m_withdrawReason = m_withdrawReason;
            record.m_exitDate = m_exitDate;
            record.m_isPrimary = m_isPrimary;
            record.m_instrProgram = m_instrProgram;
            record.m_schoolId = m_schoolId;
            record.m_serviceDistrictId = m_serviceDistrictId;
            record.m_serviceSchoolId = m_serviceSchoolId;
            record.m_yog = m_yog;

            return record;
        }

        /**
         * Gets the debug.
         *
         * @return String
         */
        public String getDebug() {
            return m_debug;
        }

        /**
         * Gets the enroll date.
         *
         * @return Plain date
         */
        public PlainDate getEnrollDate() {
            return m_enrollmentDate;
        }

        /**
         * Gets the enroll reason.
         *
         * @return String
         */
        public String getEnrollReason() {
            return m_enrollmentReason;
        }

        /**
         * Gets the exit date.
         *
         * @return Plain date
         */
        public PlainDate getExitDate() {
            return m_exitDate;
        }

        /**
         * Gets the checks if is primary.
         *
         * @return boolean
         */
        public boolean getIsPrimary() {
            return m_isPrimary;
        }

        /**
         * Gets the instr program.
         *
         * @return String
         */
        public String getInstrProgram() {
            return m_instrProgram;
        }

        /**
         * Gets the school id.
         *
         * @return String
         */
        public String getSchoolId() {
            return m_schoolId;
        }

        /**
         * Gets the service district id.
         *
         * @return String
         */
        public String getServiceDistrictId() {
            return m_serviceDistrictId;
        }

        /**
         * Gets the service school id.
         *
         * @return String
         */
        public String getServiceSchoolId() {
            return m_serviceSchoolId;
        }

        /**
         * Gets the withdraw reason.
         *
         * @return String
         */
        public String getWithdrawReason() {
            return m_withdrawReason;
        }

        /**
         * Gets the yog.
         *
         * @return int
         */
        public int getYog() {
            return m_yog;
        }

        /**
         * Sets the debug.
         *
         * @param value void
         */
        public void setDebug(String value) {
            m_debug = value;
        }

        /**
         * Sets the enroll date.
         *
         * @param value void
         */
        public void setEnrollDate(PlainDate value) {
            m_enrollmentDate = value;
        }

        /**
         * Sets the enroll reason.
         *
         * @param value void
         */
        public void setEnrollReason(String value) {
            m_enrollmentReason = value;
        }

        /**
         * Sets the exit date.
         *
         * @param value void
         */
        public void setExitDate(PlainDate value) {
            m_exitDate = value;
        }

        /**
         * Sets the instr program.
         *
         * @param value void
         */
        public void setInstrProgram(String value) {
            m_instrProgram = value;
        }

        /**
         * Sets the checks if is primary.
         *
         * @param value void
         */
        public void setIsPrimary(boolean value) {
            m_isPrimary = value;
        }

        /**
         * Sets the school id.
         *
         * @param value void
         */
        public void setSchoolId(String value) {
            if (value == null) {
                value = "";
            }

            m_schoolId = value;
        }

        /**
         * Sets the service district id.
         *
         * @param value void
         */
        public void setServiceDistrictId(String value) {
            if (value == null) {
                value = "";
            }
            m_serviceDistrictId = value.length() < 3 ? StringUtils.padLeft(value, 3, '0') : value;
        }

        /**
         * Sets the service school id.
         *
         * @param value void
         */
        public void setServiceSchoolId(String value) {
            if (value == null) {
                value = "";
            }

            m_serviceSchoolId = value.length() < 4 ? StringUtils.padLeft(value, 4, '0') : value;
        }

        /**
         * Sets the withdraw reason.
         *
         * @param value void
         */
        public void setWithdrawReason(String value) {
            m_withdrawReason = value;
        }

        /**
         * Sets the yog.
         *
         * @param value void
         */
        public void setYog(int value) {
            m_yog = value;
        }

        /**
         * To string.
         *
         * @return String
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            StringBuilder output = new StringBuilder();
            output.append("Interval:{");
            output.append(m_enrollmentDate);
            output.append(",");
            output.append(m_exitDate);
            output.append("} Reasons:{");
            output.append(m_enrollmentReason);
            output.append(",");
            output.append(m_withdrawReason);
            output.append("} School:");
            output.append(m_schoolId);
            output.append(" InstrPgm:");
            output.append(m_instrProgram);
            output.append(" YOG:");
            output.append(m_yog);
            output.append(" Serv Dist:");
            output.append(m_serviceDistrictId);
            output.append(" Serv Sch:");
            output.append(m_serviceSchoolId);
            output.append("\n");

            return output.toString();
        }

    }

    /**
     * Validate value to be not blank.
     */
    public class ValidateEmptyValue implements FieldValidator {
        public static final String VAL_ID = "VAL_EMPTY_VALUE";
        public static final String ERROR_MISSING_VALUE_KEY = "error.state.report.missingvalue";

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
            Collection errors = new ArrayList<StateReportValidationError>();
            if (StringUtils.isEmpty(value)) {
                String message = LocalizationCache.getMessages(getBroker().getPersistenceKey())
                        .getMessage(ERROR_MISSING_VALUE_KEY);
                StateReportValidationError error = new StateReportValidationError(entity, field, message, "");
                errors.add(error);
            }
            return errors;
        }
    }

    protected static final String ALIAS_BASE_SCHOOL = "all-enr-BaseSchool";
    protected static final String ALIAS_DISTRICT_SVC = "DOE DISTRICT SERVICE";
    protected static final String ALIAS_ENR_CAL_CODE = "all-enr-StudentCalendar";
    protected static final String ALIAS_INSTR_PGM = "DOE INSTRUCTIONAL PROGRAM";
    protected static final String ALIAS_OVERRIDE_CHANGE_DATE = "DOE OVERRIDE CHANGE DATE";
    protected static final String ALIAS_SCHOOL_SVC = "DOE SCHOOL  SERVICE";
    protected static final String ALIAS_STATE_COURSE_CODE = "DOE SDE COURSE CODE";
    protected static final String ALIAS_STATE_SCHOOL_ID = "DOE SCHOOL STATE ID";

    protected static final String CALENDAR_ID_STANDARD = "Standard";

    protected static final String DEFAULT_CALENDAR_CODE = "Standard";

    protected static final String KEY_DELIMITER = ":";

    protected static final String PARAM_BYPASS_DUP_SECT_TEST = "bypassDupSectionTest";
    protected static final String PARAM_FILE_SEQ = "fileSeq";
    protected static final String PARAM_PROCESS_BASE_SCHOOL = "processBaseSchool";
    protected static final String PARAM_PROGRAM_CODE = "programCode";
    protected static final String PARAM_SCHOOL_CALENDARS = "schoolCalendars";
    protected static final String PARAM_SCHOOL_YEAR_CONTEXT = "contextOid";
    protected static final String PARAM_SUPPRESS_HEADING = "suppressHeading";
    protected static final String PARAM_SUPPRESS_TRAILER = "suppressTrailer";
    protected static final String PARAM_REPORT_DATE = "reportDate";

    private static final String ENROLL_OUT_OF_STATE = "E1";
    private static final String ENROLL_TRANSFERRED = "TR";
    private static final String ENROLL_REGULAR = "E";
    private static final String ENROLL_UNDER_ACHIEVING = "TC";
    private static final String ENROLL_CONTINUE_UNDER_ACHIEVING = "EC";
    private static final String EXPORT_TYPE_PREFIX = "-V";

    private static final String WITHDRAW_SAME_SYSTEM = "04";

    /**
     * Make calendar lookup key.
     *
     * @param orgOid String
     * @param sklOid String
     * @param calendarCode String
     * @return String
     */
    public static String makeCalendarLookupKey(String orgOid, String sklOid, String calendarCode) {
        return orgOid + KEY_DELIMITER + sklOid + KEY_DELIMITER + calendarCode;
    }

    public HashMap<String, String> m_calendarOids;
    public String m_contextOid;

    public String m_fieldBaseSchool;
    public String m_fieldDistrictSvc;
    public String m_fieldSchoolSvc;
    public String m_fieldStateCourseCode;
    public String m_fieldStateSchoolId;
    public PlainDate m_reportDate;
    public PlainDate m_spanEndDate;
    public PlainDate m_spanStartDate;

    DateAsStringConverter m_dateConverter;

    private boolean m_addOnFirstDayWithdrew;
    private DistrictSchoolYearContext m_context;
    private String m_efdOverrideOid;
    private HashSet<EntityRowKeys> m_entityKeys;
    private int m_exportVersion;
    private String m_fieldOverrideChangeDate;
    private Map<String, PlainDate> m_firstDatesOfSchools;
    private TNHeadingTrailing m_headingTrailing;
    private boolean m_initialized = false;
    private boolean m_isReport;
    private List<String> m_keysBeanPaths;
    private List<String> m_keyNames;
    private TNStudentMultiYearHelper m_stdMultiYearHelper;
    private String m_programCode;
    /**
     * A map of maps of reference code.
     * The outer map is indexed by the reference table OID. It contains maps of reference codes.
     * The inner map is indexed by the reference code. It contains the RefrenceCode bean for the
     * code.
     */
    private Map<String, Map<String, ReferenceCode>> m_refTableMap = null;
    private ScheduleManager m_scheduleMgr;
    private Map<String, List<StudentRecordHelper>> m_studentHelperMap;
    private Set<String> m_uniqueKeys;


    /**
     * Increment the rowcount for this entity type.
     *
     * @param rowCount int
     */
    public void addEntityRowsCount(int rowCount) {
        m_headingTrailing.incementCount(m_programCode, rowCount);
    }

    /**
     * Lookup a map of all (enabled and disabled) reference codes for a reference table oid.
     * Cache the results for later use.
     * For TN version, include disabled codes.
     *
     * @param referenceTableOid String
     * @return Map<String, ReferenceCode>
     */
    @Override
    public Map<String, ReferenceCode> getReferenceCodes(String referenceTableOid) {
        Map<String, ReferenceCode> codeMap = null;
        if (m_refTableMap == null) {
            m_refTableMap = new HashMap<String, Map<String, ReferenceCode>>();
        }

        if (m_refTableMap.containsKey(referenceTableOid)) {
            codeMap = m_refTableMap.get(referenceTableOid);
        } else {
            codeMap = new HashMap<String, ReferenceCode>();
            ReferenceTable refTable =
                    (ReferenceTable) getBroker().getBeanByOid(ReferenceTable.class, referenceTableOid);
            if (refTable != null) {
                codeMap = getAllCodesMap(refTable);
            }
            m_refTableMap.put(referenceTableOid, codeMap);
        }

        return codeMap;
    }

    /**
     * Gets the efd oid.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getEfdOid()
     */
    @Override
    public String getEfdOid() {
        return m_efdOverrideOid == null ? super.getEfdOid() : m_efdOverrideOid;
    }

    /**
     * The export version for this export. This is only used for export versions greater than the
     * original export version.
     *
     * @return int
     */
    public int getExportVersion() {
        return m_exportVersion;
    }

    /**
     * Gets the heading.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getHeading()
     */
    @Override
    public String getHeading() {
        String heading = null;
        if (m_initialized && getSetupErrors().size() == 0) {
            Boolean param = (Boolean) this.getParameter(PARAM_SUPPRESS_HEADING);
            if (param == null || !param.booleanValue()) {
                if (m_headingTrailing != null) {
                    heading = m_headingTrailing.getHeading();
                }
            }
        }

        return heading;
    }

    /**
     * Return instructional program code.
     *
     * @param calendarCode String
     * @param school SisSchool
     * @return Object
     */
    public Object getInstProgramStdBean(String calendarCode, SisSchool school) {
        Object value = null;

        String key = makeCalendarLookupKey(m_contextOid, school.getOid(), calendarCode);
        if (m_calendarOids.containsKey(key)) {
            value = m_calendarOids.get(key);
        }
        return value;
    }

    /**
     * Gets the current context.
     *
     * @return District school year context
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getCurrentContext()
     */
    @Override
    public DistrictSchoolYearContext getCurrentContext() {
        if (m_context == null) {
            if (!StringUtils.isEmpty(m_contextOid)) {
                m_context = (DistrictSchoolYearContext) getBroker().getBeanByOid(DistrictSchoolYearContext.class,
                        m_contextOid);
            } else {
                m_context = super.getCurrentContext();
                m_contextOid = m_context.getOid();
            }
        }
        return m_context;
    }

    /**
     * Gets the key field ids.
     *
     * @return the ids of the key fields
     */
    public List<String> getKeyFieldIds() {
        if (m_keyNames == null) {
            m_keyNames = new ArrayList();
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(
                    ExportFormatField.REL_DEFINITION + PATH_DELIMITER + ExportFormatDefinition.COL_PROCEDURE_ID,
                    this.getProcedureId());
            criteria.addEqualTo(ExportFormatField.COL_KEY_IND, Boolean.TRUE);
            QueryByCriteria query = new QueryByCriteria(ExportFormatField.class, criteria);
            query.addOrderBy(ExportFormatField.COL_POSITION, true);
            QueryIterator fieldIterator = getBroker().getIteratorByQuery(query);
            try {
                while (fieldIterator.hasNext()) {
                    ExportFormatField field = (ExportFormatField) fieldIterator.next();
                    m_keyNames.add(field.getName());
                }
            } finally {
                fieldIterator.close();
            }
        }
        return m_keyNames;
    }

    /**
     * Method to get keys' java names for the current Export Format Definition.
     *
     * @return List
     */
    public List<String> getKeysBeanPaths() {
        if (m_keysBeanPaths == null) {
            String efdOid = getEfdOid();

            X2Criteria effCriteria = new X2Criteria();
            effCriteria.addEqualTo(ExportFormatField.COL_DEFINITION_OID, efdOid);
            effCriteria.addEqualTo(ExportFormatField.COL_KEY_IND, BooleanAsStringConverter.TRUE);

            String keyColumn = ExportFormatField.COL_NAME;
            QueryByCriteria query = new QueryByCriteria(ExportFormatField.class, effCriteria);
            query.addOrderByAscending(ExportFormatField.COL_POSITION);
            m_keysBeanPaths = new LinkedList();
            for (Object obj : getBroker().getCollectionByQuery(query)) {
                ExportFormatField bean = (ExportFormatField) obj;
                m_keysBeanPaths.add((String) bean.getFieldValueByBeanPath(keyColumn));
            }
        }

        return m_keysBeanPaths;
    }

    /**
     * Return student's calendar code.
     *
     * @param student SisStudent
     * @return String
     */
    public String getStdCalendarId(SisStudent student) {
        String calendarCode =
                (String) getStudentMultiYearHelper().getFieldValueByBeanPath(student, SisStudent.COL_CALENDAR_CODE);
        return !StringUtils.isEmpty(calendarCode) ? calendarCode : CALENDAR_ID_STANDARD;
    }

    /**
     * Gets the student helper map.
     *
     * @return Map
     */
    /*
     * get the student helper map
     */
    public Map<String, List<StudentRecordHelper>> getStudentHelperMap() {
        return m_studentHelperMap;
    }

    /**
     * Get the multi year helper for this data source. If base class of query is Student, use the
     * criteria of data as
     * criteria of TNStudentMultiYearHelper instance.
     *
     * @return TN student multi year helper
     */
    public TNStudentMultiYearHelper getStudentMultiYearHelper() {
        if (m_stdMultiYearHelper == null) {
            m_stdMultiYearHelper = new TNStudentMultiYearHelper(getOrganization(), getCurrentContext(), getBroker());
            if (getBeanClass() != null && getQuery() != null
                    && Student.class.isAssignableFrom(getQuery().getBaseClass())) {
                m_stdMultiYearHelper.setCriteria(getQuery().getCriteria());
            }
        }

        return m_stdMultiYearHelper;
    }

    /**
     * Gets the trailer.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getTrailer()
     */
    @Override
    public String getTrailer() {
        String trailer = null;
        if (m_initialized && getSetupErrors().size() == 0) {
            Boolean param = (Boolean) this.getParameter(PARAM_SUPPRESS_TRAILER);
            if (param == null || !param.booleanValue()) {
                if (m_headingTrailing != null) {
                    trailer = m_headingTrailing.getTrailer();
                }
            }
        }

        return trailer;
    }

    /**
     * Checks if this run is a validation report or export.
     *
     * @return true, if is report
     */
    public boolean isReport() {
        return m_isReport;
    }

    /**
     * Checks if row is unique.
     *
     * @param keys EntityRowKeys
     * @return true, if is unique entity row
     */
    public boolean isUniqueEntityRow(EntityRowKeys keys) {
        if (m_entityKeys == null) {
            m_entityKeys = new HashSet<EntityRowKeys>();
        }
        boolean returnValue = m_entityKeys.contains(keys) ? false : m_entityKeys.add(keys);
        return returnValue;
    }

    /**
     * The export version for this export. This is only used for export versions greater than the
     * original export version.
     *
     * @param version void
     */
    public void setExportVersion(int version) {
        ExportFormatDefinition efd =
                (ExportFormatDefinition) getBroker().getBeanByOid(ExportFormatDefinition.class, this.getEfdOid());
        if (efd != null) {
            String id = efd.getId() + EXPORT_TYPE_PREFIX + version;
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(ExportFormatDefinition.COL_PROCEDURE_ID, id);
            QueryByCriteria query = new QueryByCriteria(ExportFormatDefinition.class, criteria);
            efd = (ExportFormatDefinition) getBroker().getBeanByQuery(query);
            if (efd != null) {
                m_efdOverrideOid = efd.getOid();
            }
        }
        m_exportVersion = version;
    }

    /**
     * Configure the start and end date that given based on the student schedule span.
     *
     * @param studentScheduleSpan TNStudentScheduleSpan
     * @param startDate PlainDate
     * @param endDate PlainDate
     * @return boolean true if the student schedule span within the given start and end date range
     */
    protected boolean configureStartEndDate(TNStudentScheduleSpan studentScheduleSpan,
                                            PlainDate startDate,
                                            PlainDate endDate) {
        boolean withinSpan = false;

        if (studentScheduleSpan.getSection() != null &&
                studentScheduleSpan.getSection().getSchedule() != null &&
                studentScheduleSpan.getSection().getSchedule().getSchool() != null &&
                studentScheduleSpan.getSection().getSchedule().getSchoolOid().equals(getSchool().getOid())) {
            // First filter if the start date, and the span entry date are null
            if (startDate != null && studentScheduleSpan.getEntryDate() != null &&
            // Then filter if the span date is within the given start and end date
                    (!startDate.after(studentScheduleSpan.getExitDate()) || endDate == null
                            || !endDate.before(studentScheduleSpan.getEntryDate()))
                    &&
                    // Or filter if the student hasn't left the district
                    (studentScheduleSpan.getExitDate() != null
                            && !studentScheduleSpan.getEntryDate().equals(studentScheduleSpan.getExitDate()))) {
                withinSpan = true;

                // Get the master schedule, date converter, and override field change date
                MasterSchedule mstSched = studentScheduleSpan.getSection();

                // Check the term start and end dates
                PlainDate termStart = null;
                PlainDate termEnd = null;
                Collection<ScheduleTermDate> termDates = mstSched.getScheduleTerm().getScheduleTermDates();
                for (ScheduleTermDate termDate : termDates) {
                    if (termStart == null || termStart.after(termDate.getStartDate())) {
                        termStart = termDate.getStartDate();
                    }
                    if (termEnd == null || termEnd.before(termDate.getEndDate())) {
                        termEnd = termDate.getEndDate();
                    }
                }

                // Check the override start and end date. Use these dates if they are not null.
                // Otherwise, use the
                PlainDate overrideStartDate = getOverrideDate(studentScheduleSpan.getEntryChange());

                if (overrideStartDate != null && (startDate == null || !startDate.after(overrideStartDate))) {
                    startDate = overrideStartDate; // If the overrideStartDate is not null, then use
                                                   // that date
                } else if (startDate == null || startDate.before(studentScheduleSpan.getEntryDate())) {
                    startDate = studentScheduleSpan.getEntryDate();
                }

                PlainDate overrideEndDate = getOverrideDate(studentScheduleSpan.getExitChange());

                if (overrideEndDate != null && (endDate == null || endDate.after(overrideEndDate))) {
                    endDate = overrideEndDate; // If the overrideEndDate is not null, then use that
                                               // date
                } else if (endDate == null || endDate.after(studentScheduleSpan.getExitDate())) {
                    endDate = studentScheduleSpan.getExitDate();
                    if (studentScheduleSpan.getExitChange() != null &&
                            termEnd != null && endDate.before(termEnd) &&
                            termStart != null && endDate.after(termStart)) {
                        Calendar cal = Calendar.getInstance();
                        cal.setTime(endDate);
                        cal.add(Calendar.DATE, -1);
                        endDate = new PlainDate(cal.getTime());
                    }
                }
            }
        }

        // Assign the span date
        if (startDate != null) {
            m_spanStartDate = startDate;
        }
        if (endDate != null) {
            m_spanEndDate = endDate;
        }

        return withinSpan;
    }

    /**
     * Determine the enrollment date that should be used as the start date for
     * this student. The default case is the later of the first in-session date for the school
     * and the span first active date.
     *
     * @param student SisStudent
     * @param span TNStudentEnrollmentSpan
     * @return PlainDate
     */
    protected PlainDate determineEnrollmentDate(SisStudent student, TNStudentEnrollmentSpan span) {
        String calendarCode = getStdCalendarId(student);
        String key = calendarCode + span.getSchool().getOid();

        if (m_firstDatesOfSchools == null) {
            m_firstDatesOfSchools = new HashMap<String, PlainDate>();
        }

        PlainDate firstInSessionDate = null;
        if (m_firstDatesOfSchools.containsKey(key)) {
            firstInSessionDate = m_firstDatesOfSchools.get(key);
        } else {
            firstInSessionDate = getFirstCalendarDay(span.getSchool(), calendarCode);
            if (firstInSessionDate == null) {
                // try to use most common calendar
                if (span.getSchool().getActiveSchedule() != null) {
                    if (m_scheduleMgr == null) {
                        m_scheduleMgr = new ScheduleManager(getBroker());
                    }
                    calendarCode = m_scheduleMgr.getMostCommonCalendar(span.getSchool().getActiveSchedule(), null);
                    firstInSessionDate = getFirstCalendarDay(span.getSchool(), calendarCode);
                }
            }
            if (firstInSessionDate == null) {
                firstInSessionDate = m_context.getStartDate();
                AppGlobals.getLog().log(Level.WARNING, "First In-Session date not found for school " +
                        span.getSchool().getName() + " and calendar code " + calendarCode);
            }
            m_firstDatesOfSchools.put(key, firstInSessionDate);
        }


        PlainDate enrDate = span.getFirstActiveDate();
        if (enrDate != null && enrDate.after(firstInSessionDate)) {
            firstInSessionDate = enrDate;
        }

        return firstInSessionDate;
    }

    /**
     * Determine the withdrawal date that should be used as the exit date for
     * this student. The default case is the enrollment date from the first inactive enrollment.
     *
     * @param student SisStudent
     * @param span TNStudentEnrollmentSpan
     * @return PlainDate
     */
    protected PlainDate determineWithdrawalDate(SisStudent student,
                                                TNStudentEnrollmentSpan span) {
        return span.getLastActiveDate();
    }

    /**
     * method for getting calendars for all schools for given context.
     *
     * @param contextOid String
     * @return void
     */
    protected void getCalendarsForContextOid(String contextOid) {
        if (m_calendarOids == null) {
            m_calendarOids = new HashMap<String, String>();
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(SchoolCalendar.COL_DISTRICT_CONTEXT_OID, contextOid);

            QueryByCriteria query = new QueryByCriteria(SchoolCalendar.class, criteria);
            QueryIterator iter = getBroker().getIteratorByQuery(query);

            try {
                while (iter.hasNext()) {
                    SchoolCalendar calendar = (SchoolCalendar) iter.next();

                    String calendarCode = calendar.getCalendarId();
                    String schoolOid = calendar.getSchoolOid();
                    String key = makeCalendarLookupKey(contextOid, schoolOid, calendarCode);

                    if (!m_calendarOids.containsKey(key)) {
                        String value = (String) calendar.getFieldValueByAlias(ALIAS_INSTR_PGM);
                        String instrPgm = lookupReferenceCodeByAlias(ALIAS_INSTR_PGM, value,
                                ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                        m_calendarOids.put(key, instrPgm);
                    }
                }
            } finally {
                iter.close();
            }
        }

        if (m_calendarOids.isEmpty()) {
            addSetupError("No Data", "School calendar is not defined for any school in the current contextOid ");
        }
    }

    /**
     * Returns inner enrollments of span ordered by descending by date of enrollment. If there are
     * no inner enrollments,
     * returns empty Set.
     *
     * @param span TNStudentEnrollmentSpan
     * @return Sets the
     */
    protected Set<StudentEnrollment> getDescOrderInnerEnrollments(TNStudentEnrollmentSpan span) {
        Set<StudentEnrollment> descOrderedEnr = new TreeSet<StudentEnrollment>(new Comparator<StudentEnrollment>() {
            @Override
            public int compare(StudentEnrollment o1, StudentEnrollment o2) {
                return o2.getEnrollmentDate().compareTo(o1.getEnrollmentDate());
            }
        });

        List<StudentEnrollment> enrollments = span.getEnrollments();

        if ((span.getFirstInactiveEnrollment() != null && enrollments.size() > 2) ||
                (span.getFirstInactiveEnrollment() == null && enrollments.size() > 1)) {
            for (StudentEnrollment enrollment : enrollments) {
                if (!enrollment.equals(span.getFirstActiveEnrollment()) &&
                        !enrollment.equals(span.getFirstInactiveEnrollment()) &&
                        (!enrollment.getEnrollmentDate().before(span.getFirstActiveEnrollment().getEnrollmentDate()) &&
                                (span.getFirstInactiveEnrollment() == null ||
                                        !enrollment.getEnrollmentDate()
                                                .after(span.getFirstInactiveEnrollment().getEnrollmentDate())))) {
                    descOrderedEnr.add(enrollment);
                }
            }
        }
        return descOrderedEnr;
    }

    /**
     * Get override date.
     *
     * @param change StudentScheduleChange
     * @return Plain date
     */
    protected PlainDate getOverrideDate(StudentScheduleChange change) {
        PlainDate overrideDate = null;

        if (change != null) {
            try {
                overrideDate = (PlainDate) m_dateConverter
                        .parseSystemString((String) change.getFieldValueByBeanPath(m_fieldOverrideChangeDate));
            } catch (ClassCastException exp) {
                try {
                    overrideDate = (PlainDate) change.getFieldValueByBeanPath(m_fieldOverrideChangeDate);
                } catch (Exception finalExp) {
                    // Do nothing - date is NULL.
                }
            }
        }

        return overrideDate;
    }

    /**
     * Return student's school.
     *
     * @param student SisStudent
     * @return Sis school
     */
    protected SisSchool getStdSchool(SisStudent student) {
        return getStudentMultiYearHelper().getSchool(student);
    }

    /**
     * initialize the student helper.
     *
     * @param helper TNStudentHistoryHelper
     * @param enrollment StudentEnrollment
     * @param withdrawal StudentEnrollment
     * @param enrStartDate PlainDate
     * @param lastActiveDate PlainDate
     * @param school StudentSchool
     * @param yog int
     * @return Student record helper
     */
    protected StudentRecordHelper getStudentRecord(TNStudentHistoryHelper helper,
                                                   StudentEnrollment enrollment,
                                                   StudentEnrollment withdrawal,
                                                   PlainDate enrStartDate,
                                                   PlainDate lastActiveDate,
                                                   StudentSchool school,
                                                   int yog) {
        StudentRecordHelper record = newStudentRecordHelper();
        record.setEnrollDate(enrStartDate);
        record.setExitDate(lastActiveDate);
        String stateEnrollmentCode = null;
        StudentEnrollment entryEnrollment = enrollment;
        if (!StudentEnrollment.ENTRY.equals(enrollment.getEnrollmentType())) {
            entryEnrollment = helper.getEnrollmentForDate(enrollment.getStudentOid(), enrollment.getEnrollmentDate(),
                    StudentEnrollment.ENTRY);
        }
        if (entryEnrollment != null) {
            stateEnrollmentCode = lookupStateValue(StudentEnrollment.class, StudentEnrollment.COL_ENROLLMENT_CODE,
                    entryEnrollment.getEnrollmentCode());
        }
        if (entryEnrollment != null && entryEnrollment.getEnrollmentDate().before(m_context.getStartDate())) {
            if (ENROLL_OUT_OF_STATE.equals(stateEnrollmentCode) || ENROLL_TRANSFERRED.equals(stateEnrollmentCode)) {
                stateEnrollmentCode = ENROLL_REGULAR;
            } else if (ENROLL_UNDER_ACHIEVING.equals(stateEnrollmentCode)) {
                stateEnrollmentCode = ENROLL_CONTINUE_UNDER_ACHIEVING;
            }
        }
        record.setEnrollReason(stateEnrollmentCode);
        if (withdrawal != null) {
            String stateWithdrawalCode = lookupStateValue(StudentEnrollment.class,
                    StudentEnrollment.COL_ENROLLMENT_CODE, withdrawal.getEnrollmentCode());
            if (StringUtils.isEmpty(stateWithdrawalCode)
                    && StudentEnrollment.STATUS_CHANGE.equals(withdrawal.getEnrollmentType())) {
                stateWithdrawalCode = WITHDRAW_SAME_SYSTEM;
            }
            record.setWithdrawReason(stateWithdrawalCode);
        }
        record.setYog(yog);

        String schoolId = null;
        if (school == null) {
            schoolId = (String) enrollment.getSchool().getFieldValueByBeanPath(m_fieldStateSchoolId);
            record.setSchoolId(schoolId);
        } else {
            schoolId = (String) school.getSchool().getFieldValueByBeanPath(m_fieldStateSchoolId);
            record.setSchoolId(schoolId);
            record.setIsPrimary(false);
            if (school.getStartDate().after(record.getEnrollDate())) {
                record.setEnrollDate(school.getStartDate());
            }
            if (record.getExitDate() == null || record.getExitDate().after(school.getEndDate())) {
                record.setExitDate(school.getEndDate());
            }
        }

        String serviceSchoolId = (String) enrollment.getFieldValueByBeanPath(m_fieldSchoolSvc);
        if (StringUtils.isEmpty(serviceSchoolId)) {
            serviceSchoolId = schoolId;
        }
        record.setServiceSchoolId(serviceSchoolId);

        String serviceDistrictId = (String) enrollment.getFieldValueByBeanPath(m_fieldDistrictSvc);
        if (StringUtils.isEmpty(serviceDistrictId)) {
            serviceDistrictId = getOrganization().getId();
        }
        record.setServiceDistrictId(serviceDistrictId);

        return record;
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    /*
     * Initialize the data module. Initialize necessary working resources. Define query to load.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(java.util.Map,
     * com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    protected void initialize() throws X2BaseException {
        loadDefinitions(Arrays.asList(getProcedureId()));

        m_initialized = true;
        Integer fileSeq = (Integer) getParameter(PARAM_FILE_SEQ);
        int iSeq = fileSeq == null ? 1 : fileSeq.intValue();
        m_headingTrailing = new TNHeadingTrailing(getOrganization(), iSeq);
        m_contextOid = (String) getParameter(PARAM_SCHOOL_YEAR_CONTEXT);
        m_context = getCurrentContext();
        m_programCode = (String) getParameter(PARAM_PROGRAM_CODE);
        m_isReport = getParameter("isReport") == null ? false : ((Boolean) getParameter("isReport")).booleanValue();

        // subclasses might rewrite it
        m_reportDate = m_context.getEndDate();

        HashMap<String, FieldValidator> vals = new HashMap<String, FieldValidator>();
        vals.put(ValidateEmptyValue.VAL_ID, new ValidateEmptyValue());
        addValidators(vals);

        m_fieldBaseSchool = translateAliasToJavaName(ALIAS_BASE_SCHOOL, false);
        m_fieldSchoolSvc = translateAliasToJavaName(ALIAS_SCHOOL_SVC, true);
        m_fieldStateCourseCode = translateAliasToJavaName(ALIAS_STATE_COURSE_CODE, true);
        m_fieldStateSchoolId = translateAliasToJavaName(ALIAS_STATE_SCHOOL_ID, true);
        m_fieldDistrictSvc = translateAliasToJavaName(ALIAS_DISTRICT_SVC, true);
        m_fieldOverrideChangeDate = translateAliasToJavaName(ALIAS_OVERRIDE_CHANGE_DATE, true);
        m_dateConverter = (DateAsStringConverter) ConverterFactory.getConverterForClass(PlainDate.class.getName(),
                Locale.getDefault(), true);
    }

    /**
     * This method collects data by querying EnrollmentSpans and ProgramParticipation records for a
     * students in a query
     * and places it to the instance of StudentEnrollRecordHelper class. Later this data is used in
     * Entity class
     * when export is constructed
     *
     * @param helper TNStudentHistoryHelper
     * @param query QueryByCriteria
     */
    protected void initStudentHelperMap(TNStudentHistoryHelper helper, QueryByCriteria query) {
        boolean processBaseSchool = getParameter(PARAM_PROCESS_BASE_SCHOOL) == null ? false
                : ((Boolean) getParameter(PARAM_PROCESS_BASE_SCHOOL)).booleanValue();
        PlainDate earliestDate = m_reportDate;
        if (helper.getFirstAttendDate() != null && earliestDate.before(helper.getFirstAttendDate())) {
            earliestDate = helper.getFirstAttendDate();
        }
        Map<String, Collection<StudentSchool>> mapSecondarySchools = getSecondarySchools(query);
        m_studentHelperMap = new HashMap<String, List<StudentRecordHelper>>();

        QueryIterator iter = getBroker().getIteratorByQuery(query);
        try {
            while (iter.hasNext()) {
                SisStudent student = (SisStudent) iter.next();
                String studentOid = student.getOid();

                if (!m_studentHelperMap.containsKey(studentOid)) {
                    m_studentHelperMap.put(studentOid, new ArrayList<StudentRecordHelper>());
                }

                List<TNStudentEnrollmentSpan> spans = helper.getTNStudentEnrollmentSpans(student, true);

                for (TNStudentEnrollmentSpan nonSplitSpan : spans) {
                    List<TNStudentEnrollmentSpan> splitSpans = splitSpanByProgram(nonSplitSpan, student);
                    for (TNStudentEnrollmentSpan span : splitSpans) {
                        if (span.getSchool() != null && !StringUtils
                                .isEmpty((String) span.getSchool().getFieldValueByBeanPath(m_fieldStateSchoolId))) {
                            StudentEnrollment enrollment = span.getFirstActiveEnrollment();
                            StudentEnrollment withdrawal = span.getFirstInactiveEnrollment();
                            PlainDate enrStartDate = determineEnrollmentDate(student, span);
                            PlainDate enrEndDate = determineWithdrawalDate(student, span);
                            // Drop record if lastDate before enrollment startDate
                            if (helper.getLastDate().before(enrStartDate)) {
                                continue;
                            }

                            // Drop record if reportDate before enrollment startDate
                            if (!earliestDate.before(enrStartDate) &&
                                    (enrEndDate == null || enrStartDate.before(enrEndDate) ||
                                            ((enrStartDate.equals(enrEndDate) ||
                                                    (m_addOnFirstDayWithdrew && withdrawal != null
                                                            && enrStartDate.equals(withdrawal.getEnrollmentDate())))
                                                    &&
                                                    (withdrawal == null ||
                                                            !enrEndDate.equals(withdrawal.getEnrollmentDate()) ||
                                                            !"Y".equals(withdrawal.getEnrollmentType()))))) {
                                String instrProgram = getInstProgram(student, span);

                                StudentRecordHelper record =
                                        getStudentRecord(helper, enrollment, withdrawal, enrStartDate,
                                                enrEndDate, null, span.getYog());
                                record.setInstrProgram(instrProgram);
                                m_studentHelperMap.get(studentOid).add(record);
                                if (processBaseSchool && m_fieldBaseSchool != null) {
                                    StudentEnrollment entryEnrollment = helper.getEnrollmentForDate(studentOid,
                                            enrollment.getEnrollmentDate(), StudentEnrollment.ENTRY);
                                    if (entryEnrollment != null) {
                                        String value =
                                                (String) entryEnrollment.getFieldValueByBeanPath(m_fieldBaseSchool);
                                        if (!StringUtils.isEmpty(value)) {
                                            value = lookupStateValue(StudentEnrollment.class, m_fieldBaseSchool, value);
                                            if (!StringUtils.isEmpty(value)) {
                                                StudentRecordHelper baseRecord = record.copy();
                                                baseRecord.setIsPrimary(false);
                                                baseRecord.setSchoolId(value);
                                                if (baseRecord instanceof TNStudentEnrollmentData.StudentEnrollRecordHelper) {
                                                    ((TNStudentEnrollmentData.StudentEnrollRecordHelper) baseRecord)
                                                            .setInstrServiceTypeSecondary();
                                                }
                                                m_studentHelperMap.get(studentOid).add(baseRecord);
                                            }
                                        }
                                    }
                                }
                                if (mapSecondarySchools.containsKey(studentOid)) {
                                    for (StudentSchool school : mapSecondarySchools.get(studentOid)) {
                                        if (school.getStartDate() != null && school.getEndDate() != null
                                                && !enrStartDate.after(school.getEndDate())
                                                && (enrEndDate == null || !enrEndDate.before(school.getStartDate()))) {
                                            StudentRecordHelper recordForSec =
                                                    getStudentRecord(helper, enrollment, withdrawal,
                                                            enrStartDate, enrEndDate, school, span.getYog());
                                            recordForSec.setInstrProgram(instrProgram);
                                            /*
                                             * if student was previously enrolled in this school
                                             * this year,
                                             * use the enrollment reason from initial record.
                                             */
                                            if (recordForSec.getSchoolId() != null) {
                                                List<StudentRecordHelper> existingRecords =
                                                        m_studentHelperMap.get(studentOid);
                                                if (existingRecords != null && !existingRecords.isEmpty()) {
                                                    for (StudentRecordHelper existingRecord : existingRecords) {
                                                        if (existingRecord.getSchoolId() != null && recordForSec
                                                                .getSchoolId().equals(existingRecord.getSchoolId())) {
                                                            recordForSec
                                                                    .setEnrollReason(existingRecord.getEnrollReason());
                                                            break;
                                                        }
                                                    }
                                                }
                                            }
                                            m_studentHelperMap.get(studentOid).add(recordForSec);
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        } finally {
            iter.close();
        }
    }

    /**
     * Checks if row is unique.
     *
     * @param key String
     * @return true, if is unique key
     */
    protected boolean isUniqueKey(String key) {
        if (m_uniqueKeys == null) {
            m_uniqueKeys = new HashSet<String>();
        }
        return m_uniqueKeys.contains(key) ? false : m_uniqueKeys.add(key);
    }

    /**
     * Get a new student helper. Override this method to extend the helper
     *
     * @return StudentRecordHelper
     */
    protected StudentRecordHelper newStudentRecordHelper() {
        return new StudentRecordHelper();
    }

    /**
     * Set if students that have Withdrew on day of Entrance should be added to Student Helper Map.
     * False by default.
     *
     * @param addOnFirstDay void
     */
    protected void setOnFirstDayWithdrew(boolean addOnFirstDay) {
        m_addOnFirstDayWithdrew = addOnFirstDay;
    }

    /**
     * Returns new spans that are created as result of splitting of span by changed INSTR PROGRAM
     * during this span.
     * In the created spans may be changed only:
     * 1)FistActiveDate
     * 2)LastActiveDate
     * 3)FirstActiveEnrollment
     * 4)FirstInactiveEnrollment
     * Other fields still the same.
     *
     * @param span TNStudentEnrollmentSpan
     * @param student SisStudent
     * @return List
     */
    protected List<TNStudentEnrollmentSpan> splitSpanByProgram(TNStudentEnrollmentSpan span, SisStudent student) {
        TNStudentEnrollmentSpan oldSpan = span.getCopy();
        ArrayList<TNStudentEnrollmentSpan> splitSpans = new ArrayList<TNStudentEnrollmentSpan>();

        splitSpans.add(oldSpan);
        // If except first active enrollment and last active enrollment there is other enrollments
        // in
        // span and they contain changed calendar with changed appropriate INSTR PROGRAM, split span
        // by these enrollments.
        Set<StudentEnrollment> innerEnrollments = getDescOrderInnerEnrollments(oldSpan);
        if (!innerEnrollments.isEmpty()) {
            String oldCalendarID = getCalendarID(oldSpan.getFirstInactiveEnrollment(), student);
            String oldProgramKey = TNStateReportData.makeCalendarLookupKey(m_contextOid,
                    oldSpan.getFirstActiveEnrollment().getSchoolOid(), oldCalendarID);
            for (StudentEnrollment curEnrollment : innerEnrollments) {
                String newCalendarID = getCalendarID(curEnrollment, student);
                String newProgramKey = TNStateReportData.makeCalendarLookupKey(m_contextOid,
                        oldSpan.getFirstActiveEnrollment().getSchoolOid(), newCalendarID);

                if (// Skip enrollment (don't split)
                    // if INSTR PROGRAM of current record and INSTR PROGRAM of current enrollment
                    // both are null
                (oldCalendarID == null && newCalendarID == null) ||

                // if INSTR PROGRAM of current record and INSTR PROGRAM of current enrollment are
                // the same
                        (m_calendarOids != null && m_calendarOids.get(oldProgramKey) != null &&
                                m_calendarOids.get(oldProgramKey).equals(m_calendarOids.get(newProgramKey)))) {
                    continue;
                }

                // Otherwise split the span by enrollment.
                TNStudentEnrollmentSpan newSpan = oldSpan.getCopy();
                newSpan.m_firstInactiveEnrollment = curEnrollment;

                if (newSpan.m_preferenceMemberOnWithdrawal) {
                    newSpan.m_lastActiveDate = curEnrollment.getEnrollmentDate();
                } else {
                    newSpan.m_lastActiveDate = newSpan.findSessionDate(curEnrollment.getEnrollmentDate(), false);
                }

                oldSpan.m_firstActiveEnrollment = curEnrollment;
                if (oldSpan.m_preferenceMemberOnEntry) {
                    oldSpan.m_firstActiveDate = curEnrollment.getEnrollmentDate();
                    if (oldSpan.m_firstActiveDate.before(getCurrentContext().getStartDate())) {
                        oldSpan.m_firstActiveDate = getCurrentContext().getStartDate();
                    }
                } else {
                    // Lookup next in-session date for the school.
                    oldSpan.m_firstActiveDate = oldSpan.findSessionDate(curEnrollment.getEnrollmentDate(), true);
                }

                splitSpans.add(newSpan);

                oldSpan = newSpan;
                oldCalendarID = getCalendarID(oldSpan.getFirstInactiveEnrollment(), student);
                oldProgramKey = TNStateReportData.makeCalendarLookupKey(m_contextOid,
                        oldSpan.getFirstActiveEnrollment().getSchoolOid(), oldCalendarID);
            }
        }

        return splitSpans;
    }

    /**
     * Returns all codes (enabled and disabled).
     *
     * @param refTable ReferenceTable
     * @return Map<String, ReferenceCode>
     */
    private Map<String, ReferenceCode> getAllCodesMap(ReferenceTable refTable) {
        Collection<ReferenceCode> refCodes = refTable.getReferenceCodes();

        Map<String, ReferenceCode> refCodesMap = new HashMap<String, ReferenceCode>();
        for (ReferenceCode code : refCodes) {
            refCodesMap.put(code.getCode(), code);
        }

        return refCodesMap;
    }

    /**
     * Returns calendar ID based on enrollment and student.
     *
     * @param enrollment StudentEnrollment
     * @param student SisStudent
     * @return String
     */
    private String getCalendarID(StudentEnrollment enrollment, SisStudent student) {
        String calendarCode = null;
        if (enrollment != null) {
            calendarCode = (String) enrollment.getFieldValueByAlias(ALIAS_ENR_CAL_CODE);
        }
        if (StringUtils.isEmpty(calendarCode)) {
            calendarCode = getStdCalendarId(student);
        }
        return calendarCode;
    }

    /**
     * Returns instructional program by student calendar and span.
     *
     * @param student SisStudent
     * @param span TNStudentEnrollmentSpan
     * @return String
     */
    private String getInstProgram(SisStudent student, TNStudentEnrollmentSpan span) {
        String calendarCode = null;
        if (span.getFirstInactiveEnrollment() != null) {
            calendarCode = (String) span.getFirstInactiveEnrollment().getFieldValueByAlias(ALIAS_ENR_CAL_CODE);
        }
        // If there is no calendar, use student's calendar.
        if (StringUtils.isEmpty(calendarCode)) {
            calendarCode = getStdCalendarId(student);
        }
        String calendarKey = TNStateReportData.makeCalendarLookupKey(m_contextOid,
                span.getFirstActiveEnrollment().getSchoolOid(), calendarCode);

        return m_calendarOids.get(calendarKey);
    }

    /**
     * Returns a set of days-in-session for the given school and calendar ID combination.
     *
     * @param school SisSchool
     * @param calendar String
     * @return Set of PlainDate objects
     */
    private PlainDate getFirstCalendarDay(SisSchool school, String calendar) {
        PlainDate firstDate = null;

        Criteria criteria = new Criteria();
        criteria.addEqualTo(SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + "." + SchoolCalendar.COL_SCHOOL_OID,
                school.getOid());
        criteria.addEqualTo(SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + "." + SchoolCalendar.COL_DISTRICT_CONTEXT_OID,
                getCurrentContext().getOid());
        criteria.addEqualTo(SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + "." + SchoolCalendar.COL_CALENDAR_ID, calendar);
        criteria.addEqualTo(SisSchoolCalendarDate.COL_IN_SESSION_INDICATOR, Boolean.TRUE);

        QueryByCriteria calendarQuery = new QueryByCriteria(SisSchoolCalendarDate.class, criteria);
        calendarQuery.addOrderByAscending(SisSchoolCalendarDate.COL_DATE);
        QueryIterator calendars = null;
        try {
            calendars = getBroker().getIteratorByQuery(calendarQuery);
            if (calendars.hasNext()) {
                SisSchoolCalendarDate calendarDate = (SisSchoolCalendarDate) calendars.next();
                firstDate = calendarDate.getDate();
            }
        } finally {
            if (calendars != null) {
                calendars.close();
            }
        }

        return firstDate;
    }

    /**
     * Return student schools (secondary school) keyed on student oid.
     *
     * @param query QueryByCriteria
     * @return Map
     */
    private Map<String, Collection<StudentSchool>> getSecondarySchools(QueryByCriteria query) {
        X2Criteria criteria = new X2Criteria();
        criteria.addIn(StudentSchool.COL_STUDENT_OID,
                new SubQuery(SisStudent.class, X2BaseBean.COL_OID, query.getCriteria()));
        criteria.addEqualTo(StudentSchool.COL_TYPE, Integer.valueOf(StudentSchool.SECONDARY));
        criteria.addNotEmpty(StudentSchool.REL_SCHOOL + ModelProperty.PATH_DELIMITER + m_fieldStateSchoolId,
                getBroker().getPersistenceKey());

        PlainDate startDate = m_context.getStartDate();
        PlainDate endDate = m_context.getEndDate();

        criteria.addLessOrEqualThan(StudentSchool.COL_START_DATE, endDate);
        X2Criteria endDate1Criteria = new X2Criteria();
        endDate1Criteria.addIsNull(StudentSchool.COL_END_DATE);
        X2Criteria endDate2Criteria = new X2Criteria();
        endDate2Criteria.addGreaterOrEqualThan(StudentSchool.COL_END_DATE, startDate);
        endDate1Criteria.addOrCriteria(endDate2Criteria);
        criteria.addAndCriteria(endDate1Criteria);

        QueryByCriteria studentSchoolQuery = new QueryByCriteria(StudentSchool.class, criteria);
        return getBroker().getGroupedCollectionByQuery(studentSchoolQuery, StudentSchool.COL_STUDENT_OID, 1024);
    }
}
