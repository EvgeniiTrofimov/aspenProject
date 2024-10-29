/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2020 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */

package com.x2dev.procedures.statereporting.ma;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.StudentContact;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.StudentAttendance;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class PEBTData.
 */
public class PEBTData extends StateReportData {

    protected static final String[] CODE_LUNCH_STATUS = {"01", "02"};
    protected static final String CODE_YES = "Yes";
    protected static final String CODE_NO = "No";
    protected static final String FIRST = "firstName";
    protected static final String LAST = "lastName";

    /**
     * Class to retrieve value of the free/reduced lunch field.
     */
    protected class FreeReducedRetriever implements FieldRetriever {

        protected static final String RETRIEVER_ID = "MA_FREE_RED";

        /**
         * Instantiates a new MA retriever.
         */
        public FreeReducedRetriever() {
            super();
        }

        /**
         * Gets the field value.
         *
         * @param dataDefault StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever
         *      #getFieldValue(StateReportData, StateReportEntity, FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData dataDefault, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Student student = (Student) entity.getBean();
            Object returnValue = "";

            String lunchStatus = getStateValue(student, (String) field.getParameter());
            if (Arrays.asList(CODE_LUNCH_STATUS).contains(lunchStatus)) {
                returnValue = CODE_YES;
            } else {
                returnValue = CODE_NO;
            }

            return returnValue;
        }
    }

    /**
     * Class to retrieve value of the primary guardian's first name.
     */
    protected class GuardianNameRetriever implements FieldRetriever {

        protected static final String RETRIEVER_ID = "GUARDIAN_NAME";

        /**
         * Instantiates a new MA retriever.
         */
        public GuardianNameRetriever() {
            super();
        }

        /**
         * Gets the field value.
         *
         * @param dataDefault StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever
         *      #getFieldValue(StateReportData, StateReportEntity, FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData dataDefault, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Student student = (Student) entity.getBean();
            StudentContact guardian = getPrimaryGuardian(student);
            if (guardian == null) {
                return "";
            }

            String param = (String) field.getParameter();
            switch (param) {
                case FIRST:
                    return guardian.getContact().getPerson().getFirstName();
                case LAST:
                    return guardian.getContact().getPerson().getLastName();
                default:
                    return "Setup Error";
            }
        }
    }

    /**
     * Class to retrieve the instruction mode
     */
    protected class RetriverEligibleAtt implements FieldRetriever {

        protected static final String RETRIEVER_ID = "ELIGIBLE_ABSENCES";

        /**
         * Gets the field value.
         *
         * @param dataDefault StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever
         *      #getFieldValue(StateReportData, StateReportEntity, FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData dataDefault, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Student student = (Student) entity.getBean();
            if (m_showEligibleAtt) {
                Collection<StudentAttendance> atts = m_attByStdMap.get(student.getOid());
                if (atts != null) {
                    return String.valueOf(atts.size());
                }
            }
            return "";
        }
    }

    /**
     * Class to retrieve the school id
     */
    protected class SchoolIdRetriever implements FieldRetriever {

        protected static final String RETRIEVER_ID = "SCHOOL_ID";

        /**
         * Gets the field value.
         *
         * @param dataDefault StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever
         *      #getFieldValue(StateReportData, StateReportEntity, FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData dataDefault, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Student student = (Student) entity.getBean();

            String adjId = (String) student.getFieldValueByBeanPath(m_fieldSchoolIdAdj);
            if (!StringUtils.isEmpty(adjId)) {
                return adjId;
            }
            return student.getSchool().getFieldValueByBeanPath(m_fieldSchoolId);
        }
    }


    /**
     * Gets the primary guardian.
     *
     * @param student Student
     * @return Student contact
     */
    private StudentContact getPrimaryGuardian(Student student) {
        StudentContact guardian = null;
        for (StudentContact contact : student.getContacts(getBroker())) {
            if (contact.getLivesWithIndicator() && (guardian == null ||
                    (guardian != null && contact.getEmergencyPriority() < guardian.getEmergencyPriority()))) {
                guardian = contact;
            }
        }

        return guardian;
    }

    /**
     * Returns the state value for the passed alias.
     *
     * @param student
     * @param alias
     *
     * @return String
     */
    private String getStateValue(Student student, String alias) {
        String value = (String) student.getFieldValueByAlias(alias, getDataDictionary());
        String beanPath = translateAliasToJavaName(alias, true);

        return lookupStateValue(Student.class, beanPath, value);
    }

    /**
     * Constants.
     */
    public static final String INPUT_PARAM_ATT_CODE = "attCode";
    public static final String INPUT_PARAM_ATT_REASONS = "attReasons";
    public static final String INPUT_PARAM_ATT_DATE_END = "attendanceEndDate";
    public static final String INPUT_PARAM_ATT_DATE_START = "attendanceStartDate";
    public static final String INPUT_PARAM_ATT_ELIG = "showAttEligib";
    public static final String INPUT_PARAM_INCLUDE_NULL_REASONS = "includeNullReasons";
    public static final String INPUT_PARAM_FREE_REDUCED_LUNCH = "frLunchOnly";
    public static final String ALIAS_FREE_REDUCED_LUNCH = "DOE 19";
    public static final String ALIAS_SCHOOL_ID = "DOE 15";
    public static final String ALIAS_SCHOOL_ID_ADJ = "DOE Adjusted School";

    /**
     * Supporting instance variables.
     * These are protected rather than private so they can be accessed by the inner classes.
     */
    protected Map<String, Collection<StudentAttendance>> m_attByStdMap;
    protected PlainDate m_attDateEnd;
    protected PlainDate m_attDateStart;
    protected StudentHistoryHelper m_helper;
    protected String m_fieldFreeRedLunch;
    protected String m_fieldSchoolId;
    protected String m_fieldSchoolIdAdj;
    protected boolean m_frLunchOnly;
    protected boolean m_showEligibleAtt;

    /**
     * Initialize the data module.
     * Initialize necessary working resources.
     * Define query for students to load.
     * Define list of field definitions for the export.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    public void initialize() {
        m_attDateStart = (PlainDate) getParameter(INPUT_PARAM_ATT_DATE_START);
        m_attDateEnd = (PlainDate) getParameter(INPUT_PARAM_ATT_DATE_END);
        m_showEligibleAtt = getParameter(INPUT_PARAM_ATT_ELIG) == null ? false
                : ((Boolean) getParameter(INPUT_PARAM_ATT_ELIG)).booleanValue();
        m_frLunchOnly = getParameter(INPUT_PARAM_FREE_REDUCED_LUNCH) == null ? false
                : ((Boolean) getParameter(INPUT_PARAM_FREE_REDUCED_LUNCH)).booleanValue();
        m_fieldFreeRedLunch = translateAliasToJavaName(ALIAS_FREE_REDUCED_LUNCH, true);
        m_fieldSchoolId = translateAliasToJavaName(ALIAS_SCHOOL_ID, true);
        m_fieldSchoolIdAdj = translateAliasToJavaName(ALIAS_SCHOOL_ID_ADJ, true);

        /*
         * Build helper object.
         */
        m_helper = new StudentHistoryHelper(this);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);

        X2Criteria studentCriteria = m_helper.getStudentCriteria();
        studentCriteria.addIn(Student.COL_ENROLLMENT_STATUS,
                StudentManager.getActiveStudentCodeList(getOrganization()));

        if (m_frLunchOnly) {
            studentCriteria.addIn(m_fieldFreeRedLunch, loadLunchCodes());
        }

        if (getSchool() == null) {
            studentCriteria.addNotEqualTo(Student.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR,
                    Boolean.TRUE);
        }

        X2Criteria studentAttendanceCriteria = getStudentAttendanceCriteria();
        SubQuery studentAttendanceQuery =
                new SubQuery(StudentAttendance.class, StudentAttendance.COL_STUDENT_OID, studentAttendanceCriteria);
        m_attByStdMap =
                getBroker().getGroupedCollectionByQuery(
                        new QueryByCriteria(StudentAttendance.class, studentAttendanceCriteria),
                        StudentAttendance.COL_STUDENT_OID, 512);
        studentCriteria.addIn(X2BaseBean.COL_OID, studentAttendanceQuery);
        setQuery(m_helper.getStudentQuery(true));
        // Build a map of calculations/retrievers
        HashMap calcs = new HashMap<String, FieldRetriever>();
        calcs.put(FreeReducedRetriever.RETRIEVER_ID, new FreeReducedRetriever());
        calcs.put(GuardianNameRetriever.RETRIEVER_ID, new GuardianNameRetriever());
        calcs.put(RetriverEligibleAtt.RETRIEVER_ID, new RetriverEligibleAtt());
        calcs.put(SchoolIdRetriever.RETRIEVER_ID, new SchoolIdRetriever());
        super.addCalcs(calcs);
    }

    /**
     * Returns a criteria for the attendance records to export.
     *
     * @return X2Criteria
     */
    private X2Criteria getStudentAttendanceCriteria() {
        X2Criteria attCriteria = new X2Criteria();
        applyInputCriteria(attCriteria, false, StudentAttendance.REL_STUDENT);
        attCriteria.addGreaterOrEqualThan(StudentAttendance.COL_DATE, m_attDateStart);
        attCriteria.addLessOrEqualThan(StudentAttendance.COL_DATE, m_attDateEnd);
        if (isSchoolContext()) {
            attCriteria.addEqualTo(StudentAttendance.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            attCriteria.addNotEqualTo(StudentAttendance.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR,
                    Boolean.TRUE);
            attCriteria.addNotEqualTo(StudentAttendance.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_ARCHIVE_INDICATOR,
                    Boolean.TRUE);
        }
        String attCode = (String) getParameter(INPUT_PARAM_ATT_CODE);
        if (!StringUtils.isEmpty(attCode)) {
            attCriteria.addEqualToIgnoreCase(StudentAttendance.COL_CODE_VIEW, attCode);
        }
        String attReasons = (String) getParameter(INPUT_PARAM_ATT_REASONS);
        X2Criteria attReasonsCriteria = new X2Criteria();
        X2Criteria attOrReasonsCriteria = new X2Criteria();
        boolean includeNullReasons = getParameter(INPUT_PARAM_INCLUDE_NULL_REASONS) != null
                ? ((Boolean) getParameter(INPUT_PARAM_INCLUDE_NULL_REASONS)).booleanValue()
                : false;
        if (!StringUtils.isEmpty(attReasons)) {
            X2Criteria refCodesCriteria = new X2Criteria();
            refCodesCriteria.addIn(X2BaseBean.COL_OID, Arrays.asList((attReasons.split(","))));
            Set<String> codes =
                    getBroker().getGroupedCollectionByQuery(new QueryByCriteria(ReferenceCode.class, refCodesCriteria),
                            ReferenceCode.COL_CODE, 128).keySet();
            attReasonsCriteria.addIn(StudentAttendance.COL_REASON_CODE, codes);
        }
        if (!includeNullReasons) {
            attReasonsCriteria.addNotEmpty(StudentAttendance.COL_REASON_CODE, getBroker().getPersistenceKey());
        } else {
            attOrReasonsCriteria.addEmpty(StudentAttendance.COL_REASON_CODE, getBroker().getPersistenceKey());
            attReasonsCriteria.addOrCriteria(attOrReasonsCriteria);
        }
        attCriteria.addAndCriteria(attReasonsCriteria);
        return attCriteria;
    }


    /**
     * Load lunch codes.
     *
     * @return Map
     */
    private List<String> loadLunchCodes() {
        List<String> codes = new ArrayList<String>();
        DataDictionaryField field = getDataDictionaryField(Student.class, m_fieldFreeRedLunch);
        Collection<ReferenceCode> refCodes = field.getReferenceTable().getReferenceCodes();
        for (ReferenceCode code : refCodes) {
            if (Arrays.asList(CODE_LUNCH_STATUS).contains(code.getStateCode())) {
                codes.add(code.getCode());
            }
        }
        return codes;
    }
}
