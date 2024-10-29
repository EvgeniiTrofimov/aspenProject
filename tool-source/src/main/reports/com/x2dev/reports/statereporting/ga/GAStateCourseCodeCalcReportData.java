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
package com.x2dev.reports.statereporting.ga;
/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2004 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.RecordSet;
import com.follett.fsc.core.k12.beans.RecordSetKey;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.Course;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.SchoolScheduleContext;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentSchedule;
import com.x2dev.sis.model.beans.StudentScheduleChange;
import com.x2dev.sis.tools.reports.ScheduleReportHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares the data for the GA State Course Code Calculation report.
 *
 * @author X2 Development Corporation
 */
public class GAStateCourseCodeCalcReportData extends ReportJavaSourceNet {

    /**
     * Input parameters
     */
    public static final String INPUT_ONLY_ERRORS = "onlyErrors";
    public static final String INPUT_OUTPUT_OPTION = "outputOption";
    public static final String INPUT_QUERY_BY = "queryBy";
    public static final String INPUT_QUERY_STRING = "queryString";
    public static final String INPUT_STUDENT_SORT = "studentSort";

    /**
     * Aliases
     */
    private static final String ALIAS_CRS_EXCLUDE = "DOE EXCLUDE CRS";
    private static final String ALIAS_MST_DELIVERY_MODEL = "all-mst-SectionDeliveryModel";
    private static final String ALIAS_SCC_CALC_STATE_CRS_CODE = "DOE CALCULATED STATE CRS CODE HISTORY";
    private static final String ALIAS_SCC_INCLUDE_STD = "all-scc-DeliveryorInclusionStudent";
    private static final String ALIAS_SCC_STD_DELIVERY_MODEL = "all-scc-StudentDeliveryModel";
    private static final String ALIAS_SSC_CALC_STATE_CRS_CODE = "DOE CALCULATED STATE CRS CODE";
    private static final String ALIAS_SSC_INCLUDE_STD = "all-ssc-DeliveryorInclusionStudent";
    private static final String ALIAS_SSC_STD_DELIVERY_MODEL = "all-ssc-StudentDeliveryModel";

    /**
     * Fields in grid for iReport.
     */
    private static final String FIELD_CRS_NUM = "crsNumber";
    private static final String FIELD_ERROR_MESSAGE = "errorMessage";
    private static final String FIELD_MESSAGE = "message";
    private static final String FIELD_NAME_VIEW = "nameView";
    private static final String FIELD_SKL_ID = "schoolId";
    private static final String FIELD_STATUS = "status";
    private static final String FIELD_STUDENT = "student";
    private static final String PARAM_ANY_INIT_ERROR = "anyInitErros";
    /**
     * Other constants
     */
    private static final String ACTION_UPDATED_WITH_ZERO =
            "Result: course.number with zero following decimal replaced with value of .";
    private static final String CRS_NUM_PATTERN = "[a-zA-Z0-9]{2}.0\\d{6}";
    private static final String ERROR_INCL_FLAG_BUT_NO_DELIVERY_MODEL =
            "Error: Section delivery model set to \"Mixed\" for the class, Delivery/Inclusion Student Flag = True, but student is missing a delivery model.";
    private static final String ERROR_INCLUSION_FALSE_DELIVERY_NOT_EMPTY =
            "Error: Section delivery model set to \"Mixed\" for the class, Delivery/Inclusion Student Flag = False, but student has delivery model assigned.";
    private static final String ERROR_CRS_NUM_NOT_LEGAL = "Course number not legal";
    private static final String ERROR_MST_INVALID_DELIVERY = "Invalid inclusion/Delivery Model set on class section.";
    private static final String NO_FIELD_ERROR = "No field found with alias = ";

    /**
     * Instance variables.
     */
    private String m_crsExcludeField;
    private Map<String, ReferenceCode> m_deliveryCodesMap;
    private Boolean m_includeOnlyErrors;
    private List<String> m_initErrors = new ArrayList<>();
    private String m_mstDeliveryModelStdField;
    private boolean m_outputError = false;
    private boolean m_outputResult = false;
    private ScheduleReportHelper m_reportHelper;
    private String m_sccCalcStateCodeField;
    private String m_sccDeliveryModelStdField;
    private String m_sccInclusionStdField;
    private String m_sscCalcStateCodeField;
    private String m_sscDeliveryModelStdField;
    private String m_sscInclusionStdField;
    private Map<String, List<StudentSchedule>> m_studentSchedulesMap;
    private Map<String, List<StudentScheduleChange>> m_studentScheduleChangesMap;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {

        // get input parameters.
        m_includeOnlyErrors = (Boolean) getParameter(INPUT_ONLY_ERRORS);
        int outputOption = ((Integer) getParameter(INPUT_OUTPUT_OPTION)).intValue();
        switch (outputOption) {
            case 0: // Both
                break;
            case 1: // Errors only
                m_outputError = true;
                break;
            case 2: // Errors only
                m_outputResult = true;
                break;
            default: //
                break;
        }
        // prepare report data.
        initializeReport();
        ReportDataGrid grid = new ReportDataGrid();
        // Build and execute the query for student schedules.

        if (m_initErrors.isEmpty()) {
            addParameter(PARAM_ANY_INIT_ERROR, Boolean.FALSE);
            buildScheduleMaps();
            for (Entry<String, List<StudentSchedule>> entry : m_studentSchedulesMap.entrySet()) {
                List<StudentSchedule> sscs = entry.getValue();
                for (StudentSchedule ssc : sscs) {
                    calculationCrsNumOnSchedule(grid, ssc, ssc.getSection(), ssc.getStudent(), "Active",
                            m_sscCalcStateCodeField,
                            m_sscDeliveryModelStdField, m_sscInclusionStdField);
                }
            }
            for (Entry<String, List<StudentScheduleChange>> entry : m_studentScheduleChangesMap.entrySet()) {
                List<StudentScheduleChange> sccs = entry.getValue();
                for (StudentScheduleChange scc : sccs) {
                    calculationCrsNumOnSchedule(grid, scc, scc.getMasterSchedule(), scc.getStudent(),
                            scc.getChangeTypeCode(),
                            m_sccCalcStateCodeField, m_sccDeliveryModelStdField, m_sccInclusionStdField);
                }
            }
        } else {
            addParameter(PARAM_ANY_INIT_ERROR, Boolean.TRUE);
            for (String error : m_initErrors) {
                grid.append();
                grid.set(FIELD_ERROR_MESSAGE, error);
            }
        }
        int sort = ((Integer) getParameter(INPUT_STUDENT_SORT)).intValue();
        switch (sort) {
            case 0: // School
                grid.sort(FIELD_SKL_ID, true, true);
                grid.sort(FIELD_NAME_VIEW, true, true);
                break;

            case 1: // Name view
                grid.sort(FIELD_NAME_VIEW, true, true);
                break;
            default: // No sort specified
                break;
        }
        grid.sort(FIELD_CRS_NUM, true, true);
        grid.beforeTop();
        return grid;
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
        m_reportHelper = new ScheduleReportHelper(userData);
    }

    /**
     * Append row to grid.
     *
     * @param grid ReportDataGrid
     * @param student SisStudent
     * @param crsNumber String
     * @param typeCode String
     * @param message String
     */
    private void appendRowToGrid(ReportDataGrid grid,
                                 SisStudent student,
                                 String crsNumber,
                                 String typeCode,
                                 String message) {
        grid.append();
        grid.set(FIELD_STUDENT, student);
        grid.set(FIELD_CRS_NUM, crsNumber);
        grid.set(FIELD_STATUS, typeCode);
        grid.set(FIELD_MESSAGE, message);
        grid.set(FIELD_SKL_ID, student.getSchool().getSchoolId());
        grid.set(FIELD_NAME_VIEW, student.getNameView());
    }

    /**
     * Build a map of student schedules in effect on report date.
     * Include user specified selection criteria and sorting.
     */
    private void buildScheduleMaps() {

        // Identify current active student schedules.
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualToField(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.REL_SCHOOL + PATH_DELIMITER +
                SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER +
                SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID,
                StudentSchedule.COL_SCHEDULE_OID);
        criteria.addEqualTo(StudentSchedule.REL_SECTION + PATH_DELIMITER +
                MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                SchoolCourse.REL_COURSE + PATH_DELIMITER +
                Course.COL_MASTER_TYPE, "Class");
        criteria.addNotEqualTo(StudentSchedule.REL_SECTION + PATH_DELIMITER +
                MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                SchoolCourse.REL_COURSE + PATH_DELIMITER +
                m_crsExcludeField, Boolean.TRUE);

        // If student criteria was specified, include the student criteria.
        if (!StringUtils.isEmpty(m_reportHelper.getStudentOid())) {
            criteria.addEqualTo(StudentSchedule.COL_STUDENT_OID, m_reportHelper.getStudentOid());
        } else {
            if (isSchoolContext()) {
                criteria.addEqualTo(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER +
                        Schedule.COL_SCHOOL_OID, getSchool().getOid());
            }

            int queryBy = ((Integer) getParameter(INPUT_QUERY_BY)).intValue();
            switch (queryBy) {
                case 1: // YOG
                    criteria.addEqualTo(StudentSchedule.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_YOG,
                            getParameter(INPUT_QUERY_STRING));
                    break;

                case 2: // LASID
                    criteria.addEqualTo(StudentSchedule.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_LOCAL_ID,
                            getParameter(INPUT_QUERY_STRING));
                    break;

                case 3: // SASID
                    criteria.addEqualTo(StudentSchedule.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_STATE_ID,
                            getParameter(INPUT_QUERY_STRING));
                    break;

                default:
                    // No additional criteria (this is the case for "All")
                    break;
            }
        }
        QueryByCriteria query = new QueryByCriteria(StudentSchedule.class, criteria);
        m_studentSchedulesMap = getBroker().getGroupedCollectionByQuery(query, StudentSchedule.COL_STUDENT_OID, 1024);

        /*
         * Next, lookup all StudentScheduleChange records that may apply and update the schedule
         * list with them.
         * Pick up the schedule change records after report date in reverse date order.
         * Work back through them and adjust the schedule list:
         * 1. find a Drop, insert the dropped section into the student schedule list.
         * 2. find an Add, remove the added section from the student schedule list.
         */
        criteria = new X2Criteria();
        criteria.addEqualToField(StudentScheduleChange.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.REL_SCHOOL + PATH_DELIMITER +
                SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER +
                SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID,
                StudentSchedule.COL_SCHEDULE_OID);
        criteria.addEqualTo(StudentScheduleChange.REL_MASTER_SCHEDULE + PATH_DELIMITER +
                MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                SchoolCourse.REL_COURSE + PATH_DELIMITER +
                Course.COL_MASTER_TYPE, "Class");
        criteria.addEqualTo(StudentScheduleChange.COL_CHANGE_TYPE_CODE, StudentScheduleChange.CODE_DROP);
        criteria.addGreaterOrEqualThan(StudentScheduleChange.COL_EFFECTIVE_DATE, getCurrentContext().getStartDate());
        criteria.addNotNull(StudentScheduleChange.COL_ACTION_DATE);
        criteria.addNotEqualTo(StudentScheduleChange.REL_MASTER_SCHEDULE + PATH_DELIMITER +
                MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                SchoolCourse.REL_COURSE + PATH_DELIMITER +
                m_crsExcludeField, Boolean.TRUE);
        criteria.addEmpty(m_sccCalcStateCodeField, getBroker().getPersistenceKey());

        // If student criteria was specified, include the student criteria.
        if (!StringUtils.isEmpty(m_reportHelper.getStudentOid())) {
            criteria.addEqualTo(StudentScheduleChange.COL_STUDENT_OID, m_reportHelper.getStudentOid());
        } else {
            if (isSchoolContext()) {
                criteria.addEqualTo(StudentScheduleChange.REL_SCHEDULE + PATH_DELIMITER +
                        Schedule.COL_SCHOOL_OID, getSchool().getOid());
            }

            int queryBy = ((Integer) getParameter(INPUT_QUERY_BY)).intValue();
            switch (queryBy) {
                case 1: // YOG
                    criteria.addEqualTo(StudentScheduleChange.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_YOG,
                            getParameter(INPUT_QUERY_STRING));
                    break;

                case 2: // LASID
                    criteria.addEqualTo(StudentScheduleChange.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_LOCAL_ID,
                            getParameter(INPUT_QUERY_STRING));
                    break;

                case 3: // SASID
                    criteria.addEqualTo(StudentScheduleChange.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_STATE_ID,
                            getParameter(INPUT_QUERY_STRING));
                    break;

                case 4: // Record set
                    criteria.addIn(StudentScheduleChange.COL_STUDENT_OID, getStudentOidSubQuery());
                    break;

                default:
                    // No additional criteria (this is the case for "All")
                    break;
            }
        }
        // Order by student and descending change date.
        query = new QueryByCriteria(StudentScheduleChange.class, criteria);
        m_studentScheduleChangesMap =
                getBroker().getGroupedCollectionByQuery(query, StudentScheduleChange.COL_STUDENT_OID, 1024);
    }

    /**
     * Calculation crs num on schedule.
     *
     * @param grid ReportDataGrid
     * @param schedule X2BaseBean
     * @param mst MasterSchedule
     * @param student SisStudent
     * @param status String
     * @param scheduleStateCodeField String
     * @param scheduleDeliveryField String
     * @param scheduleInclusionField String
     */
    private void calculationCrsNumOnSchedule(ReportDataGrid grid,
                                             X2BaseBean schedule,
                                             MasterSchedule mst,
                                             SisStudent student,
                                             String status,
                                             String scheduleStateCodeField,
                                             String scheduleDeliveryField,
                                             String scheduleInclusionField) {
        String crsNumber = mst.getSchoolCourse().getCourse().getNumber();
        if (!StringUtils.isEmpty(crsNumber)) {
            if (crsNumber.matches(CRS_NUM_PATTERN)) {
                if (!StringUtils.isEmpty((String) mst.getFieldValueByBeanPath(m_mstDeliveryModelStdField))) {
                    ReferenceCode rcdDeliveryModel =
                            m_deliveryCodesMap.get(mst.getFieldValueByBeanPath(m_mstDeliveryModelStdField));
                    String mstDeliveryModel = rcdDeliveryModel == null ? ""
                            : (StringUtils.isEmpty(rcdDeliveryModel.getLocalCode()) ? ""
                                    : rcdDeliveryModel.getLocalCode());
                    if ("M".equals(mstDeliveryModel)) {
                        if (BooleanAsStringConverter.TRUE
                                .equals(schedule.getFieldValueByBeanPath(scheduleInclusionField))) {
                            String sscDeliveryModelValue = null;
                            if (schedule.getFieldValueByBeanPath(scheduleDeliveryField) != null) {
                                rcdDeliveryModel = m_deliveryCodesMap
                                        .get(schedule.getFieldValueByBeanPath(scheduleDeliveryField));
                                sscDeliveryModelValue = rcdDeliveryModel == null ? "" : rcdDeliveryModel.getLocalCode();
                            }
                            if (!StringUtils.isEmpty(sscDeliveryModelValue)
                                    && sscDeliveryModelValue.matches("\\d{1}")) {

                                StringBuilder calculatedCrsNum = new StringBuilder(crsNumber);
                                calculatedCrsNum.setCharAt(3,
                                        sscDeliveryModelValue.toCharArray()[0]);
                                if (!m_includeOnlyErrors.booleanValue()) {
                                    schedule.setFieldValueByBeanPath(scheduleStateCodeField,
                                            calculatedCrsNum.toString());
                                    getBroker().saveBeanForced(schedule);
                                }
                                if (!m_outputError) {
                                    appendRowToGrid(grid, student,
                                            mst.getCourseView(), status,
                                            ACTION_UPDATED_WITH_ZERO + sscDeliveryModelValue);
                                }

                            } else {
                                if (!m_includeOnlyErrors.booleanValue()) {
                                    schedule.setFieldValueByBeanPath(scheduleStateCodeField, crsNumber);
                                    getBroker().saveBeanForced(schedule);
                                }
                                if (m_outputError || !m_outputResult) {
                                    appendRowToGrid(grid, student, mst.getCourseView(), status,
                                            ERROR_INCL_FLAG_BUT_NO_DELIVERY_MODEL);
                                }
                            }
                        } else {
                            if (!m_includeOnlyErrors.booleanValue()) {
                                schedule.setFieldValueByBeanPath(scheduleStateCodeField, crsNumber);
                                getBroker().saveBeanForced(schedule);
                            }
                            if (!StringUtils
                                    .isEmpty((String) schedule.getFieldValueByBeanPath(scheduleDeliveryField))) {
                                if (m_outputError || !m_outputResult) {
                                    appendRowToGrid(grid, student, mst.getCourseView(), status,
                                            ERROR_INCLUSION_FALSE_DELIVERY_NOT_EMPTY);
                                }
                            }
                        }
                    } else if ("0".equals(mstDeliveryModel)) {
                        if (!m_includeOnlyErrors.booleanValue()) {
                            schedule.setFieldValueByBeanPath(scheduleStateCodeField, crsNumber);
                            getBroker().saveBeanForced(schedule);
                        }
                    } else {
                        if (!StringUtils.isEmpty(mstDeliveryModel) && mstDeliveryModel.matches("[1-9]{1}")) {
                            if (BooleanAsStringConverter.TRUE
                                    .equals(schedule.getFieldValueByBeanPath(scheduleInclusionField))) {
                                StringBuilder calculatedCrsNum = new StringBuilder(crsNumber);
                                calculatedCrsNum.setCharAt(3, mstDeliveryModel.toCharArray()[0]);
                                if (!m_includeOnlyErrors.booleanValue()) {
                                    schedule.setFieldValueByBeanPath(scheduleStateCodeField,
                                            calculatedCrsNum.toString());
                                    getBroker().saveBeanForced(schedule);
                                }
                                if (!m_outputError) {
                                    appendRowToGrid(grid, student, mst.getCourseView(), status,
                                            ACTION_UPDATED_WITH_ZERO + mstDeliveryModel);
                                }
                            } else {
                                if (!m_includeOnlyErrors.booleanValue()) {
                                    schedule.setFieldValueByBeanPath(scheduleStateCodeField, crsNumber);
                                    getBroker().saveBeanForced(schedule);
                                }
                            }
                        } else {
                            if (!m_includeOnlyErrors.booleanValue()) {
                                schedule.setFieldValueByBeanPath(scheduleStateCodeField, crsNumber);
                                getBroker().saveBeanForced(schedule);
                            }
                            if (m_outputError || !m_outputResult) {
                                appendRowToGrid(grid, student, mst.getCourseView(),
                                        status, ERROR_MST_INVALID_DELIVERY);
                            }
                        }
                    }
                } else {
                    if (!m_includeOnlyErrors.booleanValue()) {
                        schedule.setFieldValueByBeanPath(scheduleStateCodeField, crsNumber);
                        getBroker().saveBeanForced(schedule);
                    }
                }
            } else {
                if (!m_includeOnlyErrors.booleanValue()) {
                    if (crsNumber != null && crsNumber.length() > 10) {
                        throw new IllegalStateException("crsNumber = [" + crsNumber + "] from school "
                                + mst.getSchoolCourse().getSchool().getName());
                    }
                    schedule.setFieldValueByBeanPath(scheduleStateCodeField, crsNumber);
                    getBroker().saveBeanForced(schedule);
                }
            }
        } else {
            if (m_outputError || !m_outputResult) {
                appendRowToGrid(grid, student, mst.getCourseView(), status, ERROR_CRS_NUM_NOT_LEGAL);
            }
        }
    }

    /**
     * Returns the Subquery for retrieving student oids from a record set.
     *
     * @return SubQuery
     */
    private SubQuery getStudentOidSubQuery() {
        Criteria recordSetCriteria = new Criteria();
        recordSetCriteria.addEqualTo(RecordSetKey.REL_RECORD_SET + PATH_DELIMITER + RecordSet.COL_NAME,
                getParameter(INPUT_QUERY_STRING));

        SubQuery studentOidSubQuery = new SubQuery(RecordSetKey.class,
                RecordSetKey.COL_OBJECT_OID, recordSetCriteria);

        return studentOidSubQuery;
    }

    /**
     * Prepare report data, alias lookups.
     */
    private void initializeReport() {
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(ALIAS_CRS_EXCLUDE);
        if (field != null) {
            m_crsExcludeField = field.getJavaName();
        } else {
            m_initErrors.add(NO_FIELD_ERROR + ALIAS_CRS_EXCLUDE);
        }
        field = dictionary.findDataDictionaryFieldByAlias(ALIAS_SCC_CALC_STATE_CRS_CODE);
        if (field != null) {
            m_sccCalcStateCodeField = field.getJavaName();
        } else {
            m_initErrors.add(NO_FIELD_ERROR + ALIAS_SCC_CALC_STATE_CRS_CODE);
        }
        field = dictionary.findDataDictionaryFieldByAlias(ALIAS_SSC_CALC_STATE_CRS_CODE);
        if (field != null) {
            m_sscCalcStateCodeField = field.getJavaName();
        } else {
            m_initErrors.add(NO_FIELD_ERROR + ALIAS_SSC_CALC_STATE_CRS_CODE);
        }
        field = dictionary.findDataDictionaryFieldByAlias(ALIAS_SSC_INCLUDE_STD);
        if (field != null) {
            m_sscInclusionStdField = field.getJavaName();
        } else {
            m_initErrors.add(NO_FIELD_ERROR + ALIAS_SSC_INCLUDE_STD);
        }
        field = dictionary.findDataDictionaryFieldByAlias(ALIAS_SCC_INCLUDE_STD);
        if (field != null) {
            m_sccInclusionStdField = field.getJavaName();
        } else {
            m_initErrors.add(NO_FIELD_ERROR + ALIAS_SCC_INCLUDE_STD);
        }
        field = dictionary.findDataDictionaryFieldByAlias(ALIAS_MST_DELIVERY_MODEL);
        if (field != null) {
            m_mstDeliveryModelStdField = field.getJavaName();
            if (field.hasReferenceTable()) {
                m_deliveryCodesMap = field.getReferenceTable().getCodeMap();
            }
        } else {
            m_initErrors.add(NO_FIELD_ERROR + ALIAS_MST_DELIVERY_MODEL);
        }
        field = dictionary.findDataDictionaryFieldByAlias(ALIAS_SSC_STD_DELIVERY_MODEL);
        if (field != null) {
            m_sscDeliveryModelStdField = field.getJavaName();
            if (m_deliveryCodesMap == null && field.hasReferenceTable()) {
                m_deliveryCodesMap = field.getReferenceTable().getCodeMap();
            }
        } else {
            m_initErrors.add(NO_FIELD_ERROR + ALIAS_SSC_STD_DELIVERY_MODEL);
        }
        field = dictionary.findDataDictionaryFieldByAlias(ALIAS_SCC_STD_DELIVERY_MODEL);
        if (field != null) {
            m_sccDeliveryModelStdField = field.getJavaName();
            if (m_deliveryCodesMap == null && field.hasReferenceTable()) {
                m_deliveryCodesMap = field.getReferenceTable().getCodeMap();
            }
        } else {
            m_initErrors.add(NO_FIELD_ERROR + ALIAS_SCC_STD_DELIVERY_MODEL);
        }
    }
}
