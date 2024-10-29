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

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.RecordSet;
import com.follett.fsc.core.k12.beans.RecordSetKey;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.Ownable;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryTable;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.web.nav.FilterException;
import com.x2dev.sis.model.beans.GraduationCourseRequirement;
import com.x2dev.sis.model.beans.GraduationProgram;
import com.x2dev.sis.model.beans.GraduationRequirement;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.business.GraduationManager;
import com.x2dev.sis.tools.reports.GraduationReportData;
import java.io.IOException;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.jdom.JDOMException;

/**
 * Prepares the data for the "Graduation Progress" report.
 *
 * @author X2 Development Corporation
 */
public class GraduationProgressData extends GraduationReportData {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Name for the parameter for creating record sets.
     */
    private static final String CREATE_RECORD_SETS_PARAM = "createRecordSets";

    /**
     * Name for the parameter for creating history records.
     */
    private static final String CREATE_HISTORY_PARAM = "createHistory";

    /**
     * Name for the enumerated "filter" report parameter. The value is an Integer.
     */
    public static final String FILTER_BY_PARAM = "filterBy";

    /**
     * Name for the enumerated "filter" report parameter. The value is an Integer.
     */
    public static final String PERCENTAGE_PARAM = "percentage";

    /**
     * Name for the enumerated "program studies oid" report parameter. The value is an String.
     */
    public static final String PROGRAM_STUDIES_BY_PARAM = "programStudiesOid";

    /**
     * Name for the "program studies" report parameter. The value is an ProgramStudies object.
     */
    public static final String PROGRAM_STUDIES_PARAM = "programStudies";

    /**
     * Name for the "requirement" report parameter. The value is an ProgramStudies object.
     */
    public static final String REQUIREMENT_PARAM = "requirement";

    /**
     * Name for the specific requirement report parameter. This value is a String.
     */
    public static final String REQUIREMENT_BY_PARAM = "requirementOid";

    /**
     * Name for the enumerated "selection" report parameter. The value is a String.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the "selection value" report parameter. The value is a String.
     */
    public static final String QUERY_STRING_PARAM = "queryString";

    /**
     * Name for the "snapshot name" parameter. The value is a String.
     */
    public static final String SNAPSHOT_NAME_PARAM = "snapshotName";

    /**
     * Name for the "snapshot owner" parameter. The value is an Integer and corresponds to the
     * Ownable types.
     */
    public static final String SNAPSHOT_OWNER_PARAM = "snapshotOwner";

    /**
     * Name for the enumerated "sort" report parameter. The value is a String.
     */
    public static final String SORT_PARAM = "sort";

    // Grid fields
    private static final String FIELD_CREDITS_COMPLETED = "completed";
    private static final String FIELD_CREDITS_COMPLETED_STATUS = "status";
    private static final String FIELD_CREDITS_IN_PROGRESS = "progress";
    private static final String FIELD_CREDITS_REQURIED = "required";
    private static final String FIELD_CREDITS_WAIVED = "waived";
    private static final String FIELD_STUDENT = "student";
    private static final String FIELD_SCHOOL = "school";

    private GraduationManager m_graduationManager;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws FilterException exception
     * @throws JDOMException exception
     * @throws IOException Signals that an I/O exception has occurred.
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws FilterException, JDOMException, IOException {

        m_graduationManager = new GraduationManager(getBroker());
        String programStudiesOid = (String) getParameter(PROGRAM_STUDIES_BY_PARAM);
        String requirementOid = (String) getParameter(REQUIREMENT_BY_PARAM);
        Boolean createRecordSets = (Boolean) getParameter(CREATE_RECORD_SETS_PARAM);
        int filter = ((Integer) getParameter(FILTER_BY_PARAM)).intValue();
        int percentage = (Integer) getParameter(PERCENTAGE_PARAM) == null ? 0
                : ((Integer) getParameter(PERCENTAGE_PARAM)).intValue();
        boolean createHistory = ((Boolean) getParameter(CREATE_HISTORY_PARAM)).booleanValue();

        GraduationProgram program =
                (GraduationProgram) getBroker().getBeanByOid(GraduationProgram.class, programStudiesOid);
        addParameter(PROGRAM_STUDIES_PARAM, program);

        GraduationRequirement requirement =
                (GraduationRequirement) getBroker().getBeanByOid(GraduationRequirement.class, requirementOid);
        addParameter(REQUIREMENT_PARAM, requirement);

        X2Criteria criteria = new X2Criteria();

        String queryBy = (String) getParameter(QUERY_BY_PARAM);
        if (queryBy.equals(SELECTION_SPECIAL_CASE_PREFIX + CURRENT_KEY)) {
            criteria = getCurrentCriteria();
            if (getSchool() != null) {
                criteria.addEqualTo(SisStudent.COL_SCHOOL_OID, getSchool().getOid());
            } else {
                criteria.addAndCriteria(getOrganizationCriteria(SisStudent.class));
            }
        } else {
            addUserCriteria(criteria, queryBy, (String) getParameter(QUERY_STRING_PARAM), null, null);

            /*
             * Add the student criteria
             */
            X2Criteria studentCriteria = getStudentCritera(program.getOid());
            criteria.addAndCriteria(studentCriteria);
        }

        RecordSet recordSet = null;
        if (createRecordSets.booleanValue()) {
            recordSet = prepareRecordSet();
        }

        QueryByCriteria studentQuery = new QueryByCriteria(SisStudent.class, criteria);
        studentQuery.addOrderByAscending(SisStudent.COL_SCHOOL_OID);
        applyUserSort(studentQuery, (String) getParameter(SORT_PARAM));

        SubQuery subquery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, criteria);

        /*
         * Clear current year history always if creating history.
         */
        if (createHistory) {
            clearGraduationHistory(subquery, programStudiesOid);
        }

        /*
         * Get a map of the courses with partial credit course requirements.
         */
        X2Criteria partialCourseReqCriteria = new X2Criteria();
        partialCourseReqCriteria.addEqualTo(
                GraduationCourseRequirement.REL_REQUIREMENT + "." + GraduationRequirement.COL_PROGRAM_STUDIES_OID,
                programStudiesOid);
        partialCourseReqCriteria.addNotEqualTo(GraduationCourseRequirement.COL_PARTIAL_CREDIT, Double.valueOf("0.0"));

        QueryByCriteria partialQuery = new QueryByCriteria(GraduationCourseRequirement.class, partialCourseReqCriteria);

        Map<String, List<GraduationCourseRequirement>> partialCourseRequirments =
                getBroker().getGroupedCollectionByQuery(partialQuery, GraduationCourseRequirement.COL_COURSE_OID, 100);

        ReportDataGrid grid = new ReportDataGrid(10);
        QueryIterator iterator = getBroker().getIteratorByQuery(studentQuery);

        try {
            while (iterator.hasNext()) {
                SisStudent student = (SisStudent) iterator.next();

                grid.append();
                grid.set(FIELD_SCHOOL, student.getSchool().getName());
                grid.set(FIELD_STUDENT, student);
                grid.set(FIELD_CREDITS_COMPLETED, new BigDecimal(0.0));
                grid.set(FIELD_CREDITS_COMPLETED_STATUS, Double.valueOf(0.0));
                grid.set(FIELD_CREDITS_WAIVED, Double.valueOf(0.0));
                grid.set(FIELD_CREDITS_IN_PROGRESS, Double.valueOf(0.0));
                grid.set(FIELD_CREDITS_REQURIED, Double.valueOf(0.0));

                HashMap<String, List<SchoolCourse>> coursesGainedCredit = new HashMap<String, List<SchoolCourse>>();
                HashMap<String, List<SchoolCourse>> coursesTaken = new HashMap<String, List<SchoolCourse>>();
                HashMap<String, List<SchoolCourse>> coursesTaking = new HashMap<String, List<SchoolCourse>>();
                HashMap<String, Double> creditsGained = new HashMap<String, Double>();
                HashMap<String, Double> rawCreditsGained = new HashMap<String, Double>();
                HashMap<String, Double> creditsWaived = new HashMap<String, Double>();
                HashMap<String, Double> creditsRequired = new HashMap<String, Double>();
                HashMap<String, Double> creditsByCourse = new HashMap<String, Double>();
                HashMap<String, Double> creditsInProgress = new HashMap<String, Double>();
                HashMap<String, String> gradeLevelByCourse = new HashMap<String, String>();
                Map<String, Map<String, Object>> otherRequirementValues = new HashMap<String, Map<String, Object>>();
                List<String> satisfiedOtherRequirementOids = new ArrayList<String>();

                m_graduationManager.determineGraduationStatus(student,
                        getUserData(),
                        programStudiesOid,
                        coursesGainedCredit,
                        coursesTaken,
                        coursesTaking,
                        new HashMap<String, List<SchoolCourse>>(),
                        new HashMap<String, List<String>>(),
                        creditsGained,
                        rawCreditsGained,
                        creditsWaived,
                        creditsRequired,
                        creditsByCourse,
                        creditsInProgress,
                        new HashMap<String, Double>(),
                        gradeLevelByCourse,
                        false,
                        partialCourseRequirments,
                        new HashMap<String, Map<String, String>>(),
                        otherRequirementValues,
                        satisfiedOtherRequirementOids);

                String totalCompleted = "";
                double totalWaived = 0;
                double status = 0;
                double totalInProgress = 0;
                double required = 0;

                if (StringUtils.isEmpty(requirementOid)) {
                    totalCompleted = String.valueOf(
                            m_graduationManager.getTotalCreditsGained(programStudiesOid, null, rawCreditsGained));
                    totalWaived = m_graduationManager.getTotalWaiver(programStudiesOid, null, creditsWaived);
                    status = m_graduationManager.getProgramSatisfiedStatus(programStudiesOid, creditsGained,
                            rawCreditsGained,
                            creditsWaived, creditsRequired, satisfiedOtherRequirementOids, null);
                    totalInProgress = m_graduationManager.getTotalCreditsInProgress(creditsInProgress);
                    required = m_graduationManager.getTotalRequired(programStudiesOid, null, creditsRequired);
                } else {
                    status = m_graduationManager.getRequirementSatisfiedStatus(requirementOid, creditsGained,
                            rawCreditsGained, creditsWaived, creditsRequired, satisfiedOtherRequirementOids);
                    if (otherRequirementValues.get(requirementOid) != null
                            && !otherRequirementValues.get(requirementOid).isEmpty()) {
                        Map<String, Object> otherValues = otherRequirementValues.get(requirementOid);

                        for (String key : otherValues.keySet()) {
                            if (otherValues.get(key) instanceof String) {
                                String[] values = ((String) otherValues.get(key)).split(":");
                                totalCompleted = totalCompleted.concat(values[1] + " ");
                            }
                        }
                    } else {
                        totalCompleted = String.valueOf(
                                m_graduationManager.getTotalCreditsGained(null, requirementOid, rawCreditsGained));
                    }
                    totalWaived = m_graduationManager.getTotalWaiver(null, requirementOid, creditsWaived);
                    totalInProgress = 0;
                    if (coursesTaking.get(requirementOid) != null) {
                        for (SchoolCourse course : coursesTaking.get(requirementOid)) {
                            totalInProgress += course.getCredit().doubleValue();
                        }
                    }
                    required = m_graduationManager.getTotalRequired(null, requirementOid, creditsRequired);
                }

                boolean includeStudent = true;

                if ((Integer) getParameter(PERCENTAGE_PARAM) != null) {
                    switch (filter) {
                        case -1: // Less than
                            includeStudent = status < percentage;
                            break;

                        case 0: // Equal
                            includeStudent = status == percentage;
                            break;

                        case 1: // Greater than
                            includeStudent = status > percentage;
                            break;
                    }
                }

                if (includeStudent) {
                    grid.set(FIELD_CREDITS_COMPLETED, totalCompleted);
                    grid.set(FIELD_CREDITS_COMPLETED_STATUS, Double.valueOf(status));
                    grid.set(FIELD_CREDITS_WAIVED, Double.valueOf(totalWaived));
                    grid.set(FIELD_CREDITS_IN_PROGRESS, Double.valueOf(totalInProgress));
                    grid.set(FIELD_CREDITS_REQURIED, Double.valueOf(required));

                    if (recordSet != null) {
                        RecordSetKey recordSetKey =
                                X2BaseBean.newInstance(RecordSetKey.class, getBroker().getPersistenceKey());
                        recordSetKey.setRecordSetOid(recordSet.getOid());
                        recordSetKey.setObjectOid(student.getOid());
                        getBroker().saveBean(recordSetKey);
                    }
                } else {
                    grid.deleteRow();
                }

                if (createHistory) {
                    /*
                     * Creates graduation histories
                     */
                    createGraduationHistory(programStudiesOid, requirementOid, student,
                            coursesTaking, creditsGained, rawCreditsGained, creditsWaived, creditsRequired,
                            creditsInProgress, otherRequirementValues, satisfiedOtherRequirementOids, totalCompleted,
                            totalWaived, status, totalInProgress, required, m_graduationManager);
                }
            }
        } finally {
            if (iterator != null) {
                iterator.close();
            }
        }

        grid.beforeTop();

        return grid;
    }

    /**
     * Prepares a record set to be populated with the members of the passed qualification list.
     * <ol>
     * <li>Deletes the record set for the passed list if one exists
     * <li>Creates and returns a new record set
     * </ol>
     *
     * @return RecordSet
     */
    private RecordSet prepareRecordSet() {
        String name = (String) getParameter(SNAPSHOT_NAME_PARAM);
        RecordSet recordSet = null;
        X2Broker broker = getBroker();

        if (!StringUtils.isEmpty(name)) {
            String recordSetName = name;
            int recordSetOwner = ((Integer) getParameter(SNAPSHOT_OWNER_PARAM)).intValue();

            // Get the length of the record set name field
            DataDictionaryField rsNameField =
                    DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey()).findDataDictionaryField(
                            RecordSet.class.getName(), RecordSet.COL_NAME);

            int fieldLength = rsNameField.getDatabaseLength();

            if (recordSetName.length() > fieldLength) {
                recordSetName = recordSetName.substring(0, fieldLength - 1);
            }

            Criteria criteria = new Criteria();
            criteria.addEqualTo(RecordSet.COL_NAME, recordSetName);
            switch (recordSetOwner) {
                case Ownable.OWNER_TYPE_ORG1:
                    criteria.addEqualTo(RecordSet.COL_OWNER_OID, getOrganization().getOid());
                    break;

                case Ownable.OWNER_TYPE_SCHOOL:
                    criteria.addEqualTo(RecordSet.COL_OWNER_OID, getSchool().getOid());
                    break;

                case Ownable.OWNER_TYPE_USER:
                default:
                    criteria.addEqualTo(RecordSet.COL_OWNER_OID, getUser().getOid());
                    break;
            }


            QueryByCriteria query = new QueryByCriteria(RecordSet.class, criteria);
            recordSet = (RecordSet) broker.getBeanByQuery(query);

            if (recordSet != null) {
                broker.deleteBean(recordSet); // Assumes RSN-->RSK cascading delete!
            }

            DataDictionaryTable table =
                    DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey())
                            .findDataDictionaryTableByClass(
                                    SisStudent.class.getName());

            recordSet = X2BaseBean.newInstance(RecordSet.class, getBroker().getPersistenceKey());
            recordSet.setName(recordSetName);
            recordSet.setDataTableOid(table.getSystemOid());

            switch (((Integer) getParameter(SNAPSHOT_OWNER_PARAM)).intValue()) {
                case Ownable.OWNER_TYPE_ORG1:
                    recordSet.setOwnerOid(getOrganization().getOid());
                    recordSet.setOwnerType(Ownable.OWNER_TYPE_ORG1);
                    break;

                case Ownable.OWNER_TYPE_SCHOOL:
                    recordSet.setOwnerOid(getSchool().getOid());
                    recordSet.setOwnerType(Ownable.OWNER_TYPE_SCHOOL);
                    break;

                case Ownable.OWNER_TYPE_USER:
                default:
                    recordSet.setOwnerOid(getUser().getOid());
                    recordSet.setOwnerType(Ownable.OWNER_TYPE_USER);
                    break;
            }

            broker.saveBean(recordSet);
        }
        return recordSet;
    }
}
