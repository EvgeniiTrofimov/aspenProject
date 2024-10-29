/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2006 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.reports.sys.sped.nj;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.StudentContact;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.Publishable;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.business.sped.SpedUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.types.PlainDate;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Map;
import java.util.TreeMap;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares the data for the "NJ IEP Progress Report" report.
 *
 * @author X2 Development Corporation
 */
public class IepProgressReportData extends ReportJavaSourceNet implements Publishable {
    /**
     * Publishing Parameters
     */
    public static final String NO_PUBLISH_EMAIL_PARAM = "noEmail";

    /**
     * Input definition parameter names.
     */
    public static final String PARAM_CONTEXT_OID = "contextOid";
    public static final String PARAM_END_DATE = "endDate";
    public static final String PARAM_INCLUDE_UNPOSTED = "includeUnposted";
    public static final String PARAM_REPORT_DATE = "printedDate";
    public static final String PARAM_REPORT_PERIOD = "reportPeriod";
    public static final String PARAM_STAFF_OID = "staffOid";
    public static final String PARAM_START_DATE = "startDate";

    public static final String PUBLISH_RECIPIENTS_PARAM = "publishTo";


    /**
     * Aliases
     */
    protected static final String ALIAS_DEPT_SUPERVISOR = "DOE DEPT SUPERVISOR";
    protected static final String ALIAS_GOAL_ACADEMIC_FOCUS = "iep-acad-goal-focus";
    protected static final String ALIAS_OBJECTIVE_BENCHMARK = "iep-acad-short-term-obj";
    protected static final String ALIAS_OBJECTIVE_CRITERIA = "iep-acad-obj-criteria";
    protected static final String ALIAS_OBJECTIVE_PROGRESS_CODE = "iep-acad-prog-code";
    protected static final String ALIAS_SCHOOL_FAX = "DOE SCHOOL FAX";
    protected static final String ALIAS_STAFF_ROLE = "DOE STAFF ROLE";

    protected static final int CODE_PUBLISH_TO_ALL = 0;
    protected static final int CODE_PUBLISH_TO_CONTACTS = 1;
    protected static final int CODE_PUBLISH_TO_STUDENTS = 2;

    protected static final String FAX = "FAX";

    protected static final String REGEX_P1 = "P1|Q1";
    protected static final String REGEX_P2 = "P2|Q2";
    protected static final String REGEX_P3 = "P3|Q3";
    protected static final String REGEX_P4 = "P4|Q4";

    protected static final String REPORT_FIELD_EVAL_P1 = "P1";
    protected static final String REPORT_FIELD_EVAL_P2 = "P2";
    protected static final String REPORT_FIELD_EVAL_P3 = "P3";
    protected static final String REPORT_FIELD_EVAL_P4 = "P4";
    protected static final String REPORT_FIELD_GOAL_ACEDEMIC_AREA = "goalAcademicArea";
    protected static final String REPORT_FIELD_GOAL_DESCRIPTION = "goalDescription";
    protected static final String REPORT_FIELD_OBJECTIVE_BENCHMARK = "objectiveBenchmark";
    protected static final String REPORT_FIELD_OBJECTIVE_CRITERIA = "objectiveCriteria";
    protected static final String REPORT_FIELD_OBJECTIVE_SEQUENCE = "objectiveSequence";
    protected static final String REPORT_FIELD_REPORT_DATE = "reportPrintDate";
    protected static final String REPORT_FIELD_REPORT_PERIOD = "reportPeriod";
    protected static final String REPORT_FIELD_SCHOOL_ADDRESS_LINE1 = "schoolAddressLine1";
    protected static final String REPORT_FIELD_SCHOOL_ADDRESS_LINE2 = "schoolAddressLine2";
    protected static final String REPORT_FIELD_SCHOOL_ADMIN1 = "schoolAdmin1";
    protected static final String REPORT_FIELD_SCHOOL_ADMIN2 = "schoolAdmin2";
    protected static final String REPORT_FIELD_SCHOOL_ADMIN3 = "schoolAdmin3";
    protected static final String REPORT_FIELD_SCHOOL_FAX_NUMBER = "schoolFaxNumber";
    protected static final String REPORT_FIELD_SCHOOL_NAME = "schoolName";
    protected static final String REPORT_FIELD_SCHOOL_PHONE_NUMBER = "schoolPhoneNumber";
    protected static final String REPORT_FIELD_STAFF_NAME = "staffName";
    protected static final String REPORT_FIELD_STUDENT_NAME = "studentName";
    protected static final String REPORT_FIELD_STUDENT_OID = "studentOid";

    protected static final String REPORT_PARAM_SCHOOL_ADDRESS_LINE1 = "schoolAddressLine1";
    protected static final String REPORT_PARAM_SCHOOL_ADDRESS_LINE2 = "schoolAddressLine2";
    protected static final String REPORT_PARAM_SCHOOL_ADMIN1 = "schoolAdmin1";
    protected static final String REPORT_PARAM_SCHOOL_ADMIN2 = "schoolAdmin2";
    protected static final String REPORT_PARAM_SCHOOL_ADMIN3 = "schoolAdmin3";
    protected static final String REPORT_PARAM_SCHOOL_FAX_NUMBER = "schoolFaxNumber";
    protected static final String REPORT_PARAM_SCHOOL_PHONE_NUMBER = "schoolPhoneNumber";
    protected static final String REPORT_PARAM_SCHOOL_YEAR_CONTEXT = "schoolYearContext";
    protected static final String REPORT_PARAM_SSS_STAFF = "sssStaff";

    protected static final String STRING_COMMA = ",";
    protected static final String STRING_EMPTY = "";
    protected static final String STRING_HYPHEN = "-";
    protected static final String STRING_SPACE = " ";

    /**
     * Variables
     */
    private String m_contextOid;
    private SisStudent m_currentStudent;
    private Map<String, Collection<StudentContact>> m_emailRecipients;
    private PlainDate m_endDate;
    private IepData m_iep = null;
    private DataDictionary m_iepDictionary;
    private Collection<IepGoalProgress> m_iepGoalProgressList;
    private boolean m_includeUnposted;
    private String m_postingPeriod;
    private IepGoalProgress m_progress = null;
    private PlainDate m_reportPrintDate;
    private SimpleDateFormat m_shortDateFormat = new SimpleDateFormat("MM/dd/yyyy");
    private String m_sssStaffName;
    private String m_staffOid;
    private PlainDate m_startDate;

    /**
     * The Class GoalProgressRecord.
     */
    private class GoalProgressRecord {
        private Integer objectiveSequence;
        private SisStudent student;
        private String goalAcademicArea;
        private String goalDescription;
        private String staffName;
        private String objectiveBenchmark;
        private String objectiveCriteria;
        private String progressPeriod1;
        private String progressPeriod2;
        private String progressPeriod3;
        private String progressPeriod4;
        private String reportPeriod;

        /**
         * Instantiates a new goal progress record.
         */
        public GoalProgressRecord() {

        }

        /**
         * Gets the student.
         *
         * @return Sis student
         */
        public SisStudent getStudent() {
            return student;
        }

        /**
         * Sets the student.
         *
         * @param student void
         */
        public void setStudent(SisStudent student) {
            this.student = student;
        }

        /**
         * Gets the goal academic area.
         *
         * @return String
         */
        public String getGoalAcademicArea() {
            return goalAcademicArea;
        }

        /**
         * Sets the goal academic area.
         *
         * @param goalAcademicArea void
         */
        public void setGoalAcademicArea(String goalAcademicArea) {
            this.goalAcademicArea = goalAcademicArea;
        }

        /**
         * Gets the goal description.
         *
         * @return String
         */
        public String getGoalDescription() {
            return goalDescription;
        }

        /**
         * Sets the goal description.
         *
         * @param goalDescription void
         */
        public void setGoalDescription(String goalDescription) {
            this.goalDescription = goalDescription;
        }

        /**
         * Gets the staff name.
         *
         * @return String
         */
        public String getStaffName() {
            return staffName;
        }

        /**
         * Sets the staff name.
         *
         * @param staffName void
         */
        public void setStaffName(String staffName) {
            this.staffName = staffName;
        }

        /**
         * Gets the objective benchmark.
         *
         * @return String
         */
        public String getObjectiveBenchmark() {
            return objectiveBenchmark;
        }

        /**
         * Sets the objective benchmark.
         *
         * @param objectiveBenchmark void
         */
        public void setObjectiveBenchmark(String objectiveBenchmark) {
            this.objectiveBenchmark = objectiveBenchmark;
        }

        /**
         * Gets the objective criteria.
         *
         * @return String
         */
        public String getObjectiveCriteria() {
            return objectiveCriteria;
        }

        /**
         * Sets the objective criteria.
         *
         * @param objectiveCriteria void
         */
        public void setObjectiveCriteria(String objectiveCriteria) {
            this.objectiveCriteria = objectiveCriteria;
        }

        /**
         * Gets the objective sequence.
         *
         * @return Integer
         */
        public Integer getObjectiveSequence() {
            return objectiveSequence;
        }

        /**
         * Sets the objective sequence.
         *
         * @param objectiveSequence void
         */
        public void setObjectiveSequence(Integer objectiveSequence) {
            this.objectiveSequence = objectiveSequence;
        }

        /**
         * Gets the progress period 1.
         *
         * @return String
         */
        public String getProgressPeriod1() {
            return progressPeriod1;
        }

        /**
         * Sets the progress period 1.
         *
         * @param progressPeriod1 void
         */
        public void setProgressPeriod1(String progressPeriod1) {
            this.progressPeriod1 = progressPeriod1;
        }

        /**
         * Gets the progress period 2.
         *
         * @return String
         */
        public String getProgressPeriod2() {
            return progressPeriod2;
        }

        /**
         * Sets the progress period 2.
         *
         * @param progressPeriod2 void
         */
        public void setProgressPeriod2(String progressPeriod2) {
            this.progressPeriod2 = progressPeriod2;
        }

        /**
         * Gets the progress period 3.
         *
         * @return String
         */
        public String getProgressPeriod3() {
            return progressPeriod3;
        }

        /**
         * Sets the progress period 3.
         *
         * @param progressPeriod3 void
         */
        public void setProgressPeriod3(String progressPeriod3) {
            this.progressPeriod3 = progressPeriod3;
        }

        /**
         * Gets the progress period 4.
         *
         * @return String
         */
        public String getProgressPeriod4() {
            return progressPeriod4;
        }

        /**
         * Sets the progress period 4.
         *
         * @param progressPeriod4 void
         */
        public void setProgressPeriod4(String progressPeriod4) {
            this.progressPeriod4 = progressPeriod4;
        }

        /**
         * Gets the report period.
         *
         * @return String
         */
        public String getReportPeriod() {
            return reportPeriod;
        }

        /**
         * Sets the report period.
         *
         * @param reportPeriod void
         */
        public void setReportPeriod(String reportPeriod) {
            this.reportPeriod = reportPeriod;
        }
    }

    /**
     * Gets the data break column.
     *
     * @return String
     * @see com.x2dev.sis.tools.reports.Publishable#getDataBreakColumn()
     */
    @Override
    public String getDataBreakColumn() {
        return "studentOid";
    }

    /**
     * Gets the description.
     *
     * @param bean X2BaseBean
     * @return String
     * @see
     *      com.x2dev.sis.tools.reports.Publishable#getDescription(com.x2dev.sis.model.beans.X2BaseBean)
     */
    @Override
    public String getDescription(X2BaseBean bean) {
        return "Published Reports " + ((SisStudent) bean).getNameView();
    }

    /**
     * Gets the email address.
     *
     * @param person Person
     * @return String
     * @see com.x2dev.sis.tools.reports.Publishable#getEmailAddress(SisPerson)
     */
    @Override
    public String getEmailAddress(Person person) {
        boolean sendEmail = ((Boolean) getParameter(NO_PUBLISH_EMAIL_PARAM)).booleanValue();
        if (!sendEmail) {
            return person.getEmail01();
        }
        return STRING_EMPTY;
    }

    /**
     * Gets the email recipients.
     *
     * @param bean X2BaseBean
     * @return Collection
     * @see com.x2dev.sis.tools.reports.Publishable#getEmailRecipients(X2BaseBean)
     */
    @Override
    public Collection<Person> getEmailRecipients(X2BaseBean bean) {
        int publishTo = ((Integer) getParameter(PUBLISH_RECIPIENTS_PARAM)).intValue();
        Collection<Person> recipients = null;
        recipients = new ArrayList<Person>(3);

        if (publishTo == CODE_PUBLISH_TO_ALL || publishTo == CODE_PUBLISH_TO_STUDENTS) {
            recipients.add(((SisStudent) bean).getPerson());
        }

        if (publishTo == CODE_PUBLISH_TO_ALL || publishTo == CODE_PUBLISH_TO_CONTACTS) {
            if (m_emailRecipients == null) {
                loadEmailRecipients();
            }

            Collection<StudentContact> contacts = m_emailRecipients.get(bean.getOid());
            if (contacts != null) {

                for (StudentContact contact : contacts) {
                    if ((SisPerson) contact.getPerson() != null) {
                        recipients.add(contact.getPerson());
                    }
                }
            }
        }

        return recipients;
    }

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        ReportDataGrid grid = new ReportDataGrid();

        initializeFields();

        SisDistrictSchoolYearContext schoolYearContext = (SisDistrictSchoolYearContext) getBroker()
                .getBeanByOid(SisDistrictSchoolYearContext.class, m_contextOid);
        addParameter(REPORT_PARAM_SCHOOL_YEAR_CONTEXT, schoolYearContext);

        loadIEPGoalProgressObjectivesList();

        SisStudent student = null;

        TreeMap<String, GoalProgressRecord> records = new TreeMap<String, GoalProgressRecord>();

        // Combine multiple GoalProgresses of the same Goal/Staff/Objective to one record.
        for (IepGoalProgress goalProgress : m_iepGoalProgressList) {
            IepGoal iepGoal = goalProgress.getIepGoal();
            IepGoalObjective iepGoalObjective = goalProgress.getIepGoalObjective();

            student = iepGoal.getStudent();

            String staffOid = STRING_EMPTY;
            String staffName = STRING_EMPTY;
            if (goalProgress.getStaff() != null) {
                staffName = goalProgress.getStaff().getNameView();
                staffOid = goalProgress.getStaffOid();
            }

            // Goal
            String goalAcademicArea = STRING_EMPTY;
            String goalDescription = STRING_EMPTY;

            if (iepGoal != null) {
                goalAcademicArea = (String) iepGoal.getFieldValueByAlias(ALIAS_GOAL_ACADEMIC_FOCUS, m_iepDictionary);
                if (goalAcademicArea == null) {
                    goalAcademicArea = STRING_EMPTY;
                }

                goalDescription = iepGoal.getGoal();
                if (goalDescription == null) {
                    goalDescription = STRING_EMPTY;
                }
            }

            // Goal Objective
            String objectiveBenchmark = STRING_EMPTY;
            String objectiveCriteria = STRING_EMPTY;
            Integer objectiveSequence = null;
            if (iepGoalObjective != null) {
                objectiveBenchmark =
                        (String) iepGoalObjective.getFieldValueByAlias(ALIAS_OBJECTIVE_BENCHMARK, m_iepDictionary);
                if (objectiveBenchmark == null) {
                    objectiveBenchmark = STRING_EMPTY;
                }

                objectiveCriteria =
                        (String) iepGoalObjective.getFieldValueByAlias(ALIAS_OBJECTIVE_CRITERIA, m_iepDictionary);
                if (objectiveCriteria == null) {
                    objectiveCriteria = STRING_EMPTY;
                }
                objectiveSequence = Integer.valueOf(iepGoalObjective.getSequenceNumber());
            }

            String quarter = STRING_EMPTY;
            if (goalProgress.getReportPeriod() != null) {
                quarter = goalProgress.getReportPeriod();
            }

            String progressCode = STRING_EMPTY;
            if (goalProgress.getProgressCode() != null) {
                progressCode = goalProgress.getProgressCode();
            }

            // GoalProgresses are unique by GoalAcademeicArea/Staff/GoalObjectiveBenchmark
            String key = goalAcademicArea + STRING_HYPHEN + staffOid + STRING_HYPHEN + objectiveBenchmark;
            if (records.containsKey(key)) {
                GoalProgressRecord currentRecord = records.get(key);
                if (quarter.matches(REGEX_P1)) {
                    currentRecord.setProgressPeriod1(progressCode);
                } else if (quarter.matches(REGEX_P2)) {
                    currentRecord.setProgressPeriod2(progressCode);
                } else if (quarter.matches(REGEX_P3)) {
                    currentRecord.setProgressPeriod3(progressCode);
                } else if (quarter.matches(REGEX_P4)) {
                    currentRecord.setProgressPeriod4(progressCode);
                }

                records.put(key, currentRecord);
            } else {
                GoalProgressRecord currentRecord = new GoalProgressRecord();

                currentRecord.setStudent(student);
                currentRecord.setGoalAcademicArea(goalAcademicArea);
                currentRecord.setGoalDescription(goalDescription);
                currentRecord.setObjectiveBenchmark(objectiveBenchmark);
                currentRecord.setObjectiveCriteria(objectiveCriteria);
                currentRecord.setObjectiveSequence(objectiveSequence);
                currentRecord.setStaffName(staffName);
                currentRecord.setProgressPeriod1(STRING_EMPTY);
                currentRecord.setProgressPeriod2(STRING_EMPTY);
                currentRecord.setProgressPeriod3(STRING_EMPTY);
                currentRecord.setProgressPeriod4(STRING_EMPTY);
                currentRecord.setReportPeriod(quarter);
                if (quarter.matches(REGEX_P1)) {
                    currentRecord.setProgressPeriod1(progressCode);
                } else if (quarter.matches(REGEX_P2)) {
                    currentRecord.setProgressPeriod2(progressCode);
                } else if (quarter.matches(REGEX_P3)) {
                    currentRecord.setProgressPeriod3(progressCode);
                } else if (quarter.matches(REGEX_P4)) {
                    currentRecord.setProgressPeriod4(progressCode);
                }

                records.put(key, currentRecord);
            }

        }

        Collection<GoalProgressRecord> newRecords = records.values();
        for (GoalProgressRecord record : newRecords) {
            grid.append();

            setReportHeader(grid, record.getStudent());

            String schoolName = STRING_EMPTY;
            String studentName = STRING_EMPTY;
            String studentOid = STRING_EMPTY;

            if (record.getStudent() != null) {
                studentName = record.getStudent().getNameView();
                studentOid = record.getStudent().getOid();

                SisSchool studentSchool = record.getStudent().getSchool();
                if (studentSchool != null) {
                    schoolName = studentSchool.getName();
                }
            }

            grid.set(REPORT_FIELD_SCHOOL_NAME, schoolName);
            grid.set(REPORT_FIELD_STUDENT_NAME, studentName);
            grid.set(REPORT_FIELD_STUDENT_OID, studentOid);
            grid.set(REPORT_FIELD_GOAL_ACEDEMIC_AREA, record.getGoalAcademicArea());
            grid.set(REPORT_FIELD_GOAL_DESCRIPTION, record.getGoalDescription());
            grid.set(REPORT_FIELD_STAFF_NAME, record.getStaffName());
            grid.set(REPORT_FIELD_OBJECTIVE_BENCHMARK, record.getObjectiveBenchmark());
            grid.set(REPORT_FIELD_OBJECTIVE_CRITERIA, record.getObjectiveCriteria());
            grid.set(REPORT_FIELD_OBJECTIVE_SEQUENCE, record.getObjectiveSequence());
            grid.set(REPORT_FIELD_EVAL_P1, record.getProgressPeriod1());
            grid.set(REPORT_FIELD_EVAL_P2, record.getProgressPeriod2());
            grid.set(REPORT_FIELD_EVAL_P3, record.getProgressPeriod3());
            grid.set(REPORT_FIELD_EVAL_P4, record.getProgressPeriod4());
            grid.set(REPORT_FIELD_REPORT_PERIOD, record.getReportPeriod());
            grid.set(REPORT_FIELD_REPORT_DATE, String.valueOf(m_shortDateFormat.format(m_reportPrintDate)));
        }
        grid.sort(
                Arrays.asList(new String[] {REPORT_FIELD_STUDENT_NAME, REPORT_FIELD_GOAL_ACEDEMIC_AREA,
                        REPORT_FIELD_GOAL_DESCRIPTION, REPORT_FIELD_OBJECTIVE_SEQUENCE,
                        REPORT_FIELD_OBJECTIVE_BENCHMARK, REPORT_FIELD_REPORT_PERIOD, REPORT_FIELD_STAFF_NAME}),
                false);
        grid.beforeTop();

        return grid;
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
        if (!userData.getCurrentList().getDataClass().equals(IepData.class)) {
            m_iep = userData.getCurrentRecord(IepData.class);
        }

        m_progress = userData.getCurrentRecord(IepGoalProgress.class);
    }

    /**
     * Initialize Fields.
     */
    private void initializeFields() {
        m_contextOid = (String) getParameter(PARAM_CONTEXT_OID);

        m_postingPeriod = (String) getParameter(PARAM_REPORT_PERIOD);

        m_startDate = (PlainDate) getParameter(PARAM_START_DATE);

        m_endDate = (PlainDate) getParameter(PARAM_END_DATE);

        m_staffOid = (String) getParameter(PARAM_STAFF_OID);

        m_reportPrintDate = (PlainDate) getParameter(PARAM_REPORT_DATE);

        m_includeUnposted = false;
        if (getParameter(PARAM_INCLUDE_UNPOSTED) != null) {
            m_includeUnposted = ((Boolean) getParameter(PARAM_INCLUDE_UNPOSTED)).booleanValue();
        }

        SisExtendedDataDictionary extendedDictionary = SpedUtils.getIepDictionary(getOrganization(), getBroker());
        m_iepDictionary = DataDictionary.getDistrictDictionary(extendedDictionary, getBroker().getPersistenceKey());

        initSssName();
    }

    /**
     * Initialize Supervisor Name.
     */
    private void initSssName() {
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(ALIAS_STAFF_ROLE);
        if (field != null) {
            String beanPath = field.getJavaName();
            ReferenceTable refTable = field.getReferenceTable();
            Collection<ReferenceCode> codes = refTable.getReferenceCodes();

            String sssCode = null;

            for (ReferenceCode code : codes) {
                String stateCode = code.getStateCode();
                if (!StringUtils.isEmpty(stateCode) &&
                        stateCode.equals("SSS")) {
                    sssCode = code.getCode();
                }
            }

            if (!StringUtils.isEmpty(sssCode)) {
                X2Criteria criteria = new X2Criteria();
                criteria.addEqualTo(beanPath, sssCode);
                QueryByCriteria query = new QueryByCriteria(SisStaff.class, criteria);
                SisStaff sssStaff = (SisStaff) getBroker().getBeanByQuery(query);
                m_sssStaffName = sssStaff.getPerson().getFirstName() + " " + sssStaff.getPerson().getLastName();
            }
        }
    }

    /**
     * Populates the email recipients map, which stores the contacts that will be notified of
     * students published grade reports.
     */
    private void loadEmailRecipients() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(StudentContact.COL_PORTAL_ACCESS_INDICATOR, Boolean.TRUE);

        if (isSchoolContext()) {
            criteria.addEqualTo(StudentContact.REL_STUDENT + PATH_DELIMITER +
                    SisStudent.COL_SCHOOL_OID, ((SisSchool) getSchool()).getOid());
        }

        if (m_currentStudent != null) {
            criteria.addEqualTo(StudentContact.COL_STUDENT_OID, m_currentStudent.getOid());
        }

        QueryByCriteria query = new QueryByCriteria(StudentContact.class, criteria);

        m_emailRecipients = getBroker().getGroupedCollectionByQuery(query, StudentContact.COL_STUDENT_OID, 2000);
    }

    /**
     * Load IEP Goal Progress and Goal Objectives Lists.
     */
    private void loadIEPGoalProgressObjectivesList() {
        X2Criteria criteria = new X2Criteria();

        if (m_contextOid != null) {
            criteria.addEqualTo(IepGoalProgress.COL_DISTRICT_CONTEXT_OID, m_contextOid);
        }

        if (!StringUtils.isEmpty(m_postingPeriod)) {
            criteria.addEqualTo(IepGoalProgress.COL_REPORT_PERIOD, m_postingPeriod);
        }

        if (m_startDate != null) {
            criteria.addGreaterOrEqualThan(IepGoalProgress.COL_DATE, m_startDate);
        }

        if (m_endDate != null) {
            criteria.addLessOrEqualThan(IepGoalProgress.COL_DATE, m_endDate);
        }

        if (!StringUtils.isEmpty(m_staffOid)) {
            criteria.addEqualTo(IepGoalProgress.COL_STAFF_OID, m_staffOid);
        }

        if (m_includeUnposted) {
            criteria.addEqualTo(IepGoalProgress.COL_POSTED_INDICATOR, Boolean.TRUE);
        }

        if (m_progress != null) {
            criteria.addEqualTo(X2BaseBean.COL_OID, m_progress.getOid());
        } else if (m_iep != null) {
            criteria.addEqualTo(IepGoalProgress.COL_IEP_DATA_OID, m_iep.getOid());
        } else {
            SubQuery iepSubQuery = new SubQuery(IepData.class, X2BaseBean.COL_OID, getCurrentCriteria());
            criteria.addIn(IepGoalProgress.COL_IEP_DATA_OID, iepSubQuery);
        }

        QueryByCriteria iepGoalProgressQuery = new QueryByCriteria(IepGoalProgress.class, criteria);

        if (m_progress == null && m_iep == null) {
            applyCurrentSort(iepGoalProgressQuery);
        }

        iepGoalProgressQuery.addOrderByAscending(IepGoalProgress.REL_IEP_GOAL + PATH_DELIMITER + IepGoal.COL_ID);
        iepGoalProgressQuery.addOrderByAscending(IepGoalProgress.COL_STAFF_OID);
        iepGoalProgressQuery.addOrderByAscending(IepGoalProgress.COL_DATE);

        m_iepGoalProgressList = getBroker().getCollectionByQuery(iepGoalProgressQuery);

        ArrayList<String> iepGoalObjectiveOids = new ArrayList<String>();
        for (IepGoalProgress IepGoalProgress : m_iepGoalProgressList) {
            if (IepGoalProgress.getIepGoalObjectiveOid() != null) {
                iepGoalObjectiveOids.add(IepGoalProgress.getIepGoalObjectiveOid());
            }
        }
    }


    /**
     * Sets the report header.
     *
     * @param grid ReportDataGrid
     * @param student SisStudent
     */
    private void setReportHeader(ReportDataGrid grid, SisStudent student) {
        String adminName1 = STRING_EMPTY;
        grid.set(REPORT_FIELD_SCHOOL_ADDRESS_LINE1, STRING_EMPTY);
        grid.set(REPORT_FIELD_SCHOOL_ADDRESS_LINE2, STRING_EMPTY);
        grid.set(REPORT_FIELD_SCHOOL_PHONE_NUMBER, STRING_EMPTY);
        grid.set(REPORT_FIELD_SCHOOL_FAX_NUMBER, STRING_EMPTY);
        grid.set(REPORT_FIELD_SCHOOL_ADMIN1, STRING_EMPTY);
        grid.set(REPORT_FIELD_SCHOOL_ADMIN2, STRING_EMPTY);
        grid.set(REPORT_FIELD_SCHOOL_ADMIN3, STRING_EMPTY);

        SisSchool school = student.getSchool();

        if (school != null) {
            String schoolFaxNumber = (String) school.getFieldValueByAlias(ALIAS_SCHOOL_FAX);
            if (!StringUtils.isEmpty(schoolFaxNumber)) {
                schoolFaxNumber = FAX + STRING_SPACE + schoolFaxNumber;
                grid.set(REPORT_FIELD_SCHOOL_FAX_NUMBER, schoolFaxNumber);
            }

            SisAddress schoolAddress = school.getAddress();

            if (schoolAddress != null) {
                if (!StringUtils.isEmpty(schoolAddress.getPhone01())
                        || !StringUtils.isEmpty(schoolAddress.getPhone02())) {
                    String schoolPhoneNumber = STRING_EMPTY;
                    if (!StringUtils.isEmpty(schoolAddress.getPhone01())) {
                        schoolPhoneNumber = schoolAddress.getPhone01();
                    } else {
                        schoolPhoneNumber = schoolAddress.getPhone02();
                    }
                    grid.set(REPORT_FIELD_SCHOOL_PHONE_NUMBER, schoolPhoneNumber);
                }

                if (schoolAddress.getAddressLine01() != null) {
                    grid.set(REPORT_FIELD_SCHOOL_ADDRESS_LINE1, schoolAddress.getAddressLine01());
                }
                if (schoolAddress.getAddressLine03() != null) {
                    grid.set(REPORT_FIELD_SCHOOL_ADDRESS_LINE2, schoolAddress.getAddressLine03());
                }
            }

            if (school.getAdministrator1() != null) {
                SisStaff superintendent = school.getAdministrator1();
                if (superintendent != null) {
                    SisPerson adminPerson1 = superintendent.getPerson();
                    if (adminPerson1 != null) {
                        adminName1 = adminPerson1.getFirstName() + STRING_SPACE + adminPerson1.getLastName();
                        grid.set(REPORT_FIELD_SCHOOL_ADMIN1, adminName1);
                    }

                    String superintendentName = (String) superintendent.getFieldValueByAlias(ALIAS_DEPT_SUPERVISOR);
                    if (!StringUtils.isEmpty(superintendentName)) {
                        String[] adminName2 = superintendentName.split(STRING_COMMA);
                        if (adminName2 != null && adminName2.length > 1) {
                            grid.set(REPORT_FIELD_SCHOOL_ADMIN2, adminName2[1] + STRING_SPACE + adminName2[0]);
                        }
                    }
                }
            }
        }

        addParameter(REPORT_PARAM_SSS_STAFF, m_sssStaffName);
    }

}
