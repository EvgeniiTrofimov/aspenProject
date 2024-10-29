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
package com.x2dev.reports.sys.sped;
/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2011 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.IepAccessLog;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepTeamMember;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.Section;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.SisUser;
import com.x2dev.sis.model.beans.StudentSchedule;
import com.x2dev.utils.KeyValuePair;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Data source for the "IEP Access Log" report.
 *
 * <p>
 * Generates access log reports, how many times a staff accessed an IEP
 * </p>
 *
 * @author X2 Development Corporation
 */
public class IepAccessLogByStudentData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Input parameter containing the end date in the date range to display. If null, an end date is
     * not applied.
     */
    public static final String PARAM_END_DATE = "endDate";

    /**
     * Input parameter containing the integer that identifies the query option.
     */
    public static final String PARAM_QUERY_BY = "queryBy";

    /**
     * Input parameter containing the integer that identifies the grouping.
     */
    public static final String PARAM_GROUP_BY = "groupBy";

    /**
     * Input parameter containing the start date in the date range to display. If null, start date
     * is not applied.
     */
    public static final String PARAM_START_DATE = "startDate";

    /*
     * Delimiter for unique person id for maps, is also used for the IEP Type delimiter
     */
    private static final String KEY_DELIMITER = "_";

    /*
     * Grid Field Constants
     */
    private static final String FIELD_COUNT = "Count";
    private static final String FIELD_FIRST_DATE = "FirstDate";
    private static final String FIELD_IEP_DATA = "IepData";
    private static final String FIELD_IEP_TYPE = "IepType";
    private static final String FIELD_IS_TEACHER = "IsTeacher";
    private static final String FIELD_LAST_DATE = "LastDate";
    private static final String FIELD_PERSON_KEY = "PersonKey";
    private static final String FIELD_ROLE = "Role";
    private static final String FIELD_STUDENT = "Student";

    // Person is the staff on the Report
    private static final String FIELD_PERSON = "Person";

    /*
     * Create Grid Variables
     */
    private PlainDate m_endDate;
    private ReportDataGrid m_grid;
    private IepData m_iep;
    private Criteria m_iepAccessLogCriteria;
    private QueryByCriteria m_query;
    private Map<String, Collection<StudentSchedule>> m_studentSectionsByStudentOid;
    private PlainDate m_startDate;
    private List<IepData.StatusCode> m_statusCodes;

    // Iep Data Oid <Staff Oid, <First Accessed Date/Last Accessed Date>>
    private HashMap<String, HashMap<String, KeyValuePair<PlainDate, PlainDate>>> m_dateMap =
            new HashMap<String, HashMap<String, KeyValuePair<PlainDate, PlainDate>>>();

    // Iep Data Oid <Staff Oid, Access Count>
    private HashMap<String, HashMap<String, Integer>> m_countMap = new HashMap<String, HashMap<String, Integer>>();

    // Iep Data Oid <Staff Oid, <Role (Or) Course/Is Teacher?>>
    private HashMap<String, HashMap<String, KeyValuePair<String, Boolean>>> m_roleMap =
            new HashMap<String, HashMap<String, KeyValuePair<String, Boolean>>>();

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.x2dev.sis.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        m_grid = new ReportDataGrid(5);
        m_startDate = (PlainDate) getParameter(PARAM_START_DATE);
        m_endDate = (PlainDate) getParameter(PARAM_END_DATE);

        createAccessLogQuery(m_startDate, m_endDate);
        loadStudentSections();
        buildMap();
        populateGrid();
        createSort();

        m_grid.beforeTop();

        return m_grid;
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @see
     *      com.x2dev.sis.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) {
        m_iep = userData.getCurrentRecord(IepData.class);
    }

    /**
     * Adds accessed staff to map.
     *
     * @param currentUser SisUser
     * @param iepOid String
     * @param date PlainDate
     * @param personKey String
     */
    private void addAccessedStaff(SisUser currentUser,
                                  String iepOid,
                                  PlainDate date,
                                  String personKey) {

        PlainDate firstAccess = null;
        PlainDate lastAccess = null;

        KeyValuePair<PlainDate, PlainDate> datePair;

        if (m_countMap.containsKey(iepOid)) {
            if (m_countMap.get(iepOid).containsKey(personKey)
                    && m_countMap.get(iepOid).get(personKey).intValue() != 0) {
                datePair = m_dateMap.get(iepOid).get(personKey);

                if ((datePair.getKey() != null)) {
                    if (datePair.getKey().after(date)) {
                        firstAccess = date;
                    } else {
                        firstAccess = datePair.getKey();
                    }
                }

                if (datePair.getValue().before(date)) {
                    lastAccess = date;
                } else {
                    lastAccess = datePair.getValue();
                }
                m_countMap.get(iepOid).put(personKey,
                        Integer.valueOf(m_countMap.get(iepOid).get(personKey).intValue() + 1));
                m_dateMap.get(iepOid).put(personKey, new KeyValuePair<PlainDate, PlainDate>(firstAccess, lastAccess));
            } else {
                m_dateMap.get(iepOid).put(personKey, new KeyValuePair<PlainDate, PlainDate>(date, date));
                m_countMap.get(iepOid).put(personKey, Integer.valueOf(1));
                m_roleMap.get(iepOid).put(personKey,
                        new KeyValuePair<String, Boolean>(getRole(currentUser, iepOid), Boolean.valueOf(false)));
            }
        } else {
            HashMap<String, KeyValuePair<PlainDate, PlainDate>> tempDate =
                    new HashMap<String, KeyValuePair<PlainDate, PlainDate>>();
            tempDate.put(personKey, new KeyValuePair<PlainDate, PlainDate>(date, date));
            m_dateMap.put(iepOid, tempDate);

            HashMap<String, Integer> tempCount = new HashMap<String, Integer>();
            tempCount.put(personKey, Integer.valueOf(1));
            m_countMap.put(iepOid, tempCount);

            HashMap<String, KeyValuePair<String, Boolean>> tempRole =
                    new HashMap<String, KeyValuePair<String, Boolean>>();
            tempRole.put(personKey,
                    new KeyValuePair<String, Boolean>(getRole(currentUser, iepOid), Boolean.valueOf(false)));
            m_roleMap.put(iepOid, tempRole);
        }
    }

    /**
     * Populates grid with Scheduled teachers who have not accessed the Iep Data.
     */
    private void buildMap() {
        QueryIterator iter = getBroker().getIteratorByQuery(m_query);

        try {
            while (iter.hasNext()) {
                IepAccessLog currentLog = (IepAccessLog) iter.next();

                String studentOid = currentLog.getIepData().getStudentOid();
                String iepOid = currentLog.getIepDataOid();
                SisUser currentUser = currentLog.getUser();
                PlainDate date = currentLog.getDate();

                String personKey = currentLog.getNameView() + "_";

                if (currentLog.getUserOid() != null) {
                    personKey += currentLog.getUserOid();
                }

                loadTeachers(studentOid, iepOid);

                addAccessedStaff(currentUser, iepOid, date, personKey);
            }
        } finally {
            iter.close();
        }
    }

    /**
     * Creates the query for the access log to find the specific Iep Log within a certain date
     * range.
     *
     * @param startDate PlainDate
     * @param endDate PlainDate
     * @return QueryByCriteria
     */
    private void createAccessLogQuery(PlainDate startDate, PlainDate endDate) {
        m_iepAccessLogCriteria = new Criteria();

        if (m_iep != null) {
            m_iepAccessLogCriteria.addEqualTo(IepAccessLog.COL_IEP_DATA_OID, m_iep.getOid());
        } else {
            String queryBy = (String) getParameter(PARAM_QUERY_BY);
            addUserCriteria(m_iepAccessLogCriteria, queryBy, "", IepAccessLog.class, IepData.class,
                    IepAccessLog.COL_IEP_DATA_OID);

            if (queryBy.equals("##all")) {
                if (isSchoolContext()) {
                    m_iepAccessLogCriteria.addEqualTo(IepAccessLog.REL_IEP_DATA + PATH_DELIMITER + IepData.REL_STUDENT
                            + PATH_DELIMITER + SisStudent.COL_SCHOOL_OID, getSchool().getOid());
                } else {
                    X2Criteria studentOrgCriteria = getOrganizationCriteria(SisStudent.class);
                    m_iepAccessLogCriteria.addIn(IepAccessLog.REL_IEP_DATA + PATH_DELIMITER + IepData.COL_STUDENT_OID,
                            new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentOrgCriteria));
                }
            }
        }

        if (startDate != null) {
            m_iepAccessLogCriteria.addGreaterOrEqualThan(IepAccessLog.COL_DATE, startDate);
        }
        if (endDate != null) {
            m_iepAccessLogCriteria.addLessOrEqualThan(IepAccessLog.COL_DATE, endDate);
        }

        m_iepAccessLogCriteria.addNotNull(IepAccessLog.REL_IEP_DATA + PATH_DELIMITER + IepData.COL_STUDENT_OID);

        m_query = new QueryByCriteria(IepAccessLog.class, m_iepAccessLogCriteria);
        m_query.addOrderByAscending(IepAccessLog.COL_NAME_VIEW);
    }

    /**
     * Sorts the grid.
     */
    private void createSort() {
        ArrayList<String> sortList = new ArrayList<String>();
        sortList.add(FIELD_STUDENT + PATH_DELIMITER + SisStudent.COL_NAME_VIEW);
        sortList.add(FIELD_IEP_DATA + PATH_DELIMITER + X2BaseBean.COL_OID);
        sortList.add(FIELD_IS_TEACHER);
        sortList.add(FIELD_PERSON);

        ArrayList<Boolean> sortOrders = new ArrayList<Boolean>();
        sortOrders.add(Boolean.valueOf(true));
        sortOrders.add(Boolean.valueOf(true));
        sortOrders.add(Boolean.valueOf(false));
        sortOrders.add(Boolean.valueOf(true));

        m_grid.sort(sortList, sortOrders, false);
    }

    /**
     * Returns the currentUser's role.
     *
     * @param currentUser SisUser
     * @param iepOid String
     * @return String
     */
    private String getRole(SisUser currentUser, String iepOid) {
        String role = null;

        if (currentUser != null) {
            IepTeamMember teamMember = getTeamMember(currentUser.getPersonOid(), iepOid);

            if (teamMember != null) {
                role = teamMember.getMemberRoleCode();
            }
        }

        return role;
    }

    /**
     * Returns the current team member record.
     *
     * @param personOid String
     * @param iepOid String
     * @return IepTeamMember
     */
    private IepTeamMember getTeamMember(String personOid, String iepOid) {
        Criteria teamMemberCriteria = new Criteria();

        teamMemberCriteria.addEqualTo(IepTeamMember.COL_PERSON_OID, personOid);
        teamMemberCriteria.addEqualTo(IepTeamMember.COL_IEP_DATA_OID, iepOid);

        QueryByCriteria teamMemberQuery = new QueryByCriteria(IepTeamMember.class, teamMemberCriteria);

        return (IepTeamMember) getBroker().getBeanByQuery(teamMemberQuery);
    }

    /**
     * Loads a collection of student schedule beans keyed on student OID.
     */
    private void loadStudentSections() {
        Criteria studentScheduleCriteria = new Criteria();
        studentScheduleCriteria.addEqualTo(
                StudentSchedule.REL_SCHEDULE + PATH_DELIMITER + Schedule.COL_DISTRICT_CONTEXT_OID,
                getCurrentContext().getOid());
        studentScheduleCriteria
                .addNotNull(StudentSchedule.REL_SECTION + PATH_DELIMITER + Section.COL_PRIMARY_STAFF_OID);

        SubQuery studentSub = new SubQuery(IepAccessLog.class,
                IepAccessLog.REL_IEP_DATA + PATH_DELIMITER + IepData.COL_STUDENT_OID,
                m_iepAccessLogCriteria,
                true);
        studentScheduleCriteria.addIn(StudentSchedule.COL_STUDENT_OID, studentSub);

        QueryByCriteria query = new QueryByCriteria(StudentSchedule.class, studentScheduleCriteria);

        m_studentSectionsByStudentOid =
                getBroker().getGroupedCollectionByQuery(query, StudentSchedule.COL_STUDENT_OID, 500);
    }

    /**
     * Adds scheduled teachers to Map.
     *
     * @param studentOid String
     * @param iepOid String
     */
    private void loadTeachers(String studentOid, String iepOid) {
        Collection<StudentSchedule> studentSections = m_studentSectionsByStudentOid.get(studentOid);
        if (studentSections != null) {
            for (StudentSchedule studentSection : studentSections) {
                Section section = studentSection.getSection();
                String nameView = section.getPrimaryStaff().getNameView();
                SisUser curUser = section.getPrimaryStaff().getPerson().getUser();

                String personKey = nameView + KEY_DELIMITER;

                if (curUser != null) {
                    String userOid = curUser.getOid();
                    personKey += userOid;
                }

                String course = section.getCourseView() + " " + section.getDescription();

                if (!m_countMap.containsKey(iepOid)) {
                    HashMap<String, Integer> tempCount = new HashMap<String, Integer>();
                    tempCount.put(personKey, Integer.valueOf(0));
                    m_countMap.put(iepOid, tempCount);

                    HashMap<String, KeyValuePair<PlainDate, PlainDate>> tempDate =
                            new HashMap<String, KeyValuePair<PlainDate, PlainDate>>();
                    tempDate.put(personKey, new KeyValuePair<PlainDate, PlainDate>(null, null));
                    m_dateMap.put(iepOid, tempDate);

                    HashMap<String, KeyValuePair<String, Boolean>> tempRole =
                            new HashMap<String, KeyValuePair<String, Boolean>>();
                    tempRole.put(personKey, new KeyValuePair<String, Boolean>(course, Boolean.valueOf(true)));
                    m_roleMap.put(iepOid, tempRole);
                } else if (!m_countMap.get(iepOid).containsKey(personKey)) {
                    m_countMap.get(iepOid).put(personKey, Integer.valueOf(0));
                    m_dateMap.get(iepOid).put(personKey, new KeyValuePair<PlainDate, PlainDate>(null, null));
                    m_roleMap.get(iepOid).put(personKey, new KeyValuePair<String, Boolean>(course, Boolean.valueOf(true)));
                } else {
                    m_roleMap.get(iepOid).put(personKey, new KeyValuePair<String, Boolean>(course, Boolean.valueOf(true)));
                }
            }
        }
    }

    /**
     * Adds the student information to the grid.
     */
    private void populateGrid() {
        m_statusCodes = Arrays.asList(IepData.StatusCode.values());

        for (String iepOid : m_countMap.keySet()) {
            IepData iepData = ((IepData) getBroker().getBeanByOid(IepData.class, iepOid));
            SisStudent student = iepData.getStudent();

            for (String personKey : m_countMap.get(iepOid).keySet()) {
                String personName = personKey.substring(0, personKey.lastIndexOf(KEY_DELIMITER));

                Integer count = m_countMap.get(iepOid).get(personKey);
                String role = m_roleMap.get(iepOid).get(personKey).getKey();
                Boolean isTeacher = m_roleMap.get(iepOid).get(personKey).getValue();
                PlainDate firstDate = m_dateMap.get(iepOid).get(personKey).getKey();
                PlainDate lastDate = m_dateMap.get(iepOid).get(personKey).getValue();

                m_grid.append();
                m_grid.set(FIELD_ROLE, role);
                m_grid.set(FIELD_STUDENT, student);
                m_grid.set(FIELD_IEP_DATA, iepData);
                m_grid.set(FIELD_PERSON, personName);
                m_grid.set(FIELD_COUNT, count);
                m_grid.set(FIELD_FIRST_DATE, firstDate);
                m_grid.set(FIELD_LAST_DATE, lastDate);
                m_grid.set(FIELD_IS_TEACHER, isTeacher);
                m_grid.set(FIELD_IEP_TYPE,
                        StringUtils.properCase(m_statusCodes.get(iepData.getStatusCode()).toString(), true)
                                .replace(KEY_DELIMITER, " "));
                m_grid.set(FIELD_PERSON_KEY, personKey);
            }
        }
    }
}
