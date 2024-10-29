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
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.x2dev.sis.model.beans.IepAccessLog;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.Section;
import com.x2dev.sis.model.beans.SisStaff;
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
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Data source for the "IEP Access Log By Staff" report.
 *
 * <p>
 * Creates a report which lists Staff's scheduled students IEPs and how many times the staff
 * accessed the IEP
 * </p>
 *
 * @author X2 Development Corporation
 */
public class IepAccessLogByStaffData extends ReportJavaSourceNet {
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
    public static final String PARAM_SORT_BY = "sort";

    /**
     * Input parameter containing the start date in the date range to display. If null, start date
     * is not applied.
     */
    public static final String PARAM_START_DATE = "startDate";

    /**
     * Input parameter containing the type of IEP status to display on report
     */
    public static final String PARAM_STATUS = "status";

    /**
     * Input parameter containing which type of Staff to display, Only scheduled staff? Or all
     */
    public static final String PARAM_ONLY_SCHED = "onlySched";

    /*
     * Grid Field Constants
     */
    private static final String FIELD_COUNT = "count";
    private static final String FIELD_FIRST_DATE = "firstDate";
    private static final String FIELD_IEP_DATA = "iepData";
    private static final String FIELD_IEP_TYPE = "iepType";
    private static final String FIELD_LAST_DATE = "lastDate";
    private static final String FIELD_STAFF = "staff";
    private static final String FIELD_STUDENT = "student";

    private static final String MESSAGE = "NONE";

    /*
     * Create Grid Variables
     */
    private PlainDate m_endDate;
    private ReportDataGrid m_grid;
    private QueryByCriteria m_query;
    private PlainDate m_startDate;

    // Staff Oid <Iep Data Oid, <First Accessed Date/Last Accessed Date>>
    private HashMap<String, HashMap<String, KeyValuePair<PlainDate, PlainDate>>> m_dateMap =
            new HashMap<String, HashMap<String, KeyValuePair<PlainDate, PlainDate>>>();
    // Staff Oid <Iep Data Oid, Access Count>
    private HashMap<String, HashMap<String, Integer>> m_countMap = new HashMap<String, HashMap<String, Integer>>();

    // Staff OID <ArrayList <Iep OID>>
    private HashMap<String, ArrayList<String>> m_teacherIepMap = new HashMap<String, ArrayList<String>>();

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.x2dev.sis.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        m_grid = new ReportDataGrid(11);
        m_startDate = (PlainDate) getParameter(PARAM_START_DATE);
        m_endDate = (PlainDate) getParameter(PARAM_END_DATE);

        m_query = createQueryByCriteria(SisStaff.class, getCurrentCriteria());

        QueryIterator iterStaff = getBroker().getIteratorByQuery(m_query);

        try {
            while (iterStaff.hasNext()) {
                SisStaff staff = (SisStaff) iterStaff.next();
                String staffOid = staff.getOid();

                buildIepMap(staffOid);
            }
        } finally {
            iterStaff.close();
        }

        buildStudentMap();

        populateGrid();
        createSort();

        m_grid.beforeTop();

        return m_grid;
    }

    /**
     * Builds Date map .
     *
     * @param staffOid String
     * @param iepOid String
     * @param currentLog IepAccessLog
     * @param date PlainDate
     */
    private void buildDateMap(String staffOid, String iepOid, IepAccessLog currentLog, PlainDate date) {
        String personOid = ((SisStaff) getBroker().getBeanByOid(SisStaff.class, staffOid)).getPersonOid();

        PlainDate firstDate;
        PlainDate lastDate;

        HashMap<String, KeyValuePair<PlainDate, PlainDate>> iepDateMap;

        Integer accessCount;

        String accessorOid = getPersonOid(currentLog.getUser());

        if (accessorOid.equals(personOid)) {
            if (m_dateMap.containsKey(staffOid) && m_dateMap.get(staffOid).containsKey(iepOid)) {
                firstDate = m_dateMap.get(staffOid).get(iepOid).getKey();
                lastDate = m_dateMap.get(staffOid).get(iepOid).getValue();

                if (firstDate.after(date)) {
                    firstDate = date;
                }

                if (lastDate.before(date)) {
                    lastDate = date;
                }

                iepDateMap = m_dateMap.get(staffOid);

                accessCount = Integer.valueOf(m_countMap.get(staffOid).get(iepOid).intValue() + 1);
            } else {
                firstDate = date;
                lastDate = date;

                if (m_dateMap.containsKey(staffOid)) {
                    iepDateMap = m_dateMap.get(staffOid);
                } else {
                    iepDateMap = new HashMap<String, KeyValuePair<PlainDate, PlainDate>>();
                }

                accessCount = Integer.valueOf(1);
            }

            KeyValuePair<PlainDate, PlainDate> datePair = new KeyValuePair<PlainDate, PlainDate>(firstDate, lastDate);

            iepDateMap.put(iepOid, datePair);

            m_dateMap.put(staffOid, iepDateMap);

            if (m_countMap.containsKey(staffOid)) {
                m_countMap.get(staffOid).put(iepOid, accessCount);
            } else {
                HashMap<String, Integer> temp = new HashMap<String, Integer>();
                temp.put(iepOid, accessCount);
                m_countMap.put(staffOid, temp);
            }
        }
    }

    /**
     * Creates scheduled Student Map.
     *
     * @param staffOid String
     */
    private void buildIepMap(String staffOid) {
        Criteria courseCriteria = new Criteria();
        courseCriteria.addEqualTo(StudentSchedule.REL_SECTION + PATH_DELIMITER + Section.COL_PRIMARY_STAFF_OID,
                staffOid);
        courseCriteria.addEqualTo(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER + Schedule.COL_DISTRICT_CONTEXT_OID,
                getCurrentContext().getOid());

        QueryByCriteria query = new QueryByCriteria(StudentSchedule.class, courseCriteria);

        QueryIterator iter = getBroker().getIteratorByQuery(query);

        try {
            if (((Integer) getParameter(PARAM_ONLY_SCHED)).intValue() == 1) {
                if (!iter.hasNext()) {
                    ArrayList<String> tempIepList = new ArrayList<String>();
                    tempIepList.add(MESSAGE);
                    m_teacherIepMap.put(staffOid, tempIepList);
                }
            }

            while (iter.hasNext()) {
                StudentSchedule studentSchedule = (StudentSchedule) iter.next();
                SisStudent student = studentSchedule.getStudent();
                Collection<IepData> iepDataList = student.getIepData();

                for (IepData iepData : iepDataList) {
                    if (((Integer) getParameter(PARAM_STATUS)).intValue() == 9
                            || iepData.getStatusCode() == ((Integer) getParameter(PARAM_STATUS)).intValue()) {
                        if (m_teacherIepMap.get(staffOid) == null) {
                            ArrayList<String> tempIepList = new ArrayList<String>();
                            tempIepList.add(iepData.getOid());
                            m_teacherIepMap.put(staffOid, tempIepList);
                        } else {
                            if (!m_teacherIepMap.get(staffOid).contains(iepData.getOid())) {
                                ArrayList<String> tempIepList = m_teacherIepMap.get(staffOid);
                                tempIepList.add(iepData.getOid());
                                m_teacherIepMap.put(staffOid, tempIepList);
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
     * Fills date map, and count Map.
     */
    private void buildStudentMap() {
        for (String staffOid : m_teacherIepMap.keySet()) {
            ArrayList<String> iepOids = m_teacherIepMap.get(staffOid);
            for (String iepOid : iepOids) {
                if (!MESSAGE.equals(iepOid)) {
                    IepData iepData = (IepData) getBroker().getBeanByOid(IepData.class, iepOid);
                    Collection<IepAccessLog> accessLog = iepData.getIepAccessLogs();

                    for (IepAccessLog currentLog : accessLog) {
                        PlainDate date = currentLog.getDate();

                        if ((m_startDate == null || date.after(m_startDate)) &&
                                (m_endDate == null || date.before(m_endDate))) {
                            buildDateMap(staffOid, iepOid, currentLog, date);
                        }
                    }
                } else {
                    HashMap<String, KeyValuePair<PlainDate, PlainDate>> iepDateMap =
                            new HashMap<String, KeyValuePair<PlainDate, PlainDate>>();
                    KeyValuePair<PlainDate, PlainDate> datePair = new KeyValuePair<PlainDate, PlainDate>(null, null);

                    iepDateMap.put(iepOid, datePair);

                    m_dateMap.put(staffOid, iepDateMap);

                    HashMap<String, Integer> temp = new HashMap<String, Integer>();
                    temp.put(iepOid, Integer.valueOf(0));
                    m_countMap.put(staffOid, temp);
                }
            }
        }
    }

    /**
     * Sorts the grid.
     */
    private void createSort() {
        ArrayList<String> sortList = new ArrayList<String>();
        ArrayList<Boolean> sortOrders = new ArrayList<Boolean>();

        if (((Integer) getParameter(PARAM_SORT_BY)).intValue() == 1) {
            sortList.add(FIELD_STAFF + PATH_DELIMITER + SisStaff.COL_DEPARTMENT_CODE);
            sortOrders.add(Boolean.valueOf(true));
        }

        sortList.add(FIELD_STAFF + PATH_DELIMITER + SisStaff.COL_NAME_VIEW);
        sortList.add(FIELD_STUDENT + PATH_DELIMITER + SisStudent.COL_NAME_VIEW);

        sortOrders.add(Boolean.valueOf(true));
        sortOrders.add(Boolean.valueOf(true));

        m_grid.sort(sortList, sortOrders, false);
    }

    /**
     * Retrieves person id from the current user.
     *
     * @param currentUser SisUser
     * @return String
     */
    private String getPersonOid(SisUser currentUser) {
        String personOid = "";

        if (currentUser != null) {
            if (currentUser.getPersonOid() != null) {
                personOid = currentUser.getPersonOid();
            }
        }

        return personOid;
    }

    /**
     * Adds the student information to the grid.
     */
    private void populateGrid() {
        List<IepData.StatusCode> iepStatus = Arrays.asList(IepData.StatusCode.values());

        for (String staffOid : m_countMap.keySet()) {
            SisStaff staff = (SisStaff) getBroker().getBeanByOid(SisStaff.class, staffOid);

            for (String iepOid : m_countMap.get(staffOid).keySet()) {
                if (!MESSAGE.equals(iepOid)) {
                    IepData iepData = (IepData) getBroker().getBeanByOid(IepData.class, iepOid);

                    Integer count = m_countMap.get(staffOid).get(iepOid);

                    PlainDate firstDate;
                    PlainDate lastDate;

                    if (m_dateMap.containsKey(staffOid) && m_dateMap.get(staffOid).containsKey(iepOid)) {
                        firstDate = m_dateMap.get(staffOid).get(iepOid).getKey();
                        lastDate = m_dateMap.get(staffOid).get(iepOid).getValue();
                    } else {
                        firstDate = null;
                        lastDate = null;
                    }

                    String iepType = StringUtils.properCase(iepStatus.get(iepData.getStatusCode()).toString(), true)
                            .replace("_", " ");

                    m_grid.append();
                    m_grid.set(FIELD_IEP_DATA, iepData);
                    m_grid.set(FIELD_FIRST_DATE, firstDate);
                    m_grid.set(FIELD_LAST_DATE, lastDate);
                    m_grid.set(FIELD_IEP_TYPE, iepType);
                    m_grid.set(FIELD_COUNT, count);
                    m_grid.set(FIELD_STAFF, staff);
                    m_grid.set(FIELD_STUDENT, iepData.getStudent());
                } else {
                    m_grid.append();
                    m_grid.set(FIELD_STAFF, staff);
                }
            }
        }
    }
}
