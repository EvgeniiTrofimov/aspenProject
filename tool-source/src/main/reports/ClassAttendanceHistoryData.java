/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2003 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.k12.tools.reports.QueryIteratorDataSource;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.ScheduleTeacher;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.beans.StudentPeriodAttendance;
import com.x2dev.utils.types.PlainDate;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Data source for the class attendance history report.
 *
 * @author X2 Development Corporation
 */
public class ClassAttendanceHistoryData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;
    private static final String INPUT_PARAM_END_DATE = "endDate";
    private static final String INPUT_PARAM_START_DATE = "startDate";
    private static final String INPUT_PARAM_QUERY_BY = "queryBy"; // 0 = current section, 1 = all
    private static final String INPUT_PARAM_SORT = "sort"; // 1 = date, 0 = student

    private Schedule m_currentSchedule = null;
    private MasterSchedule m_currentSection = null;
    private PlainDate m_endDate = null;
    private SisStaff m_staff = null;
    private PlainDate m_startDate = null;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        Criteria criteria = getBaseAttendanceCriteria();
        criteria.addGreaterOrEqualThan(StudentPeriodAttendance.COL_DATE, m_startDate);
        criteria.addLessOrEqualThan(StudentPeriodAttendance.COL_DATE, m_endDate);

        QueryByCriteria query = new QueryByCriteria(StudentPeriodAttendance.class, criteria);

        applyUserSort(query, ((String) getParameter(INPUT_PARAM_SORT)));

        return new QueryIteratorDataSource(getBroker().getIteratorByQuery(query));
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
        m_startDate = (PlainDate) getParameter(INPUT_PARAM_START_DATE);
        m_endDate = (PlainDate) getParameter(INPUT_PARAM_END_DATE);

        Integer printFor = (Integer) getParameter(INPUT_PARAM_QUERY_BY);
        if (printFor.intValue() == 0) {
            ScheduleTeacher parent = userData.getCurrentRecord(ScheduleTeacher.class);
            m_currentSection = parent.getSection();
        }

        m_currentSchedule = ((SisSchool) getSchool()).getActiveSchedule();
        m_staff = (SisStaff) userData.getStaff();
    }

    /**
     * Returns a Criteria object that finds class attendance records for the current section, or
     * all sections for the current teacher based on the "print for" input parameter.
     *
     * @return Criteria
     */
    private Criteria getBaseAttendanceCriteria() {
        Criteria criteria = new Criteria();
        if (m_currentSection != null) {
            criteria.addEqualTo(StudentPeriodAttendance.COL_MASTER_SCHEDULE_OID, m_currentSection.getOid());
        } else {
            Criteria subQueryCriteria = new Criteria();
            subQueryCriteria.addEqualTo(ScheduleTeacher.COL_STAFF_OID, m_staff.getOid());
            subQueryCriteria.addEqualTo(ScheduleTeacher.REL_SECTION + "." +
                    MasterSchedule.COL_SCHEDULE_OID, m_currentSchedule.getOid());
            subQueryCriteria.addGreaterThan(ScheduleTeacher.REL_SECTION + "." +
                    MasterSchedule.COL_ENROLLMENT_TOTAL, Integer.valueOf(0));

            SubQuery subQuery = new SubQuery(ScheduleTeacher.class, ScheduleTeacher.COL_SECTION_OID, subQueryCriteria);

            criteria.addIn(StudentPeriodAttendance.COL_MASTER_SCHEDULE_OID, subQuery);
        }

        return criteria;
    }
}
