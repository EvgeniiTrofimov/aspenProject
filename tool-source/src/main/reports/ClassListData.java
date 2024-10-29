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
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.reports.QueryIteratorDataSource;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.ApplicationContext;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.business.schedule.ScheduleManager;
import com.x2dev.sis.tools.reports.ScheduleReportHelper;
import com.x2dev.utils.StringUtils;
import java.util.Collection;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares the data for the Class List report. This report lists the students enrolled in each
 * section in the master schedule. The exact sections included in the report can be specified with
 * input parameters.
 *
 * @author X2 Development Corporation
 */
public class ClassListData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Name for the "includeStudyClass" report parameter. The value is an Boolean.
     */
    public static final String INCLUDE_STUDY_CLASS = "includeStudyClass";

    /**
     * Name for the enumerated "selection" report parameter. The value is an Integer.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the "selection value" report parameter. The value is a String.
     */
    public static final String QUERY_STRING_PARAM = "queryString";

    /**
     * Name for the enumerated "sort" report parameter. The value is an Integer.
     */
    public static final String SORT_PARAM = "sort";

    private ScheduleReportHelper m_reportHelper;
    private String m_teacherOid;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        X2Criteria criteria = new X2Criteria();
        /*
         * Build the schedule criteria based on the school's current schedule and the optional
         * section filters from the user input.
         */
        criteria.addEqualTo(StudentSection.COL_SCHEDULE_OID, m_reportHelper.getScheduleOid());

        /*
         * If we are running in the staff view, limit the results to the sections of the current
         * teacher.
         */
        if (!StringUtils.isEmpty(m_teacherOid)) {
            criteria.addEqualTo(StudentSection.REL_SECTION + PATH_DELIMITER +
                    Section.COL_PRIMARY_STAFF_OID, m_teacherOid);
        }

        /*
         * Exclude study class when required for.
         */
        boolean includeStudyClass = ((Boolean) getParameter(INCLUDE_STUDY_CLASS)).booleanValue();
        if (!includeStudyClass) {
            criteria.addNotEqualTo(StudentSection.REL_SECTION + PATH_DELIMITER + Section.REL_SCHOOL_COURSE +
                    PATH_DELIMITER + SchoolCourse.COL_MASTER_TYPE, SchoolCourse.MASTER_TYPE_STUDY);

        }

        int queryBy = ((Integer) getParameter(QUERY_BY_PARAM)).intValue();
        switch (queryBy) {
            case 5: // Current Selection
                Criteria masterCriteria = getCurrentCriteria();
                SubQuery subQuery = new SubQuery(m_reportHelper.getSectionClass(), X2BaseBean.COL_OID, masterCriteria);
                criteria.addIn(StudentSection.COL_SECTION_OID, subQuery);
                break;

            case 1: // Term code
                Criteria termCriteria = new Criteria();
                termCriteria.addEqualTo(ScheduleTerm.COL_SCHEDULE_OID, m_reportHelper.getTimeScheduleOid());
                termCriteria.addEqualTo(ScheduleTerm.COL_CODE, getParameter(QUERY_STRING_PARAM));

                QueryByCriteria termQuery = new QueryByCriteria(ScheduleTerm.class, termCriteria);
                ScheduleTerm term = (ScheduleTerm) getBroker().getBeanByQuery(termQuery);

                ScheduleManager scheduleManager = new ScheduleManager(getBroker());

                if (m_reportHelper.getSectionClass().equals(MasterSchedule.class)) {
                    Collection coveredTerms = scheduleManager.getCoveredTermOids(term);
                    if (coveredTerms != null && !coveredTerms.isEmpty()) {
                        criteria.addIn(StudentSchedule.REL_SECTION + PATH_DELIMITER +
                                MasterSchedule.REL_MASTER_TERMS + PATH_DELIMITER + MasterTerm.COL_SCHEDULE_TERM_OID,
                                coveredTerms);
                    }
                } else if (m_reportHelper.getSectionClass().equals(BuildMasterSchedule.class)) {
                    Collection coveredTerms = scheduleManager.getCoveredTermMaps(term);
                    if (coveredTerms != null && !coveredTerms.isEmpty()) {
                        criteria.addIn(BuildStudentSchedule.REL_SECTION + PATH_DELIMITER +
                                BuildMasterSchedule.COL_TERM_MAP, coveredTerms);
                    }
                }

                break;

            case 2: // Course number
                criteria.addEqualTo(StudentSection.REL_SECTION + PATH_DELIMITER + Section.REL_SCHOOL_COURSE +
                        PATH_DELIMITER + SchoolCourse.COL_NUMBER, getParameter(QUERY_STRING_PARAM));
                break;

            case 3: // Section
                criteria.addEqualTo(StudentSection.REL_SECTION + PATH_DELIMITER +
                        Section.COL_COURSE_VIEW, getParameter(QUERY_STRING_PARAM));
                break;

            case 4: // Schedule
                criteria.addEqualTo(StudentSection.REL_SECTION + PATH_DELIMITER +
                        Section.COL_SCHEDULE_DISPLAY, getParameter(QUERY_STRING_PARAM));
                break;

            case 6: // Teacher
                criteria.addBeginsWithIgnoreCase(StudentSection.REL_SECTION + PATH_DELIMITER +
                        Section.REL_PRIMARY_STAFF + PATH_DELIMITER + SisStaff.COL_NAME_VIEW,
                        getParameter(QUERY_STRING_PARAM));
                break;

            default:
                // No additional criteria (this is the case for "All")
                break;
        }

        QueryByCriteria query = new QueryByCriteria(m_reportHelper.getStudentSectionClass(), criteria);

        /*
         * Build the sort based on user input.
         */
        applyUserSort(query, ((String) getParameter(SORT_PARAM)));


        /*
         * Execute the query
         */
        return new QueryIteratorDataSource(getBroker().getIteratorByQuery(query));
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @see
     *      com.follett.fsc.core.k12.tools.reports.ReportJavaDataSource#saveState(com.follett.fsc.core.
     *      k12.web.UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) {
        if (userData.getSessionNavConfig().getApplicationContext().equals(ApplicationContext.STAFF)) {
            m_teacherOid = userData.getStaffOid();
        }

        m_reportHelper = new ScheduleReportHelper(userData);
    }
}
