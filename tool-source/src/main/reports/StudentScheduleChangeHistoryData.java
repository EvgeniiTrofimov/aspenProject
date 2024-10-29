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

import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.tools.reports.QueryIteratorDataSource;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.ApplicationContext;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.follett.fsc.core.k12.web.presentation.FieldFormatter;
import com.follett.fsc.core.k12.web.presentation.FieldFormatterFactory;
import com.follett.fsc.core.k12.web.presentation.SessionAwareFieldFormatter;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentScheduleChange;
import com.x2dev.sis.web.SisUserDataContainer;
import com.x2dev.utils.types.PlainDate;
import net.sf.jasperreports.engine.JRDataSource;
import net.sf.jasperreports.engine.JREmptyDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares the data for the "Student Schedule Change History" report. This report lists drops and
 * adds to a student's schedule for a specified date range. It is sorted by teacher and sections
 * with the "adds" together then the "drops".
 *
 * @author X2 Development Corporation
 */
public class StudentScheduleChangeHistoryData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Report parameter name for the end of the date range. This value is a PlainDate object.
     */
    public static final String END_DATE_PARAM = "endDate";

    /**
     * Report parameter name for the start of the date range. This value is a PlainDate object.
     */
    public static final String START_DATE_PARAM = "startDate";

    /**
     * Report parameter name for the timestamp formatter. This value is a
     * com.follett.fsc.core.k12.web.presentation.FieldFormatter object.
     */
    public static final String TIMESTAMP_FORMATTER_PARAM = "timestampFormatter";

    private String m_scheduleOid;
    private UserDataContainer m_userData;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        JRDataSource dropAdds = null;

        PlainDate startDate = (PlainDate) getParameter(START_DATE_PARAM);
        PlainDate endDate = (PlainDate) getParameter(END_DATE_PARAM);
        if (startDate != null && endDate != null) {
            Criteria criteria = new Criteria();
            if (isSchoolContext()) {
                criteria.addEqualTo(StudentScheduleChange.COL_SCHEDULE_OID, m_scheduleOid);
                /*
                 * If ever you want to allow the date range to cover more than the
                 * currentActiveSchedule, we must
                 * select all scheduleOids that belong to this school.
                 * This might never be needed, that's why it is commented.
                 * criteria.addEqualTo(StudentScheduleChange.REL_SCHEDULE+"."+Schedule.
                 * COL_SCHOOL_OID,getSchool().getOid());
                 */
            } else {
                criteria.addAndCriteria(getOrganizationCriteria(StudentScheduleChange.class));
            }

            criteria.addGreaterOrEqualThan(StudentScheduleChange.COL_EFFECTIVE_DATE, startDate);
            criteria.addLessOrEqualThan(StudentScheduleChange.COL_EFFECTIVE_DATE, endDate);
            criteria.addNotNull(StudentScheduleChange.COL_ACTION_DATE);

            QueryByCriteria query = new QueryByCriteria(StudentScheduleChange.class, criteria);
            query.addOrderByAscending(StudentScheduleChange.REL_MASTER_SCHEDULE + "." + MasterSchedule.COL_STAFF_VIEW);
            query.addOrderByAscending(
                    StudentScheduleChange.REL_MASTER_SCHEDULE + "." + MasterSchedule.COL_PRIMARY_STAFF_OID);
            query.addOrderByAscending(StudentScheduleChange.REL_MASTER_SCHEDULE + "." + MasterSchedule.COL_COURSE_VIEW);
            query.addOrderByAscending(StudentScheduleChange.REL_STUDENT + "." + SisStudent.COL_NAME_VIEW);
            query.addOrderByAscending(StudentScheduleChange.COL_TIMESTAMP);

            dropAdds = new QueryIteratorDataSource(getBroker().getIteratorByQuery(query));
        } else {
            dropAdds = new JREmptyDataSource();
        }

        /*
         * Add a parameter for a TimestampFormatter so the report format can display the timestamps
         * properly.
         */
        ModelProperty property = new ModelProperty(StudentScheduleChange.class.getName(),
                StudentScheduleChange.COL_TIMESTAMP,
                getBroker().getPersistenceKey());

        FieldFormatter formatter = FieldFormatterFactory.createFieldFormatter(property, getPrivilegeSet());
        if (formatter instanceof SessionAwareFieldFormatter) {
            ((SessionAwareFieldFormatter) formatter).setUserData(m_userData);
        }

        addParameter(TIMESTAMP_FORMATTER_PARAM, formatter);

        return dropAdds;
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
        m_userData = userData;

        if (m_userData.getSessionNavConfig().getApplicationContext() == ApplicationContext.BUILD) {
            m_scheduleOid = ((SisUserDataContainer) m_userData).getBuildScheduleOid();
        } else {
            m_scheduleOid = ((SisSchool) getSchool()).getActiveScheduleOid();
        }
    }
}
