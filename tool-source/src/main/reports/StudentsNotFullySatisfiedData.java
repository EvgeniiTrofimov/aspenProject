/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2006 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.RecordSet;
import com.follett.fsc.core.k12.beans.RecordSetKey;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.Ownable;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryTable;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.BuildStudentSchedule;
import com.x2dev.sis.model.beans.CourseRequest;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.Section;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentScheduleAttributes;
import com.x2dev.sis.model.beans.StudentSection;
import com.x2dev.sis.tools.reports.ScheduleReportHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import net.sf.jasperreports.engine.data.JRBeanCollectionDataSource;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Displays a list of students with unscheduled requests for the current schedule. Optionally, a
 * snapshot can be created of the students appearing on the report.
 * <p>
 * An option to group the students by counselor is provided. Since counselor is not a built-in field
 * on the student table, the alias "counselor" is used to retrieve the value. Therefore a field
 * with the alias "counselor" must exist.
 *
 * @author X2 Development Corporation
 */
public class StudentsNotFullySatisfiedData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Name for the "active students only" parameter. The value is a Boolean.
     */
    public static final String ACTIVE_ONLY_PARAM = "activeOnly";

    /**
     * Name for the "group by counselor" parameter. The value is a Boolean.
     */
    public static final String COUNSELOR_PARAM = "groupByCounselor";

    /**
     * Name for the "ignore studies" parameter. The value is a Boolean.
     */
    public static final String IGNORE_STUDIES_PARAM = "ignoreStudies";

    /**
     * Name for the "scheduled students only" parameter. The value is a Boolean.
     */
    public static final String SCHEDULED_ONLY_PARAM = "scheduledOnly";

    /**
     * Name for the "primary students only" parameter. The value is a Boolean.
     */
    public static final String PRIMARY_ONLY_PARAM = "primaryOnly";

    /**
     * Name for the "snapshot name" parameter. The value is a String.
     */
    public static final String SNAPSHOT_NAME_PARAM = "snapshotName";

    /**
     * Name for the "snapshot owner" parameter. The value is an Integer and corresponds to the
     * Ownable types.
     */
    public static final String SNAPSHOT_OWNER_PARAM = "snapshotOwner";

    private ScheduleReportHelper m_reportHelper;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        /*
         * Load requests map
         */
        X2Criteria requestCriteria = new X2Criteria();
        requestCriteria.addEqualTo(CourseRequest.COL_DISTRICT_CONTEXT_OID,
                m_reportHelper.getSchedule().getDistrictContextOid());
        requestCriteria.addEqualTo(CourseRequest.COL_SCHOOL_OID, m_reportHelper.getSchedule().getSchoolOid());

        QueryByCriteria requestQuery = new QueryByCriteria(CourseRequest.class, requestCriteria);
        Map studentRequestMap =
                getBroker().getGroupedCollectionByQuery(requestQuery, CourseRequest.COL_STUDENT_OID, 5000);

        Boolean ignoreStudies = (Boolean) getParameter(IGNORE_STUDIES_PARAM);

        /*
         * Load student schedule map
         */
        X2Criteria scheduleCriteria = new X2Criteria();
        scheduleCriteria.addEqualTo(StudentSection.COL_SCHEDULE_OID, m_reportHelper.getScheduleOid());
        if (ignoreStudies.booleanValue()) {
            scheduleCriteria.addNotEqualTo(StudentSection.REL_SECTION + "." + Section.REL_SCHOOL_COURSE +
                    "." + SchoolCourse.REL_COURSE + "." + SchoolCourse.COL_MASTER_TYPE, SchoolCourse.MASTER_TYPE_STUDY);
        }

        QueryByCriteria scheduleQuery = new QueryByCriteria(m_reportHelper.getStudentSectionClass(), scheduleCriteria);
        Map studentScheduleMap =
                getBroker().getGroupedCollectionByQuery(scheduleQuery, BuildStudentSchedule.COL_STUDENT_OID, 5000);

        /*
         * Load student attribute map
         */
        X2Criteria studentCriteria = new X2Criteria();
        studentCriteria.addEqualTo(StudentScheduleAttributes.COL_SCHEDULE_OID, m_reportHelper.getStudentScheduleOid());

        Boolean scheduledOnly = (Boolean) getParameter(SCHEDULED_ONLY_PARAM);
        if (scheduledOnly.booleanValue()) {
            studentCriteria.addEqualTo(StudentScheduleAttributes.COL_SCHEDULE_INCLUDE_INDICATOR, Boolean.TRUE);
        }

        Boolean primaryOnly = (Boolean) getParameter(PRIMARY_ONLY_PARAM);
        if (primaryOnly.booleanValue()) {
            studentCriteria.addEqualTo(StudentScheduleAttributes.REL_STUDENT + "." + SisStudent.COL_SCHOOL_OID,
                    m_reportHelper.getSchedule().getSchoolOid());
        }

        Boolean activeOnly = (Boolean) getParameter(ACTIVE_ONLY_PARAM);
        if (activeOnly.booleanValue()) {
            studentCriteria.addAndCriteria(StudentManager.getActiveStudentStatusCriteria(getOrganization(),
                    StudentScheduleAttributes.REL_STUDENT + "." + SisStudent.COL_ENROLLMENT_STATUS));

        }

        Collection beans = new ArrayList();

        QueryByCriteria studentQuery = new QueryByCriteria(StudentScheduleAttributes.class, studentCriteria);

        /*
         * Sort by counselor if grouping by counselor
         */
        Boolean groupByCounselor = (Boolean) getParameter(COUNSELOR_PARAM);
        if (groupByCounselor.booleanValue()) {
            DataDictionaryField counselorField = DataDictionary.getDistrictDictionary(getUser().getPersistenceKey())
                    .findDataDictionaryFieldByAlias(Student.ALIAS_COUNSELOR);
            if (counselorField != null) {
                studentQuery.addOrderByAscending(StudentScheduleAttributes.REL_STUDENT + "."
                        + counselorField.getSystemDataField().getJavaName());
            }
        }

        studentQuery.addOrderByAscending(StudentScheduleAttributes.REL_STUDENT + "." + SisStudent.COL_NAME_VIEW);

        Collection students = getBroker().getCollectionByQuery(studentQuery);

        Iterator studentIterator = students.iterator();
        while (studentIterator.hasNext()) {
            StudentScheduleAttributes studentAttribute = (StudentScheduleAttributes) studentIterator.next();
            Collection requests = (Collection) studentRequestMap.get(studentAttribute.getStudentOid());
            Collection schedules = (Collection) studentScheduleMap.get(studentAttribute.getStudentOid());

            int requestsCount = requests == null ? 0 : requests.size();
            int scheduleCount = schedules == null ? 0 : schedules.size();

            if (scheduleCount < requestsCount) {
                beans.add(studentAttribute);
            }
        }

        createSnapshot(beans);

        return new JRBeanCollectionDataSource(beans);
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
     * Creates a snapshot of the students represented by the collection of beans if a name was
     * specified.
     *
     * @param beans a Collection of StudentScheduleAttributes beans
     */
    private void createSnapshot(Collection beans) {
        String name = (String) getParameter(SNAPSHOT_NAME_PARAM);
        if (!StringUtils.isEmpty(name)) {
            DataDictionary dictionary = DataDictionary.getDistrictDictionary(getUser().getPersistenceKey());
            DataDictionaryTable table =
                    dictionary.findDataDictionaryTableByClass(SisStudent.class.getName());

            RecordSet snapshot = X2BaseBean.newInstance(RecordSet.class, getBroker().getPersistenceKey());
            snapshot.setDataTableOid(table.getSystemOid());
            snapshot.setName(name);

            switch (((Integer) getParameter(SNAPSHOT_OWNER_PARAM)).intValue()) {
                case Ownable.OWNER_TYPE_ORG1:
                    snapshot.setOwnerOid(getOrganization().getOid());
                    snapshot.setOwnerType(Ownable.OWNER_TYPE_ORG1);
                    break;

                case Ownable.OWNER_TYPE_SCHOOL:
                    snapshot.setOwnerOid(getSchool().getOid());
                    snapshot.setOwnerType(Ownable.OWNER_TYPE_SCHOOL);
                    break;

                case Ownable.OWNER_TYPE_USER:
                default:
                    snapshot.setOwnerOid(getUser().getOid());
                    snapshot.setOwnerType(Ownable.OWNER_TYPE_USER);
                    break;
            }

            Iterator iterator = beans.iterator();
            while (iterator.hasNext()) {
                StudentScheduleAttributes bean = (StudentScheduleAttributes) iterator.next();

                RecordSetKey key = X2BaseBean.newInstance(RecordSetKey.class, getBroker().getPersistenceKey());
                key.setObjectOid(bean.getStudentOid());

                snapshot.addToRecordSetKeys(key);
            }

            getBroker().saveBeanForced(snapshot);
        }
    }
}
