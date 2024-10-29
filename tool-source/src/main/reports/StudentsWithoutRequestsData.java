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

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
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
import com.follett.fsc.core.k12.web.ApplicationContext;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.CourseRequest;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentScheduleAttributes;
import com.x2dev.sis.web.SisUserDataContainer;
import com.x2dev.utils.StringUtils;
import java.util.Collection;
import java.util.Iterator;
import net.sf.jasperreports.engine.JRDataSource;
import net.sf.jasperreports.engine.data.JRBeanCollectionDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Displays a list of students without requests for the current school year. Optionally, a snapshot
 * can be created of the students appearing on the report.
 * <p>
 * An option to group the students by counselor is provided. Since counselor is not a built-in field
 * on the student table, the alias "counselor" is used to retrieve the value. Therefore a field
 * with the alias "counselor" must exist.
 *
 * @author X2 Development Corporation
 */
public class StudentsWithoutRequestsData extends ReportJavaSourceNet {
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

    private Schedule m_schedule;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        // Build a subquery on requests
        Criteria reqCriteria = new Criteria();
        reqCriteria.addEqualTo(CourseRequest.COL_DISTRICT_CONTEXT_OID, m_schedule.getDistrictContextOid());
        reqCriteria.addEqualTo(CourseRequest.COL_SCHOOL_OID, m_schedule.getSchoolOid());

        SubQuery reqQuery = new SubQuery(CourseRequest.class, CourseRequest.COL_STUDENT_OID, reqCriteria);

        /*
         * Build a criteria for StudentScheduleAttributes beans, which includes a NOT IN on the
         * requests subquery
         */
        Criteria ssaCriteria = new Criteria();
        ssaCriteria.addEqualTo(StudentScheduleAttributes.COL_SCHEDULE_OID, m_schedule.getStudentScheduleOid());
        ssaCriteria.addNotIn(StudentScheduleAttributes.COL_STUDENT_OID, reqQuery);

        Boolean scheduledOnly = (Boolean) getParameter(SCHEDULED_ONLY_PARAM);
        if (scheduledOnly.booleanValue()) {
            ssaCriteria.addEqualTo(StudentScheduleAttributes.COL_SCHEDULE_INCLUDE_INDICATOR, Boolean.TRUE);
        }

        Boolean primaryOnly = (Boolean) getParameter(PRIMARY_ONLY_PARAM);
        if (primaryOnly.booleanValue()) {
            ssaCriteria.addEqualTo(StudentScheduleAttributes.REL_STUDENT + PATH_DELIMITER +
                    SisStudent.COL_NEXT_SCHOOL_OID, m_schedule.getSchoolOid());
        }

        Boolean activeOnly = (Boolean) getParameter(ACTIVE_ONLY_PARAM);
        if (activeOnly.booleanValue()) {
            ssaCriteria.addAndCriteria(StudentManager.getActiveStudentStatusCriteria(getOrganization(),
                    StudentScheduleAttributes.REL_STUDENT + PATH_DELIMITER +
                            SisStudent.COL_ENROLLMENT_STATUS));

        }

        // Build a query on StudentScheduleAttributes; order by counselor if grouping by counselor
        QueryByCriteria ssaQuery = new QueryByCriteria(StudentScheduleAttributes.class, ssaCriteria);

        Boolean groupByCounselor = (Boolean) getParameter(COUNSELOR_PARAM);
        if (groupByCounselor.booleanValue()) {
            DataDictionaryField counselorField = DataDictionary.getDistrictDictionary(getUser().getPersistenceKey())
                    .findDataDictionaryFieldByAlias(Student.ALIAS_COUNSELOR);
            if (counselorField != null) {
                ssaQuery.addOrderByAscending(StudentScheduleAttributes.REL_STUDENT + PATH_DELIMITER +
                        counselorField.getSystemDataField().getJavaName());
            }
        }

        ssaQuery.addOrderByAscending(StudentScheduleAttributes.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_NAME_VIEW);

        Collection attributes = getBroker().getCollectionByQuery(ssaQuery);
        createSnapshot(attributes);

        return new JRBeanCollectionDataSource(attributes);
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize(com.follett.fsc.core.k12.web.
     *      UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) {
        if (userData.getSessionNavConfig().getApplicationContext() == ApplicationContext.BUILD) {
            m_schedule = ((SisUserDataContainer) userData).getBuildSchedule();
        } else {
            m_schedule = ((SisSchool) userData.getSchool()).getActiveSchedule();
        }
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
