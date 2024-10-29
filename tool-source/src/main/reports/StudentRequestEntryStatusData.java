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
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryTable;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.x2dev.sis.model.beans.CourseRequest;
import com.x2dev.sis.model.beans.CourseRequestTransaction;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.StringUtils;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares the data for the "Student Request Entry Status" report.
 *
 * @author X2 Development Corporation
 */
public class StudentRequestEntryStatusData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Name for the "active only" report parameter. The value is an Boolean.
     */
    public static final String ACTIVE_ONLY_PARAM = "activeOnly";

    /**
     * Name for the enumerated "filter" report parameter. The value is an Integer.
     */
    public static final String FILTER_BY_PARAM = "filterBy";

    /**
     * Name for the "selection" report parameter. The value is a String.
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
     * Name for the "sort" report parameter. The value is a String.
     */
    public static final String SORT_PARAM = "sort";

    // Grid fields
    private static final String FIELD_APPROVED = "approved";
    private static final String FIELD_POSTED = "posted";
    private static final String FIELD_STUDENT = "student";
    private static final String FIELD_TOTAL_CREDIT_COUNT = "totalCredits";
    private static final String FIELD_TOTAL_REQUEST_COUNT = "total";

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        X2Criteria criteria = new X2Criteria();
        criteria.addNotEmpty(SisStudent.COL_NEXT_SCHOOL_OID, getBroker().getPersistenceKey());
        criteria.addEqualTo(SisStudent.COL_NEXT_SCHOOL_OID, getSchool().getOid());

        String queryBy = (String) getParameter(QUERY_BY_PARAM);
        String queryString = (String) getParameter(QUERY_STRING_PARAM);

        addUserCriteria(criteria, queryBy, queryString, null, null);

        boolean activeOnly = ((Boolean) getParameter(ACTIVE_ONLY_PARAM)).booleanValue();
        if (activeOnly) {
            criteria.addAndCriteria(
                    StudentManager.getActiveStudentStatusCriteria(getOrganization(), SisStudent.COL_ENROLLMENT_STATUS));

        }

        QueryByCriteria studentQuery = new QueryByCriteria(SisStudent.class, criteria);

        String sort = (String) getParameter(SORT_PARAM);
        applyUserSort(studentQuery, sort);

        SubQuery studentSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, criteria);
        Map<String, Collection<CourseRequest>> requestsByStudent = getRequestsCounts(studentSubQuery);
        Collection<String> studentsPosted = getRequestsPosted(studentSubQuery);
        Collection<String> studentsApproved = getRequestsApproved(studentSubQuery);

        Collection<SisStudent> studentsIncluded = new HashSet<SisStudent>();
        ReportDataGrid grid = new ReportDataGrid(10);
        QueryIterator iterator = getBroker().getIteratorByQuery(studentQuery);
        try {
            while (iterator.hasNext()) {
                SisStudent student = (SisStudent) iterator.next();
                int requestCount = requestsByStudent.get(student.getOid()) == null ? 0
                        : requestsByStudent.get(student.getOid()).size();

                grid.append();
                grid.set(FIELD_STUDENT, student);
                grid.set(FIELD_TOTAL_REQUEST_COUNT, Integer.valueOf(requestCount));

                saveGridRow(grid, student, requestsByStudent.get(student.getOid()), studentsPosted, studentsApproved,
                        studentsIncluded);
            }
        } finally {
            iterator.close();
        }

        createSnapshot(studentsIncluded);

        grid.beforeTop();
        return grid;
    }

    /**
     * Creates a snapshot of the students represented by the collection of beans if a name was
     * specified.
     *
     * @param beans a Collection of Student beans
     */
    private void createSnapshot(Collection<SisStudent> beans) {
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
                SisStudent bean = (SisStudent) iterator.next();

                RecordSetKey key = X2BaseBean.newInstance(RecordSetKey.class, getBroker().getPersistenceKey());
                key.setObjectOid(bean.getOid());

                snapshot.addToRecordSetKeys(key);
            }

            getBroker().saveBeanForced(snapshot);
        }
    }

    /**
     * Returns the list students oids whose requests have been approved.
     *
     * @param studentSubQuery SubQuery
     * @return Collection<String>
     */
    private Collection<String> getRequestsApproved(SubQuery studentSubQuery) {
        X2Criteria requestCriteria = getOrganizationCriteria(CourseRequestTransaction.class);
        requestCriteria.addIn(CourseRequestTransaction.COL_STUDENT_OID, studentSubQuery);
        requestCriteria.addEqualTo(CourseRequestTransaction.COL_DISTRICT_CONTEXT_OID, getSchool().getBuildContextOid());
        requestCriteria.addGreaterThan(CourseRequestTransaction.COL_APPROVED_TIME, Integer.valueOf(0));

        SubQuery subQuey =
                new SubQuery(CourseRequestTransaction.class, CourseRequestTransaction.COL_STUDENT_OID, requestCriteria);

        return getBroker().getSubQueryCollectionByQuery(subQuey);
    }

    /**
     * Returns the list courses requested by student.
     *
     * @param studentSubQuery SubQuery
     * @return Map<String, Collection<String>>
     */
    private Map<String, Collection<CourseRequest>> getRequestsCounts(SubQuery studentSubQuery) {
        X2Criteria requestCriteria = new X2Criteria();
        requestCriteria.addIn(CourseRequest.COL_STUDENT_OID, studentSubQuery);
        requestCriteria.addEqualTo(CourseRequest.COL_DISTRICT_CONTEXT_OID, getSchool().getBuildContextOid());
        requestCriteria.addEqualTo(CourseRequest.COL_SCHOOL_OID, getSchool().getOid());

        QueryByCriteria requestQuery = new QueryByCriteria(CourseRequest.class, requestCriteria);

        return getBroker().getGroupedCollectionByQuery(requestQuery, CourseRequest.COL_STUDENT_OID, 10000);
    }

    /**
     * Returns the list students oids who have posted their requests.
     *
     * @param studentSubQuery SubQuery
     * @return Collection<String>
     */
    private Collection<String> getRequestsPosted(SubQuery studentSubQuery) {
        X2Criteria requestCriteria = getOrganizationCriteria(CourseRequestTransaction.class);
        requestCriteria.addIn(CourseRequestTransaction.COL_STUDENT_OID, studentSubQuery);
        requestCriteria.addEqualTo(CourseRequestTransaction.COL_DISTRICT_CONTEXT_OID, getSchool().getBuildContextOid());
        requestCriteria.addGreaterThan(CourseRequestTransaction.COL_LAST_POSTED_TIME, Integer.valueOf(0));

        SubQuery subQuey =
                new SubQuery(CourseRequestTransaction.class, CourseRequestTransaction.COL_STUDENT_OID, requestCriteria);

        return getBroker().getSubQueryCollectionByQuery(subQuey);
    }

    /**
     * Save the grid row if the student meets the criteria. Otherwise drop the student from the
     * grid.
     *
     * @param grid ReportDataGrid
     * @param student SisStudent
     * @param requestedCourses Collection<CourseRequest>
     * @param studentsPosted Collection<String>
     * @param studentsApproved Collection<String>
     * @param studentsIncluded Collection<SisStudent>
     */
    private void saveGridRow(ReportDataGrid grid,
                             SisStudent student,
                             Collection<CourseRequest> requestedCourses,
                             Collection<String> studentsPosted,
                             Collection<String> studentsApproved,
                             Collection<SisStudent> studentsIncluded) {
        int filterBy = ((Integer) getParameter(FILTER_BY_PARAM)).intValue();
        boolean isDropped = false;

        switch (filterBy) {
            case 1: // Entered requests
                isDropped = requestedCourses == null || requestedCourses.size() == 0;
                break;

            case 2: // Posted
                isDropped = !studentsPosted.contains(student.getOid());
                break;

            case 3: // Approved
                isDropped = !studentsApproved.contains(student.getOid());
                break;

            default: // All
                break;
        }

        if (isDropped) {
            grid.deleteRow();
        } else {
            double totalCredits = 0;
            if (requestedCourses != null) {
                for (CourseRequest request : requestedCourses) {
                    totalCredits += request.getSchoolCourse().getCredit().doubleValue();
                }
            }

            grid.set(FIELD_APPROVED, studentsApproved.contains(student.getOid()) ? "Y" : "N");
            grid.set(FIELD_POSTED, studentsPosted.contains(student.getOid()) ? "Y" : "N");
            grid.set(FIELD_TOTAL_CREDIT_COUNT, Double.valueOf(totalCredits));

            studentsIncluded.add(student);
        }
    }
}
