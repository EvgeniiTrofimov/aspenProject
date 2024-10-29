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
import com.follett.fsc.core.k12.beans.Address;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.StudentContact;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.follett.fsc.core.k12.web.ApplicationContext;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.CourseRequest;
import com.x2dev.sis.model.beans.CourseRequestAdjustment;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentScheduleAttributes;
import com.x2dev.sis.web.SisUserDataContainer;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Prepares the data for the "Student Request" report.
 *
 * @author X2 Development Corporation
 */
public class StudentRequestData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Name for the "multiple mailings" report parameter. The value is a Boolean.
     */
    public static final String MULTIPLE_MAILINGS_PARAM = "multipleMailings";

    /**
     * Name for the "recommendation" report parameter. The value is an Map of school course oids
     * recommended
     * for each student.
     */
    public static final String RECOMMENDATION_MAP = "recommendationMap";

    /**
     * Name for querying based on recommendation.
     */
    public static final String QUERY_BASED_ON_RECOMMENDATION = "basedOnRecommendation";

    /**
     * Name for the enumerated "selection" report parameter. The value is an Integer.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the "selection value" report parameter. The value is a String.
     */
    public static final String QUERY_STRING_PARAM = "queryString";

    /**
     * Name for the enumerated "student sort" report parameter. The value is an Integer.
     */
    public static final String SORT_PARAM = "sort";

    /*
     * Grid fields
     */
    private static final String FIELD_ADDRESS = "address";
    private static final String FIELD_ALTERNATE_ONE = "alternateSchoolCourse01";
    private static final String FIELD_ALTERNATE_TWO = "alternateSchoolCourse02";
    private static final String FIELD_IS_ALTERNATE = "alternateIndicator";
    private static final String FIELD_SCHOOL = "school";
    private static final String FIELD_SCHOOL_COURSE = "schoolCourse";
    private static final String FIELD_STUDENT = "student";

    /*
     * Report parameters
     */
    private static final String PARAMETER_ATTRIBUTE_MAP = "scheduleAttributesMap";

    private String m_buildSchedule;
    private SisStudent m_currentStudent;
    private Map m_studentContacts;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        boolean multipleMailings = ((Boolean) getParameter(MULTIPLE_MAILINGS_PARAM)).booleanValue();
        boolean basedOnRecommendation = ((Boolean) getParameter(QUERY_BASED_ON_RECOMMENDATION)).booleanValue();

        String contextOidField;
        String studentOidField;
        String schoolOidField;
        String relatedStudentField;
        String relatedSchoolCourseField;
        if (basedOnRecommendation) {
            contextOidField = CourseRequestAdjustment.COL_DISTRICT_CONTEXT_OID;
            studentOidField = CourseRequestAdjustment.COL_STUDENT_OID;
            schoolOidField = CourseRequestAdjustment.COL_SCHOOL_OID;
            relatedStudentField = CourseRequestAdjustment.REL_STUDENT;
            relatedSchoolCourseField = CourseRequestAdjustment.REL_SCHOOL_COURSE;
        } else {
            contextOidField = CourseRequest.COL_DISTRICT_CONTEXT_OID;
            studentOidField = CourseRequest.COL_STUDENT_OID;
            schoolOidField = CourseRequest.COL_SCHOOL_OID;
            relatedStudentField = CourseRequest.REL_STUDENT;
            relatedSchoolCourseField = CourseRequest.REL_SCHOOL_COURSE;
        }

        /*
         * Standard report...
         */
        Criteria criteria = new Criteria();
        criteria.addEqualTo(contextOidField, getSchool().getBuildContextOid());

        if (m_currentStudent != null) {
            criteria.addEqualTo(studentOidField, m_currentStudent.getOid());
        } else {
            criteria.addEqualTo(schoolOidField, getSchool().getOid());

            int queryBy = ((Integer) getParameter(QUERY_BY_PARAM)).intValue();
            switch (queryBy) {
                case 0: // Current selection
                    Criteria studentCriteria = getCurrentCriteria();
                    SubQuery subQuery = new SubQuery(StudentScheduleAttributes.class,
                            StudentScheduleAttributes.COL_STUDENT_OID, studentCriteria);

                    criteria.addIn(studentOidField, subQuery);
                    break;

                case 1: // YOG
                    criteria.addEqualTo(relatedStudentField + PATH_DELIMITER + SisStudent.COL_YOG,
                            getParameter(QUERY_STRING_PARAM));
                    break;

                case 3: // Snapshot
                    criteria.addIn(studentOidField,
                            ReportUtils.getRecordSetSubQuery((String) getParameter(QUERY_STRING_PARAM),
                                    getUser(), getSchool()));
                    break;

                default:
                    // No additional criteria (this is the case for "All")
                    break;
            }
        }

        QueryByCriteria query = null;

        if (basedOnRecommendation) {
            criteria.addEqualTo(CourseRequestAdjustment.COL_TYPE,
                    Integer.valueOf(CourseRequestAdjustment.TypeCode.Recommended.ordinal()));
            query = new QueryByCriteria(CourseRequestAdjustment.class, criteria);
        } else {
            query = new QueryByCriteria(CourseRequest.class, criteria);
        }

        // Sort query.
        int sort = ((Integer) getParameter(SORT_PARAM)).intValue();
        switch (sort) {
            case 0: // Name
                query.addOrderByAscending(relatedStudentField + PATH_DELIMITER + SisStudent.COL_NAME_VIEW);
                break;

            case 1: // YOG

                query.addOrderByAscending(relatedStudentField + PATH_DELIMITER + SisStudent.COL_YOG);
                query.addOrderByAscending(relatedStudentField + PATH_DELIMITER + SisStudent.COL_NAME_VIEW);
                break;

            default: // No sort specified
                break;
        }
        query.addOrderByAscending(studentOidField);
        query.addOrderByAscending(relatedSchoolCourseField + PATH_DELIMITER + SchoolCourse.COL_NUMBER);

        if (multipleMailings) {
            loadMailingContacts();
        }

        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        ReportDataGrid grid = new ReportDataGrid(2000, 5);
        SisStudent lastStudent = null;

        int studentStart = 0;
        int studentEnd = 0;

        try {
            while (iterator.hasNext()) {
                X2BaseBean request = (X2BaseBean) iterator.next();
                SisStudent student = request instanceof CourseRequest ? ((CourseRequest) request).getStudent()
                        : ((CourseRequestAdjustment) request).getStudent();

                grid.append();
                grid.set(FIELD_STUDENT, student);
                grid.set(FIELD_ADDRESS, student.getPerson().getResolvedMailingAddress());
                grid.set(FIELD_ALTERNATE_ONE, request instanceof CourseRequest
                        ? ((CourseRequest) request).getAlternateSchoolCourse01() : null);
                grid.set(FIELD_ALTERNATE_TWO, request instanceof CourseRequest
                        ? ((CourseRequest) request).getAlternateSchoolCourse02() : null);
                grid.set(FIELD_IS_ALTERNATE, Boolean.valueOf(
                        request instanceof CourseRequest ? ((CourseRequest) request).getAlternateIndicator() : false));
                grid.set(FIELD_SCHOOL, request instanceof CourseRequest ? ((CourseRequest) request).getSchool()
                        : ((CourseRequestAdjustment) request).getSchool());
                grid.set(FIELD_SCHOOL_COURSE,
                        request instanceof CourseRequest ? ((CourseRequest) request).getSchoolCourse()
                                : ((CourseRequestAdjustment) request).getSchoolCourse());

                if ((lastStudent == null || !student.equals(lastStudent)) && multipleMailings) {

                    if (lastStudent != null) {
                        studentEnd = grid.currentRowNumber() - 1;
                        addStudentMailings(grid, lastStudent, studentStart, studentEnd);
                    }

                    studentStart = grid.currentRowNumber();
                }

                lastStudent = student;
            }
        } finally {
            iterator.close();
        }

        /*
         * Add multipleMailings for last student in grid (if exists)
         */
        if (lastStudent != null && multipleMailings) {
            studentEnd = grid.currentRowNumber();
            addStudentMailings(grid, lastStudent, studentStart, studentEnd);
        }

        /*
         * Add a map of student OIDs to StudentScheduleAttributes for the current build scenario.
         */
        Map attributesByStudentOid = null;
        if (m_buildSchedule != null) {
            Criteria attributeCriteria = new Criteria();
            attributeCriteria.addEqualTo(StudentScheduleAttributes.COL_SCHEDULE_OID, m_buildSchedule);

            QueryByCriteria attributeQuery = new QueryByCriteria(StudentScheduleAttributes.class,
                    attributeCriteria);
            attributesByStudentOid = getBroker().getMapByQuery(attributeQuery,
                    StudentScheduleAttributes.COL_STUDENT_OID, 1000);
        } else {
            attributesByStudentOid = new HashMap();
        }

        addParameter(PARAMETER_ATTRIBUTE_MAP, attributesByStudentOid);

        /*
         * Recommended course by student
         */
        if (!basedOnRecommendation) {
            X2Criteria recommendCriteria = new X2Criteria();
            recommendCriteria.addEqualTo(CourseRequestAdjustment.COL_DISTRICT_CONTEXT_OID,
                    getSchool().getBuildContextOid());
            recommendCriteria.addEqualTo(CourseRequestAdjustment.COL_SCHOOL_OID, getSchool().getOid());
            recommendCriteria.addEqualTo(CourseRequestAdjustment.COL_TYPE,
                    Integer.valueOf(CourseRequestAdjustment.TypeCode.Recommended.ordinal()));
            if (m_currentStudent != null) {
                recommendCriteria.addEqualTo(CourseRequestAdjustment.COL_STUDENT_OID, m_currentStudent.getOid());
            }

            String[] columns = new String[] {CourseRequestAdjustment.COL_STUDENT_OID,
                    CourseRequestAdjustment.COL_SCHOOL_COURSE_OID};
            ReportQueryByCriteria recommendQuery =
                    new ReportQueryByCriteria(CourseRequestAdjustment.class, columns, recommendCriteria);
            Map<String, Collection<String>> recommendationByStudentMap =
                    getBroker().getGroupedColumnCollectionByQuery(recommendQuery, 10000);

            addParameter(RECOMMENDATION_MAP, recommendationByStudentMap);
        }

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
        if (userData.getSessionNavConfig().getApplicationContext() == ApplicationContext.BUILD) {
            StudentScheduleAttributes currentAttributes;
            currentAttributes = userData.getCurrentRecord(StudentScheduleAttributes.class);
            if (currentAttributes != null) {
                m_currentStudent = currentAttributes.getStudent();
            }

            m_buildSchedule = ((SisUserDataContainer) userData).getBuildSchedule().getStudentScheduleOid();
        } else {
            m_currentStudent = userData.getCurrentRecord(SisStudent.class);
        }
    }

    /**
     * Adds copies of student's transcript information for mailings to multiple contacts.
     *
     * @param grid ReportDataGrid
     * @param student SisStudent
     * @param startRow int
     * @param endRow int
     */
    private void addStudentMailings(ReportDataGrid grid,
                                    SisStudent student,
                                    int startRow,
                                    int endRow) {
        Collection mailingContacts = (Collection) m_studentContacts.get(student.getOid());
        if (mailingContacts != null) {
            /*
             * Group the rows to copy together.
             */
            Collection studentRows = new ArrayList(20);
            grid.gotoRow(startRow);

            /*
             * hasNext variable is used because with the grid we cannot use gotoRow to go back to -1
             * (student being duplicated starts at index = 0).
             */
            boolean hasNext = true;
            while (grid.currentRowNumber() <= endRow && hasNext) {
                Map gridRow = new HashMap(grid.getCurrentRow());

                studentRows.add(gridRow);
                hasNext = grid.next();
            }

            /*
             * Re-add student's information for each contact.
             */
            Iterator mailings = mailingContacts.iterator();
            grid.gotoRow(endRow);

            while (mailings.hasNext()) {
                StudentContact contact = (StudentContact) mailings.next();
                Address address = contact.getContact().getPerson().getResolvedMailingAddress();

                Iterator newRows = studentRows.iterator();
                int currentPosition = grid.currentRowNumber() + 1;

                while (newRows.hasNext()) {
                    Map entryRow = new HashMap((Map) newRows.next());

                    grid.insertRow(currentPosition, entryRow);
                    grid.set(FIELD_ADDRESS, address);

                    currentPosition = grid.currentRowNumber() + 1;
                }
            }

            grid.next();
        }
    }

    /**
     * Loads the mailing contacts for students into a Map of StudentContacts keyed to student OID.
     */
    private void loadMailingContacts() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(StudentContact.COL_OTHER_MAILING_INDICATOR, Boolean.TRUE);
        criteria.addNotEqualTo(StudentContact.COL_LIVES_WITH_INDICATOR, Boolean.TRUE);

        if (isSchoolContext()) {
            criteria.addEqualTo(StudentContact.REL_STUDENT + PATH_DELIMITER +
                    SisStudent.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            criteria.addAndCriteria(getOrganizationCriteria(StudentContact.class));
        }

        QueryByCriteria query = new QueryByCriteria(StudentContact.class, criteria);

        m_studentContacts = getBroker().getGroupedCollectionByQuery(query,
                StudentContact.COL_STUDENT_OID, 2000);
    }
}
