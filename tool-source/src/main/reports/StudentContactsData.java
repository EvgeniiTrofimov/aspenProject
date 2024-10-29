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
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.StudentContact;
import com.follett.fsc.core.k12.tools.reports.QueryIteratorDataSource;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.tools.reports.StudentContextReportHelper;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Data source for the "Student Contacts" report.
 *
 * @author X2 Development Corporation
 */
public class StudentContactsData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Name for the "active only" report parameter. The value is an Boolean.
     */
    public static final String ACTIVE_ONLY_PARAM = "activeOnly";

    /**
     * Name for the "contact sort" report parameter. The value is a String.
     */
    public static final String CONTACT_SORT_PARAM = "contactSort";

    /**
     * Name for the "query by" report parameter. The value is a String.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the "selection value" report parameter. The value is a String.
     */
    public static final String QUERY_STRING_PARAM = "queryString";

    /**
     * Name for the "student sort" report parameter. The value is a String.
     */
    public static final String STUDENT_SORT_PARAM = "studentSort";

    private SisStudent m_student;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        X2Criteria criteria = buildCriteria();
        QueryByCriteria query = new QueryByCriteria(StudentContact.class, criteria);

        applySortOrder(query);

        QueryIterator contacts = getBroker().getIteratorByQuery(query);
        return new QueryIteratorDataSource(contacts);
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
        m_student = userData.getCurrentRecord(SisStudent.class);
    }

    /**
     * Applies a sort order to the student contact query based on report input parameters.
     *
     * @param query QueryByCriteria
     */
    private void applySortOrder(QueryByCriteria query) {
        String studentSort = (String) getParameter(STUDENT_SORT_PARAM);
        applyUserSort(query, studentSort);

        query.addOrderByAscending(StudentContact.COL_STUDENT_OID);

        String contactSort = (String) getParameter(CONTACT_SORT_PARAM);
        applyUserSort(query, contactSort);
    }

    /**
     * Builds the criteria for the student contact query based on report input parameters.
     *
     * @return X2Criteria
     */
    private X2Criteria buildCriteria() {
        X2Criteria criteria = new X2Criteria();
        if (m_student != null) {
            criteria.addEqualTo(StudentContact.COL_STUDENT_OID, m_student.getOid());
        } else {
            StudentContextReportHelper helper =
                    new StudentContextReportHelper(getOrganization(), getCurrentContext(), getBroker());

            String queryBy = (String) getParameter(QUERY_BY_PARAM);
            String queryString = (String) getParameter(QUERY_STRING_PARAM);

            addUserCriteria(criteria, queryBy, queryString, StudentContact.class, SisStudent.class,
                    StudentContact.COL_STUDENT_OID);

            if (isSchoolContext()) {
                criteria.addEqualTo(StudentContact.REL_STUDENT + PATH_DELIMITER + helper.getSchoolOidField(),
                        getSchool().getOid());
            } else {
                criteria.addAndCriteria(getOrganizationCriteria(StudentContact.class));
            }

            boolean activeOnly = ((Boolean) getParameter(ACTIVE_ONLY_PARAM)).booleanValue();
            if (activeOnly) {
                criteria.addAndCriteria(helper.getActiveStudentCriteria(StudentContact.REL_STUDENT + PATH_DELIMITER));

            }
        }

        return criteria;
    }
}
