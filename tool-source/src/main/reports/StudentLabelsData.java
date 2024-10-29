/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2003 X2 Development Corporation.
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
import com.follett.fsc.core.k12.beans.StudentContact;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.x2dev.sis.model.beans.SisStudent;
import java.util.Collection;
import java.util.Iterator;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares the data for the Student Label reports.
 *
 * These reports simply select students from the current school (with an optional criteria for YOG
 * or homeroom) and order the results by YOG, homeroom, or last name, with an option to print for
 * contacts with alternate mailing options.
 *
 * @author X2 Development Corporation
 */
public class StudentLabelsData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Name for the "active only" report parameter. The value is a Boolean.
     */
    public static final String ACTIVE_ONLY_PARAM = "activeOnly";

    /**
     * Name for the "multiple mailings" report parameter. The value is a Boolean.
     */
    public static final String MULTIPLE_MAILINGS_PARAM = "multipleMailings";

    /**
     * Name for the enumerated "selection" report parameter. The value is a String.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the "selection value" report parameter. The value is a String.
     */
    public static final String QUERY_STRING_PARAM = "queryString";

    /**
     * Name for the "secondary students" report parameter. The value is a Boolean.
     */
    public static final String SECONDARY_STUDENT_PARAM = "secondaryStudent";

    /**
     * Name for the enumerated "sort" report parameter. The value is a String.
     */
    public static final String SORT_PARAM = "sort";

    /*
     * Grid fields
     */
    private static final String FIELD_ADDRESS = "address";
    private static final String FIELD_PERSON = "person";
    private static final String FIELD_STUDENT = "student";

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

        /*
         * Build the criteria based on user input
         */
        Criteria criteria = new Criteria();

        if (isSchoolContext()) {
            criteria.addEqualTo(SisStudent.COL_SCHOOL_OID, getSchool().getOid());

            /*
             * Include secondary students of the school if needed.
             */
            if (((Boolean) getParameter(SECONDARY_STUDENT_PARAM)).booleanValue()) {
                criteria.addOrCriteria(StudentManager.buildSecondaryStudentCriteria(getSchool()));
            }
        } else {
            criteria.addAndCriteria(getOrganizationCriteria(SisStudent.class));
        }

        boolean activeOnly = ((Boolean) getParameter(ACTIVE_ONLY_PARAM)).booleanValue();
        if (activeOnly) {
            criteria.addAndCriteria(
                    StudentManager.getActiveStudentStatusCriteria(getOrganization(), SisStudent.COL_ENROLLMENT_STATUS));

        }

        addUserCriteria(criteria, (String) getParameter(QUERY_BY_PARAM), (String) getParameter(QUERY_STRING_PARAM),
                null, null);

        QueryByCriteria query = createQueryByCriteria(SisStudent.class, criteria);
        applyUserSort(query, (String) getParameter(SORT_PARAM));

        if (multipleMailings) {
            loadMailingContacts(criteria);
        }

        ReportDataGrid grid = new ReportDataGrid(5);
        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                SisStudent student = (SisStudent) iterator.next();

                grid.append();
                grid.set(FIELD_STUDENT, student);
                grid.set(FIELD_PERSON, student.getPerson());
                grid.set(FIELD_ADDRESS, student.getPerson().getResolvedMailingAddress());

                if (multipleMailings) {
                    Collection contacts = (Collection) m_studentContacts.get(student.getOid());
                    if (contacts != null) {
                        Iterator contactIterator = contacts.iterator();
                        while (contactIterator.hasNext()) {
                            StudentContact contact = (StudentContact) contactIterator.next();

                            grid.append();
                            grid.set(FIELD_STUDENT, student);
                            grid.set(FIELD_PERSON, student.getPerson());
                            grid.set(FIELD_ADDRESS, contact.getContact().getPerson().getResolvedMailingAddress());
                        }
                    }
                }
            }
        } finally {
            iterator.close();
        }

        grid.beforeTop();
        return grid;
    }

    /**
     * Loads the mailing contacts for students into a Map of StudentContacts keyed to student OID.
     *
     * @param studentCriteria Criteria
     */
    private void loadMailingContacts(Criteria studentCriteria) {
        Criteria criteria = new Criteria();
        criteria.addNotEqualTo(StudentContact.COL_LIVES_WITH_INDICATOR, Boolean.TRUE);
        criteria.addIn(StudentContact.COL_STUDENT_OID,
                new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria));

        X2Criteria mailToCriteria = new X2Criteria();
        mailToCriteria.addOrEqualTo(StudentContact.COL_CONDUCT_MAILING_INDICATOR, Boolean.TRUE);
        mailToCriteria.addOrEqualTo(StudentContact.COL_GRADE_MAILING_INDICATOR, Boolean.TRUE);
        mailToCriteria.addOrEqualTo(StudentContact.COL_OTHER_MAILING_INDICATOR, Boolean.TRUE);

        criteria.addAndCriteria(mailToCriteria);

        QueryByCriteria query = new QueryByCriteria(StudentContact.class, criteria);

        m_studentContacts = getBroker().getGroupedCollectionByQuery(query,
                StudentContact.COL_STUDENT_OID, 2000);
    }
}
