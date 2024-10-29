/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2010 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.reports.sys.sped.ma;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.StudentContact;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.ReferenceDescriptionLookup;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepData.StatusCode;
import com.x2dev.sis.model.beans.IepMeeting;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Java source for the special education register, which displays a list of students along with
 * contact and IEP information.
 * <p>
 * This report prints based on the current student selection and current sort.
 *
 * @author X2 Development Corporation
 */
public class SpedRegisterData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Report parameter containing the IEP dictionary. This is required by the report format to
     * retrieve extended field value by alias.
     */
    public static final String PARAM_DICTIONARY = "dictionary";

    /**
     * Report date parameter. The IEP to display on the report for each student is retrieved based
     * on this date.
     */
    public static final String PARAM_REPORT_DATE = "reportDate";

    /**
     * Reference Description lookup parameter. The reference lookup table to obtain the student's
     * SPED primary disability code.
     */
    public static final String REFERENCE_DESCRIPTION_LOOKUP_PARAM = "refLookup";

    // ReportDataGrid column constant
    private static final String COL_CONTACT = "contact"; // used as a prefix - e.g. contact0,
                                                         // contact1...
    private static final String COL_IEP = "iep";
    private static final String COL_MEETING = "meeting"; // used as a prefix - e.g. meeting0,
                                                         // meeting1...
    private static final String COL_STUDENT = "student";

    /**
     * Returns the correct IEP based on the report date. Rules for determining the correct IEP are
     * as follows:
     * <ul>
     * <li>If the report date within the date range of any amount of implemented IEPs, those IEPs
     * are compared by status and end date (in that order) and one is returned.
     * <li>If the report date is after the end date of the most recent implemented IEP, the draft
     * IEP is returned if present; otherwise null is returned
     * <li>If the report date is prior to the start date of the oldest implemented IEP, null is
     * returned
     * <li>If the report date is not within the date range of an implemented IEP, but is between
     * the end date and the start date of two consecutive implemented IEPs, the most recent
     * IEP prior to the report date is returned
     * </ul>
     * Note: this method is "public static" for testing purposes.
     *
     * @param reportDate the report date
     * @param draftIep a draft IEP, if one exists
     * @param implementedIeps a collection of implemented IEPs - expected to be in ascending order
     *        by start date
     *
     * @return IepData
     */
    public static IepData findCorrectIep(PlainDate reportDate,
                                         IepData draftIep,
                                         Collection<IepData> implementedIeps) {
        boolean found = false;
        IepData iep = null;

        // the list of ieps within the accepted date range
        ArrayList<IepData> iepList = new ArrayList<IepData>();

        if (implementedIeps != null) {
            for (IepData implementedIep : implementedIeps) {

                if (reportDate.before(implementedIep.getStartDate())) {
                    // we will return null in this case
                    found = true;
                } else if (!reportDate.after(implementedIep.getEndDate())) {
                    iepList.add(implementedIep);
                    found = true;
                } else {
                    iep = implementedIep;
                }
            }
        }

        if (!found) {
            iep = draftIep;
        } else if (!iepList.isEmpty()) {
            iep = priorityIep(iepList);
        }

        return iep;
    }

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        ReportDataGrid grid = new ReportDataGrid();

        PlainDate reportDate = (PlainDate) getParameter(PARAM_REPORT_DATE);

        Map<String, Collection<StudentContact>> contactMap = getContacts();
        Map<String, IepData> draftIepMap = getDraftIeps();
        Map<String, Collection<IepData>> implementedIepMap = getImplementedIeps();
        Map<String, Collection<IepMeeting>> meetingMap = getMeetings();

        // Query and iterate over the students, build the grid
        Criteria currentCriteria = getCurrentCriteria();
        if (isSchoolContext()) {
            currentCriteria.addEqualTo(SisStudent.COL_SCHOOL_OID, getSchool().getOid());
        }
        QueryByCriteria studentQuery = new QueryByCriteria(SisStudent.class, currentCriteria);
        applyCurrentSort(studentQuery);

        QueryIterator students = getBroker().getIteratorByQuery(studentQuery);
        try {
            while (students.hasNext()) {
                SisStudent student = (SisStudent) students.next();

                IepData iep = findCorrectIep(reportDate,
                        draftIepMap.get(student.getOid()),
                        implementedIepMap.get(student.getOid()));

                grid.append();
                grid.set(COL_STUDENT, student);
                grid.set(COL_IEP, iep);

                Collection<StudentContact> contacts = contactMap.get(student.getOid());
                if (contacts != null) {
                    int i = 0;
                    for (StudentContact contact : contacts) {
                        grid.set(COL_CONTACT + (i++), contact);
                    }
                }

                if (iep != null) {
                    Collection<IepMeeting> meetings = meetingMap.get(iep.getOid());
                    if (meetings != null) {
                        int i = 0;
                        for (IepMeeting meeting : meetings) {
                            grid.set(COL_MEETING + (i++), meeting);
                        }
                    }
                }
            }
        } finally {
            students.close();
        }

        grid.beforeTop();

        DataDictionary dictionary =
                DataDictionary.getDistrictDictionary(getExtendedDictionary(), getBroker().getPersistenceKey());
        addParameter(PARAM_DICTIONARY, dictionary);

        addParameter(REFERENCE_DESCRIPTION_LOOKUP_PARAM,
                new ReferenceDescriptionLookup(getBroker(), getOrganization()));

        return grid;
    }

    /**
     * Creates a query based on the current contexts.
     *
     * @param schoolContext true if the query should be limited to a particular school, false
     *        otherwise
     * @param iepImplementationContext true if Implementation context, false if Draft context
     * @return a new QueryByCriteria object
     */
    private QueryByCriteria createQuery(boolean schoolContext, boolean iepImplementationContext) {
        Criteria criteria = new Criteria();
        criteria.addIn(IepData.COL_STUDENT_OID,
                new SubQuery(SisStudent.class, X2BaseBean.COL_OID, getCurrentCriteria()));

        if (iepImplementationContext) {
            criteria.addNotEqualTo(IepData.COL_STATUS_CODE, Integer.valueOf(IepData.StatusCode.DRAFT.ordinal()));
            criteria.addNotNull(IepData.COL_END_DATE);
            criteria.addNotNull(IepData.COL_START_DATE);
        } else {
            criteria.addEqualTo(IepData.COL_STATUS_CODE, Integer.valueOf(IepData.StatusCode.DRAFT.ordinal()));
        }

        if (schoolContext) {
            Criteria schoolCriteria = new Criteria();
            schoolCriteria.addEqualTo(SisStudent.COL_SCHOOL_OID, getSchool().getOid());
            SubQuery studentSub = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, schoolCriteria);
            criteria.addIn(IepData.COL_STUDENT_OID, studentSub);
        }

        return new QueryByCriteria(IepData.class, criteria);
    }

    /**
     * Returns a map of student contacts for each student in the selection. The contacts are sorted
     * in ascending order by emergency priority.
     *
     * @return Map
     */
    private Map<String, Collection<StudentContact>> getContacts() {
        QueryByCriteria contactQuery = new QueryByCriteria(SisStudent.class, getCurrentCriteria());
        contactQuery.setObjectProjectionAttribute(SisStudent.REL_CONTACTS);
        contactQuery.addOrderByAscending(StudentContact.COL_EMERGENCY_PRIORITY);

        return getBroker().getGroupedCollectionByQuery(contactQuery, StudentContact.COL_STUDENT_OID, 256);
    }

    /**
     * Returns a map of draft IEPs for each student in the selection. Note that a student can only
     * have 0 or 1 draft IEPs at a time.
     *
     * @return Map
     */
    private Map<String, IepData> getDraftIeps() {
        QueryByCriteria query = createQuery(isSchoolContext(), false);
        query.addOrderByAscending(IepData.COL_END_DATE);

        return getBroker().getMapByQuery(query, IepData.COL_STUDENT_OID, 256);
    }

    /**
     * Returns a map of implemented IEPs for each student in the selection. An IEP with both a
     * start and end date that is not a draft IEP is considered implemented.
     *
     * @return Map
     */
    private Map<String, Collection<IepData>> getImplementedIeps() {
        QueryByCriteria query = createQuery(isSchoolContext(), true);
        query.addOrderByAscending(IepData.COL_END_DATE);

        return getBroker().getGroupedCollectionByQuery(query, IepData.COL_STUDENT_OID, 256);
    }

    /**
     * Returns a map of meetings for each student in the selection. Meetings are sorted in
     * descending order by date.
     *
     * @return Map
     */
    private Map<String, Collection<IepMeeting>> getMeetings() {
        Criteria criteria = new Criteria();
        criteria.addIn(IepMeeting.COL_STUDENT_OID,
                new SubQuery(SisStudent.class, X2BaseBean.COL_OID, getCurrentCriteria()));

        QueryByCriteria query = new QueryByCriteria(IepMeeting.class, criteria);
        query.addOrderByDescending(IepMeeting.COL_DATE);

        return getBroker().getGroupedCollectionByQuery(query, IepMeeting.COL_IEP_DATA_OID, 256);
    }

    /**
     * Determines which iep in the given collection is most applicable.
     * Active ieps and those with the latest end date will take precedence.
     *
     * @param ieps Collection<IepData>
     * @return IepData
     */
    private static IepData priorityIep(Collection<IepData> ieps) {
        IepData priority = null;

        for (IepData iep : ieps) {
            if (priority == null) {
                priority = iep;
            } else {
                boolean iepAfterPriority = iep.getEndDate().after(priority.getEndDate());
                boolean iepIsActive = iep.getStatusCodeEnum().equals(StatusCode.ACTIVE);
                boolean priorityIsActive = priority.getStatusCodeEnum().equals(StatusCode.ACTIVE);

                if ((iepIsActive && iepAfterPriority) ||
                        (!priorityIsActive && (iepIsActive || iepAfterPriority))) {
                    priority = iep;
                }
            }
        }
        return priority;
    }
}
