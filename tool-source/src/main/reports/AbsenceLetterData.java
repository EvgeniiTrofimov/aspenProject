/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2011 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.StudentContact;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.tools.reports.Publishable;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.follett.fsc.core.k12.web.ContextList;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAttendance;
import com.x2dev.sis.model.beans.StudentEventTracking;
import com.x2dev.sis.tools.reports.StudentContextReportHelper;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares the data for the "Absence Letter" report. This report lists the student attendance
 * matching the total criteria within the entered date range.
 *
 * This report also creates StudentEventTracking records for the students receiving letters.
 *
 * @author X2 Development Corporation
 */
public class AbsenceLetterData extends ReportJavaSourceNet implements Publishable {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Name for the "absences" report parameter. The value is a String representation of a
     * BigDecimal.
     */
    public static final String ABSENCES_PARAM = "absences";

    /**
     * Name for the "end date" report parameter. The value is a PlainDate.
     */
    public static final String END_DATE_PARAM = "endDate";

    /**
     * Name for the "exclude excused absences" report parameter. The value is a Boolean.
     */
    public static final String EXCLUDE_EXCUSED_PARAM = "excludeExcused";

    /**
     * Name for the "include students with previous mailing" report parameter. The value is a
     * Boolean.
     */
    public static final String INCLUDE_ALL_PARAM = "includeAll";

    /**
     * Name for the "multiple mailings" report parameter. The value is a Boolean.
     */
    public static final String MULTIPLE_MAILINGS_PARAM = "multipleMailings";

    /**
     * Name for the "sort" report parameter. The value is an Integer.
     */
    public static final String SORT_PARAM = "sort";

    /**
     * Name for the "start date" report parameter. The value is a PlainDate.
     */
    public static final String START_DATE_PARAM = "startDate";

    private static final String EVENT_PREFIX = "Absence Letter ";

    /*
     * Grid fields
     */
    public static final String COL_ABSENT_TOTAL = "absences";
    public static final String COL_ADDRESS = "address";
    public static final String COL_STUDENT = "student";
    public static final String COL_STUDENT_OID = "studentOid";

    private StudentContextReportHelper m_helper;
    private Map<String, Collection<Person>> m_recipientCache;
    private Map<String, Collection<StudentContact>> m_studentContacts;
    private String m_studentOid;

    /**
     * Gets the data break column.
     *
     * @return String
     * @see com.x2dev.sis.tools.reports.Publishable#getDataBreakColumn()
     */
    @Override
    public String getDataBreakColumn() {
        return StudentAttendance.COL_STUDENT_OID;
    }

    /**
     * Gets the description.
     *
     * @param bean X2BaseBean
     * @return String
     * @see com.x2dev.sis.tools.reports.Publishable#getDescription(X2BaseBean)
     */
    @Override
    public String getDescription(X2BaseBean bean) {
        return "Absence Letter for " + ((SisStudent) bean).getNameView();
    }

    /**
     * Gets the email address.
     *
     * @param person Person
     * @return String
     * @see
     *      com.x2dev.sis.tools.reports.Publishable#getEmailAddress(com.x2dev.sis.model.beans.Person)
     */
    @Override
    public String getEmailAddress(Person person) {
        return person.getEmail01();
    }

    /**
     * Gets the email recipients.
     *
     * @param bean X2BaseBean
     * @return Collection
     * @see com.x2dev.sis.tools.reports.Publishable#getEmailRecipients(com.x2dev.sis.model.beans.
     *      X2BaseBean)
     */
    @Override
    public Collection<Person> getEmailRecipients(X2BaseBean bean) {
        return m_recipientCache.get(bean.getOid());
    }

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.x2dev.sis.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        ReportDataGrid grid = new ReportDataGrid(300, 2);
        Collection<String> studentOids = new ArrayList<String>();

        /*
         * Prepare the SQL parameters based on preferences/user input.
         */
        boolean excludeExcused = ((Boolean) getParameter(EXCLUDE_EXCUSED_PARAM)).booleanValue();
        boolean includeAll = ((Boolean) getParameter(INCLUDE_ALL_PARAM)).booleanValue();
        boolean multipleMailings = ((Boolean) getParameter(MULTIPLE_MAILINGS_PARAM)).booleanValue();

        PlainDate startDate = (PlainDate) getParameter(START_DATE_PARAM);
        PlainDate endDate = (PlainDate) getParameter(END_DATE_PARAM);

        BigDecimal count = new BigDecimal((String) getParameter(ABSENCES_PARAM));
        String eventType = EVENT_PREFIX + count;

        /*
         * Execute a SQL statement to get the students and their absence total. The SQL is easier
         * to write/maintain even though a query is then run for every student that matches the
         * result set.
         */
        if (startDate != null && endDate != null && endDate.after(startDate)) {
            StringBuilder sql = new StringBuilder(512);
            sql.append("SELECT A.ATT_STD_OID, SUM(A.ATT_PORTION_ABSENT), T0.TRK_OID, STD_NAME_VIEW, STD_YOG, "
                    + m_helper.getHomeroomColumn());
            sql.append("  FROM (");

            sql.append("   SELECT ATT_STD_OID, ATT_PORTION_ABSENT");
            sql.append("     FROM STUDENT_ATTENDANCE");
            sql.append("    INNER JOIN STUDENT ON ATT_STD_OID = STD_OID");

            if (m_helper.isContextOverride()) {
                sql.append(" INNER JOIN STUDENT_CONTEXT_ATTRIBUTES ON STD_SXA_OID_CURRENT = SXA_OID ");
            }

            sql.append("    WHERE ATT_DATE >= ?");
            sql.append("      AND ATT_DATE <= ?");
            sql.append("      AND " + StudentManager.getActiveStudentDirectSQL(getOrganization(),
                    m_helper.getEnrollmentStatusColumn()));
            sql.append("      AND " + m_helper.getSchoolOidColumn() + " = '" + getSchool().getOid() + "'");

            if (m_studentOid != null) {
                sql.append("      AND STD_OID = '" + m_studentOid + "'");
            }

            if (excludeExcused) {
                sql.append("      AND ATT_EXCUSED_IND = '0'");
            }

            sql.append(") A INNER JOIN STUDENT ON A.ATT_STD_OID = STD_OID ");
            sql.append("    LEFT OUTER JOIN STUDENT_EVENT_TRACKING T0 ON T0.TRK_STD_OID = STD_OID ");
            sql.append("                                             AND T0.TRK_CTX_OID = '"
                    + getCurrentContext().getOid() + "'");
            sql.append("                                             AND T0.TRK_EVENT_TYPE = '" + eventType + "'");

            if (!includeAll) {
                sql.append(" WHERE ATT_STD_OID NOT IN (SELECT T1.TRK_STD_OID ");
                sql.append("                             FROM STUDENT_EVENT_TRACKING T1");
                sql.append("                            WHERE T1.TRK_CTX_OID = '" + getCurrentContext().getOid() + "'");
                sql.append("                              AND T1.TRK_EVENT_TYPE = '" + eventType + "')");
            }

            sql.append("  GROUP BY A.ATT_STD_OID, T0.TRK_OID, STD_NAME_VIEW, STD_YOG, " + m_helper.getHomeroomColumn());
            sql.append(" HAVING SUM(A.ATT_PORTION_ABSENT) >= " + count.doubleValue());
            sql.append("  ORDER BY ");

            int sort = ((Integer) getParameter(SORT_PARAM)).intValue();
            switch (sort) {
                case 1: // YOG
                    sql.append("5, 4");
                    break;

                case 2: // Homeroom
                    sql.append("6, 4");
                    break;

                default: // Name (case 0)
                    sql.append("4");
                    break;
            }

            Connection connection = getBroker().borrowConnection();
            PreparedStatement statement = null;
            ResultSet results = null;

            if (multipleMailings) {
                loadMailingContacts();
            }

            try {
                ModelBroker broker = new ModelBroker(getPrivilegeSet());
                PlainDate today = new PlainDate(getTimeZone());

                statement = connection.prepareStatement(sql.toString());
                statement.setDate(1, startDate);
                statement.setDate(2, endDate);

                results = statement.executeQuery();
                while (results.next()) {
                    String studentOid = results.getString(1);
                    double absentTotal = results.getDouble(2);
                    String eventOid = results.getString(3);

                    Student student =
                            (Student) getBroker().getBeanByOid(Student.class, studentOid);

                    /*
                     * Add the student bean the absence total to a grid. Create a event record if
                     * one doesn't already exist.
                     */
                    grid.append();
                    grid.set(COL_STUDENT_OID, studentOid);
                    grid.set(COL_STUDENT, student);
                    grid.set(COL_ABSENT_TOTAL, Double.valueOf(absentTotal));
                    grid.set(COL_ADDRESS, student.getPerson().getResolvedMailingAddress());

                    if (multipleMailings) {
                        Collection<StudentContact> contacts = m_studentContacts.get(studentOid);
                        if (contacts != null) {
                            for (StudentContact contact : contacts) {
                                grid.append();
                                grid.set(COL_STUDENT_OID, studentOid);
                                grid.set(COL_STUDENT, student);
                                grid.set(COL_ABSENT_TOTAL, Double.valueOf(absentTotal));
                                grid.set(COL_ADDRESS, contact.getContact().getPerson().getResolvedMailingAddress());
                            }
                        }
                    }

                    if (eventOid == null) {
                        StudentEventTracking event = new StudentEventTracking(getBroker().getPersistenceKey());

                        event.setDistrictContextOid(getCurrentContext().getOid());
                        event.setEventDate(today);
                        event.setEventType(eventType);
                        event.setStudentOid(studentOid);

                        broker.saveBean(event);
                    }

                    studentOids.add(studentOid);
                }
            } catch (SQLException sqle) {
                AppGlobals.getLog().log(Level.WARNING, sqle.getMessage(), sqle);
            } finally {
                try {
                    if (results != null) {
                        results.close();
                    }

                    if (statement != null) {
                        statement.close();
                    }
                } catch (Exception e) {
                    AppGlobals.getLog().log(Level.WARNING, e.getMessage(), e);
                }

                getBroker().returnConnection();
            }
        }

        grid.beforeTop();
        return grid;
    }

    /**
     * Initialize.
     *
     * @see com.x2dev.sis.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        m_helper = new StudentContextReportHelper(getOrganization(), getCurrentContext(), getBroker());

        setRecipientCache();
    }

    /**
     * Remember the currently selected student if this report is being run from the student module.
     *
     * @param userData UserDataContainer
     * @see com.x2dev.sis.tools.ToolJavaSource#saveState(com.x2dev.sis.web.UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) {
        ContextList parentList = userData.getParentList();
        if (parentList != null && parentList.getDataClass().equals(Student.class)) {
            m_studentOid = parentList.getCurrentRecord().getOid();
        }
    }

    /**
     * Loads the mailing contacts for students into a Map of StudentContacts keyed to student OID.
     */
    private void loadMailingContacts() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(StudentContact.COL_CONDUCT_MAILING_INDICATOR, Boolean.TRUE);
        criteria.addNotEqualTo(StudentContact.COL_LIVES_WITH_INDICATOR, Boolean.TRUE);

        if (isSchoolContext()) {
            criteria.addEqualTo(StudentContact.REL_STUDENT + PATH_DELIMITER +
                    m_helper.getSchoolOidField(), getSchool().getOid());
        }

        QueryByCriteria query = new QueryByCriteria(StudentContact.class, criteria);
        m_studentContacts = getBroker().getGroupedCollectionByQuery(query, StudentContact.COL_STUDENT_OID, 2000);
    }

    /**
     * Populates the recipient cache for published reports.
     */
    private void setRecipientCache() {
        Map<String, Collection<Person>> recipientCacheMap = new HashMap<String, Collection<Person>>();

        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(StudentContact.COL_PORTAL_ACCESS_INDICATOR, Boolean.TRUE);

        if (isSchoolContext()) {
            criteria.addEqualTo(StudentContact.REL_STUDENT + PATH_DELIMITER +
                    m_helper.getSchoolOidField(), getSchool().getOid());
        }

        QueryByCriteria query = new QueryByCriteria(StudentContact.class, criteria);
        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                StudentContact contact = (StudentContact) iterator.next();
                String studentOid = contact.getStudentOid();
                Person person = contact.getPerson();

                Collection<Person> recipients = recipientCacheMap.get(studentOid);
                if (recipients == null) {
                    recipients = new ArrayList<Person>();
                }

                recipients.add(person);
                recipientCacheMap.put(studentOid, recipients);
            }
        } finally {
            iterator.close();
        }

        m_recipientCache = recipientCacheMap;
    }
}
