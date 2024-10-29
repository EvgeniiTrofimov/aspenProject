/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2006 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
import com.follett.fsc.core.k12.beans.DataFieldConfig;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.RecordSet;
import com.follett.fsc.core.k12.beans.RecordSetKey;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.reports.QueryIteratorDataSource;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.x2dev.sis.model.beans.GradeTerm;
import com.x2dev.sis.model.beans.ScheduleTerm;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.SchoolScheduleContext;
import com.x2dev.sis.model.beans.StudentSchedule;
import com.x2dev.sis.model.beans.TranscriptColumnDefinition;
import com.x2dev.utils.StringUtils;
import java.util.LinkedList;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.QueryBySQL;

/**
 * Report that identifies courses from student schedules without grades in a selected column.
 *
 * @author X2 Development Corporation
 */
public class TranscriptAuditData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Parameter containing the selected grade column bean.
     */
    public static final String PARAM_COLUMN = "transcriptColumn";

    /**
     * Parameter containing the OID of the selected grade column.
     */
    public static final String PARAM_COLUMN_OID = "transcriptColumnOid";

    /**
     * Parameter containing the selected context bean.
     */
    public static final String PARAM_CONTEXT = "context";

    /**
     * Parameter containing the OID of the selected school year.
     */
    public static final String PARAM_CONTEXT_OID = "contextOid";

    /**
     * Parameter containing the "query by" value.
     */
    public static final String PARAM_QUERY_BY = "queryBy";

    /**
     * Parameter containing the query string value.
     */
    public static final String PARAM_QUERY_STRING = "queryString";

    /**
     * Parameter containing the sort order.
     */
    public static final String PARAM_SORT = "sort";

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        TranscriptColumnDefinition column =
                (TranscriptColumnDefinition) getBroker().getBeanByOid(TranscriptColumnDefinition.class,
                        (String) getParameter(PARAM_COLUMN_OID));
        addParameter(PARAM_COLUMN, column);

        DistrictSchoolYearContext context =
                (DistrictSchoolYearContext) getBroker().getBeanByOid(DistrictSchoolYearContext.class,
                        (String) getParameter(PARAM_CONTEXT_OID));
        addParameter(PARAM_CONTEXT, context);

        DataFieldConfig field = column.getDataFieldConfig();

        String scheduleOid = getScheduleOid();
        String termOids = getScheduleTermOids(column, scheduleOid);

        String sql = "SELECT * " +
                "FROM STUDENT_SCHEDULE " +
                "INNER JOIN STUDENT " +
                "ON SSC_STD_OID = STD_OID " +
                "INNER JOIN SCHEDULE_MASTER " +
                "ON SSC_MST_OID = MST_OID " +
                "INNER JOIN COURSE_SCHOOL " +
                "ON MST_CSK_OID = CSK_OID " +
                "LEFT OUTER JOIN STUDENT_TRANSCRIPT " +
                "ON SSC_MST_OID = TRN_MST_OID " +
                "AND SSC_STD_OID = TRN_STD_OID " +
                "WHERE CSK_MASTER_TYPE = '" + SchoolCourse.MASTER_TYPE_CLASS + "' " +
                "AND SSC_SCH_OID = '" + scheduleOid + "' ";

        if (termOids != null) {
            sql += "AND EXISTS (SELECT MTM_OID " +
                    " FROM SCHEDULE_MASTER_TERM " +
                    " WHERE MTM_TRM_OID IN (" + termOids + ") " +
                    " AND MTM_MST_OID = SCHEDULE_MASTER.MST_OID) ";
        }

        sql += "AND (" + field.getDataField().getDatabaseName() + " IS NULL " +
                "OR " + field.getDataField().getDatabaseName() + " = '') ";

        int queryBy = ((Integer) getParameter(PARAM_QUERY_BY)).intValue();
        String queryString = (String) getParameter(PARAM_QUERY_STRING);
        switch (queryBy) {
            case 0: // YOG
                sql += " AND STD_YOG = " + queryString + " ";
                break;

            case 1: // Record set
                RecordSet recordSet = getRecordSet(queryString);
                if (recordSet != null) {
                    sql += "AND STD_OID IN (SELECT RSK_OBJ_OID " +
                            " FROM RECORD_SET_KEY " +
                            "WHERE RSK_RSN_OID = '" + recordSet.getOid() + "') ";
                } else {
                    sql += "AND STD_OID = 'dummyOID' ";
                }
                break;

            default: // All
        }

        int sort = ((Integer) getParameter(PARAM_SORT)).intValue();
        switch (sort) {
            case 0: // Student
                sql += "ORDER BY STD_NAME_VIEW, STD_YOG, STD_OID, MST_COURSE_VIEW ";
                break;

            case 1: // Teacher
            default:
                sql += "ORDER BY MST_STAFF_VIEW, MST_STF_OID_PRIMARY, MST_COURSE_VIEW, MST_CSK_OID, STD_NAME_VIEW ";
        }

        QueryBySQL query = new QueryBySQL(StudentSchedule.class, sql);
        QueryIterator iterator = getBroker().getIteratorByQuery(query);

        return new QueryIteratorDataSource(iterator,
                DataDictionary.getDistrictDictionary(getUser().getPersistenceKey()), true, getLocale());
    }

    /**
     * Returns the record set visible to the current user with the passed name, if any. If one does
     * not exist, null is returned.
     *
     * @param name String
     * @return RecordSet
     */
    private RecordSet getRecordSet(String name) {
        Criteria criteria = ReportUtils.getRecordSetCriteria(name, getUser(), getSchool());
        QueryByCriteria query = new QueryByCriteria(RecordSetKey.class, criteria);
        query.setObjectProjectionAttribute(RecordSetKey.REL_RECORD_SET);

        return (RecordSet) getBroker().getBeanByQuery(query);
    }

    /**
     * Returns the schedule term OIDs that should be considered for the passed column.
     * <p>
     * If the column specifies a grade term, the schedule terms covered by that grade term are
     * returned. If no covered schedule terms are found, a dummy term OID is included so that the
     * query will result in no records.
     * <p>
     * If the column does not specify a grade term, null is returned.
     *
     * @param column TranscriptColumnDefinition
     * @param scheduleOid String
     * @return String - comma delimited, single-quote wrapped list of schedule term OIDs
     */
    private String getScheduleTermOids(TranscriptColumnDefinition column, String scheduleOid) {
        String scheduleTermOids = null;

        GradeTerm gradeTerm = column.getGradeTerm();
        if (gradeTerm != null) {
            LinkedList<String> oidList = new LinkedList<String>();

            Criteria criteria = new Criteria();
            criteria.addEqualTo(ScheduleTerm.COL_SCHEDULE_OID, scheduleOid);

            QueryByCriteria query = new QueryByCriteria(ScheduleTerm.class, criteria);

            QueryIterator iterator = getBroker().getIteratorByQuery(query);
            try {
                while (iterator.hasNext()) {
                    ScheduleTerm term = (ScheduleTerm) iterator.next();

                    if (term.coversGradeTerm(gradeTerm)) {
                        oidList.add(term.getOid());
                    }
                }
            } finally {
                iterator.close();
            }

            if (oidList.isEmpty()) {
                oidList.add("dummyOID");
            }

            scheduleTermOids = StringUtils.convertCollectionToDelimitedString(oidList, ",", "'");
        }

        return scheduleTermOids;
    }

    /**
     * Returns the active schedule OID for the current school and selected school year. If one
     * does not exist, a dummy value is returned so that the query will result in no records.
     *
     * @return String
     */
    private String getScheduleOid() {
        String scheduleOid = "dummyOID";

        String contextOid = (String) getParameter(PARAM_CONTEXT_OID);

        Criteria criteria = new Criteria();
        criteria.addEqualTo(SchoolScheduleContext.COL_SCHOOL_OID, getSchool().getOid());
        criteria.addEqualTo(SchoolScheduleContext.COL_DISTRICT_CONTEXT_OID, contextOid);

        QueryByCriteria query = new QueryByCriteria(SchoolScheduleContext.class, criteria);

        SchoolScheduleContext context = (SchoolScheduleContext) getBroker().getBeanByQuery(query);

        if (context != null) {
            scheduleOid = context.getActiveScheduleOid();
        }

        return scheduleOid;
    }
}
