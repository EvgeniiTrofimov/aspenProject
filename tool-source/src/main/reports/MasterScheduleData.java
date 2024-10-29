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

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.tools.reports.QueryIteratorDataSource;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.Section;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentSection;
import com.x2dev.sis.tools.reports.ScheduleReportHelper;
import com.x2dev.utils.X2BaseException;
import java.util.HashMap;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Prepares the data for the Master Schedule report. This report lists all the sections in the
 * active schedule for the selected school. The sections will be ordered according to the user
 * input.
 * <p>
 * This report supports both the MasterSchedule and BuildMasterSchedule objects.
 *
 * @author X2 Development Corporation
 */
public class MasterScheduleData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Name for the section-OID-to-female-count report parameter. The value is a Map of female
     * enrollment counts (Long objects) keyed on MasterSchedule OIDs (String objects).
     */
    public static final String GENDER_MAP_PARAM = "genderMap";

    /**
     * Name for the "selection" report parameter. The value is a String.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the "sort" report parameter. The value is a String.
     */
    public static final String SORT_PARAM = "sort";

    private ScheduleReportHelper m_reportHelper;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        Criteria criteria = new Criteria();

        /*
         * Build the schedule criteria based on the school's current schedule and the user's input.
         */
        addUserCriteria(criteria, (String) getParameter(QUERY_BY_PARAM), null, null, null);
        criteria.addEqualTo(Section.COL_SCHEDULE_OID, m_reportHelper.getScheduleOid());

        QueryByCriteria query = createQueryByCriteria(m_reportHelper.getSectionClass(), criteria);

        /*
         * Build the sort based on user input.
         */
        applyUserSort(query, (String) getParameter(SORT_PARAM));

        /*
         * First create a map to link each section to a percentage of female students. This map will
         * become a report parameter.
         *
         * BYT 11/17/05 move the following code after updating the sort order of the query. The
         * reason
         * is OJB caches the sql for a query once it runs. Therefore any change made after a query
         * is run won't take affect.
         */
        addParameter(GENDER_MAP_PARAM, getFemaleCounts(getBroker().getCount(query)));

        /*
         * Execute the query
         */
        return new QueryIteratorDataSource(getBroker().getIteratorByQuery(query));
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
     * Returns a non-null map that relates all sections in the selected school's active schedule to
     * the number of enrolled female students.
     *
     * @param sectionCount the number of sections in the master schedule
     *
     * @return A Map of String keys to Long values
     */
    private Map getFemaleCounts(int sectionCount) {
        HashMap femaleCounts = new HashMap((int) (sectionCount * 1.5 + 1));

        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(StudentSection.COL_SCHEDULE_OID, m_reportHelper.getScheduleOid());
        criteria.addBeginsWithIgnoreCase(StudentSection.REL_STUDENT + "." +
                SisStudent.REL_PERSON + "." + SisPerson.COL_GENDER_CODE, "F");

        String[] columns = new String[] {StudentSection.COL_SECTION_OID, "count(*)"};
        ReportQueryByCriteria query =
                new ReportQueryByCriteria(m_reportHelper.getStudentSectionClass(), columns, criteria);
        query.addGroupBy(StudentSection.COL_SECTION_OID);

        ReportQueryIterator results = null;
        try {
            results = getBroker().getReportQueryIteratorByQuery(query);
            while (results.hasNext()) {
                Object[] values = (Object[]) results.next();

                /*
                 * Different Jdbc driver returns different value type.
                 * SQL server returns Integer while Mysql returns Long.
                 * Oracle ojdbc6 driver returns BigDecimal
                 */

                String countString = values[1].toString();
                Long count = Long.valueOf(countString);

                femaleCounts.put(values[0], count);
            }
        } finally {
            if (results != null) {
                results.close();
            }
        }

        return femaleCounts;
    }
}
