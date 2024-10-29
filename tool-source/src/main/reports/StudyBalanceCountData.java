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
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sched.utils.ScheduleMap;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.tools.reports.ScheduleReportHelper;
import com.x2dev.utils.X2BaseException;
import java.math.BigDecimal;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Prepares the data for Study Balance Count report.
 *
 * @author X2 Development Corporation
 */
public class StudyBalanceCountData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Name of the grid column that contains the count values for the first YOG.
     */
    public static final String FIELD_COUNT1 = "count1";

    /**
     * Name of the grid column that contains the count values for the second YOG.
     */
    public static final String FIELD_COUNT2 = "count2";

    /**
     * Name of the grid column that contains the count values for the third YOG.
     */
    public static final String FIELD_COUNT3 = "count3";

    /**
     * Name of the grid column that contains the count values for the fourth YOG.
     */
    public static final String FIELD_COUNT4 = "count4";

    /**
     * Name of the grid column that contains the count values for YOGs after the fourth.
     */
    public static final String FIELD_COUNT_OTHER = "countOther";

    /**
     * Name of the grid column that contains the day values.
     */
    public static final String FIELD_DAY = "day";

    /**
     * Name of the grid column that contains the period values.
     */
    public static final String FIELD_PERIOD = "period";

    /**
     * Name of the grid column that contains the study section values.
     */
    public static final String FIELD_SECTION = "studySection";

    /**
     * Name of the grid column that contains the term values.
     */
    public static final String FIELD_TERM = "term";

    /**
     * Name of the grid column that contains the first YOG values.
     */
    public static final String FIELD_YOG1 = "yog1";

    /**
     * Name of the grid column that contains the second YOG values.
     */
    public static final String FIELD_YOG2 = "yog2";

    /**
     * Name of the grid column that contains the third YOG values.
     */
    public static final String FIELD_YOG3 = "yog3";

    /**
     * Name of the grid column that contains the fourth YOG values.
     */
    public static final String FIELD_YOG4 = "yog4";

    /**
     * Name of the grid column that contains the YOG values not already in 1 through 4.
     */
    public static final String FIELD_YOG_OTHER = "other";

    /**
     * Name for the "selection" report parameter. The value is a String.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the "selection value" report parameter. The value is a String.
     */
    public static final String QUERY_STRING_PARAM = "queryString";

    private ScheduleReportHelper m_reportHelper;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        ReportDataGrid grid = new ReportDataGrid(100, 6);

        if (m_reportHelper.getSchedule() != null) {
            /*
             * Builds a term numbers map. The key of the map is a termOid , the value of the map
             * is the collection term numbers the key has covered.
             */
            HashMap termNumberMap = new HashMap();
            Iterator termIt = m_reportHelper.getSchedule().getScheduleTerms(getBroker()).iterator();
            while (termIt.hasNext()) {
                ScheduleTerm term = (ScheduleTerm) termIt.next();
                termNumberMap.put(term.getOid(), term.countUsedTermNumbers());
            }

            /*
             * Builds a day convert map. The key of the map is a dayOid, the value of the map is the
             * day object corresponding to the key.
             */
            HashMap dayConvertMap = new HashMap();
            Iterator dayIt = m_reportHelper.getSchedule().getScheduleDays(getBroker()).iterator();
            while (dayIt.hasNext()) {
                ScheduleDay day = (ScheduleDay) dayIt.next();
                dayConvertMap.put(day.getOid(), day);
            }

            /*
             * Builds a period convert map. The key of the map is a periodOid, the value of the map
             * is the
             * day object corresponding to the key.
             */
            HashMap periodConvertMap = new HashMap();
            Iterator perIt = m_reportHelper.getSchedule().getSchedulePeriods(getBroker()).iterator();
            while (perIt.hasNext()) {
                SchedulePeriod period = (SchedulePeriod) perIt.next();
                periodConvertMap.put(period.getOid(), period);
            }

            /*
             * Get the all the different YOG of the current school.
             */
            Criteria studentCriteria = new Criteria();
            studentCriteria.addEqualTo(SisStudent.COL_SCHOOL_OID, getSchool().getOid());
            studentCriteria.addEqualTo(SisStudent.REL_STUDENT_SCHEDULE_ATTRIBUTES + PATH_DELIMITER +
                    StudentScheduleAttributes.COL_SCHEDULE_OID, m_reportHelper.getScheduleOid());
            studentCriteria.addEqualTo(SisStudent.REL_STUDENT_SCHEDULE_ATTRIBUTES + PATH_DELIMITER +
                    StudentScheduleAttributes.COL_SCHEDULE_INCLUDE_INDICATOR, "1");

            SubQuery yogQuery = new SubQuery(SisStudent.class, SisStudent.COL_YOG, studentCriteria);
            yogQuery.setDistinct(true);
            yogQuery.addOrderBy(SisStudent.COL_YOG, true);

            HashMap yogMap = new HashMap();
            HashMap yogCountMap = new HashMap();
            Iterator yogIt = getBroker().getSubQueryCollectionByQuery(yogQuery).iterator();
            int index = 0;
            while (yogIt.hasNext()) {
                String yog = String.valueOf(((BigDecimal) yogIt.next()).intValue());
                index++;
                switch (index) {
                    case 1:
                        yogMap.put(yog, FIELD_YOG1);
                        yogCountMap.put(yog, FIELD_COUNT1);
                        break;

                    case 2:
                        yogMap.put(yog, FIELD_YOG2);
                        yogCountMap.put(yog, FIELD_COUNT2);
                        break;

                    case 3:
                        yogMap.put(yog, FIELD_YOG3);
                        yogCountMap.put(yog, FIELD_COUNT3);
                        break;

                    case 4:
                        yogMap.put(yog, FIELD_YOG4);
                        yogCountMap.put(yog, FIELD_COUNT4);
                        break;

                    default:
                        yogMap.put(" ", FIELD_YOG_OTHER);
                        yogCountMap.put(" ", FIELD_COUNT_OTHER);
                        break;
                }
            }

            /*
             * Get all the study sections of the current school.
             */
            Criteria studySectionCriteria = new Criteria();
            studySectionCriteria.addEqualTo(Section.COL_SCHEDULE_OID, m_reportHelper.getScheduleOid());
            studySectionCriteria.addEqualTo(Section.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.COL_MASTER_TYPE,
                    SchoolCourse.MASTER_TYPE_STUDY);

            String queryBy = (String) getParameter(QUERY_BY_PARAM);
            String queryString = (String) getParameter(QUERY_STRING_PARAM);

            addUserCriteria(studySectionCriteria, queryBy, queryString, null, null);

            QueryByCriteria sectionQuery = new QueryByCriteria(m_reportHelper.getSectionClass(), studySectionCriteria);
            sectionQuery.addOrderByAscending(Section.COL_COURSE_VIEW);

            QueryIterator studySectionIt = getBroker().getIteratorByQuery(sectionQuery);
            try {
                while (studySectionIt.hasNext()) {
                    X2BaseBean studySection = (X2BaseBean) studySectionIt.next();

                    /*
                     * Create the grid for the current section.
                     */
                    for (int termNumber = 1; termNumber <= m_reportHelper.getSchedule().getTerms(); termNumber++) {
                        perIt = m_reportHelper.getSchedule().getSchedulePeriods(getBroker()).iterator();
                        while (perIt.hasNext()) {
                            SchedulePeriod curPeriod = (SchedulePeriod) perIt.next();
                            dayIt = m_reportHelper.getSchedule().getScheduleDays(getBroker()).iterator();
                            while (dayIt.hasNext()) {
                                ScheduleDay curDay = (ScheduleDay) dayIt.next();

                                grid.append();
                                grid.set(FIELD_SECTION, studySection);
                                grid.set(FIELD_TERM, String.valueOf(termNumber));
                                grid.set(FIELD_PERIOD, curPeriod);
                                grid.set(FIELD_DAY, curDay);

                                Iterator it = yogMap.keySet().iterator();
                                while (it.hasNext()) {
                                    String curYog = (String) it.next();
                                    String yogField = (String) yogMap.get(curYog);
                                    String yogCountField = (String) yogCountMap.get(curYog);

                                    grid.set(yogField, curYog);
                                    grid.set(yogCountField, Integer.valueOf(0));
                                }
                            }
                        }
                    }

                    /*
                     * Get the number of students for each different term/period/day combination for
                     * the section.
                     */
                    Iterator resultIt = getStudentCountsForTermDayPeriodCombo(studySection);
                    while (resultIt.hasNext()) {
                        Object[] values = (Object[]) resultIt.next();
                        SchedulePeriod period = (SchedulePeriod) periodConvertMap.get(values[1]);
                        ScheduleDay day = (ScheduleDay) dayConvertMap.get(values[2]);
                        String yog = String.valueOf(((BigDecimal) values[3]).intValue());
                        String yogField = (String) yogMap.get(yog);
                        if (yogField == null) {
                            yogField = FIELD_YOG_OTHER;
                            yog = " ";
                        }
                        String yogCountField = (String) yogCountMap.get(yog);
                        if (yogCountField == null) {
                            yogCountField = FIELD_COUNT_OTHER;
                        }

                        int count = 0;

                        /*
                         * Different Jdbc driver returns different value type.
                         * SQL server returns integer while Mysql returns long.
                         */
                        if (values[3] instanceof Integer) {
                            count = ((Integer) values[4]).intValue();
                        } else {
                            count = (Integer.valueOf(String.valueOf(values[4]))).intValue();
                        }

                        String termOid = (String) values[0];
                        Iterator termNumberIt = ((Collection) termNumberMap.get(termOid)).iterator();
                        while (termNumberIt.hasNext()) {
                            Integer termNumber = (Integer) termNumberIt.next();

                            Map locateCriteria = new HashMap(5);
                            locateCriteria.put(FIELD_SECTION, studySection);
                            locateCriteria.put(FIELD_TERM, String.valueOf(termNumber.intValue()));
                            locateCriteria.put(FIELD_PERIOD, period);
                            locateCriteria.put(FIELD_DAY, day);
                            locateCriteria.put(yogField, yog);

                            /*
                             * Finds the existing row and update the count for the corresponding
                             * column.
                             */
                            if (grid.locate(locateCriteria)) {
                                Integer existingCount = (Integer) grid.get(yogCountField);
                                count += existingCount.intValue();
                                grid.set(yogCountField, Integer.valueOf(count));
                            }
                        }
                    }
                }
            } finally {
                studySectionIt.close();
            }

            grid.beforeTop();
        }

        return grid;
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
     * Returns the student counts by yog for each day/period/term combination.
     *
     * @param studySection X2BaseBean
     * @return Iterator
     */
    private Iterator getStudentCountsForTermDayPeriodCombo(X2BaseBean studySection) {
        Iterator resultIterator = null;

        if (studySection != null) {
            if (studySection.getClass().equals(MasterSchedule.class)) {
                X2Criteria criteria = new X2Criteria();
                criteria.addEqualTo(StudentSchedule.COL_SECTION_OID, studySection.getOid());

                String[] columns = new String[] {StudentSchedule.REL_STUDENT_SCHEDULE_TERMS + PATH_DELIMITER
                        + StudentScheduleTerm.COL_SCHEDULE_TERM_OID,
                        StudentSchedule.REL_STUDENT_SCHEDULE_TERMS + PATH_DELIMITER
                                + StudentScheduleTerm.REL_STUDENT_SCHEDULE_MATRICES + PATH_DELIMITER +
                                StudentScheduleMatrix.REL_SCHEDULE_MATRIX + PATH_DELIMITER
                                + ScheduleMatrix.COL_SCHEDULE_PERIOD_OID,
                        StudentSchedule.REL_STUDENT_SCHEDULE_TERMS + PATH_DELIMITER
                                + StudentScheduleTerm.REL_STUDENT_SCHEDULE_MATRICES + PATH_DELIMITER +
                                StudentScheduleMatrix.REL_SCHEDULE_MATRIX + PATH_DELIMITER
                                + ScheduleMatrix.COL_SCHEDULE_DAY_OID,
                        StudentSchedule.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_YOG,
                        "count(" + X2BaseBean.COL_OID + ")"};
                ReportQueryByCriteria query = new ReportQueryByCriteria(StudentSchedule.class, columns, criteria);
                query.addGroupBy(StudentSchedule.REL_STUDENT_SCHEDULE_TERMS + PATH_DELIMITER
                        + StudentScheduleTerm.COL_SCHEDULE_TERM_OID);
                query.addGroupBy(StudentSchedule.REL_STUDENT_SCHEDULE_TERMS + PATH_DELIMITER
                        + StudentScheduleTerm.REL_STUDENT_SCHEDULE_MATRICES + PATH_DELIMITER +
                        StudentScheduleMatrix.REL_SCHEDULE_MATRIX + PATH_DELIMITER
                        + ScheduleMatrix.COL_SCHEDULE_PERIOD_OID);
                query.addGroupBy(StudentSchedule.REL_STUDENT_SCHEDULE_TERMS + PATH_DELIMITER
                        + StudentScheduleTerm.REL_STUDENT_SCHEDULE_MATRICES + PATH_DELIMITER +
                        StudentScheduleMatrix.REL_SCHEDULE_MATRIX + PATH_DELIMITER
                        + ScheduleMatrix.COL_SCHEDULE_DAY_OID);
                query.addGroupBy(StudentSchedule.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_YOG);
                query.addOrderBy(StudentSchedule.REL_STUDENT_SCHEDULE_TERMS + PATH_DELIMITER
                        + StudentScheduleTerm.COL_SCHEDULE_TERM_OID, true);
                query.addOrderBy(StudentSchedule.REL_STUDENT_SCHEDULE_TERMS + PATH_DELIMITER
                        + StudentScheduleTerm.REL_STUDENT_SCHEDULE_MATRICES + PATH_DELIMITER +
                        StudentScheduleMatrix.REL_SCHEDULE_MATRIX + PATH_DELIMITER
                        + ScheduleMatrix.COL_SCHEDULE_PERIOD_OID, true);
                query.addOrderBy(StudentSchedule.REL_STUDENT_SCHEDULE_TERMS + PATH_DELIMITER
                        + StudentScheduleTerm.REL_STUDENT_SCHEDULE_MATRICES + PATH_DELIMITER +
                        StudentScheduleMatrix.REL_SCHEDULE_MATRIX + PATH_DELIMITER
                        + ScheduleMatrix.COL_SCHEDULE_DAY_OID, true);
                query.addOrderBy(StudentSchedule.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_YOG, true);

                resultIterator = getBroker().getReportQueryIteratorByQuery(query);
            } else if (studySection.getClass().equals(BuildMasterSchedule.class)) {
                Collection studentCounts = new LinkedList();

                X2Criteria criteria = new X2Criteria();
                criteria.addEqualTo(BuildStudentSchedule.COL_SECTION_OID, studySection.getOid());

                QueryByCriteria studentScheduleQuery = new QueryByCriteria(BuildStudentSchedule.class, criteria);
                studentScheduleQuery.addOrderBy(StudentSchedule.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_YOG,
                        true);

                HashMap studentScheduleMap = new HashMap();
                QueryIterator studentScheduleIterator = getBroker().getIteratorByQuery(studentScheduleQuery);
                try {
                    while (studentScheduleIterator.hasNext()) {
                        BuildStudentSchedule studentSchedule = (BuildStudentSchedule) studentScheduleIterator.next();
                        ScheduleMap map = studentSchedule.getScheduleMap();

                        if (map != null) {
                            studentScheduleMap.put(studentSchedule.getStudent(), map);
                        }
                    }
                } finally {
                    studentScheduleIterator.close();
                }

                Iterator termIterator = m_reportHelper.getSchedule().getScheduleTerms().iterator();
                while (termIterator.hasNext()) {
                    ScheduleTerm term = (ScheduleTerm) termIterator.next();

                    Iterator periodIterator = m_reportHelper.getSchedule().getSchedulePeriods().iterator();
                    while (periodIterator.hasNext()) {
                        SchedulePeriod period = (SchedulePeriod) periodIterator.next();

                        Iterator dayIterator = m_reportHelper.getSchedule().getScheduleDays().iterator();
                        while (dayIterator.hasNext()) {
                            ScheduleDay day = (ScheduleDay) dayIterator.next();

                            HashMap yogCountMap = new HashMap();
                            Iterator studentIterator = studentScheduleMap.keySet().iterator();
                            while (studentIterator.hasNext()) {
                                SisStudent student = (SisStudent) studentIterator.next();
                                ScheduleMap map = (ScheduleMap) studentScheduleMap.get(student);

                                if (map.isScheduled(term.getBaseTermMap(), day.getNumber(), period.getNumber())) {
                                    Integer yogCount = (Integer) yogCountMap.get(new BigDecimal(student.getYog()));
                                    if (yogCount == null) {
                                        yogCount = Integer.valueOf(0);
                                    }
                                    yogCountMap.put(new BigDecimal(student.getYog()),
                                            Integer.valueOf(yogCount.intValue() + 1));
                                }
                            }

                            Iterator yogIterator = yogCountMap.keySet().iterator();
                            while (yogIterator.hasNext()) {
                                BigDecimal yog = (BigDecimal) yogIterator.next();
                                Integer count = (Integer) yogCountMap.get(yog);

                                Object[] termDayPeriodCount = new Object[5];
                                termDayPeriodCount[0] = term.getOid();
                                termDayPeriodCount[1] = period.getOid();
                                termDayPeriodCount[2] = day.getOid();
                                termDayPeriodCount[3] = yog;
                                termDayPeriodCount[4] = count;

                                studentCounts.add(termDayPeriodCount);
                            }
                        }
                    }
                }

                resultIterator = studentCounts.iterator();
            }
        }

        return resultIterator;
    }
}
