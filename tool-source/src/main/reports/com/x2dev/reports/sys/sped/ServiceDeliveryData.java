/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2009 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.reports.sys.sped;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.CoreCalendarManager;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepPlacement;
import com.x2dev.sis.model.beans.IepPlacementProgram;
import com.x2dev.sis.model.beans.IepService;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.business.sped.IepHistory;
import com.x2dev.sis.model.business.sped.IepHistory.LightIep;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedList;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Collects the data needed for the Service Delivery report. Input needed is a start date, end date,
 * optional placement program, and optional case manager. Using that information, iep services in
 * effect in that date range are collected. The services are added to the grid along with parameters
 * calculating some statistics.
 *
 * @author X2 Development Corporation
 */
public class ServiceDeliveryData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * An optional parameter specifying a specific case manager to filter the report on
     */
    public static final String CASE_MANAGER_PARAM = "caseManager";

    /**
     * End date parameter specifying the end range which the report will return results for
     */
    public static final String END_DATE_PARAM = "end_date";

    /**
     * A parameter specifying if to exclude consultation services from the report
     */
    public static final String EXCLUDE_CONSULT_PARAM = "excludeConsult";

    /**
     * A parameter specifying if students should be grouped by their respective service provider
     */
    public static final String GROUP_BY_PROVIDER_PARAM = "groupByServProv";

    /**
     * An optional parameter specifying a specific placement program to filter the report on
     */
    public static final String PLACEMENT_PARAM = "placement";

    /**
     * A parameter passed to iReport specifying whether or not an individual provider was selected
     */
    public static final String PROVIDER_SPECIFIED = "providerSpecified";

    /**
     * School OID parameter specifying which school the services should be listed for
     */
    public static final String SCHOOL_OID_PARAM = "schoolOid";

    /**
     * Start service provider parameter specifying the name of the service provider
     */
    public static final String SERVICE_PROVIDER_PARAM = "serviceProvider";

    /**
     * Start date parameter specifying the start range which the report will return results for
     */
    public static final String START_DATE_PARAM = "start_date";

    /**
     * A parameter passed to iReport specifying whether or not the data should be grouped by
     * providers
     */
    public static final String SORT_BY_PROVIDER = "sortByProvider";

    /**
     * A parameter passed to iReport specifying whether or not the data should be sorted by
     * grade level
     */
    public static final String SORT_BY_GRADE_LEVEL = "sortByGradeLevel";

    // Constants
    private static final String ED_ENV = "edEnv";
    private static final String GRADE_LEVEL = "grade_level";
    private static final String IEP_DATA = "iepData";
    private static final String IPP = "IPP";
    private static final String NAME_VIEW = "name_view";
    private static final String PERSON = "person";
    private static final String SERVICE = "service";
    private static final String STAFF = "staff";
    private static final String START_DATE = "start_date";
    private static final String STUDENT = "student";
    private static final String TOTAL_SESSION_DAYS = "totalSessionDays";
    private static final String TOTAL_MINUTES = "totalMinutes";
    private static final String TOTAL_WEEKDAYS = "total_weekdays";

    // Database Field constant (TODO: make this a system preference)
    private static String CONSULTATION_FIELD = "Consultation";

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        IepHistory iepHistory = new IepHistory(getBroker(), getOrganization());

        Collection<IepService> services = getServicesInEffect(iepHistory);
        ReportDataGrid grid = addToGrid(services, iepHistory);

        // sort order
        if (((Boolean) getParameter(GROUP_BY_PROVIDER_PARAM)).booleanValue()) {
            grid.sort(new ArrayList<String>() {
                /**
                    *
                    */
                private static final long serialVersionUID = 1L;

                {
                    add(STAFF);
                    if (((Boolean) getParameter(SORT_BY_GRADE_LEVEL)).booleanValue()) {
                        add(GRADE_LEVEL);
                    }
                    add(NAME_VIEW);
                    add(START_DATE);
                }
            }, false);
            addParameter(SORT_BY_PROVIDER, Boolean.valueOf(true));
        } else {
            grid.sort(new ArrayList<String>() {
                /**
                    *
                    */
                private static final long serialVersionUID = 1L;

                {
                    if (((Boolean) getParameter(SORT_BY_GRADE_LEVEL)).booleanValue()) {
                        add(GRADE_LEVEL);
                    }
                    add(NAME_VIEW);
                    add(START_DATE);
                }
            }, false);
            addParameter(SORT_BY_PROVIDER, Boolean.valueOf(false));
        }
        grid.beforeTop();

        // used in student total time spent per date range calculation
        addParameter(TOTAL_WEEKDAYS,
                Integer.valueOf(
                        DateUtils.countWeekdays(
                                (PlainDate) getParameter(START_DATE_PARAM),
                                (PlainDate) getParameter(END_DATE_PARAM))));

        return grid;
    }

    /**
     * Creates the data grid which holds the data the report will reference when making the report.
     *
     * @param services Collection<IepService>
     * @param iepHistory IepHistory
     * @return ReportDataGrid
     */
    private ReportDataGrid addToGrid(Collection<IepService> services, IepHistory iepHistory) {
        ReportDataGrid grid = new ReportDataGrid(services.size(), 11);

        Collection<PlainDate> districtSessionDates =
                CoreCalendarManager.getDistrictInSessionDates((PlainDate) getParameter(START_DATE_PARAM),
                        (PlainDate) getParameter(END_DATE_PARAM),
                        getOrganization(),
                        getBroker());

        String lastStdOid = null;
        Collection<PlainDate> sessionDates = null;

        for (IepService service : services) {
            grid.append();

            SisStudent student = service.getStudent();
            SisPerson person = student.getPerson();
            IepData iepData = service.getIepData();

            LightIep lightIep = iepHistory.getEffectiveIep(student, service.getStartDate());
            if (lightIep != null) {
                IepPlacementProgram ipp = lightIep.getPlacementProgramAsOf(service.getStartDate());
                grid.set(IPP, ipp);
            }

            SisStaff staff = service.getStaff();
            String staffName = (staff != null) ? staff.getNameView() : null;

            PlainDate startDate = service.getStartDate();
            String startDateString = (startDate != null) ? startDate.toString() : "";

            grid.set(STUDENT, student);
            grid.set(GRADE_LEVEL, student.getGradeLevel());
            grid.set(NAME_VIEW, student.getNameView());
            grid.set(STAFF, staffName);
            grid.set(PERSON, person);
            grid.set(IEP_DATA, iepData);
            grid.set(SERVICE, service);
            grid.set(ED_ENV, formatEdEnv(student));
            grid.set(START_DATE, startDateString);

            // Each time we get to a new student, the sessionDates Collection needs to be refreshed.
            // This is because each student can be in a different school with different days in
            // session
            if (lastStdOid == null || !lastStdOid.equals(student.getOid())) {
                sessionDates = CoreCalendarManager.getInSessionDates((PlainDate) getParameter(START_DATE_PARAM),
                        (PlainDate) getParameter(END_DATE_PARAM),
                        student,
                        getBroker());

                /*
                 * CalendarManager.getInSessionDates() only works with active students and student's
                 * whose current school is the school they were in for the date range specified. If
                 * no results were found, default to the district calendar in session days.
                 *
                 * Another approach would be to use the student's enrollment history to get the
                 * school they
                 * were in for each date in the date range. Then use that school's calendar to see
                 * if
                 * that day was in session. We chose not to do this, as we are just estimating
                 * averages
                 * and believe this accuracy would suffice.
                 *
                 * Another know limitation is that the averages will incorrectly calculate if a date
                 * range
                 * is specified which contains no days in session for the school. In this case, we
                 * fall back
                 * on the district calendar even though the school may legitimately not have any
                 * days
                 * in session. Fall back is needed in cases where the student is no longer in a
                 * school
                 * with session days (history school). With the current design, it is very difficult
                 * to know what school the student was in.
                 */
                if (sessionDates.isEmpty()) {
                    sessionDates = districtSessionDates;
                }

                lastStdOid = student.getOid();
            }

            grid.set(TOTAL_SESSION_DAYS, Integer.valueOf(sessionDates.size()));

            int serviceDays = serviceDaysInSession(service.getStartDate(), service.getEndDate(), sessionDates);

            double cycle = service.getDaysPerCycle();
            int duration = service.getDuration();
            BigDecimal frequency = service.getFrequency();

            double totalMinutes = 0.0;

            if (frequency != null && cycle != 0.0) {
                totalMinutes = serviceDays * ((frequency.doubleValue() * duration) / cycle);
            }

            grid.set(TOTAL_MINUTES, Double.valueOf(totalMinutes));
        }

        return grid;
    }

    /**
     * Formats the Educational Environment column for the report. The data is taken from the
     * DOE 32 & DOE 34 aliased fields.
     *
     * @param student SisStudent
     * @return String
     */
    private String formatEdEnv(SisStudent student) {
        String edEnv = "";
        String value = (String) student.getFieldValueByAlias("DOE 32");

        if (!StringUtils.isEmpty(value)) {
            edEnv += value + "(3-5)";
        }

        value = (String) student.getFieldValueByAlias("DOE 34");

        if (!StringUtils.isEmpty(value)) {
            if (!StringUtils.isEmpty(edEnv)) {
                edEnv += " : ";
            }

            edEnv += value + "(6-21)";
        }

        return edEnv;
    }

    /**
     * Gets all IepService objects whose date range at some point falls in between the start
     * and end date parameters chosen by the user. If a placement program or case manager parameter
     * was chosen, then in addition to filtering on the date range, we will filter on records which
     * match the parameter chosen.
     *
     * @return QueryIterator
     */
    private QueryIterator getServices() {
        /*
         * select *
         * from IepService, IepData, IepPlacementProgram
         * where isvStartDate <= endDate &&
         * isvEndDate >= startDate &&
         * ippOid = placement &&
         * iepStfOid = caseManager
         * order by iepStudentOid
         */
        X2Criteria criteria = new X2Criteria();
        criteria.addLessOrEqualThan(IepService.COL_START_DATE, getParameter(END_DATE_PARAM));
        criteria.addGreaterOrEqualThan(IepService.COL_END_DATE, getParameter(START_DATE_PARAM));

        String schoolOid = (String) getParameter(SCHOOL_OID_PARAM);

        if (!StringUtils.isEmpty(schoolOid)) {
            criteria.addEqualTo(IepService.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_SCHOOL_OID, schoolOid);
        } else {
            X2Criteria studentOrgCriteria = getOrganizationCriteria(SisStudent.class);
            criteria.addIn(IepService.COL_STUDENT_OID,
                    new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentOrgCriteria));
        }

        if (!StringUtils.isEmpty((String) getParameter(CASE_MANAGER_PARAM))) {
            criteria.addEqualTo(IepService.REL_IEP_DATA + PATH_DELIMITER + IepData.COL_STAFF_OID,
                    getParameter(CASE_MANAGER_PARAM));
        }

        if (!StringUtils.isEmpty((String) getParameter(PLACEMENT_PARAM))) {
            criteria.addEqualTo(IepService.REL_IEP_DATA + PATH_DELIMITER +
                    IepData.REL_PLACEMENTS + PATH_DELIMITER +
                    IepPlacement.COL_IEP_PLACEMENT_PROGRAM_OID,
                    getParameter(PLACEMENT_PARAM));
        }

        if (((Boolean) getParameter(EXCLUDE_CONSULT_PARAM)).booleanValue()) {
            criteria.addNotEqualTo(IepService.COL_SERVICE_MODE, CONSULTATION_FIELD);
        }

        if (!StringUtils.isEmpty((String) getParameter(SERVICE_PROVIDER_PARAM))) {
            criteria.addEqualTo(IepService.COL_STAFF_OID, getParameter(SERVICE_PROVIDER_PARAM));
            addParameter(PROVIDER_SPECIFIED, Boolean.valueOf(true));
        }

        QueryByCriteria query = new QueryByCriteria(IepService.class, criteria);

        return getBroker().getIteratorByQuery(query);
    }

    /**
     * Takes a list of IepService objects, and returns a collection of the ones that were active
     * at any point in the date range parameters set by the user.
     *
     * @param history IepHistory
     * @return Collection<IepService>
     */
    private Collection<IepService> getServicesInEffect(IepHistory history) {
        LinkedList<IepService> list = new LinkedList<IepService>();

        QueryIterator services = getServices();

        try {
            while (services.hasNext()) {
                IepService service = (IepService) services.next();

                if (history.wasInEffect(service.getIepData(),
                        (PlainDate) getParameter(START_DATE_PARAM),
                        (PlainDate) getParameter(END_DATE_PARAM))) {
                    list.add(service);
                }
            }
        } finally {
            services.close();
        }

        return list;
    }

    /**
     * Counts the number of dates in the collection which fall within the date range provided.
     *
     * @param start PlainDate
     * @param end PlainDate
     * @param sessionDates Collection<PlainDate>
     * @return int
     */
    private int serviceDaysInSession(PlainDate start, PlainDate end, Collection<PlainDate> sessionDates) {
        int datesInRange = 0;

        while (!start.after(end)) {
            if (sessionDates.contains(start)) {
                datesInRange++;
            }

            start = DateUtils.add(start, 1);
        }

        return datesInRange;
    }
}
