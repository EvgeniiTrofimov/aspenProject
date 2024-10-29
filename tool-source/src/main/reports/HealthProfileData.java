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

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.StudentAlert;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.HealthCondition;
import com.x2dev.sis.model.beans.HealthImmunizationDefinition;
import com.x2dev.sis.model.beans.HealthImmunizationDose;
import com.x2dev.sis.model.beans.HealthImmunizationSeries;
import com.x2dev.sis.model.beans.HealthLog;
import com.x2dev.sis.model.beans.HealthMedicationOrder;
import com.x2dev.sis.model.beans.HealthScreening;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.types.PlainDate;
import java.text.DateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares the data for the "Health Profile" report. This report lists out students' basic
 * demographic information along with user-selected health information (alerts, conditions,
 * immunizations, screenings, or visits).
 *
 * @author X2 Development Corporation
 */
public class HealthProfileData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Name for the "active only" report parameter. This value is an Boolean.
     */
    public static final String ACTIVE_ONLY_PARAM = "activeOnly";

    /**
     * Value for the "alert" input parameter. This value is a Boolean.
     */
    public static final String ALERT_PARAM = "alert";

    /**
     * Value for the "condition" input parameter. This value is a Boolean.
     */
    public static final String CONDITION_PARAM = "condition";

    /**
     * Value for the "end date" input parameter. This value is a PlainDate.
     */
    public static final String END_DATE_PARAM = "endDate";

    /**
     * Value for the "immunization" input parameter. This value is a Boolean.
     */
    public static final String IMMUNIZATION_PARAM = "immunization";

    /**
     * Value for the "medication" input parameter. This value is a Boolean.
     */
    public static final String MEDICATION_PARAM = "medication";

    /**
     * Value for the "Query By" input parameter. This value is an Integer.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Value for the "Query String" input parameter. This value is a String.
     */
    public static final String QUERY_STRING_PARAM = "queryString";

    /**
     * Value for the "screening" input parameter. This value is a Boolean.
     */
    public static final String SCREENING_PARAM = "screening";

    /**
     * Name for the "secondary students" report parameter. The value is a Boolean.
     */
    public static final String SECONDARY_STUDENT_PARAM = "secondaryStudent";

    /**
     * Value for the "sort" input parameter. This value is an Integer.
     */
    public static final String SORT_PARAM = "sort";

    /**
     * Value for the "start date" input parameter. This value is a PlainDate.
     */
    public static final String START_DATE_PARAM = "startDate";

    /**
     * Value for the "visit" input parameter. This value is a Boolean.
     */
    public static final String VISIT_PARAM = "visit";

    /*
     * Grid fields
     */
    private static final String FIELD_COMMENT = "comment";
    private static final String FIELD_DATE = "date";
    private static final String FIELD_NAME = "name";
    private static final String FIELD_STATUS = "status";
    private static final String FIELD_STUDENT = "student";
    private static final String FIELD_TYPE = "type";

    /*
     * Report constants
     */
    private static final String TYPE_ALERT = "Alert";
    private static final String TYPE_CONDITION = "Condition";
    private static final String TYPE_IMMUNE = "Immune";
    private static final String TYPE_MEDICATION = "Medication";
    private static final String TYPE_SCREEN = "Screen";
    private static final String TYPE_VISIT = "Visit";

    private SisStudent m_currentStudent;
    private PlainDate m_endDate;
    private PlainDate m_startDate;

    private Map<String, Collection<StudentAlert>> m_alertMap;
    private Map<String, Collection<HealthCondition>> m_conditionMap;
    private Map<String, Collection<HealthImmunizationDose>> m_immuneMap;
    private Map<String, Collection<HealthMedicationOrder>> m_medicationMap;
    private Map<String, Collection<HealthScreening>> m_screeningMap;
    private Map<String, Collection<HealthLog>> m_visitMap;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        m_startDate = (PlainDate) getParameter(START_DATE_PARAM);
        m_endDate = (PlainDate) getParameter(END_DATE_PARAM);

        Criteria studentCriteria = new Criteria();

        if (m_currentStudent != null) {
            studentCriteria.addEqualTo(X2BaseBean.COL_OID, m_currentStudent.getOid());
        } else {
            String queryBy = (String) getParameter(QUERY_BY_PARAM);
            String queryString = (String) getParameter(QUERY_STRING_PARAM);
            addUserCriteria(studentCriteria, queryBy, queryString, SisStudent.class, X2BaseBean.COL_OID);

            /*
             * Active only students (if current selection was not chosen)
             */
            boolean activeOnly = ((Boolean) getParameter(ACTIVE_ONLY_PARAM)).booleanValue();
            if (activeOnly && !queryBy.contains(CURRENT_KEY)) {
                studentCriteria.addAndCriteria(StudentManager.getActiveStudentStatusCriteria(getOrganization(),
                        SisStudent.COL_ENROLLMENT_STATUS));
            }

            /*
             * Get records for current school only (if current selection was not chosen)
             */
            if (isSchoolContext() && !queryBy.contains(CURRENT_KEY)) {
                studentCriteria.addEqualTo(SisStudent.COL_SCHOOL_OID, getSchool().getOid());

                /*
                 * Include secondary students of the school if needed.
                 */
                if (((Boolean) getParameter(SECONDARY_STUDENT_PARAM)).booleanValue()) {
                    studentCriteria.addOrCriteria(StudentManager.buildSecondaryStudentCriteria(getSchool()));
                }
            } else if (!queryBy.contains(CURRENT_KEY)) {
                studentCriteria.addAndCriteria(getOrganizationCriteria(SisStudent.class));
            }
        }

        boolean alert = ((Boolean) getParameter(ALERT_PARAM)).booleanValue();
        boolean condition = ((Boolean) getParameter(CONDITION_PARAM)).booleanValue();
        boolean immunization = ((Boolean) getParameter(IMMUNIZATION_PARAM)).booleanValue();
        boolean medication = ((Boolean) getParameter(MEDICATION_PARAM)).booleanValue();
        boolean screening = ((Boolean) getParameter(SCREENING_PARAM)).booleanValue();
        boolean visit = ((Boolean) getParameter(VISIT_PARAM)).booleanValue();

        SubQuery studentSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria);
        loadHealthInformation(studentSubQuery, alert, condition, immunization, medication, screening, visit);

        /*
         * Build and sort the query
         */
        QueryByCriteria studentQuery = new QueryByCriteria(SisStudent.class, studentCriteria);
        applyUserSort(studentQuery, (String) getParameter(SORT_PARAM));

        ReportDataGrid grid = new ReportDataGrid(6);
        QueryIterator iterator = getBroker().getIteratorByQuery(studentQuery);
        try {
            while (iterator.hasNext()) {
                SisStudent student = (SisStudent) iterator.next();

                if (immunization) {
                    grid.append(getImmunizationRecords(student));
                }

                if (screening) {
                    grid.append(getScreeningRecords(student));
                }

                if (medication) {
                    grid.append(getMedicationRecords(student));
                }

                if (condition) {
                    grid.append(getConditionRecords(student));
                }

                if (visit) {
                    grid.append(getVisitRecords(student));
                }

                if (alert) {
                    grid.append(getAlertRecords(student));
                }
            }
        } finally {
            iterator.close();
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
        m_currentStudent = userData.getCurrentRecord(SisStudent.class);
    }

    /**
     * Formats two dates "startDate to endDate".
     *
     * @param startDate PlainDate
     * @param endDate PlainDate
     * @return String
     */
    private String formatRange(PlainDate startDate, PlainDate endDate) {
        DateFormat format = DateFormat.getDateInstance(DateFormat.SHORT, getLocale());
        String range = "";

        if (startDate != null && endDate != null) {
            range = format.format(startDate) + " - " + format.format(endDate);
        }

        return range;
    }

    /**
     * Builds a grid of StudentAlert records.
     *
     * @param student SisStudent
     * @return ReportDataGrid
     */
    private ReportDataGrid getAlertRecords(SisStudent student) {
        ReportDataGrid grid = new ReportDataGrid(6);

        Collection<StudentAlert> alerts = m_alertMap.get(student.getOid());
        if (alerts == null) {
            alerts = new ArrayList(0);
        }

        for (StudentAlert alert : alerts) {
            grid.append();
            grid.set(FIELD_STUDENT, student);
            grid.set(FIELD_TYPE, TYPE_ALERT);

            if (alert.getAlertDescription() != null) {
                grid.set(FIELD_COMMENT, alert.getAlertDescription());
            }
        }

        return grid;
    }

    /**
     * Builds a grid of Health Condition records.
     *
     * @param student SisStudent
     * @return ReportDataGrid
     */
    private ReportDataGrid getConditionRecords(SisStudent student) {
        ReportDataGrid grid = new ReportDataGrid(6);

        Collection<HealthCondition> conditions = m_conditionMap.get(student.getOid());
        if (conditions == null) {
            conditions = new ArrayList(0);
        }

        for (HealthCondition condition : conditions) {
            grid.append();
            grid.set(FIELD_STUDENT, student);
            grid.set(FIELD_TYPE, TYPE_CONDITION);
            grid.set(FIELD_NAME, condition.getConditionType());
            grid.set(FIELD_STATUS, condition.getConditionCode());

            if (condition.getComment() != null) {
                grid.set(FIELD_COMMENT, condition.getComment());
            }
        }

        return grid;
    }

    /**
     * Builds a grid of HealthImmuniziationDose records.
     *
     * @param student SisStudent
     * @return ReportDataGrid
     */
    private ReportDataGrid getImmunizationRecords(SisStudent student) {
        ReportDataGrid grid = new ReportDataGrid(6);

        Collection<HealthImmunizationDose> immunities = m_immuneMap.get(student.getOid());
        if (immunities == null) {
            immunities = new ArrayList(0);
        }

        for (HealthImmunizationDose dose : immunities) {
            grid.append();
            grid.set(FIELD_STUDENT, student);
            grid.set(FIELD_TYPE, TYPE_IMMUNE);
            grid.set(FIELD_DATE, dose.getDate());
            grid.set(FIELD_NAME, dose.getImmunizationSeries().getImmunizationDefinition().getSeriesId());

            if (dose.getComment() != null) {
                grid.set(FIELD_COMMENT, dose.getComment());
            }
        }

        return grid;
    }

    /**
     * Builds a grid of HealthMedicationOrder records.
     *
     * @param student SisStudent
     * @return ReportDataGrid
     */
    private ReportDataGrid getMedicationRecords(SisStudent student) {
        ReportDataGrid grid = new ReportDataGrid(6);

        Collection<HealthMedicationOrder> medications = m_medicationMap.get(student.getOid());
        if (medications == null) {
            medications = new ArrayList(0);
        }

        for (HealthMedicationOrder medication : medications) {
            grid.append();
            grid.set(FIELD_STUDENT, student);
            grid.set(FIELD_TYPE, TYPE_MEDICATION);
            grid.set(FIELD_NAME, medication.getMedicationName());
            grid.set(FIELD_STATUS, formatRange(medication.getStartDate(), medication.getStopDate()));

            if (medication.getComment() != null) {
                grid.set(FIELD_COMMENT, medication.getComment());
            }
        }

        return grid;
    }

    /**
     * Builds a grid of HealthScreening records.
     *
     * @param student SisStudent
     * @return ReportDataGrid
     */
    private ReportDataGrid getScreeningRecords(SisStudent student) {
        ReportDataGrid grid = new ReportDataGrid(6);

        Collection<HealthScreening> screenings = m_screeningMap.get(student.getOid());
        if (screenings == null) {
            screenings = new ArrayList(0);
        }

        for (HealthScreening screen : screenings) {
            grid.append();
            grid.set(FIELD_STUDENT, student);
            grid.set(FIELD_TYPE, TYPE_SCREEN);
            grid.set(FIELD_DATE, screen.getDate());
            grid.set(FIELD_NAME, screen.getExtendedDataDictionary().getName());
            grid.set(FIELD_STATUS, screen.getResultCode());

            if (screen.getComment() != null) {
                grid.set(FIELD_COMMENT, screen.getComment());
            }
        }

        return grid;
    }

    /**
     * Builds a grid of HealthLog records.
     *
     * @param student SisStudent
     * @return ReportDataGrid
     */
    private ReportDataGrid getVisitRecords(SisStudent student) {
        ReportDataGrid grid = new ReportDataGrid(6);

        Collection<HealthLog> visits = m_visitMap.get(student.getPersonOid());
        if (visits != null) {
            for (HealthLog visit : visits) {
                grid.append();
                grid.set(FIELD_STUDENT, student);
                grid.set(FIELD_TYPE, TYPE_VISIT);
                grid.set(FIELD_DATE, visit.getDate());
                grid.set(FIELD_NAME, visit.getPrimaryComplaintCode());
                grid.set(FIELD_STATUS, visit.getPrimaryTreatmentCode());

                if (visit.getNotes() != null) {
                    grid.set(FIELD_COMMENT, visit.getNotes());
                }
            }
        }

        return grid;
    }

    /**
     * Load Maps of health information keyed to Student OIDs.
     * <ul>
     * <li>StudentAlert</li>
     * <li>HealthCondition</li>
     * <li>HealthImmunizationDose</li>
     * <li>HealthMedicationOrder</li>
     * <li>HealthScreening</li>
     * <li>HealthLog</li>
     * </ul>
     *
     * @param studentSubQuery SubQuery
     * @param alert boolean
     * @param condition boolean
     * @param immune boolean
     * @param medication boolean
     * @param screen boolean
     * @param visit boolean
     */
    private void loadHealthInformation(SubQuery studentSubQuery,
                                       boolean alert,
                                       boolean condition,
                                       boolean immune,
                                       boolean medication,
                                       boolean screen,
                                       boolean visit) {
        Criteria criteria;
        QueryByCriteria query;

        /*
         * Alerts
         */
        if (alert) {
            criteria = new Criteria();
            criteria.addIn(StudentAlert.COL_STUDENT_OID, studentSubQuery);
            criteria.addEqualTo(StudentAlert.COL_ALERT_TYPE, Integer.valueOf(StudentAlert.AlertType.MEDICAL.ordinal()));

            query = new QueryByCriteria(StudentAlert.class, criteria);
            query.addOrderByAscending(StudentAlert.COL_STUDENT_OID);
            query.addOrderByAscending(StudentAlert.COL_ALERT_TYPE);

            m_alertMap = getBroker().getGroupedCollectionByQuery(query,
                    StudentAlert.COL_STUDENT_OID, 5000);
        }

        /*
         * Conditions
         */
        if (condition) {
            criteria = new Criteria();
            criteria.addIn(HealthCondition.COL_STUDENT_OID, studentSubQuery);

            query = new QueryByCriteria(HealthCondition.class, criteria);
            query.addOrderByAscending(HealthCondition.COL_STUDENT_OID);
            query.addOrderByAscending(HealthCondition.COL_CONDITION_CODE);
            query.addOrderByAscending(HealthCondition.COL_CONDITION_TYPE);

            m_conditionMap = getBroker().getGroupedCollectionByQuery(query,
                    HealthCondition.COL_STUDENT_OID, 5000);
        }

        /*
         * Immunizations
         */
        if (immune) {
            criteria = new Criteria();
            criteria.addIn(HealthImmunizationDose.COL_STUDENT_OID, studentSubQuery);
            criteria.addGreaterOrEqualThan(HealthImmunizationDose.COL_DATE, m_startDate);
            criteria.addLessOrEqualThan(HealthImmunizationDose.COL_DATE, m_endDate);

            query = new QueryByCriteria(HealthImmunizationDose.class, criteria);
            query.addOrderByAscending(HealthImmunizationDose.COL_STUDENT_OID);
            query.addOrderByAscending(HealthImmunizationDose.REL_IMMUNIZATION_SERIES + PATH_DELIMITER +
                    HealthImmunizationSeries.REL_IMMUNIZATION_DEFINITION + PATH_DELIMITER +
                    HealthImmunizationDefinition.COL_SERIES_ID);
            query.addOrderByAscending(HealthImmunizationDose.COL_DATE);

            m_immuneMap = getBroker().getGroupedCollectionByQuery(query,
                    HealthImmunizationDose.COL_STUDENT_OID, 5000);
        }

        /*
         * Medications
         */
        if (medication) {
            criteria = new Criteria();
            criteria.addIn(HealthMedicationOrder.COL_STUDENT_OID, studentSubQuery);

            query = new QueryByCriteria(HealthMedicationOrder.class, criteria);
            query.addOrderByAscending(HealthMedicationOrder.COL_STUDENT_OID);
            query.addOrderByAscending(HealthMedicationOrder.COL_START_DATE);

            m_medicationMap = getBroker().getGroupedCollectionByQuery(query,
                    HealthMedicationOrder.COL_STUDENT_OID, 5000);
        }

        /*
         * Screenings
         */
        if (screen) {
            criteria = new Criteria();
            criteria.addIn(HealthScreening.COL_STUDENT_OID, studentSubQuery);
            criteria.addGreaterOrEqualThan(HealthScreening.COL_DATE, m_startDate);
            criteria.addLessOrEqualThan(HealthScreening.COL_DATE, m_endDate);

            query = new QueryByCriteria(HealthScreening.class, criteria);
            query.addOrderByAscending(HealthScreening.COL_STUDENT_OID);
            query.addOrderByAscending(HealthScreening.REL_EXTENDED_DATA_DICTIONARY + PATH_DELIMITER +
                    ExtendedDataDictionary.COL_NAME);
            query.addOrderByAscending(HealthScreening.COL_DATE);

            m_screeningMap = getBroker().getGroupedCollectionByQuery(query,
                    HealthScreening.COL_STUDENT_OID, 5000);
        }

        /*
         * Visits
         */
        if (visit) {
            /*
             * Query is temp. switched to return person oid instead of student oid because
             * Health Logs are person based, not student based. However, all the other types
             * of health data that use this subquery are student based.
             */
            studentSubQuery.setAttributes(new String[] {SisStudent.COL_PERSON_OID});

            criteria = new Criteria();
            criteria.addIn(HealthLog.COL_PERSON_OID, studentSubQuery);
            criteria.addGreaterOrEqualThan(HealthLog.COL_DATE, m_startDate);
            criteria.addLessOrEqualThan(HealthLog.COL_DATE, m_endDate);

            query = new QueryByCriteria(HealthLog.class, criteria);
            query.addOrderByAscending(HealthLog.COL_PERSON_OID);
            query.addOrderByAscending(HealthLog.COL_PRIMARY_COMPLAINT_CODE);
            query.addOrderByAscending(HealthLog.COL_DATE);

            m_visitMap = getBroker().getGroupedCollectionByQuery(query,
                    HealthLog.COL_PERSON_OID, 5000);

            studentSubQuery.setAttributes(new String[] {X2BaseBean.COL_OID});
        }
    }
}
