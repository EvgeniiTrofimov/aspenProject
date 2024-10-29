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
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.HealthImmunizationDefinition;
import com.x2dev.sis.model.beans.HealthImmunizationDose;
import com.x2dev.sis.model.beans.HealthImmunizationSeries;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.SisUser;
import com.x2dev.sis.model.business.health.ImmunizationManager;
import com.x2dev.utils.StringUtils;
import java.math.BigDecimal;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Prepares the data for the SchoolImmunizationStatus report.
 *
 * These reports tallies student immunization totals for schools.
 *
 * @author X2 Development Corporation
 */
public class SchoolImmunizationStatusData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;
    /*
     * Grid fields
     */
    private static final String FIELD_DEFINITION = "definition";
    private static final String FIELD_SCHOOL = "school";

    /*
     * Report parameters
     */
    private static final String PARAM_CURRENT_USER = "person";
    private static final String PARAM_SCHOOL_TOTALS = "schoolTotals";
    private static final String PARAM_STUDENT_EXEMPT = "exemptMap";
    private static final String PARAM_STUDENT_IMMUNE = "immunityMap";
    private static final String PARAM_STUDENT_NO_IMMUNE = "noImmuneMap";

    private SisUser m_currentUser;
    private Map m_gradeLevelMap;
    private ImmunizationManager m_immuneManager;
    private Collection m_studentsExempt;
    private Collection m_studentsWith;
    private Collection m_studentsWithout;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(HealthImmunizationDefinition.COL_ORGANIZATION1_OID,
                getOrganization().getRootOrganization().getOid());
        criteria.addEqualTo(HealthImmunizationDefinition.COL_REQUIRED_INDICATOR, "1");

        QueryByCriteria query = new QueryByCriteria(HealthImmunizationDefinition.class, criteria);
        query.addOrderByAscending(HealthImmunizationDefinition.COL_SERIES_ID);

        ReportDataGrid grid = new ReportDataGrid(20, 10);
        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            loadStudents();
            loadGradeLevelMap();

            while (iterator.hasNext()) {
                HealthImmunizationDefinition definition = (HealthImmunizationDefinition) iterator.next();

                grid.append();
                grid.set(FIELD_DEFINITION, definition);
                grid.set(FIELD_SCHOOL, getSchool());

                setNeededTotals(definition, grid);
            }
        } finally {
            iterator.close();
        }

        grid.beforeTop();

        setStudentHealthInformation();
        addParameter(PARAM_SCHOOL_TOTALS, getSchoolTotals());

        if (m_currentUser != null) {
            addParameter(PARAM_CURRENT_USER, m_currentUser.getPerson());
        }

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
        m_currentUser = userData.getCurrentRecord(SisUser.class);
    }

    /**
     * Counts grade level totals of passed students.
     *
     * @param studentIterator Iterator
     * @return Map of a Map of totals keyed to column #, keyed to school oid
     */
    private Map buildSchoolCounts(Iterator studentIterator) {
        Map schoolMap = new HashMap(32);
        Map totalsMap = new HashMap(8);

        SisSchool lastSchool = null;
        while (studentIterator.hasNext()) {
            SisStudent student = (SisStudent) studentIterator.next();
            SisSchool school = student.getSchool();
            String column = (String) m_gradeLevelMap.get(student.getGradeLevel());

            if (lastSchool == null || !school.equals(lastSchool)) {
                if (lastSchool != null) {
                    schoolMap.put(lastSchool.getOid(), totalsMap);
                }

                totalsMap = new HashMap(8);
            }

            /*
             * Update totals
             */
            Integer total = (Integer) totalsMap.get(column);
            if (total == null) {
                totalsMap.put(column, Integer.valueOf(1));
            } else {
                totalsMap.put(column, Integer.valueOf(total.intValue() + 1));
            }

            lastSchool = school;
        }

        /*
         * Add last school
         */
        if (lastSchool != null) {
            schoolMap.put(lastSchool.getOid(), totalsMap);
        }

        return schoolMap;
    }

    /**
     * Returns a Map of Longs representing total active students in a school keyed to their School
     * OID.
     *
     * @return Map
     */
    private Map getSchoolTotals() {
        HashMap schoolToStudentTotals = new HashMap();

        Criteria criteria = getStudentCriteria();

        String[] columns = new String[] {SisStudent.COL_SCHOOL_OID, "count(*)"};
        ReportQueryByCriteria query = new ReportQueryByCriteria(SisStudent.class, columns, criteria);
        query.addGroupBy(SisStudent.COL_SCHOOL_OID);

        ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                Object[] row = (Object[]) iterator.next();

                String schoolOid = (String) row[0];
                Long studentCount = null;

                if (row[1] instanceof Long) {
                    studentCount = (Long) row[1];
                } else if (row[1] instanceof BigDecimal) {
                    studentCount = Long.valueOf(((BigDecimal) row[1]).longValue());
                } else {
                    studentCount = Long.valueOf(((Integer) row[1]).intValue());
                }

                schoolToStudentTotals.put(schoolOid, studentCount);
            }
        } finally {
            iterator.close();
        }

        return schoolToStudentTotals;
    }

    /**
     * Returns Criteria for students.
     *
     * @return Criteria
     */
    private Criteria getStudentCriteria() {
        Criteria studentCriteria = new Criteria();

        // Get records for current school or organization
        if (isSchoolContext()) {
            studentCriteria.addEqualTo(SisStudent.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            studentCriteria.addAndCriteria(getOrganizationCriteria(SisStudent.class));
        }

        studentCriteria.addAndCriteria(
                StudentManager.getActiveStudentStatusCriteria(getOrganization(), SisStudent.COL_ENROLLMENT_STATUS));

        return studentCriteria;
    }

    /**
     * Returns a map of the report columns keyed to the corresponding grade level.
     *
     * @return Map
     */
    private void loadGradeLevelMap() {
        m_gradeLevelMap = new HashMap(32);

        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getUser().getPersistenceKey());
        DataDictionaryField field =
                dictionary.findDataDictionaryField(SisStudent.class.getName(), SisStudent.COL_GRADE_LEVEL);

        Iterator codeIterator = field.getReferenceTable().getReferenceCodes().iterator();
        while (codeIterator.hasNext()) {
            ReferenceCode code = (ReferenceCode) codeIterator.next();

            int offset = -10000;
            if (StringUtils.isInteger(code.getFieldA005())) {
                offset = Integer.parseInt(code.getFieldA005());
            }

            switch (offset) {
                case -2: // PK
                    m_gradeLevelMap.put(code.getCode(), "0");
                    break;

                case -1: // PK
                    m_gradeLevelMap.put(code.getCode(), "0");
                    break;

                case 0: // K
                    m_gradeLevelMap.put(code.getCode(), "1");
                    break;

                case 1:
                    m_gradeLevelMap.put(code.getCode(), "2");
                    break;

                case 2:
                    m_gradeLevelMap.put(code.getCode(), "3");
                    break;

                case 3:
                    m_gradeLevelMap.put(code.getCode(), "4");
                    break;

                case 4:
                    m_gradeLevelMap.put(code.getCode(), "4");
                    break;

                case 5:
                    m_gradeLevelMap.put(code.getCode(), "4");
                    break;

                case 6:
                    m_gradeLevelMap.put(code.getCode(), "5");
                    break;

                case 7:
                    m_gradeLevelMap.put(code.getCode(), "5");
                    break;

                case 8:
                    m_gradeLevelMap.put(code.getCode(), "5");
                    break;

                case 9:
                    m_gradeLevelMap.put(code.getCode(), "6");
                    break;

                case 10:
                    m_gradeLevelMap.put(code.getCode(), "6");
                    break;

                case 11:
                    m_gradeLevelMap.put(code.getCode(), "6");
                    break;

                case 12:
                    m_gradeLevelMap.put(code.getCode(), "6");
                    break;
            }
        }
    }

    /**
     * Loads the collection of students with immunizations, without immunizations, and with
     * exemptions.
     */
    private void loadStudents() {
        /*
         * Build HealthImmunizationDose criteria
         */
        Criteria doseCriteria = new Criteria();
        doseCriteria.addEqualTo(HealthImmunizationDose.REL_IMMUNIZATION_SERIES + "." +
                HealthImmunizationSeries.REL_IMMUNIZATION_DEFINITION + "." +
                HealthImmunizationDefinition.COL_REQUIRED_INDICATOR, "1");
        SubQuery doseSub = new SubQuery(HealthImmunizationDose.class,
                HealthImmunizationDose.COL_STUDENT_OID, doseCriteria);

        /*
         * Build HealthImmunizationSeries criteria
         */
        Criteria seriesCriteria = new Criteria();
        seriesCriteria.addEqualTo(HealthImmunizationSeries.COL_WAIVED_INDICATOR, "1");
        seriesCriteria.addEqualTo(HealthImmunizationSeries.REL_IMMUNIZATION_DEFINITION + "." +
                HealthImmunizationDefinition.COL_REQUIRED_INDICATOR, "1");
        SubQuery seriesSub = new SubQuery(HealthImmunizationSeries.class,
                HealthImmunizationSeries.COL_STUDENT_OID, seriesCriteria);

        /*
         * Build Students with immunizations
         */
        Criteria withCriteria = getStudentCriteria();
        withCriteria.addIn(X2BaseBean.COL_OID, doseSub);

        QueryByCriteria withQuery = new QueryByCriteria(SisStudent.class, withCriteria);
        withQuery.addOrderByAscending(SisStudent.REL_SCHOOL + "." + SisSchool.COL_NAME);
        withQuery.addOrderByAscending(SisStudent.COL_SCHOOL_OID);
        m_studentsWith = getBroker().getCollectionByQuery(withQuery);

        /*
         * Build Students without immunizations
         */
        Criteria withoutCriteria = getStudentCriteria();
        withoutCriteria.addNotIn(X2BaseBean.COL_OID, doseSub);

        QueryByCriteria withoutQuery = new QueryByCriteria(SisStudent.class, withoutCriteria);
        withoutQuery.addOrderByAscending(SisStudent.REL_SCHOOL + "." + SisSchool.COL_NAME);
        withoutQuery.addOrderByAscending(SisStudent.COL_SCHOOL_OID);
        m_studentsWithout = getBroker().getCollectionByQuery(withoutQuery);

        /*
         * Build students with exemptions
         */
        Criteria exemptCriteria = getStudentCriteria();
        exemptCriteria.addIn(X2BaseBean.COL_OID, seriesSub);

        QueryByCriteria exemptQuery = new QueryByCriteria(SisStudent.class, exemptCriteria);
        exemptQuery.addOrderByAscending(SisStudent.REL_SCHOOL + "." + SisSchool.COL_NAME);
        exemptQuery.addOrderByAscending(SisStudent.COL_SCHOOL_OID);
        m_studentsExempt = getBroker().getCollectionByQuery(exemptQuery);
    }

    /**
     * Counts the number of students that have not received the required doses of the given
     * HealthImmunizationDefitition.
     *
     * @param definition HealthImmunizationDefinition
     * @param grid ReportDataGrid
     */
    private void setNeededTotals(HealthImmunizationDefinition definition, ReportDataGrid grid) {
        Iterator studentIterator = m_studentsWith.iterator();
        while (studentIterator.hasNext()) {
            SisStudent student = (SisStudent) studentIterator.next();
            String column = (String) m_gradeLevelMap.get(student.getGradeLevel());

            // If the student's grade level is one that is being reported....
            if (!StringUtils.isEmpty(column)) {
                m_immuneManager = new ImmunizationManager(getBroker(), student);
                List doses = m_immuneManager.getDoses(definition.getSeriesId());

                boolean waivedSeries = false;
                HealthImmunizationSeries series = m_immuneManager.getSeriesBean(definition.getSeriesId());
                if (series != null) {
                    waivedSeries = series.getWaivedIndicator();
                }

                if (doses.size() < definition.getRequiredDoseCount() && !waivedSeries) {
                    Integer count = (Integer) grid.get(column);
                    if (count == null) {
                        grid.set(column, Integer.valueOf(1));
                    } else {
                        grid.set(column, Integer.valueOf(count.intValue() + 1));
                    }
                }
            }
        }
    }

    /**
     * Counts totals of students with immunizations, without immunizations, and with exemptions.
     */
    private void setStudentHealthInformation() {
        Map studentMap;

        studentMap = buildSchoolCounts(m_studentsExempt.iterator());
        addParameter(PARAM_STUDENT_EXEMPT, studentMap);

        studentMap = buildSchoolCounts(m_studentsWith.iterator());
        addParameter(PARAM_STUDENT_IMMUNE, studentMap);

        studentMap = buildSchoolCounts(m_studentsWithout.iterator());
        addParameter(PARAM_STUDENT_NO_IMMUNE, studentMap);
    }
}
