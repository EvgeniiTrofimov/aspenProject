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
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.StudentContact;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.HealthImmunizationDefinition;
import com.x2dev.sis.model.beans.HealthImmunizationDose;
import com.x2dev.sis.model.beans.HealthImmunizationSeries;
import com.x2dev.sis.model.beans.SisStudent;
import java.util.Map;
import java.util.logging.Level;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares the data for the following health log reports:
 * These reports simply collect students health logs from the current school.
 *
 * @author X2 Development Corporation
 */
public class SchoolHealthRecordData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Value for the "active only" input parameter. This value is a Boolean.
     */
    public static final String ACTIVE_ONLY_PARAM = "activeOnly";

    /**
     * Value for the "query by" input parameter. This value is a String.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Value for the "query string" input parameter. This value is a String.
     */
    public static final String QUERY_STRING_PARAM = "queryString";

    /**
     * Value for the "required only" input parameter. This value is a Boolean.
     */
    public static final String REQUIRED_ONLY_PARAM = "requiredOnly";

    /**
     * Value for the "sort" input parameter. This value is a String.
     */
    public static final String SORT_PARAM = "sort";

    /*
     * Report parameters
     */
    private static final String PARAMETER_FATHER = "fatherMap";
    private static final String PARAMETER_GUARDIAN = "guardianMap";
    private static final String PARAMETER_MOTHER = "motherMap";
    /*
     * Grid fields
     */
    private static final String FIELD_STUDENT = "student";
    private static final String FIELD_DATE = "date";
    private static final String FIELD_SERIES = "series";

    /*
     * Alias for the birthplace field.
     */
    private static final String ALIAS_BIRTHPLACE = "birthplace";

    /*
     * Report constants
     */
    private static String FATHER = "Father";
    private static String GUARDIAN = "Guardian";
    private static String MOTHER = "Mother";
    private static int ROW_COUNT = 30;

    private Map m_fatherMap;
    private Map m_guardianMap;
    private Map m_motherMap;

    private SisStudent m_currentStudent;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        checkBirthplace();

        /*
         * Build the criteria based on user input
         */
        Criteria criteria = new Criteria();

        if (m_currentStudent != null) {
            criteria.addEqualTo(HealthImmunizationDose.COL_STUDENT_OID, m_currentStudent.getOid());
        } else {
            addUserCriteria(criteria, (String) getParameter(QUERY_BY_PARAM), (String) getParameter(QUERY_STRING_PARAM),
                    null, null);

            boolean activeOnly = ((Boolean) getParameter(ACTIVE_ONLY_PARAM)).booleanValue();
            if (activeOnly) {
                criteria.addAndCriteria(StudentManager.getActiveStudentStatusCriteria(getOrganization(),
                        HealthImmunizationDose.REL_STUDENT + PATH_DELIMITER +
                                SisStudent.COL_ENROLLMENT_STATUS));

            }

            // Get records for current school only
            if (isSchoolContext()) {
                criteria.addEqualTo(HealthImmunizationDose.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_SCHOOL_OID,
                        getSchool().getOid());
            } else {
                criteria.addAndCriteria(getOrganizationCriteria(HealthImmunizationDose.class));
            }
        }

        // Only grab required immunizations
        boolean requiredOnly = ((Boolean) getParameter(REQUIRED_ONLY_PARAM)).booleanValue();
        if (requiredOnly) {
            criteria.addEqualTo(HealthImmunizationDose.REL_IMMUNIZATION_SERIES + PATH_DELIMITER +
                    HealthImmunizationSeries.REL_IMMUNIZATION_DEFINITION + PATH_DELIMITER +
                    HealthImmunizationDefinition.COL_REQUIRED_INDICATOR, "1");
        }

        QueryByCriteria query = new QueryByCriteria(HealthImmunizationDose.class, criteria);
        applyUserSort(query, (String) getParameter(SORT_PARAM));

        // Order by series
        query.addOrderByAscending(HealthImmunizationDose.COL_STUDENT_OID);
        query.addOrderByAscending(HealthImmunizationDose.REL_IMMUNIZATION_SERIES + PATH_DELIMITER +
                HealthImmunizationSeries.REL_IMMUNIZATION_DEFINITION + PATH_DELIMITER +
                HealthImmunizationDefinition.COL_SERIES_ID);
        query.addOrderByAscending(HealthImmunizationDose.REL_IMMUNIZATION_SERIES + PATH_DELIMITER +
                HealthImmunizationSeries.COL_IMMUNIZATION_DEFINITION_OID);
        query.addOrderByAscending(HealthImmunizationDose.COL_DATE);

        ReportDataGrid grid = new ReportDataGrid(10000, 20);
        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            SisStudent lastStudent = null;
            int count = 0;

            while (iterator.hasNext()) {
                HealthImmunizationDose dose = (HealthImmunizationDose) iterator.next();
                SisStudent student = dose.getStudent();

                if (lastStudent == null || !student.equals(lastStudent)) {
                    if (lastStudent != null) {
                        padGrid(grid, count, lastStudent);
                    }

                    count = 0;
                    lastStudent = student;
                }

                grid.append();
                grid.set(FIELD_STUDENT, student);
                grid.set(FIELD_SERIES, dose.getImmunizationSeries());
                grid.set(FIELD_DATE, dose.getDate());
                count++;
            }

            if (lastStudent != null) {
                padGrid(grid, count, lastStudent);
            }
        } finally {
            iterator.close();
        }
        /*
         * Add report parameters
         */
        loadContactMaps();
        addParameter(PARAMETER_FATHER, m_fatherMap);
        addParameter(PARAMETER_GUARDIAN, m_guardianMap);
        addParameter(PARAMETER_MOTHER, m_motherMap);

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
     * Pads the passed grid to have the specified number of rows regardless of the amount of data.
     *
     * @param grid ReportDataGrid
     * @param count int
     * @param student SisStudent
     */
    private void padGrid(ReportDataGrid grid, int count, SisStudent student) {
        for (int i = count; i < ROW_COUNT; i++) {
            grid.append();
            grid.set(FIELD_STUDENT, student);
        }
    }

    /**
     * Loads maps of StudentContacts (father, mother, guardian) keyed to the student OID.
     */
    private void loadContactMaps() {
        X2Criteria motherCriteria = new X2Criteria();
        motherCriteria.addEqualToIgnoreCase(StudentContact.COL_RELATIONSHIP_CODE, MOTHER);
        motherCriteria.addEqualTo(StudentContact.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_SCHOOL_OID,
                getSchool().getOid());

        X2Criteria fatherCriteria = new X2Criteria();
        fatherCriteria.addEqualToIgnoreCase(StudentContact.COL_RELATIONSHIP_CODE, FATHER);
        fatherCriteria.addEqualTo(StudentContact.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_SCHOOL_OID,
                getSchool().getOid());

        X2Criteria guardianCriteria = new X2Criteria();
        guardianCriteria.addEqualTo(StudentContact.COL_RELATIONSHIP_CODE, GUARDIAN);
        guardianCriteria.addEqualTo(StudentContact.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_SCHOOL_OID,
                getSchool().getOid());

        QueryByCriteria query = new QueryByCriteria(StudentContact.class, motherCriteria);
        m_motherMap = getBroker().getMapByQuery(query, StudentContact.COL_STUDENT_OID, 5000);

        query = new QueryByCriteria(StudentContact.class, fatherCriteria);
        m_fatherMap = getBroker().getMapByQuery(query, StudentContact.COL_STUDENT_OID, 5000);

        query = new QueryByCriteria(StudentContact.class, guardianCriteria);
        m_guardianMap = getBroker().getMapByQuery(query, StudentContact.COL_STUDENT_OID, 5000);
    }

    /**
     * Check if there is a field aliased as birthplace field.
     * If not - log warning.
     */
    private void checkBirthplace() {
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField birthplaceField = dictionary.findDataDictionaryFieldByAlias(ALIAS_BIRTHPLACE);

        if (birthplaceField == null) {
            this.logToolMessage(Level.WARNING, "\"School Health Record\" - field \"Birthplace\" not found", true);
        }
    }
}
