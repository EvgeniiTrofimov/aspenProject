/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2014 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.reports.bc;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.Staff;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.StudentAlert;
import com.follett.fsc.core.k12.beans.StudentSchool;
import com.follett.fsc.core.k12.business.StudentManager;
import com.x2dev.sis.model.beans.HealthCondition;
import com.x2dev.sis.model.beans.HealthMedicationOrder;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.tools.reports.StudentContextReportHelper;
import com.x2dev.utils.CollectionUtils;
import com.x2dev.utils.DataGrid;
import com.x2dev.utils.ObjectUtils;
import com.x2dev.utils.StringUtils;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import org.apache.commons.lang.exception.ExceptionUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Exports medical information (conducts with a UDF link to medication) for BC's GDE.
 *
 * @author Follett Software Company
 */
public class MedicalInformationExport extends GdeExportJavaSource {
    private static final long serialVersionUID = 1L;

    // Grid fields
    private static final String FIELD_STUDENT_ID = "Student Number";
    private static final String FIELD_STUDENT_LAST_NAME = "Student Last Name";
    private static final String FIELD_STUDENT_FIRST_NAME = "Student First Name";
    private static final String FIELD_GRADE = "Grade";
    private static final String FIELD_HOMEROOM = "Homeroom";
    private static final String FIELD_TEACHER_NAME = "Teacher Name";
    private static final String FIELD_CONDITION = "Condition";
    private static final String FIELD_LIFE_THREATENING = "Life Threatening Flag";
    private static final String FIELD_PER_REQUEST = "Per Request Flag";
    private static final String FIELD_MEDICATION_TYPE = "Medication Type";
    private static final String FIELD_DOSAGE = "Dosage";
    private static final String FIELD_DOSAGE_TYPE = "Dosage Unit Type";

    // Aliases
    private static final String ALIAS_LIFE_THREATENING = "hcn-life-threatening";
    private static final String ALIAS_MEDICATION_NAME = "hcn-medication-name";

    // Valid booleans
    private static final Collection<String> TRUE_BOOLEANS = Arrays.asList("YES", "TRUE", "Y", "T");

    // Other constants
    private static final int FIELD_COUNT = 12;

    private Map<String, Collection<String>> m_alertMap;
    private List<String> m_columns;
    private StudentContextReportHelper m_helper;
    private Map<String, Collection<HealthMedicationOrder>> m_orderMap;

    /**
     * Gather data.
     *
     * @return DataGrid
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#gatherData()
     */
    @Override
    protected DataGrid gatherData() throws Exception {
        DataGrid grid = new DataGrid() {
            private static final long serialVersionUID = 1L;

            @Override
            public void set(String column, Object value) {
                super.set(column, wrap((String) value));
            }
        };

        QueryByCriteria query = new QueryByCriteria(HealthCondition.class, buildCriteria());
        query.setDistinct(true);
        query.addOrderByAscending(HealthCondition.REL_STUDENT + PATH_DELIMITER + m_helper.getSchoolRelationship()
                + PATH_DELIMITER + SisSchool.COL_SCHOOL_ID);
        query.addOrderByAscending(HealthCondition.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_NAME_VIEW);
        query.addOrderByAscending(HealthCondition.COL_STUDENT_OID);
        query.addOrderByAscending(HealthCondition.COL_CONDITION_CODE);

        QueryIterator conditions = getBroker().getIteratorByQuery(query);
        try {
            while (conditions.hasNext()) {
                HealthCondition condition = (HealthCondition) conditions.next();
                boolean deleteRow = false;
                try {
                    SisStudent student = condition.getStudent();

                    String gradeLevel = student.getGradeLevel(getCurrentContext().getOid(), getBroker());
                    String subGradeLevel = (String) student.getFieldValueByAlias("std-sub-grade");

                    String homeroom = student.getHomeroom(getCurrentContext().getOid(), getBroker());
                    SisSchool school = student.getSchool(getCurrentContext().getOid(), getBroker());

                    grid.append();
                    deleteRow = true;

                    grid.set(FIELD_STUDENT_ID, student.getLocalId());
                    grid.set(FIELD_STUDENT_LAST_NAME, student.getPerson().getLastName());
                    grid.set(FIELD_STUDENT_FIRST_NAME, student.getPerson().getFirstName());
                    grid.set(FIELD_GRADE, StringUtils.isEmpty(subGradeLevel) ? gradeLevel : subGradeLevel);
                    grid.set(FIELD_LIFE_THREATENING,
                            formatStringBoolean((String) condition.getFieldValueByAlias(ALIAS_LIFE_THREATENING)));
                    grid.set(FIELD_HOMEROOM, homeroom);

                    /*
                     * Set homeroom teacher name
                     */
                    Map<String, Staff> staffMap = getHomeroomToStaffMap(school);
                    if (staffMap != null) {
                        Staff staff = staffMap.get(homeroom);
                        if (staff != null) {
                            grid.set(FIELD_TEACHER_NAME, staff.getNameView());
                        }
                    }

                    /*
                     * Set the condition description (based on medical alerts)
                     */
                    Collection<String> alerts = m_alertMap.get(student.getOid());
                    if (!CollectionUtils.isEmpty(alerts)) {
                        grid.set(FIELD_CONDITION, StringUtils.convertCollectionToDelimitedString(alerts, "; "));
                    }

                    /*
                     * Load the related orders, based on user-defined fields
                     */
                    Collection<HealthMedicationOrder> orders = m_orderMap.get(condition.getStudentOid());
                    if (!CollectionUtils.isEmpty(orders)) {
                        for (HealthMedicationOrder order : orders) {
                            if (order.getMedicationName()
                                    .equals(condition.getFieldValueByAlias(ALIAS_MEDICATION_NAME))) {
                                grid.set(FIELD_PER_REQUEST, Boolean.toString(order.getAsNeededIndicator()));
                                grid.set(FIELD_MEDICATION_TYPE, order.getMedicationType());
                                grid.set(FIELD_DOSAGE, order.getAdministrationDose().toString());
                                grid.set(FIELD_DOSAGE_TYPE, order.getAdministrationUnit());
                            }
                        }
                    }
                } catch (NullPointerException npe) {
                    StringBuilder strBldr = new StringBuilder();
                    strBldr.append("Unable to export ");
                    strBldr.append(query.getClass().getName());
                    strBldr.append(" with OID: [");
                    strBldr.append(condition.getOid());
                    SisStudent student = condition.getStudent();
                    if (student != null) {
                        strBldr.append("] for the Student with Local ID: [");
                        strBldr.append(student.getLocalId());
                        strBldr.append("].");
                    } else {
                        strBldr.append("] as it has no related Student.");
                    }

                    // deleteRow is true if an incomplete row has been added to the grid from
                    // grid.append()
                    if (!deleteRow) {
                        strBldr.append("Null encountered before adding to export.");
                    } else {
                        strBldr.append("Null encountered when setting Columns.");
                        grid.deleteRow(); // Delete the incomplete row that was appended to the
                                          // grid.
                    }

                    strBldr.append("\n\n\nNullPointerException: \n");
                    strBldr.append(ExceptionUtils.getStackTrace(npe));
                    logToolMessage(Level.WARNING, strBldr.toString(), false);
                }
            }
        } finally {
            conditions.close();
        }
        return grid;
    }

    /**
     * Gets the column names.
     *
     * @return List
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getColumnNames()
     */
    @Override
    protected List<String> getColumnNames() {
        return m_columns;
    }

    /**
     * Gets the column user names.
     *
     * @return List
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getColumnUserNames()
     */
    @Override
    protected List<String> getColumnUserNames() {
        return m_columns;
    }

    /**
     * Initialize.
     *
     * @see com.x2dev.sis.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        super.initialize();

        m_columns = new ArrayList<String>(FIELD_COUNT);
        m_columns.add(FIELD_STUDENT_ID);
        m_columns.add(FIELD_STUDENT_LAST_NAME);
        m_columns.add(FIELD_STUDENT_FIRST_NAME);
        m_columns.add(FIELD_GRADE);
        m_columns.add(FIELD_HOMEROOM);
        m_columns.add(FIELD_TEACHER_NAME);
        m_columns.add(FIELD_CONDITION);
        m_columns.add(FIELD_LIFE_THREATENING);
        m_columns.add(FIELD_PER_REQUEST);
        m_columns.add(FIELD_MEDICATION_TYPE);
        m_columns.add(FIELD_DOSAGE);
        m_columns.add(FIELD_DOSAGE_TYPE);

        m_helper = new StudentContextReportHelper(getOrganization(), getCurrentContext(), getBroker());

        /*
         * Load lookup maps
         */
        loadAlertDisplays();
        loadMedicationOrders();
    }

    /**
     * Adds the criteria to filter students based on school and enrollments status.
     *
     * @param criteria Criteria
     * @param prefix String
     * @param activeOnly boolean
     * @param includeSecondary boolean
     * @return Criteria
     * @Param criteria
     */
    private void addSchoolCriteria(Criteria criteria, String prefix, boolean activeOnly, boolean includeSecondary) {
        X2Criteria schoolCriteria = new X2Criteria();
        schoolCriteria.addAndCriteria(getSchoolCriteria().copyWithAdjustedPath(
                prefix + m_helper.getSchoolRelationship(), prefix + m_helper.getSchoolOidField()));

        if (activeOnly) {
            schoolCriteria.addAndCriteria(m_helper.getActiveStudentCriteria(prefix));
        }

        if (includeSecondary) {
            String secondaryPrefix = prefix + SisStudent.REL_STUDENT_SCHOOLS + PATH_DELIMITER;
            Criteria secondaryCriteria = StudentManager.getSecondaryStudentCriteria(
                    secondaryPrefix,
                    getCurrentContext().getOid(),
                    null,
                    null,
                    null,
                    getBroker().getPersistenceKey());
            secondaryCriteria.addAndCriteria(getSchoolCriteria().copyWithAdjustedPath(
                    secondaryPrefix + StudentSchool.REL_SCHOOL, secondaryPrefix + StudentSchool.COL_SCHOOL_OID));

            schoolCriteria.addOrCriteria(secondaryCriteria);
        }

        criteria.addAndCriteria(schoolCriteria);
    }

    /**
     * Build the criteria used to pull the health conditions for the export.
     * 
     * @return Criteria
     */
    private Criteria buildCriteria() {
        X2Criteria criteria = new X2Criteria();
        addSchoolCriteria(criteria, HealthCondition.REL_STUDENT + PATH_DELIMITER, false, true);

        return criteria;
    }

    /**
     * Returns the appropriate Y/N value for the passed boolean value as a String.
     *
     * @param value String
     * @return String Y or N
     */
    private String formatStringBoolean(String value) {
        String formatted = "N";

        if (!StringUtils.isEmpty(value) && TRUE_BOOLEANS.contains(value.toUpperCase())) {
            formatted = "Y";
        }

        return formatted;
    }

    /**
     * Loads the medical alerts for the students in the current school.
     */
    private void loadAlertDisplays() {
        m_alertMap = new HashMap<String, Collection<String>>(2048);

        Criteria criteria = new Criteria();
        criteria.addEqualTo(StudentAlert.COL_ALERT_TYPE, Integer.valueOf(StudentAlert.AlertType.MEDICAL.ordinal()));

        addSchoolCriteria(criteria, StudentAlert.REL_STUDENT + PATH_DELIMITER, false, true);

        QueryByCriteria query = new QueryByCriteria(StudentAlert.class, criteria);
        query.setDistinct(true);
        query.addOrderByAscending(StudentAlert.COL_STUDENT_OID);
        query.addOrderByAscending(StudentAlert.COL_SEQUENCE_NUMBER);

        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            Student lastStudent = null;
            Collection<String> displays = new LinkedList<String>();

            while (iterator.hasNext()) {
                StudentAlert alert = (StudentAlert) iterator.next();
                Student student = alert.getStudent();

                if (!ObjectUtils.match(student, lastStudent)) {
                    displays = new LinkedList<String>();
                    m_alertMap.put(student.getOid(), displays);
                }

                displays.add(alert.getAlertDescription());

                lastStudent = student;
            }
        } finally {
            iterator.close();
        }
    }

    /**
     * Loads the medication orders for the students included in the export.
     */
    private void loadMedicationOrders() {
        X2Criteria criteria = new X2Criteria();
        criteria.addGreaterOrEqualThan(HealthMedicationOrder.COL_STOP_DATE, getCurrentContext().getStartDate());

        addSchoolCriteria(criteria, HealthMedicationOrder.REL_STUDENT + PATH_DELIMITER, true, true);

        QueryByCriteria query = new QueryByCriteria(HealthMedicationOrder.class, criteria);
        query.setDistinct(true);
        query.addOrderByAscending(HealthMedicationOrder.COL_STUDENT_OID);
        query.addOrderByAscending(HealthMedicationOrder.COL_MEDICATION_NAME);

        m_orderMap = getBroker().getGroupedCollectionByQuery(query, HealthMedicationOrder.COL_STUDENT_OID, 4096);
    }
}
