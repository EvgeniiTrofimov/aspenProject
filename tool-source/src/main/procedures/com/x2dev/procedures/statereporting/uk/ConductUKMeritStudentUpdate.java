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
package com.x2dev.procedures.statereporting.uk;

import static com.x2dev.sis.model.business.conduct.ConductAliases.ALIAS_REF_CODE_MERIT_POINTS;
import static com.x2dev.sis.model.business.conduct.ConductAliases.ALIAS_STUDENT_DEMERIT_POINTS;
import static com.x2dev.sis.model.business.conduct.ConductAliases.ALIAS_STUDENT_MERIT_POINTS;
import static com.x2dev.sis.model.business.conduct.ConductAliases.ALIAS_STUDENT_TOTAL_MERIT;
import static com.x2dev.sis.model.business.conduct.ConductAliases.REF_CODE_CONDUCT_INCIDENT_TYPE;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.x2dev.sis.model.beans.ConductIncident;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;


/**
 * Cycles though all Conduct Incidents, summarize the Merit Values by Student and save it to the
 * student record.
 *
 * This Procedure is expected to be run from a School View
 *
 * @author X2 Development Corporation
 */
public class ConductUKMeritStudentUpdate extends ProcedureJavaSource {
    private Collection<ConductIncident> m_conductIncidents;
    private int m_incidentCount = 0;
    private Map<String, BigDecimal> m_referenceCodes = new HashMap<String, BigDecimal>();
    private School m_school;
    private PlainDate m_startDate;
    private PlainDate m_endDate;
    private int m_studentCount = 0;
    private Collection<Student> m_students;
    private Map<String, BigDecimal> m_studentIncidentNegativePoints = new HashMap<String, BigDecimal>();
    private Map<String, BigDecimal> m_studentIncidentPositivePoints = new HashMap<String, BigDecimal>();
    private Map<String, BigDecimal> m_studentIncidentTotalPoints = new HashMap<String, BigDecimal>();

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.x2dev.sis.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        logMessage("Start Updating Student Merit/Demerit Totals.");

        m_school = getSchool();
        if (m_school != null) {
            logMessage("Running for School: " + m_school.getName());
        } else {
            // This is only for District Administrators
            logMessage("Running for all Schools.");
        }

        // Get the current Active Schedule Dates
        loadActiveScheduleDates();
        logMessage("Active School Year Dates: " + m_startDate.toString() + " to " + m_endDate.toString());

        // Get the Conduct Incident Reference Codes
        loadConductIncidentReferenceCodes();
        logMessage("Using " + m_referenceCodes.size() + " Conduct Incident Reference Codes.");

        // Get all Conduct Incidents in the current Active Schedule Dates
        loadConductIncidents();
        if (m_conductIncidents.size() == 0) {
            logMessage("Error: No Conduct Incidents were found to update.");
            return;
        }
        logMessage("Found " + m_conductIncidents.size() + " Conduct Incidents.");

        // Summarize the Conduct incident Merits and De-merits Points by Student Oid
        sumStudentConductMeritPointTotals();

        // Load the students for update from the Conduct Incidents
        loadStudents();
        logMessage("For " + m_students.size() + " Students.");

        // Cycle though all Conduct Incidents, summarize the Merit Values by Student and save it to
        // the student record.
        updateStudentConductMeritPoints();
        logMessage("Updated " + m_studentCount + " Student(s) having " + m_incidentCount + " Incident(s).");

        logMessage("Finish Updating Student Merit/Demerit Totals.");
    }

    /**
     * Load the Active Schedule Dates.
     */
    private void loadActiveScheduleDates() {
        SisSchool school = (SisSchool) getSchool();
        if (school != null) {
            Schedule activeSchedule = school.getActiveSchedule();
            if (activeSchedule != null) {
                m_startDate = activeSchedule.getStartDate();
                m_endDate = activeSchedule.getEndDate();
            } else {
                m_startDate = getCurrentContext().getStartDate();
                m_endDate = getCurrentContext().getEndDate();
            }
        } else {
            m_startDate = getCurrentContext().getStartDate();
            m_endDate = getCurrentContext().getEndDate();
        }
    }

    /**
     * Load the Conduct Incident Reference Codes.
     */
    private void loadConductIncidentReferenceCodes() {
        Criteria refCodeCriteria = new Criteria();
        refCodeCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, REF_CODE_CONDUCT_INCIDENT_TYPE);
        QueryByCriteria refCodeQuery = new QueryByCriteria(ReferenceCode.class, refCodeCriteria);
        Collection<ReferenceCode> referenceCodes = getBroker().getCollectionByQuery(refCodeQuery);

        m_referenceCodes = new HashMap<String, BigDecimal>();
        for (ReferenceCode referenceCode : referenceCodes) {
            BigDecimal meritPoints = new BigDecimal(0);
            String conductCodeMeritPoints = (String) referenceCode.getFieldValueByAlias(ALIAS_REF_CODE_MERIT_POINTS);
            if (conductCodeMeritPoints != null && StringUtils.isNumeric(conductCodeMeritPoints)) {
                meritPoints = BigDecimal.valueOf(Double.parseDouble(conductCodeMeritPoints));
            }
            m_referenceCodes.put(referenceCode.getCode(), meritPoints);
        }
    }

    /**
     * Load the Conduct Incidents within the current Active Schedule Dates.
     */
    private void loadConductIncidents() {
        Criteria incidentsCriteria = new Criteria();
        if (m_school != null) {
            incidentsCriteria.addEqualTo(ConductIncident.COL_SCHOOL_OID, m_school.getOid());
        }
        incidentsCriteria.addGreaterOrEqualThan(ConductIncident.COL_INCIDENT_DATE, m_startDate);
        incidentsCriteria.addLessOrEqualThan(ConductIncident.COL_INCIDENT_DATE, m_endDate);

        QueryByCriteria incidentQuery = new QueryByCriteria(ConductIncident.class, incidentsCriteria);
        incidentQuery.addOrderByAscending(ConductIncident.COL_STUDENT_OID);
        m_conductIncidents = getBroker().getCollectionByQuery(incidentQuery);
    }

    /**
     * Load the students for update from the Conduct Incidents.
     */
    private void loadStudents() {
        Criteria studentCriteria = new Criteria();
        if (m_school != null) {
            studentCriteria.addEqualTo(ConductIncident.COL_SCHOOL_OID, m_school.getOid());
        }
        // Exclude archived schools and inactive schools
        studentCriteria.addEqualTo(ConductIncident.REL_SCHOOL + "." + School.COL_ARCHIVE_INDICATOR, Boolean.valueOf(false));
        studentCriteria.addEqualTo(ConductIncident.REL_SCHOOL + "." + School.COL_INACTIVE_INDICATOR,
                Boolean.valueOf(false));

        QueryByCriteria studentQuery = new QueryByCriteria(Student.class, studentCriteria);

        m_students = getBroker().getCollectionByQuery(studentQuery);
    }

    /**
     * Summarize the Conduct incident Merits and De-merits Points by Student Oid.
     */
    private void sumStudentConductMeritPointTotals() {
        String lastStudentOid = "";

        double positivePoints = 0;
        double negativePoints = 0;
        double totalPoints = 0;

        for (ConductIncident conductIncident : m_conductIncidents) {
            String currentStudentOid = conductIncident.getStudentOid();
            String incidentCode = conductIncident.getIncidentCode();
            m_incidentCount++;

            if (!(currentStudentOid.equalsIgnoreCase(lastStudentOid)) || m_incidentCount == 1) {
                if (m_incidentCount == 1) {
                    lastStudentOid = currentStudentOid;
                }
                m_studentIncidentPositivePoints.put(lastStudentOid, new BigDecimal(positivePoints));
                m_studentIncidentNegativePoints.put(lastStudentOid, new BigDecimal(negativePoints));
                m_studentIncidentTotalPoints.put(lastStudentOid, new BigDecimal(positivePoints + negativePoints));

                positivePoints = 0;
                negativePoints = 0;
                totalPoints = 0;

                m_studentCount++;
            }

            double IncidentCodePoints = 0;
            if (m_referenceCodes.containsKey(incidentCode)) {
                IncidentCodePoints = m_referenceCodes.get(incidentCode).doubleValue();
            }

            totalPoints = totalPoints + IncidentCodePoints;
            if (IncidentCodePoints >= 0) {
                positivePoints = positivePoints + IncidentCodePoints;
            } else {
                negativePoints = negativePoints + IncidentCodePoints;
            }

            lastStudentOid = currentStudentOid;
        }

        // Save last student
        m_studentIncidentPositivePoints.put(lastStudentOid, new BigDecimal(positivePoints));
        m_studentIncidentNegativePoints.put(lastStudentOid, new BigDecimal(negativePoints));
        m_studentIncidentTotalPoints.put(lastStudentOid, new BigDecimal(positivePoints + negativePoints));
        m_studentCount++;
    }

    /**
     * Takes all Conduct Incidents and Summarize Merit Values and saves to the Student record.
     */
    private void updateStudentConductMeritPoints() {
        double positivePoints = 0;
        double negativePoints = 0;
        double totalPoints = 0;
        for (Student student : m_students) {
            String studentOid = student.getOid();
            positivePoints = 0;
            if (m_studentIncidentPositivePoints.containsKey(studentOid)) {
                positivePoints = m_studentIncidentPositivePoints.get(studentOid).doubleValue();
            }
            negativePoints = 0;
            if (m_studentIncidentNegativePoints.containsKey(studentOid)) {
                negativePoints = m_studentIncidentNegativePoints.get(studentOid).doubleValue();
            }
            totalPoints = 0;
            if (m_studentIncidentTotalPoints.containsKey(studentOid)) {
                totalPoints = m_studentIncidentTotalPoints.get(studentOid).doubleValue();
            }

            student.setFieldValueByAlias(ALIAS_STUDENT_MERIT_POINTS, (new BigDecimal(positivePoints)).toString());
            student.setFieldValueByAlias(ALIAS_STUDENT_DEMERIT_POINTS, (new BigDecimal(negativePoints)).toString());
            student.setFieldValueByAlias(ALIAS_STUDENT_TOTAL_MERIT, (new BigDecimal(totalPoints)).toString());

            getBroker().saveBeanForced(student);
        }
    }

}
