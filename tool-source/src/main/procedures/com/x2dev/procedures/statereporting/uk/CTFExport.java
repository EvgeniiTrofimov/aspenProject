/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2002-2012 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.uk;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.StudentSchool;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.ToolJavaSource;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.sis.model.beans.AssessmentDefinition;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAssessment;
import com.x2dev.sis.model.beans.StudentAttendance;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.utils.StreamUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.io.ByteArrayInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.struts.util.MessageResources;
import org.jdom.Document;
import org.jdom.Element;
import org.jdom.output.Format;
import org.jdom.output.XMLOutputter;

/**
 * This class imports exam results for the UK education system.
 *
 * @author Follett Software Company
 */

public class CTFExport extends ToolJavaSource {
    public static final String REPORT_CTF = "CTF";

    private List<String> m_messages = new LinkedList();
    private DfEExportManager m_dfEExportManager = null;
    private Document XMLDocument = null;

    private ArrayList<String> m_studentOids = new ArrayList<String>();
    private Collection m_students;
    private HashMap<String, Collection<StudentEnrollment>> m_studentEnrollments =
            new HashMap<String, Collection<StudentEnrollment>>();
    private HashMap<String, Collection<StudentAssessment>> m_studentAssessments =
            new HashMap<String, Collection<StudentAssessment>>();
    private HashMap<String, Collection<StudentSchool>> m_studentAttendanceHistories =
            new HashMap<String, Collection<StudentSchool>>();
    private Map<String, Collection<StudentAttendance>> m_studentAttendances;
    private Map<String, Map<PlainDate, StudentAttendance>> m_attendancesByDate;

    private SisSchool m_school = null;
    private String m_sourceLEA = null;
    private String m_sourceEstab = null;
    private String m_destLEA = null;
    private String m_destEstab = null;
    private HashMap<String, Boolean> m_activeSections = new HashMap<String, Boolean>();

    public static final String QUERY_BY_PARAM = "queryBy";
    public static final String QUERY_STRING_PARAM = "queryString";

    public static final List VALID_ENROLLMENT_TYPES = new ArrayList(Arrays.asList(new String[] {"E", "W"}));


    /**
     * Export to XMLDocument.
     */
    protected void exportData() {
        if (validateSchool()) {
            if (m_students.size() > 0) {
                XMLDocument = createXMLDocument();
            } else {
                logMessage("Error: No students selected.");
            }
        }
    }

    /**
     * Returns the value of the passed resource key. If the key is not found, the key itself is
     * returned.
     *
     * @param key String
     * @param resources MessageResources
     * @return String
     */
    protected String getMessage(String key, MessageResources resources) {
        String message = resources.getMessage(getLocale(), key);
        if (message == null) {
            message = key;
        }

        return message;
    }

    /**
     * Returns the entire list of log messages.
     * 
     * @return List<String>
     */
    protected List<String> getMessages() {
        return m_messages;
    }

    /**
     * Load parameters.
     *
     * @throws X2BaseException exception
     */
    protected void loadParameters() throws X2BaseException {
        // Student Criteria
        Criteria criteria = new Criteria();
        if (isSchoolContext()) {
            criteria.addEqualTo(Student.REL_SCHOOL + ModelProperty.PATH_DELIMITER + X2BaseBean.COL_OID,
                    getSchool().getOid());
        }
        String queryBy = (String) getParameter(QUERY_BY_PARAM);

        addUserCriteria(criteria, queryBy, null, Student.class, X2BaseBean.COL_OID);

        SubQuery subQuery = new SubQuery(Student.class, X2BaseBean.COL_OID, criteria);
        Collection<String> selectedStudentOids = getBroker().getSubQueryCollectionByQuery(subQuery);

        m_studentOids.addAll(selectedStudentOids);

        if (m_studentOids.size() == 0) {
            logMessage("Error: No students selected.");
        } else {
            loadStudents();

            loadStudentsEnrollments();

            loadStudentsAssessments();

            loadStudentsAttendanceHistories();
        }

        m_school = (SisSchool) getSchool();
        if (m_school == null) {
            logMessage("Error: m_school is empty.");
        }

        m_destLEA = (String) getParameter(DfEManager.PARAM_DESTINATION_LEA);
        logMessage("m_destLEA: " + m_destLEA);
        m_destEstab = (String) getParameter(DfEManager.PARAM_DESTINATION_ESTAB);
        logMessage("m_destEstab: " + m_destEstab);

        Boolean code = (Boolean) getParameter(DfEManager.PARAM_CORE);
        m_activeSections.put(DfEManager.PARAM_CORE, code);
        Boolean basicDetails = (Boolean) getParameter(DfEManager.PARAM_BASIC_DETAILS);
        m_activeSections.put(DfEManager.PARAM_BASIC_DETAILS, basicDetails);
        Boolean sENHistory = (Boolean) getParameter(DfEManager.PARAM_SEN_HISTORY);
        m_activeSections.put(DfEManager.PARAM_SEN_HISTORY, sENHistory);
        Boolean fSMHistory = (Boolean) getParameter(DfEManager.PARAM_FSM_HISTORY);
        m_activeSections.put(DfEManager.PARAM_FSM_HISTORY, fSMHistory);
        Boolean nAWDetails = (Boolean) getParameter(DfEManager.PARAM_NAW_DETAILS);
        m_activeSections.put(DfEManager.PARAM_NAW_DETAILS, nAWDetails);
        Boolean lookedAfter = (Boolean) getParameter(DfEManager.PARAM_LOOKED_AFTER);
        m_activeSections.put(DfEManager.PARAM_LOOKED_AFTER, lookedAfter);
        Boolean address = (Boolean) getParameter(DfEManager.PARAM_ADDRESS_PHONE_EMAIL);
        m_activeSections.put(DfEManager.PARAM_ADDRESS_PHONE_EMAIL, address);
        Boolean contacts = (Boolean) getParameter(DfEManager.PARAM_CONTACTS);
        m_activeSections.put(DfEManager.PARAM_CONTACTS, contacts);
        Boolean assessments = (Boolean) getParameter(DfEManager.PARAM_ASSESSMENTS);
        m_activeSections.put(DfEManager.PARAM_ASSESSMENTS, assessments);
        Boolean attendance = (Boolean) getParameter(DfEManager.PARAM_ATTENDANCE);
        m_activeSections.put(DfEManager.PARAM_ATTENDANCE, attendance);
        Boolean attendanceSessions = (Boolean) getParameter(DfEManager.PARAM_ATTENDANCE_SESSIONS);
        m_activeSections.put(DfEManager.PARAM_ATTENDANCE_SESSIONS, attendanceSessions);
        Boolean schoolHistory = (Boolean) getParameter(DfEManager.PARAM_SCHOOL_HISTORY);
        m_activeSections.put(DfEManager.PARAM_SCHOOL_HISTORY, schoolHistory);
    }

    /**
     * Load Students.
     */
    protected void loadStudents() {
        if (m_studentOids.size() > 0) {
            Criteria studentCriteria = new Criteria();
            studentCriteria.addIn(X2BaseBean.COL_OID, m_studentOids);

            QueryByCriteria studentQuery = new QueryByCriteria(Student.class, studentCriteria);

            loadStudentAttendance(studentCriteria);
            m_students = getBroker().getCollectionByQuery(studentQuery);
        } else {
            m_students = new ArrayList<SisStudent>();
        }
    }

    /**
     * Load Students Assessments
     *
     * Only StudentAssessment using the SAT AssessmentDefinition are included.
     */
    protected void loadStudentsAssessments() {
        if (m_studentOids.size() > 0) {
            // If this default SAT Assessment Definition exists then don't continue
            Criteria assessmentDefinitonCriteria = new Criteria();
            assessmentDefinitonCriteria.addEqualTo(AssessmentDefinition.COL_NAME, DfEManager.ASSESSMENT_DEFINITION_SAT);
            QueryByCriteria assessmentDefinitonQuery =
                    new QueryByCriteria(AssessmentDefinition.class, assessmentDefinitonCriteria);
            AssessmentDefinition sATDefinition =
                    (AssessmentDefinition) getBroker().getBeanByQuery(assessmentDefinitonQuery);

            if (sATDefinition != null) {
                DataDictionary dataDictionary =
                        DataDictionary.getDistrictDictionary(sATDefinition, getBroker().getPersistenceKey());
                DataDictionaryField stageField =
                        dataDictionary.findDataDictionaryFieldByAlias(DfEManager.ALIAS_NAME_STAGE);
                DataDictionaryField localeField =
                        dataDictionary.findDataDictionaryFieldByAlias(DfEManager.ALIAS_NAME_LOCALE);
                DataDictionaryField yearTakenField =
                        dataDictionary.findDataDictionaryFieldByAlias(DfEManager.ALIAS_NAME_YEAR_TAKEN);
                DataDictionaryField subjectField =
                        dataDictionary.findDataDictionaryFieldByAlias(DfEManager.ALIAS_NAME_SUBJECT);

                // If the SAT Assessment Definition Aliases are not setup then don't continue
                if (stageField != null && localeField != null && yearTakenField != null && subjectField != null) {
                    Criteria studentAssessmentCriteria = new Criteria();
                    studentAssessmentCriteria.addIn(StudentAssessment.COL_STUDENT_OID, m_studentOids);
                    studentAssessmentCriteria.addEqualTo(
                            StudentAssessment.REL_ASSESSMENT_DEFINITION + "." + AssessmentDefinition.COL_NAME,
                            DfEManager.ASSESSMENT_DEFINITION_SAT);
                    studentAssessmentCriteria.addNotNull(stageField.getJavaName());
                    // TODO select only these Stages types: EYF, FSP, KS1, K1T, K2P, K3P, KS2, KS3,
                    // SL2, SL3, SL4, SL5, SL6

                    QueryByCriteria studentAssessmentQuery =
                            new QueryByCriteria(StudentAssessment.class, studentAssessmentCriteria);
                    studentAssessmentQuery.addOrderByAscending(StudentAssessment.COL_STUDENT_OID);
                    studentAssessmentQuery.addOrderByAscending(stageField.getJavaName());
                    studentAssessmentQuery.addOrderByAscending(localeField.getJavaName());
                    studentAssessmentQuery.addOrderByAscending(yearTakenField.getJavaName());
                    studentAssessmentQuery.addOrderByAscending(subjectField.getJavaName());

                    m_studentAssessments = (HashMap<String, Collection<StudentAssessment>>) getBroker()
                            .getGroupedCollectionByQuery(studentAssessmentQuery, StudentAssessment.COL_STUDENT_OID, 64);
                }
            }
        } else {
            m_studentAssessments = new HashMap<String, Collection<StudentAssessment>>();
        }
    }

    /**
     * Load Students Attendance Histories
     *
     * Old imported Student Attendance information is stored in SchoolHistory table. Not to be
     * confused with the CTF School History section
     */
    protected void loadStudentsAttendanceHistories() {
        String cTFSchoolOid = m_dfEExportManager.m_cTFSchool.getOid();
        if (m_studentOids.size() > 0) {
            X2Criteria studentSchoolCriteria = new X2Criteria();
            studentSchoolCriteria.addEqualTo(StudentSchool.COL_SCHOOL_OID, cTFSchoolOid);

            QueryByCriteria studentSchoolQuery = new QueryByCriteria(StudentSchool.class, studentSchoolCriteria);
            studentSchoolQuery.addOrderByAscending(StudentSchool.COL_STUDENT_OID);

            m_studentAttendanceHistories = (HashMap<String, Collection<StudentSchool>>) getBroker()
                    .getGroupedCollectionByQuery(studentSchoolQuery, StudentSchool.COL_STUDENT_OID, 64);
        } else {
            m_studentAttendanceHistories = new HashMap<String, Collection<StudentSchool>>();
        }
    }

    /**
     * Load Students Enrollments
     *
     * Used for SchoolHistory section.
     */
    protected void loadStudentsEnrollments() {
        if (m_studentOids.size() > 0) {
            Criteria studentEnrollmentCriteria = new Criteria();
            studentEnrollmentCriteria.addIn(StudentEnrollment.COL_STUDENT_OID, m_studentOids);
            studentEnrollmentCriteria.addIn(StudentEnrollment.COL_ENROLLMENT_TYPE, VALID_ENROLLMENT_TYPES);

            QueryByCriteria studentEnrollmentQuery =
                    new QueryByCriteria(StudentEnrollment.class, studentEnrollmentCriteria);
            studentEnrollmentQuery.addOrderByAscending(StudentEnrollment.COL_STUDENT_OID);
            studentEnrollmentQuery.addOrderByAscending(StudentEnrollment.COL_ENROLLMENT_DATE);
            studentEnrollmentQuery.addOrderByDescending(StudentEnrollment.COL_ENROLLMENT_TYPE);

            m_studentEnrollments = (HashMap<String, Collection<StudentEnrollment>>) getBroker()
                    .getGroupedCollectionByQuery(studentEnrollmentQuery, StudentEnrollment.COL_STUDENT_OID, 64);
        } else {
            m_studentEnrollments = new HashMap<String, Collection<StudentEnrollment>>();
        }
    }

    /**
     * Logs a message that will be written to the results file. Each message is written on its own
     * line.
     * 
     * @param key a resource key, if a corresponding message cannot be found then the key itself
     *        will be logged
     */
    protected void logMessage(String key) {
        m_messages.add(key);
    }

    /**
     * Release resources.
     *
     * @see com.x2dev.sis.tools.ToolJavaSource#releaseResources()
     */
    @Override
    protected void releaseResources() {
        super.releaseResources();
        return;
    }

    /**
     * Run.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#run()
     */
    @Override
    protected void run() throws Exception {
        logMessage("CTF Export Starting...");

        m_dfEExportManager = new DfEExportManager(getBroker(), getLocale());

        loadParameters();

        exportData();

        logMessage("CTF Export Completed.");

        if (XMLDocument != null) {
            exportResults();
        } else {
            displayMessages();
        }
    }

    /**
     * Create DfE CTF XML Document from Student.
     *
     * @return Document
     */
    private Document createXMLDocument() {
        Document xmlDocument = new Document();

        Element rootElement = new Element(DfEHeader.ELEMENT_CTFILE);

        Element headerElement = getCreateHeaderElement(m_school);
        rootElement.addContent(headerElement);

        Element cTFPupilDataElement = new Element(DfEPupil.ELEMENT_CTF_PUPIL_DATA);
        Iterator it = m_students.iterator();
        while (it.hasNext()) {
            SisStudent student = (SisStudent) it.next();

            String studentOid = student.getOid();
            Collection<StudentEnrollment> enrollments = new ArrayList<StudentEnrollment>();
            if (m_studentEnrollments.containsKey(studentOid)) {
                enrollments = m_studentEnrollments.get(studentOid);
            }

            Collection<StudentAssessment> assessments = new ArrayList<StudentAssessment>();
            if (m_studentAssessments.containsKey(studentOid)) {
                assessments = m_studentAssessments.get(studentOid);
            }

            Collection<StudentSchool> attendanceHistories = new ArrayList<StudentSchool>();
            if (m_studentAttendanceHistories.containsKey(studentOid)) {
                attendanceHistories = m_studentAttendanceHistories.get(studentOid);
            }

            Collection<StudentAttendance> attendances = new ArrayList<StudentAttendance>();
            if (m_studentAttendances.containsKey(studentOid)) {
                attendances = m_studentAttendances.get(studentOid);
            }

            Map<PlainDate, StudentAttendance> attendancesByDate = new HashMap<PlainDate, StudentAttendance>();
            if (m_attendancesByDate.containsKey(studentOid)) {
                attendancesByDate = m_attendancesByDate.get(studentOid);
            }

            Element pupilElement = getCreateCTFPupilDataElement(student, enrollments, assessments, attendanceHistories,
                    attendances, attendancesByDate);

            cTFPupilDataElement.addContent(pupilElement);
        }
        rootElement.addContent(cTFPupilDataElement);

        xmlDocument.setRootElement(rootElement);

        return xmlDocument;
    }

    /**
     * Print out the logs to the user.
     *
     * @throws X2BaseException exception
     */
    private void displayMessages() throws X2BaseException {
        StringBuilder buffer = new StringBuilder(256);

        MessageResources resources = LocalizationCache.getMessages(getUser().getPersistenceKey());

        buffer.append(resources.getMessage(getLocale(), "message.procedure.results.messages"));
        buffer.append('\n');
        buffer.append(resources.getMessage(getLocale(), "message.procedure.results.messagesRule"));
        buffer.append('\n');
        buffer.append('\n');

        if (m_messages.isEmpty()) {
            buffer.append(resources.getMessage(getLocale(), "message.procedure.results.noMessages"));
        } else {
            Iterator messages = m_messages.iterator();
            while (messages.hasNext()) {
                String key = (String) messages.next();

                String message = getMessage(key, resources);

                buffer.append(message);
                buffer.append('\n');
            }
        }

        try {
            byte[] messageBytes = buffer.toString().getBytes();
            ByteArrayInputStream inputStream = new ByteArrayInputStream(messageBytes);
            try {
                OutputStream outputStream = getResultHandler().getOutputStream();
                StreamUtils.copyStream(inputStream, outputStream);
            } finally {
                inputStream.close();
            }
        } catch (FileNotFoundException fnfe) {
            throw new X2BaseException(fnfe);
        } catch (IOException ioe) {
            throw new X2BaseException(ioe);
        }
    }

    /**
     * Exports the XML.
     */
    private void exportResults() {
        try {
            XMLOutputter xMLOutputter = new XMLOutputter();
            Format format = Format.getPrettyFormat();
            xMLOutputter.setFormat(format);

            OutputStream outputStream = getResultHandler().getOutputStream();
            xMLOutputter.output(XMLDocument, outputStream);
        } catch (FileNotFoundException e) {
            AppGlobals.getLog().log(Level.SEVERE, e.getMessage(), e);
        } catch (IOException e) {
            AppGlobals.getLog().log(Level.SEVERE, e.getMessage(), e);
        }
    }

    /**
     * Create DfE CTF PupilData Element.
     *
     * @param student SisStudent
     * @param enrollments Collection<StudentEnrollment>
     * @param assessments Collection<StudentAssessment>
     * @param attendanceHistories Collection<StudentSchool>
     * @param attendances Collection<StudentAttendance>
     * @param attendancesByDate Map<PlainDate,StudentAttendance>
     * @return Element
     */
    private Element getCreateCTFPupilDataElement(SisStudent student,
                                                 Collection<StudentEnrollment> enrollments,
                                                 Collection<StudentAssessment> assessments,
                                                 Collection<StudentSchool> attendanceHistories,
                                                 Collection<StudentAttendance> attendances,
                                                 Map<PlainDate, StudentAttendance> attendancesByDate) {
        DfEPupil dfEPupil = m_dfEExportManager.getDfEPupilByStudent(student, enrollments, assessments,
                attendanceHistories, attendances, attendancesByDate);

        Element pupilElement = m_dfEExportManager.getPupilDataElement(REPORT_CTF, dfEPupil, m_activeSections);

        return pupilElement;
    }

    /**
     * Create DfE CTF Header Element from Student.
     *
     * @param school SisSchool
     * @return Element
     */
    private Element getCreateHeaderElement(SisSchool school) {
        Schedule schedule = school.getActiveSchedule();
        DistrictSchoolYearContext context = schedule.getDistrictContext();
        String sourceSchoolAcademicYear = Integer.toString(context.getSchoolYear());

        DfEHeader dfEHeader = m_dfEExportManager.getDfEHeader(REPORT_CTF, m_sourceLEA, m_sourceEstab, school.getName(),
                sourceSchoolAcademicYear, m_destLEA, m_destEstab);

        Element headerElement = m_dfEExportManager.getHeaderElement(dfEHeader);

        return headerElement;
    }

    /**
     * Load Student Class Attendance Information.
     *
     * @param studentCriteria Criteria
     */
    private void loadStudentAttendance(Criteria studentCriteria) {
        SubQuery studentSubQuery = new SubQuery(Student.class, X2BaseBean.COL_OID, studentCriteria);

        Criteria criteria = new Criteria();
        criteria.addIn(StudentAttendance.COL_STUDENT_OID, studentSubQuery);

        QueryByCriteria query = new QueryByCriteria(StudentAttendance.class, criteria);
        query.addOrderByAscending(StudentAttendance.COL_STUDENT_OID);
        query.addOrderByAscending(StudentAttendance.COL_DATE);

        m_studentAttendances = getBroker().getGroupedCollectionByQuery(query, StudentAttendance.COL_STUDENT_OID, 1024);

        m_attendancesByDate = getBroker().getNestedMapByQuery(query, StudentAttendance.COL_STUDENT_OID,
                StudentAttendance.COL_DATE, 1024, 1024);
    }

    /**
     * Validate the school selected.
     * 
     * @return boolean
     */
    private boolean validateSchool() {
        if (m_school == null) {
            logMessage("Error: The selected school doesn't exist.");

            return false;
        }
        m_sourceEstab = m_dfEExportManager.getEstabBySchool(m_school);

        Schedule schedule = m_school.getActiveSchedule();
        if (schedule == null) {
            logMessage("Error: The selected school " + m_school.getName() + " doesn't have an active schedule.");

            return false;
        }

        m_sourceLEA = m_dfEExportManager.getLEABySchool(m_school);
        if (m_sourceLEA == null) {
            logMessage("Error: The selected school's LEA field has not been setup in the Data Dictionary.");

            return false;
        }

        return true;
    }

}
