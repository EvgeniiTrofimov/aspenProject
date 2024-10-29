/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2017 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.reports.statereporting.md;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.ToolInput;
import com.follett.fsc.core.k12.tools.ToolJob;
import com.follett.fsc.core.k12.tools.reports.ReportCompiler;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.HealthCondition;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.tools.stateexports.StudentEnrollmentSpan;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.TransformerFactoryConfigurationError;
import net.sf.jasperreports3.engine.JRException;
import net.sf.jasperreports3.engine.JRRewindableDataSource;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.ls.DOMImplementationLS;
import org.w3c.dom.ls.LSOutput;
import org.w3c.dom.ls.LSSerializer;
import org.xml.sax.SAXException;

/**
 * Prepares the data for the Health Conditions Statistics.
 *
 * @author Follett Software Company
 */
public class MDHealthConditionStatistics extends ReportJavaSourceNet {

    /**
     * The Class RewindableReportDataGrid.
     */
    class RewindableReportDataGrid extends ReportDataGrid implements JRRewindableDataSource {

        /**
         * Next.
         *
         * @return true, if successful
         * @see com.x2dev.utils.DataGrid#next()
         */
        @Override
        public boolean next() {
            boolean thereAreRecords = super.next();
            if (!thereAreRecords) {
                try {
                    moveFirst();
                } catch (JRException e) {
                    e.printStackTrace();
                }
            }
            return thereAreRecords;
        }

        /**
         * @see net.sf.jasperreports3.engine.JRRewindableDataSource#moveFirst()
         */
        @Override
        public void moveFirst() throws JRException {
            beforeTop();

            this.sort(FIELD_MEDICAL_CONDITION, true, true);
            this.sort(FIELD_CONDITION_TYPE, true, true);
        }
    }

    /**
     * Enrollment helper to get student criteria, student's school/date on passed date.
     *
     * @author Follett Software Company
     */
    class EnrollmentHelper extends StateReportData {
        private Collection<DistrictSchoolYearContext> m_contexts;
        private Map<String, ReferenceCode> m_referenceGradeCodeMap = null;
        private StudentHistoryHelper m_studentHelper;

        /**
         * Gets the enrollment date.
         *
         * @param studentOid String
         * @param date PlainDate
         * @return Plain date
         */
        public PlainDate getEnrollmentDate(String studentOid, PlainDate date) {
            return m_studentHelper.getEnrollmentForDate(studentOid, date, "E").getEnrollmentDate();
        }

        /**
         * Gets the enrollment for date.
         *
         * @param student SisStudent
         * @param date PlainDate
         * @param types String
         * @return Student enrollment
         */
        public StudentEnrollment getEnrollmentForDate(SisStudent student, PlainDate date, String types) {
            StudentEnrollment studentEnrollment = null;
            StudentEnrollmentSpan spanForDate = getSpanForDate(student, date);
            if (spanForDate != null) {
                studentEnrollment = spanForDate.getEnrollmentForDate(date, types);
            }

            return studentEnrollment;
        }

        /**
         * Return the current student criteria.
         *
         * @return X2Criteria
         */
        public X2Criteria getStudentCriteria() {
            return m_studentHelper.getStudentCriteria();
        }

        /**
         * Return student's grade on enrollment date.
         *
         * @param enrollment StudentEnrollment
         * @return String
         */
        public String getStudentGrade(StudentEnrollment enrollment) {
            int yog = enrollment.getYog();
            if (yog == 0) {
                yog = enrollment.getStudent().getYog();
            }
            PlainDate date = enrollment.getEnrollmentDate();
            ReferenceCode gradeCode = getGradeLevel(yog, getSchoolYear(date));

            return gradeCode == null ? null : gradeCode.getCode();
        }

        /**
         * Return oid of student's school on enrollment date.
         *
         * @param enrollment StudentEnrollment
         * @return SisSchool
         */
        public SisSchool getStudentSchool(StudentEnrollment enrollment) {
            return enrollment.getSchool();
        }

        /**
         * Initialize the export.
         * Set up the student history helper.
         *
         * @throws X2BaseException exception
         */
        @Override
        public void initialize() throws X2BaseException {
            m_studentHelper = new StudentHistoryHelper(this);
            m_studentHelper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_SNAPSHOT);
            m_studentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, getEndDate());

            super.initialize();
        }

        /**
         * Returns grade level based on yog and schoolYear.
         *
         * @param yog int
         * @param schoolYear int
         * @return ReferenceCode
         */
        private ReferenceCode getGradeLevel(int yog, int schoolYear) {
            if (m_referenceGradeCodeMap == null) {
                m_referenceGradeCodeMap = loadRefCodeMapByField(SisStudent.class, SisStudent.COL_GRADE_LEVEL);
            }

            ReferenceCode gradeCode = null;

            ModelBroker broker = new ModelBroker(getPrivilegeSet());
            TreeMap sortedGradeLevels = StudentManager.buildGradeLevelMap(broker);
            int maxGradeLevel = StudentManager.getMaxGradeLevel(getBroker());
            List<String> matchingGradeLevels =
                    StudentManager.getMatchingGradeLevels(maxGradeLevel, yog, schoolYear, sortedGradeLevels);
            for (String matchingGradeLevel : matchingGradeLevels) {
                gradeCode = m_referenceGradeCodeMap.get(matchingGradeLevel);
                if (gradeCode != null) {
                    break;
                }
            }
            return gradeCode;
        }

        /**
         * Returns school year based on date.
         *
         * @param date PlainDate
         * @return int
         */
        private int getSchoolYear(PlainDate date) {
            int schoolYear = 0;

            if (m_contexts == null) {
                m_contexts = loadContexts();
            }

            for (DistrictSchoolYearContext context : m_contexts) {
                if (!date.before(context.getStartDate()) && !date.after(context.getEndDate())) {
                    schoolYear = context.getSchoolYear();
                }
            }

            return schoolYear;
        }

        /**
         * Gets the span for date.
         *
         * @param student SisStudent
         * @param date PlainDate
         * @return Student enrollment span
         */
        private StudentEnrollmentSpan getSpanForDate(SisStudent student, PlainDate date) {
            StudentEnrollmentSpan spanForDate = null;
            for (StudentEnrollmentSpan span : m_studentHelper.getStudentEnrollmentSpans(student, false)) {
                PlainDate startDate = null;
                PlainDate endDate = null;
                StudentEnrollment firstAEnr = span.getFirstActiveEnrollment();
                if (firstAEnr != null) {
                    startDate = firstAEnr.getEnrollmentDate();
                }
                StudentEnrollment firstINAEnr = span.getFirstInactiveEnrollment();
                if (firstINAEnr != null) {
                    endDate = firstINAEnr.getEnrollmentDate();
                }


                if (startDate != null && endDate == null) {
                    if (!date.before(startDate)) {
                        spanForDate = span;
                        break;
                    }
                } else if (startDate != null && endDate != null) {
                    if (!date.before(startDate) && !date.after(endDate)) {
                        spanForDate = span;
                        break;
                    }
                }
            }
            return spanForDate;
        }

        /**
         * Load contexts to determine school year for grade calculation.
         * Contexts are restricted by selected date range.
         *
         * @return Collection
         */
        private Collection<DistrictSchoolYearContext> loadContexts() {
            X2Criteria ctxsCriteria = new X2Criteria();
            ctxsCriteria.addLessOrEqualThan(DistrictSchoolYearContext.COL_START_DATE, getEndDate());
            ctxsCriteria.addGreaterOrEqualThan(DistrictSchoolYearContext.COL_END_DATE, getEndDate());

            QueryByCriteria ctxsQuery = new QueryByCriteria(DistrictSchoolYearContext.class, ctxsCriteria);

            return getBroker().getCollectionByQuery(ctxsQuery);
        }

        /**
         * Load reference code map by field name.
         *
         * @param beanClass Class
         * @param fieldName String
         * @return Map<String, ReferenceCode>
         */
        private Map<String, ReferenceCode> loadRefCodeMapByField(Class beanClass, String fieldName) {
            Map<String, ReferenceCode> refCodeMap = null;
            DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
            ModelProperty prop = new ModelProperty(beanClass, fieldName, getBroker().getPersistenceKey());
            DataDictionaryField field = dictionary.findDataDictionaryField(prop.getFieldId());
            if (field != null) {
                ReferenceTable referenceTable = field.getReferenceTable();
                refCodeMap = referenceTable.getCodeMap();
            }
            return refCodeMap;
        }
    }

    private static final String COMMA = ",";
    private static final String EMPTY_STRING = "";

    private static final String FIELD_CONDITION_TYPE = "conditionType";
    private static final String FIELD_ENROLLMENT_DATE = "enrollmentDate";
    private static final String FIELD_GRADE = "grade";
    private static final String FIELD_SCHOOL_NAME = "school";
    private static final String FIELD_STUDENT_LASID = "lasid";
    private static final String FIELD_STUDENT_NAME = "name";
    private static final String FIELD_STUDENT_OID = "studentOid";
    private static final String FIELD_MEDICAL_CONDITION = "medicalCondition";

    private static final int GRADE_COLUMN_WIDTH = 30;

    private static final String INITIALIZE_KEY = "label.state.report.initialize";

    private static final String INPUT_PARAM_COND_OIDS = "condOids";
    private static final String INPUT_PARAM_END_DATE = "endDate";
    private static final String INPUT_PARAM_FORMAT_ID_CSV = "CSV_FORMAT_ID";
    private static final String INPUT_PARAM_FORMAT_ID_PDF = "PDF_FORMAT_ID";
    private static final String INPUT_PARAM_GRADES_OIDS = "gradesOids";
    private static final String INPUT_PARAM_SCHOOL_OIDS = "schoolOids";
    private static final String INPUT_PARAM_STATE_CODE_IND = "stateCondInd";
    private static final String INPUT_PARAM_SUBREPORT_ID_DETAIL = "SUBREPORT_DETAIL";
    private static final String INPUT_PARAM_SUBREPORT_ID_SUMMARY = "SUBREPORT_SUMMARY";

    private static final String JR_ELEMENT_REPORT_ELEMENT = "reportElement";
    private static final String JR_ELEMENT_STATIC_TEXT = "staticText";
    private static final String JR_ELEMENT_TEXT = "text";
    private static final String JR_ELEMENT_TEXT_FIELD = "textField";
    private static final String JR_ELEMENT_TEXT_ELEMENT = "textElement";
    private static final String JR_ELEMENT_TEXT_FIELD_EXPRESSION = "textFieldExpression";
    private static final String JR_KEY_TYPE_ANCHOR = "Anchor";
    private static final String JR_KEY_TYPE_SHIFTED = "Shifted";
    private static final String JR_MARKED_ELEMENT_AREA_COND = FIELD_MEDICAL_CONDITION;
    private static final String JR_MARKED_ELEMENT_AREA_COND_TYPE = FIELD_CONDITION_TYPE;
    private static final String JR_MARKED_ELEMENT_AREA_COND_TYPE_FOOTER = "conditionTypeFooter";
    private static final String JR_MARKED_ELEMENT_AREA_FOOTER = "footer";
    private static final String JR_MARKED_ELEMENT_AREA_HEADER = "header";
    private static final String JR_REPORT_ATTRIBUTE_ALIGNMENT = "textAlignment";
    private static final String JR_REPORT_ATTRIBUTE_EVALUATION_GROUP = "evaluationGroup";
    private static final String JR_REPORT_ATTRIBUTE_EVALUATION_TIME = "evaluationTime";
    private static final String JR_REPORT_ATTRIBUTE_KEY = "key";
    private static final String JR_REPORT_ATTRIBUTE_LEFT = "x";
    private static final String JR_REPORT_ATTRIBUTE_WIDTH = "width";
    private static final String JR_REPORT_ATT_VALUE_ALIGNMENT_RIGHT = "Right";
    private static final String JR_REPORT_ATT_VALUE_GROUP = "Group";
    private static final String JR_REPORT_ATT_VALUE_GROUP_COND = FIELD_MEDICAL_CONDITION;
    private static final String JR_REPORT_ATT_VALUE_GROUP_COND_TYPE = FIELD_CONDITION_TYPE;
    private static final String JR_VAR_POSTFIX_TOTAL = "total";
    private static final String JR_VAR_POSTFIX_TOTAL_TYPE = "typeTotal";

    private static final String NEW_COLUMN = "newColumn";

    private static final String PARAM_DISTRICT_SUMMARY = "districtSummary";
    private static final String PARAM_END_DATE = "endDate";
    private static final String PARAM_FORMAT_DETAIL = "formatDetail";
    private static final String PARAM_FORMAT_SUMMARY = "formatSummary";
    private static final String PARAM_GRADES = "selectedGrades";
    private static final String PARAM_ORGANIZATION = "organization";
    private static final String PARAM_SCHOOL_STD_MAP = "schoolStudentsMap";

    private boolean m_allSchoolSelected;
    private boolean m_conditionsSelected;
    private EnrollmentHelper m_data;
    private boolean m_districtSummary;
    private int m_formatType;
    private ReportDataGrid m_grid;
    private Report m_report;
    private boolean m_schoolSelected;
    private Collection<String> m_selectedConditions;
    private Collection<String> m_selectedGrades;
    private Collection<String> m_selectedSchools;
    private SimpleDateFormat m_simpleDateFormat = new SimpleDateFormat("MM/dd/yyyy");
    private Report m_subreportDetail;
    private Report m_subreportSummary;

    private List<String> m_validGrades =
            Arrays.asList("K4", "K", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12");

    /**
     * Gets the datasource.
     *
     * @return Report data grid
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    protected ReportDataGrid getDatasource() throws Exception {
        X2Criteria studentCriteria = m_data.getStudentCriteria();

        X2Criteria healthcriteria = new X2Criteria();
        SubQuery healthSubQuery = new SubQuery(HealthCondition.class, HealthCondition.COL_STUDENT_OID, healthcriteria);
        studentCriteria.addIn(X2BaseBean.COL_OID, healthSubQuery);
        if (m_schoolSelected) {
            X2Criteria enrollmentCriteria = new X2Criteria();
            enrollmentCriteria.addIn(StudentEnrollment.COL_SCHOOL_OID, m_selectedSchools);
            X2Criteria studentSubCriteriaFroEnr = m_data.getStudentCriteria().copy();
            enrollmentCriteria.addIn(StudentEnrollment.COL_STUDENT_OID,
                    new SubQuery(Student.class, X2BaseBean.COL_OID, studentSubCriteriaFroEnr));
            SubQuery enrollmentSubQuery =
                    new SubQuery(StudentEnrollment.class, StudentEnrollment.COL_STUDENT_OID, enrollmentCriteria);
            studentCriteria.addIn(X2BaseBean.COL_OID, enrollmentSubQuery);
        }
        QueryByCriteria studentQuery = new QueryByCriteria(SisStudent.class, studentCriteria);

        Map<String, SisStudent> students = getBroker().getMapByQuery(studentQuery, X2BaseBean.COL_OID, 1000);


        PlainDate endDate = (PlainDate) getParameter(PARAM_END_DATE);

        Map<String, String> studentGradeMap = new HashMap<String, String>();
        Map<String, String> studentSchoolNameMap = new HashMap<String, String>();

        for (SisStudent student : students.values()) {
            StudentEnrollment enrollment = m_data.getEnrollmentForDate(student, endDate, "EY");

            if (enrollment != null) {
                SisSchool school = m_data.getStudentSchool(enrollment);

                if (!m_schoolSelected || m_selectedSchools.contains(school.getOid())) {
                    String grade = m_data.getStudentGrade(enrollment);
                    studentGradeMap.put(student.getOid(), grade);
                    studentSchoolNameMap.put(student.getOid(), school.getName());
                }

            }
        }
        X2Criteria criteria = new X2Criteria();
        criteria.addIn(HealthCondition.COL_STUDENT_OID, studentSchoolNameMap.keySet());
        String[] columns = {HealthCondition.COL_STUDENT_OID, HealthCondition.COL_CONDITION_TYPE,
                HealthCondition.COL_CONDITION_CODE,
                HealthCondition.REL_STUDENT + ModelProperty.PATH_DELIMITER + SisStudent.COL_NAME_VIEW,
                HealthCondition.REL_STUDENT + ModelProperty.PATH_DELIMITER + SisStudent.COL_LOCAL_ID};
        ReportQueryByCriteria query = new ReportQueryByCriteria(HealthCondition.class, columns, criteria);

        RewindableReportDataGrid grid = new RewindableReportDataGrid();

        HashMap<String, HashSet<String>> schoolStudentsMap = new HashMap<String, HashSet<String>>();

        ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);

        try {
            while (iterator.hasNext()) {
                Object[] item = (Object[]) iterator.next();

                String studentOid = (String) item[0];
                String schoolName = studentSchoolNameMap.get(studentOid);
                String grade = studentGradeMap.get(studentOid);
                String conditionType = (String) item[1];
                String medicalCondition = (String) item[2];

                if (m_selectedConditions == null || m_selectedConditions.contains(medicalCondition)) {
                    grid.append();
                    grid.set(FIELD_SCHOOL_NAME, schoolName);
                    grid.set(FIELD_STUDENT_OID, studentOid);
                    grid.set(FIELD_GRADE, grade);
                    grid.set(FIELD_CONDITION_TYPE, conditionType == null ? "Other" : conditionType);
                    grid.set(FIELD_MEDICAL_CONDITION, medicalCondition == null ? "Other" : medicalCondition);

                    if (ToolInput.CSV_FORMAT == m_formatType) {
                        String studentName = (String) item[3];
                        String studentLasid = (String) item[4];

                        grid.set(FIELD_STUDENT_NAME, studentName);
                        grid.set(FIELD_STUDENT_LASID, studentLasid);

                        PlainDate entryDate = m_data.getEnrollmentDate(studentOid, getEndDate());
                        grid.set(FIELD_ENROLLMENT_DATE, m_simpleDateFormat.format(entryDate));
                    }

                    HashSet<String> studentsSet = schoolStudentsMap.get(studentSchoolNameMap.get(studentOid));
                    if (studentsSet == null) {
                        studentsSet = new HashSet<String>();
                        schoolStudentsMap.put(studentSchoolNameMap.get(studentOid), studentsSet);
                    }
                    studentsSet.add(studentOid);
                }
            }
        } finally {
            iterator.close();
        }

        if (ToolInput.CSV_FORMAT != m_formatType) {
            grid.sort(FIELD_MEDICAL_CONDITION, true, true);
            grid.sort(FIELD_CONDITION_TYPE, true, true);
            grid.sort(FIELD_SCHOOL_NAME, true, true);
        } else {
            grid.sort(FIELD_STUDENT_NAME, true, true);
            grid.sort(FIELD_MEDICAL_CONDITION, true, true);
            grid.sort(FIELD_CONDITION_TYPE, true, true);
            grid.sort(FIELD_GRADE, true, true);
            grid.sort(FIELD_SCHOOL_NAME, true, true);
        }

        grid.beforeTop();

        addParameter("rewindableDatasource", grid);
        addParameter(PARAM_SCHOOL_STD_MAP, schoolStudentsMap);

        return grid;
    }

    /**
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        ReportDataGrid grid = new ReportDataGrid();

        if (ToolInput.CSV_FORMAT != m_formatType) {
            if (m_grid.getRows().size() != 0) {
                grid.append();
                grid.beforeTop();
            }
        } else {
            grid = getDatasource();
        }

        return grid;
    }

    /**
     * Returns selected end date.
     *
     * @return PlainDate.
     */
    protected PlainDate getEndDate() {
        return (PlainDate) getParameter(INPUT_PARAM_END_DATE);
    }

    /**
     * Compile and use adjusted format.
     *
     * @param format Document
     * @return Input stream
     * @throws X2BaseException exception
     */
    protected InputStream getFormat(Document format) throws X2BaseException {
        byte[] compiledFormat = null;

        try {
            ReportCompiler reportCompiler = new ReportCompiler(getJob().getTempFolder(), getBroker());

            DOMImplementationLS domImplementationLS =
                    (DOMImplementationLS) format.getImplementation().getFeature("LS", "3.0");
            LSOutput lsOutput = domImplementationLS.createLSOutput();
            ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
            lsOutput.setByteStream(outputStream);
            LSSerializer lsSerializer = domImplementationLS.createLSSerializer();
            lsSerializer.write(format, lsOutput);

            compiledFormat = reportCompiler.compileReport(outputStream.toByteArray(), m_report.getEngineVersion(),
                    m_report.getJarPluginPath());
        } catch (JRException | TransformerFactoryConfigurationError | IOException | X2BaseException
                | dori.jasper.engine.JRException | net.sf.jasperreports.engine.JRException
                | net.sf.jasperreports5.engine.JRException | net.sf.jasperreports6.engine.JRException e) {
            throw new X2BaseException(e);
        }

        return new ByteArrayInputStream(compiledFormat);
    }

    /**
     * Returns collection of selected grades.
     *
     * @return Collection<String>
     */
    protected Collection<String> getGrades() {
        if (m_selectedGrades == null) {
            String gradesOidsString = (String) getParameter(INPUT_PARAM_GRADES_OIDS);
            Collection<String> refOids = null;

            if (!StringUtils.isEmpty(gradesOidsString)) {
                refOids = Arrays.asList(gradesOidsString.split(COMMA));
            }
            m_selectedGrades = getReferenceCodes("rtbGradeLevel", refOids, false);
        }
        return m_selectedGrades;
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        Collection<StateReportValidationError> initErrors = new ArrayList<StateReportValidationError>();
        try {
            m_data = new EnrollmentHelper();
            m_data.setBroker(getBroker());
            m_data.setOrganization(getOrganization());
            m_data.setPrivilegeSet(getPrivilegeSet());
            m_data.setSchoolContext(false);
            m_data.setParameters(getParameters());
            m_data.setUser(getUser());
            m_data.initializeExport();
            initErrors.addAll(m_data.getSetupErrors());
        } catch (X2BaseException x2be) {
            String init_msg = LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale())
                    .getMessage(INITIALIZE_KEY);
            initErrors.add(new StateReportValidationError(init_msg, init_msg, init_msg, x2be.getMessage()));
        }

        m_conditionsSelected = !StringUtils.isEmpty((String) getParameter(INPUT_PARAM_COND_OIDS));
        m_schoolSelected = !StringUtils.isEmpty((String) getParameter(INPUT_PARAM_SCHOOL_OIDS));

        ToolJob job = this.getJob();
        m_formatType = job.getInput().getFormat();

        if (m_schoolSelected) {
            m_selectedSchools = Arrays.asList(((String) getParameter(INPUT_PARAM_SCHOOL_OIDS)).split(COMMA));
        }

        if (m_schoolSelected) {
            X2Criteria activeSchoolsCriteria = new X2Criteria();
            activeSchoolsCriteria.addNotEqualTo(SisSchool.COL_INACTIVE_INDICATOR, Boolean.valueOf(true));
            activeSchoolsCriteria.addNotEqualTo(SisSchool.COL_ARCHIVE_INDICATOR, Boolean.valueOf(true));
            QueryByCriteria activeSchoolsQuery = new QueryByCriteria(SisSchool.class, activeSchoolsCriteria);
            Collection<SisSchool> activeSchools = getBroker().getCollectionByQuery(activeSchoolsQuery);
            if (activeSchools.size() == m_selectedSchools.size()) {
                m_allSchoolSelected = true;
            }
        } else {
            m_allSchoolSelected = true;
        }

        if (m_allSchoolSelected) {
            m_districtSummary = true;
        }

        String conditionsOidsString = (String) getParameter(INPUT_PARAM_COND_OIDS);

        boolean withStateCode = ((Boolean) getParameter(INPUT_PARAM_STATE_CODE_IND)).booleanValue();
        Collection<String> selectedConditionsOids =
                !m_conditionsSelected ? null : Arrays.asList(conditionsOidsString.split(COMMA));
        if (m_conditionsSelected || withStateCode) {
            m_selectedConditions = getReferenceCodes("rtbConditions", selectedConditionsOids, withStateCode);
        }

        initReportsFormat();

        m_report = ReportUtils.getReport(getFormatID(), getBroker());

        addParameter(PARAM_DISTRICT_SUMMARY, Boolean.valueOf(m_districtSummary));
        addParameter(PARAM_GRADES, getGrades());
        addParameter(PARAM_ORGANIZATION, getOrganization());
        addParameter(PARAM_END_DATE, getEndDate());
        addParameter(PARAM_FORMAT_DETAIL, getFormat(getAdjustedFormat(m_subreportDetail.getFormat())));
        addParameter(PARAM_FORMAT_SUMMARY, getFormat(getAdjustedFormat(m_subreportSummary.getFormat())));

        super.initialize();

        try {
            m_grid = getDatasource();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * Add selected grades dynamically and return adjusted format.
     *
     * @param format String
     * @return Document
     */
    private Document getAdjustedFormat(String format) {
        Document formatDoc = null;
        try {
            DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
            dbf.setNamespaceAware(true);
            dbf.setFeature("http://apache.org/xml/features/nonvalidating/load-dtd-grammar", false);
            dbf.setFeature("http://apache.org/xml/features/nonvalidating/load-external-dtd", false);

            DocumentBuilder builder = dbf.newDocumentBuilder();
            formatDoc = builder.parse(new ByteArrayInputStream(format.getBytes()));
            Element root = formatDoc.getDocumentElement();

            NodeList reportElements = root.getElementsByTagName(JR_ELEMENT_REPORT_ELEMENT);
            /*
             * Find Anchors nodes.
             */
            Map<String, Node> bindAnchorMap = initBindNodeMap(reportElements, JR_KEY_TYPE_ANCHOR);

            /*
             * Find Shifted nodes.
             */

            Map<String, Node> bindShiftedMap = initBindNodeMap(reportElements, JR_KEY_TYPE_SHIFTED);
            /*
             * Insert grades.
             */
            for (String grade : m_validGrades) {
                if (!getGrades().contains(grade)) {
                    continue;
                }

                for (Entry<String, Node> entry : bindAnchorMap.entrySet()) {
                    String bind = entry.getKey();
                    Node anchor = entry.getValue();

                    String newColumnPrefix = NEW_COLUMN + bind;

                    Node newElement = anchor.cloneNode(true);
                    NodeList newElementChildren = newElement.getChildNodes();

                    for (int k = 0; k < newElementChildren.getLength(); k++) {
                        Node newElementChild = newElementChildren.item(k);
                        if (attributeKeyContains(newElementChild, JR_KEY_TYPE_ANCHOR)) {
                            newElementChild.getAttributes().getNamedItem(JR_REPORT_ATTRIBUTE_KEY)
                                    .setTextContent(newColumnPrefix + "0");
                        }
                    }

                    Node bindNode = anchor.getParentNode();
                    bindNode.insertBefore(newElement, bindShiftedMap.get(bind));

                    /*
                     * Get element that before new added element to determine correct position and
                     * key of the new element.
                     */
                    String reportElementContainerName = null;
                    String textContainerName = null;
                    switch (bind) {
                        case JR_MARKED_ELEMENT_AREA_HEADER:
                            reportElementContainerName = JR_ELEMENT_STATIC_TEXT;
                            textContainerName = JR_ELEMENT_TEXT;
                            break;
                        case JR_MARKED_ELEMENT_AREA_COND_TYPE:
                            reportElementContainerName = JR_ELEMENT_STATIC_TEXT;
                            textContainerName = JR_ELEMENT_TEXT;
                            break;
                        case JR_MARKED_ELEMENT_AREA_COND_TYPE_FOOTER:
                            reportElementContainerName = JR_ELEMENT_TEXT_FIELD;
                            textContainerName = JR_ELEMENT_TEXT_FIELD_EXPRESSION;
                            break;
                        case JR_MARKED_ELEMENT_AREA_COND:
                            reportElementContainerName = JR_ELEMENT_TEXT_FIELD;
                            textContainerName = JR_ELEMENT_TEXT_FIELD_EXPRESSION;
                            break;
                        case JR_MARKED_ELEMENT_AREA_FOOTER:
                            reportElementContainerName = JR_ELEMENT_TEXT_FIELD;
                            textContainerName = JR_ELEMENT_TEXT_FIELD_EXPRESSION;
                            break;
                        default:
                            break;
                    }

                    NodeList prevElementChildren =
                            getPreviousElementByName(newElement, reportElementContainerName).getChildNodes();
                    Node prevReportElement = getNodeFromNodeList(JR_ELEMENT_REPORT_ELEMENT, prevElementChildren);

                    int attPrevLeft = Integer.parseInt(
                            prevReportElement.getAttributes().getNamedItem(JR_REPORT_ATTRIBUTE_LEFT).getTextContent());
                    int attPrevWidth = Integer.parseInt(
                            prevReportElement.getAttributes().getNamedItem(JR_REPORT_ATTRIBUTE_WIDTH).getTextContent());
                    String attPrevKey =
                            prevReportElement.getAttributes().getNamedItem(JR_REPORT_ATTRIBUTE_KEY).getTextContent();

                    Node newReportElement = getNodeFromNodeList(JR_ELEMENT_REPORT_ELEMENT, newElementChildren);
                    if (attPrevKey.startsWith(NEW_COLUMN)) {
                        int numOfColumn = Integer.parseInt(attPrevKey.replace(newColumnPrefix, EMPTY_STRING));
                        newReportElement.getAttributes().getNamedItem(JR_REPORT_ATTRIBUTE_KEY)
                                .setTextContent(newColumnPrefix + String.valueOf(++numOfColumn));
                    }
                    newReportElement.getAttributes().getNamedItem(JR_REPORT_ATTRIBUTE_LEFT)
                            .setTextContent(String.valueOf(attPrevLeft + attPrevWidth));
                    newReportElement.getAttributes().getNamedItem(JR_REPORT_ATTRIBUTE_WIDTH)
                            .setTextContent(String.valueOf(GRADE_COLUMN_WIDTH));
                    Node newFieldExpresElement = getNodeFromNodeList(textContainerName, newElementChildren);

                    Node textElement = null;

                    switch (bind) {
                        case JR_MARKED_ELEMENT_AREA_HEADER:
                            newFieldExpresElement.setTextContent(grade);
                            break;
                        case JR_MARKED_ELEMENT_AREA_COND_TYPE:
                            break;
                        case JR_MARKED_ELEMENT_AREA_COND_TYPE_FOOTER:
                            newFieldExpresElement.setTextContent(wrapAsVariable(grade + JR_VAR_POSTFIX_TOTAL_TYPE));

                            textElement = getNodeFromNodeList(JR_ELEMENT_TEXT_ELEMENT, newElementChildren);

                            ((Element) textElement).setAttribute(JR_REPORT_ATTRIBUTE_ALIGNMENT,
                                    JR_REPORT_ATT_VALUE_ALIGNMENT_RIGHT);

                            ((Element) newElement).setAttribute(JR_REPORT_ATTRIBUTE_EVALUATION_TIME,
                                    JR_REPORT_ATT_VALUE_GROUP);
                            ((Element) newElement).setAttribute(JR_REPORT_ATTRIBUTE_EVALUATION_GROUP,
                                    JR_REPORT_ATT_VALUE_GROUP_COND_TYPE);
                            break;
                        case JR_MARKED_ELEMENT_AREA_COND:
                            newFieldExpresElement.setTextContent(wrapAsVariable(grade));

                            textElement = getNodeFromNodeList(JR_ELEMENT_TEXT_ELEMENT, newElementChildren);

                            ((Element) textElement).setAttribute(JR_REPORT_ATTRIBUTE_ALIGNMENT,
                                    JR_REPORT_ATT_VALUE_ALIGNMENT_RIGHT);

                            ((Element) newElement).setAttribute(JR_REPORT_ATTRIBUTE_EVALUATION_TIME,
                                    JR_REPORT_ATT_VALUE_GROUP);
                            ((Element) newElement).setAttribute(JR_REPORT_ATTRIBUTE_EVALUATION_GROUP,
                                    JR_REPORT_ATT_VALUE_GROUP_COND);
                            break;
                        case JR_MARKED_ELEMENT_AREA_FOOTER:

                            textElement = getNodeFromNodeList(JR_ELEMENT_TEXT_ELEMENT, newElementChildren);

                            ((Element) textElement).setAttribute(JR_REPORT_ATTRIBUTE_ALIGNMENT,
                                    JR_REPORT_ATT_VALUE_ALIGNMENT_RIGHT);

                            newFieldExpresElement.setTextContent(wrapAsVariable(grade + JR_VAR_POSTFIX_TOTAL));
                            getNodeFromNodeList(JR_ELEMENT_REPORT_ELEMENT, newElementChildren).getAttributes()
                                    .getNamedItem("mode").setTextContent("Transparent");
                            break;

                        default:
                            break;
                    }
                    /*
                     * Shift shifted
                     */
                    Node shiftedReportElement =
                            getNodeFromNodeList(JR_ELEMENT_REPORT_ELEMENT, bindShiftedMap.get(bind).getChildNodes());
                    int attShiftedLeft = Integer.parseInt(shiftedReportElement.getAttributes()
                            .getNamedItem(JR_REPORT_ATTRIBUTE_LEFT).getTextContent());
                    shiftedReportElement.getAttributes().getNamedItem(JR_REPORT_ATTRIBUTE_LEFT)
                            .setTextContent(String.valueOf(attShiftedLeft + GRADE_COLUMN_WIDTH));
                }
            }
        } catch (ParserConfigurationException | SAXException | IOException e) {
            e.printStackTrace();
        }

        return formatDoc;
    }

    /**
     * Return true if the node has attribute key and the key contains passed keyType.
     *
     * @param node Node
     * @param keyType String
     * @return boolean
     */
    private boolean attributeKeyContains(Node node, String keyType) {
        boolean attributeKeyContains = false;
        if (node.getAttributes() != null && node.getAttributes().getNamedItem(JR_REPORT_ATTRIBUTE_KEY) != null) {
            attributeKeyContains =
                    node.getAttributes().getNamedItem(JR_REPORT_ATTRIBUTE_KEY).getTextContent().contains(keyType);
        }
        return attributeKeyContains;
    }

    /**
     * Return last node with passed node name from the node list.
     *
     * @param nodeName String
     * @param nodeList NodeList
     * @return Node
     */
    private Node getNodeFromNodeList(String nodeName, NodeList nodeList) {
        Node reportElement = null;
        for (int k = 0; k < nodeList.getLength(); k++) {
            if (nodeList.item(k).getNodeName().equals(nodeName)) {
                reportElement = nodeList.item(k);
            }
        }

        return reportElement;
    }

    /**
     * Returns previous element that before passed node and with passed name.
     *
     * @param newNode Node
     * @param name String
     * @return Node
     */
    private Node getPreviousElementByName(Node newNode, String name) {
        Node currentNode = newNode.getPreviousSibling();

        while (!name.equals(currentNode.getNodeName())) {
            currentNode = currentNode.getPreviousSibling();
        }
        if (!name.equals(currentNode.getNodeName())) {
            currentNode = null;
        }
        return currentNode;
    }

    /**
     * Returns the collection of code values corresponding to a list of reference code oids.
     *
     * @param rtbOid String
     * @param oids Collection<String>
     * @param withStateCode boolean
     * @return Collection<String>
     */
    private Collection<String> getReferenceCodes(String rtbOid, Collection<String> oids, boolean withStateCode) {
        Collection<String> codes = new HashSet<String>();

        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ReferenceCode.REL_REFERENCE_TABLE + ModelProperty.PATH_DELIMITER +
                X2BaseBean.COL_OID, rtbOid);
        if (oids != null) {
            criteria.addIn(X2BaseBean.COL_OID, oids);
        }
        QueryByCriteria rcdQuery = new QueryByCriteria(ReferenceCode.class, criteria);
        Collection<ReferenceCode> refCodes = getBroker().getCollectionByQuery(rcdQuery);

        for (ReferenceCode code : refCodes) {
            if (!withStateCode || !StringUtils.isEmpty(code.getStateCode())) {
                codes.add(code.getCode());
            }
        }
        return codes;
    }

    /**
     * Initialize map of nodes based on passed type and using passed list of elements.
     * We need determine concrete nodes that should be used in dynamically added format, e.g. to
     * determine
     * start position where we should add new nodes in the export format should be node containing
     * "Anchor" in
     * report element key, etc.
     *
     * @param reportElements NodeList
     * @param keyType String
     * @return Map<String, Node>
     */
    private Map<String, Node> initBindNodeMap(NodeList reportElements, String keyType) {
        Map<String, Node> bindNodeMap = new HashMap<String, Node>();
        for (int i = 0; i < reportElements.getLength(); i++) {
            Node reportElement = reportElements.item(i);
            if (attributeKeyContains(reportElement, keyType)) {
                Node container = reportElement.getParentNode();
                String bind = reportElement.getAttributes().getNamedItem(JR_REPORT_ATTRIBUTE_KEY).getTextContent()
                        .replace(keyType, EMPTY_STRING);

                bindNodeMap.put(bind, container);
            }
        }
        return bindNodeMap;
    }

    /**
     * Initialize report formats.
     */
    private void initReportsFormat() {
        m_subreportDetail = ReportUtils.getReport((String) getParameter(INPUT_PARAM_SUBREPORT_ID_DETAIL), getBroker());
        m_subreportSummary =
                ReportUtils.getReport((String) getParameter(INPUT_PARAM_SUBREPORT_ID_SUMMARY), getBroker());

        String formatPDF = (String) getParameter(INPUT_PARAM_FORMAT_ID_PDF);
        String formatCSV = (String) getParameter(INPUT_PARAM_FORMAT_ID_CSV);

        switch (m_formatType) {
            case ToolInput.CSV_FORMAT:
                this.setFormatId(formatCSV);
                break;
            case ToolInput.HTML_FORMAT:
                this.setFormatId(formatPDF);
                break;
            case ToolInput.PDF_FORMAT:
                this.setFormatId(formatPDF);
                break;
            case ToolInput.XLS_FORMAT:
                this.setFormatId(formatPDF);
                break;
        }
    }

    /**
     * Wrap passed name as variable of jasper report format.
     *
     * @param variableName String
     * @return String
     */
    private String wrapAsVariable(String variableName) {
        return "$V{" + variableName + "}";
    }
}
