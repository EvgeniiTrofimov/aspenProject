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

package com.x2dev.reports.statereporting.pa;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.ToolInput;
import com.follett.fsc.core.k12.tools.ToolJob;
import com.follett.fsc.core.k12.tools.reports.ReportCompiler;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.HealthLog;
import com.x2dev.sis.model.beans.HealthLogComplaint;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
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
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.ls.DOMImplementationLS;
import org.w3c.dom.ls.LSOutput;
import org.w3c.dom.ls.LSSerializer;
import org.xml.sax.SAXException;

/**
 * Prepares the data for the Health Log Statistics.
 *
 * @author Follett Software Company
 */
public class HealthLogStatistics extends PAPrintInitialErrorsReport {
    /**
     * Iterface to get field value of group with repeated value.
     *
     * @author Follett Software Company
     */
    protected interface GroupFieldRetriever {

        /**
         * Gets the group field value.
         *
         * @param healthLog HealthLog
         * @return String
         */
        public String getGroupFieldValue(HealthLog healthLog);
    }

    /**
     * Abstract class to implement the functionality for each data group.
     *
     * @author Follett Software Company
     */
    abstract static protected class DataGroupValuesRetriever {

        /**
         * Gets the group elements.
         *
         * @param healthLog HealthLog
         * @param referenceCodes Map<String,ReferenceCode>
         * @return Map
         */
        abstract public Map<String, String> getGroupElements(HealthLog healthLog,
                                                             Map<String, ReferenceCode> referenceCodes);

        /**
         * Gets the ref table oid.
         *
         * @param dictionary DataDictionary
         * @return String
         */
        abstract public String getRefTableOid(DataDictionary dictionary);
    }

    /**
     * Data groups with appropriate functionality to get group elements.
     *
     * @author Follett Software Company
     */
    enum LogStatisticsDataGroup {
        COMPLAINT("Complaint", new DataGroupValuesRetriever() {
            @Override
            public Map<String, String> getGroupElements(HealthLog healthLog,
                                                        Map<String, ReferenceCode> referenceCodes) {
                Map<String, String> complaintCodes = new HashMap<String, String>();

                Collection<HealthLogComplaint> complaints = healthLog.getComplaints();
                for (HealthLogComplaint complaint : complaints) {
                    complaintCodes.put(complaint.getOid(), complaint.getComplaintCode());
                }

                return complaintCodes;
            }

            /**
             * Needed to show Description + State code for Location and Time Period groups
             *
             * @
             */
            @Override
            public String getRefTableOid(DataDictionary dictionary) {
                return null;
            }

        }),

        TIME_PERIOD("Time Period", new DataGroupValuesRetriever() {
            @Override
            public Map<String, String> getGroupElements(HealthLog healthLog,
                                                        Map<String, ReferenceCode> referenceCodes) {
                return getRepeatedValues(new GroupFieldRetriever() {

                    @Override
                    public String getGroupFieldValue(HealthLog healthLogForRetriever) {
                        return (String) healthLogForRetriever.getFieldValueByAlias(ALIAS_TIME_PERIOD);
                    }

                }, healthLog, referenceCodes);
            }

            @Override
            public String getRefTableOid(DataDictionary dictionary) {
                return dictionary.findDataDictionaryFieldByAlias(ALIAS_TIME_PERIOD).getReferenceTableOid();
            }
        }),

        LOCATION("Location", new DataGroupValuesRetriever() {
            @Override
            public Map<String, String> getGroupElements(HealthLog healthLog,
                                                        Map<String, ReferenceCode> referenceCodes) {
                return getRepeatedValues(new GroupFieldRetriever() {

                    @Override
                    public String getGroupFieldValue(HealthLog healthLogForRetriever) {
                        return healthLogForRetriever.getLocationCode();
                    }

                }, healthLog, referenceCodes);
            }

            @Override
            public String getRefTableOid(DataDictionary dictionary) {
                return dictionary.findDataDictionaryField(HealthLog.class.getName(),
                        HealthLog.COL_LOCATION_CODE).getReferenceTableOid();
            }

        });

        /**
         * Instantiates a new log statistics data group.
         *
         * @param groupName String
         * @param retriever DataGroupValuesRetriever
         */
        private LogStatisticsDataGroup(String groupName, DataGroupValuesRetriever retriever) {
            m_groupName = groupName;
            m_retriever = retriever;
        }

        private String m_groupName = null;
        private DataGroupValuesRetriever m_retriever = null;

        /**
         * Get group elements.
         *
         * @param healthLog HealthLog
         * @param referenceCodes Map<String,ReferenceCode>
         * @return String
         */
        public Map<String, String> getGroupElements(HealthLog healthLog, Map<String, ReferenceCode> referenceCodes) {
            return m_retriever.getGroupElements(healthLog, referenceCodes);
        }

        /**
         * Get group name.
         *
         * @return String
         */
        public String getName() {
            return m_groupName;
        }

        /**
         * Needed to show Description + State code for Location and Time Period groups.
         *
         * @param dictionary DataDictionary
         * @param broker X2Broker
         * @return Map<String, ReferenceCode>
         */
        public Map<String, ReferenceCode> getReferenceCodes(DataDictionary dictionary, X2Broker broker) {
            return getReferenceCodes(m_retriever.getRefTableOid(dictionary), broker);
        }

        /**
         * Returns map of repeaded values by complaint codes.
         *
         * @param groupFieldRetriever GroupFieldRetriever
         * @param healthLog HealthLog
         * @param referenceCodes Map<String,ReferenceCode>
         * @return Map<String, String>
         */
        static protected Map<String, String> getRepeatedValues(GroupFieldRetriever groupFieldRetriever,
                                                               HealthLog healthLog,
                                                               Map<String, ReferenceCode> referenceCodes) {
            String value = groupFieldRetriever.getGroupFieldValue(healthLog);

            Map<String, String> values = new HashMap<String, String>();

            StringBuilder printableValue = new StringBuilder();
            if (!StringUtils.isEmpty(value)) {
                ReferenceCode refCode = referenceCodes.get(value);
                if (refCode != null) {
                    printableValue.append(refCode.getDescription()).append(" ").append(refCode.getStateCode());
                }
            }

            Collection<HealthLogComplaint> complaints = healthLog.getComplaints();
            for (HealthLogComplaint complaint : complaints) {
                values.put(complaint.getOid(), printableValue.length() == 0 ? value : printableValue.toString());
            }

            return values;
        }

        /**
         * Returns map of reference codes by codes.
         *
         * @param rtbOid String
         * @param broker X2Broker
         * @return Collection<String>
         */
        private Map<String, ReferenceCode> getReferenceCodes(String rtbOid, X2Broker broker) {
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, rtbOid);
            QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, criteria);

            return broker.getMapByQuery(query, ReferenceCode.COL_CODE, 20);
        }
    }

    /**
     * Rewindable report data grid. Needed bacause the same datasource is used for subreports.
     *
     * @author Follett Software Company
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
         * Rewind and resort datasource for next subreport.
         *
         * @throws JRException exception
         * @see net.sf.jasperreports3.engine.JRRewindableDataSource#moveFirst()
         */
        @Override
        public void moveFirst() throws JRException {
            this.sort(FIELD_GRADE, true);
            this.sort(FIELD_ELEMENT_CODE, true);
            this.sort(FIELD_HEALTH_LOG_VISIT_TYPE, true);
            this.sort(FIELD_GROUP_NAME, true);
            this.sort(FIELD_GROUP_COUNTER, true);
            beforeTop();
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
         * Returns student criteria.
         *
         * @return X2Criteria
         */
        public X2Criteria getStudentCriteria() {
            return m_studentHelper.getStudentCriteria();
        }

        /**
         * Return student's grade on passed date.
         *
         * @param student SisStudent
         * @param date PlainDate
         * @return X2Criteria
         */
        public String getStudentGradeOnDate(SisStudent student, PlainDate date) {
            StudentEnrollment enrollment = m_studentHelper.getEnrollmentForDate(student.getOid(), date, "EW");
            int yog = enrollment.getYog();
            if (yog == 0) {
                yog = student.getYog();
            }

            if (StudentEnrollment.WITHDRAWAL.equals(enrollment.getEnrollmentType())) {
                date = enrollment.getEnrollmentDate();
            }

            ReferenceCode gradeCode = getGradeLevel(yog, getSchoolYear(date));

            return gradeCode == null ? null : gradeCode.getCode();
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
            m_studentHelper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
            m_studentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE, getStartDate());
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
         * Load contexts to determine school year for grade calculation.
         * Contexts are restricted by selected date range.
         *
         * @return Collection
         */
        private Collection<DistrictSchoolYearContext> loadContexts() {
            X2Criteria ctxsCriteria = new X2Criteria();
            ctxsCriteria.addLessOrEqualThan(DistrictSchoolYearContext.COL_START_DATE, getEndDate());
            ctxsCriteria.addGreaterOrEqualThan(DistrictSchoolYearContext.COL_END_DATE, getStartDate());

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

    private static final String ALIAS_TIME_PERIOD = "all-hlg-TimePeriod";
    private static final String ALIAS_SERIOUS_INJURY = "all-hlg-SeriousSchoolInjury";

    private static final String COMMA = ",";
    private static final String EMPTY_STRING = "";

    private static final String FIELD_COMPLAINT_OID = "complaintOid";
    private static final String FIELD_ELEMENT_CODE = "elementCode";
    private static final String FIELD_GROUP_COUNTER = "groupCounter";
    private static final String FIELD_GROUP_NAME = "groupName";
    private static final String FIELD_GRADE = "grade";
    private static final String FIELD_HEALTH_LOG_COMPLAINT = "complaint";
    private static final String FIELD_HEALTH_LOG_DATE = "date";
    private static final String FIELD_HEALTH_LOG_LOCATION = "location";
    private static final String FIELD_HEALTH_LOG_TIME_PERIOD = "timePeriod";
    private static final String FIELD_HEALTH_LOG_VISIT_TYPE = "visitType";
    private static final String FIELD_STUDENT_LASID = "lasid";
    private static final String FIELD_SCHOOL_NAME = "school";
    private static final String FIELD_SERIOUS_INJURY = "seriousInjury";
    private static final String FIELD_STUDENT = "student";
    private static final String FIELD_STUDENT_NAME = "studentName";
    private static final String FIELD_STUDENT_OID = "studentOid";

    private static final int GRADE_COLUMN_WIDTH = 30;

    private static final String INITIALIZE_KEY = "label.state.report.initialize";

    private static final String INPUT_PARAM_BEGIN_DATE = "beginDate";
    private static final String INPUT_PARAM_END_DATE = "endDate";
    private static final String INPUT_PARAM_FORMAT_ID_CSV = "CSV_FORMAT_ID";
    private static final String INPUT_PARAM_FORMAT_ID_PDF = "PDF_FORMAT_ID";
    private static final String INPUT_PARAM_GRADES_OIDS = "gradesOids";
    private static final String INPUT_PARAM_SCHOOL_OIDS = "schoolOids";
    private static final String INPUT_PARAM_SERIOUS_INJ = "seriousInjuries";
    private static final String INPUT_PARAM_SUBREPORT_ID_DETAIL = "SUBREPORT_DETAIL";
    private static final String INPUT_PARAM_SUBREPORT_ID_SUMMARY = "SUBREPORT_SUMMARY";
    private static final String INPUT_PARAM_VISIT_TYPES = "visitTypes";

    private static final String JR_ELEMENT_REPORT_ELEMENT = "reportElement";
    private static final String JR_ELEMENT_STATIC_TEXT = "staticText";
    private static final String JR_ELEMENT_TEXT = "text";
    private static final String JR_ELEMENT_TEXT_FIELD = "textField";
    private static final String JR_ELEMENT_TEXT_ELEMENT = "textElement";
    private static final String JR_ELEMENT_TEXT_FIELD_EXPRESSION = "textFieldExpression";
    private static final String JR_KEY_TYPE_ANCHOR = "Anchor";
    private static final String JR_KEY_TYPE_SHIFTED = "Shifted";
    private static final String JR_MARKED_ELEMENT_AREA_ELEMENT_CODE = FIELD_ELEMENT_CODE;
    private static final String JR_MARKED_ELEMENT_AREA_GROUP_NAME = FIELD_GROUP_NAME;
    private static final String JR_MARKED_ELEMENT_AREA_VISIT_TYPE = FIELD_HEALTH_LOG_VISIT_TYPE;
    private static final String JR_MARKED_ELEMENT_AREA_FOOTER = "footer";
    private static final String JR_REPORT_ATTRIBUTE_ALIGNMENT = "textAlignment";
    private static final String JR_REPORT_ATTRIBUTE_EVALUATION_GROUP = "evaluationGroup";
    private static final String JR_REPORT_ATTRIBUTE_EVALUATION_TIME = "evaluationTime";
    private static final String JR_REPORT_ATTRIBUTE_KEY = "key";
    private static final String JR_REPORT_ATTRIBUTE_LEFT = "x";
    private static final String JR_REPORT_ATTRIBUTE_WIDTH = "width";
    private static final String JR_REPORT_ATT_VALUE_ALIGNMENT_RIGHT = "Right";
    private static final String JR_REPORT_ATT_VALUE_GROUP = "Group";
    private static final String JR_REPORT_ATT_VALUE_ELEMENT_CODE = FIELD_ELEMENT_CODE;
    private static final String JR_TOTAL_VAR_POSTFIX = "total";

    private static final String NEW_COLUMN = "newColumn";

    private static final String PARAM_DISTRICT_SUMMARY = "districtSummary";
    private static final String PARAM_END_DATE = "endDate";
    private static final String PARAM_FORMAT_DETAIL = "formatDetail";
    private static final String PARAM_FORMAT_SUMMARY = "formatSummary";
    private static final String PARAM_GRADES = "selectedGrades";
    private static final String PARAM_ORGANIZATION = "organization";
    private static final String PARAM_SERIOUS_INJURY = "withSeriousInjury";
    private static final String PARAM_START_DATE = "startDate";

    DataDictionary m_dictionary;
    Boolean m_withSeriousInjuries;

    private boolean m_allSchoolSelected;
    private EnrollmentHelper m_data;
    private Map<String, Map<String, ReferenceCode>> m_dataRefCodes;
    private SimpleDateFormat m_dateFormat;
    private boolean m_districtSummary;
    private String m_fieldSeriousInjury;
    private ReportDataGrid m_grid;
    private Report m_report;
    private int m_reportFormatType;
    private Map<String, SisSchool> m_schoolMap;
    private boolean m_schoolsSelected = false;
    private Collection<String> m_selectedGrades;
    private Map<String, SisStudent> m_studentMap;

    private Report m_subreportDetail;
    private Report m_subreportSummary;

    private List<String> m_validGrades =
            Arrays.asList("K4", "K", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12");
    private boolean m_visitTypeSelected = false;

    /**
     * @see com.x2dev.reports.statereporting.pa.PAPrintInitialErrorsReport#reportGatherData()
     */
    @Override
    protected Object reportGatherData() throws Exception {
        ReportDataGrid grid = new ReportDataGrid();

        if (ToolInput.CSV_FORMAT != m_reportFormatType) {
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
     * Gets the datasource.
     *
     * @return Report data grid
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    protected ReportDataGrid getDatasource() {
        RewindableReportDataGrid grid = new RewindableReportDataGrid();

        Collection<HealthLog> healthLogs = getHealthLogs();

        int groupCounter = 0;

        if (ToolInput.CSV_FORMAT == m_reportFormatType) {
            groupCounter++;
            addGroupRecords(grid, healthLogs, groupCounter, LogStatisticsDataGroup.COMPLAINT);

            grid.sort(FIELD_HEALTH_LOG_COMPLAINT, true);
            grid.sort(FIELD_STUDENT_NAME, true);
            grid.sort(FIELD_SCHOOL_NAME, true);
        } else {
            for (LogStatisticsDataGroup group : LogStatisticsDataGroup.values()) {
                groupCounter++;
                addGroupRecords(grid, healthLogs, groupCounter, group);
            }

            grid.sort(FIELD_GRADE, true);
            grid.sort(FIELD_ELEMENT_CODE, true);
            grid.sort(FIELD_HEALTH_LOG_VISIT_TYPE, true);
            grid.sort(FIELD_GROUP_NAME, true);
            grid.sort(FIELD_GROUP_COUNTER, true);
            grid.sort(FIELD_SCHOOL_NAME, true);
        }

        grid.beforeTop();

        addParameter("rewindableDatasource", grid);

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
                | net.sf.jasperreports5.engine.JRException |
                net.sf.jasperreports6.engine.JRException e) {
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

            m_selectedGrades = getCodes("rtbGradeLevel", refOids, false);
        }
        return m_selectedGrades;
    }

    /**
     * Returns selected start date.
     *
     * @return PlainDate
     */
    protected PlainDate getStartDate() {
        return (PlainDate) getParameter(INPUT_PARAM_BEGIN_DATE);
    }

    /**
     * @see com.x2dev.reports.statereporting.pa.PAPrintInitialErrorsReport#initialRequiredResources()
     */
    @Override
    protected void initialRequiredResources() {
        requiredAlias(ALIAS_TIME_PERIOD);
        requiredAlias(ALIAS_SERIOUS_INJURY);
    }

    /**
     * Report initialize.
     *
     * @throws X2BaseException exception
     * @see PAPrintInitialErrorsReport#reportInitialize()
     */
    @Override
    protected void reportInitialize() throws X2BaseException {
        m_withSeriousInjuries = (Boolean) getParameter(INPUT_PARAM_SERIOUS_INJ);

        m_dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());

        m_dateFormat = new SimpleDateFormat("MM/dd/yyyy");

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

        m_schoolsSelected = !StringUtils.isEmpty((String) getParameter(INPUT_PARAM_SCHOOL_OIDS));
        m_visitTypeSelected = !StringUtils.isEmpty((String) getParameter(INPUT_PARAM_VISIT_TYPES));

        X2Criteria schoolCriteria = new X2Criteria();
        if (m_schoolsSelected) {
            schoolCriteria.addIn(X2BaseBean.COL_OID,
                    Arrays.asList(((String) getParameter(INPUT_PARAM_SCHOOL_OIDS)).split(COMMA)));
        }

        m_schoolMap =
                getBroker().getMapByQuery(new QueryByCriteria(SisSchool.class, schoolCriteria), X2BaseBean.COL_OID, 5);

        if (m_schoolsSelected) {
            X2Criteria activeSchoolsCriteria = new X2Criteria();
            activeSchoolsCriteria.addNotEqualTo(SisSchool.COL_INACTIVE_INDICATOR, Boolean.valueOf(true));
            activeSchoolsCriteria.addNotEqualTo(SisSchool.COL_ARCHIVE_INDICATOR, Boolean.valueOf(true));
            QueryByCriteria activeSchoolsQuery = new QueryByCriteria(SisSchool.class, activeSchoolsCriteria);
            Collection<SisSchool> activeSchools = getBroker().getCollectionByQuery(activeSchoolsQuery);
            if (activeSchools.size() == m_schoolMap.size()) {
                m_allSchoolSelected = true;
            }
        } else {
            m_allSchoolSelected = true;
        }

        if (m_allSchoolSelected) {
            m_districtSummary = true;
        }

        m_studentMap = getBroker().getMapByQuery(new QueryByCriteria(SisStudent.class, m_data.getStudentCriteria()),
                SisStudent.REL_PERSON + ModelProperty.PATH_DELIMITER + X2BaseBean.COL_OID, 100);

        initializeFields();

        initReportsFormat();

        m_report = ReportUtils.getReport(getFormatID(), getBroker());

        addParameter(PARAM_DISTRICT_SUMMARY, Boolean.valueOf(m_districtSummary));
        addParameter(PARAM_GRADES, getGrades());
        addParameter(PARAM_ORGANIZATION, getOrganization());
        addParameter(PARAM_START_DATE, getStartDate());
        addParameter(PARAM_END_DATE, getEndDate());
        addParameter(PARAM_SERIOUS_INJURY, m_withSeriousInjuries);
        if (ToolInput.CSV_FORMAT != m_reportFormatType) {
            addParameter(PARAM_FORMAT_DETAIL, getFormat(getAdjustedFormat(m_subreportDetail.getFormat())));
            addParameter(PARAM_FORMAT_SUMMARY, getFormat(getAdjustedFormat(m_subreportSummary.getFormat())));
        }

        initGroupsRefCodes();

        m_grid = getDatasource();
    }

    /**
     * Add group records.
     *
     * @param grid RewindableReportDataGrid
     * @param healthLogs Collection<HealthLog>
     * @param groupCounter int
     * @param group LogStatisticsDataGroup
     */
    private void addGroupRecords(RewindableReportDataGrid grid,
                                 Collection<HealthLog> healthLogs,
                                 int groupCounter,
                                 LogStatisticsDataGroup group) {
        for (HealthLog currentLog : healthLogs) {
            String personOid = currentLog.getPerson().getOid();
            SisStudent student = m_studentMap.get(personOid);

            if (student != null) {
                SisSchool school = m_schoolMap.get(currentLog.getSchoolOid());
                String grade = m_data.getStudentGradeOnDate(student, currentLog.getDate());

                Map<String, ReferenceCode> refCodes = m_dataRefCodes.get(group.getName());

                for (Entry<String, String> entry : group.getGroupElements(currentLog, refCodes).entrySet()) {
                    grid.append();

                    grid.set(FIELD_STUDENT, student);

                    grid.set(FIELD_SCHOOL_NAME, school.getName());
                    grid.set(FIELD_GROUP_NAME, group.getName());
                    grid.set(FIELD_HEALTH_LOG_VISIT_TYPE, StringUtils.isEmpty(currentLog.getVisitType())
                            ? "Visit Type missing" : currentLog.getVisitType());

                    String elementCode = entry.getValue();
                    String adjustedElementCode =
                            StringUtils.isEmpty(elementCode) ? (group.getName() + " missing") : elementCode;

                    grid.set(FIELD_ELEMENT_CODE, adjustedElementCode);
                    grid.set(FIELD_GRADE, grade);
                    grid.set(FIELD_STUDENT_OID, student.getOid());
                    grid.set(FIELD_COMPLAINT_OID, entry.getKey());
                    grid.set(FIELD_GROUP_COUNTER, Integer.valueOf(groupCounter));

                    // Fields for CSV only
                    grid.set(FIELD_STUDENT_NAME, student.getNameView());
                    grid.set(FIELD_STUDENT_LASID, student.getLocalId());
                    grid.set(FIELD_HEALTH_LOG_DATE, m_dateFormat.format(currentLog.getDate()));
                    grid.set(FIELD_HEALTH_LOG_COMPLAINT, adjustedElementCode);
                    grid.set(FIELD_HEALTH_LOG_LOCATION, currentLog.getLocationCode());
                    grid.set(FIELD_HEALTH_LOG_TIME_PERIOD, currentLog.getFieldValueByAlias(ALIAS_TIME_PERIOD));

                    String seriousInjury =
                            BooleanAsStringConverter.TRUE.equals(currentLog.getFieldValueByAlias(ALIAS_SERIOUS_INJURY))
                                    ? "Y" : "N";
                    grid.set(FIELD_SERIOUS_INJURY, seriousInjury);
                }
            }
        }
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
                        case JR_MARKED_ELEMENT_AREA_GROUP_NAME:
                            reportElementContainerName = JR_ELEMENT_TEXT_FIELD;
                            textContainerName = JR_ELEMENT_TEXT_FIELD_EXPRESSION;
                            break;
                        case JR_MARKED_ELEMENT_AREA_VISIT_TYPE:
                            reportElementContainerName = JR_ELEMENT_STATIC_TEXT;
                            textContainerName = JR_ELEMENT_TEXT;
                            break;
                        case JR_MARKED_ELEMENT_AREA_ELEMENT_CODE:
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
                    Node newTextElement = getNodeFromNodeList(textContainerName, newElementChildren);

                    Node textElement = null;

                    switch (bind) {
                        case JR_MARKED_ELEMENT_AREA_GROUP_NAME:
                            newTextElement.setTextContent("\"" + grade + "\"");
                            break;
                        case JR_MARKED_ELEMENT_AREA_ELEMENT_CODE:

                            textElement = getNodeFromNodeList(JR_ELEMENT_TEXT_ELEMENT, newElementChildren);

                            ((Element) textElement).setAttribute(JR_REPORT_ATTRIBUTE_ALIGNMENT,
                                    JR_REPORT_ATT_VALUE_ALIGNMENT_RIGHT);

                            newTextElement.setTextContent(wrapAsVariable(grade));
                            ((Element) newElement).setAttribute(JR_REPORT_ATTRIBUTE_EVALUATION_TIME,
                                    JR_REPORT_ATT_VALUE_GROUP);
                            ((Element) newElement).setAttribute(JR_REPORT_ATTRIBUTE_EVALUATION_GROUP,
                                    JR_REPORT_ATT_VALUE_ELEMENT_CODE);
                            break;
                        case JR_MARKED_ELEMENT_AREA_FOOTER:

                            textElement = getNodeFromNodeList(JR_ELEMENT_TEXT_ELEMENT, newElementChildren);

                            ((Element) textElement).setAttribute(JR_REPORT_ATTRIBUTE_ALIGNMENT,
                                    JR_REPORT_ATT_VALUE_ALIGNMENT_RIGHT);

                            newTextElement.setTextContent(wrapAsVariable(grade + JR_TOTAL_VAR_POSTFIX));
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
     * Returns the collection of code values corresponding to a list of reference code oids.
     *
     * @param rtbOid String
     * @param oids Collection<String>
     * @param stateCode boolean
     * @return Collection<String>
     */
    private Collection<String> getCodes(String rtbOid, Collection<String> oids, boolean stateCode) {
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
            if (!stateCode) {
                codes.add(code.getCode());
            } else if (!StringUtils.isEmpty(code.getStateCode())) {
                codes.add(code.getStateCode());
            }
        }
        return codes;
    }

    /**
     * Returns collection of health logs based on input parameters.
     *
     * @return Collection<HealthLog>
     */
    private Collection<HealthLog> getHealthLogs() {
        X2Criteria hlgCriteria = new X2Criteria();

        if (m_schoolsSelected) {
            Collection<String> selectedSchoolOids =
                    Arrays.asList(((String) getParameter(INPUT_PARAM_SCHOOL_OIDS)).split(COMMA));
            hlgCriteria.addIn(HealthLog.REL_SCHOOL + ModelProperty.PATH_DELIMITER + X2BaseBean.COL_OID,
                    selectedSchoolOids);
        }

        if (m_visitTypeSelected) {
            Collection<String> visitTypes = getCodes("rtbVisitTypes",
                    Arrays.asList(((String) getParameter(INPUT_PARAM_VISIT_TYPES)).split(COMMA)), false);
            hlgCriteria.addIn(HealthLog.COL_VISIT_TYPE, visitTypes);
        }

        if (m_withSeriousInjuries.booleanValue()) {
            hlgCriteria.addEqualTo(m_fieldSeriousInjury, BooleanAsStringConverter.TRUE);
        }

        hlgCriteria.addGreaterOrEqualThan(HealthLog.COL_DATE, getStartDate());
        hlgCriteria.addLessOrEqualThan(HealthLog.COL_DATE, getEndDate());

        QueryByCriteria hlgQuery = new QueryByCriteria(HealthLog.class, hlgCriteria, false);

        Collection<HealthLog> healthLogs = getBroker().getCollectionByQuery(hlgQuery);

        return healthLogs;
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
     * Initialize fields.
     */
    private void initializeFields() {
        m_fieldSeriousInjury = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_SERIOUS_INJURY).getJavaName();
    }

    /**
     * Initialize groups reference codes (needed to show Description + State code for Location and
     * Time Period groups).
     */
    private void initGroupsRefCodes() {
        m_dataRefCodes = new HashMap<String, Map<String, ReferenceCode>>();

        for (LogStatisticsDataGroup dataGroup : LogStatisticsDataGroup.values()) {
            m_dataRefCodes.put(dataGroup.getName(), dataGroup.getReferenceCodes(m_dictionary, getBroker()));
        }
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
        ToolJob job = this.getJob();
        m_reportFormatType = job.getInput().getFormat();
        switch (m_reportFormatType) {
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
