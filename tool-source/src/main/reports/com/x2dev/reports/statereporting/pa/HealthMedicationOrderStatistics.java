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
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.ToolInput;
import com.follett.fsc.core.k12.tools.ToolJob;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.HealthInventoryTransaction;
import com.x2dev.sis.model.beans.HealthMedicationOrder;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.io.ByteArrayInputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;
import net.sf.jasperreports3.engine.JRException;
import net.sf.jasperreports3.engine.JRRewindableDataSource;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares the data for the Health Medication Order Statistics.
 *
 * @author Follett Software Company
 */
public class HealthMedicationOrderStatistics extends ReportJavaSourceNet {
    /**
     * Enrollment helper to get student criteria, student's school on passed date.
     *
     * @author Follett Software Company
     */
    class EnrollmentHelper extends StateReportData {
        private Collection<DistrictSchoolYearContext> m_contexts;
        private Map<String, ReferenceCode> m_referenceGradeCodeMap = null;
        private StudentHistoryHelper m_studentHelper;

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
         * Return the current student criteria.
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

            this.sort(FIELD_HMO_NAME, true, true);
            this.sort(FIELD_HMO_TYPE, true, true);
        }
    }

    private static final String COMMA = ",";

    private static final String FIELD_HIT_DATE = "hitDate";
    private static final String FIELD_HIT_OID = "hitOid";
    private static final String FIELD_HMO_INDIVIDUAL = "hmoIndividual";
    private static final String FIELD_HMO_NAME = "hmoName";
    private static final String FIELD_HMO_OID = "hmoOid";
    private static final String FIELD_HMO_STANDING = "hmoStanding";
    private static final String FIELD_HMO_START_DATE = "hmoStartDate";
    private static final String FIELD_HMO_STOP_DATE = "hmoStopDate";
    private static final String FIELD_HMO_TYPE = "hmoType";
    private static final String FIELD_SCHOOL = "school";
    private static final String FIELD_STUDENT_GRADE = "grade";
    private static final String FIELD_STUDENT_LASID = "lasid";
    private static final String FIELD_STUDENT_NAME = "studentName";
    private static final String FIELD_STUDENT_OID = "studentOid";

    private static final String INITIALIZE_KEY = "label.state.report.initialize";

    private static final String INPUT_PARAM_BEGIN_DATE = "beginDate";
    private static final String INPUT_PARAM_END_DATE = "endDate";
    private static final String INPUT_PARAM_FORMAT_ID_CSV = "CSV_FORMAT_ID";
    private static final String INPUT_PARAM_FORMAT_ID_PDF = "PDF_FORMAT_ID";
    private static final String INPUT_PARAM_SCHOOL_OIDS = "schoolOids";
    private static final String INPUT_PARAM_SUBREPORT_ID_DETAIL = "SUBREPORT_DETAIL";
    private static final String INPUT_PARAM_SUBREPORT_ID_SUMMARY = "SUBREPORT_SUMMARY";

    private static final String PARAM_DISTRICT_SUMMARY = "districtSummary";
    private static final String PARAM_END_DATE = "endDate";
    private static final String PARAM_FORMAT_DETAIL = "formatDetail";
    private static final String PARAM_FORMAT_SUMMARY = "formatSummary";
    private static final String PARAM_ORGANIZATION = "organization";
    private static final String PARAM_START_DATE = "startDate";

    private boolean m_allSchoolSelected;
    private EnrollmentHelper m_data;
    private boolean m_districtSummary;
    private int m_formatType;
    private ReportDataGrid m_grid;
    private boolean m_schoolSelected;
    private Collection<String> m_selectedSchools;
    private Report m_subreportDetail;
    private Report m_subreportSummary;

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
     * Gets the datasource.
     *
     * @return Report data grid
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    protected ReportDataGrid getDatasource() {
        X2Criteria hmoCriteria = new X2Criteria();
        hmoCriteria.addLessOrEqualThan(HealthMedicationOrder.COL_START_DATE, getEndDate());
        hmoCriteria.addGreaterOrEqualThan(HealthMedicationOrder.COL_STOP_DATE, getStartDate());
        if (m_schoolSelected) {
            hmoCriteria.addIn(HealthMedicationOrder.COL_SCHOOL_OID, m_selectedSchools);
        }
        QueryByCriteria hmoQuery = new QueryByCriteria(HealthMedicationOrder.class, hmoCriteria);
        Map<String, HealthMedicationOrder> hmoMap = getBroker().getMapByQuery(hmoQuery, X2BaseBean.COL_OID, 100);

        X2Criteria hitCriteria = new X2Criteria();
        hitCriteria.addIn(HealthInventoryTransaction.COL_MEDICATION_ORDER_OID, hmoMap.keySet());
        hitCriteria.addEqualTo(HealthInventoryTransaction.COL_TRANSACTION_TYPE,
                Integer.valueOf(HealthInventoryTransaction.TransactionType.WITHDRAWAL.ordinal()));
        QueryByCriteria hitQuery = new QueryByCriteria(HealthInventoryTransaction.class, hitCriteria);
        Map<String, Collection<HealthInventoryTransaction>> hmoHitsMap =
                getBroker().getGroupedCollectionByQuery(hitQuery, HealthInventoryTransaction.COL_MEDICATION_ORDER_OID,
                        100);

        ReportDataGrid grid = new RewindableReportDataGrid();

        for (Entry<String, Collection<HealthInventoryTransaction>> hmoHitsEntry : hmoHitsMap.entrySet()) {
            String hmoOid = hmoHitsEntry.getKey();
            Collection<HealthInventoryTransaction> hits = hmoHitsEntry.getValue();

            HealthMedicationOrder hmo = hmoMap.get(hmoOid);

            SisStudent student = hmo.getStudent();
            String type = hmo.getMedicationType();
            String name = hmo.getMedicationName();
            Boolean standing = Boolean.valueOf(hmo.getHmo() != null && StringUtils.isEmpty(hmo.getHmo().getStudentOid()));
            Boolean individual = Boolean.valueOf(StringUtils.isEmpty(hmo.getHmoOid()));

            for (HealthInventoryTransaction hit : hits) {
                grid.append();

                grid.set(FIELD_STUDENT_OID, student == null ? null : student.getOid());
                grid.set(FIELD_SCHOOL, hmo.getSchool().getName());
                grid.set(FIELD_HMO_TYPE, type == null ? "Other" : type);
                grid.set(FIELD_HMO_NAME, name == null ? "Other" : name);
                grid.set(FIELD_HMO_OID, hmo.getOid());
                grid.set(FIELD_HMO_STANDING, standing);
                grid.set(FIELD_HMO_INDIVIDUAL, individual);
                grid.set(FIELD_HIT_OID, hit.getOid());

                if (student != null && ToolInput.CSV_FORMAT == m_formatType) {
                    grid.set(FIELD_STUDENT_NAME, student == null ? null : student.getNameView());
                    grid.set(FIELD_STUDENT_LASID, student == null ? null : student.getLocalId());

                    String grade = null;

                    if (student != null) {
                        PlainDate dateForGrade = null;
                        PlainDate hmoStopDate = hmo.getStopDate();
                        boolean stopDateBeforeEndDate = hmoStopDate != null && hmoStopDate.before(getEndDate());
                        dateForGrade = stopDateBeforeEndDate ? hmoStopDate : getEndDate();
                        grade = m_data.getStudentGradeOnDate(student, dateForGrade);
                    }

                    grid.set(FIELD_STUDENT_GRADE, grade);
                    grid.set(FIELD_HMO_START_DATE, hmo.getStartDate());
                    grid.set(FIELD_HMO_STOP_DATE, hmo.getStopDate());

                    grid.set(FIELD_HIT_DATE, hit.getDate());
                }
            }
        }

        grid.sort(FIELD_HMO_NAME, true, true);
        grid.sort(FIELD_HMO_TYPE, true, true);
        grid.sort(FIELD_SCHOOL, true, true);

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
     * Returns selected start date.
     *
     * @return PlainDate
     */
    protected PlainDate getStartDate() {
        return (PlainDate) getParameter(INPUT_PARAM_BEGIN_DATE);
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

        ToolJob job = this.getJob();
        m_formatType = job.getInput().getFormat();

        m_schoolSelected = !StringUtils.isEmpty((String) getParameter(INPUT_PARAM_SCHOOL_OIDS));

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

        initReportsFormat();

        addParameter(PARAM_DISTRICT_SUMMARY, Boolean.valueOf(m_districtSummary));
        addParameter(PARAM_ORGANIZATION, getOrganization());
        addParameter(PARAM_START_DATE, getStartDate());
        addParameter(PARAM_END_DATE, getEndDate());
        addParameter(PARAM_FORMAT_DETAIL, new ByteArrayInputStream(m_subreportDetail.getCompiledFormat()));
        addParameter(PARAM_FORMAT_SUMMARY, new ByteArrayInputStream(m_subreportSummary.getCompiledFormat()));

        super.initialize();

        m_grid = getDatasource();
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
        switch (job.getInput().getFormat()) {
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
}
