/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2019 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.reports.statereporting.on;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.StudentSchool;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.x2dev.procedures.statereporting.common.CriteriaHelper;
import com.x2dev.procedures.statereporting.common.CriteriaHelper.EnrollmentSpanCriteria;
import com.x2dev.procedures.statereporting.common.DictionaryExtractor;
import com.x2dev.procedures.statereporting.common.FilterableFactory;
import com.x2dev.procedures.statereporting.common.FilterableFactory.Filter;
import com.x2dev.procedures.statereporting.common.FilterableFactory.Filterable;
import com.x2dev.procedures.statereporting.common.ToolBean;
import com.x2dev.procedures.statereporting.common.ToolBean.DistrictManager;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolBeanColumn;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolEnrollment;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolStudent;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.AnnualSpan;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.AspenSpan;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnAnnualSpanFactory;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnEnrollment;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnSchool;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnStudent;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnStudentSchool;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnStudentSped;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.SubmissionType;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.beans.path.SisBeanPaths;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.BiFunction;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class OnReportJavaSource.
 *
 * @author Follett Software Company
 * @copyright 2019
 */
public class OnReportJavaSourceNew extends OnLocalizedReport {

    /**
     * The Interface FieldInterface.
     */
    public interface FieldInterface {
        //
    }

    /**
     * The Interface RecordInterface.
     */
    public interface RecordInterface {

        /**
         * Gets the.
         *
         * @param field FieldInterface
         * @return Object
         */
        public Object get(FieldInterface field);

        /**
         * Sets the.
         *
         * @param field FieldInterface
         * @param value Object
         */
        public void set(FieldInterface field, Object value);
    }

    /**
     * The Class ReportStudentSchool.
     */
    public static class ReportStudentSchool extends OnStudentSchool {

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolBean.FULL_DEFINITION
                .expand(FIELD_ARRIVAL_STATUS,
                        FIELD_BRD_RES_STAT_TYPE,
                        FIELD_DISTRICT_CONTEXT_OID,
                        FIELD_END_DATE,
                        FIELD_MAIN_SCHOOL_FLAG_OVERRIDE,
                        FIELD_START_DATE,
                        FIELD_STUDENT_OID,
                        FIELD_SCHOOL_OID)
                .expandCriteriaFunctions(new BiFunction<X2Broker, X2Criteria, X2Criteria>() {
                    @Override
                    public X2Criteria apply(X2Broker broker, X2Criteria criteria) {
                        criteria.addEqualTo(ToolStudentSchool.FIELD_TYPE.resolve(null),
                                Integer.valueOf(StudentSchool.SECONDARY));
                        return criteria;
                    }
                });

        /**
         * Instantiates a new onsis student school.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public ReportStudentSchool(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

    }

    /**
     * The Class ReportStudentSchool.
     */
    public static class ReportStudent extends OnStudent {

        /**
         * Instantiates a new onsis student school.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public ReportStudent(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets description for birth country ref code
         *
         * @return string
         */
        public String getBirthCountryDescription() {
            String value = null;
            DictionaryExtractor extractor = ToolBean.getDictionaryExtractor();
            DataDictionaryField field = FIELD_BIRTH_COUNTRY.getField(extractor);
            Map<String, ReferenceCode> refCodes = extractor.getReferenceCodes(field.getReferenceTableOid());
            ReferenceCode refCode = refCodes.get(getBirthCountryPlain());
            if (refCode != null) {
                value = refCode.getDescription();
            }
            return value;
        }

        /**
         * Gets the enrollment spans without sort.
         *
         * @param broker X2Broker
         * @param queryAsOfDate PlainDate
         * @param isBreakOnYog boolean
         * @param isBreakOnStatus boolean
         * @return List
         */
        @Override
        public List<AnnualSpan> getEnrollmentSpans(X2Broker broker,
                                                   PlainDate queryAsOfDate,
                                                   boolean isBreakOnYog,
                                                   boolean isBreakOnStatus) {
            String key = Boolean.toString(isBreakOnYog) + Boolean.toString(isBreakOnStatus) + queryAsOfDate;
            if (m_enrollmentSpans == null) {
                m_enrollmentSpans = new HashMap();
            }

            List<AnnualSpan> spans = m_enrollmentSpans.get(key);

            if (spans == null) {
                List<AspenSpan> aspenSpans = getAspenSpans(broker, isBreakOnYog, isBreakOnStatus);
                spans = splitSpansIntoContexts(broker, aspenSpans, queryAsOfDate, isBreakOnYog);
                spans.addAll(getSecondarySpans(broker));

                if (queryAsOfDate != null) {
                    spans = spans.stream()
                            .filter(span -> !span.getSpanStartDate().after(queryAsOfDate))
                            .collect(Collectors.toList());
                }
                m_enrollmentSpans.put(key, spans);
            }
            return spans;
        }

    }

    /**
     * The Class DataErrorException.
     */
    public static class DataErrorException extends RuntimeException {

        /**
         * Instantiates a new data error exception.
         *
         * @param errorMessage the error message
         */
        public DataErrorException(String errorMessage) {
            super(errorMessage);
        }
    }

    public static final String ALIAS_DDX_NUM_GRADE = "NumericGradeLevel";
    public static final String ALIAS_RCD_BOARD_NUMBER = "rcd-bsid-board-number";

    public static final String BOARD_RES_STATUS_ELEARNING = "09";
    public static final String BOARD_RES_STATUS_INDEPENDENT = "10";
    public static final String BOARD_RES_STATUS_INDEPENDENT_STUDY = "06";
    public static final String BOARD_RES_STATUS_OTHER_CANADA_GOV = "03";
    public static final String BOARD_RES_STATUS_PUPIL_OF_THE_BOARD = "01";
    public static final String BOARD_RES_STATUS_SHARED = "08";

    public static final String DDX_ID_REF_BSID_SCHOOL = "REF-School-BSID";
    public static final String DDX_ID_GRADES = "REF-GRADE-LEVELS";
    public static final String INPUT_PARAM_ALL_SCHOOLS = "allSchools";
    public static final String INPUT_PARAM_ELEMENTARY = "elementaryReport";
    public static final String INPUT_PARAM_REPORT_DATE = "reportDate";
    public static final String INPUT_PARAM_SCHOOL_OIDS = "schoolOids";
    public static final String INPUT_PARAM_SUBMISSION_TYPE = "submissionType";
    public static final String REF_TABLE_NAME_BSID_BOARD = "BSID - Boards";
    public static final String REF_TABLE_NAME_BSID_SCHOOL = "BSID - Schools";


    public static final String PARAMETER_NAME_ERRORS_LOG = "errorsLog";
    public static final String REPORT_PARAM_AS_OF_DATE = "asOfDate";
    public static final String REPORT_PARAM_VERSION = "version";
    public static final List SKL_CODES_ELEM = Arrays.asList("01");
    public static final List SKL_CODES_SEC = Arrays.asList("02", "03");

    public static final BigDecimal FTE_FT_FINAL_THRESHOLD = BigDecimal.valueOf(0.7F);

    public static final SimpleDateFormat s_aliasDateFormat = new SimpleDateFormat("yyyy-MM-dd");
    public static final SimpleDateFormat s_asOfDateFormat = new SimpleDateFormat("yyyy/MM/dd");

    protected static final String ALIAS_SKL_BSID_NUMBER = "all-skl-BSID";
    protected static final String DDX_ID_SAL = "STD-PGM-SAL";

    protected transient Predicate<AnnualSpan> m_annualSpanFilter = new Predicate<AnnualSpan>() {
        @Override
        public boolean test(AnnualSpan span) {
            PlainDate spanStartDate = span.getFirstActiveInSessionDate();
            PlainDate spanEndDate = span.getSpanEndDate();
            return !getReportDate().before(spanStartDate)
                    && (spanEndDate == null || !getReportDate().after(spanEndDate));
        }
    };

    private PlainDate m_december31;
    private StringBuilder m_errorsLog = new StringBuilder();
    private boolean m_isAllSchools = false;
    private boolean m_isElementaryReport = false;
    private Map<String, Filterable<ReferenceCode>> m_refCodesFilterables = new HashMap<>();
    private PlainDate m_reportDate;
    private List<String> m_schoolOids;
    private Filterable<OnSchool> m_schools;
    private SubmissionType m_submissionType;


    /**
     * Filter by fields.
     *
     * @param <T> the generic type
     * @param <E> the element type
     * @param fields List<FteRecord.Field>
     * @param record FteRecord
     * @param filterable Filterable<FteRecord>
     * @return Filterable
     */
    public <T extends RecordInterface, E extends FieldInterface> Filterable<T> filterByFields(List<E> fields,
                                                                                              T record,
                                                                                              Filterable<T> filterable) {
        return filterable.filter(
                fields.stream().map(FieldInterface::toString).collect(Collectors.toList()),
                fields.stream().map(field -> record.get(field)).collect(Collectors.toList()));
    }

    /**
     * Checks if is elementary report.
     *
     * @return true, if is elementary report
     */
    public boolean isElementaryReport() {
        return m_isElementaryReport;
    }

    /**
     * Gets the board by school number.
     *
     * @param schoolNumber String
     * @return Reference code
     */
    public ReferenceCode getBoardBySchoolNumber(String schoolNumber) {
        ReferenceCode boardRefCode = null;
        ReferenceCode schoolRefCode =
                getRefCodesFilterable(REF_TABLE_NAME_BSID_SCHOOL).extractFirst(ReferenceCode.COL_CODE, schoolNumber);
        if (schoolRefCode != null) {
            String boardNumber = (String) schoolRefCode.getFieldValueByAlias(ALIAS_RCD_BOARD_NUMBER,
                    getDictExtractor().getDictionary(DDX_ID_REF_BSID_SCHOOL));
            boardRefCode =
                    getRefCodesFilterable(REF_TABLE_NAME_BSID_BOARD).extractFirst(ReferenceCode.COL_CODE, boardNumber);
        }
        return boardRefCode;
    }

    /**
     * Gets the december 31.
     *
     * @return Plain date
     */
    public PlainDate getDecember31() {
        if (m_december31 == null) {
            Calendar calendar = Calendar.getInstance();
            calendar.set(Calendar.MONTH, Calendar.DECEMBER);
            calendar.set(Calendar.DAY_OF_MONTH, 31);
            calendar.set(Calendar.YEAR, getCurrentContext().getSchoolYear() - 1);
            m_december31 = new PlainDate(calendar.getTime());
        }
        return m_december31;
    }

    /**
     * Gets the description.
     *
     * @param enrollment OnEnrollment
     * @param field ToolBeanColumn
     * @return String
     */
    public String getDescription(OnEnrollment enrollment, ToolBeanColumn field) {
        DataDictionaryField dictField = field.getField(getDictExtractor());
        Map<String, ReferenceCode> refCodes = getDictExtractor().getReferenceCodes(dictField.getReferenceTableOid());
        ReferenceCode refCode = refCodes.get(enrollment.getValueString(field));
        if (refCode == null) {
            ReportStudent student = (ReportStudent) enrollment.getStudent(getBroker());
            String errorMessage = "Cannot determine reference code for enrollment of student "
                    + getStudentNameFirstMiddleLast(student)
                    + " "
                    + student.getOen()
                    + " for field " + dictField.getUserLongName();
            throw new DataErrorException(errorMessage);
        }
        return refCode.getDescription();
    }

    /**
     * Gets the dictionary.
     *
     * @return Data dictionary
     */
    public DataDictionary getDictionary() {
        return DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
    }

    /**
     * Gets the iep flag.
     *
     * @param program OnStudentSped
     * @param student ReportStudent
     * @param date PlainDate
     * @return Boolean
     */
    public Boolean getIepFlag(OnStudentSped program, ReportStudent student, PlainDate date) {
        Boolean value = Boolean.FALSE;
        if (program == null) {
            List<OnStudentSped> programs = student.getSpedPrograms(getBroker());
            if (programs != null && !programs.isEmpty()) {
                program = programs.get(programs.size() - 1);
            }
        }
        if (program != null) {
            value = program.getIepFlag(getBroker(), date);
        }
        return value == null ? Boolean.FALSE : value;
    }

    /**
     * Gets the errors log.
     *
     * @return String builder
     */
    public StringBuilder getErrorsLog() {
        return m_errorsLog;
    }

    /**
     * Gets the field by alias.
     *
     * @param alias String
     * @return Data dictionary field
     */
    public DataDictionaryField getFieldByAlias(String alias) {
        return getDictionary().findDataDictionaryFieldByAlias(alias);
    }

    /**
     * Gets the historical cutoff date.
     *
     * @return Plain date
     */
    public PlainDate getHistoricalCutoffDate() {
        PlainDate reportDate = getReportDate();

        if (reportDate != null) {
            DistrictSchoolYearContext dateContext =
                    FilterableFactory.create(getBroker(), DistrictSchoolYearContext.class)
                            .extractFirst(new Filter<DistrictSchoolYearContext>() {

                                @Override
                                public boolean isFiltered(DistrictSchoolYearContext ctx) {
                                    return !reportDate.before(ctx.getStartDate())
                                            && !reportDate.after(ctx.getEndDate());
                                }
                            });
            if (dateContext != null) {
                return dateContext.getStartDate();
            }
        } else {
            return getCurrentContext().getStartDate();
        }
        return reportDate;
    }

    /**
     * Gets the main except flag.
     *
     * @param program OnStudentSped
     * @param student ReportStudent
     * @return Boolean
     */
    public Boolean getMainExceptFlag(OnStudentSped program, ReportStudent student) {
        Boolean value = Boolean.FALSE;
        if (program == null) {
            List<OnStudentSped> programs = student.getSpedPrograms(getBroker());
            if (programs != null && !programs.isEmpty()) {
                program = programs.get(programs.size() - 1);
            }
        }
        if (program != null) {
            String exceptionalityType = program.getExceptionalityPlain();
            if (!StringUtils.isEmpty(exceptionalityType)) {
                value = program.getMainExceptFlagPlain();
            }
        }
        return value == null ? Boolean.FALSE : value;
    }

    /**
     * Gets the ref code.
     *
     * @param bean X2BaseBean
     * @param field DataDictionaryField
     * @return Reference code
     */
    public ReferenceCode getRefCode(X2BaseBean bean, DataDictionaryField field) {
        ReferenceTable refTable = field.getReferenceTable();
        if (refTable == null) {
            throw new RuntimeException("Cannot find reference table for field [" + field.getJavaName() + "]");
        }
        Filterable<ReferenceCode> codes = getRefCodesFilterable(refTable.getUserName());
        Object value = bean.getFieldValueByBeanPath(field.getJavaName());
        if (value == null) {
            return null;
        }
        return codes.extractFirst(ReferenceCode.COL_CODE, value);
    }

    /**
     * Gets the full student name (First, Middle, Last).
     *
     * @param student the student
     * @return String
     */
    public String getStudentNameFirstMiddleLast(OnStudent student) {
        return String.join(" ", Arrays.asList(
                student.getLegalFirstName(),
                student.getLegalMiddleName(),
                student.getLegalLastName())
                .stream()
                .filter(it -> !StringUtils.isBlank(it))
                .collect(Collectors.toList()));
    }

    /**
     * Gets the student last name.
     *
     * @param student the student
     * @return String
     */
    public String getStudentNameLast(OnStudent student) {
        return student.getLegalLastName();
    }

    /**
     * Gets the full student name (Last with comma, First, Middle).
     *
     * @param student the student
     * @return String
     */
    public String getStudentNameLastFirstMiddle(OnStudent student) {
        return String.join(" ", Arrays.asList(
                student.getLegalLastName() + ",",
                student.getLegalFirstName(),
                student.getLegalMiddleName())
                .stream()
                .filter(it -> !StringUtils.isBlank(it))
                .collect(Collectors.toList()));
    }

    /**
     * Gets the submission type.
     *
     * @return Submission type
     */
    public SubmissionType getSubmissionType() {
        if (m_submissionType == null) {
            String submissionOid = (String) getParameter(INPUT_PARAM_SUBMISSION_TYPE);
            m_submissionType =
                    ToolBean.getBeanByOid(getBroker(), getDictExtractor(), SubmissionType.class, submissionOid, false);
        }
        return m_submissionType;
    }

    /**
     * Gets the report date.
     *
     * @return Plain date
     */
    public PlainDate getReportDate() {
        if (m_reportDate == null) {
            m_reportDate = (PlainDate) getParameter(INPUT_PARAM_REPORT_DATE);
        }
        return m_reportDate;
    }

    /**
     * Gets the school oids.
     *
     * @return List
     */
    public List<String> getSchoolOids() {
        if (m_schoolOids == null) {
            String schoolOids = (String) getParameter(INPUT_PARAM_SCHOOL_OIDS);
            m_schoolOids = Arrays.asList(schoolOids.split(","));
        }
        return m_schoolOids;
    }

    /**
     * Gets the schools.
     *
     * @return Filterable
     */
    public Filterable<OnSchool> getSchools() {
        if (m_schools == null) {
            m_schools = FilterableFactory.create(ToolBean.getBroker(true), ToolBean.getDictionaryExtractor(),
                    OnSchool.class, new X2Criteria(), null)
                    .filter(new Filter<OnSchool>() {
                        @Override
                        public boolean isFiltered(OnSchool school) {
                            return !StringUtils.isEmpty(school.getBsid())
                                    && (m_isAllSchools || getSchoolOids().contains(school.getOid()));
                        }
                    });
        }
        return m_schools;
    }

    /**
     * Gets the schools from input.
     *
     * @return Filterable
     */
    public Filterable<OnSchool> getSchoolsFromInput() {
        Filterable<OnSchool> filterableSkls = null;
        if (isSchoolContext()) {
            Filterable<OnSchool> filterableContextSchool =
                    FilterableFactory.createFilterableToolBeans(Arrays.asList(getOnSchool()));
            if (m_isElementaryReport) {
                filterableSkls = filterableContextSchool.filter(new Filter<OnSchool>() {
                    @Override
                    public boolean isFiltered(OnSchool skl) {
                        return SKL_CODES_ELEM.contains(skl.getSchoolLevelCode()) && !skl.getArchiveIndicator()
                                && !skl.getInactiveIndicator();
                    }
                });
            } else {
                filterableSkls = filterableContextSchool.filter(new Filter<OnSchool>() {
                    @Override
                    public boolean isFiltered(OnSchool skl) {
                        return SKL_CODES_SEC.contains(skl.getSchoolLevelCode()) && !skl.getArchiveIndicator()
                                && !skl.getInactiveIndicator();
                    }
                });
            }
        } else if (m_isElementaryReport) {
            filterableSkls = getSchools().filter(new Filter<OnSchool>() {
                @Override
                public boolean isFiltered(OnSchool skl) {
                    return SKL_CODES_ELEM.contains(skl.getSchoolLevelCode()) && !skl.getArchiveIndicator()
                            && !skl.getInactiveIndicator();
                }
            });
        } else {
            filterableSkls = getSchools().filter(new Filter<OnSchool>() {
                @Override
                public boolean isFiltered(OnSchool skl) {
                    return SKL_CODES_SEC.contains(skl.getSchoolLevelCode()) && !skl.getArchiveIndicator()
                            && !skl.getInactiveIndicator();
                }
            });
        }
        return filterableSkls;
    }

    /**
     * Gets the span configuration.
     *
     * @param asOfDate PlainDate
     * @return Span configuration
     */
    public EnrollmentSpanCriteria getSpanConfiguration(PlainDate asOfDate) {
        ToolBean.setPreference(ToolBean.PREFERENCE_HISTORICAL_CUTOFF_DATE, getHistoricalCutoffDate());
        ToolBean.setPreference(ToolBean.PREFERENCE_QUERY_AS_OF_DATE, asOfDate);
        EnrollmentSpanCriteria spanConfig = new EnrollmentSpanCriteria()
                .setExcludeStudent(ReportStudent.FIELD_EXCLUDE_FROM_REPORTING);
        return spanConfig;
    }

    public Collection<ReportStudent> getStudents(EnrollmentSpanCriteria spanCriteria,
                                                 boolean isBreakOnYog,
                                                 boolean isBreakOnStatus,
                                                 Predicate<AnnualSpan> spanFilter) {
        ToolBean.clearAllCachedToolBeans(ReportStudent.class);
        ToolBean.clearAllCachedToolBeans(ToolEnrollment.class);
        ToolBean.clearAllCachedToolBeans(ReportStudentSchool.class);

        ToolBean.resetCriteria(getBroker(), ReportStudent.class);
        ToolBean.resetCriteria(getBroker(), ToolEnrollment.class);
        ToolBean.resetCriteria(getBroker(), ReportStudentSchool.class);

        X2Criteria candidateCriteria = CriteriaHelper.getStudentCandidateCriteria(spanCriteria,
                getBroker());
        candidateCriteria.addNotEmpty(SisBeanPaths.STUDENT.stateId().getPath(), getBroker().getPersistenceKey());

        // load students with filterable
        FilterableFactory.create(getBroker(), getDictExtractor(), ReportStudent.class,
                candidateCriteria, null);

        // load enrollments and student school
        ToolBean.preload(getBroker(), getDictExtractor(),
                Arrays.asList(ToolEnrollment.FIELD_DATE_DESC, ToolEnrollment.FIELD_TIMESTAMP_DESC),
                ToolStudent.CHILD_STUDENT_ENROLLMENTS);

        X2Criteria secondaryCriteria = new X2Criteria();
        secondaryCriteria.addIn(ReportStudentSchool.FIELD_SCHOOL_OID.resolve(getDictExtractor()),
                spanCriteria.getSchoolOids());
        ToolBean.addAndCriteria(getBroker(), ReportStudentSchool.class, secondaryCriteria);
        ToolBean.preload(getBroker(), getDictExtractor(),
                Arrays.asList(ReportStudentSchool.FIELD_START_DATE_DESC, ToolBean.FIELD_OID),
                ToolStudent.CHILD_STUDENT_SCHOOLS);

        DistrictManager.setAnnualSpanFactory(new OnAnnualSpanFactory(getBroker()));

        Set<String> allStudentsList = new HashSet();
        PlainDate queryAsOfDate = (PlainDate) ToolBean.getPreference(ToolBean.PREFERENCE_QUERY_AS_OF_DATE);
        ToolBean.getCachedToolBeans(ReportStudent.class).stream()
                .map(student -> student.getEnrollmentSpans(getBroker(), isBreakOnYog, isBreakOnStatus))
                .flatMap(List::stream)
                .filter(span -> {
                    // test secondary spans
                    if (!spanCriteria.isIncludeSecondarySpans() && span.isSecondary()) {
                        return false;
                    }
                    // test school
                    if (spanCriteria.getSchoolOids() != null
                            && !spanCriteria.getSchoolOids().contains(span.getSchool().getOid())) {
                        return false;
                    }
                    ToolEnrollment lastEnrolment = span.getAllEnrollmentsDescend().stream()
                            .filter(enr -> enr.getEnrollmentDate().before(queryAsOfDate))
                            .findFirst()
                            .orElse(null);
                    if (lastEnrolment == null || lastEnrolment.getEnrollmentType().equals(StudentEnrollment.WITHDRAWAL)) {
                        return false;
                    }
                    if (spanFilter != null && !spanFilter.test(span)) {
                        return false;
                    }
                    return true;
                })
                .forEach(span -> {
                    allStudentsList.add(span.getStudent().getOid());
                });

        // remove unused students
        ToolBean.filterCachedToolBeans(ReportStudent.class,
                student -> allStudentsList.contains(student.getOid()));

        return ToolBean.getCachedToolBeans(ReportStudent.class);
    }

    /**
     * Gets the state value.
     *
     * @param bean X2BaseBean
     * @param field DataDictionaryField
     * @return String
     */
    public String getStateValue(X2BaseBean bean, DataDictionaryField field) { //
        ReferenceCode code = getRefCode(bean, field);
        return code == null ? null : code.getStateCode();
    }

    /**
     * Gets the yog.
     *
     * @param student SisStudent
     * @param enrollment StudentEnrollment
     * @return int
     */
    public int getYog(ReportStudent student, OnEnrollment enrollment) {
        int yog = student.getYog();
        if (enrollment != null) {
            yog = enrollment.getYog();
        }
        return yog;
    }

    /**
     * Checks if is 21 plus.
     *
     * @param student the student
     * @return true, if is 21 plus
     */
    public boolean is21plus(ReportStudent student) {
        return student.getAgeAsOfDate(getDecember31()) >= 21;
    }

    /**
     * Log error.
     *
     * @param error String
     */
    public void logError(String error) {
        m_errorsLog.append(error);
        m_errorsLog.append("\n");
    }

    /**
     * Gather data.
     *
     * @return Object
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        return null;
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();

        ToolBean.registerClass(ReportStudent.class);
        ToolBean.registerClass(OnEnrollment.class);
        ToolBean.registerClass(ReportStudentSchool.class);
        m_isElementaryReport =
                getParameter(INPUT_PARAM_ELEMENTARY) != null
                        && ((Boolean) getParameter(INPUT_PARAM_ELEMENTARY)).booleanValue();

        Boolean isAllSchools = (Boolean) getParameter(INPUT_PARAM_ALL_SCHOOLS);
        if (isAllSchools != null) {
            m_isAllSchools = isAllSchools;
        }
    }

    /**
     * Gets the ref codes filterable.
     *
     * @param refTableName String
     * @return Filterable
     */
    private Filterable<ReferenceCode> getRefCodesFilterable(String refTableName) {
        Filterable<ReferenceCode> refCodesFilterable = m_refCodesFilterables.get(refTableName);
        if (refCodesFilterable == null) {
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(ReferenceTable.COL_USER_NAME, refTableName);
            ReferenceTable referenceTable =
                    getBroker().getBeanByQuery(new QueryByCriteria(ReferenceTable.class, criteria));
            if (referenceTable == null) {
                throw new RuntimeException("Cannot find reference table " + refTableName);
            }
            refCodesFilterable = FilterableFactory.create(referenceTable.getCodeMap().values(),
                    Arrays.asList(X2BaseBean.COL_OID),
                    FilterableFactory.Filterable.PredefinedResolver.X2BASE_BEAN);
            m_refCodesFilterables.put(refTableName, refCodesFilterable);
        }
        return refCodesFilterable;
    }

}
