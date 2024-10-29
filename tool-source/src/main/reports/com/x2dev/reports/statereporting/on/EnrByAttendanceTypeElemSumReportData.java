/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2018 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */

package com.x2dev.reports.statereporting.on;

import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.x2dev.procedures.statereporting.common.CriteriaHelper.EnrollmentSpanCriteria;
import com.x2dev.procedures.statereporting.common.DictionaryExtractor;
import com.x2dev.procedures.statereporting.common.FilterableFactory;
import com.x2dev.procedures.statereporting.common.FilterableFactory.Filter;
import com.x2dev.procedures.statereporting.common.FilterableFactory.Filterable;
import com.x2dev.procedures.statereporting.common.FilterableFactory.Filterable.Mapper;
import com.x2dev.procedures.statereporting.common.FilterableFactory.MultiLevelMap.ValueByKeyResolver;
import com.x2dev.procedures.statereporting.common.ToolBean;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.AnnualSpan;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnAnnualSpan;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnEnrollment;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnSchool;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnStudent;
import com.x2dev.procedures.statereporting.on.revised.OnHelpersContainer.OnSchoolDateRangeProvider;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class EnrByAttendanceTypeSecSumReportData.
 */
public class EnrByAttendanceTypeElemSumReportData extends OnReportJavaSourceNew {
    private static final long serialVersionUID = 1L;


    private static final String BOARD_RES_STATUS_01 = "01";
    private static final String BOARD_RES_STATUS_02 = "02";
    private static final String BOARD_RES_STATUS_03 = "03";
    private static final String BOARD_RES_STATUS_05 = "05";
    private static final String BOARD_RES_STATUS_07 = "07";
    private static List<String> ENR_RECORDS_KEYS_BY_AGE_AND_ATTENDANCE =
            Arrays.asList("ftU21", "ptU21", "ftO21", "ptO21");
    private static final String GRADE_LEVEL_JK = "JK";
    private static final String GRADE_LEVEL_K = "SK";
    private static final Integer INT_1 = 1;
    private static final Integer INT_3 = 3;
    private static final Integer INT_4 = 4;
    private static final Integer INT_6 = 6;
    private static final Integer INT_7 = 7;
    private static final Integer INT_8 = 8;

    /**
     * Class members
     */
    private DecimalFormat m_decimalFormat = new DecimalFormat("#.##");
    private SimpleDateFormat m_formatter = new SimpleDateFormat("yyyy/MM/dd hh:mm:ss");
    private SchoolDateRangeProvider m_sklDateRangeProvider;
    private EnrollmentSpanCriteria m_spanCriteria;

    /**
     * The Class EnrByAttSummRecord.
     */
    private static class EnrByAttSummRecord {
        private Map<Field, Object> m_fieldValuePairs = new HashMap<Field, Object>();

        /**
         * The Enum Field.
         */
        private enum Field {
            boardName,
            //
            boardNumber,
            //
            schoolName,
            //
            schoolNumber,
            //
            academicYears,
            //
            programName,
            //
            currentDate,
            //
            pupilsOfBoardJuniorKindergarten,
            //
            pupilsOfBoardHearingPreSchool,
            //
            pupilsOfBoardKindergarten,
            //
            pupilsOfBoardGrade1,
            //
            pupilsOfBoardGrade1To3,
            //
            pupilsOfBoardGrade4to8,
            //
            pupilsOfBoardGrade4to6,
            //
            pupilsOfBoardGrade7to8,
            //
            nativeEducationAuthority,
            //
            governmentOfCanada,
            //
            studyPermitTempResident,
            //
            otherStudents,
            //
            summaryValues
        }

        private static List<String> s_uniqueFields =
                Arrays.asList(Field.boardNumber.toString(),
                        Field.schoolName.toString(),
                        Field.schoolNumber.toString());

        private static ValueByKeyResolver<EnrByAttSummRecord> s_valueResolver =
                new ValueByKeyResolver<EnrByAttSummRecord>() {
                    @Override
                    public Object getValue(String key, EnrByAttSummRecord entity) {
                        return entity.get(Field.valueOf(key));
                    }
                };

        /**
         * Gets the.
         *
         * @param key Field
         * @return Object
         */
        Object get(Field key) {
            return m_fieldValuePairs.get(key);
        }

        /**
         * Sets the.
         *
         * @param field Field
         * @param value Object
         */
        void set(Field field, Object value) {
            m_fieldValuePairs.put(field, value);
        }
    }

    /**
     * The Class EnrByAttSummaryContainer.
     */
    public static class EnrByAttSummaryContainer {
        private Integer m_fullTimeU21NumberOfStudents = 0;
        private Double m_fullTimeU21RegularFTE = 0.0d;
        private Integer m_partTimeU21NumberOfStudents = 0;
        private Double m_partTimeU21RegularFTE = 0.0d;

        private Integer m_fullTimeO21NumberOfStudents = 0;
        private Double m_fullTimeO21RegularFTE = 0.0d;
        private Integer m_partTimeO21NumberOfStudents = 0;
        private Double m_partTimeO21RegularFTE = 0.0d;

        /**
         * Gets the full time U 21 number of students.
         *
         * @return the fullTimeU21NumberOfStudents
         */
        public Integer getFullTimeU21NumberOfStudents() {
            return m_fullTimeU21NumberOfStudents;
        }

        /**
         * Sets the full time U 21 number of students.
         *
         * @param fullTimeU21NumberOfStudents void
         */
        public void setFullTimeU21NumberOfStudents(Integer fullTimeU21NumberOfStudents) {
            this.m_fullTimeU21NumberOfStudents = fullTimeU21NumberOfStudents;
        }

        /**
         * Gets the full time U 21 regular FTE.
         *
         * @return the fullTimeU21RegularFTE
         */
        public Double getFullTimeU21RegularFTE() {
            return m_fullTimeU21RegularFTE;
        }

        /**
         * Sets the full time U 21 regular FTE.
         *
         * @param fullTimeU21RegularFTE void
         */
        public void setFullTimeU21RegularFTE(Double fullTimeU21RegularFTE) {
            this.m_fullTimeU21RegularFTE = fullTimeU21RegularFTE;
        }

        /**
         * Gets the part time U 21 number of students.
         *
         * @return the partTimeU21NumberOfStudents
         */
        public Integer getPartTimeU21NumberOfStudents() {
            return m_partTimeU21NumberOfStudents;
        }

        /**
         * Sets the part time U 21 number of students.
         *
         * @param partTimeU21NumberOfStudents void
         */
        public void setPartTimeU21NumberOfStudents(Integer partTimeU21NumberOfStudents) {
            this.m_partTimeU21NumberOfStudents = partTimeU21NumberOfStudents;
        }

        /**
         * Gets the part time U 21 regular FTE.
         *
         * @return the partTimeU21RegularFTE
         */
        public Double getPartTimeU21RegularFTE() {
            return m_partTimeU21RegularFTE;
        }

        /**
         * Sets the part time U 21 regular FTE.
         *
         * @param partTimeU21RegularFTE void
         */
        public void setPartTimeU21RegularFTE(Double partTimeU21RegularFTE) {
            this.m_partTimeU21RegularFTE = partTimeU21RegularFTE;
        }

        /**
         * Gets the full time O 21 number of students.
         *
         * @return Integer
         */
        public Integer getFullTimeO21NumberOfStudents() {
            return m_fullTimeO21NumberOfStudents;
        }

        /**
         * Sets the full time 021 number of students.
         *
         * @param fullTimeO21NumberOfStudents void
         */
        public void setFullTimeO21NumberOfStudents(Integer fullTimeO21NumberOfStudents) {
            this.m_fullTimeO21NumberOfStudents = fullTimeO21NumberOfStudents;
        }

        /**
         * Gets the full time O 21 regular FTE.
         *
         * @return Double
         */
        public Double getFullTimeO21RegularFTE() {
            return m_fullTimeO21RegularFTE;
        }

        /**
         * Sets the full time O 21 regular FTE.
         *
         * @param fullTimeO21RegularFTE void
         */
        public void setFullTimeO21RegularFTE(Double fullTimeO21RegularFTE) {
            this.m_fullTimeO21RegularFTE = fullTimeO21RegularFTE;
        }

        /**
         * Gets the part time O 21 number of students.
         *
         * @return Integer
         */
        public Integer getPartTimeO21NumberOfStudents() {
            return m_partTimeO21NumberOfStudents;
        }

        /**
         * Sets the part time O 21 number of students.
         *
         * @param partTimeO21NumberOfStudents void
         */
        public void setPartTimeO21NumberOfStudents(Integer partTimeO21NumberOfStudents) {
            this.m_partTimeO21NumberOfStudents = partTimeO21NumberOfStudents;
        }

        /**
         * Gets the part time O 21 regular FTE.
         *
         * @return Double
         */
        public Double getPartTimeO21RegularFTE() {
            return m_partTimeO21RegularFTE;
        }

        /**
         * Sets the part time O 21 regular FTE.
         *
         * @param partTimeO21RegularFTE void
         */
        public void setPartTimeO21RegularFTE(Double partTimeO21RegularFTE) {
            this.m_partTimeO21RegularFTE = partTimeO21RegularFTE;
        }

        /**
         * To string.
         *
         * @return String
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            return "EnrollmentByAttendanceSummaryInstance [fullTimeU21NumberOfStudents=" + m_fullTimeU21NumberOfStudents
                    + ", fullTimeU21RegularFTE=" + m_fullTimeU21RegularFTE
                    + ", partTimeU21NumberOfStudents=" + m_partTimeU21NumberOfStudents
                    + ", partTimeU21RegularFTE=" + m_partTimeU21RegularFTE
                    + "]";
        }

    }

    /**
     * The Class SchoolDateRangeProvider.
     */
    class SchoolDateRangeProvider implements OnSchoolDateRangeProvider {

        X2Broker m_broker;
        DistrictSchoolYearContext m_ctxByDate;
        DataDictionary m_dictionary;

        /**
         * Gets the broker.
         *
         * @return X 2 broker
         * @see com.x2dev.procedures.statereporting.on.OnsisHelpersContainer.OnsisBrokerProvider#getBroker()
         */
        @Override
        public X2Broker getBroker() {
            return m_broker;
        }

        /**
         * Gets the context.
         *
         * @return District school year context
         * @see com.x2dev.procedures.statereporting.on.OnsisHelpersContainer.OnsisCTXProvider#getDistrictSchoolYearContext()
         */
        @Override
        public DistrictSchoolYearContext getContext() {
            return getCurrentContext();
        }

        /**
         * Gets the context by date.
         *
         * @param date PlainDate
         * @return District school year context
         */
        public DistrictSchoolYearContext getContextByDate(PlainDate date) {
            if (m_ctxByDate == null) {
                QueryByCriteria query = new QueryByCriteria(DistrictSchoolYearContext.class);
                query.addOrderByDescending(DistrictSchoolYearContext.COL_START_DATE);
                Collection<DistrictSchoolYearContext> contexts = getBroker().getCollectionByQuery(query);
                /*
                 * Walk context years in descending order until one of them starts on/before date
                 */
                DistrictSchoolYearContext found = null;
                Iterator<DistrictSchoolYearContext> iter = contexts.iterator();
                while (found == null && iter.hasNext()) {
                    DistrictSchoolYearContext ctx = iter.next();

                    // Check that context starts before date.
                    if (date.before(ctx.getStartDate())) {
                        continue;
                    }

                    /*
                     * Check date is within 370 days of startDate
                     * to detect if we've gone too far (ie no context contains the date)
                     *
                     * Don't compare vs Context End Date
                     * because contexts might not extend until next context start date
                     */
                    PlainDate extendedContextEnd = DateUtils.add(ctx.getStartDate(), 370);
                    if (date.after(extendedContextEnd)) {
                        break;
                    }

                    found = ctx;
                }
                m_ctxByDate = found != null ? found : getContext();
            }

            return m_ctxByDate;
        }

        /**
         * Gets the dictionary extractor.
         *
         * @return Dictionary extractor
         * @see com.x2dev.procedures.statereporting.on.revised.OnHelpersContainer.OnDictionaryExtractorProvider#getDictionaryExtractor()
         */
        @Override
        public DictionaryExtractor getDictionaryExtractor() {
            return getDictExtractor();
        }

        /**
         * Gets the end date.
         *
         * @return Plain date
         * @see com.x2dev.procedures.statereporting.on.OnsisHelpersContainer.OnsisDateRangeProvider#getEndDate()
         */
        @Override
        public PlainDate getEndDate() {
            return getContextByDate(getReportDate()).getEndDate();
        }

        /**
         * Gets the school.
         *
         * @return School
         * @see com.x2dev.procedures.statereporting.on.OnsisHelpersContainer.OnOnSchoolProvider#getSchool()
         */
        @Override
        public OnSchool getSchool() {
            return m_school;
        }

        /**
         * Gets the start date.
         *
         * @return Plain date
         * @see com.x2dev.procedures.statereporting.on.OnsisHelpersContainer.OnsisDateRangeProvider#getStartDate()
         */
        @Override
        public PlainDate getStartDate() {
            return getReportDate();
        }

        /**
         * Sets the broker.
         *
         * @param broker void
         */
        public void setBroker(X2Broker broker) {
            m_broker = broker;
        }

        /**
         * Sets the school.
         *
         * @param school void
         */
        public void setSchool(OnSchool school) {
            m_school = school;
        }
    }

    /**
     * The Enum EnrolmentRegister.
     */
    private enum EnrolmentRegister {
        FT, PT;
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
        ReportDataGrid grid = new ReportDataGrid();
        Filterable<OnSchool> filterableSkls = getSchoolsFromInput();
        if (filterableSkls != null) {
            Collection<String> sklOidsToReport =
                    filterableSkls.extract().stream().map(skl -> skl.getOid()).collect(Collectors.toList());
            Filterable<EnrByAttSummRecord> fteSummarryRecords = filterableSkls
                    .map(new Mapper<OnSchool, EnrByAttSummRecord>() {
                        @Override
                        public Filterable<EnrByAttSummRecord> map(OnSchool school) {
                            initializeHelpersForSchool(school);
                            Filterable<OnStudent> students = FilterableFactory.createFilterableToolBeans(
                                    getStudents(m_spanCriteria, false, false, m_annualSpanFilter));
                            return getRecords(students, sklOidsToReport);
                        }
                    });
            for (EnrByAttSummRecord record : fteSummarryRecords.extract()) {
                grid.append();
                EnrByAttSummaryContainer summaryValues = new EnrByAttSummaryContainer();
                for (EnrByAttSummRecord.Field field : record.m_fieldValuePairs.keySet()) {
                    if (record.m_fieldValuePairs.get(field) instanceof EnrByAttSummaryContainer) {
                        if (field != EnrByAttSummRecord.Field.pupilsOfBoardHearingPreSchool
                                && field != EnrByAttSummRecord.Field.pupilsOfBoardGrade1
                                && field != EnrByAttSummRecord.Field.pupilsOfBoardGrade4to6
                                && field != EnrByAttSummRecord.Field.pupilsOfBoardGrade7to8) {
                            addDataToSummary((EnrByAttSummaryContainer) record.m_fieldValuePairs.get(field),
                                    summaryValues);
                        }
                        EnrByAttSummaryContainer container =
                                (EnrByAttSummaryContainer) record.m_fieldValuePairs.get(field);
                        populateGridWithValues(grid, container, field.toString());
                    } else if (!(record.m_fieldValuePairs.get(field) instanceof EnrByAttSummaryContainer)) {
                        grid.set(field.toString(), record.m_fieldValuePairs.get(field));
                    }
                }
                populateGridWithValues(grid, summaryValues, EnrByAttSummRecord.Field.summaryValues.toString());
            }
            grid.beforeTop();
            if (getErrorsLog().length() > 0) {
                addParameter(PARAMETER_NAME_ERRORS_LOG, getErrorsLog().toString());
            }
            addParameter(REPORT_PARAM_VERSION, getJob().getTool().getComment());
            addParameter(REPORT_PARAM_AS_OF_DATE, s_asOfDateFormat.format(getReportDate()));
        }
        return grid;
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
    }

    /**
     * Computes summary values.
     *
     * @param enrollmentByAttendanceValues EnrByAttSummaryContainer
     * @param summaryValues EnrByAttSummaryContainer
     */
    private void addDataToSummary(EnrByAttSummaryContainer enrollmentByAttendanceValues,
                                  EnrByAttSummaryContainer summaryValues) {
        summaryValues.setFullTimeU21NumberOfStudents(summaryValues.getFullTimeU21NumberOfStudents()
                + enrollmentByAttendanceValues.getFullTimeU21NumberOfStudents());
        summaryValues.setFullTimeU21RegularFTE(
                summaryValues.getFullTimeU21RegularFTE() + enrollmentByAttendanceValues.getFullTimeU21RegularFTE());
        summaryValues.setPartTimeU21NumberOfStudents(summaryValues.getPartTimeU21NumberOfStudents()
                + enrollmentByAttendanceValues.getPartTimeU21NumberOfStudents());
        summaryValues.setPartTimeU21RegularFTE(
                summaryValues.getPartTimeU21RegularFTE() + enrollmentByAttendanceValues.getPartTimeU21RegularFTE());

        summaryValues.setFullTimeO21NumberOfStudents(summaryValues.getFullTimeO21NumberOfStudents()
                + enrollmentByAttendanceValues.getFullTimeO21NumberOfStudents());
        summaryValues.setFullTimeO21RegularFTE(
                summaryValues.getFullTimeO21RegularFTE() + enrollmentByAttendanceValues.getFullTimeO21RegularFTE());
        summaryValues.setPartTimeO21NumberOfStudents(summaryValues.getPartTimeO21NumberOfStudents()
                + enrollmentByAttendanceValues.getPartTimeO21NumberOfStudents());
        summaryValues.setPartTimeO21RegularFTE(
                summaryValues.getPartTimeO21RegularFTE() + enrollmentByAttendanceValues.getPartTimeO21RegularFTE());
    }

    /**
     * Creates filter to retrieve data by board res. status
     *
     * @param attendanceType String
     * @param boardResStatus String
     * @return Filter
     */
    private Filter createFileterByBoardResStatus(String attendanceType,
                                                 String boardResStatus,
                                                 boolean isPlus21) {
        return new Filter<OnEnrollment>() {
            @Override
            public boolean isFiltered(OnEnrollment toFilter) {
                try {
                    return isPlus21 == is21plus((ReportStudent) toFilter.getStudent(getBroker()))
                            && attendanceType.equals(toFilter.getRegister())
                            && boardResStatus.equals(toFilter.getBoardResidentStatus());
                } catch (Exception e) {
                    // e.printStackTrace();
                    return false;
                }
            }
        };
    }

    /**
     * Creates filter to retrieve data, which grade level
     * is between provided interval.
     *
     * @param attendanceType String
     * @param gradeLevel1 Integer
     * @param gradeLevel2 Integer
     * @param boardResStatus String
     * @return Filter
     */
    private Filter createFileterByGradeInterval(String attendanceType,
                                                Integer gradeLevel1,
                                                Integer gradeLevel2,
                                                String boardResStatus,
                                                boolean isPlus21) {
        return new Filter<OnEnrollment>() {

            @Override
            public boolean isFiltered(OnEnrollment toFilter) {

                try {
                    String gradeLevel = toFilter.getStudent(getBroker()).getGradeLevel();
                    Integer gradeLevelInt = Integer.valueOf(gradeLevel);
                    return isPlus21 == is21plus((ReportStudent) toFilter.getStudent(getBroker()))
                            && attendanceType.equals(toFilter.getRegister())
                            && (gradeLevelInt >= gradeLevel1 && gradeLevelInt <= gradeLevel2)
                            && boardResStatus.equals(toFilter.getBoardResidentStatus());

                } catch (Exception e) {
                    // e.printStackTrace();
                    return false;
                }
            }
        };
    }

    /**
     * Filter to retrieve data, where age is in provided interval.
     *
     * @param attendanceType String
     * @param age1 int
     * @param age2 int
     * @param greadeLevel String
     * @param boardResStatus String
     * @return Filter
     */
    @SuppressWarnings("unused")
    private Filter createFileterForDefinedYears(String attendanceType,
                                                int age1,
                                                int age2,
                                                String greadeLevel,
                                                String boardResStatus,
                                                boolean isPlus21) {
        return new Filter<OnEnrollment>() {
            @Override
            public boolean isFiltered(OnEnrollment toFilter) {
                try {
                    OnStudent std = (OnStudent) toFilter.getStudent(getBroker());
                    if (std == null) {
                        return false;
                    }
                    int personAge = std.getAgeAsOfDate(getDecember31());
                    return isPlus21 == is21plus((ReportStudent) std)
                            && attendanceType.equals(toFilter.getRegister())
                            && (personAge == age1 || personAge == age2) &&
                            greadeLevel.equals(std.getGradeLevel()) &&
                            boardResStatus.equals(toFilter.getBoardResidentStatus());
                } catch (Exception e) {
                    return false;
                }

            }
        };
    }

    /**
     * Creates filter to retrieve data, which grade level
     * is fixed value.
     *
     * @param attendanceType String
     * @param greadeLevel String
     * @param boardResStatus String
     * @return Filter
     */
    private Filter createFileterForFixedGradeLevel(String attendanceType,
                                                   String greadeLevel,
                                                   String boardResStatus,
                                                   boolean isPlus21) {
        return new Filter<OnEnrollment>() {
            @Override
            public boolean isFiltered(OnEnrollment toFilter) {
                try {
                    return isPlus21 == is21plus((ReportStudent) toFilter.getStudent(getBroker()))
                            && attendanceType.equals(toFilter.getRegister())
                            && greadeLevel.equals(toFilter.getStudent(getBroker()).getGradeLevel())
                            && boardResStatus.equals(toFilter.getBoardResidentStatus());
                } catch (Exception e) {
                    return false;
                }
            }
        };
    }

    /**
     * Creates filter to retrieve data, which satisfied to
     * 'Grade 1 Age appropriate' condition:
     * (STUDENT_SCHOOL_ENROLMENT.GRADE_TYPE_CODE= "JK" or "K",
     * STUDENT_SCHOOL_ENROLMENT.STU_BRD_RES_STAT_TYPE_ID="01")
     *
     * @param attendanceType String
     * @param greadeLevel1 String
     * @param gradeLevel2 String
     * @param boardResStatus String
     * @return Filter
     */
    private Filter createFileterForGrade1Age(String attendanceType,
                                             String greadeLevel1,
                                             String gradeLevel2,
                                             String boardResStatus,
                                             boolean isPlus21) {
        return new Filter<OnEnrollment>() {

            @Override
            public boolean isFiltered(OnEnrollment toFilter) {
                try {
                    OnStudent std = (OnStudent) toFilter.getStudent(getBroker());
                    if (std == null) {
                        return false;
                    }
                    int personAge = std.getAgeAsOfDate(getDecember31());
                    if (personAge < 6) {
                        return false;
                    }
                    return isPlus21 == is21plus((ReportStudent) std)
                            && attendanceType.equals(toFilter.getRegister()) &&
                            (greadeLevel1.equals(std.getGradeLevel())
                                    || gradeLevel2.equals(std.getGradeLevel()))
                            && boardResStatus.equals(toFilter.getBoardResidentStatus());

                } catch (Exception e) {
                    return false;
                }
            }
        };

    }

    /**
     * Gets the fte records.
     *
     * @param students Filterable<OnStudent>
     * @param sklOidsToReport Collection<String>
     * @return Filterable
     */
    private Filterable<EnrByAttSummRecord> getRecords(Filterable<OnStudent> students,
                                                      Collection<String> sklOidsToReport) {
        String schoolName = m_sklDateRangeProvider.getSchool().getName();
        String schoolNumber = m_sklDateRangeProvider.getSchool().getBsid();
        ReferenceCode board = getBoardBySchoolNumber(schoolNumber);
        String boardNumber = board == null ? "Cannot determine board by school number" : board.getCode();
        String boardName = board == null ? "Cannot determine board by school number" : board.getDescription();
        String academicYears = getCurrentContext().getContextId();
        String programName = "Aspen";
        String currentDate = m_formatter
                .format(new Date(DateUtils.currentTimeMillis(OrganizationManager.getTimeZone(getOrganization()))));
        List<OnEnrollment> enrollments = new ArrayList<>();
        for (OnStudent student : students.extract()) {
            List<AnnualSpan> annualSpans =
                    student.getEnrollmentSpans(getBroker(), true, true).stream()
                            .filter(annualSpan -> annualSpan.getContext().getOid()
                                    .equals(m_sklDateRangeProvider.getContextByDate(getReportDate()).getOid()))
                            .collect(Collectors.toList());
            OnEnrollment recentEnrolment = null;
            if (annualSpans != null && !annualSpans.isEmpty()) {
                OnAnnualSpan firstAnnualSpan =
                        (OnAnnualSpan) ToolsSharedContainer.reverse(annualSpans.stream()).findFirst().get();
                if (firstAnnualSpan != null) {
                    recentEnrolment = (OnEnrollment) firstAnnualSpan.getRecentEnrollmentESY();
                    if (recentEnrolment == null && firstAnnualSpan.isSecondary()
                            && sklOidsToReport.contains(firstAnnualSpan.getSecondary().getSchool(getBroker()).getOid())
                            && firstAnnualSpan
                                    .getBestPrimarySpanFor(getBroker(), null) != null) {
                        recentEnrolment = (OnEnrollment) firstAnnualSpan
                                .getBestPrimarySpanFor(getBroker(), null)
                                .getRecentEnrollmentESY();
                    }
                }
            }
            if (recentEnrolment != null) {
                enrollments.add(recentEnrolment);
            }
        }
        List<EnrByAttSummRecord> enrByAttSummRecords = new ArrayList<EnrByAttSummRecord>();
        Filterable<OnEnrollment> filtrableEnrollments = new Filterable<OnEnrollment>() {
            @Override
            public Map<String, OnEnrollment> initializeEntities() {
                Map<String, OnEnrollment> filtrableValues = new HashMap<>();
                for (OnEnrollment enrollment : enrollments) {
                    filtrableValues.put(enrollment.getOid(), enrollment);
                }
                return filtrableValues;
            }

            @Override
            public PredefinedResolver initializeValueResolver() {
                return PredefinedResolver.RPT_BEAN;
            }

            @Override
            public List<String> initializeUniqueKeys() {
                return Arrays.asList(ToolBean.FIELD_OID.resolve(null));
            }
        };

        try {
            EnrByAttSummRecord enrByAttSummRecord = new EnrByAttSummRecord();
            enrByAttSummRecord.set(EnrByAttSummRecord.Field.schoolName, schoolName);
            enrByAttSummRecord.set(EnrByAttSummRecord.Field.schoolNumber, schoolNumber);
            enrByAttSummRecord.set(EnrByAttSummRecord.Field.boardNumber, boardNumber);
            enrByAttSummRecord.set(EnrByAttSummRecord.Field.boardName, boardName);
            enrByAttSummRecord.set(EnrByAttSummRecord.Field.academicYears, academicYears);
            enrByAttSummRecord.set(EnrByAttSummRecord.Field.programName, programName);
            enrByAttSummRecord.set(EnrByAttSummRecord.Field.currentDate, currentDate);
            enrByAttSummRecord.set(EnrByAttSummRecord.Field.pupilsOfBoardJuniorKindergarten,
                    retrieveJuniorKindergartenElementaryData(filtrableEnrollments));
            // TODO I would suggest we leave that row alone for now. The odds of having a student
            // with our
            // clients is pretty rare.
            // enrByAttSummRecord.set(EnrByAttSummRecord.Field.pupilsOfBoardHearingPreSchool,
            // retrieveHearingPreSchoolData(filtrableEnrollments));
            enrByAttSummRecord.set(EnrByAttSummRecord.Field.pupilsOfBoardKindergarten,
                    retrieveKindergartenElementaryData(filtrableEnrollments));
            enrByAttSummRecord.set(EnrByAttSummRecord.Field.pupilsOfBoardGrade1,
                    retrieveGrade1AgeData(filtrableEnrollments));
            enrByAttSummRecord.set(EnrByAttSummRecord.Field.pupilsOfBoardGrade1To3,
                    retrieveGrade1To3AgeData(filtrableEnrollments));
            enrByAttSummRecord.set(EnrByAttSummRecord.Field.pupilsOfBoardGrade4to8,
                    retrieveGrade4To8AgeData(filtrableEnrollments));
            enrByAttSummRecord.set(EnrByAttSummRecord.Field.pupilsOfBoardGrade4to6,
                    retrieveGrade4To6AgeData(filtrableEnrollments));
            enrByAttSummRecord.set(EnrByAttSummRecord.Field.pupilsOfBoardGrade7to8,
                    retrieveGrade7To8AgeData(filtrableEnrollments));
            enrByAttSummRecord.set(EnrByAttSummRecord.Field.nativeEducationAuthority,
                    retrieveNativeEducationAuthorityData(filtrableEnrollments));
            enrByAttSummRecord.set(EnrByAttSummRecord.Field.governmentOfCanada,
                    retrieveGovernmentOfCanadaData(filtrableEnrollments));
            enrByAttSummRecord.set(EnrByAttSummRecord.Field.studyPermitTempResident,
                    retrieveTempResidentData(filtrableEnrollments));
            enrByAttSummRecord.set(EnrByAttSummRecord.Field.otherStudents,
                    retrieveOtherStudentsData(filtrableEnrollments));
            enrByAttSummRecords.add(enrByAttSummRecord);
        } catch (DataErrorException e) {
            logError(e.getMessage());
        }

        return FilterableFactory.create(enrByAttSummRecords, EnrByAttSummRecord.s_uniqueFields,
                EnrByAttSummRecord.s_valueResolver);
    }

    /**
     * Initialize helpers.
     *
     * @param school OnSchool
     */
    private void initializeHelpersForSchool(OnSchool school) {
        m_sklDateRangeProvider = new SchoolDateRangeProvider();
        m_sklDateRangeProvider.setBroker(getBroker());
        m_sklDateRangeProvider.setSchool(school);
        m_spanCriteria = getSpanConfiguration(m_sklDateRangeProvider.getStartDate())
                .setSchoolOids(Arrays.asList(school.getOid()))
                .setIncludeSecondarySpans(true);
    }

    /**
     * Populates grid witn needed data.
     *
     * @param grid ReportDataGrid
     * @param container EnrByAttSummaryContainer
     * @param fieldName String
     */
    private void populateGridWithValues(ReportDataGrid grid,
                                        EnrByAttSummaryContainer container,
                                        String fieldName) {
        LinkedList<String> listOfValuesToSet = new LinkedList();
        listOfValuesToSet.add(0, container.getFullTimeU21NumberOfStudents().toString());
        listOfValuesToSet.add(1, m_decimalFormat.format(container.getFullTimeU21RegularFTE().doubleValue()));
        listOfValuesToSet.add(2, "");
        listOfValuesToSet.add(3, container.getPartTimeU21NumberOfStudents().toString());
        listOfValuesToSet.add(4, m_decimalFormat.format(container.getPartTimeU21RegularFTE().doubleValue()));
        listOfValuesToSet.add(5, "");
        listOfValuesToSet.add(6, container.getFullTimeO21NumberOfStudents().toString());
        listOfValuesToSet.add(7, container.getPartTimeO21NumberOfStudents().toString());
        listOfValuesToSet.add(8, m_decimalFormat.format(container.getPartTimeO21RegularFTE().doubleValue()));
        grid.set(fieldName, listOfValuesToSet);
    }

    /**
     * Computes data of summary report by filtering data from enrollments
     * according to provided filters.
     *
     * @param onEnrollments Filterable<OnEnrollment>
     * @param enrollmentFilters Map<String,Filter>
     * @return EnrByAttSummaryContainer
     */
    private EnrByAttSummaryContainer retrieveEnrollmentReportData(Filterable<OnEnrollment> onEnrollments,
                                                                  Map<String, Filter> enrollmentFilters) {
        EnrByAttSummaryContainer enrByAttendanceSumRecord = null;
        if (onEnrollments != null) {
            enrByAttendanceSumRecord =
                    new EnrByAttSummaryContainer();
            Collection<OnEnrollment> ftU21Enrollments = onEnrollments
                    .filter(enrollmentFilters.get(ENR_RECORDS_KEYS_BY_AGE_AND_ATTENDANCE.get(0))).extract();
            enrByAttendanceSumRecord
                    .setFullTimeU21NumberOfStudents(ftU21Enrollments.size());
            enrByAttendanceSumRecord.setFullTimeU21RegularFTE(
                    getFTEValue(ftU21Enrollments, EnrolmentRegister.FT.toString()).doubleValue());

            Collection<OnEnrollment> ptU21Enrollments = onEnrollments
                    .filter(enrollmentFilters.get(ENR_RECORDS_KEYS_BY_AGE_AND_ATTENDANCE.get(1))).extract();
            enrByAttendanceSumRecord
                    .setPartTimeU21NumberOfStudents(ptU21Enrollments.size());
            enrByAttendanceSumRecord.setPartTimeU21RegularFTE(
                    getFTEValue(ptU21Enrollments, EnrolmentRegister.PT.toString()).doubleValue());

            Collection<OnEnrollment> ftO21Enrollments = onEnrollments
                    .filter(enrollmentFilters.get(ENR_RECORDS_KEYS_BY_AGE_AND_ATTENDANCE.get(2))).extract();
            enrByAttendanceSumRecord
                    .setFullTimeO21NumberOfStudents(ftO21Enrollments.size());
            enrByAttendanceSumRecord.setFullTimeO21RegularFTE(
                    getFTEValue(ftO21Enrollments, EnrolmentRegister.FT.toString()).doubleValue());

            Collection<OnEnrollment> ptO21Enrollments = onEnrollments
                    .filter(enrollmentFilters.get(ENR_RECORDS_KEYS_BY_AGE_AND_ATTENDANCE.get(3))).extract();
            enrByAttendanceSumRecord
                    .setPartTimeO21NumberOfStudents(ptO21Enrollments.size());
            enrByAttendanceSumRecord.setPartTimeO21RegularFTE(
                    getFTEValue(ptO21Enrollments, EnrolmentRegister.PT.toString()).doubleValue());
        }
        return enrByAttendanceSumRecord;
    }

    /**
     * Computes data of 'Government of Canada' records.
     *
     * @param onEnrollments Filterable<OnEnrollment>
     * @return EnrByAttSummaryContainer
     */
    private EnrByAttSummaryContainer retrieveGovernmentOfCanadaData(Filterable<OnEnrollment> onEnrollments) {
        Map<String, Filter> enrollmentFilters = new HashMap<>();
        enrollmentFilters.put(ENR_RECORDS_KEYS_BY_AGE_AND_ATTENDANCE.get(0),
                createFileterByBoardResStatus(EnrolmentRegister.FT.toString(), BOARD_RES_STATUS_03, false));
        enrollmentFilters.put(ENR_RECORDS_KEYS_BY_AGE_AND_ATTENDANCE.get(1),
                createFileterByBoardResStatus(EnrolmentRegister.PT.toString(), BOARD_RES_STATUS_03, false));
        enrollmentFilters.put(ENR_RECORDS_KEYS_BY_AGE_AND_ATTENDANCE.get(2),
                createFileterByBoardResStatus(EnrolmentRegister.FT.toString(), BOARD_RES_STATUS_03, true));
        enrollmentFilters.put(ENR_RECORDS_KEYS_BY_AGE_AND_ATTENDANCE.get(3),
                createFileterByBoardResStatus(EnrolmentRegister.PT.toString(), BOARD_RES_STATUS_03, true));
        return retrieveEnrollmentReportData(onEnrollments, enrollmentFilters);
    }

    /**
     * Computes data of 'Grade 1 Age appropriate' records.
     *
     * @param onEnrollments Filterable<OnEnrollment>
     * @return EnrByAttSummaryContainer
     */
    private EnrByAttSummaryContainer retrieveGrade1AgeData(Filterable<OnEnrollment> onEnrollments) {
        Map<String, Filter> enrollmentFilters = new HashMap<>();
        enrollmentFilters.put(ENR_RECORDS_KEYS_BY_AGE_AND_ATTENDANCE.get(0),
                createFileterForGrade1Age(EnrolmentRegister.FT.toString(), GRADE_LEVEL_JK, GRADE_LEVEL_K,
                        BOARD_RES_STATUS_01, false));
        enrollmentFilters.put(ENR_RECORDS_KEYS_BY_AGE_AND_ATTENDANCE.get(1),
                createFileterForGrade1Age(EnrolmentRegister.PT.toString(), GRADE_LEVEL_JK, GRADE_LEVEL_K,
                        BOARD_RES_STATUS_01, false));

        enrollmentFilters.put(ENR_RECORDS_KEYS_BY_AGE_AND_ATTENDANCE.get(2),
                createFileterForGrade1Age(EnrolmentRegister.FT.toString(), GRADE_LEVEL_JK, GRADE_LEVEL_K,
                        BOARD_RES_STATUS_01, true));
        enrollmentFilters.put(ENR_RECORDS_KEYS_BY_AGE_AND_ATTENDANCE.get(3),
                createFileterForGrade1Age(EnrolmentRegister.PT.toString(), GRADE_LEVEL_JK, GRADE_LEVEL_K,
                        BOARD_RES_STATUS_01, true));

        return retrieveEnrollmentReportData(onEnrollments, enrollmentFilters);
    }

    /**
     * Computes data of 'Grades 1-3' records.
     *
     * @param onEnrollments Filterable<OnEnrollment>
     * @return EnrByAttSummaryContainer
     */
    private EnrByAttSummaryContainer retrieveGrade1To3AgeData(Filterable<OnEnrollment> onEnrollments) {
        Map<String, Filter> enrollmentFilters = new HashMap<>();
        enrollmentFilters.put(ENR_RECORDS_KEYS_BY_AGE_AND_ATTENDANCE.get(0),
                createFileterByGradeInterval(EnrolmentRegister.FT.toString(), INT_1, INT_3,
                        BOARD_RES_STATUS_01, false));
        enrollmentFilters.put(ENR_RECORDS_KEYS_BY_AGE_AND_ATTENDANCE.get(1),
                createFileterByGradeInterval(EnrolmentRegister.PT.toString(), INT_1, INT_3,
                        BOARD_RES_STATUS_01, false));

        enrollmentFilters.put(ENR_RECORDS_KEYS_BY_AGE_AND_ATTENDANCE.get(2),
                createFileterByGradeInterval(EnrolmentRegister.FT.toString(), INT_1, INT_3,
                        BOARD_RES_STATUS_01, true));
        enrollmentFilters.put(ENR_RECORDS_KEYS_BY_AGE_AND_ATTENDANCE.get(3),
                createFileterByGradeInterval(EnrolmentRegister.PT.toString(), INT_1, INT_3,
                        BOARD_RES_STATUS_01, true));

        return retrieveEnrollmentReportData(onEnrollments, enrollmentFilters);
    }

    /**
     * Computes data of 'Grades 4-8' records.
     *
     * @param onEnrollments Filterable<OnEnrollment>
     * @return EnrByAttSummaryContainer
     */
    private EnrByAttSummaryContainer retrieveGrade4To8AgeData(Filterable<OnEnrollment> onEnrollments) {
        Map<String, Filter> enrollmentFilters = new HashMap<>();
        enrollmentFilters.put(ENR_RECORDS_KEYS_BY_AGE_AND_ATTENDANCE.get(0),
                createFileterByGradeInterval(EnrolmentRegister.FT.toString(), INT_4, INT_8,
                        BOARD_RES_STATUS_01, false));
        enrollmentFilters.put(ENR_RECORDS_KEYS_BY_AGE_AND_ATTENDANCE.get(1),
                createFileterByGradeInterval(EnrolmentRegister.PT.toString(), INT_4, INT_8,
                        BOARD_RES_STATUS_01, false));

        enrollmentFilters.put(ENR_RECORDS_KEYS_BY_AGE_AND_ATTENDANCE.get(2),
                createFileterByGradeInterval(EnrolmentRegister.FT.toString(), INT_4, INT_8,
                        BOARD_RES_STATUS_01, true));
        enrollmentFilters.put(ENR_RECORDS_KEYS_BY_AGE_AND_ATTENDANCE.get(3),
                createFileterByGradeInterval(EnrolmentRegister.PT.toString(), INT_4, INT_8,
                        BOARD_RES_STATUS_01, true));

        return retrieveEnrollmentReportData(onEnrollments, enrollmentFilters);
    }

    /**
     * Computes data of 'Grades 4-6' records.
     *
     * @param onEnrollments Filterable<OnEnrollment>
     * @return EnrByAttSummaryContainer
     */
    private EnrByAttSummaryContainer retrieveGrade4To6AgeData(Filterable<OnEnrollment> onEnrollments) {
        Map<String, Filter> enrollmentFilters = new HashMap<>();
        enrollmentFilters.put(ENR_RECORDS_KEYS_BY_AGE_AND_ATTENDANCE.get(0),
                createFileterByGradeInterval(EnrolmentRegister.FT.toString(), INT_4, INT_6,
                        BOARD_RES_STATUS_01, false));
        enrollmentFilters.put(ENR_RECORDS_KEYS_BY_AGE_AND_ATTENDANCE.get(1),
                createFileterByGradeInterval(EnrolmentRegister.PT.toString(), INT_4, INT_6,
                        BOARD_RES_STATUS_01, false));

        enrollmentFilters.put(ENR_RECORDS_KEYS_BY_AGE_AND_ATTENDANCE.get(2),
                createFileterByGradeInterval(EnrolmentRegister.FT.toString(), INT_4, INT_6,
                        BOARD_RES_STATUS_01, true));
        enrollmentFilters.put(ENR_RECORDS_KEYS_BY_AGE_AND_ATTENDANCE.get(3),
                createFileterByGradeInterval(EnrolmentRegister.PT.toString(), INT_4, INT_6,
                        BOARD_RES_STATUS_01, true));

        return retrieveEnrollmentReportData(onEnrollments, enrollmentFilters);
    }

    /**
     * Computes data of 'Grades 7-8' records.
     *
     * @param onEnrollments Filterable<OnEnrollment>
     * @return EnrByAttSummaryContainer
     */
    private EnrByAttSummaryContainer retrieveGrade7To8AgeData(Filterable<OnEnrollment> onEnrollments) {
        Map<String, Filter> enrollmentFilters = new HashMap<>();
        enrollmentFilters.put(ENR_RECORDS_KEYS_BY_AGE_AND_ATTENDANCE.get(0),
                createFileterByGradeInterval(EnrolmentRegister.FT.toString(), INT_7, INT_8,
                        BOARD_RES_STATUS_01, false));
        enrollmentFilters.put(ENR_RECORDS_KEYS_BY_AGE_AND_ATTENDANCE.get(1),
                createFileterByGradeInterval(EnrolmentRegister.PT.toString(), INT_7, INT_8,
                        BOARD_RES_STATUS_01, false));

        enrollmentFilters.put(ENR_RECORDS_KEYS_BY_AGE_AND_ATTENDANCE.get(2),
                createFileterByGradeInterval(EnrolmentRegister.FT.toString(), INT_7, INT_8,
                        BOARD_RES_STATUS_01, true));
        enrollmentFilters.put(ENR_RECORDS_KEYS_BY_AGE_AND_ATTENDANCE.get(3),
                createFileterByGradeInterval(EnrolmentRegister.PT.toString(), INT_7, INT_8,
                        BOARD_RES_STATUS_01, true));

        return retrieveEnrollmentReportData(onEnrollments, enrollmentFilters);
    }

    // TODO I would suggest we leave that row alone for now. The odds of having a student with our
    // clients is pretty rare.
    // /**
    // * Computes data of 'Deaf/Hard of Hearing-Pre-school' records.
    // *
    // * @param OnEnrollments Filterable<OnEnrollment>
    // * @return EnrByAttSummaryContainer
    // */
    // private EnrByAttSummaryContainer retrieveHearingPreSchoolData(Filterable<OnEnrollment>
    // OnEnrollments) {
    // Map<String, Filter> enrollmentFilters = new HashMap<>();
    // return retrieveEnrollmentReportData(OnEnrollments, enrollmentFilters);
    // }

    /**
     * Computes data of 'Junior Kindergarten' records.
     *
     * @param onEnrollments Filterable<OnEnrollment>
     * @return EnrByAttSummaryContainer
     */
    private EnrByAttSummaryContainer retrieveJuniorKindergartenElementaryData(Filterable<OnEnrollment> onEnrollments) {
        Map<String, Filter> enrollmentFilters = new HashMap<>();
        enrollmentFilters.put(ENR_RECORDS_KEYS_BY_AGE_AND_ATTENDANCE.get(0),
                createFileterForFixedGradeLevel(EnrolmentRegister.FT.toString(), GRADE_LEVEL_JK,
                        BOARD_RES_STATUS_01, false));
        enrollmentFilters.put(ENR_RECORDS_KEYS_BY_AGE_AND_ATTENDANCE.get(1),
                createFileterForFixedGradeLevel(EnrolmentRegister.PT.toString(), GRADE_LEVEL_JK,
                        BOARD_RES_STATUS_01, false));

        enrollmentFilters.put(ENR_RECORDS_KEYS_BY_AGE_AND_ATTENDANCE.get(2),
                createFileterForFixedGradeLevel(EnrolmentRegister.FT.toString(), GRADE_LEVEL_JK,
                        BOARD_RES_STATUS_01, true));
        enrollmentFilters.put(ENR_RECORDS_KEYS_BY_AGE_AND_ATTENDANCE.get(3),
                createFileterForFixedGradeLevel(EnrolmentRegister.PT.toString(), GRADE_LEVEL_JK,
                        BOARD_RES_STATUS_01, true));

        return retrieveEnrollmentReportData(onEnrollments, enrollmentFilters);
    }

    /**
     * Computes data of 'Kindergarten' records.
     *
     * @param onEnrollments Filterable<OnEnrollment>
     * @return EnrByAttSummaryContainer
     */
    private EnrByAttSummaryContainer retrieveKindergartenElementaryData(Filterable<OnEnrollment> onEnrollments) {
        Map<String, Filter> enrollmentFilters = new HashMap<>();
        enrollmentFilters.put(ENR_RECORDS_KEYS_BY_AGE_AND_ATTENDANCE.get(0),
                createFileterForFixedGradeLevel(EnrolmentRegister.FT.toString(), GRADE_LEVEL_K,
                        BOARD_RES_STATUS_01, false));
        enrollmentFilters.put(ENR_RECORDS_KEYS_BY_AGE_AND_ATTENDANCE.get(1),
                createFileterForFixedGradeLevel(EnrolmentRegister.PT.toString(), GRADE_LEVEL_K,
                        BOARD_RES_STATUS_01, false));

        enrollmentFilters.put(ENR_RECORDS_KEYS_BY_AGE_AND_ATTENDANCE.get(2),
                createFileterForFixedGradeLevel(EnrolmentRegister.FT.toString(), GRADE_LEVEL_K,
                        BOARD_RES_STATUS_01, true));
        enrollmentFilters.put(ENR_RECORDS_KEYS_BY_AGE_AND_ATTENDANCE.get(3),
                createFileterForFixedGradeLevel(EnrolmentRegister.PT.toString(), GRADE_LEVEL_K,
                        BOARD_RES_STATUS_01, true));

        return retrieveEnrollmentReportData(onEnrollments, enrollmentFilters);
    }

    /**
     * Computes data of 'Native students under tuition agreement with a Native
     * education authority' records.
     *
     * @param onEnrollments Filterable<OnEnrollment>
     * @return EnrByAttSummaryContainer
     */
    private EnrByAttSummaryContainer retrieveNativeEducationAuthorityData(Filterable<OnEnrollment> onEnrollments) {
        Map<String, Filter> enrollmentFilters = new HashMap<>();
        enrollmentFilters.put(ENR_RECORDS_KEYS_BY_AGE_AND_ATTENDANCE.get(0),
                createFileterByBoardResStatus(EnrolmentRegister.FT.toString(), BOARD_RES_STATUS_02, false));
        enrollmentFilters.put(ENR_RECORDS_KEYS_BY_AGE_AND_ATTENDANCE.get(1),
                createFileterByBoardResStatus(EnrolmentRegister.PT.toString(), BOARD_RES_STATUS_02, false));

        enrollmentFilters.put(ENR_RECORDS_KEYS_BY_AGE_AND_ATTENDANCE.get(2),
                createFileterByBoardResStatus(EnrolmentRegister.FT.toString(), BOARD_RES_STATUS_02, true));
        enrollmentFilters.put(ENR_RECORDS_KEYS_BY_AGE_AND_ATTENDANCE.get(3),
                createFileterByBoardResStatus(EnrolmentRegister.PT.toString(), BOARD_RES_STATUS_02, true));

        return retrieveEnrollmentReportData(onEnrollments, enrollmentFilters);
    }

    /**
     * Computes data of 'Other Students' records.
     *
     * @param onEnrollments Filterable<OnEnrollment>
     * @return EnrByAttSummaryContainer
     */
    private EnrByAttSummaryContainer retrieveOtherStudentsData(Filterable<OnEnrollment> onEnrollments) {
        Map<String, Filter> enrollmentFilters = new HashMap<>();
        enrollmentFilters.put(ENR_RECORDS_KEYS_BY_AGE_AND_ATTENDANCE.get(0),
                createFileterByBoardResStatus(EnrolmentRegister.FT.toString(), BOARD_RES_STATUS_07, false));
        enrollmentFilters.put(ENR_RECORDS_KEYS_BY_AGE_AND_ATTENDANCE.get(1),
                createFileterByBoardResStatus(EnrolmentRegister.PT.toString(), BOARD_RES_STATUS_07, false));

        enrollmentFilters.put(ENR_RECORDS_KEYS_BY_AGE_AND_ATTENDANCE.get(2),
                createFileterByBoardResStatus(EnrolmentRegister.FT.toString(), BOARD_RES_STATUS_07, true));
        enrollmentFilters.put(ENR_RECORDS_KEYS_BY_AGE_AND_ATTENDANCE.get(3),
                createFileterByBoardResStatus(EnrolmentRegister.PT.toString(), BOARD_RES_STATUS_07, true));

        return retrieveEnrollmentReportData(onEnrollments, enrollmentFilters);
    }

    /**
     * Computes data of 'Study Permit/Temp Resident' records.
     *
     * @param onEnrollments Filterable<OnEnrollment>
     * @return EnrByAttSummaryContainer
     */
    private EnrByAttSummaryContainer retrieveTempResidentData(Filterable<OnEnrollment> onEnrollments) {
        Map<String, Filter> enrollmentFilters = new HashMap<>();
        enrollmentFilters.put(ENR_RECORDS_KEYS_BY_AGE_AND_ATTENDANCE.get(0),
                createFileterByBoardResStatus(EnrolmentRegister.FT.toString(), BOARD_RES_STATUS_05, false));
        enrollmentFilters.put(ENR_RECORDS_KEYS_BY_AGE_AND_ATTENDANCE.get(1),
                createFileterByBoardResStatus(EnrolmentRegister.PT.toString(), BOARD_RES_STATUS_05, false));

        enrollmentFilters.put(ENR_RECORDS_KEYS_BY_AGE_AND_ATTENDANCE.get(2),
                createFileterByBoardResStatus(EnrolmentRegister.FT.toString(), BOARD_RES_STATUS_05, true));
        enrollmentFilters.put(ENR_RECORDS_KEYS_BY_AGE_AND_ATTENDANCE.get(3),
                createFileterByBoardResStatus(EnrolmentRegister.PT.toString(), BOARD_RES_STATUS_05, true));

        return retrieveEnrollmentReportData(onEnrollments, enrollmentFilters);
    }

    /**
     * Gets the FTE value.
     *
     * @param stdEnrollments Collection<OnEnrollment>
     * @param attendanceType String
     * @return Big decimal
     */
    private BigDecimal getFTEValue(Collection<OnEnrollment> stdEnrollments,
                                   String attendanceType) {
        BigDecimal sumFTE = BigDecimal.ZERO;
        for (OnEnrollment stdEnrollment : stdEnrollments) {
            BigDecimal fte = stdEnrollment.getFte();

            BigDecimal finalFte = BigDecimal.ZERO;
            if (EnrolmentRegister.FT.toString().equals(attendanceType)) {
                finalFte = FTE_FT_FINAL_THRESHOLD.compareTo(fte) > 0 ? fte : BigDecimal.ONE;
            } else {
                finalFte = fte;
            }
            sumFTE = sumFTE.add(finalFte);
        }
        return sumFTE;
    }
}
