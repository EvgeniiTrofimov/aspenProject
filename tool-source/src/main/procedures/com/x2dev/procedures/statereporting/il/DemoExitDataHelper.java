/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2017 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.il;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictCalendar;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.StudentSchool;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.x2dev.procedures.statereporting.il.DemoExitDataHelper.DateDataStorePrimary.DataStorePrimary;
import com.x2dev.procedures.statereporting.il.DemoExitDataHelper.DateDataStoreSummer.DataStoreSummer;
import com.x2dev.procedures.statereporting.il.DemoExitDataHelper.SpansFactory.DemoExitSpan;
import com.x2dev.procedures.statereporting.il.DemoExitDataHelper.SpansFactory.StudentDemoDatasets;
import com.x2dev.procedures.statereporting.il.DemoExitDataHelper.SpansFactory.StudentDemoDatasets.DemoDataset;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.sis.tools.stateexports.StudentScheduleSpan;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.util.*;
import java.util.Map.Entry;
import org.apache.commons.lang3.builder.CompareToBuilder;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * This class is common data helper for IL Student Demographics and IL Exit Enrollment exports.
 * It is used for IL Student Demographics as calculator of DemoDatasets based on report date.
 * For IL Exit Enrollment it is used as calculator of DemoExitSpans based on comparing of the
 * DemoDatasets between Last Submission
 * Date and Current Submission Date.
 * It makes the exports interdependent, because result of IL Exit Enrollment depends of IL Student
 * Demographics results
 * on each date during date range.
 *
 * @author Follett Software Company
 */
/**
 * @author Follett Software Company
 */
public class DemoExitDataHelper {
    /**
     * This inner class is container of data that is calculated on concrete date and is used to
     * calculate DemoDatasets.
     *
     * @author Follett Software Company
     */
    protected class DateDataStorePrimary {
        /**
         * Container of data that is used to calculate DemoDatasets.
         *
         * @author Follett Software Company
         */
        protected class DataStorePrimary {
            private Map<String, StudentSchool> m_secondarySchoolMap = new HashMap<String, StudentSchool>();
            private Map<String, PlainDate> m_secondarySchoolMapEndDate = new HashMap<String, PlainDate>();

            /**
             * Instantiates a new data store primary.
             *
             * @param date PlainDate
             */
            public DataStorePrimary(PlainDate date) {
                initSecondarySchools(date);
            }

            /**
             * Gets the secondary school end date map.
             *
             * @return map of Secondary Schools End Dates where key is studentOid and value is
             *         secondary school End Date.
             */
            public Map<String, PlainDate> getSecondarySchoolEndDateMap() {
                return m_secondarySchoolMapEndDate;
            }

            /**
             * Gets the secondary school map.
             *
             * @return map of Secondary Schools where key is studentOid and value is student's
             *         Secondary School.
             */
            public Map<String, StudentSchool> getSecondarySchoolMap() {
                return m_secondarySchoolMap;
            }

            /**
             * Initializes Secondary Schools and Secondary School End Dates maps.
             *
             * @param date PlainDate
             */
            private void initSecondarySchools(PlainDate date) {
                for (StudentSchool secSchool : m_secondarySchools) {
                    if (!secSchool.getStartDate().after(date)) {
                        m_secondarySchoolMap.put(secSchool.getStudentOid(), secSchool);
                        if (secSchool.getEndDate() != null && secSchool.getEndDate().before(date)) {
                            m_secondarySchoolMapEndDate.put(secSchool.getStudentOid(), secSchool.getEndDate());
                        } else if (m_secondarySchoolMapEndDate.containsKey(secSchool.getStudentOid())) {
                            m_secondarySchoolMapEndDate.remove(secSchool.getStudentOid());
                        }
                    }
                }
            }
        }

        protected Collection<StudentSchool> m_secondarySchools = null;

        private Map<PlainDate, DataStorePrimary> m_structuresMap = new HashMap<PlainDate, DataStorePrimary>();

        /**
         * Instantiates a new date data store primary.
         */
        public DateDataStorePrimary() {
            initSecondarySchools();
            for (PlainDate date : m_dates) {
                DataStorePrimary strHelper = new DataStorePrimary(date);
                m_structuresMap.put(date, strHelper);
            }
        }

        /**
         * Gets the data store by date.
         *
         * @param date PlainDate
         * @return DataStore containing Secondary Schools and Secondary School End Dates maps on
         *         passed date.
         */
        public DataStorePrimary getDataStoreByDate(PlainDate date) {
            return m_structuresMap.get(date);
        }

        /**
         * Gets the second school end date map.
         *
         * @param date PlainDate
         * @return Secondary School End Date Map on passed date.
         */
        public Map<String, PlainDate> getSecondSchoolEndDateMap(PlainDate date) {
            return m_structuresMap.get(date).getSecondarySchoolEndDateMap();
        }

        /**
         * Gets the second school map.
         *
         * @param date PlainDate
         * @return Secondary School Map on passed date.
         */
        public Map<String, StudentSchool> getSecondSchoolMap(PlainDate date) {
            return m_structuresMap.get(date).getSecondarySchoolMap();
        }

        /**
         * Initializes collection of Secondary Schools to calculate Secondary School and Secondary
         * School End Date maps.
         */
        private void initSecondarySchools() {
            X2Criteria secondaryOutplacementCriteria = new X2Criteria();
            SubQuery studentSubQuery =
                    new SubQuery(SisStudent.class, X2BaseBean.COL_OID, getEnrollmentHelper().getStudentCriteria());
            secondaryOutplacementCriteria.addIn(StudentSchool.COL_STUDENT_OID, studentSubQuery);
            secondaryOutplacementCriteria.addEqualTo(StudentSchool.COL_TYPE, Integer.valueOf(StudentSchool.SECONDARY));
            secondaryOutplacementCriteria.addEqualTo(StudentSchool.COL_DISTRICT_CONTEXT_OID,
                    m_data.getOrganization().getCurrentContextOid());

            QueryByCriteria secondaryOutplacementQuery =
                    new QueryByCriteria(StudentSchool.class, secondaryOutplacementCriteria);
            secondaryOutplacementQuery.addOrderBy(StudentSchool.COL_START_DATE, true);

            m_secondarySchools = m_data.getBroker().getCollectionByQuery(secondaryOutplacementQuery);
        }
    }

    /**
     * This inner class is container of data that is calculated on concrete date and is used to
     * calculate DemoDatasets.
     *
     * @author Follett Software Company
     */
    protected class DateDataStoreSummer {
        private String m_fieldSummerIndicator =
                m_data.translateAliasToJavaName(StudentDemographics.ALIAS_SUMMER_IND, true);

        /**
         * Container of data that is used to calculate DemoDatasets.
         *
         * @author Follett Software Company
         */
        protected class DataStoreSummer {
            private Map<String, Collection<StudentSchool>> m_summerSchoolsMap =
                    new HashMap<String, Collection<StudentSchool>>();

            /**
             * Instantiates a new data store summer.
             *
             * @param date PlainDate
             */
            public DataStoreSummer(PlainDate date) {
                initSecondarySchools(date);
            }

            /**
             * Gets the student summer schools.
             *
             * @param stdOid String
             * @return Collection
             */
            public Collection<StudentSchool> getStudentSummerSchools(String stdOid) {
                Collection<StudentSchool> summerSchools = m_summerSchoolsMap.get(stdOid);
                if (summerSchools == null) {
                    summerSchools = new ArrayList<StudentSchool>();
                }
                return summerSchools;
            }

            /**
             * Initializes Secondary Schools and Secondary School End Dates maps.
             *
             * @param date PlainDate
             */
            private void initSecondarySchools(PlainDate date) {
                for (StudentSchool secSchool : m_summerSchools) {
                    if (!secSchool.getStartDate().after(date) &&
                            (secSchool.getEndDate() == null || !secSchool.getEndDate().before(date))) {
                        Collection<StudentSchool> schools = m_summerSchoolsMap.get(secSchool.getStudentOid());
                        if (schools == null) {
                            schools = new ArrayList<StudentSchool>();
                            m_summerSchoolsMap.put(secSchool.getStudentOid(), schools);
                        }
                        schools.add(secSchool);
                    }
                }
            }
        }

        protected Collection<StudentSchool> m_summerSchools = null;

        private Map<PlainDate, DataStoreSummer> m_structuresMap = new HashMap<PlainDate, DataStoreSummer>();

        /**
         * Instantiates a new date data store summer.
         */
        public DateDataStoreSummer() {
            initSummerSchools();
            for (PlainDate date : m_dates) {
                DataStoreSummer strHelper = new DataStoreSummer(date);
                m_structuresMap.put(date, strHelper);
            }
        }

        /**
         * Gets the data store by date.
         *
         * @param date PlainDate
         * @return DataStore containing Secondary Schools and Secondary School End Dates maps on
         *         passed date.
         */
        public DataStoreSummer getDataStoreByDate(PlainDate date) {
            return m_structuresMap.get(date);
        }

        /**
         * Initializes collection of Secondary Schools to calculate Secondary School and Secondary
         * School End Date maps.
         */
        private void initSummerSchools() {
            X2Criteria summerSchoolsCriteria = new X2Criteria();
            SubQuery studentSubQuery =
                    new SubQuery(SisStudent.class, X2BaseBean.COL_OID, getEnrollmentHelper().getStudentCriteria());
            summerSchoolsCriteria.addIn(StudentSchool.COL_STUDENT_OID, studentSubQuery);
            summerSchoolsCriteria.addEqualTo(
                    StudentSchool.REL_SCHOOL + ModelProperty.PATH_DELIMITER + m_fieldSummerIndicator,
                    BooleanAsStringConverter.TRUE);
            summerSchoolsCriteria.addEqualTo(StudentSchool.COL_TYPE, Integer.valueOf(StudentSchool.SECONDARY));
            summerSchoolsCriteria.addEqualTo(StudentSchool.COL_DISTRICT_CONTEXT_OID,
                    m_data.getOrganization().getCurrentContextOid());

            QueryByCriteria summerSchoolsQuery = new QueryByCriteria(StudentSchool.class, summerSchoolsCriteria);
            summerSchoolsQuery.addOrderBy(StudentSchool.COL_START_DATE, true);

            m_summerSchools = m_data.getBroker().getCollectionByQuery(summerSchoolsQuery);
        }
    }

    /**
     * Factory of spans. This class is used for:
     * 1) calculating of DemoDatasets for each date during selected date range
     * 2) calculating of DemoExitSpans during process of comparing of the calculated DemoDatasets on
     * each date with
     * next date during selected date range.
     *
     * @author Follett Software Company
     */
    protected class SpansFactory {
        /**
         * DemoExitSpan contains info from DemoDataset + Exit Date and Exit Code.
         *
         * @author Follett Software Company
         */
        protected class DemoExitSpan {
            final private DemoDataset m_demoDataset;

            private String m_exitCode = null;

            private PlainDate m_exitDate = null;

            /**
             * Instantiates a new demo exit span.
             *
             * @param dataset DemoDataset
             */
            public DemoExitSpan(DemoDataset dataset) {
                m_demoDataset = dataset;
            }

            /**
             * Equals.
             *
             * @param obj Object
             * @return true, if successful
             * @see java.lang.Object#equals(java.lang.Object)
             */
            @Override
            public boolean equals(Object obj) {
                if (!(obj instanceof DemoExitSpan)) {
                    return false;
                }
                if (obj == this) {
                    return true;
                }

                DemoExitSpan comparedSpan = (DemoExitSpan) obj;
                return new EqualsBuilder().append(m_demoDataset.getFte(), comparedSpan.m_demoDataset.getFte())
                        .append(m_demoDataset.getGradeLvl(), comparedSpan.m_demoDataset.getGradeLvl())
                        .append(m_demoDataset.getHomeSchoolCode(), comparedSpan.m_demoDataset.getHomeSchoolCode())
                        .append(m_demoDataset.getServiceSchoolCode(), comparedSpan.m_demoDataset.getServiceSchoolCode())
                        .append(m_exitDate, comparedSpan.m_exitDate).append(m_exitCode, comparedSpan.m_exitCode)
                        .isEquals();
            }

            /**
             * Gets the demo dataset.
             *
             * @return DemoDataset of span.
             */
            public DemoDataset getDemoDataset() {
                return m_demoDataset;
            }

            /**
             * Gets the exit code.
             *
             * @return Exit Code of span.
             */
            public String getExitCode() {
                return m_exitCode;
            }

            /**
             * Gets the exit date.
             *
             * @return exit date of span.
             */
            public PlainDate getExitDate() {
                return m_exitDate;
            }

            /**
             * Gets the home school.
             *
             * @return Home school code of span.
             */
            public String getHomeSchool() {
                return m_demoDataset.getHomeSchoolCode();
            }

            /**
             * Gets the school.
             *
             * @return School (Primary or Secondary, depending on type of record) of span.
             */
            public School getSchool() {
                return m_demoDataset.getSchool();
            }

            /**
             * Gets the serving school.
             *
             * @return Serving school code of span.
             */
            public String getServingSchool() {
                return m_demoDataset.getServiceSchoolCode();
            }

            /**
             * Gets the start date.
             *
             * @return start date of span.
             */
            public PlainDate getStartDate() {
                return m_demoDataset.getBeginDate();
            }

            /**
             * Hash code.
             *
             * @return int
             * @see java.lang.Object#hashCode()
             */
            @Override
            public int hashCode() {
                return new HashCodeBuilder(17, 31).append(m_demoDataset.getFte()).append(m_demoDataset.getGradeLvl())
                        .append(m_demoDataset.getHomeSchoolCode()).append(m_demoDataset.getServiceSchoolCode())
                        .toHashCode();
            }

            /**
             * Set exit code of span.
             *
             * @param exitCode void
             */
            public void setExitCode(String exitCode) {
                m_exitCode = exitCode;
            }

            /**
             * Set exit date of span.
             *
             * @param exitDate void
             */
            public void setExitDate(PlainDate exitDate) {
                m_exitDate = exitDate;
            }

            @Override
            public String toString() {
                StringBuilder output = new StringBuilder();
                output.append("[");
                output.append("School: ").append(getSchool() != null ? getSchool().getName() : "null").append(" - ");
                output.append("Start Date: ").append(getStartDate()).append(" - ");
                output.append("Exit Date: ").append(getExitDate()).append(" - ");
                output.append("Exit Code: ").append(getExitCode()).append(" - ");
                output.append("Home School: ").append(getHomeSchool()).append(" - ");
                output.append("Serving School: ").append(getServingSchool());
                output.append("]");
                return output.toString();
            }
        }

        /**
         * Container of student's DemoDatasets calculated on concrete date.
         *
         * @author Follett Software Company
         */
        protected class StudentDemoDatasets {
            /**
             * DemoDataset is container of next data:
             * 1)begin date of Demographics record
             * 2)fte of Demographics record
             * 3)grade level of Demographics record
             * 4)home school code of Demographics record
             * 5)instance of School (Primary or Secondary, depending on type of record)
             * 6)service school code of Demographics record.
             *
             * @author Follett Software Company
             */
            protected class DemoDataset {
                private PlainDate m_beginDate;
                private Float m_fte;
                private String m_gradeLvl;
                private String m_homeSchoolCode;
                private School m_school;
                private String m_serviceSchoolCode;

                /**
                 * Equals.
                 *
                 * @param obj Object
                 * @return true, if successful
                 * @see java.lang.Object#equals(java.lang.Object)
                 */
                @Override
                public boolean equals(Object obj) {
                    if (!(obj instanceof DemoDataset)) {
                        return false;
                    }
                    if (obj == this) {
                        return true;
                    }

                    DemoDataset comparedDataset = (DemoDataset) obj;
                    return new EqualsBuilder().append(m_beginDate, comparedDataset.m_beginDate)
                            .append(m_fte, comparedDataset.m_fte).append(m_gradeLvl, comparedDataset.m_gradeLvl)
                            .append(m_homeSchoolCode, comparedDataset.m_homeSchoolCode)
                            .append(m_serviceSchoolCode, comparedDataset.m_serviceSchoolCode).isEquals();
                }

                /**
                 * Gets the begin date.
                 *
                 * @return begin date of Demographics record
                 */
                public PlainDate getBeginDate() {
                    return m_beginDate;
                }

                /**
                 * Gets the fte.
                 *
                 * @return fte of Demographics record
                 */
                public Float getFte() {
                    return m_fte;
                }

                /**
                 * Gets the grade lvl.
                 *
                 * @return grade level of Demographics record
                 */
                public String getGradeLvl() {
                    return m_gradeLvl;
                }

                /**
                 * Gets the home school code.
                 *
                 * @return home school code of Demographics record
                 */
                public String getHomeSchoolCode() {
                    return m_homeSchoolCode;
                }

                /**
                 * Gets the school.
                 *
                 * @return instance of School (Primary or Secondary depending on type of record)
                 */
                public School getSchool() {
                    return m_school;
                }

                /**
                 * Gets the service school code.
                 *
                 * @return service school code of Demographics record
                 */
                public String getServiceSchoolCode() {
                    return m_serviceSchoolCode;
                }

                /**
                 * Hash code.
                 *
                 * @return int
                 * @see java.lang.Object#hashCode()
                 */
                @Override
                public int hashCode() {
                    return new HashCodeBuilder(17, 31). // two randomly chosen prime numbers
                    // if deriving: appendSuper(super.hashCode()).
                            append(m_beginDate).append(m_fte).append(m_gradeLvl).append(m_homeSchoolCode)
                            .append(m_serviceSchoolCode).toHashCode();
                }

                /**
                 * Set begin date of Demographics record.
                 *
                 * @param beginDate void
                 */
                public void setBeginDate(PlainDate beginDate) {
                    m_beginDate = beginDate;
                }

                /**
                 * Set FTE of Demographics record.
                 *
                 * @param fte void
                 */
                public void setFTE(Float fte) {
                    m_fte = fte;
                }

                /**
                 * Set grade level of Demographics record.
                 *
                 * @param gradeLvl void
                 */
                public void setGradeLvl(String gradeLvl) {
                    m_gradeLvl = gradeLvl;
                }

                /**
                 * Set home school of Demographics record.
                 *
                 * @param homeSchoolCode void
                 */
                public void setHomeSchoolCode(String homeSchoolCode) {
                    m_homeSchoolCode = homeSchoolCode;
                }

                /**
                 * Set instance of School (Primary or Secondary depending on type of record) .
                 *
                 * @param school void
                 */
                public void setSchool(School school) {
                    m_school = school;
                }

                /**
                 * Set service school code of Demographics record.
                 *
                 * @param serviceSchoolCode void
                 */
                public void setServiceSchoolCode(String serviceSchoolCode) {
                    m_serviceSchoolCode = serviceSchoolCode;
                }
            }

            /**
             * Only difference from parent class is access to End date.
             *
             * @author Follett Software Company
             */
            protected class DemoDatasetSummer extends DemoDataset {
                PlainDate m_endDate = null;
                StudentSchool m_summerSchool = null;

                /**
                 * Return End date.
                 *
                 * @return Plain date
                 */
                public PlainDate getEndDate() {
                    return m_endDate;
                }

                /**
                 * Return SummerSchool.
                 *
                 * @return Student school
                 */
                public StudentSchool getSummerSchool() {
                    return m_summerSchool;
                }

                /**
                 * Set End date.
                 *
                 * @param endDate void
                 */
                public void setEndDate(PlainDate endDate) {
                    m_endDate = endDate;
                }

                /**
                 * Sets the summer school.
                 *
                 * @param summerSchool void
                 */
                public void setSummerSchool(StudentSchool summerSchool) {
                    m_summerSchool = summerSchool;
                }
            }

            private Collection<DemoDataset> m_datasets = null;
            private boolean m_hasNoCreditInEitherSchool = false;
            private PlainDate m_nextAfterSecondInSessDate = null;
            private StudentEnrollment m_primaryEnrollment = null;
            private PlainDate m_reportDate = null;
            private boolean m_reportStudent = false;
            private Collection<StudentScheduleSpan> m_schedSpans = null;
            private boolean m_secEndDateAfterReportDate = false;
            private PlainDate m_secondaryEndDate = null;
            private StudentSchool m_secondaryStudentSchool = null;
            private SisStudent m_student = null;
            private Collection<StudentSchool> m_summerSchools = null;

            /**
             * Instantiates a new student demo datasets.
             *
             * @param student SisStudent
             * @param date PlainDate
             * @param schedSpans Collection<StudentScheduleSpan>
             * @param dataStore DataStorePrimary
             */
            public StudentDemoDatasets(SisStudent student, PlainDate date, Collection<StudentScheduleSpan> schedSpans,
                    DataStorePrimary dataStore) {
                m_reportStudent = reportStudent(student, date);
                m_secondaryStudentSchool = dataStore.getSecondarySchoolMap().get(student.getOid());
                m_secondaryEndDate = dataStore.getSecondarySchoolEndDateMap().get(student.getOid());
                m_secEndDateAfterReportDate = m_secondaryEndDate == null || m_secondaryEndDate.after(date);
                m_nextAfterSecondInSessDate = getNextInSessionDate(m_secondaryEndDate, date);
                m_primaryEnrollment = getEnrollmentHelper().getEnrollmentForDate(student.getOid(), date, "EYS");
                m_schedSpans = getScheduleSpansOnDate(schedSpans, date);

                m_student = student;
                m_reportDate = date;
            }

            /**
             * Instantiates a new student demo datasets.
             *
             * @param student SisStudent
             * @param date PlainDate
             * @param schedSpans Collection<StudentScheduleSpan>
             * @param dataStore DataStoreSummer
             */
            public StudentDemoDatasets(SisStudent student, PlainDate date, Collection<StudentScheduleSpan> schedSpans,
                    DataStoreSummer dataStore) {
                m_reportStudent = reportStudent(student, date);
                m_summerSchools = dataStore.getStudentSummerSchools(student.getOid());
                m_secEndDateAfterReportDate = m_secondaryEndDate == null || m_secondaryEndDate.after(date);
                m_nextAfterSecondInSessDate = getNextInSessionDate(m_secondaryEndDate, date);
                m_primaryEnrollment = getEnrollmentHelper().getEnrollmentForDate(student.getOid(), date, "EYS");
                m_schedSpans = getScheduleSpansOnDate(schedSpans, date);

                m_student = student;
                m_reportDate = date;
            }

            /**
             * Method needed to determine if we can avoid unnecessary calculations, as result
             * increase performance.
             *
             * @param sdd StudentDemoDatasets
             * @return true if result of calculation of passed StudentDemoDatasets will be the same
             *         as result of
             *         calculation for current StudentDemoDataset, otherwise false.
             */
            public boolean compareBeforeInitializing(StudentDemoDatasets sdd) {
                boolean result = false;

                if (m_reportStudent == sdd.m_reportStudent &&

                        (m_secondaryStudentSchool == sdd.m_secondaryStudentSchool ||
                                (m_secondaryStudentSchool != null && sdd.m_secondaryStudentSchool != null
                                        && m_secondaryStudentSchool.equals(sdd.m_secondaryStudentSchool)))
                        &&

                        (m_summerSchools == sdd.m_summerSchools ||
                                (m_summerSchools != null && sdd.m_summerSchools != null
                                        && m_summerSchools.equals(sdd.m_summerSchools)))
                        &&

                        (m_secondaryEndDate == sdd.m_secondaryEndDate ||
                                (m_secondaryEndDate != null && sdd.m_secondaryEndDate != null
                                        && m_secondaryEndDate.equals(sdd.m_secondaryEndDate)))
                        &&

                        m_secEndDateAfterReportDate == sdd.m_secEndDateAfterReportDate &&

                        (m_nextAfterSecondInSessDate == sdd.m_nextAfterSecondInSessDate ||
                                (m_nextAfterSecondInSessDate != null && sdd.m_nextAfterSecondInSessDate != null
                                        && m_nextAfterSecondInSessDate.equals(sdd.m_nextAfterSecondInSessDate)))
                        &&

                        (m_primaryEnrollment == sdd.m_primaryEnrollment ||
                                (m_primaryEnrollment != null && sdd.m_primaryEnrollment != null
                                        && m_primaryEnrollment.equals(sdd.m_primaryEnrollment)))
                        &&

                        (m_schedSpans.equals(sdd.m_schedSpans) ||
                        // In this case cannot construct DemoDatasets
                                (m_primaryEnrollment == sdd.m_primaryEnrollment) ||
                                datasetsFteEquals(
                                        getDatasetsInPrimaryMode(m_primaryEnrollment, m_secondaryStudentSchool,
                                                m_secEndDateAfterReportDate, m_schedSpans),
                                        getDatasetsInPrimaryMode(sdd.m_primaryEnrollment, sdd.m_secondaryStudentSchool,
                                                sdd.m_secEndDateAfterReportDate, sdd.m_schedSpans)))) {
                    result = true;
                }

                return result;
            }

            /**
             * Gets the datasets.
             *
             * @return Collection
             */
            public Collection<DemoDataset> getDatasets() {
                return m_datasets;
            }

            /**
             * Gets the date.
             *
             * @return date that was used to calculate student's DemoDatasets
             */
            public PlainDate getDate() {
                return m_reportDate;
            }

            /**
             * Gets the student.
             *
             * @return student of StudentDemoDataset
             */
            public SisStudent getStudent() {
                return m_student;
            }

            /**
             * Initialize DemoDatasets.
             */
            public void initializeDemoDatasets() {
                if (getHelperMode().equals(MODE_PRIMARY_SCHOOL)) {
                    initPrimaryDemoDatasets(m_reportStudent, m_primaryEnrollment, m_secondaryStudentSchool,
                            m_schedSpans, m_secEndDateAfterReportDate);
                } else if (getHelperMode().equals(MODE_SUMMER_SCHOOL)) {
                    initSummerDemoDatasets(m_reportStudent, m_primaryEnrollment, m_summerSchools,
                            m_schedSpans, m_secEndDateAfterReportDate);
                }

            }

            /**
             * Set DemoDatasets.
             *
             * @param datasets void
             */
            public void setDatasets(Collection<DemoDataset> datasets) {
                m_datasets = datasets;
            }

            /**
             * Set report date.
             *
             * @param reportDate void
             */
            public void setDate(PlainDate reportDate) {
                m_reportDate = reportDate;
            }

            /**
             * Adjust begin date:
             *
             * If begin date < first day of school,
             * return the first day of school for reporting year as the begin date.
             *
             * If begin date < day of primary enrollment (it's last enrollment)
             * return date of the primary enrollment
             *
             *
             * @param enrollmentDate PlainDate
             * @param primaryEnrollment StudentEnrollment
             * @return adjusted begin date
             */
            private PlainDate adjustBeginDate(PlainDate enrollmentDate, StudentEnrollment primaryEnrollment) {
                PlainDate beginDate = enrollmentDate;
                /*
                 * If begin date < first day of school,
                 * return the first day of school for reporting year as the begin date.
                 */
                PlainDate yearStartDate = getSchoolStartDate(primaryEnrollment.getSchool());

                if (beginDate.before(yearStartDate)) {
                    beginDate = yearStartDate;
                }

                /*
                 * If begin date < day of primary enrollment (it's last enrollment)
                 * return date of the primary enrollment
                 */
                if (beginDate.before(primaryEnrollment.getEnrollmentDate())) {
                    beginDate = primaryEnrollment.getEnrollmentDate();
                }

                return beginDate;
            }

            /**
             * Returns true if datasets have the same fte.
             *
             * @param leftDatasets ArrayList<DemoDataset>
             * @param rightDataset ArrayList<DemoDataset>
             * @return fteAreEquals
             */
            private boolean datasetsFteEquals(ArrayList<DemoDataset> leftDatasets,
                                              ArrayList<DemoDataset> rightDataset) {
                boolean fteAreEquals = false;
                if (leftDatasets.size() == rightDataset.size()) {
                    for (int i = 0; i < leftDatasets.size(); i++) {
                        if (!leftDatasets.get(i).getFte().equals(rightDataset.get(i).getFte())) {
                            break;
                        }
                        fteAreEquals = true;
                    }
                }

                return fteAreEquals;
            }

            /**
             * Create datasets with instance of School and FTE.
             *
             * @param primaryEnrollment StudentEnrollment
             * @param secondaryStudentSchool StudentSchool
             * @param secEndDateAfterReportDate boolean
             * @param scheduleSpans Collection<StudentScheduleSpan>
             * @return Array list
             */
            private ArrayList<DemoDataset> getDatasetsInPrimaryMode(StudentEnrollment primaryEnrollment,
                                                                    StudentSchool secondaryStudentSchool,
                                                                    boolean secEndDateAfterReportDate,
                                                                    Collection<StudentScheduleSpan> scheduleSpans) {
                SisSchool primarySchool = primaryEnrollment.getSchool();
                boolean notCalculatePrimaryFte = BooleanAsStringConverter.TRUE
                        .equals(primarySchool.getFieldValueByBeanPath(m_fieldSklNonCalcFte));

                boolean notCalculateSecondaryFte = false;
                if (secondaryStudentSchool != null && secondaryStudentSchool.getSchool() != null) {
                    notCalculateSecondaryFte = BooleanAsStringConverter.TRUE
                            .equals(secondaryStudentSchool.getSchool().getFieldValueByBeanPath(m_fieldSklNonCalcFte));
                }

                ArrayList<DemoDataset> datasets = new ArrayList<DemoDataset>();

                Map<School, Float> studentSchoolsPeriods = new HashMap<School, Float>();
                studentSchoolsPeriods.put(primarySchool, Float.valueOf(0));

                // Include secondary school if it active.
                // An Active Secondary Enrollment is determined based on the School Year being =
                // report year and the
                // start date found in the secondary enrollment being < = 'Report Date' where end
                // date is blank OR > 'Report Date'
                School secondarySchool = null;
                Float overridenFte = null;

                if (secondaryStudentSchool != null) {
                    if (secEndDateAfterReportDate) {
                        String fteToParse = (String) secondaryStudentSchool.getFieldValueByBeanPath(m_fieldFteOverride);
                        if (!StringUtils.isEmpty(fteToParse)) {
                            overridenFte = Float.valueOf(Float.parseFloat(fteToParse));
                        }

                        secondarySchool = secondaryStudentSchool.getSchool();
                        studentSchoolsPeriods.put(secondarySchool, Float.valueOf(0));
                    }
                }

                DemoDataset primaryDataset = null;
                DemoDataset secondaryDataset = null;

                /*
                 * If student has a Secondary Enrollment where StudentSchoolAssociation.[DOE
                 * OUTPLACED FTE OVERRIDE] is not null or zero:
                 * o The Secondary School association is returned will the value found in DOE
                 * OUTPLACED FTE OVERRIDE.
                 * o The Primary School shall be calculated as the difference between 1.0 and value
                 * found
                 * in DOE OUTPLACED FTE OVERRIDE.
                 */
                if (overridenFte != null && overridenFte.floatValue() > 0) {
                    float primarySchoolFTE = notCalculatePrimaryFte ? getPrimaryEnrFte(primaryEnrollment)
                            : 1.0F - overridenFte.floatValue();
                    primaryDataset = new DemoDataset();
                    primaryDataset.setFTE(Float.valueOf(primarySchoolFTE));

                    secondaryDataset = new DemoDataset();
                    secondaryDataset.setFTE(overridenFte);
                }
                /*
                 * Otherwise FTE will always be 1.0 unless a student has a Primary and Secondary
                 * Enrollment where both schools
                 * have credit earning classes in their scheduled based on 'Report Date'.
                 */
                else {
                    /*
                     * Calculate FTE based on SUM of the number of class periods that the student
                     * has in both of the schools.
                     * o The total number of class periods for the student where crsCredit not null
                     * and > 0 becomes the denominator in the equation.
                     * o The numerator is the total number of periods for credit earning classes
                     * from the Primary School of enrollment.
                     */
                    for (StudentScheduleSpan span : scheduleSpans) {
                        School spanSchool = span.getSection().getSchedule().getSchool();
                        BigDecimal credit = span.getSection().getSchoolCourse().getCourse().getCredit();
                        // count section in FTE calculation if
                        // 1)span from primary or secondary school
                        // 2)it was added on or before Report Date
                        // 3)it has exit date on or after Report Date
                        // 4)crsCredit not null and > 0
                        if (studentSchoolsPeriods.containsKey(spanSchool) &&
                                credit != null && credit.compareTo(new BigDecimal(0)) > 0) {
                            if (m_sectionNumOfPeriodsMap == null) {
                                initializeNumOfPeriods();
                            }
                            if (m_sectionNumOfSheduleDays == null) {
                                initializeNumOfScheduleDays();
                            }
                            /*
                             * Get number of periods for this class per day.
                             */
                            Integer numOfPeriods = m_sectionNumOfPeriodsMap.get(span.getSection().getOid());

                            if (numOfPeriods == null) {
                                numOfPeriods = Integer.valueOf(0);
                            }

                            Integer numOfDays = m_sectionNumOfSheduleDays.get(span.getSection().getOid());

                            float periodsPerScheduleDay = numOfPeriods.floatValue() / numOfDays.floatValue();

                            float totalSchoolPeriodsNum = studentSchoolsPeriods.get(spanSchool).floatValue();
                            studentSchoolsPeriods.put(spanSchool,
                                    Float.valueOf(totalSchoolPeriodsNum + periodsPerScheduleDay));
                        }
                    }

                    float totalPeriodsNum = 0F;
                    for (Float periodNumbers : studentSchoolsPeriods.values()) {
                        totalPeriodsNum += periodNumbers.floatValue();
                    }

                    if (totalPeriodsNum == 0F
                            || secondarySchool == null
                            || studentSchoolsPeriods.get(secondarySchool) == null
                            || studentSchoolsPeriods.get(secondarySchool).floatValue() == 0F) {
                        primaryDataset = new DemoDataset();
                        primaryDataset.setFTE(getPrimaryEnrFte(primaryEnrollment));

                        if (secondaryStudentSchool != null
                                && (studentSchoolsPeriods.get(primarySchool).floatValue() == 0F
                                        && (studentSchoolsPeriods.get(secondarySchool) == null
                                                || studentSchoolsPeriods.get(secondarySchool).floatValue() == 0F))) {
                            m_hasNoCreditInEitherSchool = true;
                        }
                    } else {
                        float primarySchoolFTE = notCalculatePrimaryFte ? 1.0F
                                : studentSchoolsPeriods.get(primarySchool).floatValue() / totalPeriodsNum;
                        if (primarySchoolFTE != 0F) {
                            primaryDataset = new DemoDataset();
                            primaryDataset.setFTE(Float.valueOf(primarySchoolFTE));
                        }

                        float secondarySpanFTE = notCalculateSecondaryFte ? 1.0F : 1.0F - primarySchoolFTE;
                        secondaryDataset = new DemoDataset();
                        secondaryDataset.setFTE(Float.valueOf(secondarySpanFTE));
                    }
                }

                if (primaryDataset != null) {
                    primaryDataset.setSchool(primarySchool);
                    datasets.add(primaryDataset);
                }

                if (secondaryDataset != null) {
                    secondaryDataset.setSchool(secondarySchool);
                    datasets.add(secondaryDataset);
                }

                return datasets;
            }

            private Float getPrimaryEnrFte(StudentEnrollment primaryEnrollment) {
                Float enrFte = 1.0F;
                String enrFteString = (String) primaryEnrollment.getFieldValueByBeanPath(m_fieldEnrFteOverride);
                if (!StringUtils.isEmpty(enrFteString)) {
                    enrFte = new Float(Float.parseFloat(enrFteString));
                    if (enrFte <= 0F) {
                        enrFte = 1.0F;
                    }
                }
                return enrFte;
            }

            /**
             * Create datasets with instance of School and FTE.
             *
             * @param primaryEnrollment StudentEnrollment
             * @param studentSchools Collection<StudentSchool>
             * @param secEndDateAfterReportDate boolean
             * @param scheduleSpans Collection<StudentScheduleSpan>
             * @return Collection
             */
            private Collection<DemoDataset> getDatasetsInSummerMode(StudentEnrollment primaryEnrollment,
                                                                    Collection<StudentSchool> studentSchools,
                                                                    boolean secEndDateAfterReportDate,
                                                                    Collection<StudentScheduleSpan> scheduleSpans) {
                SisSchool primarySchool = primaryEnrollment.getSchool();
                String excludeSchoolIndicator =
                        (String) primarySchool.getFieldValueByAlias(StudentDemographics.ALIAS_EXCLUDE_SKL);
                Collection<DemoDataset> datasets = new ArrayList<DemoDataset>();

                Map<School, Map<StudentSchool, Float>> summerSchoolsPeriods =
                        new HashMap<School, Map<StudentSchool, Float>>();

                /*
                 * From the Specification Version 1.0 on 6/30/2016:
                 * "The export will consider the School.[DOE EXCLUDE SKL] indicator.
                 * If set to true for a primary school, all students assigned in that school shall
                 * be excluded EVEN
                 * If they have an active secondary enrollment record.
                 * All information regarding student start dates and end dates are pulled from the
                 * Secondary Enrollment Record(s) only. No consideration is made to the student's
                 * primary enrollment
                 * record other than to determine the "RCDTS Homeschool".
                 */
                if (StringUtils.isEmpty(excludeSchoolIndicator) ||
                        !excludeSchoolIndicator.equals(BooleanAsStringConverter.TRUE)) {
                    // Include secondary school if it active.
                    // An Active Secondary Enrollment is determined based on the School Year being =
                    // report year and the
                    // start date found in the secondary enrollment being < = 'Report Date' where
                    // end date is blank OR > 'Report Date'
                    for (StudentSchool studentSchool : studentSchools) {
                        School summerSchool = null;
                        if (studentSchool != null) {
                            if (secEndDateAfterReportDate) {
                                summerSchool = studentSchool.getSchool();
                                Map<StudentSchool, Float> stdSklPeriodsMap = summerSchoolsPeriods.get(summerSchool);
                                if (stdSklPeriodsMap == null) {
                                    stdSklPeriodsMap = new HashMap<StudentSchool, Float>();
                                    summerSchoolsPeriods.put(summerSchool, stdSklPeriodsMap);
                                }
                                stdSklPeriodsMap.put(studentSchool, Float.valueOf(0));
                            }
                        }
                    }

                    /*
                     * Calculate FTE based on SUM of the number of class periods that the student
                     * has in summer schools.
                     * o The total number of class periods for the student where crsCredit not null
                     * and > 0 becomes the denominator in the equation.
                     * o The numerator is the total number of periods for credit earning classes
                     * from the Primary School of enrollment.
                     */
                    for (StudentScheduleSpan span : scheduleSpans) {
                        School spanSchool = span.getSection().getSchedule().getSchool();
                        BigDecimal credit = span.getSection().getSchoolCourse().getCourse().getCredit();
                        // count section in FTE calculation if
                        // 1)span from secondary school with [DOE SUMMER IND] = true
                        // 2)it was added on or before Report Date
                        // 3)it has exit date on or after Report Date
                        // 4)crsCredit not null and > 0
                        if (summerSchoolsPeriods.containsKey(spanSchool) &&
                                credit != null && credit.compareTo(new BigDecimal(0)) > 0) {
                            if (m_sectionNumOfPeriodsMap == null) {
                                initializeNumOfPeriods();
                            }
                            if (m_sectionNumOfSheduleDays == null) {
                                initializeNumOfScheduleDays();
                            }
                            /*
                             * Get number of periods for this class per day.
                             */
                            Integer numOfPeriods = m_sectionNumOfPeriodsMap.get(span.getSection().getOid());

                            if (numOfPeriods == null) {
                                numOfPeriods = Integer.valueOf(0);
                            }

                            Integer numOfDays = m_sectionNumOfSheduleDays.get(span.getSection().getOid());

                            float periodsPerScheduleDay = numOfPeriods.floatValue() / numOfDays.floatValue();
                            Map<StudentSchool, Float> stdSklPeroidsMap = summerSchoolsPeriods.get(spanSchool);
                            for (Entry<StudentSchool, Float> entry : stdSklPeroidsMap.entrySet()) {
                                float totalStdSchoolPeriodsNum = entry.getValue().floatValue();
                                entry.setValue(Float.valueOf(totalStdSchoolPeriodsNum + periodsPerScheduleDay));
                            }
                        }
                    }
                    // Determine summerSchool with multiple StudentSchools and divide periods number
                    // on equal parts.
                    for (Map<StudentSchool, Float> stdSklPeriodsMap : summerSchoolsPeriods.values()) {
                        if (stdSklPeriodsMap.size() > 1) {
                            for (Entry<StudentSchool, Float> innerEntry : stdSklPeriodsMap.entrySet()) {
                                Float dividedPerNumber =
                                        Float.valueOf(innerEntry.getValue().floatValue() / stdSklPeriodsMap.size());
                                innerEntry.setValue(dividedPerNumber);
                            }
                        }
                    }
                }

                float totalPeriodsNum = 0F;
                for (Map<StudentSchool, Float> stdSklPeriodsMap : summerSchoolsPeriods.values()) {
                    for (Float periodsNumber : stdSklPeriodsMap.values()) {
                        totalPeriodsNum += periodsNumber.floatValue();
                    }
                }

                /*
                 * FTE will always be 1.0 unless a student has a multiple secondary enrollments
                 * where both schools
                 * have credit earning classes in their scheduled based on "Report Date".
                 * EXCLUDE students from returning IF they have no active credit earning courses.
                 */
                if (totalPeriodsNum != 0F && !summerSchoolsPeriods.isEmpty()) {
                    for (Entry<School, Map<StudentSchool, Float>> studentSchoolPeriod : summerSchoolsPeriods
                            .entrySet()) {
                        School school = studentSchoolPeriod.getKey();
                        Map<StudentSchool, Float> stdSklPeriodsNum = studentSchoolPeriod.getValue();
                        for (Entry<StudentSchool, Float> entry : stdSklPeriodsNum.entrySet()) {
                            DemoDatasetSummer dataset = new DemoDatasetSummer();
                            dataset.setSchool(school);
                            dataset.setFTE(BooleanAsStringConverter.TRUE
                                    .equals(school.getFieldValueByBeanPath(m_fieldSklNonCalcFte))
                                            ? Float.valueOf(Double.valueOf(1.0).floatValue())
                                            : Float.valueOf(entry.getValue().floatValue() / totalPeriodsNum));
                            dataset.setSummerSchool(entry.getKey());
                            dataset.setEndDate(entry.getKey().getEndDate());

                            if (dataset.getFte().floatValue() > 0) {
                                datasets.add(dataset);
                            }
                        }
                    }
                } else {
                    datasets = Collections.EMPTY_LIST;
                }

                return datasets;
            }

            /**
             * Gets the grade lvl.
             *
             * @param yog Integer
             * @return grade level based on passed YOG.
             */
            private String getGradeLvl(Integer yog) {
                if (!m_gradeLevelMap.containsKey(yog)) {
                    List<String> matchingGradeLevels =
                            StudentManager.getMatchingGradeLevels(m_maxGradeLevel, yog.intValue(),
                                    m_data.getCurrentContext().getSchoolYear(), m_sortedGradeLevels);

                    String gradeLevel = matchingGradeLevels == null || matchingGradeLevels.size() == 0 ? ""
                            : matchingGradeLevels.get(0);
                    if (!StringUtils.isEmpty(gradeLevel)) {
                        gradeLevel = m_data.lookupStateValue(SisStudent.class, SisStudent.COL_GRADE_LEVEL, gradeLevel);
                    }
                    m_gradeLevelMap.put(yog, gradeLevel);
                }
                return m_gradeLevelMap.get(yog);
            }

            /**
             * Gets the next in session date.
             *
             * @param date PlainDate
             * @param reportDate PlainDate
             * @return first in session date in district calendar after passed date.
             */
            private PlainDate getNextInSessionDate(PlainDate date, PlainDate reportDate) {
                PlainDate firstInSessionDate = null;
                if (date != null) {
                    for (DistrictCalendar day : m_days) {
                        if (day.getInSessionIndicator() && day.getDate().after(date)) {
                            // Initialize by first day from collection if wasn't initialized to this
                            // point.
                            if (firstInSessionDate == null) {
                                firstInSessionDate = day.getDate();
                            }

                            if (firstInSessionDate.after(day.getDate())) {
                                firstInSessionDate = day.getDate();
                            }
                        }
                    }

                    /*
                     * If the next instructional date is null or after "Report date", remove record.
                     */
                    if (firstInSessionDate != null && firstInSessionDate.after(reportDate)) {
                        firstInSessionDate = null;
                    }
                }
                return firstInSessionDate;
            }

            /**
             * Gets the schedule spans on date.
             *
             * @param spans Collection<StudentScheduleSpan>
             * @param date PlainDate
             * @return collection of StudentScheduleSpan for student on passed date.
             */
            private Collection<StudentScheduleSpan> getScheduleSpansOnDate(Collection<StudentScheduleSpan> spans,
                                                                           PlainDate date) {
                ArrayList<StudentScheduleSpan> schedSpansOnDate = new ArrayList<StudentScheduleSpan>();
                for (StudentScheduleSpan span : spans) {
                    if (span.getEntryDate() != null && span.getExitDate() != null && !span.getEntryDate().after(date)
                            && !span.getExitDate().before(date)) {
                        schedSpansOnDate.add(span);
                    }
                }

                return schedSpansOnDate;
            }

            /**
             * Gets the school start date.
             *
             * @param school SisSchool
             * @return first year date of passed school
             */
            private PlainDate getSchoolStartDate(SisSchool school) {
                PlainDate yearStartDate = null;

                if (school != null && school.getActiveSchedule() != null) {
                    yearStartDate = school.getActiveSchedule().getStartDate();
                } else {
                    yearStartDate = m_data.getOrganization().getCurrentContext().getStartDate();
                }

                return yearStartDate;
            }

            /**
             * Initialize RCDTS Serving School and Begin date for Primary Record.
             *
             * @param dataset DemoDataset
             */
            private void initializePrimaryDataset(DemoDataset dataset, StudentEnrollment primEnr) {
                setBeginDateForPrimary(dataset);
                setServingSchoolForPrimary(dataset, primEnr);
            }

            /**
             * Initialize RCDTS Serving School and Begin date for Secondary Record.
             *
             * @param dataset DemoDataset
             */
            private void initializeSecondaryDataset(DemoDataset dataset, StudentEnrollment primEnr) {
                // If there is active secondary school based on report date
                if (getHelperMode().equals(MODE_PRIMARY_SCHOOL) && m_secondaryStudentSchool != null) {
                    setBeginDateFromSecondary(dataset, m_secondaryStudentSchool);
                    initServingSklFromSecEnr(dataset, m_secondaryStudentSchool, primEnr);
                } else if (getHelperMode().equals(MODE_SUMMER_SCHOOL)) {
                    setBeginDateFromSecondary(dataset, ((DemoDatasetSummer) dataset).getSummerSchool());
                    initServingSklFromSecEnr(dataset, ((DemoDatasetSummer) dataset).getSummerSchool(), primEnr);
                }
            }

            /**
             * Initialize DemoDatasets based on passed parameters.
             * NOTE: this method should return the same result each time when passed parameters are
             * equal. Needed to
             * avoid unnecessary calculations to increase performance.
             *
             * @param reportStudent boolean
             * @param primaryEnrollment StudentEnrollment
             * @param secondaryStudentSchool StudentSchool
             * @param schedSpans Collection<StudentScheduleSpan>
             * @param secEndDateAfterReportDate boolean
             */
            private void initPrimaryDemoDatasets(boolean reportStudent,
                                                 StudentEnrollment primaryEnrollment,
                                                 StudentSchool secondaryStudentSchool,
                                                 Collection<StudentScheduleSpan> schedSpans,
                                                 boolean secEndDateAfterReportDate) {
                if (reportStudent) {
                    m_datasets = getDatasetsInPrimaryMode(primaryEnrollment, secondaryStudentSchool,
                            secEndDateAfterReportDate, schedSpans);

                    SisSchool primarySchool = primaryEnrollment.getSchool();
                    String homeSchool = null;
                    String codeForNonFte = null;
                    // RCDTS Homeschool will ALWAYS be the student's primary school of enrollment.
                    if (BooleanAsStringConverter.TRUE
                            .equals(primarySchool.getFieldValueByBeanPath(m_fieldSklNonCalcFte))) {
                        if (StudentEnrollment.YOG_CHANGE.equals(primaryEnrollment.getEnrollmentType()) ||
                                StudentEnrollment.STATUS_CHANGE.equals(primaryEnrollment.getEnrollmentType())) {
                            primaryEnrollment =
                                    getEnrollmentHelper().getEnrollmentForDate(primaryEnrollment.getStudent().getOid(),
                                            m_currentSubmissionDate, StudentEnrollment.ENTRY);
                        }
                        codeForNonFte = (String) primaryEnrollment.getFieldValueByBeanPath(m_fieldEnrSklHome);
                        if (!StringUtils.isEmpty(codeForNonFte)) {
                            codeForNonFte =
                                    m_data.lookupStateValue(StudentEnrollment.class, m_fieldEnrSklHome, codeForNonFte);
                        }
                    }
                    homeSchool = !StringUtils.isEmpty(codeForNonFte) ? codeForNonFte
                            : (String) primaryEnrollment.getSchool().getFieldValueByBeanPath(m_fieldSchoolCode);
                    String gradelvl = getGradeLvl(Integer.valueOf(primaryEnrollment.getYog()));
                    for (DemoDataset dataset : m_datasets) {
                        dataset.setHomeSchoolCode(homeSchool);
                        dataset.setGradeLvl(gradelvl);

                        // Initialize primary record.
                        if (dataset.getSchool().equals(primarySchool)) {
                            initializePrimaryDataset(dataset, primaryEnrollment);
                            // If there is no begin date for primary record, skip the student.
                            if (dataset.getBeginDate() == null) {
                                m_datasets = Collections.EMPTY_LIST;
                                break;
                            }
                        }
                        // Initialize secondary record.
                        else {
                            initializeSecondaryDataset(dataset, primaryEnrollment);
                        }
                    }
                } else {
                    m_datasets = Collections.EMPTY_LIST;
                }
            }

            /**
             * Initialize DemoDatasets based on passed parameters.
             * NOTE: this method should return the same result each time when passed parameters are
             * equal. Needed to
             * avoid unnecessary calculations to increase performance.
             *
             * @param reportStudent boolean
             * @param primaryEnrollment StudentEnrollment
             * @param summerSchools Collection<StudentSchool>
             * @param schedSpans Collection<StudentScheduleSpan>
             * @param secEndDateAfterReportDate boolean
             */
            private void initSummerDemoDatasets(boolean reportStudent,
                                                StudentEnrollment primaryEnrollment,
                                                Collection<StudentSchool> summerSchools,
                                                Collection<StudentScheduleSpan> schedSpans,
                                                boolean secEndDateAfterReportDate) {
                if (reportStudent) {
                    m_datasets = getDatasetsInSummerMode(primaryEnrollment, summerSchools, secEndDateAfterReportDate,
                            schedSpans);

                    SisSchool primarySchool = primaryEnrollment.getSchool();
                    String homeSchool = null;
                    String codeForNonFte = null;
                    // RCDTS Homeschool will ALWAYS be the student's primary school of enrollment.
                    if (BooleanAsStringConverter.TRUE
                            .equals(primarySchool.getFieldValueByBeanPath(m_fieldSklNonCalcFte))) {
                        if (StudentEnrollment.YOG_CHANGE.equals(primaryEnrollment.getEnrollmentType()) ||
                                StudentEnrollment.STATUS_CHANGE.equals(primaryEnrollment.getEnrollmentType())) {
                            primaryEnrollment =
                                    getEnrollmentHelper().getEnrollmentForDate(primaryEnrollment.getStudent().getOid(),
                                            m_currentSubmissionDate, StudentEnrollment.ENTRY);
                        }
                        codeForNonFte = (String) primaryEnrollment.getFieldValueByBeanPath(m_fieldEnrSklHome);
                        if (!StringUtils.isEmpty(codeForNonFte)) {
                            codeForNonFte =
                                    m_data.lookupStateValue(StudentEnrollment.class, m_fieldEnrSklHome, codeForNonFte);
                        }
                    }
                    homeSchool = !StringUtils.isEmpty(codeForNonFte) ? codeForNonFte
                            : (String) primaryEnrollment.getSchool().getFieldValueByBeanPath(m_fieldSchoolCode);

                    // RCDTS Homeschool will ALWAYS be the student's primary school of enrollment.
                    String gradelvl = getGradeLvl(Integer.valueOf(primaryEnrollment.getYog()));
                    for (DemoDataset dataset : m_datasets) {
                        if (!dataset.getSchool().equals(primarySchool)) {
                            dataset.setHomeSchoolCode(homeSchool);
                            dataset.setGradeLvl(gradelvl);

                            initializeSecondaryDataset(dataset, primaryEnrollment);
                        }
                    }
                    /*
                     * The export will NOT return records for summer school enrollments where
                     * RCDTS Home school and Serving School are equal AND FTE = 1.0.
                     * We ONLY need to report students that are attending a school that is not their
                     * home school.
                     * We will need to report students that are attending a different serving school
                     * IF they are also attending their home school.
                     * Here are some use cases:
                     * 1) Student's home school and serving school are equal and FTE = 1.00 =
                     * Student is not returned in the file
                     * 2) Student's home school and serving school are not equal and FTE = 1.00
                     * Student is returned.
                     * 3) Student's home school and serving school are equal and FTE < 1.00 student
                     * is returned.
                     * This student would also have an additional record where home school and
                     * serving school are NOT equal that would be returned.
                     * 4) Student's home school and serving school are not equal and FTE is < 1.00.
                     * This student would have two records indicating the appropriate FTE at each
                     * serving school.
                     *
                     */
                    Iterator<DemoDataset> iterator = m_datasets.iterator();
                    while (iterator.hasNext()) {
                        DemoDataset dataset = iterator.next();
                        if (((dataset.getHomeSchoolCode() == dataset.getServiceSchoolCode()) ||
                                (dataset.getHomeSchoolCode() != null && dataset.getServiceSchoolCode() != null &&
                                        dataset.getHomeSchoolCode().equals(dataset.getServiceSchoolCode())))
                                &&
                                dataset.getFte().floatValue() == 1) {
                            iterator.remove();
                        }
                    }
                } else {
                    m_datasets = Collections.EMPTY_LIST;
                }
            }

            /**
             * Initialize Serving school code from Secondary school.
             *
             * @param dataset DemoDataset
             * @param secondaryStudentSchool StudentSchool
             */
            private void initServingSklFromSecEnr(DemoDataset dataset,
                                                  StudentSchool secondaryStudentSchool,
                                                  StudentEnrollment primEnr) {
                if (secondaryStudentSchool != null) {
                    String servingSchool =
                            (String) secondaryStudentSchool.getFieldValueByBeanPath(m_fieldRcdtsForServingSchool);
                    servingSchool =
                            m_data.lookupStateValue(StudentSchool.class, m_fieldRcdtsForServingSchool, servingSchool);
                    if (StringUtils.isEmpty(servingSchool)) {
                        School secondarySchool = secondaryStudentSchool.getSchool();
                        servingSchool = (String) secondarySchool.getFieldValueByBeanPath(m_fieldSchoolCode);
                    }

                    if (BooleanAsStringConverter.TRUE
                            .equals(secondaryStudentSchool.getSchool().getFieldValueByBeanPath(m_fieldSklNonCalcFte))) {
                        if (StudentEnrollment.YOG_CHANGE.equals(primEnr.getEnrollmentType()) ||
                                StudentEnrollment.STATUS_CHANGE.equals(primEnr.getEnrollmentType())) {
                            primEnr =
                                    getEnrollmentHelper().getEnrollmentForDate(primEnr.getStudent().getOid(),
                                            m_currentSubmissionDate, StudentEnrollment.ENTRY);
                        }
                        String code = (String) primEnr.getFieldValueByBeanPath(m_fieldEnrSklService);
                        if (!StringUtils.isEmpty(code)) {
                            servingSchool =
                                    m_data.lookupStateValue(StudentEnrollment.class, m_fieldEnrSklService, code);
                        }
                    }

                    dataset.setServiceSchoolCode(servingSchool);
                }
            }

            /**
             * Report student.
             *
             * @param student SisStudent
             * @param date PlainDate
             * @return true if student should be report, otherwise false.
             */
            private boolean reportStudent(SisStudent student, PlainDate date) {
                boolean reportStudent = true;

                StudentEnrollment activeEnrollment =
                        getEnrollmentHelper().getEnrollmentForDate(student.getOid(), date, "EWYS");
                StudentEnrollment eyEnrollment =
                        getEnrollmentHelper().getEnrollmentForDate(student.getOid(), date, "EY");

                if (activeEnrollment == null || !m_activeCode.equals(activeEnrollment.getStatusCode()) ||
                        eyEnrollment == null || eyEnrollment.getSchool() == null ||
                        BooleanAsStringConverter.TRUE
                                .equals(eyEnrollment.getSchool().getFieldValueByBeanPath(m_excludeSklField))) {
                    reportStudent = false;
                }

                return reportStudent;
            }

            /**
             * Returns begin date for Primary Record.
             *
             * @param dataset void
             */
            private void setBeginDateForPrimary(DemoDataset dataset) {
                /*
                 * Set begin date as primary enrollment date.
                 */
                PlainDate enrollmentDate = m_primaryEnrollment.getEnrollmentDate();

                PlainDate beginDate = adjustBeginDate(enrollmentDate, m_primaryEnrollment);
                /*
                 * IF a student has a secondary enrollment record in the current year that
                 * has ended based on 'Report Date', the begin date for the Primary Enrollment
                 * record will be the next instructional date found after the date ending the
                 * secondary enrollment.
                 */
                if (m_secondaryEndDate != null && m_secondaryEndDate.after(beginDate)) {
                    beginDate = m_nextAfterSecondInSessDate;
                }
                // Use start date of active secondary school if there are active secondary school
                // and only one record
                // should be exported.
                if (m_secondaryStudentSchool != null &&
                        (m_secondaryEndDate == null || beginDate != null && m_secondaryEndDate.after(beginDate)) &&
                        m_datasets.size() == 1) {
                    beginDate = adjustBeginDate(m_secondaryStudentSchool.getStartDate(), m_primaryEnrollment);
                }

                dataset.setBeginDate(beginDate);

                // When a student has a Primary Enrollment and later has a secondary enrollment, and
                // the FTE
                // for the student changes, student should return with two records where start date
                // on the
                // Primary Enrollment record is Start Date found on the student's secondary
                // enrollment record.
                if (m_secondaryStudentSchool != null && m_datasets.size() > 1 &&
                        m_secondaryStudentSchool.getStartDate().after(beginDate) &&
                        // Only if primary begin date before secondary end date.
                        (m_secondaryEndDate == null || m_secondaryEndDate.after(beginDate))) {
                    setBeginDateFromSecondary(dataset, m_secondaryStudentSchool);
                }
            }

            /**
             * Returns begin date from Secondary Record.
             *
             * @param dataset DemoDataset
             * @param studentSchool StudentSchool
             */
            private void setBeginDateFromSecondary(DemoDataset dataset, StudentSchool studentSchool) {
                PlainDate beginDate = studentSchool.getStartDate();
                beginDate = adjustBeginDate(beginDate, m_primaryEnrollment);
                dataset.setBeginDate(beginDate);
            }


            /**
             * Returns serving school code for Primary Dataset.
             *
             * @param dataset void
             */
            private void setServingSchoolForPrimary(DemoDataset dataset, StudentEnrollment primEnr) {
                School activeSecondarySchool = null;
                if (m_secondaryStudentSchool != null &&
                        (m_secondaryEndDate == null || dataset.getBeginDate() != null
                                && m_secondaryEndDate.after(dataset.getBeginDate()))) {
                    activeSecondarySchool = m_secondaryStudentSchool.getSchool();
                }
                String servingCode = null;

                /*
                 * RCDTS Serving School = home school for record if:
                 * 1) student has a primary enrollment and does not have an active secondary
                 * enrollment based on 'Report Date'
                 * 2) student has active credit earning courses in both schools
                 */
                if (activeSecondarySchool != null && m_datasets.size() == 1) {
                    if (!BooleanAsStringConverter.TRUE
                            .equals(primEnr.getSchool().getFieldValueByBeanPath(m_fieldSklNonCalcFte))) {
                        if (m_hasNoCreditInEitherSchool) {
                            servingCode =
                                    (String) m_secondaryStudentSchool
                                            .getFieldValueByBeanPath(m_fieldRcdtsForServingSchool);
                            servingCode =
                                    m_data.lookupStateValue(StudentSchool.class, m_fieldRcdtsForServingSchool,
                                            servingCode);
                            if (StringUtils.isEmpty(servingCode)) {
                                servingCode =
                                        (String) activeSecondarySchool.getFieldValueByBeanPath(m_fieldSchoolCode);
                            }
                        } else {
                            servingCode = (String) primEnr.getFieldValueByBeanPath(m_fieldEnrSklService);
                            if (!StringUtils.isEmpty(servingCode)) {
                                servingCode =
                                        m_data.lookupStateValue(StudentEnrollment.class, m_fieldEnrSklService,
                                                servingCode);
                            }
                        }
                        dataset.setServiceSchoolCode(!StringUtils.isEmpty(servingCode) ? servingCode
                                : (String) primEnr.getSchool().getFieldValueByBeanPath(m_fieldSchoolCode));
                    } else {
                        if (StudentEnrollment.YOG_CHANGE.equals(primEnr.getEnrollmentType()) ||
                                StudentEnrollment.STATUS_CHANGE.equals(primEnr.getEnrollmentType())) {
                            primEnr =
                                    getEnrollmentHelper().getEnrollmentForDate(primEnr.getStudent().getOid(),
                                            m_currentSubmissionDate, StudentEnrollment.ENTRY);
                        }
                        servingCode = (String) primEnr.getFieldValueByBeanPath(m_fieldEnrSklService);
                        if (!StringUtils.isEmpty(servingCode)) {
                            servingCode =
                                    m_data.lookupStateValue(StudentEnrollment.class, m_fieldEnrSklService,
                                            servingCode);
                        }
                        dataset.setServiceSchoolCode(!StringUtils.isEmpty(servingCode) ? servingCode
                                : (String) primEnr.getSchool().getFieldValueByBeanPath(m_fieldSchoolCode));

                    }
                } else if (activeSecondarySchool == null) {
                    if (!BooleanAsStringConverter.TRUE
                            .equals(primEnr.getSchool().getFieldValueByBeanPath(m_fieldSklNonCalcFte))) {
                        dataset.setServiceSchoolCode(dataset.getHomeSchoolCode());
                    } else {
                        if (StudentEnrollment.YOG_CHANGE.equals(primEnr.getEnrollmentType())
                                || StudentEnrollment.STATUS_CHANGE.equals(primEnr.getEnrollmentType())) {
                            primEnr =
                                    getEnrollmentHelper().getEnrollmentForDate(primEnr.getStudent().getOid(),
                                            m_currentSubmissionDate, StudentEnrollment.ENTRY);
                        }
                        servingCode = (String) primEnr.getFieldValueByBeanPath(m_fieldEnrSklService);
                        if (!StringUtils.isEmpty(servingCode)) {
                            servingCode =
                                    m_data.lookupStateValue(StudentEnrollment.class, m_fieldEnrSklService,
                                            servingCode);
                        }
                        dataset.setServiceSchoolCode(!StringUtils.isEmpty(servingCode) ? servingCode
                                : (String) primEnr.getSchool().getFieldValueByBeanPath(m_fieldSchoolCode));
                    }
                }
                /*
                 * Otherwise get from Primary school.
                 */
                else if (m_datasets.size() > 1) {
                    dataset.setServiceSchoolCode(
                            (String) primEnr.getSchool().getFieldValueByBeanPath(m_fieldSchoolCode));
                }
            }
        }

        protected Map<String, Integer> m_sectionNumOfPeriodsMap = null;
        protected Map<String, Integer> m_sectionNumOfSheduleDays = null;
        protected Map<SisStudent, HashSet<DemoExitSpan>> m_studentSpans = null;

        private Map<SisStudent, TreeMap<PlainDate, Collection<DemoDataset>>> m_studentDateDatasetsMap =
                new HashMap<SisStudent, TreeMap<PlainDate, Collection<DemoDataset>>>();

        /**
         * Initialize new instance of StudentDemoDatasets, add to m_studentDemoDatasetMap and
         * return.
         *
         * @param student SisStudent
         * @param date PlainDate
         * @param schedSpans Collection<StudentScheduleSpan>
         * @param previousStudWithDatasets StudentDemoDatasets
         * @return initialized StudentDemoDatasets
         */
        public StudentDemoDatasets addStudentWithDatasets(SisStudent student,
                                                          PlainDate date,
                                                          Collection<StudentScheduleSpan> schedSpans,
                                                          StudentDemoDatasets previousStudWithDatasets) {
            StudentDemoDatasets studentDatasets = null;
            // We need this object to share supporting objects e.g. primary enrollment, secondary
            // school etc.
            // and to further generation of DemoExitSpans.
            if (getHelperMode().equals(MODE_PRIMARY_SCHOOL)) {
                studentDatasets = new StudentDemoDatasets(student, date, schedSpans,
                        m_dateDataStorePrimary.getDataStoreByDate(date));
            } else if (getHelperMode().equals(MODE_SUMMER_SCHOOL)) {
                studentDatasets = new StudentDemoDatasets(student, date, schedSpans,
                        m_dateDataStoreSummer.getDataStoreByDate(date));
            }

            // Compare parameters of previous instance with parameters of just created instance. If
            // they are equal,
            // use datasets of previous instance and set report date as current date.
            if (previousStudWithDatasets != null
                    && previousStudWithDatasets.compareBeforeInitializing(studentDatasets)) {
                studentDatasets.setDatasets(previousStudWithDatasets.getDatasets());
                studentDatasets.setDate(date);
            } else {
                studentDatasets.initializeDemoDatasets();
            }

            if (studentDatasets.getDatasets() != null) {
                TreeMap<PlainDate, Collection<DemoDataset>> dateDatasets = m_studentDateDatasetsMap.get(student);

                if (dateDatasets == null) {
                    dateDatasets = new TreeMap<PlainDate, Collection<DemoDataset>>();
                    m_studentDateDatasetsMap.put(student, dateDatasets);
                }
                // Store new object only if it is not equal to the previous dataset,
                // should improve performance.
                PlainDate previousDate = getPreviousPlainDate(studentDatasets.getDate());
                Collection<DemoDataset> previousDatasetCollection = dateDatasets.get(previousDate);
                if (previousDatasetCollection != null
                        && previousDatasetCollection.equals(studentDatasets.getDatasets())) {
                    dateDatasets.put(studentDatasets.getDate(), previousDatasetCollection);
                } else {
                    dateDatasets.put(studentDatasets.getDate(), studentDatasets.getDatasets());
                }
            }

            return studentDatasets;
        }

        /**
         * Find dataset using comparator.
         *
         * @param datasets List<DemoDataset>
         * @param dataset DemoDataset
         * @param comparator Comparator<DemoDataset>
         * @return index of founded DemoDataset if it founded, otherwise negative value.
         */
        public int findDatasetUsingComparator(List<DemoDataset> datasets,
                                              DemoDataset dataset,
                                              Comparator<DemoDataset> comparator) {
            Collections.sort(datasets, comparator);
            return Collections.binarySearch(datasets, dataset, comparator);
        }

        /**
         * Gets the datasets.
         *
         * @param student SisStudent
         * @param date PlainDate
         * @return DemoDatasets of students on passed date.
         */
        public Collection<DemoDataset> getDatasets(SisStudent student, PlainDate date) {
            Collection<DemoDataset> datasets = new ArrayList<DemoDataset>();
            Map<PlainDate, Collection<DemoDataset>> dateDatasets = m_studentDateDatasetsMap.get(student);
            if (dateDatasets != null) {
                datasets = dateDatasets.get(date);
            }

            return datasets;
        }

        /**
         * Gets the demo exit spans.
         *
         * @param student SisStudent
         * @return DemoExitSpans of passed student.
         */
        public HashSet<DemoExitSpan> getDemoExitSpans(SisStudent student) {
            if (m_studentSpans == null) {
                calcSpansInPrimaryMode();
            }

            return m_studentSpans.get(student);
        }

        /**
         * Calculate DemoExitSpans of students in Primary mode.
         */
        protected void calcSpansInPrimaryMode() {
            m_studentSpans = new HashMap<SisStudent, HashSet<DemoExitSpan>>();

            for (Entry<SisStudent, TreeMap<PlainDate, Collection<DemoDataset>>> entry : m_studentDateDatasetsMap
                    .entrySet()) {
                SisStudent student = entry.getKey();

                List<DemoDataset> previousDatasets = null;
                HashSet<DemoExitSpan> spans = new HashSet<DemoExitSpan>();

                Map<PlainDate, Collection<DemoDataset>> datasetsByDates = entry.getValue();

                for (Entry<PlainDate, Collection<DemoDataset>> datasetsOnDate : datasetsByDates.entrySet()) {
                    PlainDate date = datasetsOnDate.getKey();
                    Collection<DemoDataset> currentDatasets = datasetsOnDate.getValue();

                    if (previousDatasets == null) {
                        for (DemoDataset dataset : currentDatasets) {
                            addSpan(spans, dataset);
                        }
                    } else if (currentDatasets.isEmpty() && !previousDatasets.isEmpty()) {
                        accessAppToFinishPrevSpans(student, previousDatasets, spans, date);
                    } else {
                        for (DemoDataset currentDataset : currentDatasets) {
                            if (!previousDatasets.contains(currentDataset)) {
                                /*
                                 * Try to find changed dataset.
                                 */
                                // Check if Serving school or FTE were changed.
                                int i = -1;
                                if ((i = findDatasetUsingComparator(previousDatasets, currentDataset,
                                        m_comparatorWOServSchool)) >= 0 ||
                                        (i = findDatasetUsingComparator(previousDatasets, currentDataset,
                                                m_comparatorWOFTE)) >= 0
                                        ||
                                        (i = findDatasetUsingComparator(previousDatasets, currentDataset,
                                                m_comparatorWOServSchoolAndFTE)) >= 0) {
                                    DemoDataset firstPreviousDataset = previousDatasets.get(i);
                                    DemoExitSpan firstSpanWithPrevDataset =
                                            findSpanByDataset(spans, firstPreviousDataset);

                                    if (firstSpanWithPrevDataset != null) {
                                        firstSpanWithPrevDataset.setExitCode("17");
                                        firstSpanWithPrevDataset.setExitDate(getLastInSessionDate(date));
                                    }

                                    // Remove founded dataset from previous datasets and run search
                                    // again
                                    // (needed to finish both records if num of records = 2)
                                    previousDatasets.remove(i);

                                    if ((i = findDatasetUsingComparator(previousDatasets, currentDataset,
                                            m_comparatorWOServSchool)) >= 0 ||
                                            (i = findDatasetUsingComparator(previousDatasets, currentDataset,
                                                    m_comparatorWOFTE)) >= 0
                                            ||
                                            (i = findDatasetUsingComparator(previousDatasets, currentDataset,
                                                    m_comparatorWOServSchoolAndFTE)) >= 0) {
                                        DemoDataset secondPreviousDataset = previousDatasets.get(i);
                                        DemoExitSpan secondSpanWithPrevDataset =
                                                findSpanByDataset(spans, secondPreviousDataset);
                                        if (secondSpanWithPrevDataset != null) {
                                            secondSpanWithPrevDataset.setExitCode("17");
                                            secondSpanWithPrevDataset.setExitDate(getLastInSessionDate(date));
                                        }
                                    }
                                    addSpan(spans, currentDataset);
                                }
                                // Check if grade level was changed.
                                else if ((i = findDatasetUsingComparator(previousDatasets, currentDataset,
                                        m_comparatorWOGradeLvl)) >= 0 ||
                                        (i = findDatasetUsingComparator(previousDatasets, currentDataset,
                                                m_comparatorWOGradeLvlAndFTE)) >= 0) {
                                    DemoDataset firstPreviousDataset = previousDatasets.get(i);
                                    DemoExitSpan firstSpanWithPrevDataset =
                                            findSpanByDataset(spans, firstPreviousDataset);

                                    // Remove founded dataset from previous datasets and run search
                                    // again
                                    // (needed to finish both records if num of records = 2)
                                    previousDatasets.remove(i);

                                    DemoDataset secondPreviousDataset = null;
                                    DemoExitSpan secondSpanWithPrevDataset = null;

                                    if ((i = findDatasetUsingComparator(previousDatasets, currentDataset,
                                            m_comparatorWOGradeLvl)) >= 0) {
                                        secondPreviousDataset = previousDatasets.get(i);
                                        secondSpanWithPrevDataset = findSpanByDataset(spans, secondPreviousDataset);
                                    }

                                    Integer previousYog = null;
                                    Integer currentYog = null;

                                    for (Entry<Integer, String> entryYogGrade : m_gradeLevelMap.entrySet()) {
                                        if (entryYogGrade.getValue().equals(firstPreviousDataset.getGradeLvl())) {
                                            previousYog = entryYogGrade.getKey();
                                        } else if (entryYogGrade.getValue().equals(currentDataset.getGradeLvl())) {
                                            currentYog = entryYogGrade.getKey();
                                        }
                                    }

                                    if (currentYog.intValue() > previousYog.intValue()) {
                                        if (firstSpanWithPrevDataset != null) {
                                            firstSpanWithPrevDataset.setExitCode("12");
                                        }
                                        if (secondSpanWithPrevDataset != null) {
                                            secondSpanWithPrevDataset.setExitCode("12");
                                        }
                                    } else {
                                        if (firstSpanWithPrevDataset != null) {
                                            firstSpanWithPrevDataset.setExitCode("05");
                                        }
                                        if (secondSpanWithPrevDataset != null) {
                                            secondSpanWithPrevDataset.setExitCode("05");
                                        }
                                    }

                                    firstSpanWithPrevDataset.setExitDate(getLastInSessionDate(date));
                                    if (secondSpanWithPrevDataset != null) {
                                        secondSpanWithPrevDataset.setExitDate(getLastInSessionDate(date));
                                    }

                                    addSpan(spans, currentDataset);
                                }
                                // Otherwise check if the student withdrawals from the RCDTS home
                                // school.
                                else if (previousDatasets.size() > 0 &&
                                        !previousDatasets.iterator().next().getHomeSchoolCode()
                                                .equals(currentDataset.getHomeSchoolCode())) {
                                    accessAppToFinishPrevSpans(student, previousDatasets, spans, date);

                                    addSpans(spans, currentDatasets);
                                    break;
                                } else {
                                    addSpan(spans, currentDataset);
                                }
                            }
                        }
                    }
                    previousDatasets = new ArrayList<>(currentDatasets);
                }

                m_studentSpans.put(student, spans);
            }
        }

        /**
         * Calculate DemoExitSpans of students in Summer mode.
         */
        protected void calcSpansInSummerMode() {
            m_studentSpans = new HashMap<SisStudent, HashSet<DemoExitSpan>>();

            for (Entry<SisStudent, TreeMap<PlainDate, Collection<DemoDataset>>> entry : m_studentDateDatasetsMap
                    .entrySet()) {
                SisStudent student = entry.getKey();

                List<DemoDataset> previousDatasets = null;
                HashSet<DemoExitSpan> spans = new HashSet<DemoExitSpan>();

                Map<PlainDate, Collection<DemoDataset>> datasetsByDates = entry.getValue();

                for (Entry<PlainDate, Collection<DemoDataset>> datasetsOnDate : datasetsByDates.entrySet()) {
                    Collection<DemoDataset> currentDatasets = datasetsOnDate.getValue();
                    PlainDate date = datasetsOnDate.getKey();

                    if (previousDatasets == null) {
                        for (DemoDataset dataset : currentDatasets) {
                            addSpan(spans, dataset);
                        }
                    } else {
                        for (DemoDataset currentDataset : currentDatasets) {
                            if (!previousDatasets.contains(currentDataset)) {
                                /*
                                 * Try to find changed dataset.
                                 */
                                // Check if Serving school or FTE were changed.
                                int i = -1;
                                if ((i = findDatasetUsingComparator(previousDatasets, currentDataset,
                                        m_comparatorWOServSchool)) >= 0 ||
                                        (i = findDatasetUsingComparator(previousDatasets, currentDataset,
                                                m_comparatorWOFTE)) >= 0
                                        ||
                                        (i = findDatasetUsingComparator(previousDatasets, currentDataset,
                                                m_comparatorWOServSchoolAndFTE)) >= 0) {
                                    DemoDataset previousDataset = previousDatasets.get(i);
                                    DemoExitSpan spanWithPrevDataset = findSpanByDataset(spans, previousDataset);
                                    spanWithPrevDataset.setExitCode("17");
                                    spanWithPrevDataset.setExitDate(getLastInSessionDate(date));

                                    addSpan(spans, currentDataset);

                                    previousDatasets.remove(i);
                                }
                                // Check if grade level was changed.
                                else if ((i = findDatasetUsingComparator(previousDatasets, currentDataset,
                                        m_comparatorWOGradeLvl)) >= 0 ||
                                        (i = findDatasetUsingComparator(previousDatasets, currentDataset,
                                                m_comparatorWOGradeLvlAndFTE)) >= 0) {
                                    DemoDataset firstPreviousDataset = previousDatasets.get(i);
                                    DemoExitSpan spanWithPrevDataset = findSpanByDataset(spans, firstPreviousDataset);

                                    Integer previousYog = null;
                                    Integer currentYog = null;

                                    for (Entry<Integer, String> entryYogGrade : m_gradeLevelMap.entrySet()) {
                                        if (entryYogGrade.getValue().equals(firstPreviousDataset.getGradeLvl())) {
                                            previousYog = entryYogGrade.getKey();
                                        } else if (entryYogGrade.getValue().equals(currentDataset.getGradeLvl())) {
                                            currentYog = entryYogGrade.getKey();
                                        }
                                    }

                                    if (currentYog.intValue() > previousYog.intValue()) {
                                        spanWithPrevDataset.setExitCode("12");
                                    } else {
                                        spanWithPrevDataset.setExitCode("05");
                                    }

                                    spanWithPrevDataset.setExitDate(getLastInSessionDate(date));

                                    addSpan(spans, currentDataset);

                                    previousDatasets.remove(i);
                                } else {
                                    addSpan(spans, currentDataset);
                                }
                            }
                        }
                    }
                    // Check if still there are previous DemoDatasets that not included in current
                    // DemoDatasets and
                    // try to finish them with end date of Secondary school, otherwise throw
                    // exception.
                    if (previousDatasets != null) {
                        for (DemoDataset previousDataset : previousDatasets) {
                            if (!currentDatasets.contains(previousDataset)) {
                                DemoExitSpan span = findSpanByDataset(spans, previousDataset);
                                if (m_dates.contains(getPreviousPlainDate(date))) {
                                    span.setExitDate(getPreviousPlainDate(date));
                                    span.setExitCode("05");
                                }
                            }
                        }
                    }

                    previousDatasets = new ArrayList<>(currentDatasets);
                }

                m_studentSpans.put(student, spans);
            }
        }

        /**
         * Initialize number of periods for sections.
         */
        protected void initializeNumOfPeriods() {
            if (m_sectionNumOfPeriodsMap == null) {
                m_sectionNumOfPeriodsMap = new HashMap<String, Integer>();
                X2Criteria studentScheduleCriteria = getScheduleHelper().getStudentScheduleCriteria();
                SubQuery scheduleSubQuery = new SubQuery(StudentSchedule.class, StudentSchedule.REL_SECTION +
                        ModelProperty.PATH_DELIMITER + X2BaseBean.COL_OID, studentScheduleCriteria);

                X2Criteria studentScheduleChangeCriteria = getScheduleHelper().getStudentScheduleChangeCriteria();
                SubQuery changeSubQuery = new SubQuery(StudentScheduleChange.class,
                        StudentScheduleChange.REL_MASTER_SCHEDULE + ModelProperty.PATH_DELIMITER + X2BaseBean.COL_OID,
                        studentScheduleChangeCriteria);

                X2Criteria mstCriteria = new X2Criteria();
                mstCriteria.addIn(X2BaseBean.COL_OID, scheduleSubQuery);
                X2Criteria orCriteria = new X2Criteria();
                orCriteria.addIn(X2BaseBean.COL_OID, changeSubQuery);
                mstCriteria.addOrCriteria(orCriteria);

                String[] columns = {X2BaseBean.COL_OID, MasterSchedule.REL_MASTER_TERMS + ModelProperty.PATH_DELIMITER +
                        MasterTerm.REL_MASTER_MATRICES + ModelProperty.PATH_DELIMITER +
                        MasterScheduleMatrix.REL_SCHEDULE_MATRIX + ModelProperty.PATH_DELIMITER +
                        ScheduleMatrix.REL_SCHEDULE_PERIOD + ModelProperty.PATH_DELIMITER +
                        X2BaseBean.COL_OID};

                ReportQueryByCriteria query =
                        new ReportQueryByCriteria(MasterSchedule.class, columns, mstCriteria, true);

                ReportQueryIterator iterator = m_data.getBroker().getReportQueryIteratorByQuery(query);

                try {
                    while (iterator.hasNext()) {
                        Object[] item = (Object[]) iterator.next();

                        String sectionOid = (String) item[0];

                        Integer curNumOfPeriods = m_sectionNumOfPeriodsMap.get(sectionOid);
                        if (curNumOfPeriods == null) {
                            curNumOfPeriods = Integer.valueOf(0);
                        }
                        int newNumOfPeriods = curNumOfPeriods.intValue() + 1;
                        m_sectionNumOfPeriodsMap.put(sectionOid, Integer.valueOf(newNumOfPeriods));
                    }
                } finally {
                    iterator.close();
                }
            }
        }

        /**
         * Initialize number of schedule days for sections.
         */
        protected void initializeNumOfScheduleDays() {
            if (m_sectionNumOfSheduleDays == null) {
                m_sectionNumOfSheduleDays = new HashMap<String, Integer>();

                X2Criteria ssCriteria = getScheduleHelper().getStudentScheduleCriteria();
                SubQuery ssSubQuery =
                        new SubQuery(StudentSchedule.class, StudentSchedule.REL_SECTION + ModelProperty.PATH_DELIMITER +
                                X2BaseBean.COL_OID, ssCriteria);

                X2Criteria sscCriteria = getScheduleHelper().getStudentScheduleChangeCriteria();
                SubQuery sscSubQuery =
                        new SubQuery(StudentScheduleChange.class, StudentScheduleChange.REL_MASTER_SCHEDULE +
                                ModelProperty.PATH_DELIMITER + X2BaseBean.COL_OID, sscCriteria);

                X2Criteria andCriteria = new X2Criteria();
                X2Criteria orCriteria = new X2Criteria();

                andCriteria.addIn(X2BaseBean.COL_OID, ssSubQuery);
                orCriteria.addIn(X2BaseBean.COL_OID, sscSubQuery);
                andCriteria.addOrCriteria(orCriteria);

                X2Criteria mstCriteria = new X2Criteria();
                mstCriteria.addAndCriteria(andCriteria);

                String[] columns = {X2BaseBean.COL_OID, MasterSchedule.REL_SCHEDULE + ModelProperty.PATH_DELIMITER +
                        Schedule.REL_SCHEDULE_DAYS + ModelProperty.PATH_DELIMITER + X2BaseBean.COL_OID};

                ReportQueryByCriteria query =
                        new ReportQueryByCriteria(MasterSchedule.class, columns, mstCriteria, true);

                ReportQueryIterator iterator = m_data.getBroker().getReportQueryIteratorByQuery(query);

                try {
                    while (iterator.hasNext()) {
                        Object[] item = (Object[]) iterator.next();

                        String sectionOid = (String) item[0];

                        Integer curNumOfScheduleDays = m_sectionNumOfSheduleDays.get(sectionOid);
                        if (curNumOfScheduleDays == null) {
                            curNumOfScheduleDays = Integer.valueOf(0);
                        }
                        int newNumOfScheduleDays = curNumOfScheduleDays.intValue() + 1;
                        m_sectionNumOfSheduleDays.put(sectionOid, Integer.valueOf(newNumOfScheduleDays));
                    }
                } finally {
                    iterator.close();
                }
            }
        }

        /**
         * Finish previous DemoExitSpans with withdrawal enrollments.
         *
         * @param student SisStudent
         * @param previousDatasets List<DemoDataset>
         * @param spans HashSet<DemoExitSpan>
         * @param date PlainDate
         */
        private void accessAppToFinishPrevSpans(SisStudent student,
                                                List<DemoDataset> previousDatasets,
                                                HashSet<DemoExitSpan> spans,
                                                PlainDate date) {
            StudentEnrollment enrollment =
                    getEnrollmentHelper().getEnrollmentForDate(student.getOid(), date, "EW");
            StudentEnrollment withdrawalEnrollment =
                    getEnrollmentHelper().getEnrollmentForDate(student.getOid(), date, "W");

            boolean previousIsEntry =
                    enrollment != null && enrollment.getEnrollmentType().equals(StudentEnrollment.ENTRY);

            if (previousIsEntry) {
                // check previous span for be sure that it contain secondary school and reason for
                // end previous span - fte changes
                // When secondary school end participant, but primary school still
                // enrolled and schedule is end
                if (previousDatasets.size() == 2) {
                    for (DemoDataset dataset : previousDatasets) {
                        DemoExitSpan span = findSpanByDataset(spans, dataset);
                        if (span != null) {
                            span.setExitCode("17");
                            span.setExitDate(getLastInSessionDate(date));
                        }
                    }
                }
            } else {
                for (DemoDataset dataset : previousDatasets) {
                    String exitCode = withdrawalEnrollment.getEnrollmentCode();
                    exitCode = m_data.lookupStateValue(StudentEnrollment.class,
                            StudentEnrollment.COL_ENROLLMENT_CODE, exitCode);
                    DemoExitSpan span = findSpanByDataset(spans, dataset);
                    if (span != null) {
                        span.setExitCode(exitCode);
                        span.setExitDate(withdrawalEnrollment.getEnrollmentDate());
                    }
                }
            }

        }

        /**
         * Add new DemoExitSpan.
         *
         * @param spans HashSet<DemoExitSpan>
         * @param currentDataset DemoDataset
         */
        private void addSpan(HashSet<DemoExitSpan> spans, DemoDataset currentDataset) {
            DemoExitSpan newSpan = new DemoExitSpan(currentDataset);
            spans.add(newSpan);
        }

        /**
         * Add new DemoExitSpans.
         *
         * @param spans HashSet<DemoExitSpan>
         * @param currentDatasets Collection<DemoDataset>
         */
        private void addSpans(HashSet<DemoExitSpan> spans, Collection<DemoDataset> currentDatasets) {
            for (DemoDataset currentDataset : currentDatasets) {
                addSpan(spans, currentDataset);
            }
        }

        /**
         * Find span by dataset.
         *
         * @param spans Collection<DemoExitSpan>
         * @param dataset DemoDataset
         * @return DemoExitSpan with passed DemoDataset.
         */
        private DemoExitSpan findSpanByDataset(Collection<DemoExitSpan> spans, DemoDataset dataset) {
            DemoExitSpan span = null;

            for (DemoExitSpan currentSpan : spans) {
                if (currentSpan.getDemoDataset().equals(dataset)) {
                    span = currentSpan;
                }
            }

            return span;
        }

        /**
         * Gets the previous plain date.
         *
         * @param date PlainDate
         * @return date before passed date.
         */
        private PlainDate getPreviousPlainDate(PlainDate date) {
            Calendar cal = Calendar.getInstance();
            cal.setTime(date);
            cal.add(Calendar.DATE, -1);
            return new PlainDate(cal.getTime());
        }
    }

    public static final String MODE_PRIMARY_SCHOOL = "primarySchool";
    public static final String MODE_SUMMER_SCHOOL = "summerSchool";

    protected String m_activeCode = null;


    /**
     * Comparator to compare DemoDatasets without FTE.
     */
    protected Comparator m_comparatorWOFTE = new Comparator<DemoDataset>() {
        @Override
        public int compare(DemoDataset o1, DemoDataset o2) {
            return new CompareToBuilder().append(o1.getServiceSchoolCode(), o2.getServiceSchoolCode())
                    .append(o1.getGradeLvl(), o2.getGradeLvl()).append(o1.getHomeSchoolCode(), o2.getHomeSchoolCode())
                    .toComparison();
        }
    };
    /**
     * Comparator to compare DemoDatasets without grade level.
     */
    protected Comparator m_comparatorWOGradeLvl = new Comparator<DemoDataset>() {
        @Override
        public int compare(DemoDataset o1, DemoDataset o2) {
            return new CompareToBuilder().append(o1.getServiceSchoolCode(), o2.getServiceSchoolCode())
                    .append(o1.getFte(), o2.getFte()).append(o1.getHomeSchoolCode(), o2.getHomeSchoolCode())
                    .toComparison();
        }
    };
    /**
     * Comparator to compare DemoDatasets without grade level.
     */
    protected Comparator m_comparatorWOGradeLvlAndFTE = new Comparator<DemoDataset>() {
        @Override
        public int compare(DemoDataset o1, DemoDataset o2) {
            return new CompareToBuilder().append(o1.getServiceSchoolCode(), o2.getServiceSchoolCode())
                    .append(o1.getHomeSchoolCode(), o2.getHomeSchoolCode()).toComparison();
        }
    };
    /**
     * Comparator to compare DemoDatasets without Serving school.
     */
    protected Comparator m_comparatorWOServSchool = new Comparator<DemoDataset>() {
        @Override
        public int compare(DemoDataset o1, DemoDataset o2) {
            return new CompareToBuilder().append(o1.getFte(), o2.getFte()).append(o1.getGradeLvl(), o2.getGradeLvl())
                    .append(o1.getHomeSchoolCode(), o2.getHomeSchoolCode()).toComparison();
        }
    };
    /**
     * Comparator to compare DemoDatasets without Serving school and FTE.
     */
    protected Comparator m_comparatorWOServSchoolAndFTE = new Comparator<DemoDataset>() {
        @Override
        public int compare(DemoDataset o1, DemoDataset o2) {
            return new CompareToBuilder().append(o1.getGradeLvl(), o2.getGradeLvl())
                    .append(o1.getHomeSchoolCode(), o2.getHomeSchoolCode()).toComparison();
        }
    };
    protected PlainDate m_currentSubmissionDate = null;
    protected StateReportData m_data = null;
    protected DateDataStorePrimary m_dateDataStorePrimary = null;
    protected DateDataStoreSummer m_dateDataStoreSummer = null;
    protected Collection<PlainDate> m_dates = null;
    protected Collection<DistrictCalendar> m_days = null;
    protected String m_fieldEnrSklHome;
    protected String m_fieldEnrSklService;
    protected String m_fieldEnrFteOverride;
    protected String m_excludeSklField;
    protected String m_fieldFteOverride;
    protected String m_fieldRcdtsForServingSchool;
    protected String m_fieldSchoolCode;
    protected String m_fieldSklNonCalcFte;
    protected HashMap<Integer, String> m_gradeLevelMap = new HashMap<Integer, String>();
    protected PlainDate m_lastSubmissionDate = null;
    protected int m_maxGradeLevel;
    protected TreeMap<Integer, List<String>> m_sortedGradeLevels;

    private StudentHistoryHelper m_enrollmentHelper = null;
    private String m_helperMode = null;
    private StudentHistoryHelper m_scheduleHelper = null;
    private SpansFactory m_spansFactory = null;
    private Map<String, String> m_schoolCodeSchoolOidMap = null;

    /**
     * Instantiates a new demo exit data helper.
     *
     * @param data StateReportData
     */
    public DemoExitDataHelper(StateReportData data) {
        m_data = data;
        m_days = m_data.getOrganization().getCurrentContext().getCalendars();
        m_activeCode = PreferenceManager.getPreferenceValue(m_data.getOrganization(),
                SystemPreferenceDefinition.STUDENT_ACTIVE_CODE);
        m_excludeSklField = m_data.translateAliasToJavaName(StudentDemographics.ALIAS_EXCLUDE_SKL, true);
        m_fieldSchoolCode = m_data.translateAliasToJavaName(StudentDemographics.ALIAS_SCHOOL_ID, true);
        m_fieldRcdtsForServingSchool =
                m_data.translateAliasToJavaName(StudentDemographics.ALIAS_RCDTS_FOR_SERVING_SCHOOL, true);
        m_fieldFteOverride = m_data.translateAliasToJavaName(StudentDemographics.ALIAS_OUTPLACED_FTE_OVERRIDE, true);
        m_fieldSklNonCalcFte = m_data.translateAliasToJavaName(StudentDemographics.ALIAS_DOE_NON_CALC_FTE, true);
        m_fieldEnrSklHome = m_data.translateAliasToJavaName(StudentDemographics.ALIAS_ENR_SKL_HOME, true);
        m_fieldEnrSklService = m_data.translateAliasToJavaName(StudentDemographics.ALIAS_ENR_SKL_SERVICE, true);
        m_fieldEnrFteOverride = m_data.translateAliasToJavaName(StudentDemographics.ALIAS_ENR_FTE_OVERRIDE, true);

        m_sortedGradeLevels = StudentManager.buildGradeLevelMap(m_data.getBroker());
        m_maxGradeLevel = StudentManager.getMaxGradeLevel(m_data.getBroker());

    }

    /**
     * Gets the datasets.
     *
     * @param student SisStudent
     * @param date PlainDate
     * @return DemoDataset for passed student on passed date.
     */
    public Collection<DemoDataset> getDatasets(SisStudent student, PlainDate date) {
        initializeFields();
        initializeSpansFactory();
        return m_spansFactory.getDatasets(student, date);
    }

    /**
     *
     * @param schoolOid
     * @return
     */
    public String getSchoolCodeBySchOid(String schoolOid) {
        if (m_schoolCodeSchoolOidMap == null) {
            loadSchoolOidCodeMapping();
        }
        return m_schoolCodeSchoolOidMap.get(schoolOid);
    }

    /**
     * Gets the helper mode.
     *
     * @return String
     */
    public String getHelperMode() {
        if (m_helperMode == null) {
            m_helperMode = MODE_PRIMARY_SCHOOL;
        }
        return m_helperMode;
    }

    /**
     * Gets the enrollment helper.
     *
     * @return StudentHistoryHelper in mode MODE_STUDENT_ACTIVE_ANY_TIME
     */
    public StudentHistoryHelper getEnrollmentHelper() {
        if (m_enrollmentHelper == null) {
            initDates();

            m_enrollmentHelper = new StudentHistoryHelper(m_data);
            m_enrollmentHelper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
            m_enrollmentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE,
                    m_data.getOrganization().getCurrentContext().getStartDate());
            m_enrollmentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_currentSubmissionDate);
            m_enrollmentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_SPAN_BREAK_ON_YOG, Boolean.TRUE);
        }

        return m_enrollmentHelper;
    }

    /**
     * Gets the schedule helper.
     *
     * @return StudentHistoryHelper in mode MODE_SCHEDULE_SPANS
     */
    public StudentHistoryHelper getScheduleHelper() {
        if (m_scheduleHelper == null) {
            initDates();

            m_scheduleHelper = new StudentHistoryHelper(m_data);
            m_scheduleHelper.setStudentSelectionMode(StudentHistoryHelper.MODE_SCHEDULE_SPANS);
            m_scheduleHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_SCHOOL, Boolean.FALSE);
            m_scheduleHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_INCLUDE_SECONDARY, Boolean.TRUE);
            m_scheduleHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE,
                    m_data.getOrganization().getCurrentContext().getStartDate());
            m_scheduleHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_currentSubmissionDate);
        }

        return m_scheduleHelper;
    }

    /**
     * Gets the spans.
     *
     * @param student SisStudent
     * @return DemoExitSpans for student.
     */
    public HashSet<DemoExitSpan> getSpans(SisStudent student) {
        initializeFields();
        initializeSpansFactory();
        return m_spansFactory.getDemoExitSpans(student);
    }

    /**
     * Set Current submission date.
     *
     * @param date void
     */
    public void setCurrentSubmissionDate(PlainDate date) {
        m_currentSubmissionDate = date;
    }

    /**
     * Sets the helper mode.
     *
     * @param helperMode void
     */
    public void setHelperMode(String helperMode) {
        m_helperMode = helperMode;
    }

    /**
     * Set Last submission date.
     *
     * @param date void
     */
    public void setLastSubmissionDate(PlainDate date) {
        m_lastSubmissionDate = date;
    }

    /**
     * Initialize DemoDatasets.
     */
    public void initializeDatasets() {
        initializeFields();
        initializeSpansFactory();
    }

    /**
     * Initialize DemoExitSpans.
     */
    public void initializeSpans() {
        initializeFields();
        initializeSpansFactory();
        if (m_spansFactory.m_studentSpans == null) {
            if (getHelperMode().equals(MODE_PRIMARY_SCHOOL)) {
                m_spansFactory.calcSpansInPrimaryMode();
            } else {
                m_spansFactory.calcSpansInSummerMode();
            }
        }
    }

    /**
     * Gets the last in session date.
     *
     * @param date PlainDate
     * @return last in session date in district calendar before passed date.
     */
    protected PlainDate getLastInSessionDate(PlainDate date) {
        Collection<DistrictCalendar> days = m_data.getOrganization().getCurrentContext().getCalendars();
        PlainDate lastInSessionDate = null;
        for (DistrictCalendar day : days) {
            if (day.getInSessionIndicator() && day.getDate().before(date)) {
                // Initialize by first day from collection if wasn't initialized to this point.
                if (lastInSessionDate == null) {
                    lastInSessionDate = day.getDate();
                }

                if (lastInSessionDate.before(day.getDate())) {
                    lastInSessionDate = day.getDate();
                }
            }
        }
        return lastInSessionDate;
    }

    /**
     * Initialize DateDataStores.
     */
    private void initDataStores() {
        if (getHelperMode().equals(MODE_PRIMARY_SCHOOL)) {
            if (m_dateDataStorePrimary == null) {
                m_dateDataStorePrimary = new DateDataStorePrimary();
            }
        } else if (getHelperMode().equals(MODE_SUMMER_SCHOOL)) {
            if (m_dateDataStoreSummer == null) {
                m_dateDataStoreSummer = new DateDataStoreSummer();
            }
        }
    }

    /**
     * Initialize dates between Last submission date and Current submission date.
     */
    private void initDates() {
        if (m_dates == null) {
            m_dates = new ArrayList<PlainDate>();

            if (m_currentSubmissionDate == null) {
                m_currentSubmissionDate = new PlainDate(new Date());
            }
            if (m_lastSubmissionDate == null) {
                m_lastSubmissionDate = getLastInSessionDate(m_currentSubmissionDate);
            }

            Calendar cal = Calendar.getInstance();
            cal.setTime(m_lastSubmissionDate);

            while (!cal.getTime().after(m_currentSubmissionDate)) {
                m_dates.add(new PlainDate(cal.getTime()));
                cal.add(Calendar.DATE, 1);
            }
        }
    }

    /**
     * Initialize used data.
     */
    private void initializeFields() {
        initDates();
        initDataStores();
    }

    /**
     * Initialize SpanFactory.
     */
    private void initializeSpansFactory() {
        if (m_spansFactory == null) {
            Collection<SisStudent> students = m_data.getBroker().getCollectionByQuery(m_data.getQuery());
            m_spansFactory = new SpansFactory();
            for (SisStudent student : students) {
                Collection<StudentScheduleSpan> schedSpans = getScheduleHelper().getStudentScheduleSpans(student);

                StudentDemoDatasets previousStudentDatasets = null;
                for (PlainDate date : m_dates) {
                    StudentEnrollment onOrpreviousEnr =
                            getEnrollmentHelper().getEnrollmentForDate(student.getOid(), date, "EW");

                    // if enrollment has enrollment date = current("for" cycle) date and is W - try
                    // add datasets
                    // if it enrollment has enrollment date before date and is W. It mean that
                    // student is not enrolled on date. Or
                    // has data issue if
                    // after W foolow Y or S
                    if (onOrpreviousEnr != null) {
                        boolean isOnTheDate = onOrpreviousEnr.getEnrollmentDate().equals(date);
                        boolean isWithdrawal = onOrpreviousEnr.getEnrollmentType().equals(StudentEnrollment.WITHDRAWAL);
                        if (!isWithdrawal || isOnTheDate) {
                            previousStudentDatasets =
                                    m_spansFactory.addStudentWithDatasets(student, date, schedSpans,
                                            previousStudentDatasets);
                        }
                    }
                }
            }
        }
    }

    /**
     *
     */
    private void loadSchoolOidCodeMapping() {
        m_schoolCodeSchoolOidMap = new HashMap<String, String>();
        Criteria criteria = new X2Criteria();

        criteria.addNotEqualTo(SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
        criteria.addNotEqualTo(SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);

        QueryIterator iterator = null;
        try {
            iterator = m_data.getBroker().getIteratorByQuery(new QueryByCriteria(SisSchool.class, criteria));
            while (iterator.hasNext()) {
                SisSchool school = (SisSchool) iterator.next();
                String schoolCode = (String) school.getFieldValueByBeanPath(m_fieldSchoolCode);
                String oid = school.getOid();
                m_schoolCodeSchoolOidMap.put(schoolCode, oid);
            }
        } finally {
            iterator.close();
        }

    }
}
