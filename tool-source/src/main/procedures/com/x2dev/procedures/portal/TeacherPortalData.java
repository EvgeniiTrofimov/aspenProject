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
package com.x2dev.procedures.portal;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.framework.persistence.adjusters.DistinctAdjuster;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryTable;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.web.WebUtils;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.business.GradesManager;
import com.x2dev.sis.web.presentation.GradeTypeFieldFormatter;
import com.x2dev.sis.web.presentation.TranscriptColumnTypeFieldFormatter;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.lang.reflect.Method;
import java.math.BigDecimal;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class TeacherPortalData.
 *
 * @author Follett Software Company
 * @copyright 2018
 */
public class TeacherPortalData extends StateReportData {

    // duplicated parameters in com.follett.fsc.aspenmobile.bridge.api.TeacherDashboardBridge
    public static final String PARAM_SCHEDULE_OID = "scheduleOid";
    public static final String PARAM_SECTIONS_LIST = "sectionsList";
    public static final String PARAM_STAFF_OID = "staffOid";

    /**
     * Class used to implement invocation of methods defined as parameters to field retriever.
     * The method must return an empty string for not value and may not return null.
     *
     * @author Follett Software Company
     */
    protected class ParameterRetriever implements FieldRetriever {
        protected Pattern m_methodPattern = Pattern.compile("(.*)\\((.*)\\)");

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object value = null;
            String patternString = (String) field.getParameter();
            Matcher matcher = m_methodPattern.matcher(patternString);
            if (matcher.find()) {
                Class[] args = {StateReportData.class, StateReportEntity.class, FieldDefinition.class, String.class};
                String methodName = matcher.group(1);
                String parameter = matcher.group(2);
                try {
                    Method m = getClass().getDeclaredMethod(methodName, args);
                    Object params[] = {data, entity, field, parameter};
                    value = m.invoke(this, params);
                } catch (Exception e) {
                    // Method invocation fails - return
                    value = "###";
                }
            }
            return value;
        }

        /**
         * Concat.
         *
         * @param bean X2BaseBean
         * @param parameter String
         * @return Object
         * @throws X2BaseException exception
         */
        protected Object concat(X2BaseBean bean, String parameter) throws X2BaseException {
            StringBuilder value = new StringBuilder();
            String[] parameters = parameter.split(",");
            for (String param : parameters) {
                param = param.trim();
                if (param.substring(0, 1).equals("\"")) {
                    value.append(param.substring(1, param.length() - 1));
                } else {
                    value.append((String) WebUtils.getProperty(bean, param));
                }
            }
            return value.toString();
        }
    }

    /**
     * The Class RetrieveAttendance.
     */
    protected class RetrieveAttendance extends ParameterRetriever {
        public static final String CALC_ID = "ATTENDANCE";

        private Map<String, Collection<StudentAttendance>> m_mapStdAtt;

        /**
         * Absent.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param parameter String
         * @return Object
         * @throws X2BaseException exception
         */
        public Object absent(@SuppressWarnings("unused") StateReportData data,
                             StateReportEntity entity,
                             @SuppressWarnings("unused") FieldDefinition field,
                             @SuppressWarnings("unused") String parameter)
                throws X2BaseException {
            initialize(data);

            BigDecimal total = BigDecimal.ZERO;

            Collection<StudentAttendance> atts = m_mapStdAtt.get(((StudentSchedule) entity.getBean()).getStudentOid());
            if (atts != null && !atts.isEmpty()) {
                for (StudentAttendance att : atts) {
                    if (att.getAbsentIndicator()) {
                        total = total.add(att.getPortionAbsent());
                    }
                }
            }
            return total;
        }

        /**
         * Tardy.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param parameter String
         * @return Object
         * @throws X2BaseException exception
         */
        public Object tardy(@SuppressWarnings("unused") StateReportData data,
                            StateReportEntity entity,
                            @SuppressWarnings("unused") FieldDefinition field,
                            @SuppressWarnings("unused") String parameter)
                throws X2BaseException {
            initialize(data);

            int total = 0;

            Collection<StudentAttendance> atts = m_mapStdAtt.get(((StudentSchedule) entity.getBean()).getStudentOid());
            if (atts != null && !atts.isEmpty()) {
                for (StudentAttendance att : atts) {
                    if (att.getTardyIndicator()) {
                        ++total;
                    }
                }
            }
            return Integer.valueOf(total);
        }

        /**
         * Initialize.
         *
         * @param data StateReportData
         */
        private void initialize(StateReportData data) {
            if (m_mapStdAtt == null) {
                X2Criteria criteria = new X2Criteria();
                criteria.addEqualToField(StudentAttendance.COL_SCHOOL_OID,
                        StudentAttendance.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_SCHOOL_OID);
                criteria.addGreaterOrEqualThan(StudentAttendance.COL_DATE, getCurrentContext().getStartDate());
                criteria.addLessOrEqualThan(StudentAttendance.COL_DATE, getCurrentContext().getEndDate());
                QueryByCriteria query = getStudentSelectionQuery(StudentAttendance.class, criteria,
                        StudentAttendance.COL_STUDENT_OID);
                m_mapStdAtt = getBroker().getGroupedCollectionByQuery(query, StudentAttendance.COL_STUDENT_OID, 1024);
            }
        }
    }

    /**
     * The Class RetrieveTranscript.
     */
    protected class RetrieveTranscript extends ParameterRetriever {
        public static final String CALC_ID = "TRANSCRIPT";

        private static final String GRADE_TERM = "GradeTerm";

        /**
         * The Class MyGradeTypeFieldFormatter.
         */
        class MyGradeTypeFieldFormatter extends GradeTypeFieldFormatter {

            /**
             * Gets the lookup map.
             *
             * @return Map
             */
            public Map<String, Integer> getLookupMap() {
                Map<String, Integer> map = new HashMap();
                Locale locale = LocalizationCache.getPrimarySystemLocale(getBroker().getPersistenceKey());
                Map<String, String> resourceMap = getResourceKeyMap();
                for (Map.Entry<String, String> entry : resourceMap.entrySet()) {
                    String resourceKey = entry.getValue();
                    Integer index = Integer.valueOf(Integer.parseInt(entry.getKey()));
                    String value =
                            LocalizationCache.getMessages(getPersistenceKey(), locale).getMessage(locale, resourceKey);
                    map.put(value, index);
                }
                return map;
            }
        }

        /**
         * The Class MyTranscriptColumnTypeFieldFormatter.
         */
        class MyTranscriptColumnTypeFieldFormatter extends TranscriptColumnTypeFieldFormatter {

            /**
             * Gets the lookup map.
             *
             * @return Map
             */
            public Map<String, Integer> getLookupMap() {
                Map<String, Integer> map = new HashMap();
                Locale locale = LocalizationCache.getPrimarySystemLocale(getBroker().getPersistenceKey());
                Map<String, String> resourceMap = getResourceKeyMap();
                for (Map.Entry<String, String> entry : resourceMap.entrySet()) {
                    String resourceKey = entry.getValue();
                    Integer index = Integer.valueOf(Integer.parseInt(entry.getKey()));
                    String value =
                            LocalizationCache.getMessages(getPersistenceKey(), locale).getMessage(locale, resourceKey);
                    map.put(value, index);
                }
                return map;
            }
        }

        private GradesManager m_gradesManager;
        private Collection m_gradeTerms = null;
        private Map<String, Integer> m_mapColumnType;
        private Map<String, Map<String, TranscriptColumnDefinition>> m_mapGtc = new HashMap();
        private Map<String, Integer> m_mapReportType;
        private Map<String, Map<String, Collection<Transcript>>> m_mapTrn;

        /**
         * Instantiates a new retrieve transcript.
         *
         * @param transcriptFields List<FieldDefinition>
         */
        public RetrieveTranscript(List<FieldDefinition> transcriptFields) {
            super();
            for (FieldDefinition transcriptField : transcriptFields) {
                if (transcriptField.getParameter() instanceof String) {
                    Matcher matcher = m_methodPattern.matcher((String) transcriptField.getParameter());
                    if (matcher.find()) {
                        if ("columnType".equals(matcher.group(1))) {
                            String[] args = matcher.group(2).split("\\s*,\\s*");
                            if (args.length == 2) {
                                Integer reportType = getReportType(args[0]);
                                Integer colType = getColumnType(args[1]);

                                List<FieldDefinition> definitions = getFieldDefinitions();
                                int insertPos = definitions.indexOf(transcriptField);
                                if (insertPos >= 0 && definitions.remove(transcriptField)) {
                                    for (TranscriptColumnDefinition gtc : getTranscriptColumnDefinitions(reportType,
                                            colType)) {
                                        FieldDefinition termDefinition =
                                                new FieldDefinition(gtc.getGradeColumnHeader(),
                                                        transcriptField.getBeanPath(),
                                                        transcriptField.getDefaultValue(),
                                                        transcriptField.getMappedLookup(),
                                                        transcriptField.getMinLength(),
                                                        transcriptField.getMaxLength(),
                                                        transcriptField.getPattern(),
                                                        transcriptField.getFormatter(),
                                                        this,
                                                        transcriptField.getValidator(),
                                                        "column(" + gtc.getGradeColumnHeader() + ")");
                                        definitions.add(insertPos++, termDefinition);
                                    }
                                    setFieldDefinitions(definitions);
                                }
                            }
                        }
                    }
                }
            }
        }

        /**
         * Column.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param parameter String
         * @return Object
         * @throws X2BaseException exception
         */
        public Object column(@SuppressWarnings("unused") StateReportData data,
                             @SuppressWarnings("unused") StateReportEntity entity,
                             @SuppressWarnings("unused") FieldDefinition field,
                             @SuppressWarnings("unused") String parameter)
                throws X2BaseException {
            Number grade = BigDecimal.ZERO;

            StudentSchedule ssc = (StudentSchedule) entity.getBean();
            Transcript trn = getTranscript(ssc);
            if (trn != null) {
                TranscriptColumnDefinition gtc = getTranscriptColumnDefinition(trn, parameter);
                if (gtc != null) {
                    Object value =
                            trn.getFieldValueByBeanPath(gtc.getDataFieldConfig().getDataField().getJavaName());
                    if (value != null) {
                        if (Number.class.isAssignableFrom(value.getClass())) {
                            grade = (Number) value;
                        } else if (value instanceof String && ((String) value).length() > 0) {
                            String finalGrade = (String) value;
                            GradeScale scale = null;
                            if (StringUtils.isNumeric(finalGrade)) {
                                // Try the final grade as a number.
                                try {
                                    grade = new BigDecimal(finalGrade);
                                } catch (NumberFormatException nfe) {
                                    // nothing. The grade is not numeric.
                                }
                            } else if (!StringUtils.isEmpty(trn.getTranscriptDefinitionOid())
                                    && (scale = gtc.getGradeScale()) != null) {
                                grade = getGradesManager().getNumericValue(finalGrade, scale, trn.getSchool(),
                                        trn.getSchoolCourseOid());
                            } else {
                                grade = null;
                            }
                        }
                    }
                } else {
                    throw new IllegalStateException(
                            "Invalid arguments in gradeTerm method of TeacherPortalData.RetrieveTranscript");
                }
            }
            return grade;
        }

        /**
         * Gets the column type.
         *
         * @param columnType String
         * @return Integer
         */
        private Integer getColumnType(String columnType) {
            if (m_mapColumnType == null) {
                MyTranscriptColumnTypeFieldFormatter formatter = new MyTranscriptColumnTypeFieldFormatter();

                m_mapColumnType = formatter.getLookupMap();
            }
            return m_mapColumnType.get(columnType);
        }

        /**
         * Gets the grades manager.
         *
         * @return Grades manager
         */
        private GradesManager getGradesManager() {
            if (m_gradesManager == null) {
                m_gradesManager = new GradesManager(getBroker());
            }
            return m_gradesManager;
        }

        /**
         * Gets the grade terms.
         *
         * @return Collection
         */
        private Collection<GradeTerm> getGradeTerms() {
            if (m_gradeTerms == null) {
                X2Criteria mstCriteria = new X2Criteria();
                mstCriteria.addEqualTo(MasterSchedule.REL_TEACHER_SECTIONS + PATH_DELIMITER +
                        ScheduleTeacher.COL_STAFF_OID, getParameter(PARAM_STAFF_OID));
                mstCriteria.addEqualTo(MasterSchedule.COL_SCHEDULE_OID, getParameter(PARAM_SCHEDULE_OID));
                SubQuery subQuery =
                        new SubQuery(MasterSchedule.class, MasterSchedule.COL_SCHOOL_COURSE_OID, mstCriteria);

                X2Criteria criteria = new X2Criteria();
                criteria.addIn(GradeTerm.REL_TRANSCRIPT_COLUMN_DEFINITIONS + PATH_DELIMITER +
                        TranscriptColumnDefinition.REL_TRANSCRIPT_DEFINITION + PATH_DELIMITER +
                        TranscriptDefinition.REL_SCHOOL_COURSES + PATH_DELIMITER +
                        X2BaseBean.COL_OID, subQuery);

                BeanQuery query = new BeanQuery(GradeTerm.class, criteria);
                query.setDistinct(true);
                DataDictionaryTable table =
                        getDataDictionary().findDataDictionaryTableByClass(GradeTerm.class.getName());
                DistinctAdjuster adjuster =
                        new DistinctAdjuster(table.getPrimaryKeyColumn(), getBroker().getPersistenceKey());
                query.addQueryAdjuster(adjuster);
                query.addOrderByAscending(GradeTerm.COL_GRADE_TERM_ID);

                m_gradeTerms = getBroker().getCollectionByQuery(query);
            }
            return m_gradeTerms;
        }

        /**
         * Gets the report type.
         *
         * @param reportType String
         * @return Integer
         */
        private Integer getReportType(String reportType) {
            if (m_mapReportType == null) {
                MyGradeTypeFieldFormatter formatter = new MyGradeTypeFieldFormatter();

                m_mapReportType = formatter.getLookupMap();
            }
            return m_mapReportType.get(reportType);
        }

        /**
         * Gets the transcript.
         *
         * @param ssc StudentSchedule
         * @return Transcript
         */
        private Transcript getTranscript(StudentSchedule ssc) {
            if (m_mapTrn == null) {
                X2Criteria criteria = new X2Criteria();
                criteria.addIn(Transcript.COL_MASTER_SCHEDULE_OID,
                        (Collection<String>) getParameter(PARAM_SECTIONS_LIST));
                BeanQuery query = new BeanQuery(Transcript.class, criteria);
                m_mapTrn = getBroker().getGroupedCollectionByQuery(query,
                        new String[] {Transcript.COL_MASTER_SCHEDULE_OID, Transcript.COL_STUDENT_OID},
                        new int[] {16, 64});
            }

            Transcript trn = null;
            Map<String, Collection<Transcript>> students = m_mapTrn.get(ssc.getSectionOid());
            if (students != null) {
                Collection<Transcript> transcripts = students.get(ssc.getStudentOid());
                if (transcripts != null && !transcripts.isEmpty()) {
                    trn = transcripts.iterator().next();
                }
            }
            return trn;
        }

        /**
         * Gets the transcript column definition.
         *
         * @param trn Transcript
         * @param colName String
         * @return Transcript column definition
         */
        private TranscriptColumnDefinition getTranscriptColumnDefinition(Transcript trn, String colName) {
            TranscriptColumnDefinition gtd = null;
            if (!StringUtils.isEmpty(trn.getTranscriptDefinitionOid())) {
                Map<String, TranscriptColumnDefinition> gtdColumns = m_mapGtc.get(trn.getTranscriptDefinitionOid());
                if (gtdColumns == null) {
                    X2Criteria criteria = new X2Criteria();
                    criteria.addEqualTo(TranscriptColumnDefinition.COL_TRANSCRIPT_DEFINITION_OID,
                            trn.getTranscriptDefinitionOid());
                    BeanQuery query = new BeanQuery(TranscriptColumnDefinition.class, criteria);

                    gtdColumns =
                            getBroker().getMapByQuery(query, TranscriptColumnDefinition.COL_GRADE_COLUMN_HEADER, 64);
                    m_mapGtc.put(trn.getTranscriptDefinitionOid(), gtdColumns);
                }
                if (gtdColumns != null) {
                    gtd = gtdColumns.get(colName);
                }
            }
            return gtd;
        }

        /**
         * Gets the transcript column definitions.
         *
         * @param reportType Integer
         * @param colType Integer
         * @return List
         */
        private List<TranscriptColumnDefinition> getTranscriptColumnDefinitions(Integer reportType, Integer colType) {
            X2Criteria cskCriteria = new X2Criteria();
            cskCriteria.addIn(SchoolCourse.REL_MASTER_SCHEDULES + PATH_DELIMITER +
                    X2BaseBean.COL_OID, (Collection<String>) getParameter(PARAM_SECTIONS_LIST));
            SubQuery cskSubQuery =
                    new SubQuery(SchoolCourse.class, SchoolCourse.COL_TRANSCRIPT_DEFINITION_OID, cskCriteria);

            X2Criteria criteria = new X2Criteria();
            criteria.addIn(TranscriptColumnDefinition.COL_TRANSCRIPT_DEFINITION_OID, cskSubQuery);
            criteria.addEqualTo(TranscriptColumnDefinition.COL_REPORT_TYPE, reportType);
            criteria.addEqualTo(TranscriptColumnDefinition.COL_COLUMN_TYPE_CODE, colType);
            BeanQuery query = new BeanQuery(TranscriptColumnDefinition.class, criteria);
            query.setDistinct(true);
            DataDictionaryTable table =
                    getDataDictionary().findDataDictionaryTableByClass(TranscriptColumnDefinition.class.getName());
            DistinctAdjuster adjuster =
                    new DistinctAdjuster(table.getPrimaryKeyColumn(), getBroker().getPersistenceKey());
            query.addQueryAdjuster(adjuster);
            query.addOrderByAscending(TranscriptColumnDefinition.COL_GRADE_COLUMN_HEADER);

            return (List<TranscriptColumnDefinition>) getBroker().getCollectionByQuery(query);
        }
    }

    /**
     * Gets the student selection query.
     *
     * @param queryClass Class
     * @param criteria X2Criteria
     * @param column String
     * @return Bean query
     */
    protected BeanQuery getStudentSelectionQuery(Class queryClass, X2Criteria criteria, String column) {
        BeanQuery query;
        X2Criteria andCriteria = new X2Criteria();
        andCriteria.addIn(column,
                new SubQuery(StudentSchedule.class, StudentSchedule.COL_STUDENT_OID, getQuery().getCriteria()));
        criteria.addAndCriteria(andCriteria);
        query = new BeanQuery(queryClass, criteria);
        return query;
    }

    /**
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();

        if (getSetupErrors().size() == 0) {
            // Build a map of calculations/retrievers
            Map<String, List<FieldDefinition>> calcsUsed = getCalcsUsed();
            HashMap calcs = new HashMap<String, FieldRetriever>();
            if (calcsUsed.containsKey(RetrieveAttendance.CALC_ID)) {
                calcs.put(RetrieveAttendance.CALC_ID, new RetrieveAttendance());
            }
            if (calcsUsed.containsKey(RetrieveTranscript.CALC_ID)) {
                calcs.put(RetrieveTranscript.CALC_ID,
                        new RetrieveTranscript(calcsUsed.get(RetrieveTranscript.CALC_ID)));
            }
            super.addCalcs(calcs);
        }
    }

    /**
     * Gets the calcs used.
     *
     * @return Map
     */
    private Map<String, List<FieldDefinition>> getCalcsUsed() {
        Map<String, List<FieldDefinition>> map = new HashMap();
        List<FieldDefinition> list = getFieldDefinitions();
        if (list != null) {
            for (FieldDefinition item : list) {
                if (!StringUtils.isEmpty(item.getCalcId())) {
                    List<FieldDefinition> itemList = map.get(item.getCalcId());
                    if (itemList == null) {
                        itemList = new LinkedList();
                        map.put(item.getCalcId(), itemList);
                    }
                    itemList.add(item);
                }
            }
        }
        return map;
    }

}
