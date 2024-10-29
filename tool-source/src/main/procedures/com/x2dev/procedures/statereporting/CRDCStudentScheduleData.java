/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2016 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.procedures.statereporting;

import com.follett.fsc.core.framework.persistence.ColumnQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.web.WebUtils;
import com.x2dev.sis.model.beans.AssessmentDefinition;
import com.x2dev.sis.model.beans.Course;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAssessment;
import com.x2dev.sis.model.beans.Transcript;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.sis.tools.stateexports.StudentScheduleSpan;
import com.x2dev.utils.KeyValueTrio;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.SystemStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Dataset for Student Schedule data to be used in final CRDC export.
 *
 * @author X2 Development Corporation
 */
public class CRDCStudentScheduleData extends CRDCReportData {
    /**
     * Implementation of StateReportEntity to be used for Student Schedule CRDC Data export.
     * This must be a public static inner class with a public no argument
     * constructor so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class StudentScheduleCRDCEntity extends StateReportEntity {

        /**
         * Instantiates a new student schedule CRDC entity.
         */
        public StudentScheduleCRDCEntity() {
            // Public no argument constructor for dynamic instantiation.
        }

        private CRDCStudentScheduleData m_sscData;
        private SisStudent m_student;
        private ArrayList<MasterSchedule> m_mstCollection = new ArrayList<MasterSchedule>();

        /**
         * Gets the current MST.
         *
         * @return Master schedule
         */
        public MasterSchedule getCurrentMST() {
            return m_mstCollection.get(getCurrentRow());
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            String name = "[SASID: " + m_student.getStateId() +
                    ", LASID: " + m_student.getLocalId() + "] " +
                    getCurrentMST().getCourseView() + " - " + m_student.getNameView();

            return name;
        }

        /**
         * Initialize the entity for the student bean provided.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc
         *      .core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            m_sscData = (CRDCStudentScheduleData) data;

            m_student = (SisStudent) bean;

            Collection<StudentScheduleSpan> scheduleSpans =
                    m_sscData.m_studentScheduleHelper.getStudentScheduleSpans(m_student);

            if (!scheduleSpans.isEmpty()) {
                for (StudentScheduleSpan span : scheduleSpans) {
                    MasterSchedule mst = span.getSection();
                    Course crs = mst.getSchoolCourse().getCourse();
                    boolean includeMst = true;
                    if (m_sscData.m_includeCrsOnlyWithCategory.booleanValue()) {
                        includeMst =
                                !StringUtils.isEmpty((String) crs.getFieldValueByBeanPath(m_sscData.m_fieldCrsCategory))
                                        || !StringUtils.isEmpty(
                                                (String) crs.getFieldValueByBeanPath(m_sscData.m_fieldCrsCategoryAP));
                    }
                    if (includeMst && blockDatesOverlapWith(span) && !m_mstCollection.contains(mst)) {
                        m_mstCollection.add(mst);
                    }
                }
            }

            setRowCount(m_mstCollection.size());
        }

        /**
         * Override toString to return identifying information.
         *
         * @return String
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            return getEntityName();
        }

        /**
         * Block dates overlap with.
         *
         * @param span StudentScheduleSpan
         * @return true, if successful
         */
        private boolean blockDatesOverlapWith(StudentScheduleSpan span) {
            boolean onBlockDates = false;
            if (span.getEntryDate() != null && span.getExitDate() != null &&
                    ((!span.getEntryDate().after(m_sscData.m_reportDateBlock1)
                            && !span.getExitDate().before(m_sscData.m_reportDateBlock1)) ||
                            (m_sscData.m_reportDateBlock2 != null
                                    && !span.getEntryDate().after(m_sscData.m_reportDateBlock2) &&
                                    !span.getExitDate().before(m_sscData.m_reportDateBlock2)))) {
                onBlockDates = true;
            }

            return onBlockDates;
        }
    }

    /**
     * IL Retriever to determine if a student Took AP Test or Qualifying AP Score.
     *
     * @author X2 Development Corporation
     */
    public class ILRetrieverAP implements FieldRetriever {
        public static final String ALIAS_ASM_RAW_SCORE = "all-asm-CRDCAPRawScore";
        public static final String ALIAS_CRS_AP_CRS_CATEGORY = "all-crs-CRDCAPCourseCategory";
        public static final String ALIAS_CRS_AP_TEST_ID = "all-crs-CRDCAPTestID";
        public static final String CALC_PARAM_SCORE = "Score";
        public static final String CALC_PARAM_TEST = "Test";

        String m_fieldCrsAPTestId;
        Map<String, Map<String, StudentAssessment>> m_asmByAsdAndStudent = new HashMap();
        Map<String, Map<String, String>> m_schAPTestIds = new HashMap();
        Map<String, KeyValueTrio<AssessmentDefinition, String, SystemStringConverter>> m_mapCodeToAsd = new HashMap();

        /**
         * Instantiates a new IL retriever AP.
         */
        public ILRetrieverAP() {
            super();

            m_fieldCrsAPTestId = translateAliasToJavaName(ALIAS_CRS_AP_TEST_ID, true);

            QueryByCriteria asdQuery = new QueryByCriteria(AssessmentDefinition.class);
            Map<String, AssessmentDefinition> asdById =
                    getBroker().getMapByQuery(asdQuery, AssessmentDefinition.COL_ID, 128);

            DataDictionaryField crsApTestId = getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_CRS_AP_TEST_ID);
            DataDictionaryField aliasField = getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_CRDC_REF_CODE);
            Set<String> asdOids = new HashSet();
            asdOids.add("---- DUMMY ----");

            if (aliasField != null && crsApTestId != null && !StringUtils.isEmpty(crsApTestId.getReferenceTableOid())) {
                for (Entry<String, ReferenceCode> entry : getReferenceCodes(crsApTestId.getReferenceTableOid())
                        .entrySet()) {
                    String crdcCode = (String) entry.getValue().getFieldValueByBeanPath(aliasField.getJavaName());
                    if (!StringUtils.isEmpty(crdcCode) && asdById.containsKey(crdcCode)) {
                        AssessmentDefinition asd = asdById.get(crdcCode);
                        DataDictionary dictionary =
                                DataDictionary.getDistrictDictionary(asd, getBroker().getPersistenceKey());
                        DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(ALIAS_ASM_RAW_SCORE);
                        if (field != null) {
                            SystemStringConverter converter = null;
                            if (field.isString()) {
                                Converter baseConverter = ConverterFactory.getConverterForClass(
                                        field.getEffectiveJavaType(),
                                        LocalizationCache.getPrimarySystemLocale(getBroker().getPersistenceKey()),
                                        field.isString());
                                if (baseConverter instanceof SystemStringConverter) {
                                    converter = ((SystemStringConverter) baseConverter);
                                }
                            }
                            m_mapCodeToAsd.put(entry.getKey(), new KeyValueTrio(asd, field.getJavaName(), converter));
                            asdOids.add(asd.getOid());
                        }
                    }
                }
            }

            X2Criteria asmCriteria = new X2Criteria();
            asmCriteria.addGreaterOrEqualThan(StudentAssessment.COL_DATE, getCurrentContext().getStartDate());
            asmCriteria.addLessOrEqualThan(StudentAssessment.COL_DATE, getCurrentContext().getEndDate());
            asmCriteria.addIn(StudentAssessment.COL_ASSESSMENT_DEFINITION_OID, asdOids);

            QueryByCriteria asmQuery = new QueryByCriteria(StudentAssessment.class, asmCriteria);
            m_asmByAsdAndStudent = getBroker().getNestedMapByQuery(asmQuery,
                    StudentAssessment.COL_ASSESSMENT_DEFINITION_OID, StudentAssessment.COL_STUDENT_OID,
                    64, 1024);
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object value = Boolean.FALSE;
            SisStudent student = (SisStudent) entity.getBean();
            String apTestId = getAPTestId(((StudentScheduleCRDCEntity) entity).getCurrentMST());
            if (!StringUtils.isEmpty(apTestId)) {
                KeyValueTrio<AssessmentDefinition, String, SystemStringConverter> trio = m_mapCodeToAsd.get(apTestId);
                if (trio != null) {
                    AssessmentDefinition asd = trio.getKey();
                    String beanPath = trio.getValue1();
                    SystemStringConverter converter = trio.getValue2();
                    if (m_asmByAsdAndStudent.containsKey(asd.getOid())) {
                        StudentAssessment asm = m_asmByAsdAndStudent.get(asd.getOid()).get(student.getOid());
                        if (asm != null) {
                            value = WebUtils.getProperty(asm, beanPath);
                            if (value instanceof String && converter != null) {
                                value = converter.parseSystemString((String) value);
                            }
                        }

                    }

                }
            }

            if (CALC_PARAM_TEST.equals(field.getParameter())) {
                if (value != Boolean.FALSE) {
                    value = Boolean.TRUE;
                }
            } else if (CALC_PARAM_SCORE.equals(field.getParameter())) {
                if (value != Boolean.FALSE && value != null && Number.class.isAssignableFrom(value.getClass())) {
                    double score = ((Number) value).doubleValue();
                    value = score >= 3.0 ? value = Boolean.TRUE : Boolean.FALSE;
                }
            }

            return value != null ? value : Boolean.FALSE;
        }

        /**
         * Gets the AP test id.
         *
         * @param section MasterSchedule
         * @return String
         */
        private String getAPTestId(MasterSchedule section) {
            String id = null;
            if (section.getScheduleOid() != null) {
                Map<String, String> mstAPTestIds = m_schAPTestIds.get(section.getScheduleOid());
                if (mstAPTestIds == null) {
                    mstAPTestIds = new HashMap();
                    m_schAPTestIds.put(section.getScheduleOid(), mstAPTestIds);

                    X2Criteria mstCriteria = new X2Criteria();
                    mstCriteria.addEqualTo(MasterSchedule.COL_SCHEDULE_OID, section.getScheduleOid());
                    mstCriteria.addIn(MasterSchedule.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER +
                            SchoolCourse.REL_COURSE + ModelProperty.PATH_DELIMITER + m_fieldCrsAPTestId,
                            m_mapCodeToAsd.keySet());

                    String[] columns = new String[] {X2BaseBean.COL_OID,
                            MasterSchedule.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER +
                                    SchoolCourse.REL_COURSE + ModelProperty.PATH_DELIMITER + m_fieldCrsAPTestId};

                    ColumnQuery query = new ColumnQuery(MasterSchedule.class, columns, mstCriteria);
                    ReportQueryIterator queryItr = getBroker().getReportQueryIteratorByQuery(query);
                    try {
                        while (queryItr.hasNext()) {
                            Object[] row = (Object[]) queryItr.next();
                            String mstOid = (String) row[0];
                            String apTestCode = (String) row[1];
                            mstAPTestIds.put(mstOid, apTestCode);
                        }
                    } finally {
                        if (queryItr != null) {
                            queryItr.close();
                        }
                    }
                }
                id = mstAPTestIds.get(section.getOid());
            }
            return id;
        }
    }

    /**
     * The Class RetrieverMstInfo.
     */
    public class RetrieverMstInfo implements FieldRetriever {
        public static final String CALC_CRS_NUM = "CRS_NUM";
        public static final String CALC_MST_OID = "MST_OID";
        public static final String CALC_SKL_NAME = "SKL_NAME";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            StudentScheduleCRDCEntity sscEntity = (StudentScheduleCRDCEntity) entity;
            String value = null;

            if (CALC_MST_OID.equals(field.getParameter())) {
                value = sscEntity.getCurrentMST().getOid();
            } else if (CALC_CRS_NUM.equals(field.getParameter())) {
                value = sscEntity.getCurrentMST().getCourseView();
            } else if (CALC_SKL_NAME.equals(field.getParameter())) {
                value = sscEntity.getCurrentMST().getSchedule().getSchool().getName();
            }

            return value;
        }

    }

    /**
     * Standard Retriever to determine if a student Took AP Test or Qualifying AP Score.
     *
     * @author X2 Development Corporation
     */
    public class RetrieverAP implements FieldRetriever {
        public static final String ALIAS_TRN_AP_TEST_RESULT = "all-trn-CRDCAPTestResult";
        public static final String CALC_PARAM_SCORE = "Score";
        public static final String CALC_PARAM_TEST = "Test";
        public static final String TRN_CRDC_CODE_F = "F";
        public static final String TRN_CRDC_CODE_P = "P";
        public static final String TRN_CRDC_CODE_NT = "NT";

        Map<String, String> m_crdcByRefCodeMap = new HashMap<String, String>();
        Map<String, Map<String, String>> m_testResultCodesByStdOid = new HashMap<String, Map<String, String>>();

        /**
         * Instantiates a new retriever AP.
         */
        public RetrieverAP() {
            super();

            String javaNameAPTestResult = translateAliasToJavaName(ALIAS_TRN_AP_TEST_RESULT, true);
            String javaNameRefCodeCrdc = translateAliasToJavaName(ALIAS_CRDC_REF_CODE, true);

            DataDictionaryField trnApTest =
                    getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_TRN_AP_TEST_RESULT);

            if (trnApTest != null && !StringUtils.isEmpty(trnApTest.getReferenceTableOid())) {
                X2Criteria criteria = new X2Criteria();
                criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, trnApTest.getReferenceTableOid());
                criteria.addIn(javaNameRefCodeCrdc, Arrays.asList(TRN_CRDC_CODE_F, TRN_CRDC_CODE_P, TRN_CRDC_CODE_NT));

                QueryByCriteria refCodesQuery = new QueryByCriteria(ReferenceCode.class, criteria);
                Collection<ReferenceCode> refCodes =
                        CRDCStudentScheduleData.this.getBroker().getCollectionByQuery(refCodesQuery);
                if (refCodes != null && !refCodes.isEmpty()) {
                    for (ReferenceCode refCode : refCodes) {
                        m_crdcByRefCodeMap.put(refCode.getCode(),
                                (String) refCode.getFieldValueByBeanPath(javaNameRefCodeCrdc));
                    }
                }

                X2Criteria trnCriteria = m_studentScheduleHelper.getStudentTranscriptCriteria();
                SubQuery studentSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID,
                        m_studentScheduleHelper.getStudentCriteria());
                trnCriteria.addIn(Transcript.COL_STUDENT_OID, studentSubQuery);
                trnCriteria.addIn(javaNameAPTestResult, m_crdcByRefCodeMap.keySet());

                String[] columns = new String[] {Transcript.COL_STUDENT_OID,
                        javaNameAPTestResult,
                        Transcript.REL_MASTER_SCHEDULE + ModelProperty.PATH_DELIMITER +
                                X2BaseBean.COL_OID};

                ReportQueryByCriteria trnQuery = new ReportQueryByCriteria(Transcript.class, columns, trnCriteria);

                ReportQueryIterator iterator =
                        CRDCStudentScheduleData.this.getBroker().getReportQueryIteratorByQuery(trnQuery);
                try {
                    while (iterator.hasNext()) {
                        Object[] record = (Object[]) iterator.next();
                        String studentOid = (String) record[0];
                        String refCode = (String) record[1];
                        String mstOid = (String) record[2];
                        String crdcCode = m_crdcByRefCodeMap.get(refCode);

                        Map<String, String> crdcCodes = m_testResultCodesByStdOid.get(studentOid);
                        if (crdcCodes == null) {
                            crdcCodes = new HashMap<String, String>();
                            m_testResultCodesByStdOid.put(studentOid, crdcCodes);
                        }

                        crdcCodes.put(mstOid, crdcCode);
                    }
                } finally {
                    iterator.close();
                }
            }
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            boolean value = false;

            SisStudent student = (SisStudent) entity.getBean();
            StudentScheduleCRDCEntity crdcEntity = (StudentScheduleCRDCEntity) entity;
            Map<String, String> codes = m_testResultCodesByStdOid.get(student.getOid());

            String code = null;

            if (codes != null) {
                code = codes.get(crdcEntity.getCurrentMST().getOid());
            }
            if (code != null) {
                if (CALC_PARAM_TEST.equals(field.getParameter())) {
                    if ((TRN_CRDC_CODE_F.equals(code) || TRN_CRDC_CODE_P.equals(code))) {
                        value = true;
                    }
                } else if (CALC_PARAM_SCORE.equals(field.getParameter())) {
                    if (TRN_CRDC_CODE_P.equals(code)) {
                        value = true;
                    }
                }
            }

            return Boolean.valueOf(value);
        }
    }

    /**
     * The Class RetrieverCRDCCode.
     */
    public class RetrieverCrsInfo implements FieldRetriever {
        public static final String CALC_PARAM_CATEGORY = "CRS_CATEGORY";
        public static final String CALC_PARAM_CATEGORY_AP = "CRS_CATEGORY_AP";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            CRDCReportData crdcData = (CRDCReportData) data;
            StudentScheduleCRDCEntity sscEntity = (StudentScheduleCRDCEntity) entity;
            String value = null;
            Course crs = sscEntity.getCurrentMST().getSchoolCourse().getCourse();
            if (CALC_PARAM_CATEGORY.equals(field.getParameter())) {
                String code = (String) crs.getFieldValueByBeanPath(m_fieldCrsCategory);
                if (!StringUtils.isEmpty(code)) {
                    value = crdcData.lookupCRDCCodeByBeanPath(Course.class, m_fieldCrsCategory, code);
                }
            } else if (CALC_PARAM_CATEGORY_AP.equals(field.getParameter())) {
                String code = (String) crs.getFieldValueByBeanPath(m_fieldCrsCategoryAP);
                if (!StringUtils.isEmpty(code)) {
                    value = crdcData.lookupCRDCCodeByBeanPath(Course.class, m_fieldCrsCategoryAP, code);
                }
            }
            return value;
        }
    }

    /**
     * RI Retriever to determine if a student Took AP Test or Qualifying AP Score.
     *
     * @author X2 Development Corporation
     */
    public class RIRetrieverAP implements FieldRetriever {
        public static final String ALIAS_TRN_AP_TEST_RESULT = "all-trn-CRDCAPTestResult";
        public static final String CALC_PARAM_SCORE = "Score";
        public static final String CALC_PARAM_TEST = "Test";
        public static final String TRN_CODE_F = "F";
        public static final String TRN_CODE_P = "P";

        Map<String, Map<String, Collection<Transcript>>> m_trnByStdOid;

        /**
         * Instantiates a new RI retriever AP.
         */
        public RIRetrieverAP() {
            super();

            DataDictionaryField trnApTest =
                    getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_TRN_AP_TEST_RESULT);

            if (trnApTest != null && !StringUtils.isEmpty(trnApTest.getReferenceTableOid())) {

                X2Criteria criteria = new X2Criteria();
                criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, trnApTest.getReferenceTableOid());
                criteria.addIn(ReferenceCode.COL_CODE, Arrays.asList(TRN_CODE_F, TRN_CODE_P));

                QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, criteria);

                Collection<ReferenceCode> refCodes =
                        CRDCStudentScheduleData.this.getBroker().getCollectionByQuery(query);
                Collection<String> codes = null;
                if (refCodes != null && !refCodes.isEmpty()) {
                    codes = new ArrayList<String>();

                    for (ReferenceCode refCode : refCodes) {
                        codes.add(refCode.getCode());
                    }
                }

                X2Criteria trnCriteria = m_studentScheduleHelper.getStudentTranscriptCriteria();

                SubQuery studentSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID,
                        m_studentScheduleHelper.getStudentCriteria());

                trnCriteria.addIn(Transcript.COL_STUDENT_OID, studentSubQuery);

                if (codes != null) {
                    trnCriteria.addIn(trnApTest.getJavaName(), codes);
                } else {
                    trnCriteria.addEqualTo(trnApTest.getJavaName(), "__dummyValue__");
                }
                String[] columns = {Transcript.COL_STUDENT_OID,
                        Transcript.REL_MASTER_SCHEDULE + ModelProperty.PATH_DELIMITER + X2BaseBean.COL_OID};
                int[] sizes = {512, 1024};

                QueryByCriteria trnQuery = new QueryByCriteria(Transcript.class, trnCriteria);
                m_trnByStdOid = CRDCStudentScheduleData.this.getBroker().getGroupedCollectionByQuery(trnQuery,
                        columns,
                        sizes);
            }
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            boolean value = false;
            if (m_trnByStdOid != null && !m_trnByStdOid.isEmpty()) {
                SisStudent student = (SisStudent) entity.getBean();
                StudentScheduleCRDCEntity crdcEntity = (StudentScheduleCRDCEntity) entity;
                Map<String, Collection<Transcript>> mstTranscriptsMap = m_trnByStdOid.get(student.getOid());

                if (mstTranscriptsMap != null) {
                    Collection<Transcript> transcripts = mstTranscriptsMap.get(crdcEntity.getCurrentMST().getOid());
                    if (transcripts != null) {
                        if (CALC_PARAM_TEST.equals(field.getParameter())) {
                            value = true;
                        } else if (CALC_PARAM_SCORE.equals(field.getParameter())) {
                            for (Transcript transcript : transcripts) {
                                if (TRN_CODE_P.equals(transcript.getFieldValueByAlias(ALIAS_TRN_AP_TEST_RESULT))) {
                                    value = true;
                                }
                            }
                        }
                    }
                }
            }
            return Boolean.valueOf(value);
        }
    }

    protected static final String ALIAS_CRS_CATEGORY = "all-crs-CRDCCourseCategory";
    protected static final String ALIAS_CRS_CATEGORY_AP = "all-crs-CRDCAPCourseCategory";
    protected static final String INPUT_PARAM_BLANK_CATEGORY = "crsWithBlankCategory";

    protected String m_fieldCrsCategory;
    protected String m_fieldCrsCategoryAP;
    protected Boolean m_includeCrsOnlyWithCategory;
    protected PlainDate m_reportDateBlock1;
    protected PlainDate m_reportDateBlock2;
    protected CRDCStudentHistoryHelper m_studentScheduleHelper;



    /**
     * @see com.x2dev.procedures.statereporting.CRDCReportData#initialize()
     */
    @Override
    public void initialize() throws X2BaseException {
        super.initialize();

        m_reportDateBlock1 = (PlainDate) getParameter(PARAM_PART_1_DATE);
        m_includeCrsOnlyWithCategory =
                getParameter(INPUT_PARAM_BLANK_CATEGORY) != null ? (Boolean) getParameter(INPUT_PARAM_BLANK_CATEGORY)
                        : Boolean.FALSE;
        m_fieldCrsCategoryAP = translateAliasToJavaName(ALIAS_CRS_CATEGORY_AP, true);
        m_fieldCrsCategory = translateAliasToJavaName(ALIAS_CRS_CATEGORY, true);
        initializeCtxByReportDate();
        if (getParameter(PARAM_USE_BLOCK_SCHEDULING) != null) {
            Boolean useBlockScheduling = (Boolean) getParameter(PARAM_USE_BLOCK_SCHEDULING);
            if (useBlockScheduling.booleanValue()) {
                Calendar calendar = Calendar.getInstance();
                calendar.setTime(m_reportDateBlock1);
                calendar.add(Calendar.MONTH, 5);
                m_reportDateBlock2 = new PlainDate(calendar.getTime());
            }
        }
        m_studentScheduleHelper = new CRDCStudentHistoryHelper(this);
        m_studentScheduleHelper.setStudentSelectionMode(StudentHistoryHelper.MODE_SCHEDULE_SPANS);
        m_studentScheduleHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_INPUT, Boolean.TRUE);
        m_studentScheduleHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_EXCLUDE, Boolean.TRUE);
        setQuery(m_studentScheduleHelper.getStudentQuery(true));
        setEntityClass(StudentScheduleCRDCEntity.class);
        CRDCDataHelper crdcHelper = new CRDCDataHelper(this);
        addCalcs(crdcHelper.getUsedRetrievers());
        addValidators(crdcHelper.getUsedValidators());
    }

    /**
     * Initialize CTX by report date.
     */
    private void initializeCtxByReportDate() {
        X2Criteria criteria = new X2Criteria();
        criteria.addGreaterOrEqualThan(DistrictSchoolYearContext.COL_END_DATE, m_reportDateBlock1);
        criteria.addLessOrEqualThan(DistrictSchoolYearContext.COL_START_DATE, m_reportDateBlock1);

        DistrictSchoolYearContext context = (DistrictSchoolYearContext) getBroker()
                .getBeanByQuery(new QueryByCriteria(DistrictSchoolYearContext.class, criteria));
        this.setCurrentContext(context);
    }
}
