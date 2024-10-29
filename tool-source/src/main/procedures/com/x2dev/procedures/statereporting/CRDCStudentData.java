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

package com.x2dev.procedures.statereporting;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.ColumnQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.*;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.dictionary.DictionaryHelper;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.business.ContextAttributesManager;
import com.x2dev.sis.model.business.GradesManager;
import com.x2dev.sis.model.business.StudentContextAttributesManager;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.sis.tools.stateexports.StudentScheduleSpan;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.DateAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.Map.Entry;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Dataset for Student data to be used in final CRDC export.
 *
 * @author X2 Development Corporation
 */
public class CRDCStudentData extends CRDCReportData {
    /**
     * Implementation of StateReportEntity to be used for Student CRDC Data export.
     * This must be a public static inner class with a public no argument
     * constructor so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class StudentCRDCEntity extends StateReportEntity {

        /**
         * Instantiates a new student CRDC entity.
         */
        public StudentCRDCEntity() {
            // Public no argument constructor for dynamic instantiation.
        }

        SisStudent m_student;

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            String name = "[SASID: " + m_student.getStateId() +
                    ", LASID: " + m_student.getLocalId() + "] " + m_student.getNameView();

            return name;
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getFieldValidations()
         */
        @Override
        public Collection<StateReportValidationError> getFieldValidations() {
            Collection<StateReportValidationError> errors = Collections.EMPTY_LIST;
            String activeValue1 = getFieldValue(FIELD_ACTIVE_PART_1);
            String activeValue2 = getFieldValue(FIELD_ACTIVE_PART_2);
            if (CODE_YES.equals(activeValue1) || CODE_YES.equals(activeValue2)) {
                errors = super.getFieldValidations();
            }
            return errors;
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
            CRDCStudentData crdcData = (CRDCStudentData) data;
            m_student = (SisStudent) bean;
            if (data.isSchoolContext() && data.getSchool() != null
                    && !data.getSchool().getOid().equals(getFieldValue(FIELD_SCHOOL_OID))) {
                this.setRowCount(0);
            }
            // remove students with no enrollment before end date
            StudentEnrollment enrollment =
                    crdcData.m_studentHelper.getEnrollmentForDate(m_student.getOid(),
                            crdcData.getCurrentContext().getEndDate(), "EWSY");
            if (enrollment == null) {
                this.setRowCount(0);
            }
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
    }

    /**
     * Retrieve if a student is in 504 special education.
     *
     * @author X2 Development Corporation
     *
     */
    public class CARetriever504 implements FieldRetriever {
        public static final String STATE_CODE_504 = "101";

        Collection<String> m_studentsWith504 = new ArrayList<String>();

        /**
         * Instantiates a new CA retriever 504.
         */
        public CARetriever504() {
            super();

            DataDictionaryField programCodeField =
                    getDataDictionary().findDataDictionaryField(StudentProgramParticipation.class.getName(),
                            StudentProgramParticipation.COL_PROGRAM_CODE);

            if (!StringUtils.isEmpty(programCodeField.getReferenceTableOid())) {
                Calendar cal = Calendar.getInstance();
                cal.set(Calendar.DATE, 1);
                cal.set(Calendar.MONTH, 9);
                cal.set(Calendar.YEAR, 2015);
                Date validProgramDate = cal.getTime();

                X2Criteria criteria = new X2Criteria();
                criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, programCodeField.getReferenceTableOid());
                criteria.addEqualTo(ReferenceCode.COL_STATE_CODE, STATE_CODE_504);
                SubQuery subQuery = new SubQuery(ReferenceCode.class, ReferenceCode.COL_CODE, criteria);
                Collection<String> codes504 = getBroker().getSubQueryCollectionByQuery(subQuery);

                X2Criteria pgmCriteria = new X2Criteria();
                X2Criteria endDateCriteria = new X2Criteria();
                endDateCriteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_END_DATE, validProgramDate);
                X2Criteria orCriteria = new X2Criteria();
                orCriteria.addEmpty(StudentProgramParticipation.COL_END_DATE, getBroker().getPersistenceKey());
                endDateCriteria.addOrCriteria(orCriteria);
                pgmCriteria.addAndCriteria(endDateCriteria);
                pgmCriteria.addIn(StudentProgramParticipation.COL_PROGRAM_CODE, codes504);

                QueryByCriteria pgmQuery = new QueryByCriteria(StudentProgramParticipation.class, pgmCriteria);

                QueryIterator iterator = CRDCStudentData.this.getBroker().getIteratorByQuery(pgmQuery);
                try {
                    while (iterator.hasNext()) {
                        StudentProgramParticipation pgm = (StudentProgramParticipation) iterator.next();
                        m_studentsWith504.add(pgm.getStudentOid());
                    }
                } finally {
                    iterator.close();
                }
            }
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            return Boolean.valueOf(m_studentsWith504.contains(entity.getBean().getOid()));
        }
    }

    /**
     * Retrieve if a student is in IDEA special education.
     *
     * @author X2 Development Corporation
     *
     */
    public class CARetrieverIdea implements FieldRetriever {
        public static final String STATE_CODE_IDEA = "144";

        Collection<String> m_studentsWithIdea = new ArrayList<String>();

        /**
         * Instantiates a new CA retriever idea.
         */
        public CARetrieverIdea() {
            super();

            DataDictionaryField programCodeField =
                    getDataDictionary().findDataDictionaryField(StudentProgramParticipation.class.getName(),
                            StudentProgramParticipation.COL_PROGRAM_CODE);

            if (!StringUtils.isEmpty(programCodeField.getReferenceTableOid())) {
                Calendar cal = Calendar.getInstance();
                cal.set(Calendar.DATE, 1);
                cal.set(Calendar.MONTH, 9);
                cal.set(Calendar.YEAR, 2015);
                Date validProgramDate = cal.getTime();

                X2Criteria criteria = new X2Criteria();
                criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, programCodeField.getReferenceTableOid());
                criteria.addEqualTo(ReferenceCode.COL_STATE_CODE, STATE_CODE_IDEA);
                SubQuery subQuery = new SubQuery(ReferenceCode.class, ReferenceCode.COL_CODE, criteria);
                Collection<String> ideaCodes = getBroker().getSubQueryCollectionByQuery(subQuery);

                X2Criteria pgmCriteria = new X2Criteria();
                X2Criteria endDateCriteria = new X2Criteria();
                endDateCriteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_END_DATE, validProgramDate);
                X2Criteria orCriteria = new X2Criteria();
                orCriteria.addEmpty(StudentProgramParticipation.COL_END_DATE, getBroker().getPersistenceKey());
                endDateCriteria.addOrCriteria(orCriteria);
                pgmCriteria.addAndCriteria(endDateCriteria);
                pgmCriteria.addIn(StudentProgramParticipation.COL_PROGRAM_CODE, ideaCodes);

                QueryByCriteria pgmQuery = new QueryByCriteria(StudentProgramParticipation.class, pgmCriteria);

                QueryIterator iterator = CRDCStudentData.this.getBroker().getIteratorByQuery(pgmQuery);
                try {
                    while (iterator.hasNext()) {
                        StudentProgramParticipation pgm = (StudentProgramParticipation) iterator.next();
                        m_studentsWithIdea.add(pgm.getStudentOid());
                    }
                } finally {
                    iterator.close();
                }
            }
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            return Boolean.valueOf(m_studentsWithIdea.contains(entity.getBean().getOid()));
        }
    }

    /**
     * Retrieve if a student is in LEP for CA state.
     *
     * @author X2 Development Corporation
     *
     */
    public class CARetrieverLep extends RetrieverByStdAliasAndCode {
        private static final String ALIAS_EL_ACQUSITION_STATUS = "DOE ELA STATUS CODE";
        private static final String ALIAS_EL_START_DATE = "DOE ELA START DATE";

        private static final String EL_CODE = "EL";

        DateAsStringConverter m_dateConverter;

        String m_fieldElStatus;
        String m_fieldStartDate;

        Date m_validProgramDate;

        /**
         * Instantiates a new CA retriever lep.
         */
        public CARetrieverLep() {
            m_fieldElStatus = translateAliasToJavaName(ALIAS_EL_ACQUSITION_STATUS, true);
            m_fieldStartDate = translateAliasToJavaName(ALIAS_EL_START_DATE, true);

            Calendar cal = Calendar.getInstance();
            cal.set(Calendar.DATE, 1);
            cal.set(Calendar.MONTH, 9);
            cal.set(Calendar.YEAR, 2015);
            m_validProgramDate = cal.getTime();

            m_dateConverter = (DateAsStringConverter) ConverterFactory.getConverterForClass(PlainDate.class.getName(),
                    Locale.getDefault(), true);
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.x2dev.procedures.statereporting.CRDCStudentData.RetrieverByStdAliasAndCode#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();

            boolean value = false;

            if (EL_CODE.equals(student.getFieldValueByBeanPath(m_fieldElStatus))) {
                String startDateString = (String) student.getFieldValueByBeanPath(m_fieldStartDate);
                if (!StringUtils.isEmpty(startDateString)) {
                    PlainDate startDate = (PlainDate) m_dateConverter.parseSystemString(startDateString);
                    if (!startDate.after(m_validProgramDate)) {
                        value = true;
                    }
                }
            }

            return Boolean.valueOf(value);
        }
    }

    /**
     * CT Retriever to determine if a student is Dual Enrolled.
     *
     * @author X2 Development Corporation
     *
     */
    public class CTRetrieverDualEnr implements FieldRetriever {
        public static final String ALIAS_MST_DUAL_ENR = "DOE Dual Enrollment";

        Map<String, Collection<Transcript>> m_trnMapbyStdOid;

        /**
         * Instantiates a new CT retriever dual enr.
         */
        public CTRetrieverDualEnr() {
            super();

            DataDictionaryField mstDualEnr = getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_MST_DUAL_ENR);

            if (mstDualEnr != null) {
                X2Criteria trnCriteria = m_studentHelper.getStudentTranscriptCriteria().copy();
                trnCriteria.addNotEmpty(
                        Transcript.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER + SchoolCourse.REL_COURSE
                                + ModelProperty.PATH_DELIMITER + mstDualEnr.getJavaName(),
                        CRDCStudentData.this.getBroker().getPersistenceKey());
                trnCriteria.addEqualTo(Transcript.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());

                m_trnMapbyStdOid = CRDCStudentData.this.getBroker().getGroupedCollectionByQuery(
                        new QueryByCriteria(Transcript.class, trnCriteria), Transcript.COL_STUDENT_OID, 1024);
            } else {
                m_trnMapbyStdOid = new HashMap();
            }
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            boolean value = false;

            SisStudent student = (SisStudent) entity.getBean();

            if (m_trnMapbyStdOid.get(student.getOid()) != null && !m_trnMapbyStdOid.get(student.getOid()).isEmpty()) {
                value = true;
            }

            return Boolean.valueOf(value);
        }
    }

    /**
     * CT Retriever to determine if a student is Gifted.
     *
     * @author X2 Development Corporation
     *
     */
    public class CTRetrieverGifted implements FieldRetriever {
        private static final String ALIAS_GIFTED = "PSIS27";

        private static final String CRDC_CODE_GIFTED = "GiftedTalented";

        private Collection<String> m_codesWithCrdcGiftedTalented;

        private String m_fieldGifted;

        private boolean m_initialized;

        /**
         * Instantiates a new CT retriever gifted.
         */
        public CTRetrieverGifted() {
            m_fieldGifted = translateAliasToJavaName(ALIAS_GIFTED, true);
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            initialize();

            boolean value = false;

            SisStudent student = (SisStudent) entity.getBean();
            String giftedCode = (String) student.getFieldValueByBeanPath(m_fieldGifted);

            if (!StringUtils.isEmpty(giftedCode) && m_codesWithCrdcGiftedTalented.contains(giftedCode)) {
                value = true;
            }

            return Boolean.valueOf(value);
        }

        /**
         * Initialize.
         */
        private void initialize() {
            if (!m_initialized) {
                m_codesWithCrdcGiftedTalented =
                        getCodesForCRDCValue(SisStudent.class, m_fieldGifted, Arrays.asList(CRDC_CODE_GIFTED));
            }
        }
    }

    /**
     * CT Retriever to determine if a student is Gifted.
     *
     * @author X2 Development Corporation
     *
     */
    public class CTRetrieverIDEA implements FieldRetriever {
        private static final String ALIAS_IDEA = "PSIS16";

        private static final String CRDC_CODE_ACTIVE = "Active";

        private Collection<String> m_codesWithCrdcActive;

        private String m_fieldIdea;

        private boolean m_initialized;

        /**
         * Instantiates a new CT retriever IDEA.
         */
        public CTRetrieverIDEA() {
            m_fieldIdea = translateAliasToJavaName(ALIAS_IDEA, true);
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            initialize();

            boolean value = false;

            SisStudent student = (SisStudent) entity.getBean();

            if (m_stdSnapIDEA != null) {
                return Boolean.valueOf(m_stdSnapIDEA.contains(student.getOid()));
            }

            String ideaCode = (String) student.getFieldValueByBeanPath(m_fieldIdea);

            if (!StringUtils.isEmpty(ideaCode) && m_codesWithCrdcActive.contains(ideaCode)) {
                value = true;
            }

            return Boolean.valueOf(value);
        }

        /**
         * Initialize.
         */
        private void initialize() {
            if (!m_initialized) {
                m_codesWithCrdcActive =
                        getCodesForCRDCValue(SisStudent.class, m_fieldIdea, Arrays.asList(CRDC_CODE_ACTIVE));
            }
        }
    }

    /**
     * MA Retriever to determine if a student is an LEP student.
     *
     * @author X2 Development Corporation
     *
     */
    public class CTRetrieverLEP implements FieldRetriever {
        public static final String ALIAS_PSIS14 = "PSIS14";
        private String m_fieldPSIS14;

        /**
         * Instantiates a new MA retriever LEP.
         */
        public CTRetrieverLEP() {
            super();
            m_fieldPSIS14 = CRDCStudentData.this.translateAliasToJavaName(ALIAS_PSIS14, true);
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();
            String stdOid = student.getOid();

            if (m_stdSnapLEP != null) {
                return Boolean.valueOf(m_stdSnapLEP.contains(stdOid));
            }

            if (m_fieldPSIS14 != null) {
                return student.getFieldValueByBeanPath(m_fieldPSIS14);
            }

            return "N";
        }
    }

    /**
     * CT Retriever to determine if a student is in LEP enrolled.
     *
     * @author X2 Development Corporation
     *
     */
    public class CTRetrieverLepEnr implements FieldRetriever {
        public static final String ALIAS_STD_LEP_ENR = "PSIS15";

        Collection<String> m_codes = null;

        /**
         * Instantiates a new CT retriever lep enr.
         */
        public CTRetrieverLepEnr() {
            super();

            DataDictionaryField stdLepEnrCode = getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_STD_LEP_ENR);

            if (stdLepEnrCode != null && !StringUtils.isEmpty(stdLepEnrCode.getReferenceTableOid())) {

                X2Criteria criteria = new X2Criteria();
                criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, stdLepEnrCode.getReferenceTableOid());
                criteria.addNotEmpty(ReferenceCode.COL_STATE_CODE,
                        CRDCStudentData.this.getBroker().getPersistenceKey());

                QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, criteria);

                Map<String, ReferenceCode> mapCodesByStateCode = CRDCStudentData.this.getBroker().getMapByQuery(query,
                        ReferenceCode.COL_CODE,
                        1024);

                if (mapCodesByStateCode != null && !mapCodesByStateCode.isEmpty()) {
                    m_codes = new ArrayList<String>();
                    m_codes.addAll(mapCodesByStateCode.keySet());
                }
            }
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();
            String stdOid = student.getOid();

            if (m_stdSnapLEPPgm != null) {
                return Boolean.valueOf(m_stdSnapLEPPgm.contains(stdOid));
            }

            boolean value = false;

            if (m_codes != null && m_codes.contains(student.getFieldValueByAlias(ALIAS_STD_LEP_ENR))) {
                value = true;
            }
            return Boolean.valueOf(value);
        }
    }

    /**
     * Retrieves if there is any conduct for student with appropriate logical field value == true
     * based on field parameter.
     *
     * @author Follett Software Company
     */
    public class CTRetrievStdActBool implements FieldRetriever {
        private static final String ALIAS_ARRESTED = "DOE ARRESTED";

        private static final String PARAM_ARRESTED = "SchoolRelatedArrest";

        private Set<String> m_crdcActionCodes = new HashSet<String>();

        /**
         * Instantiates a new CT retriev std act bool.
         */
        public CTRetrievStdActBool() {
            /*
             * Needed to report validation error if alias is not setup.
             */
            CRDCStudentData.this.translateAliasToJavaName(ALIAS_ARRESTED, true);

            DataDictionaryField crdcRefCodeField =
                    getDataDictionary().findDataDictionaryFieldByAlias(CRDCReportData.ALIAS_CRDC_REF_CODE);

            ModelProperty actionCodeProperty = new ModelProperty(ConductAction.class,
                    ConductAction.COL_ACTION_CODE,
                    CRDCStudentData.this.getBroker().getPersistenceKey());

            DataDictionaryField actionCodeField =
                    getDataDictionary().findDataDictionaryField(actionCodeProperty.getFieldId());

            if (crdcRefCodeField != null && !StringUtils.isEmpty(actionCodeField.getReferenceTableOid())) {
                Collection<ReferenceCode> refActionCodes = actionCodeField.getReferenceTable().getReferenceCodes();

                for (ReferenceCode refActionCode : refActionCodes) {
                    String currentCrdcActionCode =
                            (String) refActionCode.getFieldValueByAlias(CRDCReportData.ALIAS_CRDC_REF_CODE);
                    if (!StringUtils.isEmpty(currentCrdcActionCode)) {
                        m_crdcActionCodes.add(currentCrdcActionCode);
                    }
                }
            }
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            boolean isExist = false;
            String parameter = field.getParameter() == null ? "" : field.getParameter().toString();
            String alias = null;

            switch (parameter) {
                case PARAM_ARRESTED:
                    alias = ALIAS_ARRESTED;
                    break;

                default:
                    break;
            }

            CRDCStudentData stdCRDCData = (CRDCStudentData) data;
            for (String crdcActionCode : m_crdcActionCodes) {
                Set<ConductAction> actions =
                        stdCRDCData.m_conductHelper.getActionsForStudent(entity.getBean().getOid(), crdcActionCode);

                if (actions != null) {
                    for (ConductAction action : actions) {
                        String valueByAlias = (String) action.getFieldValueByAlias(alias);

                        if (BooleanAsStringConverter.TRUE.equals(valueByAlias)) {
                            isExist = true;
                        }
                    }
                }
            }

            return Boolean.valueOf(isExist);
        }
    }

    /**
     * GA Retriever to determine if a student participate in Credit Recovery Program.
     *
     * @author X2 Development Corporation
     *
     */
    public class GARetrieverCredRecov extends RetrieverStdCrs {
        public static final String ALIAS_MST_DOE_CREDIT_RECOVERY = "DOE Credit Recovery";
        private final static String GRADE_FIELD_NAME = "Grade";
        private final Set<String> INCLUDE_GRADE_LEVELS = new HashSet(Arrays.asList("09", "10", "11", "12"));

        String m_fieldCreditRecovery;

        /**
         * Instantiates a new GA retriever cred recov.
         */
        public GARetrieverCredRecov() {
            super();
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.x2dev.procedures.statereporting.CRDCStudentData.RetrieverStdCrs#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object value = Boolean.FALSE;
            if (INCLUDE_GRADE_LEVELS.contains(entity.getFieldValue(GRADE_FIELD_NAME))) {
                value = super.getFieldValue(data, entity, field);
            }
            return value;
        }

        /**
         * Apply criteria.
         *
         * @see com.x2dev.procedures.statereporting.CRDCStudentData.RetrieverStdCrs#applyCriteria()
         */
        @Override
        void applyCriteria() {
            m_fieldCreditRecovery = CRDCStudentData.this.translateAliasToJavaName(ALIAS_MST_DOE_CREDIT_RECOVERY, true);
            this.addEqualSectionField(m_fieldCreditRecovery, BooleanAsStringConverter.TRUE);
        }

    }

    /**
     * The Class GARetrieverDistEd.
     */
    public class GARetrieverDistEd extends RetrieverStdCrsNoLike {

        /**
         * Gets the pattern.
         *
         * @return String
         * @see com.x2dev.procedures.statereporting.CRDCStudentData.RetrieverStdCrsNoLike#getPattern()
         */
        @Override
        String getPattern() {
            return "__.3______";
        }
    }

    /**
     * GA Retriever to determine if a student is dual enrolled.
     *
     * @author X2 Development Corporation
     *
     */
    public class GARetrieverDualEnr extends RetrieverStdCrsNoLike {
        private final static String GRADE_FIELD_NAME = "Grade";
        private final Set<String> INCLUDE_GRADE_LEVELS = new HashSet(Arrays.asList("09", "10", "11", "12"));

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.x2dev.procedures.statereporting.CRDCStudentData.RetrieverStdCrs#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object value = Boolean.FALSE;
            String grade = entity.getFieldValue(GRADE_FIELD_NAME);
            if (INCLUDE_GRADE_LEVELS.contains(grade)) {
                value = super.getFieldValue(data, entity, field);
            }
            return value;
        }

        /**
         * Do span dates check.
         *
         * @return true, if successful
         * @see com.x2dev.procedures.statereporting.CRDCStudentData.RetrieverStdCrs#doSpanDatesCheck()
         */
        @Override
        boolean doSpanDatesCheck() {
            return true;
        }

        /**
         * Gets the pattern.
         *
         * @return String
         * @see com.x2dev.procedures.statereporting.CRDCStudentData.RetrieverStdCrsNoLike#getPattern()
         */
        @Override
        String getPattern() {
            return "__.____4__";
        }
    }

    /**
     * Retrieve if a student is Expelled or Educational Services while expelled.
     *
     * @author X2 Development Corporation
     *
     */
    public class GARetrieverExpelled implements FieldRetriever {
        public static final String ALIAS_ACT_DOE_CONTINUATION = "DOE CONTINUATION";
        public static final String CALC_PARAM_ED_SERV = "EdServWhileExpelled";
        public static final String CALC_PARAM_EXPELLED = "Expelled";
        public static final String CRDC_CODE_ED_SERV = "EdServWhileExpelled";
        public static final String CRDC_CODE_EXPELLED = "Expelled";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            boolean value = false;
            String param = (String) field.getParameter();
            CRDCStudentData stdCRDCData = (CRDCStudentData) data;
            Set<ConductAction> actions =
                    stdCRDCData.m_conductHelper.getActionsForStudent(entity.getBean().getOid(), CRDC_CODE_EXPELLED);

            if (actions != null) {
                if (CALC_PARAM_EXPELLED.equals(param)) {
                    value = true;
                    for (ConductAction act : actions) {
                        if (BooleanAsStringConverter.TRUE
                                .equals(act.getFieldValueByAlias(ALIAS_ACT_DOE_CONTINUATION))) {
                            value = false;
                            break;
                        }
                    }
                } else if (CALC_PARAM_ED_SERV.equals(param)) {
                    for (ConductAction act : actions) {
                        if (BooleanAsStringConverter.TRUE
                                .equals(act.getFieldValueByAlias(ALIAS_ACT_DOE_CONTINUATION))) {
                            value = true;
                            break;
                        }
                    }

                    if (!value) {
                        Set<ConductAction> actionsWhileExpelled = stdCRDCData.m_conductHelper
                                .getActionsForStudent(entity.getBean().getOid(), CRDC_CODE_ED_SERV);

                        if (actionsWhileExpelled != null && !actionsWhileExpelled.isEmpty()) {
                            value = true;
                        }
                    }
                }
            }
            return Boolean.valueOf(value);
        }
    }

    /**
     * GA Retriever to determine if a student is Gifted/Talented.
     *
     * @author X2 Development Corporation
     *
     */
    public class GARetrieverGifted extends RetrieverStdCrsNoLike {

        /**
         * Do span dates check.
         *
         * @return true, if successful
         * @see com.x2dev.procedures.statereporting.CRDCStudentData.RetrieverStdCrs#doSpanDatesCheck()
         */
        @Override
        boolean doSpanDatesCheck() {
            return true;
        }

        /**
         * Gets the pattern.
         *
         * @return String
         * @see com.x2dev.procedures.statereporting.CRDCStudentData.RetrieverStdCrsNoLike#getPattern()
         */
        @Override
        String getPattern() {
            return "__.2______";
        }
    }

    /**
     * FL Retriever to determine if a student is in 504 Section.
     *
     * @author X2 Development Corporation
     *
     */
    public class FLRetriever504 implements FieldRetriever {
        private static final String ALIAS_ELIGIBLE = "pgm-504-eligible";
        private static final String ALIAS_REQUIRES_PLAN = "pgm-504-requires-plan";

        private static final String DDX_ID = "FL-PGM-504";
        private static final String FIELD_GRADE = "Grade";
        private static final String GRADE_LEVEL_PK = "PK";

        private DataDictionaryField m_fieldEligible;
        private DataDictionaryField m_fieldRequiresPlan;

        private Collection<String> m_stdOids;

        /**
         * Instantiates a new FL retriever idea.
         */
        public FLRetriever504() {
            super();

            X2Criteria ddxCriteria = new X2Criteria();

            ddxCriteria.addEqualTo(ExtendedDataDictionary.COL_ID, DDX_ID);

            QueryByCriteria ddxQuery = new QueryByCriteria(ExtendedDataDictionary.class, ddxCriteria);
            ExtendedDataDictionary ddx = (ExtendedDataDictionary) getBroker().getBeanByQuery(ddxQuery);

            DataDictionary dictionary = DataDictionary.getDistrictDictionary(ddx, getBroker().getPersistenceKey());

            m_fieldEligible = dictionary.findDataDictionaryFieldByAlias(ALIAS_ELIGIBLE);
            m_fieldRequiresPlan = dictionary.findDataDictionaryFieldByAlias(ALIAS_REQUIRES_PLAN);

            X2Criteria criteria = new X2Criteria();
            // Date range criteria
            criteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, m_reportDatePart1);
            X2Criteria endDate1Criteria = new X2Criteria();
            endDate1Criteria.addEmpty(StudentProgramParticipation.COL_END_DATE, getBroker().getPersistenceKey());
            X2Criteria endDate2Criteria = new X2Criteria();
            endDate2Criteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_END_DATE, m_reportDatePart1);
            endDate1Criteria.addOrCriteria(endDate2Criteria);
            criteria.addAndCriteria(endDate1Criteria);

            // Extended data dictionary
            criteria.addEqualTo(StudentProgramParticipation.REL_EXTENDED_DATA_DICTIONARY + PATH_DELIMITER
                    + ExtendedDataDictionary.COL_ID, DDX_ID);

            criteria.addNotEqualTo(m_fieldEligible.getJavaName(), BooleanAsStringConverter.FALSE);
            criteria.addEqualTo(m_fieldRequiresPlan.getJavaName(), BooleanAsStringConverter.TRUE);

            SubQuery subQuery = new SubQuery(StudentProgramParticipation.class,
                    StudentProgramParticipation.COL_STUDENT_OID, criteria);
            m_stdOids = getBroker().getSubQueryCollectionByQuery(subQuery);
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();
            String stdOid = student.getOid();

            if (m_stdSnap504 != null) {
                return Boolean.valueOf(m_stdSnap504.contains(stdOid));
            }

            boolean value = false;

            String retievedIdea = entity.getFieldValue(FIELD_IDEA);

            if (CODE_YES.equals(retievedIdea)) {
                return Boolean.valueOf(value);
            }

            boolean isPKgradeLevel = GRADE_LEVEL_PK.equals(entity.getFieldValue(FIELD_GRADE));
            boolean isStudent504Eligible = m_stdOids.contains(student.getOid());

            return Boolean.valueOf(!isPKgradeLevel && isStudent504Eligible);
        }
    }

    /**
     * Retriever to determine if a student was arrested.
     *
     * @author Follett Software Company
     */
    public class FLRetrieverArrested implements FieldRetriever {
        public static final String ALIAS_ARRESTED = "all-cnd-Arrested";

        private Collection<String> m_stdOids = null;

        /**
         * Instantiates a new FL retriever arrested.
         */
        public FLRetrieverArrested() {
            X2Criteria criteria = new X2Criteria();
            criteria.addGreaterOrEqualThan(ConductIncident.COL_INCIDENT_DATE, getCurrentContext().getStartDate());
            criteria.addLessOrEqualThan(ConductIncident.COL_INCIDENT_DATE, getCurrentContext().getEndDate());
            String arrestedJavaName = translateAliasToJavaName(ALIAS_ARRESTED, true);
            criteria.addEqualTo(arrestedJavaName, BooleanAsStringConverter.TRUE);

            SubQuery query = new SubQuery(ConductIncident.class, ConductIncident.COL_STUDENT_OID, criteria);

            m_stdOids = getBroker().getSubQueryCollectionByQuery(query);
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            return Boolean.valueOf(m_stdOids.contains(entity.getBean().getOid()));
        }
    }

    /**
     * Retrieves count of student's bullying related incidents based on field parameter.
     */
    public class FLRetrBullCndCnt implements FieldRetriever {
        private static final String ALIAS_BULLYING = "all-cnd-BullyingRelated";
        private static final String ALIAS_HARASSMENT_DISB = "all-cnd-HarassmentBasedOnDisability";
        private static final String ALIAS_HARASSMENT_RACE = "all-cnd-HarassmentBasedOnRace";
        private static final String ALIAS_HARASSMENT_SEX = "all-cnd-HarassmentBasedOnSex";

        private static final int INDEX_SUBJECT = 0;
        private static final int INDEX_TYPE = 1;

        private static final String PARAM_TYPE_DISB = "DISB";
        private static final String PARAM_TYPE_RACE = "RACE";
        private static final String PARAM_TYPE_SEX = "SEX";

        private static final String PARAM_SEPARATOR = ":";

        private static final String PARAM_SUBJECT_STUDENT = "STUDENT";
        private static final String PARAM_SUBJECT_VICTIM = "VICTIM";

        private Map<String, Collection<ConductIncident>> m_stdIncidents = null;
        private Map<String, Collection<ConductIncident>> m_victimIncidents = null;

        String m_disbJavaName = null;
        String m_raceJavaName = null;
        String m_sexJavaName = null;

        /**
         * Instantiates a new FLRetrBullCndCnt.
         */
        public FLRetrBullCndCnt() {
            m_stdIncidents = new HashMap<>();
            m_victimIncidents = new HashMap<>();

            m_disbJavaName = translateAliasToJavaName(ALIAS_HARASSMENT_DISB, true);
            m_raceJavaName = translateAliasToJavaName(ALIAS_HARASSMENT_RACE, true);
            m_sexJavaName = translateAliasToJavaName(ALIAS_HARASSMENT_SEX, true);

            X2Criteria criteria = new X2Criteria();
            criteria.addGreaterOrEqualThan(ConductIncident.COL_INCIDENT_DATE, getCurrentContext().getStartDate());
            criteria.addLessOrEqualThan(ConductIncident.COL_INCIDENT_DATE, getCurrentContext().getEndDate());
            String bullyingJavaName = translateAliasToJavaName(ALIAS_BULLYING, true);
            criteria.addEqualTo(bullyingJavaName, BooleanAsStringConverter.TRUE);

            X2Criteria andCriteria = new X2Criteria();
            String disbJavaName = translateAliasToJavaName(ALIAS_HARASSMENT_DISB, true);
            String raceJavaName = translateAliasToJavaName(ALIAS_HARASSMENT_RACE, true);
            String sexJavaName = translateAliasToJavaName(ALIAS_HARASSMENT_SEX, true);
            andCriteria.addEqualTo(disbJavaName, BooleanAsStringConverter.TRUE);
            andCriteria.addOrEqualTo(raceJavaName, BooleanAsStringConverter.TRUE);
            andCriteria.addOrEqualTo(sexJavaName, BooleanAsStringConverter.TRUE);

            criteria.addAndCriteria(andCriteria);

            QueryByCriteria query = new QueryByCriteria(ConductIncident.class, criteria);

            Collection<ConductIncident> incidents = getBroker().getCollectionByQuery(query);

            for (ConductIncident inc : incidents) {
                addIncident(inc.getStudentOid(), inc, m_stdIncidents);
                addIncident(inc.getVictimOid(), inc, m_victimIncidents);
            }
        }

        /**
         * Adds the incident.
         *
         * @param key String
         * @param inc ConductIncident
         * @param map Map<String,Collection<ConductIncident>>
         */
        private void addIncident(String key, ConductIncident inc, Map<String, Collection<ConductIncident>> map) {
            if (key == null) {
                return;
            }
            Collection<ConductIncident> incidents = map.get(key);
            if (incidents == null) {
                incidents = new ArrayList<>();
                map.put(key, incidents);
            }
            incidents.add(inc);
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String parameter = (String) field.getParameter();

            String subject = null;
            String incidentTypeSelector = null;
            if (parameter != null) {
                subject = parameter.split(PARAM_SEPARATOR)[INDEX_SUBJECT];
                incidentTypeSelector = parameter.split(PARAM_SEPARATOR)[INDEX_TYPE];
            }

            Map<String, Collection<ConductIncident>> incidents = null;
            if (PARAM_SUBJECT_STUDENT.equals(subject)) {
                incidents = m_stdIncidents;
            } else if (PARAM_SUBJECT_VICTIM.equals(subject)) {
                incidents = m_victimIncidents;
            }

            Collection<ConductIncident> studentIncidents = null;
            if (incidents != null) {
                studentIncidents = incidents.get(entity.getBean().getOid());
            }

            String filterJavaName = null;
            if (PARAM_TYPE_DISB.equals(incidentTypeSelector)) {
                filterJavaName = m_disbJavaName;
            } else if (PARAM_TYPE_RACE.equals(incidentTypeSelector)) {
                filterJavaName = m_raceJavaName;
            } else if (PARAM_TYPE_SEX.equals(incidentTypeSelector)) {
                filterJavaName = m_sexJavaName;
            }

            int count = 0;
            if (studentIncidents != null) {
                for (ConductIncident inc : studentIncidents) {
                    count += BooleanAsStringConverter.TRUE.equals(inc.getFieldValueByBeanPath(filterJavaName)) ? 1 : 0;
                }
            }

            return Integer.valueOf(count);
        }
    }

    /**
     * Retrieve if a student is dual enrolled.
     *
     * @author X2 Development Corporation
     *
     */
    public class FLRetrieverDualEnr extends RetrieverStdCrs {

        protected static final String ALIAS_DUAL_IND_SSC = "all-ssc-DualEnrollmentIndicator";
        protected static final String ALIAS_DUAL_IND_SCC = "all-scc-DualEnrollmentIndicator";
        protected static final String ALIAS_DUAL_IND_MST = "all-mst-DualEnrollmentIndicator";

        private Collection<String> m_dualCodes = null;

        /**
         * Instantiates a new FL retriever dual enr.
         */
        public FLRetrieverDualEnr() {
            m_dualCodes = Arrays.asList("A", "B", "C", "E");
        }

        /**
         * Apply criteria.
         *
         * @see com.x2dev.procedures.statereporting.CRDCStudentData.RetrieverStdCrs#applyCriteria()
         */
        @Override
        void applyCriteria() {

            if (m_tnHelper == null) {
                m_tnHelper = getSchedHistoryHelper(m_reportDatePart1);
            }

            X2Criteria sccAndCriteria = new X2Criteria();
            X2Criteria sscAndCriteria = new X2Criteria();

            DataDictionaryField sccDualIndField =
                    getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_DUAL_IND_SCC);
            DataDictionaryField sscDualIndField =
                    getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_DUAL_IND_SSC);
            DataDictionaryField mstDualIndField =
                    getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_DUAL_IND_MST);

            sccAndCriteria.addIn(sccDualIndField.getJavaName(), m_dualCodes);
            X2Criteria orMstDualCriteria = new X2Criteria();
            orMstDualCriteria.addIn(StudentScheduleChange.REL_MASTER_SCHEDULE + PATH_DELIMITER +
                    mstDualIndField.getJavaName(), m_dualCodes);
            sccAndCriteria.addOrCriteria(orMstDualCriteria);
            m_tnHelper.getStudentScheduleChangeCriteria().addAndCriteria(sccAndCriteria);

            sscAndCriteria.addIn(sscDualIndField.getJavaName(), m_dualCodes);
            orMstDualCriteria = new X2Criteria();
            orMstDualCriteria.addIn(StudentSchedule.REL_SECTION + PATH_DELIMITER +
                    mstDualIndField.getJavaName(), m_dualCodes);
            sscAndCriteria.addOrCriteria(orMstDualCriteria);
            m_tnHelper.getStudentScheduleCriteria().addAndCriteria(sscAndCriteria);
        }

        /**
         * Do span dates check.
         *
         * @return true, if successful
         * @see com.x2dev.procedures.statereporting.CRDCStudentData.RetrieverStdCrs#doSpanDatesCheck()
         */
        @Override
        boolean doSpanDatesCheck() {
            return true;
        }
    }

    /**
     * Retrieve FL RecvHSEquivalency field.
     *
     * @author X2 Development Corporation
     *
     */
    public class FLRetrieverGED implements FieldRetriever {
        public static final String ALIAS_DOCUMENT_DATE = "all-std-DiplomaDate";
        public static final String ALIAS_DOCUMENT_TYPE = "all-std-DiplomaType";

        private String m_fieldDocumentDate;
        private String m_fieldDocumentType;
        private Set<String> m_matchingOids = new HashSet();

        /**
         * Instantiates a new TN retriever GED.
         */
        public FLRetrieverGED() {
            m_fieldDocumentDate = CRDCStudentData.this.translateAliasToJavaName(ALIAS_DOCUMENT_DATE, true);
            m_fieldDocumentType = CRDCStudentData.this.translateAliasToJavaName(ALIAS_DOCUMENT_TYPE, true);
            if (m_fieldDocumentDate != null && m_fieldDocumentType != null) {
                DateAsStringConverter converter =
                        (DateAsStringConverter) ConverterFactory.getConverterForClass(PlainDate.class.getName(),
                                Locale.getDefault(), true);
                String startDate = converter.getSystemString(CRDCStudentData.this.getCurrentContext().getStartDate());
                String endDate = converter.getSystemString(CRDCStudentData.this.getCurrentContext().getEndDate());
                Set gedCodes = CRDCStudentData.this.getCodesForCRDCValue(SisStudent.class, m_fieldDocumentType,
                        Arrays.asList("GED"));
                X2Criteria criteria = new X2Criteria();
                criteria.addLessOrEqualThan(m_fieldDocumentDate, endDate);
                criteria.addGreaterOrEqualThan(m_fieldDocumentDate, startDate);
                criteria.addIn(m_fieldDocumentType, gedCodes);

                String[] columns = new String[] {X2BaseBean.COL_OID};
                ReportQueryByCriteria query = new ReportQueryByCriteria(SisStudent.class, columns, criteria);
                ReportQueryIterator queryItr = CRDCStudentData.this.getBroker().getReportQueryIteratorByQuery(query);
                try {
                    while (queryItr.hasNext()) {
                        Object[] row = (Object[]) queryItr.next();
                        String oid = (String) row[0];
                        m_matchingOids.add(oid);
                    }
                } finally {
                    queryItr.close();
                }
            }
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            return Boolean.valueOf(m_matchingOids.contains(entity.getBean().getOid()));
        }
    }

    /**
     * FL Retriever to determine if a student is gifted/talented.
     *
     * @author X2 Development Corporation
     *
     */
    public class FLRetrieverGift implements FieldRetriever {

        private static final String ALIAS_GIFTED_ELIGIBILITY = "pgm-gifted-eligibility";
        private static final String DDX_ID = "FL-PGM-EXCEPT";

        Collection<String> m_eligibleCodes = Arrays.asList("A", "B");

        private DataDictionaryField m_fieldGiftedEligibility;

        private Collection<String> m_stdOids;

        /**
         * Instantiates a new FL retriever gift.
         */
        public FLRetrieverGift() {

            X2Criteria ddxCriteria = new X2Criteria();

            ddxCriteria.addEqualTo(ExtendedDataDictionary.COL_ID, DDX_ID);

            QueryByCriteria ddxQuery = new QueryByCriteria(ExtendedDataDictionary.class, ddxCriteria);
            ExtendedDataDictionary ddx = (ExtendedDataDictionary) getBroker().getBeanByQuery(ddxQuery);

            DataDictionary dictionary = DataDictionary.getDistrictDictionary(ddx, getBroker().getPersistenceKey());

            m_fieldGiftedEligibility = dictionary.findDataDictionaryFieldByAlias(ALIAS_GIFTED_ELIGIBILITY);

            X2Criteria criteria = new X2Criteria();
            // Date range criteria
            criteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, m_reportDatePart1);
            X2Criteria endDate1Criteria = new X2Criteria();
            endDate1Criteria.addEmpty(StudentProgramParticipation.COL_END_DATE, getBroker().getPersistenceKey());
            X2Criteria endDate2Criteria = new X2Criteria();
            endDate2Criteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_END_DATE, m_reportDatePart1);
            endDate1Criteria.addOrCriteria(endDate2Criteria);
            criteria.addAndCriteria(endDate1Criteria);

            // Extended data dictionary
            criteria.addEqualTo(StudentProgramParticipation.REL_EXTENDED_DATA_DICTIONARY + PATH_DELIMITER
                    + ExtendedDataDictionary.COL_ID, DDX_ID);

            criteria.addIn(m_fieldGiftedEligibility.getJavaName(), m_eligibleCodes);

            SubQuery subQuery = new SubQuery(StudentProgramParticipation.class,
                    StudentProgramParticipation.COL_STUDENT_OID, criteria);
            m_stdOids = getBroker().getSubQueryCollectionByQuery(subQuery);
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.x2dev.procedures.statereporting.CRDCStudentData.RetrieverByStdAliasAndCode#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();
            String stdOid = student.getOid();

            return Boolean.valueOf(m_stdOids.contains(stdOid));
        }
    }

    /**
     * Retrieve if a student is in IDEA special education for FL state.
     *
     * @author X2 Development Corporation
     *
     */
    public class FLRetrieverIdea implements FieldRetriever {
        private static final String DDX_ID = "FL-PGM-IDEA-B";

        private Collection<String> m_stdOids;

        /**
         * Instantiates a new FL retriever idea.
         */
        public FLRetrieverIdea() {
            super();

            X2Criteria criteria = new X2Criteria();
            // Date range criteria
            criteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, m_reportDatePart1);
            X2Criteria endDate1Criteria = new X2Criteria();
            endDate1Criteria.addEmpty(StudentProgramParticipation.COL_END_DATE, getBroker().getPersistenceKey());
            X2Criteria endDate2Criteria = new X2Criteria();
            endDate2Criteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_END_DATE, m_reportDatePart1);
            endDate1Criteria.addOrCriteria(endDate2Criteria);
            criteria.addAndCriteria(endDate1Criteria);

            // Extended data dictionary
            criteria.addEqualTo(StudentProgramParticipation.REL_EXTENDED_DATA_DICTIONARY + PATH_DELIMITER
                    + ExtendedDataDictionary.COL_ID, DDX_ID);

            SubQuery subQuery = new SubQuery(StudentProgramParticipation.class,
                    StudentProgramParticipation.COL_STUDENT_OID, criteria);
            m_stdOids = getBroker().getSubQueryCollectionByQuery(subQuery);
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();
            String stdOid = student.getOid();

            if (m_stdSnapIDEA != null) {
                return Boolean.valueOf(m_stdSnapIDEA.contains(stdOid));
            }

            boolean value = false;

            if (m_stdOids.contains(student.getOid())) {
                value = true;
            }
            return Boolean.valueOf(value);
        }
    }

    /**
     * Retrieve if a student is in LEP for FL state.
     *
     * @author X2 Development Corporation
     *
     */
    public class FLRetrieverLep implements FieldRetriever {

        public static final String DDX_ID = "FL-PGM-ELL";

        private static final String ALIAS_ELL_CODE = "pgm-ell-code";

        private Collection<String> m_ellCodes = Arrays.asList("LF", "LP", "LY", "LZ");

        private DataDictionaryField m_fieldELLCode;

        protected Collection<String> m_stdOids;

        /**
         * Instantiates a new FL retriever lep.
         */
        public FLRetrieverLep() {
            super();

            X2Criteria ddxCriteria = new X2Criteria();

            ddxCriteria.addEqualTo(ExtendedDataDictionary.COL_ID, DDX_ID);

            QueryByCriteria ddxQuery = new QueryByCriteria(ExtendedDataDictionary.class, ddxCriteria);
            ExtendedDataDictionary ddx = (ExtendedDataDictionary) getBroker().getBeanByQuery(ddxQuery);

            DataDictionary dictionary = DataDictionary.getDistrictDictionary(ddx, getBroker().getPersistenceKey());

            m_fieldELLCode = dictionary.findDataDictionaryFieldByAlias(ALIAS_ELL_CODE);

            X2Criteria criteria = new X2Criteria();
            // Date range criteria
            criteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, m_reportDatePart1);
            X2Criteria endDate1Criteria = new X2Criteria();
            endDate1Criteria.addEmpty(StudentProgramParticipation.COL_END_DATE, getBroker().getPersistenceKey());
            X2Criteria endDate2Criteria = new X2Criteria();
            endDate2Criteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_END_DATE, m_reportDatePart1);
            endDate1Criteria.addOrCriteria(endDate2Criteria);
            criteria.addAndCriteria(endDate1Criteria);

            // Extended data dictionary
            criteria.addEqualTo(StudentProgramParticipation.REL_EXTENDED_DATA_DICTIONARY + PATH_DELIMITER
                    + ExtendedDataDictionary.COL_ID, DDX_ID);

            criteria.addIn(m_fieldELLCode.getJavaName(), m_ellCodes);

            SubQuery subQuery = new SubQuery(StudentProgramParticipation.class,
                    StudentProgramParticipation.COL_STUDENT_OID, criteria);
            m_stdOids = getBroker().getSubQueryCollectionByQuery(subQuery);
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.x2dev.procedures.statereporting.CRDCStudentData.RetrieverByStdAliasAndCode#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();
            String stdOid = student.getOid();

            if (m_stdSnapLEP != null) {
                return Boolean.valueOf(m_stdSnapLEP.contains(stdOid));
            }

            return Boolean.valueOf(m_stdOids.contains(stdOid));
        }

    }

    /**
     * Retrieve FL LEP field.
     */
    public class FLRetrieverLEPEnroll extends FLRetrieverLep {

        /**
         * Instantiates a new FL retriever LEP enroll.
         */
        public FLRetrieverLEPEnroll() {
            super();
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.x2dev.procedures.statereporting.CRDCStudentData.RetrieverPgmRepDate#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();
            String stdOid = student.getOid();

            if (m_stdSnapLEPPgm != null) {
                return Boolean.valueOf(m_stdSnapLEPPgm.contains(stdOid));
            }

            return Boolean.valueOf(m_stdOids.contains(stdOid));
        }
    }

    /**
     * Retriever to determine if a student is Law Enforced.
     *
     * @author Follett Software Company
     */
    public class FLRetrieverRefLaw implements FieldRetriever {
        public static final String ALIAS_LAW_ENFORCEMENT = "all-cnd-ReportedToLawEnforcement";
        private Collection<String> m_stdOids = null;

        /**
         * Instantiates a new FL retriever ref law.
         */
        public FLRetrieverRefLaw() {
            X2Criteria criteria = new X2Criteria();
            criteria.addGreaterOrEqualThan(ConductIncident.COL_INCIDENT_DATE, getCurrentContext().getStartDate());
            criteria.addLessOrEqualThan(ConductIncident.COL_INCIDENT_DATE, getCurrentContext().getEndDate());
            String lawEnforcementJavaName = translateAliasToJavaName(ALIAS_LAW_ENFORCEMENT, true);
            criteria.addEqualTo(lawEnforcementJavaName, CODE_YES);

            SubQuery query = new SubQuery(ConductIncident.class, ConductIncident.COL_STUDENT_OID, criteria);

            m_stdOids = getBroker().getSubQueryCollectionByQuery(query);
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            return Boolean.valueOf(m_stdOids.contains(entity.getBean().getOid()));
        }
    }

    /**
     * Retriever to determine if a student is expelled due to zero tolerance.
     *
     * @author Follett Software Company
     */
    public class FLZeroTolerance implements FieldRetriever {
        public static final String ALIAS_ZERO_TOLERANCE = "all-cnd-ZeroTolerance";

        private Collection<String> m_stdOids = null;

        /**
         * Instantiates a new FL retriever arrested.
         */
        public FLZeroTolerance() {
            X2Criteria criteria = new X2Criteria();
            criteria.addGreaterOrEqualThan(ConductIncident.COL_INCIDENT_DATE, getCurrentContext().getStartDate());
            criteria.addLessOrEqualThan(ConductIncident.COL_INCIDENT_DATE, getCurrentContext().getEndDate());
            String arrestedJavaName = translateAliasToJavaName(ALIAS_ZERO_TOLERANCE, true);
            criteria.addEqualTo(arrestedJavaName, BooleanAsStringConverter.TRUE);

            SubQuery query = new SubQuery(ConductIncident.class, ConductIncident.COL_STUDENT_OID, criteria);

            m_stdOids = getBroker().getSubQueryCollectionByQuery(query);
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            return Boolean.valueOf(m_stdOids.contains(entity.getBean().getOid()));
        }
    }

    /**
     * Retrieve if a student is in IDEA special education for GA state.
     *
     * @author X2 Development Corporation
     *
     */
    public class GARetrieverIdea implements FieldRetriever {
        Set<String> m_stdOids;

        /**
         * Instantiates a new GA retriever idea.
         */
        public GARetrieverIdea() {
            super();

            m_stdOids = new HashSet<String>();

            X2Criteria iepCriteria = new X2Criteria();
            iepCriteria.addLessOrEqualThan(IepData.COL_START_DATE, m_reportDatePart1);
            iepCriteria.addGreaterOrEqualThan(IepData.COL_END_DATE, m_reportDatePart1);

            X2Criteria studentExitDateCriteria = new X2Criteria();
            studentExitDateCriteria.addIsNull(IepData.REL_STUDENT + ModelProperty.PATH_DELIMITER +
                    Student.COL_SPED_EXIT_DATE);

            X2Criteria studentExitDateOrCriteria = new X2Criteria();
            studentExitDateOrCriteria.addGreaterThan(IepData.REL_STUDENT + ModelProperty.PATH_DELIMITER +
                    Student.COL_SPED_EXIT_DATE, m_reportDatePart1);

            studentExitDateCriteria.addOrCriteria(studentExitDateOrCriteria);
            studentExitDateCriteria.setEmbraced(true);

            iepCriteria.addAndCriteria(studentExitDateCriteria);

            QueryByCriteria iepQuery = new QueryByCriteria(IepData.class, iepCriteria);
            Map<String, Collection<IepData>> iepDataByStdOid =
                    CRDCStudentData.this.getBroker().getGroupedCollectionByQuery(iepQuery,
                            IepData.COL_STUDENT_OID,
                            1024);

            if (!iepDataByStdOid.isEmpty()) {
                m_stdOids.addAll(iepDataByStdOid.keySet());
            }
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();
            String stdOid = student.getOid();

            if (m_stdSnapIDEA != null) {
                return Boolean.valueOf(m_stdSnapIDEA.contains(stdOid));
            }

            boolean value = false;

            if (m_stdOids.contains(student.getOid())) {
                value = true;
            }
            return Boolean.valueOf(value);
        }
    }

    /**
     * Retrieve if a student is in LEP for GA state.
     *
     * @author X2 Development Corporation
     *
     */
    public class GARetrieverLep extends RetrieverByStdAliasAndCode {
        public static final String ALIAS_DOE_ELL = "DOE ELL";

        /**
         * Instantiates a new GA retriever lep.
         */
        public GARetrieverLep() {
            super(ALIAS_DOE_ELL, CODE_YES);
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.x2dev.procedures.statereporting.CRDCStudentData.RetrieverByStdAliasAndCode#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();
            String stdOid = student.getOid();

            if (m_stdSnapLEP != null) {
                return Boolean.valueOf(m_stdSnapLEP.contains(stdOid));
            }

            return super.getFieldValue(data, entity, field);
        }

    }

    /**
     * IL Retriever to determine if a student is Dual Enrolled.
     *
     * @author X2 Development Corporation
     *
     */
    public class ILRetrieverDualEnr implements FieldRetriever {
        public static final String ALIAS_SSC_DUAl_CREDIT = "DOE DUAL CREDIT";

        Map<String, Collection<StudentSchedule>> m_sscMapByStdOid;

        /**
         * Instantiates a new IL retriever dual enr.
         */
        public ILRetrieverDualEnr() {
            super();

            DataDictionaryField sscDualCreditField =
                    getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_SSC_DUAl_CREDIT);

            if (sscDualCreditField != null) {

                X2Criteria sscCriteria = m_studentHelper.getStudentScheduleCriteria().copy();
                sscCriteria.addEqualTo(sscDualCreditField.getJavaName(), BooleanAsStringConverter.TRUE);

                QueryByCriteria sscQuery = new QueryByCriteria(StudentSchedule.class, sscCriteria);
                m_sscMapByStdOid = CRDCStudentData.this.getBroker().getGroupedCollectionByQuery(sscQuery,
                        StudentSchedule.COL_STUDENT_OID,
                        1024);
            }
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            boolean value = false;
            SisStudent student = (SisStudent) entity.getBean();

            if (m_sscMapByStdOid != null && !m_sscMapByStdOid.isEmpty()
                    && m_sscMapByStdOid.get(student.getOid()) != null &&
                    !m_sscMapByStdOid.get(student.getOid()).isEmpty()) {
                value = true;
            }

            return Boolean.valueOf(value);
        }
    }

    /**
     * IL Retriever to determine if a student is in IDEA special education.
     *
     * @author X2 Development Corporation
     *
     */
    public class ILRetrieverIdea implements FieldRetriever {
        Set<String> m_stdOids;

        /**
         * Instantiates a new IL retriever idea.
         */
        public ILRetrieverIdea() {
            super();

            m_stdOids = new HashSet<String>();

            X2Criteria iepCriteria = new X2Criteria();
            iepCriteria.addEqualTo(IepData.COL_STATUS_CODE, Integer.valueOf(IepData.StatusCode.ACTIVE.ordinal()));
            iepCriteria.addLessOrEqualThan(IepData.COL_START_DATE, m_reportDatePart1);

            X2Criteria iepEndDateAndCriteria = new X2Criteria();
            X2Criteria iepEndDateOrCriteria = new X2Criteria();

            iepEndDateOrCriteria.addGreaterOrEqualThan(IepData.COL_END_DATE, m_reportDatePart1);

            iepEndDateAndCriteria.addIsNull(IepData.COL_END_DATE);
            iepEndDateAndCriteria.addOrCriteria(iepEndDateOrCriteria);

            iepCriteria.addAndCriteria(iepEndDateAndCriteria);

            QueryByCriteria iepQuery = new QueryByCriteria(IepData.class, iepCriteria);
            Map<String, Collection<IepData>> iepDataByStdOid =
                    CRDCStudentData.this.getBroker().getGroupedCollectionByQuery(iepQuery,
                            IepData.COL_STUDENT_OID,
                            1024);

            if (!iepDataByStdOid.isEmpty()) {
                m_stdOids.addAll(iepDataByStdOid.keySet());
            }
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();
            String stdOid = student.getOid();

            if (m_stdSnapIDEA != null) {
                return Boolean.valueOf(m_stdSnapIDEA.contains(stdOid));
            }

            boolean value = false;

            if (m_stdOids.contains(student.getOid())) {
                value = true;
            }
            return Boolean.valueOf(value);
        }
    }

    /**
     * MA Retriever to determine if a student is in 504 special education.
     *
     * @author X2 Development Corporation
     *
     */
    public class MARetriever504 implements FieldRetriever {
        public static final String ALIAS_STD_DOE_39 = "DOE 39";
        public static final String NOT_CODE_504 = "00";
        public static final String STATUS_ACTIVE = "Active";
        public static final String STATUS_EXITED = "Exited";

        String m_fieldDoe39;

        /**
         * Instantiates a new MA retriever 504.
         */
        public MARetriever504() {
            super();
            m_fieldDoe39 = CRDCStudentData.this.translateAliasToJavaName(ALIAS_STD_DOE_39, true);
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();
            String stdOid = student.getOid();

            if (m_stdSnap504 != null) {
                return Boolean.valueOf(m_stdSnap504.contains(stdOid));
            }

            boolean value = false;

            String retievedIdea = entity.getFieldValue(FIELD_IDEA);

            if (CODE_YES.equals(retievedIdea)) {
                return Boolean.valueOf(value);
            }

            if (m_fieldDoe39 != null) {
                String is504 = data.lookupReferenceCodeByBeanPath(getBeanClass(), m_fieldDoe39,
                        (String) student.getFieldValueByBeanPath(m_fieldDoe39),
                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                if (!StringUtils.isEmpty(is504) && !NOT_CODE_504.equals(is504)) {
                    String code = student.getSection504StatusCode();
                    PlainDate exitDate = student.getSection504LastEndDate();

                    if (STATUS_ACTIVE.equals(code) ||
                            (STATUS_EXITED.equals(code) && exitDate != null
                                    && !CRDCStudentData.this.m_reportDatePart1.after(exitDate))) {
                        value = true;
                    }
                }
            }
            return Boolean.valueOf(value);
        }
    }

    /**
     * MA Retriever to determine if a student is in IDEA special education.
     *
     * @author X2 Development Corporation
     *
     */
    public class MARetrieverIDEA implements FieldRetriever {
        public static final String ALIAS_STD_DOE_32 = "DOE 32";
        public static final String ALIAS_STD_DOE_34 = "DOE 34";
        public static final String NOT_IDEA_CODE = "00";

        String m_fieldDoe32;
        String m_fieldDoe34;

        /**
         * Instantiates a new MA retriever IDEA.
         */
        public MARetrieverIDEA() {
            super();
            m_fieldDoe32 = CRDCStudentData.this.translateAliasToJavaName(ALIAS_STD_DOE_32, true);
            m_fieldDoe34 = CRDCStudentData.this.translateAliasToJavaName(ALIAS_STD_DOE_34, true);
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();
            String stdOid = student.getOid();

            if (m_stdSnapIDEA != null) {
                return Boolean.valueOf(m_stdSnapIDEA.contains(stdOid));
            }

            boolean value = false;

            if (m_fieldDoe32 != null) {
                String code = data.lookupReferenceCodeByBeanPath(getBeanClass(), m_fieldDoe32,
                        (String) student.getFieldValueByBeanPath(m_fieldDoe32),
                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                if (!StringUtils.isEmpty(code) && !NOT_IDEA_CODE.equals(code)) {
                    value = true;
                }
            }

            if (m_fieldDoe34 != null && !StringUtils.isEmpty((String) student.getFieldValueByBeanPath(m_fieldDoe34))) {
                String code = data.lookupReferenceCodeByBeanPath(getBeanClass(), m_fieldDoe34,
                        (String) student.getFieldValueByBeanPath(m_fieldDoe34),
                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                if (!StringUtils.isEmpty(code) && !NOT_IDEA_CODE.equals(code)) {
                    value = true;
                }
            }

            return Boolean.valueOf(value);
        }
    }

    /**
     * MA Retriever to determine if a student is an LEP student.
     *
     * @author X2 Development Corporation
     *
     */
    public class MARetrieverLEP implements FieldRetriever {
        public static final String ALIAS_STD_DOE_21 = "DOE 21";
        public static final String ALIAS_STD_DOE_25 = "DOE 25";

        private Set<String> m_doe21Codes = new HashSet(Arrays.asList("01", "02"));
        private Set<String> m_doe25Codes = new HashSet(Arrays.asList("01"));
        private String m_fieldDoe21;
        private String m_fieldDoe25;

        /**
         * Instantiates a new MA retriever LEP.
         */
        public MARetrieverLEP() {
            super();
            m_fieldDoe21 = CRDCStudentData.this.translateAliasToJavaName(ALIAS_STD_DOE_21, true);
            m_fieldDoe25 = CRDCStudentData.this.translateAliasToJavaName(ALIAS_STD_DOE_25, true);
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();
            String stdOid = student.getOid();

            if (m_stdSnapLEP != null) {
                return Boolean.valueOf(m_stdSnapLEP.contains(stdOid));
            }

            boolean value = false;

            if (m_fieldDoe21 != null) {
                String code = data.lookupReferenceCodeByBeanPath(getBeanClass(), m_fieldDoe21,
                        (String) student.getFieldValueByBeanPath(m_fieldDoe21),
                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                if (!StringUtils.isEmpty(code) && m_doe21Codes.contains(code)) {
                    value = true;
                }
            }

            if (m_fieldDoe25 != null) {
                String code = data.lookupReferenceCodeByBeanPath(getBeanClass(), m_fieldDoe25,
                        (String) student.getFieldValueByBeanPath(m_fieldDoe25),
                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                if (!StringUtils.isEmpty(code) && m_doe25Codes.contains(code)) {
                    value = true;
                }
            }

            return Boolean.valueOf(value);
        }
    }

    /**
     * MA Retriever to determine if a student is an LEP Enrolled student.
     *
     * @author X2 Development Corporation
     *
     */
    public class MARetrieverLEPEnroll implements FieldRetriever {
        public static final String ALIAS_STD_DOE_26 = "DOE 26";

        private Set<String> m_doe26Codes = new HashSet(Arrays.asList("01", "02", "03"));
        private String m_fieldDoe26;

        /**
         * Instantiates a new MA retriever LEP enroll.
         */
        public MARetrieverLEPEnroll() {
            super();
            m_fieldDoe26 = CRDCStudentData.this.translateAliasToJavaName(ALIAS_STD_DOE_26, true);
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();
            String stdOid = student.getOid();

            if (m_stdSnapLEPPgm != null) {
                return Boolean.valueOf(m_stdSnapLEPPgm.contains(stdOid));
            }

            boolean value = false;

            if (m_fieldDoe26 != null) {
                String code = data.lookupReferenceCodeByBeanPath(getBeanClass(), m_fieldDoe26,
                        (String) student.getFieldValueByBeanPath(m_fieldDoe26),
                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                if (!StringUtils.isEmpty(code) && m_doe26Codes.contains(code)) {
                    value = true;
                }
            }

            return Boolean.valueOf(value);
        }
    }

    /**
     * MD Retriever to determine if a student is enrolled in distance education.
     *
     * @author X2 Development Corporation
     *
     */
    public class MDRetrieverDistEd extends RetrieverStdCrs {
        public static final String ALIAS_DOE_CRS_DELIVERY = "DOE COURSE DELIVERY";
        public static final String CRDC_CODE_DISTANCE = "Distance";

        /**
         * Apply criteria.
         *
         * @see com.x2dev.procedures.statereporting.CRDCStudentData.RetrieverStdCrs#applyCriteria()
         */
        @Override
        void applyCriteria() {
            DataDictionaryField crsDeliveryField =
                    getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_DOE_CRS_DELIVERY);
            Collection<String> crsDelivery = new ArrayList();

            if (crsDeliveryField != null) {
                ReferenceTable refTable = crsDeliveryField.getReferenceTable();
                if (refTable != null) {
                    Collection<ReferenceCode> refCodes = refTable.getReferenceCodes();

                    if (refCodes != null) {
                        for (ReferenceCode refCode : refCodes) {
                            String crdcCode = (String) refCode.getFieldValueByAlias(ALIAS_CRDC_REF_CODE);

                            if (CRDC_CODE_DISTANCE.equalsIgnoreCase(crdcCode)) {
                                crsDelivery.add(refCode.getCode());
                            }
                        }
                    }
                }

                if (!crsDelivery.isEmpty()) {
                    this.addInCourseField(crsDeliveryField.getJavaName(), crsDelivery);
                }
            }
        }
    }

    /**
     * MD Retriever to determine if a student is in IDEA special education.
     *
     * @author X2 Development Corporation
     *
     */
    public class MDRetrieverIdea implements FieldRetriever {
        public static final String ALIAS_DOE_STD_SPED_BEGIN = "DOE SPED BEGIN";
        public static final String ALIAS_DOE_STD_SPED_END = "DOE SPED END";
        public static final String CRDC_CODE_IDEA = "A";

        Collection<String> m_codes = new ArrayList<String>();

        /**
         * Instantiates a new MD retriever idea.
         */
        public MDRetrieverIdea() {
            super();

            DataDictionaryField aliasField = getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_CRDC_REF_CODE);
            DataDictionaryField stdSpedStatusField =
                    getDataDictionary().findDataDictionaryField(SisStudent.class.getName(),
                            SisStudent.COL_SPED_STATUS_CODE);

            if (aliasField != null && stdSpedStatusField != null &&
                    !StringUtils.isEmpty(stdSpedStatusField.getReferenceTableOid())) {
                X2Criteria criteria = new X2Criteria();
                criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, stdSpedStatusField.getReferenceTableOid());
                criteria.addEqualTo(aliasField.getJavaName(), CRDC_CODE_IDEA);

                QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, criteria);

                QueryIterator iterator = CRDCStudentData.this.getBroker().getIteratorByQuery(query);
                try {
                    while (iterator.hasNext()) {
                        ReferenceCode record = (ReferenceCode) iterator.next();
                        String code = record.getCode();
                        m_codes.add(code);
                    }
                } finally {
                    iterator.close();
                }
            }
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();
            String stdOid = student.getOid();

            if (m_stdSnapIDEA != null) {
                return Boolean.valueOf(m_stdSnapIDEA.contains(stdOid));
            }

            boolean value = false;

            if (m_codes.contains(student.getSpedStatusCode()) &&
                    (student.getFieldValueByAlias(ALIAS_DOE_STD_SPED_BEGIN) == null
                            || m_reportDatePart1
                                    .after((PlainDate) student.getFieldValueByAlias(ALIAS_DOE_STD_SPED_BEGIN)))
                    &&
                    (student.getFieldValueByAlias(ALIAS_DOE_STD_SPED_END) == null
                            || m_reportDatePart1
                                    .before((PlainDate) student.getFieldValueByAlias(ALIAS_DOE_STD_SPED_END)))) {
                value = true;
            }

            if (!value && !m_codes.contains(student.getSpedStatusCode()) &&
                    student.getFieldValueByAlias(ALIAS_DOE_STD_SPED_BEGIN) != null &&
                    student.getFieldValueByAlias(ALIAS_DOE_STD_SPED_END) != null &&
                    m_reportDatePart1.before((PlainDate) student.getFieldValueByAlias(ALIAS_DOE_STD_SPED_END))) {
                value = true;
            }

            return Boolean.valueOf(value);
        }
    }

    /**
     * NJ Retriever to determine if a student is in 504 special education.
     *
     * @author X2 Development Corporation
     *
     */
    public class NJRetriever504 implements FieldRetriever {
        Collection<String> m_codes = new ArrayList<String>();

        /**
         * Instantiates a new NJ retriever 504.
         */
        public NJRetriever504() {
            super();

            DataDictionaryField stdSpedStatusField =
                    getDataDictionary().findDataDictionaryField(SisStudent.class.getName(),
                            SisStudent.COL_SECTION504_STATUS_CODE);

            if (stdSpedStatusField != null && !StringUtils.isEmpty(stdSpedStatusField.getReferenceTableOid())) {
                X2Criteria criteria = new X2Criteria();
                criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, stdSpedStatusField.getReferenceTableOid());
                criteria.addEqualTo(ReferenceCode.COL_STATE_CODE, CODE_YES);

                QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, criteria);

                QueryIterator iterator = CRDCStudentData.this.getBroker().getIteratorByQuery(query);
                try {
                    while (iterator.hasNext()) {
                        ReferenceCode record = (ReferenceCode) iterator.next();
                        String code = record.getCode();
                        m_codes.add(code);
                    }
                } finally {
                    iterator.close();
                }
            }
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();
            String stdOid = student.getOid();

            if (m_stdSnap504 != null) {
                return Boolean.valueOf(m_stdSnap504.contains(stdOid));
            }

            boolean value = false;

            String retievedIdea = entity.getFieldValue(FIELD_IDEA);

            if (CODE_YES.equals(retievedIdea)) {
                return Boolean.valueOf(value);
            }

            if (m_codes.contains(student.getSection504StatusCode())) {
                value = true;
            }
            return Boolean.valueOf(value);
        }
    }

    /**
     * NJ Retriever to retriever if a student is dual enrolled.
     *
     * @author X2 Development Corporation
     *
     */
    public class NJRetrieverDualEnr implements FieldRetriever {
        public static final String ALIAS_MST_DOE_COURSE_TYPE = "DOE COURSE TYPE";
        public static final String MST_STATE_CODE_DUAL = "C";

        /**
         * Instantiates a new NJ retriever dual enr.
         */
        public NJRetrieverDualEnr() {
            super();

            DataDictionaryField mstCrsTypeField =
                    getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_MST_DOE_COURSE_TYPE);

            if (mstCrsTypeField != null && !StringUtils.isEmpty(mstCrsTypeField.getReferenceTableOid())) {
                Set<String> codes = new HashSet<String>();

                X2Criteria criteria = new X2Criteria();
                criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, mstCrsTypeField.getReferenceTableOid());
                criteria.addEqualTo(ReferenceCode.COL_STATE_CODE, MST_STATE_CODE_DUAL);

                QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, criteria);

                QueryIterator iterator = CRDCStudentData.this.getBroker().getIteratorByQuery(query);
                try {
                    while (iterator.hasNext()) {
                        ReferenceCode record = (ReferenceCode) iterator.next();
                        String code = record.getCode();
                        codes.add(code);
                    }
                } finally {
                    iterator.close();
                }

                X2Criteria sccCriteria = m_studentHelper.getStudentScheduleChangeCriteria();
                sccCriteria.addEqualTo(StudentScheduleChange.REL_MASTER_SCHEDULE + ModelProperty.PATH_DELIMITER +
                        mstCrsTypeField.getJavaName(), codes);

                X2Criteria sscCriteria = m_studentHelper.getStudentScheduleCriteria().copy();
                sscCriteria.addEqualTo(
                        StudentSchedule.REL_SECTION + ModelProperty.PATH_DELIMITER + mstCrsTypeField.getJavaName(),
                        codes);
            }
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            boolean value = false;

            SisStudent student = (SisStudent) entity.getBean();
            Collection<StudentScheduleSpan> schSpans = m_studentHelper.getStudentScheduleSpans(student);

            if (schSpans != null) {
                value = isSpanActiveOnReportDates(schSpans);
            }
            return Boolean.valueOf(value);
        }

        /**
         * Checks if is date in range.
         *
         * @param checkedDate PlainDate
         * @param entryDate PlainDate
         * @param exitDate PlainDate
         * @return true, if is date in range
         */
        private boolean isDateInRange(PlainDate checkedDate, PlainDate entryDate, PlainDate exitDate) {
            return (entryDate == null || !entryDate.after(checkedDate)) &&
                    (exitDate == null || !exitDate.before(checkedDate));
        }

        /**
         * Checks if is span active on report dates.
         *
         * @param schSpans Collection<StudentScheduleSpan>
         * @return true, if is span active on report dates
         */
        private boolean isSpanActiveOnReportDates(Collection<StudentScheduleSpan> schSpans) {
            boolean isSpanActiveOnReportDates = false;

            Iterator<StudentScheduleSpan> schIterator = schSpans.iterator();
            while (isSpanActiveOnReportDates == false && schIterator.hasNext()) {
                StudentScheduleSpan schSpan = schIterator.next();

                PlainDate entryDate = schSpan.getEntryDate();
                PlainDate exitDate = schSpan.getExitDate();

                isSpanActiveOnReportDates = isDateInRange(m_reportDatePart1, entryDate, exitDate);

                if (isSpanActiveOnReportDates == false && m_reportDatePart2 != null) {
                    isSpanActiveOnReportDates = isDateInRange(m_reportDatePart2, entryDate, exitDate);
                }
            }

            return isSpanActiveOnReportDates;
        }
    }

    /**
     * NJ Retriever to determine if a student is in IDEA special education.
     *
     * @author X2 Development Corporation
     *
     */
    public class NJRetrieverIdea implements FieldRetriever {
        public static final String ALIAS_STD_SPED_CLASS = "DOE SPECIAL ED CLASSIFICATION";
        public final Collection<String> SPED_STATE_CODES = Arrays.asList("00", "99");

        Set<String> m_codes;
        Set<String> m_stdOids;

        /**
         * Instantiates a new NJ retriever idea.
         */
        public NJRetrieverIdea() {
            super();

            loadStudentsWithIep();
            loadCodesForIep();
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();
            String stdOid = student.getOid();

            if (m_stdSnapIDEA != null) {
                return Boolean.valueOf(m_stdSnapIDEA.contains(stdOid));
            }

            boolean value = false;

            if (m_stdOids.contains(student.getOid())) {
                value = true;
            }

            String code = null;

            if (!value && !StringUtils.isEmpty(code = (String) student.getFieldValueByAlias(ALIAS_STD_SPED_CLASS))
                    && !m_codes.contains(code)) {
                value = true;
            }
            return Boolean.valueOf(value);
        }

        /**
         * Method to load students with active IEP.
         */
        private void loadStudentsWithIep() {
            m_stdOids = new HashSet<String>();

            X2Criteria iepCriteria = new X2Criteria();

            iepCriteria.addEqualTo(IepData.COL_STATUS_CODE, Integer.valueOf(IepData.StatusCode.ACTIVE.ordinal()));
            iepCriteria.addLessOrEqualThan(IepData.COL_START_DATE, m_reportDatePart1);

            X2Criteria endDateCriteria = new X2Criteria();
            X2Criteria endDateOrCriteria = new X2Criteria();

            endDateCriteria.addIsNull(IepData.COL_END_DATE);
            endDateOrCriteria.addGreaterOrEqualThan(IepData.COL_END_DATE, m_reportDatePart1);

            endDateCriteria.addOrCriteria(endDateOrCriteria);
            iepCriteria.addAndCriteria(endDateCriteria);

            QueryByCriteria iepQuery = new QueryByCriteria(IepData.class, iepCriteria);
            Map<String, Collection<IepData>> iepDataByStdOid = CRDCStudentData.this.getBroker()
                    .getGroupedCollectionByQuery(iepQuery, IepData.COL_STUDENT_OID, 1024);

            if (!iepDataByStdOid.isEmpty()) {
                m_stdOids.addAll(iepDataByStdOid.keySet());
            }
        }

        /**
         * Method to load codes with state codes 00 and 99.
         */
        private void loadCodesForIep() {
            m_codes = new HashSet<String>();

            DataDictionaryField stdSpedClassField =
                    getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_STD_SPED_CLASS);

            if (stdSpedClassField != null && !StringUtils.isEmpty(stdSpedClassField.getReferenceTableOid())) {
                X2Criteria criteria = new X2Criteria();
                criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, stdSpedClassField.getReferenceTableOid());
                criteria.addIn(ReferenceCode.COL_STATE_CODE, SPED_STATE_CODES);

                QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, criteria);

                QueryIterator iterator = CRDCStudentData.this.getBroker().getIteratorByQuery(query);
                try {
                    while (iterator.hasNext()) {
                        ReferenceCode record = (ReferenceCode) iterator.next();
                        String code = record.getCode();
                        m_codes.add(code);
                    }
                } finally {
                    iterator.close();
                }
            }
        }
    }

    /**
     * NJ Retriever to determine if a student is in LEP program.
     *
     * @author X2 Development Corporation
     *
     */
    public class NJRetrievLep implements FieldRetriever {
        public static final String PGM_STATE_CODE_LEP = "LEP";

        protected Map<String, Collection<StudentProgramParticipation>> m_pgmsByStd;

        /**
         * Instantiates a new NJ retriev lep.
         */
        public NJRetrievLep() {
            super();

            Collection<String> codes = new ArrayList<String>();
            DataDictionaryField stdSpedStatusField = getDataDictionary().findDataDictionaryField(
                    StudentProgramParticipation.class.getName(), StudentProgramParticipation.COL_PROGRAM_CODE);

            if (stdSpedStatusField != null && !StringUtils.isEmpty(stdSpedStatusField.getReferenceTableOid())) {
                X2Criteria criteria = new X2Criteria();
                criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, stdSpedStatusField.getReferenceTableOid());
                criteria.addEqualTo(ReferenceCode.COL_STATE_CODE, PGM_STATE_CODE_LEP);

                QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, criteria);

                QueryIterator iterator = CRDCStudentData.this.getBroker().getIteratorByQuery(query);
                try {
                    while (iterator.hasNext()) {
                        ReferenceCode record = (ReferenceCode) iterator.next();
                        String code = record.getCode();
                        codes.add(code);
                    }
                } finally {
                    iterator.close();
                }
            }

            if (!codes.isEmpty()) {
                X2Criteria stdCriteria = CRDCStudentData.this.m_studentHelper.getStudentCriteria();
                SubQuery subQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, stdCriteria);

                X2Criteria pgmCriteria = new X2Criteria();
                pgmCriteria.addIn(StudentProgramParticipation.COL_STUDENT_OID, subQuery);
                pgmCriteria.addIn(StudentProgramParticipation.COL_PROGRAM_CODE, codes);

                pgmCriteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, m_reportDatePart1);

                X2Criteria endDate1Criteria = new X2Criteria();
                endDate1Criteria.addEmpty(StudentProgramParticipation.COL_END_DATE, getBroker().getPersistenceKey());
                X2Criteria endDate2Criteria = new X2Criteria();
                endDate2Criteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_END_DATE, m_reportDatePart1);
                endDate1Criteria.addOrCriteria(endDate2Criteria);
                pgmCriteria.addAndCriteria(endDate1Criteria);

                QueryByCriteria pgmQuery = new QueryByCriteria(StudentProgramParticipation.class, pgmCriteria);

                m_pgmsByStd = getBroker().getGroupedCollectionByQuery(pgmQuery,
                        StudentProgramParticipation.COL_STUDENT_OID, 1024);
            } else {
                m_pgmsByStd = new HashMap<String, Collection<StudentProgramParticipation>>();
            }
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();
            String stdOid = student.getOid();

            if (m_stdSnapLEP != null) {
                return Boolean.valueOf(m_stdSnapLEP.contains(stdOid));
            }

            boolean value = false;

            if (m_pgmsByStd.get(stdOid) != null && !m_pgmsByStd.get(stdOid).isEmpty()) {
                value = true;
            }
            return Boolean.valueOf(value);
        }
    }

    /**
     * NJ Retriever to determine if a student has program with CRDC code (calc param) on report
     * m_reportDatePart1.
     *
     * @author Follett Software Company
     */
    public class NJRetrievLepEnr extends NJRetrievLep {
        public static final String ALIAS_PGM_DOE_REFUSED_SERV_LEP = "DOE PARENT REFUSED SERVICES";

        Map<String, Collection<StudentProgramParticipation>> m_pgmMapbyStdOid;

        /**
         * Instantiates a new NJ retriev lep enr.
         */
        public NJRetrievLepEnr() {
            super();
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.x2dev.procedures.statereporting.CRDCStudentData.NJRetrievLep#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();
            String stdOid = student.getOid();

            if (m_stdSnapLEPPgm != null) {
                return Boolean.valueOf(m_stdSnapLEPPgm.contains(stdOid));
            }

            boolean value = false;

            Collection<StudentProgramParticipation> pgms = m_pgmsByStd.get(stdOid);

            if (pgms != null) {
                value = true;
                for (StudentProgramParticipation pgm : pgms) {
                    if (BooleanAsStringConverter.TRUE
                            .equals(pgm.getFieldValueByAlias(ALIAS_PGM_DOE_REFUSED_SERV_LEP))) {
                        value = false;
                        break;
                    }
                }
            }
            return Boolean.valueOf(value);
        }
    }

    /**
     * NH Retriever to determine if a student is in 504 Section.
     *
     * @author X2 Development Corporation
     *
     */
    public class NHRetriever504 extends RetrieverByStdAliasAndCode {
        public static final String ALIAS_STD_504 = "504";
        public static final String CRDC_CODE_504 = "504";

        /**
         * Instantiates a new NH retriever 504.
         */
        public NHRetriever504() {
            super(ALIAS_STD_504, CRDC_CODE_504);
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.x2dev.procedures.statereporting.CRDCStudentData.RetrieverByStdAliasAndCode#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();
            String stdOid = student.getOid();

            if (m_stdSnap504 != null) {
                return Boolean.valueOf(m_stdSnap504.contains(stdOid));
            }

            boolean value = false;

            if (m_stdFieldBeanPath != null
                    && m_codes.contains(student.getFieldValueByBeanPath(m_stdFieldBeanPath))
                    && (student.getSection504LastEndDate() == null
                            || m_reportDatePart1.before(student.getSection504LastEndDate()))) {
                value = true;
            }

            return Boolean.valueOf(value);
        }
    }

    /**
     * Retrieve NH RecvHSEquivalency field.
     */
    public class NHRetrieverGED implements FieldRetriever {
        public static final String ALIAS_DIPLOMA_TYPE = "i4see 620";

        private String m_fieldDiplomaType;
        private final List<String> m_stateDiplomaCodes =
                Collections.unmodifiableList(new ArrayList<String>(Arrays.asList("4")));
        private Set<String> m_diplomaCodes = new HashSet();

        /**
         * Instantiates a new NH retriever GED.
         */
        public NHRetrieverGED() {
            m_fieldDiplomaType = translateAliasToJavaName(ALIAS_DIPLOMA_TYPE, true);
            DataDictionaryField teachMethodField =
                    getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_DIPLOMA_TYPE);

            if (teachMethodField != null) {
                ReferenceTable refTable = teachMethodField.getReferenceTable();
                if (refTable != null) {
                    Collection<ReferenceCode> refCodes = refTable.getReferenceCodes();

                    if (refCodes != null) {
                        for (ReferenceCode refCode : refCodes) {
                            if (m_stateDiplomaCodes.contains(refCode.getStateCode())) {
                                m_diplomaCodes.add(refCode.getCode());
                            }
                        }
                    }
                }
            }
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            return m_diplomaCodes.contains(data.getPropertyAsJavaType(entity.getBean(), m_fieldDiplomaType))
                    ? Boolean.TRUE
                    : Boolean.FALSE;
        }
    }

    /**
     * NH retriever to identify IDEA students.
     */
    public class NHRetrieverIdea extends RetrieverByStdAliasAndCode {
        public static final String ALIAS_IDEA = "IDEA";
        public static final String CRDC_CODE_IDEA = "IDEA";

        /**
         * Instantiates a new NH retriever idea.
         */
        public NHRetrieverIdea() {
            super(ALIAS_IDEA, CRDC_CODE_IDEA);
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.x2dev.procedures.statereporting.CRDCStudentData.RetrieverByStdAliasAndCode#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();
            String stdOid = student.getOid();

            if (m_stdSnapIDEA != null) {
                return Boolean.valueOf(m_stdSnapIDEA.contains(stdOid));
            }

            return super.getFieldValue(data, entity, field);
        }

    }

    /**
     * Retrieve NH LEP field.
     */
    public class NHRetrieverLEP implements FieldRetriever {
        public static final String ALIAS_ESL_FLAG = "ESL";
        private static final String EXPECTED_STATUS_STRING = "Active";

        private String m_fieldEslFlag;

        /**
         * Instantiates a new NH retriever LEP.
         */
        public NHRetrieverLEP() {
            m_fieldEslFlag = translateAliasToJavaName(ALIAS_ESL_FLAG, true);
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();
            String stdOid = student.getOid();

            if (m_stdSnapLEP != null) {
                return Boolean.valueOf(m_stdSnapLEP.contains(stdOid));
            }

            boolean value = false;
            Object fieldValue = data.getPropertyAsJavaType(entity.getBean(), m_fieldEslFlag);
            if (fieldValue instanceof String) {
                String stateValue =
                        data.lookupStateValue(entity.getBean().getClass(), m_fieldEslFlag, (String) fieldValue);
                if (EXPECTED_STATUS_STRING.equals((fieldValue)) || EXPECTED_STATUS_STRING.equals((stateValue))) {
                    value = true;
                }
            } else if (fieldValue instanceof Boolean) {
                value = ((Boolean) fieldValue).booleanValue();
            }
            return value ? Boolean.TRUE : Boolean.FALSE;
        }
    }

    /**
     * Retrieve NH LEP field.
     */
    public class NHRetrieverLEPEnroll extends RetrieverPgmRepDate {

        /**
         * Instantiates a new NH retriever LEP enroll.
         */
        public NHRetrieverLEPEnroll() {
            super();
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.x2dev.procedures.statereporting.CRDCStudentData.RetrieverPgmRepDate#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();
            String stdOid = student.getOid();

            if (m_stdSnapLEPPgm != null) {
                return Boolean.valueOf(m_stdSnapLEPPgm.contains(stdOid));
            }

            return super.getFieldValue(data, entity, field);
        }
    }

    /**
     * NY Retriever to determine if a student participate in Credit Recovery Program.
     *
     * @author X2 Development Corporation
     *
     */
    public class NYRetrCredRec implements FieldRetriever {
        public static final String ALIAS_TRN_CRED_REC = "DOE CREDIT RECOVERY";

        Map<String, Collection<Transcript>> m_trnMapbyStdOid;

        /**
         * Instantiates a new NY retr cred rec.
         */
        public NYRetrCredRec() {
            super();

            DataDictionaryField trnCreditRecovery =
                    getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_TRN_CRED_REC);

            if (trnCreditRecovery != null) {
                X2Criteria trnCriteria = m_studentHelper.getStudentTranscriptCriteria().copy();
                trnCriteria.addEqualTo(trnCreditRecovery.getJavaName(), BooleanAsStringConverter.TRUE);
                trnCriteria.addEqualTo(Transcript.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());

                m_trnMapbyStdOid = CRDCStudentData.this.getBroker().getGroupedCollectionByQuery(
                        new QueryByCriteria(Transcript.class, trnCriteria), Transcript.COL_STUDENT_OID, 1024);
            } else {
                m_trnMapbyStdOid = new HashMap();
            }
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            boolean value = false;

            SisStudent student = (SisStudent) entity.getBean();

            if (m_trnMapbyStdOid.get(student.getOid()) != null && !m_trnMapbyStdOid.get(student.getOid()).isEmpty()) {
                value = true;
            }

            return Boolean.valueOf(value);
        }
    }

    /**
     * The Class NYRetrieverHSEqiv.
     */
    public class NYRetrieverHSEqiv implements FieldRetriever {
        public static final String ALIAS_DIPLOMA_DATE = "all-std-DiplomaDate";
        public static final String ALIAS_DIPLOMA_TYPE = "DOE DIPLOMA TYPE";

        private Set<String> m_diplomaCodes = new HashSet();
        private String m_fieldDiplomaDate;
        private String m_fieldDiplomaType;

        /**
         * Instantiates a new NY retriever HS eqiv.
         */
        public NYRetrieverHSEqiv() {
            super();
            m_fieldDiplomaDate = translateAliasToJavaName(ALIAS_DIPLOMA_DATE, true);
            m_fieldDiplomaType = translateAliasToJavaName(ALIAS_DIPLOMA_TYPE, true);
            if (!StringUtils.isEmpty(m_fieldDiplomaType)) {
                m_diplomaCodes = getCodesForCRDCValue(SisStudent.class, m_fieldDiplomaType, Arrays.asList("GED"));
            }
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return String
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public String getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String value = "N";
            if (!StringUtils.isEmpty(m_fieldDiplomaDate) && !StringUtils.isEmpty(m_fieldDiplomaType) &&
                    m_diplomaCodes.contains(entity.getBean().getFieldValueByBeanPath(m_fieldDiplomaType))) {
                PlainDate date = (PlainDate) data.getPropertyAsJavaType(entity.getBean(), m_fieldDiplomaDate);
                if (date != null && !data.getCurrentContext().getStartDate().after(date) &&
                        !data.getCurrentContext().getEndDate().before(date)) {
                    value = "Y";
                }
            }
            return value;
        }

    }

    /**
     * PA Retriever to determine if a student is in 504 Section.
     *
     * @author X2 Development Corporation
     *
     */
    public class PARetriever504 implements FieldRetriever {
        public static final String ALIAS_STD_504 = "504 Plan";

        /**
         * Instantiates a new PA retriever 504.
         */
        public PARetriever504() {
            super();
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();
            String stdOid = student.getOid();

            if (m_stdSnap504 != null) {
                return Boolean.valueOf(m_stdSnap504.contains(stdOid));
            }

            boolean value = false;

            String retievedIdea = entity.getFieldValue(FIELD_IDEA);

            if (CODE_YES.equals(retievedIdea)) {
                return Boolean.valueOf(value);
            }

            if (CODE_YES.equals(student.getFieldValueByAlias(ALIAS_STD_504))
                    && (student.getSection504LastEndDate() == null
                            || m_reportDatePart1.before(student.getSection504LastEndDate()))) {
                value = true;
            }

            return Boolean.valueOf(value);
        }
    }

    /**
     * PA Retriever if a student is enrolled in distance education.
     *
     * @author X2 Development Corporation
     *
     */
    public class PARetrieverDistEd extends RetrieverStdCrs {
        public static final String ALIAS_DOE_CRS_DELIVERY = "DOE COURSE DELIVERY";
        public static final String CRDC_CODE_DISTANCE = "Distance";

        /**
         * Apply criteria.
         *
         * @see com.x2dev.procedures.statereporting.CRDCStudentData.RetrieverStdCrs#applyCriteria()
         */
        @Override
        void applyCriteria() {
            DataDictionaryField teachMethodField =
                    getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_DOE_CRS_DELIVERY);
            Collection<String> crsCodeForTeachMethod = new ArrayList();

            if (teachMethodField != null) {
                ReferenceTable refTable = teachMethodField.getReferenceTable();
                if (refTable != null) {
                    Collection<ReferenceCode> refCodes = refTable.getReferenceCodes();

                    if (refCodes != null) {
                        for (ReferenceCode refCode : refCodes) {
                            String crdcCode = (String) refCode.getFieldValueByAlias(ALIAS_CRDC_REF_CODE);

                            if (CRDC_CODE_DISTANCE.equalsIgnoreCase(crdcCode)) {
                                crsCodeForTeachMethod.add(refCode.getCode());
                            }
                        }
                    }
                }

                if (!crsCodeForTeachMethod.isEmpty()) {
                    this.addInCourseField(teachMethodField.getJavaName(), crsCodeForTeachMethod);
                }
            }
        }
    }

    public class PARetrieverGifted implements FieldRetriever {
        private static final String ALIAS_GIFTED_CODE = "DOE GIFTED STATUS";
        private static final String GIFTED_CODE = "Y";
        private static final String PGM_CRDC_GIFTED_CODE = "GT";

        private String m_fieldGiftedStatusCode;
        private Set<String> m_giftedStudents = new HashSet();

        public PARetrieverGifted() {
            super();

            m_fieldGiftedStatusCode = translateAliasToJavaName(ALIAS_GIFTED_CODE, true);

            if (m_fieldGiftedStatusCode != null) {
                X2Criteria stdCriteria = CRDCStudentData.this.m_studentHelper.getStudentCriteria();
                SubQuery subQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, stdCriteria);

                X2Criteria pgmCriteria = new X2Criteria();
                pgmCriteria.addIn(StudentProgramParticipation.COL_STUDENT_OID, subQuery);
                pgmCriteria.addIn(StudentProgramParticipation.COL_PROGRAM_CODE,
                        CRDCStudentData.this.getCodesForCRDCValue(StudentProgramParticipation.class,
                                StudentProgramParticipation.COL_PROGRAM_CODE, Arrays.asList(PGM_CRDC_GIFTED_CODE)));
                pgmCriteria.addIn(m_fieldGiftedStatusCode,
                        CRDCStudentData.this.getCodesForCRDCValue(StudentProgramParticipation.class,
                                m_fieldGiftedStatusCode, Arrays.asList(GIFTED_CODE)));

                pgmCriteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, m_reportDatePart1);

                X2Criteria endDate1Criteria = new X2Criteria();
                endDate1Criteria.addEmpty(StudentProgramParticipation.COL_END_DATE, getBroker().getPersistenceKey());
                X2Criteria endDate2Criteria = new X2Criteria();
                endDate2Criteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_END_DATE, m_reportDatePart1);
                endDate1Criteria.addOrCriteria(endDate2Criteria);
                pgmCriteria.addAndCriteria(endDate1Criteria);

                String[] columns = new String[] {StudentProgramParticipation.COL_STUDENT_OID};
                ColumnQuery query = new ColumnQuery(StudentProgramParticipation.class, columns, pgmCriteria);
                try (QueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query)) {
                    while (iterator.hasNext()) {
                        Object[] row = (Object[]) iterator.next();
                        m_giftedStudents.add((String) row[0]);
                    }
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
            return Boolean.valueOf(m_giftedStudents.contains(entity.getBean().getOid()));
        }

    }
    /**
     * PA Retriever to determine if a student is in IDEA special education.
     *
     * @author X2 Development Corporation
     *
     */
    public class PARetrieverIdea implements FieldRetriever {
        public static final String PGM_CRDC_CODE_IEP = "IEP";

        Set<String> m_codes;
        Set<String> m_stdOids;

        /**
         * Instantiates a new PA retriever idea.
         */
        public PARetrieverIdea() {
            super();

            m_stdOids = new HashSet<String>();
            m_codes = new HashSet<String>();
            m_codes.addAll(CRDCStudentData.this.getCodesForCRDCValue(StudentProgramParticipation.class,
                    StudentProgramParticipation.COL_PROGRAM_CODE, Arrays.asList(PGM_CRDC_CODE_IEP)));

            if (m_codes.size() > 1) {
                SubQuery studentSubquery =
                        new SubQuery(SisStudent.class, X2BaseBean.COL_OID, m_studentHelper.getStudentCriteria());

                X2Criteria criteria = new X2Criteria();
                criteria.addIn(StudentProgramParticipation.COL_STUDENT_OID, studentSubquery);
                criteria.addIn(StudentProgramParticipation.COL_PROGRAM_CODE, m_codes);
                criteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, m_reportDatePart1);

                X2Criteria endDate1Criteria = new X2Criteria();
                endDate1Criteria.addEmpty(StudentProgramParticipation.COL_END_DATE, getBroker().getPersistenceKey());
                X2Criteria endDate2Criteria = new X2Criteria();
                endDate2Criteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_END_DATE, m_reportDatePart1);
                endDate1Criteria.addOrCriteria(endDate2Criteria);
                criteria.addAndCriteria(endDate1Criteria);

                String[] columns = new String[] {StudentProgramParticipation.COL_STUDENT_OID};
                ColumnQuery query = new ColumnQuery(StudentProgramParticipation.class, columns, criteria);
                try (QueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query)) {
                    while (iterator.hasNext()) {
                        Object[] row = (Object[]) iterator.next();
                        m_stdOids.add((String) row[0]);
                    }
                }
            } else {
                addSetupError("No reportable program codes", "Student Programs, reference table, state code: IEP");
            }
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();
            String stdOid = student.getOid();

            if (m_stdSnapIDEA != null) {
                return Boolean.valueOf(m_stdSnapIDEA.contains(stdOid));
            }

            boolean value = false;

            if (m_stdOids.contains(student.getOid())) {
                value = true;
            }
            return Boolean.valueOf(value);
        }
    }

    /**
     * Retrieves Count of discipline incidents for student (with OR without a discipline action)
     * where CRDC Code = Harassment Bullying Sex
     * OR Harassment Bullying Race, Color or Nat'l origin
     * OR Harassment Bullying Disability
     *
     * @author X2 Development Corporation
     *
     */
    public class RetrieverAllegCnt implements FieldRetriever {
        /**
         * Calc-parameters map for 'HarassBullySexRace' and 'HarassBullySexRelig' fields
         */
        private Map<String, String> CALC_PARAM_VALUES = new HashMap() {
            {
                put("HBS", "Harassment Bullying Sex");
                put("HBRCNAT", "Harassment Bullying Race, Color or Nat'l origin");
                put("HBD", "Harassment Bullying Disability");
                put("HBSO", "Harassment Bullying Sexual Orientation");
                put("HBR", "Harassment Bullying Religion");

                put("HBRA", "Harassment Bullying Religion Atheist/Agnostic");
                put("HBRB", "Harassment Bullying Religion Buddhist");
                put("HBRC", "Harassment Bullying Religion Catholic");
                put("HBREO", "Harassment Bullying Religion Eastern Orthodox");
                put("HBRH", "Harassment Bullying Religion Hindu");
                put("HBRI", "Harassment Bullying Religion Islamic");
                put("HBRJW", "Harassment Bullying Religion Jehovah's Witness");
                put("HBRJ", "Harassment Bullying Religion Jewish");
                put("HBRM", "Harassment Bullying Religion Mormon");
                put("HBRMR", "Harassment Bullying Multiple Religions, groups");
                put("HBROC", "Harassment Bullying Religion Other Christian");
                put("HBROR", "Harassment Bullying Other Religion");
                put("HBRP", "Harassment Bullying Religion Protestant");
                put("HBRS", "Harassment Bullying Religion Sikh");
            }
        };

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            int count = 0;
            String[] calcParamKeys = ((String) field.getParameter()).split(",");

            for (String calcParam : calcParamKeys) {
                Set<ConductIncident> incidents =
                        CRDCStudentData.this.m_conductHelper.getIncidentsForStudent(entity.getBean().getOid(),
                                CALC_PARAM_VALUES.get(calcParam));
                if (incidents != null) {
                    count += incidents.size();
                }
            }
            return Integer.valueOf(count);
        }
    }


    /**
     * Retriever to calculate value by alias.
     *
     * @author X2 Development Corporation
     *
     */
    public abstract class RetrieverByAlias implements FieldRetriever {
        Set<String> m_usedAliases = new HashSet<String>();

        /**
         * Instantiates a new retriever by alias.
         */
        public RetrieverByAlias() {
            String className =
                    this.getClass().getName().replaceAll(this.getClass().getEnclosingClass().getName() + ".{1}", "");

            int numOfFields = CRDCStudentData.this.getFieldCount();
            for (int i = 0; i < numOfFields; i++) {
                FieldDefinition currentField = CRDCStudentData.this.getFieldDefinition(i);

                if (currentField.getCalcId() != null && currentField.getCalcId().equals(className)) {
                    String paramAlias = (String) currentField.getParameter();
                    if (paramAlias != null) {
                        m_usedAliases.add(paramAlias);
                    }
                }
            }

            for (String alias : m_usedAliases) {
                CRDCStudentData.this.translateAliasToJavaName(alias, true);
            }
        }
    }

    /**
     * VA Retriever to determine if a student is in LEP.
     *
     * @author X2 Development Corporation
     *
     */
    public class RetrieverByStdAliasAndCode implements FieldRetriever {
        Set<String> m_codes;
        String m_stdFieldBeanPath;

        /**
         * Instantiates a new retriever by std alias and code.
         */
        public RetrieverByStdAliasAndCode() {
            super();
        }

        /**
         * Instantiates a new retriever by std alias and code.
         *
         * @param alias String
         * @param crdcCode String
         */
        public RetrieverByStdAliasAndCode(String alias, String crdcCode) {
            DataDictionaryField aliasField = getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_CRDC_REF_CODE);
            DataDictionaryField stdField = getDataDictionary().findDataDictionaryFieldByAlias(alias);
            m_codes = new HashSet<String>();

            if (aliasField != null && stdField != null && !StringUtils.isEmpty(stdField.getReferenceTableOid())) {
                m_stdFieldBeanPath = stdField.getJavaName();

                X2Criteria criteria = new X2Criteria();
                criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, stdField.getReferenceTableOid());

                QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, criteria);

                QueryIterator iterator = CRDCStudentData.this.getBroker().getIteratorByQuery(query);
                try {
                    while (iterator.hasNext()) {
                        ReferenceCode record = (ReferenceCode) iterator.next();
                        String crdcCodes = (String) record.getFieldValueByBeanPath(aliasField.getJavaName());
                        if (!StringUtils.isEmpty(crdcCodes)
                                && Arrays.asList(crdcCodes.split("\\s*,\\s*")).contains(crdcCode)) {
                            String code = record.getCode();
                            m_codes.add(code);
                        }
                    }
                } finally {
                    iterator.close();
                }
            }
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            boolean value = false;

            SisStudent student = (SisStudent) entity.getBean();

            if (m_stdFieldBeanPath != null && m_codes.contains(student.getFieldValueByBeanPath(m_stdFieldBeanPath))) {
                value = true;
            }

            return Boolean.valueOf(value);
        }
    }

    /**
     * Retriever to determine if a student is Out of District Placement.
     *
     * @author X2 Development Corporation
     *
     */
    public class RetrieverOutOfDistr implements FieldRetriever {

        /**
         * Instantiates a new retriever out of distr.
         */
        public RetrieverOutOfDistr() {
            super();
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            boolean value = false;

            SisStudent student = (SisStudent) entity.getBean();
            String stdOid = student.getOid();

            if (m_stdSnapOOD != null) {
                return Boolean.valueOf(m_stdSnapOOD.contains(stdOid));
            }

            return Boolean.valueOf(value);
        }
    }

    /**
     * Retrieves if student has program with CRDC code (calc param) on report m_reportDatePart1.
     *
     * @author Follett Software Company
     */
    public class RetrieverPgmRepDate implements FieldRetriever {
        private static final String PARAM_CODE_504 = "504";

        /**
         * Instantiates a new retriever pgm rep date.
         */
        public RetrieverPgmRepDate() {
            loadStdCrdcPgmCodesMap(CRDCStudentData.this);
        }

        protected QueryByCriteria m_pgmQuery;
        protected Map<String, Set<String>> m_studentsCrdcPgmCodesMap = null;

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();
            String stdOid = student.getOid();
            String paramCrdcPgmCode = (String) field.getParameter();

            if (m_stdSnapIDEA != null && PARAM_CODE_IDEA.equals(paramCrdcPgmCode)) {
                return Boolean.valueOf(m_stdSnapIDEA.contains(stdOid));
            }

            if (m_stdSnap504 != null && PARAM_CODE_504.equals(paramCrdcPgmCode)) {
                return Boolean.valueOf(m_stdSnap504.contains(stdOid));
            }

            if (m_stdSnapLEP != null && PARAM_CODE_LEP.equals(paramCrdcPgmCode)) {
                return Boolean.valueOf(m_stdSnapLEP.contains(stdOid));
            }

            if (m_stdSnapLEPPgm != null && PARAM_LEP_ENROLLED.equals(paramCrdcPgmCode)) {
                return Boolean.valueOf(m_stdSnapLEPPgm.contains(stdOid));
            }

            boolean value = false;

            Set<String> stdCrdcPgmCodes = m_studentsCrdcPgmCodesMap.get(stdOid);


            if (stdCrdcPgmCodes != null && stdCrdcPgmCodes.contains(paramCrdcPgmCode)) {
                value = true;
            }

            return Boolean.valueOf(value);
        }

        /**
         * Load std crdc pgm codes map.
         *
         * @param crdcStdData CRDCStudentData
         */
        private void loadStdCrdcPgmCodesMap(CRDCStudentData crdcStdData) {
            m_studentsCrdcPgmCodesMap = new HashMap<String, Set<String>>();

            String className =
                    this.getClass().getName().replaceAll(this.getClass().getEnclosingClass().getName() + ".{1}", "");

            Set<String> usedProgramsCrdcCodes = new HashSet<String>();
            int numOfFields = crdcStdData.getFieldCount();
            for (int i = 0; i < numOfFields; i++) {
                FieldDefinition currentField = crdcStdData.getFieldDefinition(i);

                if (currentField.getCalcId() != null && currentField.getCalcId().equals(className)) {
                    String paramCrdcPgmCode = (String) currentField.getParameter();
                    if (paramCrdcPgmCode != null) {
                        usedProgramsCrdcCodes.add(paramCrdcPgmCode);
                    }
                }
            }

            DataDictionaryField pgmCodeField = crdcStdData.getDataDictionaryField(StudentProgramParticipation.class,
                    StudentProgramParticipation.COL_PROGRAM_CODE);
            ReferenceTable refTable = pgmCodeField.getReferenceTable();
            Collection<ReferenceCode> refCodes = refTable.getReferenceCodes();

            // Allow for comma separated CRDC Codes per reference code
            Map<String, Set<String>> usedPgmCodeCrdcCodeMap = new HashMap();

            for (ReferenceCode refCode : refCodes) {
                String code = refCode.getCode();
                String crdcCodes = (String) refCode.getFieldValueByAlias(ALIAS_CRDC_REF_CODE);
                if (!StringUtils.isEmpty(crdcCodes)) {
                    Set<String> codeSet = new HashSet();
                    for (String crdcCode : Arrays.asList(crdcCodes.split("\\s*,\\s*"))) {
                        if (usedProgramsCrdcCodes.contains(crdcCode)) {
                            codeSet.add(crdcCode);
                        }
                    }
                    usedPgmCodeCrdcCodeMap.put(code, codeSet);
                }
            }

            if (!usedProgramsCrdcCodes.isEmpty()) {
                X2Criteria stdCriteria = crdcStdData.m_studentHelper.getStudentCriteria();
                SubQuery subQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, stdCriteria);

                X2Criteria pgmCriteria = new X2Criteria();
                pgmCriteria.addIn(StudentProgramParticipation.COL_STUDENT_OID, subQuery);
                pgmCriteria.addIn(StudentProgramParticipation.COL_PROGRAM_CODE, usedPgmCodeCrdcCodeMap.keySet());

                pgmCriteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, m_reportDatePart1);

                X2Criteria endDate1Criteria = new X2Criteria();
                endDate1Criteria.addEmpty(StudentProgramParticipation.COL_END_DATE, getBroker().getPersistenceKey());
                X2Criteria endDate2Criteria = new X2Criteria();
                endDate2Criteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_END_DATE, m_reportDatePart1);
                endDate1Criteria.addOrCriteria(endDate2Criteria);
                pgmCriteria.addAndCriteria(endDate1Criteria);

                m_pgmQuery = new QueryByCriteria(StudentProgramParticipation.class, pgmCriteria);

                Collection<StudentProgramParticipation> programs = getBroker().getCollectionByQuery(m_pgmQuery);

                for (StudentProgramParticipation program : programs) {
                    String studentOid = program.getStudentOid();

                    Set<String> stdPrograms = m_studentsCrdcPgmCodesMap.get(studentOid);

                    if (stdPrograms == null) {
                        stdPrograms = new HashSet<String>();
                        m_studentsCrdcPgmCodesMap.put(studentOid, stdPrograms);
                    }

                    String programCode = program.getProgramCode();
                    Set<String> crdcProgramCodes = usedPgmCodeCrdcCodeMap.get(programCode);

                    if (crdcProgramCodes != null && !crdcProgramCodes.isEmpty()) {
                        for (String crdcProgramCode : crdcProgramCodes) {
                            stdPrograms.add(crdcProgramCode);
                        }
                    }
                }
            }
        }
    }

    /**
     * Retrieves if student has program with CRDC code (calc param) on report m_reportDatePart1
     * where start date and end date aren't equal.
     *
     * @author Follett Software Company
     */
    public class RetrieverPgmRepDtNtE implements FieldRetriever {

        /**
         * Instantiates a new retriever pgm rep dt nt E.
         */
        public RetrieverPgmRepDtNtE() {
            loadStdCrdcPgmCodesMap(CRDCStudentData.this);
        }

        private Map<String, Set<String>> m_studentsCrdcPgmCodesMap = null;

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();
            String stdOid = student.getOid();

            if (m_stdSnapLEPPgm != null) {
                return Boolean.valueOf(m_stdSnapLEPPgm.contains(stdOid));
            }

            boolean value = false;

            Set<String> stdCrdcPgmCodes = m_studentsCrdcPgmCodesMap.get(stdOid);
            String paramCrdcPgmCode = (String) field.getParameter();

            if (stdCrdcPgmCodes != null && stdCrdcPgmCodes.contains(paramCrdcPgmCode)) {
                value = true;
            }

            return Boolean.valueOf(value);
        }

        /**
         * Load std crdc pgm codes map.
         *
         * @param crdcStdData CRDCStudentData
         */
        private void loadStdCrdcPgmCodesMap(CRDCStudentData crdcStdData) {
            m_studentsCrdcPgmCodesMap = new HashMap<String, Set<String>>();

            String className =
                    this.getClass().getName().replaceAll(this.getClass().getEnclosingClass().getName() + ".{1}", "");

            Set<String> usedProgramsCrdcCodes = new HashSet<String>();
            int numOfFields = crdcStdData.getFieldCount();
            for (int i = 0; i < numOfFields; i++) {
                FieldDefinition currentField = crdcStdData.getFieldDefinition(i);

                if (currentField.getCalcId() != null && currentField.getCalcId().equals(className)) {
                    String paramCrdcPgmCode = (String) currentField.getParameter();
                    if (paramCrdcPgmCode != null) {
                        usedProgramsCrdcCodes.add(paramCrdcPgmCode);
                    }
                }
            }

            DataDictionaryField pgmCodeField = crdcStdData.getDataDictionaryField(StudentProgramParticipation.class,
                    StudentProgramParticipation.COL_PROGRAM_CODE);
            ReferenceTable refTable = pgmCodeField.getReferenceTable();
            Collection<ReferenceCode> refCodes = refTable.getReferenceCodes();

            // Allow for comma separated CRDC Codes per reference code
            Map<String, Set<String>> usedPgmCodeCrdcCodeMap = new HashMap();

            for (ReferenceCode refCode : refCodes) {
                String code = refCode.getCode();
                String crdcCodes = (String) refCode.getFieldValueByAlias(ALIAS_CRDC_REF_CODE);
                if (!StringUtils.isEmpty(crdcCodes)) {
                    Set<String> codeSet = new HashSet();
                    for (String crdcCode : Arrays.asList(crdcCodes.split("\\s*,\\s*"))) {
                        if (usedProgramsCrdcCodes.contains(crdcCode)) {
                            codeSet.add(crdcCode);
                        }
                    }
                    usedPgmCodeCrdcCodeMap.put(code, codeSet);
                }
            }

            if (!usedProgramsCrdcCodes.isEmpty()) {
                X2Criteria stdCriteria = crdcStdData.m_studentHelper.getStudentCriteria();
                SubQuery subQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, stdCriteria);

                X2Criteria pgmCriteria = new X2Criteria();
                pgmCriteria.addIn(StudentProgramParticipation.COL_STUDENT_OID, subQuery);
                pgmCriteria.addIn(StudentProgramParticipation.COL_PROGRAM_CODE, usedPgmCodeCrdcCodeMap.keySet());

                pgmCriteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, m_reportDatePart1);

                X2Criteria endDate1Criteria = new X2Criteria();
                endDate1Criteria.addEmpty(StudentProgramParticipation.COL_END_DATE, getBroker().getPersistenceKey());
                X2Criteria endDate2Criteria = new X2Criteria();
                endDate2Criteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_END_DATE, m_reportDatePart1);
                endDate1Criteria.addOrCriteria(endDate2Criteria);
                pgmCriteria.addAndCriteria(endDate1Criteria);

                X2Criteria equalDate1Criteria = new X2Criteria();
                equalDate1Criteria.addEmpty(StudentProgramParticipation.COL_END_DATE, getBroker().getPersistenceKey());
                X2Criteria equalDate2Criteria = new X2Criteria();
                equalDate2Criteria.addNotEqualToField(StudentProgramParticipation.COL_START_DATE,
                        StudentProgramParticipation.COL_END_DATE);
                equalDate1Criteria.addOrCriteria(equalDate2Criteria);
                pgmCriteria.addAndCriteria(equalDate1Criteria);

                QueryByCriteria pgmQuery = new QueryByCriteria(StudentProgramParticipation.class, pgmCriteria);

                Collection<StudentProgramParticipation> programs = getBroker().getCollectionByQuery(pgmQuery);

                for (StudentProgramParticipation program : programs) {
                    String studentOid = program.getStudentOid();

                    Set<String> stdPrograms = m_studentsCrdcPgmCodesMap.get(studentOid);

                    if (stdPrograms == null) {
                        stdPrograms = new HashSet<String>();
                        m_studentsCrdcPgmCodesMap.put(studentOid, stdPrograms);
                    }

                    String programCode = program.getProgramCode();
                    Set<String> crdcProgramCodes = usedPgmCodeCrdcCodeMap.get(programCode);

                    if (crdcProgramCodes != null && !crdcProgramCodes.isEmpty()) {
                        for (String crdcProgramCode : crdcProgramCodes) {
                            stdPrograms.add(crdcProgramCode);
                        }
                    }
                }
            }
        }
    }

    /**
     * Retrieves student's race.
     *
     * @author Follett Software Company
     */
    public class RetrieverRace implements FieldRetriever {
        public static final String MULTY_RACES = "Two or More Races";

        protected Map<String, Collection<Race>> m_studentRaces;

        /**
         * Instantiates a new retriever race.
         */
        public RetrieverRace() {
            super();

            if (m_studentRaces == null) {

                SubQuery studentSubQuery =
                        new SubQuery(SisStudent.class, SisStudent.COL_PERSON_OID, m_studentHelper.getStudentCriteria());

                X2Criteria racesCriteria = new X2Criteria();
                racesCriteria.addIn(Race.REL_PERSON + ModelProperty.PATH_DELIMITER + X2BaseBean.COL_OID,
                        studentSubQuery);
                QueryByCriteria racesQuery = new QueryByCriteria(Race.class, racesCriteria);

                m_studentRaces = getBroker().getGroupedCollectionByQuery(racesQuery, Race.COL_PERSON_OID, 200);
            }
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String value = null;

            CRDCStudentData stdCRDCData = (CRDCStudentData) data;
            SisStudent student = (SisStudent) entity.getBean();

            Set<String> crdcRaces = new HashSet();
            Collection<Race> races = m_studentRaces.get(student.getPersonOid());
            if (races != null) {
                for (Race race : races) {
                    String crdcCode =
                            stdCRDCData.lookupCRDCCodeByBeanPath(Race.class, Race.COL_RACE_CODE, race.getRaceCode());
                    if (!StringUtils.isEmpty(crdcCode)) {
                        crdcRaces.add(crdcCode);
                    }
                }
            }

            if (!crdcRaces.isEmpty()) {
                if (crdcRaces.size() > 1) {
                    value = MULTY_RACES;
                } else {
                    value = crdcRaces.iterator().next();
                }
            }
            return value;
        }
    }

    /**
     * Retrieves if a student is in Section 504 Program.
     *
     * @author Follett Software Company
     */
    public class RetrieverSepRepDate implements FieldRetriever {

        /**
         * Instantiates a new retriever sep rep date.
         */
        public RetrieverSepRepDate() {
            loadStdCrdcSepCodesMap(CRDCStudentData.this);
        }

        private Map<String, Map<String, Collection<StudentEdPlan>>> m_studentsPlansMap = null;

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();
            String stdOid = student.getOid();

            if (m_stdSnap504 != null) {
                return Boolean.valueOf(m_stdSnap504.contains(stdOid));
            }

            boolean value = false;

            String retievedIdea = entity.getFieldValue(FIELD_IDEA);

            if (CODE_YES.equals(retievedIdea)) {
                return Boolean.valueOf(value);
            }

            String ddxName = (String) field.getParameter();
            if (StringUtils.isEmpty(ddxName)) {
                for (Map<String, Collection<StudentEdPlan>> plansByStudent : m_studentsPlansMap.values()) {
                    Collection<StudentEdPlan> edPlans = plansByStudent.get(stdOid);

                    if (edPlans != null && edPlans.size() > 0) {
                        value = true;
                    }
                }
            } else {
                Map<String, Collection<StudentEdPlan>> plansByStudent = m_studentsPlansMap.get(ddxName);
                if (plansByStudent != null) {
                    Collection<StudentEdPlan> edPlans = plansByStudent.get(stdOid);

                    if (edPlans != null && edPlans.size() > 0) {
                        value = true;
                    }
                }
            }
            return Boolean.valueOf(value);
        }

        /**
         * Load std crdc sep codes map.
         *
         * @param crdcStdData CRDCStudentData
         */
        private void loadStdCrdcSepCodesMap(CRDCStudentData crdcStdData) {
            X2Criteria stdCriteria = crdcStdData.m_studentHelper.getStudentCriteria();
            SubQuery subQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, stdCriteria);

            X2Criteria sepCriteria = new X2Criteria();
            sepCriteria.addIn(StudentEdPlan.COL_STUDENT_OID, subQuery);

            sepCriteria.addLessOrEqualThan(StudentEdPlan.COL_EFFECTIVE_DATE, m_reportDatePart1);

            X2Criteria endDate1Criteria = new X2Criteria();
            endDate1Criteria.addEmpty(StudentEdPlan.COL_END_DATE, getBroker().getPersistenceKey());
            endDate1Criteria.addEqualTo(StudentEdPlan.COL_STATUS_CODE,
                    Integer.valueOf(StudentEdPlan.StatusCode.ACTIVE.ordinal()));

            X2Criteria endDate2Criteria = new X2Criteria();
            endDate2Criteria.addGreaterOrEqualThan(StudentEdPlan.COL_END_DATE, m_reportDatePart1);
            endDate1Criteria.addOrCriteria(endDate2Criteria);
            sepCriteria.addAndCriteria(endDate1Criteria);

            QueryByCriteria sepQuery = new QueryByCriteria(StudentEdPlan.class, sepCriteria);

            m_studentsPlansMap = getBroker().getGroupedCollectionByQuery(sepQuery,
                    new String[] {StudentEdPlan.REL_EXTENDED_DATA_DICTIONARY + ModelProperty.PATH_DELIMITER
                            + ExtendedDataDictionary.COL_NAME,
                            StudentEdPlan.COL_STUDENT_OID},
                    new int[] {16, 256});
        }
    }

    /**
     * Retrieves if a student is Active.
     *
     * @author Follett Software Company
     */
    public class RetrieverStdActive implements FieldRetriever {
        public static final String PARAM_ACTIVE_1 = "PARAM_ACTIVE_1";
        public static final String PARAM_ACTIVE_2 = "PARAM_ACTIVE_2";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String value = BooleanAsStringConverter.FALSE;

            CRDCStudentData stdCRDCData = (CRDCStudentData) data;
            SisStudent student = (SisStudent) entity.getBean();
            PlainDate activeDate = null;
            if (PARAM_ACTIVE_1.equals(field.getParameter())) {
                activeDate = stdCRDCData.m_reportDatePart1;
            } else if (PARAM_ACTIVE_2.equals(field.getParameter())) {
                activeDate = stdCRDCData.m_reportDatePart2;
            }

            if (activeDate != null) {
                StudentEnrollment enrollment =
                        stdCRDCData.m_studentHelper.getEnrollmentForDate(student.getOid(), activeDate, "EWSY");

                if (enrollment != null
                        && StudentManager.isActiveStudent(getOrganization(), enrollment.getStatusCode())) {
                    value = BooleanAsStringConverter.TRUE;
                }
            }
            return value;
        }
    }

    /**
     * Retrieves count of student's conducts based on field parameter.
     *
     * @author Follett Software Company
     */
    public class RetrieverStdActCnt implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            int count = 0;
            String code = field.getParameter() == null ? null : field.getParameter().toString();
            CRDCStudentData stdCRDCData = (CRDCStudentData) data;
            Set<ConductAction> actions =
                    stdCRDCData.m_conductHelper.getActionsForStudent(entity.getBean().getOid(), code);
            if (actions != null) {
                count = actions.size();
            }
            return Integer.valueOf(count);
        }
    }

    /**
     * Retrieves if there is any conduct for student based on field parameter.
     *
     * @author Follett Software Company
     */
    public class RetrieverStdActIs implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            boolean isExist = false;
            String code = field.getParameter() == null ? null : field.getParameter().toString();
            CRDCStudentData stdCRDCData = (CRDCStudentData) data;
            Set<ConductAction> actions =
                    stdCRDCData.m_conductHelper.getActionsForStudent(entity.getBean().getOid(), code);
            if (actions != null) {
                isExist = actions.size() > 0 ? true : false;
            }
            return Boolean.valueOf(isExist);
        }
    }

    /**
     * Retrieves sum of penalty time for student.
     *
     * @author Follett Software Company
     */
    public class RetrieverStdActSum implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            int numDays = 0;
            String code = field.getParameter() == null ? null : field.getParameter().toString();
            CRDCStudentData stdCRDCData = (CRDCStudentData) data;
            Set<ConductAction> actions =
                    stdCRDCData.m_conductHelper.getActionsForStudent(entity.getBean().getOid(), code);
            if (actions != null) {
                for (ConductAction action : actions) {
                    if (action.getActionPenaltyTime() != null) {
                        numDays += action.getActionPenaltyTime().setScale(0, RoundingMode.HALF_UP).intValue();
                    }
                }
            }
            return Integer.valueOf(numDays);
        }
    }

    /**
     * Retrieves count of student's incidents based on field parameter.
     *
     * @author Follett Software Company
     */
    public class RetrieverStdCndCnt implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            int count = 0;
            String code = field.getParameter() == null ? null : field.getParameter().toString();
            Set<ConductIncident> incidents =
                    CRDCStudentData.this.m_conductHelper.getIncidentsForStudent(entity.getBean().getOid(), code);
            if (incidents != null) {
                for (ConductIncident incident : incidents) {
                    if (incident.getConductActions() != null || incident.getConductActions().size() > 0) {
                        count++;
                    }
                }
            }
            return Integer.valueOf(count);
        }
    }

    /**
     * Retriever for SAT and ACT tests.
     *
     * @author Follett Software Company
     */
    public class RetrieverStdCrdcAsd implements FieldRetriever {
        private static final String ALIAS_ASD_CRDC_ASSESSMENT_CODE = "all-asd-CRDCAssessmentCode";

        private String m_fieldAsdCrdcCode;
        private Map<String, Set<String>> m_studentsCrdcAsdCodesMap = null;

        /**
         * Instantiates a new retriever std crdc asd.
         */
        public RetrieverStdCrdcAsd() {
            m_fieldAsdCrdcCode = translateAliasToJavaName(ALIAS_ASD_CRDC_ASSESSMENT_CODE, true);
            if (m_fieldAsdCrdcCode != null) {
                loadStdWithCrdcAsd(CRDCStudentData.this);
            }
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String stdOid = entity.getBean().getOid();
            Set<String> stdCrdcPgmCodes = m_studentsCrdcAsdCodesMap.get(stdOid);
            String paramCrdcPgmCode = (String) field.getParameter();

            boolean value = false;

            if (stdCrdcPgmCodes != null && stdCrdcPgmCodes.contains(paramCrdcPgmCode)) {
                value = true;
            }
            return Boolean.valueOf(value);
        }

        /**
         * Load std with crdc asd.
         *
         * @param crdcStdData CRDCStudentData
         */
        private void loadStdWithCrdcAsd(CRDCStudentData crdcStdData) {
            m_studentsCrdcAsdCodesMap = new HashMap<String, Set<String>>();

            String className =
                    this.getClass().getName().replaceAll(this.getClass().getEnclosingClass().getName() + ".{1}", "");

            Set<String> usedAsdCrdcCodes = new HashSet<String>();
            int numOfFields = crdcStdData.getFieldCount();
            for (int i = 0; i < numOfFields; i++) {
                FieldDefinition currentField = crdcStdData.getFieldDefinition(i);

                if (currentField.getCalcId() != null && currentField.getCalcId().equals(className)) {
                    String paramCrdcAsdCode = (String) currentField.getParameter();
                    if (paramCrdcAsdCode != null) {
                        usedAsdCrdcCodes.add(paramCrdcAsdCode);
                    }
                }
            }

            if (!usedAsdCrdcCodes.isEmpty()) {
                X2Criteria stdCriteria = crdcStdData.m_studentHelper.getStudentCriteria();
                SubQuery subQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, stdCriteria);

                X2Criteria asdCriteria = new X2Criteria();
                asdCriteria.addIn(StudentAssessment.COL_STUDENT_OID, subQuery);
                asdCriteria.addIn(StudentAssessment.REL_ASSESSMENT_DEFINITION + ModelProperty.PATH_DELIMITER +
                        m_fieldAsdCrdcCode, usedAsdCrdcCodes);
                asdCriteria.addGreaterOrEqualThan(StudentAssessment.COL_DATE,
                        crdcStdData.getCurrentContext().getStartDate());
                asdCriteria.addLessOrEqualThan(StudentAssessment.COL_DATE,
                        crdcStdData.getCurrentContext().getEndDate());

                String[] columns =
                        new String[] {StudentAssessment.COL_STUDENT_OID, StudentAssessment.REL_ASSESSMENT_DEFINITION +
                                ModelProperty.PATH_DELIMITER + m_fieldAsdCrdcCode};

                ReportQueryByCriteria query = new ReportQueryByCriteria(StudentAssessment.class, columns, asdCriteria);

                ReportQueryIterator iterator = CRDCStudentData.this.getBroker().getReportQueryIteratorByQuery(query);
                try {
                    while (iterator.hasNext()) {
                        Object[] record = (Object[]) iterator.next();
                        String studentOid = (String) record[0];
                        String crdcCode = (String) record[1];

                        Set<String> crdcCodes = m_studentsCrdcAsdCodesMap.get(studentOid);

                        if (crdcCodes == null) {
                            crdcCodes = new HashSet<String>();
                            m_studentsCrdcAsdCodesMap.put(studentOid, crdcCodes);
                        }

                        crdcCodes.add(crdcCode);
                    }
                } finally {
                    iterator.close();
                }
            }
        }
    }

    /**
     * Retriever to get values based on student's programs.
     *
     * @author Follett Software Company
     */
    public class RetrieverStdCrdcPgm implements FieldRetriever {

        /**
         * Instantiates a new retriever std crdc pgm.
         */
        public RetrieverStdCrdcPgm() {
            loadStdCrdcPgmCodesMap(CRDCStudentData.this);
        }

        private Map<String, Set<String>> m_studentsCrdcPgmCodesMap = null;

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();
            String stdOid = student.getOid();
            String paramCrdcPgmCode = (String) field.getParameter();

            if (m_stdSnapLEP != null && PARAM_CODE_LEP.equals(paramCrdcPgmCode)) {
                return Boolean.valueOf(m_stdSnapLEP.contains(stdOid));
            }

            if (m_stdSnapIDEA != null && PARAM_CODE_IDEA.equals(paramCrdcPgmCode)) {
                return Boolean.valueOf(m_stdSnapIDEA.contains(stdOid));
            }

            if (m_stdSnap504 != null && PARAM_CODE_SECTIN_504.equals(paramCrdcPgmCode)) {
                return Boolean.valueOf(m_stdSnap504.contains(stdOid));
            }

            boolean value = false;

            Set<String> stdCrdcPgmCodes = m_studentsCrdcPgmCodesMap.get(stdOid);

            if (stdCrdcPgmCodes != null && stdCrdcPgmCodes.contains(paramCrdcPgmCode)) {
                value = true;
            }

            return Boolean.valueOf(value);
        }

        /**
         * Load std crdc pgm codes map.
         *
         * @param crdcStdData CRDCStudentData
         */
        private void loadStdCrdcPgmCodesMap(CRDCStudentData crdcStdData) {
            m_studentsCrdcPgmCodesMap = new HashMap<String, Set<String>>();

            String className =
                    this.getClass().getName().replaceAll(this.getClass().getEnclosingClass().getName() + ".{1}", "");

            Set<String> usedProgramsCrdcCodes = new HashSet<String>();
            int numOfFields = crdcStdData.getFieldCount();
            for (int i = 0; i < numOfFields; i++) {
                FieldDefinition currentField = crdcStdData.getFieldDefinition(i);

                if (currentField.getCalcId() != null && currentField.getCalcId().equals(className)) {
                    String paramCrdcPgmCode = (String) currentField.getParameter();
                    if (paramCrdcPgmCode != null) {
                        usedProgramsCrdcCodes.add(paramCrdcPgmCode);
                    }
                }
            }

            DataDictionaryField pgmCodeField = crdcStdData.getDataDictionaryField(StudentProgramParticipation.class,
                    StudentProgramParticipation.COL_PROGRAM_CODE);
            ReferenceTable refTable = pgmCodeField.getReferenceTable();
            Collection<ReferenceCode> refCodes = refTable.getReferenceCodes();

            // Allow for comma separated CRDC Codes per reference code
            Map<String, Set<String>> usedPgmCodeCrdcCodeMap = new HashMap();

            for (ReferenceCode refCode : refCodes) {
                String code = refCode.getCode();
                String crdcCodes = (String) refCode.getFieldValueByAlias(ALIAS_CRDC_REF_CODE);
                if (!StringUtils.isEmpty(crdcCodes)) {
                    Set<String> codeSet = new HashSet();
                    for (String crdcCode : Arrays.asList(crdcCodes.split("\\s*,\\s*"))) {
                        if (usedProgramsCrdcCodes.contains(crdcCode)) {
                            codeSet.add(crdcCode);
                        }
                    }
                    usedPgmCodeCrdcCodeMap.put(code, codeSet);
                }
            }

            if (!usedProgramsCrdcCodes.isEmpty()) {
                X2Criteria stdCriteria = crdcStdData.m_studentHelper.getStudentCriteria();
                SubQuery subQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, stdCriteria);

                X2Criteria pgmCriteria = new X2Criteria();
                pgmCriteria.addIn(StudentProgramParticipation.COL_STUDENT_OID, subQuery);
                pgmCriteria.addIn(StudentProgramParticipation.COL_PROGRAM_CODE, usedPgmCodeCrdcCodeMap.keySet());

                pgmCriteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE,
                        crdcStdData.getCurrentContext().getEndDate());

                X2Criteria endDate1Criteria = new X2Criteria();
                endDate1Criteria.addEmpty(StudentProgramParticipation.COL_END_DATE, getBroker().getPersistenceKey());
                X2Criteria endDate2Criteria = new X2Criteria();
                endDate2Criteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_END_DATE,
                        crdcStdData.getCurrentContext().getStartDate());
                endDate1Criteria.addOrCriteria(endDate2Criteria);
                pgmCriteria.addAndCriteria(endDate1Criteria);

                QueryByCriteria pgmQuery = new QueryByCriteria(StudentProgramParticipation.class, pgmCriteria);

                Collection<StudentProgramParticipation> programs = getBroker().getCollectionByQuery(pgmQuery);

                for (StudentProgramParticipation program : programs) {
                    String studentOid = program.getStudentOid();

                    Set<String> stdPrograms = m_studentsCrdcPgmCodesMap.get(studentOid);

                    if (stdPrograms == null) {
                        stdPrograms = new HashSet<String>();
                        m_studentsCrdcPgmCodesMap.put(studentOid, stdPrograms);
                    }

                    String programCode = program.getProgramCode();
                    Set<String> crdcProgramCodes = usedPgmCodeCrdcCodeMap.get(programCode);

                    if (crdcProgramCodes != null && !crdcProgramCodes.isEmpty()) {
                        for (String crdcProgramCode : crdcProgramCodes) {
                            stdPrograms.add(crdcProgramCode);
                        }
                    }
                }
            }
        }
    }

    /**
     * Retriever.
     *
     * @author X2 Development Corporation
     */
    public abstract class RetrieverStdCrs implements FieldRetriever {
        protected CRDCStudentHistoryHelper m_tnHelper;

        /**
         * Instantiates a new retriever std crs.
         */
        public RetrieverStdCrs() {
            super();
            applyCriteria();
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            boolean value = false;

            if (m_tnHelper != null) {
                List<StudentScheduleSpan> schSpans = m_tnHelper.getStudentScheduleSpans((SisStudent) entity.getBean());

                if (schSpans != null) {
                    if (doSpanDatesCheck()) {
                        value = isSpanActiveOnReportDates(schSpans);
                    } else if (!schSpans.isEmpty()) {
                        value = true;
                    }
                }
            }

            return Boolean.valueOf(value);
        }

        /**
         * Adds the equal course field.
         *
         * @param fieldName String
         * @param value String
         */
        void addEqualCourseField(String fieldName, String value) {
            if (m_tnHelper == null) {
                m_tnHelper = getSchedHistoryHelper(m_reportDatePart1);
            }

            X2Criteria sccCriteria = m_tnHelper.getStudentScheduleChangeCriteria();
            sccCriteria.addEqualTo(StudentScheduleChange.REL_MASTER_SCHEDULE + ModelProperty.PATH_DELIMITER +
                    MasterSchedule.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER +
                    SchoolCourse.REL_COURSE + ModelProperty.PATH_DELIMITER +
                    fieldName, value);

            X2Criteria sscCriteria = m_tnHelper.getStudentScheduleCriteria();
            sscCriteria.addEqualTo(StudentSchedule.REL_SECTION + ModelProperty.PATH_DELIMITER +
                    MasterSchedule.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER +
                    SchoolCourse.REL_COURSE + ModelProperty.PATH_DELIMITER +
                    fieldName, value);
        }

        /**
         * Adds the equal section field.
         *
         * @param fieldName String
         * @param value String
         */
        void addEqualSectionField(String fieldName, String value) {
            if (m_tnHelper == null) {
                m_tnHelper = getSchedHistoryHelper(m_reportDatePart1);
            }

            X2Criteria sccCriteria = m_tnHelper.getStudentScheduleChangeCriteria();
            sccCriteria.addEqualTo(StudentScheduleChange.REL_MASTER_SCHEDULE + ModelProperty.PATH_DELIMITER +
                    fieldName, value);

            X2Criteria sscCriteria = m_tnHelper.getStudentScheduleCriteria();
            sscCriteria.addEqualTo(StudentSchedule.REL_SECTION + ModelProperty.PATH_DELIMITER +
                    fieldName, value);
        }

        /**
         * Adds the in course field.
         *
         * @param fieldName String
         * @param values Collection<String>
         */
        void addInCourseField(String fieldName, Collection<String> values) {
            if (m_tnHelper == null) {
                m_tnHelper = getSchedHistoryHelper(m_reportDatePart1);
            }

            X2Criteria sccCriteria = m_tnHelper.getStudentScheduleChangeCriteria();
            sccCriteria.addIn(StudentScheduleChange.REL_MASTER_SCHEDULE + ModelProperty.PATH_DELIMITER +
                    MasterSchedule.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER +
                    SchoolCourse.REL_COURSE + ModelProperty.PATH_DELIMITER +
                    fieldName, values);

            X2Criteria sscCriteria = m_tnHelper.getStudentScheduleCriteria();
            sscCriteria.addIn(StudentSchedule.REL_SECTION + ModelProperty.PATH_DELIMITER +
                    MasterSchedule.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER +
                    SchoolCourse.REL_COURSE + ModelProperty.PATH_DELIMITER +
                    fieldName, values);
        }

        /**
         * Adds the in section field.
         *
         * @param fieldName String
         * @param values Collection<String>
         */
        void addInSectionField(String fieldName, Collection<String> values) {
            if (m_tnHelper == null) {
                m_tnHelper = getSchedHistoryHelper(m_reportDatePart1);
            }

            X2Criteria sccCriteria = m_tnHelper.getStudentScheduleChangeCriteria();
            sccCriteria.addIn(StudentScheduleChange.REL_MASTER_SCHEDULE + ModelProperty.PATH_DELIMITER +
                    fieldName, values);

            X2Criteria sscCriteria = m_tnHelper.getStudentScheduleCriteria();
            sscCriteria.addIn(StudentSchedule.REL_SECTION + ModelProperty.PATH_DELIMITER +
                    fieldName, values);
        }

        /**
         * Adds the like course field.
         *
         * @param fieldName String
         * @param value String
         */
        void addLikeCourseField(String fieldName, String value) {
            if (m_tnHelper == null) {
                m_tnHelper = getSchedHistoryHelper(m_reportDatePart1);
            }

            X2Criteria sccCriteria = m_tnHelper.getStudentScheduleChangeCriteria();
            sccCriteria.addLike(StudentScheduleChange.REL_MASTER_SCHEDULE + ModelProperty.PATH_DELIMITER +
                    MasterSchedule.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER +
                    SchoolCourse.REL_COURSE + ModelProperty.PATH_DELIMITER +
                    fieldName, value);

            X2Criteria sscCriteria = m_tnHelper.getStudentScheduleCriteria();
            sscCriteria.addLike(StudentSchedule.REL_SECTION + ModelProperty.PATH_DELIMITER +
                    MasterSchedule.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER +
                    SchoolCourse.REL_COURSE + ModelProperty.PATH_DELIMITER +
                    fieldName, value);
        }

        /**
         * Apply criteria.
         */
        abstract void applyCriteria();

        /**
         * Do span dates check.
         *
         * @return true, if successful
         */
        boolean doSpanDatesCheck() {
            return false;
        }

        /**
         * Checks if is date in range.
         *
         * @param checkedDate PlainDate
         * @param entryDate PlainDate
         * @param exitDate PlainDate
         * @return true, if is date in range
         */
        private boolean isDateInRange(PlainDate checkedDate, PlainDate entryDate, PlainDate exitDate) {
            return (entryDate == null || !entryDate.after(checkedDate)) &&
                    (exitDate == null || !exitDate.before(checkedDate));
        }

        /**
         * Checks if is span active on report dates.
         *
         * @param schSpans Collection<StudentScheduleSpan>
         * @return true, if is span active on report dates
         */
        private boolean isSpanActiveOnReportDates(Collection<StudentScheduleSpan> schSpans) {
            boolean isSpanActiveOnReportDates = false;

            Iterator<StudentScheduleSpan> schIterator = schSpans.iterator();
            while (isSpanActiveOnReportDates == false && schIterator.hasNext()) {
                StudentScheduleSpan schSpan = schIterator.next();

                PlainDate entryDate = schSpan.getEntryDate();
                PlainDate exitDate = schSpan.getExitDate();

                isSpanActiveOnReportDates = isDateInRange(m_reportDatePart1, entryDate, exitDate);

                if (isSpanActiveOnReportDates == false && m_reportDatePart2 != null) {
                    isSpanActiveOnReportDates = isDateInRange(m_reportDatePart2, entryDate, exitDate);
                }
            }

            return isSpanActiveOnReportDates;
        }
    }

    /**
     * The Class RetrieverStdCrsNoLike.
     */
    public abstract class RetrieverStdCrsNoLike extends RetrieverStdCrs {

        /**
         * Apply criteria.
         *
         * @see com.x2dev.procedures.statereporting.CRDCStudentData.RetrieverStdCrs#applyCriteria()
         */
        @Override
        final void applyCriteria() {
            String like = getPattern();
            this.addLikeCourseField(Course.COL_NUMBER, like);
        }

        /**
         * Gets the pattern.
         *
         * @return String
         */
        abstract String getPattern();
    }

    /**
     * Retrive if student has specific courses on report date based on passed alias.
     *
     * @author Follett Software Company
     */
    public class RetrieverStdCrsIs extends RetrieverByAlias {

        /**
         * Instantiates a new retriever std crs is.
         */
        public RetrieverStdCrsIs() {
            m_scheduleHelper = getSchedHistoryHelperCrs(m_reportDatePart1, m_usedAliases);
        }

        private CRDCStudentHistoryHelper m_scheduleHelper = null;

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {

            SisStudent student = (SisStudent) entity.getBean();

            String paramAlias = (String) field.getParameter();
            String javaName = translateAliasToJavaName(paramAlias, true);
            List<StudentScheduleSpan> spans = m_scheduleHelper.getStudentScheduleSpans(student);

            String value = BooleanAsStringConverter.FALSE;

            for (StudentScheduleSpan span : spans) {
                PlainDate entryDate = span.getEntryDate();
                PlainDate exitDate = span.getExitDate();

                boolean isSpanActiveOnReportDates = isDateInRange(m_reportDatePart1, entryDate, exitDate);
                if (isSpanActiveOnReportDates == false && m_reportDatePart2 != null) {
                    isSpanActiveOnReportDates = isDateInRange(m_reportDatePart2, entryDate, exitDate);
                }

                if (isSpanActiveOnReportDates) {
                    Course course = span.getSection().getSchoolCourse().getCourse();
                    String courseIndicator = (String) course.getFieldValueByBeanPath(javaName);
                    if (BooleanAsStringConverter.TRUE.equals(courseIndicator)) {
                        value = BooleanAsStringConverter.TRUE;
                        break;
                    }
                }
            }
            return value;
        }

        /**
         * Checks if is date in range.
         *
         * @param checkedDate PlainDate
         * @param entryDate PlainDate
         * @param exitDate PlainDate
         * @return true, if is date in range
         */
        private boolean isDateInRange(PlainDate checkedDate, PlainDate entryDate, PlainDate exitDate) {
            return (entryDate == null || !entryDate.after(checkedDate)) &&
                    (exitDate == null || !exitDate.before(checkedDate));
        }
    }

    /**
     * Retrive if student has specific courses in reporting year based on passed alias.
     *
     * @author Follett Software Company
     */
    public class RetrieverStdCrsIsYr extends RetrieverByAlias {
        Map<String, Set<String>> m_hasTranscript = new HashMap();

        /**
         * Instantiates a new retriever std crs is yr.
         */
        public RetrieverStdCrsIsYr() {
            m_scheduleHelper = getSchedHistoryHelperCrs(m_reportDatePart1, m_usedAliases);

            X2Criteria transcriptCriteria = new X2Criteria();
            transcriptCriteria.addEqualTo(Transcript.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());
            X2Criteria andCriteria = new X2Criteria();
            String[] columns = new String[m_usedAliases.size() + 1];
            int columnIndex = 0;
            columns[columnIndex++] = Transcript.COL_STUDENT_OID;
            for (String alias : m_usedAliases) {
                m_hasTranscript.put(alias, new HashSet());
                String javaName = translateAliasToJavaName(alias, true);
                X2Criteria orCriteria = new X2Criteria();
                String column = Transcript.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER +
                        SchoolCourse.REL_COURSE + ModelProperty.PATH_DELIMITER + javaName;
                columns[columnIndex++] = column;
                orCriteria.addEqualTo(column, BooleanAsStringConverter.TRUE);
                andCriteria.addOrCriteria(orCriteria);
            }
            transcriptCriteria.addAndCriteria(andCriteria);

            ColumnQuery query = new ColumnQuery(Transcript.class, columns, transcriptCriteria);
            ReportQueryIterator queryItr = getBroker().getReportQueryIteratorByQuery(query);
            try {
                while (queryItr.hasNext()) {
                    Object[] row = (Object[]) queryItr.next();
                    String studentOid = (String) row[0];
                    int index = 1;
                    for (String alias : m_usedAliases) {
                        String value = (String) row[index++];
                        if (BooleanAsStringConverter.TRUE.equals(value)) {
                            m_hasTranscript.get(alias).add(studentOid);
                        }
                    }
                }
            } finally {
                queryItr.close();
            }
        }

        private CRDCStudentHistoryHelper m_scheduleHelper = null;

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();
            String value = BooleanAsStringConverter.FALSE;

            String paramAlias = (String) field.getParameter();
            if (m_hasTranscript.get(paramAlias) != null && m_hasTranscript.get(paramAlias).contains(student.getOid())) {
                value = BooleanAsStringConverter.TRUE;
            } else {
                String javaName = translateAliasToJavaName(paramAlias, true);
                List<StudentScheduleSpan> spans = m_scheduleHelper.getStudentScheduleSpans(student);


                for (StudentScheduleSpan span : spans) {
                    Course course = span.getSection().getSchoolCourse().getCourse();
                    String courseIndicator = (String) course.getFieldValueByBeanPath(javaName);
                    if (BooleanAsStringConverter.TRUE.equals(courseIndicator)) {
                        value = BooleanAsStringConverter.TRUE;
                        break;
                    }
                }
            }

            return value;
        }
    }

    /**
     * Retrieve days student absence during current CTX.
     *
     * @author X2 Development Corporation
     *
     */
    public class RetrieverStdDaysAbs implements FieldRetriever {
        Map<String, Collection<StudentAttendance>> m_attMapByStdOid;

        /**
         * Instantiates a new retriever std days abs.
         */
        public RetrieverStdDaysAbs() {
            super();

            X2Criteria attCriteria = new X2Criteria();

            SubQuery stdSubQuery =
                    new SubQuery(SisStudent.class, X2BaseBean.COL_OID, m_studentHelper.getStudentCriteria());
            attCriteria.addIn(StudentAttendance.COL_STUDENT_OID, stdSubQuery);
            attCriteria.addEqualTo(StudentAttendance.COL_ABSENT_INDICATOR, BooleanAsStringConverter.TRUE);
            attCriteria.addGreaterOrEqualThan(StudentAttendance.COL_DATE, getCurrentContext().getStartDate());
            attCriteria.addLessOrEqualThan(StudentAttendance.COL_DATE, getCurrentContext().getEndDate());

            QueryByCriteria attQuery = new QueryByCriteria(StudentAttendance.class, attCriteria);

            m_attMapByStdOid = CRDCStudentData.this.getBroker().getGroupedCollectionByQuery(attQuery,
                    StudentAttendance.COL_STUDENT_OID, 1024);
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent std = (SisStudent) entity.getBean();

            return m_attMapByStdOid.get(std.getOid()) != null
                    ? Integer.valueOf(m_attMapByStdOid.get(std.getOid()).size())
                    : Integer.valueOf(0);
        }
    }

    /**
     * Retriever to get student grade level based on YOG and YOG change.
     *
     * @author X2 Development Corporation
     *
     */
    public class RetrieverStdGradeLev implements FieldRetriever {
        HashMap<String, String> m_crdcLookup = new HashMap();
        HashMap<Integer, String> m_gradeLevelMap = new HashMap();
        int m_maxGradeLevel;
        TreeMap<Integer, List<String>> m_sortedGradeLevels;

        /**
         * Instantiates a new retriever std grade lev.
         */
        public RetrieverStdGradeLev() {
            super();

            m_sortedGradeLevels = StudentManager.buildGradeLevelMap(CRDCStudentData.this.getBroker());
            m_maxGradeLevel = StudentManager.getMaxGradeLevel(CRDCStudentData.this.getBroker());

            DataDictionaryField aliasField = getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_CRDC_REF_CODE);
            ModelProperty prop = new ModelProperty(SisStudent.class, SisStudent.COL_GRADE_LEVEL,
                    CRDCStudentData.this.getBroker().getPersistenceKey());
            DataDictionaryField field = getDataDictionary().findDataDictionaryField(prop.getFieldId());

            if (aliasField != null && !StringUtils.isEmpty(field.getReferenceTableOid())) {
                X2Criteria criteria = new X2Criteria();
                criteria.addNotEmpty(aliasField.getJavaName(), CRDCStudentData.this.getBroker().getPersistenceKey());
                criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, field.getReferenceTableOid());

                String[] columns = new String[] {ReferenceCode.COL_CODE, aliasField.getJavaName()};

                ReportQueryByCriteria query = new ReportQueryByCriteria(ReferenceCode.class, columns, criteria);

                ReportQueryIterator iterator = CRDCStudentData.this.getBroker().getReportQueryIteratorByQuery(query);
                try {
                    while (iterator.hasNext()) {
                        Object[] record = (Object[]) iterator.next();
                        String code = (String) record[0];
                        String crdcCode = (String) record[1];
                        m_crdcLookup.put(code, crdcCode);
                    }
                } finally {
                    iterator.close();
                }
            }

        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();
            CRDCStudentData stdData = (CRDCStudentData) data;
            String gradeLevel = null;

            StudentEnrollment enrollment =
                    m_studentHelper.getEnrollmentForDate(student.getOid(), stdData.m_reportDatePart1, "EWSY");

            if (enrollment == null && stdData.m_reportDatePart2 != null) {
                enrollment = m_studentHelper.getEnrollmentForDate(student.getOid(), stdData.m_reportDatePart2, "EWSY");
            }
            if (enrollment == null) {
                enrollment = m_studentHelper.getEnrollmentForDate(student.getOid(),
                        stdData.getCurrentContext().getEndDate(), "EWSY");
            }
            if (enrollment == null) {
                // use next enrollment after context end date
                List<StudentEnrollment> enrollments = m_studentHelper.getStudentEnrollments(student);
                if (enrollments != null && !enrollments.isEmpty()) {
                    enrollment = enrollments.get(enrollments.size() - 1);
                }
            }
            if (enrollment != null) {
                gradeLevel = getGradeLevel(Integer.valueOf(enrollment.getYog()));
            }

            // Try student yog if enrollment fails
            if (StringUtils.isEmpty(gradeLevel)) {
                gradeLevel = getGradeLevel(Integer.valueOf(student.getYog()));
            }
            return gradeLevel;
        }

        /**
         * Return the cached CRDCCode for the grade level for this yog.
         *
         * @param yog Integer
         * @return String
         */
        private String getGradeLevel(Integer yog) {
            String value = null;

            if (!m_gradeLevelMap.containsKey(yog)) {
                String gradeLevel = null;
                List<String> matchingGradeLevels =
                        StudentManager.getMatchingGradeLevels(m_maxGradeLevel, yog.intValue(),
                                CRDCStudentData.this.getCurrentContext().getSchoolYear(), m_sortedGradeLevels);
                for (String matchingGradeLevel : matchingGradeLevels) {
                    gradeLevel = m_crdcLookup.get(matchingGradeLevel);
                    if (!StringUtils.isEmpty(gradeLevel)) {
                        break;
                    }
                }
                m_gradeLevelMap.put(yog, gradeLevel);
            }
            value = m_gradeLevelMap.get(yog);
            return value;
        }
    }

    /**
     * Retrieve if a student pass algebra 1 for WA.
     *
     * @author X2 Development Corporation
     */
    public class RetrieverStdPasAlgWA implements FieldRetriever {

        /**
         * Class provide method for working with TranscriptDefinition.
         *
         * @author X2 Development Corporation
         */
        public class TranscriptColumnWAHelper {
            private Map<String, Map<String, List<String>>> someMap = new HashMap<String, Map<String, List<String>>>();
            private Map<String, Map<String, DataFieldConfig>> fieldConfigurationMap =
                    new HashMap<String, Map<String, DataFieldConfig>>();

            /**
             * Gets the transcript field definition for term.
             *
             * @param transcript Transcript
             * @param term String
             * @return DataFieldConfig where transcript keeps grade for input "term"
             */
            public DataFieldConfig getTranscriptFieldDefinitionForTerm(Transcript transcript, String term) {
                String trDefOid = transcript.getTranscriptDefinitionOid();
                Map<String, DataFieldConfig> termFieldCfgMap = fieldConfigurationMap.get(trDefOid);
                if (termFieldCfgMap == null) {
                    termFieldCfgMap = new HashMap<String, DataFieldConfig>();

                    for (TranscriptColumnDefinition columnDefinition : transcript.getTranscriptDefinition()
                            .getTranscriptColumnDefinitions()) {
                        String gradeName = columnDefinition.getGradeName();
                        termFieldCfgMap.put(gradeName, columnDefinition.getDataFieldConfig());
                    }

                    fieldConfigurationMap.put(trDefOid, termFieldCfgMap);
                }

                return termFieldCfgMap.get(term + " Grade");



            }

            /**
             * Checks if is grade passed.
             *
             * @param transcript Transcript
             * @param gradeCode String
             * @param term String
             * @return true if gradeCode is passed
             */
            public boolean isGradePassed(Transcript transcript, String gradeCode, String term) {
                boolean isPassed = false;
                String trDefOid = transcript.getTranscriptDefinitionOid();
                Map<String, List<String>> columnsMap = someMap.get(trDefOid);
                if (columnsMap == null) {
                    columnsMap = new HashMap<String, List<String>>();
                    someMap.put(trDefOid, columnsMap);
                }

                List<String> passedGrades = columnsMap.get(term);
                if (passedGrades == null) {
                    passedGrades = initializePassedGrades(transcript, term);
                    columnsMap.put(term, passedGrades);
                }
                if (passedGrades.contains(gradeCode)) {
                    isPassed = true;
                }
                return isPassed;
            }

            /**
             * Initialize passed grades.
             *
             * @param transcript Transcript
             * @param termCode String
             * @return List
             */
            private List<String> initializePassedGrades(Transcript transcript, String termCode) {
                List<String> passedGrades = new ArrayList<String>();
                String fieldName = termCode + " Grade";
                for (TranscriptColumnDefinition columnDefinition : transcript.getTranscriptDefinition()
                        .getTranscriptColumnDefinitions()) {

                    String gradeName = columnDefinition.getGradeName();
                    if (gradeName.equals(fieldName)) {
                        for (GradeScaleGradeDefinition scaleCode : columnDefinition.getGradeScale()
                                .getGradeScaleDefinitions()) {
                            if (scaleCode.getCreditIndicator()) {
                                passedGrades.add(scaleCode.getGradeCode());
                            }
                        }
                        break;
                    }
                }

                return passedGrades;
            }
        }

        public static final String CRDC_CODE_ALGEBRA_I = "Algebra I";
        public static final String CRDC_CODE_MIDDLE_SCHOOL = "Middle School";
        public static final String CRDC_CODE_HIGH_SCHOOL = "High School";
        public static final String TERM_CODE_Q0 = "Q0";
        public static final String TERM_CODE_T0 = "T0";
        public static final String TERM_S1 = "S1";
        public static final String TERM_S2 = "S2";
        public static final String TERM_SS = "SS";
        public static final String TERM_T3 = "T3";
        public static final String TERM_T2 = "T2";
        public static final String TERM_T1 = "T1";

        protected X2Criteria m_courseCriteria = null;
        protected String m_crdcRefCodeField = null;
        protected String m_crdcCourseCtgField = null;
        protected Map<String, Collection<StudentSchedule>> m_stdScheduleMapWA = null;
        protected Map<String, Collection<Transcript>> m_stdTranscriptMapWA = null;
        protected TranscriptColumnWAHelper m_transcriptColumnWAHelper = new TranscriptColumnWAHelper();


        /**
         * Instantiates a new retriever std pas alg WA.
         */
        public RetrieverStdPasAlgWA() {
            m_crdcRefCodeField = translateAliasToJavaName(AliasField.CRDC_CODE.getAlias(), true);
            m_crdcCourseCtgField = translateAliasToJavaName(AliasField.COURSE_CATEGORY.getAlias(), true);
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String stdOid = entity.getBean().getOid();
            List<String> earnedCreditTerms = getEarnedCreditTerms(stdOid);
            boolean isAlgebraPassed = isCoveredByScheduleTems(earnedCreditTerms, stdOid);

            return isAlgebraPassed ? RetrieverStdPassAlg.RESULT_YES : RetrieverStdPassAlg.RESULT_NO;
        }

        /**
         * Just for WA.
         *
         * @param stdOid String
         * @return Collection of StudentSchedule for Algebra I crdc, for current school year context
         */
        private Collection<StudentSchedule> getAlgebra1StdScheduleWAByStdOid(String stdOid) {
            Collection<StudentSchedule> returnStdSchedule = null;
            if (m_stdScheduleMapWA == null) {
                initializeStudentScheduleMap(m_studentHelper.getStudentCriteria().copy());
            }
            returnStdSchedule = m_stdScheduleMapWA.get(stdOid);
            if (returnStdSchedule == null) {
                returnStdSchedule = new ArrayList<StudentSchedule>();
            }
            return returnStdSchedule;
        }

        /**
         * Just for WA.
         *
         * @param stdOid String
         * @return Collection of Transcript for Algebra I crdc, for current school year context
         */
        private Collection<Transcript> getAlgebra1TranscriptsWAByStdOid(String stdOid) {
            Collection<Transcript> returnTranscript = null;
            if (m_stdTranscriptMapWA == null) {
                initializeStudentTranscriptMap(m_studentHelper.getStudentCriteria().copy());
            }
            returnTranscript = m_stdTranscriptMapWA.get(stdOid);
            if (returnTranscript == null) {
                returnTranscript = new ArrayList<Transcript>();
            }
            return returnTranscript;
        }

        /**
         * Gets the copied course criteria.
         *
         * @return X 2 criteria
         */
        private X2Criteria getCopiedCourseCriteria() {
            if (m_courseCriteria == null) {
                m_courseCriteria = new X2Criteria();
                List<Object> crdcFieldValuesSchLvl =
                        new ArrayList<Object>(Arrays.asList(CRDC_CODE_HIGH_SCHOOL, CRDC_CODE_MIDDLE_SCHOOL));
                X2Criteria refCodeCriteriaSchLvl = getRefCodeCriteriaForCRDCfield(Course.class.getName(),
                        Course.COL_SCHOOL_LEVEL, crdcFieldValuesSchLvl);
                SubQuery rcdSubQuerySchLvl =
                        new SubQuery(ReferenceCode.class, ReferenceCode.COL_CODE, refCodeCriteriaSchLvl);
                m_courseCriteria.addIn(Course.COL_SCHOOL_LEVEL, rcdSubQuerySchLvl);

                List<Object> crdcFieldValuesCrsCtg = new ArrayList<Object>(Arrays.asList(CRDC_CODE_ALGEBRA_I));
                X2Criteria refCodeCriteriaCrsCtg = getRefCodeCriteriaForCRDCfield(Course.class.getName(),
                        m_crdcCourseCtgField, crdcFieldValuesCrsCtg);
                SubQuery rcdSubQueryCrsCtg =
                        new SubQuery(ReferenceCode.class, ReferenceCode.COL_CODE, refCodeCriteriaCrsCtg);
                m_courseCriteria.addIn(m_crdcCourseCtgField, rcdSubQueryCrsCtg);
            }
            return m_courseCriteria.copy();
        }

        /**
         * Just for WA
         * Returns the grade value for the Final term code
         * doesn't work with SS, QO, TO. QO should be converted into S1 and S2, TO should be
         * converted into T1,T2 and T3
         *
         * @param transcript Transcript
         * @param termCode String
         * @return String
         * @throws X2BaseException exception
         */
        private String getGradeWA(Transcript transcript, String termCode) throws X2BaseException {
            String grade = null;
            DataFieldConfig dfc = m_transcriptColumnWAHelper.getTranscriptFieldDefinitionForTerm(transcript, termCode);
            if (dfc == null) {
                throw new X2BaseException();
            }
            grade = (String) transcript.getFieldValueByBeanPath(dfc.getDataField().getJavaName());

            if (grade == null) {
                grade = "";
            }

            return grade;
        }

        /**
         * Gets the ref code criteria for CRD cfield.
         *
         * @param className String
         * @param attribute String
         * @param crdcFieldValues List<Object>
         * @return X 2 criteria
         */
        private X2Criteria getRefCodeCriteriaForCRDCfield(String className,
                                                          String attribute,
                                                          List<Object> crdcFieldValues) {
            X2Criteria refCodeCriteria = null;
            DataDictionaryField ddxField = getDataDictionary().findDataDictionaryField(className, attribute);
            if (ddxField != null) {
                String refTableOid = ddxField.getReferenceTableOid();
                if (!StringUtils.isEmpty(refTableOid)) {
                    refCodeCriteria = new X2Criteria();
                    refCodeCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, refTableOid);
                    refCodeCriteria.addIn(m_crdcRefCodeField, crdcFieldValues);

                }
            }

            return refCodeCriteria;
        }

        /**
         * Just for WA
         * return list of basic terms codes for stdSchedule
         * ignore SS term.
         *
         * @param stdSchedule StudentSchedule
         * @return List
         */
        private List<String> getStdScheduleTermCodesWA(StudentSchedule stdSchedule) {
            List<String> returnCodes = new ArrayList<String>();
            MasterSchedule mst = stdSchedule.getSection();
            String termCode = null;
            if (mst != null && mst.getScheduleTerm() != null) {
                termCode = mst.getScheduleTerm().getCode();
            }

            if (StringUtils.isEmpty(termCode)) {
                termCode = stdSchedule.getTermView();
            }

            if (StringUtils.isEmpty(termCode) && mst != null) {
                termCode = mst.getTermView();
            }

            if (!StringUtils.isEmpty(termCode)) {
                if (TERM_CODE_Q0.equals(termCode)) {
                    returnCodes.add(TERM_S1);
                    returnCodes.add(TERM_S2);
                } else if (TERM_CODE_T0.equals(termCode)) {
                    returnCodes.add(TERM_T1);
                    returnCodes.add(TERM_T2);
                    returnCodes.add(TERM_T3);
                } else if (!TERM_SS.equals(termCode)) {
                    returnCodes.add(termCode);
                }
            }
            return returnCodes;
        }

        /**
         * Just for WA
         * return list of basic terms codes for transcript
         * ignore SS term.
         *
         * @param transcript Transcript
         * @return List
         */
        private List<String> getTranscriptTermCodesWA(Transcript transcript) {
            List<String> returnCodes = new ArrayList<String>();
            String termCode = null;
            MasterSchedule mst = transcript.getMasterSchedule();
            if (mst != null && mst.getScheduleTerm() != null) {
                termCode = transcript.getMasterSchedule().getScheduleTerm().getCode();
            }

            if (StringUtils.isEmpty(termCode)) {
                termCode = transcript.getTermCode();
            }

            if (StringUtils.isEmpty(termCode) && mst != null) {
                termCode = mst.getTermView();
            }

            if (!StringUtils.isEmpty(termCode)) {
                if (TERM_CODE_Q0.equals(termCode)) {
                    returnCodes.add(TERM_S1);
                    returnCodes.add(TERM_S2);
                } else if (TERM_CODE_T0.equals(termCode)) {
                    returnCodes.add(TERM_T1);
                    returnCodes.add(TERM_T2);
                    returnCodes.add(TERM_T3);
                } else if (!TERM_SS.equals(termCode)) {
                    returnCodes.add(termCode);
                }
            }

            return returnCodes;
        }

        /**
         * Gets the earned credit terms.
         *
         * @param stdOid String
         * @return List
         * @throws X2BaseException exception
         */
        private List<String> getEarnedCreditTerms(String stdOid) throws X2BaseException {

            List<String> earnedCreditTerms = new ArrayList<String>();

            Collection<Transcript> algeBraTranscripts = getAlgebra1TranscriptsWAByStdOid(stdOid);
            for (Transcript transcript : algeBraTranscripts) {
                List<String> termCodes = getTranscriptTermCodesWA(transcript);

                for (String termCode : termCodes) {
                    String algebra1Grade = getGradeWA(transcript, termCode);

                    if (m_transcriptColumnWAHelper.isGradePassed(transcript, algebra1Grade, termCode)) {
                        earnedCreditTerms.add(termCode);
                    }

                }
            }
            return earnedCreditTerms;
        }

        /**
         * Initialize student transcript map.
         *
         * @param studentCriteria X2Criteria
         */
        private void initializeStudentTranscriptMap(X2Criteria studentCriteria) {
            X2Criteria studentTransciptCriteria = new X2Criteria();
            SubQuery stdSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria);
            studentTransciptCriteria.addIn(Transcript.COL_STUDENT_OID, stdSubQuery);

            String currentCtxOid = getCurrentContext().getOid();
            studentTransciptCriteria.addEqualTo(Transcript.COL_DISTRICT_CONTEXT_OID, currentCtxOid);

            X2Criteria courseCriteria = getCopiedCourseCriteria();
            SubQuery crsSubQuery = new SubQuery(Course.class, X2BaseBean.COL_OID, courseCriteria);
            studentTransciptCriteria.addIn(Transcript.REL_MASTER_SCHEDULE + PATH_DELIMITER +
                    MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                    SchoolCourse.COL_COURSE_OID, crsSubQuery);

            QueryByCriteria stdTranscriptQuery = new QueryByCriteria(Transcript.class, studentTransciptCriteria);
            m_stdTranscriptMapWA =
                    getBroker().getGroupedCollectionByQuery(stdTranscriptQuery, Transcript.COL_STUDENT_OID, 1024);
        }

        /**
         * Initialize student schedule map.
         *
         * @param studentCriteria X2Criteria
         */
        private void initializeStudentScheduleMap(X2Criteria studentCriteria) {
            X2Criteria studentScheduleCriteria = new X2Criteria();
            SubQuery stdSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria);
            studentScheduleCriteria.addIn(StudentSchedule.COL_STUDENT_OID, stdSubQuery);

            String currentCtxOid = getCurrentContext().getOid();
            studentScheduleCriteria.addEqualTo(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.COL_DISTRICT_CONTEXT_OID, currentCtxOid);

            X2Criteria courseCriteria = getCopiedCourseCriteria();
            SubQuery crsSubQuery = new SubQuery(Course.class, X2BaseBean.COL_OID, courseCriteria);
            studentScheduleCriteria.addIn(StudentSchedule.REL_SECTION + PATH_DELIMITER +
                    MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                    SchoolCourse.COL_COURSE_OID, crsSubQuery);

            QueryByCriteria stdScheduleQuery = new QueryByCriteria(StudentSchedule.class, studentScheduleCriteria);
            m_stdScheduleMapWA =
                    getBroker().getGroupedCollectionByQuery(stdScheduleQuery, StudentSchedule.COL_STUDENT_OID, 1024);
        }

        /**
         * Checks if is covered by schedule tems.
         *
         * @param earnedCreditTerms List<String>
         * @param stdOid String
         * @return true, if is covered by schedule tems
         */
        private boolean isCoveredByScheduleTems(List<String> earnedCreditTerms, String stdOid) {
            boolean isCovered = false;
            Collection<StudentSchedule> stdSchedules = getAlgebra1StdScheduleWAByStdOid(stdOid);

            // scheduleTerms should be duplicate
            Set<String> scheduleTerms = new TreeSet<String>();
            for (StudentSchedule stdSchedule : stdSchedules) {
                scheduleTerms.addAll(getStdScheduleTermCodesWA(stdSchedule));
            }

            if (!scheduleTerms.isEmpty()) {
                isCovered = earnedCreditTerms.containsAll(scheduleTerms);
            }

            return isCovered;

        }
    }

    /**
     * Retrieve if a student pass algebra 1.
     *
     * @author X2 Development Corporation
     */
    public class RetrieverStdPassAlg implements FieldRetriever {
        public static final String ALIAS_CRDC_CRS_SUBJ_CODE = "all-crs-CRDCCourseCategory";
        public static final String ALIAS_GSG_NOT_COMPLETED = "all-gsg-notCompleted";
        public static final String CRDC_CODE_ALG1 = "Algebra I";

        public static final String RESULT_ENROLLED = "E";
        public static final String RESULT_NO = "N";
        public static final String RESULT_YES = "Y";


        Map<String, Collection<Transcript>> m_trnMapbyStdOid;
        GradesManager m_gradesManager;
        DataDictionaryField m_gsgNotCompleted;

        /**
         * Instantiates a new retriever std pass alg.
         */
        public RetrieverStdPassAlg() {
            super();

            m_gsgNotCompleted =
                    getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_GSG_NOT_COMPLETED);

            DataDictionaryField crsSubjField =
                    getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_CRDC_CRS_SUBJ_CODE);

            if (crsSubjField != null) {
                m_gradesManager = new GradesManager(CRDCStudentData.this.getBroker());

                X2Criteria trnCriteria = m_studentHelper.getStudentTranscriptCriteria().copy();
                trnCriteria.addEqualTo(Transcript.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER
                        + SchoolCourse.REL_COURSE + ModelProperty.PATH_DELIMITER + crsSubjField.getJavaName(),
                        CRDC_CODE_ALG1);
                trnCriteria.addEqualTo(Transcript.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());
                trnCriteria.addNotEmpty(Transcript.COL_FINAL_GRADE,
                        CRDCStudentData.this.getBroker().getPersistenceKey());

                m_trnMapbyStdOid = CRDCStudentData.this.getBroker().getGroupedCollectionByQuery(
                        new QueryByCriteria(Transcript.class, trnCriteria), Transcript.COL_STUDENT_OID, 1024);
            } else {
                m_trnMapbyStdOid = new HashMap();
            }
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String value = RESULT_NO;

            SisStudent student = (SisStudent) entity.getBean();
            Collection<Transcript> stdTranscripts = m_trnMapbyStdOid.get(student.getOid());

            if (stdTranscripts != null && !stdTranscripts.isEmpty()) {
                value = RESULT_ENROLLED;
                for (Transcript trn : stdTranscripts) {
                    TranscriptColumnDefinition finalColumn = trn.getTranscriptDefinition().getFinalColumnDefinition();

                    if (finalColumn != null) {
                        GradeScale gradeScale = finalColumn.getGradeScale();

                        if (gradeScale != null) {
                            String finalGrade = trn.getFinalGrade();
                            if (StringUtils.isNumeric(finalGrade) && gradeScale != null) {
                                // Try the final grade as a number.
                                BigDecimal gradeAsNumber = null;
                                try {
                                    gradeAsNumber = new BigDecimal(finalGrade);
                                } catch (NumberFormatException nfe) {
                                    // nothing. The grade is not numeric.
                                }

                                if (gradeAsNumber != null) {
                                    finalGrade = m_gradesManager.getLetterValue(gradeAsNumber, gradeScale,
                                            trn.getSchool(), trn.getSchoolCourseOid());
                                }
                            }
                            GradeScaleGradeDefinition gsg = m_gradesManager.getGradeDefinition(finalGrade, gradeScale,
                                    trn.getSchoolOid(), trn.getSchoolCourseOid());

                            if (gsg != null) {
                                if (gsg.getCreditIndicator()) {
                                    value = RESULT_YES;
                                    break;
                                } else if (m_gsgNotCompleted != null) {
                                    Object prop = data.getPropertyAsJavaType(gsg, m_gsgNotCompleted.getJavaName());
                                    if (prop != null) {
                                        if (prop instanceof Boolean) {
                                            if (((Boolean) prop).booleanValue()) {
                                                value = RESULT_NO;
                                            }
                                        } else {
                                            throw new IllegalStateException("The alias field " + ALIAS_GSG_NOT_COMPLETED
                                                    + " must be configured as a logical field");
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            return value;
        }
    }

    /**
     * Retriever if a student was retained during current CTX.
     *
     * @author X2 Development Corporation
     *
     */
    public class RetrieverStdRetained implements FieldRetriever {
        // Map<sklOid, calendarId> for most common calendar
        Map<String, String> m_calCodeMapBySkl = new HashMap();

        // Map<sklOid, SchoolCalendar> for most common calendar
        Map<String, SchoolCalendar> m_calMapBySkl = new HashMap();

        // Map<sklOid, calendarId, first Date>
        Map<String, Map<String, PlainDate>> m_csdMapBySklByCode = new HashMap();
        DistrictSchoolYearContext m_nextContext;

        /**
         * Instantiates a new retriever std retained.
         */
        public RetrieverStdRetained() {
            super();

            if (initNextContext()) {
                initiMostCommonCalendarsForNextYear();
                initSchoolCalendarDateForNextYear();
            } else {
                CRDCStudentData.this.addSetupError(this.getClass().getName(), "Next Year context is required");
            }
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            boolean value = false;
            SisStudent student = (SisStudent) entity.getBean();
            CRDCStudentData stdData = (CRDCStudentData) data;

            if (!StringUtils.isEmpty(m_selSnapRetained)) {
                value = m_stdSnapRetained.contains(student.getOid());
            } else {
                StudentEnrollment mainEnr =
                        m_studentHelper.getEnrollmentForDate(student.getOid(), stdData.m_reportDatePart1, "EWSY");
                if (mainEnr != null) {
                    int yog = mainEnr.getYog();
                    PlainDate mainEnrDate = mainEnr.getEnrollmentDate();
                    PlainDate firstNextYearDate = getFirstDaySchoolNextYear(student.getSchool());

                    StudentEnrollment nextEnr =
                            m_studentHelper.getEnrollmentForDate(student.getOid(), firstNextYearDate, "EWSY");
                    if (nextEnr != null && nextEnr.getEnrollmentDate().after(mainEnrDate) && yog < nextEnr.getYog()) {
                        value = true;
                    }
                }
            }

            return Boolean.valueOf(value);
        }

        /**
         * Return first in-session day for the next school year.
         *
         * @param school SisSchool
         * @return Plain date
         */
        protected PlainDate getFirstDaySchoolNextYear(SisSchool school) {
            PlainDate value = m_nextContext.getStartDate();

            SchoolCalendar calendar = m_calMapBySkl.get(school.getOid());
            String calendarCode = m_calCodeMapBySkl.get(school.getOid());

            if (calendar != null && calendarCode != null && m_csdMapBySklByCode.get(school.getOid()) != null) {

                PlainDate date = m_csdMapBySklByCode.get(school.getOid()).get(calendarCode);

                if (date != null) {
                    value = date;
                }
            }
            return value;
        }

        /**
         * Build map of maps of SchoolCalendars keyed on school oid and school calendar id for the
         * next yeaar.
         *
         * @return Map<String, Map<String, Collection<SchoolCalendar>>>
         */
        protected Map<String, Map<String, Collection<SchoolCalendar>>> getSchoolCalendarsForNextYear() {
            X2Criteria casCriteria = new X2Criteria();

            casCriteria.addEqualTo(SchoolCalendar.COL_DISTRICT_CONTEXT_OID, m_nextContext.getOid());
            // Filter to eliminate unused schools.
            casCriteria.addNotEqualTo(SchoolCalendar.REL_SCHOOL + ModelProperty.PATH_DELIMITER +
                    SisSchool.COL_INACTIVE_INDICATOR,
                    Boolean.TRUE);
            casCriteria.addNotEqualTo(SchoolCalendar.REL_SCHOOL + ModelProperty.PATH_DELIMITER +
                    SisSchool.COL_ARCHIVE_INDICATOR,
                    Boolean.TRUE);

            QueryByCriteria query = new QueryByCriteria(SchoolCalendar.class, casCriteria);

            return getBroker().getGroupedCollectionByQuery(query, new String[] {SchoolCalendar.COL_SCHOOL_OID,
                    SchoolCalendar.COL_CALENDAR_ID}, new int[] {100, 5});
        }

        /**
         * Initialize most common calendars keyed on SKL OID and valued on SchoolCalendar and
         * SchoolCalendarId.
         */
        protected void initiMostCommonCalendarsForNextYear() {
            Map<String, Map<String, Collection<SchoolCalendar>>> mapSchoolCalendars = getSchoolCalendarsForNextYear();

            String activeStatus = PreferenceManager.getPreferenceValue(getOrganization(),
                    SystemPreferenceDefinition.STUDENT_ACTIVE_CODE);

            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(SisStudent.COL_ENROLLMENT_STATUS, activeStatus);

            String[] columns = new String[] {SisStudent.COL_SCHOOL_OID, SisStudent.COL_CALENDAR_CODE, "count(*)"};

            ReportQueryByCriteria query = new ReportQueryByCriteria(SisStudent.class, columns, criteria);
            query.addGroupBy(SisStudent.COL_SCHOOL_OID);
            query.addGroupBy(SisStudent.COL_CALENDAR_CODE);
            query.addOrderByDescending("count(*)");

            ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);
            try {
                while (iterator.hasNext()) {
                    Object[] row = (Object[]) iterator.next();
                    String schoolOid = (String) row[0];
                    String calendarCode = (String) row[1];

                    if (!m_calMapBySkl.containsKey(schoolOid)) {
                        Map<String, Collection<SchoolCalendar>> mapCalendars = mapSchoolCalendars.get(schoolOid);
                        if (mapCalendars != null && mapCalendars.containsKey(calendarCode)) {
                            SchoolCalendar schoolCalendar = mapCalendars.get(calendarCode).iterator().next();
                            m_calCodeMapBySkl.put(schoolOid, schoolCalendar.getCalendarId());
                            m_calMapBySkl.put(schoolOid, schoolCalendar);
                        }
                    }
                }
            } finally {
                iterator.close();
            }

            // Add schools without students - any calendar will do
            for (String oid : mapSchoolCalendars.keySet()) {
                if (!m_calMapBySkl.containsKey(oid)) {
                    Map<String, Collection<SchoolCalendar>> mapCalendars = mapSchoolCalendars.get(oid);
                    SchoolCalendar schoolCalendar = mapCalendars.values().iterator().next().iterator().next();
                    m_calMapBySkl.put(oid, schoolCalendar);
                    m_calCodeMapBySkl.put(oid, schoolCalendar.getCalendarId());
                }
            }
        }

        /**
         * Initialize next CTX.
         *
         * @return true, if successful
         */
        protected boolean initNextContext() {
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(DistrictSchoolYearContext.COL_SCHOOL_YEAR,
                    Integer.valueOf(getCurrentContext().getSchoolYear() + 1));
            m_nextContext = (DistrictSchoolYearContext) getBroker()
                    .getBeanByQuery(new QueryByCriteria(DistrictSchoolYearContext.class, criteria));
            return m_nextContext == null ? false : true;
        }

        /**
         * Initialize SchoolCalendarDates keyed on SKL OID and Calendar Code.
         */
        protected void initSchoolCalendarDateForNextYear() {
            List<String> oids = new LinkedList();
            for (SchoolCalendar cal : m_calMapBySkl.values()) {
                oids.add(cal.getOid());
            }
            X2Criteria csdCriteria = new X2Criteria();
            csdCriteria.addIn(SchoolCalendarDate.COL_SCHOOL_CALENDAR_OID, oids);

            QueryByCriteria csdQuery = new QueryByCriteria(SchoolCalendarDate.class, csdCriteria);
            csdQuery.addOrderByAscending(SchoolCalendarDate.COL_DATE);

            QueryIterator iterator = CRDCStudentData.this.getBroker().getIteratorByQuery(csdQuery);
            try {
                while (iterator.hasNext()) {
                    SchoolCalendarDate record = (SchoolCalendarDate) iterator.next();
                    if (isValid(record)) {
                        String sklOid = record.getSchoolCalendar().getSchoolOid();
                        String calendarId = record.getSchoolCalendar().getCalendarId();
                        PlainDate date = record.getDate();

                        Map<String, PlainDate> calendarMap = m_csdMapBySklByCode.get(sklOid);
                        if (calendarMap == null) {
                            calendarMap = new HashMap();
                            m_csdMapBySklByCode.put(sklOid, calendarMap);
                        }
                        PlainDate currentDate = calendarMap.get(calendarId);
                        if (currentDate == null) {
                            calendarMap.put(calendarId, date);
                        }
                    }
                }
            } finally {
                iterator.close();
            }
        }

        /**
         * Checks if is valid.
         *
         * @param record SchoolCalendarDate
         * @return true, if is valid
         */
        protected boolean isValid(SchoolCalendarDate record) {
            return record.getInSessionIndicator();
        }

    }

    /**
     * Get the school oid based on the school where the student was enrolled on the Part 1 date.
     * If the student was not yet enrolled on the part 1 date, use the school where the student
     * first enrolled.
     *
     * @author Follett Software Company
     */
    public class RetrieverStdSchool implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String value = "";

            CRDCStudentData stdCRDCData = (CRDCStudentData) data;
            SisStudent student = (SisStudent) entity.getBean();
            StudentEnrollment enr = null;
            List<StudentEnrollment> enrollments = m_studentHelper.getStudentEnrollments(student);
            if (enrollments != null) {
                for (StudentEnrollment enrollment : enrollments) {
                    enr = enrollment;
                    if (enrollment.getEnrollmentDate() != null
                            && !enrollment.getEnrollmentDate().after(stdCRDCData.m_reportDatePart1)) {
                        break;
                    }
                }
            }
            if (enr != null) {
                value = enr.getSchoolOid();
            }

            return value;
        }
    }

    /**
     * Retrive if student has specific student schedule in reporting year based on passed alias.
     *
     * @author Follett Software Company
     */
    public class RetrieverStdSscIsYr extends RetrieverByAlias {

        /**
         * Instantiates a new retriever std ssc is yr.
         */
        public RetrieverStdSscIsYr() {
            m_scheduleHelper = getSchedHistoryHelper(getCurrentContext().getEndDate());

            X2Criteria andCriteria = new X2Criteria();
            for (String alias : m_usedAliases) {
                String javaName = translateAliasToJavaName(alias, true);
                X2Criteria orCriteria = new X2Criteria();
                orCriteria.addEqualTo(javaName, BooleanAsStringConverter.TRUE);
                andCriteria.addOrCriteria(orCriteria);
            }
            m_scheduleHelper.getStudentScheduleCriteria().addAndCriteria(andCriteria);


            // Since property evaluated is on SSC, schedule change records cannot be used to
            // generate value so none will be loaded
            m_scheduleHelper.getStudentScheduleChangeCriteria().addEqualTo(X2BaseBean.COL_OID, "--dummy--");
        }

        private CRDCStudentHistoryHelper m_scheduleHelper = null;

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();

            String paramAlias = (String) field.getParameter();
            String javaName = translateAliasToJavaName(paramAlias, true);
            List<StudentScheduleSpan> spans = m_scheduleHelper.getStudentScheduleSpans(student);

            String value = BooleanAsStringConverter.FALSE;

            for (StudentScheduleSpan span : spans) {
                StudentSchedule schedule = span.getSchedule();
                if (schedule != null) {
                    String sscIndicator = (String) schedule.getFieldValueByBeanPath(javaName);
                    if (BooleanAsStringConverter.TRUE.equals(sscIndicator)) {
                        value = BooleanAsStringConverter.TRUE;
                        break;
                    }
                }
            }

            return value;
        }
    }

    /**
     * Retriever to determine count of incidents for victim.
     *
     * @author X2 Development Corporation
     *
     */
    public class RetrieverVictimCnt implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            int count = 0;
            String code = field.getParameter() == null ? null : field.getParameter().toString();
            Set<ConductIncident> incidents =
                    CRDCStudentData.this.m_conductHelper.getIncidentsForVictim(entity.getBean().getOid(), code);
            if (incidents != null) {
                count = incidents.size();
            }
            return Integer.valueOf(count);
        }
    }

    /**
     * RI Retriever to determine if a student is in 504 Section.
     *
     * @author X2 Development Corporation
     *
     */
    public class RIRetriever504 implements FieldRetriever {
        public static final String STD_504_STATUS_ACTIVE = "Active";

        /**
         * Instantiates a new RI retriever 504.
         */
        public RIRetriever504() {
            super();
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();
            String stdOid = student.getOid();

            if (m_stdSnap504 != null) {
                return Boolean.valueOf(m_stdSnap504.contains(stdOid));
            }

            boolean value = false;

            String retievedIdea = entity.getFieldValue(FIELD_IDEA);

            if (CODE_YES.equals(retievedIdea)) {
                return Boolean.valueOf(value);
            }

            if (STD_504_STATUS_ACTIVE.equals(student.getSection504StatusCode())) {
                value = true;
            }

            return Boolean.valueOf(value);
        }
    }

    /**
     * RI Retriever to determine if a student is enrolled in distance education.
     *
     * @author X2 Development Corporation
     *
     */
    public class RIRetrieverEnrDistEd extends RetrieverStdCrs {
        public static final String ALIAS_MST_SETTING_ID = "DOE SECTION SETTING ID";
        public static final String CRS_SETTING_ID_STATE_CODE = "ONLINE";

        /**
         * Apply criteria.
         *
         * @see com.x2dev.procedures.statereporting.CRDCStudentData.RetrieverStdCrs#applyCriteria()
         */
        @Override
        void applyCriteria() {
            Collection<String> codes = new ArrayList();
            codes.add("__dummyCode__");
            String fieldName = CRDCStudentData.this.translateAliasToJavaName(ALIAS_MST_SETTING_ID, true);

            DataDictionaryField mstSettingId = getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_MST_SETTING_ID);

            if (mstSettingId != null && !StringUtils.isEmpty(mstSettingId.getReferenceTableOid())) {
                X2Criteria criteria = new X2Criteria();
                criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, mstSettingId.getReferenceTableOid());
                criteria.addEqualToIgnoreCase(ReferenceCode.COL_STATE_CODE, CRS_SETTING_ID_STATE_CODE);

                QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, criteria);

                Collection<ReferenceCode> refCodes = CRDCStudentData.this.getBroker().getCollectionByQuery(query);

                for (ReferenceCode code : refCodes) {
                    codes.add(code.getCode());
                }
            }
            this.addInSectionField(fieldName, codes);
        }

    }

    /**
     * RI Retriever to determine if a student is GED preparation or completed.
     *
     * @author X2 Development Corporation
     *
     */
    public class RIRetrieverGED implements FieldRetriever {
        public static final String ALIAS_ENR_EXIT_TYPE = "RI Exit Type";
        public static final String ENR_STATE_CODE_GED_PREP = "23";

        Set<String> m_studentOids = new HashSet();

        /**
         * Instantiates a new RI retriever GED.
         */
        public RIRetrieverGED() {
            super();
            String field = translateAliasToJavaName(ALIAS_ENR_EXIT_TYPE, true);
            DataDictionaryField mstSettingId = getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_ENR_EXIT_TYPE);

            if (mstSettingId != null && !StringUtils.isEmpty(mstSettingId.getReferenceTableOid())) {
                X2Criteria rcdCriteria = new X2Criteria();
                rcdCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, mstSettingId.getReferenceTableOid());
                rcdCriteria.addEqualTo(ReferenceCode.COL_STATE_CODE, ENR_STATE_CODE_GED_PREP);

                X2Criteria enrCriteria = new X2Criteria();
                enrCriteria.addIn(field, new SubQuery(ReferenceCode.class, ReferenceCode.COL_CODE, rcdCriteria));

                QueryByCriteria query = new QueryByCriteria(StudentEnrollment.class, enrCriteria);

                Collection<StudentEnrollment> enrollments =
                        CRDCStudentData.this.getBroker().getCollectionByQuery(query);
                for (StudentEnrollment record : enrollments) {
                    if (record.getEnrollmentDate() != null
                            && !record.getEnrollmentDate().after(getCurrentContext().getEndDate())) {
                        if (!record.getEnrollmentDate().before(getCurrentContext().getStartDate())) {
                            m_studentOids.add(record.getStudentOid());
                        } else {
                            // Check if most recent enrollment before start of this year is GED Prep
                            X2Criteria criteria = new X2Criteria();
                            criteria.addEqualTo(StudentEnrollment.COL_STUDENT_OID, record.getStudentOid());
                            criteria.addLessThan(StudentEnrollment.COL_ENROLLMENT_DATE,
                                    getCurrentContext().getStartDate());
                            QueryByCriteria enrQuery = new QueryByCriteria(StudentEnrollment.class, criteria);
                            enrQuery.addOrderByDescending(StudentEnrollment.COL_ENROLLMENT_DATE);
                            enrQuery.addOrderByDescending(X2BaseBean.COL_LAST_MODIFIED_TIME);
                            QueryIterator enrIter = CRDCStudentData.this.getBroker().getIteratorByQuery(enrQuery);
                            try {
                                if (enrIter.hasNext()) {
                                    StudentEnrollment enr = (StudentEnrollment) enrIter.next();
                                    if (ENR_STATE_CODE_GED_PREP.equals(enr.getFieldValueByBeanPath(field))) {
                                        m_studentOids.add(record.getStudentOid());
                                    }
                                }
                            } finally {
                                enrIter.close();
                            }
                        }
                    }
                }
            }
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            boolean value = false;
            SisStudent student = (SisStudent) entity.getBean();

            if (m_studentOids.contains(student.getOid())) {
                value = true;
            }
            return Boolean.valueOf(value);
        }
    }

    /**
     * RI Retriever to determine if a student is GED completed.
     *
     * @author X2 Development Corporation
     *
     */
    public class RIRetrieverHSEquiv implements FieldRetriever {
        public static final String ALIAS_ENR_EXIT_TYPE = "RI Exit Type";
        public static final String ENR_STATE_CODE_GED_COMPL = "32";

        Set<String> m_studentOids = new HashSet();

        /**
         * Instantiates a new RI retriever HS equiv.
         */
        public RIRetrieverHSEquiv() {
            super();
            String field = translateAliasToJavaName(ALIAS_ENR_EXIT_TYPE, true);
            DataDictionaryField mstSettingId = getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_ENR_EXIT_TYPE);

            if (mstSettingId != null && !StringUtils.isEmpty(mstSettingId.getReferenceTableOid())) {
                X2Criteria rcdCriteria = new X2Criteria();
                rcdCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, mstSettingId.getReferenceTableOid());
                rcdCriteria.addEqualTo(ReferenceCode.COL_STATE_CODE, ENR_STATE_CODE_GED_COMPL);

                X2Criteria enrCriteria = new X2Criteria();
                enrCriteria.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE,
                        getCurrentContext().getStartDate());
                enrCriteria.addLessOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, getCurrentContext().getEndDate());
                enrCriteria.addIn(field, new SubQuery(ReferenceCode.class, ReferenceCode.COL_CODE, rcdCriteria));

                QueryByCriteria query = new QueryByCriteria(StudentEnrollment.class, enrCriteria);

                QueryIterator iterator = CRDCStudentData.this.getBroker().getIteratorByQuery(query);
                try {
                    while (iterator.hasNext()) {
                        StudentEnrollment record = (StudentEnrollment) iterator.next();
                        m_studentOids.add(record.getStudentOid());
                    }
                } finally {
                    iterator.close();
                }
            }
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            boolean value = false;
            SisStudent student = (SisStudent) entity.getBean();

            if (m_studentOids.contains(student.getOid())) {
                value = true;
            }
            return Boolean.valueOf(value);
        }
    }

    /**
     * RI Retriever to determine if a student is in IB program.
     *
     * @author X2 Development Corporation
     *
     */
    public class RIRetrieverIbPgm implements FieldRetriever {
        public static final String ALIAS_CRS_POST_SEC_PGM = "DOE POST SECONDARY CREDIT";
        public static final String CRS_POST_SEC_STATE_CODE = "IB";

        Map<String, Collection<Transcript>> m_trnByStdOid;

        /**
         * Instantiates a new RI retriever ib pgm.
         */
        public RIRetrieverIbPgm() {
            super();

            DataDictionaryField crsSecPgm = getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_CRS_POST_SEC_PGM);

            if (crsSecPgm != null && !StringUtils.isEmpty(crsSecPgm.getReferenceTableOid())) {

                X2Criteria criteria = new X2Criteria();
                criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, crsSecPgm.getReferenceTableOid());
                criteria.addEqualToIgnoreCase(ReferenceCode.COL_STATE_CODE, CRS_POST_SEC_STATE_CODE);

                QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, criteria);

                ReferenceCode refCode = (ReferenceCode) CRDCStudentData.this.getBroker().getBeanByQuery(query);

                X2Criteria trnCriteria = m_studentHelper.getStudentTranscriptCriteria().copy();

                SubQuery studentSubQuery =
                        new SubQuery(SisStudent.class, X2BaseBean.COL_OID, m_studentHelper.getStudentCriteria());

                trnCriteria.addIn(Transcript.COL_STUDENT_OID, studentSubQuery);
                trnCriteria.addEqualTo(Transcript.REL_SCHOOL_COURSE
                        + ModelProperty.PATH_DELIMITER
                        + SchoolCourse.REL_COURSE
                        + ModelProperty.PATH_DELIMITER
                        + crsSecPgm.getJavaName(), refCode != null ? refCode.getCode() : "__dummyCode__");

                QueryByCriteria trnQuery = new QueryByCriteria(Transcript.class, trnCriteria);
                m_trnByStdOid = CRDCStudentData.this.getBroker().getGroupedCollectionByQuery(trnQuery,
                        Transcript.COL_STUDENT_OID,
                        1024);
            }
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
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

                Collection<Transcript> transcripts = m_trnByStdOid.get(student.getOid());

                if (transcripts != null && !transcripts.isEmpty()) {
                    value = true;
                }
            }
            return Boolean.valueOf(value);
        }
    }

    /**
     * RI Retriever to determine if a student is in IDEA special education.
     *
     * @author X2 Development Corporation
     *
     */
    public class RIRetrieverIdea implements FieldRetriever {
        public static final String STD_SPED_STATUS_ACTIVE = "Active";
        public static final String STD_SPED_STATUS_EXITED = "Exited";

        /**
         * Instantiates a new RI retriever idea.
         */
        public RIRetrieverIdea() {
            super();
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();
            String stdOid = student.getOid();

            if (m_stdSnapIDEA != null) {
                return Boolean.valueOf(m_stdSnapIDEA.contains(stdOid));
            }

            boolean value = false;

            if (STD_SPED_STATUS_ACTIVE.equals(student.getSpedStatusCode())) {
                value = true;
            } else if (STD_SPED_STATUS_EXITED.equals(student.getSpedStatusCode()) &&
                    student.getSpedInitialEligibilityDate() != null &&
                    student.getSpedInitialEligibilityDate().before(m_reportDatePart1) &&
                    student.getSpedExitDate() != null &&
                    student.getSpedExitDate().after(m_reportDatePart1)) {
                value = true;
            }
            return Boolean.valueOf(value);
        }
    }

    /**
     * RI Retriever to determine if a student has LEP status.
     *
     * @author Follett Software Company
     */
    public class RIRetrieverLep implements FieldRetriever {

        public static final String ALIAS_ESL_FLAG = "ESL Flag";
        public static final String PARAM_LEP = "LEP";

        private String m_fieldEslFlag;

        /**
         * Instantiates a new RI retriever lep.
         */
        public RIRetrieverLep() {
            m_fieldEslFlag = translateAliasToJavaName(ALIAS_ESL_FLAG, true);
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();
            String stdOid = student.getOid();

            if (PARAM_LEP.equals(field.getParameter()) && m_stdSnapLEP != null) {
                return Boolean.valueOf(m_stdSnapLEP.contains(stdOid));
            }

            if (PARAM_LEP_ENROLLED.equals(field.getParameter()) && m_stdSnapLEPPgm != null) {
                return Boolean.valueOf(m_stdSnapLEPPgm.contains(stdOid));
            }

            boolean value = false;
            Object fieldValue = data.getPropertyAsJavaType(entity.getBean(), m_fieldEslFlag);
            if (fieldValue instanceof String) {
                value = BooleanAsStringConverter.TRUE.equals(fieldValue);
            } else if (fieldValue instanceof Boolean) {
                value = ((Boolean) fieldValue).booleanValue();
            }
            return value ? Boolean.TRUE : Boolean.FALSE;
        }

    }

    /**
     * Retrieve if a student is in 504 special education.
     *
     * @author X2 Development Corporation
     *
     */
    public class TNRetriever504 implements FieldRetriever {

        public static final String STATE_CODE_504 = "504";

        Collection<String> m_studentsWith504 = new ArrayList<String>();

        /**
         * Instantiates a new CA retriever 504.
         */
        public TNRetriever504() {
            super();

            DataDictionaryField programCodeField =
                    getDataDictionary().findDataDictionaryField(StudentProgramParticipation.class.getName(),
                            StudentProgramParticipation.COL_PROGRAM_CODE);

            if (!StringUtils.isEmpty(programCodeField.getReferenceTableOid())) {

                X2Criteria criteria = new X2Criteria();
                criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, programCodeField.getReferenceTableOid());
                criteria.addEqualTo(ReferenceCode.COL_STATE_CODE, STATE_CODE_504);
                SubQuery subQuery = new SubQuery(ReferenceCode.class, ReferenceCode.COL_CODE, criteria);
                Collection<String> codes504 = getBroker().getSubQueryCollectionByQuery(subQuery);

                X2Criteria pgmCriteria = new X2Criteria();
                X2Criteria endDateCriteria = new X2Criteria();
                endDateCriteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_END_DATE, m_reportDatePart1);
                X2Criteria orCriteria = new X2Criteria();
                orCriteria.addEmpty(StudentProgramParticipation.COL_END_DATE, getBroker().getPersistenceKey());
                endDateCriteria.addOrCriteria(orCriteria);
                pgmCriteria.addAndCriteria(endDateCriteria);
                pgmCriteria.addIn(StudentProgramParticipation.COL_PROGRAM_CODE, codes504);

                QueryByCriteria pgmQuery = new QueryByCriteria(StudentProgramParticipation.class, pgmCriteria);
                QueryIterator iterator = CRDCStudentData.this.getBroker().getIteratorByQuery(pgmQuery);
                try {
                    while (iterator.hasNext()) {
                        StudentProgramParticipation pgm = (StudentProgramParticipation) iterator.next();
                        if (pgm != null && pgm.getEndDate() != null) {
                            long startDateMillis = pgm.getEndDate().getTime();
                            long reportDatePart1Millis = m_reportDatePart1.getTime();

                            if (startDateMillis >= reportDatePart1Millis) {
                                m_studentsWith504.add(pgm.getStudentOid());
                            }
                        }
                    }
                } finally {
                    iterator.close();
                }
            }
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            return Boolean.valueOf(m_studentsWith504.contains(entity.getBean().getOid()));
        }
    }

    /**
     * Retrieve if a student participate in Credit Recovery Program.
     *
     * @author X2 Development Corporation
     *
     */
    public class TNRetrCreditRecov implements FieldRetriever {
        public static final String ALIAS_TRN_CREDIR_REC_PGM = "all-trn-CreditRecoveryProgram";

        Map<String, Collection<Transcript>> m_trnMapByStdOid;

        /**
         * Instantiates a new TN retr credit recov.
         */
        public TNRetrCreditRecov() {
            super();

            DataDictionaryField trnCreditRecField =
                    getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_TRN_CREDIR_REC_PGM);

            if (trnCreditRecField != null) {
                X2Criteria trnCriteria = new X2Criteria();
                trnCriteria.addEqualTo(Transcript.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());
                trnCriteria.addEqualTo(trnCreditRecField.getJavaName(), BooleanAsStringConverter.TRUE);

                m_trnMapByStdOid = CRDCStudentData.this.getBroker().getGroupedCollectionByQuery(
                        new QueryByCriteria(Transcript.class, trnCriteria),
                        Transcript.COL_STUDENT_OID, 1024);
            }
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            boolean value = false;
            SisStudent std = (SisStudent) entity.getBean();

            if (m_trnMapByStdOid != null) {
                Collection<Transcript> trnsfoStd = m_trnMapByStdOid.get(std.getOid());

                if (trnsfoStd != null && !trnsfoStd.isEmpty()) {
                    value = true;
                }
            }
            return Boolean.valueOf(value);
        }
    }

    /**
     * Retrieve if a student enrolled in distance education.
     *
     * @author X2 Development Corporation
     *
     */
    public class TNRetrieverDistEd extends RetrieverStdCrs {
        public static final String ALIAS_DOE_TEACH_METHOD = "DOE TEACHING METHOD";
        public static final String ALIAS_DOE_TEACH_METHOD_OVERRIDE = "DOE TEACHING METHOD OVERRIDE";
        public static final String CRDC_CODE_DISTANCE = "Distance";

        private String m_fieldMethod;
        private String m_fieldOverride;

        /**
         * Instantiates a new TN retriever dist ed.
         */
        public TNRetrieverDistEd() {
            m_fieldMethod = translateAliasToJavaName(ALIAS_DOE_TEACH_METHOD, true);
            m_fieldOverride = translateAliasToJavaName(ALIAS_DOE_TEACH_METHOD_OVERRIDE, true);
        }

        /**
         * Apply criteria.
         *
         * @see com.x2dev.procedures.statereporting.CRDCStudentData.RetrieverStdCrs#applyCriteria()
         */
        @Override
        void applyCriteria() {
            if (m_fieldMethod == null) {
                m_fieldMethod = translateAliasToJavaName(ALIAS_DOE_TEACH_METHOD, true);
                m_fieldOverride = translateAliasToJavaName(ALIAS_DOE_TEACH_METHOD_OVERRIDE, true);
            }
            DataDictionaryField teachMethodField =
                    getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_DOE_TEACH_METHOD);
            Collection<String> crsCodeForTeachMethod = new ArrayList();

            if (teachMethodField != null) {
                ReferenceTable refTable = teachMethodField.getReferenceTable();
                if (refTable != null) {
                    Collection<ReferenceCode> refCodes = refTable.getReferenceCodes();

                    if (refCodes != null) {
                        for (ReferenceCode refCode : refCodes) {
                            String crdcCode = (String) refCode.getFieldValueByAlias(ALIAS_CRDC_REF_CODE);

                            if (CRDC_CODE_DISTANCE.equalsIgnoreCase(crdcCode)) {
                                crsCodeForTeachMethod.add(refCode.getCode());
                            }
                        }
                    }
                }

                if (!crsCodeForTeachMethod.isEmpty()) {
                    if (m_tnHelper == null) {
                        m_tnHelper = getSchedHistoryHelper(m_reportDatePart1);
                    }

                    X2Criteria case1Criteria = new X2Criteria();
                    case1Criteria.addIn(StudentScheduleChange.REL_MASTER_SCHEDULE + ModelProperty.PATH_DELIMITER +
                            MasterSchedule.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER +
                            SchoolCourse.REL_COURSE + ModelProperty.PATH_DELIMITER +
                            m_fieldMethod, crsCodeForTeachMethod);
                    case1Criteria.addEmpty(StudentScheduleChange.REL_MASTER_SCHEDULE + ModelProperty.PATH_DELIMITER +
                            m_fieldOverride, getBroker().getPersistenceKey());

                    X2Criteria case2Criteria = new X2Criteria();
                    case2Criteria.addIn(StudentScheduleChange.REL_MASTER_SCHEDULE + ModelProperty.PATH_DELIMITER +
                            m_fieldOverride, crsCodeForTeachMethod);

                    case1Criteria.addOrCriteria(case2Criteria);
                    m_tnHelper.getStudentScheduleChangeCriteria().addAndCriteria(case1Criteria);


                    case1Criteria = new X2Criteria();
                    case1Criteria.addIn(StudentSchedule.REL_SECTION + ModelProperty.PATH_DELIMITER +
                            MasterSchedule.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER +
                            SchoolCourse.REL_COURSE + ModelProperty.PATH_DELIMITER +
                            m_fieldMethod, crsCodeForTeachMethod);
                    case1Criteria.addEmpty(StudentSchedule.REL_SECTION + ModelProperty.PATH_DELIMITER +
                            m_fieldOverride, getBroker().getPersistenceKey());

                    case2Criteria = new X2Criteria();
                    case2Criteria.addIn(StudentSchedule.REL_SECTION + ModelProperty.PATH_DELIMITER +
                            m_fieldOverride, crsCodeForTeachMethod);

                    case1Criteria.addOrCriteria(case2Criteria);
                    m_tnHelper.getStudentScheduleCriteria().addAndCriteria(case1Criteria);
                }
            }
        }
    }

    /**
     * Retrieve if a student is dual enrolled.
     *
     * @author X2 Development Corporation
     *
     */
    public class TNRetrieverDualEnr extends RetrieverStdCrs {

        protected static final String ALIAS_DUAL_CREDIT_CRS = "all-crs-DualEnrollmentCredit";
        protected static final String ALIAS_DUAL_CREDIT_CSK = "all-csk-DualEnrollmentCredit";
        protected static final String ALIAS_DUAL_CREDIT_MST = "all-mst-DualEnrollmentCredit";

        public static final String CRDC_CODE_DUAL = "Dual";

        /**
         * Apply criteria.
         *
         * @see com.x2dev.procedures.statereporting.CRDCStudentData.RetrieverStdCrs#applyCriteria()
         */
        @Override
        void applyCriteria() {

            TreeMap<String, String> mstCodeForDualEnrMap = getCodeForDualEnrollment(ALIAS_DUAL_CREDIT_MST);
            TreeMap<String, String> crsCodeForDualEnrMap = getCodeForDualEnrollment(ALIAS_DUAL_CREDIT_CRS);
            TreeMap<String, String> cskCodeForDualEnrMap = getCodeForDualEnrollment(ALIAS_DUAL_CREDIT_CSK);

            if (m_tnHelper == null) {
                m_tnHelper = getSchedHistoryHelper(m_reportDatePart1);
            }

            X2Criteria sccAndFieldsCriteria = new X2Criteria();
            X2Criteria sscAndFieldsCriteria = new X2Criteria();

            if (mstCodeForDualEnrMap != null) {
                X2Criteria sccOrMstCriteria = new X2Criteria();
                sccOrMstCriteria.addEqualTo(
                        StudentScheduleChange.REL_MASTER_SCHEDULE + ModelProperty.PATH_DELIMITER +
                                mstCodeForDualEnrMap.firstEntry().getKey(),
                        mstCodeForDualEnrMap.firstEntry().getValue());
                sccAndFieldsCriteria.addOrCriteria(sccOrMstCriteria);

                X2Criteria sscOrMstCriteria = new X2Criteria();
                sscOrMstCriteria.addEqualTo(
                        StudentSchedule.REL_SECTION + ModelProperty.PATH_DELIMITER +
                                mstCodeForDualEnrMap.firstEntry().getKey(),
                        mstCodeForDualEnrMap.firstEntry().getValue());
                sscAndFieldsCriteria.addOrCriteria(sscOrMstCriteria);

            }

            if (cskCodeForDualEnrMap != null) {
                X2Criteria sccOrCskCriteria = new X2Criteria();
                sccOrCskCriteria.addEqualTo(
                        StudentScheduleChange.REL_MASTER_SCHEDULE + ModelProperty.PATH_DELIMITER +
                                MasterSchedule.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER +
                                cskCodeForDualEnrMap.firstEntry().getKey(),
                        mstCodeForDualEnrMap.firstEntry().getValue());
                sccAndFieldsCriteria.addOrCriteria(sccOrCskCriteria);

                X2Criteria sscOrCskCriteria = new X2Criteria();
                sscOrCskCriteria.addEqualTo(
                        StudentSchedule.REL_SECTION + ModelProperty.PATH_DELIMITER + MasterSchedule.REL_SCHOOL_COURSE +
                                ModelProperty.PATH_DELIMITER + cskCodeForDualEnrMap.firstEntry().getKey(),
                        mstCodeForDualEnrMap.firstEntry().getValue());
                sscAndFieldsCriteria.addOrCriteria(sscOrCskCriteria);
            }

            if (crsCodeForDualEnrMap != null) {
                X2Criteria sccOrCrsCriteria = new X2Criteria();
                sccOrCrsCriteria.addEqualTo(StudentScheduleChange.REL_MASTER_SCHEDULE + ModelProperty.PATH_DELIMITER +
                        MasterSchedule.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER + SchoolCourse.REL_COURSE +
                        ModelProperty.PATH_DELIMITER + crsCodeForDualEnrMap.firstEntry().getKey(),
                        crsCodeForDualEnrMap.firstEntry().getValue());
                sccAndFieldsCriteria.addOrCriteria(sccOrCrsCriteria);

                X2Criteria sscOrCrsCriteria = new X2Criteria();
                sscOrCrsCriteria.addEqualTo(
                        StudentSchedule.REL_SECTION + ModelProperty.PATH_DELIMITER + MasterSchedule.REL_SCHOOL_COURSE +
                                ModelProperty.PATH_DELIMITER + SchoolCourse.REL_COURSE + ModelProperty.PATH_DELIMITER +
                                crsCodeForDualEnrMap.firstEntry().getKey(),
                        crsCodeForDualEnrMap.firstEntry().getValue());
                sscAndFieldsCriteria.addOrCriteria(sscOrCrsCriteria);

            }

            m_tnHelper.getStudentScheduleChangeCriteria().addAndCriteria(sccAndFieldsCriteria);
            m_tnHelper.getStudentScheduleCriteria().addAndCriteria(sscAndFieldsCriteria);
        }

        /**
         * Do span dates check.
         *
         * @return true, if successful
         * @see com.x2dev.procedures.statereporting.CRDCStudentData.RetrieverStdCrs#doSpanDatesCheck()
         */
        @Override
        boolean doSpanDatesCheck() {
            return true;
        }

        /**
         * Returns map keyed on java name of the field by given alias and valued by found CRDC code.
         *
         * @param alias String
         * @return Tree map
         */
        private TreeMap<String, String> getCodeForDualEnrollment(String alias) {
            DataDictionaryField dualEnrField = getDataDictionary().findDataDictionaryFieldByAlias(alias);
            String codeForDualEnr = null;
            TreeMap<String, String> codeByFieldMap = null;
            if (dualEnrField != null) {
                ReferenceTable refTable = dualEnrField.getReferenceTable();
                if (refTable != null) {
                    Collection<ReferenceCode> refCodes = refTable.getReferenceCodes();

                    if (refCodes != null) {
                        for (ReferenceCode refCode : refCodes) {
                            String crdcCode = (String) refCode.getFieldValueByAlias(ALIAS_CRDC_REF_CODE);

                            if (CRDC_CODE_DUAL.equalsIgnoreCase(crdcCode)) {
                                codeForDualEnr = refCode.getCode();

                                if (!StringUtils.isEmpty(codeForDualEnr)) {
                                    codeByFieldMap = new TreeMap<String, String>();
                                    codeByFieldMap.put(dualEnrField.getJavaName(), codeForDualEnr);
                                }
                                break;
                            }
                        }
                    }
                }
            }

            return codeByFieldMap;
        }
    }

    /**
     * Retrieve if a student is in IDEA special education.
     *
     * @author X2 Development Corporation
     *
     */
    public class TNRetrieverGED implements FieldRetriever {
        public static final String ALIAS_DOCUMENT_DATE = "DOE COMPLETION DOCUMENT DATE";
        public static final String ALIAS_DOCUMENT_TYPE = "DOE COMPLETION DOCUMENT TYPE";

        private String m_fieldDocumentDate;
        private String m_fieldDocumentType;
        private Set<String> m_matchingOids = new HashSet();

        /**
         * Instantiates a new TN retriever GED.
         */
        public TNRetrieverGED() {
            m_fieldDocumentDate = CRDCStudentData.this.translateAliasToJavaName(ALIAS_DOCUMENT_DATE, true);
            m_fieldDocumentType = CRDCStudentData.this.translateAliasToJavaName(ALIAS_DOCUMENT_TYPE, true);
            if (m_fieldDocumentDate != null && m_fieldDocumentType != null) {
                DateAsStringConverter converter =
                        (DateAsStringConverter) ConverterFactory.getConverterForClass(PlainDate.class.getName(),
                                Locale.getDefault(), true);
                String startDate = converter.getSystemString(CRDCStudentData.this.getCurrentContext().getStartDate());
                String endDate = converter.getSystemString(CRDCStudentData.this.getCurrentContext().getEndDate());
                Set gedCodes = CRDCStudentData.this.getCodesForCRDCValue(SisStudent.class, m_fieldDocumentType,
                        Arrays.asList("GED"));
                X2Criteria criteria = new X2Criteria();
                criteria.addLessOrEqualThan(m_fieldDocumentDate, endDate);
                criteria.addGreaterOrEqualThan(m_fieldDocumentDate, startDate);
                criteria.addIn(m_fieldDocumentType, gedCodes);

                String[] columns = new String[] {X2BaseBean.COL_OID};
                ReportQueryByCriteria query = new ReportQueryByCriteria(SisStudent.class, columns, criteria);
                ReportQueryIterator queryItr = CRDCStudentData.this.getBroker().getReportQueryIteratorByQuery(query);
                try {
                    while (queryItr.hasNext()) {
                        Object[] row = (Object[]) queryItr.next();
                        String oid = (String) row[0];
                        m_matchingOids.add(oid);
                    }
                } finally {
                    queryItr.close();
                }
            }
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            return Boolean.valueOf(m_matchingOids.contains(entity.getBean().getOid()));
        }
    }

    /**
     * TN Retriever to get student grade level based on YOG and YOG change.
     *
     * @author X2 Development Corporation
     *
     */
    public class TNRetrieverGrade implements FieldRetriever {
        HashMap<String, String> m_crdcLookup = new HashMap();
        HashMap<Integer, String> m_gradeLevelMap = new HashMap();
        int m_maxGradeLevel;
        TreeMap<Integer, List<String>> m_sortedGradeLevels;

        Map<String, Integer> m_stdGrdFromContextMap;

        /**
         * Instantiates a new TN retriever grade.
         */
        public TNRetrieverGrade() {
            super();

            m_sortedGradeLevels = StudentManager.buildGradeLevelMap(CRDCStudentData.this.getBroker());
            m_maxGradeLevel = StudentManager.getMaxGradeLevel(CRDCStudentData.this.getBroker());

            DataDictionaryField aliasField = getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_CRDC_REF_CODE);
            ModelProperty prop = new ModelProperty(SisStudent.class, SisStudent.COL_GRADE_LEVEL,
                    CRDCStudentData.this.getBroker().getPersistenceKey());
            DataDictionaryField field = getDataDictionary().findDataDictionaryField(prop.getFieldId());

            if (aliasField != null && !StringUtils.isEmpty(field.getReferenceTableOid())) {
                X2Criteria criteria = new X2Criteria();
                criteria.addNotEmpty(aliasField.getJavaName(), CRDCStudentData.this.getBroker().getPersistenceKey());
                criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, field.getReferenceTableOid());

                String[] columns = new String[] {ReferenceCode.COL_CODE, aliasField.getJavaName()};

                ReportQueryByCriteria query = new ReportQueryByCriteria(ReferenceCode.class, columns, criteria);

                ReportQueryIterator iterator = CRDCStudentData.this.getBroker().getReportQueryIteratorByQuery(query);
                try {
                    while (iterator.hasNext()) {
                        Object[] record = (Object[]) iterator.next();
                        String code = (String) record[0];
                        String crdcCode = (String) record[1];
                        m_crdcLookup.put(code, crdcCode);
                    }
                } finally {
                    iterator.close();
                }
            }

            initStdGrdFromContextMap();
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();
            CRDCStudentData stdData = (CRDCStudentData) data;
            String gradeLevel = null;

            StudentEnrollment enrollment =
                    m_studentHelper.getEnrollmentForDate(student.getOid(), stdData.m_reportDatePart1, "EWSY");
            if (enrollment == null && stdData.m_reportDatePart2 != null) {
                enrollment = m_studentHelper.getEnrollmentForDate(student.getOid(), stdData.m_reportDatePart2, "EWSY");
            }
            if (enrollment == null) {
                enrollment = m_studentHelper.getEnrollmentForDate(student.getOid(),
                        stdData.getCurrentContext().getEndDate(), "EWSY");
            }
            if (enrollment == null) {
                // use next enrollment after context end date
                List<StudentEnrollment> enrollments = m_studentHelper.getStudentEnrollments(student);
                if (enrollments != null && !enrollments.isEmpty()) {
                    enrollment = enrollments.get(enrollments.size() - 1);
                }
            }
            Integer contextYog = m_stdGrdFromContextMap.get(student.getOid());

            if (contextYog != null) {
                gradeLevel = getGradeLevel(contextYog);
            }

            if (gradeLevel == null && enrollment != null) {
                gradeLevel = getGradeLevel(Integer.valueOf(enrollment.getYog()));
            }

            // Try student yog if enrollment and context fails
            if (StringUtils.isEmpty(gradeLevel)) {
                gradeLevel = getGradeLevel(Integer.valueOf(student.getYog()));
            }
            return gradeLevel;
        }

        /**
         * Return the cached CRDCCode for the grade level for this yog.
         *
         * @param yog Integer
         * @return String
         */
        private String getGradeLevel(Integer yog) {
            if (!m_gradeLevelMap.containsKey(yog)) {
                String gradeLevel = null;
                List<String> matchingGradeLevels =
                        StudentManager.getMatchingGradeLevels(m_maxGradeLevel, yog.intValue(),
                                CRDCStudentData.this.getCurrentContext().getSchoolYear(), m_sortedGradeLevels);
                for (String matchingGradeLevel : matchingGradeLevels) {
                    gradeLevel = m_crdcLookup.get(matchingGradeLevel);
                    if (!StringUtils.isEmpty(gradeLevel)) {
                        break;
                    }
                }
                m_gradeLevelMap.put(yog, gradeLevel);
            }

            return m_gradeLevelMap.get(yog);
        }

        /**
         * Initializes the student yog map considering student context attributes for current school
         * year.
         */
        private void initStdGrdFromContextMap() {
            m_stdGrdFromContextMap = new HashMap<String, Integer>();

            SubQuery subQuery =
                    new SubQuery(SisStudent.class, X2BaseBean.COL_OID, m_studentHelper.getStudentCriteria());
            Collection<String> studentOids = getBroker().getSubQueryCollectionByQuery(subQuery);

            X2Criteria contextsAttributesCriteria = new X2Criteria();
            contextsAttributesCriteria.addIn(StudentContextAttributes.COL_STUDENT_OID, subQuery);
            contextsAttributesCriteria
                    .addEqualTo(StudentContextAttributes.REL_DISTRICT_CONTEXT + ModelProperty.PATH_DELIMITER +
                            DistrictSchoolYearContext.COL_SCHOOL_YEAR,
                            Integer.valueOf(getCurrentContext().getSchoolYear()));
            QueryByCriteria contextAttributesQuery =
                    new QueryByCriteria(StudentContextAttributes.class, contextsAttributesCriteria, false);
            Map<String, StudentContextAttributes> contextsAttributesMap =
                    getBroker().getMapByQuery(contextAttributesQuery, StudentContextAttributes.COL_STUDENT_OID, 5);

            StudentContextAttributesManager manager =
                    new StudentContextAttributesManager(getOrganization(), getBroker());

            for (String studentOid : studentOids) {
                StudentContextAttributes contextAttributes = contextsAttributesMap.get(studentOid);
                Integer value = null;
                if (contextAttributes != null) {
                    String contextBeanPath = manager.getAttributeColumn(SisStudent.COL_YOG);
                    if (contextBeanPath != null) {
                        value = (Integer) contextAttributes.getFieldValueByBeanPath(contextBeanPath);
                    }
                }

                m_stdGrdFromContextMap.put(studentOid, value);
            }
        }
    }

    /**
     * Retrieve if a student is in IDEA special education.
     *
     * @author X2 Development Corporation
     *
     */
    public class TNRetrieverIdea implements FieldRetriever {
        public static final String CRDC_CODE_IDEA = "A";

        Collection<String> m_codes = new ArrayList<String>();
        Map<String, String> m_stdIdeaCodesFromContextMap;

        /**
         * Instantiates a new TN retriever idea.
         */
        public TNRetrieverIdea() {
            super();

            DataDictionaryField aliasField = getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_CRDC_REF_CODE);
            DataDictionaryField stdSpedStatusField =
                    getDataDictionary().findDataDictionaryField(SisStudent.class.getName(),
                            SisStudent.COL_SPED_STATUS_CODE);

            if (aliasField != null && stdSpedStatusField != null &&
                    !StringUtils.isEmpty(stdSpedStatusField.getReferenceTableOid())) {
                X2Criteria criteria = new X2Criteria();
                criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, stdSpedStatusField.getReferenceTableOid());
                criteria.addEqualTo(aliasField.getJavaName(), CRDC_CODE_IDEA);

                QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, criteria);

                QueryIterator iterator = CRDCStudentData.this.getBroker().getIteratorByQuery(query);
                try {
                    while (iterator.hasNext()) {
                        ReferenceCode record = (ReferenceCode) iterator.next();
                        String code = record.getCode();
                        m_codes.add(code);
                    }
                } finally {
                    iterator.close();
                }
            }

            initStdIdeaCodesFromContextMap();
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();

            String spedStatusContextCode = m_stdIdeaCodesFromContextMap.get(student.getOid());
            String spedStatusCode =
                    (spedStatusContextCode == null ? student.getSpedStatusCode() : spedStatusContextCode);
            Boolean value = Boolean.valueOf(m_codes.contains(spedStatusCode));

            if (m_stdSnapIDEA != null) {
                value = Boolean.valueOf(m_stdSnapIDEA.contains(student.getOid()));
            }

            return value;
        }

        /**
         * Initializes the student Idea codes considering student context attributes for current
         * school year.
         */
        private void initStdIdeaCodesFromContextMap() {
            m_stdIdeaCodesFromContextMap = new HashMap<String, String>();

            SubQuery subQuery =
                    new SubQuery(SisStudent.class, X2BaseBean.COL_OID, m_studentHelper.getStudentCriteria());
            Collection<String> studentOids = getBroker().getSubQueryCollectionByQuery(subQuery);

            X2Criteria contextsAttributesCriteria = new X2Criteria();
            contextsAttributesCriteria.addIn(StudentContextAttributes.COL_STUDENT_OID, subQuery);
            contextsAttributesCriteria
                    .addEqualTo(StudentContextAttributes.REL_DISTRICT_CONTEXT + ModelProperty.PATH_DELIMITER +
                            DistrictSchoolYearContext.COL_SCHOOL_YEAR,
                            Integer.valueOf(getCurrentContext().getSchoolYear()));
            QueryByCriteria contextAttributesQuery =
                    new QueryByCriteria(StudentContextAttributes.class, contextsAttributesCriteria, false);
            Map<String, StudentContextAttributes> contextsAttributesMap =
                    getBroker().getMapByQuery(contextAttributesQuery, StudentContextAttributes.COL_STUDENT_OID, 5);

            StudentContextAttributesManager manager =
                    new StudentContextAttributesManager(getOrganization(), getBroker());

            for (String studentOid : studentOids) {
                StudentContextAttributes contextAttributes = contextsAttributesMap.get(studentOid);
                String value = null;
                if (contextAttributes != null) {
                    String contextBeanPath = manager.getAttributeColumn(SisStudent.COL_SPED_STATUS_CODE);
                    if (contextBeanPath != null) {
                        value = (String) contextAttributes.getFieldValueByBeanPath(contextBeanPath);
                    } else {
                        ModelProperty property = new ModelProperty(SisStudent.class, SisStudent.COL_SPED_STATUS_CODE,
                                getDataDictionary());

                        // Check if bean path is in blob data
                        Map<String, String> blobContent =
                                ContextAttributesManager.parseBlobContents(contextAttributes.getBlobInformation());
                        if (blobContent.containsKey(property.getDictionaryPath())) {
                            value = blobContent.get(property.getDictionaryPath());
                        }
                    }
                }

                m_stdIdeaCodesFromContextMap.put(studentOid, value);
            }
        }
    }

    /**
     * Retrieve if a student is in LEP.
     *
     * @author X2 Development Corporation
     *
     */
    public class TNRetrieverLep implements FieldRetriever {
        public static final String ALIAS_DOE_ELB = "DOE ELB";
        public static final String CRDC_CODE_DELIMITER = ",";

        HashMap<String, List<String>> m_crdcLookup = new HashMap();
        String m_stdLepBeanPath;
        Map<String, String> m_stdLepCodeFromContextMap;

        /**
         * Instantiates a new TN retriever lep.
         */
        public TNRetrieverLep() {
            super();

            DataDictionaryField aliasField = getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_CRDC_REF_CODE);
            DataDictionaryField stdLepField = getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_DOE_ELB);

            if (aliasField != null && stdLepField != null && !StringUtils.isEmpty(stdLepField.getReferenceTableOid())) {
                m_stdLepBeanPath = stdLepField.getJavaName();

                X2Criteria criteria = new X2Criteria();
                criteria.addNotEmpty(aliasField.getJavaName(), CRDCStudentData.this.getBroker().getPersistenceKey());
                criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, stdLepField.getReferenceTableOid());

                String[] columns = new String[] {ReferenceCode.COL_CODE, aliasField.getJavaName()};

                ReportQueryByCriteria query = new ReportQueryByCriteria(ReferenceCode.class, columns, criteria);

                ReportQueryIterator iterator = CRDCStudentData.this.getBroker().getReportQueryIteratorByQuery(query);
                try {
                    while (iterator.hasNext()) {
                        Object[] record = (Object[]) iterator.next();
                        String code = (String) record[0];
                        String crdcCodes[] = ((String) record[1]).split(CRDC_CODE_DELIMITER);

                        m_crdcLookup.put(code, Arrays.asList(crdcCodes));
                    }
                } finally {
                    iterator.close();
                }
            }

            initStdLepCodesFromContextMap();
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();
            String stdOid = student.getOid();

            if (m_stdSnapLEP != null) {
                return Boolean.valueOf(m_stdSnapLEP.contains(stdOid));
            }

            boolean value = false;

            String code = (String) field.getParameter();

            if (!StringUtils.isEmpty(m_stdLepBeanPath)) {
                String lepContextCode = m_stdLepCodeFromContextMap.get(stdOid);
                String lepCode = (lepContextCode == null ? (String) student.getFieldValueByBeanPath(m_stdLepBeanPath)
                        : lepContextCode);

                if (m_crdcLookup != null && m_crdcLookup.containsKey(lepCode)
                        && m_crdcLookup.get(lepCode).contains(code)) {
                    value = true;
                }
            }
            return Boolean.valueOf(value);
        }

        /**
         * Initializes the student Lep codes considering student context attributes for current
         * school year.
         */
        private void initStdLepCodesFromContextMap() {
            m_stdLepCodeFromContextMap = new HashMap<String, String>();

            SubQuery subQuery =
                    new SubQuery(SisStudent.class, X2BaseBean.COL_OID, m_studentHelper.getStudentCriteria());
            Collection<String> studentOids = getBroker().getSubQueryCollectionByQuery(subQuery);

            X2Criteria contextsAttributesCriteria = new X2Criteria();
            contextsAttributesCriteria.addIn(StudentContextAttributes.COL_STUDENT_OID, subQuery);
            contextsAttributesCriteria
                    .addEqualTo(StudentContextAttributes.REL_DISTRICT_CONTEXT + ModelProperty.PATH_DELIMITER +
                            DistrictSchoolYearContext.COL_SCHOOL_YEAR,
                            Integer.valueOf(getCurrentContext().getSchoolYear()));
            QueryByCriteria contextAttributesQuery =
                    new QueryByCriteria(StudentContextAttributes.class, contextsAttributesCriteria, false);
            Map<String, StudentContextAttributes> contextsAttributesMap =
                    getBroker().getMapByQuery(contextAttributesQuery, StudentContextAttributes.COL_STUDENT_OID, 5);

            StudentContextAttributesManager manager =
                    new StudentContextAttributesManager(getOrganization(), getBroker());

            for (String studentOid : studentOids) {
                StudentContextAttributes contextAttributes = contextsAttributesMap.get(studentOid);
                String value = null;
                if (contextAttributes != null) {
                    String contextBeanPath = manager.getAttributeColumn(m_stdLepBeanPath);

                    if (contextBeanPath != null) {
                        value = (String) contextAttributes.getFieldValueByBeanPath(contextBeanPath);
                    } else {
                        String id = DictionaryHelper.translateAlias(ALIAS_DOE_ELB, getDataDictionary(), true);
                        ModelProperty property = new ModelProperty(SisStudent.class, id, getDataDictionary());

                        // Check if bean path is in blob data
                        Map<String, String> blobContent =
                                ContextAttributesManager.parseBlobContents(contextAttributes.getBlobInformation());
                        if (blobContent.containsKey(property.getDictionaryPath())) {
                            value = blobContent.get(property.getDictionaryPath());
                        }
                    }
                }

                m_stdLepCodeFromContextMap.put(studentOid, value);
            }
        }
    }

    /**
     * Retriever if a student was retained during current CTX.
     *
     * @author X2 Development Corporation
     *
     */
    public class TNRetrStdRetained extends RetrieverStdRetained {
        protected static final String ALIAS_DAY_EVENT_TYPE_2 = "DOE DAY EVENT TYPE 2";
        protected static final String ALIAS_DAY_EVENT_TYPE_3 = "DOE DAY EVENT TYPE 3";

        protected static final String ALIAS_DOE_END_SERVICE_ACTION = "DOE END SERVICE ACTION";

        private static final String CODE_EVENT_TYPE_CS = "CS";

        protected String m_fieldDayEventType2;
        protected String m_fieldDayEventType3;

        private String m_fieldServiceAction;

        private Collection<String> m_retainedStudentsFromContext;

        /**
         * Instantiates a new TN retr std retained.
         */
        public TNRetrStdRetained() {
            super();
            m_fieldServiceAction = translateAliasToJavaName(ALIAS_DOE_END_SERVICE_ACTION, true);
            initRetainedStudentsFromContext();
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.x2dev.procedures.statereporting.CRDCStudentData.RetrieverStdRetained#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Boolean commonRetained = (Boolean) super.getFieldValue(data, entity, field);
            Boolean contextRetained =
                    Boolean.valueOf(m_retainedStudentsFromContext.contains(entity.getBean().getOid()));

            return commonRetained.booleanValue() ? commonRetained : contextRetained;
        }

        /**
         * Return calendar start date (Event Type = 'CS') for the school.
         *
         * @param date SchoolCalendarDate
         * @return true, if is valid
         */
        @Override
        protected boolean isValid(SchoolCalendarDate date) {
            boolean value = false;

            if (m_fieldDayEventType2 == null) {
                m_fieldDayEventType2 = CRDCStudentData.this.translateAliasToJavaName(ALIAS_DAY_EVENT_TYPE_2, true);
                m_fieldDayEventType3 = CRDCStudentData.this.translateAliasToJavaName(ALIAS_DAY_EVENT_TYPE_3, true);
            }
            if (CODE_EVENT_TYPE_CS.equals(date.getScheduleDayType())) {
                value = true;
            } else if (CODE_EVENT_TYPE_CS.equals(date.getFieldValueByBeanPath(m_fieldDayEventType2))) {
                value = true;
            } else if (CODE_EVENT_TYPE_CS.equals(date.getFieldValueByBeanPath(m_fieldDayEventType3))) {
                value = true;
            }
            return value;
        }

        /**
         * Initializes students with [DOE END SERVICE ACTION] as "R" in context attributes.
         */
        private void initRetainedStudentsFromContext() {
            m_retainedStudentsFromContext = new HashSet<String>();

            SubQuery subQuery =
                    new SubQuery(SisStudent.class, X2BaseBean.COL_OID, m_studentHelper.getStudentCriteria());
            Collection<String> studentOids = getBroker().getSubQueryCollectionByQuery(subQuery);

            X2Criteria contextsAttributesCriteria = new X2Criteria();
            contextsAttributesCriteria.addIn(StudentContextAttributes.COL_STUDENT_OID, subQuery);
            contextsAttributesCriteria
                    .addEqualTo(StudentContextAttributes.REL_DISTRICT_CONTEXT + ModelProperty.PATH_DELIMITER +
                            DistrictSchoolYearContext.COL_SCHOOL_YEAR,
                            Integer.valueOf(getCurrentContext().getSchoolYear()));
            QueryByCriteria contextAttributesQuery =
                    new QueryByCriteria(StudentContextAttributes.class, contextsAttributesCriteria, false);
            Map<String, StudentContextAttributes> contextsAttributesMap =
                    getBroker().getMapByQuery(contextAttributesQuery, StudentContextAttributes.COL_STUDENT_OID, 5);

            StudentContextAttributesManager manager =
                    new StudentContextAttributesManager(getOrganization(), getBroker());

            for (String studentOid : studentOids) {
                StudentContextAttributes contextAttributes = contextsAttributesMap.get(studentOid);
                String value = null;
                if (contextAttributes != null) {
                    String contextBeanPath = manager.getAttributeColumn(m_fieldServiceAction);
                    if (contextBeanPath != null) {
                        value = (String) contextAttributes.getFieldValueByBeanPath(contextBeanPath);
                    } else {
                        String id = DictionaryHelper.translateAlias(ALIAS_DOE_END_SERVICE_ACTION, getDataDictionary(),
                                true);
                        ModelProperty property = new ModelProperty(SisStudent.class, id, getDataDictionary());

                        // Check if bean path is in blob data
                        Map<String, String> blobContent =
                                ContextAttributesManager.parseBlobContents(contextAttributes.getBlobInformation());
                        if (blobContent.containsKey(property.getDictionaryPath())) {
                            value = blobContent.get(property.getDictionaryPath());
                        }
                    }
                }

                if ("R".equals(value)) {
                    m_retainedStudentsFromContext.add(studentOid);
                }
            }
        }
    }

    /**
     * VA Retriever to determine if a student is in 504 special education.
     *
     * @author X2 Development Corporation
     *
     */
    public class VARetriever504 implements FieldRetriever {
        public static final String CRDC_CODE_504 = "504";

        Collection<String> m_codes = new ArrayList<String>();

        /**
         * Instantiates a new VA retriever 504.
         */
        public VARetriever504() {
            super();

            DataDictionaryField aliasField = getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_CRDC_REF_CODE);
            DataDictionaryField stdSpedStatusField =
                    getDataDictionary().findDataDictionaryField(SisStudent.class.getName(),
                            SisStudent.COL_SECTION504_STATUS_CODE);

            if (aliasField != null && stdSpedStatusField != null &&
                    !StringUtils.isEmpty(stdSpedStatusField.getReferenceTableOid())) {
                X2Criteria criteria = new X2Criteria();
                criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, stdSpedStatusField.getReferenceTableOid());
                criteria.addEqualTo(aliasField.getJavaName(), CRDC_CODE_504);

                QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, criteria);

                QueryIterator iterator = CRDCStudentData.this.getBroker().getIteratorByQuery(query);
                try {
                    while (iterator.hasNext()) {
                        ReferenceCode record = (ReferenceCode) iterator.next();
                        String code = record.getCode();
                        m_codes.add(code);
                    }
                } finally {
                    iterator.close();
                }
            }
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();
            String stdOid = student.getOid();

            if (m_stdSnap504 != null) {
                return Boolean.valueOf(m_stdSnap504.contains(stdOid));
            }

            boolean value = false;

            String retievedIdea = entity.getFieldValue(FIELD_IDEA);

            if (CODE_YES.equals(retievedIdea)) {
                return Boolean.valueOf(value);
            }

            if (m_codes.contains(student.getSection504StatusCode())) {
                value = true;
            }
            return Boolean.valueOf(value);
        }
    }


    /**
     * VA Retriever if a student is in Distance Education.
     *
     * @author X2 Development Corporation
     *
     */
    public class VARetrieverDistEd extends VARetrieverIbPgm {
        public static final String ALIAS_DOE_DIST_LEARN = "DOE DISTANCE LEARN";

        /**
         * Instantiates a new VA retriever dist ed.
         */
        public VARetrieverDistEd() {
            m_stdOids = new HashSet<String>();

            DataDictionaryField stdIbPgmField =
                    getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_DOE_DIST_LEARN);
            X2Criteria stdCriteria = m_studentHelper.getStudentCriteria().copy();

            if (stdIbPgmField != null) {
                stdCriteria.addEqualTo(stdIbPgmField.getJavaName(), BooleanAsStringConverter.TRUE);
            } else {
                stdCriteria.addEqualTo(X2BaseBean.COL_OID, "__dummyOid__");
            }

            Map<String, SisStudent> stdMap = CRDCStudentData.this.getBroker()
                    .getMapByQuery(new QueryByCriteria(SisStudent.class, stdCriteria), X2BaseBean.COL_OID, 1024);

            if (stdMap != null && !stdMap.isEmpty()) {
                m_stdOids.addAll(stdMap.keySet());
            }
        }
    }

    /**
     * VA Retriever if a student is dual enrolled.
     *
     * @author X2 Development Corporation
     *
     */
    public class VARetrieverDualEnr implements FieldRetriever {
        public static final String ALIAS_DOE_DUAL_ENR = "DOE DUAL ENR";
        public static final String ALIAS_DOE_DUAL_ENR_CTE = "DOE CTE DUAL ENR";

        Set<String> m_stdOids;

        /**
         * Instantiates a new VA retriever dual enr.
         */
        public VARetrieverDualEnr() {
            super();

            m_stdOids = new HashSet<String>();

            DataDictionaryField stdDualEnrField =
                    getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_DOE_DUAL_ENR);
            DataDictionaryField stdDualEnrCteField =
                    getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_DOE_DUAL_ENR_CTE);
            X2Criteria stdCriteria = m_studentHelper.getStudentCriteria().copy();

            X2Criteria stdAndCriteria = new X2Criteria();
            X2Criteria dualEnrOrCriteria = new X2Criteria();
            if (stdDualEnrField != null) {
                stdAndCriteria.addEqualTo(stdDualEnrField.getJavaName(), BooleanAsStringConverter.TRUE);
            } else {
                stdAndCriteria.addEqualTo(X2BaseBean.COL_OID, "__dummyOid__");
            }
            if (stdDualEnrCteField != null) {
                dualEnrOrCriteria.addEqualTo(stdDualEnrCteField.getJavaName(), BooleanAsStringConverter.TRUE);
            } else {
                dualEnrOrCriteria.addEqualTo(X2BaseBean.COL_OID, "__dummyOid__");
            }

            stdAndCriteria.addOrCriteria(dualEnrOrCriteria);
            stdCriteria.addAndCriteria(stdAndCriteria);

            Map<String, SisStudent> stdMap = CRDCStudentData.this.getBroker()
                    .getMapByQuery(new QueryByCriteria(SisStudent.class, stdCriteria), X2BaseBean.COL_OID, 1024);

            if (stdMap != null && !stdMap.isEmpty()) {
                m_stdOids.addAll(stdMap.keySet());
            }
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            boolean value = false;

            SisStudent student = (SisStudent) entity.getBean();

            if (m_stdOids.contains(student.getOid())) {
                value = true;
            }
            return Boolean.valueOf(value);
        }
    }

    /**
     * VA Retriever to determine if a student is in LEP.
     *
     * @author X2 Development Corporation
     *
     */
    public class VARetrieverGED extends RetrieverByStdAliasAndCode {
        public static final String ALIAS_DOE_GED = "DOE GED";
        public static final String CRDC_CODE_GED = "GED";

        /**
         * Instantiates a new VA retriever GED.
         */
        public VARetrieverGED() {
            super(ALIAS_DOE_GED, CRDC_CODE_GED);
        }
    }

    /**
     * VA Retriever to determine if a student is gifted/talented.
     *
     * @author X2 Development Corporation
     *
     */
    public class VARetrieverGift extends RetrieverByStdAliasAndCode {
        public static final String ALIAS_DOE_GIFTED = "DOE GIFTED";
        public static final String ALIAS_DOE_GIFT_REF = "DOE GIFTED REFERRAL";
        public static final String CRDC_CODE_GIFTED = "Gifted";

        private String m_fieldGifted;
        private String m_fieldGiftedReferral;

        /**
         * Instantiates a new VA retriever gift.
         */
        public VARetrieverGift() {
            super(ALIAS_DOE_GIFTED, CRDC_CODE_GIFTED);

            m_fieldGifted = translateAliasToJavaName(ALIAS_DOE_GIFTED, true);
            m_fieldGiftedReferral = translateAliasToJavaName(ALIAS_DOE_GIFT_REF, true);
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.x2dev.procedures.statereporting.CRDCStudentData.RetrieverByStdAliasAndCode#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            boolean value = false;

            SisStudent student = (SisStudent) entity.getBean();
            Object giftedDate = data.getPropertyAsJavaType(student, m_fieldGiftedReferral);

            if (m_codes.contains(student.getFieldValueByBeanPath(m_fieldGifted)) && giftedDate != null
                    && Date.class.isAssignableFrom(giftedDate.getClass())
                    && !m_reportDatePart1.before((PlainDate) giftedDate)) {
                value = true;
            }

            return Boolean.valueOf(value);
        }
    }

    /**
     * VA Retriever to determine count of incidents based on one of the categories (SEX, RACE,
     * DISABILITY).
     *
     * @author X2 Development Corporation
     *
     */
    public class VARetrieverHarrases implements FieldRetriever {
        public static final String ALIAS_CND_ON_DISABILITY = "BASED ON DISABILITY";
        public static final String ALIAS_CND_ON_RACE = "BASED ON RACE";
        public static final String ALIAS_CND_ON_SEX = "BASED ON SEX";

        public static final String CRDC_CODE_HARRAS = "Harrassment";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            int count = 0;
            String category = (String) field.getParameter();
            Set<ConductIncident> incidents = CRDCStudentData.this.m_conductHelper
                    .getIncidentsForVictim(entity.getBean().getOid(), CRDC_CODE_HARRAS);

            if (incidents != null) {
                if (ALIAS_CND_ON_SEX.equals(category)) {
                    for (ConductIncident cnd : incidents) {
                        if (BooleanAsStringConverter.TRUE.equals(cnd.getFieldValueByAlias(ALIAS_CND_ON_SEX))) {
                            count += 1;
                        }
                    }
                } else if (ALIAS_CND_ON_RACE.equals(category)) {
                    for (ConductIncident cnd : incidents) {
                        if (BooleanAsStringConverter.TRUE.equals(cnd.getFieldValueByAlias(ALIAS_CND_ON_RACE))) {
                            count += 1;
                        }
                    }
                } else if (ALIAS_CND_ON_DISABILITY.equals(category)) {
                    for (ConductIncident cnd : incidents) {
                        if (BooleanAsStringConverter.TRUE.equals(cnd.getFieldValueByAlias(ALIAS_CND_ON_DISABILITY))) {
                            count += 1;
                        }
                    }
                }
            }
            return Integer.valueOf(count);
        }
    }

    /**
     * VA Retriever to determine if a student is in HS Equivalency.
     *
     * @author X2 Development Corporation
     *
     */
    public class VARetrieverHSEq extends RetrieverByStdAliasAndCode {
        public static final String ALIAS_DOE_GRAD = "DOE GRAD CODE";
        public static final String CRDC_CODE_GED = "GED";

        /**
         * Instantiates a new VA retriever HS eq.
         */
        public VARetrieverHSEq() {
            super(ALIAS_DOE_GRAD, CRDC_CODE_GED);
        }
    }

    /**
     * VA Retriever if a student participate in IB program.
     *
     * @author X2 Development Corporation
     *
     */
    public class VARetrieverIbPgm implements FieldRetriever {
        public static final String ALIAS_DOE_IB_PGM = "DOE IB PROGRAM";

        Set<String> m_stdOids;

        /**
         * Instantiates a new VA retriever ib pgm.
         */
        public VARetrieverIbPgm() {
            super();

            m_stdOids = new HashSet<String>();

            DataDictionaryField stdIbPgmField = getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_DOE_IB_PGM);
            X2Criteria stdCriteria = m_studentHelper.getStudentCriteria().copy();

            if (stdIbPgmField != null) {
                stdCriteria.addEqualTo(stdIbPgmField.getJavaName(), BooleanAsStringConverter.TRUE);
            } else {
                stdCriteria.addEqualTo(X2BaseBean.COL_OID, "__dummyOid__");
            }

            Map<String, SisStudent> stdMap = CRDCStudentData.this.getBroker()
                    .getMapByQuery(new QueryByCriteria(SisStudent.class, stdCriteria), X2BaseBean.COL_OID, 1024);

            if (stdMap != null && !stdMap.isEmpty()) {
                m_stdOids.addAll(stdMap.keySet());
            }
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            boolean value = false;

            SisStudent student = (SisStudent) entity.getBean();

            if (m_stdOids.contains(student.getOid())) {
                value = true;
            }
            return Boolean.valueOf(value);
        }
    }

    /**
     * VA Retriever to determine if a student is in IDEA special education.
     *
     * @author X2 Development Corporation
     *
     */
    public class VARetrieverIdea implements FieldRetriever {
        public static final String CRDC_CODE_IDEA = "A";

        Collection<String> m_codes = new ArrayList<String>();

        /**
         * Instantiates a new VA retriever idea.
         */
        public VARetrieverIdea() {
            super();

            DataDictionaryField aliasField = getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_CRDC_REF_CODE);
            DataDictionaryField stdSpedStatusField = getDataDictionary()
                    .findDataDictionaryField(SisStudent.class.getName(), SisStudent.COL_SPED_STATUS_CODE);

            if (aliasField != null && stdSpedStatusField != null
                    && !StringUtils.isEmpty(stdSpedStatusField.getReferenceTableOid())) {
                X2Criteria criteria = new X2Criteria();
                criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, stdSpedStatusField.getReferenceTableOid());
                criteria.addEqualTo(aliasField.getJavaName(), CRDC_CODE_IDEA);

                QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, criteria);

                QueryIterator iterator = CRDCStudentData.this.getBroker().getIteratorByQuery(query);
                try {
                    while (iterator.hasNext()) {
                        ReferenceCode record = (ReferenceCode) iterator.next();
                        String code = record.getCode();
                        m_codes.add(code);
                    }
                } finally {
                    iterator.close();
                }
            }
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();
            String stdOid = student.getOid();

            if (m_stdSnapIDEA != null) {
                return Boolean.valueOf(m_stdSnapIDEA.contains(stdOid));
            }

            boolean value = false;

            if (m_codes.contains(student.getSpedStatusCode())) {
                value = true;
            }

            return Boolean.valueOf(value);
        }
    }

    /**
     * VA Retriever to determine if a student is RecvHSEquivalency.
     *
     * @author X2 Development Corporation
     *
     */
    public class VARetrieverHSEEquiv extends RetrieverByStdAliasAndCode {
        public static final String ALIAS_DOE_GRAD_CODE = "DOE GRAD CODE";
        public static final String CRDC_CODE_GED = "GED";

        /**
         * Instantiates a new VA retriever HSE equiv.
         */
        public VARetrieverHSEEquiv() {
            super(ALIAS_DOE_GRAD_CODE, CRDC_CODE_GED);
        }
    }

    /**
     * VA Retriever to determine if a student is in LEP.
     *
     * @author X2 Development Corporation
     *
     */
    public class VARetrieverLep extends RetrieverByStdAliasAndCode {
        public static final String ALIAS_DOE_ELL = "DOE ESL SERVICE";
        public static final String CRDC_CODE_LEP = "LEP";

        /**
         * Instantiates a new VA retriever lep.
         */
        public VARetrieverLep() {
            super(ALIAS_DOE_ELL, CRDC_CODE_LEP);
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.x2dev.procedures.statereporting.CRDCStudentData.RetrieverByStdAliasAndCode#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();
            String stdOid = student.getOid();

            if (m_stdSnapLEP != null) {
                return Boolean.valueOf(m_stdSnapLEP.contains(stdOid));
            }

            return super.getFieldValue(data, entity, field);
        }

    }

    /**
     * VA Retriever to determine if a student is LEP enrolled.
     *
     * @author X2 Development Corporation
     *
     */
    public class VARetrieverLepEnr extends RetrieverByStdAliasAndCode {
        public static final String ALIAS_DOE_ELL = "DOE ESL SERVICE";
        public static final String CRDC_CODE_LEP_PGM = "LEPProgram";

        /**
         * Instantiates a new VA retriever lep enr.
         */
        public VARetrieverLepEnr() {
            super(ALIAS_DOE_ELL, CRDC_CODE_LEP_PGM);
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.x2dev.procedures.statereporting.CRDCStudentData.RetrieverByStdAliasAndCode#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();
            String stdOid = student.getOid();

            if (m_stdSnapLEPPgm != null) {
                return Boolean.valueOf(m_stdSnapLEPPgm.contains(stdOid));
            }

            return super.getFieldValue(data, entity, field);
        }

    }

    /**
     * Retriever to determine if a student is Law Enforced.
     *
     * @author Follett Software Company
     */
    public class VARetrieverRefLaw implements FieldRetriever {
        public static final String ALIAS_DOE_INC_REP = "DOE INC REPORTED";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            CRDCStudentData stdCRDCData = (CRDCStudentData) data;
            Map<String, Set<ConductIncident>> cndMapByCode =
                    stdCRDCData.m_conductHelper.getStudentIncidentsMap(entity.getBean().getOid());

            if (cndMapByCode != null) {
                for (Entry<String, Set<ConductIncident>> entry : cndMapByCode.entrySet()) {
                    for (ConductIncident cnd : entry.getValue()) {
                        if (BooleanAsStringConverter.TRUE.equals(cnd.getFieldValueByAlias(ALIAS_DOE_INC_REP))) {
                            return Boolean.TRUE;
                        }
                    }
                }
            }

            return Boolean.FALSE;
        }
    }

    /**
     * Retriever to determine if a student is Law Enforced.
     *
     * @author Follett Software Company
     */
    public class VARetrieverSchDisc implements FieldRetriever {
        public static final String ALIAS_DOE_ALT_PLACE = "DOE ALT PLACEMENT";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            CRDCStudentData stdCRDCData = (CRDCStudentData) data;
            Map<String, Set<ConductIncident>> cndMapByCode =
                    stdCRDCData.m_conductHelper.getStudentIncidentsMap(entity.getBean().getOid());

            if (cndMapByCode != null) {
                for (Entry<String, Set<ConductIncident>> entry : cndMapByCode.entrySet()) {
                    for (ConductIncident cnd : entry.getValue()) {
                        if (BooleanAsStringConverter.TRUE.equals(cnd.getFieldValueByAlias(ALIAS_DOE_ALT_PLACE))) {
                            return Boolean.TRUE;
                        }
                    }
                }
            }

            return Boolean.FALSE;
        }
    }


    /**
     * Validate if a student is not in 504 and IDEA in one time.
     *
     * @author X2 Development Corporation
     *
     */
    public class Val504AndIdea implements FieldValidator {

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            String ideaValue = entity.getFieldValue(FIELD_IDEA);

            if (CODE_YES.equals(value) && CODE_YES.equals(ideaValue)) {
                errors.add(new StateReportValidationError(entity,
                        field,
                        "Idea = " + ideaValue + " 504 value = " + value,
                        "A student cannot be considered IDEA and Section 504"));
            }
            return errors;
        }
    }

    /**
     * PA Validator for IDEA field.
     *
     */
    public class ValidatePAIdea implements FieldValidator {
        public static final String ALIAS_STD_DOE_CHALLENGE = "DOE CHALLENGE";

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection<StateReportValidationError> getFieldValidation(StateReportData data,
                                                                         StateReportEntity entity,
                                                                         FieldDefinition field,
                                                                         String value) {
            Collection<StateReportValidationError> errors = new LinkedList<StateReportValidationError>();

            String ideaValue = entity.getFieldValue(FIELD_IDEA);
            String ctValue = (String) ((SisStudent) entity.getBean()).getFieldValueByAlias(ALIAS_STD_DOE_CHALLENGE);

            if (!StringUtils.isEmpty(ctValue) && !CODE_YES.equals(ideaValue)) {
                errors.add(new StateReportValidationError(entity, field, "Invalid Value",
                        "This field value must be \"Y\" if a student has a challenge type."));
            }
            return errors;
        }
    }

    /**
     * Validate PassedAlgebraI field
     * rule:
     * Create a validation error when student has a passing Alg 1 Grade but is not included in the
     * Student Enrollment due to enrollment after 10/1/xx.
     *
     * @author X2 Development Corporation
     */
    public class ValidationAlgebraI implements FieldValidator {
        private static final String SNAPSHOT_DATE = "Snapshot date - ";
        private static final String ERROR = "Student hasn't enrollment before shapshot date";

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            if (value != null && value.equals("Y")) {
                SisStudent student = (SisStudent) entity.getBean();
                StudentEnrollment enrollment =
                        m_studentHelper.getEnrollmentForDate(student.getOid(), m_reportDatePart1, "EWSY");
                if (enrollment == null && m_reportDatePart2 != null) {
                    enrollment = m_studentHelper.getEnrollmentForDate(student.getOid(), m_reportDatePart2, "EWSY");
                }
                if (enrollment == null) {
                    enrollment =
                            m_studentHelper.getEnrollmentForDate(student.getOid(), getCurrentContext().getEndDate(),
                                    "EWSY");
                }
                if (enrollment == null) {
                    // use next enrollment after context end date
                    List<StudentEnrollment> enrollments = m_studentHelper.getStudentEnrollments(student);
                    if (enrollments != null && !enrollments.isEmpty()) {
                        enrollment = enrollments.get(enrollments.size() - 1);
                    }
                }

                StringBuilder builder = new StringBuilder();
                if (enrollment == null) {
                    builder.append(SNAPSHOT_DATE);
                    builder.append(m_reportDatePart1);
                    errors.add(new StateReportValidationError(entity, field, ERROR, builder.toString()));
                }
            }
            return errors;
        }
    }

    /**
     * Validate HarassedBased OnSex,OnRace,OnDisb fields
     * rule:
     * Create a validation error when a student has an out of school conduct incident that has 0
     * days.
     *
     * @author X2 Development Corporation
     *
     */
    public class ValidationHarassed implements FieldValidator {
        private static final String FIELD = "FIELD - ";
        private static final String ERROR = "Student has a harassment conduct incident without a victim";
        private static final String INCIDENT_CODE = "Incident Code - '";
        private static final String INCIDENT_DATE = "Incident Date - '";
        private static final String AFTER_END_VALUE = "'; ";

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            String code = field.getParameter() == null ? null : field.getParameter().toString();
            Set<ConductIncident> incidents =
                    CRDCStudentData.this.m_conductHelper.getIncidentsForVictim(entity.getBean().getOid(), code);
            if (incidents != null) {
                for (ConductIncident incident : incidents) {
                    if (incident.getVictim() == null) {
                        StringBuilder builder = new StringBuilder();

                        String incidentDate = null;
                        if (incident.getIncidentDate() != null) {
                            incidentDate = m_dateFormat.format(incident.getIncidentDate());
                            builder.append(INCIDENT_DATE + STYLE_BOLD + incidentDate + STYLE_END + AFTER_END_VALUE);
                        }
                        builder.append(INCIDENT_CODE + STYLE_BOLD + incident.getIncidentCode() + STYLE_END
                                + AFTER_END_VALUE);
                        builder.append(FIELD + STYLE_BOLD + code + STYLE_END + AFTER_END_VALUE);
                        errors.add(new StateReportValidationError(entity, field, ERROR, builder.toString()));
                    }
                }
            }

            return errors;
        }
    }

    /**
     * Validate VA HarassedBased OnSex,OnRace,OnDisb fields
     * rule:
     * Create a validation error when a student has an out of school conduct incident that has 0
     * days.
     *
     * @author X2 Development Corporation
     *
     */
    public class ValidationVAHarassed implements FieldValidator {
        private static final String AFTER_END_VALUE = "'; ";
        private static final String ALIAS_CND_ON_DISABILITY = "BASED ON DISABILITY";
        private static final String ALIAS_CND_ON_RACE = "BASED ON RACE";
        private static final String ALIAS_CND_ON_SEX = "BASED ON SEX";
        private static final String CRDC_CODE_HARRAS = "Harrassment";
        private static final String ERROR = "Student has a harassment conduct incident without a victim";
        private static final String FIELD = "FIELD - ";
        private static final String INCIDENT_CODE = "Incident Code - '";
        private static final String INCIDENT_DATE = "Incident Date - '";

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            String category = (String) field.getParameter();
            Set<ConductIncident> incidents =
                    CRDCStudentData.this.m_conductHelper.getIncidentsForVictim(entity.getBean().getOid(),
                            CRDC_CODE_HARRAS);
            Set<ConductIncident> incidentsForCheck = new HashSet<ConductIncident>();
            if (incidents != null) {
                if (ALIAS_CND_ON_SEX.equals(category)) {
                    for (ConductIncident cnd : incidents) {
                        if (BooleanAsStringConverter.TRUE.equals(cnd.getFieldValueByAlias(ALIAS_CND_ON_SEX))) {
                            incidentsForCheck.add(cnd);
                        }
                    }
                } else if (ALIAS_CND_ON_RACE.equals(category)) {
                    for (ConductIncident cnd : incidents) {
                        if (BooleanAsStringConverter.TRUE.equals(cnd.getFieldValueByAlias(ALIAS_CND_ON_RACE))) {
                            incidentsForCheck.add(cnd);
                        }
                    }
                } else if (ALIAS_CND_ON_DISABILITY.equals(category)) {
                    for (ConductIncident cnd : incidents) {
                        if (BooleanAsStringConverter.TRUE.equals(cnd.getFieldValueByAlias(ALIAS_CND_ON_DISABILITY))) {
                            incidentsForCheck.add(cnd);
                        }
                    }
                }
            }

            if (incidentsForCheck != null) {
                for (ConductIncident incident : incidentsForCheck) {

                    if (incident.getVictim() == null) {
                        StringBuilder builder = new StringBuilder();

                        String incidentDate = null;
                        if (incident.getIncidentDate() != null) {
                            incidentDate = m_dateFormat.format(incident.getIncidentDate());
                            builder.append(INCIDENT_DATE + STYLE_BOLD + incidentDate + STYLE_END + AFTER_END_VALUE);
                        }
                        builder.append(INCIDENT_CODE + STYLE_BOLD + incident.getIncidentCode() + STYLE_END
                                + AFTER_END_VALUE);
                        builder.append(FIELD + STYLE_BOLD + category + STYLE_END + AFTER_END_VALUE);
                        errors.add(new StateReportValidationError(entity, field, ERROR, builder.toString()));
                    }
                }
            }

            return errors;
        }
    }

    /**
     * Validate student's race.
     *
     * @author X2 Development Corporation
     *
     */
    public class ValidationRace implements FieldValidator {
        private final List<String> m_validValuesRace = Collections.unmodifiableList(new ArrayList<String>(
                Arrays.asList("American Indian/Alaska Native", "Asian", "Native Hawaiian/Pacific Islander", "Black",
                        "White", "Two or More Races")));

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            String hispanicInd = entity.getFieldValue("Hispanic");

            if (!"Y".equals(hispanicInd) && !m_validValuesRace.contains(value)) {
                errors.add(new StateReportValidationError(entity, field,
                        STYLE_BOLD + value + STYLE_END + " is not valid Race.",
                        "Race = " + STYLE_BOLD + value + STYLE_END));
            }
            return errors;
        }
    }

    /**
     * Validate suspension days
     * rule:
     * Create a validation error when a student has an out of school conduct incident that has 0
     * days.
     *
     * @author X2 Development Corporation
     *
     */
    public class ValidationSuspDays implements FieldValidator {
        private static final String ERROR = "Number of Out-of-School Suspension days could not be blank or 0.0.";
        private static final String ACTION_CODE = "Action Code - '";
        private static final String INCIDENT_CODE = "Incident Code - '";
        private static final String INCIDENT_DATE = "Incident Date - '";
        private static final String CALC_PARAM_OUT_SCHOOL_SUSP = "Out-schoolSusp";
        private static final String AFTER_END_VALUE = "'; ";

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            String code = field.getParameter() == null ? null : field.getParameter().toString();
            if (code != null && code.equals(CALC_PARAM_OUT_SCHOOL_SUSP)) {
                CRDCStudentData stdCRDCData = (CRDCStudentData) data;
                Set<ConductAction> actions =
                        stdCRDCData.m_conductHelper.getActionsForStudent(entity.getBean().getOid(), code);
                if (actions != null) {
                    for (ConductAction action : actions) {
                        if (action.getActionPenaltyTime() == null
                                || BigDecimal.ZERO.compareTo(action.getActionPenaltyTime()) == 0) {
                            ConductIncident incident = action.getIncident();
                            String incidentDate = null;
                            StringBuilder builder = new StringBuilder();
                            if (incident.getIncidentDate() != null) {
                                incidentDate = m_dateFormat.format(incident.getIncidentDate());
                                builder.append(INCIDENT_DATE + STYLE_BOLD + incidentDate + STYLE_END + AFTER_END_VALUE);
                            }
                            builder.append(INCIDENT_CODE + STYLE_BOLD + incident.getIncidentCode() + STYLE_END
                                    + AFTER_END_VALUE);
                            builder.append(ACTION_CODE + STYLE_BOLD + action.getActionCode() + STYLE_END
                                    + AFTER_END_VALUE);
                            errors.add(new StateReportValidationError(entity, field, ERROR, builder.toString()));
                        }
                    }
                }
            }
            return errors;
        }
    }

    /**
     * Retrieve if a student is Expelled or Educational Services while expelled.
     *
     * @author X2 Development Corporation
     *
     */
    public class WARetrieverExpelled implements FieldRetriever {
        public static final String ALIAS_ACT_DOE_ACADEMIC_SERV = "DOE ACADEMIC SERVICES";
        public static final String CALC_PARAM_ED_SERV = "EdServWhileExpelled";
        public static final String CALC_PARAM_EXPELLED = "Expelled";
        public static final String CRDC_CODE_EXPELLED = "Expelled";
        public static final String CRDC_CODE_ED_SERV = "EdServWhileExpelled";
        public static final String STATE_CODE_2 = "2";

        Set<String> m_codes = new HashSet<String>();

        /**
         * Instantiates a new WA retriever expelled.
         */
        public WARetrieverExpelled() {
            super();

            DataDictionaryField actAcademicServ =
                    getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_ACT_DOE_ACADEMIC_SERV);

            if (actAcademicServ != null && !StringUtils.isEmpty(actAcademicServ.getReferenceTableOid())) {

                X2Criteria criteria = new X2Criteria();
                criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, actAcademicServ.getReferenceTableOid());
                criteria.addEqualToIgnoreCase(ReferenceCode.COL_STATE_CODE, STATE_CODE_2);

                QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, criteria);

                QueryIterator iterator = CRDCStudentData.this.getBroker().getIteratorByQuery(query);
                try {
                    while (iterator.hasNext()) {
                        ReferenceCode record = (ReferenceCode) iterator.next();
                        String code = record.getCode();
                        m_codes.add(code);
                    }
                } finally {
                    iterator.close();
                }
            }
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            boolean value = false;
            String param = (String) field.getParameter();
            CRDCStudentData stdCRDCData = (CRDCStudentData) data;
            Set<ConductAction> actions =
                    stdCRDCData.m_conductHelper.getActionsForStudent(entity.getBean().getOid(), CRDC_CODE_EXPELLED);

            if (actions != null) {
                if (CALC_PARAM_EXPELLED.equals(param)) {
                    value = true;
                    for (ConductAction act : actions) {
                        if (m_codes.contains(act.getFieldValueByAlias(ALIAS_ACT_DOE_ACADEMIC_SERV))) {
                            value = false;
                            break;
                        }
                    }
                } else if (CALC_PARAM_ED_SERV.equals(param)) {
                    for (ConductAction act : actions) {
                        if (m_codes.contains(act.getFieldValueByAlias(ALIAS_ACT_DOE_ACADEMIC_SERV))) {
                            value = true;
                            break;
                        }
                    }

                    if (!value) {
                        Set<ConductAction> actionsWhileExpelled = stdCRDCData.m_conductHelper
                                .getActionsForStudent(entity.getBean().getOid(), CRDC_CODE_ED_SERV);

                        if (actionsWhileExpelled != null && !actionsWhileExpelled.isEmpty()) {
                            value = true;
                        }
                    }
                }
            }
            return Boolean.valueOf(value);
        }
    }


    /**
     * Retrieves student's race.
     *
     * @author Follett Software Company
     */
    public class WARetrieverRace extends RetrieverRace {
        public static final String CALC_PARAM_WA_HISP_IND = "HISP_IND";
        public static final String CALC_PARAM_WA_RACE = "RACE";

        public static final String CRDC_CODE_HISPANIC = "Hispanic/Latino";
        public static final int NON_HISPANIC_ETHNICITY = 10;

        /**
         * Instantiates a new WA retriever race.
         */
        public WARetrieverRace() {
            super();
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.x2dev.procedures.statereporting.CRDCStudentData.RetrieverRace#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String value = null;

            CRDCStudentData stdCRDCData = (CRDCStudentData) data;
            SisStudent student = (SisStudent) entity.getBean();

            Set<Race> nonHispanicEthnicity = new HashSet();
            Set<Race> hispanicEthnicity = new HashSet();
            Set<Race> filteredRaces = new HashSet();
            Collection<Race> races = m_studentRaces.get(student.getPersonOid());

            if (races != null) {
                Iterator<Race> iterator = races.iterator();
                while (iterator.hasNext()) {
                    Race race = iterator.next();
                    String raceCode = stdCRDCData.lookupReferenceCodeByBeanPath(Race.class,
                            Race.COL_RACE_CODE,
                            race.getRaceCode(),
                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());

                    if (StringUtils.isNumeric(raceCode) && Integer.parseInt(raceCode) == NON_HISPANIC_ETHNICITY) {
                        nonHispanicEthnicity.add(race);
                    } else if (StringUtils.isNumeric(raceCode) && Integer.parseInt(raceCode) < 200) {
                        hispanicEthnicity.add(race);
                    } else {
                        filteredRaces.add(race);
                    }
                }
            }

            if (CALC_PARAM_WA_HISP_IND.equals(field.getParameter())) {
                value = !hispanicEthnicity.isEmpty() ? CODE_YES : null;
                if (value == null) {
                    value = !nonHispanicEthnicity.isEmpty() ? CODE_NO : null;
                }
                if (value == null) {
                    value = student.getPerson().getHispanicLatinoIndicator() ? CODE_YES : CODE_NO;
                }
            } else if (CALC_PARAM_WA_RACE.equals(field.getParameter())) {
                if (!filteredRaces.isEmpty()) {
                    if (filteredRaces.size() > 1) {
                        value = MULTY_RACES;
                    } else {
                        Race race = filteredRaces.iterator().next();
                        value = stdCRDCData.lookupCRDCCodeByBeanPath(Race.class, Race.COL_RACE_CODE,
                                race.getRaceCode());
                    }
                }
            }

            return value;
        }
    }

    /**
     * WA Retriever to determine if a student is in IB program.
     *
     * @author X2 Development Corporation
     *
     */
    public class WARetrieverIbPgm extends RetrieverStdCrs {
        public static final String ALIAS_CRS_DOE_DESIGNATION = "DOE CRS DESIGNATION";
        public static final String CRS_STATE_CODE = "I";

        /**
         * Apply criteria.
         *
         * @see com.x2dev.procedures.statereporting.CRDCStudentData.RetrieverStdCrs#applyCriteria()
         */
        @Override
        void applyCriteria() {
            DataDictionaryField crsFieldDesignation =
                    getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_CRS_DOE_DESIGNATION);
            Set<String> crsDesigantionCodes = new HashSet();

            if (crsFieldDesignation != null && !StringUtils.isEmpty(crsFieldDesignation.getReferenceTableOid())) {
                ReferenceTable refTable = crsFieldDesignation.getReferenceTable();
                if (refTable != null) {
                    Collection<ReferenceCode> refCodes = refTable.getReferenceCodes();

                    if (refCodes != null) {
                        for (ReferenceCode refCode : refCodes) {
                            if (refCode.getStateCode() != null && refCode.getStateCode().contains(CRS_STATE_CODE)) {
                                crsDesigantionCodes.add(refCode.getCode());
                            }
                        }
                    }
                }

                if (!crsDesigantionCodes.isEmpty()) {
                    this.addInCourseField(crsFieldDesignation.getJavaName(), crsDesigantionCodes);
                }
            }
        }

        /**
         * Do span dates check.
         *
         * @return true, if successful
         * @see com.x2dev.procedures.statereporting.CRDCStudentData.RetrieverStdCrs#doSpanDatesCheck()
         */
        @Override
        boolean doSpanDatesCheck() {
            return true;
        }
    }

    public static final String ALIAS_SKL_NCES_ID = "all-skl-NCESSchoolID";
    public static final String ALIAS_STD_EXCLUDE_CRDC = "all-std-ExcludefromCRDC";

    public static final String CODE_NO = "N";
    public static final String CODE_YES = "Y";
    public static final String FIELD_ACTIVE_PART_1 = "ActivePart1";
    public static final String FIELD_ACTIVE_PART_2 = "ActivePart2";
    public static final String FIELD_IDEA = "IDEA";
    public static final String FIELD_SCHOOL_OID = "SchoolOid";
    public static final String PARAM_CODE_IDEA = "IDEA";
    public static final String PARAM_CODE_LEP = "LEP";
    public static final String PARAM_LEP_ENROLLED = "LEPEnrolled";
    public static final String PARAM_CODE_SECTIN_504 = "Section504";


    // Snapshot input values
    public static final String SNAP_504 = "504";
    public static final String SNAP_IDEA = "IDEA";
    public static final String SNAP_LEP = "LEP";
    public static final String SNAP_LEP_PGM = "LEPPgm";
    public static final String SNAP_OOD = "OOD";
    public static final String SNAP_RETAINED = "retained";

    protected CRDCConductHelper m_conductHelper;
    protected String m_fieldSklIdNCES;
    protected PlainDate m_reportDatePart1;
    protected PlainDate m_reportDatePart2;
    protected String m_selSnapRetained;
    protected String m_stdExcludeFromCRDC;
    protected Collection<String> m_stdSnap504;
    protected Collection<String> m_stdSnapIDEA;
    protected Collection<String> m_stdSnapLEP;
    protected Collection<String> m_stdSnapLEPPgm;
    protected Collection<String> m_stdSnapOOD;
    protected Collection<String> m_stdSnapRetained;
    protected CRDCStudentHistoryHelper m_studentHelper;
    SimpleDateFormat m_dateFormat = new SimpleDateFormat("yyyy-MM-dd");

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.x2dev.procedures.statereporting.CRDCReportData#initialize()
     */
    @Override
    public void initialize() throws X2BaseException {
        super.initialize();


        m_stdExcludeFromCRDC = translateAliasToJavaName(ALIAS_STD_EXCLUDE_CRDC, false);
        m_fieldSklIdNCES = translateAliasToJavaName(ALIAS_SKL_NCES_ID, false);
        m_reportDatePart1 = (PlainDate) getParameter(PARAM_PART_1_DATE);

        m_selSnapRetained = (String) getParameter(SNAP_RETAINED);

        initializeCtxByReportDate();
        initializeStudentsByShapshots();

        if (getParameter(PARAM_USE_BLOCK_SCHEDULING) != null) {
            Boolean useBlockScheduling = (Boolean) getParameter(PARAM_USE_BLOCK_SCHEDULING);
            if (useBlockScheduling.booleanValue()) {
                Calendar calendar = Calendar.getInstance();
                calendar.setTime(m_reportDatePart1);
                calendar.add(Calendar.MONTH, 5);
                m_reportDatePart2 = new PlainDate(calendar.getTime());
            }
        }
        initStudentHistoryHelper();
        setQuery(m_studentHelper.getStudentQuery(true));
        setEntityClass(StudentCRDCEntity.class);
        m_conductHelper = new CRDCConductHelper(getBroker(), getCurrentContext());

        CRDCDataHelper crdcHelper = new CRDCDataHelper(this);
        addCalcs(crdcHelper.getUsedRetrievers());
        addValidators(crdcHelper.getUsedValidators());
    }

    /**
     * Initialize separate student history helper.
     *
     * @param date PlainDate
     * @return CRDC student history helper
     */
    protected CRDCStudentHistoryHelper getSchedHistoryHelper(PlainDate date) {
        CRDCStudentHistoryHelper studentHelper = new CRDCStudentHistoryHelper(this);
        studentHelper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
        studentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, date);
        studentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_INPUT, Boolean.TRUE);
        studentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_EXCLUDE, Boolean.TRUE);
        studentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_SCHEDULE_SPANS, Boolean.TRUE);
        if (!StringUtils.isEmpty(m_stdExcludeFromCRDC)) {
            m_studentHelper.getStudentCriteria().addNotEqualTo(m_stdExcludeFromCRDC, BooleanAsStringConverter.TRUE);

        }

        return studentHelper;
    }

    /**
     * Initialize a history helper with student schedules limited to a set of boolean aliases on the
     * course.
     *
     * @param date PlainDate
     * @param aliases Collection<String>
     * @return CRDC student history helper
     */
    protected CRDCStudentHistoryHelper getSchedHistoryHelperCrs(PlainDate date, Collection<String> aliases) {
        CRDCStudentHistoryHelper m_scheduleHelper = getSchedHistoryHelper(date);

        X2Criteria andCriteria = new X2Criteria();
        for (String alias : aliases) {
            String javaName = translateAliasToJavaName(alias, true);
            X2Criteria orCriteria = new X2Criteria();
            orCriteria.addEqualTo(StudentSchedule.REL_SECTION + ModelProperty.PATH_DELIMITER +
                    MasterSchedule.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER +
                    SchoolCourse.REL_COURSE + ModelProperty.PATH_DELIMITER + javaName, BooleanAsStringConverter.TRUE);
            andCriteria.addOrCriteria(orCriteria);
        }
        m_scheduleHelper.getStudentScheduleCriteria().addAndCriteria(andCriteria);

        andCriteria = new X2Criteria();
        for (String alias : aliases) {
            String javaName = translateAliasToJavaName(alias, true);
            X2Criteria orCriteria = new X2Criteria();
            orCriteria.addEqualTo(StudentScheduleChange.REL_MASTER_SCHEDULE + ModelProperty.PATH_DELIMITER +
                    MasterSchedule.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER +
                    SchoolCourse.REL_COURSE + ModelProperty.PATH_DELIMITER + javaName, BooleanAsStringConverter.TRUE);
            andCriteria.addOrCriteria(orCriteria);
        }
        m_scheduleHelper.getStudentScheduleChangeCriteria().addAndCriteria(andCriteria);

        return m_scheduleHelper;
    }

    /**
     * Initialize student history helper.
     */
    protected void initStudentHistoryHelper() {
        m_studentHelper = new CRDCStudentHistoryHelper(this);
        m_studentHelper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
        m_studentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDatePart1);
        m_studentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_INPUT, Boolean.TRUE);
        m_studentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_EXCLUDE, Boolean.TRUE);
        if (!StringUtils.isEmpty(m_stdExcludeFromCRDC)) {
            m_studentHelper.getStudentCriteria().addNotEqualTo(m_stdExcludeFromCRDC, BooleanAsStringConverter.TRUE);
        }
        /* TODO: NON_NCES input parameter is need to be ignored for now, since it causing S-72388 to appear.
           TODO:  I do not delete this until, S-72197 spike will not confirm to do so, or do something else with it.

         */
//        if (getParameter(PARAM_EXCLUDE_ENR_SKL_NON_NCES) != null
//                && ((Boolean) getParameter(PARAM_EXCLUDE_ENR_SKL_NON_NCES)).booleanValue()) {
//            if (m_fieldSklIdNCES != null) {
//                m_studentHelper.getStudentCriteria()
//                        .addNotEmpty(SisStudent.REL_SCHOOL + ModelProperty.PATH_DELIMITER + m_fieldSklIdNCES,
//                                getBroker().getPersistenceKey());
//            } else {
//                m_studentHelper.getStudentCriteria()
//                        .addEqualTo(SisStudent.COL_SCHOOL_OID, "___ZZZ___");
//            }
//        }
    }

    /**
     * Return records set by snapshot.
     *
     * @param snapshot String
     * @return Collection
     */
    private Collection getSnapRecords(String snapshot) {
        X2Criteria recordSetCriteria = new X2Criteria();
        recordSetCriteria.addEqualTo(RecordSetKey.COL_RECORD_SET_OID, snapshot);
        SubQuery recordSetSubquery = new SubQuery(RecordSetKey.class, RecordSetKey.COL_OBJECT_OID, recordSetCriteria);
        return getBroker().getSubQueryCollectionByQuery(recordSetSubquery);
    }

    /**
     * Initialize CTX by report date.
     */
    private void initializeCtxByReportDate() {
        X2Criteria criteria = new X2Criteria();
        criteria.addGreaterOrEqualThan(DistrictSchoolYearContext.COL_END_DATE, m_reportDatePart1);
        criteria.addLessOrEqualThan(DistrictSchoolYearContext.COL_START_DATE, m_reportDatePart1);

        DistrictSchoolYearContext context = (DistrictSchoolYearContext) getBroker()
                .getBeanByQuery(new QueryByCriteria(DistrictSchoolYearContext.class, criteria));
        this.setCurrentContext(context);
    }

    /**
     * Initialize students by shapshots.
     */
    private void initializeStudentsByShapshots() {
        String snap504 = (String) getParameter(SNAP_504);
        if (!StringUtils.isEmpty(snap504)) {
            m_stdSnap504 = new ArrayList<String>();
            m_stdSnap504.addAll(getSnapRecords(snap504));
        }

        String snapIDEA = (String) getParameter(SNAP_IDEA);
        if (!StringUtils.isEmpty(snapIDEA)) {
            m_stdSnapIDEA = new ArrayList<String>();
            m_stdSnapIDEA.addAll(getSnapRecords(snapIDEA));
        }

        String snapOOD = (String) getParameter(SNAP_OOD);
        if (!StringUtils.isEmpty(snapOOD)) {
            m_stdSnapOOD = new ArrayList<String>();
            m_stdSnapOOD.addAll(getSnapRecords(snapOOD));
        }

        String snapLEP = (String) getParameter(SNAP_LEP);
        if (!StringUtils.isEmpty(snapLEP)) {
            m_stdSnapLEP = new ArrayList<String>();
            m_stdSnapLEP.addAll(getSnapRecords(snapLEP));
        }

        String snapLEPPgm = (String) getParameter(SNAP_LEP_PGM);
        if (!StringUtils.isEmpty(snapLEPPgm)) {
            m_stdSnapLEPPgm = new ArrayList<String>();
            m_stdSnapLEPPgm.addAll(getSnapRecords(snapLEPPgm));
        }

        if (!StringUtils.isEmpty(m_selSnapRetained)) {
            m_stdSnapRetained = new ArrayList<String>();
            m_stdSnapRetained.addAll(getSnapRecords(m_selSnapRetained));
        }
    }
}
