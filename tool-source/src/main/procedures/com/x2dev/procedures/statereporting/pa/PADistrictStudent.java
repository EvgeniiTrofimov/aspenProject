/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2006 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.procedures.statereporting.pa;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.*;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.business.CalendarManager;
import com.x2dev.sis.tools.stateexports.StudentEnrollmentSpan;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.sis.tools.stateexports.StudentScheduleSpan;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.text.SimpleDateFormat;
import java.util.*;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * The Class PADistrictStudent.
 */
public class PADistrictStudent extends StateReportData {

    /**
     * The Class PADistrictStudentEntity.
     */
    /* Entity class */
    public static class PADistrictStudentEntity extends StateReportEntity {
        PADistrictStudent m_sData;
        Map<String, StudentEnrollmentSpan> m_schoolSpans;
        List<StudentEnrollmentSpan> m_spans;
        SisStudent m_student;

        /**
         * Instantiates a new PA district student entity.
         */
        public PADistrictStudentEntity() {
            // Public no argument constructor for dynamic instantiation.
        }

        /**
         * Return the span currently being processed.
         *
         * @return Student enrollment span
         */
        public StudentEnrollmentSpan getCurrentSpan() {
            return m_spans.get(getCurrentRow());
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            SisStudent student = (SisStudent) getBean();
            String name = student.getNameView() +
                    " [LASID: " + student.getLocalId() +
                    ", SASID: " + student.getStateId() +
                    ", School ID: " + getCurrentSpan().getSchool().getSchoolId() +
                    "] ";

            return name;
        }

        /**
         * Intitialize.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            m_sData = (PADistrictStudent) data;
            m_student = (SisStudent) getBean();
            String sklOid = m_student.getSchool().getOid();
            m_schoolSpans = new HashMap<String, StudentEnrollmentSpan>();

            if (sklOid != null) {
                for (StudentEnrollmentSpan span : m_sData.m_helper.getStudentEnrollmentSpans(m_student, true)) {
                    String spanSklOid = span.getSchool().getOid();
                    boolean isSchoolOK = sklOid.equals(spanSklOid) && m_sData.m_setSchoolOids.contains(sklOid);
                    if (isSchoolOK && dateRangeOK(span)) {
                        m_schoolSpans.put(span.getSchool().getOid(), span);
                    }
                }
            }

            m_spans = new ArrayList<StudentEnrollmentSpan>(m_schoolSpans.keySet().size());
            m_spans.addAll(m_schoolSpans.values());
            setRowCount(m_schoolSpans.keySet().size());
        }

        /**
         *
         * Test if date range is included.
         *
         * @param span StudentEnrollmentSpan
         * @return true, if successful
         */
        private boolean dateRangeOK(StudentEnrollmentSpan span) {
            boolean retValue = false;

            PlainDate startDate = span.getFirstActiveEnrollment().getEnrollmentDate();
            PlainDate endDate = span.getFirstInactiveEnrollment() == null
                    ? m_sData.getOrganization().getCurrentContext().getEndDate()
                    : span.getFirstInactiveEnrollment().getEnrollmentDate();

            if (m_sData.m_isSnapshot) {
                if (!startDate.after(m_sData.m_reportDate) && !endDate.before(m_sData.m_reportDate)) {
                    retValue = true;
                }
            } else {
                retValue = true;
            }

            return retValue;
        }
    }

    /**
     * Gets the date.
     *
     * @param month int
     * @param day int
     * @param year int
     * @return Date
     */
    private static Date getDate(int month, int day, int year) {
        Calendar calendar = Calendar.getInstance();
        calendar.set(Calendar.MONTH, month);
        calendar.set(Calendar.DATE, day);
        calendar.set(Calendar.YEAR, year);

        return calendar.getTime();
    }

    /**
     * Field retriever for district entry date.
     */
    protected class RetrieveDistrictEntryDate implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String value = null;

            SisStudent student = (SisStudent) entity.getBean();

            List<StudentEnrollmentSpan> spans = m_helper.getStudentEnrollmentSpans(student, false);
            for (StudentEnrollmentSpan span : spans) {
                if (span.getFirstActiveEnrollment() != null) {
                    value = span.getFirstActiveEnrollment().getEnrollmentDate().toString();
                    break;
                }
            }

            PADistrictStudent seData = (PADistrictStudent) data;

            if (StringUtils.isEmpty(value)) {
                value = seData.m_entryDate;
            }

            return value;
        }
    }

    /**
     * Field retriever for funding.
     */
    protected class RetrieveFunding implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            SisStudent student = (SisStudent) entity.getBean();
            String code = (String) student.getFieldValueByBeanPath(m_fieldFundingDistrictCode);
            if (StringUtils.isEmpty(code)) {
                code = (String) student.getFieldValueByBeanPath(m_fieldDistrictResidence);
            }
            if (StringUtils.isEmpty(code)) {
                code = getOrganization().getId();
            }

            return code;
        }
    }

    /**
     * Field retriever for homeroom.
     */
    protected class RetrieveHomeroom implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {

            SisStudent student = (SisStudent) entity.getBean();
            String homeroom = student.getHomeroom();

            for (StudentScheduleSpan stdScheduleSpan : m_helper.getStudentScheduleSpans(student)) {
                MasterSchedule section = stdScheduleSpan.getSection();
                Course course = section.getSchoolCourse().getCourse();
                String isHomeroom = (String) course.getFieldValueByBeanPath(m_fieldHomeroom);
                if (BooleanAsStringConverter.TRUE.equals(isHomeroom)) {
                    homeroom = section.getRoomView();
                    break;
                }
            }

            return homeroom;
        }
    }

    /**
     * Field retriever for location code of the residence.
     */
    protected class RetrieveLocationResidenceCode implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            SisStudent student = (SisStudent) entity.getBean();
            String value = (String) student.getFieldValueByBeanPath(m_fieldLocationCodeOfResidance);
            if (StringUtils.isEmpty(value)) {
                value = student.getSchool().getSchoolId();
            }

            return value;
        }
    }

    /**
     * Retrieve program related values from the student programs.
     */
    protected class RetrieveProgram implements FieldRetriever {
        private static final String CALC_PARAM_AUN = "BID_AUN";
        private static final String CALC_PARAM_SKL_NUMBER = "BID_SKL_NUMBER";
        private static final String CALC_PARAM_PASA_AUN = "PASA_AUN";
        private static final String PARAM_DISADVANTAGE = "ECONOMIC_DISADVANTAGE";
        private static final String PARAM_ECONOMIC_STATUS = "ECONOMIC_STATUS";
        private static final String PARAM_GIFTED_TALANTED = "GIFTED_AND_TALANTED";
        private static final String PARAM_LEP_ELIGIB = "LEP_ELIGIBILITY";
        private static final String PARAM_TITLE_I = "TITLE_I";
        private static final String PARAM_HOMELESS_STATUS = "HOMELESS_STATUS";
        private static final String ADULT_AFFIDAVIT_PROGRAM_CODE = "AAP";

        private static final String VALUE_N = "N";
        private static final String VALUE_Y = "Y";

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
            String parameter = (String) field.getParameter();
            Object value = null;
            if (PARAM_DISADVANTAGE.equals(parameter)) {
                value = null;
                List<StudentProgramParticipation> programs = getStatePrograms(PGM_LUNCH, entity.getBean().getOid());
                for (StudentProgramParticipation program : programs) {
                    value = program.getFieldValueByBeanPath(m_fieldReducedLunch);
                    if ("F".equals(value) || "R".equals(value)) {
                        value = VALUE_Y;
                    }
                }
            }
            if (PARAM_ECONOMIC_STATUS.equals(parameter)) {
                value = null;
                List<StudentProgramParticipation> programs = getStatePrograms(PGM_LUNCH, entity.getBean().getOid());
                for (StudentProgramParticipation program : programs) {
                    value = program.getFieldValueByBeanPath(m_fieldReducedLunch);
                    break;
                }
            } else if (PARAM_GIFTED_TALANTED.equals(parameter)) {
                value = null;
                List<StudentProgramParticipation> programs = getStatePrograms(PGM_GIFTED, entity.getBean().getOid());

                if (programs.isEmpty()) {
                    return VALUE_N;
                }

                for (StudentProgramParticipation program : programs) {
                    value = lookupReferenceCodeByBeanPath(StudentProgramParticipation.class, m_fieldGiftedStatusCode,
                            (String) program.getFieldValueByBeanPath(m_fieldGiftedStatusCode),
                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                    break;
                }
            } else if (PARAM_LEP_ELIGIB.equals(parameter)) {
                value = null;
                List<StudentProgramParticipation> localPrograms = getStatePrograms(PGM_LEP, entity.getBean().getOid());
                for (StudentProgramParticipation program : localPrograms) {
                    value = program.getFieldValueByBeanPath(m_fieldTitle3Served);
                    if (BooleanAsStringConverter.TRUE.equals(value)) {
                        value = VALUE_Y;
                        break;
                    }
                    value = VALUE_N;
                }
            } else if (CALC_PARAM_SKL_NUMBER.equals(parameter)) {
                value = null;
                List<StudentProgramParticipation> localPrograms = getStatePrograms(PGM_BID, entity.getBean().getOid());
                if (localPrograms != null && !localPrograms.isEmpty()) {
                    for (StudentProgramParticipation program : localPrograms) {
                        String bidSklNum = (String) program.getFieldValueByBeanPath(m_fieldPgmBidSchool);
                        if (!StringUtils.isEmpty(bidSklNum)) {
                            value = bidSklNum;
                            break;
                        }
                    }
                }
            } else if (CALC_PARAM_AUN.equals(parameter)) {
                value = null;
                List<StudentProgramParticipation> localPrograms = getStatePrograms(PGM_BID, entity.getBean().getOid());
                if (localPrograms != null && !localPrograms.isEmpty()) {
                    for (StudentProgramParticipation program : localPrograms) {
                        String bidAun = (String) program.getFieldValueByBeanPath(m_fieldPgmBidAun);
                        if (!StringUtils.isEmpty(bidAun)) {
                            value = bidAun;
                            break;
                        }
                    }
                }
            } else if (CALC_PARAM_PASA_AUN.equals(parameter)) {
                value = null;
                String asmPart = (String) student.getFieldValueByBeanPath(m_fieldStdAsmPart);
                if (!StringUtils.isEmpty(asmPart)) {
                    asmPart = data.lookupStateValue(SisStudent.class, m_fieldStdAsmPart, asmPart);
                    if (!StringUtils.isEmpty(asmPart) && "A".equalsIgnoreCase(asmPart)) {
                        List<StudentProgramParticipation> localPrograms =
                                getStatePrograms(PGM_IEP, entity.getBean().getOid());
                        if (localPrograms != null && !localPrograms.isEmpty()) {
                            for (StudentProgramParticipation program : localPrograms) {
                                String pasaAun = (String) program.getFieldValueByBeanPath(m_fieldPgmPasaAun);
                                if (!StringUtils.isEmpty(pasaAun)) {
                                    value = pasaAun;
                                    break;
                                }
                            }
                        }
                    }
                }
            } else if (PARAM_TITLE_I.equals(parameter)) {
                value = VALUE_N;
                List<StudentProgramParticipation> localPrograms =
                        getStatePrograms(PGM_TITLE_I, entity.getBean().getOid());
                for (StudentProgramParticipation program : localPrograms) {
                    if (insideCurrentYear(program.getStartDate(), program.getEndDate())) {
                        value = VALUE_Y;
                        break;
                    }

                }
            } else if (PARAM_HOMELESS_STATUS.equals(parameter)) {
                value = VALUE_N;
                String gradeStateCode =
                        lookupStateValue(Student.class, Student.COL_GRADE_LEVEL, student.getGradeLevel());
                if (ADULT_AFFIDAVIT_PROGRAM_CODE.equals(gradeStateCode)) {
                    List<StudentProgramParticipation> programs =
                            getStatePrograms(PGM_HOMELESS_ACC, student.getOid());
                    if (!programs.isEmpty()) {
                        value = VALUE_Y;
                    }
                    programs = getStatePrograms(PGM_HOMELESS_UNACC, student.getOid());
                    if (!programs.isEmpty()) {
                        value = VALUE_Y;
                    }
                }
            }
            return value;
        }

        /**
         * Inside current year.
         *
         * @param startDate PlainDate
         * @param endDate PlainDate
         * @return true, if successful
         */
        private boolean insideCurrentYear(PlainDate startDate, PlainDate endDate) {
            boolean isInside = false;
            if (startDate != null) {
                PlainDate startYear = getCurrentContext().getStartDate();
                PlainDate endYear = getCurrentContext().getEndDate();
                if (!startDate.after(endYear)
                        && (endDate == null || !endDate.before(startYear))) {
                    isInside = true;
                }
            }
            return isInside;
        }

        /**
         * Retrieve state program.
         *
         * @param stateCode String
         * @param studentOid String
         * @return List
         */
        private List<StudentProgramParticipation> getStatePrograms(String stateCode, String studentOid) {
            List<StudentProgramParticipation> statePrograms = new ArrayList<StudentProgramParticipation>();

            Set<String> codes = m_stateCodes.get(stateCode);
            List<StudentProgramParticipation> programs = m_programs.get(studentOid);
            if (codes != null && programs != null) {
                for (StudentProgramParticipation program : programs) {
                    if (codes.contains(program.getProgramCode())) {
                        statePrograms.add(program);
                    }
                }
            }

            return statePrograms;
        }
    }

    /**
     * Retrieve classes as an example.
     */
    protected class RetrieveRace implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            Object value = null;

            SisStudent student = (SisStudent) entity.getBean();
            Person studentPerson = student.getPerson();
            if (studentPerson == null) {
                return null;
            }
            Collection<Race> races = m_races.get(studentPerson.getOid());
            if (races == null) {
                return null;
            }
            if (studentPerson.getHispanicLatinoIndicator()) {
                value = "4";
            } else {
                if (races.size() > 1) {
                    value = "6";
                } else if (races.size() == 1) {
                    value = lookupReferenceCodeByBeanPath(Race.class,
                            Race.COL_RACE_CODE,
                            races.iterator().next().getRaceCode(),
                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                }

            }

            return value;
        }
    }


    /**
     * Retrieve report date.
     */
    protected class RetrieveReportDate implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            PADistrictStudent seData = (PADistrictStudent) data;
            if (EXPORT_TYPE_STUDENT.equals(m_exportType)) {
                return null;
            }
            return seData.m_reportDate;
        }
    }

    /**
     * Retrieve district code of the residence.
     */
    protected class RetrieveResCode implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            SisStudent student = (SisStudent) entity.getBean();
            String code = (String) student.getFieldValueByBeanPath(m_fieldDistrictCodeOfResidence);
            return StringUtils.isEmpty(code) ? getOrganization().getId() : code;
        }
    }

    /**
     * Retrieve school to which the student based on its residence.
     */
    protected class RetrieveResidenceSchool implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            PADistrictStudentEntity stdEntity = (PADistrictStudentEntity) entity;
            String value = (String) stdEntity.m_student.getFieldValueByBeanPath(m_fieldResidenceSchool);
            if (StringUtils.isEmpty(value)) {
                SisSchool school = stdEntity.getCurrentSpan().getSchool();

                value = (String) school.getFieldValueByBeanPath(m_fieldSchoolStateId);
            }
            return value;
        }
    }

    /**
     * Retrieve school.
     */
    protected class RetrieveSchool implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            PADistrictStudentEntity stdEntity = (PADistrictStudentEntity) entity;
            Object value = null;
            SisStudent std = (SisStudent) entity.getBean();
            PADistrictStudent stdData = (PADistrictStudent) data;
            if (std.getFieldValueByBeanPath(stdData.m_fieldStdSklOverride2) != null) {
                value = std.getFieldValueByBeanPath(stdData.m_fieldStdSklOverride2);
            } else {
                SisSchool school = stdEntity.getCurrentSpan().getSchool();
                value = school.getFieldValueByBeanPath(m_fieldSchoolStateId);
            }
            return value;
        }
    }

    /**
     * Retrieve entry date for school.
     */
    protected class RetrieveSchoolEntryDate implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String value = null;

            PADistrictStudentEntity stdEntity = (PADistrictStudentEntity) entity;
            PADistrictStudent seData = (PADistrictStudent) data;
            SisStudent student = (SisStudent) entity.getBean();
            SisSchool school = stdEntity.getCurrentSpan().getSchool();

            List<StudentEnrollmentSpan> spans = m_helper.getStudentEnrollmentSpans(student, false);
            if (school != null) {
                boolean otherSchoolFound = false;
                for (StudentEnrollmentSpan span : spans) {
                    if (span.getFirstActiveEnrollment() != null && span.getSchool() != null &&
                            span.getSchool().getOid().equals(school.getOid()) &&
                            !span.getFirstActiveEnrollment().getEnrollmentDate().after(seData.m_reportDate)) {
                        if (value == null || otherSchoolFound) {
                            value = span.getFirstActiveEnrollment().getEnrollmentDate().toString();
                            otherSchoolFound = false;
                        }
                    }
                    if (value != null && !span.getSchool().getOid().equals(school.getOid())) {
                        otherSchoolFound = true;
                    }
                }
            }

            if (StringUtils.isEmpty(value)) {
                value = seData.m_entryDate;
            }

            return value;
        }
    }

    /**
     * Retrieve end year date for school.
     */
    protected class RetrieveSchoolYearDate implements FieldRetriever {
        private SimpleDateFormat formatter = new SimpleDateFormat("yyyy-06-30");

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
            PlainDate date = getOrganization().getCurrentContext().getEndDate();
            Calendar cal = Calendar.getInstance();
            cal.setTime(date);

            return formatter.format(cal.getTime());
        }

    }
    /**
     * Retrieve snapshot date.
     */
    protected class RetrieveSnapshotDate implements FieldRetriever {
        private SimpleDateFormat m_formatter = new SimpleDateFormat("mm/dd/yyyy");

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            PlainDate currentDate = new PlainDate();
            Calendar cal = Calendar.getInstance();
            cal.setTime(currentDate);
            return m_formatter.format(cal.getTime());
        }
    }

    /**
     * Retrieve SSN.
     */
    protected class RetrieveSocialSecurityNo implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            Object value = null;
            SisStudent student = (SisStudent) entity.getBean();
            String ssn = student.getPerson().getPersonId();
            if (ssn != null) {
                value = ssn.replaceAll("[^\\d]", "");
            }
            return value;
        }

    }

    /**
     * Retrieve Special Education status.
     */
    protected class RetrieveSpecialEducation implements FieldRetriever {
        PlainDate m_beginDate;
        PlainDate m_endDate;

        /**
         * @param schoolYear
         */
        public RetrieveSpecialEducation(int schoolYear) {
            m_beginDate = new PlainDate(getDate(Calendar.JULY, 1, schoolYear - 1));
            m_endDate = new PlainDate(getDate(Calendar.JUNE, 30, schoolYear - 1));
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String value = "N";
            SisStudent student = (SisStudent) entity.getBean();
            StudentProgramParticipation program = m_iepProgram.get(student.getOid());
            PlainDate firstInsessionDate = getFirstSchoolYearDate(student.getSchool(), student.getCalendarCode());
            StudentEnrollment enrForDate =
                    m_helper.getEnrollmentForDate(student.getOid(), m_reportDate, StudentEnrollment.WITHDRAWAL
                            + StudentEnrollment.ENTRY + StudentEnrollment.YOG_CHANGE + StudentEnrollment.STATUS_CHANGE);
            if (program != null) {
                if (program.getEndDate() == null || !program.getEndDate().before(m_reportDate)
                        || (enrForDate != null && StudentEnrollment.WITHDRAWAL.equals(enrForDate.getEnrollmentType())
                                && program.getEndDate().after(m_endDate)
                                && program.getEndDate().before(firstInsessionDate))) {
                    value = "Y";
                } else if (program.getEndDate() != null && DateUtils.isBetween(program.getEndDate(),
                        getCurrentContext().getStartDate(), getCurrentContext().getEndDate())) {
                    value = "E";
                } else {
                    value = "N";
                }
            }
            return value;
        }
    }

    /**
     * Custom validation for required fields.
     */
    protected class ValidateFieldValue implements FieldValidator {

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

            if (StringUtils.isEmpty(value)) {
                errors.add(new StateReportValidationError(entity, field, "Value Required",
                        "This field must contain a value"));
            }

            return errors;
        }
    }



    /**
     * The Class ValidateLIEP.
     */
    protected class ValidateLIEP implements FieldValidator {

        private static final String LIEP_27 = "27";
        private static final String LIEP_26 = "26";
        private static final String VAL_ID = "STD_VAL_LIEP";
        private static final String FIELD_LIEP_TYPE = "LIEP Program Type";
        private static final String FIELD_CURRENT_GRADE_LEVEL = "CURRENT GRADE LEVEL";
        private List<String> m_elStatus0106 = Arrays.asList("01", "06");
        private List<String> m_neededGrades = Arrays.asList("PKA", "PKP", "PKF");

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
            String elStatusValue = entity.getFieldValue(FIELD_EL_STATUS);

            if (!m_elStatus0106.contains(elStatusValue) && LIEP_27.equals(value)) {
                errors.add(new StateReportValidationError(entity, field, FIELD_EL_STATUS + " should has "
                        + m_elStatus0106 + " valid values if " + FIELD_LIEP_TYPE + " = 27",
                        FIELD_EL_STATUS + " = " + elStatusValue + " " + FIELD_LIEP_TYPE + " = " + value));
            } else if (m_elStatus0106.contains(elStatusValue)) {
                String gradeLvl = entity.getFieldValue(FIELD_CURRENT_GRADE_LEVEL);
                if (m_neededGrades.contains(gradeLvl) && !LIEP_26.equals(value)) {
                    errors.add(new StateReportValidationError(entity, field, FIELD_LIEP_TYPE + " should has " + LIEP_26
                            + " value if "
                            + FIELD_EL_STATUS + " takes one of the values: " + m_elStatus0106
                            + " and grade level takes one of the values: " + m_gradeLevelsHS,
                            FIELD_EL_STATUS + " = " + elStatusValue + " " + FIELD_LIEP_TYPE + " = " + value
                                    + FIELD_CURRENT_GRADE_LEVEL + " = " + gradeLvl));
                } else if (StringUtils.isEmpty(value)) {
                    errors.add(new StateReportValidationError(entity, field, FIELD_LIEP_TYPE + " should has value if "
                            + FIELD_EL_STATUS + " takes one of the next values: " + m_elStatus0106,
                            FIELD_EL_STATUS + " = " + elStatusValue + " " + FIELD_LIEP_TYPE + " = " + value));
                }
            }

            return errors;
        }
    }
    /**
     * Custom validation for homeroom courses.
     */
    protected class ValidateHomeroom implements FieldValidator {

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

            if (!StringUtils.isEmpty(value)) {
                // check if we've exact one homeroom extracted from
                // courses/sections
                String homeroom = null;

                SisStudent student = (SisStudent) entity.getBean();

                for (StudentScheduleSpan stdScheduleSpan : m_helper.getStudentScheduleSpans(student)) {
                    MasterSchedule section = stdScheduleSpan.getSection();
                    Course course = section.getSchoolCourse().getCourse();
                    String isHomeroom = (String) course.getFieldValueByBeanPath(m_fieldHomeroom);
                    if ("1".equals(isHomeroom)) {
                        if (homeroom != null && !homeroom.equals(section.getRoomView())) {
                            errors.add(new StateReportValidationError(entity, field, "Multiple homeroom values",
                                    "There should be exact one homeroom, selected from courses and sections, but found at least two: "
                                            +
                                            homeroom + " and " + section.getRoomView()));
                            break;
                        }
                        homeroom = section.getRoomView();
                    }
                }
            } else {
                errors.add(new StateReportValidationError(entity, field, "Value Required",
                        "This field must contain a value"));
            }

            return errors;
        }
    }

    /**
     * Validate post graduate activity.
     */
    protected class ValidatePostGraduateActivity implements FieldValidator {

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

            boolean isDropout = false;
            boolean isGraduate = false;
            String gradStatus = entity.getFieldValue(FIELD_GRADUATION_STATUS);
            if (!StringUtils.isEmpty(gradStatus)) {
                isDropout = gradStatus.matches("^[ABCDWRO]$");
                isGraduate = gradStatus.matches("^[G]$");
            }
            if (StringUtils.isEmpty(value)) {
                if (isDropout || isGraduate) {
                    errors.add(new StateReportValidationError(entity, field, "Value Required",
                            "This field must contain a value for graduates and dropouts"));
                }
            } else {
                int codeValue = -1;
                try {
                    codeValue = Integer.parseInt(value);
                } catch (NumberFormatException e) {
                    // trap non-numeric code
                }
                if (codeValue == -1) {
                    errors.add(new StateReportValidationError(entity, field, "Invalid Value",
                            "The value in this field must be a numeric code"));
                } else if (codeValue == 998 && !isDropout) {
                    errors.add(new StateReportValidationError(entity, field, "Invalid Value", "The value " + value +
                            " may only be used for dropouts"));
                } else if (codeValue >= 10 && codeValue <= 100 && !isGraduate) {
                    errors.add(new StateReportValidationError(entity, field, "Invalid Value", "The value " + value +
                            " may only be used for graduates"));
                } else if (codeValue == 997) {
                    String seValue = entity.getFieldValue(FIELD_SPECIAL_EDUCATION);
                    if (StringUtils.isEmpty(seValue) || (!seValue.equals("Y") && !seValue.equals("E"))) {
                        errors.add(new StateReportValidationError(entity, field, "Invalid Value", "The value " + value +
                                " may only be used for Special Ed students"));
                    }
                }

            }
            return errors;
        }

    }

    /**
     * Validate required.
     */
    protected class ValidateRequired implements FieldValidator {

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

            if (StringUtils.isEmpty(value)) {
                errors.add(new StateReportValidationError(entity, field, "Value Required",
                        "This field must contain a value for HS students"));
            }
            return errors;
        }

    }

    /**
     * Validate whether the student are graduate student.
     */
    protected class ValidateRequiredGraduate implements FieldValidator {

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

            if (StringUtils.isEmpty(value)) {
                String gradStatus = entity.getFieldValue(FIELD_GRADUATION_STATUS);
                if (!StringUtils.isEmpty(gradStatus) && gradStatus.equals("G")) {
                    errors.add(new StateReportValidationError(entity, field, "Value Required",
                            "This field must contain a value for graduated students"));
                }
            }
            return errors;
        }

    }

    /**
     * Validate whether the student to study in high school.
     */
    protected class ValidateRequiredHighSchool implements FieldValidator {

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

            SisStudent student = (SisStudent) entity.getBean();

            if (m_gradeLevelsHS.contains(student.getGradeLevel())) {
                if (StringUtils.isEmpty(value)) {
                    errors.add(new StateReportValidationError(entity, field, "Value Required",
                            "This field must contain a value for HS students"));
                }
            }
            return errors;
        }

    }

    /**
     * Validate required LEP.
     */
    protected class ValidateRequiredLimitedEnglishProficiency implements FieldValidator {

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

            if (StringUtils.isEmpty(value)) {
                String lepValue = entity.getFieldValue(FIELD_LEP);
                if (!StringUtils.isEmpty(lepValue) && lepValue.equals("01")) {
                    errors.add(new StateReportValidationError(entity, field, "Value Required",
                            "This field must contain a value for LEP students"));
                }
            }
            return errors;
        }

    }
    /**
     * Validate required LEP and EL status.
     */
    protected class ValidateElStatus implements FieldValidator {
        private List<String> m_elStatus0106 = Arrays.asList("01", "06");

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

            if (m_elStatus0106.contains(value)) {
                String titleIIIValue = entity.getFieldValue(FIELD_TITLE_III_EL_ELIG);
                if (StringUtils.isEmpty(titleIIIValue)) {
                    errors.add(new StateReportValidationError(entity, field,
                            FIELD_TITLE_III_EL_ELIG + " should have a value if "
                                    + FIELD_EL_STATUS + " is one of the following values: " + m_elStatus0106,
                            FIELD_EL_STATUS + " = " + value + " " + FIELD_TITLE_III_EL_ELIG + " = " + titleIIIValue));
                }
            }
            return errors;
        }

    }

    /**
     * Validate required special education.
     */
    protected class ValidateRequiredSpecialEducation implements FieldValidator {

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

            if (StringUtils.isEmpty(value)) {
                String seValue = entity.getFieldValue(FIELD_SPECIAL_EDUCATION);
                if (!StringUtils.isEmpty(seValue) && (seValue.equals("Y") || seValue.equals("E"))) {
                    errors.add(new StateReportValidationError(entity, field, "Value Required",
                            "This field must contain a value for Special Ed students"));
                }
            }
            return errors;
        }

    }

    /**
     * Validate required student status.
     */
    protected class ValidateRequiredStudentStatus implements FieldValidator {

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

            if (StringUtils.isEmpty(value)) {
                String ctValue = entity.getFieldValue(FIELD_CHALLENGE_TYPE);
                String seValue = entity.getFieldValue(FIELD_SPECIAL_EDUCATION);

                if (!StringUtils.isEmpty(ctValue) && seValue.equals("Y")) {
                    errors.add(new StateReportValidationError(entity, field, "Value Required",
                            "If a Special Ed student has a challenge type, student status must be filled in."));
                }
            }
            return errors;
        }

    }


    /**
     * Validate special education if challenge type.
     */
    protected class ValidateSpecialEducationIfChallengeType implements FieldValidator {

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

            String ctValue = entity.getFieldValue(FIELD_CHALLENGE_TYPE);
            String seValue = entity.getFieldValue(FIELD_SPECIAL_EDUCATION);
            SisStudent student = (SisStudent) entity.getBean();
            PlainDate spedExitDate = student.getSpedExitDate();
            Integer schoolYear = getOrganization().getCurrentContext().getSchoolYear();
            Calendar calStart = Calendar.getInstance();
            Calendar calEnd = Calendar.getInstance();
            calStart.set(schoolYear, 5, 30, 0, 0, 0);
            calEnd.set(schoolYear, 6, 1, 0, 0, 0);

            if (seValue.equals("E") && (spedExitDate == null || spedExitDate.before(calStart.getTime())
                    || spedExitDate.after(calEnd.getTime()))) {
                errors.add(new StateReportValidationError(entity, field, "Invalid Value",
                        "This field value must be \"E\" if a student exited Special Education anytime between 7/1/SY - 6/30/SY."));
            }

            if (!StringUtils.isEmpty(ctValue) && !seValue.equals("Y")) {
                errors.add(new StateReportValidationError(entity, field, "Invalid Value",
                        "This field value must be \"Y\" if a student has a challenge type."));
            }
            return errors;
        }

    }

    protected static final String EXPORT_TYPE_CTE = "EXP_TYPE_CTE";
    protected static final String EXPORT_TYPE_ELL_ACCESS = "EXP_TYPE_ELL_ACCESS";
    protected static final String EXPORT_TYPE_ELL_EOY = "EXP_TYPE_ELL_EOY";
    protected static final String EXPORT_TYPE_OCT_1_SNAP = "EXP_TYPE_OCT_1_SNAP";
    protected static final String EXPORT_TYPE_SPED_DEC = "EXP_TYPE_SPED_DEC";
    protected static final String EXPORT_TYPE_SPED_EOY = "EXP_TYPE_SPED_EOY";
    protected static final String EXPORT_TYPE_STUDENT = "EXP_TYPE_STD";

    private static final String ALIAS_ADULTHOOD = "DOE ADULTHOOD";
    private static final String ALIAS_CIP_NUMBER = "DOE CIP NUMBER";
    private static final String ALIAS_CURRENT = "DOE CURRENT";
    private static final String ALIAS_DIPLOMA = "DOE DIPLOMA";
    private static final String ALIAS_DISTRES = "DOE DISTRICT RESIDENCE";
    private static final String ALIAS_ELL_ACCESS_TESTED = "DOE ELL ACCESS TESTED";
    private static final String ALIAS_FOREIGN = "DOE FOREIGN";
    private static final String ALIAS_FUNDING = "DOE FUNDING";
    private static final String ALIAS_GIFTED = "DOE GIFTED STATUS";
    private static final String ALIAS_HOMEMAKER = "DOE HOMEMAKER";
    private static final String ALIAS_HOMEROOM = "DOE HOMEROOM";
    private static final String ALIAS_LOCATION = "DOE SCHOOL RESIDENCE";
    private static final String ALIAS_LUNCH = "DOE REDUCED LUNCH";
    private static final String ALIAS_PARENT = "DOE PARENT";
    private static final String ALIAS_PGM_BID_AUN = "all-pgm-BIDAUN";
    private static final String ALIAS_PGM_BID_SKL_NUMBER = "all-pgm-BIDSchool";
    private static final String ALIAS_PGM_PASA_AUN = "all-pgm-PASATestingAUN";
    private static final String ALIAS_POSTGRAD = "DOE POSTGRAD";
    private static final String ALIAS_RESIDENCE = "DOE DISTRICT RESIDENCE";
    private static final String ALIAS_RESIDENCE_SCHOOL = "PA_Loc_Res";
    private static final String ALIAS_SCHOOL_STATE_ID = "DOE SCHOOL STATE ID";
    private static final String ALIAS_STD_ASM_PART = "DOE ASSESSMENT PARTICIPATION";
    private static final String ALIAS_STD_SKL_OVERRIDE_2 = "all-std-LocationCodeOverride2";
    private static final String ALIAS_TITLE_III_SERVED = "DOE TITLE III SERVED";
    private static final String ALIAS_US_ENROLL = "DOE USENROLL";
    private static final String ALIAS_US_RECENT_ENROLL = "DOE US RECENT ENROLL";

    private static final String CALC_ID_DISTENTRY = "STD_CALC_DISTENTRY";
    private static final String CALC_ID_DISTRESCODE = "STD_CALC_DISTRESCODE";
    private static final String CALC_ID_FUNDING = "STD_CALC_FUNDING";
    private static final String CALC_ID_HOMEROOM = "STD_CALC_HOMEROOM";
    private static final String CALC_ID_LOCRESCODE = "STD_CALC_LOCRESCODE";
    private static final String CALC_ID_PROGRAM = "STD_CALC_PROGRAM";
    private static final String CALC_ID_RACE = "STD_CALC_RACE";
    private static final String CALC_ID_REPORT_DATE = "STD_CALC_REPORTDATE";
    private static final Object CALC_ID_RESIDENCE_SCHOOL = "STD_CALC_RESSKL";
    private static final String CALC_ID_SKL = "STD_CALC_SKL";
    private static final String CALC_ID_SKLENTRY = "STD_CALC_SKLENTRY";
    private static final Object CALC_ID_SKLYEAR_DATE = "STD_CALC_SKLYEARDATE";
    private static final Object CALC_ID_SNAPSHOT = "SNAPSHOT_DATE";
    private static final String CALC_ID_SPECED = "STD_CALC_SPECED";
    private static final String CALC_ID_SSN = "STD_CALC_SSN";

    private static final String FIELD_CHALLENGE_TYPE = "CHALLENGE TYPE";
    private static final String FIELD_GRADUATION_STATUS = "GRADUATION STATUS CO";
    private static final String FIELD_LEP = "LEP/ELL ELIGIBILITY";
    private static final String FIELD_EL_STATUS = "EL STATUS";
    private static final String FIELD_TITLE_III_EL_ELIG = "TITLE III EL ELIG";
    private static final String FIELD_SPECIAL_EDUCATION = "SPECIAL EDUCATION";

    private static final String GENERAL_FAMILY_CONSUMER_SCIENCE = "19.0101";
    private static final String[] GRADES_HS = {"009", "010", "011", "012"};

    /*
     * User input parameters
     */
    private static final String PARAM_EXPORT_TYPE = "exportType";
    private static final String PARAM_REPORT_DATE = "reportDate";
    private static final String PARAM_SCHOOLS = "schoolOids";

    private static final String PGM_BID = "BID";
    private static final String PGM_GIFTED = "GT";
    private static final String PGM_IEP = "IEP";
    private static final String PGM_LEP = "LEP";
    private static final String PGM_LUNCH = "FRM";
    private static final String PGM_TITLE_I = "Title I";
    private static final String PGM_HOMELESS_UNACC = "055";
    private static final String PGM_HOMELESS_ACC = "056";

    private static final String VAL_ID_HOMEROOM = "STD_VAL_HOMEROOM";
    private static final String VAL_ID_ISEMPTY = "STD_VAL_ISEMPTY";
    private static final String VAL_ID_POST_GRAD_ACTIVITY = "STD_VAL_POST_GRAD";
    private static final String VAL_ID_REQ = "STD_VAL_REQ";
    private static final String VAL_ID_REQ_IF_GRAD = "STD_VAL_REQ_IF_GRAD";
    private static final String VAL_ID_REQ_IF_HS = "STD_VAL_REQ_IF_HS";
    private static final String VAL_ID_REQ_IF_LEP = "STD_VAL_REQ_IF_LEP";
    private static final String VAL_ID_EL_STATUS = "STD_VAL_EL_STATUS";
    private static final String VAL_ID_REQ_IF_SE = "STD_VAL_REQ_IF_SE";
    private static final String VAL_ID_REQ_IF_SECT = "STD_VAL_REQ_IF_SECT";
    private static final String VAL_ID_Y_IF_CT = "STD_VAL_Y_IF_CT";

    /**
     * Calculate difference from now in years.
     *
     * @param date PlainDate
     * @param reportDate PlainDate
     * @return count differences
     */
    static int calculateDifferenceFromNowInYears(PlainDate date, PlainDate reportDate) {
        Calendar calDate = getCalendarFromDate(date);
        Calendar now = getCalendarFromDate(reportDate);
        Calendar clone = (Calendar) calDate.clone();
        int years = -1;
        while (!clone.after(now)) {
            clone.add(Calendar.YEAR, 1);
            years++;
        }
        return years;
    }

    /**
     * Gets calendar from date.
     *
     * @param t Date
     * @return Calendar
     */
    private static Calendar getCalendarFromDate(java.sql.Date t) {
        java.util.Calendar ret = java.util.Calendar.getInstance();
        ret.setTimeInMillis(t.getTime());
        return ret;
    }

    protected String m_entryDate;
    protected Map<String, PlainDate> m_eslStartDates;
    protected String m_exportType;
    protected String m_fieldCipCode;
    protected String m_fieldDateFirstEnrolledUsSchool;
    protected String m_fieldDateRecentEnrolledUsSchool;
    protected String m_fieldDiplomaTypeCode;
    protected String m_fieldDistrictCodeOfResidence;
    protected String m_fieldDistrictResidence;
    protected String m_fieldEllAccessTested;
    protected String m_fieldExpectedPostGradActivity;
    protected String m_fieldFundingDistrictCode;
    protected String m_fieldGiftedStatusCode;
    protected String m_fieldGuardianRelationship;
    protected String m_fieldHomemaker;
    protected String m_fieldHomeroom;
    protected String m_fieldLocationCodeOfResidance;
    protected String m_fieldPgmBidAun;
    protected String m_fieldPgmBidSchool;
    protected String m_fieldPgmPasaAun;
    protected String m_fieldReducedLunch;
    protected String m_fieldResidenceSchool;
    protected String m_fieldSchoolStateId;
    protected String m_fieldStdAsmPart;
    protected String m_fieldStdSklOverride2;
    protected String m_fieldStudentIsASingleParent;
    protected String m_fieldStudentStatus;
    protected String m_fieldTitle3Served;
    protected String m_fieldUDFStudentVisa;
    private Map<String, Map<String, PlainDate>> m_firstSchoolYearDate = new HashMap();
    protected Set<String> m_gradeLevelsHS;
    protected StudentHistoryHelper m_helper;
    protected Map<String, StudentProgramParticipation> m_iepProgram;
    protected boolean m_isSnapshot;
    protected Map<String, Collection<String>> m_programCodeMap;
    protected Map<String, List<StudentProgramParticipation>> m_programs;
    protected Map<String, Collection<Race>> m_races;
    protected PlainDate m_reportDate;
    protected Set<String> m_setSchoolOids;
    protected Map<String, Set<String>> m_stateCodes;

    /**
     * Initialize.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    public void initialize() {
        m_reportDate = (PlainDate) getParameter(PARAM_REPORT_DATE);

        initializeFields();

        /*
         * Build helper object
         */

        if (getSetupErrors().size() == 0) {
            loadProgramRefCodes();

            m_helper = new StudentHistoryHelper(this);

            m_exportType = (String) getParameter(PARAM_EXPORT_TYPE);
            String schoolOids = (String) getParameter(PARAM_SCHOOLS);
            m_setSchoolOids = new HashSet<String>();
            m_setSchoolOids.addAll(Arrays.asList(schoolOids.split(",")));

            m_isSnapshot = false;

            if (EXPORT_TYPE_STUDENT.equals(m_exportType)) {
                m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
                m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);
            } else if (EXPORT_TYPE_ELL_ACCESS.equals(m_exportType)) {
                m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
                m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);
                m_helper.getStudentCriteria().addEqualTo(m_fieldEllAccessTested, BooleanAsStringConverter.TRUE);
            } else if (EXPORT_TYPE_ELL_EOY.equals(m_exportType)) {
                m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
                m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);
                X2Criteria criteria = m_helper.getStudentCriteria();
                Set<String> refCodes = m_stateCodes.get(PGM_LEP);
                if (refCodes == null || refCodes.size() == 0) {
                    criteria.addEqualTo(X2BaseBean.COL_OID, "_*_dummyoid_*_");
                } else {
                    criteria.addIn(SisStudent.REL_PROGRAM_PARTICIPATION + ModelProperty.PATH_DELIMITER +
                            StudentProgramParticipation.COL_PROGRAM_CODE, refCodes);
                }
                addProgramDatesCriteria(criteria, SisStudent.REL_PROGRAM_PARTICIPATION + ModelProperty.PATH_DELIMITER);
            } else if (EXPORT_TYPE_OCT_1_SNAP.equals(m_exportType)) {
                m_isSnapshot = true;
                // Get october 1 start date
                Calendar cal = Calendar.getInstance();
                cal.set(getOrganization().getCurrentContext().getSchoolYear() - 1, 9, 1, 0, 0, 0);
                m_reportDate = new PlainDate(cal.getTime());

                m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_SNAPSHOT);
                m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);
            } else if (EXPORT_TYPE_CTE.equals(m_exportType)) {
                m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
                m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);

                X2Criteria stdWithPosCriteria = new X2Criteria();
                stdWithPosCriteria.addNotEmpty(GraduationStudentProgram.REL_PROGRAM_STUDIES +
                        ModelProperty.PATH_DELIMITER +
                        m_fieldCipCode, getBroker().getPersistenceKey());
                stdWithPosCriteria.addNotEqualTo(GraduationStudentProgram.REL_PROGRAM_STUDIES +
                        ModelProperty.PATH_DELIMITER +
                        m_fieldCipCode, GENERAL_FAMILY_CONSUMER_SCIENCE);
                m_helper.getStudentCriteria().addIn(X2BaseBean.COL_OID, new SubQuery(GraduationStudentProgram.class,
                        GraduationStudentProgram.COL_STUDENT_OID, stdWithPosCriteria));
            } else if (EXPORT_TYPE_SPED_DEC.equals(m_exportType)) {
                m_isSnapshot = true;
                Calendar cal = Calendar.getInstance();
                cal.set(getOrganization().getCurrentContext().getSchoolYear() - 1, 11, 1, 0, 0, 0);
                m_reportDate = new PlainDate(cal.getTime());

                m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_SNAPSHOT);
                m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);

                X2Criteria criteria = m_helper.getStudentCriteria();
                criteria.addNotEmpty(SisStudent.COL_SPED_INITIAL_ELIGIBILITY_DATE, getBroker().getPersistenceKey());
                criteria.addLessOrEqualThan(SisStudent.COL_SPED_INITIAL_ELIGIBILITY_DATE, m_reportDate);

                X2Criteria orCriteria = new X2Criteria();
                orCriteria.addEmpty(SisStudent.COL_SPED_EXIT_DATE, getBroker().getPersistenceKey());

                Criteria andDateCriteria = new Criteria();
                andDateCriteria.addGreaterOrEqualThan(SisStudent.COL_SPED_EXIT_DATE, m_reportDate);
                andDateCriteria.addOrCriteria(orCriteria);

                criteria.addAndCriteria(andDateCriteria);
            } else if (EXPORT_TYPE_SPED_EOY.equals(m_exportType)) {
                m_reportDate = getOrganization().getCurrentContext().getEndDate();

                m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
                m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);

                X2Criteria criteria = m_helper.getStudentCriteria();
                criteria.addNotEmpty(SisStudent.COL_SPED_INITIAL_ELIGIBILITY_DATE, getBroker().getPersistenceKey());
                criteria.addLessOrEqualThan(SisStudent.COL_SPED_INITIAL_ELIGIBILITY_DATE, m_reportDate);

                X2Criteria orCriteria = new X2Criteria();
                orCriteria.addEmpty(SisStudent.COL_SPED_EXIT_DATE, getBroker().getPersistenceKey());

                Criteria andDateCriteria = new Criteria();
                andDateCriteria.addGreaterOrEqualThan(SisStudent.COL_SPED_EXIT_DATE,
                        getOrganization().getCurrentContext().getStartDate());
                andDateCriteria.addOrCriteria(orCriteria);

                criteria.addAndCriteria(andDateCriteria);
            } else {
                // No matches
                m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
                m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);
                m_helper.getStudentCriteria().addEqualTo(X2BaseBean.COL_OID, "_*_dummyoid_*_");
            }

            // Add school selection
            m_helper.getStudentCriteria().addAndCriteria(getSchoolsStudentsCriteria());

            /*
             * Add homeroom constraint to schedules filters
             */
            m_helper.getStudentScheduleCriteria()
                    .addEqualTo(StudentSchedule.REL_SECTION + ModelProperty.PATH_DELIMITER +
                            MasterSchedule.REL_SCHOOL_COURSE +
                            ModelProperty.PATH_DELIMITER +
                            SchoolCourse.REL_COURSE + ModelProperty.PATH_DELIMITER +
                            m_fieldHomeroom, BooleanAsStringConverter.TRUE);
            m_helper.getStudentScheduleChangeCriteria().addEqualTo(StudentScheduleChange.REL_MASTER_SCHEDULE +
                    ModelProperty.PATH_DELIMITER +
                    MasterSchedule.REL_SCHOOL_COURSE +
                    ModelProperty.PATH_DELIMITER +
                    SchoolCourse.REL_COURSE +
                    ModelProperty.PATH_DELIMITER + m_fieldHomeroom,
                    BooleanAsStringConverter.TRUE);

            setQuery(m_helper.getStudentQuery(false));
            setEntityClass(PADistrictStudentEntity.class);

            loadRaces();
            loadPrograms();
            loadIepPrograms();
            loadHighSchoolGradeLevels();
            loadEslStartDates();

            // Build a map of calculations/retrievers. List is not full. Just to
            // check, if i'm doing right
            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put(CALC_ID_DISTENTRY, new RetrieveDistrictEntryDate());
            calcs.put(CALC_ID_DISTRESCODE, new RetrieveResCode());
            calcs.put(CALC_ID_FUNDING, new RetrieveFunding());
            calcs.put(CALC_ID_HOMEROOM, new RetrieveHomeroom());
            calcs.put(CALC_ID_LOCRESCODE, new RetrieveLocationResidenceCode());
            calcs.put(CALC_ID_PROGRAM, new RetrieveProgram());
            calcs.put(CALC_ID_RACE, new RetrieveRace());
            calcs.put(CALC_ID_REPORT_DATE, new RetrieveReportDate());
            calcs.put(CALC_ID_RESIDENCE_SCHOOL, new RetrieveResidenceSchool());
            calcs.put(CALC_ID_SNAPSHOT, new RetrieveSnapshotDate());
            calcs.put(CALC_ID_SKL, new RetrieveSchool());
            calcs.put(CALC_ID_SKLENTRY, new RetrieveSchoolEntryDate());
            calcs.put(CALC_ID_SKLYEAR_DATE, new RetrieveSchoolYearDate());
            calcs.put(CALC_ID_SPECED, new RetrieveSpecialEducation(getCurrentContext().getSchoolYear()));
            calcs.put(CALC_ID_SSN, new RetrieveSocialSecurityNo());
            super.addCalcs(calcs);

            // Build a map of validators. Just to check, if i'm doing right
            HashMap<String, FieldValidator> validators = new HashMap<String, FieldValidator>();
            validators.put(VAL_ID_POST_GRAD_ACTIVITY, new ValidatePostGraduateActivity());
            validators.put(VAL_ID_REQ, new ValidateRequired());
            validators.put(VAL_ID_REQ_IF_GRAD, new ValidateRequiredGraduate());
            validators.put(VAL_ID_REQ_IF_HS, new ValidateRequiredHighSchool());
            validators.put(VAL_ID_REQ_IF_LEP, new ValidateRequiredLimitedEnglishProficiency());
            validators.put(VAL_ID_EL_STATUS, new ValidateElStatus());
            validators.put(VAL_ID_REQ_IF_SE, new ValidateRequiredSpecialEducation());
            validators.put(VAL_ID_REQ_IF_SECT, new ValidateRequiredStudentStatus());
            validators.put(VAL_ID_ISEMPTY, new ValidateFieldValue());
            validators.put(VAL_ID_HOMEROOM, new ValidateHomeroom());
            validators.put(VAL_ID_Y_IF_CT, new ValidateSpecialEducationIfChallengeType());
            validators.put(ValidateLIEP.VAL_ID, new ValidateLIEP());
            super.addValidators(validators);
        }

    }

    /**
     * Gets the first school year date.
     *
     * @param school School
     * @param calendarId String
     * @return Plain date
     */
    protected PlainDate getFirstSchoolYearDate(School school, String calendarId) {
        Map<String, PlainDate> calendarMap = m_firstSchoolYearDate.get(school.getOid());
        if (calendarMap == null) {
            calendarMap = new HashMap();
            m_firstSchoolYearDate.put(school.getOid(), calendarMap);
        }
        PlainDate date = calendarMap.get(calendarId);
        if (date == null) {
            List<SchoolCalendar> schoolCalendars = (List<SchoolCalendar>) school.getSchoolCalendars();
            SchoolCalendar studentCalendar = null;

            for (SchoolCalendar schoolCalendar : schoolCalendars) {
                if (schoolCalendar.getCalendarId() != null && schoolCalendar.getCalendarId().equals(calendarId) &&
                        schoolCalendar.getDistrictContext() != null
                        && schoolCalendar.getDistrictContext().equals(getCurrentContext())) {
                    studentCalendar = schoolCalendar;
                    break;
                }
            }

            SchoolCalendarDate firstInSessionDate = null;
            if (studentCalendar != null) {
                firstInSessionDate = CalendarManager.getFirstInSessionDate(studentCalendar, getBroker());
            }

            date = firstInSessionDate != null ? firstInSessionDate.getDate() : getCurrentContext().getStartDate();
            calendarMap.put(calendarId, date);
        }
        return date;
    }

    /**
     * Add program date range criteria
     *
     * Based on discussion with client, it is determined that the program lookup
     * is always snapshot based on the report date.
     *
     * @param criteria X2Criteria
     * @param prefix String
     */
    private void addProgramDatesCriteria(X2Criteria criteria, String prefix) {
        criteria.addLessOrEqualThan(prefix + StudentProgramParticipation.COL_START_DATE, m_reportDate);

        X2Criteria criteria2 = new X2Criteria();
        X2Criteria criteria3 = new X2Criteria();
        criteria2.addEmpty(prefix + StudentProgramParticipation.COL_END_DATE, getBroker().getPersistenceKey());
        criteria3.addGreaterOrEqualThan(prefix + StudentProgramParticipation.COL_END_DATE, m_reportDate);
        criteria2.addOrCriteria(criteria3);
        criteria.addAndCriteria(criteria2);
    }

    /**
     * Get entry date for school.
     *
     * @return String schoolYear + "-07-01"
     */
    private String getEntryDate() {
        return Integer.toString(getOrganization().getCurrentContext().getSchoolYear()) + "-07-01";
    }

    /**
     * Gets criteria to add to student criteria for the schools
     * Modeled on StudentHistoryHelper and adapted for multiple schools.
     *
     * @return X2Criteria
     */
    private X2Criteria getSchoolsStudentsCriteria() {
        X2Criteria enrollmentCriteria = new X2Criteria();

        // Select enrollment dates based on date, depending on mode.
        if (m_isSnapshot) {
            enrollmentCriteria.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, m_reportDate);
        } else {
            enrollmentCriteria.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE,
                    getOrganization().getCurrentContext().getStartDate());
        }

        enrollmentCriteria.addIn(StudentEnrollment.COL_SCHOOL_OID, m_setSchoolOids);

        SubQuery enrollmentSubQuery =
                new SubQuery(StudentEnrollment.class, StudentEnrollment.COL_STUDENT_OID, enrollmentCriteria);
        X2Criteria enrCriteria = new X2Criteria();
        enrCriteria.addIn(X2BaseBean.COL_OID, enrollmentSubQuery);

        X2Criteria orCriteria = new X2Criteria();

        // Select students who are active.
        String activeStatus = PreferenceManager.getPreferenceValue(getOrganization(),
                SystemPreferenceDefinition.STUDENT_ACTIVE_CODE);

        X2Criteria activeCriteria = new X2Criteria();
        activeCriteria.addEqualTo(Student.COL_ENROLLMENT_STATUS, activeStatus);
        activeCriteria.addIn(Student.COL_SCHOOL_OID, m_setSchoolOids);

        // join the two criteria in an OR.
        orCriteria.addOrCriteria(enrCriteria);
        orCriteria.addOrCriteria(activeCriteria);
        return orCriteria;
    }

    /**
     * Lookup field aliases and paths.
     */
    private void initializeFields() {
        m_fieldCipCode = translateAliasToJavaName(ALIAS_CIP_NUMBER, true);
        m_fieldGuardianRelationship = translateAliasToJavaName(ALIAS_ADULTHOOD, true);
        m_fieldExpectedPostGradActivity = translateAliasToJavaName(ALIAS_POSTGRAD, true);
        m_fieldStudentStatus = translateAliasToJavaName(ALIAS_CURRENT, true);
        m_fieldReducedLunch = translateAliasToJavaName(ALIAS_LUNCH, true);
        m_fieldUDFStudentVisa = translateAliasToJavaName(ALIAS_FOREIGN, true);
        m_fieldDiplomaTypeCode = translateAliasToJavaName(ALIAS_DIPLOMA, true);
        m_fieldDateFirstEnrolledUsSchool = translateAliasToJavaName(ALIAS_US_ENROLL, true);
        m_fieldDateRecentEnrolledUsSchool = translateAliasToJavaName(ALIAS_US_RECENT_ENROLL, true);
        m_fieldDistrictCodeOfResidence = translateAliasToJavaName(ALIAS_RESIDENCE, true);
        m_fieldResidenceSchool = translateAliasToJavaName(ALIAS_RESIDENCE_SCHOOL, true);
        m_fieldStudentIsASingleParent = translateAliasToJavaName(ALIAS_PARENT, true);
        m_fieldLocationCodeOfResidance = translateAliasToJavaName(ALIAS_LOCATION, true);
        m_fieldHomemaker = translateAliasToJavaName(ALIAS_HOMEMAKER, true);
        m_fieldHomeroom = translateAliasToJavaName(ALIAS_HOMEROOM, true);
        m_fieldFundingDistrictCode = translateAliasToJavaName(ALIAS_FUNDING, true);
        m_fieldDistrictResidence = translateAliasToJavaName(ALIAS_DISTRES, true);
        m_fieldEllAccessTested = translateAliasToJavaName(ALIAS_ELL_ACCESS_TESTED, true);
        m_fieldGiftedStatusCode = translateAliasToJavaName(ALIAS_GIFTED, true);
        m_fieldTitle3Served = translateAliasToJavaName(ALIAS_TITLE_III_SERVED, true);
        m_fieldSchoolStateId = translateAliasToJavaName(ALIAS_SCHOOL_STATE_ID, true);
        m_fieldStdSklOverride2 = translateAliasToJavaName(ALIAS_STD_SKL_OVERRIDE_2, true);
        m_fieldStdAsmPart = translateAliasToJavaName(ALIAS_STD_ASM_PART, true);
        m_fieldPgmBidAun = translateAliasToJavaName(ALIAS_PGM_BID_AUN, true);
        m_fieldPgmBidSchool = translateAliasToJavaName(ALIAS_PGM_BID_SKL_NUMBER, true);
        m_fieldPgmPasaAun = translateAliasToJavaName(ALIAS_PGM_PASA_AUN, true);
        m_entryDate = getEntryDate();
    }

    /**
     * Create and populates HashMap<String, PlainDate> m_eslStartDates.
     */
    private void loadEslStartDates() {
        X2Criteria criteria = new X2Criteria();
        criteria.addIn(StudentProgramParticipation.COL_STUDENT_OID,
                new SubQuery(SisStudent.class, X2BaseBean.COL_OID, m_helper.getStudentCriteria()));
        criteria.addEqualTo(StudentProgramParticipation.COL_PROGRAM_CODE, PGM_LEP);

        String[] attributes = new String[] {StudentProgramParticipation.COL_STUDENT_OID,
                StudentProgramParticipation.COL_START_DATE};

        ReportQueryByCriteria query =
                new ReportQueryByCriteria(StudentProgramParticipation.class, attributes, criteria);
        query.addOrderByDescending(StudentProgramParticipation.COL_STUDENT_OID);
        query.addOrderByDescending(StudentProgramParticipation.COL_START_DATE);

        m_eslStartDates = new HashMap<String, PlainDate>();

        ReportQueryIterator results = getBroker().getReportQueryIteratorByQuery(query);
        try {
            while (results.hasNext()) {
                Object[] row = (Object[]) results.next();

                String studentOid = (String) row[0];
                PlainDate startDate = new PlainDate((Date) row[1]);
                m_eslStartDates.put(studentOid, startDate);
            }
        } finally {
            results.close();
        }
    }

    /**
     * Create and populates HashSet<String> m_gradeLevelsHS.
     */
    private void loadHighSchoolGradeLevels() {
        m_gradeLevelsHS = new HashSet<String>();
        DataDictionaryField fld = getDataDictionaryField(SisStudent.class, SisStudent.COL_GRADE_LEVEL);

        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, fld.getReferenceTableOid());
        criteria.addIn(ReferenceCode.COL_STATE_CODE, Arrays.asList(GRADES_HS));

        String[] columns = new String[] {ReferenceCode.COL_CODE};
        ReportQueryByCriteria query = new ReportQueryByCriteria(ReferenceCode.class, columns, criteria);

        ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                Object[] record = (Object[]) iterator.next();
                String code = (String) record[0];
                m_gradeLevelsHS.add(code);
            }
        } finally {
            iterator.close();
        }
    }

    /**
     * Create and populates HashMap m_iepProgram.
     */
    private void loadIepPrograms() {
        Set<String> refCodes = m_stateCodes.get(PGM_IEP);

        // Load student subquery to identify students reporting.
        SubQuery studentSubquery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, m_helper.getStudentCriteria());

        // Load program records for reporting students and programs into a map.
        if (refCodes != null && refCodes.size() > 0) {
            X2Criteria criteria = new X2Criteria();
            criteria.addIn(StudentProgramParticipation.COL_STUDENT_OID, studentSubquery);
            criteria.addIn(StudentProgramParticipation.COL_PROGRAM_CODE, refCodes);

            QueryByCriteria query = new QueryByCriteria(StudentProgramParticipation.class, criteria);
            query.addOrderBy(StudentProgramParticipation.COL_STUDENT_OID, true);

            m_iepProgram = new HashMap();
            QueryIterator iterator = getBroker().getIteratorByQuery(query);
            try {
                while (iterator.hasNext()) {
                    StudentProgramParticipation program = (StudentProgramParticipation) iterator.next();
                    if (m_iepProgram.containsKey(program.getStudentOid())) {
                        StudentProgramParticipation previous = m_iepProgram.get(program.getStudentOid());
                        if (program.getEndDate() == null || (previous.getEndDate() != null
                                && previous.getEndDate().before(program.getEndDate()))) {
                            m_iepProgram.put(program.getStudentOid(), program);
                        }
                    } else {
                        m_iepProgram.put(program.getStudentOid(), program);
                    }
                }
            } finally {
                if (iterator != null) {
                    iterator.close();
                }
            }
        } else {
            addSetupError("No reportable program codes", "Student Programs, reference table, local code: HML, SPED");
        }
    }

    /**
     * Load the reference codes corresponding to the required state codes.
     */
    private void loadProgramRefCodes() {
        // Building stateCode: code - 1:N Map
        m_stateCodes = new HashMap<String, Set<String>>();
        DataDictionaryField field = getDataDictionaryField(StudentProgramParticipation.class,
                StudentProgramParticipation.COL_PROGRAM_CODE);
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, field.getReferenceTableOid());
        criteria.addIn(ReferenceCode.COL_STATE_CODE,
                Arrays.asList(PGM_LUNCH, PGM_IEP, PGM_BID, PGM_LEP, PGM_GIFTED, PGM_TITLE_I, PGM_HOMELESS_ACC,
                        PGM_HOMELESS_UNACC));
        QueryByCriteria refQuery = new QueryByCriteria(ReferenceCode.class, criteria);
        Map<String, Collection<ReferenceCode>> programStateRefCodeMap =
                getBroker().getGroupedCollectionByQuery(refQuery,
                        ReferenceCode.COL_STATE_CODE,
                        10);
        for (String stateCode : programStateRefCodeMap.keySet()) {
            Set<String> codes = new HashSet<String>();
            Collection<ReferenceCode> rcdBeans = programStateRefCodeMap.get(stateCode);
            for (ReferenceCode rcdBean : rcdBeans) {
                codes.add(rcdBean.getCode());
            }
            m_stateCodes.put(stateCode, codes);
        }
    }

    /**
     * Load a map by studentOid of student program participation records that contain a specific set
     * of codes.
     * Programs must be one of the known codes for ELL, FARMS, FOSTER, HOMELESS, 504, SPED,
     * IMMIGRANT
     */
    private void loadPrograms() {
        Set<String> refCodes = new HashSet<String>();
        for (Set<String> codes : m_stateCodes.values()) {
            refCodes.addAll(codes);
        }

        // Load student subquery to identify students reporting.
        SubQuery studentSubquery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, m_helper.getStudentCriteria());

        // Load program records for reporting students and programs into a map.
        if (refCodes.size() > 0) {
            X2Criteria criteria = new X2Criteria();
            criteria.addIn(StudentProgramParticipation.COL_STUDENT_OID, studentSubquery);
            criteria.addIn(StudentProgramParticipation.COL_PROGRAM_CODE, refCodes);
            addProgramDatesCriteria(criteria, "");
            QueryByCriteria query = new QueryByCriteria(StudentProgramParticipation.class, criteria);
            query.addOrderBy(StudentProgramParticipation.COL_STUDENT_OID, true);
            query.addOrderBy(StudentProgramParticipation.COL_START_DATE, false);
            m_programs =
                    getBroker().getGroupedCollectionByQuery(query, StudentProgramParticipation.COL_STUDENT_OID, 1024);
        } else {
            addSetupError("No reportable program codes", "Student Programs, reference table, local code: HML, SPED");
        }
    }

    /**
     * Load races.
     */
    private void loadRaces() {
        SubQuery studentSubquery =
                new SubQuery(SisStudent.class, SisStudent.COL_PERSON_OID, m_helper.getStudentCriteria());
        X2Criteria criteria = new X2Criteria();
        criteria.addIn(Race.COL_PERSON_OID, studentSubquery);
        BeanQuery query = new BeanQuery(Race.class, criteria);
        query.addOrderBy(Race.COL_PERSON_OID, true);
        m_races = getBroker().getGroupedCollectionByQuery(query, Race.COL_PERSON_OID, 100000);
    }
}
