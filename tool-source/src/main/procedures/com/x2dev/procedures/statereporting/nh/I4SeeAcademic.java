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
package com.x2dev.procedures.statereporting.nh;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.RecordSet;
import com.follett.fsc.core.k12.beans.RecordSetKey;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.follett.fsc.core.k12.web.WebUtils;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.beans.Transcript;
import com.x2dev.sis.model.business.EnrollmentManager;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.types.PlainDate;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class I4SeeAcademic.
 */
public class I4SeeAcademic extends StateReportData {

    /**
     * Implementation of StateReportEntity to be used by the I4SeeAcademic export.
     * This must be a public static inner class with a public no argument
     * constructor so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class I4SeeEntity extends StateReportEntity {
        /**
         * Placeholders for calculated unmapped fields. These can be written back to the database
         * in postProcess if update flag is set. Also, holds some calculated values that have
         * been overridden with default or related values.
         *
         * Map key should be field alias constant.
         */
        private Map<String, Object> m_updateValues = null;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public I4SeeEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Intitialize.
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
        }

        /**
         * Check enrollment membership count and membership days parameter to determine if the
         * student should be reported.
         *
         * @return StateReportValidationError
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#filterEntity()
         */
        @Override
        public StateReportValidationError filterEntity() {
            StateReportValidationError error = null;

            FieldDefinition field = getData().getFieldDefinition(I4SEE_300_DAYS_IN_ATTENDANCE);

            boolean requireMemberDay = ((Boolean) getData().getParameter(REQUIRE_MEMBER_DAY_PARAM)).booleanValue();

            /*
             * Get membership days parameter
             */
            double membershipCountAsDouble = 0;

            /*
             * Get membership count
             */
            I4SeeAcademic i4seeData = (I4SeeAcademic) getData();
            String membershipCount = i4seeData.getMembershipDays(this);

            if (membershipCount != null) {
                try {
                    membershipCountAsDouble = Double.parseDouble(membershipCount);
                } catch (NumberFormatException nfe) {
                    // invalid format, will be reported elsewhere.
                }
            }

            // check enrollment count and membership days parameter.
            if ((requireMemberDay && membershipCountAsDouble > 0) || !requireMemberDay) {
                // No filtering.
            } else {
                // Student filtered.
                error = new StateReportValidationError(this, field, "0 member days - excluded from export", "");
            }

            return error;
        }

        /**
         * Returns a field value saved before mapping.
         *
         * Certain calculated data fields (Sped valued) can be stored and retrieved before
         * reference code mapping by DOE field constant.
         *
         * @param doeId String
         * @return Object
         */
        public Object getUpdateValue(String doeId) {
            Object value = null;
            if (m_updateValues != null) {
                value = m_updateValues.get(doeId);
            }
            return value;
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
                    (getData().isSchoolContext() ? ", SCHOOL: " + student.getSchool().getName() : "") +
                    "]";

            return name;
        }

        /**
         * Sets a field value before mapping.
         *
         * Certain calculated data fields (Sped valued) can be stored
         * and retrieved before reference code mapping by DOE field constant.
         *
         * @param doeId String
         * @param value Object
         */
        public void setUpdateValue(String doeId, Object value) {
            if (m_updateValues == null) {
                m_updateValues = new HashMap<String, Object>();
            }
            m_updateValues.put(doeId, value);
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
    // ---------------------------------------------------------------------------------------------
    // --------------------------------------------------------------------- User-referencable code
    // ---------------------------------------------------------------------------------------------

    /*
     * Field aliases for "adjusted" fields (these fields may have manually entered values that
     * override/adjust calculated values).
     */
    private static final String ADJUSTED_SCHOOL_NUMBER_FIELD = "i4see Adj School Number";

    /*
     * Field alias for the adjusted district code on the SCHOOL table. This alias is optional.
     */
    private static final String ADJUSTED_DISTRICT_CODE_FIELD = "i4see ADJUSTED DISTRICT";
    /*
     * Field alias constants. These field aliases are all for the STUDENT table.
     */
    private static final String STUDENT_NAME = "name view";
    private static final String I4SEE_010_SASID = "i4see 010";
    private static final String I4SEE_100_DOB = "i4see 100";
    private static final String I4SEE_030_SAU_NUMBER = "i4see 030";
    private static final String I4SEE_040_DISTRICT_NUMBER = "i4see 040";
    private static final String I4SEE_050_SCHOOL_NUMBER = "i4see 050";
    private static final String I4SEE_300_DAYS_IN_ATTENDANCE = "i4see 300";
    private static final String I4SEE_490_TITLE1_MATH = "i4see 490";
    private static final String I4SEE_491_TITLE1_RLA = "i4see 491";
    private static final String I4SEE_492_TITLE1_SCIENCE = "i4see 492";
    private static final String I4SEE_493_TITLE1_SOC_STY = "i4see 493";
    private static final String I4SEE_494_TITLE1_VCE = "i4see 494";
    private static final String I4SEE_495_TITLE1_OI = "i4see 495";
    private static final String I4SEE_496_TITLE1_HDE = "i4see 496";
    private static final String I4SEE_497_TITLE1_SGA = "i4see 497";
    private static final String I4SEE_498_TITLE1_OSSP = "i4see 498";
    private static final String I4SEE_640_READING_RECOVERY = "i4see 640";

    /*
     * Field alias/field value for querying options on the export
     */
    private static final String I4SEE_STATUS_FIELD = "i4see Status";
    private static final String I4SEE_STATUS_FIELD_REPORT_CODE = "Report";

    /*
     * Field aliases used in retrieval of i4see 700-737
     */
    private static final String COURSE_I4SEE_AP_ID_FIELD = "i4see AP ID";
    private static final String TRANSCRIPT_AP_EXAM_TAKEN_FIELD = "AP Exam Taken";

    /*
     * AP Exam results
     */
    private static final String I4SEE_NO_AP_COURSE = "";
    // @SuppressWarnings("unused")
    // private static final String I4SEE_AP_COURSE_NO_EXAM = "1";
    // @SuppressWarnings("unused")
    // private static final String I4SEE_AP_COURSE_WITH_EXAM = "2";

    /*
     * Header names for the i4see AP fields
     */
    private String I4SEE_700_APAH_NAME = "i4see 700 APAH";
    private String I4SEE_701_APBIO_NAME = "i4see 701 APBIO";
    private String I4SEE_702_APCALCAB_NAME = "i4see 702 APCALCAB";
    private String I4SEE_703_APCALCBC_NAME = "i4see 703 APCALCBC";
    private String I4SEE_704_APCHEM_NAME = "i4see 704 APCHEM";
    private String I4SEE_705_APCS_NAME = "i4see 705 APCS";
    private String I4SEE_706_APCSAB_NAME = "i4see 706 APCSAB";
    private String I4SEE_707_APMACRO_NAME = "i4see 707 APMACRO";
    private String I4SEE_708_APMICRO_NAME = "i4see 708 APMICRO";
    private String I4SEE_709_APAPELC_NAME = "i4see 709 APELC";

    private String I4SEE_710_APELCOMP_NAME = "i4see 710 APELCOMP";
    private String I4SEE_711_APES_NAME = "i4see 711 APES";
    private String I4SEE_712_APEH_NAME = "i4see 712 APEH";
    private String I4SEE_713_APFL_NAME = "i4see 713 APFL";
    private String I4SEE_714_APFLIT_NAME = "i4see 714 APFLIT";
    private String I4SEE_715_APGL_NAME = "i4see 715 APGL";
    private String I4SEE_716_APGPC_NAME = "i4see 716 APGPC";
    private String I4SEE_717_APGPUS_NAME = "i4see 717 APGPUS";
    private String I4SEE_718_APHG_NAME = "i4see 718 APHG";
    private String I4SEE_719_APIEL_NAME = "i4see 719 APIEL";

    private String I4SEE_720_APLL_NAME = "i4see 720 APLL";
    private String I4SEE_721_APLV_NAME = "i4see 721 APLV";
    private String I4SEE_722_APMT_NAME = "i4see 722 APMT";
    private String I4SEE_723_APPB_NAME = "i4see 723 APPB";
    private String I4SEE_724_APPCEM_NAME = "i4see 724 APPCEM";
    private String I4SEE_725_APPC_NAME = "i4see 725 APPC";
    private String I4SEE_726_APPSY_NAME = "i4see 726 APPSY";
    private String I4SEE_727_APPS_NAME = "i4see 727 APPS";
    private String I4SEE_728_APSQ_NAME = "i4see 728 APSQ";
    private String I4SEE_729_APSTAT_NAME = "i4see 729 APSTAT";

    private String I4SEE_730_APMECH_NAME = "i4see 730 APMECH";
    private String I4SEE_731_APLANG_NAME = "i4see 731 APLANG";
    private String I4SEE_732_APLIT_NAME = "i4see 732 APLIT";
    private String I4SEE_733_APSA2D_NAME = "i4see 733 APSA2D";
    private String I4SEE_734_APSA3D_NAME = "i4see 734 APSA3D";
    private String I4SEE_735_APSAD_NAME = "i4see 735 APSAD";
    private String I4SEE_736_APPUSH_NAME = "i4see 736 APPUSH";
    private String I4SEE_737_APWH_NAME = "i4see 737 APWH";

    /*
     * AP Codes for the i4see AP fields
     */
    private String I4SEE_700_APAH_CODE = "700";
    private String I4SEE_701_APBIO_CODE = "701";
    private String I4SEE_702_APCALCAB_CODE = "702";
    private String I4SEE_703_APCALCBC_CODE = "703";
    private String I4SEE_704_APCHEM_CODE = "704";
    private String I4SEE_705_APCS_CODE = "705";
    private String I4SEE_706_APCSAB_CODE = "706";
    private String I4SEE_707_APMACRO_CODE = "707";
    private String I4SEE_708_APMICRO_CODE = "708";
    private String I4SEE_709_APAPELC_CODE = "709";

    private String I4SEE_710_APELCOMP_CODE = "710";
    private String I4SEE_711_APES_CODE = "711";
    private String I4SEE_712_APEH_CODE = "712";
    private String I4SEE_713_APFL_CODE = "713";
    private String I4SEE_714_APFLIT_CODE = "714";
    private String I4SEE_715_APGL_CODE = "715";
    private String I4SEE_716_APGPC_CODE = "716";
    private String I4SEE_717_APGPUS_CODE = "717";
    private String I4SEE_718_APHG_CODE = "718";
    private String I4SEE_719_APIEL_CODE = "719";

    private String I4SEE_720_APLL_CODE = "720";
    private String I4SEE_721_APLV_CODE = "721";
    private String I4SEE_722_APMT_CODE = "722";
    private String I4SEE_723_APPB_CODE = "723";
    private String I4SEE_724_APPCEM_CODE = "724";
    private String I4SEE_725_APPC_CODE = "725";
    private String I4SEE_726_APPSY_CODE = "726";
    private String I4SEE_727_APPS_CODE = "727";
    private String I4SEE_728_APSQ_CODE = "728";
    private String I4SEE_729_APSTAT_CODE = "729";

    private String I4SEE_730_APMECH_CODE = "730";
    private String I4SEE_731_APLANG_CODE = "731";
    private String I4SEE_732_APLIT_CODE = "732";
    private String I4SEE_733_APSA2D_CODE = "733";
    private String I4SEE_734_APSA3D_CODE = "734";
    private String I4SEE_735_APSAD_CODE = "735";
    private String I4SEE_736_APPUSH_CODE = "736";
    private String I4SEE_737_APWH_CODE = "737";


    /**
     * Name for the enumerated "selection" parameter. The value is an Integer.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the "selection value" parameter. The value is an String.
     */
    public static final String QUERY_STRING_PARAM = "queryString";

    /**
     * Name for the enumerated "sort" parameter. The value is an Integer.
     */
    public static final String SORT_PARAM = "sort";

    /**
     * Name for the "include student names" parameter. The value is a Boolean.
     */
    public static final String INCLUDE_STUDENT_NAMES_PARAM = "includeStudentName";

    /**
     * Name for the report date parameter. The corresponding values is a PlainDate object.
     */
    public static final String REPORT_DATE_PARAM = "reportDate";

    /**
     * Name for the "require report status" parameter. The value is a Boolean.
     */
    public static final String REQUIRE_REPORT_STATUS_PARAM = "requireReportStatus";

    /**
     * Name for the "require at lease one member day" parameter. The value is a Boolean.
     */
    public static final String REQUIRE_MEMBER_DAY_PARAM = "requireMemberDay";



    protected Map<String, Map<String, Transcript>> m_studentTranscriptMap;
    /*
     * Other internal constants
     */
    private static final String DATE_FORMAT = "MM/dd/yyyy";
    private static final String REGEX_NUMERIC = "[0123456789]*";

    /**
     * Supporting instance variables.
     * These are protected rather than private so they can be accessed by the inner classes.
     */
    protected String m_adjustedSchoolCode;
    protected boolean m_includeStudentNames;
    protected Converter m_integerConverter;
    protected String m_reportStatusField;
    protected Map<String, Schedule> m_scheduleMap;
    protected Map<String, SisSchool> m_schoolMap;
    protected DateFormat m_dateFormat;
    protected String m_i4seeCourseApId;
    protected String m_i4seeApExamTaken;
    protected EnrollmentManager m_enrollmentManager;
    protected PlainDate m_firstDayDate;
    protected Set m_firstDayMembers;
    protected PlainDate m_reportDate;
    protected HashMap m_schoolsToCalendars;


    /**
     * Returns the district number for the given student.
     */
    protected class RetrieveAPInfo implements FieldRetriever {
        private String m_code = null;

        /**
         * Constructor.
         *
         * @param code String
         */
        public RetrieveAPInfo(String code) {
            m_code = code;
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.
         *      core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String apValue = I4SEE_NO_AP_COURSE;

            SisStudent student = (SisStudent) entity.getBean();

            Map<String, Transcript> transcriptMap = m_studentTranscriptMap.get(student.getOid());

            if (transcriptMap != null) {
                Transcript transcript = transcriptMap.get(m_code);

                if (transcript != null) {
                    String apExamStatus = (String) WebUtils.getProperty(transcript, m_i4seeApExamTaken);

                    // if ("1".equals(apExamStatus))
                    // {
                    // apValue = I4SEE_AP_COURSE_WITH_EXAM;
                    // }
                    // else
                    // {
                    // apValue = I4SEE_AP_COURSE_NO_EXAM;
                    // }

                    // return directly what is in the field with no interpretation
                    if (!StringUtils.isEmpty(apExamStatus)) {
                        apValue = apExamStatus;
                    }
                }
            }

            return apValue;
        }
    }

    /**
     * Returns the district number for the given student.
     */
    protected class RetrieveDistrictNumber implements FieldRetriever {
        private String m_javaName = null;

        /**
         * Constructor.
         *
         * @param javaName String
         */
        public RetrieveDistrictNumber(String javaName) {
            m_javaName = javaName;
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.
         *      core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String districtNumber = null;

            districtNumber = (String) WebUtils.getProperty(getOrganization(), m_javaName);

            return districtNumber;
        }
    }

    /**
     * Returns the sau number for the given student.
     */
    protected class RetrieveSauNumber implements FieldRetriever {
        private String m_javaName = null;

        /**
         * Constructor.
         *
         * @param javaName String
         */
        public RetrieveSauNumber(String javaName) {
            m_javaName = javaName;
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.
         *      core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String sauNumber = null;

            sauNumber = (String) WebUtils.getProperty(getOrganization(), m_javaName);

            return sauNumber;
        }
    }

    /**
     * Returns the school number for the given student.
     * <p>
     * For non-archived students we use the snapshot. For archived students (i.e., summer
     * withdrawals) we use the school from the most recent enrollment record during summer vacation
     * (if we didn't do this then the export would show the history school's school number).
     */
    protected class RetrieveSchoolNumber implements FieldRetriever {
        private String m_javaName = null;

        /**
         * Constructor.
         *
         * @param javaName String
         */
        public RetrieveSchoolNumber(String javaName) {
            m_javaName = javaName;
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.
         *      core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();
            String schoolNumber = null;

            String adjustedSchoolNumber = (String) WebUtils.getProperty(student, m_adjustedSchoolCode);
            if (StringUtils.isEmpty(adjustedSchoolNumber)) {
                SisSchool school = m_schoolMap.get(student.getSchoolOid());
                schoolNumber = (String) WebUtils.getProperty(school, m_javaName);
            } else {
                schoolNumber = adjustedSchoolNumber;
            }

            return schoolNumber;
        }
    }

    /**
     * Initialize the data module.
     * Initialize necessary working resources.
     * Define query for students to load.
     * Define list of field definitions for the export.
     * 
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(java.util.Map,
     *      com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    public void initialize() {
        /*
         * Load initialization data
         */
        initializeFields();

        m_includeStudentNames = ((Boolean) getParameter(INCLUDE_STUDENT_NAMES_PARAM)).booleanValue();
        m_reportDate = (PlainDate) getParameter(REPORT_DATE_PARAM);

        m_dateFormat = new SimpleDateFormat(DATE_FORMAT);

        /*
         * Set the field definition array
         */
        ArrayList<FieldDefinition> fieldDefinitions = new ArrayList<FieldDefinition>(m_includeStudentNames ? 54 : 53);

        if (m_includeStudentNames) {
            fieldDefinitions.add(getName());
        }

        fieldDefinitions.add(getSasid());
        fieldDefinitions.add(getDob());
        fieldDefinitions.add(getI4see030_SauNumber());
        fieldDefinitions.add(getI4see040_DistrictNumber());
        fieldDefinitions.add(getI4see050_SchoolNumber());

        fieldDefinitions.add(getI4see490_Title1Math());
        fieldDefinitions.add(getI4see491_Title1RLA());
        fieldDefinitions.add(getI4see492_Title1Science());
        fieldDefinitions.add(getI4see493_Title1SocSty());
        fieldDefinitions.add(getI4see494_Title1Vce());

        fieldDefinitions.add(getI4see495_Title1OI());
        fieldDefinitions.add(getI4see496_Title1Hde());
        fieldDefinitions.add(getI4see497_Title1Sga());
        fieldDefinitions.add(getI4see498_Title1Ossp());
        fieldDefinitions.add(getI4see640_ReadingRecovery());

        fieldDefinitions.add(getI4see700_APAH());
        fieldDefinitions.add(getI4see701_APBIO());
        fieldDefinitions.add(getI4see702_APCALCAB());
        fieldDefinitions.add(getI4see703_APCALCBC());
        fieldDefinitions.add(getI4see704_APCHEM());

        fieldDefinitions.add(getI4see705_APCS());
        fieldDefinitions.add(getI4see706_APCSAB());
        fieldDefinitions.add(getI4see707_APMACRO());
        fieldDefinitions.add(getI4see708_APMICRO());
        fieldDefinitions.add(getI4see709_APAPELC());

        fieldDefinitions.add(getI4see710_APELCOMP());
        fieldDefinitions.add(getI4see711_APES());
        fieldDefinitions.add(getI4see712_APEH());
        fieldDefinitions.add(getI4see713_APFL());
        fieldDefinitions.add(getI4see714_APFLIT());

        fieldDefinitions.add(getI4see715_APGL());
        fieldDefinitions.add(getI4see716_APGPC());
        fieldDefinitions.add(getI4see717_APGPUS());
        fieldDefinitions.add(getI4see718_APHG());
        fieldDefinitions.add(getI4see719_APIEL());

        fieldDefinitions.add(getI4see720_APIEL());
        fieldDefinitions.add(getI4see721_APLV());
        fieldDefinitions.add(getI4see722_APMT());
        fieldDefinitions.add(getI4see723_APPB());
        fieldDefinitions.add(getI4see724_APPCEM());

        fieldDefinitions.add(getI4see725_APPC());
        fieldDefinitions.add(getI4see726_APPSY());
        fieldDefinitions.add(getI4see727_APPS());
        fieldDefinitions.add(getI4see728_APSQ());
        fieldDefinitions.add(getI4see729_APSTAT());

        fieldDefinitions.add(getI4see730_APMECH());
        fieldDefinitions.add(getI4see731_APLANG());
        fieldDefinitions.add(getI4see732_APLIT());
        fieldDefinitions.add(getI4see733_APSA2D());
        fieldDefinitions.add(getI4see734_APSA3D());

        fieldDefinitions.add(getI4see735_APLANG());
        fieldDefinitions.add(getI4see736_APPUSH());
        fieldDefinitions.add(getI4see737_APWH());

        setFieldDefinitions(fieldDefinitions);

        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {
            /*
             * Build query object that will be used to retrieve export students.
             */
            Criteria studentCriteria = getStudentCriteria();
            QueryByCriteria studentQuery = new QueryByCriteria(SisStudent.class, studentCriteria);

            int sort = ((Integer) getParameter(SORT_PARAM)).intValue();
            switch (sort) {
                case 0: // Name
                    studentQuery.addOrderByAscending(SisStudent.COL_NAME_VIEW);
                    break;

                case 1: // YOG
                    studentQuery.addOrderByAscending(SisStudent.COL_YOG);
                    studentQuery.addOrderByAscending(SisStudent.COL_NAME_VIEW);
                    break;

                case 2: // School
                    studentQuery.addOrderByAscending(SisStudent.REL_SCHOOL + PATH_DELIMITER +
                            translateAliasToJavaName(I4SEE_050_SCHOOL_NUMBER, true));
                    studentQuery.addOrderByAscending(SisStudent.COL_NAME_VIEW);
                    break;

                case 3: // LASID
                    studentQuery.addOrderByAscending(SisStudent.COL_LOCAL_ID);
                    break;

                case 4: // SASID
                    studentQuery.addOrderByAscending(SisStudent.COL_STATE_ID);
                    break;

                default:
                    studentQuery.addOrderByAscending(SisStudent.COL_NAME_VIEW);
                    break;
            }

            // Set the query to be used for student selection.
            setQuery(studentQuery);
            setEntityClass(I4SeeEntity.class);

            /*
             * Load Schools
             */
            loadSchools();

            /*
             * Load active schedules
             */
            loadActiveSchedules();

            X2Criteria criteria = new X2Criteria();

            criteria.addEqualTo(Transcript.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());
            criteria.addEqualTo(Transcript.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_ORGANIZATION1_OID,
                    getOrganization().getOid());
            criteria.addNotEmpty(Transcript.REL_SCHOOL_COURSE + PATH_DELIMITER +
                    SchoolCourse.REL_COURSE + PATH_DELIMITER +
                    m_i4seeCourseApId, getBroker().getPersistenceKey());

            SubQuery studentSub = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria);
            criteria.addIn(Transcript.COL_STUDENT_OID, studentSub);

            QueryByCriteria query = new QueryByCriteria(Transcript.class, criteria);

            m_studentTranscriptMap = getBroker().getNestedMapByQuery(query,
                    Transcript.COL_STUDENT_OID,
                    Transcript.REL_SCHOOL_COURSE + PATH_DELIMITER +
                            SchoolCourse.REL_COURSE + PATH_DELIMITER +
                            m_i4seeCourseApId,
                    (int) (getBroker().getCount(studentQuery) * 1.5),
                    64);
        }
    }

    /**
     * Returns the class of the base bean used for this export.
     *
     * @return Class
     */
    @Override
    public Class getBeanClass() {
        return SisStudent.class;
    }

    /**
     * Returns the title of the report for report headings.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getExportTitle()
     */
    @Override
    public String getExportTitle() {
        return "I4SEE ACADEMIC";
    }

    /**
     * Returns the number of days the student has been a member from the start of school to the
     * report date.
     *
     * @param i4see I4SeeEntity
     * @return String
     */
    public String getMembershipDays(I4SeeEntity i4see) {
        String count = null;
        SisStudent student = (SisStudent) i4see.getBean();

        String adjustedCount = "";
        if (!StringUtils.isEmpty(adjustedCount)) {
            count = adjustedCount;
        } else {
            // Check the active schedule for the school.
            SisSchool school = m_schoolMap.get(student.getSchoolOid());
            Schedule schedule = null;

            if (school != null) {
                schedule = m_scheduleMap.get(school.getOid());
                if (schedule != null) {
                    try {

                        count = String.valueOf(
                                m_enrollmentManager.getMembershipTotal(
                                        student,
                                        getCalendarDays(m_schoolMap.get(student.getSchoolOid()),
                                                student.getCalendarCode()),
                                        m_firstDayMembers.contains(student.getOid()),
                                        m_firstDayDate,
                                        m_reportDate,
                                        null));
                    } catch (Exception e) {
                        addSetupError("Membership days",
                                "Could not calculate membership: exception\n\t" + e.getMessage());
                    }
                }
            }
        }

        return count;
    }

    /**
     * Sets the header row display mode in the export.
     *
     * @return boolean
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getIncludeHeaderRow()
     */
    @Override
    public boolean getIncludeHeaderRow() {
        return false;
    }

    /**
     * Build Field definition for the student DOB (i4see 100).
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDob() {
        FieldDefinition field = new FieldDefinition(I4SEE_100_DOB,
                SisStudent.REL_PERSON + PATH_DELIMITER + SisPerson.COL_DOB,
                null,
                false,
                8,
                10,
                null,
                m_dateFormat,
                null,
                null,
                null);
        return field;
    }

    /**
     * Build a Field Definition for i4see 030 (sau number).
     *
     * @return FieldDefinition
     */
    protected FieldDefinition getI4see030_SauNumber() {
        String javaName = translateAliasToJavaName(I4SEE_030_SAU_NUMBER, true);
        FieldDefinition field = new FieldDefinition(I4SEE_030_SAU_NUMBER,
                SisStudent.REL_SCHOOL + PATH_DELIMITER + SisSchool.REL_ORGANIZATION1 + PATH_DELIMITER + javaName,
                null,
                false,
                1,
                5,
                REGEX_NUMERIC,
                null,
                new RetrieveSauNumber(javaName),
                null,
                null);

        return field;
    }

    /**
     * Build a Field Definition for i4see 040 (district number).
     *
     * @return FieldDefinition
     */
    protected FieldDefinition getI4see040_DistrictNumber() {
        String javaName = translateAliasToJavaName(I4SEE_040_DISTRICT_NUMBER, true);
        FieldDefinition field = new FieldDefinition(I4SEE_040_DISTRICT_NUMBER,
                SisStudent.REL_SCHOOL + PATH_DELIMITER + SisSchool.REL_ORGANIZATION1 + PATH_DELIMITER + javaName,
                null,
                false,
                1,
                5,
                REGEX_NUMERIC,
                null,
                new RetrieveDistrictNumber(javaName),
                null,
                null);

        return field;
    }

    /**
     * Build a Field Definition for i4see 050 (school number).
     *
     * @return FieldDefinition
     */
    protected FieldDefinition getI4see050_SchoolNumber() {
        String javaName = translateAliasToJavaName(I4SEE_050_SCHOOL_NUMBER, true);
        FieldDefinition field = new FieldDefinition(I4SEE_050_SCHOOL_NUMBER,
                SisStudent.REL_SCHOOL + PATH_DELIMITER + javaName,
                null,
                false,
                1,
                5,
                REGEX_NUMERIC,
                null,
                new RetrieveSchoolNumber(javaName),
                null,
                null);

        return field;
    }

    /**
     * Build Field definition for the i4see 490.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getI4see490_Title1Math() {
        String javaName = translateAliasToJavaName(I4SEE_490_TITLE1_MATH, false);
        if (javaName == null) {
            javaName = LABEL_PREFIX_CHAR + I4SEE_490_TITLE1_MATH;
        }
        FieldDefinition field = new FieldDefinition(I4SEE_490_TITLE1_MATH,
                javaName,
                "",
                false,
                0,
                1,
                REGEX_NUMERIC,
                null,
                null,
                null,
                null);
        return field;
    }

    /**
     * Build Field definition for the i4see 491.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getI4see491_Title1RLA() {
        String javaName = translateAliasToJavaName(I4SEE_491_TITLE1_RLA, false);
        if (javaName == null) {
            javaName = LABEL_PREFIX_CHAR + I4SEE_491_TITLE1_RLA;
        }
        FieldDefinition field = new FieldDefinition(I4SEE_491_TITLE1_RLA,
                javaName,
                "",
                false,
                0,
                1,
                REGEX_NUMERIC,
                null,
                null,
                null,
                null);
        return field;
    }

    /**
     * Build Field definition for the i4see 492.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getI4see492_Title1Science() {
        String javaName = translateAliasToJavaName(I4SEE_492_TITLE1_SCIENCE, false);
        if (javaName == null) {
            javaName = LABEL_PREFIX_CHAR + I4SEE_492_TITLE1_SCIENCE;
        }
        FieldDefinition field = new FieldDefinition(I4SEE_492_TITLE1_SCIENCE,
                javaName,
                "",
                false,
                0,
                1,
                REGEX_NUMERIC,
                null,
                null,
                null,
                null);
        return field;
    }

    /**
     * Build Field definition for the i4see 493.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getI4see493_Title1SocSty() {
        String javaName = translateAliasToJavaName(I4SEE_493_TITLE1_SOC_STY, false);
        if (javaName == null) {
            javaName = LABEL_PREFIX_CHAR + I4SEE_493_TITLE1_SOC_STY;
        }
        FieldDefinition field = new FieldDefinition(I4SEE_493_TITLE1_SOC_STY,
                javaName,
                "",
                false,
                0,
                1,
                REGEX_NUMERIC,
                null,
                null,
                null,
                null);
        return field;
    }

    /**
     * Build Field definition for the i4see 494.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getI4see494_Title1Vce() {
        String javaName = translateAliasToJavaName(I4SEE_494_TITLE1_VCE, false);
        if (javaName == null) {
            javaName = LABEL_PREFIX_CHAR + I4SEE_494_TITLE1_VCE;
        }
        FieldDefinition field = new FieldDefinition(I4SEE_494_TITLE1_VCE,
                javaName,
                "",
                false,
                0,
                1,
                REGEX_NUMERIC,
                null,
                null,
                null,
                null);
        return field;
    }

    /**
     * Build Field definition for the i4see 495.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getI4see495_Title1OI() {
        String javaName = translateAliasToJavaName(I4SEE_495_TITLE1_OI, false);
        if (javaName == null) {
            javaName = LABEL_PREFIX_CHAR + I4SEE_495_TITLE1_OI;
        }
        FieldDefinition field = new FieldDefinition(I4SEE_495_TITLE1_OI,
                javaName,
                "",
                false,
                0,
                1,
                REGEX_NUMERIC,
                null,
                null,
                null,
                null);
        return field;
    }

    /**
     * Build Field definition for the i4see 496.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getI4see496_Title1Hde() {
        String javaName = translateAliasToJavaName(I4SEE_496_TITLE1_HDE, false);
        if (javaName == null) {
            javaName = LABEL_PREFIX_CHAR + I4SEE_496_TITLE1_HDE;
        }
        FieldDefinition field = new FieldDefinition(I4SEE_496_TITLE1_HDE,
                javaName,
                "",
                false,
                0,
                1,
                REGEX_NUMERIC,
                null,
                null,
                null,
                null);
        return field;
    }

    /**
     * Build Field definition for the i4see 497.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getI4see497_Title1Sga() {
        String javaName = translateAliasToJavaName(I4SEE_497_TITLE1_SGA, false);
        if (javaName == null) {
            javaName = LABEL_PREFIX_CHAR + I4SEE_497_TITLE1_SGA;
        }
        FieldDefinition field = new FieldDefinition(I4SEE_497_TITLE1_SGA,
                javaName,
                "",
                false,
                0,
                1,
                REGEX_NUMERIC,
                null,
                null,
                null,
                null);
        return field;
    }

    /**
     * Build Field definition for the i4see 498.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getI4see498_Title1Ossp() {
        String javaName = translateAliasToJavaName(I4SEE_498_TITLE1_OSSP, false);
        if (javaName == null) {
            javaName = LABEL_PREFIX_CHAR + I4SEE_498_TITLE1_OSSP;
        }
        FieldDefinition field = new FieldDefinition(I4SEE_498_TITLE1_OSSP,
                javaName,
                "",
                false,
                0,
                1,
                REGEX_NUMERIC,
                null,
                null,
                null,
                null);
        return field;
    }

    /**
     * Build Field definition for the i4see 640.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getI4see640_ReadingRecovery() {
        String javaName = translateAliasToJavaName(I4SEE_640_READING_RECOVERY, false);
        if (javaName == null) {
            javaName = LABEL_PREFIX_CHAR + I4SEE_640_READING_RECOVERY;
        }
        FieldDefinition field = new FieldDefinition(I4SEE_640_READING_RECOVERY,
                javaName,
                "",
                false,
                0,
                1,
                REGEX_NUMERIC,
                null,
                null,
                null,
                null);
        return field;
    }

    /**
     * Build Field definition for the i4see 700.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getI4see700_APAH() {
        FieldDefinition field = new FieldDefinition(I4SEE_700_APAH_NAME,
                m_i4seeCourseApId,
                null,
                false,
                0,
                1,
                REGEX_NUMERIC,
                null,
                new RetrieveAPInfo(I4SEE_700_APAH_CODE),
                null,
                null);
        return field;
    }

    /**
     * Build Field definition for the i4see 701.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getI4see701_APBIO() {
        FieldDefinition field = new FieldDefinition(I4SEE_701_APBIO_NAME,
                m_i4seeCourseApId,
                null,
                false,
                0,
                1,
                REGEX_NUMERIC,
                null,
                new RetrieveAPInfo(I4SEE_701_APBIO_CODE),
                null,
                null);
        return field;
    }

    /**
     * Build Field definition for the i4see 702.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getI4see702_APCALCAB() {
        FieldDefinition field = new FieldDefinition(I4SEE_702_APCALCAB_NAME,
                m_i4seeCourseApId,
                null,
                false,
                0,
                1,
                REGEX_NUMERIC,
                null,
                new RetrieveAPInfo(I4SEE_702_APCALCAB_CODE),
                null,
                null);
        return field;
    }

    /**
     * Build Field definition for the i4see 703.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getI4see703_APCALCBC() {
        FieldDefinition field = new FieldDefinition(I4SEE_703_APCALCBC_NAME,
                m_i4seeCourseApId,
                null,
                false,
                0,
                1,
                REGEX_NUMERIC,
                null,
                new RetrieveAPInfo(I4SEE_703_APCALCBC_CODE),
                null,
                null);
        return field;
    }

    /**
     * Build Field definition for the i4see 704.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getI4see704_APCHEM() {
        FieldDefinition field = new FieldDefinition(I4SEE_704_APCHEM_NAME,
                m_i4seeCourseApId,
                null,
                false,
                0,
                1,
                REGEX_NUMERIC,
                null,
                new RetrieveAPInfo(I4SEE_704_APCHEM_CODE),
                null,
                null);
        return field;
    }

    /**
     * Build Field definition for the i4see 705.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getI4see705_APCS() {
        FieldDefinition field = new FieldDefinition(I4SEE_705_APCS_NAME,
                m_i4seeCourseApId,
                null,
                false,
                0,
                1,
                REGEX_NUMERIC,
                null,
                new RetrieveAPInfo(I4SEE_705_APCS_CODE),
                null,
                null);
        return field;
    }

    /**
     * Build Field definition for the i4see 706.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getI4see706_APCSAB() {
        FieldDefinition field = new FieldDefinition(I4SEE_706_APCSAB_NAME,
                m_i4seeCourseApId,
                null,
                false,
                0,
                1,
                REGEX_NUMERIC,
                null,
                new RetrieveAPInfo(I4SEE_706_APCSAB_CODE),
                null,
                null);
        return field;
    }

    /**
     * Build Field definition for the i4see 707.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getI4see707_APMACRO() {
        FieldDefinition field = new FieldDefinition(I4SEE_707_APMACRO_NAME,
                m_i4seeCourseApId,
                null,
                false,
                0,
                1,
                REGEX_NUMERIC,
                null,
                new RetrieveAPInfo(I4SEE_707_APMACRO_CODE),
                null,
                null);
        return field;
    }

    /**
     * Build Field definition for the i4see 708.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getI4see708_APMICRO() {
        FieldDefinition field = new FieldDefinition(I4SEE_708_APMICRO_NAME,
                m_i4seeCourseApId,
                null,
                false,
                0,
                1,
                REGEX_NUMERIC,
                null,
                new RetrieveAPInfo(I4SEE_708_APMICRO_CODE),
                null,
                null);
        return field;
    }

    /**
     * Build Field definition for the i4see 709.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getI4see709_APAPELC() {
        FieldDefinition field = new FieldDefinition(I4SEE_709_APAPELC_NAME,
                m_i4seeCourseApId,
                null,
                false,
                0,
                1,
                REGEX_NUMERIC,
                null,
                new RetrieveAPInfo(I4SEE_709_APAPELC_CODE),
                null,
                null);
        return field;
    }

    /**
     * Build Field definition for the i4see 710.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getI4see710_APELCOMP() {
        FieldDefinition field = new FieldDefinition(I4SEE_710_APELCOMP_NAME,
                m_i4seeCourseApId,
                null,
                false,
                0,
                1,
                REGEX_NUMERIC,
                null,
                new RetrieveAPInfo(I4SEE_710_APELCOMP_CODE),
                null,
                null);
        return field;
    }

    /**
     * Build Field definition for the i4see 711.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getI4see711_APES() {
        FieldDefinition field = new FieldDefinition(I4SEE_711_APES_NAME,
                m_i4seeCourseApId,
                null,
                false,
                0,
                1,
                REGEX_NUMERIC,
                null,
                new RetrieveAPInfo(I4SEE_711_APES_CODE),
                null,
                null);
        return field;
    }

    /**
     * Build Field definition for the i4see 712.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getI4see712_APEH() {
        FieldDefinition field = new FieldDefinition(I4SEE_712_APEH_NAME,
                m_i4seeCourseApId,
                null,
                false,
                0,
                1,
                REGEX_NUMERIC,
                null,
                new RetrieveAPInfo(I4SEE_712_APEH_CODE),
                null,
                null);
        return field;
    }

    /**
     * Build Field definition for the i4see 713.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getI4see713_APFL() {
        FieldDefinition field = new FieldDefinition(I4SEE_713_APFL_NAME,
                m_i4seeCourseApId,
                null,
                false,
                0,
                1,
                REGEX_NUMERIC,
                null,
                new RetrieveAPInfo(I4SEE_713_APFL_CODE),
                null,
                null);
        return field;
    }

    /**
     * Build Field definition for the i4see 714.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getI4see714_APFLIT() {
        FieldDefinition field = new FieldDefinition(I4SEE_714_APFLIT_NAME,
                m_i4seeCourseApId,
                null,
                false,
                0,
                1,
                REGEX_NUMERIC,
                null,
                new RetrieveAPInfo(I4SEE_714_APFLIT_CODE),
                null,
                null);
        return field;
    }

    /**
     * Build Field definition for the i4see 715.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getI4see715_APGL() {
        FieldDefinition field = new FieldDefinition(I4SEE_715_APGL_NAME,
                m_i4seeCourseApId,
                null,
                false,
                0,
                1,
                REGEX_NUMERIC,
                null,
                new RetrieveAPInfo(I4SEE_715_APGL_CODE),
                null,
                null);
        return field;
    }

    /**
     * Build Field definition for the i4see 716.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getI4see716_APGPC() {
        FieldDefinition field = new FieldDefinition(I4SEE_716_APGPC_NAME,
                m_i4seeCourseApId,
                null,
                false,
                0,
                1,
                REGEX_NUMERIC,
                null,
                new RetrieveAPInfo(I4SEE_716_APGPC_CODE),
                null,
                null);
        return field;
    }

    /**
     * Build Field definition for the i4see 717.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getI4see717_APGPUS() {
        FieldDefinition field = new FieldDefinition(I4SEE_717_APGPUS_NAME,
                m_i4seeCourseApId,
                null,
                false,
                0,
                1,
                REGEX_NUMERIC,
                null,
                new RetrieveAPInfo(I4SEE_717_APGPUS_CODE),
                null,
                null);
        return field;
    }

    /**
     * Build Field definition for the i4see 718.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getI4see718_APHG() {
        FieldDefinition field = new FieldDefinition(I4SEE_718_APHG_NAME,
                m_i4seeCourseApId,
                null,
                false,
                0,
                1,
                REGEX_NUMERIC,
                null,
                new RetrieveAPInfo(I4SEE_718_APHG_CODE),
                null,
                null);
        return field;
    }

    /**
     * Build Field definition for the i4see 719.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getI4see719_APIEL() {
        FieldDefinition field = new FieldDefinition(I4SEE_719_APIEL_NAME,
                m_i4seeCourseApId,
                null,
                false,
                0,
                1,
                REGEX_NUMERIC,
                null,
                new RetrieveAPInfo(I4SEE_719_APIEL_CODE),
                null,
                null);
        return field;
    }

    /**
     * Build Field definition for the i4see 720.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getI4see720_APIEL() {
        FieldDefinition field = new FieldDefinition(I4SEE_720_APLL_NAME,
                m_i4seeCourseApId,
                null,
                false,
                0,
                1,
                REGEX_NUMERIC,
                null,
                new RetrieveAPInfo(I4SEE_720_APLL_CODE),
                null,
                null);
        return field;
    }

    /**
     * Build Field definition for the i4see 721.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getI4see721_APLV() {
        FieldDefinition field = new FieldDefinition(I4SEE_721_APLV_NAME,
                m_i4seeCourseApId,
                null,
                false,
                0,
                1,
                REGEX_NUMERIC,
                null,
                new RetrieveAPInfo(I4SEE_721_APLV_CODE),
                null,
                null);
        return field;
    }

    /**
     * Build Field definition for the i4see 722.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getI4see722_APMT() {
        FieldDefinition field = new FieldDefinition(I4SEE_722_APMT_NAME,
                m_i4seeCourseApId,
                null,
                false,
                0,
                1,
                REGEX_NUMERIC,
                null,
                new RetrieveAPInfo(I4SEE_722_APMT_CODE),
                null,
                null);
        return field;
    }

    /**
     * Build Field definition for the i4see 723.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getI4see723_APPB() {
        FieldDefinition field = new FieldDefinition(I4SEE_723_APPB_NAME,
                m_i4seeCourseApId,
                null,
                false,
                0,
                1,
                REGEX_NUMERIC,
                null,
                new RetrieveAPInfo(I4SEE_723_APPB_CODE),
                null,
                null);
        return field;
    }

    /**
     * Build Field definition for the i4see 724.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getI4see724_APPCEM() {
        FieldDefinition field = new FieldDefinition(I4SEE_724_APPCEM_NAME,
                m_i4seeCourseApId,
                null,
                false,
                0,
                1,
                REGEX_NUMERIC,
                null,
                new RetrieveAPInfo(I4SEE_724_APPCEM_CODE),
                null,
                null);
        return field;
    }

    /**
     * Build Field definition for the i4see 725.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getI4see725_APPC() {
        FieldDefinition field = new FieldDefinition(I4SEE_725_APPC_NAME,
                m_i4seeCourseApId,
                null,
                false,
                0,
                1,
                REGEX_NUMERIC,
                null,
                new RetrieveAPInfo(I4SEE_725_APPC_CODE),
                null,
                null);
        return field;
    }

    /**
     * Build Field definition for the i4see 726.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getI4see726_APPSY() {
        FieldDefinition field = new FieldDefinition(I4SEE_726_APPSY_NAME,
                m_i4seeCourseApId,
                null,
                false,
                0,
                1,
                REGEX_NUMERIC,
                null,
                new RetrieveAPInfo(I4SEE_726_APPSY_CODE),
                null,
                null);
        return field;
    }

    /**
     * Build Field definition for the i4see 727.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getI4see727_APPS() {
        FieldDefinition field = new FieldDefinition(I4SEE_727_APPS_NAME,
                m_i4seeCourseApId,
                null,
                false,
                0,
                1,
                REGEX_NUMERIC,
                null,
                new RetrieveAPInfo(I4SEE_727_APPS_CODE),
                null,
                null);
        return field;
    }

    /**
     * Build Field definition for the i4see 728.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getI4see728_APSQ() {
        FieldDefinition field = new FieldDefinition(I4SEE_728_APSQ_NAME,
                m_i4seeCourseApId,
                null,
                false,
                0,
                1,
                REGEX_NUMERIC,
                null,
                new RetrieveAPInfo(I4SEE_728_APSQ_CODE),
                null,
                null);
        return field;
    }

    /**
     * Build Field definition for the i4see 729.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getI4see729_APSTAT() {
        FieldDefinition field = new FieldDefinition(I4SEE_729_APSTAT_NAME,
                m_i4seeCourseApId,
                null,
                false,
                0,
                1,
                REGEX_NUMERIC,
                null,
                new RetrieveAPInfo(I4SEE_729_APSTAT_CODE),
                null,
                null);
        return field;
    }

    /**
     * Build Field definition for the i4see 730.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getI4see730_APMECH() {
        FieldDefinition field = new FieldDefinition(I4SEE_730_APMECH_NAME,
                m_i4seeCourseApId,
                null,
                false,
                0,
                1,
                REGEX_NUMERIC,
                null,
                new RetrieveAPInfo(I4SEE_730_APMECH_CODE),
                null,
                null);
        return field;
    }

    /**
     * Build Field definition for the i4see 731.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getI4see731_APLANG() {
        FieldDefinition field = new FieldDefinition(I4SEE_731_APLANG_NAME,
                m_i4seeCourseApId,
                null,
                false,
                0,
                1,
                REGEX_NUMERIC,
                null,
                new RetrieveAPInfo(I4SEE_731_APLANG_CODE),
                null,
                null);
        return field;
    }

    /**
     * Build Field definition for the i4see 732.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getI4see732_APLIT() {
        FieldDefinition field = new FieldDefinition(I4SEE_732_APLIT_NAME,
                m_i4seeCourseApId,
                null,
                false,
                0,
                1,
                REGEX_NUMERIC,
                null,
                new RetrieveAPInfo(I4SEE_732_APLIT_CODE),
                null,
                null);
        return field;
    }

    /**
     * Build Field definition for the i4see 733.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getI4see733_APSA2D() {
        FieldDefinition field = new FieldDefinition(I4SEE_733_APSA2D_NAME,
                m_i4seeCourseApId,
                null,
                false,
                0,
                1,
                REGEX_NUMERIC,
                null,
                new RetrieveAPInfo(I4SEE_733_APSA2D_CODE),
                null,
                null);
        return field;
    }

    /**
     * Build Field definition for the i4see 734.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getI4see734_APSA3D() {
        FieldDefinition field = new FieldDefinition(I4SEE_734_APSA3D_NAME,
                m_i4seeCourseApId,
                null,
                false,
                0,
                1,
                REGEX_NUMERIC,
                null,
                new RetrieveAPInfo(I4SEE_734_APSA3D_CODE),
                null,
                null);
        return field;
    }

    /**
     * Build Field definition for the i4see 735.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getI4see735_APLANG() {
        FieldDefinition field = new FieldDefinition(I4SEE_735_APSAD_NAME,
                m_i4seeCourseApId,
                null,
                false,
                0,
                1,
                REGEX_NUMERIC,
                null,
                new RetrieveAPInfo(I4SEE_735_APSAD_CODE),
                null,
                null);
        return field;
    }

    /**
     * Build Field definition for the i4see 736.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getI4see736_APPUSH() {
        FieldDefinition field = new FieldDefinition(I4SEE_736_APPUSH_NAME,
                m_i4seeCourseApId,
                null,
                false,
                0,
                1,
                REGEX_NUMERIC,
                null,
                new RetrieveAPInfo(I4SEE_736_APPUSH_CODE),
                null,
                null);
        return field;
    }

    /**
     * Build Field definition for the i4see 737.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getI4see737_APWH() {
        FieldDefinition field = new FieldDefinition(I4SEE_737_APWH_NAME,
                m_i4seeCourseApId,
                null,
                false,
                0,
                1,
                REGEX_NUMERIC,
                null,
                new RetrieveAPInfo(I4SEE_737_APWH_CODE),
                null,
                null);
        return field;
    }

    /**
     * Build Field definition for the student name view.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getName() {
        FieldDefinition field = new FieldDefinition(STUDENT_NAME,
                SisStudent.COL_NAME_VIEW,
                null,
                false,
                1,
                32,
                null,
                null,
                null,
                null,
                null);
        return field;
    }

    /**
     * Build Field definition for the student SASID (i4see 010).
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getSasid() {
        FieldDefinition field = new FieldDefinition(I4SEE_010_SASID,
                SisStudent.COL_STATE_ID,
                null,
                false,
                1,
                32,
                null,
                null,
                null,
                null,
                null);
        return field;
    }

    /**
     * Adds criteria to filter the export selection by record set.
     *
     * @param criteria Criteria
     * @param recordSetName String
     */
    private void addRecordSetCriteria(Criteria criteria, String recordSetName) {
        Criteria recordSetCriteria = new Criteria();
        recordSetCriteria.addEqualTo(RecordSetKey.REL_RECORD_SET + "." + RecordSet.COL_NAME,
                recordSetName);

        criteria.addIn(X2BaseBean.COL_OID, new SubQuery(RecordSetKey.class,
                RecordSetKey.COL_OBJECT_OID, recordSetCriteria));
    }


    /**
     * Returns the days-in-session for the given school and calendar combination.
     *
     * @param school SisSchool
     * @param calendar String
     * @return Set of PlainDate objects
     */
    private Set getCalendarDays(SisSchool school, String calendar) {
        if (!m_schoolsToCalendars.containsKey(school.getOid())) {
            PlainDate startDate = m_scheduleMap.get(school.getOid()).getStartDate();
            Map calendarData = m_enrollmentManager.getCalendarLookup(school, startDate, m_reportDate);
            m_schoolsToCalendars.put(school.getOid(), calendarData);
        }

        return (Set) ((Map) m_schoolsToCalendars.get(school.getOid())).get(calendar);
    }

    /**
     * Builds a criteria for the students that should be reported (not considering user input
     * parameters).
     *
     * @return Criteria
     */
    private Criteria getReportingCriteria() {
        /*
         * Who should be included? Primary students and, optionally, summer withdrawals.
         *
         * The students that belong to each group depend upon how this export is being run:
         *
         * Case 1: The export is being run for either (A) the entire district or (B) a single school
         * without "nested districts" (99% of all cases)
         *
         * Case 2: The export is being run for a single school with "nested districts"
         *
         * ----------------------------------------------------------------------------------------
         *
         * Q: What are "nested districts" anyway?
         *
         * A: A single X2 district could really represent multiple districts as far as the DOE is
         * concerned. For example, Nauset is a single X2 district but only the middle and high
         * schools are in the Nauset Regional School District. All the elementary schools belong
         * to their own districts according to the DOE. These "nested districts" can be
         * represented in X2 by setting different Adjusted District Code values on the school
         * records. If "nested districts" are used then ALL schools should have an Adjusted
         * District Code (even the archive school).
         * 
         * ----------------------------------------------------------------------------------------
         *
         * Primary students, case 1:
         *
         * Students in an active, non-archived school in the district
         * 
         * ----------------------------------------------------------------------------------------
         * 
         * Primary students, case 2:
         *
         * Students in the select school as well as students who have withdrawn from the selected
         * school between the start of the school year and the report date (both dates inclusive)
         * and are now in a school with a different adjusted district code
         * 
         * ----------------------------------------------------------------------------------------
         *
         * Summer withdrawals, case 1:
         *
         * Students who withdrew during the summer (start/end dates inclusive) and are now in the
         * archive school
         * 
         * ----------------------------------------------------------------------------------------
         * 
         * Summer withdrawals, case 2:
         *
         * Students who withdrew from the selected school during the summer (start/end dates
         * inclusive) and are now either in the archive school or a school with a different
         * adjusted district code
         * 
         * ----------------------------------------------------------------------------------------
         *
         */
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(ADJUSTED_DISTRICT_CODE_FIELD);

        String adjustedDistrictCode = null;
        if (isSchoolContext() && field != null) {
            adjustedDistrictCode = (String) getSchool().getFieldValueByBeanPath(field.getJavaName());
        }

        boolean useNestedDistricts = !StringUtils.isEmpty(adjustedDistrictCode);

        /*
         * Primary students
         */
        Criteria primaryCriteria = new Criteria();
        if (isSchoolContext()) {
            primaryCriteria.addEqualTo(SisStudent.COL_SCHOOL_OID, getSchool().getOid());

            if (useNestedDistricts) {
                PlainDate startDate = ((SisSchool) getSchool()).getActiveSchedule().getStartDate();

                Criteria withdrawalsCriteria = new Criteria();
                withdrawalsCriteria.addIn(X2BaseBean.COL_OID, getStudentWithdrawalQuery(startDate, m_reportDate));
                withdrawalsCriteria.addNotEqualTo(
                        SisStudent.REL_SCHOOL + ModelProperty.PATH_DELIMITER + field.getJavaName(),
                        adjustedDistrictCode);

                primaryCriteria.addOrCriteria(withdrawalsCriteria);
            }
        } else {
            primaryCriteria.addEqualTo(SisStudent.COL_ORGANIZATION1_OID, getOrganization().getOid());
            primaryCriteria.addEqualTo(
                    SisStudent.REL_SCHOOL + ModelProperty.PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR,
                    Boolean.FALSE);
            primaryCriteria.addEqualTo(
                    SisStudent.REL_SCHOOL + ModelProperty.PATH_DELIMITER + SisSchool.COL_ARCHIVE_INDICATOR,
                    Boolean.FALSE);
        }

        X2Criteria reportingCriteria = new X2Criteria();
        reportingCriteria.addAndCriteria(primaryCriteria);

        return reportingCriteria;
    }

    /**
     * Returns the criteria that retrieves all students that should be included in the export.
     *
     * @return Criteria
     */
    private Criteria getStudentCriteria() {
        /*
         * First build the criteria based on the user's input
         */
        X2Criteria userCriteria = new X2Criteria();

        Boolean requireReportStatus = (Boolean) getParameter(REQUIRE_REPORT_STATUS_PARAM);
        if (requireReportStatus.booleanValue()) {
            userCriteria.addEqualTo(m_reportStatusField, I4SEE_STATUS_FIELD_REPORT_CODE);
        }

        String queryString = (String) getParameter(QUERY_STRING_PARAM);
        int queryBy = ((Integer) getParameter(QUERY_BY_PARAM)).intValue();
        switch (queryBy) {
            case 1: // YOG
                userCriteria.addEqualTo(SisStudent.COL_YOG, queryString);
                break;

            case 2: // LASID
                userCriteria.addEqualTo(SisStudent.COL_LOCAL_ID, queryString);
                break;

            case 3: // SASID
                userCriteria.addEqualTo(SisStudent.COL_STATE_ID, queryString);
                break;

            case 4: // Snapshot
                addRecordSetCriteria(userCriteria, queryString);
                break;

            default:
                // Take all students in the district
                break;
        }

        /*
         * Then combine the user criteria with the general reporting criteria
         */
        Criteria studentCriteria = new Criteria();
        studentCriteria.addAndCriteria(userCriteria);
        studentCriteria.addAndCriteria(getReportingCriteria());

        return studentCriteria;
    }

    /**
     * Returns a query that finds the students who withdrew during the given date range (filtered
     * by school as appropriate).
     *
     * @param startDate PlainDate
     * @param endDate PlainDate
     * @return A SubQuery that returns the student OID from StudentEnrollment records
     */
    private SubQuery getStudentWithdrawalQuery(PlainDate startDate, PlainDate endDate) {
        Criteria enrollmentCriteria = new Criteria();
        enrollmentCriteria.addEqualTo(StudentEnrollment.COL_ENROLLMENT_TYPE, StudentEnrollment.WITHDRAWAL);
        enrollmentCriteria.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, startDate);
        enrollmentCriteria.addLessOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, endDate);

        if (isSchoolContext()) {
            enrollmentCriteria.addEqualTo(StudentEnrollment.COL_SCHOOL_OID, getSchool().getOid());
        }

        return new SubQuery(StudentEnrollment.class, StudentEnrollment.COL_STUDENT_OID, enrollmentCriteria);
    }

    /**
     * Sets the Java names (bean paths) for all the exported fields.
     */
    private void initializeFields() {
        /*
         * Resolve some support aliases, log an error and stop the export if an alias is not found.
         */
        m_adjustedSchoolCode = translateAliasToJavaName(ADJUSTED_SCHOOL_NUMBER_FIELD, true);

        m_enrollmentManager = new EnrollmentManager(getBroker(), getPrivilegeSet(), getOrganization());
        m_firstDayDate = getCurrentContext().getStartDate();
        m_firstDayMembers = m_enrollmentManager.getMembershipAsOf(m_firstDayDate, getOrganization());
        m_i4seeCourseApId = translateAliasToJavaName(COURSE_I4SEE_AP_ID_FIELD, true);
        m_i4seeApExamTaken = translateAliasToJavaName(TRANSCRIPT_AP_EXAM_TAKEN_FIELD, true);
        m_reportStatusField = translateAliasToJavaName(I4SEE_STATUS_FIELD, true);
        m_schoolsToCalendars = new HashMap();
    }

    /**
     * Loads the active schedule for each school.
     */
    private void loadActiveSchedules() {
        m_scheduleMap = new HashMap();
        Collection<SisSchool> schools = m_schoolMap.values();
        for (SisSchool school : schools) {
            m_scheduleMap.put(school.getOid(), school.getActiveSchedule());
        }
    }

    /**
     * Loads all the school beans in the district.
     */
    private void loadSchools() {
        QueryByCriteria schoolQuery = new QueryByCriteria(SisSchool.class, new Criteria());
        m_schoolMap = getBroker().getMapByQuery(schoolQuery, X2BaseBean.COL_OID, getBroker().getCount(schoolQuery));
    }
}
