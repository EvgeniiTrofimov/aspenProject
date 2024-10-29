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
package com.x2dev.procedures.statereporting.fl;

import static com.x2dev.procedures.statereporting.fl.FLFasterExportConfiguration.*;
import static com.x2dev.procedures.statereporting.fl.FLWdisExportConfiguration.FIELD_RESIDENT_STATUS_STATE_COUNTY;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.FieldMergeAttributesInterface;
import com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.FieldMerger;
import com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData;
import com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.ValueAdjuster;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.types.PlainDate;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;


/**
 * The Class FLFasterMerge01StudentProcedure.
 */
public class FLFasterMerge01StudentProcedure extends RecordTypeMergeData {

    /**
     * The Enum FieldMergeAttributes.
     */
    private enum FieldMergeAttributes implements FieldMergeAttributesInterface {
        STD_NUM_ID_FL(FIELD_STD_NUM_ID_FL, new FieldMerger()),
        //
        STD_NUM_ID_ALIAS(FIELD_STD_NUM_ID_ALIAS, new FieldMerger()),
        //
        LAST_NAME(FIELD_LAST_NAME, new FieldMerger()),
        //
        APPENDAGE(FIELD_APPENDAGE, new FieldMerger()),
        //
        FIRST_NAME(FIELD_FIRST_NAME, new FieldMerger()),
        //
        MIDDLE_NAME(FIELD_MIDDLE_NAME, new FieldMerger()),
        //
        ETHNICITY(FIELD_ETHNICITY, new FieldMerger(), new ValueAdjusterEqualTo("Y")),
        //
        MSIX_ID(FIELD_MSIX_ID, new FieldMerger()),
        //
        MIGRANT_STATUS_TERM(FIELD_MIGRANT_STATUS_TERM, new FieldMerger()),
        //
        COUNTRY_OF_BIRTH(FIELD_COUNTRY_OF_BIRTH, new FieldMerger()),
        //
        DIPLOMA_DESIGNATION(FIELD_DIPLOMA_DESIGNATION, new FieldMerger()),
        //
        GRADUATION_OPTION(FIELD_GRADUATION_OPTION, new FieldMerger()),
        //
        YEAR_ENTERED_9_GRADE(FIELD_YEAR_ENTERED_9_GRADE, new FieldMerger(), new ValueAdjuster() {
            @Override
            public Object getAdjustedValue(String value) {
                return value.substring(0, 4);
            }
        }),
        //
        NTL_MERIT_SCHOLAR(FIELD_NTL_MERIT_SCHOLAR, new FieldMerger()),
        //
        NTL_ACHV_SCHOLAR(FIELD_NTL_ACHV_SCHOLAR, new FieldMerger()),
        //
        GRAD_PURP_DATE_COMM(FIELD_GRAD_PURP_DATE_COMM, new FieldMerger(),
                new ValueAdjusterDateByAlias(new ValueAdjusterDate("MMyyyy"))),
        //
        GRAD_PURP_DATE_MATH(FIELD_GRAD_PURP_DATE_MATH, new FieldMerger(),
                new ValueAdjusterDateByAlias(new ValueAdjusterDate("MMyyyy"))),
        //
        NTL_HISPANIC_SCHOLAR(FIELD_NTL_HISPANIC_SCHOLAR, new FieldMerger(), new ValueAdjusterEqualTo("S")),
        //
        ADDRESS_STREET(FIELD_ADDRESS_STREET, new FieldMerger()),
        //
        ADDRESS_APT_NUM(FIELD_ADDRESS_APT_NUM, new FieldMerger()),
        //
        ADDRESS_CITY(FIELD_ADDRESS_CITY, new FieldMerger()),
        //
        ADDRESS_STATE(FIELD_ADDRESS_STATE, new FieldMerger()),
        //
        ADDRESS_ZIP(FIELD_ADDRESS_ZIP, new FieldMerger()),
        //
        AKA_NAME_1(FIELD_AKA_NAME_1, new FieldMerger()),
        //
        AKA_NAME_2(FIELD_AKA_NAME_2, new FieldMerger()),
        //
        AKA_NAME_3(FIELD_AKA_NAME_3, new FieldMerger()),
        //
        AKA_NAME_4(FIELD_AKA_NAME_4, new FieldMerger()),
        //
        DOB_VERIFICATION(FIELD_DOB_VERIFICATION, new FieldMerger()),
        //
        BIRTH_PLACE(FIELD_BIRTH_PLACE, new FieldMerger()),
        //
        HLTH_EXAM_SKL_ENTRY(FIELD_HLTH_EXAM_SKL_ENTRY, new FieldMerger()),
        //
        CRIT_HLTH_INFO_IND(FIELD_CRIT_HLTH_INFO_IND, new FieldMerger(), new ValueAdjusterEqualTo("Y")),
        //
        ONLINE_COURSE_EXEMPT(FIELD_ONLINE_COURSE_EXEMPT, new FieldMerger()),
        //
        ASM_MET_ELA(FIELD_ASM_MET_ELA, new FieldMerger(), new ValueAdjusterEqualTo("Y")),
        //
        ASM_PASSED_ALG_2(FIELD_ASM_PASSED_ALG_2, new FieldMerger()),
        //
        ASM_MET_ALG_1(FIELD_ASM_MET_ALG_1, new FieldMerger()),
        //
        ASM_PASSED_GEOMET(FIELD_ASM_PASSED_GEOMET, new FieldMerger()),
        //
        ASM_PASSED_BIOLOGY(FIELD_ASM_PASSED_BIOLOGY, new FieldMerger()),
        //
        ASM_PASSED_USHISTORY(FIELD_ASM_PASSED_USHISTORY, new FieldMerger()),
        //
        CLASS_RANK_NUM_POS(FIELD_CLASS_RANK_NUM_POS, new FieldMerger()),
        //
        CLASS_RANK_PERCENT(FIELD_CLASS_RANK_PERCENT, new FieldMerger()),
        //
        CLASS_RANK_TOTAL(FIELD_CLASS_RANK_TOTAL, new FieldMerger()),
        //
        GPA_DISTRICT(FIELD_GPA_DISTRICT, new FieldMerger()),
        //
        GPA_STATE(FIELD_GPA_STATE, new FieldMerger()),
        //
        DIPLOMA_DATE(FIELD_DIPLOMA_DATE, new FieldMerger(),
                new ValueAdjusterDateByAlias(new ValueAdjusterDateMonthDaysZeroes())),
        //
        DIPLOMA_TYPE(FIELD_DIPLOMA_TYPE, new FieldMerger()),
        //
        CERT_OF_COMPL_DATE(FIELD_CERT_OF_COMPL_DATE, new FieldMerger(),
                new ValueAdjusterDateByAlias(new ValueAdjusterDateMonthDaysZeroes())),
        //
        CERT_OF_COMPL_TYPE(FIELD_CERT_OF_COMPL_TYPE, new FieldMerger()),
        //
        CLASS_RANK_EFF_DATE(FIELD_CLASS_RANK_EFF_DATE, new FieldMerger(),
                new ValueAdjusterDateByAlias(new ValueAdjuster() {
                    @Override
                    public Object getAdjustedValue(String value) {
                        if (!StringUtils.isEmpty(value) && !"00000000".equals(value)) {
                            SimpleDateFormat format = new SimpleDateFormat("MMddyyyy");
                            try {
                                return format.parse(value);
                            } catch (ParseException e) {
                                e.printStackTrace();
                            }
                        }

                        return null;
                    }
                })),
        //
        NATIVE_LANGUAGE(FIELD_NATIVE_LANGUAGE, new FieldMerger()),
        //
        PRIMARY_LANGUAGE(FIELD_PRIMARY_LANGUAGE, new FieldMerger()),
        //
        GRAD_REQ_BASIS(FIELD_GRAD_REQ_BASIS, new FieldMerger()),
        //
        DATE_OF_BIRTH(FIELD_DATE_OF_BIRTH, new FieldMerger()),
        //
        RESIDENT_STATUS_STATE_COUNTY(FIELD_RESIDENT_STATUS_STATE_COUNTY, new FieldMerger()),
        //
        MULTIPLE_BIRTH_STD(FIELD_MULTIPLE_BIRTH_STD, new FieldMerger(), new ValueAdjusterEqualTo("Y")),
        //
        EVEN_START_FAMIL_LIT(FIELD_EVEN_START_FAMIL_LIT, new FieldMerger(), new ValueAdjusterEqualTo("Y")),
        //
        FL_FIRST_START_PGM(FIELD_FL_FIRST_START_PGM, new FieldMerger(), new ValueAdjusterEqualTo("Y")),
        //
        DIFF_DIPLOMA(FIELD_DIFF_DIPLOMA, new FieldMerger(), new ValueAdjusterEqualTo("1")),
        //
        IB_DIPLOMA(FIELD_IB_DIPLOMA, new FieldMerger(), new ValueAdjusterEqualTo("Y")),
        //
        COLLEGE_READY_DIPLOM(FIELD_COLLEGE_READY_DIPLOM, new FieldMerger(), new ValueAdjusterEqualTo("Y")),
        //
        AIC_PROGRAM_COMPLETE(FIELD_AIC_PROGRAM_COMPLETE, new FieldMerger(), new ValueAdjusterEqualTo("Y")),
        //
        DST_COM_VOL_REQ_MET(FIELD_DST_COM_VOL_REQ_MET, new FieldMerger(), new ValueAdjusterEqualTo("Y")),
        //
        EARLY_ADM_STUDENT(FIELD_EARLY_ADM_STUDENT, new FieldMerger(), new ValueAdjusterEqualTo("Y")),
        //
        ADDL_SKL_YEAR(FIELD_ADDL_SKL_YEAR, new FieldMerger()),
        //
        AICE_DIPLOMA(FIELD_AICE_DIPLOMA, new FieldMerger(), new ValueAdjusterEqualTo("Y")),
        //
        BILITER_SEAL_DESIGN(FIELD_BILITER_SEAL_DESIGN, new FieldMerger()),
        //
        COMVOL_SERVICE_HRS(FIELD_COMVOL_SERVICE_HRS, new FieldMerger()),
        //
        PHYS_EDU_WAIVER(FIELD_PHYS_EDU_WAIVER, new FieldMerger(), new ValueAdjusterEqualTo("Y")),
        //
        GENDER(FIELD_GENDER, new FieldMerger()),
        //
        ADULT_FEE_STATUS_1ST(FIELD_ADULT_FEE_STATUS_1ST, new FieldMerger());

        private FieldMerger m_fieldMerger = null;
        private String m_fieldName = null;
        private ValueAdjuster m_valueAdjuster = null;


        /**
         * Instantiates a new field merge attributes.
         *
         * @param fieldName String
         * @param fieldMerger FieldMerger
         */
        private FieldMergeAttributes(String fieldName, FieldMerger fieldMerger) {
            m_fieldName = fieldName;
            m_fieldMerger = fieldMerger;
        }


        /**
         * Instantiates a new field merge attributes.
         *
         * @param fieldName String
         * @param fieldMerger FieldMerger
         * @param valueAdjuster ValueAdjuster
         */
        private FieldMergeAttributes(String fieldName, FieldMerger fieldMerger, ValueAdjuster valueAdjuster) {
            this(fieldName, fieldMerger);
            m_valueAdjuster = valueAdjuster;
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.FieldMergeAttributesInterface#getMerger()
         */
        @Override
        public FieldMerger getMerger() {
            return m_fieldMerger;
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.FieldMergeAttributesInterface#getValueAdjuster()
         */
        @Override
        public ValueAdjuster getValueAdjuster() {
            return m_valueAdjuster;
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.FieldMergeAttributesInterface#getFieldName()
         */
        @Override
        public String getFieldName() {
            return m_fieldName;
        }
    }


    /**
     * The Class ValueAdjusterDate.
     */
    public static class ValueAdjusterDate extends ValueAdjuster {
        private SimpleDateFormat m_format = null;


        /**
         * Instantiates a new value adjuster date.
         *
         * @param dateFormat String
         */
        public ValueAdjusterDate(String dateFormat) {
            m_format = new SimpleDateFormat(dateFormat);
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.ValueAdjuster#getAdjustedValue(java.lang.String)
         */
        @Override
        public Object getAdjustedValue(String value) {
            try {
                Date parsedDate = m_format.parse(value);
                return new PlainDate(parsedDate);
            } catch (ParseException e) {
                e.printStackTrace();
            }
            return null;
        }
    }


    /**
     * The Class ValueAdjusterEqualTo.
     */
    public static class ValueAdjusterEqualTo extends ValueAdjuster {
        String m_stringToCompare = null;


        /**
         * Instantiates a new value adjuster equal to.
         *
         * @param stringToCompare String
         */
        public ValueAdjusterEqualTo(String stringToCompare) {
            m_stringToCompare = stringToCompare;
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.ValueAdjuster#getAdjustedValue(java.lang.String)
         */
        @Override
        public Object getAdjustedValue(String value) {
            return Boolean.valueOf(m_stringToCompare.equals(value));
        }
    }


    /**
     * The Class ValueAdjusterDateMonthDaysZeroes.
     */
    public static class ValueAdjusterDateMonthDaysZeroes extends ValueAdjuster {


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.ValueAdjuster#getAdjustedValue(java.lang.String)
         */
        @Override
        public Object getAdjustedValue(String value) {
            if (!StringUtils.isEmpty(value) && !"00000000".equals(value)) {
                SimpleDateFormat format1 = new SimpleDateFormat("MMddyyyy");
                try {
                    return format1.parse(value);
                } catch (ParseException e) {
                    SimpleDateFormat format2 = new SimpleDateFormat("MM00yyyy");
                    try {
                        return format2.parse(value);
                    } catch (ParseException e1) {
                        SimpleDateFormat format3 = new SimpleDateFormat("0000yyyy");
                        try {
                            return format3.parse(value);
                        } catch (ParseException e2) {
                            e2.printStackTrace();
                        }
                    }
                }
            }

            return null;
        }
    }


    /**
     * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData#getBeanMergeTo(java.lang.String)
     */
    @Override
    protected X2BaseBean getBeanMergeTo(String fieldName) {
        return getStudent();
    }


    /**
     * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData#getClassMergeTo()
     */
    @Override
    protected Class<?> getClassMergeTo() {
        return SisStudent.class;
    }


    /**
     * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData#getFieldMergeAttributes()
     */
    @Override
    protected FieldMergeAttributesInterface[] getFieldMergeAttributes() {
        return FieldMergeAttributes.values();
    }
}
