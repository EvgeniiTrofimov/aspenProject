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
package com.x2dev.procedures.statereporting.uk;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.tools.stateexports.BeanRetriever;
import com.follett.fsc.core.k12.tools.stateexports.BeanValidator;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportModel;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.follett.fsc.core.k12.tools.stateexports.XMLStateReportData;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;
import org.apache.commons.collections.Bag;
import org.apache.commons.collections.bag.HashBag;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.jdom.Element;

/**
 * The Class EarlyYearsCensus.
 */
public class EarlyYearsCensus extends XMLStateReportData {

    private static final String EY4_YEARS = "EY4Years";
    private static final String EY3_YEARS = "EY3Years";
    private static final String EY2_YEARS = "EY2Years";

    private static final String ESTAB_EYP_STEACHERS = "EstabEYPSteachers";
    private static final String ESTAB_TEACHING_TOTAL = "EstabTeachingTotal";
    private static final String ESTAB_QTS_TEACHERS = "EstabQTSTeachers";
    private static final String EY_TEACHERS = "EstabTeachersInEY";
    private static final String EY_QTS_TEACHING_TOTAL = "EYQTS";
    private static final String EY_EYPS_TEACHERS = "EYEYPS";
    private static final String VAR_CENSUS_DATE = "censusDate";

    private PlainDate m_censusDate;

    /**
     * Check census validations for school's hours open.
     */
    protected class ValidateHoursOpen extends BeanValidator {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.BeanValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             X2BaseBean bean,
                                             FieldDefinition field,
                                             String value) {

            ArrayList<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            String schoolName = ((SisSchool) bean).getName();

            int openHours = Integer.valueOf(value).intValue();

            if ((openHours > 70) || (openHours < 1)) {
                errors.add(new StateReportValidationError(schoolName, field.getFieldId(),
                        "Query 8100Q Please check: Number of hours open per week is outside the range 1 -70 hours",
                        value));
            }

            return errors;
        }
    }

    /**
     * Check census validations for school's weeks open.
     */
    protected class ValidateWeeksOpen extends BeanValidator {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.BeanValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             X2BaseBean bean,
                                             FieldDefinition field,
                                             String value) {

            ArrayList<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            Double fundingWeeks = Double.valueOf(data.getFieldValue(bean, "200601"));

            String schoolName = ((SisSchool) bean).getName();
            int weeks = Integer.valueOf(value).intValue();

            if ((weeks > 52) || (weeks < 1)) {
                errors.add(new StateReportValidationError(schoolName, field.getFieldId(),
                        "Error 8105 Number of weeks open per year must be between 1 and 52 weeks.", value));
            }
            if (weeks > fundingWeeks.intValue()) {
                errors.add(new StateReportValidationError(schoolName, field.getFieldId(),
                        "Error 8102 The number of weeks open must be greater than or equal to the number of weeks funded.",
                        value));
            }

            return errors;
        }
    }

    /**
     * Check census validations for school's funding weeks.
     */
    protected class ValidateFundingWeeks extends BeanValidator {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.BeanValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             X2BaseBean bean,
                                             FieldDefinition field,
                                             String value) {

            ArrayList<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            String schoolName = ((SisSchool) bean).getName();
            int fundingWeeks = Double.valueOf(value).intValue();

            if ((fundingWeeks > 52) || (fundingWeeks < 1)) {
                errors.add(new StateReportValidationError(schoolName, field.getFieldId(),
                        "Error 8230 Number of weeks open and funded by the LA must be valid and in the range 1 to 52 weeks.",
                        value));
            }

            return errors;
        }
    }

    /**
     * Check census validations for school's staff total.
     */
    protected class ValidateEstTeacherTotal extends BeanValidator {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.BeanValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             X2BaseBean bean,
                                             FieldDefinition field,
                                             String value) {
            ArrayList<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            int estTotal = Integer.valueOf(value).intValue();

            if ((estTotal > 38) || (estTotal < 1)) {
                errors.add(new StateReportValidationError("School Total", field.getFieldId(),
                        "Query 8110Q Please check: Total teaching staff at the Setting is missing or outside the expected range of 1 to 38.",
                        value));
            }

            return errors;
        }
    }

    /**
     * Check census validations for school's QTS staff total.
     */
    protected class ValidateEstQTSTeacherTotal extends BeanValidator {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.BeanValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             X2BaseBean bean,
                                             FieldDefinition field,
                                             String value) {
            ArrayList<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            int qstTotal = Integer.valueOf(value).intValue();
            Integer estTotal = Integer.valueOf(data.getFieldValue(bean, "200629"));

            if ((qstTotal > 21) || (qstTotal < 0)) {
                errors.add(new StateReportValidationError("School Total", field.getFieldId(),
                        "Query 8115Q Please check: Total teaching staff with Qualified Teacher Status is missing or outside the expected range of 0 to 21",
                        value));
            }
            if (qstTotal > estTotal.intValue()) {
                errors.add(new StateReportValidationError("School Total", field.getFieldId(),
                        "Error 8120 Teachers with Qualified Teacher Status is greater then the Total Teaching Staff.",
                        value));
            }

            return errors;
        }
    }

    /**
     * Check census validations for school's EYPS staff total.
     */
    protected class ValidateEstEYPSTeachers extends BeanValidator {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.BeanValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             X2BaseBean bean,
                                             FieldDefinition field,
                                             String value) {
            ArrayList<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            Integer estTotal = Integer.valueOf(data.getFieldValue(bean, "200629"));
            int estEYPSstaff = Integer.valueOf(value).intValue();

            if ((estEYPSstaff > 10) || (estEYPSstaff < 0)) {
                errors.add(new StateReportValidationError("School Total", field.getFieldId(),
                        "Query 8117Q Please check: Total teaching staff with Early Years Professional Status is missing or outside the expected range of 0 to 10.",
                        value));
            }
            if ((estEYPSstaff > estTotal.intValue())) {
                errors.add(new StateReportValidationError("School Total", field.getFieldId(),
                        "Error 8123 Teachers with Early Years Professional Status is greater than the Total Teaching Staff.",
                        value));
            }

            return errors;
        }
    }

    /**
     * Check census validations for school's EY staff total.
     */
    protected class ValidateEarlyYearTeachers extends BeanValidator {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.BeanValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             X2BaseBean bean,
                                             FieldDefinition field,
                                             String value) {
            ArrayList<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            int eyTotal = Integer.valueOf(value).intValue();
            int estabTotal = Integer.valueOf(data.getFieldValue(bean, "200629")).intValue();

            if (eyTotal > 21 || eyTotal < 0) {
                errors.add(new StateReportValidationError("School Total", field.getFieldId(),
                        "Query 8125Q Please check: Teaching staff participating in EY Education is missing or outside the expected range of 0 to 21.",
                        value));
            }
            if (eyTotal > estabTotal) {
                errors.add(new StateReportValidationError("School Total", field.getFieldId(),
                        "Error 8140 Teaching Staff participating in EY Education is greater then Total Teaching Staff at Setting.",
                        value));
            }

            return errors;
        }
    }

    /**
     * Check census validations for school's EY QTS staff total.
     */
    protected class ValidateEarlyYearQTSTeachers extends BeanValidator {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.BeanValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             X2BaseBean bean,
                                             FieldDefinition field,
                                             String value) {
            ArrayList<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            int eyQTSstaffTotal = Integer.valueOf(value).intValue();
            int estabEYteachers = Integer.valueOf(data.getFieldValue(bean, "200345")).intValue();
            int estabQTSteachers = Integer.valueOf(data.getFieldValue(bean, "200344")).intValue();

            if (eyQTSstaffTotal > 6 || eyQTSstaffTotal < 0) {
                errors.add(new StateReportValidationError("School Total", field.getFieldId(),
                        "Query 8130Q Please check: EY Staff with Qualified Teacher Status is missing or outside the expected range of 0 to 6.",
                        value));
            }
            if (eyQTSstaffTotal > estabEYteachers) {
                errors.add(new StateReportValidationError("School Total", field.getFieldId(),
                        "Error 8135 EY Staff with Qualified Teacher Status is greater then the Teaching Staff participating in EY Education.",
                        value));
            }
            if (eyQTSstaffTotal > estabQTSteachers) {
                errors.add(new StateReportValidationError("School Total", field.getFieldId(),
                        "Error 8138 EY Staff with Qualified Teacher Status is greater then the Teaching Staff with QTS status at the Setting.",
                        value));
            }

            return errors;
        }
    }

    /**
     * Check census validations for school's EY EYP staff total.
     */
    protected class VaildateEarlyYearEYPTeachers extends BeanValidator {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.BeanValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             X2BaseBean bean,
                                             FieldDefinition field,
                                             String value) {
            ArrayList<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            int eyEYPSStaff = Integer.valueOf(value).intValue();
            int estabEYEYPSStaff = Integer.valueOf(data.getFieldValue(bean, "200565")).intValue();
            int estabEYStaff = Integer.valueOf(data.getFieldValue(bean, "200345")).intValue();

            if (eyEYPSStaff > 6 || eyEYPSStaff < 0) {
                errors.add(new StateReportValidationError("School Total", field.getFieldId(),
                        "Query 8141Q Please check: EY Staff with Early Years Professional Status is missing or outside the expected range of 0 to 6.",
                        value));
            }
            if (eyEYPSStaff > estabEYEYPSStaff) {
                errors.add(new StateReportValidationError("School Total", field.getFieldId(),
                        "Error 8143EY Staff with Early Years Professional Status is greater then the Teaching Staff with EYPS status at the Setting",
                        value));
            }
            if (eyEYPSStaff > estabEYStaff) {
                errors.add(new StateReportValidationError("School Total", field.getFieldId(),
                        "Error 8142 EY Staff with Early Years Professional Status is greater then the Teaching Staff participating in EY Education",
                        value));
            }

            return errors;
        }
    }

    /**
     * Check census validations for school's 2 year old total.
     */
    protected class ValidateEarlyYear2Total extends BeanValidator {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.BeanValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             X2BaseBean bean,
                                             FieldDefinition field,
                                             String value) {

            ArrayList<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            Integer EY2Total = Integer.valueOf(value);

            if (EY2Total.intValue() > 51 || EY2Total.intValue() < 0) {
                errors.add(new StateReportValidationError("School Total", field.getFieldId(),
                        "Query 8144Q Please check: Number of 2 year olds not in the expected range 0 to 51.", value));
            }

            return errors;
        }
    }

    /**
     * Check census validations for school's 3 year old total.
     */
    protected class ValidateEarlyYear3Total extends BeanValidator {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.BeanValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             X2BaseBean bean,
                                             FieldDefinition field,
                                             String value) {

            ArrayList<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            Integer EY2Total = Integer.valueOf(value);

            if (EY2Total.intValue() > 51 || EY2Total.intValue() < 0) {
                errors.add(new StateReportValidationError("School Total", field.getFieldId(),
                        "Query 8145Q Please check: Number of 3 year olds not in the expected range 0 to 51.", value));
            }

            return errors;
        }
    }

    /**
     * Check census validations for school's 4 year old total.
     */
    protected class ValidateEarlyYear4Total extends BeanValidator {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.BeanValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             X2BaseBean bean,
                                             FieldDefinition field,
                                             String value) {

            ArrayList<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            int EY2Total = Integer.valueOf(data.getFieldValue(bean, "200633")).intValue();
            int EY3Total = Integer.valueOf(data.getFieldValue(bean, "200630")).intValue();
            int EY4Total = Integer.valueOf(value).intValue();
            int totalEYstudents = EY2Total + EY3Total + EY4Total;

            if (EY4Total > 33 || EY4Total < 0) {
                errors.add(new StateReportValidationError("School Total", field.getFieldId(),
                        "Query 8150Q Please check: Number of 4 year olds not in the expected range 0 to 33.", value));
            }
            if (totalEYstudents <= 0) {
                errors.add(new StateReportValidationError("School Total", field.getFieldId(),
                        "Error 8155 Return shows no 2, 3 or 4 year olds at the setting.", value));
            }

            return errors;
        }
    }

    /**
     * Check census validations for students funded hours.
     */
    protected class ValidateFundedHours extends BeanValidator {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.BeanValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             X2BaseBean bean,
                                             FieldDefinition field,
                                             String value) {

            ArrayList<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            int hoursAtSetting = Integer.valueOf(data.getFieldValue(bean, "100291")).intValue();

            String studentName = ((SisStudent) bean).getNameView();

            /* Number has a decimal place */
            if (value.contains(".")) {
                int decimalSpot = value.indexOf(".");
                char firstDecimalValue = value.charAt(decimalSpot + 1);

                int fundedHours = Integer.valueOf(value.substring(0, decimalSpot)).intValue();

                /* Decimal values must be rounded to nearest half so can only be 0 or 5 */
                if ('5' != firstDecimalValue && '0' != firstDecimalValue) {
                    errors.add(new StateReportValidationError(studentName, field.getFieldId(),
                            "Query 821Q Please check: Child’s funded hours not provided or out of the usual range (0.5 - 15) to the nearest .5 .",
                            "Founded Hours total is invalid."));
                }
                if (fundedHours > 15 || fundedHours < 0) {
                    errors.add(new StateReportValidationError(studentName, field.getFieldId(),
                            "Query 821Q Please check: Child’s funded hours not provided or out of the usual range (0.5 - 15) to the nearest .5 .",
                            "Founded Hours total is invalid."));
                }
                if (fundedHours > hoursAtSetting) {
                    errors.add(new StateReportValidationError(studentName, field.getFieldId(),
                            "Error 8220 Funded hours cannot be more than the number of hours at the EY setting.",
                            "Founded Hours total is invalid."));
                }
            } else {
                int hours = Integer.valueOf(value).intValue();

                if (hours > 15 || hours < 0) {
                    errors.add(new StateReportValidationError(studentName, field.getFieldId(),
                            "Query 821Q Please check: Child’s funded hours not provided or out of the usual range (0.5 - 15) to the nearest .5 .",
                            "Founded Hours total is invalid."));
                }
                if (hours > hoursAtSetting) {
                    errors.add(new StateReportValidationError(studentName, field.getFieldId(),
                            "Error 8220 Funded hours cannot be more than the number of hours at the EY setting.",
                            "Founded Hours total is invalid."));
                }
            }

            return errors;
        }
    }

    /**
     * The Class ValidateSpringFundedHours.
     */
    protected class ValidateSpringFundedHours extends BeanValidator {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.BeanValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             X2BaseBean bean,
                                             FieldDefinition field,
                                             String value) {

            ArrayList<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();


            String studentName = ((SisStudent) bean).getNameView();

            /* Number has a decimal place */
            if (value.contains(".")) {
                int decimalSpot = value.indexOf(".");
                char firstDecimalValue = value.charAt(decimalSpot + 1);

                int fundedHours = Integer.valueOf(value.substring(0, decimalSpot)).intValue();

                /* Decimal values must be rounded to nearest half so can only be 0 or 5 */
                if ('5' != firstDecimalValue && '0' != firstDecimalValue) {
                    errors.add(new StateReportValidationError(studentName, field.getFieldId(),
                            "Query 8250Q Please check: Total Funded Spring Hours should be in the range 0.5 to 999.5, to the nearest .5.",
                            "Spring Founded Hours total is invalid."));
                }
                if (fundedHours > 999.5 || fundedHours < 0) {
                    errors.add(new StateReportValidationError(studentName, field.getFieldId(),
                            "Query 8250Q Please check: Total Funded Spring Hours should be in the range 0.5 to 999.5, to the nearest .5.",
                            "Spring Founded Hours total is invalid."));
                }
            } else {
                int hours = Integer.valueOf(value).intValue();

                if (hours > 999.5 || hours < 0) {
                    errors.add(new StateReportValidationError(studentName, field.getFieldId(),
                            "Query 8250Q Please check: Total Funded Spring Hours should be in the range 0.5 to 999.5, to the nearest .5.",
                            "Spring Founded Hours total is invalid."));
                }
            }

            return errors;
        }
    }

    /**
     * The Class ValidateHoursAtSetting.
     */
    protected class ValidateHoursAtSetting extends BeanValidator {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.BeanValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             X2BaseBean bean,
                                             FieldDefinition field,
                                             String value) {

            ArrayList<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            String studentName = ((SisStudent) bean).getNameView();

            /* Number has a decimal place */
            if (value.contains(".")) {
                int decimalSpot = value.indexOf(".");
                char firstDecimalValue = value.charAt(decimalSpot + 1);

                int springHours = Integer.valueOf(value.substring(0, decimalSpot)).intValue();

                /* Decimal values must be rounded to nearest half so can only be 0 or 5 */
                if ('5' != firstDecimalValue && '0' != firstDecimalValue) {
                    errors.add(new StateReportValidationError(studentName, field.getFieldId(),
                            "Error 8215 Child’s hours at the EY Setting not provided or out of the valid range (0 - 70) to the nearest.5 .",
                            "Hours at setting is invalid."));
                }
                if (springHours > 70 || springHours < 0) {
                    errors.add(new StateReportValidationError(studentName, field.getFieldId(),
                            "Error 8215 Child’s hours at the EY Setting not provided or out of the valid range (0 - 70) to the nearest.5 .",
                            "Hours at setting is invalid."));
                }
            } else {
                int springHours = Integer.valueOf(value).intValue();

                if (springHours > 70 || springHours < 0) {
                    errors.add(new StateReportValidationError(studentName, field.getFieldId(),
                            "Error 8215 Child’s hours at the EY Setting not provided or out of the valid range (0 - 70) to the nearest.5 .",
                            "Hours at setting is invalid."));
                }
            }

            return errors;
        }
    }

    /**
     * The Class ValidateAddressLineOne.
     */
    protected class ValidateAddressLineOne extends BeanValidator {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.BeanValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             X2BaseBean bean,
                                             FieldDefinition field,
                                             String value) {

            ArrayList<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            String studentName = ((SisStudent) bean).getNameView();

            String line1 = value;
            String line2 = data.getFieldValue(bean, "100129");
            String line3 = data.getFieldValue(bean, "100130");
            String line4 = data.getFieldValue(bean, "100131");
            String line5 = data.getFieldValue(bean, "100132");

            String remainderAddress = line2 + line3 + line4 + line5;

            /*
             * If the student had an address line one they must also have
             * information in one of the other address lines
             */
            if (!StringUtils.isEmpty(line1)) {
                if (StringUtils.isEmpty(remainderAddress)) {
                    errors.add(new StateReportValidationError(studentName, field.getFieldId(),
                            "Query 2400Q Where first line present, at least one other address line should also be present.",
                            "Address is invalid."));
                }
            }

            return errors;
        }
    }


    /**
     * The Class RetrieveStatistics.
     */
    protected class RetrieveStatistics extends BeanRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.BeanRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    X2BaseBean bean,
                                    FieldDefinition field) {
            String fieldId = field.getFieldId();
            Object result = null;

            if ("200629".equals(fieldId)) {
                result = Integer.valueOf(m_statsBag.getCount(ESTAB_TEACHING_TOTAL));
            } else if ("200344".equals(fieldId)) {
                result = Integer.valueOf(m_statsBag.getCount(ESTAB_QTS_TEACHERS));
            } else if ("200565".equals(fieldId)) {
                result = Integer.valueOf(m_statsBag.getCount(ESTAB_EYP_STEACHERS));
            } else if ("200345".equals(fieldId)) {
                result = Integer.valueOf(m_statsBag.getCount(EY_TEACHERS));
            } else if ("200346".equals(fieldId)) {
                result = Integer.valueOf(m_statsBag.getCount(EY_QTS_TEACHING_TOTAL));
            } else if ("200566".equals(fieldId)) {
                result = Integer.valueOf(m_statsBag.getCount(EY_EYPS_TEACHERS));
            } else if ("200633".equals(fieldId)) {
                result = Integer.valueOf(m_statsBag.getCount(EY2_YEARS));
            } else if ("200630".equals(fieldId)) {
                result = Integer.valueOf(m_statsBag.getCount(EY3_YEARS));
            } else if ("200631".equals(fieldId)) {
                result = Integer.valueOf(m_statsBag.getCount(EY4_YEARS));
            }

            return result;
        }
    }

    Bag m_statsBag = new HashBag();

    /**
     * Sets the elements.
     */
    /* Set new elements on template */
    public void setElements() {
        Element referenceDate = new Element("ReferenceDate");

        referenceDate.setText(m_censusDate.toString());
        setElement("ReferenceDateId", referenceDate);
    }

    /**
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        StateReportModel model = loadModel("CBDS");
        model.initializeRetriever("SYSTEM");

        Map<String, FieldRetriever> calcMap = model.getRetrievers();
        Map<String, FieldValidator> validatorMap = model.getValidators();
        calcMap.put("STATS", new RetrieveStatistics());

        m_censusDate = (PlainDate) getParameter(VAR_CENSUS_DATE);

        /* Add validation for fields in export */
        validatorMap.put("ESTTEACHTOTAL", new ValidateEstTeacherTotal());
        validatorMap.put("ESTQTSTOTAL", new ValidateEstQTSTeacherTotal());
        validatorMap.put("ESTEYPSTOTAL", new ValidateEstEYPSTeachers());
        validatorMap.put("HOURSOPEN", new ValidateHoursOpen());
        validatorMap.put("WEEKSSOPEN", new ValidateWeeksOpen());
        validatorMap.put("FUNDINGWEEKS", new ValidateFundingWeeks());
        validatorMap.put("EYSTAFFTOTAL", new ValidateEarlyYearTeachers());
        validatorMap.put("EYQTSTAFF", new ValidateEarlyYearQTSTeachers());
        validatorMap.put("EYEYPSSTAFF", new VaildateEarlyYearEYPTeachers());
        validatorMap.put("EY2TOTAL", new ValidateEarlyYear2Total());
        validatorMap.put("EY3TOTAL", new ValidateEarlyYear3Total());
        validatorMap.put("EY4TOTAL", new ValidateEarlyYear4Total());
        validatorMap.put("FUNDEDHOURS", new ValidateFundedHours());
        validatorMap.put("HOURSATSETTING", new ValidateHoursAtSetting());
        validatorMap.put("FUNDEDHOURS2", new ValidateSpringFundedHours());
        validatorMap.put("ADDRESSLINEONE", new ValidateAddressLineOne());

        super.addCalcs(calcMap);
        super.addValidators(validatorMap);

        setElements();

        SimpleDateFormat m_formatter = new SimpleDateFormat("yyyy-MM-dd");
        PlainDate censusDate = null;
        try {
            censusDate = new PlainDate(m_formatter.parse(m_censusDate.toString()));
        } catch (ParseException e) {
            addSetupError("ParseException", "Unable to set up the census date!");
            return;
        }

        StudentHistoryHelper m_helper = new StudentHistoryHelper(this);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_SNAPSHOT);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, censusDate);

        Collection<SisStudent> pupils = new ArrayList<SisStudent>();

        QueryByCriteria studentQuery = m_helper.getStudentQuery(false);
        Collection<SisStudent> students = getBroker().getCollectionByQuery(studentQuery);
        for (SisStudent student : students) {
            int ageAsOfCensusDate = student.getPerson().getAgeAsOfDate(censusDate);
            double fundedHours = 0.0;
            String fundedHoursAsString = (String) student.getFieldValueByAlias("DFE FUNDED HOURS");
            if (!StringUtils.isEmpty(fundedHoursAsString) && StringUtils.isNumeric(fundedHoursAsString)) {
                fundedHours = Double.valueOf(fundedHoursAsString).doubleValue();
            }

            // only funded, 3-4 year olds
            if (fundedHours > 0.0 && (ageAsOfCensusDate == 3 || ageAsOfCensusDate == 4)) {
                pupils.add(student);
            }

            // for <PupilChildStatistics>
            if (ageAsOfCensusDate == 2) {
                m_statsBag.add(EY2_YEARS);
            } else if (ageAsOfCensusDate == 3) {
                m_statsBag.add(EY3_YEARS);
            } else if (ageAsOfCensusDate == 4) {
                m_statsBag.add(EY4_YEARS);
            }

        }
        String activeCode =
                PreferenceManager.getPreferenceValue(getOrganization(), SystemPreferenceDefinition.STAFF_ACTIVE_CODE);
        String excludeStaffBeanPath = translateAliasToJavaName("DOE EXCLUDE STF", false);
        X2Criteria staffCriteria = new X2Criteria();
        staffCriteria.addEqualTo(SisStaff.COL_SCHOOL_OID, getSchool().getOid());
        staffCriteria.addEqualTo(SisStaff.COL_STATUS, activeCode);

        if (!StringUtils.isEmpty(excludeStaffBeanPath)) {
            staffCriteria.addNotEqualTo(excludeStaffBeanPath, BooleanAsStringConverter.TRUE);
        }

        BeanQuery staffQuery = new BeanQuery(SisStaff.class, staffCriteria);
        Collection<SisStaff> staffs = getBroker().getCollectionByQuery(staffQuery);

        for (SisStaff staff : staffs) {
            // total teaching total
            m_statsBag.add(ESTAB_TEACHING_TOTAL);

            if (staff.getFieldValueByAlias("EY STAFF") != null) {
                /* Staff is an early year teacher and EY totals should be set */
                if ("1".equals((staff.getFieldValueByAlias("EY STAFF")))) {

                    if ("1".equals(staff.getFieldValueByAlias("EYP STATUS"))) {
                        m_statsBag.add(EY_EYPS_TEACHERS);
                    }

                    if ("1".equals(staff.getFieldValueByAlias("DFE QT STATUS"))) {
                        m_statsBag.add(EY_QTS_TEACHING_TOTAL);
                    }

                    /* Add to early year total */
                    m_statsBag.add(EY_TEACHERS);
                }
            }

            if ("1".equals(staff.getFieldValueByAlias("EYP STATUS"))) {
                m_statsBag.add(ESTAB_EYP_STEACHERS);
            }

            if ("1".equals(staff.getFieldValueByAlias("DFE QT STATUS"))) {
                m_statsBag.add(ESTAB_QTS_TEACHERS);
            }
        }

        setCollectionToQuery("pupils", pupils);
    }
}
