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

package com.x2dev.procedures.statereporting.ca;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.Race;
import com.follett.fsc.core.k12.beans.Staff;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.SisDistrictSchoolYearContext;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.beans.StaffDegree;
import com.x2dev.sis.model.beans.StaffPosition;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.DateAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.text.SimpleDateFormat;
import java.util.*;
import org.apache.commons.httpclient.util.DateParseException;
import org.apache.commons.httpclient.util.DateUtil;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Export procedure for CA Staff Demographics File.
 *
 * @author X2 Development Corporation
 */
public class CAStaffDemographics extends StateReportData {
    /**
     * Entity class for Course Section export.
     *
     */
    public static class CAStaffDemographicsEntity extends StateReportEntity {
        CAStaffDemographics m_data;

        List<Race> m_races;
        SisStaff m_staff;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public CAStaffDemographicsEntity() {
            // Must have public no argument constructor
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            Staff staff = (SisStaff) getBean();
            String name = staff.getNameView() +
                    " [LASID: " + staff.getLocalId() +
                    ", SASID: " + staff.getStateId() +
                    "] ";
            return name;
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

            int rowCount = 0;

            m_data = (CAStaffDemographics) data;
            PlainDate startDate = m_data.m_schoolYearStartDate;
            PlainDate endDate = m_data.m_schoolYearEndDate;
            m_staff = (SisStaff) bean;
            Collection<Race> races = ((CAStaffDemographics) data).getRaces(m_staff);
            m_races = races == null ? new ArrayList() : new ArrayList(races);
            if (!m_races.isEmpty()) {
                Collections.sort(m_races, new Comparator<Race>() {

                    @Override
                    public int compare(Race o1, Race o2) {
                        return m_data.getRacePriority(o1) - m_data.getRacePriority(o2);
                    }

                });
            }
            String str = (String) bean.getFieldValueByBeanPath(m_data.m_fieldEmplEndDate);

            if (str == null && m_data.m_activeOnly && m_staff.getHireDate().before(endDate)) {
                rowCount = 1;
            } else if (str != null && m_data.m_activeOnly) {

                SimpleDateFormat df = new SimpleDateFormat("yyyy-MM-dd");
                PlainDate emplEndDate = null;
                try {
                    if (str != null) {
                        Date resultDate = df.parse(str);
                        emplEndDate = new PlainDate(resultDate);
                    }
                } catch (java.text.ParseException e) {
                    setRowCount(rowCount);
                }

                if (emplEndDate != null && startDate != null && endDate != null && emplEndDate.after(startDate) &&
                        emplEndDate.before(endDate)) {
                    rowCount = 1;
                }
            }

            else if (!m_data.m_activeOnly) {
                rowCount = 1;
            }
            setRowCount(rowCount);
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
     * Class to calculate report fields values.
     */
    protected class RetrieveReportValue implements FieldRetriever {
        DateAsStringConverter m_converter =
                (DateAsStringConverter) ConverterFactory.getConverterForClass(PlainDate.class.getName(),
                        Locale.getDefault(),
                        true);

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever
         *      #getFieldValue(StateReportData, StateReportEntity, FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {

            CAStaffDemographics report = (CAStaffDemographics) data;
            CAStaffDemographicsEntity myEntity = (CAStaffDemographicsEntity) entity;
            String param = (String) field.getParameter();
            if (param.equals(CAStaffDemographics.CALC_PARAM_EFFECTIVE_START_DATE)) {
                PlainDate dt1 = myEntity.m_staff.getHireDate();
                PlainDate dt2 = report.m_schoolYearStartDate;
                if (dt1 == null) {
                    dt1 = dt2;
                }
                if (dt2 == null) {
                    dt2 = dt1;
                }
                return dt1 != null ? (dt1.after(dt2) ? dt1 : dt2) : null;
            } else if (param.equals(CAStaffDemographics.CALC_PARAM_EFFECTIVE_END_DATE)) {
                return null;
            }

            else if (param.equals(CAStaffDemographics.CALC_PARAM_RACE_CODE)) {
                return getRaceCode(0, myEntity);
            } else if (param.equals(CAStaffDemographics.CALC_PARAM_RACE2_CODE)) {
                return getRaceCode(1, myEntity);
            } else if (param.equals(CAStaffDemographics.CALC_PARAM_RACE3_CODE)) {
                return getRaceCode(2, myEntity);
            } else if (param.equals(CAStaffDemographics.CALC_PARAM_RACE4_CODE)) {
                return getRaceCode(3, myEntity);
            } else if (param.equals(CAStaffDemographics.CALC_PARAM_RACE5_CODE)) {
                return getRaceCode(4, myEntity);
            } else if (param.equals(CAStaffDemographics.CALC_PARAM_DEGREE_TYPE)) {
                String highestDegreeCode = null;
                int highestDegreeValue = 0;
                Collection<StaffDegree> degrees = report.getDegrees(myEntity.m_staff);
                if (degrees != null) {
                    for (StaffDegree degree : degrees) {
                        String type = degree.getType();
                        if (!StringUtils.isEmpty(type)) {
                            String localCode = lookupReferenceCodeByBeanPath(StaffDegree.class,
                                    StaffDegree.COL_TYPE,
                                    type,
                                    ExportFormatField.ReferenceMapTypeCode.LOCAL.ordinal());
                            int degreeValue = StringUtils.isEmpty(localCode) ? 0 : Integer.parseInt(localCode);
                            String stateCode = lookupReferenceCodeByBeanPath(StaffDegree.class,
                                    StaffDegree.COL_TYPE,
                                    type,
                                    ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                            if (!StringUtils.isEmpty(stateCode) &&
                                    (highestDegreeCode == null || degreeValue > highestDegreeValue)) {
                                highestDegreeCode = stateCode;
                                highestDegreeValue = degreeValue;
                            }
                        }
                    }
                }
                return highestDegreeCode;
            } else if (param.equals(CAStaffDemographics.CALC_PARAM_LEA_BEGIN_DATE)) {
                return getYears(myEntity.m_staff, m_fieldLeaBeginDate);
            } else if (param.equals(CAStaffDemographics.CALC_PARAM_TOTAL_SVC_YEARS)) {
                int leaYears = getYears(myEntity.m_staff, m_fieldLeaBeginDate).intValue();

                int otherYears = 0;
                Object value = myEntity.m_staff.getFieldValueByAlias(ALIAS_OTHER_DISTRICT_YEARS);

                if (value != null) {
                    otherYears = Integer.parseInt((String) value);
                }
                return Integer.valueOf(leaYears + otherYears);
            } else if (param.equals(CAStaffDemographics.CALC_PARAM_EMPLOYMENT_END)) {
                PlainDate result = null;

                PlainDate endDate = (PlainDate) myEntity.m_staff.getFieldValueByAlias(ALIAS_STF_EMPLY_END_DATE);
                DistrictSchoolYearContext context = getCurrentContext();

                if (endDate != null && context != null) {
                    result = endDate.before(context.getEndDate()) ? endDate : null;
                }
                return result;
            }

            return null;
        }

        /**
         * calculates number of years up to value of date field.
         *
         * @param staff SisStaff
         * @param beanPath String
         * @return Integer
         */
        private Integer getYears(SisStaff staff, String beanPath) {
            int res = 1;
            Object value = staff.getFieldValueByBeanPath(beanPath);
            if (value != null) {
                PlainDate dt = (PlainDate) m_converter.parseSystemString(value.toString());
                Calendar cal1 = Calendar.getInstance();
                Calendar cal2 = Calendar.getInstance();
                cal1.setTime(dt);
                cal2.setTime(getCurrentContext().getEndDate());
                SimpleDateFormat format1 = new SimpleDateFormat("yyyy-MM-dd");
                format1.format(cal1.getTime());

                int diff = cal2.get(Calendar.YEAR) - cal1.get(Calendar.YEAR);

                cal2.set(Calendar.YEAR, cal1.get(Calendar.YEAR));
                if (cal2.before(cal1)) {
                    --diff;
                }
                if (diff >= 0) {
                    res = diff;
                }
            }
            return Integer.valueOf(res);
        }
    }

    /**
     * Custom validation procedures implementation.
     *
     * @author X2 Development Corporation
     */
    protected class ValidateReportValue implements FieldValidator {
        private static final String CALC_PARAM_EMPLOYMENT_STATUS = "EMPLOYMENT-STATUS";
        private static final String CALC_PARAM_GENDER_CODE = "GENDER-CODE";

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator
         *      #getFieldValidation(StateReportData, StateReportEntity, FieldDefinition, String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            String param = (String) field.getParameter();

            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            try {
                if (param.equals(CALC_PARAM_ALIAS_FIRST_NAME)) {
                    return validateAliasFirstName(entity, field, value);
                }

                if (param.equals(CALC_PARAM_ALIAS_LAST_NAME)) {
                    return validateAliasLastName(entity, field, value);
                }

                if (param.equals(CALC_PARAM_DOB)) {
                    return validatePersonDOB(entity, field, value);
                }

                if (param.equals(CALC_PARAM_HISPANIC_INDICATOR)) {
                    return validateEthnicityIndicator(entity, field, value);
                }

                if (param.equals(CALC_PARAM_MISSING_ETH)) {
                    return validateMissingEthnicityIndicator(entity, field, value);
                }

                if (param.equals(CALC_PARAM_RACE_CODE)
                        || param.equals(CALC_PARAM_RACE2_CODE)
                        || param.equals(CALC_PARAM_RACE3_CODE)
                        || param.equals(CALC_PARAM_RACE4_CODE)
                        || param.equals(CALC_PARAM_RACE5_CODE)) {
                    return validateRaceCode(entity, field, value);
                }

                if (param.equals(CALC_PARAM_EMPLOYMENT_START)) {
                    return validateEmploymentStartField(entity, field, value);
                }

                if (param.equals(CALC_PARAM_LEA_BEGIN_DATE)) {
                    return validateLeaBeginDate(entity, field, value);
                }

                if (param.equals(CALC_PARAM_TOTAL_SVC_YEARS)) {
                    return validateTotalServiceYears(entity, field, value);
                }

                if (param.equals(CALC_PARAM_DEGREE_TYPE)
                        || param.equals(CALC_PARAM_GENDER_CODE)
                        || param.equals(CALC_PARAM_EMPLOYMENT_STATUS)) {
                    return validateEmptyRequiredFields(entity, field, value);
                }

            } catch (Exception ex) {
                errors.add(new StateReportValidationError(entity, field,
                        "Exception in validation procedure",
                        "Exception is thrown for field " + field.getFieldId()));
            }

            return new ArrayList<StateReportValidationError>();
        }

        /**
         * Validate alias first name.
         *
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         */
        private Collection validateAliasFirstName(StateReportEntity entity, FieldDefinition field, String value) {
            CAStaffDemographicsEntity myEntity = (CAStaffDemographicsEntity) entity;
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            String aliasLastName = myEntity.getFieldValue(FIELD_INDEX_ALIAS_LAST_NAME);
            if (StringUtils.isEmpty(value) && !StringUtils.isEmpty(aliasLastName)) {
                errors.add(new StateReportValidationError(entity, field,
                        "Alias names",
                        "If Staff Alias Last Name is populated Then Staff Alias First Name must be populated"));
            }
            return errors;
        }

        /**
         * Validate alias last name.
         *
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         */
        private Collection validateAliasLastName(StateReportEntity entity, FieldDefinition field, String value) {
            CAStaffDemographicsEntity myEntity = (CAStaffDemographicsEntity) entity;
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            String aliasFirstName = myEntity.getFieldValue(FIELD_INDEX_ALIAS_FIRST_NAME);
            if (StringUtils.isEmpty(value) && !StringUtils.isEmpty(aliasFirstName)) {
                errors.add(new StateReportValidationError(entity, field,
                        "Alias names",
                        "If Staff Alias First Name is populated Then Staff Alias Last Name must be populated"));
            }
            return errors;
        }

        /**
         * Validate employment start field.
         *
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @throws DateParseException exception
         */
        private Collection validateEmploymentStartField(StateReportEntity entity, FieldDefinition field, String value)
                throws DateParseException {
            CAStaffDemographicsEntity myEntity = (CAStaffDemographicsEntity) entity;
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            if (StringUtils.isEmpty(value)) {
                errors.add(new StateReportValidationError(entity, field, "Value is required", "Value is required"));
                return errors;
            }

            String dobRaw = myEntity.getFieldValue(FIELD_INDEX_DOB);
            if (StringUtils.isEmpty(dobRaw)) {
                errors.add(
                        new StateReportValidationError(entity, field, "Value is required", "Person DOB is required"));
                return errors;
            }
            try {
                Date dob = DateUtil.parseDate(dobRaw, DATE_MASKS);
                Date employed = DateUtil.parseDate(value, DATE_MASKS);
                if (DateUtils.add(dob, Calendar.YEAR, 18).after(employed)) {
                    errors.add(new StateReportValidationError(entity, field,
                            "Staff Employment Start Date",
                            "Should be greater than Staff Birth Date plus 18"));
                }
                if (DateUtils.add(employed, Calendar.MONTH, 6).after(new Date())) {
                    errors.add(new StateReportValidationError(entity, field,
                            "Staff Employment Start Date",
                            "Must be less than or equal to current date plus six months"));
                }
            } catch (DateParseException ex) {
                errors.add(new StateReportValidationError(entity, field,
                        "Staff Employment Start Date",
                        "Staff DOB is in not right format. Employment Start date can't be validated."));
            }
            return errors;
        }

        /**
         * Validate empty required fields.
         *
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         */
        private Collection validateEmptyRequiredFields(StateReportEntity entity, FieldDefinition field, String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            if (StringUtils.isEmpty(value)) {
                errors.add(new StateReportValidationError(entity, field, "Value is required", "Value is required"));
            }
            return errors;
        }

        /**
         * Validate ethnicity indicator.
         *
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         */
        private Collection validateEthnicityIndicator(StateReportEntity entity, FieldDefinition field, String value) {
            CAStaffDemographicsEntity myEntity = (CAStaffDemographicsEntity) entity;
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            String missingEth = myEntity.getFieldValue(FIELD_INDEX_MISSING_ETH);
            if ("Y".equals(missingEth) && !StringUtils.isEmpty(value)) {
                errors.add(new StateReportValidationError(entity, field,
                        "Staff Hispanic Ethnicity Indicator",
                        "If Staff Ethnicity Missing Indicator is equal to Y Then Staff Hispanic Ethnicity Indicator must be blank"));
            }
            return errors;
        }

        /**
         * Validate lea begin date.
         *
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @throws DateParseException exception
         */
        private Collection validateLeaBeginDate(StateReportEntity entity, FieldDefinition field, String value)
                throws DateParseException {
            CAStaffDemographicsEntity myEntity = (CAStaffDemographicsEntity) entity;
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            if (StringUtils.isEmpty(value)) {
                errors.add(new StateReportValidationError(entity, field, "Value is required", "Value is required"));
                return errors;
            }
            String dobRaw = myEntity.getFieldValue(FIELD_INDEX_DOB);
            if (StringUtils.isEmpty(dobRaw)) {
                errors.add(
                        new StateReportValidationError(entity, field, "Value is required", "Person DOB is required"));
                return errors;
            }
            try {
                Date dob = DateUtil.parseDate(dobRaw, DATE_MASKS);
                int years = Integer.parseInt(value);
                int yearsTotal = Integer.parseInt(myEntity.getFieldValue(FIELD_INDEX_SERVICE_YEARS_TOTAL));
                int age = (int) Math.floor(DateUtils.getAgeInMonths(new PlainDate(dob), new PlainDate()) / 12);
                if (years > yearsTotal) {
                    errors.add(new StateReportValidationError(entity, field,
                            "Staff Service Years LEA",
                            "Must be less than or equal to Staff Service Years Total"));
                }
                if (years > age - 17) {
                    errors.add(new StateReportValidationError(entity, field,
                            "Staff Service Years LEA",
                            "Must be less than or equal to Staff age minus 17"));
                }
                if (years < 1) {
                    errors.add(new StateReportValidationError(entity, field,
                            "Staff Service Years LEA",
                            "Must be equal to or greater than 1"));
                }
            } catch (DateParseException ex) {
                errors.add(new StateReportValidationError(entity, field,
                        "Staff Employment Start Date",
                        "Staff DOB is in not right format. Employment Start date can't be validated."));
            }
            return errors;
        }

        /**
         * Validate missing ethnicity indicator.
         *
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         */
        private Collection validateMissingEthnicityIndicator(StateReportEntity entity,
                                                             FieldDefinition field,
                                                             String value) {
            CAStaffDemographicsEntity myEntity = (CAStaffDemographicsEntity) entity;
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            String hispanicInd = myEntity.getFieldValue(FIELD_INDEX_HISPANIC_INDICATOR);
            if (!StringUtils.isEmpty(hispanicInd) && !StringUtils.isEmpty(value) && !"N".equals(value)) {
                errors.add(new StateReportValidationError(entity, field,
                        "Staff Ethnicity Missing Indicator",
                        "If Staff Hispanic Ethnicity Indicator is populated Then Staff Ethnicity Missing Indicator must be equal to N or blank"));
            }
            return errors;
        }

        /**
         * Validate person DOB.
         *
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @throws DateParseException exception
         */
        private Collection validatePersonDOB(StateReportEntity entity, FieldDefinition field, String value)
                throws DateParseException {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            if (StringUtils.isEmpty(value)) {
                errors.add(new StateReportValidationError(entity, field, "Value is required", "Value is required"));
                return errors;
            }
            try {
                Date dob = DateUtil.parseDate(value, DATE_MASKS);

                Date now = new Date();
                if (now.before(DateUtils.add(dob, Calendar.YEAR, 18)) ||
                        now.after(DateUtils.add(dob, Calendar.YEAR, 95))) {
                    errors.add(new StateReportValidationError(entity, field,
                            "Staff Birth Date",
                            "Staff age should be greater than or equal to 18 and less than 95"));
                }
            } catch (DateParseException ex) {
                errors.add(new StateReportValidationError(entity, field,
                        "Staff Birth Date",
                        "Staff DOB is in not right format. Age can't be calculated"));
            }

            return errors;
        }

        /**
         * Validate race code.
         *
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         */
        private Collection validateRaceCode(StateReportEntity entity, FieldDefinition field, String value) {
            CAStaffDemographicsEntity myEntity = (CAStaffDemographicsEntity) entity;
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            if ("Y".equals(myEntity.getFieldValue(FIELD_INDEX_RACE_MISSING)) &&
                    !StringUtils.isEmpty(value)) {
                errors.add(new StateReportValidationError(entity, field,
                        "Staff Race Codes",
                        "If Staff Race Missing Indicator is equal to Y Then all of the Staff Race Codes must be blank"));
            }
            return errors;
        }

        /**
         * Validate total service years.
         *
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @throws DateParseException exception
         */
        private Collection validateTotalServiceYears(StateReportEntity entity, FieldDefinition field, String value)
                throws DateParseException {
            CAStaffDemographicsEntity myEntity = (CAStaffDemographicsEntity) entity;
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            if (StringUtils.isEmpty(value)) {
                errors.add(new StateReportValidationError(entity, field, "Value is required", "Value is required"));
                return errors;
            }
            if (value.equals("0")) {
                errors.add(new StateReportValidationError(entity, field, "Staff Service Years Total",
                        "Must be greater than or equal to 1"));
                return errors;
            }
            String start = (String) myEntity.getBean().getFieldValueByAlias(ALIAS_LEA_BEGIN_DATE);
            if (start == null || StringUtils.isEmpty(start)) {
                errors.add(new StateReportValidationError(entity, field, "Value is required",
                        "Service begin date is required"));
                return errors;
            }
            String dobRaw = myEntity.getFieldValue(FIELD_INDEX_DOB);
            if (StringUtils.isEmpty(dobRaw)) {
                errors.add(
                        new StateReportValidationError(entity, field, "Value is required", "Person DOB is required"));
                return errors;
            }
            Date dob = DateUtil.parseDate(dobRaw, DATE_MASKS);

            int years = Integer.parseInt(value);
            int age = (int) Math.floor(DateUtils.getAgeInMonths(new PlainDate(dob), new PlainDate()) / 12);

            if (years > age - 17) {
                errors.add(new StateReportValidationError(entity, field,
                        "Staff Service Years Total",
                        "Must be less than or equal to Staff age minus 17"));
            }
            if (years < 1) {
                errors.add(new StateReportValidationError(entity, field,
                        "Staff Service Years Total",
                        "Must be equal to or greater than 1"));
            }
            return errors;
        }
    }

    // declare constants
    public static final Object CALC_PARAM_EFFECTIVE_END_DATE = "EFFECTIVE-END-DATE";

    static final Collection DATE_MASKS = Arrays.asList("yyyyMMdd");

    protected static final String ALIAS_EMPLOY_END_DATE = "DOE STF EMPLOY END DATE";
    protected static final String ALIAS_EXCLUDE = "DOE EXCLUDE STF";
    protected static final String ALIAS_EXCLUDE_SCHOOL = "DOE EXCLUDE SKL";
    protected static final String ALIAS_LEA_BEGIN_DATE = "DOE SERVICE LEA BEGIN DATE";
    protected static final String ALIAS_OTHER_DISTRICT_YEARS = "DOE OTHER DISTRICT YEARS";
    protected static final String ALIAS_RACE_PRIORITY = "race-priority";
    protected static final String ALIAS_STF_EMPLY_END_DATE = "DOE STF EMPLY END DATE";
    protected static final String ALIAS_TOTAL_BEGIN_DATE = "DOE SERVICE TOTAL BEGIN DATE";

    protected static final String INPUT_PARAM_ALL_SCHOOLS = "allSchools";
    protected static final String INPUT_PARAM_SCHOOLS = "schoolOids";

    private static final String CALC_PARAM_ALIAS_FIRST_NAME = "ALIAS-FIRST-NAME";
    private static final String CALC_PARAM_ALIAS_LAST_NAME = "ALIAS-LAST-NAME";
    private static final String CALC_PARAM_DEGREE_TYPE = "DEGREE-TYPE";
    private static final String CALC_PARAM_DOB = "DOB";

    private static final String CALC_PARAM_EFFECTIVE_START_DATE = "EFFECTIVE-START-DATE";
    private static final String CALC_PARAM_EMPLOYMENT_START = "EMPLOYMENT-START";
    private static final String CALC_PARAM_EMPLOYMENT_END = "EMPLOYMENT-END";
    private static final String CALC_PARAM_HISPANIC_INDICATOR = "HISPANIC-INDICATOR";
    private static final String CALC_PARAM_LEA_BEGIN_DATE = "LEA-BEGIN-DATE";
    private static final String CALC_PARAM_MISSING_ETH = "MISSING-ETHNICITY";
    private static final String CALC_PARAM_RACE_CODE = "RACE-CODE";
    private static final String CALC_PARAM_RACE2_CODE = "RACE2-CODE";
    private static final String CALC_PARAM_RACE3_CODE = "RACE3-CODE";
    private static final String CALC_PARAM_RACE4_CODE = "RACE4-CODE";
    private static final String CALC_PARAM_RACE5_CODE = "RACE5-CODE";
    private static final String CALC_PARAM_TOTAL_SVC_YEARS = "TOTAL-SVC-YEARS";
    private static final String CALCULATION_ID = "EXPDATA-CA-SDE";
    private static final int FIELD_INDEX_ALIAS_FIRST_NAME = 12;

    private static final int FIELD_INDEX_ALIAS_LAST_NAME = 14;
    private static final int FIELD_INDEX_DOB = 15;
    private static final int FIELD_INDEX_HISPANIC_INDICATOR = 17;
    private static final int FIELD_INDEX_MISSING_ETH = 18;
    private static final int FIELD_INDEX_RACE_MISSING = 24;
    private static final int FIELD_INDEX_SERVICE_YEARS_TOTAL = 30;
    private static final String VALIDATION_ID = "EXP-CA-SDE-VAL";



    private static final String INPUT_PARAM_ACTIVE_ONLY = "activeOnly";

    boolean m_activeOnly;
    Map<Integer, SisDistrictSchoolYearContext> m_ctxCashe;
    PlainDate m_schoolYearStartDate;
    PlainDate m_schoolYearEndDate;

    protected String m_fieldEmplEndDate;
    protected String m_fieldExcludeSchool;
    protected String m_fieldLeaBeginDate;
    protected String m_fieldOverallBeginDate;
    protected String m_fieldRacePriority;

    protected Map<String, Collection<Race>> m_personRaceMap;

    protected Map<String, Collection<StaffDegree>> m_staffDegreesMap;
    private Map<String, Collection<StaffPosition>> m_staffPositionsMap;



    /**
     * Returns a list of Person Race records for the requested student. If no race codes
     * are found, returns null.
     *
     * @param staff SisStaff
     * @return Collection<Race>
     */
    Collection<StaffDegree> getDegrees(SisStaff staff) {
        return m_staffDegreesMap.get(staff.getOid());
    }

    /**
     * Returns a list of Staff Position records for the requested staff.
     *
     * @param staff SisStaff
     * @return Collection<StaffPosition>
     */
    Collection<StaffPosition> getPositions(SisStaff staff) {
        return m_staffPositionsMap.get(staff.getOid());
    }

    /**
     * look for race code.
     *
     * @param index int
     * @param myEntity CAStaffDemographicsEntity
     * @return String
     */
    String getRaceCode(int index, CAStaffDemographicsEntity myEntity) {
        String res = "";
        if (myEntity.m_races.size() > index) {
            String key = myEntity.m_races.get(index).getRaceCode();
            if (key != null) {
                res = lookupReferenceCodeByBeanPath(Race.class, Race.COL_RACE_CODE, key,
                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
            }
        }
        return res;
    }

    /**
     * Get the integer priority for this race or 0 if not defined.
     *
     * @param race Race
     * @return priority
     */
    int getRacePriority(Race race) {
        int value = 0;
        if (m_fieldRacePriority != null) {
            String priority = (String) race.getFieldValueByBeanPath(m_fieldRacePriority);
            try {
                value = Integer.parseInt(priority);
            } catch (NumberFormatException e) {
                // retain 0 if format exception
            }
        }
        return value;
    }

    /**
     * Returns a list of Person Race records for the requested student. If no race codes
     * are found, returns null.
     *
     * @param staff SisStaff
     * @return Collection<Race>
     */
    Collection<Race> getRaces(SisStaff staff) {
        return m_personRaceMap.get(staff.getPersonOid());
    }

    /**
     * Initialize the data module.
     * Initialize necessary working resources.
     * Define query
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(java.util.Map,
     *      com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    protected void initialize() throws X2BaseException {
        m_ctxCashe = new HashMap();
        // Setup OK
        m_fieldExcludeSchool = translateAliasToJavaName(ALIAS_EXCLUDE_SCHOOL, true);
        m_fieldLeaBeginDate = translateAliasToJavaName(ALIAS_LEA_BEGIN_DATE, true);
        m_fieldOverallBeginDate = translateAliasToJavaName(ALIAS_TOTAL_BEGIN_DATE, true);
        m_fieldEmplEndDate = translateAliasToJavaName(ALIAS_EMPLOY_END_DATE, true);
        m_fieldRacePriority = translateAliasToJavaName(ALIAS_RACE_PRIORITY, false);

        if (this.getSetupErrors().size() == 0) {
            DistrictSchoolYearContext ctx = getCurrentContext();
            if (ctx != null) {
                m_schoolYearStartDate = ctx.getStartDate();
                m_schoolYearEndDate = ctx.getEndDate() != null ? ctx.getEndDate() : new PlainDate();
            }

            // Build query
            X2Criteria criteria = new X2Criteria();

            criteria.addNotEmpty(SisStaff.COL_STATE_ID, getBroker().getPersistenceKey());

            m_activeOnly = ((Boolean) getParameter(INPUT_PARAM_ACTIVE_ONLY)).booleanValue();

            if (m_activeOnly) {
                String activeCode = PreferenceManager.getPreferenceValue(getOrganization(),
                        SystemPreferenceDefinition.STAFF_ACTIVE_CODE);
                criteria.addEqualTo(SisStaff.COL_STATUS, activeCode);
            }

            String schoolOids = (String) getParameter(INPUT_PARAM_SCHOOLS);
            Boolean isAllSchools = (Boolean) getParameter(INPUT_PARAM_ALL_SCHOOLS);
            X2Criteria sklCriteria = new X2Criteria();

            if (isAllSchools.booleanValue()) {
                sklCriteria.addNotEqualTo(SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
                sklCriteria.addNotEqualTo(SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
            } else {
                Set<String> setSchoolOids = new HashSet<String>();
                setSchoolOids.addAll(Arrays.asList(schoolOids.split(",")));
                sklCriteria.addIn(X2BaseBean.COL_OID, setSchoolOids);
            }

            sklCriteria.addNotEqualTo(m_fieldExcludeSchool, BooleanAsStringConverter.TRUE);

            SubQuery sklSubQuery = new SubQuery(SisSchool.class, X2BaseBean.COL_OID, sklCriteria);
            criteria.addIn(SisStaff.COL_SCHOOL_OID, sklSubQuery);

            addStandardExcludesCriteria(criteria, ALIAS_EXCLUDE);

            this.applyInputCriteria(criteria, false, null);

            initializeDegrees(criteria);
            initializeRaces(criteria);
            initializeStaffPositions(criteria);
            // create query - use the appropriate class
            QueryByCriteria query = new QueryByCriteria(SisStaff.class, criteria);

            this.applyInputSort(query, null);
            this.setQuery(query);

            // Set Custom Entity
            this.setEntityClass(CAStaffDemographicsEntity.class);

            // Build and attach retrievers
            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put(CALCULATION_ID, new RetrieveReportValue());
            super.addCalcs(calcs);

            HashMap validators = new HashMap<String, FieldValidator>();
            validators.put(VALIDATION_ID, new ValidateReportValue());
            super.addValidators(validators);

        }
    }

    /**
     * Helper method to apply standard excludes to criteria.
     *
     * @param criteria X2Criteria
     * @param alias String
     */
    private void addStandardExcludesCriteria(X2Criteria criteria, String alias) {
        String fieldExclude = translateAliasToJavaName(alias, false);
        if (!StringUtils.isEmpty(fieldExclude)) {
            criteria.addNotEqualTo(fieldExclude, BooleanAsStringConverter.TRUE);
        }
    }

    /**
     * Initializes the degrees map for the staff in the current selection.
     *
     * @param staffCriteria X2Criteria
     */
    private void initializeDegrees(X2Criteria staffCriteria) {
        SubQuery staffQuery = new SubQuery(SisStaff.class, X2BaseBean.COL_OID, staffCriteria);
        X2Criteria degreeCriteria = new X2Criteria();
        degreeCriteria.addIn(StaffDegree.COL_STAFF_OID, staffQuery);
        QueryByCriteria degreeQuery = new QueryByCriteria(StaffDegree.class, degreeCriteria);
        degreeQuery.addOrderBy(StaffDegree.COL_STAFF_OID, true);
        m_staffDegreesMap = getBroker().getGroupedCollectionByQuery(degreeQuery, StaffDegree.COL_STAFF_OID, 500);
    }

    /**
     * Initializes the positions map for the staff in the current selection.
     *
     * @param staffCriteria X2Criteria
     */
    private void initializeStaffPositions(X2Criteria staffCriteria) {
        SubQuery staffQuery = new SubQuery(SisStaff.class, X2BaseBean.COL_OID, staffCriteria);
        X2Criteria staffPosCriteria = new X2Criteria();
        staffPosCriteria.addIn(StaffPosition.COL_STAFF_OID, staffQuery);
        QueryByCriteria positionsQuery = new QueryByCriteria(StaffPosition.class, staffPosCriteria);
        positionsQuery.addOrderBy(StaffPosition.COL_STAFF_OID, true);
        m_staffPositionsMap = getBroker().getGroupedCollectionByQuery(positionsQuery, StaffPosition.COL_STAFF_OID, 500);
    }

    /**
     * Initializes the races map for the staff in the current selection.
     *
     * @param staffCriteria X2Criteria
     */
    private void initializeRaces(X2Criteria staffCriteria) {
        SubQuery staffQuery = new SubQuery(SisStaff.class, SisStaff.COL_PERSON_OID, staffCriteria);
        X2Criteria raceCriteria = new X2Criteria();
        raceCriteria.addIn(Race.COL_PERSON_OID, staffQuery);
        QueryByCriteria raceQuery = new QueryByCriteria(Race.class, raceCriteria);
        raceQuery.addOrderBy(Race.COL_PERSON_OID, true);
        raceQuery.addOrderBy(Race.COL_RACE_CODE, true);
        m_personRaceMap = getBroker().getGroupedCollectionByQuery(raceQuery, Race.COL_PERSON_OID, 500);
    }
}
