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

package com.x2dev.procedures.statereporting.tn;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.SchoolCalendar;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.Course;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.ScheduleTermDate;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Class for TN 030 Class Section export.
 */
public class TNClassSectionData extends TNStateReportData {
    private static final String NO_PERFMON4J_INSTRUMENTATION = "";

    /**
     * Entity class for Class Section export.
     *
     */
    public static class TNClassSectionEntity extends TNStateReportEntity {
        private List<MasterSchedule> m_sections;

        /**
         * Instantiates a new TN class section entity.
         */
        public TNClassSectionEntity() {
            // Public no argument constructor for dynamic instantiation.
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            SisSchool school = (SisSchool) getBean();
            MasterSchedule section = getSection();
            String name = school.getName() + " - " + section.getCourseView();
            return name;
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            TNClassSectionData tnData = (TNClassSectionData) data;

            Collection<String> schoolOids = tnData.m_classSectionHelper.getSchoolOidsCourse();

            if (schoolOids == null || schoolOids.isEmpty() || schoolOids.contains(bean.getOid())) {
                m_sections = new ArrayList<>(tnData.m_classSectionHelper.getReportableSectionForSchool(bean.getOid()));

                setRowCount(m_sections.size());
            } else {
                setRowCount(0);
            }

            tnData.addEntityRowsCount(getRowCount());
        }

        /**
         * Gets the section.
         *
         * @return Master schedule
         */
        protected MasterSchedule getSection() {
            return m_sections.get(getCurrentRow());
        }

    }

    /**
     * Field retriever for Class begin and end dates.
     */
    protected class RetrieveBeanPath implements FieldRetriever {
        static final String ALIAS_EXPRESSION = ".*\\[(.*)\\].*";
        Pattern m_aliasPattern;

        /**
         * Instantiates a new retrieve bean path.
         */
        public RetrieveBeanPath() {
            m_aliasPattern = Pattern.compile(ALIAS_EXPRESSION);
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity reportEntity, FieldDefinition field)
                throws X2BaseException {
            MasterSchedule section = ((TNClassSectionEntity) reportEntity).getSection();
            String beanPath = (String) field.getParameter();
            Matcher matcher = m_aliasPattern.matcher(beanPath);
            if (matcher.find()) {
                String alias = matcher.group(1);
                String path = data.translateAliasToJavaName(alias, false);
                if (!StringUtils.isEmpty(path)) {
                    beanPath = beanPath.replace("[" + alias + "]", path);
                }
            }
            return getProperty(section, beanPath);
        }
    }

    /**
     * Field retriever for Class begin and end dates.
     */
    protected class RetrieveCdates implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity reportEntity, FieldDefinition field) {
            MasterSchedule section = ((TNClassSectionEntity) reportEntity).getSection();
            Collection<ScheduleTermDate> trmDates =
                    section.getScheduleTerm() == null ? null : section.getScheduleTerm().getScheduleTermDates();

            String param = (String) field.getParameter();

            PlainDate startDate = null;
            PlainDate endDate = null;
            if (trmDates != null) {
                for (ScheduleTermDate trmDate : trmDates) {
                    if (startDate == null || startDate.after(trmDate.getStartDate())) {
                        startDate = trmDate.getStartDate();
                    }
                    if (endDate == null || endDate.before(trmDate.getEndDate())) {
                        endDate = trmDate.getEndDate();
                    }
                }
            }

            if (param.equalsIgnoreCase(CALC_PARAM_CLASSBEGINDATE)) {
                return startDate;
            }

            if (param.equalsIgnoreCase(CALC_PARAM_CLASSENDDATE)) {
                return endDate;
            }
            return "";
        }
    }

    /**
     * Field retriever for instructional program and school year.
     */
    protected class RetrieveDefault implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity reportEntity, FieldDefinition field) {
            TNClassSectionData seData = (TNClassSectionData) data;

            String param = (String) field.getParameter();

            if (param.equalsIgnoreCase(CALC_PARAM_SCHOOLYEAR)) {
                return seData.m_schoolYear;
            } else if (param.equalsIgnoreCase(CALC_PARAM_LOCALCNUM)) {
                MasterSchedule section = ((TNClassSectionEntity) reportEntity).getSection();
                return section.getCourseView().replace("-", "");
            } else if (param.equalsIgnoreCase(CALC_PARAM_SERVDISTID)) {
                MasterSchedule section = ((TNClassSectionEntity) reportEntity).getSection();
                Course crs = section.getSchoolCourse().getCourse();
                String servDist = (String) crs.getFieldValueByAlias(ALIAS_SERV_DIST_ID);
                if (StringUtils.isEmpty(servDist)) {
                    servDist = crs.getOrganization1().getId();
                }
                return servDist;
            } else if (param.equalsIgnoreCase(CALC_PARAM_SERVSKLID)) {
                MasterSchedule section = ((TNClassSectionEntity) reportEntity).getSection();
                Course crs = section.getSchoolCourse().getCourse();
                String servSchool = (String) crs.getFieldValueByAlias(ALIAS_SERV_SKL_ID);
                if (StringUtils.isEmpty(servSchool)) {
                    servSchool =
                            (String) section.getSchoolCourse().getSchool()
                                    .getFieldValueByBeanPath(m_fieldStateSchoolId);
                }
                return servSchool;
            }

            return "";
        }
    }

    /**
     * Retrieve Dual Enrollment/Credit for export.
     * If State Value = Enroll populate Dual Enrollment = Y. other two N
     * If State Value = Local populate Local Dual Credit = Y, other two N
     * If State Value = State populate State Dual Credit = Y, other two N
     */
    protected class RetrieveDualCredit implements FieldRetriever {
        private String CALC_PARAM_DUAL = "DUAL";
        private String CALC_PARAM_LOCAL = "LOCAL";
        private String CALC_PARAM_STATE = "STATE";
        private String STATE_CODE_DUAL = "Enroll";
        private String STATE_CODE_LOCAL = "Local";
        private String STATE_CODE_STATE = "State";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String param = (String) field.getParameter();
            MasterSchedule mst = ((TNClassSectionEntity) entity).getSection();
            String dualCreditState = null;
            String dualCredit = (String) mst.getFieldValueByBeanPath(m_fieldDualCreditMst);

            if (!StringUtils.isEmpty(dualCredit)) {
                dualCreditState = lookupStateValue(MasterSchedule.class, m_fieldDualCreditMst, dualCredit);
            } else {
                dualCredit = (String) mst.getSchoolCourse().getFieldValueByBeanPath(m_fieldDualCreditCsk);

                if (!StringUtils.isEmpty(dualCredit)) {
                    dualCreditState = lookupStateValue(SchoolCourse.class, m_fieldDualCreditCsk, dualCredit);
                } else {
                    dualCredit =
                            (String) mst.getSchoolCourse().getCourse().getFieldValueByBeanPath(m_fieldDualCreditCrs);
                    if (!StringUtils.isEmpty(dualCredit)) {
                        dualCreditState = lookupStateValue(Course.class, m_fieldDualCreditCrs, dualCredit);
                    }
                }
            }

            if (!StringUtils.isEmpty(dualCreditState)) {
                if (CALC_PARAM_DUAL.equals(param) && STATE_CODE_DUAL.equals(dualCreditState)) {
                    return VALUE_YES;
                } else if (CALC_PARAM_LOCAL.equals(param) && STATE_CODE_LOCAL.equals(dualCreditState)) {
                    return VALUE_YES;
                } else if (CALC_PARAM_STATE.equals(param) && STATE_CODE_STATE.equals(dualCreditState)) {
                    return VALUE_YES;
                }
            }
            return VALUE_NO;
        }
    }

    /**
     * Field retriever for Instructional program field.
     * Can be used only with SisStudent beans.
     */
    protected class RetrieveInstProgram implements FieldRetriever {
        public static final String TN_CALC_INSTPGM_ID = "TN_INSTRPGM_MST";
        private static final String INSTPGM_EMPTY_ERROR = "Instructional Program is empty.";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            TNClassSectionData tnData = (TNClassSectionData) data;

            MasterSchedule section = ((TNClassSectionEntity) entity).getSection();
            SisSchool school = section.getSchedule().getSchool();
            String sectionOid = section.getOid();

            String instPgm = (tnData.m_classSectionHelper.m_instProgMap.containsKey(sectionOid))
                    ? tnData.m_classSectionHelper.m_instProgMap.get(sectionOid)
                    : tnData.m_classSectionHelper.m_defaultInstProgMap.get(school.getOid());
            if (!StringUtils.isEmpty(instPgm)) {
                return instPgm;
            }
            String calendarCode = tnData.m_classSectionHelper.m_sectionToCalendarCode.get(section.getOid());
            if (calendarCode == null) {
                calendarCode = EMPTY_STRING;
            }
            String message = "Value not specified. Set to " + field.getDefaultValue() + " by default. Context:" +
                    tnData.getCurrentContext().getContextId() + ". School:" + school.getName() +
                    ". Calendar code:" + calendarCode;
            StateReportValidationError error =
                    new StateReportValidationError(entity, field, INSTPGM_EMPTY_ERROR, message);
            entity.addRetrievalError(field.getFieldId(), error);
            return null;
        }
    }

    /**
     * Retrieve Post Secondary Institution ID.
     */
    protected class RetrievePostSecInstitution implements FieldRetriever {

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
            MasterSchedule mst = ((TNClassSectionEntity) entity).getSection();
            String value = null;
            String secInstitutionId = (String) mst.getFieldValueByBeanPath(m_fieldPostInstMst);

            if (!StringUtils.isEmpty(secInstitutionId)) {
                value = lookupStateValue(MasterSchedule.class, m_fieldPostInstMst, secInstitutionId);
            } else {
                secInstitutionId = (String) mst.getSchoolCourse().getFieldValueByBeanPath(m_fieldPostInstCsk);

                if (!StringUtils.isEmpty(secInstitutionId)) {
                    value = lookupStateValue(SchoolCourse.class, m_fieldPostInstCsk, secInstitutionId);
                } else {
                    secInstitutionId =
                            (String) mst.getSchoolCourse().getCourse().getFieldValueByBeanPath(m_fieldPostInstCrs);
                    if (!StringUtils.isEmpty(secInstitutionId)) {
                        value = lookupStateValue(Course.class, m_fieldPostInstCrs, secInstitutionId);
                    }
                }
            }

            return value;
        }
    }

    /**
     * Retriever to get values for fields: Class Type, Teaching Method, and Honors Class
     * First check mst values. They override course values.
     */
    protected class RetrieveSectionLevelOverValues implements FieldRetriever {
        public static final String MST_CALC_ID = "MST_OVERRIDDEN";
        private static final String PARAM_CLASS_TYPE = "CLASS_TYPE";
        private static final String PARAM_TEACHING_METHOD = "TEACHING_METHOD";
        private static final String PARAM_HONORS = "HONORS";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String param = (String) field.getParameter();
            MasterSchedule section = ((TNClassSectionEntity) entity).getSection();

            Object value = null;
            if (PARAM_CLASS_TYPE.equals(param)) {
                value = data.lookupStateValue(MasterSchedule.class, m_fieldClassTypeOver,
                        (String) section.getFieldValueByBeanPath(m_fieldClassTypeOver));
                if (StringUtils.isEmpty((String) value)) {
                    String courseValue =
                            (String) section.getSchoolCourse().getCourse().getFieldValueByBeanPath(m_fieldClassType);
                    value = data.lookupStateValue(Course.class, m_fieldClassType, courseValue);
                }
            } else if (PARAM_HONORS.equals(param)) {
                value = section.getFieldValueByBeanPath(m_fieldHonorsIndOver);
                if (!BooleanAsStringConverter.TRUE.equals(value)) {
                    value = section.getSchoolCourse().getCourse().getFieldValueByBeanPath(m_fieldHonorsInd);
                }
                value = BooleanAsStringConverter.TRUE.equals(value) ? Boolean.TRUE : Boolean.FALSE;
            } else if (PARAM_TEACHING_METHOD.equals(param)) {
                value = data.lookupStateValue(MasterSchedule.class, m_fieldTeachingMethodOver,
                        (String) section.getFieldValueByBeanPath(m_fieldTeachingMethodOver));
                if (StringUtils.isEmpty((String) value)) {
                    String courseValue = (String) section.getSchoolCourse().getCourse()
                            .getFieldValueByBeanPath(m_fieldTeachingMethod);
                    value = data.lookupStateValue(Course.class, m_fieldTeachingMethod, courseValue);
                }
            }
            return value;
        }

    }

    /**
     * Field retriever for Test Admin Window.
     */
    protected class RetrieveTestAdmin implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity reportEntity, FieldDefinition field)
                throws X2BaseException {
            MasterSchedule section = ((TNClassSectionEntity) reportEntity).getSection();
            String value = (String) section.getFieldValueByBeanPath(m_fieldTestAdmin);
            if (!StringUtils.isEmpty(value)) {
                value = lookupReferenceCodeByBeanPath(MasterSchedule.class, m_fieldTestAdmin, value,
                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
            }
            return value;
        }
    }

    /**
     * Value must represent if state value for dual credit exist.
     */
    protected class ValidateInstitution implements FieldValidator {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection errors = new ArrayList<StateReportValidationError>();
            MasterSchedule mst = ((TNClassSectionEntity) entity).getSection();
            String dualCreditState = null;

            String dualCredit = (String) mst.getFieldValueByBeanPath(m_fieldDualCreditMst);
            if (!StringUtils.isEmpty(dualCredit)) {
                dualCreditState = lookupStateValue(MasterSchedule.class, m_fieldDualCreditMst, dualCredit);
            } else {
                dualCredit = (String) mst.getSchoolCourse().getFieldValueByBeanPath(m_fieldDualCreditCsk);
                if (!StringUtils.isEmpty(dualCredit)) {
                    dualCreditState = lookupStateValue(SchoolCourse.class, m_fieldDualCreditCsk, dualCredit);
                } else {
                    dualCredit =
                            (String) mst.getSchoolCourse().getCourse().getFieldValueByBeanPath(m_fieldDualCreditCrs);
                    if (!StringUtils.isEmpty(dualCredit)) {
                        dualCreditState = lookupStateValue(Course.class, m_fieldDualCreditCrs, dualCredit);
                    }
                }
            }

            if (!StringUtils.isEmpty(dualCreditState) && StringUtils.isEmpty(value)) {
                String errorMessage = field.getFieldId() +
                        " must exist then Dual Enrollment/Credit state value is not empty. Dual Credit state value: " +
                        dualCreditState;
                StateReportValidationError error =
                        new StateReportValidationError(entity, field, field.getFieldId() + " is empty.", errorMessage);
                errors.add(error);
            } else if (StringUtils.isEmpty(dualCreditState) && !StringUtils.isEmpty(value)) {
                String errorMessage =
                        field.getFieldId() + " should not exist then Dual Enrollment/Credit state value is empty. " +
                                field.getFieldId() + ": " + value;
                StateReportValidationError error = new StateReportValidationError(entity, field,
                        field.getFieldId() + " is not empty.", errorMessage);
                errors.add(error);
            }
            return errors;
        }

    }

    /**
     * Add error if a code present that does not exist in Reference table.
     */
    protected class ValidateTestAdmin implements FieldValidator {
        Collection<String> m_referenceCodes = null;

        /**
         * Instantiates a new validate test admin.
         */
        public ValidateTestAdmin() {
            DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
            DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(ALIAS_TEST_ADMIN_WINDOW);
            String refTableOId = field.getReferenceTableOid();

            if (!StringUtils.isEmpty(refTableOId)) {
                Map<String, ReferenceCode> refCodesMap = getReferenceCodes(refTableOId);
                m_referenceCodes = refCodesMap.keySet();
            } else {
                String errorMessage =
                        "There is no Reference Table assigned to field with alias [" + ALIAS_TEST_ADMIN_WINDOW + "].";
                String errorType = "Reference Table error";

                addSetupError(errorType, errorMessage);
            }
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection errors = new ArrayList<StateReportValidationError>();
            MasterSchedule mst = ((TNClassSectionEntity) entity).getSection();
            String notMappedValue = (String) mst.getFieldValueByAlias(ALIAS_TEST_ADMIN_WINDOW);
            if (!StringUtils.isEmpty(notMappedValue) &&
                    m_referenceCodes != null && !m_referenceCodes.contains(notMappedValue)) {
                String errorMessage = field.getFieldId() +
                        ": there is no code " + notMappedValue + " in Reference Table by alias ["
                        + ALIAS_TEST_ADMIN_WINDOW + "].";

                StateReportValidationError error = new StateReportValidationError(entity, field, field.getFieldId() +
                        " code error.", errorMessage);
                errors.add(error);
            }

            return errors;
        }

    }

    /**
     * Constants: Aliases, Fields, IDs, Parameters
     */
    protected static final String ALIAS_CLASS_TYPE = "DOE CLASS TYPE";
    protected static final String ALIAS_CLASS_TYPE_OVER = "DOE CLASS TYPE OVERRIDE";
    protected static final String ALIAS_DUAL_CREDIT_CRS = "all-crs-DualEnrollmentCredit";
    protected static final String ALIAS_DUAL_CREDIT_CSK = "all-csk-DualEnrollmentCredit";
    protected static final String ALIAS_DUAL_CREDIT_MST = "all-mst-DualEnrollmentCredit";
    protected static final String ALIAS_EXCLUDE_CRS = "DOE EXCLUDE CRS";
    protected static final String ALIAS_HONORS_IND = "DOE HONORS";
    protected static final String ALIAS_HONORS_IND_OVER = "DOE HONORS IND OVERRIDE";
    protected static final String ALIAS_POST_INST_MST = "all-mst-PSInstitutionId";
    protected static final String ALIAS_POST_INST_CRS = "all-crs-PSInstitutionId";
    protected static final String ALIAS_POST_INST_CSK = "all-csk-PSInstitutionId";
    protected static final String ALIAS_SERV_DIST_ID = "DOE CRS SERVICE DISTRICT ID";
    protected static final String ALIAS_SERV_SKL_ID = "DOE CRS SERVICE SCHOOL ID";
    protected static final String ALIAS_TEACHING_METHOD = "DOE TEACHING METHOD";
    protected static final String ALIAS_TEACHING_METHOD_OVER = "DOE TEACHING METHOD OVERRIDE";
    protected static final String ALIAS_TEST_ADMIN_WINDOW = "all-mst-TestAdminWindow";

    protected static final String CALC_PARAM_CLASSBEGINDATE = "CLASSBEGINDATE";
    protected static final String CALC_PARAM_CLASSENDDATE = "CLASSENDDATE";
    protected static final String CALC_PARAM_LOCALCNUM = "LOCALCNUM";
    protected static final String CALC_PARAM_SCHOOLYEAR = "SCHOOLYEAR";
    protected static final String CALC_PARAM_SERVDISTID = "SERVDISTID";
    protected static final String CALC_PARAM_SERVSKLID = "SERVSKLID";

    private static final String CALC_ID_BEAN_PATH = "MST_BEAN_PATH";
    private static final String CALC_ID_CDATES = "MST_CALC_CDATES";
    private static final String CALC_ID_DEFAULT = "MST_CALC_DEFAULT";
    private static final String CALC_ID_DCREDIT = "MST_CALC_DCREDIT";
    private static final Object CALC_ID_POST_INST = "MST_POST_INST";
    private static final String CALC_ID_TEST_ADMIN = "MST_TEST_ADMIN";
    private static final String VAL_ID_INSTITUTION = "MST_VAL_INSTITUTION";
    private static final String VAL_ID_TEST_ADMIN = "MST_VAL_TEST_ADMIN";

    private static final String VALUE_NO = "N";
    private static final String VALUE_YES = "Y";

    /**
     * Instance variables.
     */
    protected Map<String, SchoolCalendar> m_calendarCache = new HashMap<String, SchoolCalendar>();
    protected TNClassSectionHelper m_classSectionHelper;
    protected String m_fieldClassType;
    protected String m_fieldClassTypeOver;
    protected String m_fieldDualCreditCrs;
    protected String m_fieldDualCreditCsk;
    protected String m_fieldDualCreditMst;
    protected String m_fieldExcludeCrs;
    protected String m_fieldHonorsInd;
    protected String m_fieldHonorsIndOver;
    protected String m_fieldPostInstMst;
    protected String m_fieldPostInstCrs;
    protected String m_fieldPostInstCsk;
    protected String m_fieldTeachingMethod;
    protected String m_fieldTeachingMethodOver;
    protected String m_fieldTestAdmin;
    protected String m_schoolYear;

    /**
     * @see com.x2dev.procedures.statereporting.tn.TNStateReportData#initialize()
     */
    /*
     * Initialize the data module.
     * Initialize necessary working resources.
     * Define query to load.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(java.util.Map,
     * com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    public void initialize() throws X2BaseException {
        super.initialize();
        initializeFields();
        School school = null;
        if (this.isSchoolContext()) {
            school = getSchool();
        }
        m_classSectionHelper = new TNClassSectionHelper(this, school, false, null, getBypassValue());
        if (getSetupErrors().size() != 0) {
            return;
        }
        if (getCurrentContext().getSchoolYear() > 2019) {
            setExportVersion(4);
        }
        getCalendarsForContextOid(m_contextOid);
        X2Criteria sklCriteria = new X2Criteria();
        sklCriteria.addNotEmpty(m_fieldStateSchoolId, getBroker().getPersistenceKey());
        QueryByCriteria sklQuery = new QueryByCriteria(SisSchool.class, sklCriteria);
        applyInputSort(sklQuery, null);
        setQuery(sklQuery);
        setEntityClass(TNClassSectionEntity.class);
        m_classSectionHelper.loadDefaultInstProgMap();
        m_classSectionHelper.loadInstProgMap();
        initFieldRetrievers();
    }

    /**
     * get the parameter setting for bypass value. Default to false.
     *
     * @return boolean
     */
    private boolean getBypassValue() {
        boolean value = false;
        if (getParameter(PARAM_BYPASS_DUP_SECT_TEST) != null) {
            value = ((Boolean) getParameter(PARAM_BYPASS_DUP_SECT_TEST)).booleanValue();
        }
        return value;
    }

    /**
     * Method for implementing business rule for schoolYear.
     *
     * @return string representation of school year
     */
    private String getCurrentSchoolYear() {
        return Integer.toString(getCurrentContext().getSchoolYear() - 1);
    }

    /**
     * Register custom field Retrievers.
     */
    private void initFieldRetrievers() {
        HashMap calcs = new HashMap<String, FieldRetriever>();
        calcs.put(CALC_ID_BEAN_PATH, new RetrieveBeanPath());
        calcs.put(CALC_ID_CDATES, new RetrieveCdates());
        calcs.put(CALC_ID_DCREDIT, new RetrieveDualCredit());
        calcs.put(CALC_ID_DEFAULT, new RetrieveDefault());
        calcs.put(CALC_ID_TEST_ADMIN, new RetrieveTestAdmin());
        calcs.put(CALC_ID_POST_INST, new RetrievePostSecInstitution());
        calcs.put(RetrieveSectionLevelOverValues.MST_CALC_ID, new RetrieveSectionLevelOverValues());
        calcs.put(RetrieveInstProgram.TN_CALC_INSTPGM_ID, new RetrieveInstProgram());
        super.addCalcs(calcs);

        HashMap validations = new HashMap<String, FieldValidator>();
        validations.put(VAL_ID_INSTITUTION, new ValidateInstitution());
        validations.put(VAL_ID_TEST_ADMIN, new ValidateTestAdmin());
        super.addValidators(validations);
    }

    /**
     * Lookup field aliases and paths.
     */
    private void initializeFields() {
        m_schoolYear = getCurrentSchoolYear();
        m_reportDate = (PlainDate) getParameter(PARAM_REPORT_DATE);
        m_fieldDualCreditCrs = translateAliasToJavaName(ALIAS_DUAL_CREDIT_CRS, true);
        m_fieldDualCreditCsk = translateAliasToJavaName(ALIAS_DUAL_CREDIT_CSK, true);
        m_fieldDualCreditMst = translateAliasToJavaName(ALIAS_DUAL_CREDIT_MST, true);
        m_fieldPostInstMst = translateAliasToJavaName(ALIAS_POST_INST_MST, true);
        m_fieldPostInstCsk = translateAliasToJavaName(ALIAS_POST_INST_CSK, true);
        m_fieldPostInstCrs = translateAliasToJavaName(ALIAS_POST_INST_CRS, true);
        m_fieldExcludeCrs = translateAliasToJavaName(ALIAS_EXCLUDE_CRS, true);
        m_fieldClassType = translateAliasToJavaName(ALIAS_CLASS_TYPE, true);
        m_fieldClassTypeOver = translateAliasToJavaName(ALIAS_CLASS_TYPE_OVER, true);
        m_fieldHonorsInd = translateAliasToJavaName(ALIAS_HONORS_IND, true);
        m_fieldHonorsIndOver = translateAliasToJavaName(ALIAS_HONORS_IND_OVER, true);
        m_fieldTeachingMethod = translateAliasToJavaName(ALIAS_TEACHING_METHOD, true);
        m_fieldTeachingMethodOver = translateAliasToJavaName(ALIAS_TEACHING_METHOD_OVER, true);
        m_fieldTestAdmin = translateAliasToJavaName(ALIAS_TEST_ADMIN_WINDOW, true);
    }
}
