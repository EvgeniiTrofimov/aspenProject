/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2014 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.procedures.statereporting.il;

import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.text.SimpleDateFormat;
import java.util.HashMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.commons.lang3.StringUtils;

/**
 * Procedure class for "IL Gifted and Accelerate Export" export.
 */
public class ILGiftedAcceleratedData extends StateReportData {
    /**
     * One entity stands for one student and produce a row
     * for each another student with same State ID as entity student.
     */
    public static class ILGiftedAcceleratedDataEntity extends StateReportEntity {

        /**
         * The effective Entry student enrollment record for report date.
         */
        protected StudentEnrollment m_enrollment = null;

        /**
         * Returns the Entry enrollment record.
         *
         * @return StudentEnrollment
         */
        public StudentEnrollment getEffectiveEnrollment() {
            return m_enrollment;
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
                    ", SASID: " + student.getStateId() + "]";
            return name;
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            SisStudent student = (SisStudent) getBean();
            ILGiftedAcceleratedData gaData = (ILGiftedAcceleratedData) data;
            m_enrollment = gaData.m_helper.getEnrollmentForDate(student.getOid(),
                    new PlainDate(), "EYSW");
            if (m_enrollment.getSchool().getFieldValueByBeanPath(gaData.m_fieldExcludeSkl) != null
                    && !BooleanAsStringConverter.TRUE
                            .equals(m_enrollment.getSchool().getFieldValueByBeanPath(gaData.m_fieldExcludeSkl))) {
                gaData.m_totalStudentCount++;
            } else {
                setRowCount(0);
            }
        }
    }

    /**
     * Retrieve the RCDTS of the student's home/serving school
     *
     * Parameter this retriever accepts:
     * - "H" for the home school's RCDTS
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveRcdts implements FieldRetriever {
        public static final String CALC_ID = "STD-RCDTS";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String param = (String) field.getParameter();
            ILGiftedAcceleratedDataEntity gaEntity = (ILGiftedAcceleratedDataEntity) entity;
            String rcdts = null;
            if (param.equals("H") && gaEntity.getEffectiveEnrollment() != null
                    && gaEntity.getEffectiveEnrollment().getSchool() != null) {
                StudentEnrollment primEnr = gaEntity.getEffectiveEnrollment();
                if (primEnr != null) {
                    ILGiftedAcceleratedData gaData = (ILGiftedAcceleratedData) data;
                    String codeForNonFte = (String) primEnr.getFieldValueByBeanPath(gaData.m_fieldEnrSklHome);
                    if (!StringUtils.isEmpty(codeForNonFte)) {
                        rcdts = gaData.lookupStateValue(StudentEnrollment.class, m_fieldEnrSklHome, codeForNonFte);
                    }
                }
                if (StringUtils.isEmpty(rcdts)) {
                    rcdts = (String) primEnr.getSchool().getFieldValueByBeanPath(m_fieldSchoolCode);
                }
            }
            return rcdts;
        }
    }

    /**
     * Retrieve school year.
     */
    public class RetrieveSklYear implements FieldRetriever {
        public static final String CALC_ID = "SKL-YEAR";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            return String.valueOf(getCurrentContext().getSchoolYear());
        }
    }

    /**
     * Retrieve a bean property and strip off invalid characters.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveStripNameChar implements FieldRetriever {
        public static final String CALC_ID = "GA-STRIPCHAR";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            String cleanValue = null;
            String nameValue = (String) getProperty(entity.getBean(), field.getBeanPath());
            if (nameValue != null) {
                Matcher matcher = m_illegalNameCharacters.matcher(nameValue);
                cleanValue = matcher.replaceAll("");
            } else {
                cleanValue = "";
            }
            return cleanValue;
        }
    }

    /**
     * The header prints out the name of the export, total count of records,
     * the file name, and the date.
     *
     * @return String
     */
    @Override
    public String getHeading() {
        StringBuilder heading = new StringBuilder(100);
        heading.append("Gifted and Accelerated");
        heading.append(',');
        heading.append(m_totalStudentCount);
        heading.append(',');
        heading.append(getFileName());
        heading.append(',');
        heading.append(m_dateFormat.format(new PlainDate()));
        heading.append(',');
        if (getOrganization() != null) {
            heading.append(getOrganization().getFieldValueByBeanPath(m_fieldDistrictCode));
        }
        heading.append("\n");
        return heading.toString();
    }

    /**
     * Aliases
     */
    protected static final String ALIAS_DISTRICT_ID = "DOE DISTRICT ID";
    protected static final String ALIAS_ENR_SKL_HOME = "DOE SCHOOL HOME";
    protected static final String ALIAS_EXCLUDE_STD = "DOE EXCLUDE STD";
    protected static final String ALIAS_EXCLUDE_SKL = "DOE EXCLUDE SKL";
    protected static final String ALIAS_SCHOOL_ID = "DOE SCHOOL ID";

    /**
     * Other constants
     */
    protected static final String ILLEGAL_NAME_CHARACTERS = "[^A-Za-z ]";

    /**
     * Members.
     */
    protected SimpleDateFormat m_dateFormat = new SimpleDateFormat("MM/dd/yyyy");
    protected String m_fieldDistrictCode;
    protected String m_fieldEnrSklHome;
    protected String m_fieldExcludeSkl;
    protected String m_fieldExcludeStd;
    protected String m_fieldSchoolCode;
    protected StudentHistoryHelper m_helper;
    protected Pattern m_illegalNameCharacters = Pattern.compile(ILLEGAL_NAME_CHARACTERS);
    protected int m_totalStudentCount;

    /**
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    public void initialize() {
        initializeFields();
        m_helper = new StudentHistoryHelper(this);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
        m_helper.getStudentCriteria().addNotEqualTo(m_fieldExcludeStd, Boolean.TRUE);
        if (getSetupErrors().size() == 0) {
            setEntityClass(ILGiftedAcceleratedDataEntity.class);
            setQuery(m_helper.getStudentQuery(true));
            HashMap<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
            calcs.put(RetrieveSklYear.CALC_ID, new RetrieveSklYear());
            calcs.put(RetrieveRcdts.CALC_ID, new RetrieveRcdts());
            calcs.put(RetrieveStripNameChar.CALC_ID, new RetrieveStripNameChar());
            super.addCalcs(calcs);
        }
    }

    /**
     * Generate the filename for this export.
     *
     * @return <district name>_<report date>_001.csv
     */
    private String getFileName() {
        StringBuilder fileName = new StringBuilder();
        if (getOrganization() != null) {
            fileName.append(getOrganization().getFieldValueByBeanPath(m_fieldDistrictCode));
        }
        fileName.append("_");
        fileName.append(new SimpleDateFormat("MMddyyyy").format(new PlainDate()));
        fileName.append("_");
        fileName.append("001.txt");
        return fileName.toString();
    }

    /**
     * Initialize aliases and other report data.
     */
    private void initializeFields() {
        m_fieldEnrSklHome = translateAliasToJavaName(ALIAS_ENR_SKL_HOME, true);
        m_fieldSchoolCode = translateAliasToJavaName(ALIAS_SCHOOL_ID, true);
        m_fieldExcludeSkl = translateAliasToJavaName(ALIAS_EXCLUDE_SKL, true);
        m_fieldExcludeStd = translateAliasToJavaName(ALIAS_EXCLUDE_STD, false);
        m_fieldDistrictCode = translateAliasToJavaName(ALIAS_DISTRICT_ID, true);
    }
}
