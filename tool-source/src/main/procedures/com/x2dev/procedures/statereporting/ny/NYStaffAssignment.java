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
package com.x2dev.procedures.statereporting.ny;

import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.beans.StaffPosition;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class NYStaffAssignment.
 */
public class NYStaffAssignment extends StateReportData {
    /**
     * Entity class for Staff Snapshot Export.
     *
     * @author X2 Development Corporation
     */

    public static class NYStaffAssignmentEntity extends StateReportEntity {
        private NYStaffAssignment saData;
        /**
         * Public no argument constructor for dynamic instantiation.
         */
        ArrayList<String> reportableGradeLevels = new ArrayList<String>();

        /**
         * Instantiates a new NY staff assignment entity.
         */
        public NYStaffAssignmentEntity() {
            // no argument constructor
        }

        /**
         * Initialize the data module.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            saData = (NYStaffAssignment) data;
            StaffPosition staffPosition = (StaffPosition) bean;
            String gradeLevelString = (String) staffPosition.getFieldValueByBeanPath(saData.m_gradeLevelPos);
            if (!StringUtils.isEmpty(gradeLevelString)) {
                gradeLevelString = gradeLevelString.toUpperCase();
            }
            parseGradeLevelString(gradeLevelString);
            setRowCount(reportableGradeLevels.size());
        }

        /**
         * Takes the string of grade levels separated by commas and dashes. We allow user to input
         * in either.
         * Looks to see if the string is an 'All' as there is special logic for that, otherwise
         * breaks by commas
         *
         * @param gradeLevelString String
         */
        private void parseGradeLevelString(String gradeLevelString) {
            if (StringUtils.isEmpty(gradeLevelString) || CODE_ALL.equals(gradeLevelString)) {
                reportableGradeLevels.add(CODE_ALL);
            } else {
                String[] gradeLevelComma = gradeLevelString.split(REGEX_COMMA);
                for (String gradeLevel : gradeLevelComma) {
                    reportableGradeLevels.addAll(parseGradeLevelDash(gradeLevel));
                }
            }
        }

        /**
         * Parses the data by dashes, and converts those into grade ranges.
         *
         * @param gradeLevel String
         * @return ArrayList
         */
        private ArrayList<String> parseGradeLevelDash(String gradeLevel) {
            ArrayList<String> gradeLevels = new ArrayList<String>();
            if (gradeLevel.contains(REGEX_DASH)) {
                String[] gradeLevelDash = gradeLevel.split(REGEX_DASH);
                if (gradeLevelDash.length != 2) {
                    // Improper format (not ,#-#, )
                    return gradeLevels;
                }

                Integer startingGradeLevel = getValueFromStringForGradeLevel(gradeLevelDash[0]);
                Integer endingingGradeLevel = getValueFromStringForGradeLevel(gradeLevelDash[1]);
                if (startingGradeLevel.intValue() > endingingGradeLevel.intValue()) {
                    int dummySwap = startingGradeLevel.intValue();
                    startingGradeLevel = endingingGradeLevel;
                    endingingGradeLevel = Integer.valueOf(dummySwap);
                }
                for (int gradeLevelCounter = startingGradeLevel.intValue(); gradeLevelCounter <= endingingGradeLevel
                        .intValue(); gradeLevelCounter++) {
                    gradeLevels.add("" + gradeLevelCounter);
                }
            } else {
                gradeLevels.add(gradeLevel);
            }
            return gradeLevels;
        }

        /**
         * Handles cases of PK and K otherwise just returns the numeric value that is in the string.
         *
         * @param gradeLevel String
         * @return Integer
         */
        private Integer getValueFromStringForGradeLevel(String gradeLevel) {
            Integer gradeLevelInteger = null;
            if (CODE_PREK.equals(gradeLevel)) {
                return Integer.valueOf(VALUE_PRE_K);
            } else if (CODE_K.equals(gradeLevel)) {
                return Integer.valueOf(VALUE_K);
            } else {
                try {
                    gradeLevelInteger = Integer.valueOf(gradeLevel);
                } catch (NumberFormatException nfe) {
                    // Will drop down to return null...
                }
            }
            return gradeLevelInteger;
        }

        /**
         * Returns the current staff.
         *
         * @return SisStaff
         */
        public SisStaff getStaff() {
            return ((StaffPosition) getBean()).getStaff();
        }

        /**
         * Gets the grade level.
         *
         * @return String
         */
        public String getGradeLevel() {
            return reportableGradeLevels.get(getCurrentRow());
        }
    }

    /**
     * Retrieves the status for the given StaffSnapshotEntity.
     */
    protected class RetrieveStaffDates implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object value = null;
            NYStaffAssignmentEntity saEntity = (NYStaffAssignmentEntity) entity;
            String param = (String) field.getParameter();
            SisStaff staff = saEntity.getStaff();
            PlainDate startDate = null;
            PlainDate endDate = null;
            for (StaffPosition position : staff.getStaffPositions()) {
                if (startDate == null || startDate.after(position.getStartDate())) {
                    PlainDate positionEndDate = position.getEndDate();
                    // We have nothing
                    if ((startDate == null && endDate == null) ||
                    // We have a start and an end date, but the position is ongoing (ie no end date)
                            (startDate != null && endDate != null && positionEndDate == null) ||
                            // Or the both have ended, but the new one ended later
                            (positionEndDate != null && endDate != null && positionEndDate.after(endDate))
                            // They are both null
                            || (endDate == null && positionEndDate == null)) {
                        startDate = position.getStartDate();
                        endDate = position.getEndDate();
                    }
                }
            }
            if ("COMPLETION_DATE".equals(param)) {
                value = endDate;
            } else if ("ASSIGNMENT_DATE".equals(param)) {
                PlainDate firstDayOfSchool = getCurrentContext().getStartDate();// getFirstDayOfSchool(saData,
                                                                                // staff.getSchool());

                if (startDate == null || startDate.before(firstDayOfSchool)) {
                    value = firstDayOfSchool;
                } else {
                    value = startDate;
                }
            }
            return value;
        }
    }

    /**
     * Retrieves the status for the given StaffSnapshotEntity.
     */
    protected class RetrieveGradeLevel implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            NYStaffAssignmentEntity saEntity = (NYStaffAssignmentEntity) entity;
            return saEntity.getGradeLevel();
        }
    }

    /**
     * Aliases
     */
    protected static final String ALIAS_COMPLETION_DATE = "NY COMPLETION DATE";
    protected static final String ALIAS_EXCLUDE_STF = "DOE EXCLUDE STF";
    protected static final String ALIAS_GRADE_LEVEL_POS = "DOE POS GRADE LEVEL";
    protected static final String ALIAS_SKL_EXCLUDE_SCHOOL = "DOE EXCLUDE SKL";

    /**
     * Constants for reporting information.
     */
    protected static final String STAFF_STATUS_ACTIVE = "A";
    protected static final String STAFF_STATUS_INACTIVE = "I";

    protected static final int VALUE_K = 0;
    protected static final int VALUE_PRE_K = -1;

    protected static final String CODE_ALL = "ALL";
    protected static final String CODE_K = "K";
    protected static final String CODE_PREK = "PK";

    protected static final String REGEX_COMMA = ",";
    protected static final String REGEX_DASH = "-";

    /**
     * Instance variables
     */
    protected String m_completionDate;
    protected String m_excludeStfField;
    protected String m_gradeLevelPos;
    protected String m_fieldSklExcludeSchool;
    protected HashMap<String, PlainDate> m_firstSchoolDaysByOid = new HashMap<String, PlainDate>();

    public static final String PARAM_REMOVE_HEADER = "removeHeader";

    /**
     * Allows the user to choose if the export should have the header on or not at runtime.
     *
     * @return String
     */
    @Override
    public String getHeading() {
        if (((Boolean) getParameter(PARAM_REMOVE_HEADER)).booleanValue()) {
            return null;
        }
        List<FieldDefinition> definitions = getFieldDefinitions();
        StringBuilder heading = new StringBuilder("");
        for (FieldDefinition fieldDefinition : definitions) {
            if (!StringUtils.isEmpty(heading.toString())) {
                heading.append("\t");
            }
            heading.append(fieldDefinition.getFieldId());
        }
        heading.append("\r\n");
        return heading.toString();
    }

    /**
     * Initialize the data module.
     */
    @Override
    public void initialize() {
        initializeFields();

        if (getSetupErrors().size() == 0) {
            Criteria criteria = getStaffCriteria();
            // … create staff Query …
            QueryByCriteria query = new QueryByCriteria(StaffPosition.class, criteria);
            applyInputSort(query, null);
            setQuery(query);
            setEntityClass(NYStaffAssignmentEntity.class);

            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put("STFASS-GRDLVL", new RetrieveGradeLevel());
            calcs.put("STFASS-DATES", new RetrieveStaffDates());

            HashMap validators = new HashMap<String, FieldRetriever>();
            super.addCalcs(calcs);
            super.addValidators(validators);
        }
    }

    /**
     * Sets the Java names (bean paths) for all the exported fields.
     */
    private void initializeFields() {
        m_excludeStfField = translateAliasToJavaName(ALIAS_EXCLUDE_STF, false);
        m_gradeLevelPos = translateAliasToJavaName(ALIAS_GRADE_LEVEL_POS, true);
        m_fieldSklExcludeSchool = translateAliasToJavaName(ALIAS_SKL_EXCLUDE_SCHOOL, true);
    }

    /**
     * Returns the criteria that retrieves all staff that should be included in the export.
     *
     * @return Criteria
     */
    private Criteria getStaffCriteria() {
        Criteria staffCriteria = new Criteria();
        staffCriteria.addNotNull(X2BaseBean.COL_OID);

        applyInputCriteria(staffCriteria, true, null);
        if (!isSchoolContext()) {
            staffCriteria.addNotEqualTo(
                    StaffPosition.REL_SCHOOL + ModelProperty.PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR,
                    BooleanAsStringConverter.TRUE);
            staffCriteria.addNotEqualTo(
                    StaffPosition.REL_SCHOOL + ModelProperty.PATH_DELIMITER + SisSchool.COL_ARCHIVE_INDICATOR,
                    BooleanAsStringConverter.TRUE);
        }
        staffCriteria.addNotEqualTo(
                StaffPosition.REL_SCHOOL + ModelProperty.PATH_DELIMITER + m_fieldSklExcludeSchool,
                BooleanAsStringConverter.TRUE);

        return staffCriteria;
    }
}
