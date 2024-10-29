/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2011 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.il;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.procedures.statereporting.il.DemoExitDataHelper.SpansFactory.DemoExitSpan;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentSchedule;
import com.x2dev.sis.model.beans.StudentScheduleChange;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.text.SimpleDateFormat;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Illinois state export procedure for Student Demographic/Enrollment.
 *
 * @author X2 Development Corporation
 */
public class RcdtsBasedProcedureData extends StateReportData {
    /**
     * Implementation of StateReportEntity to be used by the IL.
     * This must be a public static inner class with a public no argument
     * constructor so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class RcdtsEntity extends StateReportEntity {
        private DemoExitSpan m_matchingSpan = null;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public RcdtsEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Gets the current span.
         *
         * @return Demo exit span
         */
        public DemoExitSpan getCurrentSpan() {
            return m_matchingSpan;
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

            RcdtsBasedProcedureData rcdtsData = (RcdtsBasedProcedureData) data;
            SisStudent student = (SisStudent) bean;
            int rowCount = 0;

            HashSet<DemoExitSpan> allSpans = rcdtsData.m_dataHelper.getSpans(student);
            if (allSpans != null) {
                PlainDate mostRecentDate = null;
                LinkedList<DemoExitSpan> matchingSpans = new LinkedList();
                LinkedList<DemoExitSpan> mostRecentSpans = null;
                for (DemoExitSpan span : allSpans) {
                    if (span.getExitDate() != null && span.getExitDate().before(rcdtsData.m_reportDate)) {
                        if (mostRecentDate == null || mostRecentDate.before(span.getExitDate())) {
                            mostRecentDate = span.getExitDate();
                            mostRecentSpans = new LinkedList();
                            mostRecentSpans.add(span);
                        } else if (mostRecentDate.equals(span.getExitDate())) {
                            mostRecentSpans.add(span);
                        }
                    }
                    if (span.getStartDate() != null && !span.getStartDate().after(rcdtsData.m_reportDate)
                            && (span.getExitDate() == null || !span.getExitDate().before(rcdtsData.m_reportDate))) {
                        matchingSpans.add(span);
                    }
                }

                m_matchingSpan = null;
                if (rcdtsData.isActiveOnly()) {
                    m_matchingSpan = selectSpan(matchingSpans);
                } else if (rcdtsData.isInactiveOnly()) {
                    if (m_matchingSpan == null) {
                        m_matchingSpan = selectSpan(mostRecentSpans);
                    }
                } else {
                    if (matchingSpans.isEmpty()) {
                        m_matchingSpan = selectSpan(mostRecentSpans);
                    } else {
                        m_matchingSpan = selectSpan(matchingSpans);
                    }
                }
                if (m_matchingSpan != null) {
                    rowCount = 1;
                }
            }

            setRowCount(rowCount);
            rcdtsData.m_totalStudentCount += rowCount;
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
         * @param spans
         * @return
         */
        private DemoExitSpan selectSpan(LinkedList<DemoExitSpan> spans) {
            DemoExitSpan selectedSpan = null;
            if (spans != null && !spans.isEmpty()) {
                if (spans.size() == 1) {
                    selectedSpan = spans.get(0);
                } else {
                    Float highestFTE = null;
                    // Use highest FTE unless there is a record where home school and serving school
                    // match
                    for (DemoExitSpan span : spans) {
                        if (highestFTE == null
                                || span.getDemoDataset().getFte().floatValue() > highestFTE.floatValue()) {
                            highestFTE = span.getDemoDataset().getFte();
                            selectedSpan = span;
                        }
                        if (span.getHomeSchool().equals(span.getServingSchool())) {
                            selectedSpan = span;
                            break;
                        }
                    }
                }
            }
            return selectedSpan;
        }

    }

    /**
     * The Class RetrieveSpan.
     */
    protected class RetrieveSpan implements FieldRetriever {
        private static final String PARAM_HOME = "H";
        private static final String PARAM_SERVING = "S";

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
            DemoExitSpan span = ((RcdtsEntity) entity).getCurrentSpan();
            if (span != null) {
                switch ((String) field.getParameter()) {
                    case PARAM_HOME:
                        value = span.getHomeSchool();
                        break;
                    case PARAM_SERVING:
                        value = span.getServingSchool();
                        break;
                    default:
                        break;
                }
            }
            return value;
        }
    }


    /**
     * Retrieve a bean property and strip off invalid characters.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveStripNameChar implements FieldRetriever {
        private static final String ILLEGAL_NAME_CHARACTERS = "[^A-Za-z -]";

        private Pattern m_illegalNameCharacters = Pattern.compile(ILLEGAL_NAME_CHARACTERS);

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

    private static final String ALIAS_DISTRICT_ID = "DOE DISTRICT ID";

    /*
     * Parameters
     */
    private static final String PARAM_REPORT_DATE = "reportDate";

    /**
     * Helper class:
     */
    protected DemoExitDataHelper m_dataHelper = null;

    /**
     * The report date when enrollment is evaluated
     */
    protected PlainDate m_reportDate;

    /**
     * Keep track of number of students
     */
    protected int m_totalStudentCount;

    /*
     * Instance variables
     */
    private SimpleDateFormat m_dateFormat = new SimpleDateFormat("MM/dd/yyyy");
    private String m_fieldDistrictCode;
    private Organization m_organization;

    /**
     * The header prints out the name of the export, total count of records,
     * the file name, and the date.
     *
     * @return String
     */
    @Override
    public String getHeading() {
        StringBuilder heading = new StringBuilder(100);
        heading.append(getExportName()).append(',');
        heading.append(m_totalStudentCount);
        heading.append(',');
        heading.append(getFileName());
        heading.append(',');
        if (m_reportDate != null) {
            heading.append(m_dateFormat.format(m_reportDate));
        }
        heading.append(',');
        if (m_organization != null && m_fieldDistrictCode != null) {
            heading.append(m_organization.getFieldValueByBeanPath(m_fieldDistrictCode));
        }
        heading.append("\n");
        return heading.toString();
    }

    /**
     * Initialize.
     *
     * @see com.x2dev.sis.tools.stateexports.StateReportData#initialize()
     */
    @Override
    public void initialize() {
        m_organization = getOrganization();
        if (m_organization == null) {
            m_organization = OrganizationManager.getRootOrganization(getBroker());
        }

        initializeFields();
        setEntityClass(RcdtsEntity.class);

        m_reportDate = ((PlainDate) getParameter(PARAM_REPORT_DATE));

        m_dataHelper = new DemoExitDataHelper(this);
        m_dataHelper.setCurrentSubmissionDate(m_reportDate);
        m_dataHelper.setLastSubmissionDate(getCurrentContext().getStartDate());

        addSelectionCriteria(m_dataHelper.getEnrollmentHelper().getStudentCriteria(), "");
        addSelectionCriteria(m_dataHelper.getScheduleHelper().getStudentScheduleCriteria(),
                StudentSchedule.REL_STUDENT + ModelProperty.PATH_DELIMITER);
        addSelectionCriteria(m_dataHelper.getScheduleHelper().getStudentScheduleChangeCriteria(),
                StudentScheduleChange.REL_STUDENT + ModelProperty.PATH_DELIMITER);
        // Map of IepData on report date with active status

        // Set the query to be used for student selection.
        QueryByCriteria studentQuery = m_dataHelper.getEnrollmentHelper().getStudentQuery(false);
        setQuery(studentQuery);

        HashMap<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
        calcs.put("DEMO-STRIPCHAR", new RetrieveStripNameChar());
        calcs.put("DEMO-RCDTS", new RetrieveSpan());
        addCalcs(calcs);

        HashMap<String, FieldValidator> validators = new HashMap<String, FieldValidator>();
        addValidators(validators);
    }

    /**
     * Adds the selection criteria.
     *
     * @param criteria X2Criteria
     * @param prefix String
     */
    protected void addSelectionCriteria(X2Criteria criteria, String prefix) {
        // no additional criteria by default
    }

    /**
     * @return
     */
    protected String getExportName() {
        return "";
    }

    protected boolean isActiveOnly() {
        return false;
    }

    protected boolean isInactiveOnly() {
        return false;
    }

    /**
     * Generate the filename for this export.
     *
     * @return <district name>_<report date>_001.csv
     */
    private String getFileName() {
        StringBuilder fileName = new StringBuilder();
        if (m_organization != null && m_fieldDistrictCode != null) {
            fileName.append(m_organization.getFieldValueByBeanPath(m_fieldDistrictCode));
        }
        fileName.append("_");
        if (m_reportDate != null) {
            fileName.append(new SimpleDateFormat("MMddyyyy").format(m_reportDate));
        }
        fileName.append("_");
        fileName.append("001.csv");
        return fileName.toString();
    }

    /**
     * Sets the Java names (bean paths) for all the exported fields.
     */
    private void initializeFields() {
        m_fieldDistrictCode = translateAliasToJavaName(ALIAS_DISTRICT_ID, true);
    }
}
