/*
 * ====================================================================
 *
 * Follett School Solutions
 *
 * Copyright (c) 2020 Follett School Solutions
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett School Solutions.
 *
 * ====================================================================
 */

package com.x2dev.procedures.statereporting.il;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.RecordSet;
import com.follett.fsc.core.k12.beans.RecordSetKey;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.procedures.statereporting.il.DemoExitDataHelper.SpansFactory.StudentDemoDatasets.DemoDataset;
import com.x2dev.sis.model.beans.ConductAction;
import com.x2dev.sis.model.beans.SisDistrictSchoolYearContext;
import com.x2dev.sis.model.beans.SisOrganization;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Illinois state export procedure for Student Demographic/Enrollment.
 *
 * @author Follett School Solutions
 */
public class StudentConductPRTO extends StateReportData {
    /**
     * Implementation of StateReportEntity to be used by the IL.
     * This must be a public static inner class with a public no argument
     * constructor so it can be instantiated through reflection.
     *
     * @author Follett School Solutions
     */
    public static class StudentConductPRTOEntity extends StateReportEntity {
        List<DemoDataset> m_datasets = new ArrayList<DemoDataset>();
        /**
         * StudentDemographics data.
         */
        StudentConductPRTO m_scPrto = null;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public StudentConductPRTOEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            ConductAction act = (ConductAction) getBean();
            SisStudent student = act.getStudent();
            String name = "Action Code: " + act.getActionCode() + student.getNameView() +
                    ", SASID: " + student.getStateId() + "]";

            return name;
        }

        /**
         * Gets the school with fte.
         *
         * @return Demo dataset
         */
        public DemoDataset getSchoolWithFte() {
            return m_datasets.get(getCurrentRow());
        }

        /**
         * Initialize and increment counter.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            m_scPrto = (StudentConductPRTO) data;

            ConductAction act = (ConductAction) bean;
            SisStudent student = act.getStudent();

            m_datasets = new ArrayList<DemoDataset>(
                    m_scPrto.m_ilExitDemoHelper.getDatasets(student, act.getActionStartDate()));

            setRowCount(m_datasets.size());

            m_scPrto.m_totalCount += m_datasets.size();

            String key = act.getActionStartDate().getTime() + student.getOid();
            if (!m_scPrto.m_eventNumbers.containsKey(key)) {
                m_scPrto.m_eventNumbers.put(key, new Integer(1));
            } else {
                int newEventNum = m_scPrto.m_eventNumbers.get(key).intValue() + 1;
                m_scPrto.m_eventNumbers.put(key, new Integer(newEventNum));
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
     * Retrieve a boolean bean property and return 01 or 02, respectively.
     *
     * @author Follett School Solutions
     */
    protected class RetrieveActionCode implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String value = "";
            String nameValue = (String) getProperty(entity.getBean(), field.getBeanPath());
            ReferenceCode rcd = m_actionCodes.get(nameValue);
            if (nameValue != null && rcd != null && StringUtils.equals(rcd.getStateCode(), "PR")) {
                value = "01";
            }
            if (nameValue != null && rcd != null && StringUtils.equals(rcd.getStateCode(), "TO")) {
                value = "02";
            }

            return value;
        }
    }

    /**
     * Retrieve a boolean bean property and return 01 or 02, respectively.
     *
     * @author Follett School Solutions
     */
    protected class RetrieveBooleanNumeric implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String value = "02";
            String nameValue = (String) getProperty(entity.getBean(), field.getBeanPath());
            if (nameValue != null && StringUtils.equals(nameValue, BooleanAsStringConverter.TRUE)) {
                value = "01";
            }

            return value;
        }
    }

    /**
     * Retrieve Delete PRTO Record indicator.
     *
     * @author Follett School Solutions
     */
    protected class RetrieveDeletePRTO implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String value = "";
            String nameValue = (String) getProperty(entity.getBean(), field.getBeanPath());
            if (nameValue != null && StringUtils.equals(nameValue, BooleanAsStringConverter.TRUE)) {
                value = "99";
            }

            return value;
        }
    }

    /**
     * Retrieve event number.
     *
     * @author Follett School Solutions
     */
    protected class RetrieveEventNumber implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            ConductAction act = (ConductAction) entity.getBean();
            SisStudent student = act.getStudent();
            String key = act.getActionStartDate().getTime() + student.getOid();

            return String.format("%02d", m_eventNumbers.get(key));
        }
    }


    /**
     * Retrieve the RCDTS of the student's home/serving school
     *
     * Parameter this retriever accepts:
     * - "H" for the home school's RCDTS
     * - "S" for the serving school's RCDTS.
     *
     * @author Follett School Solutions
     */
    protected class RetrieveRcdts implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String param = (String) field.getParameter();
            String rcdts = null;
            if (param.equals("H")) {
                rcdts = ((StudentConductPRTOEntity) entity).getSchoolWithFte().getHomeSchoolCode();
            } else if (param.equals("S")) {
                rcdts = ((StudentConductPRTOEntity) entity).getSchoolWithFte().getServiceSchoolCode();
            }
            return rcdts;
        }
    }

    /**
     * Retrieve a bean property and strip off invalid characters.
     *
     * @author Follett School Solutions
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

    /**
     * The header prints out the name of the export, total count of records,
     * the file name, and the date.
     *
     * @return String
     */
    @Override
    public String getHeading() {
        StringBuilder heading = new StringBuilder(100);
        heading.append("Physical Restraint and Time Out");
        heading.append(',');
        heading.append(m_totalCount);
        heading.append(',');
        heading.append(getFileName());
        heading.append(',');
        heading.append(m_dateFormat.format(new PlainDate()));
        heading.append(',');
        heading.append(getOrganization().getFieldValueByBeanPath(m_fieldDistrictCode));
        heading.append("\n");
        return heading.toString();
    }

    /*
     * Parameters
     */
    private static final String PARAM_SORT = "sort";
    private static final String PARAM_QUERY_BY = "queryBy";
    private static final String PARAM_QUERY_STRING = "queryString";

    /*
     * Aliases
     */
    protected static final String ALIAS_DISTRICT_ID = "DOE DISTRICT ID";
    protected static final String ALIAS_ACTION_CODE = "DOE ACTION CODE";

    private final List ACTION_CODES = Arrays.asList("PR", "TO");

    /*
     * Instance variables
     */
    protected SimpleDateFormat m_dateFormat = new SimpleDateFormat("MM/dd/yyyy");
    protected String m_fieldActionCode;
    protected String m_fieldDistrictCode;
    protected DemoExitDataHelper m_ilExitDemoHelper;

    /**
     * Keep track of number of records
     */
    protected int m_totalCount;


    /**
     * Maps.
     */
    protected Map<String, ReferenceCode> m_actionCodes;
    protected Map<String, Integer> m_eventNumbers;

    /**
     * Initialize.
     *
     * @see com.x2dev.sis.tools.stateexports.StateReportData#initialize()
     */
    @Override
    public void initialize() {
        setEntityClass(StudentConductPRTOEntity.class);
        initializeFields();

        m_ilExitDemoHelper = new DemoExitDataHelper(this);
        m_ilExitDemoHelper.setLastSubmissionDate(getOrganization().getCurrentContext().getStartDate());
        m_ilExitDemoHelper.setCurrentSubmissionDate(getOrganization().getCurrentContext().getEndDate());
        m_ilExitDemoHelper.setHelperMode(DemoExitDataHelper.MODE_PRIMARY_SCHOOL);

        m_eventNumbers = new HashMap<String, Integer>();
        m_actionCodes = loadActionCodes();


        X2Criteria actCriteria = new X2Criteria();
        actCriteria.addIn(m_fieldActionCode, m_actionCodes.keySet());
        actCriteria.addGreaterOrEqualThanField(ConductAction.COL_ACTION_START_DATE,
                ConductAction.REL_SCHOOL + ModelProperty.PATH_DELIMITER +
                        SisSchool.REL_ORGANIZATION1 + ModelProperty.PATH_DELIMITER + SisOrganization.REL_CURRENT_CONTEXT
                        + ModelProperty.PATH_DELIMITER + SisDistrictSchoolYearContext.COL_START_DATE);
        actCriteria.addLessOrEqualThanField(ConductAction.COL_ACTION_START_DATE,
                ConductAction.REL_SCHOOL + ModelProperty.PATH_DELIMITER +
                        SisSchool.REL_ORGANIZATION1 + ModelProperty.PATH_DELIMITER + SisOrganization.REL_CURRENT_CONTEXT
                        + ModelProperty.PATH_DELIMITER + SisDistrictSchoolYearContext.COL_END_DATE);
        filterStudentCriteria(actCriteria);
        QueryByCriteria actQuery = new QueryByCriteria(ConductAction.class, actCriteria);
        applySort(actQuery);


        X2Criteria helperStudentCriteria = new X2Criteria();
        helperStudentCriteria.addIn(X2BaseBean.COL_OID,
                new SubQuery(ConductAction.class, ConductAction.COL_STUDENT_OID, actCriteria));
        BeanQuery stdQuery = new BeanQuery(Student.class, helperStudentCriteria);
        setQuery(stdQuery);
        m_ilExitDemoHelper.initializeDatasets();

        setQuery(actQuery);

        // Build a map of calculations/retrievers.
        HashMap<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
        calcs.put("ACT-BOOL-NUM", new RetrieveBooleanNumeric());
        calcs.put("ACT-CODE", new RetrieveActionCode());
        calcs.put("ACT-DEL-PRTO", new RetrieveDeletePRTO());
        calcs.put("ACT-EVENT-NUM", new RetrieveEventNumber());
        calcs.put("ACT-STRIPCHAR", new RetrieveStripNameChar());
        calcs.put("ACT-RCDTS", new RetrieveRcdts());
        super.addCalcs(calcs);
    }

    /**
     * Implementing the select student filter based on parameters.
     *
     * @param actCriteria X2Criteria
     */
    private void filterStudentCriteria(X2Criteria actCriteria) {
        String queryString = (String) getParameter(PARAM_QUERY_STRING);
        int queryBy = ((Integer) getParameter(PARAM_QUERY_BY)).intValue();
        switch (queryBy) {
            case 1: // YOG
                actCriteria.addEqualTo(ConductAction.REL_STUDENT + PATH_DELIMITER + Student.COL_YOG,
                        queryString);
                break;

            case 2: // LASID
                actCriteria.addEqualTo(ConductAction.REL_STUDENT + PATH_DELIMITER + Student.COL_LOCAL_ID,
                        queryString);
                break;

            case 3: // SASID
                actCriteria.addEqualTo(ConductAction.REL_STUDENT + PATH_DELIMITER + Student.COL_STATE_ID,
                        queryString);
                break;

            case 4: // Snapshot
                addRecordSetCriteria(actCriteria, queryString);
                break;

            default:
                // Take all students in the district
                break;
        }
    }

    /**
     * Adds criteria to filter the export selection by record set.
     *
     * @param criteria Criteria
     * @param recordSetName String
     */
    private void addRecordSetCriteria(Criteria criteria, String recordSetName) {
        Criteria recordSetCriteria = new Criteria();
        recordSetCriteria.addEqualTo(RecordSetKey.REL_RECORD_SET + PATH_DELIMITER + RecordSet.COL_NAME,
                recordSetName);

        criteria.addIn(ConductAction.REL_STUDENT + PATH_DELIMITER + X2BaseBean.COL_OID,
                new SubQuery(RecordSetKey.class, RecordSetKey.COL_OBJECT_OID, recordSetCriteria));
    }

    /**
     * Load action codes.
     *
     * @return Map
     */
    private Map<String, ReferenceCode> loadActionCodes() {
        Map<String, ReferenceCode> codeMap = new HashMap<String, ReferenceCode>();
        DataDictionaryField field = getDataDictionaryField(ConductAction.class, m_fieldActionCode);
        Collection<ReferenceCode> refCodes = field.getReferenceTable().getReferenceCodes();
        for (ReferenceCode code : refCodes) {
            if (ACTION_CODES.contains(code.getStateCode())) {
                codeMap.put(code.getCode(), code);
            }
        }
        return codeMap;
    }

    /**
     * Sort the query
     *
     * @param criteria Criteria
     * @param recordSetName String
     */
    private void applySort(QueryByCriteria query) {
        int sort = ((Integer) getParameter(PARAM_SORT)).intValue();

        switch (sort) {
            case 1: // Name
                query.addOrderByAscending(
                        ConductAction.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_NAME_VIEW);
                break;
            case 2: // YOG
                query.addOrderByAscending(
                        ConductAction.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_YOG);
                break;
            case 3: // School
                query.addOrderByAscending(
                        ConductAction.REL_SCHOOL + PATH_DELIMITER + School.COL_NAME);
                break;

            case 4: // LASID
                query.addOrderByAscending(
                        ConductAction.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_LOCAL_ID);
                break;

            case 5: // SASID
                query.addOrderByAscending(
                        ConductAction.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_STATE_ID);
                break;

            default:
                query.addOrderByAscending(
                        ConductAction.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_NAME_VIEW);
                break;
        }
    }

    /**
     * Generate the filename for this export.
     *
     * @return <district name>_<today's date>_001.csv
     */
    private String getFileName() {
        StringBuilder fileName = new StringBuilder();
        fileName.append(getOrganization().getFieldValueByBeanPath(m_fieldDistrictCode));
        fileName.append("_");
        fileName.append(new SimpleDateFormat("MMddyyyy").format(new PlainDate()));
        fileName.append("_");
        fileName.append("001.csv");
        return fileName.toString();
    }

    /**
     * Sets the Java names (bean paths) for all the exported fields.
     */
    private void initializeFields() {
        m_fieldActionCode = translateAliasToJavaName(ALIAS_ACTION_CODE, true);
        m_fieldDistrictCode = translateAliasToJavaName(ALIAS_DISTRICT_ID, true);
    }
}
