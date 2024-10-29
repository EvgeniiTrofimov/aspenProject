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
package com.x2dev.procedures.statereporting.wa;
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

import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.StudentSchool;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.types.PlainDate;
import java.util.HashMap;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Procedure to mass add school assocation records for a student.
 *
 * @author X2 Development Corporation
 */
public class WAMassAddStudentSchoolAssociation extends ProcedureJavaSource {
    /**
     * 
     */
    private static final long serialVersionUID = 1L;

    // Input parameters
    private static final String sskSchOid_PARAM = "sskSchOid_";
    private static final String sskYear_PARAM = "sskYear_";
    private static final String sskAssocType_PARAM = "sskAssocType_";
    private static final String sskAttMagType_PARAM = "sskAttMagType_";
    private static final String sskStartDate_PARAM = "sskStartDate_";
    private static final String sskEndDate_PARAM = "sskEndDate_";
    private static final String sskOodName_PARAM = "sskOodName_";
    private static final String sskOodAddr_PARAM = "sskOodAddr_";
    private static final String sskEntryCode_PARAM = "sskEntryCode_";
    private static final String sskWithdrawCode_PARAM = "sskWithdrawCode_";
    private static final String sskIncludeOnTranscript_PARAM = "sskIncludeOnTranscript_";

    // Student School fields
    private static final String SSK_SCH_OID_COLUMN = StudentSchool.COL_SCHOOL_OID;
    private static final String SSK_ASSOC_TYPE_COLUMN = StudentSchool.COL_TYPE;
    private static final String SSK_ATT_MAG_TYPE_COLUMN = StudentSchool.COL_ATTENDANCE_MANAGEMENT_TYPE;
    private static final String SSK_START_DATE_COLUMN = StudentSchool.COL_START_DATE;
    private static final String SSK_END_DATE_COLUMN = StudentSchool.COL_END_DATE;

    private String SSK_OOD_SCH_NAME_COLUMN;
    private String SSK_OOD_ADDRESS_COLUMN;
    private String SSK_ENTRY_CODE_COLUMN;
    private String SSK_WITHDRAW_CODE_COLUMN;
    private String SSK_INCLUDE_ON_TRANSCRIPT_COLUMN;

    // Student School Alias Fields
    private static final String ALIAS_SSK_ENTRY_CODE = "DOE SCHOOL ENTRY CODE";
    private static final String ALIAS_SSK_WITHDRAW_CODE = "DOE SCHOOL EXIT CODE";
    private static final String ALIAS_SSK_SCHOOL_ADDRESS = "DOE SCHOOL ADDRESS";
    private static final String ALIAS_SSK_SCHOOL_NAME = "DOE SCHOOL NAME";
    private static final String ALIAS_SSK_INCLUDE_ON_TRANSCRIPT = "DOE INCLUDE ON TRANSCRIPT";

    private static final int MAX_STUDENTSCHOOLS = 10;

    private ModelBroker m_broker;
    private SisStudent m_currentStudent;
    private Map<String, String> m_studentSchoolColumns;
    private Map<String, Map<String, StudentSchool>> m_studentSchools;
    private Map<String, String> m_contextOidMap;

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.x2dev.sis.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        int count = 0;
        loadStudentSchoolAssociations();

        if (m_currentStudent != null) {
            for (int i = 1; i <= MAX_STUDENTSCHOOLS; i++) {
                /*
                 * Check to see if course information was entered.
                 */
                String year = (String) getParameter(sskYear_PARAM + i);
                String contextOid = m_contextOidMap.get(year);

                String schoolOid = (String) getParameter(sskSchOid_PARAM + i);

                PlainDate startDate = (PlainDate) getParameter(sskStartDate_PARAM + i);
                PlainDate endDate = (PlainDate) getParameter(sskEndDate_PARAM + i);

                if (!StringUtils.isEmpty(schoolOid) && !StringUtils.isEmpty(contextOid)) {
                    if (!inRange(m_studentSchools, schoolOid, contextOid, startDate, endDate)) {
                        StudentSchool studentSchool =
                                X2BaseBean.newInstance(StudentSchool.class, getBroker().getPersistenceKey());
                        studentSchool.setStudentOid(m_currentStudent.getOid());
                        studentSchool.setDistrictContextOid(contextOid);

                        /*
                         * Set the grade values.
                         */
                        for (String beanPath : m_studentSchoolColumns.keySet()) {
                            String parameter = m_studentSchoolColumns.get(beanPath);
                            Object value = null;
                            if (getParameter(parameter + i) != null) {
                                if (parameter.contains("Type") || parameter.contains("Include")) {
                                    value = getParameter(parameter + i);
                                } else if (parameter.contains("Date")) {
                                    value = getParameter(parameter + i);
                                } else {
                                    value = getParameter(parameter + i);
                                }

                                studentSchool.setFieldValueByBeanPath(beanPath, value);
                            }
                        }

                        if (studentSchool.getStartDate() != null && studentSchool.getEndDate() != null) {
                            getBroker().saveBeanForced(studentSchool);
                            count++;

                            Map<String, StudentSchool> studentSchoolByContext = new HashMap<String, StudentSchool>();

                            if (m_studentSchools != null) {
                                if (m_studentSchools.containsKey(contextOid)) {
                                    studentSchoolByContext = m_studentSchools.get(contextOid);
                                }
                            } else {
                                m_studentSchools = new HashMap<String, Map<String, StudentSchool>>();
                            }

                            studentSchoolByContext.put(schoolOid, studentSchool);
                            m_studentSchools.put(contextOid, studentSchoolByContext);
                        }
                    } else {
                        logMessage("The inputted StudentSchool record with a schoolOid = " + schoolOid
                                + ", contextOid = " + contextOid +
                                ", startDate = " + startDate + ", endDate = " + endDate + " is within a current range");
                    }
                }
            }
        }

        logMessage(count + " StudentSchool record" + (count != 1 ? "" : "s") + " created.");
    }

    /**
     * Gets the broker.
     *
     * @return X 2 broker
     * @see com.x2dev.sis.tools.ToolJavaSource#getBroker()
     */
    @Override
    protected X2Broker getBroker() {
        return m_broker;
    }

    /**
     * Initialize.
     *
     * @see com.x2dev.sis.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        m_broker = new ModelBroker(getPrivilegeSet());

        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField entryCodeField = dictionary.findDataDictionaryFieldByAlias(ALIAS_SSK_ENTRY_CODE);
        DataDictionaryField exitCodeField = dictionary.findDataDictionaryFieldByAlias(ALIAS_SSK_WITHDRAW_CODE);
        DataDictionaryField schoolAddressField = dictionary.findDataDictionaryFieldByAlias(ALIAS_SSK_SCHOOL_ADDRESS);
        DataDictionaryField schoolNameField = dictionary.findDataDictionaryFieldByAlias(ALIAS_SSK_SCHOOL_NAME);
        DataDictionaryField includeOnTranscript =
                dictionary.findDataDictionaryFieldByAlias(ALIAS_SSK_INCLUDE_ON_TRANSCRIPT);

        SSK_ENTRY_CODE_COLUMN = entryCodeField.getJavaName();
        SSK_WITHDRAW_CODE_COLUMN = exitCodeField.getJavaName();
        SSK_OOD_ADDRESS_COLUMN = schoolAddressField.getJavaName();
        SSK_OOD_SCH_NAME_COLUMN = schoolNameField.getJavaName();
        SSK_INCLUDE_ON_TRANSCRIPT_COLUMN = includeOnTranscript.getJavaName();

        m_studentSchoolColumns = new HashMap<String, String>(10);
        m_studentSchoolColumns.put(SSK_SCH_OID_COLUMN, sskSchOid_PARAM);
        m_studentSchoolColumns.put(SSK_ASSOC_TYPE_COLUMN, sskAssocType_PARAM);
        m_studentSchoolColumns.put(SSK_ATT_MAG_TYPE_COLUMN, sskAttMagType_PARAM);
        m_studentSchoolColumns.put(SSK_START_DATE_COLUMN, sskStartDate_PARAM);
        m_studentSchoolColumns.put(SSK_END_DATE_COLUMN, sskEndDate_PARAM);
        m_studentSchoolColumns.put(SSK_OOD_SCH_NAME_COLUMN, sskOodName_PARAM);
        m_studentSchoolColumns.put(SSK_OOD_ADDRESS_COLUMN, sskOodAddr_PARAM);
        m_studentSchoolColumns.put(SSK_ENTRY_CODE_COLUMN, sskEntryCode_PARAM);
        m_studentSchoolColumns.put(SSK_WITHDRAW_CODE_COLUMN, sskWithdrawCode_PARAM);
        m_studentSchoolColumns.put(SSK_INCLUDE_ON_TRANSCRIPT_COLUMN, sskIncludeOnTranscript_PARAM);

        /*
         * Initialize context map
         */
        m_contextOidMap = new HashMap<String, String>(64);

        QueryByCriteria query = new QueryByCriteria(DistrictSchoolYearContext.class);
        QueryIterator contexts = getBroker().getIteratorByQuery(query);

        try {
            while (contexts.hasNext()) {
                DistrictSchoolYearContext context = (DistrictSchoolYearContext) contexts.next();
                m_contextOidMap.put(String.valueOf(context.getSchoolYear()), context.getOid());
            }
        } finally {
            contexts.close();
        }
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @see com.x2dev.sis.tools.ToolJavaSource#saveState(com.x2dev.sis.web.UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) {
        /*
         * If we're in the context of a single student, run for just that student.
         */
        m_currentStudent = userData.getCurrentRecord(SisStudent.class);
    }

    /**
     * Loads a map of StudentSchool records keyed on contextOid,schoolOid.
     */
    private void loadStudentSchoolAssociations() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(StudentSchool.COL_STUDENT_OID, m_currentStudent.getOid());

        QueryByCriteria query = new QueryByCriteria(StudentSchool.class, criteria);

        m_studentSchools = getBroker().getNestedMapByQuery(query, StudentSchool.COL_DISTRICT_CONTEXT_OID,
                StudentSchool.COL_SCHOOL_OID, 10, 10);
    }

    /**
     * Determines if either enteredStartDate or enteredEndDate is in the StudentSchool records date
     * range.
     *
     * @param studentSchools Map<String,Map<String,StudentSchool>>
     * @param schoolOid String
     * @param contextOid String
     * @param enteredStartDate PlainDate
     * @param enteredEndDate PlainDate
     * @return inRange
     */
    private boolean inRange(Map<String, Map<String, StudentSchool>> studentSchools,
                            String schoolOid,
                            String contextOid,
                            PlainDate enteredStartDate,
                            PlainDate enteredEndDate) {
        boolean inRange = false;

        if (studentSchools != null) {
            Map<String, StudentSchool> studentSchoolByContext = studentSchools.get(contextOid);

            if (studentSchoolByContext != null && studentSchoolByContext.containsKey(schoolOid)) {
                StudentSchool studentSchool = studentSchoolByContext.get(schoolOid);

                if (studentSchool != null) {
                    PlainDate startDate = studentSchool.getStartDate();
                    PlainDate endDate = studentSchool.getEndDate();

                    if (isBetween(startDate, endDate, enteredStartDate) ||
                            isBetween(startDate, endDate, enteredEndDate)) {
                        inRange = true;
                    }
                }
            }
        }

        return inRange;
    }

    /**
     * Determines if the enteredDate is between the passed start and end dates.
     *
     * @param startDate PlainDate
     * @param endDate PlainDate
     * @param enteredDate PlainDate
     * @return isBetween
     */
    private boolean isBetween(PlainDate startDate, PlainDate endDate, PlainDate enteredDate) {
        boolean isBetween = false;

        if (startDate != null && endDate != null && enteredDate != null) {
            if ((startDate.before(enteredDate) || startDate.equals(enteredDate)) &&
                    (endDate.after(enteredDate)) || endDate.equals(enteredDate)) {
                isBetween = true;
            }
        }

        return isBetween;
    }
}
