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
package com.x2dev.reports.statereporting.ny;

import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.SisUser;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2002-2014 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;

/**
 * Prepares the data for the NY Immunization Status report. This report counts compliance totals for
 * immunizations
 * by invoking the immunizations state export.
 *
 * @author Follett Software Company
 */
public class NYSchoolImmunizationStatusData extends ReportJavaSourceNet {
    /**
     * Aliases
     */
    // private static final String ALIAS_EXEMPT_REASON = "DOE IMMUN EXEMPT";

    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Value for the "active only" input parameter. This value is a Boolean.
     */
    public static final String ACTIVE_ONLY_PARAM = "activeOnly";

    /**
     * Value for the "required only" input parameter. This value is a Boolean.
     */
    // public static final String GRADE_LEVEL_PARAM = "gradeLevel";

    /**
     * Value for the "query by" input parameter. This value is a String.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Value for the "query string" input parameter. This value is a String.
     */
    public static final String QUERY_STRING_PARAM = "queryString";

    /**
     * Value for the "required only" input parameter. This value is a Boolean.
     */
    public static final String REQUIRED_ONLY_PARAM = "requiredOnly";

    /*
     * Grid fields
     */
    private static final String FIELD_DEFINITION = "definition";
    private static final String FIELD_SCHOOL = "school";

    private static final String PROCEDURE_ID = "procedureId";

    private static final String DATA_FIELD_SCHOOL = "School";
    private static final String DATA_FIELD_GRADE_LEVEL = "Grade level";
    private static final String DATA_FIELD_DT = "Diptheria/Tetanus";
    private static final String DATA_FIELD_DIPTHERIA = "Diptheria";
    private static final String DATA_FIELD_TETANUS = "Tetanus";
    private static final String DATA_FIELD_POLIO = "Polio";
    private static final String DATA_FIELD_PERTUSSIS = "Pertussis";
    private static final String DATA_FIELD_MMR = "MMR";
    private static final String DATA_FIELD_MEASLES = "Measles";
    private static final String DATA_FIELD_MUMPS = "Mumps";
    private static final String DATA_FIELD_RUBELLA = "Reubella";
    private static final String DATA_FIELD_HEPB = "Hep B";
    private static final String DATA_FIELD_VARICELLA = "Varicella";
    private static final String DATA_FIELD_HIB = "HIB";
    private static final String DATA_FIELD_TDAP = "Tdap";
    // private static final String DATA_FIELD_PNUEMOCOCCAL = "Pneumococcal";

    private int m_schoolPosition = -1;
    private int m_gradeLevelPosition = -1;
    private int m_dtPosition = -1;
    private int m_polioPosition = -1;
    private int m_pertussisPosition = -1;
    private int m_mmrPosition = -1;
    private int m_hepbPosition = -1;
    private int m_varicellaPosition = -1;
    private int m_hibPosition = -1;
    private int m_tdapPosition = -1;
    // private int m_pnuemococcalPosition = -1;

    private Collection<StateReportValidationError> m_initErrors = null;
    private StateReportData m_reportData = null;

    /*
     * Report parameters
     */
    private static final String PARAM_CURRENT_USER = "person";
    private static final String PARAM_SCHOOL_TOTALS = "schoolTotals";
    private static final String PARAM_STUDENT_EXEMPT = "exemptMap";
    private static final String PARAM_STUDENT_COMPLIANT = "compliantMap";
    private static final String PARAM_STUDENT_COMPLIANT_WITH_RULES = "compliantWithRulesMap";
    private static final String PARAM_STUDENT_NO_IMMUNE = "noImmuneMap";
    private static final String PARAM_STUDENT_RELIGIOUS = "religiousMap";
    private static final String PARAM_STUDENT_MEDICAL = "medicalMap";
    private static final String PARAM_STUDENT_PERSONAL = "personalMap";
    // private static final String PARAM_GRADE_LEVEL = "gradeLevel";
    private static final String PARAM_STUDENT_ALL = "allMap";

    private SisUser m_currentUser;
    private Map m_gradeLevelMap;
    private Collection m_studentsExempt;
    private Collection m_studentsCompliantWithRules;
    private Collection m_studentsCompliant;
    private Collection m_studentsWithout;
    private Collection m_studentsReligiousExempt;
    private Collection m_studentsMedicalExempt;
    private Collection m_studentsPersonalExempt;
    private Collection m_studentsAll;
    private Collection m_entitiesAll;

    // private GradeLevelHistory m_gradeLevelHistory;
    protected String m_fieldImmunizationSeriesExempt;

    List<String> m_diseases = Arrays.asList(
            DATA_FIELD_DT, DATA_FIELD_DIPTHERIA, DATA_FIELD_TETANUS,
            "Polio", "Pertussis",
            DATA_FIELD_MMR, DATA_FIELD_MEASLES, DATA_FIELD_MUMPS, DATA_FIELD_RUBELLA,
            "Hep B", "Varicella", DATA_FIELD_HIB, DATA_FIELD_TDAP /* "Pneumococcal" */);

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws X2BaseException {
        ReportDataGrid grid = new ReportDataGrid(20, 10);

        loadStudents();
        loadGradeLevelMap();

        for (String disease : m_diseases) {
            grid.append();
            grid.set(FIELD_DEFINITION, disease);
            grid.set(FIELD_SCHOOL, getSchool());

            setNeededTotals(disease, grid);
        }

        grid.beforeTop();

        setStudentHealthInformation();
        addParameter(PARAM_SCHOOL_TOTALS, getSchoolTotals());

        if (m_currentUser != null) {
            addParameter(PARAM_CURRENT_USER, m_currentUser.getPerson());
        }

        // String gradeLevel = ((String) getParameter(GRADE_LEVEL_PARAM)).toString();
        // addParameter(PARAM_GRADE_LEVEL, gradeLevel);

        return grid;
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.
     *      UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) {
        m_currentUser = userData.getCurrentRecord(SisUser.class);
    }

    /**
     * Counts grade level totals of passed students.
     *
     * @param studentIterator Iterator
     * @return Map of a Map of totals keyed to column #, keyed to school oid
     */
    private Map buildSchoolCounts(Iterator studentIterator) {
        Map schoolMap = new HashMap(32);
        Map totalsMap = new HashMap(8);

        SisSchool lastSchool = null;
        while (studentIterator.hasNext()) {
            SisStudent student = (SisStudent) studentIterator.next();

            SisSchool school = student.getSchool();
            String column = (String) m_gradeLevelMap.get(student.getGradeLevel());

            if (lastSchool == null || !school.equals(lastSchool)) {
                if (lastSchool != null) {
                    schoolMap.put(lastSchool.getOid(), totalsMap);
                }

                totalsMap = new HashMap(8);
            }

            /*
             * Update totals
             */
            Integer total = (Integer) totalsMap.get(column);
            if (total == null) {
                totalsMap.put(column, Integer.valueOf(1));
            } else {
                totalsMap.put(column, Integer.valueOf(total.intValue() + 1));
            }

            lastSchool = school;
        }

        /*
         * Add last school
         */
        if (lastSchool != null) {
            schoolMap.put(lastSchool.getOid(), totalsMap);
        }

        return schoolMap;
    }

    /**
     * Returns a Map of Longs representing total active students in a school keyed to their School
     * OID.
     *
     * @return Map
     */
    private Map getSchoolTotals() {
        HashMap schoolToStudentTotals = new HashMap();


        Iterator iterator = m_studentsAll.iterator();
        while (iterator.hasNext()) {
            SisStudent student = (SisStudent) iterator.next();
            String schoolOid = student.getSchoolOid();
            Long studentCount = (Long) schoolToStudentTotals.get(schoolOid);
            if (studentCount != null) {
                studentCount = Long.valueOf(studentCount.intValue() + 1);
            } else {
                studentCount = Long.valueOf(1);
            }
            schoolToStudentTotals.put(schoolOid, studentCount);
        }

        return schoolToStudentTotals;
    }


    /**
     * Returns a map of the report columns keyed to the corresponding grade level.
     *
     * @return Map
     */
    private void loadGradeLevelMap() {
        m_gradeLevelMap = new HashMap(32);

        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getUser().getPersistenceKey());
        DataDictionaryField field =
                dictionary.findDataDictionaryField(SisStudent.class.getName(), SisStudent.COL_GRADE_LEVEL);

        Iterator codeIterator = field.getReferenceTable().getReferenceCodes().iterator();
        while (codeIterator.hasNext()) {
            ReferenceCode code = (ReferenceCode) codeIterator.next();

            int offset = -10000;
            if (StringUtils.isInteger(code.getFieldA005())) {
                offset = Integer.parseInt(code.getFieldA005());
            }

            switch (offset) {
                case -2: // PK
                    m_gradeLevelMap.put(code.getCode(), "0");
                    break;

                case -1: // PK
                    m_gradeLevelMap.put(code.getCode(), "0");
                    break;

                case 0: // K
                    m_gradeLevelMap.put(code.getCode(), "1");
                    break;

                case 1:
                    m_gradeLevelMap.put(code.getCode(), "2");
                    break;

                case 2:
                    m_gradeLevelMap.put(code.getCode(), "3");
                    break;

                case 3:
                    m_gradeLevelMap.put(code.getCode(), "4");
                    break;

                case 4:
                    m_gradeLevelMap.put(code.getCode(), "5");
                    break;

                case 5:
                    m_gradeLevelMap.put(code.getCode(), "6");
                    break;

                case 6:
                    m_gradeLevelMap.put(code.getCode(), "7");
                    break;

                case 7:
                    m_gradeLevelMap.put(code.getCode(), "8");
                    break;

                case 8:
                    m_gradeLevelMap.put(code.getCode(), "9");
                    break;

                case 9:
                    m_gradeLevelMap.put(code.getCode(), "10");
                    break;

                case 10:
                    m_gradeLevelMap.put(code.getCode(), "11");
                    break;

                case 11:
                    m_gradeLevelMap.put(code.getCode(), "12");
                    break;

                case 12:
                    m_gradeLevelMap.put(code.getCode(), "13");
                    break;
            }
        }
    }

    /**
     * Loads the collection of students with immunizations, without immunizations, and with
     * exemptions.
     *
     * @throws X2BaseException exception
     */
    private void loadStudents() throws X2BaseException {
        // Ok, so here let me parse out the data. I probably want to set things aside in the same
        // general fashion so I can use the methods already here.

        String procedureId = (String) getParameter(PROCEDURE_ID);
        m_initErrors = new ArrayList<StateReportValidationError>();

        // Lookup State report source data procedure
        m_reportData = StateReportData.getReportDataFromProcedure(procedureId, getBroker(), m_initErrors);

        if (m_reportData != null && m_initErrors.size() == 0) {
            try {
                // Initialize the report data object.
                m_reportData.setBroker(getBroker());
                m_reportData.setCurrentContext(getCurrentContext());
                m_reportData.setOrganization(getOrganization());
                m_reportData.setPrivilegeSet(getPrivilegeSet());
                m_reportData.setSchoolContext(isSchoolContext());
                m_reportData.setSchool(getSchool());
                m_reportData.setParameters(getParameters());
                m_reportData.setUser(getUser());
                m_reportData.initializeExport();
            } catch (X2BaseException x2be) {
                String init_msg = "Failure initializing data structure in WABasicSupport";
                m_initErrors.add(new StateReportValidationError(init_msg, init_msg, init_msg, x2be.getMessage()));

                throw x2be;
            }

            m_initErrors.addAll(m_reportData.getSetupErrors());
        }

        if (m_reportData != null && m_reportData.open() && initializeFieldPositions()) {
            // build the iterators manually based on each students status
            StateReportEntity entity = null;
            m_entitiesAll = new ArrayList<StateReportEntity>();
            m_studentsAll = new ArrayList<SisStudent>();
            m_studentsCompliant = new ArrayList<SisStudent>();
            m_studentsCompliantWithRules = new ArrayList<SisStudent>();
            m_studentsWithout = new ArrayList<SisStudent>();
            m_studentsExempt = new ArrayList<SisStudent>();
            m_studentsReligiousExempt = new ArrayList<SisStudent>();
            m_studentsMedicalExempt = new ArrayList<SisStudent>();
            m_studentsPersonalExempt = new ArrayList<SisStudent>();
            while ((entity = m_reportData.next()) != null) {
                entity.preProcess();
                SisStudent student = (SisStudent) entity.getBean();

                m_entitiesAll.add(entity);
                m_studentsAll.add(student);
                if (isEntityCompliant(entity)) {
                    m_studentsCompliant.add(student);
                } else {
                    m_studentsWithout.add(student);
                }

                if (isEntityCompliantWithRules(entity)) {
                    m_studentsCompliantWithRules.add(student);
                }

                if (isEntityExempt(entity)) {
                    m_studentsExempt.add(student);
                }
                if (religiousExempt(entity)) {
                    m_studentsReligiousExempt.add(student);
                }
                if (medicalExempt(entity)) {
                    m_studentsMedicalExempt.add(student);
                }
                if (personalExempt(entity)) {
                    m_studentsPersonalExempt.add(student);
                }

                entity.postProcess();
            }
        }
    }

    /**
     * Medical exempt.
     *
     * @param entity StateReportEntity
     * @return true, if successful
     */
    private boolean medicalExempt(StateReportEntity entity) {
        return checkForAny(entity, "M");
    }

    /**
     * Religious exempt.
     *
     * @param entity StateReportEntity
     * @return true, if successful
     */
    private boolean religiousExempt(StateReportEntity entity) {
        return checkForAny(entity, "R");
    }

    /**
     * Personal exempt.
     *
     * @param entity StateReportEntity
     * @return true, if successful
     */
    private boolean personalExempt(StateReportEntity entity) {
        return checkForAny(entity, "P");
    }

    /**
     * Checks if is entity exempt.
     *
     * @param entity StateReportEntity
     * @return true, if is entity exempt
     */
    private boolean isEntityExempt(StateReportEntity entity) {
        return checkForAny(entity, "E");
    }

    /**
     * Checks if is entity compliant.
     *
     * @param entity StateReportEntity
     * @return true, if is entity compliant
     */
    private boolean isEntityCompliant(StateReportEntity entity) {
        return !checkForAny(entity, "N");
    }

    /**
     * Checks if is entity compliant with rules.
     *
     * @param entity StateReportEntity
     * @return true, if is entity compliant with rules
     */
    private boolean isEntityCompliantWithRules(StateReportEntity entity) {
        return checkForAll(entity, "C");
    }

    /**
     * Check for any.
     *
     * @param entity StateReportEntity
     * @param checkValue String
     * @return true, if successful
     */
    private boolean checkForAny(StateReportEntity entity, String checkValue) {
        for (String disease : m_diseases) {
            if (checkValue.equals(getStudentStatusForDisease(entity, disease))) {
                return true;
            }
        }
        return false;
    }

    /**
     * Check for all.
     *
     * @param entity StateReportEntity
     * @param checkValue String
     * @return true, if successful
     */
    private boolean checkForAll(StateReportEntity entity, String checkValue) {
        for (String disease : m_diseases) {
            if (!checkValue.equals(getStudentStatusForDisease(entity, disease))) {
                return false;
            }
        }
        return true;
    }

    /**
     * Gets the student status for disease.
     *
     * @param entity StateReportEntity
     * @param disease String
     * @return String
     */
    private String getStudentStatusForDisease(StateReportEntity entity, String disease) {
        int position = -1;
        if (DATA_FIELD_DT.equals(disease)
                || DATA_FIELD_DIPTHERIA.equals(disease)
                || DATA_FIELD_TETANUS.equals(disease)) {
            position = m_dtPosition;
        } else if (DATA_FIELD_POLIO.equals(disease)) {
            position = m_polioPosition;
        } else if (DATA_FIELD_PERTUSSIS.equals(disease)) {
            position = m_pertussisPosition;
        } else if (DATA_FIELD_MMR.equals(disease)
                || DATA_FIELD_MEASLES.equals(disease)
                || DATA_FIELD_MUMPS.equals(disease)
                || DATA_FIELD_RUBELLA.equals(disease)) {
            position = m_mmrPosition;
        } else if (DATA_FIELD_HEPB.equals(disease)) {
            position = m_hepbPosition;
        } else if (DATA_FIELD_VARICELLA.equals(disease)) {
            position = m_varicellaPosition;
        } else if (DATA_FIELD_HIB.equals(disease)) {
            position = m_hibPosition;
        } else if (DATA_FIELD_TDAP.equals(disease)) {
            position = m_tdapPosition;
        }
        // else if (DATA_FIELD_PNUEMOCOCCAL.equals(disease))
        // {
        // position = m_pnuemococcalPosition;
        // }
        return entity.getFieldValue(position);
    }

    /**
     * Initializes the field position members for the data source.
     *
     * @return true, if successful
     */
    private boolean initializeFieldPositions() {
        for (int pos = 0; pos < m_reportData.getFieldCount(); pos++) {
            FieldDefinition field = m_reportData.getFieldDefinition(pos);
            String fieldName = field.getFieldId();
            if (DATA_FIELD_SCHOOL.equals(fieldName)) {
                m_schoolPosition = pos;
            } else if (DATA_FIELD_DT.equals(fieldName)) {
                m_dtPosition = pos;
            } else if (DATA_FIELD_GRADE_LEVEL.equals(fieldName)) {
                m_gradeLevelPosition = pos;
            } else if (DATA_FIELD_HEPB.equals(fieldName)) {
                m_hepbPosition = pos;
            } else if (DATA_FIELD_MMR.equals(fieldName)) {
                m_mmrPosition = pos;
            } else if (DATA_FIELD_PERTUSSIS.equals(fieldName)) {
                m_pertussisPosition = pos;
            } else if (DATA_FIELD_POLIO.equals(fieldName)) {
                m_polioPosition = pos;
            } else if (DATA_FIELD_VARICELLA.equals(fieldName)) {
                m_varicellaPosition = pos;
            } else if (DATA_FIELD_HIB.equals(fieldName)) {
                m_hibPosition = pos;
            } else if (DATA_FIELD_TDAP.equals(fieldName)) {
                m_tdapPosition = pos;
            }
        }
        if (m_schoolPosition < 0 ||
                m_gradeLevelPosition < 0 ||
                m_pertussisPosition < 0 ||
                m_mmrPosition < 0 ||
                m_hepbPosition < 0 ||
                m_dtPosition < 0 ||
                m_polioPosition < 0 ||
                m_varicellaPosition < 0 ||
                m_hibPosition < 0 ||
                m_tdapPosition < 0) {
            return false;
        }
        return true;
    }

    /**
     * Counts the number of students that have not met the compliance rules of the given
     * HealthImmunizationDefitition.
     *
     * @param disease String
     * @param grid ReportDataGrid
     */
    private void setNeededTotals(String disease, ReportDataGrid grid) {
        if (m_entitiesAll != null) {
            Iterator entityIterator = m_entitiesAll.iterator();
            while (entityIterator.hasNext()) {
                StateReportEntity entity = (StateReportEntity) entityIterator.next();
                SisStudent student = (SisStudent) entity.getBean();
                String column = (String) m_gradeLevelMap.get(student.getGradeLevel());

                // If the student's grade level is one that is being reported....
                if (!StringUtils.isEmpty(column)) {
                    if (!"N".equals(getStudentStatusForDisease(entity, disease))) {
                        Integer count = (Integer) grid.get(column);
                        if (count == null) {
                            grid.set(column, Integer.valueOf(1));
                        } else {
                            grid.set(column, Integer.valueOf(count.intValue() + 1));
                        }
                    }
                }
            }
        }
    }

    /**
     * Counts totals of students with immunizations, without immunizations, and with exemptions.
     */
    private void setStudentHealthInformation() {
        Map studentMap;

        studentMap = buildSchoolCounts(m_studentsExempt.iterator());
        addParameter(PARAM_STUDENT_EXEMPT, studentMap);

        studentMap = buildSchoolCounts(m_studentsCompliant.iterator());
        addParameter(PARAM_STUDENT_COMPLIANT, studentMap);

        studentMap = buildSchoolCounts(m_studentsCompliantWithRules.iterator());
        addParameter(PARAM_STUDENT_COMPLIANT_WITH_RULES, studentMap);

        studentMap = buildSchoolCounts(m_studentsWithout.iterator());
        addParameter(PARAM_STUDENT_NO_IMMUNE, studentMap);

        studentMap = buildSchoolCounts(m_studentsReligiousExempt.iterator());
        addParameter(PARAM_STUDENT_RELIGIOUS, studentMap);

        studentMap = buildSchoolCounts(m_studentsMedicalExempt.iterator());
        addParameter(PARAM_STUDENT_MEDICAL, studentMap);

        studentMap = buildSchoolCounts(m_studentsPersonalExempt.iterator());
        addParameter(PARAM_STUDENT_PERSONAL, studentMap);

        studentMap = buildSchoolCounts(m_studentsAll.iterator());
        addParameter(PARAM_STUDENT_ALL, studentMap);
    }
}
