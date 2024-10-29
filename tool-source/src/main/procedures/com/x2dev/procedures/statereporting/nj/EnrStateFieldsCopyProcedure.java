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
package com.x2dev.procedures.statereporting.nj;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.util.Collections;
import java.util.LinkedList;
import java.util.Map;
import java.util.Map.Entry;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * This procedure will process all active students with no enrollment activity over the summer.
 * It will and a withdrawal and entry for end of school year and first day of next school year.
 *
 * @author X2 Development Corporation
 */
public class EnrStateFieldsCopyProcedure extends ProcedureJavaSource {
    private static final String ALIAS_ENR_ATTENDING_SCHOOL = "DOE ATTENDING SCHOOL";
    private static final String ALIAS_ENR_CLASSIFIER = "DOE ENROLL CLASSIFIER";
    private static final String ALIAS_ENR_IN_DISTR_PLACE = "DOE IN DISTRICT PLACEMENT";
    private static final String ALIAS_ENR_MUNICIPAL_CODE = "DOE MUNICIPAL CODE";
    private static final String ALIAS_ENR_RECEIVING_SCHOOL = "DOE RECEIVING SCHOOL";
    private static final String ALIAS_ENR_RESIDING_SCHOOL = "DOE RESIDING SCHOOL";
    private static final String ALIAS_ENR_TUITION_CODE = "DOE TUITION CODE";
    private static final String ALIAS_SKL_STATE_ID = "StateID";

    private DataDictionary m_dictionary;
    private String m_fieldMunicipalCode;
    private String m_fieldClassifier;
    private String m_fieldAttSchool;
    private String m_fieldEnrInDistrPlace;
    private String m_fieldRecSchool;
    private String m_fieldResSchool;
    private String m_fieldSklStateId;
    private String m_fieldTuitionCode;
    private boolean m_initError = false;

    /**
     * Maps
     */
    private Map<String, LinkedList<StudentEnrollment>> m_newEnrollmentsMap;
    private Map<String, LinkedList<StudentEnrollment>> m_priorEnrollmentsMap;

    /**
     * Translates an alias into a Java bean path name. An initialization error will be logged
     * if the alias does not exist.
     *
     * @param alias String
     * @param required boolean
     * @return String
     */
    public String translateAliasToJavaName(String alias, boolean required) {
        String javaName = null;

        DataDictionaryField field = m_dictionary.findDataDictionaryFieldByAlias(alias);
        if (field != null) {
            javaName = field.getJavaName();
        } else if (required) {
            m_initError = true;
            logMessage("Alias = " + alias + " is not found in Data Dictionary.");
        }

        return javaName;
    }

    /**
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        if (!m_initError) {
            for (Entry<String, LinkedList<StudentEnrollment>> entry : m_newEnrollmentsMap.entrySet()) {
                LinkedList<StudentEnrollment> newEnrs = entry.getValue();
                StudentEnrollment newEnr = null;
                if (newEnrs != null && !newEnrs.isEmpty()) {
                    newEnr = newEnrs.getFirst();
                }
                String newEnrSklStateId = (String) newEnr.getSchool().getFieldValueByBeanPath(m_fieldSklStateId);
                boolean isNewEnrStateIdBlank = StringUtils.isEmpty(newEnrSklStateId) ? true : false;
                StudentEnrollment priorEnr = null;
                LinkedList<StudentEnrollment> priorEnrs = m_priorEnrollmentsMap.get(entry.getKey());
                if (priorEnrs != null && !priorEnrs.isEmpty()) {
                    priorEnr = priorEnrs.getFirst();
                }
                if (newEnr != null) {
                    String newTuitionCode = (String) newEnr.getFieldValueByBeanPath(m_fieldTuitionCode);
                    String newClassifierCode = (String) newEnr.getFieldValueByBeanPath(m_fieldClassifier);
                    String newMunicipaleCode = (String) newEnr.getFieldValueByBeanPath(m_fieldMunicipalCode);
                    String newAttSchool = (String) newEnr.getFieldValueByBeanPath(m_fieldAttSchool);
                    String newRecSchool = (String) newEnr.getFieldValueByBeanPath(m_fieldRecSchool);
                    String newResSchool = (String) newEnr.getFieldValueByBeanPath(m_fieldResSchool);
                    if (StringUtils.isEmpty(newResSchool) &&
                            StringUtils.isEmpty(newTuitionCode) &&
                            StringUtils.isEmpty(newMunicipaleCode) &&
                            StringUtils.isEmpty(newAttSchool) &&
                            StringUtils.isEmpty(newRecSchool) &&
                            StringUtils.isEmpty(newClassifierCode)) {
                        if (priorEnr != null) {
                            String priopTuition = (String) priorEnr.getFieldValueByBeanPath(m_fieldTuitionCode);
                            String priopClassifier = (String) priorEnr.getFieldValueByBeanPath(m_fieldClassifier);

                            String priorSklStateId =
                                    (String) priorEnr.getSchool().getFieldValueByBeanPath(m_fieldSklStateId);
                            if ((!isNewEnrStateIdBlank && newEnrSklStateId.equals(priorSklStateId))
                                    || isNewEnrStateIdBlank) {
                                newEnr.setFieldValueByBeanPath(m_fieldEnrInDistrPlace,
                                        priorEnr.getFieldValueByBeanPath(m_fieldEnrInDistrPlace));
                            }
                            if (!StringUtils.isEmpty(priopClassifier)) {
                                newEnr.setFieldValueByBeanPath(m_fieldTuitionCode, priopTuition);
                                newEnr.setFieldValueByBeanPath(m_fieldMunicipalCode,
                                        priorEnr.getFieldValueByBeanPath(m_fieldMunicipalCode));
                                newEnr.setFieldValueByBeanPath(m_fieldClassifier,
                                        priorEnr.getFieldValueByBeanPath(m_fieldClassifier));
                                // Set attending school
                                if (isNewEnrStateIdBlank || "Shared-Time".equals(priopClassifier)) {
                                    newEnr.setFieldValueByBeanPath(m_fieldAttSchool,
                                            priorEnr.getFieldValueByBeanPath(m_fieldAttSchool));
                                } else if ("Full-Time".equals(priopClassifier)) {
                                    newEnr.setFieldValueByBeanPath(m_fieldAttSchool,
                                            newEnr.getSchool().getFieldValueByBeanPath(m_fieldSklStateId));
                                }
                                // Set receiving school
                                if (isNewEnrStateIdBlank) {
                                    String priorRecSkl =
                                            (String) priorEnr.getFieldValueByBeanPath(m_fieldRecSchool);
                                    newEnr.setFieldValueByBeanPath(m_fieldRecSchool, priorRecSkl);
                                } else {
                                    newEnr.setFieldValueByBeanPath(m_fieldRecSchool,
                                            newEnr.getSchool().getFieldValueByBeanPath(m_fieldSklStateId));
                                }
                                // Set residing school
                                if (!isNewEnrStateIdBlank && StringUtils.isEmpty(priopTuition)
                                        && newEnr.getSchool() != null && priorEnr.getSchool() != null
                                        && priorEnr.getSchool().getFieldValueByBeanPath(m_fieldSklStateId) != null
                                        && !priorEnr.getSchool().getFieldValueByBeanPath(m_fieldSklStateId)
                                                .equals(newEnr.getSchool()
                                                        .getFieldValueByBeanPath(m_fieldSklStateId))) {
                                    newEnr.setFieldValueByBeanPath(m_fieldResSchool,
                                            newEnr.getSchool().getFieldValueByBeanPath(m_fieldSklStateId));
                                } else {
                                    newEnr.setFieldValueByBeanPath(m_fieldResSchool,
                                            priorEnr.getFieldValueByBeanPath(m_fieldResSchool));
                                }
                            } else {
                                logMessage(
                                        "EMPTY CLASSIFIER. Enrollment classifier is empty for enrollment in the prior school year for student Name = "
                                                +
                                                newEnr.getStudent().getNameView() + ", LocalID = "
                                                + newEnr.getStudent().getLocalId());
                            }
                        } else {
                            logMessage(
                                    "NOT FOUND ENROLLMENT. Was not found entry enrollment in the prior school year for student with Name = "
                                            +
                                            newEnr.getStudent().getNameView() + ", LocalID = "
                                            + newEnr.getStudent().getLocalId());
                        }

                        if (newEnr.isDirty()) {
                            getBroker().saveBeanForced(newEnr, m_dictionary);

                            String updTuitionCode = (String) newEnr.getFieldValueByBeanPath(m_fieldTuitionCode);
                            String updClassifierCode = (String) newEnr.getFieldValueByBeanPath(m_fieldClassifier);
                            String updMunicipaleCode = (String) newEnr.getFieldValueByBeanPath(m_fieldMunicipalCode);
                            String updAttSchool = (String) newEnr.getFieldValueByBeanPath(m_fieldAttSchool);
                            String updRecSchool = (String) newEnr.getFieldValueByBeanPath(m_fieldRecSchool);
                            String updResSchool = (String) newEnr.getFieldValueByBeanPath(m_fieldResSchool);

                            logMessage("UPDATED Enrollment. SchoolName = " + newEnr.getSchool().getName() +
                                    "; StudentName = " + newEnr.getStudent().getNameView() +
                                    "; StudentID = " + newEnr.getStudent().getLocalId() +
                                    "; TuitionCode = " + updTuitionCode +
                                    "; ClassifierCode = " + updClassifierCode +
                                    "; MunicipaleCode = " + updMunicipaleCode +
                                    "; AttSchool = " + updAttSchool +
                                    "; RecSchool = " + updRecSchool +
                                    "; ResSchool = " + updResSchool);

                        }
                    } else {
                        logMessage("SKIPPED Enrollment. SchoolName = " + newEnr.getSchool().getName() +
                                "; StudentName = " + newEnr.getStudent().getNameView() +
                                "; StudentID = " + newEnr.getStudent().getLocalId() +
                                "; TuitionCode = " + newTuitionCode +
                                "; ClassifierCode = " + newClassifierCode +
                                "; MunicipaleCode = " + newMunicipaleCode +
                                "; AttSchool = " + newAttSchool +
                                "; RecSchool = " + newRecSchool +
                                "; ResSchool = " + newResSchool);
                    }
                }
            }
            Collections.sort(getMessages());
        }
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();

        initializeFields();

        if (!m_initError) {
            loadEnrollments();
        }
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.
     *      UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException {
        super.saveState(userData);
    }

    /**
     * Initialize parameters and instance variables.
     */
    private void initializeFields() {
        m_dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        m_fieldTuitionCode = translateAliasToJavaName(ALIAS_ENR_TUITION_CODE, true);
        m_fieldMunicipalCode = translateAliasToJavaName(ALIAS_ENR_MUNICIPAL_CODE, true);
        m_fieldClassifier = translateAliasToJavaName(ALIAS_ENR_CLASSIFIER, true);
        m_fieldAttSchool = translateAliasToJavaName(ALIAS_ENR_ATTENDING_SCHOOL, true);
        m_fieldRecSchool = translateAliasToJavaName(ALIAS_ENR_RECEIVING_SCHOOL, true);
        m_fieldResSchool = translateAliasToJavaName(ALIAS_ENR_RESIDING_SCHOOL, true);
        m_fieldSklStateId = translateAliasToJavaName(ALIAS_SKL_STATE_ID, true);
        m_fieldEnrInDistrPlace = translateAliasToJavaName(ALIAS_ENR_IN_DISTR_PLACE, true);
    }

    /**
     * Load a map of student enrollments by student Oid.
     *
     * @return Map<String, StudentEnrollment>
     */
    private void loadEnrollments() {
        Criteria newEnrCriteria = new Criteria();
        newEnrCriteria.addAndCriteria(StudentManager.getActiveStudentStatusCriteria(getOrganization(),
                StudentEnrollment.REL_STUDENT +
                        PATH_DELIMITER +
                        SisStudent.COL_ENROLLMENT_STATUS));
        newEnrCriteria.addEqualTo(StudentEnrollment.COL_ENROLLMENT_TYPE, StudentEnrollment.ENTRY);
        newEnrCriteria.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, getCurrentContext().getStartDate());
        newEnrCriteria.addLessOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, getCurrentContext().getEndDate());
        QueryByCriteria newEnrquery = new QueryByCriteria(StudentEnrollment.class, newEnrCriteria);
        newEnrquery.addOrderByDescending(StudentEnrollment.COL_ENROLLMENT_DATE);
        m_newEnrollmentsMap =
                getBroker().getGroupedCollectionByQuery(newEnrquery, StudentEnrollment.COL_STUDENT_OID, 2048);

        Criteria priorEnrCriteria = new Criteria();
        priorEnrCriteria.addEqualTo(StudentEnrollment.COL_ENROLLMENT_TYPE, StudentEnrollment.ENTRY);
        priorEnrCriteria.addIn(StudentEnrollment.COL_STUDENT_OID, m_newEnrollmentsMap.keySet());
        X2Criteria priorCtxCriteria = new X2Criteria();
        priorCtxCriteria.addEqualTo(DistrictSchoolYearContext.COL_SCHOOL_YEAR,
                Integer.valueOf(getCurrentContext().getSchoolYear() - 1));
        DistrictSchoolYearContext priorCtx = (DistrictSchoolYearContext) getBroker()
                .getBeanByQuery(new QueryByCriteria(DistrictSchoolYearContext.class, priorCtxCriteria));
        priorEnrCriteria.addLessOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, priorCtx.getEndDate());
        QueryByCriteria priorEnrQuery = new QueryByCriteria(StudentEnrollment.class, priorEnrCriteria);
        priorEnrQuery.addOrderByDescending(StudentEnrollment.COL_ENROLLMENT_DATE);
        m_priorEnrollmentsMap =
                getBroker().getGroupedCollectionByQuery(priorEnrQuery, StudentEnrollment.COL_STUDENT_OID,
                        2048);
    }
}
