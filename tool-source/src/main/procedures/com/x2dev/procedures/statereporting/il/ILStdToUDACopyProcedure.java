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

package com.x2dev.procedures.statereporting.il;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryUtils;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.UserDefinedTableA;
import com.x2dev.utils.X2BaseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class ILStdToUDACopyProcedure.
 *
 * @author Follett Software Company
 *
 *         Procedure which will process daily to create TRN records from SCC records.
 *         This procedure will create/update TRN records based on the information in the SCC alias
 *         fields.
 */
public class ILStdToUDACopyProcedure extends ProcedureJavaSource {

    public enum ALIASES_ER {
        ALIAS_SKL("DOE EC ER HOME RCDTS",
                "all-uda-ECRatingSchool"), ALIAS_SOC_REL("DOE EC ER SOCIAL RELATIONS",
                        "all-uda-ECSocialRelations"), ALIAS_SKILLS("DOE EC ER AQUIRE USE KNOW SK",
                                "all-uda-ECKnowledgeSkills"), ALIAS_MET_NEED("DOE EC ER ACTION TO MEET NEED",
                                        "all-uda-ECActionToMeetNeeds"), ALIAS_RAT_PARENT("DOE EC ER PARENT IN RATINGS",
                                                "all-uda-ECParentInvolved"), ALIAS_RAT_ADMIN(
                                                        "DOE EC ER CO/LEA/ADM RATINGS",
                                                        "all-uda-ECAdminParticipate"), ALIAS_RAT_TEACHER(
                                                                "DOE EC ER TEACHER IN RATINGS",
                                                                "all-uda-ECTeacherParticipate"), ALIAS_RAT_PSYHO(
                                                                        "DOE EC ER PSY/SOC IN RATINGS",
                                                                        "all-uda-ECPsychSocialWorker"), ALIAS_RAT_LANG(
                                                                                "DOE EC ER SP/LANGPATH RATINGS",
                                                                                "all-uda-ECSpeechLangPath"), ALIAS_RAT_OTHER(
                                                                                        "DOE EC ER RELATED SER RATINGS",
                                                                                        "all-uda-ECOtherProvider");

        private String m_stdAlias;
        private String m_udaAlias;

        public String getStdAlias() {
            return m_stdAlias;
        }

        public String getUdaAlias() {
            return m_udaAlias;
        }

        private ALIASES_ER(String stdAlias, String udaAlias) {
            m_stdAlias = stdAlias;
            m_udaAlias = udaAlias;
        }
    }

    public enum ALIASES_PR {
        ALIAS_SKL("DOE EC PR HOME RCDTS",
                "all-uda-ECRatingSchool"), ALIAS_SOC_REL("DOE EC PR SOCIAL RELATIONS",
                        "all-uda-ECSocialRelations"), ALIAS_SKILLS("DOE EC PR AQUIRE USE KNOW SK",
                                "all-uda-ECKnowledgeSkills"), ALIAS_MET_NEED("DOE EC PR ACTION TO MEET NEED",
                                        "all-uda-ECActionToMeetNeeds"), ALIAS_RAT_PARENT("DOE EC PR PARENT IN RATINGS",
                                                "all-uda-ECParentInvolved"), ALIAS_RAT_ADMIN(
                                                        "DOE EC PR CO/LEA/ADM RATINGS",
                                                        "all-uda-ECAdminParticipate"), ALIAS_RAT_TEACHER(
                                                                "DOE EC PR TEACHER IN RATINGS",
                                                                "all-uda-ECTeacherParticipate"), ALIAS_RAT_PSYHO(
                                                                        "DOE EC PR PSY/SOC IN RATINGS",
                                                                        "all-uda-ECPsychSocialWorker"), ALIAS_RAT_LANG(
                                                                                "DOE EC PR SP/LANGPATH RATINGS",
                                                                                "all-uda-ECSpeechLangPath"), ALIAS_RAT_OTHER(
                                                                                        "DOE EC PR RELATED SER RATINGS",
                                                                                        "all-uda-ECOtherProvider"), ALIAS_PRIM_ASM(
                                                                                                "DOE EC PR PRIMARY ASSESSMENT",
                                                                                                "all-uda-ECPRAssessment"), ALIAS_SOC_REL_PR(
                                                                                                        "DOE EC PR PROGRESS SOC RELAT",
                                                                                                        "all-uda-ECMadePRSocialRelation"), ALIAS_SKILLS_PR(
                                                                                                                "DOE EC PR PROG USE KNOWL SKLS",
                                                                                                                "all-uda-ECPRKnowledgeSkills"), ALIAS_MET_NEED_PR(
                                                                                                                        "DOE EC PR PROG MEET OWN NEEDS",
                                                                                                                        "all-uda-ECMadePRMeetNeeds");


        private String m_stdAlias;
        private String m_udaAlias;

        public String getStdAlias() {
            return m_stdAlias;
        }

        public String getUdaAlias() {
            return m_udaAlias;
        }

        private ALIASES_PR(String stdAlias, String udaAlias) {
            m_stdAlias = stdAlias;
            m_udaAlias = udaAlias;
        }
    }

    /**
     * Aliases
     */
    protected static final String ALIAS_RAT_DATE_STD_ER = "DOE EC ENTRY RATING DATE";
    protected static final String ALIAS_RAT_DATE_STD_PR = "DOE EC PROGRESS RATING DATE";
    protected static final String ALIAS_RAT_DATE_UDA = "all-uda-ECRatingDate";
    protected static final String ALIAS_RAT_TYPE_UDA = "all-uda-ECRatingType";

    /**
     * Input parameters
     */
    protected static final String INPUT_PARAM_COMMIT = "commit";

    /**
     * Other constants
     */
    protected static final String DDX_ID_RATINGS = "IL EC Outcomes";
    protected static final String RATING_TYPE_ENTRY = "Entry";
    protected static final String RATING_TYPE_PROGRESS = "Progress";
    protected static final String VALID_STATE_GRADE = "14";

    /**
     * Supporting instance variables.
     */
    protected boolean m_commit;
    protected SimpleDateFormat m_dateFormat = new SimpleDateFormat("MM/dd/yyyy");
    protected SimpleDateFormat m_dateFormatString = new SimpleDateFormat("yyyy-MM-dd");
    private DataDictionary m_dictionary;
    private DataDictionary m_dictionaryExtended;
    private String m_fieldRatDateStdER;
    private String m_fieldRatDateStdPR;
    private String m_fieldRatDateUda;
    private String m_fieldRatTypeUda;
    private boolean m_initError = false;
    private Map<String, String> m_javaNamesERMap = new HashMap<>();
    private Map<String, String> m_javaNamesPRMap = new HashMap<>();
    private Map<String, Collection<UserDefinedTableA>> m_ratingsMapER;
    private Map<String, Collection<UserDefinedTableA>> m_ratingsMapPR;
    private Map<String, SisStudent> m_studentMapER;
    private Map<String, SisStudent> m_studentMapPR;

    /**
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        if (!m_initError) {
            logMessage("Started execute ENTRY records");
            logMessage("------------------------------");
            executeERRatings();
            logMessage("Ended execute ENTRY records");
            logMessage("---------------------------");
            logMessage("Starting execute ENTRY records");
            logMessage("------------------------------");
            executePRRatings();
            logMessage("Ended execute ENTRY records");
            logMessage("---------------------------");
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
        m_commit =
                getParameter(INPUT_PARAM_COMMIT) != null ? ((Boolean) getParameter(INPUT_PARAM_COMMIT)).booleanValue()
                        : false;
        initializeFieldsMap();
        if (!m_initError) {
            loadStudentsER();
            loadRatingsER();
            loadStudentsPR();
            loadRatingsPR();
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
     * Execute ENTRY records and populates with data.
     */
    private void executeERRatings() {
        for (Entry<String, SisStudent> entry : m_studentMapER.entrySet()) {
            String stdOid = entry.getKey();
            SisStudent std = entry.getValue();
            Collection<UserDefinedTableA> ratings = m_ratingsMapER.get(stdOid);
            Object dateFromStd = std.getFieldValueByBeanPath(m_fieldRatDateStdER);
            if (dateFromStd != null) {
                if (ratings == null) {
                    UserDefinedTableA ratingToSave =
                            X2BaseBean.newInstance(UserDefinedTableA.class, getBroker().getPersistenceKey());
                    ratingToSave.setStudentOid(stdOid);
                    ratingToSave.setExtendedDataDictionaryOid(m_dictionaryExtended.getExtendedDictionaryOid());
                    ratingToSave.setFieldValueByAlias(ALIAS_RAT_DATE_UDA, dateFromStd, m_dictionaryExtended);
                    ratingToSave.setFieldValueByAlias(ALIAS_RAT_TYPE_UDA, RATING_TYPE_ENTRY, m_dictionaryExtended);
                    populateRatingER(std, ratingToSave);
                    logMessage("Local ID = " + std.getLocalId()
                            + ": New Rating Record (instanse of UDA) was created: "
                            + "Rating Date = " + "\"" + dateFromStd
                            + "\", Rating Type = "
                            + RATING_TYPE_ENTRY + ".");
                    if (m_commit) {
                        getBroker().saveBeanForced(ratingToSave);
                    }
                } else {
                    for (UserDefinedTableA rating : ratings) {
                        if (rating.getFieldValueByAlias(ALIAS_RAT_DATE_UDA, m_dictionaryExtended) != null
                                && rating.getFieldValueByAlias(ALIAS_RAT_DATE_UDA, m_dictionaryExtended)
                                        .equals(dateFromStd)) {
                            populateRatingER(std, rating);
                            logMessage("Local ID = " + std.getLocalId()
                                    + ": Rating Record (instanse of UDA) was updated"
                                    + ". Rating Date = " + "\"" + dateFromStd
                                    + "\", Rating Type = "
                                    + RATING_TYPE_ENTRY + ".");
                            if (m_commit && rating.isDirty()) {
                                getBroker().saveBeanForced(rating);
                            }
                            break;
                        }
                    }
                }
            }
        }
    }

    /**
     * Execute PROGRESS records and populates with data.
     */
    private void executePRRatings() {
        for (Entry<String, SisStudent> entry : m_studentMapPR.entrySet()) {
            String stdOid = entry.getKey();
            SisStudent std = entry.getValue();
            Collection<UserDefinedTableA> ratings = m_ratingsMapPR.get(stdOid);
            Object dateFromStd = std.getFieldValueByBeanPath(m_fieldRatDateStdPR);
            if (dateFromStd != null) {
                if (ratings == null) {
                    UserDefinedTableA ratingToSave =
                            X2BaseBean.newInstance(UserDefinedTableA.class, getBroker().getPersistenceKey());
                    ratingToSave.setStudentOid(stdOid);
                    ratingToSave.setExtendedDataDictionaryOid(m_dictionaryExtended.getExtendedDictionaryOid());
                    ratingToSave.setFieldValueByAlias(ALIAS_RAT_DATE_UDA, dateFromStd, m_dictionaryExtended);
                    ratingToSave.setFieldValueByAlias(ALIAS_RAT_TYPE_UDA, RATING_TYPE_PROGRESS, m_dictionaryExtended);
                    populateRatingPR(std, ratingToSave);
                    logMessage("Local ID = " + std.getLocalId()
                            + ": New Rating Record (instanse of UDA) was created: "
                            + ". Rating Date = " + "\"" + dateFromStd
                            + "\", Rating Type = "
                            + RATING_TYPE_PROGRESS + ".");
                    if (m_commit) {
                        getBroker().saveBeanForced(ratingToSave);
                    }
                } else {
                    for (UserDefinedTableA rating : ratings) {
                        if (rating.getFieldValueByAlias(ALIAS_RAT_DATE_UDA, m_dictionaryExtended) != null
                                && rating.getFieldValueByAlias(ALIAS_RAT_DATE_UDA, m_dictionaryExtended)
                                        .equals(dateFromStd)) {
                            populateRatingPR(std, rating);
                            logMessage("Local ID = " + std.getLocalId()
                                    + ": Rating Record (instanse of UDA) was updated"
                                    + ". Rating Date = " + "\"" + dateFromStd
                                    + "\", Rating Type = "
                                    + RATING_TYPE_PROGRESS + ".");
                            if (m_commit && rating.isDirty()) {
                                getBroker().saveBeanForced(rating);
                            }
                            break;
                        }
                    }
                }
            }
        }
    }

    /**
     * Return codes for grades.
     *
     * @return
     */
    private List<String> getValidGrades() {
        List<String> gradesToReturn = new ArrayList<>();
        ModelProperty prop =
                new ModelProperty(SisStudent.class, SisStudent.COL_GRADE_LEVEL, getBroker().getPersistenceKey());
        DataDictionaryField gradeField = m_dictionary.findDataDictionaryField(prop.getFieldId());
        if (gradeField != null && gradeField.hasReferenceTable()) {
            X2Criteria gradesCriteria = new X2Criteria();
            gradesCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, gradeField.getReferenceTableOid());
            gradesCriteria.addEqualTo(ReferenceCode.COL_STATE_CODE, VALID_STATE_GRADE);
            QueryByCriteria gradesQuery = new QueryByCriteria(ReferenceCode.class, gradesCriteria);
            gradesToReturn
                    .addAll(getBroker().getGroupedCollectionByQuery(gradesQuery, ReferenceCode.COL_CODE, 128).keySet());
        }
        return gradesToReturn;
    }

    /**
     * Initialize fields.
     */
    private void initializeFieldsMap() {
        m_dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        m_dictionaryExtended = DataDictionaryUtils.getExtendedDictionaryById(DDX_ID_RATINGS, getBroker());
        for (ALIASES_ER aliasPair : ALIASES_ER.values()) {
            m_javaNamesERMap.put(translateAliasToJavaName(aliasPair.getStdAlias(), true), aliasPair.getUdaAlias());
        }
        for (ALIASES_PR aliasPair : ALIASES_PR.values()) {
            m_javaNamesPRMap.put(translateAliasToJavaName(aliasPair.getStdAlias(), true), aliasPair.getUdaAlias());
        }
        m_fieldRatDateStdER = translateAliasToJavaName(ALIAS_RAT_DATE_STD_ER, true);
        m_fieldRatDateStdPR = translateAliasToJavaName(ALIAS_RAT_DATE_STD_PR, true);
        m_fieldRatDateUda = translateAliasToJavaNameForUDA(ALIAS_RAT_DATE_UDA, true);
        m_fieldRatTypeUda = translateAliasToJavaNameForUDA(ALIAS_RAT_TYPE_UDA, true);
    }

    /**
     * Loads UDA (ratings) if they already exist.<br>
     * Then we will update existing if matched are found or create new ones.
     */
    private void loadRatingsER() {
        if (m_studentMapER == null) {
            loadStudentsER();
        }
        X2Criteria criteria = new X2Criteria();
        criteria.addNotEmpty(m_fieldRatDateUda, getBroker().getPersistenceKey());
        criteria.addEqualTo(UserDefinedTableA.REL_EXTENDED_DATA_DICTIONARY + ModelProperty.PATH_DELIMITER
                + ExtendedDataDictionary.COL_ID, DDX_ID_RATINGS);
        criteria.addIn(UserDefinedTableA.COL_STUDENT_OID, m_studentMapER.keySet());
        QueryByCriteria query = new QueryByCriteria(UserDefinedTableA.class, criteria);
        m_ratingsMapER =
                getBroker().getGroupedCollectionByQuery(query, UserDefinedTableA.COL_STUDENT_OID, 512);
    }

    /**
     * Loads UDA (ratings) if they already exist.<br>
     * Then we will update existing if matched are found or create new ones.
     */
    private void loadRatingsPR() {
        if (m_studentMapPR == null) {
            loadStudentsPR();
        }
        X2Criteria criteria = new X2Criteria();
        criteria.addNotEmpty(m_fieldRatDateUda, getBroker().getPersistenceKey());
        criteria.addEqualTo(UserDefinedTableA.REL_EXTENDED_DATA_DICTIONARY + ModelProperty.PATH_DELIMITER
                + ExtendedDataDictionary.COL_ID, DDX_ID_RATINGS);
        criteria.addIn(UserDefinedTableA.COL_STUDENT_OID, m_studentMapPR.keySet());
        QueryByCriteria query = new QueryByCriteria(UserDefinedTableA.class, criteria);
        m_ratingsMapPR =
                getBroker().getGroupedCollectionByQuery(query, UserDefinedTableA.COL_STUDENT_OID, 512);
    }

    /**
     * Loads students with state grade code = 14 and not empty field by alias "DOE EC ENTRY RATING
     * DATE".<br>
     * The data concerning ratings from these students will be moved to UDA table.
     */
    private void loadStudentsER() {
        X2Criteria stdCriteria = new X2Criteria();
        stdCriteria.addNotEmpty(m_fieldRatDateStdER, getBroker().getPersistenceKey());
        stdCriteria.addIn(SisStudent.COL_GRADE_LEVEL, getValidGrades());
        m_studentMapER = getBroker().getMapByQuery(new QueryByCriteria(SisStudent.class, stdCriteria),
                X2BaseBean.COL_OID, 1024);
    }

    /**
     * Loads students with state grade code = 14 and not empty field by alias "DOE EC ENTRY RATING
     * DATE".<br>
     * The data concerning ratings from these students will be moved to UDA table.
     */
    private void loadStudentsPR() {
        X2Criteria stdCriteria = new X2Criteria();
        stdCriteria.addNotEmpty(m_fieldRatDateStdPR, getBroker().getPersistenceKey());
        stdCriteria.addIn(SisStudent.COL_GRADE_LEVEL, getValidGrades());
        m_studentMapPR = getBroker().getMapByQuery(new QueryByCriteria(SisStudent.class, stdCriteria),
                X2BaseBean.COL_OID, 1024);
    }

    /**
     * Populates UDA (rating) with values from student table.
     *
     * @param std
     * @param rating
     */
    private void populateRatingER(SisStudent std, UserDefinedTableA rating) {
        for (Entry<String, String> entry : m_javaNamesERMap.entrySet()) {
            String stdJavaBean = entry.getKey();
            String udaAlias = entry.getValue();
            if (stdJavaBean != null && udaAlias != null) {
                Object stdValue = std.getFieldValueByBeanPath(stdJavaBean);
                if (stdValue != null) {
                    rating.setFieldValueByAlias(udaAlias, stdValue, m_dictionaryExtended);
                }
            }
        }
    }

    /**
     * Populates UDA (rating) with values from student table.
     *
     * @param std
     * @param rating
     */
    private void populateRatingPR(SisStudent std, UserDefinedTableA rating) {
        for (Entry<String, String> entry : m_javaNamesPRMap.entrySet()) {
            String stdJavaBean = entry.getKey();
            String udaAlias = entry.getValue();
            if (stdJavaBean != null && udaAlias != null) {
                Object stdValue = std.getFieldValueByBeanPath(stdJavaBean);
                if (stdValue != null) {
                    rating.setFieldValueByAlias(udaAlias, stdValue, m_dictionaryExtended);
                }
            }
        }
    }

    /**
     * Translates an alias into a Java bean path name. An initialization error will be logged
     * if the alias does not exist.
     *
     * @param alias String
     * @param required boolean
     * @return String
     */
    private String translateAliasToJavaName(String alias, boolean required) {
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
     * Translates an alias into a Java bean path name. An initialization error will be logged
     * if the alias does not exist.
     *
     * @param alias String
     * @param required boolean
     * @return String
     */
    private String translateAliasToJavaNameForUDA(String alias, boolean required) {
        String javaName = null;
        DataDictionaryField field =
                m_dictionaryExtended.findDataDictionaryFieldByAlias(alias);
        if (field != null) {
            javaName = field.getJavaName();
        } else if (required) {
            m_initError = true;
            logMessage("No data dictionary field was found for alias = " + alias + " for extended dictionary with ID = "
                    + DDX_ID_RATINGS);
        }
        return javaName;
    }
}
