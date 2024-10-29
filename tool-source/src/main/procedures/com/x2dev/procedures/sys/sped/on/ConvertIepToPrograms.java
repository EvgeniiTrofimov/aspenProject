/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2020 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.sys.sped.on;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.UpdateQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.business.localization.LocalizationMessageResources;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.web.ContextList;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.IepAccommodation;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepDisability;
import com.x2dev.sis.model.beans.IepPlacement;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentProgramDetail;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.sis.model.business.sped.SpedUtils;
import com.x2dev.utils.CollectionUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.DateAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.text.MessageFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.apache.ojb.broker.query.Query;

/**
 * @author Follett Software Company
 * @copyright 2020
 */
public class ConvertIepToPrograms extends ProcedureJavaSource {

    private static final String ALIAS_PGM_EXCEPTIONALITY = "pgm-speced-exceptionality";
    private static final String ALIAS_PGM_PRIMARY_EXCEPTIONALITY = "pgm-speced-pri-exceptionality";
    private static final String ALIAS_PGM_REPORT = "pgm-speced-report-ind";
    private static final String ALIAS_PGM_IEP = "pgm-speced-iep";

    private static final String ALIAS_PGD_PLACEMENT = "pgd-speced-placement-type";
    private static final String ALIAS_PGD_START_DATE = "pgd-speced-start-date";
    private static final String ALIAS_PGD_PROGRAM_NAME = "pgd-speced-program-name";
    private static final String ALIAS_PGD_PROGRAM_LOCATION = "pgd-speced-program-location";
    private static final String ALIAS_PGD_END_DATE = "pgd-speced-end-date";
    private static final String ALIAS_PGD_TYPE = "pgd-speced-type";
    private static final String ALIAS_PGD_IEP = "pgd-speced-iep-required";

    private static final String ALIAS_IEP_PLACEMENT = "iep-iprc-placement-decision";
    private static final String ALIAS_IEP_PROGRAM = "iep-program";
    private static final String ALIAS_IEP_PROGRAM_LOCATION = "iep-program-location";
    private static final String ALIAS_IEP_INITIAL_DATE = "iep-date-iprc";
    private static final String ALIAS_IEP_REVIEW_DATE = "iep-iprc-last-review-date";
    // private static final String ALIAS_IEP_REASON = "iep-reason";
    private static final String ALIAS_IEP_ADL_INSTRUCT_ACCOM = "iep-accomm-instructional-addl";
    private static final String ALIAS_IEP_ADL_ENVIRON_ACCOM = "iep-accomm-environmental-addl";
    private static final String ALIAS_IEP_ADL_ASSESS_ACCOM = "iep-accomm-assessment-addl";

    private static final String ALIAS_IPA_ACCOMMODATION = "ipa-accommodation";
    private static final String ALIAS_IPA_ASSESSMENT_PARTICIPATION = "ipa-participation-accomm";

    private static final String ALIAS_IAC_EQAO_ACCOMMODATION = "all-iac-EqaoAccommodation";
    private static final String ALIAS_IAC_EQAO_ACCOMM_REASON = "all-iac-EqaoSpecAccomm";

    private static final String ACCOM_CAT_INSTRUCT = "Instructional";
    private static final String ACCOM_CAT_ENVIRON = "Environmental";
    private static final String ACCOM_CAT_ASSESS = "Assessment";
    private static final String PLACEMENT_PARTICIPATION = "Accommodations";

    private static final String DICTIONARY_SPECED = "STD-PGM-SPECED";
    private static final String PROGRAM_SPECED = "SPECED";

    private static final String PARAM_SOURCE = "source";
    private static final String PARAM_CLEAR = "clear";
    private static final String SOURCE_CURRENT = "current";
    private static final String INDENT = "\n      ";

    private int ENROLLMENT_GAP_DAYS = 90;

    // private classes for accumulating Exceptionality and Program/Placement information.
    private static class ProgramInfo {
        String m_program;
        String m_startDate;
        String m_endDate;
        String m_location;

        @Override
        public String toString() {
            StringBuilder builder = new StringBuilder();
            builder.append("\n  [").append(m_program).append("]");
            builder.append("  ").append(m_startDate).append(" to ").append(m_endDate);
            return builder.toString();
        }
    }
    private static class ExceptionalityInfo {
        String m_exceptionality;
        boolean m_primary;
        boolean m_active;
        PlainDate m_startDate;
        PlainDate m_endDate;
        List<ProgramInfo> m_programs = new ArrayList<ProgramInfo>();
        List<ProgramInfo> m_placement = new ArrayList<ProgramInfo>();

        @Override
        public String toString() {
            StringBuilder builder = new StringBuilder();
            builder.append("[").append(m_exceptionality).append("]");
            if (m_primary) {
                builder.append(" primary");
            }
            if (m_active) {
                builder.append(" active");
            }
            builder.append("  ").append(m_startDate).append(" to ").append(m_endDate);
            builder.append("\n Programs:");
            for (ProgramInfo pInfo : m_programs) {
                builder.append(pInfo.toString());
            }
            builder.append("\n Placements:");
            for (ProgramInfo pInfo : m_placement) {
                builder.append(pInfo.toString());
            }
            return builder.toString();
        }
    }

    // "Records read: "
    private static final String LABEL_RECORDS_READ = "label.procedure.convertIep.records.read";

    // "Ending Exceptionality (Report Indicator false) for {0} as of today."
    private static final String LABEL_ENDING_EXCEPT = "label.procedure.convertIep.ending.exceptionality";

    // "Add new Exeptionality : "
    private static final String LABEL_NEW_EXCEPT = "label.procedure.convertIep.new.exceptionality";

    // "Updating Exeptionality : "
    private static final String LABEL_UPDATE_EXCEPT = "label.procedure.convertIep.update.exceptionality";

    // "Setting Last Evaluation Date on student "
    private static final String LABEL_LAST_EVAL = "label.procedure.convertIep.last.evaluation";

    // " setting start date "
    private static final String LABEL_START_DATE = "label.procedure.convertIep.start.date";

    // " setting end date "
    private static final String LABEL_END_DATE = "label.procedure.convertIep.end.date";

    // " setting end date empty"
    // private static final String LABEL_END_DATE_EMPTY =
    // "label.procedure.convertIep.end.date.empty";

    // " setting program start date to "
    // private static final String LABEL_PROGRAM_START_DATE =
    // "label.procedure.convertIep.program.start.date";

    // " setting program end date to "
    // private static final String LABEL_PROGRAM_END_DATE =
    // "label.procedure.convertIep.program.end.date";

    // " setting program end date empty"
    // private static final String LABEL_PROGRAM_END_DATE_EMPTY =
    // "label.procedure.convertIep.program.end.date.empty";

    // " Add new Placement : "
    private static final String LABEL_NEW_PLACEMENT = "label.procedure.convertIep.add.placement";

    // " Update Placement : "
    private static final String LABEL_UPDATE_PLACEMENT = "label.procedure.convertIep.update.placement";

    // " Add new Program : "
    private static final String LABEL_NEW_PROGRAM = "label.procedure.convertIep.add.program";

    // " Update Program : "
    private static final String LABEL_UPDATE_PROGRAM = "label.procedure.convertIep.update.program";

    // "Add new IEP {0} accommodation: "
    private static final String LABEL_NEW_ACCOM = "label.procedure.convertIep.new.accommodation";

    // "Removing excess IEP {0} accommodation: "
    private static final String LABEL_REMOVE_ACCOM = "label.procedure.convertIep.remove.accommodation";

    // "Add new EQAO accommodation: "
    private static final String LABEL_NEW_EQAO_ACCOM = "label.procedure.convertIep.new.eqao.accommodation";

    // "Removing excess EQAO accommodation: "
    private static final String LABEL_REMOVE_EQAO_ACCOM = "label.procedure.convertIep.remove.eqao.accommodation";

    protected DataDictionary m_iepDictionary;
    protected DataDictionary m_programDictionary;
    protected String m_dictionaryOid;
    protected Map<String, Collection<StudentProgramParticipation>> m_programs = null;
    protected SimpleDateFormat m_dateFormat = new SimpleDateFormat("yyyy-MM-dd");
    protected DateAsStringConverter m_dateConverter;
    protected DataDictionaryField m_placementField;
    protected DataDictionaryField m_programField;
    protected int m_recordsRead;
    protected DateAsStringConverter m_converter;
    protected List<String> m_studentOids;
    protected ModelBroker m_broker;
    protected LocalizationMessageResources m_resources;

    /**
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        m_broker = new ModelBroker(getPrivilegeSet());
        m_converter = (DateAsStringConverter) ConverterFactory.getConverterForClass(Converter.DATE_CONVERTER,
                getLocale(), true);
        m_resources = LocalizationCache.getMessages(m_broker.getPersistenceKey(), getLocale(), true);
        m_dateConverter = (DateAsStringConverter) ConverterFactory.getConverterForClass(Converter.DATE_CONVERTER, getLocale(), true);
        
        ExtendedDataDictionary iepDictionary = SpedUtils.getIepDictionary(getOrganization(), getBroker());
        m_iepDictionary = DataDictionary.getDistrictDictionary(iepDictionary, getBroker().getPersistenceKey());

        // Clear student program records (DO NOT REPORT) that are not generated by this copy procededure.
        boolean clear = ((Boolean) getParameter(PARAM_CLEAR)).booleanValue();
        if (clear) {
            int count = clearProgramIndicators();
        }

        // Load student program participation for SPED programs.
        loadProgramMaps();
        
        m_placementField = m_programDictionary.findDataDictionaryFieldByAlias(ALIAS_PGD_PLACEMENT);
        m_programField = m_programDictionary.findDataDictionaryFieldByAlias(ALIAS_PGD_PROGRAM_NAME);

        Collection<SisStudent> students = getModelBroker().getCollectionByQuery(getStudentQuery());
        for (SisStudent student : students) {
            StringBuilder builder = new StringBuilder();
            builder.append(student.getLocalId()).append(" ").append(student.getNameView());
            m_recordsRead++;
            Collection<StudentProgramParticipation> programs = m_programs.get(student.getOid());
            List<IepData> ieps = getStudentIeps(student.getOid());
            HashSet<StudentProgramParticipation> programsSet =
                    (programs == null) ? new HashSet<StudentProgramParticipation>()
                            : new HashSet<StudentProgramParticipation>(programs);
            setupPrograms(student, ieps, programsSet, builder);
            logMessage(builder.toString());
        }
        logMessage("");
        logMessage(getMessage(LABEL_RECORDS_READ, "Records read: ", null) + Integer.toString(m_recordsRead));
    }

    /**
     * If the selection is for current list, capture the current list (student or IEP student).
     *
     * @throws X2BaseException
     *
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException {
        super.saveState(userData);
        String source = (String) getParameter(PARAM_SOURCE);
        if (SOURCE_CURRENT.equals(source)) {
            ContextList list = userData.getCurrentList();
            if (SisStudent.class.isAssignableFrom(list.getDataClass())) {
                SubQuery query = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, list.getQuery().getCriteria());
                query.addQueryAdjuster(list.getQuery().getQueryAdjuster());
                m_studentOids = (List<String>) getBroker().getSubQueryCollectionByQuery(query);
            } else if (IepData.class.isAssignableFrom(list.getDataClass())) {
                SubQuery query = new SubQuery(IepData.class, IepData.COL_STUDENT_OID, list.getQuery().getCriteria());
                query.addQueryAdjuster(list.getQuery().getQueryAdjuster());
                m_studentOids = (List<String>) getBroker().getSubQueryCollectionByQuery(query);
            }
        }
    }

    /**
     * Provide an override ModelBroker for all save actions, so calcualted fields will be processed.
     *
     * @return ModelBroker
     */
    protected ModelBroker getModelBroker() {
        return m_broker;
    }

    /**
     * Returns the active and previous/amended IEPs for the student.
     *
     * @param studentOid
     *
     * @return Collection<IepData>
     */
    private List<IepData> getStudentIeps(String studentOid) {
        List<Integer> iepStatusCodes = new ArrayList<Integer>(3);
        iepStatusCodes.add(Integer.valueOf(IepData.StatusCode.ACTIVE.ordinal()));
        iepStatusCodes.add(Integer.valueOf(IepData.StatusCode.PREVIOUS.ordinal()));
        iepStatusCodes.add(Integer.valueOf(IepData.StatusCode.AMENDED.ordinal()));

        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(IepData.COL_STUDENT_OID, studentOid);
        criteria.addIn(IepData.COL_STATUS_CODE, iepStatusCodes);
        BeanQuery query = new BeanQuery(IepData.class, criteria);
        query.addOrderBy(IepData.COL_START_DATE, false);
        return (List<IepData>) getModelBroker().getCollectionByQuery(query);
    }

    /**
     * Build and return a query criteria for the active special ed students to process.
     *
     * @return X2Criteria
     */
    private X2Criteria getStudentCriteria() {
        String source = (String) getParameter(PARAM_SOURCE);

        String studentActiveStatus =
                PreferenceManager.getPreferenceValue(getOrganization(), SystemPreferenceDefinition.STUDENT_ACTIVE_CODE);
        String studentActiveStatus2 = PreferenceManager.getPreferenceValue(getOrganization(),
                SystemPreferenceDefinition.STUDENT_ACTIVE_CODE_NO_PRIMARY);
        String spedActiveStatus =
                PreferenceManager.getPreferenceValue(getOrganization(), SisPreferenceConstants.SPED_ACTIVE_CODE);
        String spedExitStatus =
                PreferenceManager.getPreferenceValue(getOrganization(), SisPreferenceConstants.SPED_EXITED_CODE);
        List<String> activeCodes = new ArrayList<String>();
        activeCodes.add(studentActiveStatus);
        activeCodes.add(studentActiveStatus2);
        List<String> spedCodes = new ArrayList<String>();
        spedCodes.add(spedActiveStatus);
        spedCodes.add(spedExitStatus);

        X2Criteria criteria = new X2Criteria();
        criteria.addIn(SisStudent.COL_ENROLLMENT_STATUS, activeCodes);
        criteria.addIn(SisStudent.COL_SPED_STATUS_CODE, spedCodes);
        if (SOURCE_CURRENT.equals(source)) {
            if (m_studentOids != null && m_studentOids.size() > 0) {
                criteria.addIn(X2BaseBean.COL_OID, m_studentOids);
            } else {
                criteria.addEqualTo(X2BaseBean.COL_OID, "--Not-Found--");
            }
        }
        return criteria;
    }
    
    /**
     * Build and return a query for the active special ed students to process.
     *
     * @return Query
     */
    private Query getStudentQuery() {
        X2Criteria criteria = getStudentCriteria();
        BeanQuery query = new BeanQuery(SisStudent.class, criteria);
        return query;
    }

    /**
     * Preloads all StudentProgramParticipation for SPECED programs by student Oid.
     */
    private void loadProgramMaps() {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ExtendedDataDictionary.COL_ID, DICTIONARY_SPECED);
        BeanQuery query = new BeanQuery(ExtendedDataDictionary.class, criteria);
        ExtendedDataDictionary ddx = getModelBroker().getBeanByQuery(query);
        m_programDictionary = DataDictionary.getDistrictDictionary(ddx, getModelBroker().getPersistenceKey());
        m_dictionaryOid = ddx.getOid();
        DataDictionaryField pgmIepField = m_programDictionary.findDataDictionaryFieldByAlias(ALIAS_PGM_IEP);  

        criteria = new X2Criteria();
        criteria.addEqualTo(StudentProgramParticipation.COL_PROGRAM_CODE, PROGRAM_SPECED);
        criteria.addEqualTo(pgmIepField.getJavaName(), BooleanAsStringConverter.TRUE);
        /*
         * X2Criteria orCriteria1 = new X2Criteria();
         * X2Criteria orCriteria2 = new X2Criteria();
         * X2Criteria endCriteria = new X2Criteria();
         * orCriteria1.addIsNull(StudentProgramParticipation.COL_END_DATE);
         * orCriteria2.addGreaterOrEqualThan(StudentProgramParticipation.COL_END_DATE,
         * getCurrentContext().getStartDate());
         * endCriteria.addOrCriteria(orCriteria1);
         * endCriteria.addOrCriteria(orCriteria2);
         * criteria.addAndCriteria(endCriteria);
         */

        query = new BeanQuery(StudentProgramParticipation.class, criteria);
        query.addOrderBy(StudentProgramParticipation.COL_STUDENT_OID, true);
        m_programs =
                getModelBroker().getGroupedCollectionByQuery(query, StudentProgramParticipation.COL_STUDENT_OID, 128);
    }

    /**
     * Clear all student programs that are not created by this Copy procedure by marking them as DO NOT REPORT.
     * This cleans excess procedures 
     */
    private int clearProgramIndicators() {
        int updateCount = 0;
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ExtendedDataDictionary.COL_ID, DICTIONARY_SPECED);
        BeanQuery query = new BeanQuery(ExtendedDataDictionary.class, criteria);
        ExtendedDataDictionary ddx = getModelBroker().getBeanByQuery(query);
        m_programDictionary = DataDictionary.getDistrictDictionary(ddx, getModelBroker().getPersistenceKey());
        String dictionaryOid = ddx.getOid();
        DataDictionaryField pgmIepField = m_programDictionary.findDataDictionaryFieldByAlias(ALIAS_PGM_IEP);  
        DataDictionaryField pgmReportField = m_programDictionary.findDataDictionaryFieldByAlias(ALIAS_PGM_REPORT);  

        if (pgmIepField != null && pgmReportField != null) {
            SubQuery studentSubquery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, getStudentCriteria());
            criteria = new X2Criteria();
            criteria.addEqualTo(StudentProgramParticipation.COL_EXTENDED_DATA_DICTIONARY_OID, dictionaryOid);
            criteria.addNotEqualTo(pgmIepField.getJavaName(), BooleanAsStringConverter.TRUE);
            criteria.addEqualTo(pgmReportField.getJavaName(), BooleanAsStringConverter.TRUE);
            criteria.addIn(StudentProgramParticipation.COL_STUDENT_OID, studentSubquery);
            
            UpdateQuery update = new UpdateQuery(StudentProgramParticipation.class, criteria, pgmReportField.getJavaName(), BooleanAsStringConverter.FALSE);
            updateCount = getBroker().executeUpdateQuery(update);
        }
        return updateCount;
    }
    
    /**
     * @param student
     * @param ieps
     * @param programs
     */
    private void setupPrograms(SisStudent student,
                               List<IepData> ieps,
                               Collection<StudentProgramParticipation> programs,
                               StringBuilder builder) {
        Map<String, Collection<ExceptionalityInfo>> exceptionalities =
                new HashMap<String, Collection<ExceptionalityInfo>>();

        PlainDate lastIPRCDate = null;
        PlainDate initialIPRCDate = null;
        // The start date of the "next" IEP so the "previous" IEP can end sooner of they overlap.
        PlainDate previousStartDate = null; 
        for (IepData iep : ieps) {
            Collection<IepDisability> disabilities = iep.getIepDisability();
            if (!CollectionUtils.isEmpty(disabilities)) {
                for (IepDisability disability : disabilities) {
                    gatherExceptionality(exceptionalities, iep, disability, previousStartDate);
                }
            } else {
                gatherExceptionality(exceptionalities, iep, null, previousStartDate);
            }
            if (iep.getStatusCode() == IepData.StatusCode.ACTIVE.ordinal()) {
                String reviewDate = (String) iep.getFieldValueByAlias(ALIAS_IEP_REVIEW_DATE, m_iepDictionary);
                lastIPRCDate = (PlainDate) m_converter.parseSystemString(reviewDate);
                String initialDate = (String) iep.getFieldValueByAlias(ALIAS_IEP_INITIAL_DATE, m_iepDictionary);
                initialIPRCDate = (PlainDate) m_converter.parseSystemString(initialDate);
                setupAccommodations(student, iep, builder);
            }
            previousStartDate = iep.getStartDate();
        }

        applyExceptionalities(student, programs, exceptionalities, builder);

        if (lastIPRCDate == null) {
            lastIPRCDate = initialIPRCDate;
        }
        if (lastIPRCDate != null && !lastIPRCDate.equals(student.getSpedLastEvaluationDate())) {
            student.setSpedLastEvaluationDate(lastIPRCDate);
            getModelBroker().saveBeanForced(student);
            builder.append(INDENT).append(getMessage(LABEL_LAST_EVAL, "Setting Last Evaluation Date on student ", null))
                    .append(lastIPRCDate);
        }
    }

    /**
     * Add all related info of the IEP and Disability to the exceptionalities map.
     *
     * @param exceptionalities
     * @param iep
     * @param disability
     */
    private void gatherExceptionality(Map<String, Collection<ExceptionalityInfo>> exceptionalities,
                                      IepData iep,
                                      IepDisability disability,
                                      PlainDate previousStartDate) {
        boolean isActive = (IepData.StatusCode.ACTIVE.ordinal() == iep.getStatusCode());
        boolean disabilityPrimary = false;
        String disabilityCode = null;
        String disabilityKey = null;
        if (disability != null) {
            disabilityCode = disability.getDisabilityCode();
            disabilityPrimary = disability.getPrimaryIndicator();
            disabilityKey = (disabilityPrimary ? "T-" : "F-") + disabilityCode; 
            if (disabilityCode == null) {
                logMessage("IEP missing exceptionality code: " + iep.getStudent().getNameView());
                return;
            }
        } else {
            disabilityCode = "No Exceptionality";
            disabilityPrimary = true;
            disabilityKey = (disabilityPrimary ? "T-" : "F-") + disabilityCode; 
        }
        PlainDate startDate = iep.getStartDate();
        PlainDate endDate = iep.getEndDate();
        if (previousStartDate != null && (endDate == null || previousStartDate.before(endDate))) {
            endDate = previousStartDate;
        }
        String iepStartDate = null;
        String iepEndDate = null;
        if (startDate != null) {
            iepStartDate = m_dateFormat.format(startDate);
        } else {
            logMessage("IEP missing start date: " + iep.getStudent().getNameView());
            return;
        }
        if (endDate != null) {
            iepEndDate = m_dateFormat.format(endDate);
        } else {
            logMessage("IEP missing end date: " + iep.getStudent().getNameView());
            return;
        }

        Collection<ExceptionalityInfo> eInfos = exceptionalities.get(disabilityKey);
        if (eInfos == null) {
            eInfos = new ArrayList<ExceptionalityInfo>();
            exceptionalities.put(disabilityKey, eInfos);
        }
        ExceptionalityInfo eInfo = null;
        // Find any exceptionalites where start/end date ranges are within 90 days of
        // The an existing exceptionality instance. If the exceptionality exists,
        // but is more than 90 days apart, treat it as a distinct instance of
        // start/end date range.
        for (ExceptionalityInfo ei : eInfos) {
            if (!endDate.before(ei.m_startDate)) {
                eInfo = ei;
                break;
            }
            Calendar cal = Calendar.getInstance();
            cal.setTime(endDate);
            cal.add(Calendar.DAY_OF_YEAR, ENROLLMENT_GAP_DAYS);
            PlainDate gapDate = new PlainDate(cal.getTimeInMillis());
            if (!gapDate.before(ei.m_startDate)) {
                eInfo = ei;
                break;
            }
        }
        if (eInfo == null) {
            eInfo = new ExceptionalityInfo();
            eInfo.m_exceptionality = disabilityCode;
            eInfo.m_startDate = startDate;
            eInfo.m_endDate = endDate;
            eInfos.add(eInfo);
        }

        // Set up Exceptionality.
        eInfo.m_active |= isActive;
        eInfo.m_primary |= disabilityPrimary;

        if (eInfo.m_startDate == null || (startDate != null && eInfo.m_startDate.after(startDate))) {
            eInfo.m_startDate = startDate;
        }
        if (eInfo.m_endDate == null || (endDate != null && eInfo.m_endDate.before(endDate))) {
            eInfo.m_endDate = endDate;
        }

        // Set up program and placement.
        String iepPlacement = (String) iep.getFieldValueByAlias(ALIAS_IEP_PLACEMENT, m_iepDictionary);
        String iepProgram = (String) iep.getFieldValueByAlias(ALIAS_IEP_PROGRAM, m_iepDictionary);
        String iepProgramLocation = (String) iep.getFieldValueByAlias(ALIAS_IEP_PROGRAM_LOCATION, m_iepDictionary);

        if (!StringUtils.isEmpty(iepProgram)) {
            // Trim the program string to the length of the field in the detail.
            // It is smaller than the field in the IEP.
            if (iepProgram.length() > m_programField.getDatabaseLength()) {
                iepProgram = iepProgram.substring(0, m_programField.getDatabaseLength());
            }
            boolean pgFound = false;
            for (ProgramInfo pInfo : eInfo.m_programs) {
                if (StringUtils.isEqual(pInfo.m_program, iepProgram) &&
                     (pInfo.m_startDate == null || (iepEndDate != null && pInfo.m_startDate.compareTo(iepEndDate) <= 0)) && 
                     (pInfo.m_endDate == null || (iepStartDate != null &&  pInfo.m_endDate.compareTo(iepStartDate) >= 0))) {
                    if (iepProgramLocation != null && !iepProgramLocation.equals(pInfo.m_location)) {
                        pInfo.m_location = iepProgramLocation;
                    }
                    if (StringUtils.isEmpty(pInfo.m_location)) {
                        pInfo.m_location = iep.getStudent().getSchoolOid();
                    }
                    if (pInfo.m_startDate == null || (pInfo.m_startDate.compareTo(iepStartDate) > 0)) {
                        pInfo.m_startDate = iepStartDate;
                    }
                    if (pInfo.m_endDate == null || (pInfo.m_endDate.compareTo(iepEndDate) < 0)) {
                        pInfo.m_endDate = iepEndDate;
                    }
                    pgFound = true;
                    break;
                }
            }
            if (!pgFound) {
                ProgramInfo pInfo = new ProgramInfo();
                pInfo.m_program = iepProgram;
                pInfo.m_location = iepProgramLocation;
                if (StringUtils.isEmpty(pInfo.m_location)) {
                    pInfo.m_location = iep.getStudent().getSchoolOid();
                }
                pInfo.m_startDate = iepStartDate;
                pInfo.m_endDate = iepEndDate;
                eInfo.m_programs.add(pInfo);
            }

        }
        if (!StringUtils.isEmpty(iepPlacement)) {
            // Trim the placement string to the length of the field in the detail.
            // It is smaller than the field in the IEP.
            if (iepPlacement.length() > m_placementField.getDatabaseLength()) {
                iepPlacement = iepPlacement.substring(0, m_placementField.getDatabaseLength());
            }
            boolean pgFound = false;
            for (ProgramInfo pInfo : eInfo.m_placement) {
                if (StringUtils.isEqual(pInfo.m_program, iepPlacement) &&
                     (pInfo.m_startDate == null || (iepEndDate != null && pInfo.m_startDate.compareTo(iepEndDate) <= 0)) && 
                     (pInfo.m_endDate == null || (iepStartDate != null &&  pInfo.m_endDate.compareTo(iepStartDate) >= 0))) {

                    if (pInfo.m_startDate == null || (pInfo.m_startDate.compareTo(iepStartDate) > 0)) {
                        pInfo.m_startDate = iepStartDate;
                    }
                    if (pInfo.m_endDate == null || (pInfo.m_endDate.compareTo(iepEndDate) < 0)) {
                        pInfo.m_endDate = iepEndDate;
                    }
                    pgFound = true;
                    break;
                }
            }
            if (!pgFound) {
                ProgramInfo pInfo = new ProgramInfo();
                pInfo.m_program = iepPlacement;
                pInfo.m_startDate = iepStartDate;
                pInfo.m_endDate = iepEndDate;
                eInfo.m_placement.add(pInfo);
            }
        }
    }

    /**
     * @param student
     * @param programs
     * @param exceptionalities
     * @param builder
     */
    private void applyExceptionalities(SisStudent student,
                                       Collection<StudentProgramParticipation> programs,
                                       Map<String, Collection<ExceptionalityInfo>> exceptionalities,
                                       StringBuilder builder) {
        PlainDate today = new PlainDate();
        String todayStr = m_dateFormat.format(today);

        for (StudentProgramParticipation program : programs) {
            String exceptionality =
                    (String) program.getFieldValueByAlias(ALIAS_PGM_EXCEPTIONALITY, m_programDictionary);
            String primaryFlag = (String) program.getFieldValueByAlias(ALIAS_PGM_PRIMARY_EXCEPTIONALITY, m_programDictionary);
            String exceptionalityKey = (BooleanAsStringConverter.TRUE.equals(primaryFlag) ? "T-" : "F-") + exceptionality;

            boolean isIepSource = BooleanAsStringConverter.TRUE.equals(program.getFieldValueByAlias(ALIAS_PGM_IEP, m_programDictionary));
            
            Collection<ExceptionalityInfo> eInfos = exceptionalities.get(exceptionalityKey);
            boolean applied = false;
            if (isIepSource && eInfos != null && eInfos.size() > 0) {
                applied = applyToProgram(eInfos, program, builder);
            }

            if (!applied) {
                // Close existing programs that do not have a matching IEP Exceptionality.
                StringBuilder removeBuilder = new StringBuilder();
                boolean updated = false;
                program.setFieldValueByAlias(ALIAS_PGM_REPORT, BooleanAsStringConverter.FALSE,
                m_programDictionary);
             
                PlainDate endDate = program.getEndDate();
                if (endDate == null || endDate.after(today)) {
                    program.setEndDate(today);
                }
                if (program.isDirty()) {
                    getModelBroker().saveBeanForced(program);
                    updated = true;
                }
                removeBuilder.append(INDENT).append(getMessage(LABEL_ENDING_EXCEPT,
                        "Ending Exceptionality (Report Indicator false) : {0}",
                        new String[] {exceptionality}));
             
                Collection<StudentProgramDetail> details = program.getProgramDetails(getModelBroker());
             
                for (StudentProgramDetail detail : details) {
                    String detailEndDate = (String) detail.getFieldValueByAlias(ALIAS_PGD_END_DATE, m_programDictionary);
                    if (detailEndDate == null || detailEndDate.compareTo(todayStr) >= 0) {
                        detail.setFieldValueByAlias(ALIAS_PGD_END_DATE, todayStr, m_programDictionary);
                        removeBuilder.append(INDENT).append("  ").append(getMessage(LABEL_END_DATE,
                                "  setting end date ", null))
                                .append(todayStr);
                        if (detail.isDirty()) {
                            getModelBroker().saveBeanForced(detail);
                            updated = true;
                        }
                    }
                }
                if (updated) {
                    builder.append(removeBuilder);
                }
            }
        }

        for (Collection<ExceptionalityInfo> eInfos : exceptionalities.values()) {
            for (ExceptionalityInfo eInfo : eInfos) {
                StudentProgramParticipation program = X2BaseBean.newInstance(StudentProgramParticipation.class,
                        getModelBroker().getPersistenceKey());
                program.setStudentOid(student.getOid());
                program.setProgramCode(PROGRAM_SPECED);
                program.setStartDate(eInfo.m_startDate);
                program.setEndDate(eInfo.m_endDate);
                program.setExtendedDataDictionaryOid(m_dictionaryOid);

                setFieldValue(program, ALIAS_PGM_EXCEPTIONALITY, eInfo.m_exceptionality, m_programDictionary);
                program.setFieldValueByAlias(ALIAS_PGM_PRIMARY_EXCEPTIONALITY,
                        eInfo.m_primary ? BooleanAsStringConverter.TRUE : BooleanAsStringConverter.FALSE,
                        m_programDictionary);
                program.setFieldValueByAlias(ALIAS_PGM_REPORT, BooleanAsStringConverter.TRUE, m_programDictionary);
                program.setFieldValueByAlias(ALIAS_PGM_IEP, BooleanAsStringConverter.TRUE, m_programDictionary);
                getBroker().saveBeanForced(program);
                builder.append(INDENT)
                        .append(getMessage(LABEL_NEW_EXCEPT, "Add new Exeptionality : ", null))
                        .append(eInfo.m_exceptionality);

                for (ProgramInfo pInfo : eInfo.m_placement) {
                    StudentProgramDetail detail =
                            X2BaseBean.newInstance(StudentProgramDetail.class, m_programDictionary);
                    detail.setProgramOid(program.getOid());
                    setFieldValue(detail, ALIAS_PGD_IEP, BooleanAsStringConverter.TRUE, m_programDictionary);
                    setFieldValue(detail, ALIAS_PGD_TYPE, "Placement", m_programDictionary);
                    setFieldValue(detail, ALIAS_PGD_PLACEMENT, pInfo.m_program, m_programDictionary);
                    setFieldValue(detail, ALIAS_PGD_START_DATE, pInfo.m_startDate, m_programDictionary);
                    setFieldValue(detail, ALIAS_PGD_END_DATE, pInfo.m_endDate, m_programDictionary);
                    if (detail.isDirty()) {
                        getBroker().saveBeanForced(detail);
                        builder.append(INDENT)
                                .append(getMessage(LABEL_NEW_PLACEMENT, "  Add new Placement : ", null))
                                .append(pInfo.m_program);
                    }
                }
                for (ProgramInfo pInfo : eInfo.m_programs) {
                    StudentProgramDetail detail =
                            X2BaseBean.newInstance(StudentProgramDetail.class, m_programDictionary);
                    detail.setProgramOid(program.getOid());
                    setFieldValue(detail, ALIAS_PGD_IEP, BooleanAsStringConverter.TRUE, m_programDictionary);
                    setFieldValue(detail, ALIAS_PGD_TYPE, "Program", m_programDictionary);
                    setFieldValue(detail, ALIAS_PGD_PROGRAM_NAME, pInfo.m_program, m_programDictionary);
                    setFieldValue(detail, ALIAS_PGD_START_DATE, pInfo.m_startDate, m_programDictionary);
                    setFieldValue(detail, ALIAS_PGD_END_DATE, pInfo.m_endDate, m_programDictionary);
                    if (pInfo.m_location != null) {
                        SisSchool school = (SisSchool) getModelBroker().getBeanByOid(SisSchool.class, pInfo.m_location);
                        if (school != null) {
                            setFieldValue(detail, ALIAS_PGD_PROGRAM_LOCATION, school.getName(), m_programDictionary);
                        } else {
                            setFieldValue(detail, ALIAS_PGD_PROGRAM_LOCATION, pInfo.m_location, m_programDictionary);
                        }
                    }
                    if (detail.isDirty()) {
                        getBroker().saveBeanForced(detail);
                        builder.append(INDENT)
                                .append(getMessage(LABEL_NEW_PLACEMENT, "  Add new Program : ", null))
                                .append(pInfo.m_program);
                    }
                }
            }
        }
    }

    /**
     * @param eInfo
     * @param program
     * @param builder
     */
    private boolean applyToProgram(Collection<ExceptionalityInfo> eInfos,
                                   StudentProgramParticipation program,
                                   StringBuilder builder) {
        // Matching program. get it up to date.
        boolean matched = false;
        for (ExceptionalityInfo eInfo : eInfos) {
            // Check if this eInfo aligns with the current program.
            boolean found = false;
            PlainDate prgStartDate = program.getStartDate();
            PlainDate prgEndDate = program.getEndDate();

            PlainDate iepStartDate = eInfo.m_startDate;
            PlainDate iepEndDate = eInfo.m_endDate;
            if ((prgEndDate == null || !prgEndDate.before(iepStartDate)) &&
                    (iepEndDate == null || !prgStartDate.after(iepEndDate))) {
                found = true;
            }

            if (found) {
                boolean updated = false;
                StringBuilder exceptionalityBuilder = new StringBuilder();
                PlainDate today = new PlainDate();
                String todayStr = m_dateFormat.format(today);
                String exceptionality =
                        (String) program.getFieldValueByAlias(ALIAS_PGM_EXCEPTIONALITY, m_programDictionary);
                program.setStartDate(eInfo.m_startDate);
                program.setEndDate(eInfo.m_endDate);
                program.setFieldValueByAlias(ALIAS_PGM_REPORT, BooleanAsStringConverter.TRUE, m_programDictionary);
                program.setFieldValueByAlias(ALIAS_PGM_IEP, BooleanAsStringConverter.TRUE, m_programDictionary);
                program.setFieldValueByAlias(ALIAS_PGM_PRIMARY_EXCEPTIONALITY,
                        eInfo.m_primary ? BooleanAsStringConverter.TRUE : BooleanAsStringConverter.FALSE,
                        m_programDictionary);
                exceptionalityBuilder.append(INDENT)
                        .append(getMessage(LABEL_UPDATE_EXCEPT, "Updating Exeptionality : ", null))
                        .append(exceptionality);
                if (program.isDirty()) {
                    getModelBroker().saveBeanForced(program);
                    updated = true;
                }

                Map<String, List<StudentProgramDetail>> detailsMap = getProgramDetails(program);
                List<StudentProgramDetail> detailPrograms = detailsMap.get("Program");
                List<StudentProgramDetail> detailPlacements = detailsMap.get("Placement");
                for (ProgramInfo pInfo : eInfo.m_programs) {
                    boolean pgdFound = false;
                    for (StudentProgramDetail detail : detailPrograms) {
                        // StudentProgramDetail detail = detailPrograms.remove(pInfo.m_program);
                        String detailProgram =
                               (String) detail.getFieldValueByAlias(ALIAS_PGD_PROGRAM_NAME, m_programDictionary);
                        if (detailProgram != null && detailProgram.equals(pInfo.m_program)) {
                            String pStartDate = (String) detail.getFieldValueByAlias(ALIAS_PGD_START_DATE, m_programDictionary);
                            PlainDate pgdStartDate = (PlainDate) m_dateConverter.parseSystemString(pStartDate); 
                            String pEndDate = (String) detail.getFieldValueByAlias(ALIAS_PGD_END_DATE, m_programDictionary);
                            PlainDate pgdEndDate = (PlainDate) m_dateConverter.parseSystemString(pEndDate);
                                    
                            PlainDate eprgStartDate = StringUtils.isEmpty(pInfo.m_startDate) ? null : PlainDate.fromString(pInfo.m_startDate);
                            PlainDate eprgEndDate = StringUtils.isEmpty(pInfo.m_endDate) ? null : PlainDate.fromString(pInfo.m_endDate);
                            if ((pgdEndDate == null || !pgdEndDate.before(eprgStartDate)) &&
                                    (eprgEndDate == null || !pgdStartDate.after(eprgEndDate))) {
                                pgdFound = true;
                            }
                    
                            if (pgdFound) {
                                StringBuilder subBuilder = new StringBuilder();
                                detail.setFieldValueByAlias(ALIAS_PGD_IEP, BooleanAsStringConverter.TRUE, m_programDictionary);
                                if (pInfo.m_startDate != null) {
                                    setFieldValue(detail, ALIAS_PGD_START_DATE, pInfo.m_startDate, m_programDictionary);
                                    subBuilder.append(INDENT).append("  ")
                                            .append(getMessage(LABEL_START_DATE, "     setting start date to ", null))
                                            .append(pStartDate);
                                }
                                if (pInfo.m_endDate != null) {
                                    setFieldValue(detail, ALIAS_PGD_END_DATE, pInfo.m_endDate, m_programDictionary);
                                    subBuilder.append(INDENT).append("  ")
                                            .append(getMessage(LABEL_END_DATE, "  setting end date ", null))
                                            .append(pInfo.m_endDate);
                                }
        
                                if (pInfo.m_location != null) {
                                    SisSchool school =
                                            (SisSchool) getModelBroker().getBeanByOid(SisSchool.class, pInfo.m_location);
                                    if (school != null) {
                                        setFieldValue(detail, ALIAS_PGD_PROGRAM_LOCATION, school.getName(),
                                                m_programDictionary);
                                    } else {
                                        setFieldValue(detail, ALIAS_PGD_PROGRAM_LOCATION, pInfo.m_location,
                                                m_programDictionary);
                                    }
                                }
                                if (detail.isDirty()) {
                                    getBroker().saveBeanForced(detail);
                                    updated = true;
                                    exceptionalityBuilder.append(INDENT)
                                            .append(getMessage(LABEL_UPDATE_PROGRAM, "  Update Program : ", null))
                                            .append(pInfo.m_program).append(subBuilder);
                                }
                                detailPrograms.remove(detail);
                                break;
                            }
                        }
                    }
                    if (!pgdFound) {
                        StudentProgramDetail detail = X2BaseBean.newInstance(StudentProgramDetail.class, m_programDictionary);
                        detail.setProgramOid(program.getOid());
                        setFieldValue(detail, ALIAS_PGD_IEP, BooleanAsStringConverter.TRUE, m_programDictionary);
                        setFieldValue(detail, ALIAS_PGD_TYPE, "Program", m_programDictionary);
                        setFieldValue(detail, ALIAS_PGD_PROGRAM_NAME, pInfo.m_program, m_programDictionary);
                        setFieldValue(detail, ALIAS_PGD_START_DATE, pInfo.m_startDate, m_programDictionary);
                        setFieldValue(detail, ALIAS_PGD_END_DATE, pInfo.m_endDate, m_programDictionary);
                        if (pInfo.m_location != null) {
                            SisSchool school =
                                    (SisSchool) getModelBroker().getBeanByOid(SisSchool.class, pInfo.m_location);
                            if (school != null) {
                                setFieldValue(detail, ALIAS_PGD_PROGRAM_LOCATION, school.getName(),
                                        m_programDictionary);
                            } else {
                                setFieldValue(detail, ALIAS_PGD_PROGRAM_LOCATION, pInfo.m_location,
                                        m_programDictionary);
                            }
                        }
                        if (detail.isDirty()) {
                            getBroker().saveBeanForced(detail);
                            updated = true;
                            exceptionalityBuilder.append(INDENT)
                                    .append(getMessage(LABEL_NEW_PROGRAM, "  Add new Program : ", null))
                                    .append(pInfo.m_program);
                        }
                    }
                }
                for (StudentProgramDetail detail : detailPrograms) {
                    String endDate = (String) detail.getFieldValueByAlias(ALIAS_PGD_END_DATE, m_programDictionary);
                    if (endDate == null || endDate.compareTo(todayStr) > 0) {
                        String programName =
                                (String) detail.getFieldValueByAlias(ALIAS_PGD_PROGRAM_NAME, m_programDictionary);
                        detail.setFieldValueByAlias(ALIAS_PGD_END_DATE, todayStr, m_programDictionary);
                        exceptionalityBuilder.append(INDENT)
                                .append(getMessage(LABEL_UPDATE_PROGRAM, "  Update Program : ", null))
                                .append(programName);
                        exceptionalityBuilder.append(INDENT).append("  ")
                                .append(getMessage(LABEL_END_DATE, "  setting end date ", null))
                                .append(todayStr);
                        updated = true;
                        if (detail.isDirty()) {
                            getBroker().saveBeanForced(detail);
                        }
                    }
                }

                for (ProgramInfo pInfo : eInfo.m_placement) {
                    boolean pgdFound = false;
                    for (StudentProgramDetail detail : detailPlacements) {
                        String detailPlacement =
                               (String) detail.getFieldValueByAlias(ALIAS_PGD_PLACEMENT, m_programDictionary);
                        if (detailPlacement != null && detailPlacement.equals(pInfo.m_program)) {
                            String pStartDate = (String) detail.getFieldValueByAlias(ALIAS_PGD_START_DATE, m_programDictionary);
                            PlainDate pgdStartDate = (PlainDate) m_dateConverter.parseSystemString(pStartDate); 
                            String pEndDate = (String) detail.getFieldValueByAlias(ALIAS_PGD_END_DATE, m_programDictionary);
                            PlainDate pgdEndDate = (PlainDate) m_dateConverter.parseSystemString(pEndDate);
                            
                            PlainDate eprgStartDate = StringUtils.isEmpty(pInfo.m_startDate) ? null : PlainDate.fromString(pInfo.m_startDate);
                            PlainDate eprgEndDate = StringUtils.isEmpty(pInfo.m_endDate) ? null : PlainDate.fromString(pInfo.m_endDate);
                            if ((pgdEndDate == null || !pgdEndDate.before(eprgStartDate)) &&
                                    (eprgEndDate == null || !pgdStartDate.after(eprgEndDate))) {
                                pgdFound = true;
                            }
                    
                            if (pgdFound) {
                                StringBuilder subBuilder = new StringBuilder();
                                detail.setFieldValueByAlias(ALIAS_PGD_IEP, BooleanAsStringConverter.TRUE, m_programDictionary);
                                if (pInfo.m_startDate != null) {
                                    detail.setFieldValueByAlias(ALIAS_PGD_START_DATE, pInfo.m_startDate, m_programDictionary);
                                    subBuilder.append(INDENT).append("  ")
                                            .append(getMessage(LABEL_START_DATE, "     setting start date to ", null))
                                            .append(pInfo.m_startDate);
                                }
                                if (pInfo.m_endDate != null) {
                                    detail.setFieldValueByAlias(ALIAS_PGD_END_DATE, pInfo.m_endDate, m_programDictionary);
                                    subBuilder.append(INDENT).append("  ")
                                            .append(getMessage(LABEL_END_DATE, "  setting end date ", null))
                                            .append(pInfo.m_endDate);
                                }
        
                                if (detail.isDirty()) {
                                    getBroker().saveBeanForced(detail);
                                    updated = true;
                                    exceptionalityBuilder.append(INDENT)
                                            .append(getMessage(LABEL_UPDATE_PLACEMENT, "  Update Placement : ", null))
                                            .append(pInfo.m_program).append(subBuilder);
                                }
                                detailPlacements.remove(detail);
                                break;
                            }
                        }
                    }
                    if (!pgdFound) {
                        StudentProgramDetail detail = X2BaseBean.newInstance(StudentProgramDetail.class, m_programDictionary);
                        detail.setProgramOid(program.getOid());
                        setFieldValue(detail, ALIAS_PGD_IEP, BooleanAsStringConverter.TRUE, m_programDictionary);
                        setFieldValue(detail, ALIAS_PGD_TYPE, "Placement", m_programDictionary);
                        setFieldValue(detail, ALIAS_PGD_PLACEMENT, pInfo.m_program, m_programDictionary);
                        setFieldValue(detail, ALIAS_PGD_START_DATE, pInfo.m_startDate, m_programDictionary);
                        setFieldValue(detail, ALIAS_PGD_END_DATE, pInfo.m_endDate, m_programDictionary);
                        if (detail.isDirty()) {
                            getBroker().saveBeanForced(detail);
                            updated = true;
                            exceptionalityBuilder.append(INDENT)
                                    .append(getMessage(LABEL_NEW_PLACEMENT, "  Add new Placement : ", null))
                                    .append(pInfo.m_program);
                        }
                    }
                }
                for (StudentProgramDetail detail : detailPlacements) {
                    String placement = (String) detail.getFieldValueByAlias(ALIAS_PGD_PLACEMENT, m_programDictionary);
                    String endDate = (String) detail.getFieldValueByAlias(ALIAS_PGD_END_DATE, m_programDictionary);
                    if (endDate == null || endDate.compareTo(todayStr) > 0) {
                        detail.setFieldValueByAlias(ALIAS_PGD_END_DATE, todayStr, m_programDictionary);
                        updated = true;
                        exceptionalityBuilder.append(INDENT)
                                .append(getMessage(LABEL_UPDATE_PLACEMENT, "  Update Placement : ", null))
                                .append(placement);
                        exceptionalityBuilder.append(INDENT).append("  ")
                                .append(getMessage(LABEL_END_DATE, "  setting end date ", null))
                                .append(todayStr);
                        if (detail.isDirty()) {
                            getBroker().saveBeanForced(detail);
                        }
                    }
                }
                if (updated) {
                    builder.append(exceptionalityBuilder);
                }
                eInfos.remove(eInfo);
                matched = true;
                break;
            }
        }
        return matched;
    }

    /**
     * @param program
     * @return
     */
    private Map<String, List<StudentProgramDetail>> getProgramDetails(StudentProgramParticipation program) {
        Collection<StudentProgramDetail> details = program.getProgramDetails(getModelBroker());

        Map<String, List<StudentProgramDetail>> detailsMap =
                new HashMap<String, List<StudentProgramDetail>>();
        List<StudentProgramDetail> detailPrograms = new ArrayList<StudentProgramDetail>();
        List<StudentProgramDetail> detailPlacements = new ArrayList<StudentProgramDetail>();
        detailsMap.put("Placement", detailPlacements);
        detailsMap.put("Program", detailPrograms);

        for (StudentProgramDetail detail : details) {
            String type = (String) detail.getFieldValueByAlias(ALIAS_PGD_TYPE, m_programDictionary);
            // String detailPlacement =
            //        (String) detail.getFieldValueByAlias(ALIAS_PGD_PLACEMENT, m_programDictionary);
            // String detailProgram =
            //        (String) detail.getFieldValueByAlias(ALIAS_PGD_PROGRAM_NAME, m_programDictionary);
            if ("Program".equals(type)) {
                detailPrograms.add(detail);
            } else if ("Placement".equals(type)) {
                detailPlacements.add(detail);
            }
        }
        return detailsMap;
    }

    /**
     * Copy/Merge accommodation records from the IEP to the student accommodations.
     *
     * @param student
     * @param iep
     */
    private void setupAccommodations(SisStudent student, IepData iep, StringBuilder builder) {
        List<IepAccommodation> iepAccommodations = new ArrayList<IepAccommodation>();
        List<IepAccommodation> stdIepAccommodations = new ArrayList<IepAccommodation>();
        List<IepAccommodation> stdEqaoAccommodations = new ArrayList<IepAccommodation>();
        List<IepAccommodation> accommodations = getAccommodations(student);
        for (IepAccommodation accommodation : accommodations) {
            if (iep.getOid().equals(accommodation.getIepDataOid())) {
                iepAccommodations.add(accommodation);
            } else if ("IEP".equals(accommodation.getType())) {
                stdIepAccommodations.add(accommodation);
            } else if ("EQAO".equals(accommodation.getType())
                    && StringUtils.isEmpty(accommodation.getIepDataOid())
                    && StringUtils.isEmpty(accommodation.getStudentEdPlanOid())) {
                stdEqaoAccommodations.add(accommodation);
            }
        }
        setupIepAccommodations(student, iep, iepAccommodations, stdIepAccommodations, builder);
        setupEqaoAccommodations(student, iep, stdEqaoAccommodations, builder);
    }

    /**
     * Compare and copy IEP Accommodations from the IEP to the Student Accommodations.
     * Student accommodations are not owned by the IEP, and have a type of "IEP".
     * IEP accommodations are owned by the IEP (IepData OID).
     * The IEP also has text fields with additional accommodations that need to be
     * merged into the Student IEP Accommodations list.
     *
     * @param student
     * @param iep
     * @param iepAccommodations
     * @param stdIepAccommodations
     */
    private boolean setupIepAccommodations(SisStudent student,
                                           IepData iep,
                                           List<IepAccommodation> iepAccommodations,
                                           List<IepAccommodation> stdIepAccommodations,
                                           StringBuilder builder) {
        // Compare/copy from IEP accommodations to the student IEP accommodations.
        boolean updated = false;
        for (IepAccommodation accommodation : iepAccommodations) {
            String category = accommodation.getCategory();
            String name = accommodation.getName();
            boolean found = false;
            Iterator<IepAccommodation> stdAccomItr = stdIepAccommodations.iterator();
            while (stdAccomItr.hasNext()) {
                IepAccommodation stdAccommodation = stdAccomItr.next();
                if (StringUtils.isEqual(category, stdAccommodation.getCategory())
                        && StringUtils.isEqual(name, stdAccommodation.getName())) {
                    // This accommodation is good. Remove it from collection so we can delete the
                    // rest.
                    stdAccomItr.remove();
                    found = true;
                    break;
                }
            }
            if (!found) {
                // Add the accommodation.
                IepAccommodation newAccom = (IepAccommodation) accommodation.copyBean();
                newAccom.setExtendedDataDictionaryOid(null);
                newAccom.setIepDataOid(null);
                newAccom.setStudentEdPlanOid(null);
                newAccom.setType("IEP");
                getModelBroker().saveBeanForced(newAccom);
                updated = true;
                builder.append(INDENT)
                        .append(getMessage(LABEL_NEW_ACCOM, "Add new IEP {0}  accommodation: ",
                                new String[] {category}))
                        .append(name);
            }
        }
        // Add IPE Additional instructional accommodations.
        List<String> addlInstruct = StringUtils.convertDelimitedStringToList(
                (String) iep.getFieldValueByAlias(ALIAS_IEP_ADL_INSTRUCT_ACCOM, m_iepDictionary), "\n,", true);
        for (String name : addlInstruct) {
            String sname = StringUtils.substring(name, 50);
            String category = ACCOM_CAT_INSTRUCT;
            boolean found = false;
            Iterator<IepAccommodation> stdAccomItr = stdIepAccommodations.iterator();
            while (stdAccomItr.hasNext()) {
                IepAccommodation stdAccommodation = stdAccomItr.next();
                if (StringUtils.isEqual(category, stdAccommodation.getCategory())
                        && StringUtils.isEqual(sname, stdAccommodation.getName())) {
                    // This accommodation is good. Remove it from collection so we can delete the
                    // rest.
                    stdAccomItr.remove();
                    found = true;
                    break;
                }
            }
            if (!found) {
                // Add the accommodation.
                IepAccommodation newAccom =
                        X2BaseBean.newInstance(IepAccommodation.class, getModelBroker().getPersistenceKey());
                newAccom.setStudentOid(student.getOid());
                newAccom.setType("IEP");
                newAccom.setCategory(category);
                newAccom.setName(sname);
                newAccom.setDescription(name);
                newAccom.setContentArea("All");
                getModelBroker().saveBeanForced(newAccom);
                updated = true;
                builder.append(INDENT)
                        .append(getMessage(LABEL_NEW_ACCOM, "Add new IEP {0}  accommodation: ",
                                new String[] {category}))
                        .append(sname);
            }
        }
        // Add IPE Additional instructional accommodations.
        List<String> addlEnviron = StringUtils.convertDelimitedStringToList(
                (String) iep.getFieldValueByAlias(ALIAS_IEP_ADL_ENVIRON_ACCOM, m_iepDictionary), "\n", true);
        for (String name : addlEnviron) {
            String sname = StringUtils.substring(name, 50);
            String category = ACCOM_CAT_ENVIRON;
            boolean found = false;
            Iterator<IepAccommodation> stdAccomItr = stdIepAccommodations.iterator();
            while (stdAccomItr.hasNext()) {
                IepAccommodation stdAccommodation = stdAccomItr.next();
                if (StringUtils.isEqual(category, stdAccommodation.getCategory())
                        && StringUtils.isEqual(sname, stdAccommodation.getName())) {
                    // This accommodation is good. Remove it from collection so we can delete the
                    // rest.
                    stdAccomItr.remove();
                    found = true;
                    break;
                }
            }
            if (!found) {
                // Add the accommodation.
                IepAccommodation newAccom =
                        X2BaseBean.newInstance(IepAccommodation.class, getModelBroker().getPersistenceKey());
                newAccom.setStudentOid(student.getOid());
                newAccom.setType("IEP");
                newAccom.setCategory(category);
                newAccom.setName(sname);
                newAccom.setDescription(name);
                newAccom.setContentArea("All");
                getModelBroker().saveBeanForced(newAccom);
                updated = true;
                builder.append(INDENT)
                        .append(getMessage(LABEL_NEW_ACCOM, "Add new IEP {0}  accommodation: ",
                                new String[] {category}))
                        .append(sname);
            }
        }

        List<String> addlAssess = StringUtils.convertDelimitedStringToList(
                (String) iep.getFieldValueByAlias(ALIAS_IEP_ADL_ASSESS_ACCOM, m_iepDictionary), "\n", true);
        for (String name : addlAssess) {
            String sname = StringUtils.substring(name, 50);
            String category = ACCOM_CAT_ASSESS;
            boolean found = false;
            Iterator<IepAccommodation> stdAccomItr = stdIepAccommodations.iterator();
            while (stdAccomItr.hasNext()) {
                IepAccommodation stdAccommodation = stdAccomItr.next();
                if (StringUtils.isEqual(category, stdAccommodation.getCategory())
                        && StringUtils.isEqual(sname, stdAccommodation.getName())) {
                    // This accommodation is good. Remove it from collection so we can delete the
                    // rest.
                    stdAccomItr.remove();
                    found = true;
                    break;
                }
            }
            if (!found) {
                // Add the accommodation.
                IepAccommodation newAccom =
                        X2BaseBean.newInstance(IepAccommodation.class, getModelBroker().getPersistenceKey());
                newAccom.setStudentOid(student.getOid());
                newAccom.setType("IEP");
                newAccom.setCategory(category);
                newAccom.setName(sname);
                newAccom.setDescription(name);
                newAccom.setContentArea("All");
                getModelBroker().saveBeanForced(newAccom);
                updated = true;
                builder.append(INDENT)
                        .append(getMessage(LABEL_NEW_ACCOM, "Add new IEP {0}  accommodation: ",
                                new String[] {category}))
                        .append(sname);
            }
        }

        for (IepAccommodation stdAccommodation : stdIepAccommodations) {
            builder.append(INDENT)
                    .append(getMessage(LABEL_REMOVE_ACCOM, "Add new IEP {0}  accommodation: ",
                            new String[] {stdAccommodation.getCategory()}))
                    .append(stdAccommodation.getName());
            getModelBroker().deleteBean(stdAccommodation);
            updated = true;
        }
        return updated;
    }

    /**
     * Copy/Merge EQAO accommodations from the IEP to the Student Accommodations for EQAO.
     *
     * @param student
     * @param iep
     * @param stdEqaoAccommodations
     */
    private boolean setupEqaoAccommodations(SisStudent student,
                                            IepData iep,
                                            List<IepAccommodation> stdEqaoAccommodations,
                                            StringBuilder builder) {
        boolean updated = false;
        Collection<IepPlacement> placements = iep.getPlacements(getModelBroker());
        Set<String> eqaoAssessments = new HashSet<String>();
        for (IepPlacement placement : placements) {
            String participation =
                    (String) placement.getFieldValueByAlias(ALIAS_IPA_ASSESSMENT_PARTICIPATION, m_iepDictionary);
            if (placement.getStatusCode() == 1 && PLACEMENT_PARTICIPATION.equals(participation)) {
                List<String> eqaoPlacementCodes = StringUtils.convertDelimitedStringToList(
                        (String) placement.getFieldValueByAlias(ALIAS_IPA_ACCOMMODATION, m_iepDictionary), ",", true);
                eqaoAssessments.addAll(eqaoPlacementCodes);
            }
        }


        // Add EQAO accommodations.
        for (String name : eqaoAssessments) {
            boolean found = false;
            Iterator<IepAccommodation> stdAccomItr = stdEqaoAccommodations.iterator();
            while (stdAccomItr.hasNext()) {
                IepAccommodation stdAccommodation = stdAccomItr.next();
                String eqaoAccomCode =
                        (String) stdAccommodation.getFieldValueByAlias(ALIAS_IAC_EQAO_ACCOMMODATION, m_iepDictionary);
                if (StringUtils.isEqual(name, eqaoAccomCode)) {
                    // This accommodation is good. Remove it from collection so we can delete the
                    // rest.
                    stdAccomItr.remove();
                    found = true;
                    break;
                }
            }
            if (!found) {
                // Add the accommodation.
                IepAccommodation newAccom =
                        X2BaseBean.newInstance(IepAccommodation.class, getModelBroker().getPersistenceKey());
                newAccom.setStudentOid(student.getOid());
                newAccom.setType("EQAO");
                newAccom.setFieldValueByAlias(ALIAS_IAC_EQAO_ACCOMMODATION, name, m_iepDictionary);
                newAccom.setFieldValueByAlias(ALIAS_IAC_EQAO_ACCOMM_REASON, "SpecPermIEP", m_iepDictionary);
                getModelBroker().saveBeanForced(newAccom);
                updated = true;
                builder.append(INDENT)
                        .append(getMessage(LABEL_NEW_EQAO_ACCOM, "Add new EQAO accommodation: ", null))
                        .append(name);
            }
        }

        for (IepAccommodation stdAccommodation : stdEqaoAccommodations) {
            builder.append(INDENT)
                    .append(getMessage(LABEL_REMOVE_EQAO_ACCOM, "Removing excess EQAO accommodation: ", null))
                    .append(stdAccommodation.getName());
            getModelBroker().deleteBean(stdAccommodation);
            updated = true;
        }
        return updated;
    }

    /**
     * Get all accommodation records for the student.
     *
     * @param student
     *
     * @return List<IepAccommodation>
     */
    private List<IepAccommodation> getAccommodations(SisStudent student) {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(IepAccommodation.COL_STUDENT_OID, student.getOid());
        BeanQuery query = new BeanQuery(IepAccommodation.class, criteria);
        return (List<IepAccommodation>) getModelBroker().getCollectionByQuery(query);
    }

    /**
     * Lookup a message resource, or apply the default text.
     * format parameters into the mesage text if parameters are provided.
     *
     * @param key
     * @param defaultMessage
     * @param parameters
     *
     * @return String
     */
    private String getMessage(String key, String defaultMessage, Object[] parameters) {
        String message = m_resources.getMessage(getLocale(), key);
        if (message == null || message.startsWith("??")) {
            message = defaultMessage;
        }

        String formattedMessage = message;
        if (parameters != null && parameters.length > 0) {
            try {
                formattedMessage = MessageFormat.format(formattedMessage, parameters);
            } catch (IllegalArgumentException iae) {
                StringBuilder parameterBuffer = new StringBuilder(parameters.length * 16);

                for (int i = 0; i < parameters.length; i++) {
                    parameterBuffer.append("; ");
                    parameterBuffer.append(parameters[i].toString());
                }

                formattedMessage += parameterBuffer.toString();
            }
        }

        return formattedMessage;
    }

    /**
     * Safely set an aliased field value.
     * Check the length of the custom field and the value being applies.
     * If necessary, trim the value.
     *
     * @param bean
     * @param alias
     * @param value
     * @param dictionary
     */
    private void setFieldValue(X2BaseBean bean, String alias, String value, DataDictionary dictionary) {
        DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(alias);
        if (field != null) {
            if (value != null) {
                int fieldLength = field.getLength();
                if (value.length() > fieldLength) {
                    value = value.substring(0, fieldLength);
                }
            }
            bean.setFieldValueByBeanPath(field.getJavaName(), value);
        }
    }
}
