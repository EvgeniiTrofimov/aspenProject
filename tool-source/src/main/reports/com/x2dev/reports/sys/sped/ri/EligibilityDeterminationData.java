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
package com.x2dev.reports.sys.sped.ri;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.GenericFormChildData;
import com.follett.fsc.core.k12.beans.GenericFormData;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.BeanCollectionDataSource;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepDisability;
import com.x2dev.sis.model.beans.IepMeeting;
import com.x2dev.sis.model.beans.IepTeamMember;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections.Predicate;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Java source for the RI IEP Review of Referral form.
 *
 * @author X2 Development Corporation
 */
public class EligibilityDeterminationData extends BaseFormReportJavaSource {
    private static final String ALIAS_DDX_PROVIDED_EVAL_COPY = "provided-eval-copy";
    private static final String ALIAS_DDX_PROVIDED_PROC_SAFEGUARDS = "provided-procedural-safeguards";
    private static final String ALIAS_DDX_PROVIDED_TEAM_RESULTS = "provided-team-results";
    private static final String ALIAS_DDX_PROVIDED_LDID = "provided-ldid";
    private static final String ALIAS_EVAL_DATE_COMPLETED = "eval-date-completed";
    private static final String ALIAS_ITM_ELIG = "itm-elig-tm";
    private static final String ALIAS_MTG_ELIG = "meeting-eligibility";
    private static final String DATASOURCE_DEF_EVAL = "datasource_eval";
    private static final String DATASOURCE_DEF_TEAM = "datasource_team";

    private static final String FORMAT_DEF_EVAL = "format_eval";
    private static final String FORMAT_DEF_TEAM = "format_team";

    private static final String RP_PROVIDED_EVAL_COPY = "provEvalCopy";
    private static final String RP_PROVIDED_PROC_SAFEGUARDS = "provProcSafeguards";
    private static final String RP_PROVIDED_TEAM_RESULTS = "provTeamResults";
    private static final String RP_PROVIDED_LDID = "providedLdid";

    private static final String SUBREPORT_EVAL_FORMAT_ID = "SYS-SPED-RI-ELIG-EVL";
    private static final String SUBREPORT_TEAM_FORMAT_ID = "SYS-SPED-RI-ELIG-TM";

    public static final String PARAM_AS_OF_GRADE = "asOfGrade";
    public static final String PARAM_AS_OF_SCHOOL = "asOfSchool";
    public static final String PARAM_DISABILITIES = "disabilities";
    public static final String PARAM_IS_ADVERSELY_IMPACTED = "isAdverselyImpacted";
    public static final String PARAM_FORM_DATE = "formDate";
    protected SimpleDateFormat m_formatterFrom = new SimpleDateFormat("yyyy-MM-dd");
    private Map<String, ReferenceCode> m_dsblCodesMap = null;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws Exception exception
     * @see com.x2dev.sis.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        SimpleFormDataSource dataSource = null;
        IepData iep = (IepData) getFormOwner();
        GenericFormData gfd = (GenericFormData) getFormStorage();
        if (iep != null && gfd != null) {
            Collection<GenericFormChildData> children = gfd.getGenericFormDataChildren(getBroker());
            if (children.isEmpty() || isBlank()) {
                GenericFormChildData blankAction =
                        X2BaseBean.newInstance(GenericFormChildData.class, getBroker().getPersistenceKey());
                children.add(blankAction);
            }

            PlainDate startDate = (getFormInstance() != null && getFormInstance().getCreatedTime() > 0
                    ? new PlainDate(getFormInstance().getCreatedTime())
                    : new PlainDate());
            SisStudent student = iep.getStudent();
            StudentEnrollment lastEnrollment = getStudentEnrollmentAsOf(startDate, student);
            DistrictSchoolYearContext asOfContext = getContextAsOf(startDate);

            List<String> gradeLevels = null;
            if (lastEnrollment != null &&
                    asOfContext != null) {
                gradeLevels = StudentManager.getMatchingGradeLevels(StudentManager.getMaxGradeLevel(getBroker()),
                        lastEnrollment.getYog(),
                        asOfContext.getSchoolYear(),
                        StudentManager.buildGradeLevelMap(getBroker()));
            }

            if (gradeLevels != null && !gradeLevels.isEmpty()) {
                addParameter(PARAM_AS_OF_GRADE, gradeLevels.get(0));
            } else {
                addParameter(PARAM_AS_OF_GRADE, student.getGradeLevel());
            }

            if (lastEnrollment != null) {
                addParameter(PARAM_AS_OF_SCHOOL, lastEnrollment.getSchool().getName());
            } else {
                addParameter(PARAM_AS_OF_SCHOOL, student.getSchool().getName());
            }

            addParameter(PARAM_DISABILITIES, getListStudentDisabilityStateCodes());
            addParameter(PARAM_IS_ADVERSELY_IMPACTED, disabilityMakeAdverselyImpacts());
            /*
             * Add support for the evaluations sub-reports
             */
            Report evalSubreport = ReportUtils.getReport(SUBREPORT_EVAL_FORMAT_ID, getBroker());
            addParameter(FORMAT_DEF_EVAL, evalSubreport.getCompiledFormat());
            addParameter(DATASOURCE_DEF_EVAL, new BeanCollectionDataSource(children, getDictionary(), getLocale()));

            /*
             * Add support for the team members sub-reports
             */
            Report teamSubreport = ReportUtils.getReport(SUBREPORT_TEAM_FORMAT_ID, getBroker());
            Collection<IepTeamMember> teamMembers = getTeamMembers();
            addParameter(FORMAT_DEF_TEAM, teamSubreport.getCompiledFormat());
            addParameter(DATASOURCE_DEF_TEAM, new BeanCollectionDataSource(teamMembers, getDictionary(), getLocale()));

            long formCreateTime = 0;
            Date formDate = getPlainDateByBeenAlias(gfd, ALIAS_EVAL_DATE_COMPLETED, getDictionary());

            if (formDate == null && getFormInstance() != null) {
                formCreateTime = getFormInstance().getCreatedTime();
                formDate = new Date(formCreateTime);
            }
            addParameter("formDate", formDate);
            addParameter(RP_PROVIDED_EVAL_COPY,
                    gfd.getFieldValueByAlias(ALIAS_DDX_PROVIDED_EVAL_COPY, getDictionary()));
            addParameter(RP_PROVIDED_LDID, gfd.getFieldValueByAlias(ALIAS_DDX_PROVIDED_LDID, getDictionary()));
            addParameter(RP_PROVIDED_PROC_SAFEGUARDS,
                    gfd.getFieldValueByAlias(ALIAS_DDX_PROVIDED_PROC_SAFEGUARDS, getDictionary()));
            addParameter(RP_PROVIDED_TEAM_RESULTS,
                    gfd.getFieldValueByAlias(ALIAS_DDX_PROVIDED_TEAM_RESULTS, getDictionary()));
        }
        dataSource = new SimpleFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale());
        return dataSource;
    }

    /**
     * Gets the context as of.
     *
     * @param startDate PlainDate
     * @return District school year context
     */
    private DistrictSchoolYearContext getContextAsOf(PlainDate startDate) {
        DistrictSchoolYearContext asOfContext = null;
        X2Criteria criteria = new X2Criteria();
        criteria.addLessOrEqualThan(DistrictSchoolYearContext.COL_START_DATE, startDate);
        criteria.addGreaterOrEqualThan(DistrictSchoolYearContext.COL_END_DATE, startDate);

        QueryByCriteria query = new QueryByCriteria(DistrictSchoolYearContext.class, criteria);

        asOfContext = (DistrictSchoolYearContext) getBroker().getBeanByQuery(query);
        return asOfContext;
    }

    /**
     * Gets the plain date by been alias.
     *
     * @param baseBean X2BaseBean
     * @param alias String
     * @param dictionary DataDictionary
     * @return Plain date
     */
    private PlainDate getPlainDateByBeenAlias(X2BaseBean baseBean, String alias, DataDictionary dictionary) {
        PlainDate returnValue = null;
        String value = (String) baseBean.getFieldValueByAlias(alias, dictionary);
        if (value != null && !value.isEmpty()) {
            try {
                returnValue = new PlainDate(m_formatterFrom.parse(value));
            } catch (ParseException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }
        return returnValue;
    }

    /**
     * return list of state code for each student disability.
     * One disability has one state code, but student can has many disabilities.
     *
     * @return
     */
    private List<String> getListStudentDisabilityStateCodes() {
        List<String> dsblStateCodes = new ArrayList<String>();
        if (m_dsblCodesMap == null) {
            loadDisabilityCodes();
        }
        IepData iepData = (IepData) getFormOwner();
        GenericFormData formData = (GenericFormData) getFormStorage();
        if (formData != null && !StringUtils.isEmpty(formData.getOid())) {
            for (IepDisability dsbl : iepData.getIepDisability()) {
                String dsblCode = dsbl.getDisabilityCode();
                ReferenceCode refCode = m_dsblCodesMap.get(dsblCode);
                if (refCode != null) {
                    dsblStateCodes.add(refCode.getStateCode());
                }
            }
        }
        return dsblStateCodes;
    }

    /**
     * Gets the student enrollment as of.
     *
     * @param startDate PlainDate
     * @param student SisStudent
     * @return Student enrollment
     */
    private StudentEnrollment getStudentEnrollmentAsOf(PlainDate startDate, SisStudent student) {
        StudentEnrollment lastEnrollment = null;
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(StudentEnrollment.COL_STUDENT_OID, student.getOid());
        criteria.addLessOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, startDate);
        criteria.addEqualTo(StudentEnrollment.COL_ENROLLMENT_TYPE, "E");

        QueryByCriteria query = new QueryByCriteria(StudentEnrollment.class, criteria);
        query.addOrderByDescending(StudentEnrollment.COL_ENROLLMENT_DATE);

        lastEnrollment = (StudentEnrollment) getBroker().getBeanByQuery(query);

        if (lastEnrollment == null) {
            criteria.addEqualTo(StudentEnrollment.COL_STUDENT_OID, student.getOid());
            criteria.addEqualTo(StudentEnrollment.COL_ENROLLMENT_TYPE, "E");

            query = new QueryByCriteria(StudentEnrollment.class, criteria);
            query.addOrderByAscending(StudentEnrollment.COL_ENROLLMENT_DATE);

            lastEnrollment = (StudentEnrollment) getBroker().getBeanByQuery(query);
        }
        return lastEnrollment;
    }

    /**
     * Gets the team members.
     *
     * @return Collection
     */
    private Collection<IepTeamMember> getTeamMembers() {
        IepData iepData = (IepData) getFormOwner();

        Collection<IepTeamMember> members = iepData.getTeamMembers();

        // get the latest meeting to get state of Eligibility Team flag
        LinkedList<IepMeeting> meetings = new LinkedList(iepData.getIepMeeting());
        Collections.sort(meetings, new Comparator<IepMeeting>() {
            @Override
            public int compare(IepMeeting o1, IepMeeting o2) {
                return o1.getDate().compareTo(o2.getDate());
            }
        });
        IepMeeting lastMeeting = null;
        try {
            lastMeeting = meetings.getLast();
        } catch (Exception e) {
            // nothing to do
        }

        DataDictionaryField mtgEligField = getDictionary().findDataDictionaryFieldByAlias(ALIAS_MTG_ELIG);
        final DataDictionaryField itmEligField = getDictionary().findDataDictionaryFieldByAlias(ALIAS_ITM_ELIG);

        boolean canBeFiltered = lastMeeting != null && mtgEligField != null && itmEligField != null;

        if (canBeFiltered) {
            // if Eligibility Determination flag is true, filter members by Eligibility Team == true
            boolean isEligDetermination =
                    BooleanAsStringConverter.TRUE
                            .equals(lastMeeting.getFieldValueByBeanPath(mtgEligField.getJavaName()));
            if (isEligDetermination) {
                CollectionUtils.filter(members, new Predicate() {
                    @Override
                    public boolean evaluate(Object object) {
                        IepTeamMember member = (IepTeamMember) object;
                        return BooleanAsStringConverter.TRUE
                                .equals(member.getFieldValueByBeanPath(itmEligField.getJavaName()));
                    }
                });
            }
        }

        return members;
    }

    /**
     *
     * @return
     */
    private Boolean disabilityMakeAdverselyImpacts() {
        boolean isAdvImpact = false;
        IepData iepData = (IepData) getFormOwner();
        for (IepDisability dsbl : iepData.getIepDisability()) {
            if (dsbl.getPrimaryIndicator()) {
                isAdvImpact = true;
            }
        }
        return Boolean.valueOf(isAdvImpact);
    }

    /**
     * load reference codes attached into IepDisability table - disabilityCode field - ref table
     */
    private void loadDisabilityCodes() {
        DataDictionaryField field =
                getDictionary().findDataDictionaryField(IepDisability.class.getName(),
                        IepDisability.COL_DISABILITY_CODE);
        ReferenceTable table = field.getReferenceTable();
        if (table != null) {
            m_dsblCodesMap = table.getCodeMap();
        } else {
            m_dsblCodesMap = new HashMap<String, ReferenceCode>();
        }



    }
}
