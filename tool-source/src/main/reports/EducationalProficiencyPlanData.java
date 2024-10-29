/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2010 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.BeanCollectionDataSource;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.follett.fsc.core.k12.tools.reports.SimpleBeanDataSource;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.*;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import java.io.ByteArrayInputStream;
import java.util.Collection;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Java source for the Educational Proficiency Plan (EPP). The primary data source for this report
 * is a SimpleBeanDataSource that provides access to values in the underlying StudentEdPlan object.
 * Subreports are used to display related course, assessment, transcript, journal, and meeting data.
 *
 * @author X2 Development Corporation
 */
public class EducationalProficiencyPlanData extends ReportJavaSourceNet {
    // Alias constants

    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Alias for the StudentEdPlanAssessment ELA indicator.
     */
    public static final String ALIAS_ASSESSMENT_ELA = "epp-asm-ela";

    /**
     * Alias for the StudentEdPlanAssessment math indicator.
     */
    public static final String ALIAS_ASSESSMENT_MATH = "epp-asm-math";

    /**
     * Alias for the StudentEdPlanAssessment STE indicator.
     */
    public static final String ALIAS_ASSESSMENT_STE = "epp-asm-ste";

    /**
     * Alias for the StudentEdPlanCourse ELA indicator.
     */
    public static final String ALIAS_COURSE_ELA = "epp-crs-ela";

    /**
     * Alias for the StudentEdPlanCourse math indicator.
     */
    public static final String ALIAS_COURSE_MATH = "epp-crs-math";

    /**
     * Alias for the StudentEdPlanCourse STE indicator.
     */
    public static final String ALIAS_COURSE_STE = "epp-crs-ste";

    /**
     * Alias for the StudentEdPlanTranscript ELA indicator.
     */
    public static final String ALIAS_TRANSCRIPT_ELA = "epp-trn-ela";

    /**
     * Alias for the StudentEdPlanTranscript math indicator.
     */
    public static final String ALIAS_TRANSCRIPT_MATH = "epp-trn-math";

    /**
     * Alias for the StudentEdPlanTranscript STE indicator.
     */
    public static final String ALIAS_TRANSCRIPT_STE = "epp-trn-ste";

    /**
     * ID of the report. Subreport IDs are build by appending subreport constants below to this ID.
     */
    public static final String REPORT_ID = "SYS-STD-EPP";

    /*
     * Subreport constants. Report IDs are these constants appended to the REPORT_ID. These
     * constants
     * are also used as parameter keys to pass subreports into the report format.
     */

    /**
     * ID of the part 1 ELA assessments subreport.
     */
    public static final String SUBREPORT_P1_ASSESSMENTS = "SUB1";

    /**
     * ID of the part 1 ELA courses subreport.
     */
    public static final String SUBREPORT_P1_COURSES = "SUB2";

    /**
     * ID of the part 2 ELA courses subreport.
     */
    public static final String SUBREPORT_P2_COURSES = "SUB3";

    /**
     * ID of the meetings subreport.
     */
    public static final String SUBREPORT_MEETINGS = "SUB4";

    /**
     * ID of the journals subreport.
     */
    public static final String SUBREPORT_JOURNALS = "SUB5";

    // Subreport parameter suffixes
    private static final String SUBREPORT_DATA_SUFFIX = "data";
    private static final String SUBREPORT_FORMAT_SUFFIX = "format";

    private DataDictionary m_dictionary = null;
    private StudentEdPlan m_epp = null;

    /**
     * Adds support for each subreport and returns a SimpleBeanDataSource for the primary EPP
     * record.
     *
     * @return Object
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        m_dictionary = DataDictionary.getDistrictDictionary(getExtendedDictionary(), getBroker().getPersistenceKey());

        addSubreport(SUBREPORT_P1_ASSESSMENTS, SUBREPORT_P1_ASSESSMENTS + "ela", getAssessments(true, false, false));
        addSubreport(SUBREPORT_P1_ASSESSMENTS, SUBREPORT_P1_ASSESSMENTS + "math", getAssessments(false, true, false));
        addSubreport(SUBREPORT_P1_ASSESSMENTS, SUBREPORT_P1_ASSESSMENTS + "ste", getAssessments(false, false, true));

        addSubreport(SUBREPORT_P1_COURSES, SUBREPORT_P1_COURSES + "ela", getTranscripts(true, false, false));
        addSubreport(SUBREPORT_P2_COURSES, SUBREPORT_P2_COURSES + "ela", getCourses(true, false, false));

        addSubreport(SUBREPORT_P1_COURSES, SUBREPORT_P1_COURSES + "math", getTranscripts(false, true, false));
        addSubreport(SUBREPORT_P2_COURSES, SUBREPORT_P2_COURSES + "math", getCourses(false, true, false));

        addSubreport(SUBREPORT_P1_COURSES, SUBREPORT_P1_COURSES + "ste", getTranscripts(false, false, true));
        addSubreport(SUBREPORT_P2_COURSES, SUBREPORT_P2_COURSES + "ste", getCourses(false, false, true));

        addSubreport(SUBREPORT_MEETINGS, SUBREPORT_MEETINGS, getMeetings());
        addSubreport(SUBREPORT_JOURNALS, SUBREPORT_JOURNALS, getJournals());

        return new SimpleBeanDataSource(m_epp, m_dictionary, getLocale());
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
        m_epp = userData.getCurrentRecord(StudentEdPlan.class);
    }

    /**
     * Modifies the passed criteria to find records categorized as MCAS ELA, MCAS math, MCAS STE, or
     * any combination.
     *
     * @param ela boolean
     * @param math boolean
     * @param ste boolean
     * @param mathIndicatorField DataDictionaryField
     * @param elaIndicatorField DataDictionaryField
     * @param steIndicatorField DataDictionaryField
     * @param criteria Criteria
     */
    private void addMcasCriteria(boolean ela,
                                 boolean math,
                                 boolean ste,
                                 DataDictionaryField mathIndicatorField,
                                 DataDictionaryField elaIndicatorField,
                                 DataDictionaryField steIndicatorField,
                                 Criteria criteria) {
        if (ela) {
            criteria.addEqualTo(elaIndicatorField.getJavaName(), BooleanAsStringConverter.TRUE);
        }
        if (math) {
            criteria.addEqualTo(mathIndicatorField.getJavaName(), BooleanAsStringConverter.TRUE);
        }
        if (ste) {
            criteria.addEqualTo(steIndicatorField.getJavaName(), BooleanAsStringConverter.TRUE);
        }
    }

    /**
     * Adds support for an EPP subreport. A BeanCollectionDataSource is used to display each
     * bean in the passed collection on the report.
     *
     * @param formatId one of the SUBREPORT_ constants defined in this class
     * @param dataId unique identifier to use for the subreport data source
     * @param beans Collection<? extends X2BaseBean>
     */
    private void addSubreport(String formatId, String dataId, Collection<? extends X2BaseBean> beans) {
        Report report = ReportUtils.getReport(REPORT_ID + "-" + formatId, getBroker());

        addParameter(formatId + "-" + SUBREPORT_FORMAT_SUFFIX,
                new ByteArrayInputStream(report.getCompiledFormat()));
        addParameter(dataId + "-" + SUBREPORT_DATA_SUFFIX,
                new BeanCollectionDataSource(beans, true, m_dictionary, getLocale()));
    }

    /**
     * Returns a collection of related StudentEdPlanAssessment objects categorized as MCAS ELA,
     * MCAS math, MCAS STE, or any combination.
     *
     * @param ela boolean
     * @param math boolean
     * @param ste boolean
     * @return Collection of StudentEdPlanAssessment objects
     */
    private Collection<StudentEdPlanAssessment> getAssessments(boolean ela, boolean math, boolean ste) {
        DataDictionaryField mathIndicatorField =
                m_dictionary.findDataDictionaryFieldByAlias(ALIAS_ASSESSMENT_MATH);

        DataDictionaryField elaIndicatorField =
                m_dictionary.findDataDictionaryFieldByAlias(ALIAS_ASSESSMENT_ELA);

        DataDictionaryField steIndicatorField =
                m_dictionary.findDataDictionaryFieldByAlias(ALIAS_ASSESSMENT_STE);

        Criteria criteria = new Criteria();
        criteria.addEqualTo(StudentEdPlanAssessment.COL_STUDENT_ED_PLAN_OID, m_epp.getOid());

        addMcasCriteria(ela, math, ste, mathIndicatorField, elaIndicatorField, steIndicatorField, criteria);

        QueryByCriteria query = new QueryByCriteria(StudentEdPlanAssessment.class, criteria);
        query.addOrderByAscending(StudentEdPlanAssessment.REL_ASSESSMENT + "." + StudentAssessment.COL_DATE);

        return getBroker().getCollectionByQuery(query);
    }

    /**
     * Returns a collection of related StudentEdPlanCourse objects categorized as MCAS ELA,
     * MCAS math, or any combination.
     *
     * @param ela boolean
     * @param math boolean
     * @param ste boolean
     * @return Collection of StudentEdPlanCourse objects
     */
    private Collection<StudentEdPlanCourse> getCourses(boolean ela, boolean math, boolean ste) {
        DataDictionaryField mathIndicatorField =
                m_dictionary.findDataDictionaryFieldByAlias(ALIAS_COURSE_MATH);

        DataDictionaryField elaIndicatorField =
                m_dictionary.findDataDictionaryFieldByAlias(ALIAS_COURSE_ELA);

        DataDictionaryField steIndicatorField =
                m_dictionary.findDataDictionaryFieldByAlias(ALIAS_COURSE_STE);

        Criteria criteria = new Criteria();
        criteria.addEqualTo(StudentEdPlanCourse.COL_STUDENT_ED_PLAN_OID, m_epp.getOid());

        addMcasCriteria(ela, math, ste, mathIndicatorField, elaIndicatorField, steIndicatorField, criteria);

        QueryByCriteria query = new QueryByCriteria(StudentEdPlanCourse.class, criteria);
        query.addOrderByAscending(StudentEdPlanCourse.REL_COURSE + "." + Course.COL_NUMBER);

        return getBroker().getCollectionByQuery(query);
    }

    /**
     * Returns the related StudentEdPlanJournal objects.
     *
     * @return Collection of StudentEdPlanJournal objects
     */
    private Collection<StudentEdPlanJournal> getJournals() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(StudentEdPlanJournal.COL_STUDENT_ED_PLAN_OID, m_epp.getOid());

        QueryByCriteria query = new QueryByCriteria(StudentEdPlanJournal.class, criteria);
        query.addOrderByAscending(StudentEdPlanJournal.REL_STUDENT_JOURNAL + "." + StudentJournal.COL_DATE);

        return getBroker().getCollectionByQuery(query);
    }

    /**
     * Returns the related StudentEdPlanMeeting objects.
     *
     * @return Collection of StudentEdPlanMeeting objects
     */
    private Collection<StudentEdPlanMeeting> getMeetings() {
        return m_epp.getStudentEdPlanMeetings(getBroker());
    }

    /**
     * Returns a collection of StudentEdPlanTranscript objects categorized as MCAS ELA, MCAS math,
     * MCAS STE, or any combination.
     *
     * @param ela boolean
     * @param math boolean
     * @param ste boolean
     * @return Collection of StudentEdPlanTranscript objects
     */
    private Collection<StudentEdPlanTranscript> getTranscripts(boolean ela, boolean math, boolean ste) {
        DataDictionaryField mathIndicatorField =
                m_dictionary.findDataDictionaryFieldByAlias(ALIAS_TRANSCRIPT_MATH);

        DataDictionaryField elaIndicatorField =
                m_dictionary.findDataDictionaryFieldByAlias(ALIAS_TRANSCRIPT_ELA);

        DataDictionaryField steIndicatorField =
                m_dictionary.findDataDictionaryFieldByAlias(ALIAS_TRANSCRIPT_STE);

        Criteria criteria = new Criteria();
        criteria.addEqualTo(StudentEdPlanTranscript.COL_STUDENT_ED_PLAN_OID, m_epp.getOid());

        addMcasCriteria(ela, math, ste, mathIndicatorField, elaIndicatorField, steIndicatorField, criteria);

        QueryByCriteria query = new QueryByCriteria(StudentEdPlanTranscript.class, criteria);
        query.addOrderByAscending(StudentEdPlanTranscript.REL_TRANSCRIPT + "." +
                Transcript.REL_DISTRICT_CONTEXT + "." +
                DistrictSchoolYearContext.COL_SCHOOL_YEAR);
        query.addOrderByAscending(StudentEdPlanTranscript.REL_TRANSCRIPT + "." +
                Transcript.REL_SCHOOL_COURSE + "." +
                SchoolCourse.COL_NUMBER);

        return getBroker().getCollectionByQuery(query);
    }
}
