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

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.GenericFormChildData;
import com.follett.fsc.core.k12.beans.GenericFormData;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.BeanCollectionDataSource;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepMeeting;
import com.x2dev.sis.model.beans.IepTeamMember;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAttendance;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.DateAsStringConverter;
import java.io.ByteArrayInputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import net.sf.jasperreports5.engine.JRException;
import net.sf.jasperreports5.engine.data.JRBeanCollectionDataSource;
import net.sf.jasperreports5.engine.util.JRLoader;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections.Predicate;

/**
 * Java source for the RI IEP Review of Referral form.
 *
 * @author X2 Development Corporation
 */
public class SpecificLearningDisabilitiesData extends BaseFormReportJavaSource {

    private static final String ALIAS_TYPE = "sld-type";
    private static final String ALIAS_SOURCE_MEASURE = "sld-source-measure";
    private static final String ALIAS_SOURCE_DATE = "sld-source-date";
    private static final String ALIAS_SOURCE_SCORE = "sld-source-score";
    private static final String ALIAS_SOURCE_PROFICIENT_SCORE = "sld-source-proficient-score";
    
    private static final String ALIAS_SLD_READING_SERVICES = "sld-reading-services";
    private static final String ALIAS_ITM_ELIG = "itm-elig-tm";
    private static final String ALIAS_MTG_ELIG = "meeting-eligibility";

    /*
    private static final String ALIAS_LOP_INTERVENTION = "sld-lop-intervention";
    private static final String ALIAS_LOP_START_DATE = "sld-lop-start-date";
    private static final String ALIAS_LOP_END_DATE = "sld-lop-end-date";
    private static final String ALIAS_LOP_FREQUENCY = "sld-lop-frequency";
    private static final String ALIAS_LOP_DURATION = "sld-lop-duration";
    private static final String ALIAS_LOP_INTERVENTIONIST = "sld-lop-interventionist";
    private static final String ALIAS_LOP_TITLE = "sld-lop-title";
    private static final String ALIAS_LOP_MEASURE = "sld-lop-measure";
    private static final String ALIAS_LOP_BASELINE = "sld-lop-baseline";
    private static final String ALIAS_LOP_GOAL_SCORE = "sld-lop-goal-score";
    private static final String ALIAS_LOP_RATE_PROGRESS = "sld-lop-rate-progress";
    private static final String ALIAS_LOP_WEEK_1 = "sld-lop-week-1";
    private static final String ALIAS_LOP_WEEK_2 = "sld-lop-week-2";
    private static final String ALIAS_LOP_WEEK_3 = "sld-lop-week-3";
    private static final String ALIAS_LOP_WEEK_4 = "sld-lop-week-4";
    private static final String ALIAS_LOP_WEEK_5 = "sld-lop-week-5";
    private static final String ALIAS_LOP_WEEK_6 = "sld-lop-week-6";
    private static final String ALIAS_LOP_WEEK_7 = "sld-lop-week-7";
    private static final String ALIAS_LOP_WEEK_8 = "sld-lop-week-8";
    private static final String ALIAS_LOP_WEEK_9 = "sld-lop-week-9";
    private static final String ALIAS_LOP_WEEK_10 = "sld-lop-week-10";
    private static final String ALIAS_LOP_WEEK_11 = "sld-lop-week-11";
    private static final String ALIAS_LOP_WEEK_12 = "sld-lop-week-12";
    private static final String ALIAS_LOP_WEEK_13 = "sld-lop-week-13";
    private static final String ALIAS_LOP_WEEK_14 = "sld-lop-week-14";
    private static final String ALIAS_LOP_WEEK_15 = "sld-lop-week-15";
    private static final String ALIAS_LOP_RATE_OF_PROGRESS = "sld-lop-rate-of-progress";
    private static final String ALIAS_LOP_GOAL_EOY = "sld-lop-goal-eoy";
    private static final String ALIAS_LOP_OUTCOME = "sld-lop-outcome";
    */
    private static final String ALIAS_SPB_GRADE_LEVEL = "sld-spb-grade-level";
    private static final String ALIAS_SPB_SPB_MEASURE = "sld-spb-measure";
    private static final String ALIAS_SPB_STANDARD_SCORE = "sld-spb-standard-score";
    private static final String ALIAS_SPB_PERCENT_PROFICIENT = "sld-spb-percent-proficient";
    private static final String ALIAS_SPB_DATE = "sld-spb-date";
    private static final String ALIAS_OBS_DATE = "sld-obs-date";
    private static final String ALIAS_OBS_CONTENT_AREA = "sld-obs-content-area";
    private static final String ALIAS_OBS_TEACHER = "sld-obs-teacher";
    private static final String ALIAS_OBS_METHOD = "sld-obs-method";

    private static final String PARAM_INTERVENTION_FORMAT = "interventionFormat";
    private static final String PARAM_INTERVENTION_DATA = "interventionData";
    private static final String PARAM_MEMBERS_DATA = "membersData";
    private static final String PARAM_OBSERVATIONS_DATA = "observationsData";
    private static final String PARAM_SOURCES_DATA = "sourcesData";
    private static final String PARAM_STANDARDS_DATA = "standardsData";
    private static final String PARAM_STDDOB = "stddob";
    private static final String PARAM_STDGRADE = "stdgrade";
    private static final String PARAM_STDNAME = "stdname";
    private static final String PARAM_ATTENDANCE = "attendance";
    
    private static final String MEMBER_NAME = "memberName";
    private static final String TYPE_INTERVENTION = "inter";
    private static final String TYPE_OBSERVATION = "observ";
    private static final String TYPE_SOURCE = "source";
    private static final String TYPE_STANDARD = "standard";
    
    private static final String SUBREPORT_FORMAT_ID = "SYS-SPED-RI-SLD-INT";
    
    private DateAsStringConverter m_dateConverter;
    
    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws Exception exception
     * @see com.x2dev.sis.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {

        GenericFormData gfd = (GenericFormData) getFormStorage();
        IepData iep = (IepData) getFormOwner();
        
        DataDictionary sldDict = DataDictionary.getDistrictDictionary(getFormDefinition().getExtendedDataDictionary(), getBroker().getPersistenceKey());
        String ellSupport = (String) gfd.getFieldValueByAlias(ALIAS_SLD_READING_SERVICES, sldDict);
        if (ellSupport == null) {
            ellSupport = "";
        }
        addParameter(ALIAS_SLD_READING_SERVICES, ellSupport);
        
        loadChildData();
        prepareTeamMembers();
        loadStudentInfo();
        initSubReport();

        SimpleFormDataSource dataSource = new SimpleFormDataSource(gfd, getFormOwner(), getDictionary(), getLocale());
        return dataSource;
    }

    /**
     * Initialize.
     *
     * @see com.x2dev.sis.tools.reports.BaseFormReportJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        super.initialize();
        
        m_dateConverter = (DateAsStringConverter) ConverterFactory.getConverterForClass(Converter.DATE_CONVERTER, getLocale(), true);
    }

    /**
     * Load the subreport format for intervention.
     */
    private void initSubReport() {
        try {
            byte[] compiledFormat = ReportUtils.getReport(SUBREPORT_FORMAT_ID, getBroker()).getCompiledFormat();
            Object loadedJRReport = JRLoader.loadObject(new ByteArrayInputStream(compiledFormat));
            addParameter(PARAM_INTERVENTION_FORMAT, loadedJRReport);
        } catch (JRException e) {
            String message = "ERROR: Loading subreport for '" + PARAM_INTERVENTION_FORMAT + "' from report " + SUBREPORT_FORMAT_ID;
            message += "\n" + e.getMessage();
            this.addCustomErrorMessage(message);
        }
    }

    /**
     * Load all child records and collect into datasets.
     */
    private void loadChildData() {
        List<Map<String, String>> sources = new ArrayList<Map<String, String>>();
        List<Map<String, String>> standards = new ArrayList<Map<String, String>>();
        List<Map<String, String>> observations = new ArrayList<Map<String, String>>();
        List<GenericFormChildData> interventions = new ArrayList<GenericFormChildData>();
        int interventionCount = 0;
        
        Collection<GenericFormChildData> children =
                ((GenericFormData) getFormStorage()).getGenericFormDataChildren(getBroker());
        if (children.isEmpty() || isBlank()) {
            GenericFormChildData blankAction =
                    X2BaseBean.newInstance(GenericFormChildData.class, getBroker().getPersistenceKey());
            children.add(blankAction);
        } else {
            for (GenericFormChildData child : children) {
                String type = (String) child.getFieldValueByAlias(ALIAS_TYPE, getDictionary());
                if (TYPE_SOURCE.equals(type)) {
                    Map<String, String> item = new HashMap<String, String>();
                    item.put(ALIAS_SOURCE_MEASURE, getField(child, ALIAS_SOURCE_MEASURE, false));
                    item.put(ALIAS_SOURCE_DATE, getField(child, ALIAS_SOURCE_DATE, true));
                    item.put(ALIAS_SOURCE_SCORE, getField(child, ALIAS_SOURCE_SCORE, false));
                    item.put(ALIAS_SOURCE_PROFICIENT_SCORE, getField(child, ALIAS_SOURCE_PROFICIENT_SCORE, false));
                    sources.add(item);
                } else if (TYPE_STANDARD.equals(type)) {
                    Map<String, String> item = new HashMap<String, String>();
                    item.put(ALIAS_SPB_GRADE_LEVEL, getField(child, ALIAS_SPB_GRADE_LEVEL, false));
                    item.put(ALIAS_SPB_SPB_MEASURE, getField(child, ALIAS_SPB_SPB_MEASURE, false));
                    item.put(ALIAS_SPB_STANDARD_SCORE, getField(child, ALIAS_SPB_STANDARD_SCORE, false));
                    item.put(ALIAS_SPB_PERCENT_PROFICIENT, getField(child, ALIAS_SPB_PERCENT_PROFICIENT, false));
                    item.put(ALIAS_SPB_DATE, getField(child, ALIAS_SPB_DATE, true));
                    standards.add(item);
                } else if (TYPE_OBSERVATION.equals(type)) {
                    Map<String, String> item = new HashMap<String, String>();
                    item.put(ALIAS_OBS_CONTENT_AREA, getField(child, ALIAS_OBS_CONTENT_AREA, false));
                    item.put(ALIAS_OBS_TEACHER, getField(child, ALIAS_OBS_TEACHER, false));
                    item.put(ALIAS_OBS_METHOD, getField(child, ALIAS_OBS_METHOD, false));
                    item.put(ALIAS_OBS_DATE, getField(child, ALIAS_OBS_DATE, true));
                    observations.add(item);
                } else if (TYPE_INTERVENTION.equals(type)) {
                    // Put a count value into a field for the row number value in the report.
                    interventionCount += 1; 
                    GenericFormChildData newChild = child.clone();
                    newChild.setFieldO015(Integer.toString(interventionCount) + ".");
                    interventions.add(newChild);
                }
            }
        }
        
        /*
         * Add empty entries in the lists to make them all at least one entry.
         */
        while (sources.size() < 1) {
            Map<String, String> item = new HashMap<String, String>();
            item.put(ALIAS_SOURCE_MEASURE, "");
            item.put(ALIAS_SOURCE_DATE, "");
            item.put(ALIAS_SOURCE_SCORE, "");
            item.put(ALIAS_SOURCE_PROFICIENT_SCORE, "");
            sources.add(item);
        }
        while (standards.size() < 1) {
            Map<String, String> item = new HashMap<String, String>();
            item.put(ALIAS_SPB_GRADE_LEVEL, "");
            item.put(ALIAS_SPB_SPB_MEASURE, "");
            item.put(ALIAS_SPB_STANDARD_SCORE, "");
            item.put(ALIAS_SPB_PERCENT_PROFICIENT, "");
            item.put(ALIAS_SPB_DATE, "");
            standards.add(item);
        }
        while (observations.size() < 1) {
            Map<String, String> item = new HashMap<String, String>();
            item.put(ALIAS_OBS_CONTENT_AREA, "");
            item.put(ALIAS_OBS_TEACHER, "");
            item.put(ALIAS_OBS_METHOD, "");
            item.put(ALIAS_OBS_DATE, "");
            observations.add(item);
        }
        while (interventionCount < 1) {
            interventionCount += 1; 
            GenericFormChildData newChild = X2BaseBean.newInstance(GenericFormChildData.class, getDictionary());
            newChild.setFieldO015(Integer.toString(interventionCount) + ".");
            interventions.add(newChild);
        }

        addParameter(PARAM_INTERVENTION_DATA, new BeanCollectionDataSource(interventions, getDictionary(), getLocale()));
        addParameter(PARAM_OBSERVATIONS_DATA, new JRBeanCollectionDataSource(observations));
        addParameter(PARAM_SOURCES_DATA, new JRBeanCollectionDataSource(sources));
        addParameter(PARAM_STANDARDS_DATA, new JRBeanCollectionDataSource(standards));
        
    }

    /**
     * Load student specific parameters from the form owner.
     * The owner may be IepData of Student, so check type to find values.
     */
    private void loadStudentInfo() {
        SisStudent student = null;
        X2BaseBean owner = getFormOwner();
        if (owner instanceof SisStudent) {
            student = (SisStudent) owner;
        } else if (owner instanceof IepData) {
            student = ((IepData) owner).getStudent();
        }
        if (student != null) {
            // Lookup demographics.
            Converter converter = ConverterFactory.getConverterForClass(Converter.DATE_CONVERTER, getLocale(), false);
            addParameter(PARAM_STDNAME, student.getNameView());
            addParameter(PARAM_STDGRADE, student.getGradeLevel());
            addParameter(PARAM_STDDOB, converter.javaToString(student.getPerson().getDob()));
        
            // Lookup attendance counts.
            int[] attendance = new int[3];
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(StudentAttendance.COL_STUDENT_OID, student.getOid());
            criteria.addBetween(StudentAttendance.COL_DATE, getCurrentContext().getStartDate(), getCurrentContext().getEndDate());
            BeanQuery query = new BeanQuery(StudentAttendance.class, criteria);
            Collection<StudentAttendance> atts = getBroker().getCollectionByQuery(query);
            for (StudentAttendance att : atts) {
                if (att.getAbsentIndicator()) {
                    attendance[0] += 1;
                }
                if (att.getTardyIndicator()) {
                    attendance[1] += 1;
                }
                if (att.getDismissedIndicator()) {
                    attendance[2] += 1;
                }
            }
            StringBuilder builder = new StringBuilder();
            builder.append(Integer.toString(attendance[0]));
            builder.append(" / ");
            builder.append(Integer.toString(attendance[1]));
            builder.append(" / ");
            builder.append(Integer.toString(attendance[2]));
            addParameter(PARAM_ATTENDANCE, builder.toString());
        }
    }

    /**
     * Retrieve the aliased field value from a bean.
     * If date formatting is necessary, perform date converter formatting.
     * 
     * @param bean
     * @param field
     * @param asDate
     * 
     * @return String
     */
    private String getField(X2BaseBean bean, String field, boolean asDate) {
        String value = (String) bean.getFieldValueByAlias(field, getDictionary());
        if (!StringUtils.isEmpty(value)) {
            if (asDate) {
                value = m_dateConverter.javaToString(value);
            } else {
                while (value.endsWith("\n")) {
                    value = value.substring(0, value.length() - 1);
                }
            }
        } 
        if (value == null) {
            value = "";
        }
        return value;
    }

    /**
     * Gets the team members.
     *
     * @return Collection
     */
    private void prepareTeamMembers() {
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

        List<Map<String, String>> membersList = new ArrayList<Map<String, String>>();
        for (IepTeamMember member : members) {
            Map<String, String> memberMap = new HashMap<String, String>();
            String name = member.getNameView();
            memberMap.put(MEMBER_NAME, name);
            membersList.add(memberMap);
        }
        if (membersList.size() == 0) {
            Map<String, String> memberMap = new HashMap<String, String>();
            memberMap.put(MEMBER_NAME, "");
            membersList.add(memberMap);
        }

        addParameter(PARAM_MEMBERS_DATA, new JRBeanCollectionDataSource(membersList));
    }
}
