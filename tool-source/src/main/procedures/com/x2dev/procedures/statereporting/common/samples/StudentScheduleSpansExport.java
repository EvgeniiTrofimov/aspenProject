/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2021 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.common.samples;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.tools.exports.ExportJavaSource;
import com.x2dev.procedures.statereporting.common.DictionaryExtractor;
import com.x2dev.procedures.statereporting.common.FilterableFactory;
import com.x2dev.procedures.statereporting.common.FilterableFactory.Filterable;
import com.x2dev.procedures.statereporting.common.ToolBean;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolSection;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolStudent;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolStudentSchedule;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolStudentScheduleChange;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolTranscript;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StudentScheduleSpan;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.SchoolScheduleContext;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.StudentSchedule;
import com.x2dev.sis.model.beans.StudentScheduleChange;
import com.x2dev.sis.model.beans.Transcript;
import com.x2dev.utils.DataGrid;
import com.x2dev.utils.types.PlainDate;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

/**
 * The Class StudentScheduleSpansExport.
 *
 * @author Follett Software Company
 * @copyright 2021
 */
public class StudentScheduleSpansExport extends ExportJavaSource {
    /**
     * The Class MyStudent.
     */
    public static class MyStudent extends ToolStudent {
        // Query Fields
        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolStudent.FULL_DEFINITION;

        /**
         * Instantiates a new my enrollment.
         *
         * @param columns RptBeanColumns
         * @param data Object[]
         */
        public MyStudent(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }
    }

    /**
     * Name for the "selection" report parameter. The value is a String.
     */
    public static final String PARAM_QUERY_BY = "queryBy";

    /**
     * Name for the "selection value" report parameter. The value is a String.
     */
    public static final String PARAM_QUERY_STRING = "queryString";

    /**
     * Name for the "context" report parameter. The value is an oid.
     */
    protected static final String PARAM_SCHOOL_YEAR_CONTEXT = "schoolYearContext";

    private static final String FIELD_SCHSPAN_ENTRY_CHANGE = "schSpanEntryChange";
    private static final String FIELD_SCHSPAN_EXIT_CHANGE = "schSpanExitChange";
    private static final String FIELD_SCHSPAN_EXIT_DATE = "schSpanExitDate";
    private static final String FIELD_SCHSPAN_SCHOOL = "schSpanSchool";
    private static final String FIELD_SCHSPAN_SECTION = "schSpanSection";
    private static final String FIELD_SCHSPAN_START_DATE = "schSpanStartDate";
    private static final String FIELD_SCHSPAN_TERM = "schSpanTerm";
    private static final String FIELD_SKL_NAME = "schoolName";
    private static final String FIELD_STD_NAME = "studentName";


    /**
     * Gather data.
     *
     * @return DataGrid
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#gatherData()
     */
    @Override
    protected DataGrid gatherData() throws Exception {
        // must be in session to use temporary tables
        X2Broker broker = getBroker();
        broker.beginSession();

        DataGrid grid;
        try {
            grid = new DataGrid();
            DictionaryExtractor dictionaryExtractor = new DictionaryExtractor(broker);
            ToolBean.registerClass(MyStudent.class);
            ToolBean.registerClass(ToolSection.class);
            ToolBean.registerClass(ToolStudentSchedule.class);

            Filterable<MyStudent> students =
                    FilterableFactory.create(broker, MyStudent.class, getStudentCriteria(),
                            Arrays.asList(ToolStudent.FIELD_NAME_VIEW, ToolBean.FIELD_OID));

            // Preload sections
            @SuppressWarnings("unused")
            Filterable<ToolSection> sections =
                    FilterableFactory.create(broker, ToolSection.class, getSectionCriteria(), null);

            ToolBean.preload(broker, dictionaryExtractor, null, ToolStudentSchedule.PARENT_SECTION,
                    ToolStudentSchedule.PARENT_STUDENT);
            ToolBean.preload(broker, dictionaryExtractor, null, ToolStudentScheduleChange.PARENT_SECTION,
                    ToolStudentScheduleChange.PARENT_STUDENT);
            ToolBean.preload(broker, dictionaryExtractor, null, ToolTranscript.PARENT_SECTION,
                    ToolTranscript.PARENT_STUDENT);

            for (MyStudent student : students.extract()) {
                for (StudentScheduleSpan span : student
                        .getStudentScheduleSpans(getBroker()).stream()
                        .sorted(new Comparator<StudentScheduleSpan>() {
                            @Override
                            public int compare(StudentScheduleSpan span1, StudentScheduleSpan span2) {
                                int result = span1.getSection().getSchedule(broker).getSchool(broker).getName()
                                        .compareTo(span2.getSection().getSchedule(broker).getSchool(broker).getName());
                                if (result == 0) {
                                    result = span1.getEntryDate().compareTo(span2.getEntryDate());
                                }
                                if (result == 0) {
                                    result = span1.getSection().getCourseView()
                                            .compareTo(span2.getSection().getCourseView());
                                }
                                return result;
                            }
                        }).collect(Collectors.toList())) {
                    grid.append();
                    grid.set(FIELD_SKL_NAME, getSchool() == null ? "Multi School" : getSchool().getName());
                    grid.set(FIELD_STD_NAME, student.getNameView());
                    grid.set(FIELD_SCHSPAN_SCHOOL,
                            span.getSection().getSchedule(broker).getSchool(broker).getName());
                    grid.set(FIELD_SCHSPAN_SECTION, span.getSection().getCourseView());
                    grid.set(FIELD_SCHSPAN_TERM, span.getSection().getTermView());
                    grid.set(FIELD_SCHSPAN_START_DATE, getDateString(span.getEntryDate()));
                    grid.set(FIELD_SCHSPAN_EXIT_DATE, getDateString(span.getExitDate()));
                    grid.set(FIELD_SCHSPAN_ENTRY_CHANGE, span.getEntryChange() != null ? "Yes" : "No");
                    grid.set(FIELD_SCHSPAN_EXIT_CHANGE, span.getExitChange() != null ? "Yes" : "No");
                }
            }
        } finally {
            broker.endSession();
        }
        return grid;
    }

    /**
     * Gets the column names.
     *
     * @return List
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getColumnNames()
     */
    @Override
    protected List getColumnNames() {
        return Arrays.asList(FIELD_SKL_NAME,
                FIELD_STD_NAME,
                FIELD_SCHSPAN_SCHOOL,
                FIELD_SCHSPAN_SECTION,
                FIELD_SCHSPAN_TERM,
                FIELD_SCHSPAN_START_DATE,
                FIELD_SCHSPAN_EXIT_DATE,
                FIELD_SCHSPAN_ENTRY_CHANGE,
                FIELD_SCHSPAN_EXIT_CHANGE);
    }

    /**
     * Gets the column user names.
     *
     * @return List
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getColumnUserNames()
     */
    @Override
    protected List getColumnUserNames() {
        return getColumnNames();
    }

    /**
     * Gets the comment.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getComment()
     */
    @Override
    protected String getComment() {
        // TODO Auto-generated method stub
        return null;
    }

    /**
     * Gets the header.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getHeader()
     */
    @Override
    protected String getHeader() {
        // TODO Auto-generated method stub
        return null;
    }


    /**
     * Gets the date string.
     *
     * @param date PlainDate
     * @return String
     */
    private String getDateString(PlainDate date) {
        return date == null ? null : date.toString();
    }


    /**
     * Gets the selected context.
     *
     * @return District school year context
     */
    private DistrictSchoolYearContext getSelectedContext() {
        return getBroker().getBeanByOid(DistrictSchoolYearContext.class, getSelectedContextOid());
    }

    /**
     * Gets the selected context oid.
     *
     * @return String
     */
    private String getSelectedContextOid() {
        return (String) getParameter(PARAM_SCHOOL_YEAR_CONTEXT);
    }

    /**
     * Gets the student criteria.
     *
     * @return X 2 criteria
     */
    private X2Criteria getStudentCriteria() {
        X2Criteria scheduleCriteria = getStudentScheduleCriteria();
        SubQuery scheduleSubquery =
                new SubQuery(StudentSchedule.class, StudentSchedule.COL_STUDENT_OID, scheduleCriteria);
        X2Criteria sub1Criteria = new X2Criteria();
        sub1Criteria.addIn(X2BaseBean.COL_OID, scheduleSubquery);

        X2Criteria scheduleChangeCriteria = getStudentScheduleChangeCriteria();
        SubQuery scheduleChangeSubquery = new SubQuery(StudentScheduleChange.class,
                StudentScheduleChange.COL_STUDENT_OID, scheduleChangeCriteria);
        X2Criteria sub2Criteria = new X2Criteria();
        sub2Criteria.addIn(X2BaseBean.COL_OID, scheduleChangeSubquery);
        sub1Criteria.addOrCriteria(sub2Criteria);

        X2Criteria transcriptCriteria = getStudentTranscriptCriteria();
        SubQuery transcriptSubquery =
                new SubQuery(Transcript.class, Transcript.COL_STUDENT_OID, transcriptCriteria);
        sub2Criteria = new X2Criteria();
        sub2Criteria.addIn(X2BaseBean.COL_OID, transcriptSubquery);
        sub1Criteria.addOrCriteria(sub2Criteria);

        String queryBy = (String) getParameter(PARAM_QUERY_BY);
        addUserCriteria(sub1Criteria, queryBy, (String) getParameter(PARAM_QUERY_STRING), null, null);

        return sub1Criteria;
    }

    /**
     * Gets the section criteria.
     *
     * @return X 2 criteria
     */
    private X2Criteria getSectionCriteria() {
        X2Criteria sectionCriteria = new X2Criteria();

        // From Class type section
        sectionCriteria.addEqualTo(MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                SchoolCourse.COL_MASTER_TYPE, SchoolCourse.MASTER_TYPE_CLASS);

        // From active Schedule
        sectionCriteria.addEqualTo(MasterSchedule.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.REL_ACTIVE_SCHOOL_SCHEDULE_CONTEXTS + PATH_DELIMITER +
                SchoolScheduleContext.COL_DISTRICT_CONTEXT_OID,
                getSelectedContextOid());

        if (isSchoolContext()) {
            sectionCriteria.addEqualTo(MasterSchedule.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            sectionCriteria.addNotEqualTo(MasterSchedule.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
            sectionCriteria.addNotEqualTo(MasterSchedule.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
        }

        return sectionCriteria;
    }

    /**
     * Gets the student schedule change criteria.
     *
     * @return X 2 criteria
     */
    private X2Criteria getStudentScheduleChangeCriteria() {
        X2Criteria studentScheduleChangeCriteria = new X2Criteria();
        studentScheduleChangeCriteria.addNotNull(StudentScheduleChange.COL_ACTION_DATE);

        // From Class type section
        studentScheduleChangeCriteria.addEqualTo(StudentScheduleChange.REL_MASTER_SCHEDULE + PATH_DELIMITER +
                MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                SchoolCourse.COL_MASTER_TYPE, SchoolCourse.MASTER_TYPE_CLASS);

        // From active Schedule
        studentScheduleChangeCriteria.addEqualTo(StudentScheduleChange.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.REL_ACTIVE_SCHOOL_SCHEDULE_CONTEXTS + PATH_DELIMITER +
                SchoolScheduleContext.COL_DISTRICT_CONTEXT_OID,
                getSelectedContextOid());

        if (isSchoolContext()) {
            studentScheduleChangeCriteria.addEqualTo(StudentScheduleChange.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            studentScheduleChangeCriteria.addNotEqualTo(StudentScheduleChange.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
            studentScheduleChangeCriteria.addNotEqualTo(StudentScheduleChange.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
        }

        return studentScheduleChangeCriteria;
    }

    /**
     * Gets the student schedule criteria.
     *
     * @return X 2 criteria
     */
    private X2Criteria getStudentScheduleCriteria() {
        X2Criteria studentScheduleCriteria = new X2Criteria();

        // Master type Class
        studentScheduleCriteria.addEqualTo(StudentSchedule.REL_SECTION + PATH_DELIMITER +
                MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                SchoolCourse.COL_MASTER_TYPE, SchoolCourse.MASTER_TYPE_CLASS);

        // From active Schedule for the selected year.
        studentScheduleCriteria.addEqualTo(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.REL_ACTIVE_SCHOOL_SCHEDULE_CONTEXTS + PATH_DELIMITER +
                SchoolScheduleContext.COL_DISTRICT_CONTEXT_OID,
                getSelectedContextOid());

        // check school or organization selection.
        if (isSchoolContext()) {
            studentScheduleCriteria.addEqualTo(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            studentScheduleCriteria.addNotEqualTo(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
            studentScheduleCriteria.addNotEqualTo(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
        }
        return studentScheduleCriteria;
    }

    /**
     * Gets the student transcript criteria.
     *
     * @return X 2 criteria
     */
    private X2Criteria getStudentTranscriptCriteria() {
        X2Criteria transcriptCriteria = new X2Criteria();

        transcriptCriteria.addEqualTo(Transcript.COL_DISTRICT_CONTEXT_OID, getSelectedContextOid());
        transcriptCriteria.addNotEmpty(Transcript.COL_MASTER_SCHEDULE_OID,
                getBroker().getPersistenceKey());

        if (isSchoolContext()) {
            transcriptCriteria.addEqualTo(Transcript.REL_SCHOOL,
                    getSchool().getOid());
        } else {
            transcriptCriteria.addNotEqualTo(Transcript.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
            transcriptCriteria.addNotEqualTo(Transcript.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
        }

        return transcriptCriteria;
    }

}
