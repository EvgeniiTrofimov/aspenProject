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

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.tools.exports.ExportJavaSource;
import com.x2dev.procedures.statereporting.common.DictionaryExtractor;
import com.x2dev.procedures.statereporting.common.FilterableFactory;
import com.x2dev.procedures.statereporting.common.ToolBean;
import com.x2dev.procedures.statereporting.common.FilterableFactory.Filterable;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolBeanColumn;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolBeanDefinition;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolBeanRelationship;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolSchool;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolStudent;
import com.x2dev.sis.model.beans.path.SisBeanPaths;
import com.x2dev.utils.DataGrid;
import com.x2dev.utils.types.PlainDate;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.function.BiFunction;
import java.util.stream.Collectors;

/**
 * @author Follett Software Company
 * @copyright 2021
 */
public class StudentProgramsExport extends ExportJavaSource {
    public static class HomelessProgram extends ToolBean {
        public static final ToolBeanColumn FIELD_STUDENT_OID =
                new ToolBeanColumn(SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION.studentOid());
        public static final ToolBeanColumn FIELD_START_DATE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION.startDate());
        public static final ToolBeanColumn FIELD_END_DATE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION.endDate());
        public static final ToolBeanColumn FIELD_MCKINNEY_VENTO_FLAG =
                new ToolBeanColumn(SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION, "DOE MCKINNEY VENTO");
        public static final ToolBeanColumn FIELD_HOMELESS_UNACCOMPANIED_FLAG =
                new ToolBeanColumn(SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION, "DOE UNACCOMPANIED HOMELESS");
        public static final ToolBeanColumn FIELD_NIGHT_TIME_RESIDENCE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION, "DOE NIGHT TIME RES");

        public static ToolBeanRelationship PARENT_STUDENT =
                new ToolBeanRelationship(HomelessProgram.class,
                        MyStudent.class,
                        SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION.studentOid().getPath(),
                        MyStudent.CHILD_KEY_HOMELESS_PROGRAM,
                        SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION.student().getRelationshipType());

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolBean.FULL_DEFINITION.expand(
                FIELD_STUDENT_OID,
                FIELD_START_DATE,
                FIELD_END_DATE,
                FIELD_MCKINNEY_VENTO_FLAG,
                FIELD_HOMELESS_UNACCOMPANIED_FLAG,
                FIELD_NIGHT_TIME_RESIDENCE).expandRelationships(PARENT_STUDENT)
                .expandCriteriaFunctions(new BiFunction<X2Broker, X2Criteria, X2Criteria>() {

                    @Override
                    public X2Criteria apply(X2Broker broker, X2Criteria criteria) {
                        DictionaryExtractor extractor = new DictionaryExtractor(broker);
                        List<String> codes = extractor
                                .getRefCodesWithStateValue(SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION
                                        .programCode().getField(broker), Arrays.asList("H"))
                                .stream().map(ReferenceCode::getCode).collect(Collectors.toList());
                        if (!codes.isEmpty()) {
                            criteria.addIn(SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION.programCode().getPath(),
                                    codes);
                        }
                        return criteria;
                    }
                });

        /**
         * Gets the X2 base class.
         *
         * @return Class
         */
        public static final Class getX2BaseClass() {
            return SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION.getBeanType();
        }

        /**
         * Instantiates a new student.
         *
         * @param columns RptBeanColumns
         * @param data Object[]
         */
        public HomelessProgram(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        public PlainDate getEndDate() {
            return getValueDate(FIELD_END_DATE);
        }

        public boolean getHomelessUnaccompaniedIndicator() {
            return getValueLogical(FIELD_HOMELESS_UNACCOMPANIED_FLAG);
        }

        public boolean getMcKinneyVentoIndicator() {
            return getValueLogical(FIELD_MCKINNEY_VENTO_FLAG);
        }

        public String getNighttimeResidence() {
            return getValueString(FIELD_NIGHT_TIME_RESIDENCE);
        }

        public PlainDate getStartDate() {
            return getValueDate(FIELD_START_DATE);
        }

        public MyStudent getStudent(X2Broker broker) {
            String stdOid = getValueString(FIELD_STUDENT_OID);
            return ToolBean.getBeanByOid(broker, MyStudent.class, stdOid, true);

        }
    }

    public static class MySchool extends ToolSchool {
        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION =
                ToolSchool.FULL_DEFINITION.expandSort(ToolSchool.FIELD_NAME)
                        .expandCriteriaFunctions(new BiFunction<X2Broker, X2Criteria, X2Criteria>() {

                            @Override
                            public X2Criteria apply(X2Broker broker, X2Criteria criteria) {
                                criteria.addNotEqualTo(SisBeanPaths.SCHOOL.inactiveIndicator().getPath(), Boolean.TRUE);
                                criteria.addNotEqualTo(SisBeanPaths.SCHOOL.archiveIndicator().getPath(), Boolean.TRUE);
                                return criteria;
                            }
                        });

        /**
         * @param columns
         * @param data
         */
        public MySchool(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

    }

    /**
     * The Class MyStudent.
     */
    public static class MyStudent extends ToolStudent {
        public static final String CHILD_KEY_HOMELESS_PROGRAM = "homelessPrograms";
        // Query Fields
        public static ToolBeanRelationship CHILD_HOMELESS_PROGRAMS =
                new ToolBeanRelationship(MyStudent.class,
                        HomelessProgram.class,
                        CHILD_KEY_HOMELESS_PROGRAM,
                        SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION.studentOid().toString(),
                        SisBeanPaths.STUDENT.programParticipation().getRelationshipType());

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolStudent.FULL_DEFINITION
                .expandRelationships(CHILD_HOMELESS_PROGRAMS)
                .expandSort(ToolStudent.FIELD_NAME_VIEW, ToolBean.FIELD_OID);

        /**
         * @param columns RptBeanColumns
         * @param data Object[]
         */
        public MyStudent(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        public Collection<HomelessProgram> getHomelessPrograms(X2Broker broker) {
            return (Collection<HomelessProgram>) getChildren(broker, CHILD_HOMELESS_PROGRAMS);
        }
    }

    private static final String COL_END_DATE = "endDate";
    private static final String COL_HOMELESS_UNACCOMPANIED = "unaccompanied";
    private static final String COL_MCKINNEY_VENTO = "mckinneyVento";
    private static final String COL_NIGHT_TIME_RESIDENCE = "nightTimeResidence";
    private static final String COL_SKL_NAME = "schoolName";
    private static final String COL_START_DATE = "startDate";
    private static final String COL_STD_NAME = "studentName";

    /**
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
            ToolBean.registerClass(MySchool.class);
            X2Criteria activeCriteria = new X2Criteria();
            Collection<String> activeStudentCodes = StudentManager.getActiveStudentCodeList(getOrganization());
            activeCriteria.addIn(SisBeanPaths.STUDENT.enrollmentStatus().toString(), activeStudentCodes);
            ToolBean.load(broker, dictionaryExtractor, MySchool.class);
            for (MySchool school : ToolBean.getCachedToolBeans(MySchool.class)) {

                X2Criteria criteria = activeCriteria.copy();
                criteria.addEqualTo(SisBeanPaths.STUDENT.schoolOid().getPath(), school.getOid());

                Filterable<MyStudent> students =
                        FilterableFactory.create(broker, MyStudent.class, criteria,
                                Arrays.asList(ToolStudent.FIELD_NAME_VIEW, ToolBean.FIELD_OID));

                // Preload programs
                X2Criteria endDateEmpty = new X2Criteria();
                endDateEmpty.addEmpty(SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION.endDate().getPath(),
                        broker.getPersistenceKey());
                ToolBean.resetCriteria(broker, HomelessProgram.class);
                ToolBean.addAndCriteria(broker, HomelessProgram.class, endDateEmpty);

                ToolBean.preload(broker, dictionaryExtractor, null, HomelessProgram.PARENT_STUDENT);

                for (MyStudent student : students.extract()) {
                    for (HomelessProgram homeless : student.getHomelessPrograms(broker)) {
                        grid.append();
                        grid.set(COL_SKL_NAME, school.getName());
                        grid.set(COL_STD_NAME, student.getNameView());
                        grid.set(COL_START_DATE, getDateString(homeless.getStartDate()));
                        grid.set(COL_END_DATE, getDateString(homeless.getEndDate()));
                        grid.set(COL_HOMELESS_UNACCOMPANIED,
                                getBooleanString(homeless.getHomelessUnaccompaniedIndicator()));
                        grid.set(COL_MCKINNEY_VENTO, getBooleanString(homeless.getMcKinneyVentoIndicator()));
                        grid.set(COL_NIGHT_TIME_RESIDENCE, homeless.getNighttimeResidence());
                    }
                }
                System.out.println(ToolBean.getCachedCounts());
                ToolBean.clearAllCachedToolBeans(MyStudent.class);
                ToolBean.clearAllCachedToolBeans(HomelessProgram.class);
            }
        } finally {
            broker.endSession();
        }
        return grid;
    }

    /**
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getColumnNames()
     */
    @Override
    protected List getColumnNames() {
        return Arrays.asList(COL_SKL_NAME,
                COL_STD_NAME,
                COL_START_DATE,
                COL_END_DATE,
                COL_HOMELESS_UNACCOMPANIED,
                COL_MCKINNEY_VENTO,
                COL_NIGHT_TIME_RESIDENCE);
    }

    /**
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getColumnUserNames()
     */
    @Override
    protected List getColumnUserNames() {
        return getColumnNames();
    }

    /**
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getComment()
     */
    @Override
    protected String getComment() {
        // TODO Auto-generated method stub
        return null;
    }

    /**
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
    private String getBooleanString(Boolean value) {
        return value == null ? "null" : value.toString();
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
}
