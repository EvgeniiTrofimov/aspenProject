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
import com.follett.fsc.core.framework.persistence.adjusters.JoinAdjuster.JoinType;
import com.follett.fsc.core.k12.tools.exports.ExportJavaSource;
import com.x2dev.procedures.statereporting.common.FilterableFactory;
import com.x2dev.procedures.statereporting.common.FilterableFactory.Filterable;
import com.x2dev.procedures.statereporting.common.FilterableFactory.Filterable.Folder;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolBeanDefinition;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolBeanDefinition.JoinAdjusterPattern;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolStudent;
import com.x2dev.sis.model.beans.path.SisBeanPaths;
import com.x2dev.utils.DataGrid;
import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

/**
 * @author Follett Software Company
 * @copyright 2021
 */
public class ToolBeanExport extends ExportJavaSource {
    public static class RptStudent extends ToolStudent {
        public static final ToolBeanColumn FIELD_LAST_NAME =
                new ToolBeanColumn(SisBeanPaths.STUDENT.person().lastName().toString());
        public static final ToolBeanColumn FIELD_FIRST_NAME =
                new ToolBeanColumn(SisBeanPaths.STUDENT.person().firstName().toString());
        public static final ToolBeanColumn FIELD_SCHOOL =
                new ToolBeanColumn(SisBeanPaths.STUDENT.school().name().toString());
        public static final ToolBeanColumn FIELD_GRADE =
                new ToolBeanColumn(SisBeanPaths.STUDENT.gradeLevel().toString());
        public static final ToolBeanColumn FIELD_SOME_ALIAS =
                new ToolBeanColumn(SisBeanPaths.STUDENT, "DOE BIRTH CITY");

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolStudent.FULL_DEFINITION.expand(
                FIELD_LAST_NAME,
                FIELD_FIRST_NAME,
                FIELD_SCHOOL,
                FIELD_GRADE,
                FIELD_SOME_ALIAS).expandJoinAdjusters(
                        new JoinAdjusterPattern(JoinType.LEFT_OUTER, SisBeanPaths.STUDENT_CONTACT.getDatabaseName()),
                        new JoinAdjusterPattern(JoinType.LEFT_OUTER, SisBeanPaths.CONTACT.getDatabaseName()));

        /**
         * Instantiates a new rpt student.
         *
         * @param columns RptBeanColumns
         * @param data Object[]
         */
        public RptStudent(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the first name.
         *
         * @return String
         */
        public String getFirstName() {
            return getValueString(FIELD_FIRST_NAME);
        }

        /**
         * Gets the grade.
         *
         * @return String
         */
        public String getGrade() {
            return getValueString(FIELD_GRADE);
        }

        /**
         * Gets the local id.
         *
         * @return String
         */
        @Override
        public String getLocalId() {
            return getValueString(FIELD_LOCAL_ID);
        }

        /**
         * Gets the last name.
         *
         * @return String
         */
        public String getLastName() {
            return getValueString(FIELD_LAST_NAME);
        }

        /**
         * Gets the oid.
         *
         * @return String
         */
        @Override
        public String getOid() {
            return getValueString(FIELD_OID);
        }

        /**
         * Gets the school.
         *
         * @return String
         */
        public String getSchool() {
            return getValueString(FIELD_SCHOOL);
        }

        /**
         * Gets the something.
         *
         * @return String
         */
        public String getSomething() {
            return getValueString(FIELD_SOME_ALIAS);
        }

        /**
         * Gets the state code.
         *
         * @return String
         */
        public String getStateCode() {
            return null;
        }
    }

    /**
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#gatherData()
     */
    @Override
    protected DataGrid gatherData() throws Exception {
        X2Criteria criteria = new X2Criteria();
        Filterable<RptStudent> students =
                FilterableFactory.create(getBroker(), RptStudent.class, criteria,
                        Arrays.asList(RptStudent.FIELD_FIRST_NAME, RptStudent.FIELD_LAST_NAME));

        Set<String> gradeList = new TreeSet();
        students.fold(gradeList, new Folder<RptStudent, Set<String>>() {

            @Override
            public Set<String> fold(RptStudent item, Set<String> accumulator) {
                String grade = item.getGrade();
                if (grade != null) {
                    accumulator.add(item.getGrade());
                }
                return accumulator;
            }

        });

        Set<String> schoolList = new TreeSet();
        students.fold(schoolList, new Folder<RptStudent, Set<String>>() {

            @Override
            public Set<String> fold(RptStudent item, Set<String> accumulator) {
                String school = item.getSchool();
                if (school != null) {
                    accumulator.add(item.getSchool());
                }
                return accumulator;
            }

        });


        DataGrid grid = new DataGrid();
        for (String school : schoolList) {
            for (String grade : gradeList) {
                students.filter(new ToolBeanDefinition(RptStudent.FIELD_SCHOOL, RptStudent.FIELD_GRADE),
                        Arrays.asList(school, grade)).extract().forEach(student -> {
                            grid.append();
                            grid.set("School", student.getSchool());
                            grid.set("Grade", student.getGrade());
                            grid.set("ID", student.getLocalId());
                            grid.set("First Name", student.getFirstName());
                            grid.set("Last Name", student.getLastName());
                            grid.set("Something", student.getSomething());
                        });
            }
        }
        return grid;
    }

    /**
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getColumnNames()
     */
    @Override
    protected List getColumnNames() {
        return Arrays.asList("School", "Grade", "ID", "First Name", "Last Name", "Something");
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
        return null;
    }

    /**
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getHeader()
     */
    @Override
    protected String getHeader() {
        return null;
    }

}
