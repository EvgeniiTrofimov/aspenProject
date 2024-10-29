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

import com.follett.fsc.core.framework.persistence.adjusters.JoinAdjuster.JoinType;
import com.x2dev.procedures.statereporting.common.ToolBean;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolBeanDefinition.JoinAdjusterPattern;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportData;
import com.x2dev.sis.model.beans.path.SisBeanPaths;
import com.x2dev.utils.X2BaseException;

/**
 * @author Follett Software Company
 * @copyright 2021
 */
public class MstStateReportData extends StateReportData {
    public static class MstBean extends ToolBean {
        // Query Fields
        public static final ToolBeanColumn FIELD_SCHOOL =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER.schedule().school().name());
        public static final ToolBeanColumn FIELD_COURSE_VIEW =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER.courseView());
        public static final ToolBeanColumn FIELD_DESCRIPTION =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER.description());
        public static final ToolBeanColumn FIELD_STAFF_NAME =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER.primaryStaff().nameView());
        public static final ToolBeanColumn FIELD_PLATOON =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER.platoonCode());
        public static final ToolBeanColumn FIELD_SCHEDULE_TERM_CODE =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER.scheduleTerm().code());
        public static final ToolBeanColumn FIELD_SCHEDULE =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER.scheduleDisplay());
        public static final ToolBeanColumn FIELD_ROOM_NUMBER =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER.primaryRoom().roomNumber());
        public static final ToolBeanColumn FIELD_ENROLLMENT_TOTAL =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER.enrollmentTotal());
        public static final ToolBeanColumn FIELD_ENROLLMENT_MAX =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER.enrollmentMax());
        public static final ToolBeanColumn FIELD_ACADEMIC_LEVEL =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER.schoolCourse().course().academicLevel());
        public static final ToolBeanColumn FIELD_VOCATIONAL_CLASSIFICATION =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER.schoolCourse().course(),
                        "DOE VOC CLASSIFICATION");
        public static final ToolBeanColumn FIELD_DEPARTMENT =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER.schoolCourse().course().departmentCode());
        public static final ToolBeanColumn FIELD_STATE_COURSE_CODE =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER.schoolCourse().course(),
                        "DOE SDE COURSE CODE");

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolBean.FULL_DEFINITION.expand(FIELD_SCHOOL,
                FIELD_COURSE_VIEW,
                FIELD_DESCRIPTION,
                FIELD_STAFF_NAME,
                FIELD_PLATOON,
                FIELD_SCHEDULE_TERM_CODE,
                FIELD_SCHEDULE,
                FIELD_ROOM_NUMBER,
                FIELD_ENROLLMENT_TOTAL,
                FIELD_ENROLLMENT_MAX,
                FIELD_ACADEMIC_LEVEL,
                FIELD_VOCATIONAL_CLASSIFICATION,
                FIELD_DEPARTMENT,
                FIELD_STATE_COURSE_CODE).expandJoinAdjusters(
                        new JoinAdjusterPattern(JoinType.LEFT_OUTER, SisBeanPaths.SCHEDULE_TERM.getDatabaseName()),
                        new JoinAdjusterPattern(JoinType.LEFT_OUTER, SisBeanPaths.STAFF.getDatabaseName()),
                        new JoinAdjusterPattern(JoinType.LEFT_OUTER, SisBeanPaths.SCHOOL_ROOM.getDatabaseName()));

        /**
         * Gets the X2 base class.
         *
         * @return Class
         */
        public static final Class getX2BaseClass() {
            return SisBeanPaths.SCHEDULE_MASTER.getBeanType();
        }

        /**
         * Instantiates a new enrollment.
         *
         * @param columns RptBeanColumns
         * @param data Object[]
         */
        public MstBean(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }
    }

    /**
     * @see com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportData#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();

        ToolBean.registerClass(MstBean.class);
    }

}
