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


import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.BuildMasterSchedule;
import com.x2dev.sis.model.beans.BuildStudentSchedule;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.Section;
import com.x2dev.sis.model.beans.StudentSchedule;
import com.x2dev.sis.model.beans.StudentScheduleGroup;
import com.x2dev.sis.model.beans.StudentSection;
import com.x2dev.sis.model.business.ElementaryScheduleManager;
import com.x2dev.sis.model.business.schedule.ScheduleManager;
import com.x2dev.sis.web.schedule.ScheduleUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.util.Collection;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Procedure to copy school capacity records from one school year context to another.
 *
 * @author X2 Development Corporation
 */
public class PushScheduleCrossGroupProcedure extends ProcedureJavaSource {
    /**
     * Name for the "source group" input parameter. The value is a String.
     */
    public static final String SOURCE_GROUP_PARAM = "sourceGroup";

    /**
     * Name for the "target groups" input parameter. The value is a String.
     */
    public static final String TARGET_GROUP_PARAM = "targetGroup";

    private ModelBroker m_broker;
    private Schedule m_schedule;
    private Class m_sectionClass;

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        /*
         * Retrieve other groups to share the same program code.
         */
        StudentScheduleGroup sourceGroup = (StudentScheduleGroup) getBroker().getBeanByOid(StudentScheduleGroup.class,
                (String) getParameter(SOURCE_GROUP_PARAM));

        Collection<String> targetGroups =
                StringUtils.convertDelimitedStringToList((String) getParameter(TARGET_GROUP_PARAM), ',');

        X2Criteria groupCriteria = new X2Criteria();
        groupCriteria.addEqualTo(StudentScheduleGroup.COL_DISTRICT_CONTEXT_OID, m_schedule.getDistrictContextOid());
        groupCriteria.addEqualTo(StudentScheduleGroup.COL_SCHOOL_OID, m_schedule.getSchoolOid());
        groupCriteria.addEqualTo(StudentScheduleGroup.COL_PROGRAM_OID, sourceGroup.getProgramOid());
        groupCriteria.addNotEqualTo(X2BaseBean.COL_OID, sourceGroup.getOid());
        if (!targetGroups.isEmpty()) {
            groupCriteria.addIn(X2BaseBean.COL_OID, targetGroups);
        }

        QueryByCriteria groupQuery = new QueryByCriteria(StudentScheduleGroup.class, groupCriteria);
        Collection<StudentScheduleGroup> groups = getBroker().getCollectionByQuery(groupQuery);

        /*
         * Sections for the source group
         */
        X2Criteria sectionCriteria = new X2Criteria();
        sectionCriteria.addEqualTo(Section.COL_SCHEDULE_OID, m_schedule.getOid());
        sectionCriteria.addEqualTo(Section.REL_SCHOOL_COURSE + "." + SchoolCourse.COL_MASTER_TYPE,
                SchoolCourse.MASTER_TYPE_CLASS);

        if (sourceGroup.getParentStudentScheduleGroup() != null) {
            sectionCriteria.addEqualTo(Section.COL_SECTION_TYPE,
                    sourceGroup.getParentStudentScheduleGroup().getId().trim());
            sectionCriteria.addEqualTo(Section.COL_PLATOON_CODE, sourceGroup.getId().trim());
        } else {
            sectionCriteria.addEqualTo(Section.COL_SECTION_TYPE, sourceGroup.getId().trim());
            sectionCriteria.addEmpty(Section.COL_PLATOON_CODE, getBroker().getPersistenceKey());
        }

        QueryByCriteria sectionQuery = new QueryByCriteria(m_sectionClass, sectionCriteria);
        Collection<Section> sourceSections = getBroker().getCollectionByQuery(sectionQuery);

        /*
         * Now push the schedule to other groups
         */
        for (StudentScheduleGroup otherGroup : groups) {

            logMessage("Target Group " + otherGroup.getDescription());

            /*
             * Collect the list of students for the group
             */
            Collection<String> students =
                    ElementaryScheduleManager.getGroupMembersForGroup(m_schedule, otherGroup, false, m_broker);


            /*
             * For each source section, find the matching section and update it accordingly.
             */
            for (Section sourceSection : sourceSections) {
                Section otherSection = getSectionForGroupCourse(m_schedule, m_sectionClass, otherGroup,
                        sourceSection.getSchoolCourseOid(), m_broker);

                logMessage("Source Section " + sourceSection.getCourseView());
                logMessage("Other Matching Section " + otherSection != null ? otherSection.getCourseView() : "");

                // If the matching section is scheduled, leave it alone such as specials
                if (otherSection != null &&
                        StringUtils.isEmpty(otherSection.getScheduleDisplay())) {
                    otherSection.setScheduleTermOid(sourceSection.getScheduleTermOid());
                    otherSection.setScheduleDisplay(sourceSection.getScheduleDisplay());

                    m_broker.saveBeanForced((X2BaseBean) otherSection);

                    if (students != null &&
                            !students.isEmpty()) {
                        /*
                         * Delete existing student schedule
                         */
                        X2Criteria studentSectionCriteria = new X2Criteria();
                        studentSectionCriteria.addEqualTo(StudentSection.COL_SECTION_OID, otherSection.getOid());

                        QueryByCriteria studentSectionQuery =
                                new QueryByCriteria(BuildStudentSchedule.class, studentSectionCriteria);
                        m_broker.deleteByQuery(studentSectionQuery);

                        /*
                         * Re-create student schedules
                         */
                        for (String student : students) {
                            StudentSection studentSection = BuildMasterSchedule.class.equals(m_sectionClass)
                                    ? new BuildStudentSchedule(getBroker().getPersistenceKey())
                                    : new StudentSchedule(getBroker().getPersistenceKey());

                            studentSection.setScheduleDisplay(otherSection.getScheduleDisplay());
                            studentSection.setScheduleOid(m_schedule.getOid());
                            studentSection.setSectionOid(otherSection.getOid());
                            studentSection.setStudentOid(student);

                            m_broker.saveBeanForced((X2BaseBean) studentSection);
                        }

                        ScheduleManager manager = new ScheduleManager(m_broker);
                        if (BuildMasterSchedule.class.equals(m_sectionClass)) {
                            manager.recalculateEnrollmentTotalForBuildMaster((BuildMasterSchedule) otherSection);
                        } else {
                            manager.recalculateEnrollmentTotalForMaster((MasterSchedule) otherSection);
                        }
                    }
                }
            }
        }
    }

    /**
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.UserDataContainer)
     */
    /*
     * (non-Javadoc)
     *
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.
     * UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException {
        m_schedule = ScheduleUtils.getSchedule(userData);
        m_sectionClass = ScheduleUtils.getSectionClass(userData);
        m_broker = new ModelBroker(userData.getPrivilegeSet());
        super.saveState(userData);
    }

    /**
     * Returns the section for the passed group and course.
     *
     * @param schedule Schedule
     * @param sectionClass Class
     * @param group StudentScheduleGroup
     * @param courseOid String
     * @param broker ModelBroker
     * @return Section
     */
    private Section getSectionForGroupCourse(Schedule schedule,
                                             Class sectionClass,
                                             StudentScheduleGroup group,
                                             String courseOid,
                                             ModelBroker broker) {
        /*
         * Sections should always exist now.
         * Retrieve the section by matching the section type and platoon code
         */
        X2Criteria sectionCriteria = new X2Criteria();
        sectionCriteria.addEqualTo(Section.COL_SCHEDULE_OID, schedule.getOid());
        sectionCriteria.addEqualTo(Section.COL_SCHOOL_COURSE_OID, courseOid);

        if (group.getParentStudentScheduleGroup() != null) {
            sectionCriteria.addEqualTo(Section.COL_SECTION_TYPE, group.getParentStudentScheduleGroup().getId().trim());
            sectionCriteria.addEqualTo(Section.COL_PLATOON_CODE, group.getId().trim());
        } else {
            sectionCriteria.addEqualTo(Section.COL_SECTION_TYPE, group.getId().trim());
            sectionCriteria.addEmpty(Section.COL_PLATOON_CODE, broker.getPersistenceKey());
        }

        QueryByCriteria sectionQuery = new QueryByCriteria(sectionClass, sectionCriteria);

        return (Section) broker.getBeanByQuery(sectionQuery);
    }
}
