/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2003 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.business.StudentManager;
import com.x2dev.sched.utils.ScheduleMap;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.beans.Schedule.ScheduleMode;
import com.x2dev.sis.tools.procedures.ClassSizeSummaryProcedure;
import com.x2dev.sis.web.schedule.ClassSizeSummaryGrid;
import com.x2dev.utils.StringUtils;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Procedure for standard class size procedure.
 *
 * @author X2 Development Corporation
 */
public class StandardClassSizeSummaryProcedure extends ClassSizeSummaryProcedure {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Calculates the class size for the passed start and end grade level.
     *
     * @param startGrade String
     * @param endGrade String
     * @param classizeCap double
     * @param sectionClass Class
     * @param currentYear boolean
     * @param yearContextOid String
     * @param grid ClassSizeSummaryGrid
     */
    @Override
    public void calculateClassSize(String startGrade,
                                   String endGrade,
                                   double classizeCap,
                                   Class sectionClass,
                                   boolean currentYear,
                                   String yearContextOid,
                                   ClassSizeSummaryGrid grid) {
        Class studentSectionClass =
                MasterSchedule.class.equals(sectionClass) ? StudentSchedule.class : BuildStudentSchedule.class;

        if (getSchedule() != null) {
            ScheduleTerm term = getScheduleTerm();

            if (term != null) {
                TreeMap sortedGradeLevels = StudentManager.buildGradeLevelMap(getBroker());
                HashMap<Integer, List<String>> matchingGradeLevelsByYog = new HashMap<Integer, List<String>>();
                HashMap<String, String> gradeLevelByStudent = new HashMap<String, String>();
                Map<String, ScheduleMap> sectionScheduleMap = getSectionToScheduleMap();
                Map<String, Collection<TeacherSection>> sectionTeacherSections = getSectionToTeacherSections();

                X2Criteria studentScheduleCriteria = new X2Criteria();
                studentScheduleCriteria.addEqualTo(StudentSection.COL_SCHEDULE_OID, getSchedule().getOid());
                if (getScheduleMode() == ScheduleMode.Elementary.ordinal()) {
                    studentScheduleCriteria.addEqualTo(
                            StudentSection.REL_SECTION + "." + Section.COL_ELEMENTARY_INDICATOR, Boolean.valueOf(true));
                } else if (getScheduleMode() == ScheduleMode.Secondary.ordinal()) {
                    studentScheduleCriteria.addEqualTo(
                            StudentSection.REL_SECTION + "." + Section.COL_ELEMENTARY_INDICATOR, Boolean.valueOf(false));
                }

                // Update students with grade level range criteria
                getStudentCriteria(getSchedule().getSchool(), startGrade, endGrade, currentYear,
                        studentScheduleCriteria);

                if (getSectionCourseIncludeCriteria() != null) {
                    studentScheduleCriteria.addAndCriteria(getSectionCourseIncludeCriteria());
                }

                if (getSectionRoomsIncludeCriteria() != null) {
                    studentScheduleCriteria.addAndCriteria(getSectionRoomsIncludeCriteria());
                }

                QueryByCriteria studentScheduleQuery =
                        new QueryByCriteria(studentSectionClass, studentScheduleCriteria);

                Map<String, Collection<StudentSection>> sectionsByRoomMap = getBroker().getGroupedCollectionByQuery(
                        studentScheduleQuery, StudentSection.REL_SECTION + "." + Section.COL_PRIMARY_ROOM_OID, 1000);
                Collection<SchedulePeriod> periods =
                        getScheduleMode() == 0 || getScheduleMode() == ScheduleMode.Secondary.ordinal()
                                ? getSchedule().getSchedulePeriods(getBroker(), false)
                                : getSchedule().getSchedulePeriods(getBroker(), true);
                Collection<ScheduleDay> days =
                        getScheduleMode() == 0 || getScheduleMode() == ScheduleMode.Secondary.ordinal()
                                ? getSchedule().getScheduleDays(getBroker(), false)
                                : getSchedule().getScheduleDays(getBroker(), true);

                for (String roomOid : sectionsByRoomMap.keySet()) {
                    SchoolRoom room = (SchoolRoom) getBroker().getBeanByOid(SchoolRoom.class, roomOid);

                    if (room != null) {
                        Collection<Section> sections = new HashSet<Section>();
                        HashMap<String, HashMap<String, Integer>> studentsByGrade =
                                new HashMap<String, HashMap<String, Integer>>();

                        /*
                         * Identify the students group for the room.
                         */
                        boolean calcNextGrade = !currentYear;
                        identifyStudent(startGrade,
                                endGrade,
                                sortedGradeLevels,
                                matchingGradeLevelsByYog,
                                gradeLevelByStudent,
                                calcNextGrade,
                                sectionsByRoomMap,
                                studentsByGrade,
                                roomOid,
                                term,
                                sections);

                        /*
                         * Looping through each term/day/period/room combination
                         */
                        for (int termNumber : term.countUsedTermNumbers()) {
                            for (ScheduleDay day : days) {
                                for (SchedulePeriod period : periods) {
                                    /*
                                     * Retrieve the grade level for most students.
                                     */
                                    String gradeLevelWithMaxCount = "";
                                    int maxCount = 0;

                                    String timeSlotKey = termNumber + "|" + day.getNumber() + "|" + period.getNumber();
                                    HashMap<String, Integer> studentsByGradeForTimeSlot =
                                            studentsByGrade.get(timeSlotKey);

                                    if (studentsByGradeForTimeSlot != null) {
                                        for (String gradeLevel : studentsByGradeForTimeSlot.keySet()) {
                                            if (studentsByGradeForTimeSlot.get(gradeLevel).intValue() > maxCount) {
                                                gradeLevelWithMaxCount = gradeLevel;
                                                maxCount = studentsByGradeForTimeSlot.get(gradeLevel).intValue();
                                            }
                                        }
                                    }

                                    Collection<SisStaff> teachersInSection = new HashSet<SisStaff>();
                                    int totalStudentCounts = 0;
                                    Collection<String> courses = new HashSet<String>();

                                    for (Section section : sections) {
                                        ScheduleMap sectionMap = sectionScheduleMap.get(section.getOid());
                                        if (sectionMap == null) {
                                            sectionMap = section.getScheduleMap(getBroker());
                                            sectionScheduleMap.put(section.getOid(), sectionMap);
                                        }
                                        if (sectionMap != null
                                                && sectionMap.getBaseTerms() < getSchedule().getTerms()) {
                                            sectionMap = sectionMap.expand(getSchedule().getTerms());
                                        }

                                        if (sectionMap != null &&
                                                sectionMap.isScheduled(termNumber, day.getNumber(),
                                                        period.getNumber())) {
                                            Collection teacherSections = sectionTeacherSections.get(section.getOid());
                                            if (teacherSections == null) {
                                                teacherSections = MasterSchedule.class.equals(sectionClass)
                                                        ? ((MasterSchedule) section).getTeacherSections()
                                                        : ((BuildMasterSchedule) section).getTeacherSections();
                                                sectionTeacherSections.put(section.getOid(), teacherSections);
                                            }
                                            for (TeacherSection teacherSection : (Collection<TeacherSection>) teacherSections) {
                                                teachersInSection.add(teacherSection.getStaff());
                                            }

                                            totalStudentCounts += section.getEnrollmentTotal();
                                            courses.add(getSectionCourseNumber(section));
                                        }
                                    }

                                    double avergaeSize = teachersInSection.size() == 0 ? 0
                                            : totalStudentCounts / teachersInSection.size();

                                    if (totalStudentCounts > 0) {
                                        grid.append();
                                        grid.set(ClassSizeSummaryGrid.COL_COURSE,
                                                StringUtils.convertCollectionToDelimitedString(courses, ','));
                                        grid.set(ClassSizeSummaryGrid.COL_CLASS_SIZE, Double.valueOf(avergaeSize));
                                        grid.set(ClassSizeSummaryGrid.COL_CLASS_SIZE_CAP, Double.valueOf(classizeCap));
                                        grid.set(ClassSizeSummaryGrid.COL_GROUP,
                                                String.valueOf(startGrade + " - " + endGrade));
                                        grid.set(ClassSizeSummaryGrid.COL_PERIOD, period.getId());
                                        grid.set(ClassSizeSummaryGrid.COL_DAY, day.getId());
                                        grid.set(ClassSizeSummaryGrid.COL_ROOM_NUMBER, getSectionRoomNumber(room));
                                        grid.set(ClassSizeSummaryGrid.COL_TERM, Integer.valueOf(termNumber));
                                        grid.set(ClassSizeSummaryGrid.COL_TEACHERS,
                                                Integer.valueOf(teachersInSection.size()));
                                        grid.set(ClassSizeSummaryGrid.COL_OVER,
                                                Boolean.valueOf(avergaeSize > classizeCap ? true : false));
                                        grid.set(ClassSizeSummaryGrid.COL_GRADE, gradeLevelWithMaxCount);
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    /**
     * Identifies the list of sections and student involved.
     *
     * @param startGrade String
     * @param endGrade String
     * @param sortedGradeLevels TreeMap
     * @param matchingGradeLevelsByYog HashMap<Integer,List<String>>
     * @param gradeLevelByStudent HashMap<String,String>
     * @param calcNextGrade boolean
     * @param sectionsByRoomMap Map<String,Collection<StudentSection>>
     * @param studentsByGrade HashMap<String,HashMap<String,Integer>>
     * @param roomOid String
     * @param term ScheduleTerm
     * @param sections Collection<Section>
     */
    private void identifyStudent(String startGrade,
                                 String endGrade,
                                 TreeMap sortedGradeLevels,
                                 HashMap<Integer, List<String>> matchingGradeLevelsByYog,
                                 HashMap<String, String> gradeLevelByStudent,
                                 boolean calcNextGrade,
                                 Map<String, Collection<StudentSection>> sectionsByRoomMap,
                                 HashMap<String, HashMap<String, Integer>> studentsByGrade,
                                 String roomOid,
                                 ScheduleTerm term,
                                 Collection<Section> sections) {
        int maxGradeLevel = StudentManager.getMaxGradeLevel(getBroker());

        for (StudentSection studentSection : sectionsByRoomMap.get(roomOid)) {
            String studentGradeLevel = gradeLevelByStudent.get(studentSection.getStudentOid());
            if (studentGradeLevel == null) {
                // Calculate the student grade level. It can be the student's next grade level.
                studentGradeLevel = studentSection.getStudent().getGradeLevel();
                if (calcNextGrade) {
                    List matchingGradeLevels =
                            matchingGradeLevelsByYog.get(Integer.valueOf(studentSection.getStudent().getYog()));
                    if (matchingGradeLevels == null) {
                        matchingGradeLevels = StudentManager.getMatchingGradeLevels(maxGradeLevel,
                                studentSection.getStudent().getYog(),
                                studentSection.getSchedule().getDistrictContext().getSchoolYear(),
                                sortedGradeLevels);

                        matchingGradeLevelsByYog.put(Integer.valueOf(studentSection.getStudent().getYog()),
                                matchingGradeLevels);
                    }
                    studentGradeLevel = matchingGradeLevels.size() == 1 ? (String) matchingGradeLevels.get(0) : null;
                }

                gradeLevelByStudent.put(studentSection.getStudentOid(), studentGradeLevel);
            }


            /*
             * The student must within the range
             */
            if (studentGradeLevel != null &&
                    studentGradeLevel.compareToIgnoreCase(startGrade) >= 0 &&
                    studentGradeLevel.compareToIgnoreCase(endGrade) <= 0) {
                ScheduleMap studentSectionMap = studentSection.getScheduleMap(getBroker());

                if (studentSectionMap != null) {
                    for (int termNumber : term.countUsedTermNumbers()) {
                        for (int day = 1; day <= studentSection.getSchedule().getDays(); day++) {
                            for (int period = 1; period <= studentSection.getSchedule().getPeriods(); period++) {
                                if (studentSectionMap.isScheduled(termNumber, day, period)) {
                                    String timeSlotKey = termNumber + "|" + day + "|" + period;
                                    HashMap<String, Integer> studentsByGradeForTimeSlot =
                                            studentsByGrade.get(timeSlotKey);
                                    if (studentsByGradeForTimeSlot == null) {
                                        studentsByGradeForTimeSlot = new HashMap<String, Integer>();
                                        studentsByGrade.put(timeSlotKey, studentsByGradeForTimeSlot);
                                    }

                                    Integer countByGradeLevel = studentsByGradeForTimeSlot.get(studentGradeLevel);
                                    if (countByGradeLevel == null) {
                                        studentsByGradeForTimeSlot.put(studentGradeLevel, Integer.valueOf(1));
                                    } else {
                                        studentsByGradeForTimeSlot.put(studentGradeLevel,
                                                Integer.valueOf(countByGradeLevel.intValue() + 1));
                                    }
                                }
                            }
                        }
                    }

                    // If there is any student that is within the grade level range, count the
                    // section in.
                    sections.add(studentSection.getSection());
                }
            }
        }
    }

    /**
     * Gets the section course include criteria.
     *
     * @return X 2 criteria
     * @see
     *      com.x2dev.sis.tools.procedures.ClassSizeSummaryProcedure#getSectionCourseIncludeCriteria()
     */
    @Override
    public X2Criteria getSectionCourseIncludeCriteria() {
        /*
         * Be default, only "class" sections are included.
         */
        X2Criteria studentSectionCourseCriteria = new X2Criteria();
        studentSectionCourseCriteria.addEqualTo(
                StudentSection.REL_SECTION + "." + Section.REL_SCHOOL_COURSE + "." + SchoolCourse.COL_MASTER_TYPE,
                SchoolCourse.MASTER_TYPE_CLASS);
        studentSectionCourseCriteria.addGreaterThan(StudentSection.REL_SECTION + "." + Section.COL_ENROLLMENT_TOTAL,
                Integer.valueOf(0));
        if (getScheduleMode() == ScheduleMode.Elementary.ordinal()) {
            studentSectionCourseCriteria.addEqualTo(StudentSection.REL_SECTION + "." + Section.COL_ELEMENTARY_INDICATOR,
                    Boolean.valueOf(true));
        } else if (getScheduleMode() == ScheduleMode.Secondary.ordinal()) {
            studentSectionCourseCriteria.addEqualTo(StudentSection.REL_SECTION + "." + Section.COL_ELEMENTARY_INDICATOR,
                    Boolean.valueOf(false));
        }

        return studentSectionCourseCriteria;
    }

    /**
     * Gets the section course number.
     *
     * @param section Section
     * @return String
     * @see
     *      com.x2dev.sis.tools.procedures.ClassSizeSummaryProcedure#getSectionCourseNumber(com.x2dev.sis
     *      .model.beans.Section)
     */
    @Override
    public String getSectionCourseNumber(Section section) {
        SchoolCourse course = section.getSchoolCourse();
        String description = StringUtils.isEmpty(course.getShortDescription()) ? course.getDescription()
                : course.getShortDescription();

        return StringUtils.isEmpty(description) ? course.getNumber() : course.getNumber() + " (" + description + ")";
    }

    /**
     * Gets the section room number.
     *
     * @param room SchoolRoom
     * @return String
     * @see
     *      com.x2dev.sis.tools.procedures.ClassSizeSummaryProcedure#getSectionCourseNumber(com.x2dev.sis
     *      .model.beans.Section)
     */
    @Override
    public String getSectionRoomNumber(SchoolRoom room) {
        return room.getRoomNumber();
    }

    /**
     * Gets the section rooms include criteria.
     *
     * @return X 2 criteria
     * @see
     *      com.x2dev.sis.tools.procedures.ClassSizeSummaryProcedure#getSectionRoomsIncludeCriteria()
     */
    @Override
    public X2Criteria getSectionRoomsIncludeCriteria() {
        /*
         * By default, only sections with non-empty room should be involved.
         */
        X2Criteria studentSectionRoomCriteria = new X2Criteria();
        studentSectionRoomCriteria.addNotEmpty(StudentSection.REL_SECTION + "." + Section.COL_PRIMARY_ROOM_OID,
                getBroker().getPersistenceKey());
        if (getScheduleMode() == ScheduleMode.Elementary.ordinal()) {
            studentSectionRoomCriteria.addEqualTo(StudentSection.REL_SECTION + "." + Section.COL_ELEMENTARY_INDICATOR,
                    Boolean.valueOf(true));
        } else if (getScheduleMode() == ScheduleMode.Secondary.ordinal()) {
            studentSectionRoomCriteria.addEqualTo(StudentSection.REL_SECTION + "." + Section.COL_ELEMENTARY_INDICATOR,
                    Boolean.valueOf(false));
        }
        return studentSectionRoomCriteria;
    }
}
