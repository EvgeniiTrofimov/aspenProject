/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2004 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.ScheduleLunchConfig;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.Section;
// import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentSection;
import com.x2dev.sis.model.business.ElementaryScheduleManager;
import com.x2dev.sis.tools.reports.ScheduleReportHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.TreeMap;
import java.util.TreeSet;
import net.sf.jasperreports.engine.JRDataSource;
// import java.util.logging.Level;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares the data for the "Elementary Lunch Schedule Summary" report.
 *
 * @author X2 Development Corporation
 */
public class ElementaryLunchScheduleSummaryData extends ReportJavaSourceNet {

    private static final long serialVersionUID = 1L;

    private static final String GRID_FIELD_LUNCH_BUILDING = "lunchBuildingId";
    private static final String GRID_FIELD_LUNCH_GROUP = "lunchGroups";
    private static final String GRID_FIELD_LUNCH_LINE = "lunchLine";
    private static final String GRID_FIELD_LUNCH_TABLE = "lunchTable";
    private static final String GRID_FIELD_LUNCH_TEACHER = "lunchTeacher";
    private static final String GRID_FIELD_LUNCH_TIME = "lunchTime";

    // Input parameters
    private static final String LUNCH_CONFIG_OID_PARAM = "lunchConfigOid";

    private static final String PARAM_SCHOOL_YEAR = "schoolYear";
    private static final String PARAM_DISPLAY_BUILDING_ID = "displayBuildID";

    private static final String LUNCH_NOTE_PREFIX_LINE = "Line:";
    private static final String LUNCH_NOTE_SEPARATOR = ";";

    private ScheduleReportHelper m_reportHelper;

    /**
     * Gather data.
     *
     * @param lunchLine
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {

        ReportDataGrid grid = new ReportDataGrid(10, 15);
        ScheduleLunchConfig lunchConfig = (ScheduleLunchConfig) getBroker().getBeanByOid(ScheduleLunchConfig.class,
                (String) getParameter(LUNCH_CONFIG_OID_PARAM));

        X2Criteria lunchSectionCriteria = new X2Criteria();
        lunchSectionCriteria.addEqualTo(Section.COL_SCHEDULE_OID, m_reportHelper.getSchedule().getOid());
        lunchSectionCriteria.addEqualTo(Section.REL_SCHOOL_COURSE + "." + SchoolCourse.COL_MASTER_TYPE,
                SchoolCourse.MASTER_TYPE_LUNCH);
        lunchSectionCriteria.addNotEmpty(Section.COL_SCHEDULE_DISPLAY, getBroker().getPersistenceKey());

        QueryByCriteria lunchSectionQuery = new QueryByCriteria(m_reportHelper.getSectionClass(), lunchSectionCriteria);
        lunchSectionQuery.addOrderByAscending(Section.COL_HOUSE_CODE);
        if (m_reportHelper.getSchedule().isLunchSectionByHomeroom()) {
            lunchSectionQuery.addOrderByAscending(Section.COL_SECTION_NUMBER);
        } else {
            lunchSectionQuery.addOrderByAscending(Section.COL_SCHEDULE_DISPLAY);
        }

        Collection<String> lunchBuildingIds = new HashSet<String>();
        QueryIterator lunchSectionIterator = getBroker().getIteratorByQuery(lunchSectionQuery);
        while (lunchSectionIterator.hasNext()) {

            Section lunchSection = (Section) lunchSectionIterator.next();
            if (!StringUtils.isEmpty(lunchSection.getHouseCode())) {
                lunchBuildingIds.add(lunchSection.getHouseCode());
            }
            if (lunchConfig == null
                    || StringUtils.isEqual(lunchConfig.getLunchBuildingId().trim(),
                            lunchSection.getHouseCode().trim())) {
                grid.append();

                String lunchTime = lunchSection.getScheduleDisplay();
                grid.set(GRID_FIELD_LUNCH_TIME,
                        lunchTime == null ? "" : lunchTime.substring(0, lunchTime.indexOf("(")));
                grid.set(GRID_FIELD_LUNCH_BUILDING, lunchSection.getHouseCode());

                TreeSet<String> lines = new TreeSet<String>();
                Map<String, Map<String, Collection<String>>> tablesByLine =
                        new HashMap<String, Map<String, Collection<String>>>();
                Map<String, Collection<String>> teacherByLine = new HashMap<String, Collection<String>>();

                parseLunchNotes(lunchSection, lines, tablesByLine, teacherByLine);

                int loops = lines.size() % 2 == 0 ? lines.size() / 2 : lines.size() / 2 + 1;

                Collection<String> lineProcessed = new ArrayList<String>();
                if (loops == 0 && m_reportHelper.getSchedule().isLunchSectionByHomeroom()) {
                    Collection<String> groups = getGroupInLunch(lunchSection, null);
                    grid.set(GRID_FIELD_LUNCH_GROUP + "_" + String.valueOf(1),
                            StringUtils.convertCollectionToDelimitedString(groups, ","));

                } else {
                    for (int loop = 0; loop < loops; loop++) {
                        if (loop > 0) {
                            grid.append();
                            grid.set(GRID_FIELD_LUNCH_TIME,
                                    lunchTime == null ? "" : lunchTime.substring(0, lunchTime.indexOf("(")));
                            grid.set(GRID_FIELD_LUNCH_BUILDING, lunchSection.getHouseCode());
                        }

                        int i = 1;
                        for (String lunchLine : lines) {

                            if (!lineProcessed.contains(lunchLine)) {
                                lineProcessed.add(lunchLine);
                                Collection<String> groups = getGroupInLunch(lunchSection, lunchLine);
                                grid.set(GRID_FIELD_LUNCH_GROUP + "_" + String.valueOf(i),
                                        StringUtils.convertCollectionToDelimitedString(groups, ","));

                                Collection<String> teachers = teacherByLine.get(lunchLine);
                                grid.set(GRID_FIELD_LUNCH_TEACHER + "_" + String.valueOf(i), teachers == null ? ""
                                        : StringUtils.convertCollectionToDelimitedString(teachers, "\n"));
                                grid.set(GRID_FIELD_LUNCH_LINE + "_" + String.valueOf(i), lunchLine);

                                Map<String, Collection<String>> tablesByTeacher = tablesByLine.get(lunchLine);
                                String lunchLines = "";
                                if (tablesByTeacher != null) {
                                    for (String teacher : tablesByTeacher.keySet()) {
                                        lunchLines += tablesByTeacher.get(teacher) == null ? ""
                                                : StringUtils.convertCollectionToDelimitedString(
                                                        tablesByTeacher.get(teacher), ",");

                                        if (tablesByTeacher.size() > 1) {
                                            lunchLines += "\n";
                                        }
                                    }
                                }
                                grid.set(GRID_FIELD_LUNCH_TABLE + "_" + String.valueOf(i),
                                        lunchLines);

                                i++;
                            }

                            if (i > 2) {
                                break;
                            }
                        }
                    }
                }
            }
        }

        grid.beforeTop();

        addParameter(PARAM_SCHOOL_YEAR, m_reportHelper.getSchedule().getDistrictContext().getContextId());
        addParameter(PARAM_DISPLAY_BUILDING_ID, Boolean.valueOf(lunchBuildingIds.size() > 1));

        return grid;
    }

    /**
     * Returns the lunch line
     *
     * @param section Section
     * @param lines Collection<String>
     * @param tablesByLine Map<String, Collection<String>>
     * @param teachersByLine Map<Strng, Collection<String>>
     */
    private void parseLunchNotes(Section section,
                                 TreeSet<String> lines,
                                 Map<String, Map<String, Collection<String>>> tablesByLineByTeacher,
                                 Map<String, Collection<String>> teachersByLine) {
        X2Criteria studentInLunchCriteria = new X2Criteria();
        studentInLunchCriteria.addEqualTo(StudentSection.COL_SCHEDULE_OID, m_reportHelper.getScheduleOid());
        studentInLunchCriteria.addEqualTo(StudentSection.COL_SECTION_OID, section.getOid());

        SubQuery studentInSectionSubQuery = new SubQuery(m_reportHelper.getStudentSectionClass(),
                StudentSection.COL_SCHEDULE_NOTES, studentInLunchCriteria);
        studentInSectionSubQuery.setDistinct(true);
        studentInSectionSubQuery.addOrderByAscending(StudentSection.COL_SCHEDULE_NOTES);

        Collection<String> lunchNotes = getBroker().getSubQueryCollectionByQuery(studentInSectionSubQuery);
        for (String lunchNote : lunchNotes) {
            if (lunchNote != null) {

                String[] split = lunchNote.split("\\" + LUNCH_NOTE_SEPARATOR);

                String line = split[0].substring(split[0].indexOf(":") + 1).trim();
                String tables = split.length > 1 ? split[1].substring(split[1].indexOf(":") + 1).trim() : null;
                String teacher = split.length > 2 ? split[2].substring(split[2].indexOf(":") + 1).trim() : null;

                if (line != null) {
                    lines.add(line);

                    if (!StringUtils.isEmpty(teacher)) {
                        Collection<String> existingTeacher = teachersByLine.get(line);
                        if (existingTeacher == null) {
                            existingTeacher = new TreeSet<String>();
                            teachersByLine.put(line, existingTeacher);
                        }
                        existingTeacher.add(teacher);
                    }

                    if (tables != null) {
                        Map<String, Collection<String>> existingTables = tablesByLineByTeacher.get(line);
                        if (existingTables == null) {
                            existingTables = new TreeMap<String, Collection<String>>();
                            tablesByLineByTeacher.put(line, existingTables);
                        }

                        String teacherKey = StringUtils.isEmpty(teacher) ? "" : teacher;
                        Collection<String> tablesByTeacher = existingTables.get(teacherKey);
                        if (tablesByTeacher == null) {
                            tablesByTeacher = new TreeSet<String>();
                            existingTables.put(teacherKey, tablesByTeacher);
                        }
                        tablesByTeacher.add(tables);
                    }
                }
            }
        }
    }

    /**
     * Returns the groups in the lunch section
     *
     * @param section Section
     * @param lunchLine String
     *
     * @return Collection<String>
     */
    private Collection<String> getGroupInLunch(Section section, String lunchLine) {
        X2Criteria studentInLunchCriteria = new X2Criteria();
        studentInLunchCriteria.addEqualTo(StudentSection.COL_SCHEDULE_OID, m_reportHelper.getScheduleOid());
        studentInLunchCriteria.addEqualTo(StudentSection.COL_SECTION_OID, section.getOid());

        if (!StringUtils.isEmpty(lunchLine)) {
            X2Criteria lineCriteria1 = new X2Criteria();
            lineCriteria1.addContains(StudentSection.COL_SCHEDULE_NOTES, LUNCH_NOTE_PREFIX_LINE + lunchLine);

            X2Criteria lineCriteria2 = new X2Criteria();
            lineCriteria2.addContains(StudentSection.COL_SCHEDULE_NOTES, LUNCH_NOTE_PREFIX_LINE + " " + lunchLine);

            X2Criteria lineCriteria = new X2Criteria();
            lineCriteria.addAndCriteria(lineCriteria1);
            lineCriteria.addOrCriteria(lineCriteria2);

            studentInLunchCriteria.addAndCriteria(lineCriteria);
        }

        SubQuery studentInSectionSubQuery = new SubQuery(m_reportHelper.getStudentSectionClass(),
                StudentSection.COL_STUDENT_OID, studentInLunchCriteria);

        X2Criteria studentCriteria = new X2Criteria();
        studentCriteria.addIn(X2BaseBean.COL_OID, studentInSectionSubQuery);

        SubQuery studentHomeroomSubQuery = new SubQuery(SisStudent.class,
                ElementaryScheduleManager.getStudentHomeroomField(m_reportHelper.getSchedule()), studentCriteria);
        studentHomeroomSubQuery.setDistinct(true);
        Collection<String> groups = getBroker().getSubQueryCollectionByQuery(studentHomeroomSubQuery);
        return groups;
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
        m_reportHelper = new ScheduleReportHelper(userData);
    }
}
