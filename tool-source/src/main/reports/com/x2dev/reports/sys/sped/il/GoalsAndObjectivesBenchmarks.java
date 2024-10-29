/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2002-2017 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.reports.sys.sped.il;

import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepGoal;
import com.x2dev.sis.model.beans.IepGoalObjective;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;

/**
 * Java source for a form-based report. All fields and aliases present on the form storage and owner
 * objects are available for use in the format. The storage and owner objects are retrieved and made
 * available by the superclass - <code>BaseFormReportJavaSource</code>.
 * <p>
 * In the report format, fields and aliases from the storage object can be accessed directly. To
 * retrieve values from the owner object, the prefix <b>"owner"</b> must be present on the field
 * or alias. See <code>SimpleFormDataSource</code> for more information.
 *
 * @author X2 Development Corporation
 */
public class GoalsAndObjectivesBenchmarks extends BeanReport {
    private static final String ALIAS_IGL_GOAL_ACADEMIC = "igl-academic";
    private static final String ALIAS_IGL_GOAL_FUNCTIONAL = "igl-functional";
    private static final String ALIAS_IGL_GOAL_TRANSITION = "igl-transition";
    private static final String ALIAS_IGL_GOAL_STANDARD = "igl-goal-standard";
    private static final String ALIAS_IGL_SHORT_DESCR = "igl-goal-short-descr";
    private static final String ALIAS_IGO_CRIT_SUCCESSES = "igo-crit_successes";
    private static final String ALIAS_IGO_CRIT_OTHER = "igo-crit-other";
    private static final String ALIAS_IGO_CRIT_ATTEMPTS = "igo-crit-attempts";
    private static final String ALIAS_IGO_CRIT_ACCURACY = "igo-crit-accuracy";
    private static final String ALIAS_IGO_OBJECTIVE = "igo-objective";
    private static final String ALIAS_IGO_OID = "igo-oid";
    private static final String ALIAS_IGO_PROGRESS = "igo-progress";
    private static final String ALIAS_IGO_PROCEDURE_OTHER = "igo-procedure-other";
    private static final String ALIAS_IGO_PROCEDURE = "igo-procedure";
    private static final String ALIAS_IGO_SCHEDULE_OTHER = "igo-schedule-other";
    private static final String ALIAS_IGO_SCHEDULE = "igo-schedule";
    private static final String ALIAS_OTHER_IMPLEMENTERS = "igl-other-implementers";
    private static final ArrayList<String> ALIASES_IMPLEMENTERS = new ArrayList<String>(
            Arrays.asList(
                    "igl-coordinator", "igl-general-teacher",
                    "igl-hearing-impirment", "igl-occupational",
                    "igl-orientation", "igl-std", "igl-physical-therapist",
                    "igl-social-worker", "igl-sped-teacher",
                    "igl-speech pathologist", "igl-visitor-teacher"));

    private static final String EMPTY = "";

    private static final String FIELD_IEP_GOAL = "iepGoal";
    private static final String FIELD_IMPLEMENTERS = "implementers";
    private static final String FIELD_GOAL_DESCRIPTION = "goalDescription";
    private static final String FIELD_GOAL_STANDARD = "goalStandard";

    private static final String PARAM_OID = "oid";
    private static final String PARAM_OWNER = "owner";

    private static final String STRING_1 = "1";
    private static final String STRING_2 = "2";
    private static final String STRING_3 = "3";

    private static final long serialVersionUID = 1L;

    private Map<String, String> m_defaultImplementers = new HashMap<String, String>();
    private Comparator<IepGoal> m_goalComparator = null;
    private ReportDataGrid m_grid;
    private Map<String, String> m_implementersMap;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.x2dev.reports.sys.sped.il.BeanReport#fillGrid(Map, Locale)
     */
    @Override
    protected JRDataSource gatherData() {
        IepData storageTable = (IepData) getFormStorage();
        addParameter(PARAM_OID, storageTable == null ? null : storageTable.getOid());
        m_grid = new ReportDataGrid();
        m_implementersMap = new HashMap<String, String>();
        if (storageTable != null && storageTable.getOid() != null) {
            addParameter(PARAM_OWNER, storageTable);

            addParameter(FIELD_IMPLEMENTERS, m_implementersMap);
            List<IepGoal> listCollection = new ArrayList<IepGoal>(storageTable.getIepGoals());
            sortIepGoal(listCollection);

            for (IepGoal goal : listCollection) {
                Collection<IepGoalObjective> goalObjectives = goal.getIepGoalObjectives();
                if (goalObjectives.isEmpty()) {
                    m_grid.append();
                    fillGoal(goal);
                }
                for (IepGoalObjective goalObjective : goalObjectives) {
                    m_grid.append();
                    fillGoal(goal);
                    m_grid.set(ALIAS_IGO_OID, goalObjective.getOid());
                    m_grid.set(ALIAS_IGO_OBJECTIVE, goalObjective.getObjective());
                    String fromattedCritAccuracy = formatNumber(
                            (String) goalObjective.getFieldValueByAlias(ALIAS_IGO_CRIT_ACCURACY,
                                    getDictionary()));
                    fromattedCritAccuracy = fromattedCritAccuracy == null ? "" : fromattedCritAccuracy;
                    if (fromattedCritAccuracy.equals("0")) {
                        fromattedCritAccuracy = "";
                    }

                    m_grid.set(ALIAS_IGO_CRIT_ACCURACY, fromattedCritAccuracy);
                    m_grid.set(ALIAS_IGO_CRIT_SUCCESSES, formatNumber(
                            (String) goalObjective.getFieldValueByAlias(ALIAS_IGO_CRIT_SUCCESSES,
                                    getDictionary())));
                    m_grid.set(ALIAS_IGO_CRIT_ATTEMPTS, formatNumber(
                            (String) goalObjective.getFieldValueByAlias(ALIAS_IGO_CRIT_ATTEMPTS,
                                    getDictionary())));
                    m_grid.set(ALIAS_IGO_CRIT_OTHER, goalObjective.getFieldValueByAlias(ALIAS_IGO_CRIT_OTHER,
                            getDictionary()));
                    m_grid.set(ALIAS_IGO_PROCEDURE, goalObjective.getFieldValueByAlias(ALIAS_IGO_PROCEDURE,
                            getDictionary()));
                    m_grid.set(ALIAS_IGO_PROCEDURE_OTHER, goalObjective.getFieldValueByAlias(ALIAS_IGO_PROCEDURE_OTHER,
                            getDictionary()));
                    m_grid.set(ALIAS_IGO_SCHEDULE, goalObjective.getFieldValueByAlias(ALIAS_IGO_SCHEDULE,
                            getDictionary()));
                    m_grid.set(ALIAS_IGO_SCHEDULE_OTHER, goalObjective.getFieldValueByAlias(ALIAS_IGO_SCHEDULE_OTHER,
                            getDictionary()));
                    m_grid.set(ALIAS_IGO_PROGRESS, goalObjective.getFieldValueByAlias(ALIAS_IGO_PROGRESS,
                            getDictionary()));

                }
            }
        }
        if (m_grid.isEmpty()) {
            m_grid.append();
            m_grid.set(ALIAS_IGO_OID, STRING_1);
            m_grid.append();
            m_grid.set(ALIAS_IGO_OID, STRING_2);
            m_grid.append();
            m_grid.set(ALIAS_IGO_OID, STRING_3);
        }
        m_grid.beforeTop();
        return m_grid;
    }


    /**
     * fill in current record on m_grid using data from IepGoal<br>
     * fill m_implementersMap, key goal ID, value concatenation string from<br>
     * selected implementers checkboxes .
     *
     * @param goal IepGoal
     */
    private void fillGoal(IepGoal goal) {
        m_grid.set(FIELD_IEP_GOAL, goal);
        m_grid.set(FIELD_GOAL_STANDARD, goal.getFieldValueByAlias(ALIAS_IGL_GOAL_STANDARD, getDictionary()));
        m_grid.set(FIELD_GOAL_DESCRIPTION, goal.getFieldValueByAlias(ALIAS_IGL_SHORT_DESCR, getDictionary()));
        m_grid.set(ALIAS_IGL_GOAL_ACADEMIC, goal.getFieldValueByAlias(ALIAS_IGL_GOAL_ACADEMIC, getDictionary()));
        m_grid.set(ALIAS_IGL_GOAL_FUNCTIONAL, goal.getFieldValueByAlias(ALIAS_IGL_GOAL_FUNCTIONAL, getDictionary()));
        m_grid.set(ALIAS_IGL_GOAL_TRANSITION, goal.getFieldValueByAlias(ALIAS_IGL_GOAL_TRANSITION, getDictionary()));
        m_implementersMap.put(goal.getOid(), getImplementers(goal));

    }

    /**
     * Format number.
     *
     * @param number String
     * @return String
     */
    private String formatNumber(String number) {
        String returnValue = null;
        if (number != null) {
            int length = number.length();
            int startPosition = 0;
            for (int i = 0; i < length - 1; i++) {
                char someNumber = number.charAt(i);
                if (someNumber == '0') {
                    startPosition++;
                } else {
                    break;
                }
            }

            if (startPosition < length) {
                returnValue = number.substring(startPosition, length);
            } else {
                returnValue = "0";
            }

        }
        return returnValue;
    }

    /**
     * Gets the goal comparator.
     *
     * @return Comparator
     */
    private Comparator<IepGoal> getGoalComparator() {
        if (m_goalComparator == null) {
            m_goalComparator = new Comparator<IepGoal>() {

                @Override
                public int compare(IepGoal arg0, IepGoal arg1) {
                    int returnValue = 0;
                    String number0 = arg0 == null ? EMPTY : arg0.getId() == null ? EMPTY : arg0.getId();
                    String number1 = arg1 == null ? EMPTY : arg1.getId() == null ? EMPTY : arg1.getId();
                    number0 = number0.trim();
                    number1 = number1.trim();
                    boolean numeric0 = StringUtils.isNumeric(number0);
                    boolean numeric1 = StringUtils.isNumeric(number1);
                    if (numeric0 && numeric1) {
                        double int0 = Double.parseDouble(number0);
                        double int1 = Double.parseDouble(number1);
                        returnValue = int0 == int1 ? 0 : int0 < int1 ? -1 : 1;
                    } else if (!numeric0 && !numeric1) {
                        returnValue = number0.compareTo(number1);
                    } else if (numeric0 && !numeric1) {
                        returnValue = -1;
                    } else {
                        returnValue = 1;
                    }
                    return returnValue;
                }
            };
        }
        return m_goalComparator;
    }


    /**
     * Gets the implementers.
     *
     * @param goal IepGoal
     * @return Concatenation string from<br>
     *         selected implementers checkboxes
     */
    private String getImplementers(IepGoal goal) {
        StringBuilder returnValue = new StringBuilder();
        for (String alias : ALIASES_IMPLEMENTERS) {
            String value = (String) goal.getFieldValueByAlias(alias, getDictionary());
            if (value != null && value.equals(BooleanAsStringConverter.TRUE)) {
                String implementerName = (String) getParameter(alias);
                if (StringUtils.isEmpty(implementerName)) {
                    initializeDefaultImplementersMap();
                    implementerName = m_defaultImplementers.get(alias);
                }
                returnValue.append(implementerName);
                returnValue.append(", ");
            }
        }

        String value = (String) goal.getFieldValueByAlias(ALIAS_OTHER_IMPLEMENTERS, getDictionary());
        if (value != null && !value.isEmpty()) {
            returnValue.append(value);
            returnValue.append(", ");
        }

        if (returnValue.length() > 1) {
            returnValue.delete(returnValue.length() - 2, returnValue.length());
            returnValue.append(".");
        }


        return returnValue.toString();
    }


    /**
     * Initialize default implementers map.
     */
    private void initializeDefaultImplementersMap() {
        if (m_defaultImplementers.isEmpty()) {
            m_defaultImplementers.put("igl-coordinator", "Case Coordinator");
            m_defaultImplementers.put("igl-general-teacher", "General Ed. Teacher");
            m_defaultImplementers.put("igl-hearing-impirment", "Hearing Impairment Itinerant");
            m_defaultImplementers.put("igl-occupational", "Occupational Therapist");
            m_defaultImplementers.put("igl-orientation", "Orientation and Mobility");
            m_defaultImplementers.put("igl-std", "Student");
            m_defaultImplementers.put("igl-physical-therapist", "Physical Therapist");
            m_defaultImplementers.put("igl-social-worker", "Social Worker");
            m_defaultImplementers.put("igl-sped-teacher", "Special Ed. Teacher");
            m_defaultImplementers.put("igl-speech pathologist", "Speech/Language Pathologist");
            m_defaultImplementers.put("igl-visitor-teacher", "Vision Teacher");
        }
    }


    /**
     * Sort iep goal.
     *
     * @param listCollection List<IepGoal>
     */
    private void sortIepGoal(List<IepGoal> listCollection) {
        Collections.sort(listCollection, getGoalComparator());
    }


}
