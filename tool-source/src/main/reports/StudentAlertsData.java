/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2003 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.k12.beans.StudentAlert;
import com.follett.fsc.core.k12.beans.StudentAlert.AlertType;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.reports.QueryIteratorDataSource;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.presentation.ResourceEnumFormatter;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.StringUtils;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Queue;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares the data for the "Student Alerts" report.
 *
 * @author X2 Development Corporation
 */
public class StudentAlertsData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Name for the enumerated "alert type" report parameter. The value is an Integer.
     */
    public static final String ALERT_TYPE_PARAM = "alertType";

    /**
     * Name for the "active only" report parameter. The value is an Boolean.
     */
    public static final String ACTIVE_ONLY_PARAM = "activeOnly";

    /**
     * Name for the enumerated "selection" report parameter. The value is an Integer.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the "selection value" report parameter. The value is a String.
     */
    public static final String QUERY_STRING_PARAM = "queryString";

    /**
     * Name for the parameter containing alert labels.
     */
    public static final String LABEL_MAP_PARAM = "alertLabels";

    /**
     * Name for the parameter containing alert descriptions.
     */
    public static final String DESCRIPTION_MAP_PARAM = "alertDescriptions";

    /**
     * Name for the enumerated "sort" report parameter. The value is an Integer.
     */
    public static final String SORT_PARAM = "sort";

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        /*
         * Build the criteria based on user input
         */
        Criteria criteria = new Criteria();

        // If a valid alert type was selected, show only the corresponding alerts
        int alertNumber = ((Integer) getParameter(ALERT_TYPE_PARAM)).intValue();
        if (alertNumber < AlertType.values().length) {
            criteria.addEqualTo(StudentAlert.COL_ALERT_TYPE, Integer.valueOf(alertNumber));
        }

        addUserCriteria(criteria, (String) getParameter(QUERY_BY_PARAM), (String) getParameter(QUERY_STRING_PARAM),
                null, null);

        if (isSchoolContext()) {
            criteria.addEqualTo(StudentAlert.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_SCHOOL_OID,
                    getSchool().getOid());
        } else {
            criteria.addAndCriteria(getOrganizationCriteria(StudentAlert.class));
        }

        boolean activeOnly = ((Boolean) getParameter(ACTIVE_ONLY_PARAM)).booleanValue();
        if (activeOnly) {
            criteria.addAndCriteria(StudentManager.getActiveStudentStatusCriteria(getOrganization(),
                    StudentAlert.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_ENROLLMENT_STATUS));

        }

        QueryByCriteria query = new QueryByCriteria(StudentAlert.class, criteria);

        /*
         * Build the sort based on user input (first sort by the school)
         */
        query.addOrderByAscending(StudentAlert.REL_STUDENT + PATH_DELIMITER + SisStudent.REL_SCHOOL + PATH_DELIMITER
                + SisSchool.COL_NAME);
        query.addOrderByAscending(StudentAlert.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_SCHOOL_OID);

        applyUserSort(query, (String) getParameter(SORT_PARAM));

        addParameter(LABEL_MAP_PARAM, buildAlertLabelMap());
        addParameter(DESCRIPTION_MAP_PARAM, buildAlertDescriptionMap(query));

        /*
         * Execute the query and return the results
         */
        return new QueryIteratorDataSource(getBroker().getIteratorByQuery(query));
    }

    /**
     * Returns a Map containing alert labels keyed on alert number.
     *
     * @return Map
     */
    private Map<Integer, String> buildAlertLabelMap() {
        int alertCount = AlertType.values().length;

        HashMap<Integer, String> alertLabels = new HashMap<Integer, String>(alertCount);

        for (int i = 0; i < alertCount; i++) {
            String resourceKey = ResourceEnumFormatter.getResourceKey(AlertType.values()[i]);
            String label = LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale())
                    .getMessage(getLocale(), resourceKey);

            alertLabels.put(Integer.valueOf(i), label);
        }

        return alertLabels;
    }

    /**
     * Returns a Map containing alert descriptions.
     *
     * @return Map
     */
    private Map<String, Map<Integer, Queue<String>>> buildAlertDescriptionMap(QueryByCriteria query) {
        Map<String, Map<Integer, Queue<String>>> descriptions = new HashMap<>();
        Collection<StudentAlert> studentAlerts = getBroker().getCollectionByQuery(query);
        for (StudentAlert studentAlert : studentAlerts) {
            String nameView = studentAlert.getStudent().getNameView();
            int type = studentAlert.getAlertType();
            String alertDescription = studentAlert.getAlertDescription();
            Map<Integer, Queue<String>> alerts = descriptions.get(nameView);
            if (alerts == null) {
                alerts = new HashMap<>();
            }
            Queue<String> descriptionQueue = alerts.get(type);
            if (descriptionQueue == null) {
                descriptionQueue = new ArrayDeque<>();
            }
            if (type == AlertType.ATRISK.ordinal()) {
                alertDescription = convertAtRiskDescriptionHTMLtoText(alertDescription);
            }
            if (!StringUtils.isEmpty(alertDescription)) {
                ((ArrayDeque) descriptionQueue).push(alertDescription);
            }
            alerts.put(type, descriptionQueue);
            descriptions.put(nameView, alerts);
        }
        return descriptions;
    }

    /**
     * Converts the HTML Alert At-Risk Description to a format text for Jasper report by replacing
     * special characters with their textual equivalent and removing all tags.
     * @param html String
     * @return String
     */
    public static String convertAtRiskDescriptionHTMLtoText(String html) {
        String rowsWithoutHTML = html.replaceAll("\\r\\n", "\n").replaceAll("\\r|\\n|\\t", " ")
                .replaceAll("<br>|</li>|</ol>|</ul>", "\n").replace("</p>", "\n\n")
                .replace("<li>", "\t").replace("&nbsp;", "").replace("&amp;", "&")
                .replace("&#39;", "'").replace("&quot;", "\"").replaceAll("<.*?>", "")
                .replace("&lt;", "<").replace("&gt;", ">");

        String[] allRows = rowsWithoutHTML.split("   ");

        ArrayList<String> descriptionRowsList = new ArrayList();

        String totalValue = null;

        for (int i = 0; i < allRows.length; i++){
            allRows[i] = allRows[i].replaceAll("^\\s+|\\s+$", "");

            if (i == 0){
                descriptionRowsList.add((allRows[i].replaceFirst(" ", " -- ")
                        .replaceFirst("(^.+)\\s(.+$)", "$1 -- $2") + "\n").toUpperCase());
            }
            else {
                if (allRows[i].length() > 10) {
                    descriptionRowsList.add(allRows[i].replaceFirst(" ", " -- ")
                            .replaceFirst("(^.+)\\s(.+$)", "$1 -- $2"));
                }
                else  {
                    totalValue = allRows[i];
                    descriptionRowsList.add("\nTotal = " + totalValue);
                }
            }
        }

        StringBuilder builder = new StringBuilder();
        for (int i = 0; i < descriptionRowsList.size(); i++){
            builder.append(descriptionRowsList.get(i) + "\n");
        }

        return builder.toString();
    }
}
