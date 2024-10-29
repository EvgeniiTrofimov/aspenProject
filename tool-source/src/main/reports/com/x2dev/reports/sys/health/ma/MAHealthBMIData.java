/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2009 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.reports.sys.health.ma;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.StudentContact;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.dictionary.ExtendedDictionaryAttributes;
import com.follett.fsc.core.k12.tools.reports.BeanCollectionDataSource;
import com.follett.fsc.core.k12.tools.reports.Publishable;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.HealthScreening;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.Map;
import java.util.Set;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares the data for the MA BMI report letter. This report includes the height,
 * weight, and BMI percentage for a student.
 *
 * @author X2 Development Corporation
 */
public class MAHealthBMIData extends ReportJavaSourceNet implements Publishable {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

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
     * Name for the "secondary students" report parameter. The value is a Boolean.
     */
    public static final String SECONDARY_STUDENT_PARAM = "secondaryStudent";

    /**
     * Name for the enumerated "sort" report parameter. The value is an Integer.
     */
    public static final String SORT_PARAM = "sort";

    /*
     * Other constants
     */
    private static final String ALIAS_BMI = "hsc-general-bmi";
    private static final String HSC_GENERAL_OID = "ddxHscGeneral";

    private SisStudent m_currentStudent;
    private Map<String, Collection<Person>> m_recipientCache;

    /**
     * Gets the data break column.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.reports.Publishable#getDataBreakColumn()
     */
    @Override
    public String getDataBreakColumn() {
        return HealthScreening.COL_STUDENT_OID;
    }

    /**
     * Gets the description.
     *
     * @param bean X2BaseBean
     * @return String
     * @see com.follett.fsc.core.k12.tools.reports.Publishable#getDescription(X2BaseBean)
     */
    @Override
    public String getDescription(X2BaseBean bean) {
        return "BMI Report for " + ((SisStudent) bean).getNameView();
    }

    /**
     * Gets the email address.
     *
     * @param person Person
     * @return String
     * @see
     *      com.follett.fsc.core.k12.tools.reports.Publishable#getEmailAddress(com.x2dev.sis.model.beans.
     *      Person)
     */
    @Override
    public String getEmailAddress(Person person) {
        return person.getEmail01();
    }

    /**
     * Gets the email recipients.
     *
     * @param bean X2BaseBean
     * @return Collection
     * @see
     *      com.follett.fsc.core.k12.tools.reports.Publishable#getEmailRecipients(com.x2dev.sis.model.
     *      beans.X2BaseBean)
     */
    @Override
    public Collection<Person> getEmailRecipients(X2BaseBean bean) {
        return m_recipientCache.get(bean.getOid());
    }

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws Exception exception
     * @see com.x2dev.sis.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        Collection dataSet = new LinkedList();

        ExtendedDictionaryAttributes extendedDictionaryAttributes =
                (ExtendedDictionaryAttributes) getBroker().getBeanByOid(ExtendedDataDictionary.class, HSC_GENERAL_OID);
        DataDictionary extendedDictionary =
                DataDictionary.getDistrictDictionary(extendedDictionaryAttributes, getBroker().getPersistenceKey());

        /*
         * Build the criteria based on user input
         */
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(HealthScreening.COL_EXTENDED_DATA_DICTIONARY_OID, HSC_GENERAL_OID);

        /*
         * If bmi alias exists, only get records with BMI data
         */
        DataDictionaryField hscBmiField = extendedDictionary.findDataDictionaryFieldByAlias(ALIAS_BMI);
        if (hscBmiField != null) {
            criteria.addNotEmpty(hscBmiField.getJavaName(), getBroker().getPersistenceKey());
        } else {
            AppGlobals.getLog().warning("Cannot check existence of BMI's index . Report may contain incorrect data.");
        }

        QueryByCriteria query = null;
        QueryIterator iterator = null;

        if (m_currentStudent != null) {
            criteria.addEqualTo(HealthScreening.COL_STUDENT_OID, m_currentStudent.getOid());
            query = new QueryByCriteria(HealthScreening.class, criteria);
            query.addOrderByDescending(HealthScreening.COL_DATE);
            query.addOrderByDescending(X2BaseBean.COL_LAST_MODIFIED_TIME);

            iterator = getBroker().getIteratorByQuery(query);

            try {
                if (iterator.hasNext()) {
                    HealthScreening latestScreening = (HealthScreening) iterator.next();
                    dataSet.add(latestScreening);
                }
            } finally {
                iterator.close();
            }

            setRecipientCache(null);
        } else {
            applyUserCriteria(criteria);
            query = createQueryByCriteria(HealthScreening.class, criteria);
            applySort(query);

            LinkedHashMap map = new LinkedHashMap();
            iterator = getBroker().getIteratorByQuery(query);

            /*
             * Iterates through the health screenings and puts the most recent screening
             * into a map which is then returned
             */
            try {
                while (iterator.hasNext()) {
                    HealthScreening screening = (HealthScreening) iterator.next();
                    String studentOid = screening.getStudentOid();

                    if (!map.containsKey(studentOid)) {
                        map.put(studentOid, screening);
                    }
                }
                dataSet = map.values();

            } finally {
                iterator.close();
            }

            setRecipientCache(map.keySet());
        }

        return new BeanCollectionDataSource(dataSet, false, extendedDictionary, getLocale());
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @see
     *      com.x2dev.sis.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) {
        /*
         * If we're in the context of a single student, print the report that student
         */
        m_currentStudent = userData.getCurrentRecord(SisStudent.class);
    }

    /**
     * Builds the sort based on the user input (first sorts by the school).
     *
     * @param query QueryByCriteria
     */
    private void applySort(QueryByCriteria query) {
        query.addOrderByAscending(HealthScreening.REL_STUDENT + PATH_DELIMITER + SisStudent.REL_SCHOOL +
                PATH_DELIMITER + SisSchool.COL_NAME);
        query.addOrderByAscending(HealthScreening.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_SCHOOL_OID);

        if (getParameter(SORT_PARAM) != null) {
            int sort = ((Integer) getParameter(SORT_PARAM)).intValue();
            switch (sort) {
                case 0: // Current sort
                    applyCurrentSort(query);
                    break;

                case 1: // Name view
                    query.addOrderByAscending(HealthScreening.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_NAME_VIEW);
                    break;

                case 2: // YOG
                    query.addOrderByAscending(HealthScreening.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_YOG);
                    query.addOrderByAscending(HealthScreening.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_NAME_VIEW);
                    break;

                case 3: // Homeroom
                    query.addOrderByAscending(HealthScreening.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_HOMEROOM);
                    query.addOrderByAscending(HealthScreening.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_NAME_VIEW);
                    break;

                default:
                    break;
            }
        }

        query.addOrderByDescending(HealthScreening.COL_DATE);
        query.addOrderByDescending(X2BaseBean.COL_LAST_MODIFIED_TIME);
    }

    /**
     * Adds criteria based on user input.
     *
     * @param criteria Criteria
     */
    private void applyUserCriteria(Criteria criteria) {
        int queryBy = ((Integer) getParameter(QUERY_BY_PARAM)).intValue();
        switch (queryBy) {
            case 0: // Current selection
                Criteria studentCriteria = getCurrentCriteria();
                SubQuery studentSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria);
                criteria.addIn(HealthScreening.COL_STUDENT_OID, studentSubQuery);
                break;

            case 2: // YOG
                criteria.addEqualTo(HealthScreening.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_YOG,
                        getParameter(QUERY_STRING_PARAM));
                break;

            case 3: // Homeroom
                criteria.addEqualTo(HealthScreening.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_HOMEROOM,
                        getParameter(QUERY_STRING_PARAM));
                break;

            default:
                // No additional criteria (this is the case for "All")
                break;
        }

        if (isSchoolContext() && queryBy != 0) {
            criteria.addEqualTo(HealthScreening.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_SCHOOL_OID,
                    getSchool().getOid());

            /*
             * Include secondary students of the school if needed.
             */
            if (((Boolean) getParameter(SECONDARY_STUDENT_PARAM)).booleanValue()) {
                criteria.addOrCriteria(StudentManager.buildSecondaryStudentCriteria(getSchool()));
            }
        }

        boolean activeOnly = ((Boolean) getParameter(ACTIVE_ONLY_PARAM)).booleanValue();
        if (activeOnly) {
            criteria.addAndCriteria(StudentManager.getActiveStudentStatusCriteria(getOrganization(),
                    HealthScreening.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_ENROLLMENT_STATUS));
        }
    }

    /**
     * Populates the recipient cache for published reports.
     *
     * @param studentOids void
     */
    private void setRecipientCache(Set<String> studentOids) {
        HashMap<String, Collection<Person>> map = new HashMap<String, Collection<Person>>();

        Criteria criteria = new Criteria();

        if (m_currentStudent != null) {
            criteria.addEqualTo(StudentContact.COL_STUDENT_OID, m_currentStudent.getOid());
        } else if (studentOids != null && !studentOids.isEmpty()) {
            criteria.addIn(StudentContact.COL_STUDENT_OID, studentOids);
        } else {
            // If we have no students, we should not sent letter at all.
            addNoMatchCriteria(criteria);
        }

        criteria.addEqualTo(StudentContact.COL_PORTAL_ACCESS_INDICATOR, Boolean.TRUE);

        QueryByCriteria query = new QueryByCriteria(StudentContact.class, criteria);
        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                StudentContact contact = (StudentContact) iterator.next();
                String key = contact.getStudentOid();
                Person value = contact.getPerson();

                if (!map.containsKey(key)) {
                    map.put(key, new ArrayList<Person>());
                }

                if (!map.get(key).contains(value)) {
                    map.get(key).add(value);
                }
            }
        } finally {
            iterator.close();
        }

        m_recipientCache = map;
    }
}
