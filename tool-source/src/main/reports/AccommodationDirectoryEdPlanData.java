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

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.reports.QueryIteratorDataSource;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.IepAccommodation;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.StringUtils;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares the data for the "Accommodation Directory (Ed Plan)" report.
 *
 * @author X2 Development Corporation
 */
public class AccommodationDirectoryEdPlanData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Name for the "accommodation name" report parameter. This value is a String.
     */
    public static final String ACCOMMODATION_NAME_PARAM = "accommodationName";

    /**
     * Name for the "case manager OID" report parameter. This value is a string.
     */
    public static final String CASE_MANAGER_OID_PARAM = "caseManagerOid";

    /**
     * Name for the "extended dictionary OID" report parameter. This value is a String.
     */
    public static final String EXTENDED_DICTIONARY_OID_PARAM = "extendedDictionaryOid";

    private SisStudent m_currentStudent;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        Criteria criteria = new Criteria();

        if (m_currentStudent != null) {
            criteria.addEqualTo(IepAccommodation.COL_STUDENT_OID, m_currentStudent.getOid());
        }

        String accommodationName = (String) getParameter(ACCOMMODATION_NAME_PARAM);
        if (!StringUtils.isEmpty(accommodationName)) {
            criteria.addEqualTo(IepAccommodation.COL_NAME, accommodationName);
        }

        String caseManagerOid = (String) getParameter(CASE_MANAGER_OID_PARAM);
        if (!StringUtils.isEmpty(caseManagerOid)) {
            criteria.addEqualTo(IepAccommodation.REL_IEP_DATA + PATH_DELIMITER + IepData.COL_STAFF_OID, caseManagerOid);
        }

        String extendedDictionaryOid = (String) getParameter(EXTENDED_DICTIONARY_OID_PARAM);
        if (!StringUtils.isEmpty(extendedDictionaryOid)) {
            criteria.addEqualTo(IepAccommodation.COL_EXTENDED_DATA_DICTIONARY_OID, extendedDictionaryOid);
        }

        criteria.addAndCriteria(getOrganizationCriteria(IepAccommodation.class));

        QueryByCriteria query = new QueryByCriteria(IepAccommodation.class, criteria);
        query.addOrderByAscending(IepAccommodation.COL_NAME);
        query.addOrderByAscending(IepAccommodation.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_NAME_VIEW);
        query.addOrderByAscending(IepAccommodation.COL_STUDENT_OID);

        return new QueryIteratorDataSource(getBroker().getIteratorByQuery(query),
                DataDictionary.getDistrictDictionary(getExtendedDictionary(),
                        getBroker().getPersistenceKey()),
                true, getLocale());
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.
     *      UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) {
        /*
         * If we're in the context of a single student, run for just that student.
         */
        m_currentStudent = userData.getCurrentRecord(SisStudent.class);
    }
}
