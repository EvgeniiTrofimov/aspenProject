/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2009 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.reports.QueryIteratorDataSource;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.tools.reports.StudentReportJavaSource;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares the data for the following student reports:
 * <ul>
 * <li>Student Directory
 * <li>Student List
 * </ul>
 * These reports simply select students from the current school (with an optional criteria for YOG
 * or homeroom) and order the results by YOG, homeroom, or last name.
 *
 * @author X2 Development Corporation
 */
public class StudentListData extends StudentReportJavaSource {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        Criteria criteria = buildCriteria();
        QueryByCriteria query = createQueryByCriteria(SisStudent.class, criteria);
        query.setDistinct(true);

        /*
         * Build the sort based on user input
         *
         * If we are not in the context of a school, sort by the school first to support school
         * grouping on the format.
         */
        if (!isSchoolContext()) {
            query.addOrderByAscending(SisStudent.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_NAME);
            query.addOrderByAscending(SisStudent.COL_SCHOOL_OID);
        }

        String sort = (String) getParameter(SORT_PARAM);
        applyUserSort(query, sort);

        /*
         * Execute the query and return the results
         */
        return new QueryIteratorDataSource(getBroker().getIteratorByQuery(query),
                DataDictionary.getDistrictDictionary(getUser().getPersistenceKey()), true, getLocale());
    }
}
