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
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.tools.reports.QueryIteratorDataSource;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.x2dev.sis.model.beans.SisUser;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares the data for the "User Orientation Labels" reports.
 *
 * @author X2 Development Corporation
 */
public class UserOrientationLabelsData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Name for the enumerated "selection" report parameter. The value is a String.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the "sort" report parameter. The value is a String.
     */
    public static final String SORT_PARAM = "sort";

    /**
     * Name for the "URL" report parameter. The value is a String.
     */
    public static final String URL_PARAM = "url";

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
        X2Criteria criteria = getOrganizationCriteria(SisUser.class);

        String queryBy = (String) getParameter(QUERY_BY_PARAM);
        if ("generated".equals(queryBy)) {
            criteria.addNotEmpty(SisUser.COL_GENERATED_PASSWORD, getBroker().getPersistenceKey());
        } else {
            addUserCriteria(criteria, queryBy, null, null, null);
        }

        QueryByCriteria query = createQueryByCriteria(SisUser.class, criteria);
        applyUserSort(query, (String) getParameter(SORT_PARAM));

        return new QueryIteratorDataSource(getBroker().getIteratorByQuery(query));
    }
}
