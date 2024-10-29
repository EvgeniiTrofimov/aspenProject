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
import com.follett.fsc.core.k12.beans.DataField;
import com.follett.fsc.core.k12.beans.DataFieldConfig;
import com.follett.fsc.core.k12.beans.DataTableConfig;
import com.follett.fsc.core.k12.tools.reports.QueryIteratorDataSource;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares the data for the Data Dictionary report.
 *
 * @author X2 Development Corporation
 */
public class DataDictionaryData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Name for the "enabled fields only" report parameter. The value is an Boolean.
     */
    public static final String ENABLED_ONLY_PARAM = "enabledOnly";

    /**
     * Name for the enumerated "selection" report parameter. The value is a String.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the "sort" report parameter. The value is a String.
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
        QueryByCriteria query = new QueryByCriteria(DataFieldConfig.class, buildCriteria());

        applyUserSort(query, (String) getParameter(SORT_PARAM));

        return new QueryIteratorDataSource(getBroker().getIteratorByQuery(query));
    }

    /**
     * Build the criteria based on user input.
     *
     * @return Criteria
     */
    private Criteria buildCriteria() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(DataFieldConfig.COL_ORGANIZATION1_OID, getOrganization().getRootOrganization().getOid());

        addUserCriteria(criteria,
                (String) getParameter(QUERY_BY_PARAM),
                null,
                DataTableConfig.class,
                DataTableConfig.COL_DATA_TABLE_OID,
                DataFieldConfig.REL_DATA_FIELD + PATH_DELIMITER + DataField.COL_DATA_TABLE_OID);

        /*
         * Current select versus all fields
         */
        if (((Boolean) getParameter(ENABLED_ONLY_PARAM)).booleanValue()) {
            criteria.addEqualTo(DataFieldConfig.COL_ENABLED_INDICATOR, Boolean.TRUE);
        }

        return criteria;
    }
}
