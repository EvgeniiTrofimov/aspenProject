/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2017 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.tn;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.ExportFormatResult;
import com.follett.fsc.core.k12.beans.ExportFormatRow;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.x2dev.utils.X2BaseException;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class TNDeleteResultsProcedure.
 */
public class TNDeleteResultsProcedure extends ProcedureJavaSource {
    private static final String NO_PERFMON4J_INSTRUMENTATION = ""; 
    /**
     * Class members
     */
    private int m_numOfMonth;
    private Map<String, ExportFormatResult> m_resultByOid;
    private Map<String, List<ExportFormatResult>> m_resultsByCtx = new LinkedHashMap();

    /**
     * Input params
     */
    private static final String INPUT_EFD_OIDS = "efdOids";
    private static final String INPUT_MONTH = "numOfMonth";

    /**
     * Other constants
     */
    private static final SimpleDateFormat DATE_FORMAT = new SimpleDateFormat("yyyy-MM-dd");
    private static final String REGEX_EFR_NAME = "TN %%% %%% Uploads for %%%%";

    /**
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        for (String ctxOid : m_resultsByCtx.keySet()) {
            removeData(m_resultsByCtx.get(ctxOid));
        }
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();

        m_numOfMonth = ((Integer) getParameter(INPUT_MONTH)).intValue();

        populateResults();
        populateResultsByContext();

    }

    /**
     * Populate list of ExportFormatResults keyed on ExportFormatDefinition.
     */
    private void populateResults() {
        String efdOidString = (String) getParameter(INPUT_EFD_OIDS);
        if (!StringUtils.isEmpty(efdOidString)) {
            List<String> efdOids = Arrays.asList(((String) getParameter(INPUT_EFD_OIDS)).split(","));

            X2Criteria efrCriteria = new X2Criteria();

            efrCriteria.addIn(ExportFormatResult.COL_DEFINITION_OID, efdOids);
            efrCriteria.addNotLike(ExportFormatResult.COL_NAME, REGEX_EFR_NAME);

            QueryByCriteria efrQuery = new QueryByCriteria(ExportFormatResult.class, efrCriteria);
            efrQuery.addOrderByDescending(ExportFormatResult.COL_RUN_DATE);

            m_resultByOid = getBroker().getMapByQuery(efrQuery, X2BaseBean.COL_OID, 1024);
        } else {
            logMessage("At least one export should be selected");
        }
    }

    /**
     * Populate map of ExportFormatRows keyed on ctxOid.
     */
    private void populateResultsByContext() {
        Pattern pattern = Pattern.compile(".*(20\\d{2})$");
        Map<String, Set<String>> savedEfdCtx = new HashMap();
        Collection<DistrictSchoolYearContext> contexts =
                getBroker().getCollectionByQuery(new QueryByCriteria(DistrictSchoolYearContext.class));
        for (ExportFormatResult efr : m_resultByOid.values()) {
            DistrictSchoolYearContext ctxToPut = null;

            Matcher m = pattern.matcher(efr.getName());
            if (m.find()) {
                String yearString = m.group(1);
                for (DistrictSchoolYearContext ctx : contexts) {
                    if (yearString.equals(Integer.toString(ctx.getSchoolYear()))) {
                        ctxToPut = ctx;
                        break;
                    }
                }

            }
            if (ctxToPut == null) {
                long efrRunDate = efr.getRunDate();

                for (DistrictSchoolYearContext ctx : contexts) {
                    if ((efrRunDate >= ctx.getStartDate().getTime()) && (efrRunDate <= ctx.getEndDate().getTime())) {
                        ctxToPut = ctx;
                        break;
                    }
                }
            }

            if (ctxToPut != null) {
                long efrRunDate = efr.getRunDate();
                if (efr.getName().contains("Export") && efrRunDate <= ctxToPut.getEndDate().getTime()) {
                    Set<String> savedEfds = savedEfdCtx.get(ctxToPut.getOid());
                    if (savedEfds == null) {
                        savedEfds = new HashSet();
                        savedEfdCtx.put(ctxToPut.getOid(), savedEfds);
                    }
                    if (!savedEfds.contains(efr.getDefinitionOid())) {
                        savedEfds.add(efr.getDefinitionOid());
                        continue;
                    }
                }
                List<ExportFormatResult> efrs = m_resultsByCtx.get(ctxToPut.getOid());

                if (efrs == null) {
                    efrs = new LinkedList();
                    m_resultsByCtx.put(ctxToPut.getOid(), efrs);
                }
                efrs.add(efr);
            }
        }
    }

    /**
     * Remove old data from DB.
     *
     * @param list List<ExportFormatResult>
     */
    private void removeData(List<ExportFormatResult> list) {
        Calendar cal = Calendar.getInstance();
        cal.add(Calendar.MONTH, (-m_numOfMonth));
        long dateBefore = cal.getTimeInMillis();

        for (ExportFormatResult efr : list) {
            if (dateBefore > efr.getRunDate()) {
                X2Criteria deleteCriteria = new X2Criteria();
                deleteCriteria.addEqualTo(ExportFormatRow.COL_RESULT_OID, efr.getOid());
                QueryByCriteria deleteQuery = new QueryByCriteria(ExportFormatRow.class, deleteCriteria);
                getBroker().deleteByQuery(deleteQuery);
                getBroker().deleteBean(efr);

                logMessage(efr.getName() + " run on " +
                        DATE_FORMAT.format(new Date(efr.getRunDate())) + " was deleted");
            }
        }
    }
}
