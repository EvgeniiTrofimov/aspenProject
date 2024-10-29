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
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.x2dev.sis.model.beans.RubricCriterion;
import com.x2dev.sis.model.beans.RubricDefinition;
import com.x2dev.utils.CollectionUtils;
import com.x2dev.utils.StringUtils;
import java.util.Collection;
import java.util.Map;
import java.util.Set;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2012 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

/**
 * Blank Procedure that does nothing. Can be used as a stub for procedures that work solely by
 * using the custom HREF data.
 *
 * @author X2 Development Corporation
 */
public class RefreshRubricLevelCounts extends ProcedureJavaSource {
    /**
     * True if testing only. False will actually update the beans.
     */
    public static final String TEST_ONLY_PARAM = "testOnly";

    private static final long serialVersionUID = 1L;

    // Set to true to see some additional debugging
    private static boolean DEBUG = false;

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        boolean previewMode = ((Boolean) getParameter(TEST_ONLY_PARAM)).booleanValue();

        // Get all RubricDefinition's on the current list
        QueryByCriteria query = createQueryByCriteria(RubricDefinition.class, getCurrentCriteria());
        Collection<RubricDefinition> defs = getBroker().getCollectionByQuery(query);

        if (!defs.isEmpty()) {
            // Get all RubricCriterion for all RubricDefinitions
            String[] cols = new String[] {RubricCriterion.COL_RUBRIC_DEFINITION_OID,
                    RubricCriterion.COL_RUBRIC_CRITERION_OID};
            int[] colsSize = new int[] {32, 4};

            Criteria criterionCriteria = new Criteria();
            criterionCriteria.addIn(RubricCriterion.COL_RUBRIC_DEFINITION_OID,
                    CollectionUtils.getPropertyCollection(defs, X2BaseBean.COL_OID));

            QueryByCriteria criterionQuery = new QueryByCriteria(RubricCriterion.class, criterionCriteria);
            Map<String, Map<String, Collection<RubricCriterion>>> map =
                    getBroker().getGroupedCollectionByQuery(criterionQuery, cols, colsSize);

            // For each RunricDefinition, calculate the level count for all RubricCriterion children
            Set<String> defOids = map.keySet();
            for (String defOid : defOids) {
                if (DEBUG || previewMode) {
                    logMessage("Rubric Definition - RBD_OID: " + defOid);
                    logMessage("---------------------------------------------");
                    logMessage("");
                }

                Map<String, Collection<RubricCriterion>> innerMap = map.get(defOid);
                if (innerMap != null) {
                    // For each root level RubricCriterion, calculate the level count
                    Collection<RubricCriterion> criteria = innerMap.get(null);
                    if (criteria != null) {
                        for (RubricCriterion criterion : criteria) {
                            updateCriterion(criterion, innerMap);
                        }
                    }
                }

                if (DEBUG || previewMode) {
                    logMessage("");
                }
            }

            if (!previewMode) {
                if (DEBUG) {
                    logMessage("");
                }

                logMessage(defOids.size() + " Rubric Definition(s) had their level count refreshed.");
            } else {
                logMessage("");
                logMessage(defOids.size() + " Rubric Definition(s) would have their level count refreshed.");
            }
        }
    }

    /**
     * Updates the level count for the passed criterion and its children. The criterion's calculated
     * level count is saved on the bean and returned.
     *
     * @param criterion RubricCriterion
     * @param map Map<String,Collection<RubricCriterion>>
     * @return int
     */
    private int updateCriterion(RubricCriterion criterion, Map<String, Collection<RubricCriterion>> map) {
        int level = 0;

        Collection<RubricCriterion> children = map.get(criterion.getOid());

        if (children != null) {
            for (RubricCriterion rubricChild : children) {
                int childLevel = updateCriterion(rubricChild, map);

                if (childLevel + 1 > level) {
                    level = childLevel + 1;
                }
            }
        }

        criterion.setLevelCount(level);

        boolean previewMode = ((Boolean) getParameter(TEST_ONLY_PARAM)).booleanValue();

        if (!previewMode) {
            getBroker().saveBeanForced(criterion, true, false);
        }

        if (DEBUG || previewMode) {
            String msg = StringUtils.padLeft("", level, '-') + level;
            logMessage(msg);
        }

        return level;
    }
}
