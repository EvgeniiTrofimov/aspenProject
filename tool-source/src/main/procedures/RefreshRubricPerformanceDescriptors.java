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
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.web.GenericDetail;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.RubricDefinition;
import com.x2dev.sis.web.assessment.RatingRefreshHelper;
import com.x2dev.sis.web.assessment.RubricCriterionDetail;
import com.x2dev.sis.web.assessment.RubricDetail;
import com.x2dev.utils.X2BaseException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * This procedure refreshes performance descriptors associated to rubric definitions.
 *
 * <p>
 * Usage:
 * <ul>
 * <li>Navigate to a rubric list in district view.</li>
 * <li>Select one or more rubrics whose performance descriptors are to be refreshed.</li>
 * <li>Run the procedure against the current selection.</li>
 * </ul>
 * </p>
 *
 * @author X2 Development Corporation
 */
public class RefreshRubricPerformanceDescriptors extends ProcedureJavaSource {
    private static final long serialVersionUID = 1L;

    private static final String MATCH_BASIS_PARAM = "matchBasis";
    private static final String ADD_NEW_RATINGS_PARAM = "addNewRatings";
    private static final String DROP_MISSING_RATINGS_PARAM = "dropMissingRatings";
    private static final String REFRESH_CHILDREN_PARAM = "refreshChildren";

    private UserDataContainer m_userData;

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @throws X2BaseException exception
     * @see
     *      com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#saveState(UserDataContainer
     *      userData)
     */
    @Override
    protected void saveState(UserDataContainer userData) throws com.x2dev.utils.X2BaseException {
        m_userData = userData;
    }

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        ModelBroker broker = new ModelBroker(m_userData); // Refresh Ratings Helper is super old and
                                                          // requires us to pass a ModelBroker
                                                          // instance instead of an X2Broker.

        // Get all RubricDefinitions based on the current list selection
        QueryByCriteria query = createQueryByCriteria(RubricDefinition.class, getCurrentCriteria());
        Collection<RubricDefinition> rubricDefinitions = broker.getCollectionByQuery(query);

        List<String> failedToUpdateOids = new ArrayList<String>();
        List<String> updatedRubricNames = new ArrayList<String>();

        if (rubricDefinitions != null && !rubricDefinitions.isEmpty()) {
            int refreshedCount = 0;
            logMessage("Rating Scales associated to the " + rubricDefinitions.size()
                    + " selected Rubric Definition(s) will be refreshed.");

            int matchBasis = ((Integer) getParameter(MATCH_BASIS_PARAM)).intValue();
            boolean addNewRatings = ((Boolean) getParameter(ADD_NEW_RATINGS_PARAM)).booleanValue();
            boolean dropMissingRatings = ((Boolean) getParameter(DROP_MISSING_RATINGS_PARAM)).booleanValue();
            boolean refreshChildren = ((Boolean) getParameter(REFRESH_CHILDREN_PARAM)).booleanValue();

            RatingRefreshHelper refreshHelper = new RatingRefreshHelper(matchBasis,
                    addNewRatings,
                    dropMissingRatings,
                    refreshChildren,
                    getLocale(),
                    m_userData,
                    broker);

            for (RubricDefinition rubricDefinition : rubricDefinitions) {
                try {
                    RubricDetail rubricDetail = new RubricDetail(rubricDefinition, m_userData,
                            "assessment.rubric.list.detail", broker, getLocale());
                    for (GenericDetail childDetail : rubricDetail.getChildDetailSet().getChildDetails()) {
                        RubricCriterionDetail criterionDetail = (RubricCriterionDetail) childDetail;
                        refreshHelper.refreshRatings(criterionDetail, broker);
                        criterionDetail.save(null, m_userData, broker);
                    }
                    updatedRubricNames.add(rubricDefinition.getName());
                    refreshedCount++;
                } catch (Exception e) {
                    failedToUpdateOids.add(rubricDefinition.getOid());
                    logMessage(
                            "An unexpected exception occurred while attempting to refresh rating scales for rubric definition: "
                                    + rubricDefinition.getOid() + ". Exception message follows: " + e.getMessage());
                }
            }

            logMessage("Successfully refreshed " + refreshedCount
                    + " Rubric Definition Rating Scale(s). They are as follows: " + updatedRubricNames);
            if (failedToUpdateOids.size() > 0) {
                logMessage(
                        "Errors occurred that prevented Rubric Scales related to these rubric definitions from being refreshed.  Consult the log for more details.  Here is the list of failed OIDs: "
                                + failedToUpdateOids);
            }
        } else {
            logMessage(
                    "No Rubric Rating Scales were selected or none could be found based on the current list.  Please ensure at least one rating scale is selected when running this procedure.");
        }
    }
}
