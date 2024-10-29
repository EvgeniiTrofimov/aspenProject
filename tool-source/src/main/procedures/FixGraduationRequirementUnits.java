/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2019 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.x2dev.sis.model.beans.GraduationRequirement;
import com.x2dev.sis.model.business.GraduationRequirementCreditsHelper;

/**
 * The Class FixGraduationRequirementUnits.
 *
 * @author Follett Software Company
 * @copyright 2019
 */
public class FixGraduationRequirementUnits extends ProcedureJavaSource {

    /**
     * Save all credit/other graduation requirements. This will automatically calculate all
     * graduation requirement group required units, graduation requirement sub-program required
     * units, and all graduation program total credits
     *
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {

        // Get a list of all graduation programs
        X2Criteria criteria = new X2Criteria();
        GraduationRequirementCreditsHelper.appendMassUpdateCriteria(criteria);
        BeanQuery query = new BeanQuery(GraduationRequirement.class, criteria);

        try (QueryIterator iterator = getBroker().getIteratorByQuery(query)) {
            while (iterator.hasNext()) {
                /*
                 * Saving the requirement will trigger the core code to auto-recalculate the parent
                 * requirement groups and programs
                 */
                getBroker().saveBeanForced((GraduationRequirement) iterator.next());
            }
        }
    }
}
