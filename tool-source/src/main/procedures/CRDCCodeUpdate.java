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
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import org.apache.ojb.broker.query.QueryByCriteria;


/**
 * Procedure to update CRDC code.
 *
 * @author X2 Development Corporation
 */
public class CRDCCodeUpdate extends ProcedureJavaSource {

    protected static final String ALIAS_RCD_CRDC_CODE = "all-rcd-CRDCCode";
    protected static final String INPUT_COMMIT = "commit";
    protected static final String INPUT_NEW_CODE = "newCRDCCode";
    protected static final String INPUT_OLD_CODE = "oldCRDCCode";

    private static final long serialVersionUID = 1L;

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        /*
         * Retrieve teachers by homeroom
         */
        String oldCrdcCode = (String) getParameter(INPUT_OLD_CODE);
        String newCrdcCode = (String) getParameter(INPUT_NEW_CODE);
        Boolean commit = (Boolean) getParameter(INPUT_COMMIT);
        if (!StringUtils.isEmpty(oldCrdcCode)) {
            DataDictionaryField field = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey())
                    .findDataDictionaryFieldByAlias(ALIAS_RCD_CRDC_CODE);
            if (field != null) {
                String crdcCodeJavaName = field.getJavaName();
                if (!StringUtils.isEmpty(crdcCodeJavaName)) {
                    X2Criteria refCodeCriteria = new X2Criteria();
                    refCodeCriteria.addEqualTo(crdcCodeJavaName, oldCrdcCode);
                    ReferenceCode rcdToUpdate =
                            getBroker().getBeanByQuery(new QueryByCriteria(ReferenceCode.class, refCodeCriteria));
                    if (rcdToUpdate != null) {
                        if (commit.booleanValue()) {
                            rcdToUpdate.setFieldValueByBeanPath(crdcCodeJavaName, newCrdcCode);
                            getBroker().saveBeanForced(rcdToUpdate);
                            logMessage("Commit mode." + "\n" + "For table with name: \""
                                    + rcdToUpdate.getReferenceTable().getUserName() + "\" CRDC code: \"" + oldCrdcCode
                                    + "\" was updated with CRDC code: \"" + newCrdcCode + "\".");
                        } else {
                            logMessage("View mode." + "\n" + "For table with name: \""
                                    + rcdToUpdate.getReferenceTable().getUserName() + "\" CRDC code: \"" + oldCrdcCode
                                    + "\" will be updated in Commit mode with CRDC code: \"" + newCrdcCode + "\".");
                        }
                    } else {
                        logMessage("There was not found refernce code record with CRDC code: \"" + oldCrdcCode + "\".");
                    }
                }
            } else {
                logMessage(
                        "There was not found found field with alias: \"" + ALIAS_RCD_CRDC_CODE
                                + "\" in Data Dictionary.");
            }
        } else {
            logMessage("You should input old alias name to update.");
        }
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException {
        super.saveState(userData);
    }
}
