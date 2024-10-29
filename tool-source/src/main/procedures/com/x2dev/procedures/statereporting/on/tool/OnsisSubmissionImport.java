/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2022 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.on.tool;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.Procedure;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.x2dev.sis.model.beans.UserDefinedTableA;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.stream.Collectors;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.jdom.Document;
import org.jdom.Element;
import org.jdom.JDOMException;
import org.jdom.input.SAXBuilder;

/**
 * The Class OnsisSubmissionImport.
 *
 * @author Follett Software Company
 * @copyright 2022
 */
public class OnsisSubmissionImport extends ProcedureJavaSource {
    /**
     * The Class Submission.
     */
    public class Submission {
        /**
         * Instantiates a new submission.
         *
         * @param element the element
         */
        public Submission(Element element) {
            type = element.getChild(ALIAS_TYPE).getText();
            period = element.getChild(ALIAS_PERIOD).getText();
            startDate = element.getChild(ALIAS_START_DATE).getText();
            endDate = element.getChild(ALIAS_END_DATE).getText();
            batchType = element.getChild(ALIAS_BATCH_TYPE).getText();
            clearFlags = element.getChild(ALIAS_CLEAR_FLAGS).getText();
            validateEducatorAssignment = element.getChild(ALIAS_VALIDATE_EDUCATOR_ASSIGNMENT).getText();
            fields = element.getChild(ALIAS_FIELDS).getText();
        }

        private String batchType;
        private String clearFlags;
        private String endDate;
        private String fields;
        private String period;
        private String startDate;
        private String type;
        private String validateEducatorAssignment;

        /**
         * Gets the batch type.
         *
         * @return the batchType
         */
        public String getBatchType() {
            return batchType;
        }

        /**
         * Gets the clear flags.
         *
         * @return the clearFlags
         */
        public String getClearFlags() {
            return clearFlags;
        }

        /**
         * Gets the end date.
         *
         * @return the endDate
         */
        public String getEndDate() {
            return endDate;
        }

        /**
         * Gets the fields.
         *
         * @return the fields
         */
        public String getFields() {
            return fields;
        }

        /**
         * Gets the period.
         *
         * @return the period
         */
        public String getPeriod() {
            return period;
        }

        /**
         * Gets the start date.
         *
         * @return the startDate
         */
        public String getStartDate() {
            return startDate;
        }

        /**
         * Gets the type.
         *
         * @return the type
         */
        public String getType() {
            return type;
        }

        /**
         * Gets the validate educator assignment.
         *
         * @return the validateEducatorAssignment
         */
        public String getValidateEducatorAssignment() {
            return validateEducatorAssignment;
        }

    }

    private static final String ALIAS_BATCH_TYPE = "batch-type";
    private static final String ALIAS_CLEAR_FLAGS = "clear-fields";
    private static final String ALIAS_END_DATE = "period-end-date";
    private static final String ALIAS_FIELDS = "fields";
    private static final String ALIAS_PERIOD = "submission-period-code";
    private static final String ALIAS_START_DATE = "period-start-date";
    private static final String ALIAS_TYPE = "submission-type";
    private static final String ALIAS_VALIDATE_EDUCATOR_ASSIGNMENT = "validate-add-update";

    private static final String DDX_ID = "ddxOnSisSubFld";

    private static final String ELEMENT_SUBMISSION = "submission";

    private static final String PARAM_PROCEDURE_ID = "procedureId";

    private DataDictionary m_dictionary = null;

    /**
     * Execute.
     *
     * @throws Exception the exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {

        Procedure proc = getProcedure();
        loadSubmissions(proc.getFullInputDefinition()).stream().forEach(submission -> {
            X2BaseBean bean = getSubmissionBean(submission);
            bean.setFieldValueByAlias(ALIAS_TYPE, submission.getType(), getDictionary());
            bean.setFieldValueByAlias(ALIAS_PERIOD, submission.getPeriod(), getDictionary());
            bean.setFieldValueByAlias(ALIAS_START_DATE, submission.getStartDate(), getDictionary());
            bean.setFieldValueByAlias(ALIAS_END_DATE, submission.getEndDate(), getDictionary());
            bean.setFieldValueByAlias(ALIAS_BATCH_TYPE, submission.getBatchType(), getDictionary());
            bean.setFieldValueByAlias(ALIAS_CLEAR_FLAGS, submission.getClearFlags(), getDictionary());
            bean.setFieldValueByAlias(ALIAS_VALIDATE_EDUCATOR_ASSIGNMENT, submission.getValidateEducatorAssignment(),
                    getDictionary());
            bean.setFieldValueByAlias(ALIAS_FIELDS, submission.getFields(), getDictionary());
            getBroker().saveBeanForced(bean);
            verifyReferenceCode(submission.getPeriod(), submission.getBatchType());
        });
        getBroker().deleteBean(proc);

    }

    /**
     * Gets the dictionary.
     *
     * @return Data dictionary
     */
    private DataDictionary getDictionary() {
        if (m_dictionary == null) {
            X2Criteria ddxCriteria = new X2Criteria();
            ddxCriteria.addEqualTo(X2BaseBean.COL_OID, DDX_ID);
            QueryByCriteria ddxQuery = new QueryByCriteria(ExtendedDataDictionary.class, ddxCriteria);
            ExtendedDataDictionary ddx = getBroker().getBeanByQuery(ddxQuery);
            m_dictionary = DataDictionary.getDistrictDictionary(ddx, getBroker().getPersistenceKey());
        }
        return m_dictionary;
    }

    /**
     * Get the procedure bean based on the id from the input definition.
     * This procedure's input definition is the source of the SQLDocument.
     *
     * @return Procedure
     */
    private Procedure getProcedure() {
        return (Procedure) getJob().getTool();
    }

    /**
     * Gets the submission bean.
     *
     * @param submission the submission
     * @return the submission bean
     */
    private UserDefinedTableA getSubmissionBean(Submission submission) {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(getDictionary().findDataDictionaryFieldByAlias(ALIAS_TYPE).getJavaName(),
                submission.getType());
        BeanQuery query = new BeanQuery(UserDefinedTableA.class, criteria);
        UserDefinedTableA bean = getBroker().getBeanByQuery(query);
        if (bean == null) {
            bean = X2BaseBean.newInstance(UserDefinedTableA.class, getBroker().getPersistenceKey());
            OrganizationManager.setOrganizationOids(bean, getOrganization());
            bean.setExtendedDataDictionaryOid(DDX_ID);
            logMessage("Creating new submission - " + submission.getType());
        } else {
            logMessage("Updating submission - " + submission.getType());
        }
        return bean;
    }

    /**
     * Load submissions.
     *
     * @param input the input
     * @return the list
     * @throws JDOMException the JDOM exception
     * @throws IOException Signals that an I/O exception has occurred.
     */
    private List<Submission> loadSubmissions(String input) throws JDOMException, IOException {
        List<String> statements = new LinkedList();
        SAXBuilder builder = new SAXBuilder();
        Document document = builder.build(new ByteArrayInputStream(input.getBytes()));
        Element root = document.getRootElement();
        List<Element> submissions = root.getChildren(ELEMENT_SUBMISSION);
        return submissions == null ? Collections.EMPTY_LIST
                : submissions.stream().map(element -> new Submission(element)).collect(Collectors.toList());
    }

    /**
     * @param period
     * @param batchType
     */
    private void verifyReferenceCode(String period, String batchType) {
        DataDictionaryField field = getDictionary().findDataDictionaryFieldByAlias(ALIAS_PERIOD);
        if (field.hasReferenceTable()) {
            ReferenceTable referenceTable = field.getReferenceTable();
            DataDictionary dictionary = DataDictionary.getDistrictDictionary(referenceTable.getExtendedDataDictionary(),
                    getBroker().getPersistenceKey());

            ReferenceCode code = referenceTable.getCodeMap(getBroker()).values().stream()
                    .filter(rcd -> period.equals(rcd.getStateCode())).findAny().orElse(null);
            if (code == null) {
                logMessage("Adding reference code - " + period);
                ReferenceCode bean = X2BaseBean.newInstance(ReferenceCode.class, getBroker().getPersistenceKey());
                bean.setReferenceTableOid(referenceTable.getOid());
                bean.setOwnerOid(referenceTable.getOwnerOid());
                bean.setOwnerType(referenceTable.getOwnerType());
                bean.setCode(period);
                bean.setStateCode(period);
                bean.setDescription(batchType);
                bean.setFieldValueByAlias("all-rcd-MinistryTableName", "SUBMISSION_PERIOD_TYPE", dictionary);
                getBroker().saveBeanForced(bean);
            }
        }


    }


}
