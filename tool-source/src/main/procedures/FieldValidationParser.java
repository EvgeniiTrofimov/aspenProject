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
import com.follett.fsc.core.k12.beans.DataField;
import com.follett.fsc.core.k12.beans.DataFieldConfig;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.BeanFieldParser;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryTable;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.x2dev.utils.StringUtils;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import org.apache.commons.beanutils.PropertyUtils;
import org.apache.ojb.broker.query.QueryByCriteria;

/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2010 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

/**
 * Field Validation Parser
 * <p>
 * This parser queries the data dictionary for any enabled fields that have Validation Expressions*
 * specified. For each table containing fields w/ Validation Expressions, each is parsed, and the
 * fields on each bean are parsed. Data that is validated is possibly reformatted and saved.
 * Data that fails validation is reported in the message log.
 * <p>
 * <b style="color:red;">*</b> Validation Expressions are stored in the REGEX LIBRARY and can be
 * used to verify
 * the format and content of data as well as re-format the data into a particular layout
 *
 * @author X2 Development Corporation
 */
public class FieldValidationParser extends ProcedureJavaSource {

    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        logMessage("FieldValidationParser");

        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getUser().getPersistenceKey());

        /*
         * Get a collection by Table of all enabled fields that have validation expressions defined
         */
        X2Criteria criteria = new X2Criteria();
        criteria.addNotNull(DataFieldConfig.COL_VALIDATOR_EXPRESSION_IDS);
        criteria.addEqualTo(DataFieldConfig.COL_ENABLED_INDICATOR, Boolean.TRUE);
        QueryByCriteria query = new QueryByCriteria(DataFieldConfig.class, criteria);
        Map<String, Collection> dataFieldConfigMap = getBroker().getGroupedCollectionByQuery(query,
                DataFieldConfig.REL_DATA_FIELD + "." + DataField.COL_DATA_TABLE_OID, 5);

        /*
         * For each table, iterate through all of the beans that have data in the fields that have
         * validation expressions. E.g. If phone01, phone02, and phone03 fields all have validation
         * expressions, get the beans where any of those fields are not null.
         */
        for (String tableOid : dataFieldConfigMap.keySet()) {
            DataDictionaryTable dataDictionaryTable = dictionary.findDataDictionaryTableById(tableOid);
            logMessage("Validating fields in the table " + dataDictionaryTable.getClassName() + ".");

            X2Criteria beanCriteria = new X2Criteria();

            for (Object object : dataFieldConfigMap.get(tableOid)) {
                DataFieldConfig dataFieldConfig = (DataFieldConfig) object;
                DataDictionaryField field = dictionary.findDataDictionaryField(dataFieldConfig.getDataField());
                X2Criteria fieldCriteria = new X2Criteria();
                fieldCriteria.addNotNull(field.getJavaName());
                beanCriteria.addOrCriteria(fieldCriteria);
            }

            QueryByCriteria beanQuery = new QueryByCriteria(dataDictionaryTable.getBeanClass(), beanCriteria);
            QueryIterator beanIterator = getBroker().getIteratorByQuery(beanQuery);
            try {
                logMessage("Validating " + getBroker().getCount(beanQuery) + " beans in the table "
                        + dataDictionaryTable.getClassName() + ".");

                /*
                 * for each bean in the table
                 */
                while (beanIterator.hasNext()) {
                    X2BaseBean bean = (X2BaseBean) beanIterator.next();

                    /*
                     * for each field on the bean with a validation expression
                     */
                    for (Object object : dataFieldConfigMap.get(tableOid)) {
                        DataFieldConfig dataFieldConfig = (DataFieldConfig) object;
                        DataDictionaryField field = dictionary.findDataDictionaryField(dataFieldConfig.getDataField());

                        /*
                         * only parse VARCHAR fields and fields where the expression ID is actually
                         * set (vs. being 0 bytes long). The latter check is not necessary in 2.12
                         * and greater, as CLOBs will be null where BLOBs were zero byte arrays)
                         */
                        if (DataField.VARCHAR_DATABASE_TYPE.equals(field.getDatabaseType())
                                && dataFieldConfig.getValidatorExpressionIds().length() > 0) {
                            String value = (String) PropertyUtils.getProperty(bean, field.getJavaName());
                            if (!StringUtils.isEmpty(value)) {
                                String validatorExpressionIds =
                                        field.getDataFieldConfig().getValidatorExpressionIds();
                                List<String> formatterIDs =
                                        StringUtils.convertDelimitedStringToList(validatorExpressionIds, ',');

                                /*
                                 * Iterate through the validation expressions until one is
                                 * successful
                                 */
                                boolean validated = false;
                                Iterator<String> formatterIterator = formatterIDs.iterator();
                                while (formatterIterator.hasNext() && !validated) {
                                    String formatterId = formatterIterator.next().trim();

                                    BeanFieldParser beanFieldParser =
                                            BeanFieldParser.getBeanFieldParser(formatterId, getBroker());
                                    if (beanFieldParser != null) {
                                        validated = beanFieldParser.parseLine(bean,
                                                (String) PropertyUtils.getProperty(bean, field.getJavaName()), null);
                                        if (validated) {
                                            value = beanFieldParser.validateAndFormatLine(value);
                                            if (value.length() <= field.getLength() || field.getLength() == 0) {
                                                PropertyUtils.setProperty(bean, field.getJavaName(), value);
                                            }
                                        }
                                    }
                                }
                                if (!validated) {
                                    logMessage("Unable to validate field " + field.getJavaName() + " for bean "
                                            + bean.getOid() + ":  "
                                            + (String) PropertyUtils.getProperty(bean, field.getJavaName()));
                                }
                            }
                        }
                    }
                    if (bean.isDirty()) {
                        getBroker().saveBeanForced(bean);
                    }
                }
            } finally {
                beanIterator.close();
            }
        }
    }
}
