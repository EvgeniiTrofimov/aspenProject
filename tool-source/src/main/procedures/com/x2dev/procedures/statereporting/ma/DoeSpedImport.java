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
package com.x2dev.procedures.statereporting.ma;
/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2006 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.imports.TextImportJavaSource;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.types.PlainDate;
import java.text.SimpleDateFormat;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Tool for importing SPED data from a Special Education system into the X2 SIS. The SPED data
 * should be stored in a plain text file with comma-separated values wrapped in double-quotation
 * marks. The file must contain the SASID to identify students and zero or more special education
 * fields (DOE 32, 34, 36, 38, 40). The user specifies the position of each field with input
 * parameters.
 * The positions should be 1-indexed (1, 2, 3, etc.) rather than 0-indexed (0, 1, 2, etc.).
 * <p>
 * Discountinued and default values are not imported. All other values are translated from state
 * reference codes to base reference codes and then imported.
 *
 * @author X2 Development Corporation
 */
public class DoeSpedImport extends TextImportJavaSource {

    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Name for the "DOE 32 index" parameter. The value is an Integer and may be null.
     */
    public static final String DOE_32_INDEX_PARAM = "doe32Index";

    /**
     * Name for the "DOE 34 index" parameter. The value is an Integer and may be null.
     */
    public static final String DOE_34_INDEX_PARAM = "doe34Index";

    /**
     * Name for the "DOE 36 index" parameter. The value is an Integer and may be null.
     */
    public static final String DOE_36_INDEX_PARAM = "doe36Index";

    /**
     * Name for the "DOE 38 index" parameter. The value is an Integer and may be null.
     */
    public static final String DOE_38_INDEX_PARAM = "doe38Index";

    /**
     * Name for the "DOE 39 index" parameter. The value is an Integer and may be null.
     */
    public static final String DOE_39_INDEX_PARAM = "doe39Index";

    /**
     * Name for the "DOE 40 index" parameter. The value is an Integer and may be null.
     */
    public static final String DOE_40_INDEX_PARAM = "doe40Index";

    /**
     * Name for the "field count" parameter. The value is an Integer.
     */
    public static final String FIELD_COUNT_PARAM = "fieldCount";

    /**
     * Name for the "SASID index" parameter. The value is an Integer.
     */
    public static final String SASID_INDEX_PARAM = "sasidIndex";

    /**
     * Name for the "Last IEP eval date index" parameter. The value is an Integer.
     */
    public static final String LAST_EVAL_DATE_INDEX_PARAM = "spedLastEvalDateIndex";

    /*
     * DOE field alias constants.
     */
    private static final String DOE_32_SPED_PLACEMENT = "DOE 32";
    private static final String DOE_34_SPED_PLACEMENT = "DOE 34";
    private static final String DOE_36_SPED_DISABILITY = "DOE 36";
    private static final String DOE_38_SPED_LEVEL = "DOE 38";
    private static final String DOE_39_504_PROGRAM = "DOE 39";
    private static final String DOE_40_SPED_EVALUATION_RESULTS = "DOE 40";

    private Map m_aliasToIndexLookup;
    private int m_fieldCount;
    private Map m_fieldDefaults;
    private Map m_fieldToRefTable;
    private int m_sasidIndex;
    private int m_spedLastEvalDateIndex;

    /**
     * Gets the field count.
     *
     * @return int
     * @see com.follett.fsc.core.k12.tools.imports.TextImportJavaSource#getFieldCount()
     */
    @Override
    protected int getFieldCount() {
        return m_fieldCount;
    }

    /**
     * Import record.
     *
     * @param record List
     * @param lineNumber int
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.imports.TextImportJavaSource#importRecord(java.util.List,
     *      int)
     */
    @Override
    protected void importRecord(List record, int lineNumber) throws Exception {
        String sasid = (String) record.get(m_sasidIndex);

        Criteria criteria = new Criteria();
        criteria.addEqualTo(SisStudent.COL_STATE_ID, sasid);

        if (isSchoolContext()) {
            criteria.addEqualTo(SisStudent.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            criteria.addEqualTo(SisStudent.COL_ORGANIZATION1_OID, getOrganization().getOid());
        }

        QueryByCriteria query = new QueryByCriteria(SisStudent.class, criteria);

        SisStudent student = (SisStudent) getBroker().getBeanByQuery(query);
        if (student != null) {
            incrementMatchCount();

            /*
             * Iterate over the list of aliased properties and update the student with each value
             */
            Iterator aliases = m_aliasToIndexLookup.keySet().iterator();
            while (aliases.hasNext()) {
                String alias = (String) aliases.next();
                int index = ((Integer) m_aliasToIndexLookup.get(alias)).intValue();
                String value = ((String) record.get(index)).trim();

                if (StringUtils.isEmpty(value)) {
                    value = null;
                } else {
                    value = getBaseCode(alias, value);
                }

                student.setFieldValueByAlias(alias, value);
            }

            if (m_spedLastEvalDateIndex >= 0) {
                SimpleDateFormat format = new SimpleDateFormat("MM/dd/yyyy");
                String lastEvalDateString = (String) record.get(m_spedLastEvalDateIndex);
                if (!StringUtils.isEmpty(lastEvalDateString)) {
                    student.setSpedLastEvaluationDate(new PlainDate(format.parse(lastEvalDateString)));
                }
            }

            if (student.isDirty()) {
                getBroker().saveBeanForced(student);
                incrementUpdateCount();
            }
        } else {
            incrementSkipCount();
            logInvalidRecord(lineNumber, "No matching student for SASID: " + sasid);
        }
    }

    /**
     * Initialize.
     *
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        /*
         * How many fields are in each record?
         */
        m_fieldCount = ((Integer) getParameter(FIELD_COUNT_PARAM)).intValue();

        /*
         * Build the alias-to-index lookup. This map may have 0 to 5 entries depending upon
         * user input. Each index is translated to a 0-indexed value.
         */
        m_aliasToIndexLookup = new HashMap();

        Integer index = (Integer) getParameter(DOE_32_INDEX_PARAM);
        if (index != null && index.intValue() != 0) {
            m_aliasToIndexLookup.put(DOE_32_SPED_PLACEMENT, Integer.valueOf(index.intValue() - 1));
        }

        index = (Integer) getParameter(DOE_34_INDEX_PARAM);
        if (index != null && index.intValue() != 0) {
            m_aliasToIndexLookup.put(DOE_34_SPED_PLACEMENT, Integer.valueOf(index.intValue() - 1));
        }

        index = (Integer) getParameter(DOE_36_INDEX_PARAM);
        if (index != null && index.intValue() != 0) {
            m_aliasToIndexLookup.put(DOE_36_SPED_DISABILITY, Integer.valueOf(index.intValue() - 1));
        }

        index = (Integer) getParameter(DOE_38_INDEX_PARAM);
        if (index != null && index.intValue() != 0) {
            m_aliasToIndexLookup.put(DOE_38_SPED_LEVEL, Integer.valueOf(index.intValue() - 1));
        }

        index = (Integer) getParameter(DOE_39_INDEX_PARAM);
        if (index != null && index.intValue() != 0) {
            m_aliasToIndexLookup.put(DOE_39_504_PROGRAM, Integer.valueOf(index.intValue() - 1));
        }

        index = (Integer) getParameter(DOE_40_INDEX_PARAM);
        if (index != null && index.intValue() != 0) {
            m_aliasToIndexLookup.put(DOE_40_SPED_EVALUATION_RESULTS, Integer.valueOf(index.intValue() - 1));
        }

        /*
         * Get the SASID index (subtract 1 to translate it to a 0-indexed value).
         */
        m_sasidIndex = ((Integer) getParameter(SASID_INDEX_PARAM)).intValue() - 1;

        /*
         * Get the Last IEP eval date index (subtract 1 to translate it to a 0-indexed value).
         */
        if (getParameter(LAST_EVAL_DATE_INDEX_PARAM) != null) {
            m_spedLastEvalDateIndex = ((Integer) getParameter(LAST_EVAL_DATE_INDEX_PARAM)).intValue() - 1;
        } else {
            m_spedLastEvalDateIndex = -1;
        }

        loadReferenceTables();
    }

    /**
     * Returns the base code for the given state code of the property.
     *
     * @param property String
     * @param value String
     * @return String
     */
    private String getBaseCode(String property, String value) {
        Map codeMap = (Map) m_fieldToRefTable.get(property);
        return (String) codeMap.get(value);
    }

    /**
     * Returns a map of state reference codes to their base reference code equivalents for the
     * reference table used by the given student property (represented by an alias). If the student
     * property doesn't use a reference table then an empty map is returned.
     *
     * @param alias String
     * @return A Map of String keys to String values
     */
    private Map getReferenceMap(String alias) {
        HashMap stateToBaseCodes = null;

        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getUser().getPersistenceKey());
        DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(alias);
        if (field.hasReferenceTable()) {
            Collection codes = field.getReferenceTable().getReferenceCodes();
            stateToBaseCodes = new HashMap((int) (codes.size() * 1.5));
            Iterator codeIterator = codes.iterator();
            while (codeIterator.hasNext()) {
                ReferenceCode code = (ReferenceCode) codeIterator.next();
                if (!StringUtils.isEmpty(code.getStateCode()) && !code.getDisabledIndicator()) {
                    stateToBaseCodes.put(code.getStateCode(), code.getCode());
                }
            }
        } else {
            stateToBaseCodes = new HashMap();
        }

        return stateToBaseCodes;
    }

    /**
     * Returns true if the value is the default value for the propety represented by the alias,
     * false otherwise.
     *
     *
     * @param alias String
     * @param value String
     * @return boolean
     */
    private boolean isDefaultValue(String alias, String value) {
        return value.equals(m_fieldDefaults.get(alias));
    }

    /**
     * Loads the reference tables for default values and state-to-base code translation.
     */
    private void loadReferenceTables() {
        /*
         * Default values
         */
        m_fieldDefaults = new HashMap();

        m_fieldDefaults.put(DOE_32_SPED_PLACEMENT, "00");
        m_fieldDefaults.put(DOE_34_SPED_PLACEMENT, "00");
        m_fieldDefaults.put(DOE_36_SPED_DISABILITY, "500");
        m_fieldDefaults.put(DOE_38_SPED_LEVEL, "500");
        m_fieldDefaults.put(DOE_39_504_PROGRAM, "00");
        m_fieldDefaults.put(DOE_40_SPED_EVALUATION_RESULTS, "00");

        /*
         * State code maps
         */
        m_fieldToRefTable = new HashMap();

        m_fieldToRefTable.put(DOE_32_SPED_PLACEMENT, getReferenceMap(DOE_32_SPED_PLACEMENT));
        m_fieldToRefTable.put(DOE_34_SPED_PLACEMENT, getReferenceMap(DOE_34_SPED_PLACEMENT));
        m_fieldToRefTable.put(DOE_36_SPED_DISABILITY, getReferenceMap(DOE_36_SPED_DISABILITY));
        m_fieldToRefTable.put(DOE_38_SPED_LEVEL, getReferenceMap(DOE_38_SPED_LEVEL));
        m_fieldToRefTable.put(DOE_39_504_PROGRAM, getReferenceMap(DOE_39_504_PROGRAM));
        m_fieldToRefTable.put(DOE_40_SPED_EVALUATION_RESULTS, getReferenceMap(DOE_40_SPED_EVALUATION_RESULTS));
    }
}
