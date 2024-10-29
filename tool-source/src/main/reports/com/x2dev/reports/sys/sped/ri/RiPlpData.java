/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2003 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.reports.sys.sped.ri;

import com.follett.fsc.core.k12.beans.GenericFormChildData;
import com.follett.fsc.core.k12.beans.GenericFormData;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import net.sf.jasperreports.engine.JRDataSource;

/**
 * Java source for a form-based report. All fields and aliases present on the form storage and owner
 * objects are available for use in the format. The storage and owner objects are retrieved and made
 * available by the superclass - <code>BaseFormReportJavaSource</code>.
 * <p>
 * In the report format, fields and aliases from the storage object can be accessed directly. To
 * retrieve values from the owner object, the prefix <b>"owner"</b> must be present on the field
 * or alias. See <code>SimpleFormDataSource</code> for more information.
 *
 * @author X2 Development Corporation
 */
public class RiPlpData extends BaseFormReportJavaSource {
    /**
     *
     */
    private static final long serialVersionUID = 1L;
    /**
     * Constants for elements in the field description.
     */
    private static final String CHILD_PREFIX = "child.";
    private static final String SCREENING_PREFIX = "screening(";
    private static final String DIAGNOSTIC_PREFIX = "diagnostic(";
    private static final String PROGRESS_PREFIX = "progress(";
    private static final String OUTCOME_PREFIX = "outcome(";
    private static final String INDEX_POSTFIX = ").";

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws Exception exception
     * @see com.x2dev.sis.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        return new RiPlpDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale());
    }

    /**
     * An extension of simple generic form data source that includes lists of generic child data.
     * This case breaks the child data into multiple lists for different uses.
     *
     * @author X2 Development Corporation
     */
    public static class RiPlpDataSource extends SimpleFormDataSource {
        /**
         * A list of GenericFormChildData that belong to the current GenericFormData.
         */
        List<GenericFormChildData> m_screeningData = new ArrayList<GenericFormChildData>();
        List<GenericFormChildData> m_diagnosticData = new ArrayList<GenericFormChildData>();
        List<GenericFormChildData> m_progressData = new ArrayList<GenericFormChildData>();
        List<GenericFormChildData> m_outcomeData = new ArrayList<GenericFormChildData>();

        /**
         * Constructor. Call super constructor.
         *
         * @param formStorage X2BaseBean
         * @param formOwner X2BaseBean
         * @param dictionary DataDictionary
         * @param locale Locale
         */
        RiPlpDataSource(X2BaseBean formStorage, X2BaseBean formOwner,
                DataDictionary dictionary, Locale locale) {
            super(formStorage, formOwner, dictionary, locale);

            /*
             * Create lists of child form detail by type.
             */
            if (formStorage instanceof GenericFormData) {
                GenericFormData genericFormData = (GenericFormData) formStorage;
                for (GenericFormChildData child : genericFormData.getGenericFormDataChildren()) {
                    if (!StringUtils.isEmpty(child.getFieldA001())) {
                        m_screeningData.add(child);
                    } else if (!StringUtils.isEmpty(child.getFieldA002())) {
                        m_diagnosticData.add(child);
                    } else if (!StringUtils.isEmpty(child.getFieldA003())) {
                        m_progressData.add(child);
                    } else if (!StringUtils.isEmpty(child.getFieldA004())) {
                        m_outcomeData.add(child);
                    }
                }
            }
        }

        /**
         * Gets the field value.
         *
         * @param fieldName String
         * @return Object
         * @throws X2BaseException exception
         * @see com.x2dev.sis.tools.reports.BeanDataSource#getFieldValue(java.lang.String)
         */
        @Override
        protected Object getFieldValue(String fieldName) throws X2BaseException {
            Object fieldValue = "";

            /*
             * If the field starts with the child prefix, set the current bean for value retrieval
             * to
             * the identified child bean by type and index.
             * Also strip off the all prefixes and prepend the alias (note the special handling for
             * doing this
             * if a prefix is present - e.g. a:owner.alias)
             */
            if (fieldName.startsWith(CHILD_PREFIX)) {
                GenericFormChildData child = getChildBean(fieldName);
                int pos = fieldName.lastIndexOf(".");
                fieldName = "a,r:" + fieldName.substring(pos + 1);
                if (child != null) {
                    X2BaseBean storageBean = getCurrentBean();
                    setCurrentBean(child);
                    fieldValue = super.getFieldValue(fieldName);
                    setCurrentBean(storageBean);
                }
            } else {
                fieldValue = super.getFieldValue(fieldName);
            }

            return fieldValue;
        }

        /**
         * When the fieldName is prefixed with "child." parse the string to identify
         * Which child record list and index position it references.
         *
         * @param fieldName String
         * @return GenericFormChildData
         */
        private GenericFormChildData getChildBean(String fieldName) {
            // Remove the "child." prefix.
            fieldName = fieldName.substring(CHILD_PREFIX.length());

            // Get the next prefix string identifying which list of children to use.
            int pos = fieldName.indexOf('(');
            String prefix = fieldName.substring(0, pos + 1);
            fieldName = fieldName.substring(pos + 1);

            // Get the index integer identifying which child in the list to use.
            pos = fieldName.indexOf(INDEX_POSTFIX);
            String indexStr = fieldName.substring(0, pos);
            fieldName = fieldName.substring(pos + 2);
            int index = 0;
            try {
                index = Integer.parseInt(indexStr);
            } catch (NumberFormatException nfe) {
                index = 0;
            }

            // Retrieve the child from the lists.
            GenericFormChildData child = null;
            if (prefix.equals(SCREENING_PREFIX)) {
                if (index < m_screeningData.size()) {
                    child = m_screeningData.get(index);
                }
            } else if (prefix.equals(DIAGNOSTIC_PREFIX)) {
                if (index < m_diagnosticData.size()) {
                    child = m_diagnosticData.get(index);
                }
            } else if (prefix.equals(PROGRESS_PREFIX)) {
                if (index < m_progressData.size()) {
                    child = m_progressData.get(index);
                }
            } else if (prefix.equals(OUTCOME_PREFIX)) {
                if (index < m_outcomeData.size()) {
                    child = m_outcomeData.get(index);
                }
            }

            return child;
        }
    }
}
