/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2021 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.on.revised;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatDefinition;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.X2Broker;
import com.x2dev.procedures.statereporting.common.FilterableFactory;
import com.x2dev.procedures.statereporting.common.FilterableFactory.Filter;
import com.x2dev.procedures.statereporting.common.FilterableFactory.Filterable;
import com.x2dev.procedures.statereporting.on.revised.OnsisResultsHelper.ElementsHelper;
import com.x2dev.procedures.statereporting.on.revised.OnsisStateReportData.OnsisRetrieverNestedExport;
import com.x2dev.procedures.statereporting.on.revised.OnsisStateReportData.OnsisRetrieverNestedExportStreaming;
import com.x2dev.utils.StringUtils;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import org.w3c.dom.Node;

/**
 * @author Follett Software Company
 * @copyright 2021
 */
/**
 * The Class ExsmsElementsHelper.
 */
public class ExsmsElementsHelper implements ElementsHelper {
    private static final String NO_PERFMON4J_INSTRUMENTATION = "";
    private final static int NUM_OF_RECORD_TYPES = 3;

    private final Filterable<ExportFormatField> m_fieldsHelper;
    private final Filterable<ExportFormatDefinition> m_exsmsDefaultFormatsHelper;
    private final Filterable<ExportFormatDefinition> m_exsmsFormatsHelper;
    private OnsisKeysStructure m_keysStructure;
    private final Filterable<ExportFormatField> m_linkFieldsHelper;
    private final Filterable<ExportFormatDefinition> m_onsisFormatsHelper;
    private String m_rootName;

    /**
     * The Enum RECORD_TYPES_ORDER.
     */
    private enum RECORD_TYPES_ORDER {
        H1, H2, D;
    }

    /**
     * Instantiates a new exsms elements helper.
     *
     * @param broker X2Broker
     * @param formats List<ExportFormatDefinition>
     */
    public ExsmsElementsHelper(X2Broker broker, List<ExportFormatDefinition> formats) {
        Filterable<ExportFormatDefinition> formatsHelper = FilterableFactory.createFilterable(formats);

        m_exsmsFormatsHelper = FilterableFactory
                .createFilterable(formatsHelper.filter(new Filter<ExportFormatDefinition>() {
                    @Override
                    public boolean isFiltered(ExportFormatDefinition toFilter) {
                        return toFilter.getProcedureId() != null
                                && toFilter.getProcedureId().startsWith(OnsisConstants.FORMAT_PREFIX_EXSMS);
                    }
                }).extract());

        m_exsmsDefaultFormatsHelper = FilterableFactory
                .createFilterable(m_exsmsFormatsHelper.filter(new Filter<ExportFormatDefinition>() {
                    @Override
                    public boolean isFiltered(ExportFormatDefinition toFilter) {
                        return toFilter.getProcedureId() != null
                                && toFilter.getProcedureId().startsWith(OnsisExtractHelper.FORMAT_ID_DEFAULT);
                    }
                }).extract());

        m_onsisFormatsHelper = FilterableFactory
                .createFilterable(formatsHelper.filter(new Filter<ExportFormatDefinition>() {
                    @Override
                    public boolean isFiltered(ExportFormatDefinition toFilter) {
                        return toFilter.getProcedureId() != null
                                && toFilter.getProcedureId().startsWith(OnsisConstants.FORMAT_PREFIX_ONSIS);
                    }
                }).extract());

        X2Criteria fieldsCriteria = new X2Criteria();
        fieldsCriteria.addIn(ExportFormatField.REL_DEFINITION, formatsHelper.getKeySet());
        m_fieldsHelper = FilterableFactory.create(broker, ExportFormatField.class, fieldsCriteria);

        Collection<ExportFormatField> nestedFields = new ArrayList<>();
        nestedFields.addAll(m_fieldsHelper
                .filter(ExportFormatField.COL_CALCULATION_ID, OnsisRetrieverNestedExport.CALC_ID).extract());
        nestedFields.addAll(m_fieldsHelper
                .filter(ExportFormatField.COL_CALCULATION_ID, OnsisRetrieverNestedExportStreaming.CALC_ID).extract());

        m_linkFieldsHelper = OnsisHelpersContainer.createFilterable(nestedFields);
    }

    /**
     * The Class OnsisKeysStructure.
     */
    private class OnsisKeysStructure {
        private final LinkedHashMap<String, String> m_keyElementNames;
        private Map<List<String>, LinkedHashMap<String, String>> m_keysByPaths;
        private final String m_name;
        private final Map<String, OnsisKeysStructure> m_nestedStructures;

        /**
         * Instantiates a new onsis keys structure.
         *
         * @param name String
         * @param keyElementNames List<String>
         * @param nestedStructures Map<String,OnsisKeysStructure>
         */
        public OnsisKeysStructure(String name, LinkedHashMap<String, String> keyElementNames,
                Map<String, OnsisKeysStructure> nestedStructures) {
            m_name = name;
            m_keyElementNames = keyElementNames;
            m_nestedStructures = nestedStructures;
        }

        /**
         * Gets the keys by path.
         *
         * @param path List<String>
         * @return List
         */
        public LinkedHashMap<String, String> getKeySifPathsByPath(List<String> path) {
            return getKeysByPaths().get(path);
        }

        /**
         * Gets the keys by paths.
         *
         * @return Map
         */
        public Map<List<String>, LinkedHashMap<String, String>> getKeysByPaths() {
            if (isRoot()) {
                if (m_keysByPaths == null) {
                    m_keysByPaths = getNestedKeySifPathsByPaths();
                }
                return m_keysByPaths;
            }
            return getNestedKeySifPathsByPaths();
        }

        /**
         * Gets the nested keys by paths.
         *
         * @return Map
         */
        private Map<List<String>, LinkedHashMap<String, String>> getNestedKeySifPathsByPaths() {
            Map<List<String>, LinkedHashMap<String, String>> keysByPaths = new HashMap<>();
            keysByPaths.put(Arrays.asList(m_name), new LinkedHashMap<String, String>(m_keyElementNames));
            for (OnsisKeysStructure nestedStructure : m_nestedStructures.values()) {
                Map<List<String>, LinkedHashMap<String, String>> nestedKeysByPaths = nestedStructure.getKeysByPaths();
                for (Entry<List<String>, LinkedHashMap<String, String>> nestedKeysByPath : nestedKeysByPaths
                        .entrySet()) {
                    List<String> path = new ArrayList(nestedKeysByPath.getKey());
                    LinkedHashMap<String, String> keys = new LinkedHashMap(nestedKeysByPath.getValue());
                    path.add(m_name);
                    for (Entry<String, String> keyElementName : m_keyElementNames.entrySet()) {
                        if (!keys.keySet().contains(keyElementName.getKey())) {
                            keys.put(keyElementName.getKey(), keyElementName.getValue());
                        }
                    }
                    keysByPaths.put(path, keys);
                }
            }
            return keysByPaths;
        }

        /**
         * Checks if is root.
         *
         * @return true, if is root
         */
        private boolean isRoot() {
            return m_rootName.equals(m_name);
        }
    }

    /**
     * Gets the keys by path.
     *
     * @param node
     *
     * @return List
     */
    public LinkedHashMap<String, String> getKeysByNode(Node node) {
        String fullPath = OnsisResultsHelper.getFullPathOfNode(node);
        return getKeysByFullPath(fullPath);
    }

    public LinkedHashMap<String, String> getKeysByFullPath(String fullPath) {
        List<String> pathForKeySifPaths = ExsmsElementsHelper.getPathForKeySifPaths(fullPath);
        if (m_keysStructure == null) {
            m_rootName = pathForKeySifPaths.get(pathForKeySifPaths.size() - 1);
            m_keysStructure =
                    buildKeysStructure(m_rootName,
                            m_onsisFormatsHelper.extractFirst(ExportFormatDefinition.COL_SIF_TOPIC, m_rootName)
                                    .getOid());
        }
        return m_keysStructure.getKeySifPathsByPath(pathForKeySifPaths);
    }

    /**
     * Gets the keys by path.
     *
     * @param path List<String>
     * @return List
     */
    @Override
    public LinkedHashMap<String, String> getKeysByPath(List<String> path) {
        if (m_keysStructure == null) {
            m_rootName = path.get(path.size() - 1);
            m_keysStructure =
                    buildKeysStructure(m_rootName,
                            m_onsisFormatsHelper.extractFirst(ExportFormatDefinition.COL_SIF_TOPIC, m_rootName)
                                    .getOid());
        }
        return m_keysStructure.getKeySifPathsByPath(path);
    }

    /**
     * Gets the root name.
     *
     * @return String
     */
    @Override
    public String getRootName() {
        return m_rootName;
    }

    public static List<String> getPathForKeySifPaths(String fullPath) {
        List<String> fullPathElements = Arrays.asList(fullPath
                .split(OnsisResultsHelper.ELEMENTS_DELIMITER));
        Collections.reverse(fullPathElements);
        ArrayList<String> pathFromRoot = new ArrayList<>();
        String rootName = null;
        for (int i = 1; i <= fullPathElements.size(); i++) {
            String element = fullPathElements.get(fullPathElements.size() - i);
            if (OnsisResultsHelper.s_submissionsTypes.contains(element)) {
                rootName = element;
            }
            if (rootName != null) {
                pathFromRoot.add(0, element);
            }
        }
        return pathFromRoot;
    }

    /**
     * Builds the keys structure.
     *
     * @param name String
     * @param formatOid String
     * @return OnsisKeysStructure
     */
    private OnsisKeysStructure buildKeysStructure(String name, String formatOid) {
        LinkedHashMap<String, String> keyElementNames = getKeyElementNames(formatOid);
        Collection<ExportFormatField> nestedFields = getNestedFields(formatOid);
        Map<String, OnsisKeysStructure> nestedStructures = new HashMap<>();
        for (ExportFormatField nestedField : nestedFields) {
            String nestedElementName = nestedField.getSifPath();
            String topic = OnsisExtractHelper.extractTopic(nestedField);
            ExportFormatDefinition format =
                    m_onsisFormatsHelper.extractFirst(ExportFormatDefinition.COL_SIF_TOPIC, topic);
            if (format == null) {
                throw new RuntimeException(String.format(
                        "Export format definition id=%s is not found for topic=%s",
                        nestedField.getDefinition().getId(), topic));
            }
            String nestedFormatOid = format.getOid();
            nestedStructures.put(nestedElementName, buildKeysStructure(nestedElementName, nestedFormatOid));
        }

        return new OnsisKeysStructure(name, keyElementNames, nestedStructures);
    }

    /**
     * Gets the key element names.
     *
     * @param definitionOid String
     * @return List
     */
    private LinkedHashMap<String, String> getKeyElementNames(String definitionOid) {
        ExportFormatDefinition onsisFormat = m_onsisFormatsHelper.extractFirst(X2BaseBean.COL_OID, definitionOid);
        List<ExportFormatDefinition> formats = null;
        if (StringUtils.isEmpty(onsisFormat.getSifProfile())) {
            formats = new ArrayList(Arrays.asList(onsisFormat));
            for (final String defaultRecordType : OnsisExtractHelper.s_defaultFormatsRecordTypes) {
                Filter<ExportFormatDefinition> defaultRecordTypeFilter = new Filter<ExportFormatDefinition>() {
                    @Override
                    public boolean isFiltered(ExportFormatDefinition toFilter) {
                        return toFilter.getProcedureId().endsWith(defaultRecordType);
                    }
                };
                formats.add(m_exsmsDefaultFormatsHelper.extractFirst(defaultRecordTypeFilter));
            }
        } else {
            List<ExportFormatDefinition> exsmsFormats = new ArrayList<>();
            Filterable<ExportFormatDefinition> exsmsExtractTypeFormats =
                    OnsisHelpersContainer
                            .createFilterable(
                                    m_exsmsFormatsHelper.filter(ExportFormatDefinition.COL_SIF_PROFILE,
                                            onsisFormat.getSifProfile()).extract());
            exsmsFormats.addAll(exsmsExtractTypeFormats.extract());
            if (exsmsFormats.size() < NUM_OF_RECORD_TYPES) {
                for (final String defaultRecordType : OnsisExtractHelper.s_defaultFormatsRecordTypes) {
                    Filter<ExportFormatDefinition> defaultRecordTypeFilter = new Filter<ExportFormatDefinition>() {
                        @Override
                        public boolean isFiltered(ExportFormatDefinition toFilter) {
                            return toFilter.getProcedureId().endsWith(defaultRecordType);
                        }
                    };
                    boolean doesRecordTypeExist = exsmsExtractTypeFormats.any(defaultRecordTypeFilter);
                    if (!doesRecordTypeExist) {
                        exsmsFormats.add(
                                m_exsmsDefaultFormatsHelper
                                        .extractFirst(defaultRecordTypeFilter));
                    }
                }
            }
            formats = exsmsFormats;
        }

        Collections.sort(formats, new Comparator<ExportFormatDefinition>() {
            @Override
            public int compare(ExportFormatDefinition o1, ExportFormatDefinition o2) {
                int typeComparisonResult = Integer.valueOf(getOrderById(o1.getProcedureId()))
                        .compareTo(Integer.valueOf(getOrderById(o2.getProcedureId())));
                return typeComparisonResult != 0 ? typeComparisonResult : o1.getName().compareTo(o2.getName());
            }
        });

        List<ExportFormatField> allFormatsKeyFields = new ArrayList<>();
        for (ExportFormatDefinition format : formats) {
            List<ExportFormatField> keyFields = new ArrayList<>(
                    m_fieldsHelper.filter(
                            Arrays.asList(ExportFormatField.COL_DEFINITION_OID, ExportFormatField.COL_KEY_IND),
                            Arrays.asList(format.getOid(), true)).extract());

            Collections.sort(keyFields, new Comparator<ExportFormatField>() {
                @Override
                public int compare(ExportFormatField o1, ExportFormatField o2) {
                    return Integer.valueOf(o1.getPosition()).compareTo(Integer.valueOf(o2.getPosition()));
                }
            });

            allFormatsKeyFields.addAll(keyFields);
        }

        LinkedHashMap<String, String> keyElementSifPaths = new LinkedHashMap<>();
        for (ExportFormatField keyField : allFormatsKeyFields) {
            Collection<ExportFormatField> fieldsByName =
                    m_fieldsHelper.filter(ExportFormatField.COL_NAME, keyField.getName()).extract();
            for (ExportFormatField fieldByName : fieldsByName) {
                if (!StringUtils.isEmpty(fieldByName.getSifPath())
                        && !keyElementSifPaths.keySet().contains(fieldByName.getSifPath())) {
                    keyElementSifPaths.put(fieldByName.getSifPath(), fieldByName.getId());
                    break;
                }
            }
        }

        return keyElementSifPaths;
    }

    /**
     * Gets the nested fields.
     *
     * @param definitionOid String
     * @return List
     */
    private Collection<ExportFormatField> getNestedFields(String definitionOid) {
        return m_linkFieldsHelper.filter(ExportFormatField.COL_DEFINITION_OID, definitionOid).extract();
    }

    /**
     * Gets the order by id.
     *
     * @param procedureId String
     * @return int
     */
    private int getOrderById(String procedureId) {
        for (RECORD_TYPES_ORDER recordTypeOrder : RECORD_TYPES_ORDER.values()) {
            if (procedureId.contains(recordTypeOrder.name())) {
                return recordTypeOrder.ordinal();
            }
        }
        return RECORD_TYPES_ORDER.values().length + 1;
    }
}
