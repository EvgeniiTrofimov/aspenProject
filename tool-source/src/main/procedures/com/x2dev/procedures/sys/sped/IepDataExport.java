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
package com.x2dev.procedures.sys.sped;

import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.BeanCopier;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.ToolJavaSource;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.google.gson.Gson;
import com.x2dev.sis.model.beans.IepAmendment;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepGoal;
import com.x2dev.sis.model.beans.IepMeeting;
import com.x2dev.sis.model.beans.IepPerformanceLevel;
import com.x2dev.sis.model.beans.IepService;
import com.x2dev.sis.model.beans.IepTeamMember;
import com.x2dev.sis.model.beans.SisExtendedDataDictionary;
import com.x2dev.sis.model.business.sped.SpedUtils;
import com.x2dev.utils.CryptographyUtils;
import com.x2dev.utils.LoggerUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.io.BufferedWriter;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.nio.charset.StandardCharsets;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * @author Follett Software Company
 * @copyright 2021
 */
public class IepDataExport extends ToolJavaSource {

    private static class DataRow {
        private String alias;
        private String id;
        private Integer index;
        private String relationship;
        private String value;
        private String rcdStateCode;
    }

    private static class Relationship {
        private String primaryRel;
        private Relationship[] secondaryRel;

        public static Relationship of(String rel) {
            Relationship res = new Relationship();
            res.primaryRel = rel;
            return res;
        }

        public static Relationship of(String relPrimary,
                                      Relationship[] relSecondary) {
            Relationship res = Relationship.of(relPrimary);
            res.secondaryRel = relSecondary;
            return res;
        }
    }

    private static final String ENCRYPTION_KEY = "KviFiVfQbf5GMkw9QEOKaxCFSh8OfM4p";
    private static final String PARAM_ENCRYPT = "encrypt";
    private static final List<Relationship> RELATIONSHIPS = Arrays.asList(
            Relationship.of(IepData.REL_ACCOMMODATIONS),
            Relationship.of(IepData.REL_IEP_DISABILITY),
            Relationship.of(IepData.REL_IEP_GOALS,
                    new Relationship[] {Relationship.of(IepGoal.REL_IEP_GOAL_OBJECTIVES),
                            Relationship.of(IepGoal.REL_IEP_GOAL_PROGRESS),
                            Relationship.of(IepGoal.REL_IEP_SERVICE_GOAL_ALIGNMENTS)}),
            Relationship.of(IepData.REL_IEP_PERFORMANCE_LEVEL,
                    new Relationship[] {Relationship.of(IepPerformanceLevel.REL_IEP_PERFORMANCE_SOURCES)}),
            Relationship.of(IepData.REL_IEP_SERVICES,
                    new Relationship[] {Relationship.of(IepService.REL_IEP_SERVICE_GOAL_ALIGNMENTS)}),
            Relationship.of(IepData.REL_IEP_OTHER_SERVICES),
            Relationship.of(IepData.REL_PLACEMENTS),

            // New MA IEP uses User Defined table D for additional data.
            Relationship.of(IepData.REL_USER_DEFINED_RECORDS_D),
            
            // Remove IepMeeing and IepTeamMembers from export. 
            // Meetings are only relevant in the source district, not the destination district.
            // Relationship.of(IepData.REL_TEAM_MEMBERS,
            //         new Relationship[] {Relationship.of(IepTeamMember.REL_MEETING_ATTENDANCE)}),
            // Relationship.of(IepData.REL_MEETING_ATTENDANCE),
            // Relationship.of(IepData.REL_IEP_MEETING,
            //         new Relationship[] {Relationship.of(IepMeeting.REL_MEETING_ATTENDANCE)}),
            Relationship.of(IepData.REL_IEP_AMENDMENTS,
                    new Relationship[] {Relationship.of(IepAmendment.REL_IEP_AMENDMENT_DETAILS)}));


    private BeanCopier m_beanCopier;
    private String m_customFileName;
    private DataDictionary m_dictionary;
    private Boolean m_encrypt = null;
    private Map<Class<?>, List<DataDictionaryField>> m_fieldsMap;
    private IepData m_iep;

    /**
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#getCustomFileName()
     */
    @Override
    public String getCustomFileName() {
        if (m_customFileName == null) {
            SimpleDateFormat dateFormatter = new SimpleDateFormat("yyyyMMdd_hhmmss");
            String dateStr = dateFormatter.format(new Date());
            m_customFileName = "IepExport" + "_" + dateStr + (isEncrypt() ? ".enc" : ".txt");
        }
        return m_customFileName;
    }

    @Override
    protected void initialize() {
        m_fieldsMap = new HashMap<>();
        m_beanCopier = new BeanCopier(getBroker(), true);

        SisExtendedDataDictionary extendedDictionary = SpedUtils.getIepDictionary(getOrganization(), getBroker());

        m_dictionary =
                DataDictionary.getDistrictDictionary(extendedDictionary, getBroker().getPersistenceKey());
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.
     *      UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) {
        m_iep = userData.getCurrentRecord(IepData.class);
    }

    private String getRcdStateCode(DataDictionaryField field, String value) {
        if (field.getReferenceTable() != null && value != null) {
            return field.getReferenceTable().getReferenceCodes().stream()
                    .filter(rcd -> rcd.getCode().equals(value) && rcd.getStateCode() != null)
                    .map(ReferenceCode::getStateCode)
                    .findFirst()
                    .orElse(null);
        }
        return null;
    }

    private void exportFields(List<DataRow> grid, X2BaseBean bean, Integer index, String relationship) {
        getClassFields(bean).forEach(field -> {
            String alias = field.getAlias();
            DataRow row = new DataRow();
            Object value = StringUtils.isEmpty(alias)
                    ? bean.getFieldValueByBeanPath(field.getJavaName())
                    : bean.getFieldValueByAlias(alias, m_dictionary);
            String strValue = value == null ? null : value.toString();
            row.index = index;
            row.relationship = relationship;
            row.id = field.getId();
            row.alias = alias;
            row.value = strValue;
            row.rcdStateCode = getRcdStateCode(field, strValue);
            grid.add(row);
        });
    }

    private void exportRelationships(X2BaseBean parent,
                                     List<DataRow> grid,
                                     List<Relationship> relationships,
                                     String parentPath) {
        for (Relationship rel : relationships) {
            String path =
                    m_dictionary.findDataDictionaryRelationship(parent.getClass().getName(), rel.primaryRel).getId();
            String fullPath = parentPath == null ? path : parentPath + ModelProperty.PATH_DELIMITER + path;
            int index = 0;
            for (X2BaseBean bean : (Collection<X2BaseBean>) m_beanCopier.getRelatedCollectionForRelPath(parent, path)) {
                exportFields(grid, bean, index++, fullPath);
                if (rel.secondaryRel != null) {
                    exportRelationships(bean, grid, Arrays.asList(rel.secondaryRel), fullPath);
                }
            }
        }
    }

    private List<DataRow> gatherData() {
        if (m_iep == null) {
            throw new IllegalStateException("This export can only be used where the context include a single IEP");
        }
        List<DataRow> grid = new ArrayList<>();
        exportFields(grid, m_iep, 0, "");
        exportRelationships(m_iep, grid, RELATIONSHIPS, null);
        return grid;
    }

    private List<DataDictionaryField> getClassFields(X2BaseBean bean) {
        Class<?> cls = bean.getClass();
        if (!m_fieldsMap.containsKey(cls)) {
            m_fieldsMap.put(cls, loadClassFields(cls));
        }
        return m_fieldsMap.get(cls);
    }

    private boolean isEncrypt() {
        if (m_encrypt == null) {
            m_encrypt = (Boolean) getParameter(PARAM_ENCRYPT);
            if (m_encrypt == null) {
                m_encrypt = Boolean.TRUE;
            }
        }
        return m_encrypt.booleanValue();
    }

    /**
     * Load class fields.
     *
     * @param cls Class<?>
     * @return List
     */
    private List<DataDictionaryField> loadClassFields(Class<?> cls) {
        return m_dictionary.getFieldsForContext(cls.getName()).stream()
                .filter(DataDictionaryField::isEnabled)
                .collect(Collectors.toList());
    }

    /**
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#run()
     */
    @Override
    protected void run() throws X2BaseException {
        try {
            List<DataRow> data = gatherData();
            String content = new Gson().toJson(data);
            String encoding = StandardCharsets.UTF_8.displayName();
            Writer out =
                    new BufferedWriter(
                            new OutputStreamWriter(getResultHandler().getOutputStream(), encoding));
            out.write(isEncrypt() ? CryptographyUtils.encrypt(content, ENCRYPTION_KEY) : content);
            out.close();
        } catch (Exception ex) {
            String message = LoggerUtils.convertThrowableToString(ex);
            addCustomErrorMessage(message);
        }
    }
}
