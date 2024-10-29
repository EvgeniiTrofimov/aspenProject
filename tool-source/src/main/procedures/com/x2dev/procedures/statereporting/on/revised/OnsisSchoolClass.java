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

import com.follett.fsc.core.framework.persistence.CollectionCriteriaHelper;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.business.X2Broker;
import com.x2dev.procedures.statereporting.common.FilterableFactory;
import com.x2dev.procedures.statereporting.common.FilterableFactory.Filterable;
import com.x2dev.procedures.statereporting.common.ToolBean;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolScheduleClass;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolScheduleTermDate;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolSection;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldRetriever;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.Range;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportData;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnSchool;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnSection;
import com.x2dev.procedures.statereporting.on.revised.OnsisExtractHelper.OnsisExtractRecords;
import com.x2dev.procedures.statereporting.on.revised.OnsisExtractHelper.OnsisExtractRecords.OnsisCsvDataRecord;
import com.x2dev.procedures.statereporting.on.revised.OnsisSchoolData.OnsisSchoolEntity;
import com.x2dev.sis.model.beans.path.SisBeanPaths;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;
import org.w3c.dom.Element;

/**
 * The Class OnsisSchoolClass.
 *
 * @author Follett Software Company
 * @copyright 2021
 */
public class OnsisSchoolClass extends OnsisStateReportData {

    /**
     * The Class ClassWrapper.
     */
    public static class ClassWrapper {
        public static final String LANGUAGE_TYPE_BOTH = "B";

        private final ToolBean classEntity;
        private final OnSection firstSection;
        private final GlobalData globalData;

        /**
         * Instantiates a new class wrapper.
         *
         * @param globalData GlobalData
         * @param classEntity X2BaseBean
         * @param firstSection MasterSchedule
         */
        public ClassWrapper(GlobalData globalData, ToolBean classEntity, OnSection firstSection) {
            this.classEntity = Objects.requireNonNull(classEntity);
            this.globalData = globalData;
            this.firstSection = firstSection;
        }

        /**
         * Gets the class code.
         *
         * @return String
         */
        public String getClassCode() {
            X2Broker broker = globalData.getBroker();
            String classCode = null;
            if (classEntity instanceof ToolScheduleClass) {
                classCode = ((ToolScheduleClass) classEntity).getId();
            } else if (classEntity instanceof OnSection) {
                OnSection mst = ((OnSection) classEntity);
                classCode = mst.getClassCode(broker);
            } else {
                if (classEntity != null) {
                    classCode = classEntity.toString();
                }
            }

            if (classCode != null) {
                classCode = OnsisStateReportData.fixupClassCode(globalData, classCode, globalData.getBroker());
            }

            return classCode;
        }

        /**
         * Gets the class entity.
         *
         * @return X 2 base bean
         */
        public ToolBean getClassEntity() {
            return classEntity;
        }


        /**
         * Gets the first section.
         *
         * @return Master schedule
         */
        public OnSection getFirstSection() {
            return firstSection;
        }

        /**
         * Gets the language of instruction.
         *
         * @return String
         */
        public String getLanguageOfInstruction() {
            if (classEntity instanceof ToolScheduleClass) {
                ToolScheduleClass scheduleClass = (ToolScheduleClass) classEntity;
                if (scheduleClass.getSections(globalData.getBroker()).stream()
                        .map(section -> (OnSection) section)
                        .collect(Collectors.groupingBy(OnSection::getLanguageOfInstruction)).values().size() > 1) {
                    return LANGUAGE_TYPE_BOTH;
                }
            }

            X2Broker broker = globalData.getBroker();
            String languageType = firstSection.getLanguageOfInstruction();
            return !StringUtils.isEmpty(languageType) ? languageType
                    : ((OnSchool) firstSection.getSchedule(broker).getSchool(broker)).getLanguageType();
        }

    }

    /**
     * The Class OnsisSchoolClassEntity.
     */
    public static class OnsisSchoolClassEntity extends OnsisStateReportEntity {
        private static final String CLASS_TYPE_FALLBACK = "R";

        private X2Broker m_broker;
        private ClassWrapper m_classEntitiy = null;
        private OnsisCsvDataRecord m_csvRecord = null;
        private GlobalData m_globalData;
        private OnsisSchoolClass m_reportData;

        /*
         * In the case of a ScheduleClass, save the ScheduleClass as the entity bean
         */
        private ToolScheduleClass m_scheduleClass = null;

        /**
         * Instantiates a new onsis school class entity.
         */
        public OnsisSchoolClassEntity() {
            // public no argument constructor for dynamic instantiation.
        }


        /**
         * Gets the bean.
         *
         * @return X 2 base bean
         * @see com.follett.fsc.aspensif.framework.OnsisStateReportData.OnsisStateReportEntity#getEntityBean()
         */
        @Override
        public ToolBean getBean() {
            return m_scheduleClass == null ? super.getBean() : m_scheduleClass;
        }

        /**
         * Gets the class entity.
         *
         * @return Class wrapper
         */
        public ClassWrapper getClassEntity() {
            if (m_classEntitiy == null) {
                m_classEntitiy = new ClassWrapper(getGlobalData(), getBean(), getFirstSection());
            }
            return m_classEntitiy;
        }

        /**
         * Gets the class type.
         *
         * @return String
         */
        public String getClassType() {
            if (getBean() instanceof ToolScheduleClass) {
                List<OnSection> sections = m_reportData.getSections(getBean().getOid());
                String classType = null;
                for (OnSection section : sections) {
                    String currentClassType = section.getClassType();
                    if (!StringUtils.isEmpty(currentClassType)) {
                        if (classType == null) {
                            classType = currentClassType;
                        } else if (!classType.equals(currentClassType)) {
                            throw new RuntimeException("At least two different class types exist for class "
                                    + ((ToolScheduleClass) getBean()).getId() + " including " + currentClassType
                                    + " and "
                                    + classType);
                        }
                    }
                }
            }

            String classType = getFirstSection().getClassType();

            if (!StringUtils.isEmpty(classType)) {
                return classType;
            }

            return CLASS_TYPE_FALLBACK;
        }

        /**
         * Gets the end date.
         *
         * @return Plain date
         */
        public PlainDate getEndDate() {
            if (getBean() instanceof ToolScheduleClass) {
                List<OnSection> sections = m_reportData.getSections(getBean().getOid());
                return sections.stream()
                        .flatMap(section -> section.getScheduleTerms(m_broker).stream())
                        .filter(Objects::nonNull)
                        .map(trm -> trm.getEndDate(m_broker))
                        .filter(Objects::nonNull)
                        .max(Comparator.naturalOrder()).orElse(null);
            }
            OnSection currentSection = (OnSection) getBean();
            return currentSection.getTermEndDate(m_broker);
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.x2dev.sis.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            return getId();
        }

        /**
         * Return the section bean backing this entity row,
         * or if this entity row is backed by a ScheduleClass,
         * return the first child section.
         *
         * @return Master schedule
         */
        public OnSection getFirstSection() {
            OnSection section = null;
            if (getBean() instanceof ToolScheduleClass) {
                section = (OnSection) ((ToolScheduleClass) getBean()).getPrimarySection(m_broker);
                if (section == null) {
                    section = m_reportData.m_reportedClasses.get(getBean().getOid());
                }
            } else {
                section = (OnSection) getBean();
            }
            return section;
        }

        /**
         * Return the ID for this <CLASS>.
         *
         * @return String
         */
        public String getId() {
            String classCode = null;
            if (getBean() instanceof ToolScheduleClass) {
                classCode = ((ToolScheduleClass) getBean()).getId();
            } else if (getBean() instanceof ToolSection) {
                classCode = ((ToolSection) getBean()).getCourseView();
            } else {
                classCode = "" + getBean();
            }
            return OnsisStateReportData.fixupClassCode(getGlobalData(), classCode, getReportData().getBroker());
        }

        /**
         * Gets the start date.
         *
         * @return Plain date
         */
        public PlainDate getStartDate() {
            if (m_csvRecord != null) {
                return getCsvDate(m_csvRecord, OnsisExtractHelper.CsvField.CLASS_START_DATE);
            }
            if (getBean() instanceof ToolScheduleClass) {
                List<OnSection> sections = m_reportData.getSections(getBean().getOid());
                PlainDate earliestDate = null;
                for (OnSection section : sections) {
                    PlainDate startDate = section.getStartDate(m_broker, m_globalData.getDateRange());
                    if (earliestDate == null || startDate.before(earliestDate)) {
                        earliestDate = startDate;
                    }
                }
                return earliestDate;
            }
            ToolSection section = (ToolSection) getBean();
            return section.getStartDate(m_broker, m_globalData.getDateRange());
        }

        /**
         * OnsisSchoolClass produces one <CLASS> entity row per MasterSchedule (section) bean,
         * UNLESS the section is assigned to a ScheduleClass. In that case, produce one <CLASS>
         * for the ScheduleClass containing one <SEGMENT> for each related section.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see com.follett.fsc.aspensif.framework.OnsisStateReportData.OnsisStateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, ToolBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            m_reportData = (OnsisSchoolClass) data;
            m_globalData = m_reportData.getGlobalData();
            m_broker = m_globalData.getBroker();

            OnSection section = (OnSection) bean;

            // Consolidate related sections
            String scheduleClassOid = section.getSectionClassOid();
            if (scheduleClassOid != null) {
                ToolScheduleClass scheduleClass = section.getSectionClass(m_broker);
                if (scheduleClass != null) {
                    if (StringUtils.isBlank(scheduleClass.getPrimarySectionOid())
                            || !m_reportData.m_includedSectionOids.contains(scheduleClass.getPrimarySectionOid())) {
                        if (!m_reportData.m_reportedClasses.keySet().contains(scheduleClassOid)) {
                            m_scheduleClass = section.getSectionClass(m_broker);
                            m_reportData.m_reportedClasses.put(scheduleClassOid, section);
                        }
                        //
                    } else if (scheduleClass.getPrimarySectionOid().equals(section.getOid())) {
                        m_scheduleClass = scheduleClass;
                    }
                }
                if (m_scheduleClass == null) {
                    setRowCount(0);
                    return;
                }
            }

            boolean startDateOk = false;
            PlainDate startDate = null;
            Collection<ToolScheduleTermDate> termDates = section.getScheduleTermDates(m_broker);
            if (termDates != null && termDates.size() > 0) {
                for (ToolScheduleTermDate termDate : termDates) {
                    startDate = termDate.getStartDate();
                    if (startDate != null) {
                        if (!startDate.after(m_reportData.getGlobalData().getEndDate())) {
                            startDateOk = true;
                            break;
                        }
                    }
                }
            }

            if (!startDateOk) {
                setRowCount(0);
                return;
            }


            OnsisExtractRecords matcher = getReportData().getGlobalData().getExtractHelper()
                    .getMatcherByExtractType(OnsisStateReportData.EXTRACT_TYPE_SCHOOL_CLASS);
            String classCode = getFieldValue(FIELD_CLASS_CODE);
            m_csvRecord = matcher == null ? null
                    : matcher.findRecord(
                            Arrays.asList(
                                    OnsisExtractHelper.CsvField.SCHOOL_NUMBER.toString(),
                                    OnsisExtractHelper.CsvField.CLASS_CODE.toString()),
                            Arrays.asList(
                                    deepGetFieldValueByFieldName(OnsisExtractHelper.CsvField.SCHOOL_NUMBER.toString()),
                                    classCode));

            // row count defaults to 1
        }

    }

    private static final String FIELD_CLASS_CODE = "Class Code";

    protected Set<String> m_includedSectionOids;
    protected Map<String, OnSection> m_reportedClasses;
    protected Filterable<OnSection> m_sectionsFilterable;

    /**
     * Builds the beans.
     *
     * @throws X2BaseException exception
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#buildBeans()
     */
    @Override
    public void buildBeans() throws X2BaseException {
        StringBuilder debugDetail = new StringBuilder();
        Range<Date> dateRange = getGlobalData().getDateRange();
        Collection<OnSection> sections = getGlobalData().getSchoolType()
                .getSchoolClasses(ToolBean.getCachedToolBeans(OnSection.class)).stream()
                .filter(section -> {
                    boolean result = section.isStudentEnrolled(getBroker(), dateRange, true);
                    debugDetail.append("isStudentEnrolled: " + result + " Section: " + section + "\n");
                    return result;
                }).collect(Collectors.toList());


        m_includedSectionOids = sections.stream().map(section -> section.getOid()).collect(Collectors.toSet());
        setBeans(sections);

        // Create sections filterable for these sections
        m_sectionsFilterable = FilterableFactory.createFilterableToolBeans(sections);

        // preload ScheduleClass
        Set<String> scheduleClassOids =
                sections.stream().map(section -> section.getSectionClassOid()).collect(Collectors.toSet());
        X2Criteria classCcriteria = new X2Criteria();
        CollectionCriteriaHelper helper = null;
        try {
            if (scheduleClassOids.size() > ToolBean.MAX_SAFE_PARAMETERS) {
                helper = new CollectionCriteriaHelper(scheduleClassOids, getGlobalData().getBroker());
                helper.applyToCriteria(SisBeanPaths.SCHEDULE_CLASS.oid().getPath(), classCcriteria);
            } else {
                classCcriteria.addIn(SisBeanPaths.SCHEDULE_CLASS.oid().getPath(), scheduleClassOids);
            }
            FilterableFactory.create(getBroker(), getDictionaryExtractor(), ToolScheduleClass.class, classCcriteria,
                    null);
        } finally {
            if (helper != null) {
                helper.cleanup();
            }
        }
        if (getGlobalData().getDebugDetail()) {
            log(debugDetail.toString());
        }
    }

    /**
     * Gets the calcs.
     *
     * @return Map
     * @see com.follett.fsc.aspensif.framework.SifStateReportData#getCalcs()
     */
    @Override
    public Map<String, FieldRetriever> getCalcs() {
        Map<String, FieldRetriever> calcs = super.getCalcs();

        calcs.put(OnsisRetrieverAction.CALC_ID, new OnsisRetrieverAction());

        return calcs;
    }

    /**
     * Gets the sections.
     *
     * @param classOid String
     * @return List
     */
    public List<OnSection> getSections(String classOid) {
        return m_sectionsFilterable.getGroup(ToolSection.FIELD_SECTION_CLASS_OID, classOid);
    }

    /**
     * Initialize data source from parent.
     *
     * @param parentData OnsisStateReportData
     * @param parentEntity OnsisStateReportEntity
     * @throws X2BaseException exception
     * @see com.follett.fsc.aspensif.framework.OnsisStateReportData#initializeDataSourceFromParent(com.follett.fsc.aspensif.framework.OnsisStateReportData,
     *      com.follett.fsc.aspensif.framework.OnsisStateReportData.OnsisStateReportEntity,
     *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
     */
    @Override
    public void initializeDataSourceFromParent(OnsisStateReportData parentData,
                                               OnsisStateReportEntity parentEntity)
            throws X2BaseException {
        super.initializeDataSourceFromParent(parentData, parentEntity);

        m_reportedClasses = new HashMap();

        if (!(parentEntity instanceof OnsisSchoolEntity)) {
            throw new RuntimeException("Unsupported parent export " + parentEntity.getClass().getSimpleName());
        }

    }

    /**
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#generateAndAppendDelete(com.x2dev.procedures.statereporting.on.OnsisExtractHelper.OnsisExtractRecords.OnsisCsvDataRecord,
     *      java.util.List, java.util.List, org.w3c.dom.Element)
     */
    @Override
    protected Element generateAndAppendDelete(OnsisCsvDataRecord record,
                                              List<String> currentEntityKeySet,
                                              List<String> currentEntityValueSet,
                                              Element parentElement) {
        /*
         * 2020-11-03 Never send DELETE for a Class
         */
        return null;
    }

    /**
     * Initialize entity class.
     *
     * @see com.follett.fsc.aspensif.framework.PluginStateReportData#initializeEntityClass()
     */
    @Override
    protected void initializeEntityClass() {
        setEntityClass(OnsisSchoolClassEntity.class);
    }

}
