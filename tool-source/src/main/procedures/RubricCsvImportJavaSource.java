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
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.tools.imports.TextImportJavaSource;
import com.x2dev.sis.model.beans.RubricCriterion;
import com.x2dev.sis.model.beans.RubricDefinition;
import com.x2dev.sis.model.beans.RubricRatingScale;
import com.x2dev.sis.model.beans.RubricRatingScalePoints;
import com.x2dev.utils.StringUtils;
import java.io.File;
import java.math.BigDecimal;
import java.util.List;
import java.util.logging.Level;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Rubrics import.
 */
public class RubricCsvImportJavaSource extends TextImportJavaSource {
    private static final long serialVersionUID = 1L;

    private int m_indexCriterion = 0;
    private int m_indexStrand = 0;
    private int m_indexTopic = 0;
    private RubricCriterion m_rubricCriterionStrand = null;
    private RubricCriterion m_rubricCriterionTopic = null;
    private RubricDefinition m_rubricDefinition = null;
    private RubricRatingScale m_rubricRatingScale = null;

    /**
     * Default constructor.
     */
    public RubricCsvImportJavaSource() {
        super();

        setStartingLine(2);
        setValueWrappingMode(VALUE_WRAPPING_MODE.OPTIONAL);
    }

    /**
     * Gets the field count.
     *
     * @return int
     * @see com.follett.fsc.core.k12.tools.imports.TextImportJavaSource.getFieldCount()
     */
    @Override
    protected int getFieldCount() {
        return 8;
    }

    /**
     * Import data.
     *
     * @param sourceFile File
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.imports.TextImportJavaSource.importData(File sourceFile)
     *      throws Exception
     */
    @Override
    protected void importData(File sourceFile) throws Exception {
        getBroker().beginTransaction();
        try {
            setup();

            super.importData(sourceFile);
        } catch (Exception e) {
            logToolMessage(Level.SEVERE, e.getLocalizedMessage(), false);

            getBroker().rollbackTransaction();
        } finally {
            if (getBroker().isInTransaction()) {
                getBroker().commitTransaction();
            }
        }
    }

    /**
     * Import record.
     *
     * @param record List<String>
     * @param lineNumber int
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.imports.TextImportJavaSource.importRecord(java.util.List,
     *      int)
     */
    @Override
    protected void importRecord(List<String> record, int lineNumber) throws Exception {
        @SuppressWarnings("unused")
        String dbID = record.get(0);
        String code = record.get(1);
        String cluster = record.get(2);
        String framework = record.get(3);
        String strand = record.get(4);
        String topic = record.get(5);
        String text = record.get(6);
        @SuppressWarnings("unused")
        String grades = record.get(7);

        String[] codeTerms = code.split("\\.");

        // Unique IDs
        String definitionId = codeTerms[0] + "." + codeTerms[1];
        String strandId = codeTerms[0] + "." + codeTerms[1] + "." + codeTerms[2];
        String topicId = codeTerms[0] + "." + codeTerms[1] + "." + codeTerms[2] + "." + codeTerms[3];
        String criterionId = code;

        /*
         * Create the definition if it doesn't exist or if the current line has a new definition ID.
         * Assuming import file is ordered by cluster, definition
         */
        if (m_rubricDefinition == null || !m_rubricDefinition.getName().startsWith(definitionId + " - ")) {
            // Reset all the trackers
            m_indexCriterion = 0;
            m_indexStrand = 0;
            m_indexTopic = 0;
            m_rubricCriterionStrand = null;
            m_rubricCriterionTopic = null;

            String description = "ID : " + definitionId + System.lineSeparator() +
                    "Cluster : " + cluster + System.lineSeparator() +
                    "Framework : " + framework;
            String name = definitionId + " - " + framework;
            String reportDisplay = definitionId + " - " + framework;

            createDefinition(description, definitionId, name, reportDisplay);
        }

        /*
         * Create a Strand criterion if it doesn't exist or if the current line has a new Strand ID.
         * 
         * Assuming import file is ordered by Cluster, Definition, Strand
         */
        if (m_rubricCriterionStrand == null || !m_rubricCriterionStrand.getName().startsWith(strandId + " - ")) {
            // Reset the trackers below the Strand level
            m_indexCriterion = 0;
            m_indexTopic = 0;
            m_rubricCriterionTopic = null;

            String columnHeader = codeTerms[1] + "." + codeTerms[2];
            String comment = "Strand Criterion Level" + System.lineSeparator() +
                    "----------------------------------------------------------------" + System.lineSeparator() +
                    "ID : " + strandId + System.lineSeparator() +
                    "Strand : " + strand;
            String description = strandId + " - " + strand;
            String name = strandId + " - " + strand;

            m_rubricCriterionStrand = createCriterion(columnHeader, comment, description, name, m_indexStrand++, null);
        }

        /*
         * Create a Topic criterion if it doesn't exist or if the current line has a new Topic ID.
         * 
         * Assuming import file is ordered by Cluster, Definition, Strand, Topic
         */
        if (m_rubricCriterionTopic == null || !m_rubricCriterionTopic.getName().startsWith(topicId + " - ")) {
            // Reset the trackers below the Topic level
            m_indexCriterion = 0;

            String columnHeader = codeTerms[2] + "." + codeTerms[3];
            String comment = "Topic Criterion Level" + System.lineSeparator() +
                    "----------------------------------------------------------------" + System.lineSeparator() +
                    "ID : " + topicId + System.lineSeparator() +
                    "Strand : " + strand + System.lineSeparator() +
                    "Topic : " + topic;
            String description = topicId + " - " + topic;
            String name = topicId + " - " + topic;

            m_rubricCriterionTopic =
                    createCriterion(columnHeader, comment, description, name, m_indexTopic++, m_rubricCriterionStrand);
        }

        /*
         * Create a Text criterion.
         * 
         * Assuming import file is flat, each line should be a leaf criterion
         */
        String columnHeader = criterionId.substring(definitionId.length() + 1).replace(".", ""); // Take
                                                                                                 // the
                                                                                                 // remainder
                                                                                                 // after
                                                                                                 // definition
                                                                                                 // ID
        String comment = "Text Criterion Level" + System.lineSeparator() +
                "----------------------------------------------------------------" + System.lineSeparator() +
                "ID : " + criterionId + System.lineSeparator() +
                "Strand : " + strand + System.lineSeparator() +
                "Topic : " + topic + System.lineSeparator() +
                "Text : " + text;
        String description = criterionId + " - " + text;
        String name = criterionId + " - " + text;

        createCriterion(columnHeader, comment, description, name, m_indexCriterion++, m_rubricCriterionTopic);
    }

    /**
     * Creates and returns the RubricCriterion object based on passed values.
     *
     * @param columnHeader String
     * @param comment String
     * @param description String
     * @param name String
     * @param sequenceNumber int
     * @param parentRubricCriterion RubricCriterion
     * @return RubricCriterion
     */
    private RubricCriterion createCriterion(String columnHeader,
                                            String comment,
                                            String description,
                                            String name,
                                            int sequenceNumber,
                                            RubricCriterion parentRubricCriterion) {
        RubricCriterion criterion = X2BaseBean.newInstance(RubricCriterion.class, getBroker().getPersistenceKey());

        String parentRubicCriterionOid = parentRubricCriterion != null ? parentRubricCriterion.getOid() : null;

        criterion.setColumnHeader(StringUtils.substring(columnHeader, 10));
        criterion.setComment(comment);
        criterion.setCriteriaWeight(new BigDecimal(1));
        criterion.setDescription(StringUtils.substring(description, 250));
        criterion.setDiscontinuedIndicator(false);
        criterion.setLevel(0); // Calculated on bean save
        criterion.setLevelCount(0); // Calculated on bean save
        criterion.setName(StringUtils.substring(name, 60));
        criterion.setPath(null); // Calculated on bean save
        criterion.setReportDisplay(StringUtils.substring(name, 100));
        criterion.setReportType(0);
        criterion.setRubricCriterionOid(parentRubicCriterionOid);
        criterion.setRubricDefinitionOid(m_rubricDefinition.getOid());
        criterion.setRubricRatingScaleOid(m_rubricRatingScale.getOid());
        criterion.setSequenceNumber(sequenceNumber);
        criterion.setTermMap(null); // Only needed when RubricDefinition TermRestrictionsIndicator
                                    // is true

        getBroker().saveBean(criterion);

        incrementInsertCount(); // Update total RubricCriterion inserted

        return criterion;
    }

    /**
     * Creates and returns the RubricDefinition object based on passed values.
     *
     * @param description String
     * @param id String
     * @param name String
     * @param reportDisplay String
     */
    private void createDefinition(String description, String id, String name, String reportDisplay) {
        m_rubricDefinition = X2BaseBean.newInstance(RubricDefinition.class, getBroker().getPersistenceKey());

        m_rubricDefinition.setCalculationOverrideIndicator(false);
        m_rubricDefinition.setCriteriaCount(0); // Calculated on RubricCriterion bean save
        m_rubricDefinition.setDecimals(0);
        m_rubricDefinition.setDefinition(null);
        m_rubricDefinition.setDescription(StringUtils.substring(description, 250));
        m_rubricDefinition.setId(StringUtils.substring(id, 10));
        m_rubricDefinition.setLevelCount(0); // Calculated on RubricCriterion bean save
        m_rubricDefinition.setLockedIndicator(false);
        m_rubricDefinition.setMaximumPoints(new BigDecimal(0));
        m_rubricDefinition.setName(StringUtils.substring(name, 60));
        m_rubricDefinition.setNumberOfTerms(4);
        m_rubricDefinition.setMaximumPoints(BigDecimal.valueOf(5));
        OrganizationManager.setOrganizationOids(m_rubricDefinition, getOrganization().getRootOrganization());
        m_rubricDefinition.setReportDisplay(StringUtils.substring(reportDisplay, 100));
        m_rubricDefinition.setRubricRatingScaleOid(m_rubricRatingScale.getOid());
        m_rubricDefinition.setShowCalculatedIndicator(false);
        m_rubricDefinition.setSource(null);
        m_rubricDefinition.setSubjectCode(null);
        m_rubricDefinition.setTermRestrictionsIndicator(false);

        getBroker().saveBeanForced(m_rubricDefinition, true, false); // Skipping events,
                                                                     // TermRestrictionIndicator not
                                                                     // used so unnecessary
    }

    /**
     * Creates and returns the RubricRatingScalePoints object for the passed values.
     *
     * @param points int
     * @param mastery boolean
     * @param name String
     * @param description String
     */
    private void createPoints(int points, boolean mastery, String name, String description) {
        RubricRatingScalePoints scalePoints =
                X2BaseBean.newInstance(RubricRatingScalePoints.class, getBroker().getPersistenceKey());

        scalePoints.setCutoff(BigDecimal.valueOf(points));
        scalePoints.setDescription(StringUtils.substring(description, 250));
        scalePoints.setId(StringUtils.substring(String.valueOf(points), 10));
        scalePoints.setMasteryIndicator(mastery);
        scalePoints.setName(StringUtils.substring(name, 60));
        scalePoints.setPoints(BigDecimal.valueOf(points));
        scalePoints.setRubricRatingScaleOid(m_rubricRatingScale.getOid());
        scalePoints.setSequenceNumber(points);

        getBroker().saveBean(scalePoints);
    }

    /**
     * Creates and returns the RubricRatingScale object.
     *
     * @param maxPoints BigDecimal
     * @param name String
     */
    private void createScale(BigDecimal maxPoints, String name) {
        m_rubricRatingScale = X2BaseBean.newInstance(RubricRatingScale.class, getBroker().getPersistenceKey());

        m_rubricRatingScale.setComment(name);
        m_rubricRatingScale.setDescription(StringUtils.substring(name, 250));
        m_rubricRatingScale.setExplicitValueOnly(false);
        m_rubricRatingScale.setMaximumPoints(maxPoints);
        m_rubricRatingScale.setName(StringUtils.substring(name, 60));
        m_rubricRatingScale.setRatingCount(0); // Calculated when RubicRatingScalePoints are saved
        m_rubricRatingScale.setSystemIndicator(false);

        getBroker().saveBean(m_rubricRatingScale);
    }

    /**
     * Handles any setup required prior to looping through the import file.
     */
    private void setup() {
        // Check if the scale already exists, mostly for dev purposes to prevent duplicate scales
        // that need to be cleaned up.
        X2Criteria rubricRatingScaleCriteria = new X2Criteria();
        rubricRatingScaleCriteria.addEqualTo(RubricRatingScale.COL_NAME, "Scooby");

        QueryByCriteria rubricRatingScaleQuery =
                new QueryByCriteria(RubricRatingScale.class, rubricRatingScaleCriteria);
        m_rubricRatingScale = (RubricRatingScale) getBroker().getBeanByQuery(rubricRatingScaleQuery);
        if (m_rubricRatingScale == null) {
            createScale(BigDecimal.valueOf(5), "Scooby");

            createPoints(0, false, "Unstarted", "No information or practice has been provided");
            createPoints(1, false, "Beginner", "Topic presented only in a theoretical setting");
            createPoints(2, false, "Needs improvement", "Demonstrated very limited ability");
            createPoints(3, false, "Needs supervision", "Demonstrates ability but needs supervision");
            createPoints(4, true, "Good", "Demonstrates ability with minimal supervision");
            createPoints(5, true, "Outstanding", "Consistently demonstrated independent performance");
        }
    }
}
