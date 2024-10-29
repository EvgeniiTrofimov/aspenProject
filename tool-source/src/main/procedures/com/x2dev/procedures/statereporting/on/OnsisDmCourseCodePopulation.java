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
package com.x2dev.procedures.statereporting.on;

import com.follett.fsc.core.framework.persistence.ColumnQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.ToolJavaSource;
import com.x2dev.procedures.statereporting.common.FilterableFactory;
import com.x2dev.procedures.statereporting.common.FilterableFactory.Filterable;
import com.x2dev.procedures.statereporting.common.FilterableFactory.MultiLevelMap;
import com.x2dev.sis.model.beans.Course;
import com.x2dev.utils.LoggerUtils;
import com.x2dev.utils.StreamUtils;
import com.x2dev.utils.X2BaseException;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.*;
import org.apache.commons.lang3.StringUtils;

/**
 * The Class OnCourseTemplateProcedure.
 *
 * @author Follett Software Company
 * @copyright 2019
 */
public class OnsisDmCourseCodePopulation extends ToolJavaSource {
    private static final long serialVersionUID = 1L;

    private static final String NO_PERFMON4J_INSTRUMENTATION = "";

    private static class CourseCodeMessage {
        enum Field {
            MESSAGE, MESSAGE_TYPE, INFO_TYPE, COURSE, COURSE_OID;
        }
        enum InfoType {
            ADDED, SKIPPED, UPDATED;
        }
        enum MessageType {
            ERROR, INFO, WARNING;
        }

        Map<Field, Object> m_values = new HashMap<>();

        CourseCodeMessage(Course course, String message) {
            m_values.put(Field.COURSE_OID, course == null ? "" : course.getOid());
            m_values.put(Field.MESSAGE, message);
        }

        private Object get(Field field) {
            return m_values.get(field);
        }

        private void set(Field field, Object value) {
            m_values.put(field, value);
        }

        /**
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            StringBuilder description = new StringBuilder();
            Course course = (Course) get(Field.COURSE);
            String message = (String) get(Field.MESSAGE);
            if (course != null) {
                String courseNumber = course.getNumber();
                if (StringUtils.isEmpty(courseNumber)) {
                    description.append("course oid [" + course.getOid() + "]: ");
                } else {
                    description.append("course number [" + courseNumber + "]: ");
                }
                description.append(message);
            }
            return description.toString();
        }
    }

    private static final String ALIAS_COURSE_CODE_TYPE = "all-crs-CourseCodeType";
    private static final String ALIAS_MINISTRY_COURSE_CODE = "all-crs-MinistryCourseCode";
    private static final String ALIAS_RCD_INSTITUTION_CODE = "rcd-crs-institution-code";
    private static final String COURSE_TYPE_DCC = "DCC";
    private static final String DELIMITER = "      ---------------";
    private static final String INPUT_PARAMETER_INST_TYPE = "institutionType";
    private static final String INPUT_PARAMETER_IS_REVIEW = "isReview";

    private Filterable<CourseCodeMessage> m_infosHelper;
    private List<CourseCodeMessage> m_messages = new ArrayList<>();
    private Filterable<CourseCodeMessage> m_messagesHelper;
    private Set<String> m_institutionTypes = new HashSet();
    private boolean m_isReview = true;

    /**
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();
        Boolean isReview = (Boolean) getParameter(INPUT_PARAMETER_IS_REVIEW);
        if (isReview != null) {
            m_isReview = isReview;
        }

        String institutionTypeOid = (String) getParameter(INPUT_PARAMETER_INST_TYPE);
        if (StringUtils.isEmpty(institutionTypeOid)) {
            addMessage(CourseCodeMessage.MessageType.ERROR, "Institution type must be selected");
        } else {
            X2Criteria criteria = new X2Criteria();
            criteria.addIn(X2BaseBean.COL_OID, Arrays.asList(institutionTypeOid.split("\\s*,\\s*")));
            String[] columns = new String[] {ReferenceCode.COL_CODE};
            ColumnQuery query = new ColumnQuery(ReferenceCode.class, columns, criteria);
            try (QueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query)) {
                while (iterator.hasNext()) {
                    Object[] row = (Object[]) iterator.next();
                    m_institutionTypes.add((String) row[0]);
                }
            }
        }
    }

    /**
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#run()
     */
    @Override
    protected void run() throws Exception {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(Course.COL_DISTRICT_CONTEXT_OID, getOrganization().getCurrentContextOid());
        Filterable<Course> courseHelper = FilterableFactory.create(getBroker(), Course.class, criteria);

        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField ministryField = dictionary.findDataDictionaryFieldByAlias(ALIAS_MINISTRY_COURSE_CODE);
        if (ministryField == null || !ministryField.hasReferenceTable()) {
            addMessage(CourseCodeMessage.MessageType.ERROR, "Field [" + ALIAS_MINISTRY_COURSE_CODE
                    + "] doesn't have ref table, cannot determined codes to populate [" + ALIAS_COURSE_CODE_TYPE
                    + "] and [" + ALIAS_MINISTRY_COURSE_CODE + "]");
            return;
        }

        dictionary = DataDictionary.getDistrictDictionary(ministryField.getReferenceTable().getExtendedDataDictionary(),
                getBroker().getPersistenceKey());
        DataDictionaryField institutionCodeField =
                dictionary.findDataDictionaryFieldByAlias(ALIAS_RCD_INSTITUTION_CODE);
        if (institutionCodeField == null) {
            addMessage(CourseCodeMessage.MessageType.ERROR, "Field [" + ALIAS_RCD_INSTITUTION_CODE
                    + "] cannot be found in reference table extended dictionary");
            return;
        }
        X2Criteria codesCriteria = new X2Criteria();
        codesCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, ministryField.getReferenceTableOid());
        Filterable<ReferenceCode> codesHelper =
                FilterableFactory.create(getBroker(), ReferenceCode.class, codesCriteria);

        boolean isError = false;
        try {
            getBroker().beginTransaction();
            List<Course> courses = courseHelper.extractSorted(Arrays.asList(Course.COL_NUMBER), true);
            for (Course course : courses) {
                String courseNum = course.getNumber();
                if (StringUtils.isEmpty(courseNum)) {
                    addInfo(CourseCodeMessage.InfoType.SKIPPED, course, "Course number is empty");
                    continue;
                }
                if (courseNum.length() < 5) {
                    addInfo(CourseCodeMessage.InfoType.SKIPPED, course, "Course number is less than 5 characters");
                    continue;
                }
                String first5 = courseNum.substring(0, 5);
                Collection<ReferenceCode> matchingCodes =
                        codesHelper.filter(ReferenceCode.COL_STATE_CODE, first5).extract();
                if (matchingCodes == null || matchingCodes.size() == 0) {
                    addInfo(CourseCodeMessage.InfoType.SKIPPED, course,
                            "Cannot find course code for [" + first5 + "]");
                    continue;
                } else if (matchingCodes.size() > 1) {
                    boolean duplicateFound = false;
                    List<ReferenceCode> matchingDccCodes = new ArrayList();

                    for (ReferenceCode code : matchingCodes) {
                        if (!COURSE_TYPE_DCC.equals(code.getDependencyCode())) {
                            duplicateFound = true;
                            break;
                        }
                        if (m_institutionTypes
                                .contains(code.getFieldValueByBeanPath(institutionCodeField.getJavaName()))) {
                            matchingDccCodes.add(code);
                        }
                    }
                    if (duplicateFound) {
                        addInfo(CourseCodeMessage.InfoType.SKIPPED, course,
                                "Duplicate course code for [" + first5 + "]");
                    } else if (matchingDccCodes.size() == 1) {
                        matchingCodes = matchingDccCodes;
                    } else {
                        addInfo(CourseCodeMessage.InfoType.SKIPPED, course,
                                "Multiple institution codes found for course code for [" + first5 + "]");
                    }
                }
                if (matchingCodes.size() == 1) {
                    ReferenceCode code = matchingCodes.iterator().next();
                    String ministryCourseCode = code.getCode();
                    String codeType = code.getDependencyCode();
                    if (StringUtils.isEmpty(ministryCourseCode)) {
                        addInfo(CourseCodeMessage.InfoType.SKIPPED, course,
                                "Course code is empty for [" + first5 + "]");
                        continue;
                    }
                    if (StringUtils.isEmpty(codeType)) {
                        addInfo(CourseCodeMessage.InfoType.SKIPPED, course,
                                "Course code type is empty for [" + first5 + "]");
                        continue;
                    }

                    String currentCourseCode = (String) course.getFieldValueByAlias(ALIAS_MINISTRY_COURSE_CODE);
                    String currentCourseCodeType = (String) course.getFieldValueByAlias(ALIAS_COURSE_CODE_TYPE);

                    if (ministryCourseCode.equals(currentCourseCode) && codeType.equals(currentCourseCodeType)) {
                        addInfo(CourseCodeMessage.InfoType.SKIPPED, course,
                                "Course already has course code [" + ministryCourseCode + "] and course code type ["
                                        + codeType + "]");
                        continue;
                    }

                    StringBuilder message = new StringBuilder();
                    boolean isUpdated = false;
                    if (!StringUtils.isEmpty(currentCourseCode) && !currentCourseCode.equals(ministryCourseCode)) {
                        message.append(
                                "Old course code [" + currentCourseCode + "], new course code [" + ministryCourseCode
                                        + "]\n");
                        isUpdated = true;
                    }
                    if (!StringUtils.isEmpty(currentCourseCodeType) && !currentCourseCodeType.equals(codeType)) {
                        message.append(
                                "Old course code type [" + currentCourseCodeType + "], new course code type ["
                                        + codeType
                                        + "]");
                        isUpdated = true;
                    }

                    if (isUpdated) {
                        addInfo(CourseCodeMessage.InfoType.UPDATED, course, message.toString());
                    } else {
                        addInfo(CourseCodeMessage.InfoType.ADDED, course, "Course code set to [" + ministryCourseCode
                                + "], course code type set to [" + codeType + "]");
                    }

                    course.setFieldValueByAlias(ALIAS_MINISTRY_COURSE_CODE, ministryCourseCode);
                    course.setFieldValueByAlias(ALIAS_COURSE_CODE_TYPE, codeType);

                    getBroker().saveBeanForced(course);
                }
            }
        } catch (Exception e) {
            addMessage(CourseCodeMessage.MessageType.ERROR, LoggerUtils.convertThrowableToString(e));
            isError = true;
            throw e;
        } finally {
            prepareOutput();
            if (isError || m_isReview) {
                getBroker().rollbackTransaction();
            } else {
                getBroker().commitTransaction();
            }
        }
    }

    private void addInfo(CourseCodeMessage.InfoType infoType, Course course, String message) {
        CourseCodeMessage courseCodeMessage = new CourseCodeMessage(course, message);
        courseCodeMessage.set(CourseCodeMessage.Field.MESSAGE_TYPE, CourseCodeMessage.MessageType.INFO);
        courseCodeMessage.set(CourseCodeMessage.Field.MESSAGE, message);
        courseCodeMessage.set(CourseCodeMessage.Field.INFO_TYPE, infoType);
        courseCodeMessage.set(CourseCodeMessage.Field.COURSE, course);
        m_messages.add(courseCodeMessage);
    }

    private void addMessage(CourseCodeMessage.MessageType messageType, String message) {
        if (CourseCodeMessage.MessageType.INFO.equals(messageType)) {
            throw new RuntimeException("Custom message cannot be set for INFO message type");
        }
        CourseCodeMessage courseCodeMessage = new CourseCodeMessage(null, message);
        courseCodeMessage.set(CourseCodeMessage.Field.MESSAGE_TYPE, messageType);
        courseCodeMessage.set(CourseCodeMessage.Field.MESSAGE, message);
        m_messages.add(courseCodeMessage);
    }

    private void appendLine(StringBuilder stringBuilder, String line) {
        stringBuilder.append(line);
        stringBuilder.append("\n");
    }

    private void appendMessages(StringBuilder stringBuilder, List<CourseCodeMessage> messages) {
        for (CourseCodeMessage message : messages) {
            stringBuilder.append(message.toString());
            stringBuilder.append("\n");
        }
    }

    private Filterable<CourseCodeMessage> createMessagesFilterable(Collection<CourseCodeMessage> messages) {
        return FilterableFactory.create(messages,
                Arrays.asList(CourseCodeMessage.Field.COURSE_OID.toString(),
                        CourseCodeMessage.Field.MESSAGE.toString()),
                new MultiLevelMap.ValueByKeyResolver<CourseCodeMessage>() {
                    @Override
                    public Object getValue(String key, CourseCodeMessage entity) {
                        CourseCodeMessage.Field field = CourseCodeMessage.Field.valueOf(key);
                        return entity.get(field);
                    }
                });
    }

    private int getInfoCounterByInfoType(CourseCodeMessage.InfoType infoType) {
        return getInfosHelper().count(CourseCodeMessage.Field.INFO_TYPE.toString(), infoType);
    }

    private List<CourseCodeMessage> getInfosByInfoType(CourseCodeMessage.InfoType infoType) {
        List<CourseCodeMessage> messages = new ArrayList<CourseCodeMessage>(
                getInfosHelper().filter(CourseCodeMessage.Field.INFO_TYPE.toString(), infoType).extract());
        Collections.sort(messages, new Comparator<CourseCodeMessage>() {
            @Override
            public int compare(CourseCodeMessage arg0, CourseCodeMessage arg1) {
                return arg0.toString().compareTo(arg1.toString());
            }
        });
        return messages;
    }

    private Collection<CourseCodeMessage> getErrors() {
        return getMessagesByType(CourseCodeMessage.MessageType.ERROR);
    }

    private Collection<CourseCodeMessage> getInfos() {
        return getMessagesByType(CourseCodeMessage.MessageType.INFO);
    }

    private Filterable<CourseCodeMessage> getInfosHelper() {
        if (m_infosHelper == null) {
            m_infosHelper = createMessagesFilterable(getInfos());
        }
        return m_infosHelper;
    }

    private Collection<CourseCodeMessage> getWarnings() {
        return getMessagesByType(CourseCodeMessage.MessageType.WARNING);
    }

    private Collection<CourseCodeMessage> getMessagesByType(CourseCodeMessage.MessageType type) {
        return getMessagesHelper()
                .filter(CourseCodeMessage.Field.MESSAGE_TYPE.toString(), type).extract();
    }

    private Filterable<CourseCodeMessage> getMessagesHelper() {
        if (m_messagesHelper == null) {
            m_messagesHelper = createMessagesFilterable(m_messages);
        }
        return m_messagesHelper;
    }

    private void prepareOutput() {
        StringBuilder output = new StringBuilder();

        Collection<CourseCodeMessage> errors = getErrors();
        if (!errors.isEmpty()) {
            appendLine(output, DELIMITER + " Errors");
        }
        for (CourseCodeMessage error : errors) {
            appendLine(output, "ERROR: " + error.get(CourseCodeMessage.Field.MESSAGE));
        }

        Collection<CourseCodeMessage> warnings = getWarnings();
        if (!warnings.isEmpty()) {
            appendLine(output, DELIMITER + " Warnings");
        }
        for (CourseCodeMessage warning : warnings) {
            appendLine(output, "WARNING: " + warning.get(CourseCodeMessage.Field.MESSAGE));
        }

        int addedCount = getInfoCounterByInfoType(CourseCodeMessage.InfoType.ADDED);
        appendLine(output, "Newly assigned codes courses: " + addedCount);
        int skippedCount = getInfoCounterByInfoType(CourseCodeMessage.InfoType.SKIPPED);
        appendLine(output, "Skipped courses: " + skippedCount);
        int updatedCount = getInfoCounterByInfoType(CourseCodeMessage.InfoType.UPDATED);
        appendLine(output, "Updated courses: " + updatedCount);

        if (skippedCount > 0) {
            appendLine(output, DELIMITER + " Skipped records log");
            List<CourseCodeMessage> skippedMessages = getInfosByInfoType(CourseCodeMessage.InfoType.SKIPPED);
            appendMessages(output, skippedMessages);
        }

        if (updatedCount > 0) {
            appendLine(output, DELIMITER + " Updated records log");
            List<CourseCodeMessage> updatedMessages = getInfosByInfoType(CourseCodeMessage.InfoType.UPDATED);
            appendMessages(output, updatedMessages);
        }

        appendLine(output, DELIMITER);

        try (OutputStream out = getResultHandler().getOutputStream();
                InputStream inputStream = new ByteArrayInputStream(output.toString().getBytes())) {
            StreamUtils.copyStream(inputStream, out);
        } catch (IOException e) {
            e.printStackTrace();
        }

    }
}
