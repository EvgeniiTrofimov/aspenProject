/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2006 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ValidationError;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.business.localization.LocalizationMessageResources;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.web.WebUtils;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisSchoolCapacity;
import com.x2dev.utils.CollectionUtils;
import com.x2dev.utils.StringUtils;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;
import org.apache.struts.action.ActionErrors;
import org.apache.struts.action.ActionMessage;

/**
 * Procedure to copy school capacity records from one school year context to another.
 *
 * @author X2 Development Corporation
 */
public class SchoolCapacityRolloverProcedure extends ProcedureJavaSource {
    /**
     * Name for the "fields to copy" input parameter. The value is a String.
     */
    public static final String FIELDS_TO_COPY_PARAM = "fieldsToCopy";

    /**
     * Name for the "source context OID" input parameter. The value is a String.
     */
    public static final String SOURCE_CONTEXT_OID_PARAM = "sourceContextOid";

    /**
     * Name for the "target context OID" input parameter. The value is a String.
     */
    public static final String TARGET_CONTEXT_OID_PARAM = "targetContextOid";

    // Error constants
    private static final String ERROR_DUPLICATE = "School Capacity record already exists for {0} ({1}) {2} {3}.";
    private static final String ERROR_SAVE = "Error saving School Capacity record {0} ({1}) {2} {3}:";

    // Delimiter constants
    private static final char LIST_DELIMITER = ',';

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        int count = 0;

        Criteria criteria = new Criteria();
        criteria.addEqualTo(SisSchoolCapacity.COL_DISTRICT_CONTEXT_OID, getParameter(SOURCE_CONTEXT_OID_PARAM));

        if (isSchoolContext()) {
            criteria.addEqualTo(SisSchoolCapacity.COL_SCHOOL_OID, getSchool().getOid());
        }

        QueryByCriteria query = new QueryByCriteria(SisSchoolCapacity.class, criteria);
        query.addOrderByAscending(SisSchoolCapacity.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_NAME);
        query.addOrderByAscending(SisSchoolCapacity.COL_SCHOOL_OID);
        query.addOrderByAscending(SisSchoolCapacity.COL_PROGRAM_CODE);
        query.addOrderByAscending(SisSchoolCapacity.COL_GRADE_LEVEL);

        String contextOid = (String) getParameter(TARGET_CONTEXT_OID_PARAM);
        Set<String> schoolCapacityKeys = loadSchoolCapacityKeys(contextOid);

        String fieldsToCopy = (String) getParameter(FIELDS_TO_COPY_PARAM);
        List<String> beanPaths = StringUtils.convertDelimitedStringToList(fieldsToCopy, LIST_DELIMITER, true);

        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                SisSchoolCapacity schoolCapacity = (SisSchoolCapacity) iterator.next();
                String schoolCapacityKey = buildSchoolCapacityKey(schoolCapacity.getSchoolOid(),
                        schoolCapacity.getGradeLevel(),
                        schoolCapacity.getProgramCode());

                if (!schoolCapacityKeys.contains(schoolCapacityKey)) {
                    SisSchoolCapacity newSchoolCapacity =
                            X2BaseBean.newInstance(SisSchoolCapacity.class, getBroker().getPersistenceKey());

                    /*
                     * Set the new district context OID, school OID, grade level, and program code.
                     */
                    newSchoolCapacity.setDistrictContextOid(contextOid);
                    newSchoolCapacity.setSchoolOid(schoolCapacity.getSchoolOid());
                    newSchoolCapacity.setGradeLevel(schoolCapacity.getGradeLevel());
                    newSchoolCapacity.setProgramCode(schoolCapacity.getProgramCode());

                    /*
                     * Copy any fields specified in the input definition.
                     */
                    if (!CollectionUtils.isEmpty(beanPaths)) {
                        for (String beanPath : beanPaths) {
                            Object value = schoolCapacity.getFieldValueByBeanPath(beanPath);
                            newSchoolCapacity.setFieldValueByBeanPath(beanPath, value);
                        }
                    }

                    List<ValidationError> errors = getBroker().saveBean(newSchoolCapacity);
                    if (!CollectionUtils.isEmpty(errors)) {
                        logMessage(schoolCapacity, ERROR_SAVE, errors);
                    } else {
                        count++;
                    }
                } else {
                    logMessage(schoolCapacity, ERROR_DUPLICATE, null);
                }
            }
        } finally {
            iterator.close();
        }

        logMessage("");
        logMessage("Copied " + count + " School Capacity record" + (count == 1 ? "" : "s") + ".");
    }

    /**
     * Builds the school capacity key for the passed school OID, grade level, and program code.
     *
     * @param schoolOid String
     * @param gradeLevel String
     * @param programCode String
     * @return String
     */
    private String buildSchoolCapacityKey(String schoolOid, String gradeLevel, String programCode) {
        StringBuilder keyBuffer = new StringBuilder();

        if (!StringUtils.isEmpty(schoolOid)) {
            keyBuffer.append(schoolOid);
        }

        if (!StringUtils.isEmpty(gradeLevel)) {
            keyBuffer.append(gradeLevel);
        }

        if (!StringUtils.isEmpty(programCode)) {
            keyBuffer.append(programCode);
        }

        return keyBuffer.toString();
    }

    /**
     * Loads a set of school capacity keys for the passed context OID.
     *
     * @param contextOid String
     * @return Set<String>
     */
    private Set<String> loadSchoolCapacityKeys(String contextOid) {
        Set<String> schoolCapacityKeys = new HashSet<String>();

        Criteria criteria = new Criteria();
        criteria.addEqualTo(SisSchoolCapacity.COL_DISTRICT_CONTEXT_OID, contextOid);

        String[] columns = {SisSchoolCapacity.COL_SCHOOL_OID, SisSchoolCapacity.COL_GRADE_LEVEL,
                SisSchoolCapacity.COL_PROGRAM_CODE};
        ReportQueryByCriteria query = new ReportQueryByCriteria(SisSchoolCapacity.class, columns, criteria);

        ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                Object[] record = (Object[]) iterator.next();
                schoolCapacityKeys.add(buildSchoolCapacityKey((String) record[0],
                        (String) record[1],
                        (String) record[2]));
            }
        } finally {
            iterator.close();
        }

        return schoolCapacityKeys;
    }

    /**
     * Logs are error message for the passed school capacity and list of validation errors.
     *
     * @param schoolCapacity SisSchoolCapacity
     * @param message String
     * @param errors List<ValidationError>
     */
    private void logMessage(SisSchoolCapacity schoolCapacity, String message, List<ValidationError> errors) {
        SisSchool school = schoolCapacity.getSchool();

        Object[] parameters = {school.getName(),
                school.getSchoolId(),
                schoolCapacity.getGradeLevel(),
                schoolCapacity.getProgramCode()};

        logMessage(StringUtils.formatMessage((String) null, message, parameters));

        if (!CollectionUtils.isEmpty(errors)) {
            ActionErrors actionErrors = WebUtils.convertUniqueValidationErrors(errors, null);
            LocalizationMessageResources messages =
                    LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale());

            Iterator errorIterator = actionErrors.get();
            while (errorIterator.hasNext()) {
                ActionMessage error = (ActionMessage) errorIterator.next();
                logMessage(messages.getMessage(error.getKey(), error.getValues()));
            }
        }
    }
}
