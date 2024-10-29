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
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.CalculatedField;
import com.follett.fsc.core.k12.business.BeanCopier;
import com.follett.fsc.core.k12.business.CalculatedFieldManager;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.PrivilegeSet;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.Course;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.RubricDefinition;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.utils.X2BaseException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Procedure that "clones" an existing course and all of its related sections and student schedules.
 * The cloned course is then attached to a selected rubric definition. This procedure is useful if
 * a rubric has been split, and new courses must be created for the newly split rubrics with the
 * same sections, teachers, and students as the original rubric.
 * <p>
 * This procedure requires the presence of 3 field aliases on the rubric definition table:
 * <ul>
 * <li>old-crsnum - must contain the course number that the original rubric is attached to
 * <li>new-crsnum - must contain the course number of the cloned course to be created
 * <li>crs-name - must contain the name of the cloned course to be created
 * </ul>
 * Each rubric in the system containing a value for ALL of the above mentioned fields is processed
 * by this procedure. For rubric successfully processed, the new-crsnum field is blanked out to
 * prevent it from being handled again on subsequent runs of this procedure.
 *
 * @author X2 Development Corporation
 */
public class RubricCourseExploder extends ProcedureJavaSource {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Alias for the new course name field.
     */
    public static final String NEW_CRSNAME_ALIAS = "crs-name";

    /**
     * Alias for the new course number field.
     */
    public static final String NEW_CRSNUM_ALIAS = "new-crsnum";

    /**
     * Alias for the old course number field.
     */
    public static final String OLD_CRSNUM_ALIAS = "old-crsnum";

    private PrivilegeSet m_privilegeSet = null;

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getUser().getPersistenceKey());

        Collection<RubricDefinition> rubricDefinitions = getRubricDefinitions(dictionary);
        for (RubricDefinition rubricDefinition : rubricDefinitions) {
            String oldCourseNumber = (String) rubricDefinition.getFieldValueByAlias(OLD_CRSNUM_ALIAS);
            String newCourseNumber = (String) rubricDefinition.getFieldValueByAlias(NEW_CRSNUM_ALIAS);
            String courseName = (String) rubricDefinition.getFieldValueByAlias(NEW_CRSNAME_ALIAS);

            logMessage("Course " + newCourseNumber);
            logMessage("------------------------------");

            Criteria criteria = new Criteria();
            criteria.addEqualTo(SchoolCourse.COL_NUMBER, oldCourseNumber);
            criteria.addEqualTo(SchoolCourse.REL_COURSE + "." + Course.COL_DISTRICT_CONTEXT_OID,
                    getCurrentContext().getOid());

            QueryByCriteria query = new QueryByCriteria(SchoolCourse.class, criteria);
            if (getBroker().getCount(query) == 1) {
                SchoolCourse schoolCourse = (SchoolCourse) getBroker().getBeanByQuery(query);

                BeanCopier beanCopier = new BeanCopier(getBroker());

                ArrayList<String> relationships = new ArrayList(6);

                relationships.add("relCskCrsOid");
                relationships.add("relCskMstOid");
                relationships.add("relCskMstOid.relMstSscOid");
                relationships.add("relCskMstOid.relMstMtcOid");
                relationships.add("relCskMstOid.relMstMtmOid");
                relationships.add("relCskMstOid.relMstMtmOid.relMtmMmxOid");

                HashMap<ModelProperty, Object> valuesToSet = new HashMap<ModelProperty, Object>();

                valuesToSet.put(
                        new ModelProperty(SchoolCourse.class, SchoolCourse.COL_NUMBER, getBroker().getPersistenceKey()),
                        newCourseNumber);
                valuesToSet.put(new ModelProperty(SchoolCourse.class, SchoolCourse.COL_DESCRIPTION,
                        getBroker().getPersistenceKey()),
                        courseName);
                valuesToSet.put(new ModelProperty(SchoolCourse.class, SchoolCourse.COL_RUBRIC_DEFINITION_OID,
                        getBroker().getPersistenceKey()),
                        rubricDefinition.getOid());

                valuesToSet.put(new ModelProperty(SchoolCourse.class, SchoolCourse.REL_COURSE + "." + Course.COL_NUMBER,
                        getBroker().getPersistenceKey()),
                        newCourseNumber);
                valuesToSet.put(new ModelProperty(SchoolCourse.class,
                        SchoolCourse.REL_COURSE + "." + Course.COL_DESCRIPTION, getBroker().getPersistenceKey()),
                        courseName);

                SchoolCourse copy = (SchoolCourse) beanCopier.copy(schoolCourse, relationships, valuesToSet);

                if (copy != null) {
                    logMessage("Course created successfully.");
                    refreshCalculatedFields(copy, dictionary);

                    rubricDefinition.setFieldValueByAlias(NEW_CRSNUM_ALIAS, null);
                    getBroker().saveBeanForced(rubricDefinition);
                } else {
                    logMessage("Unable to clone course.");
                }
            } else {
                logMessage("Course number must identify exactly one school course.");
            }

            logMessage("");
        }
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
        m_privilegeSet = userData.getPrivilegeSet();
    }

    /**
     * Returns the RubricDefinitions to create cloned courses for.
     *
     * @param dictionary DataDictionary
     * @return Collection
     */
    private Collection<RubricDefinition> getRubricDefinitions(DataDictionary dictionary) {
        DataDictionaryField oldCrsnumField = dictionary.findDataDictionaryFieldByAlias(OLD_CRSNUM_ALIAS);
        DataDictionaryField newCrsnumField = dictionary.findDataDictionaryFieldByAlias(NEW_CRSNUM_ALIAS);
        DataDictionaryField newCrsnameField = dictionary.findDataDictionaryFieldByAlias(NEW_CRSNAME_ALIAS);

        X2Criteria criteria = new X2Criteria();
        criteria.addNotEmpty(oldCrsnumField.getJavaName(), getBroker().getPersistenceKey());
        criteria.addNotEmpty(newCrsnumField.getJavaName(), getBroker().getPersistenceKey());
        criteria.addNotEmpty(newCrsnameField.getJavaName(), getBroker().getPersistenceKey());

        QueryByCriteria query = new QueryByCriteria(RubricDefinition.class, criteria);

        return getBroker().getCollectionByQuery(query);
    }

    /**
     * Refreshes the course view and description calculated fields for all sections attached to the
     * passed course.
     *
     * @param course SchoolCourse
     * @param dictionary DataDictionary
     * @throws X2BaseException exception
     */
    private void refreshCalculatedFields(SchoolCourse course, DataDictionary dictionary) throws X2BaseException {
        CalculatedFieldManager cfManager = new CalculatedFieldManager(new ModelBroker(m_privilegeSet)); // <--
                                                                                                        // This
                                                                                                        // doesn't
                                                                                                        // take
                                                                                                        // an
                                                                                                        // X2Broker.
                                                                                                        // Yikes.

        DataDictionaryField courseViewField =
                dictionary.findDataDictionaryField(MasterSchedule.class.getName(), MasterSchedule.COL_COURSE_VIEW);
        DataDictionaryField descriptionField =
                dictionary.findDataDictionaryField(MasterSchedule.class.getName(), MasterSchedule.COL_DESCRIPTION);

        CalculatedField courseViewCf = courseViewField.getDataFieldConfig().getCalculatedField();
        CalculatedField descriptionCf = descriptionField.getDataFieldConfig().getCalculatedField();

        Criteria masterCriteria = new Criteria();
        masterCriteria.addEqualTo(MasterSchedule.COL_SCHOOL_COURSE_OID, course.getOid());

        QueryByCriteria masterQuery = new QueryByCriteria(MasterSchedule.class, masterCriteria);

        Collection<MasterSchedule> sections = getBroker().getCollectionByQuery(masterQuery);
        for (MasterSchedule section : sections) {
            cfManager.updateCalculatedValues(courseViewCf, section);
            cfManager.updateCalculatedValues(descriptionCf, section);
        }

        logMessage("Refreshed calculated fields successfully.");
    }
}
