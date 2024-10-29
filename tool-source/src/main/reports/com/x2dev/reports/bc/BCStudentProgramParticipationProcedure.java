/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2002-2016 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.reports.bc;

import static com.follett.fsc.core.k12.business.ValidationConstants.CUSTOM_ERROR;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.BeanManager;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.StudentAlert;
import com.follett.fsc.core.k12.beans.StudentAlert.AlertType;
import com.follett.fsc.core.k12.beans.User;
import com.follett.fsc.core.k12.beans.UserRole;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ValidationError;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.sis.tools.procedures.StudentProgramParticipationProcedure;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Locale;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Custom procedure for BC that defines functionality when saving designation based student program
 * participation beans.
 */
public class BCStudentProgramParticipationProcedure extends StudentProgramParticipationProcedure {
    private static final String ALIAS_STD_PGM_PRIMARY_DESIGNATION = "std-pgm-primary-designation";
    private static final String ALIAS_STUDENT_DESIGNATION = "std-sped-category";
    private static final String DESIGNATION_ALERT_ICON_PATH = "alertIcons/designation.png";
    private static final String DESIGNATION_ACCESS_ROLE_NAME = "1701 Designations";

    /**
     * Default constructor.
     *
     * @param broker X2Broker
     * @param locale Locale
     * @param organization Organization
     * @param school School
     * @param user User
     * @param schoolScoped Boolean
     */
    public BCStudentProgramParticipationProcedure(X2Broker broker, Locale locale, Organization organization,
            School school, User user, Boolean schoolScoped) {
        super(broker, locale, organization, school, user, schoolScoped);
    }

    /**
     * Validate.
     *
     * @param program StudentProgramParticipation
     * @return List
     * @see
     *      com.x2dev.sis.tools.procedures.StudentProgramParticipationProcedure#validate(com.x2dev.sis.
     *      model.beans.StudentProgramParticipation)
     */
    @Override
    public List<ValidationError> validate(StudentProgramParticipation program) {
        List<ValidationError> errors = super.validate(program);

        if (isDesignationProgram(program)) {
            if (!hasAccess(program)) // Need to do this as when the program is added for the first
                                     // time.
            {
                errors.add(new ValidationError(CUSTOM_ERROR, null,
                        "You may not have permission to perform this operation."));
            } else {
                if (getSchoolScoped()) {
                    if (getSchool() != null && !getSchool().getOid().equals(program.getStudent().getSchoolOid())) {
                        errors.add(new ValidationError(CUSTOM_ERROR, null,
                                "Only primary school can enter and edit designations for the student."));
                    }
                } else if (getOrganization() != null) {
                    if (!getOrganization().getOid().equals(program.getStudent().getOrganization1Oid()) &&
                            !getOrganization().getOid().equals(program.getStudent().getOrganization2Oid())) {
                        errors.add(new ValidationError(CUSTOM_ERROR, null,
                                "Only primary organization can enter and edit designations for the student."));
                    }
                }
            }
        }

        return errors;
    }

    /**
     * When deleting a primary designation, blank out ministry designation alias on student
     * and delete designation student alert.
     *
     * @param program StudentProgramParticipation
     * @return List
     * @see com.x2dev.sis.tools.procedures.StudentProgramParticipationProcedure#afterDelete(
     *      StudentProgramParticipation)
     */
    @Override
    public List<ValidationError> afterDelete(StudentProgramParticipation program) {
        List<ValidationError> errors = new ArrayList<ValidationError>();

        if (isDesignationProgram(program)) {
            if (isPrimaryDesignation(program)) {
                errors.addAll(updateStudentDesignationAlias(program.getStudent(), null));

                deleteStudentDesignationAlert(program.getStudentOid());
            }
        }

        return errors;
    }

    /**
     * Checks for access.
     *
     * @param program StudentProgramParticipation
     * @return true, if successful
     * @see
     *      com.x2dev.sis.tools.procedures.StudentProgramParticipationProcedure#hasAccess(com.x2dev.sis.
     *      model.beans.StudentProgramParticipation)
     */
    @Override
    public boolean hasAccess(StudentProgramParticipation program) {
        boolean hasAccess = false;

        if (isDesignationProgram(program)) {
            Collection<UserRole> userRoles = getUser().getUserRoles();
            for (UserRole userRole : userRoles) {
                if (userRole.getRole().getName().equals(DESIGNATION_ACCESS_ROLE_NAME)) {
                    hasAccess = true;
                    break;
                }
            }
        } else {
            hasAccess = true;
        }
        return hasAccess;
    }

    /**
     * When saving a primary designation program
     * <ul>
     * <li>copy over program code to ministry designation alias on student table
     * <li>create a designation student alert, if it is not already present.
     * <li>when another primary designation program exists, set end date and set primary flag to 0
     * </ul>
     * <p>
     * When saving a non-primary designation program
     * <ul>
     * update ministry designation alias on student table and delete student alert as needed.
     * <li>
     * </ul>
     *
     * @param program StudentProgramParticipation
     * @return List
     * @see com.x2dev.sis.tools.procedures.StudentProgramParticipationProcedure#afterSave(
     *      StudentProgramParticipation)
     */
    @Override
    public List<ValidationError> afterSave(StudentProgramParticipation program) {
        List<ValidationError> errors = new ArrayList<ValidationError>();

        /*
         * If the end date of the primary designation is before today, make sure to end the program
         * if it is a primary designation
         */
        PlainDate today = new PlainDate();
        if (isPrimaryDesignation(program) && program.getEndDate() != null && program.getEndDate().before(today)) {
            DataDictionary dictionary = DataDictionary.getDistrictDictionary(program.getExtendedDataDictionary(),
                    getBroker().getPersistenceKey());
            program.setFieldValueByAlias(ALIAS_STD_PGM_PRIMARY_DESIGNATION, BooleanAsStringConverter.FALSE, dictionary);
        }

        if (isPrimaryDesignation(program)) {
            // copy over program code to designation alias in student table
            errors.addAll(updateStudentDesignationAlias(program.getStudent(), program.getProgramCode()));

            // For any existing primary designation, disable primary flag
            errors.addAll(disablePreviousPrimary(program));

            QueryByCriteria query = new QueryByCriteria(StudentAlert.class,
                    getStudentDesignationAlertCriteria(program.getStudentOid()));
            StudentAlert designationAlert = (StudentAlert) getBroker().getBeanByQuery(query);
            if (designationAlert == null) {
                designationAlert = X2BaseBean.newInstance(StudentAlert.class, getBroker().getPersistenceKey());
                designationAlert.setStudentOid(program.getStudentOid());
                designationAlert.setAlertType(AlertType.OTHER.ordinal());

                String designationAlertMessage =
                        LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale())
                                .getMessage(getLocale(), "message.bc.designation.student.alert");
                designationAlert.setAlertDescription(designationAlertMessage);
                designationAlert.setIconFilename(DESIGNATION_ALERT_ICON_PATH);

                errors.addAll(getBroker().saveBean(designationAlert));
            }
        } else if (isDesignationProgram(program)) // filter out non-designation programs
        {
            SisStudent student = program.getStudent();
            String studentDesignation = (String) student.getFieldValueByAlias(ALIAS_STUDENT_DESIGNATION);
            if (!StringUtils.isEmpty(studentDesignation)) {
                StudentProgramParticipation primaryDesignation = getPrimaryDesignation(program);
                if (primaryDesignation == null || !studentDesignation.equals(primaryDesignation.getProgramCode())) {
                    errors.addAll(updateStudentDesignationAlias(student, null));

                    deleteStudentDesignationAlert(student.getOid());
                }
            }
        }

        return errors;
    }

    /**
     * Deletes a student designation alert for passed student OID.
     *
     * @param studentOid String
     */
    private void deleteStudentDesignationAlert(String studentOid) {
        QueryByCriteria query = new QueryByCriteria(StudentAlert.class, getStudentDesignationAlertCriteria(studentOid));
        getBroker().deleteByQuery(query);
    }

    /**
     * Sets end date and disables primary flag on existing primary designation, if any.
     *
     * @param program StudentProgramParticipation
     * @return List&lt;ValidationError&gt;
     */
    private List<ValidationError> disablePreviousPrimary(StudentProgramParticipation program) {
        List<ValidationError> errors = new ArrayList<ValidationError>();

        StudentProgramParticipation previousPrimary = getPrimaryDesignation(program);
        if (previousPrimary != null) {
            previousPrimary.setEndDate(getPlainDate());

            DataDictionary dictionary = DataDictionary.getDistrictDictionary(program.getExtendedDataDictionary(),
                    getBroker().getPersistenceKey());
            previousPrimary.setFieldValueByAlias(ALIAS_STD_PGM_PRIMARY_DESIGNATION, BooleanAsStringConverter.FALSE,
                    dictionary);

            if (previousPrimary.isDirty()) {
                errors.addAll(getBroker().saveBean(previousPrimary));
            }
        }

        return errors;
    }

    /**
     * Returns primary designation program.
     *
     * @param program StudentProgramParticipation
     * @return StudentProgramParticipation
     */
    private StudentProgramParticipation getPrimaryDesignation(StudentProgramParticipation program) {
        StudentProgramParticipation primaryDesignation = null;

        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(StudentProgramParticipation.COL_STUDENT_OID, program.getStudentOid());
        criteria.addEqualTo(StudentProgramParticipation.COL_EXTENDED_DATA_DICTIONARY_OID,
                program.getExtendedDataDictionaryOid());
        criteria.addNotEqualTo(X2BaseBean.COL_OID, program.getOid());

        DataDictionary dictionary = DataDictionary.getDistrictDictionary(program.getExtendedDataDictionary(),
                getBroker().getPersistenceKey());
        DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(ALIAS_STD_PGM_PRIMARY_DESIGNATION);
        criteria.addEqualTo(field.getJavaName(), BooleanAsStringConverter.TRUE);

        QueryByCriteria query = new QueryByCriteria(StudentProgramParticipation.class, criteria);
        primaryDesignation = (StudentProgramParticipation) getBroker().getBeanByQuery(query);

        return primaryDesignation;
    }

    /**
     * Returns the designation alert criteria.
     *
     * @param studentOid String
     * @return X2Criteria
     */
    private X2Criteria getStudentDesignationAlertCriteria(String studentOid) {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(StudentAlert.COL_STUDENT_OID, studentOid);
        criteria.addEqualTo(StudentAlert.COL_ALERT_TYPE, Integer.valueOf(AlertType.OTHER.ordinal()));
        criteria.addEqualTo(StudentAlert.COL_ICON_FILENAME, DESIGNATION_ALERT_ICON_PATH);

        return criteria;
    }

    /**
     * Returns true if passed program is part of designation extended dictionary which is identified
     * based on primary designation alias.
     *
     * @param program StudentProgramParticipation
     * @return boolean
     */
    private boolean isDesignationProgram(StudentProgramParticipation program) {
        boolean isDesignationProgram = false;

        if (program != null) {
            DataDictionary dictionary = DataDictionary.getDistrictDictionary(program.getExtendedDataDictionary(),
                    getBroker().getPersistenceKey());
            DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(ALIAS_STD_PGM_PRIMARY_DESIGNATION);
            if (field != null && field.getDataTableOid().equals(BeanManager
                    .getFullOid(StudentProgramParticipation.DICTIONARY_ID, getBroker().getPersistenceKey()))) {
                isDesignationProgram = true;
            }
        }

        return isDesignationProgram;
    }

    /**
     * Returns true if passed program is flagged as primary based on primary designation alias.
     *
     * @param program StudentProgramParticipation
     * @return boolean
     */
    private boolean isPrimaryDesignation(StudentProgramParticipation program) {
        boolean isPrimaryDesignation = false;

        DataDictionary dictionary =
                DataDictionary.getDistrictDictionary(program.getExtendedDataDictionary(), m_broker.getPersistenceKey());
        Boolean isPrimary =
                (Boolean) program.getFieldValueByAliasExtended(ALIAS_STD_PGM_PRIMARY_DESIGNATION, dictionary);
        if (isPrimary != null && isPrimary.booleanValue()) {
            isPrimaryDesignation = true;
        }

        return isPrimaryDesignation;
    }

    /**
     * Updates ministry designation alias on student table with passed value for passed student,
     * and saves student bean.
     *
     * @param student SisStudent
     * @param designationValue String
     * @return List&lt;ValidationError&gt;
     */
    private List<ValidationError> updateStudentDesignationAlias(SisStudent student, String designationValue) {
        List<ValidationError> errors = new ArrayList<ValidationError>();

        student.setFieldValueByAlias(ALIAS_STUDENT_DESIGNATION, designationValue);
        if (student.isDirty()) {
            errors.addAll(getBroker().saveBean(student));
        }

        return errors;
    }
}
