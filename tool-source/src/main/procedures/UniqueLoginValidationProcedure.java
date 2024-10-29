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
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.User;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.utils.X2BaseException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.TreeMap;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * This procedure runs through all of the user logins looking for case insensitive duplicates.
 * Currently the user login is case sensitive so
 * all user names unless exactly the same are allowed. IE ABC and abc are allowed, but this
 * procedure would identify that match.
 *
 * @author X2 Development Corporation
 */
public class UniqueLoginValidationProcedure extends StateReportData {
    // Case insensitive map, and tree map, for fast lookup to see if login already exists in any
    // form
    protected Map<String, String> m_logins = new TreeMap<String, String>(String.CASE_INSENSITIVE_ORDER);

    protected Map<String, Person> m_cachedPeople = new HashMap<String, Person>();

    /**
     * Entity class for UniqueLoginValidationProcedure.
     *
     * @author X2 Development Corporation
     */
    public static class UniqueLoginValidationEntity extends StateReportEntity {
        public boolean m_duplicate = false;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public UniqueLoginValidationEntity() {
            // no argument constructor
        }

        /**
         * Initialize the data module.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            UniqueLoginValidationProcedure ulData = (UniqueLoginValidationProcedure) data;
            User user = (User) bean;

            if (ulData.m_logins.get(user.getLoginName()) != null) {
                m_duplicate = true;
                setRowCount(1);
            } else {
                ulData.m_logins.put(user.getLoginName(), user.getPersonOid());
                setRowCount(0);
            }
        }
    }

    /**
     * Checks and returns duplicate information if it exists.
     */
    protected class RetrieveDuplicate implements FieldRetriever {

        /**
         * Gets the appropriate data from the user or the person for the duplicate record.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            UniqueLoginValidationProcedure ulData = (UniqueLoginValidationProcedure) data;
            User user = (User) entity.getBean();
            String returnValue = null;

            String personOid = ulData.m_logins.get(user.getLoginName());
            if ("name".equals(field.getParameter())) {
                Person duplicatePerson = ulData.m_cachedPeople.get(personOid);
                if (duplicatePerson == null) {
                    duplicatePerson = (Person) data.getBroker().getBeanByOid(Person.class, personOid);
                    ulData.m_cachedPeople.put(personOid, duplicatePerson);
                }
                returnValue = duplicatePerson.getNameView();
            } else if ("login".equals(field.getParameter())) {
                returnValue = personOid;
            }
            return returnValue;
        }
    }

    /**
     * Validate logins are unique.
     */
    protected class ValidateUnique implements FieldValidator {

        /**
         * We check if we have identified this row as a duplicate, then create a validation error to
         * notify the user.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         */
        @Override
        public Collection<StateReportValidationError> getFieldValidation(StateReportData data,
                                                                         StateReportEntity entity,
                                                                         FieldDefinition field,
                                                                         String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            UniqueLoginValidationEntity ulEntity = (UniqueLoginValidationEntity) entity;

            if (ulEntity.m_duplicate) {
                UniqueLoginValidationProcedure ulData = (UniqueLoginValidationProcedure) data;
                User user = (User) entity.getBean();
                String personOID = ulData.m_logins.get(user.getLoginName());
                Person duplicatePerson = ulData.m_cachedPeople.get(personOID);
                if (duplicatePerson == null) {
                    duplicatePerson = (Person) data.getBroker().getBeanByOid(Person.class, personOID);
                    ulData.m_cachedPeople.put(user.getPersonOid(), duplicatePerson);
                }
                errors.add(new StateReportValidationError(entity, field,
                        "Duplicate Found",
                        "A duplicate login: " + user.getLoginName() + " was found for " + user.getPerson().getNameView()
                                + " and " + duplicatePerson.getNameView()));
            }

            return errors;
        }
    }

    /**
     * Initializes the data module.
     */
    @Override
    public void initialize() {
        initializeFields();

        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {
            // Create Query
            X2Criteria userCriteria = getUserCriteria();
            QueryByCriteria query = new QueryByCriteria(User.class, userCriteria);

            query.addOrderByAscending(User.COL_LOGIN_NAME);

            setQuery(query);

            setEntityClass(UniqueLoginValidationEntity.class);

            HashMap calcs = new HashMap<String, FieldRetriever>();
            HashMap validators = new HashMap<String, FieldRetriever>();
            calcs.put("duplicate", new RetrieveDuplicate());
            validators.put("unique", new ValidateUnique());
            super.addCalcs(calcs);
            super.addValidators(validators);
        }
    }

    /**
     * Lookup field aliases and paths.
     */
    private void initializeFields() {
        // nothing yet
    }

    /**
     * Get user Criteria.
     *
     * @return X2Criteria
     */
    private X2Criteria getUserCriteria() {
        X2Criteria userCriteria = new X2Criteria();

        userCriteria.addNotNull(User.COL_LOGIN_NAME);

        return userCriteria;
    }
}
