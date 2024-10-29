/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2013 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.procedures.statereporting.pa;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.ColumnQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.Race;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.ConductIncident;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.CollectionUtils;
import com.x2dev.utils.X2BaseException;
import java.util.*;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Incident Person Export data module
 * This export includes victims and offenders.
 */
public class PAIncidentPerson extends StateReportData {

    /**
     * Entity class for Incident Person Export.
     */
    public static class PAIncidentPersonEntity extends StateReportEntity {
        private PAIncidentPerson m_srData;

        /**
         * Instantiates a new PA incident person entity.
         */
        public PAIncidentPersonEntity() {}

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            m_srData = (PAIncidentPerson) data;
            this.setRowCount(m_srData.m_persons.size());
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            Person person = getCurrentPerson();
            String name = person.getFirstName() + " " + person.getLastName() +
                    " [OID: " + person.getOid() + "] ";

            return name;
        }

        /**
         * Gets the current person.
         *
         * @return Sis person
         */
        public SisPerson getCurrentPerson() {
            return m_srData.m_persons.get(this.getCurrentRow());
        }
    }

    /**
     * Retrieve Person Id.
     */
    class RetrievePersonId implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            Object result = null;

            SisPerson person = ((PAIncidentPersonEntity) entity).getCurrentPerson();
            result = StringUtils.isEmpty(person.getOid()) ? person.getPersonId() : person.getOid();
            return result;
        }
    }

    /**
     * Retrieve Person Bean Fields.
     */
    class RetrievePersonBean implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            Object result = null;

            SisPerson person = ((PAIncidentPersonEntity) entity).getCurrentPerson();
            String beanPath = (String) field.getParameter();
            if (!StringUtils.isEmpty(beanPath)) {
                if (beanPath.contains("[")) {
                    String alias = beanPath.replace("[", "").replace("]", "");
                    result = person.getFieldValueByAlias(alias);
                } else {
                    result = person.getFieldValueByBeanPath(beanPath);
                }
            }

            return result;
        }
    }

    /**
     * Retrieve race code {@link#PADistrictStudent.RetrieveRace}
     */
    class RetrieveRace implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            SisPerson person = ((PAIncidentPersonEntity) entity).getCurrentPerson();
            if (person == null) {
                return null;
            }

            Collection<Race> races = m_races.get(person.getOid());
            if (races == null) {
                return null;
            }

            if (person.getHispanicLatinoIndicator()) {
                return "4";
            }

            if (races.size() > 1) {
                return "6";
            }

            if (races.size() == 1) {
                return lookupReferenceCodeByBeanPath(Race.class,
                        Race.COL_RACE_CODE,
                        races.iterator().next().getRaceCode(),
                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
            }
            return null;
        }
    }

    /**
     * Retrieve Student Id for person who are student.
     */
    class RetrieveStudentId implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            Object result = null;

            SisPerson person = ((PAIncidentPersonEntity) entity).getCurrentPerson();
            Student student = person.getStudent();
            if (student != null) {
                result = student.getStateId();
            }

            return result;
        }
    }

    // Export input parameters
    static final String PARAM_END_DATE = "endDate";
    static final String PARAM_START_DATE = "startDate";

    // Aliases
    private static final String ALIAS_INCIDENT_INCLUDE = "DOE PIMS INCIDENT INCLUDE";
    private static final String ALIAS_VICTIM_FNAME = "DOE EXT VIC FNAME";
    private static final String ALIAS_VICTIM_ID = "DOE VICTIM ID";
    private static final String ALIAS_VICTIM_LNAME = "DOE EXT VIC LNAME";
    // Calculation IDs
    private static final String CALC_ID_PERSON_BEAN = "PSN_CALC_PERSON_BEAN";
    private static final String CALC_ID_PERSON_ID = "PSN_CALC_PERSON_ID";
    private static final String CALC_ID_RACE_CODE = "PSN_CALC_RACE_CODE";
    private static final String CALC_ID_STUDENT_ID = "PSN_CALC_STUDENT_ID";

    // Class members
    protected String m_fieldIncidentInclude;
    protected String m_fieldVictimFname;
    protected String m_fieldVictimLname;
    protected String m_fieldVictimId;
    protected List<SisPerson> m_persons;

    protected StudentHistoryHelper m_helper;
    protected Map<String, Collection<Race>> m_races;

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        initializeFields();

        if (getSetupErrors().size() == 0) {
            m_persons = loadPersons();

            X2Criteria districtCriteria = new X2Criteria();
            QueryByCriteria query = new QueryByCriteria(Organization.class, districtCriteria);
            setQuery(query);

            setEntityClass(PAIncidentPersonEntity.class);

            // Add retrievers
            HashMap<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
            calcs.put(CALC_ID_PERSON_BEAN, new RetrievePersonBean());
            calcs.put(CALC_ID_PERSON_ID, new RetrievePersonId());
            calcs.put(CALC_ID_STUDENT_ID, new RetrieveStudentId());
            calcs.put(CALC_ID_RACE_CODE, new RetrieveRace());
            addCalcs(calcs);
        }
    }

    /**
     * Collect list of person. Check if person is not in the list add it.
     *
     * @param persons List for collecting
     * @param c List of potential
     * @param person Potential to add
     * @param psnIds list of added ids
     */
    private void collectPersons(List<SisPerson> persons,
                                Collection<SisPerson> c,
                                SisPerson person,
                                Set<String> psnIds) {
        if (!CollectionUtils.isEmpty(c) && person == null) {
            for (SisPerson psn : c) {
                if (!psnIds.contains(psn.getOid())) {
                    persons.add(psn);
                    psnIds.add(psn.getOid());
                }
            }
        } else if (person != null) {
            if (!psnIds.contains(person.getOid())) {
                persons.add(person);
                psnIds.add(person.getOid());
            }
        }
    }

    /**
     * Loads the incident codes for state reportable incidents.
     *
     * @return Collection
     */
    private Collection<String> getStateReportableCodes() {
        Collection<String> codes = null;
        Collection<String> allowedIncidentCodes = Arrays.asList(
                "1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
                "11", "12", "13", "14", "15", "16", "17", "18", "19", "20",
                "21", "22", "23", "24", "25", "26", "27", "28", "29", "30",
                "31", "32", "33", "34", "35", "36", "37", "38", "39", "40",
                "41", "42", "43", "44", "45", "46", "47", "48", "49", "50",
                "51", "52", "53", "54", "A", "C");

        DataDictionaryField field = getDataDictionaryField(ConductIncident.class, ConductIncident.COL_INCIDENT_CODE);
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, field.getReferenceTableOid());
        criteria.addNotEmpty(ReferenceCode.COL_STATE_CODE, getBroker().getPersistenceKey());
        criteria.addIn(ReferenceCode.COL_STATE_CODE, allowedIncidentCodes);

        SubQuery query = new SubQuery(ReferenceCode.class, ReferenceCode.COL_CODE, criteria);
        codes = getBroker().getSubQueryCollectionByQuery(query);

        return codes;
    }

    /**
     * Initialize alias fields.
     */
    private void initializeFields() {
        m_fieldIncidentInclude = translateAliasToJavaName(ALIAS_INCIDENT_INCLUDE, true);
        m_fieldVictimId = this.translateAliasToJavaName(ALIAS_VICTIM_ID, true);
        m_fieldVictimFname = this.translateAliasToJavaName(ALIAS_VICTIM_FNAME, true);
        m_fieldVictimLname = this.translateAliasToJavaName(ALIAS_VICTIM_LNAME, true);
    }

    /**
     *
     * Generate the criteria including student offenders, student victims and non-student victims.
     *
     * @return List
     */
    private List<SisPerson> loadPersons() {
        X2Criteria incidentCriteria = new X2Criteria();
        {

            incidentCriteria.addEqualTo(ConductIncident.REL_SCHOOL + ModelProperty.PATH_DELIMITER +
                    SisSchool.COL_ORGANIZATION1_OID, super.getOrganization().getOid());
            incidentCriteria.addIn(ConductIncident.COL_INCIDENT_CODE, getStateReportableCodes());

            if (isSchoolContext()) {
                incidentCriteria.addEqualTo(ConductIncident.COL_SCHOOL_OID, getSchool().getOid());
            } else {
                incidentCriteria.addNotEqualTo(ConductIncident.REL_SCHOOL + PATH_DELIMITER +
                        SisSchool.COL_INACTIVE_INDICATOR,
                        Boolean.TRUE);
                incidentCriteria.addNotEqualTo(ConductIncident.REL_SCHOOL + PATH_DELIMITER +
                        SisSchool.COL_ARCHIVE_INDICATOR,
                        Boolean.TRUE);
            }

            Date startDate = (Date) getParameter(PARAM_START_DATE);
            if (startDate != null) {
                incidentCriteria.addGreaterOrEqualThan(ConductIncident.COL_INCIDENT_DATE,
                        startDate);
            }

            Date endDate = (Date) getParameter(PARAM_END_DATE);
            if (endDate != null) {
                incidentCriteria.addLessOrEqualThan(ConductIncident.COL_INCIDENT_DATE,
                        endDate);
            }
            applyInputCriteria(incidentCriteria, false, null);

        }

        // "Person records are only necessary for those persons who are either
        // an offender or victim.
        // STUDENT_CONDUCT_INCIDENT.CND_STD_OID
        // or
        // STUDENT_CONDUCT_INCIDENT.CND_STD_OID_VCTIM
        // or
        // STUDENT_CONDUCT_INCIDENT.UDF representing victim other than student."
        Set<String> psnOids = new HashSet<String>();
        List<SisPerson> persons = new ArrayList();
        {
            // get all student.personOid where sdudentOid in
            // STUDENT_CONDUCT_INCIDENT.CND_STD_OID
            SubQuery incidentSubquery =
                    new SubQuery(ConductIncident.class, ConductIncident.COL_STUDENT_OID, incidentCriteria);

            X2Criteria studentCriteria = new X2Criteria();
            studentCriteria.addIn(X2BaseBean.COL_OID, incidentSubquery);
            SubQuery personQuery = new SubQuery(SisStudent.class, SisStudent.COL_PERSON_OID, studentCriteria);

            X2Criteria personCriteria = new X2Criteria();
            personCriteria.addIn(X2BaseBean.COL_OID, personQuery);
            Collection c = getBroker().getCollectionByQuery(new QueryByCriteria(SisPerson.class, personCriteria));
            collectPersons(persons, c, null, psnOids);
        }
        {
            // get all student.personOid where studentOid in
            // STUDENT_CONDUCT_INCIDENT.CND_STD_OID_VCTIM
            SubQuery victimSubquery =
                    new SubQuery(ConductIncident.class, ConductIncident.COL_VICTIM_OID, incidentCriteria);

            X2Criteria studentCriteria = new X2Criteria();
            studentCriteria.addIn(X2BaseBean.COL_OID, victimSubquery);
            SubQuery personQuery = new SubQuery(SisStudent.class, SisStudent.COL_PERSON_OID, studentCriteria);

            X2Criteria personCriteria = new X2Criteria();
            personCriteria.addIn(X2BaseBean.COL_OID, personQuery);
            Collection<SisPerson> c =
                    getBroker().getCollectionByQuery(new QueryByCriteria(SisPerson.class, personCriteria));
            collectPersons(persons, c, null, psnOids);
        }
        {
            // get all student.personOid is
            // STUDENT_CONDUCT_INCIDENT.["DOE VICTIM ID"]
            X2Criteria criteria = incidentCriteria.copy();
            criteria.addNotEmpty(m_fieldVictimId, getBroker().getPersistenceKey());

            Collection oids = new LinkedList();
            String[] columns = new String[] {m_fieldVictimId};
            ColumnQuery query = new ColumnQuery(ConductIncident.class, columns, criteria);
            try (QueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query)) {
                while (iterator.hasNext()) {
                    Object[] row = (Object[]) iterator.next();
                    oids.add(row[0]);
                }
            }
            if (!oids.isEmpty()) {
                X2Criteria personCriteria = new X2Criteria();
                personCriteria.addIn(X2BaseBean.COL_OID, oids);
                Collection c = getBroker().getCollectionByQuery(new QueryByCriteria(SisPerson.class, personCriteria));
                collectPersons(persons, c, null, psnOids);
            }
        }
        // select ConductIncident based victims
        {
            // create person bean for
            // STUDENT_CONDUCT_INCIDENT.["DOE EXT VIC FNAME"] or STUDENT_CONDUCT_INCIDENT.["DOE EXT
            // VIC LNAME"]
            X2Criteria criteria = incidentCriteria.copy();
            X2Criteria fNameCriteria = new X2Criteria();
            fNameCriteria.addNotEmpty(m_fieldVictimFname, getBroker().getPersistenceKey());
            X2Criteria lNameCriteria = new X2Criteria();
            lNameCriteria.addNotEmpty(m_fieldVictimLname, getBroker().getPersistenceKey());
            X2Criteria orCriteria = new X2Criteria();
            orCriteria.addOrCriteria(fNameCriteria);
            orCriteria.addOrCriteria(lNameCriteria);
            criteria.addAndCriteria(orCriteria);
            QueryByCriteria incidentQuery = new QueryByCriteria(ConductIncident.class, criteria);
            for (Object object : getBroker().getCollectionByQuery(incidentQuery)) {
                ConductIncident incident = (ConductIncident) object;
                SisPerson person = X2BaseBean.newInstance(SisPerson.class, getBroker().getPersistenceKey());
                person.setPersonId(incident.getOid());
                person.setFirstName((String) incident.getFieldValueByBeanPath(m_fieldVictimFname));
                person.setLastName((String) incident.getFieldValueByBeanPath(m_fieldVictimLname));
                collectPersons(persons, null, person, psnOids);
            }
        }

        loadRaces(persons);

        return persons;
    }

    /**
     * Load the race information for person's incldued in the report.
     *
     * @param persons Collection<SisPerson>
     */
    private void loadRaces(Collection<SisPerson> persons) {
        Collection personOids = new ArrayList(persons.size());
        for (SisPerson person : persons) {
            personOids.add(person.getOid());
        }

        if (personOids.size() == 0) {
            m_races = new HashMap<String, Collection<Race>>();
            return;
        }

        X2Criteria criteria = new X2Criteria();
        criteria.addIn(Race.COL_PERSON_OID, personOids);
        BeanQuery query = new BeanQuery(Race.class, criteria);
        query.addOrderBy(Race.COL_PERSON_OID, true);
        m_races = getBroker().getGroupedCollectionByQuery(query, Race.COL_PERSON_OID, 100000);
    }
}