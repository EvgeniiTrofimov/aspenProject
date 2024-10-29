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

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Contact;
import com.follett.fsc.core.k12.beans.DataFieldConfig;
import com.follett.fsc.core.k12.beans.Family;
import com.follett.fsc.core.k12.beans.FamilyMember;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.StudentContact;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.web.WebUtils;
import com.x2dev.sis.model.beans.SisAddress;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Procedure for creating Family and FamilyMember records based on common values among Person
 * records.
 *
 * @author X2 Development Corporation
 */
public class CreateFamilies extends ProcedureJavaSource {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Name for the "daughter relationship" parameter. The value is a String.
     */
    public static final String DAUGHTER_RELATIONSHIP_CODE = "daughterRelationship";

    /**
     * Name for the "default relationship" parameter. The value is a String.
     */
    public static final String DEFAULT_RELATIONSHIP_CODE = "defaultRelationship";

    /**
     * Name for the "field 1 OID" parameter. The value is a String.
     */
    public static final String FIELD_1_OID = "field1Oid";

    /**
     * Name for the "field 2 OID" parameter. The value is a String.
     */
    public static final String FIELD_2_OID = "field2Oid";

    /**
     * Name for the "field 3 OID" parameter. The value is a String.
     */
    public static final String FIELD_3_OID = "field3Oid";

    /**
     * Name for the "preview only" parameter. The value is a Boolean.
     */
    public static final String PREVIEW_ONLY = "previewOnly";

    /**
     * Name for the "son relationship" parameter. The value is a String.
     */
    public static final String SON_RELATIONSHIP_CODE = "sonRelationship";

    private Map<String, String> m_contactRelationships = null;
    private Map<String, Family> m_existingFamiliesById = null;
    private LinkedHashMap<String, Collection<SisPerson>> m_familyMembersById = null;
    private List<String> m_matchFields = null;

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        QueryByCriteria query = getPeopleQuery();

        int count = getBroker().getCount(query);
        m_familyMembersById = new LinkedHashMap<String, Collection<SisPerson>>((int) (count * 1.2));

        QueryIterator people = getBroker().getIteratorByQuery(query);
        try {
            while (people.hasNext()) {
                SisPerson person = (SisPerson) people.next();

                String familyId = getFamilyId(person);

                Collection<SisPerson> members = m_familyMembersById.get(familyId);
                if (members == null) {
                    members = new LinkedList<SisPerson>();
                    m_familyMembersById.put(familyId, members);
                }

                members.add(person);
            }
        } finally {
            people.close();
        }

        buildFamilies();
    }

    /**
     * Initialize.
     *
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        loadContactRelationships();
        loadExistingFamilies();
        loadMatchFields();
    }

    /**
     * Builds the family records based on the member map. The results are logged to the output file.
     * Single-member families are excluded.
     * <p>
     * If this procedure is being run in "preview only" mode then the actual beans won't be created,
     * only the results will be displayed.
     */
    private void buildFamilies() {
        boolean createBeans = !((Boolean) getParameter(PREVIEW_ONLY)).booleanValue();

        int createCount = 0;
        int singleMemberCount = 0;

        for (String familyId : m_familyMembersById.keySet()) {
            Collection<SisPerson> people = m_familyMembersById.get(familyId);

            Family family = m_existingFamiliesById.get(familyId);

            boolean singleMember = false;
            if (people.size() > 1 || family != null) {
                if (createBeans) {
                    if (family == null) {
                        family = X2BaseBean.newInstance(Family.class, getBroker().getPersistenceKey());

                        OrganizationManager.setOrganizationOids(family, getOrganization());
                        family.setFamilyId(familyId);
                        family.setAddressOid(null); // TODO: Select an address OID
                    }

                    for (SisPerson person : people) {
                        FamilyMember member =
                                X2BaseBean.newInstance(FamilyMember.class, getBroker().getPersistenceKey());

                        member.setPersonOid(person.getOid());
                        member.setRelationshipCode(getRelationship(person));

                        family.addToFamilyMembers(member);
                    }

                    getBroker().saveBeanForced(family);
                }

                createCount++;
            } else {
                singleMember = true;
                singleMemberCount++;
            }

            /*
             * Write the details for this family and its members to the log
             */
            logMessage(familyId + " - " + people.size() + " new member(s)" + (singleMember ? " - SKIPPED" : ""));
            for (SisPerson person : people) {
                String memberMessage = "    " + person.getNameView() + " - " + getRelationship(person);

                String functions = person.getFunctionsView();
                if (!StringUtils.isEmpty(functions)) {
                    memberMessage = memberMessage + " [" + functions + "]";
                }

                logMessage(memberMessage);
            }

            logMessage(""); // Add a blank line
        }

        logMessage(createCount + " families " + (createBeans ? "created" : "to create"));
        logMessage(singleMemberCount + " single member families " + (createBeans ? "skipped" : "to skip"));
    }

    /**
     * Returns the family ID for the given person based on the selected match fields.
     *
     * @param person SisPerson
     * @return String
     * @throws X2BaseException exception
     */
    private String getFamilyId(SisPerson person) throws X2BaseException {
        StringBuilder familyId = new StringBuilder(128);

        for (String field : m_matchFields) {
            ModelProperty modelProperty = new ModelProperty(SisPerson.class, field, getBroker().getPersistenceKey());

            String value = WebUtils.getPropertyAsString(person, modelProperty, getLocale());

            familyId.append(' ');
            familyId.append(value.replaceAll("\\W", ""));
        }

        /*
         * Family ID is only 50 characters
         */
        String id = familyId.toString().trim();
        id = StringUtils.padRight(id, 50).trim();

        return id;
    }

    /**
     * Returns the query for retrieving people who are not already part of a family.
     *
     * @return A QueryByCriteria rooted at Person
     */
    private QueryByCriteria getPeopleQuery() {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(SisPerson.COL_ORGANIZATION1_OID, getOrganization().getOid());

        /*
         * Make sure the match fields aren't empty
         */
        for (String field : m_matchFields) {
            criteria.addNotEmpty(field, getBroker().getPersistenceKey());
        }

        /*
         * Exclude people that are already in a family
         */
        SubQuery familyMemberQuery = new SubQuery(FamilyMember.class, FamilyMember.COL_PERSON_OID, null);

        criteria.addNotIn(X2BaseBean.COL_OID, familyMemberQuery);

        QueryByCriteria query = new QueryByCriteria(SisPerson.class, criteria);

        /*
         * Order first by the match fields and then by the name. We only add the last or first name
         * if they're not already match fields since some databases, like SQL Server, do not allow
         * duplicate sort fields.
         */
        boolean foundFirstName = false;
        boolean foundLastName = false;
        for (String field : m_matchFields) {
            query.addOrderByAscending(field);

            if (field.equals(SisPerson.COL_FIRST_NAME)) {
                foundFirstName = true;
            }

            if (field.equals(SisPerson.COL_LAST_NAME)) {
                foundLastName = true;
            }
        }

        if (!foundLastName) {
            query.addOrderByAscending(SisPerson.COL_LAST_NAME);
        }

        if (!foundFirstName) {
            query.addOrderByAscending(SisPerson.COL_FIRST_NAME);
        }

        return query;
    }

    /**
     * Returns the family member relationship code for the given person. The following algorithm is
     * used:
     * <ul>
     * <li>If the person is a student then either SON_RELATIONSHIP_CODE or DAUGHTER_RELTIONSHIP code
     * is used based on the person's gender code
     * <li>If the person is a contact then the most popular StudentContact relationship code is used
     * (if one exists)
     * <li>If no relationship has been determined then the DEFAULT_RELATIONSHIP_CODE is used
     * </ul>
     *
     * @param person SisPerson
     * @return String
     */
    private String getRelationship(SisPerson person) {
        String relationship = null;

        if (person.getStudentIndicator() && !StringUtils.isEmpty(person.getGenderCode())) {
            char gender = person.getGenderCode().toUpperCase().charAt(0);
            if (gender == 'F' || gender == 'G') // Female or Girl
            {
                relationship = (String) getParameter(DAUGHTER_RELATIONSHIP_CODE);
            } else if (gender == 'M' || gender == 'B') // Male or Boy
            {
                relationship = (String) getParameter(SON_RELATIONSHIP_CODE);
            }
        } else if (person.getContactIndicator()) {
            relationship = m_contactRelationships.get(person.getOid());
        }

        if (relationship == null) {
            relationship = (String) getParameter(DEFAULT_RELATIONSHIP_CODE);
        }

        return relationship;
    }

    /**
     * Loads the most popular relationship codes for people who act as student contacts.
     */
    private void loadContactRelationships() {
        X2Criteria criteria = new X2Criteria();
        criteria.addNotEmpty(StudentContact.COL_RELATIONSHIP_CODE, getBroker().getPersistenceKey());

        String[] attributes = new String[] {StudentContact.REL_CONTACT + "." + Contact.COL_PERSON_OID,
                StudentContact.COL_RELATIONSHIP_CODE,
                "count(*)"};

        ReportQueryByCriteria query = new ReportQueryByCriteria(StudentContact.class, attributes, criteria);

        query.addGroupBy(StudentContact.REL_CONTACT + "." + Contact.COL_PERSON_OID);
        query.addGroupBy(StudentContact.COL_RELATIONSHIP_CODE);

        /*
         * The ascending sort order on the count means that the most popular code will be added last
         * overriding the less popular codes.
         */
        query.addOrderByAscending("count(*)");
        query.addOrderByAscending(StudentContact.COL_RELATIONSHIP_CODE);

        int count = getBroker().getCount(query);
        m_contactRelationships = new HashMap<String, String>((int) (count * 1.2));

        ReportQueryIterator results = getBroker().getReportQueryIteratorByQuery(query);
        try {
            while (results.hasNext()) {
                Object[] row = (Object[]) results.next();

                String personOid = (String) row[0];
                String relationshipCode = (String) row[1];

                m_contactRelationships.put(personOid, relationshipCode);
            }
        } finally {
            results.close();
        }
    }

    /**
     * Loads the existing families into a map keyed on ID.
     */
    private void loadExistingFamilies() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(Family.COL_ORGANIZATION1_OID, getOrganization().getOid());

        QueryByCriteria query = new QueryByCriteria(Family.class, criteria);

        int count = getBroker().getCount(query);

        m_existingFamiliesById = getBroker().getMapByQuery(query, Family.COL_FAMILY_ID, (int) (count * 1.2));
    }

    /**
     * Loads the match fields specified by the user input.
     */
    private void loadMatchFields() {
        List<String> oids = new ArrayList<String>(3);

        String districtFieldOid1 = (String) getParameter(FIELD_1_OID);
        oids.add(districtFieldOid1);

        String districtFieldOid2 = (String) getParameter(FIELD_2_OID);
        if (!StringUtils.isEmpty(districtFieldOid2)) {
            oids.add(districtFieldOid2);
        }

        String districtFieldOid3 = (String) getParameter(FIELD_3_OID);
        if (!StringUtils.isEmpty(districtFieldOid3)) {
            oids.add(districtFieldOid3);
        }

        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getUser().getPersistenceKey());
        m_matchFields = new ArrayList<String>(oids.size());

        Criteria criteria = new Criteria();
        criteria.addEqualTo(DataFieldConfig.COL_ORGANIZATION1_OID, getOrganization().getOid());
        criteria.addIn(X2BaseBean.COL_OID, oids);

        QueryByCriteria query = new QueryByCriteria(DataFieldConfig.class, criteria);
        QueryIterator fields = getBroker().getIteratorByQuery(query);
        try {
            while (fields.hasNext()) {
                DataFieldConfig field = (DataFieldConfig) fields.next();
                DataDictionaryField dictionaryField = dictionary.findDataDictionaryField(field);

                String matchField = dictionaryField.getJavaName();

                if (dictionaryField.getTable().getBeanClass().equals(SisAddress.class)) {
                    matchField = SisPerson.REL_PHYSICAL_ADDRESS + "." + matchField;
                }

                m_matchFields.add(matchField);
            }
        } finally {
            fields.close();
        }
    }
}
