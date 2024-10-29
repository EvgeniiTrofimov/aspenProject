/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2018 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.reports.statereporting.md;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.*;
import com.follett.fsc.core.k12.beans.DataAudit.ChangeType;
import com.follett.fsc.core.k12.business.AuditXmlManager;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.SisUser;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Data source for the "Annual Secondary School Attendance" report for Allegany.
 * <p>
 * This report has been "highly" customized for Allegany and cannot be used for other districts
 * without significant
 * changes to the java source
 *
 * @author X2 Development Corporation
 */
public class StudentPersonDataSR1Side1Data extends ReportJavaSourceNet {

    /**
     * The Class EobGridData.
     */
    private class EobGridData {
        String m_personName;
        String m_staffType;
        Date m_date;

        /**
         * Instantiates a new EobGridData.
         *
         * @param dau DataAudit
         */
        protected EobGridData(DataAudit dau) {
            if (dau != null) {
                Person person = dau.getUser().getPerson();
                long dateModified = dau.getTimestamp();
                m_personName = person.getNameView();
                if (m_staffByUsr.containsKey(person.getOid())) {
                    Collection<SisStaff> staffList = m_staffByUsr.get(person.getOid());
                    if (staffList != null && !staffList.isEmpty()) {
                        m_staffType = staffList.iterator().next().getStaffType();
                    }
                }
                m_date = new Date(dateModified);
            }
        }
    }

    /**
     * The Class PadGridData.
     */
    private class PadGridData {
        PlainDate m_startDate;
        Boolean m_proofOfResidency;
        String m_responsibleName;
        String m_relationship;

        /**
         * Instantiates a new PadGridData.
         *
         * @param pad PersonAddress
         */
        protected PadGridData(PersonAddress pad) {
            if (pad != null) {
                m_startDate = pad.getStartDate();
                m_proofOfResidency = getLogicalByAlias(pad, "all-pad-ProofofResidency",
                        DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey()));
                m_responsibleName = (String) pad.getFieldValueByAlias("all-pad-ResponsibleAdult");
                m_relationship = (String) pad.getFieldValueByAlias("all-pad-RelationshiptoStudent");
            }
        }

        /**
         * Instantiates a new PadGridData.
         *
         * @param startDate Date
         */
        protected PadGridData(Date startDate) {
            m_startDate = new PlainDate(startDate);
        }
    }

    // INPUT PARAMETERS
    private static final String ACTIVE_ONLY_PARAM = "activeOnly";
    private static final String ADDRESS_TYPE_PHYSICAL = "Physical";
    private static final String COMMA = ",";
    private static final String EMPTY = "";
    private static final String SPACE = " ";
    private static final String STUDENT_SORT_PARAM = "studentSort";
    private static final String QUERY_STRING_PARAM = "queryString";
    private static final String QUERY_BY_PARAM = "queryBy";
    private static final String PARAM_RACE_CODES_REF_TABLE = "raceRefTable";
    private static final String STD_FIELD_EVIDENCE_OF_BIRTH = "all-std-EvidenceofBirth";
    private static final String STD_FIELD_PREFERRED_NAME = "all-std-PreferredName";
    private static final String TABLE_STD = "tblStudent";
    // GRID FIELDS
    private static final String FIELD_ADR_NUM = "adrNum";
    private static final String FIELD_EVIDENCE_OF_BIRTH_DATE = "evidenceOfBirthDate";
    private static final String FIELD_EVIDENCE_OF_BIRTH_TYPE = "evidenceOfBirthType";
    private static final String FIELD_GENDER_STATE_CODE = "genderStateCode";
    private static final String FIELD_PREFERRED_NAME = "preferredName";
    private static final String FIELD_PRINT_DETAIL = "printDetail";
    private static final String FIELD_PROOF_OF_RESIDENCY = "proofOfResidency";
    private static final String FIELD_RACES = "races";
    private static final String FIELD_RESPONSIBLE_NAMES = "responsibleNames";
    private static final String FIELD_RESPONSIBLE_RELATIONSHIPS = "responsibleRelationships";
    private static final String FIELD_STUDENT = "student";
    private static final String FIELD_STUDENT_ADDRESS = "studentAddress";
    private static final String FIELD_STUDENT_START_DATE = "studentStartDate";
    private static final String FIELD_STAFF_PERSON_NAME = "staffPersonName";
    private static final String FIELD_STAFF_TYPE = "staffType";

    /**
     * Class members
     */
    private SisStudent m_currentStudent;
    private Map<String, Collection<DataAudit>> m_dauByStd;
    private Map<String, Collection<PersonAddress>> m_padByStd;
    private Map<String, ReferenceCode> m_genderStateCodes;
    private Map<String, ReferenceCode> m_raceRefCodes = new HashMap<String, ReferenceCode>();
    private Map<String, ReferenceCode> m_reletationShipCodeMap = new HashMap<String, ReferenceCode>();
    private Map<String, Collection<SisStaff>> m_staffByUsr;
    private Collection<SisStudent> m_students;
    private String m_fieldEvidenceOfBirth;

    /**
     * Initialize.
     *
     * @see com.x2dev.sis.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        DataDictionary ddx = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());

        DataDictionaryField field =
                ddx.findDataDictionaryField(StudentContact.class.getName(), StudentContact.COL_RELATIONSHIP_CODE);
        ReferenceTable relationShipRefTabel = field.getReferenceTable();
        if (relationShipRefTabel != null) {
            for (ReferenceCode code : relationShipRefTabel.getReferenceCodes()) {
                if (!code.getDisabledIndicator()) {
                    m_reletationShipCodeMap.put(code.getCode(), code);
                }
            }
        }
    }

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.x2dev.sis.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        // -- load data needed to generate report --
        loadStudents();
        loadGenderStateCodeValues();
        loadRacesRefCodes();
        ReportDataGrid grid = new ReportDataGrid();
        for (SisStudent student : m_students) {
            LinkedHashMap<String, PadGridData> addrAndPadGridDataMap = getListOfAddresses(student);
            EobGridData eobGridData = getEobGridDataForStudent(student);
            if (!addrAndPadGridDataMap.isEmpty()) {
                int i = 1;
                for (Entry<String, PadGridData> entry : addrAndPadGridDataMap.entrySet()) {
                    PadGridData padGridData = entry.getValue();
                    ReferenceCode genderCode = m_genderStateCodes.get(student.getPerson().getGenderCode());
                    grid.append();
                    grid.set(FIELD_STUDENT, student);
                    grid.set(FIELD_GENDER_STATE_CODE, genderCode != null ? genderCode.getStateCode() : null);
                    grid.set(FIELD_ADR_NUM, Integer.valueOf(i));
                    grid.set(FIELD_STAFF_PERSON_NAME, eobGridData.m_personName);
                    grid.set(FIELD_STAFF_TYPE, eobGridData.m_staffType);
                    grid.set(FIELD_EVIDENCE_OF_BIRTH_DATE, eobGridData.m_date);
                    grid.set(FIELD_STUDENT_START_DATE, padGridData.m_startDate);
                    grid.set(FIELD_EVIDENCE_OF_BIRTH_DATE, eobGridData.m_date);
                    grid.set(FIELD_EVIDENCE_OF_BIRTH_TYPE, student.getFieldValueByAlias(STD_FIELD_EVIDENCE_OF_BIRTH));
                    grid.set(FIELD_PROOF_OF_RESIDENCY, padGridData.m_proofOfResidency);
                    grid.set(FIELD_RESPONSIBLE_NAMES, padGridData.m_responsibleName);
                    grid.set(FIELD_RESPONSIBLE_RELATIONSHIPS, padGridData.m_relationship);
                    grid.set(FIELD_STUDENT_ADDRESS, entry.getKey());
                    grid.set(FIELD_RACES, getStateRaces(student));
                    grid.set(FIELD_PREFERRED_NAME, student.getFieldValueByAlias(STD_FIELD_PREFERRED_NAME));
                    if (i > 4) {
                        grid.set(FIELD_PRINT_DETAIL, Boolean.FALSE);
                    } else {
                        grid.set(FIELD_PRINT_DETAIL, Boolean.TRUE);
                    }
                    i++;
                }
            }
        }
        // -- Sort the grid results --
        grid.beforeTop();
        return grid;
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @see com.x2dev.sis.tools.ToolJavaSource#saveState(com.x2dev.sis.web.UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) {
        /*
         * If we're in the context of a single student, print the report for just that student
         */
        m_currentStudent = (SisStudent) userData.getCurrentRecord(Student.class);
    }

    /**
     * Gets the adjusted address.
     *
     * @param addressView String
     * @param address Address
     * @return String
     */
    private String getAdjustedAddress(String addressView, Address address) {
        if (address != null) {
            addressView += COMMA + SPACE + address.getAddressLine03();
        }
        return addressView;
    }

    /**
     * Get Evidence of Birth DataAudit Record for Student.
     *
     * @param student SisStudent
     * @return Linked hash map
     */
    private EobGridData getEobGridDataForStudent(SisStudent student) {
        DataAudit dauEob = null;
        if (m_dauByStd.containsKey(student.getOid())) {
            Collection<DataAudit> daus = m_dauByStd.get(student.getOid());
            for (DataAudit dau : daus) {
                HashMap<String, String> changedValues = new HashMap<String, String>();
                AuditXmlManager.parseAuditChangeDefinition(dau, new HashMap<String, String>(), changedValues);
                if (m_fieldEvidenceOfBirth == null) {
                    DataDictionaryField ddf = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey())
                            .findDataDictionaryFieldByAlias(STD_FIELD_EVIDENCE_OF_BIRTH);
                    m_fieldEvidenceOfBirth = ddf.getId();
                }
                if (changedValues.containsKey(m_fieldEvidenceOfBirth)) {
                    dauEob = dau;
                    break;
                }
            }
        }
        return new EobGridData(dauEob);
    }

    /**
     * Populate list of current address and change history.
     *
     * @param student SisStudent
     * @return Linked hash map
     */
    private LinkedHashMap<String, PadGridData> getListOfAddresses(SisStudent student) {
        LinkedHashMap<String, PadGridData> addrAndDateMap = new LinkedHashMap<>();
        if (m_padByStd.containsKey(student.getPersonOid())) {
            List<PersonAddress> pads = (List) m_padByStd.get(student.getPersonOid());
            if (pads != null) {
                for (PersonAddress pad : pads) {
                    Address address = pad.getAddress();
                    addrAndDateMap.put(getAdjustedAddress(address.getAddressLine01(), address),
                            new PadGridData(pad));
                }
            }
        }
        if (addrAndDateMap.isEmpty()) {
            String addrToPut = student.getAddressView();
            Date dateToPut = getStudentStartDate(student);
            if (!StringUtils.isEmpty(addrToPut)) {
                addrAndDateMap.put(getAdjustedAddress(addrToPut, student.getPerson().getPhysicalAddress()),
                        new PadGridData(dateToPut));
            }
        }
        return addrAndDateMap;
    }

    /**
     * Gets the logical by alias.
     *
     * @param baseBean X2BaseBean
     * @param alias String
     * @param dataDictionary DataDictionary
     * @return boolean
     */
    private boolean getLogicalByAlias(X2BaseBean baseBean, String alias, DataDictionary dataDictionary) {
        String fieldValue = (String) baseBean.getFieldValueByAlias(alias, dataDictionary);
        if (fieldValue == null) {
            fieldValue = BooleanAsStringConverter.FALSE;
        }
        boolean returnValue = fieldValue.equals(BooleanAsStringConverter.TRUE) ? true : false;
        return returnValue;
    }

    /**
     * Gets the state races.
     *
     * @param student SisStudent
     * @return List
     */
    private List<String> getStateRaces(SisStudent student) {
        List<String> racesList = new ArrayList<String>();
        for (Race race : student.getPerson().getRaces()) {
            String raceCode = race.getRaceCode();
            raceCode = raceCode == null ? EMPTY : raceCode;
            ReferenceCode refCode = m_raceRefCodes.get(raceCode);
            if (refCode != null) {
                racesList.add(refCode.getStateCode());
            }
        }
        return racesList;
    }

    /**
     * Gets the student start date.
     *
     * @param student SisStudent
     * @return Plain date
     */
    private PlainDate getStudentStartDate(SisStudent student) {
        PlainDate stdStartDate = null;
        for (StudentEnrollment enr : student.getEnrollments()) {
            if (enr.getEnrollmentType().equals(StudentEnrollment.ENTRY)) {
                PlainDate enrDate = enr.getEnrollmentDate();
                if (stdStartDate == null || stdStartDate.after(enrDate)) {
                    stdStartDate = enrDate;
                }
            }
        }
        return stdStartDate;
    }

    /**
     * Loads all audit records needed for this report into collection.
     *
     * @param stdOids Collection
     */
    private void loadDataAuditsStudents(Collection stdOids) {
        X2Criteria dauCriteria = new X2Criteria();
        dauCriteria.addIn(DataAudit.COL_OBJECT_OID, stdOids);
        dauCriteria.addIn(DataAudit.COL_CHANGE_TYPE,
                Arrays.asList(Integer.valueOf(ChangeType.CREATE.ordinal()),
                        Integer.valueOf(ChangeType.MODIFY.ordinal())));
        dauCriteria.addEqualTo(DataAudit.COL_TABLE_OID, TABLE_STD);
        dauCriteria.addNotNull(DataAudit.COL_CHANGE_DEFINITION);
        QueryByCriteria dauQuery = new QueryByCriteria(DataAudit.class, dauCriteria);
        dauQuery.addOrderByDescending(DataAudit.COL_TIMESTAMP);
        Collection<String> usrOids =
                getBroker().getGroupedCollectionByQuery(dauQuery, DataAudit.COL_USER_OID, 1024).keySet();
        loadStaffDataByUsrOids(usrOids);
        m_dauByStd = getBroker().getGroupedCollectionByQuery(dauQuery, DataAudit.COL_OBJECT_OID, 1024);
    }

    /**
     * Load gender codes.
     */
    private void loadGenderStateCodeValues() {
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        ModelProperty prop =
                new ModelProperty(SisPerson.class, SisPerson.COL_GENDER_CODE, getBroker().getPersistenceKey());
        DataDictionaryField field = dictionary.findDataDictionaryField(prop.getFieldId());
        ReferenceTable referenceTable = field.getReferenceTable();
        m_genderStateCodes = referenceTable.getCodeMap();
    }

    /**
     * Loads all person addresses needed for this report into collection.
     *
     * @param stdOids Collection
     */
    private void loadPersonAddrDataStudents(Collection stdOids) {
        X2Criteria stdCriteria = new X2Criteria();
        stdCriteria.addIn(X2BaseBean.COL_OID, stdOids);
        SubQuery psnOidSubquery = new SubQuery(SisStudent.class, SisStudent.COL_PERSON_OID, stdCriteria);
        X2Criteria padCriteria = new X2Criteria();
        padCriteria.addEqualTo(PersonAddress.COL_ADDRESS_TYPE, ADDRESS_TYPE_PHYSICAL);
        padCriteria.addIn(PersonAddress.COL_PERSON_OID, psnOidSubquery);
        QueryByCriteria padQuery = new QueryByCriteria(PersonAddress.class, padCriteria);
        padQuery.addOrderByDescending(PersonAddress.COL_START_DATE);
        m_padByStd = getBroker().getGroupedCollectionByQuery(padQuery, PersonAddress.COL_PERSON_OID, 1024);
    }

    /**
     * load map key - <code>code</code> value <code>ReferenceCode</code> for Races ref table.
     */
    private void loadRacesRefCodes() {
        String refTableOid = (String) getParameter(PARAM_RACE_CODES_REF_TABLE);
        if (!StringUtils.isEmpty(refTableOid)) {
            ReferenceTable table = (ReferenceTable) getBroker().getBeanByOid(ReferenceTable.class, refTableOid);
            if (table != null) {
                for (ReferenceCode refCode : table.getReferenceCodes()) {
                    if (!refCode.getDisabledIndicator()) {
                        m_raceRefCodes.put(refCode.getCode(), refCode);
                    }
                }
            }
        }
    }

    /**
     * Loads all staff needed for this report into collection.
     *
     * @param stdOids Collection
     */
    private void loadStaffDataByUsrOids(Collection usrOids) {
        X2Criteria usrCriteria = new X2Criteria();
        usrCriteria.addIn(X2BaseBean.COL_OID, usrOids);
        SubQuery psnOidSubquery = new SubQuery(SisUser.class, SisUser.COL_PERSON_OID, usrCriteria);
        X2Criteria stfCriteria = new X2Criteria();
        stfCriteria.addIn(SisStaff.COL_PERSON_OID, psnOidSubquery);
        QueryByCriteria stfQuery = new QueryByCriteria(SisStaff.class, stfCriteria);
        m_staffByUsr = getBroker().getGroupedCollectionByQuery(stfQuery, SisStaff.COL_PERSON_OID, 1024);
    }

    /**
     * Loads all students needed for this report into collection.
     */
    private void loadStudents() {
        Criteria criteria = new Criteria();
        if (m_currentStudent != null) {
            // running for one student
            criteria.addEqualTo(X2BaseBean.COL_OID, m_currentStudent.getOid());
        } else {
            // running for multiple students
            if (isSchoolContext()) {
                criteria.addEqualTo(SisStudent.COL_SCHOOL_OID, ((SisSchool) getSchool()).getOid());
            }
            boolean activeOnly = ((Boolean) getParameter(ACTIVE_ONLY_PARAM)).booleanValue();
            if (activeOnly) {
                String activeCode = PreferenceManager.getPreferenceValue(getOrganization(),
                        SystemPreferenceDefinition.STUDENT_ACTIVE_CODE);
                criteria.addEqualTo(SisStudent.COL_ENROLLMENT_STATUS, activeCode);
            }
            String queryBy = (String) getParameter(QUERY_BY_PARAM);
            String queryString = (String) getParameter(QUERY_STRING_PARAM);
            addUserCriteria(criteria, queryBy, queryString, Student.class, X2BaseBean.COL_OID);
        }
        QueryByCriteria query = new QueryByCriteria(SisStudent.class, criteria);
        String sortBy = (String) getParameter(STUDENT_SORT_PARAM);
        applyUserSort(query, sortBy);
        Collection<String> stdOids = getBroker().getGroupedCollectionByQuery(query, X2BaseBean.COL_OID, 1024).keySet();
        loadDataAuditsStudents(stdOids);
        loadPersonAddrDataStudents(stdOids);
        m_students = getBroker().getCollectionByQuery(query);
    }
}
