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
package com.x2dev.reports.sys.sped.il;

import static com.x2dev.reports.sys.sped.il.IlSpedHelper.KEY_DATASOURCE;
import static com.x2dev.reports.sys.sped.il.IlSpedHelper.KEY_FORMAT;
import static com.x2dev.reports.sys.sped.il.IlSpedHelper.KEY_REL_SERV_OUTSIDE_GENERAL_ED;
import static com.x2dev.reports.sys.sped.il.IlSpedHelper.KEY_SPED_OUTSIDE_GENERAL_ED;
import static com.x2dev.reports.sys.sped.il.IlSpedHelper.KEY_SPED_REL_SERV_WITHIN_GENERAL_ED;
import static com.x2dev.reports.sys.sped.il.IlSpedHelper.SUB_REPORT_ID_PQ1;
import static com.x2dev.reports.sys.sped.il.IlSpedHelper.SUB_REPORT_ID_PQ2;
import com.follett.fsc.core.k12.beans.FormDefinition;
import com.follett.fsc.core.k12.beans.FormInstance;
import com.follett.fsc.core.k12.beans.GenericFormData;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.beans.StudentContact;
import com.follett.fsc.core.k12.beans.StudentSchool;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import com.x2dev.reports.sys.sped.il.IlSpedHelper.FactsSectionType;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepTeamMember;
import com.x2dev.sis.model.beans.SisAddress;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Java source for a form-based report. All fields and aliases present on the form storage and owner
 * objects are available for use in the format. The storage and owner objects are retrieved and made
 * available by the superclass - <code>BaseFormReportJavaSource</code>.
 * <p>
 * In the report format, fields and aliases from the storage object can be accessed directly. To
 * retrieve values from the owner object, the prefix <b>"owner"</b> must be present on the field
 * or alias. See <code>SimpleFormDataSource</code> for more information.
 *
 * @author X2 Development Corporation
 */
public class Facts extends BaseFormReportJavaSource {

    /**
     * The Class FactsDataSource.
     */
    public class FactsDataSource extends SimpleFormDataSource {

        private static final String NEXT_YEAR_POSTFIX = "-ny";
        private static final String LANGUAGE_ALIAS = "owner.student.DOE SCHOOL LANG";
        private static final String LANGUAGE_ALIAS_NEXT_YEAR = "fts-school-lang-ny";
        private List<String> m_aliases = new ArrayList<String>(Arrays.asList("fts-date-complited",
                "fts-private-facility", "fts-biling-sev", "fts-term", "fts-fund", "fts-ed-env", "fts-foster-guardian",
                "fts-foster-res-type", "fts-foster-agent", "fts-school-lang", "fts-begin-date"));
        private FactsSectionType m_type = null;

        /**
         * Instantiates a new facts data source.
         *
         * @param formStorage X2BaseBean
         * @param formOwner X2BaseBean
         * @param dictionary DataDictionary
         * @param locale Locale
         * @param type FactsSectionType
         */
        FactsDataSource(X2BaseBean formStorage, X2BaseBean formOwner, DataDictionary dictionary, Locale locale,
                FactsSectionType type) {
            super(formStorage, formOwner, dictionary, locale);
            m_type = type;
        }


        /**
         * Gets the field value.
         *
         * @param fieldName String
         * @return Object
         * @throws X2BaseException exception
         * @see
         *      com.follett.fsc.core.k12.tools.reports.BeanDataSource#getFieldValue(java.lang.String)
         */
        @Override
        protected Object getFieldValue(String fieldName) throws X2BaseException {
            String resultFieldName = fieldName;
            if (m_type.equals(FactsSectionType.NEXT)) {
                if (fieldName.startsWith(ALIAS_PREFIX)) {
                    String alias = fieldName.substring(ALIAS_PREFIX.length());
                    if (m_aliases.contains(alias)) {
                        resultFieldName = ALIAS_PREFIX + alias + NEXT_YEAR_POSTFIX;
                    }
                    // create map for case when we cannot use NEXT_YEAR_POSTFIX if appear more than
                    // this case, maybe put aliases into constructor for FactsSectionType like it
                    // work for PQSectionType
                    else if (alias.equals(LANGUAGE_ALIAS)) {
                        resultFieldName = ALIAS_PREFIX + LANGUAGE_ALIAS_NEXT_YEAR;
                    }

                }

            }
            return super.getFieldValue(resultFieldName);
        }



    }

    private static final String ALIAS_DATE = "date";
    private static final String ALIAS_LEP_EVIDENCE = "iep-lep-evidence";
    private static final String ALIAS_LEP_IND = "DOE LEP IND";
    private static final String ALIAS_SPEC_TRANSP_BETWEEN_SCH = "ed-env-spec-transp-between-sch";

    private static final String DASH = "-";
    private static final String DECIMAL = ", ";
    private static final String DECIMAL2 = "; ";

    private static final String EMPTY = "";
    private static final String EXTENDED_DICTIONARY_ID_3457BC = "SPED-IL-3457B/C";
    private static final String EXTENDED_DICTIOANRY_ID_SPED_IL_IEP = "SPED-IL-IEP";

    private static final String FORM_DEFINITION_3457BC = EXTENDED_DICTIONARY_ID_3457BC;

    private static final String KEY_RESIDENT_DISTRICT = "residentDistrict";
    private static final String KEY_RESIDENT_SCHOOL = "residentSchool";
    private static final String KEY_SERVING_DISTRICT = "servingDistrict";
    private static final String KEY_SERVING_SCHOOL = "servingSchool";

    private static final String NO = "No";

    private static final String PARAM_DATE_OF_PARENT_CONSENT = "dateOfParentConsent";
    private static final String PARAM_ETHNIC = "ethnic";
    private static final String PARAM_EXIT_REASON = "exitReason";
    private static final String PARAM_NEED_TRANSPORTATION = "needTransportation";
    private static final String PARAM_PARENT_ADRESSES1 = "parentAdresses1";
    private static final String PARAM_PARENT_ADRESSES2 = "parentAdresses2";
    private static final String PARAM_PARENT_NAMES = "parentNames";
    private static final String PARAM_PARENT_PHONE = "parentPhone";
    private static final String PARAM_PRIMARY_DISABILITY = "primaryDisability";
    private static final String PARAM_PRIMARY_DSBL_LOCAL_CODE = "primaryDsblLocalCode";
    private static final String PARAM_RELATION_CODE = "relationCode";
    private static final String PARAM_SECONDARY_DISABILITY = "secondaryDisability";
    private static final String PARAM_SECONDARY_DSBL_LOCAL_CODE = "secondaryDsblLocalCode";
    private static final String PARENT = "Parent";
    private static final String GUARDIAN = "Guardian";

    private static final String SPACE = " ";

    private static final String SUB_REPORT_ID_FACTS_SEC = "SYS-SPED-IL-FACTS-SC";

    private static final String YES = "Yes";

    private IlSpedHelper m_ilSpedHelper = new IlSpedHelper();
    private static final long serialVersionUID = 1L;



    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        // initialize data
        ReportDataGrid grid = new ReportDataGrid();
        IepData iepData = (IepData) getFormOwner();
        GenericFormData genericFormData = (GenericFormData) getFormStorage();
        if (iepData != null && genericFormData != null && genericFormData.getOid() != null) {
            m_ilSpedHelper.initializeHelper(getBroker(), EXTENDED_DICTIOANRY_ID_SPED_IL_IEP);


            // fillParameters
            fillDateOfParentConsent(iepData);
            fillLimitedEnglishProficiency(iepData);
            fillParentInformation(iepData);
            fillDisabilities(iepData);
            fillNeedTransportation(iepData);

            // fill other parameter using helper

            PlainDate date = new PlainDate(getFormInstance().getCreatedTime());
            StudentSchool lastOutplacement = m_ilSpedHelper.getLastOutplacement(iepData.getStudent(), null, date);
            StudentEnrollment stdEnrollment = m_ilSpedHelper.getLastStudentEnrollment(iepData.getStudent());

            addParameter(KEY_SERVING_DISTRICT, m_ilSpedHelper.getServingDistrict(lastOutplacement, stdEnrollment));
            addParameter(KEY_SERVING_SCHOOL, m_ilSpedHelper.getServingSchool(lastOutplacement, stdEnrollment));
            addParameter(KEY_RESIDENT_SCHOOL, m_ilSpedHelper.getResidentSchool(stdEnrollment));
            addParameter(KEY_RESIDENT_DISTRICT, m_ilSpedHelper.getResidentDistrict(stdEnrollment));


            addParameter(PARAM_EXIT_REASON, getExitReasonCodeAndDecription(iepData));
            addParameter(PARAM_ETHNIC, m_ilSpedHelper.calculateEthnic(iepData.getStudent().getPerson()));

            fillSection(iepData, grid, getParameters(), FactsSectionType.CURRENT, false);
            fillSection(iepData, grid, getParameters(), FactsSectionType.NEXT, false);
        } else {
            fillSection(iepData, grid, getParameters(), FactsSectionType.CURRENT, true);
        }
        grid.beforeTop();
        return grid;
    }

    /**
     * facts report consists from current year and next year sections
     * this method prepare subreport for this sections.
     *
     * @param iepData IepData
     * @param grid ReportDataGrid
     * @param map Map<String,Object>
     * @param type FactsSectionType
     * @param isBlank boolean
     */
    public void fillSection(IepData iepData,
                            ReportDataGrid grid,
                            Map<String, Object> map,
                            FactsSectionType type,
                            boolean isBlank) {
        Map<String, Object> mapForSection = new HashMap<String, Object>(map);
        mapForSection.put("title", type.getTitle());
        Report subreportSec = ReportUtils.getReport(SUB_REPORT_ID_FACTS_SEC, getBroker());
        ByteArrayInputStream formatSec = new ByteArrayInputStream(subreportSec.getCompiledFormat());
        JRDataSource subgridSec = new FactsDataSource(getFormStorage(), iepData, getDictionary(), getLocale(), type);

        if (!isBlank) {
            m_ilSpedHelper.fillParticipantServicesBlock(iepData, mapForSection, type.getPQSectionType());

        } else {
            fillEmptySubReportsIfRunBlank(mapForSection);
        }

        grid.append();
        grid.set("datasource", subgridSec);
        grid.set("format", formatSec);
        grid.set("PARAMETERS_MAP", mapForSection);


    }

    /**
     * If StringBuilder or String is not empty, add decimal on the end .
     *
     * @param decimal String
     * @param objects it is list of StringBuilder and/or String
     */
    private void addDecimalIfNotEmpty(String decimal, Object... objects) {
        for (Object object : objects) {
            if (object instanceof StringBuilder) {
                StringBuilder builder = (StringBuilder) object;
                if (builder != null && builder.length() > 0) {
                    builder.append(decimal);
                }
            } else if (object instanceof String) {
                String someString = (String) object;
                if (!StringUtils.isEmpty(someString)) {
                    someString += decimal;
                }
            }
        }
    }

    /**
     * Concatenation three addresses from SisAddress<br>
     * and pull in StringBuilder adress.
     *
     * @param adress StringBuilder
     * @param sisAddress SisAddress
     */
    private void fillAdress(StringBuilder adress, SisAddress sisAddress) {
        if (sisAddress != null) {
            adress.append(sisAddress.getAddressLine01() == null ? EMPTY : (sisAddress.getAddressLine01() + SPACE));
            adress.append(sisAddress.getAddressLine02() == null ? EMPTY : (sisAddress.getAddressLine02() + SPACE));
            adress.append(sisAddress.getAddressLine03() == null ? EMPTY : (sisAddress.getAddressLine03()));
        }
    }

    /**
     * Add parameter for "Date of Parent Consent for Initial Eval." field on report FACTS<br>
     * parameter key is dateOfParentConsent
     *
     * @param iepData IepData
     */
    private void fillDateOfParentConsent(IepData iepData) {
        String dateOfParentConsent = null;
        FormInstance formInstance3457BC = getFormInstance3457BC(iepData);
        if (formInstance3457BC != null) {
            GenericFormData formData3457BC = (GenericFormData) formInstance3457BC.getStorageObject();
            DataDictionary dictionary3457BC =
                    m_ilSpedHelper.getDictionaryByExtendedDictionaryId(EXTENDED_DICTIONARY_ID_3457BC);
            String dateOfPrntConsent = (String) formData3457BC.getFieldValueByAlias(ALIAS_DATE, dictionary3457BC);
            dateOfParentConsent = m_ilSpedHelper.formatDate(dateOfPrntConsent);
        }
        addParameter(PARAM_DATE_OF_PARENT_CONSENT, dateOfParentConsent == null ? EMPTY : dateOfParentConsent);
    }


    /**
     * Add parameters for "Disabilities" block on report FACTS<br>
     * parameter keys are<br>
     * primaryDisability<br>
     * primaryDsblLocalCode<br>
     * secondaryDisability<br>
     * secondaryDsblLocalCode.
     *
     * @param iepData IepData
     */
    private void fillDisabilities(IepData iepData) {
        Map<String, String> dsblMap = m_ilSpedHelper.getDisabilities(iepData, true);
        addParameter(PARAM_PRIMARY_DISABILITY, dsblMap.get("primaryName"));
        addParameter(PARAM_PRIMARY_DSBL_LOCAL_CODE, dsblMap.get("primaryCode"));
        addParameter(PARAM_SECONDARY_DISABILITY, dsblMap.get("secondaryNames"));
        addParameter(PARAM_SECONDARY_DSBL_LOCAL_CODE, dsblMap.get("secondaryCodes"));
    }


    /**
     * Fill empty sub reports if run blank.
     *
     * @param map Map<String,Object>
     */
    private void fillEmptySubReportsIfRunBlank(Map<String, Object> map) {
        Report subSubReport = ReportUtils.getReport(SUB_REPORT_ID_PQ1, getBroker());
        Report subSubReport2 = ReportUtils.getReport(SUB_REPORT_ID_PQ2, getBroker());
        ByteArrayInputStream subFormatformat = new ByteArrayInputStream(subSubReport.getCompiledFormat());
        ByteArrayInputStream subFormatformat2 = new ByteArrayInputStream(subSubReport2.getCompiledFormat());


        Map<String, JRDataSource> datasourceMap = new HashMap<String, JRDataSource>();
        Map<String, InputStream> formatMap = new HashMap<String, InputStream>();
        List<String> subSubKeys = new ArrayList<String>(Arrays.asList(KEY_SPED_REL_SERV_WITHIN_GENERAL_ED,
                KEY_SPED_OUTSIDE_GENERAL_ED, KEY_REL_SERV_OUTSIDE_GENERAL_ED));
        for (String servicesKey : subSubKeys) {
            ReportDataGrid grid = new ReportDataGrid();

            grid.append();
            grid.beforeTop();

            datasourceMap.put(servicesKey, grid);
            if (servicesKey.equals(KEY_REL_SERV_OUTSIDE_GENERAL_ED)
                    || servicesKey.equals(KEY_SPED_REL_SERV_WITHIN_GENERAL_ED)) {
                formatMap.put(servicesKey, subFormatformat2);
            } else {
                formatMap.put(servicesKey, subFormatformat);
            }

        }
        map.put(KEY_DATASOURCE, datasourceMap);
        map.put(KEY_FORMAT, formatMap);
    }

    /**
     * Add parameter for "Limited English Proficiency" field on report FACTS<br>
     * parameter key is "elig-lep-evidence".
     *
     * @param iepData IepData
     */
    private void fillLimitedEnglishProficiency(IepData iepData) {
        String lepInd = (String) iepData.getStudent().getFieldValueByAlias(ALIAS_LEP_IND);

        if (lepInd != null && lepInd.equals(BooleanAsStringConverter.TRUE)) {
            addParameter(ALIAS_LEP_EVIDENCE, YES);
        } else {
            addParameter(ALIAS_LEP_EVIDENCE, NO);
        }
    }


    /**
     * Add parameter for "Special Education Transportation" field on report FACTS<br>
     * parameter key is "needTransportation".
     *
     * @param iepData IepData
     */
    private void fillNeedTransportation(IepData iepData) {
        String needTransportation = null;
        DataDictionary ilIEPdictionary =
                m_ilSpedHelper.getDictionaryByExtendedDictionaryId(EXTENDED_DICTIOANRY_ID_SPED_IL_IEP);
        needTransportation = (String) iepData.getFieldValueByAlias(ALIAS_SPEC_TRANSP_BETWEEN_SCH, ilIEPdictionary);

        needTransportation = needTransportation == null ? EMPTY
                : (needTransportation.equals(BooleanAsStringConverter.TRUE) ? YES : NO);
        addParameter(PARAM_NEED_TRANSPORTATION, needTransportation);
    }


    /**
     * Add parameters with Parent Information<br>
     * in this case parents are the<br>
     * IEP team members with role "Parent".<br>
     * parameters keys are:<br>
     * parentNames<br>
     * parentAdresses1<br>
     * parentAdresses2<br>
     * relationCode<br>
     * parentPhone<br>
     *
     * @param iepData IepData
     */
    private void fillParentInformation(IepData iepData) {
        StringBuilder address1 = new StringBuilder();
        StringBuilder address2 = new StringBuilder();
        StringBuilder relationCode = new StringBuilder();
        StringBuilder parentPhone = new StringBuilder();
        StringBuilder parentNames = new StringBuilder();
        for (IepTeamMember iepTeamMember : iepData.getTeamMembers()) {
            if (iepTeamMember.getMemberRoleCode().equals(PARENT)
                    || iepTeamMember.getMemberRoleCode().equals(GUARDIAN)) {

                addDecimalIfNotEmpty(DECIMAL, parentPhone);
                addDecimalIfNotEmpty(DECIMAL2, parentNames);
                parentNames.append(iepTeamMember.getNameView());
                SisAddress sisAddress = iepTeamMember.getPerson().getPhysicalAddress();
                if (address1.length() == 0) {
                    fillAdress(address1, sisAddress);
                } else {
                    fillAdress(address2, sisAddress);
                }

                parentPhone.append(iepTeamMember.getPerson().getPhone01());
                for (StudentContact studentContact : iepData.getStudent().getContacts()) {
                    if (studentContact.getPerson().getOid().equals(iepTeamMember.getPersonOid())) {
                        addDecimalIfNotEmpty(DECIMAL, relationCode);
                        relationCode.append(studentContact.getRelationshipCode());

                        break;
                    }
                }
            }
        }
        addParameter(PARAM_PARENT_NAMES, parentNames.toString());
        addParameter(PARAM_PARENT_ADRESSES1, address1.toString());
        addParameter(PARAM_PARENT_ADRESSES2, address2.toString());
        addParameter(PARAM_RELATION_CODE, relationCode.toString());
        addParameter(PARAM_PARENT_PHONE, parentPhone.toString());
    }

    /**
     * return string exit reason code and description.
     *
     * @param iepData IepData
     * @return String
     */
    private String getExitReasonCodeAndDecription(IepData iepData) {
        StringBuilder exitReason = new StringBuilder();
        exitReason.append(
                StringUtils.isEmpty(iepData.getExitReason()) ? EMPTY : iepData.getExitReason() + SPACE + DASH + SPACE);
        String reasonDetail = m_ilSpedHelper.lookupReferenceCodeByBeanPath(
                IepData.class,
                IepData.COL_EXIT_REASON,
                iepData.getExitReason(),
                IlSpedHelper.REFERENCE_LOOKUP_CODE_DESCRIPTION);
        exitReason.append(reasonDetail == null ? EMPTY : reasonDetail);
        return exitReason.toString();
    }


    /**
     * Gets the form instance 3457 BC.
     *
     * @param iepData IepData
     * @return FormInstance based on iepData.<br>
     *         If iepData has only one formInstance with<br>
     *         fromDefinitionId SPED-IL-3457F
     */
    private FormInstance getFormInstance3457BC(IepData iepData) {
        FormInstance returnValue = null;
        Criteria formInstCriteria = new Criteria();
        formInstCriteria.addEqualTo(FormInstance.COL_OWNER_OBJECT_OID, iepData.getOid());
        formInstCriteria.addEqualTo(FormInstance.REL_FORM_DEFINITION + ModelProperty.PATH_DELIMITER +
                FormDefinition.COL_ID, FORM_DEFINITION_3457BC);
        QueryByCriteria formInstQuery = new QueryByCriteria(FormInstance.class, formInstCriteria);
        Collection<FormInstance> formInstances = getBroker().getCollectionByQuery(formInstQuery);
        if (!formInstances.isEmpty()) {
            if (formInstances.size() == 1) {
                returnValue = formInstances.iterator().next();
            }
        }
        return returnValue;
    }


}
