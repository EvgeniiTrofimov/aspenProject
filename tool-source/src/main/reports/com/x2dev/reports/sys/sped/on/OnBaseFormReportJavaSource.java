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
package com.x2dev.reports.sys.sped.on;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.*;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.OwnershipStateImpl;
import com.follett.fsc.core.k12.business.PublishReportsManager;
import com.follett.fsc.core.k12.business.ReferenceCodeRetriever;
import com.follett.fsc.core.k12.business.ReferenceCodeRetriever.ReferenceCodeCriteria;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.localization.LocalizationUtils;
import com.follett.fsc.core.k12.tools.JarPluginManager;
import com.follett.fsc.core.k12.tools.ResultHandler;
import com.follett.fsc.core.k12.tools.Tool;
import com.follett.fsc.core.k12.tools.ToolInput;
import com.follett.fsc.core.k12.tools.ToolJob;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.ReportConstants;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.follett.fsc.core.k12.web.presentation.ReferenceFieldFormatter;
import com.x2dev.sis.model.beans.*;
import com.x2dev.utils.CollectionUtils;
import com.x2dev.utils.DataGrid;
import com.x2dev.utils.FolderUtils;
import com.x2dev.utils.KeyValuePair;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.ThreadUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.DateAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FilterOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.regex.Pattern;
import net.sf.jasperreports5.engine.JRException;
import net.sf.jasperreports5.engine.JRExporterParameter;
import net.sf.jasperreports5.engine.JasperExportManager;
import net.sf.jasperreports5.engine.JasperFillManager;
import net.sf.jasperreports5.engine.JasperPrint;
import net.sf.jasperreports5.engine.export.JRCsvExporter;
import net.sf.jasperreports5.engine.export.JRHtmlExporter;
import net.sf.jasperreports5.engine.export.JRHtmlExporterParameter;
import net.sf.jasperreports5.engine.export.JRXlsExporter;
import net.sf.jasperreports5.engine.util.JRClassLoader;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.struts.util.MessageResources;

/**
 * The Class OnBaseFormReportJavaSource.
 *
 * @author Follett Software Company
 * @copyright 2019
 */
public class OnBaseFormReportJavaSource extends BaseFormReportJavaSource {
    private static final String ALIAS_IEP_IPRC_EFFECTIVE_DATE = "iep-iprc-effective-date";
    private static final String ALIAS_USE_PREFERRED_NAME = "all-std-usePreferredName";
    private static final String ALIAS_LEGAL_FIRST_NAME = "all-psn-LegalFirstName";
    private static final String ALIAS_LEGAL_LAST_NAME = "all-psn-LegalLastName";
    private static final String ALIAS_RCD_IMAGE_BASE64 = "all-rcd-ImageBase64";
    private static final String ALIAS_CTJ_GUARDIAN = "all-ctj-LegalGuardian";
    private static final String ALIAS_IMPLEMENTATION_SCHOOL = "iep-implementation-school";
    private static final String ALIAS_BSID_ELEMENTARY = "all-skl-DesignatedElementaryBSID";
    private static final String ALIAS_BSID_SECONDARY = "all-skl-DesignatedSecondaryBSID";
    private static final String ALIAS_BSID = "all-skl-BSID";


    private static final String LOGO_CODE_ON_BOARD = "OnBoardLogo";
    private static final String MEMBER_ROLE_PARENT = "Parent";
    private static final String MEMBER_ROLE_GUARDIAN = "Guardian";
    private static final String PARAM_LOGO = "logoOntario";
    private static final String PARAM_PARENTS = "parents";
    private static final String PARAM_PARENT_PHONE = "parentPhone";
    private static final String PARAM_SCHOOL = "school";
    private static final String PARAM_STUDENT_FIRSTNAME = "studentFname";
    private static final String PARAM_STUDENT_NAME = "studentName";
    private static final String PARAM_STUDENT_AGE = "studentAge";
    private static final String PARAM_SUB_DATA_STD_DETAIL = "stdDetailData";
    private static final String PARAM_TITLE = "title";
    private static final String PARAM_YEAR = "year";
    private static final String RTB_OID_ON_SIS_IMAGES = "rtbOnImage    ";// OnSIS Images
    private static final List<KeyValuePair<String, String>> BASE_SUB_REPORT_FORMATS = Arrays.asList(
            new KeyValuePair<String, String>("SYS-SPED-ON-STD-DET", "stdDetailFormat"));

    protected static final String SAVE_TO_DOCUMENTS_IEP_PARAM = "saveToDocumentsIEP";
    protected static final String DOCUMENT_NAME_PARAM = "documentName";
    protected static final String DOCUMENT_TYPE_PARAM = "documentType";
    protected static final String ALIAS_SCHOOL_ID = "cust-doc-schoolId";
    protected static final String ALIAS_UPLOAD_DATE = "cust-doc-uploadDate";
    protected static final String ALIAS_CASE_VISIBILITY = "all-doc-CaseVisibility";

    protected static final long MILLIS_PER_YEAR = 1000L * 60L * 60L * 24L * 365L;

    protected IepData m_iep;
    private File m_saveToDocumentsResultFile = null;

    /**
     * Gets the base 64 image string.
     *
     * @param imageCode String
     * @param broker X2Broker
     * @return String
     */
    public String getBase64ImageString(String imageCode, X2Broker broker) {
        String base64Image = "";

        X2Criteria imageCriteria = new X2Criteria();
        imageCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, RTB_OID_ON_SIS_IMAGES);
        imageCriteria.addEqualTo(ReferenceCode.COL_CODE, imageCode);
        BeanQuery imageQuery = new BeanQuery(ReferenceCode.class, imageCriteria);
        ReferenceCode rcdBean = broker.getBeanByQuery(imageQuery);
        if (rcdBean != null) {
            base64Image = (String) rcdBean.getFieldValueByAlias(ALIAS_RCD_IMAGE_BASE64);
        }
        return base64Image;
    }

    /**
     * Gather data.
     *
     * @return Object
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        addParameter(PARAM_TITLE, getTitle());
        addParameter(PARAM_SCHOOL, getImplementationSchool());
        addParameter(PARAM_LOGO, getBase64ImageString(LOGO_CODE_ON_BOARD, getBroker()));
        addParameter(PARAM_SUB_DATA_STD_DETAIL,
                new SimpleFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale()));
        addParameter(PARAM_YEAR, getIepYear());
        setStudentNameView();
        getParentsInformation();
        return null;
    }

    /**
     * Gets the collection from alias.
     *
     * @param pattern Pattern
     * @param bean X2BaseBean
     * @param alias String
     * @param dictionary DataDictionary
     * @return List
     */
    protected List<String> getCollectionFromAlias(Pattern pattern,
                                                  X2BaseBean bean,
                                                  String alias,
                                                  DataDictionary dictionary) {
        List<String> list = new ArrayList<String>(6);
        if (bean != null) {
            String value = (String) bean.getFieldValueByAlias(alias, dictionary);
            if (!StringUtils.isEmpty(value)) {
                list.addAll(Arrays.asList(pattern.split(value)));
            }
        }
        return list;
    }

    protected String getStringTranslationFromAlias(Pattern pattern,
                                                   X2BaseBean bean,
                                                   String alias,
                                                   DataDictionary dictionary,
                                                   MessageResources resources) {
        List<String> col = getCollectionTranslationFromAlias(pattern,
                bean, alias, dictionary, resources);
        return StringUtils.convertCollectionToDelimitedString(col, ",");
    }

    /**
     * Gets the collection from alias.
     *
     * @param pattern Pattern
     * @param bean X2BaseBean
     * @param alias String
     * @param dictionary DataDictionary
     * @return List
     */
    protected List<String> getCollectionTranslationFromAlias(Pattern pattern,
                                                             X2BaseBean bean,
                                                             String alias,
                                                             DataDictionary dictionary,
                                                             MessageResources resources) {
        List<String> list = new ArrayList<String>(6);
        if (bean != null) {
            DataDictionaryField ddf = getDictionary().findDataDictionaryFieldByAlias(alias);
            if (ddf.hasReferenceTable()) {
                ModelBroker broker = new ModelBroker(getPrivilegeSet());
                ModelProperty codeProperty =
                        new ModelProperty(ReferenceCode.class, ReferenceCode.COL_DESCRIPTION, getDictionary());
                ReferenceTable refTable = ddf.getReferenceTable();
                ReferenceCodeRetriever codeRetriever =
                        ReferenceCodeRetriever.getInstance(getBroker().getPersistenceKey());
                String fieldValue = (String) bean.getFieldValueByAlias(alias, dictionary);
                if (!StringUtils.isEmpty(fieldValue)) {

                    List<String> values = Arrays.asList(pattern.split(fieldValue));
                    for (String value : values) {
                        ReferenceCodeCriteria codeCriteria =
                                new ReferenceCodeCriteria(ddf.getReferenceTable(),
                                        ReferenceCodeRetriever.EXCLUDE_HIDDEN_CODES);
                        codeCriteria.addFieldCriteria(ReferenceCode.COL_CODE, value);
                        ReferenceCode code = codeRetriever.getCode(codeCriteria, null, broker);
                        String view = null;
                        if (code != null) {
                            view = ReferenceFieldFormatter.getReferenceTableViewFields(ddf.getReferenceTable(), code,
                                    broker,
                                    resources);
                        }
                        if (StringUtils.isEmpty(view)) {
                            view = LocalizationUtils.getRsmDataValue(resources, code, codeProperty,
                                    value);
                        }
                        if (view == null || view.startsWith("??")) {
                            if (code != null) {
                                view = code.getDescription();
                            } else {
                                view = value;
                            }
                        }
                        if (StringUtils.isEmpty(view)) {
                            view = value;
                        }
                        list.add(view);
                    }
                }
            }
        }
        return list;
    }

    /**
     * Gets the accommodations.
     *
     * @return Collection
     */
    protected Collection<IepAccommodation> getAccommodations() {
        Collection<IepAccommodation> values = Collections.EMPTY_LIST;
        if (getIep() != null) {
            values = getIep().getAccommodations();
        }
        return values;
    }

    /**
     * Returns the current IEP. If a blank form is being printed, a new (unsaved) IEP is created and
     * returned.
     *
     * @return IepData
     */
    protected IepData getIep() {
        if (m_iep == null && getFormOwner() != null) {
            if (isBlank()) {
                IepData ownerIep = (IepData) getFormOwner();

                m_iep = new IepData(getBroker().getPersistenceKey());
                m_iep.setStudentOid(ownerIep.getStudentOid());
                m_iep.setStaffOid(ownerIep.getStaffOid());
            } else {
                Object storageObject = getFormStorage();
                Object ownerObject = getFormOwner();
                if (storageObject instanceof IepData) {
                    m_iep = (IepData) storageObject;
                } else if (ownerObject instanceof IepData) {
                    m_iep = (IepData) ownerObject;
                } else if (storageObject instanceof IepMeeting) {
                    m_iep = ((IepMeeting) storageObject).getIepData();
                }
            }
        }

        return m_iep;
    }

    /**
     * Gets the iep disability.
     *
     * @return Collection
     */
    protected Collection<IepDisability> getIepDisability() {
        Collection<IepDisability> values = Collections.EMPTY_LIST;
        if (getIep() != null) {
            values = getIep().getIepDisability();
            values = CollectionUtils.sortBeans(values, IepDisability.COL_DISABILITY_CODE, false, true);
        }
        return values;
    }

    /**
     * Gets the iep goals.
     *
     * @return Collection
     */
    protected Collection<IepGoal> getIepGoals() {
        Collection<IepGoal> values = Collections.EMPTY_LIST;
        if (getIep() != null) {
            values = getIep().getIepGoals();
        }
        return values;
    }

    /**
     * Gets the iep performance level.
     *
     * @return Collection
     */
    protected Collection<IepPerformanceLevel> getIepPerformanceLevel() {
        Collection<IepPerformanceLevel> values = Collections.EMPTY_LIST;
        if (getIep() != null) {
            values = getIep().getIepPerformanceLevel();
        }
        return values;
    }

    /**
     * Gets the iep services.
     *
     * @return Collection
     */
    protected Collection<IepService> getIepServices() {
        Collection<IepService> values = Collections.EMPTY_LIST;
        if (getIep() != null) {
            values = getIep().getIepServices();
        }
        return values;
    }

    /**
     * Gets the parents information.
     * Parent is a team member who is also a student contact.
     * Parent must have a team relationship "Parent" or "Guardian"
     * or have the student contact "Legal Guardian" indicator set.
     *
     * @return String
     */
    protected void getParentsInformation() {
        List<String> parents = new ArrayList<>();
        String phone = null;

        Student student = null;
        Collection<IepTeamMember> team = null;
        X2BaseBean formStorage = getFormStorage();
        if (formStorage instanceof IepData) {
            team = ((IepData) formStorage).getTeamMembers();
            student = ((IepData) formStorage).getStudent();
        } else if (formStorage instanceof IepMeeting) {
            IepData iepData = ((IepMeeting) formStorage).getIepData();
            if (iepData != null) {
                team = iepData.getTeamMembers();
                student = iepData.getStudent();
            }
        }
        if (team != null) {
            DataDictionaryField field = getDictionary().findDataDictionaryFieldByAlias(ALIAS_CTJ_GUARDIAN);
            for (IepTeamMember teamMember : team) {
                String parent = null;
                if (MEMBER_ROLE_PARENT.equals(teamMember.getMemberRoleCode())
                        || MEMBER_ROLE_GUARDIAN.equals(teamMember.getMemberRoleCode())) {

                    parent = getParentInformation(teamMember, null);
                    if (!StringUtils.isEmpty(parent)) {
                        parents.add(parent);
                        if (StringUtils.isEmpty(phone)) {
                            phone = teamMember.getPerson().getPhone01();
                        }
                    }
                }
                if (parent == null) {
                    parent = getParentInformation(teamMember, field);
                    if (!StringUtils.isEmpty(parent)) {
                        parents.add(parent);
                        if (StringUtils.isEmpty(phone)) {
                            phone = teamMember.getPerson().getPhone01();
                        }
                    }
                }
            }
        }
        if (StringUtils.isEmpty(phone) && student != null) {
            phone = student.getPerson().getPhone01();
        }
        addParameter(PARAM_PARENTS, String.join("\n", parents.toArray(new String[parents.size()])));
        addParameter(PARAM_PARENT_PHONE, phone);
    }

    /**
     * Gets the program studies.
     *
     * @return Collection
     */
    protected Collection<GraduationStudentProgram> getProgramStudies() {
        Collection<GraduationStudentProgram> values = Collections.EMPTY_LIST;
        if (getIep() != null) {
            values = getIep().getStudent().getProgramStudies();
        }
        return values;
    }

    /**
     * Gets the school year.
     *
     * @return String
     */
    protected String getSchoolYear() {
        String value = "";
        if (getIep() != null) {
            PlainDate date = getIep().getStartDate();

            if (date != null) {
                X2Criteria criteria = new X2Criteria();
                criteria.addLessOrEqualThan(DistrictSchoolYearContext.COL_START_DATE, date);
                criteria.addGreaterOrEqualThan(DistrictSchoolYearContext.COL_END_DATE, date);
                DistrictSchoolYearContext ctx =
                        getBroker().getBeanByQuery(new QueryByCriteria(DistrictSchoolYearContext.class, criteria));
                if (ctx != null) {
                    value = Integer.toString(ctx.getSchoolYear() - 1) + "-" + Integer.toString(ctx.getSchoolYear());
                }
            }
        }
        return value;
    }

    /**
     * Get the Implementation school from the IEP, if present.
     * Otherwise returns the students current school.
     *
     * @return School
     */
    protected School getImplementationSchool() {
        School school = getSchool(); // default to the currently running school context.
        IepData iep = getIep();
        if (iep != null) {
            String schoolOid = (String) iep.getFieldValueByAlias(ALIAS_IMPLEMENTATION_SCHOOL, getDictionary());
            if (!StringUtils.isEmpty(schoolOid)) {
                school = getBroker().getBeanByOid(SisSchool.class, schoolOid);
            }
            if (school == null) {
                school = iep.getStudent().getSchool();
            }
        }
        // Look up override BSID schools.
        if (school != null && iep != null) {
            SisStudent student = iep.getStudent();
            HashMap<String, Integer> gradeLevelsMap = StudentManager.buildNumericGradeLevelMap(getBroker());
            String gradeLevel = student.getGradeLevel();
            Integer gradeLevelNumeric = gradeLevelsMap.get(gradeLevel);
            if (gradeLevelNumeric != null) {
                String bsid = null;
                if (gradeLevelNumeric.intValue() < 9) {
                    bsid = (String) school.getFieldValueByAlias(ALIAS_BSID_ELEMENTARY);
                } else {
                    bsid = (String) school.getFieldValueByAlias(ALIAS_BSID_SECONDARY);
                }
                if (!StringUtils.isEmpty(bsid)) {
                    DataDictionaryField bsidField = getDictionary().findDataDictionaryFieldByAlias(ALIAS_BSID);
                    if (bsidField != null) {
                        X2Criteria criteria = new X2Criteria();
                        criteria.addEqualTo(bsidField.getJavaName(), bsid);
                        BeanQuery query = new BeanQuery(School.class, criteria);
                        School bsidSchool = getBroker().getBeanByQuery(query);
                        if (bsidSchool != null) {
                            school = bsidSchool;
                        }
                    }
                }
            }
        }
        return school;
    }

    /**
     * Set the Student name and first name as parameters.
     * Check the preferred name option and use the legal name or preferred name.
     */
    protected void setStudentNameView() {
        String name = "";
        String firstName = "";
        int age = 0;

        SisStudent student = null;
        X2BaseBean formStorage = getFormStorage();
        if (formStorage instanceof IepData) {
            student = ((IepData) formStorage).getStudent();
        } else if (formStorage instanceof IepMeeting) {
            student = ((IepMeeting) formStorage).getStudent();
        }
        if (student != null) {
            String usePreferredName = (String) student.getFieldValueByAlias(ALIAS_USE_PREFERRED_NAME, getDictionary());
            if (BooleanAsStringConverter.TRUE.equals(usePreferredName)) {
                firstName = student.getPerson().getFirstName();
                name = firstName + " " + student.getPerson().getLastName();
            } else {
                firstName =
                        (String) student.getPerson().getFieldValueByAlias(ALIAS_LEGAL_FIRST_NAME, getDictionary());
                String lastName =
                        (String) student.getPerson().getFieldValueByAlias(ALIAS_LEGAL_LAST_NAME, getDictionary());
                name = firstName + " " + lastName;

            }
            // Get age.
            PlainDate dob = student.getPerson().getDob();
            if (dob != null) {
                PlainDate today = new PlainDate();
                long dobLong = dob.getTime();
                long todayLong = today.getTime();
                age = (int) ((todayLong - dobLong) / MILLIS_PER_YEAR);
            }

        }
        addParameter(PARAM_STUDENT_FIRSTNAME, firstName);
        addParameter(PARAM_STUDENT_NAME, name);
        addParameter(PARAM_STUDENT_AGE, Integer.valueOf(age));
    }

    /**
     * Gets the team members.
     *
     * @return Collection
     */
    protected Collection<IepTeamMember> getTeamMembers() {
        Collection<IepTeamMember> values = Collections.EMPTY_LIST;
        if (getIep() != null) {
            values = getIep().getTeamMembers();
        }
        return values;
    }

    /**
     * Gets the title.
     *
     * @return String
     */
    protected String getTitle() {
        return "TITLE NOT USED";
    }

    /**
     * Gets the view.
     *
     * @param ddf DataDictionaryField
     * @param value String
     * @return String
     */
    protected String getView(DataDictionaryField ddf, String value, String staffOid, MessageResources messageResource) {
        String view = null;
        if (ddf.hasReferenceTable()) {
            ReferenceCodeRetriever codeRetriever =
                    ReferenceCodeRetriever.getInstance(ddf.getReferenceTable().getPersistenceKey());

            ReferenceCodeCriteria codeCriteria =
                    new ReferenceCodeCriteria(ddf.getReferenceTable(),
                            ReferenceCodeRetriever.EXCLUDE_HIDDEN_CODES);
            codeCriteria.addFieldCriteria(ReferenceCode.COL_CODE, value);

            ModelBroker broker = new ModelBroker(getPrivilegeSet());
            OwnershipStateImpl owner = new OwnershipStateImpl(getOrganization(),
                    getSchool() == null ? null : getSchool().getOid(),
                    staffOid, getUser());

            ReferenceCode code = codeRetriever.getCode(codeCriteria, owner, broker);

            if (code != null) {
                view = ReferenceFieldFormatter.getReferenceTableViewFields(ddf.getReferenceTable(), code, broker,
                        messageResource);
            }
            if (StringUtils.isEmpty(view)) {
                ModelProperty property =
                        new ModelProperty(ReferenceCode.class, ReferenceCode.COL_CODE, getDictionary());
                if (value != null && messageResource != null) {
                    view = LocalizationUtils.getRsmDataValue(messageResource, code, property,
                            value);
                }
            }
            if (view != null && view.startsWith("??")) {
                view = value;
            }
        }
        return view;
    }

    protected Collection<UserDefinedTableB> getInventory(ExtendedDataDictionary ddx) {
        Collection<UserDefinedTableB> values = new ArrayList<UserDefinedTableB>();
        if (getIep() != null && ddx != null) {
            for (UserDefinedTableB tableB : getIep().getStudent().getUserDefinedRecordsB(getBroker())) {
                if (ddx.getOid().equals(tableB.getExtendedDataDictionaryOid())) {
                    values.add(tableB);
                }
            }
        }
        return values;
    }

    /**
     * Initialize.
     *
     * @see com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        super.initialize();
        initSubReports(BASE_SUB_REPORT_FORMATS);
    }

    /**
     * Inits the sub reports.
     *
     * @param formats List<KeyValuePair<String,String>>
     */
    protected void initSubReports(List<KeyValuePair<String, String>> formats) {
        for (KeyValuePair<String, String> item : formats) {
            addParameter(item.getValue(),
                    new ByteArrayInputStream(
                            ReportUtils.getReport(item.getKey(), getBroker())
                                    .getCompiledFormat()));
        }
    }

    /**
     * Override publish so we can capture the output and save results to Documents.
     * For Ontario, assume all reports are Jasper 5.5
     *
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#publishResults()
     */
    @Override
    protected void publishResults() throws Exception {

        boolean saveToDocuments = getParameter(SAVE_TO_DOCUMENTS_IEP_PARAM) != null &&
                ((Boolean) getParameter(SAVE_TO_DOCUMENTS_IEP_PARAM)).booleanValue();

        ResultHandler resultHandler = getJob().getResultHandler();
        resultHandler.open(getJob(), null);
        OutputStream pdfOutputStream = resultHandler.getOutputStream();
        if (saveToDocuments) {
            pdfOutputStream = prepareSaveToDocuments(pdfOutputStream);
        }

        Object data = getDataSource();

        /*
         * Check to see if gatherData returned a structure that contains no data.
         * If it does, discard it so that a JREmptyDataSource is created and used below
         */
        if (data != null && data instanceof DataGrid) {
            DataGrid dataSource = (DataGrid) data;
            if (dataSource.getRows().isEmpty()) {
                data = null;
                getResultHandler().setEmpty(true);
            }
        }

        if (data == null) {
            data = new net.sf.jasperreports5.engine.JREmptyDataSource(0);
        }

        logDataPrepared();

        ThreadUtils.checkInterrupt();

        /*
         * If the job has been aborted then don't bother filling the format or exporting the
         * results.
         */
        if (getJob().getStatus() != ToolJob.STATUS_ABORT) {
            if (PublishReportsManager.isPublishing(getJob(), data, this)) {
                PublishReportsManager publishManager =
                        createReportsManager(getJob(),
                                getFormat(),
                                getParameters(),
                                getSchool(),
                                getOrganization(),
                                getBroker());
                publishManager.publishReport();
            } else if (data instanceof net.sf.jasperreports5.engine.JRDataSource) {
                try {
                    JarPluginManager jarPluginManager = new JarPluginManager();
                    Tool tool = getJob().getTool();

                    if (!StringUtils.isEmpty(tool.getJarPluginPath())) {
                        ClassLoader jarClassLoader =
                                jarPluginManager.getParentClassLoader(tool, getBroker(),
                                        tool.getClass().getClassLoader(), null);
                        JRClassLoader.setCustomClassLoader(jarClassLoader);
                    }

                    try {
                        JasperPrint reportPrint = JasperFillManager.fillReport(getFormat(), getParameters(),
                                (net.sf.jasperreports5.engine.JRDataSource) data);

                        if (reportPrint != null && reportPrint.getPages().size() > 0) {
                            if (data instanceof ReportDataGrid) {
                                ((ReportDataGrid) data).applyJasperPrintParameters(reportPrint);
                            }

                            exportResults(reportPrint, getJob(), pdfOutputStream);
                        } else {
                            getJob().getResultHandler().setEmpty(true);
                        }
                    } finally {
                        JRClassLoader.clearCustomClassLoader();
                    }
                } catch (JRException jre) {
                    throw new X2BaseException(jre);
                }

            } else {
                throw new X2BaseException(AppGlobals.getLogResources(), "TLS-00007", new Object[] {
                        data.getClass().toString(), "5.5"
                });
            }
        }
        if (saveToDocuments) {
            saveToDocuments(pdfOutputStream);
        }
    }

    /**
     * Builds the document name.
     *
     * @param reportName String
     * @param context DistrictSchoolYearContext
     * @param dictionary DataDictionary
     * @return String
     */
    private String buildDocumentNameIEP(String reportName,
                                        DistrictSchoolYearContext context,
                                        DataDictionary dictionary) {
        String documentName;

        if (reportName == null) {
            return null;
        }

        DataDictionaryField documentNameField = null;
        if (dictionary != null) {
            documentNameField =
                    dictionary.findDataDictionaryField(
                            com.follett.fsc.core.k12.beans.Document.class.getName(),
                            com.follett.fsc.core.k12.beans.Document.COL_NAME);
        }

        documentName = reportName;

        if (context != null && context.getContextId() != null) {
            documentName = documentName + "_" + context.getContextId();
        }

        if (documentNameField != null) {
            if (documentName != null && documentName.length() > documentNameField.getDatabaseLength()) {
                documentName = documentName.substring(0, documentNameField.getDatabaseLength());
            }
        }

        return documentName;
    }

    /**
     * Creates the document.
     *
     * @param overwriteExisting boolean
     * @param person Person
     * @param bean X2BaseBean
     * @param school School
     * @param resultFile File
     * @return Document
     * @see com.follett.fsc.core.k12.tools.reports.SaveToDocuments#createDocument(boolean,
     *      com.follett.fsc.core.k12.beans.Person, com.follett.fsc.core.k12.beans.Student,
     *      java.io.File)
     */
    private com.follett.fsc.core.k12.beans.Document createDocumentIEP(boolean overwriteExisting,
                                                                      Person person,
                                                                      Student student,
                                                                      School school,
                                                                      File resultFile) {
        com.follett.fsc.core.k12.beans.Document document = null;

        DataDictionary dictionary =
                DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField documentTypeField = dictionary.findDataDictionaryField(
                com.follett.fsc.core.k12.beans.Document.class.getName(),
                com.follett.fsc.core.k12.beans.Document.COL_TYPE_CODE);

        String reportName = (getParameter(DOCUMENT_NAME_PARAM) != null
                ? (String) getParameter(DOCUMENT_NAME_PARAM)
                : "IEPReport");
        String documentName = buildDocumentNameIEP(reportName, getCurrentContext(), dictionary);
        String documentType =
                getParameter(DOCUMENT_TYPE_PARAM) != null ? (String) getParameter(DOCUMENT_TYPE_PARAM) : "IEP";

        String fileName = org.apache.commons.lang3.StringUtils.stripAccents(student.getNameView()).replace("'", "")
                .replace(",", "").replace(" ", "")
                + "_" + student.getOid() + "_" + documentName + "_" + new PlainDate()
                + FolderUtils.FILE_EXTENSION_SEPARATOR + "pdf";

        if (documentType != null && documentType.length() > documentTypeField.getDatabaseLength()) {
            documentType = documentType.substring(0, documentTypeField.getDatabaseLength());
        }

        if (overwriteExisting) {
            Collection<com.follett.fsc.core.k12.beans.Document> documents =
                    getExistingDocumentsIEP(person.getOid(), documentName);
            document = getMostRecentDocumentIEP(documents);
        }

        if (document == null) {
            document = X2BaseBean.newInstance(com.follett.fsc.core.k12.beans.Document.class, dictionary);
            document.setPersonOid(person.getOid());
            document.setName(documentName);
            document.setFormatCode("pdf");
            document.setFilename(fileName);
        }

        document.setBinaryFile(resultFile);
        document.setTypeCode(documentType);
        document.setName(documentName);
        document.setFilename(fileName);
        document.setFieldValueByAlias(ALIAS_UPLOAD_DATE, getPlainDate().toString());
        document.setFieldValueByAlias(ALIAS_SCHOOL_ID, school.getSchoolId());
        document.setFieldValueByAlias(ALIAS_CASE_VISIBILITY, BooleanAsStringConverter.TRUE);
        return document;
    }

    /**
     * Exports the Jasper results to an appropriately named file based on the output format.
     *
     * @param reportPrint JasperPrint
     * @param job ToolJob
     * @param resultHandler ResultHandler
     * @throws JRException exception
     * @throws IOException Signals that an I/O exception has occurred.
     */
    private void exportResults(JasperPrint reportPrint, ToolJob job, OutputStream outputStream)
            throws JRException, IOException {
        switch (job.getInput().getFormat()) {
            case ToolInput.CSV_FORMAT:
                JRCsvExporter csvExporter = new JRCsvExporter();
                csvExporter.setParameter(JRExporterParameter.JASPER_PRINT, reportPrint);
                csvExporter.setParameter(JRExporterParameter.OUTPUT_STREAM, outputStream);
                csvExporter.exportReport();
                break;

            case ToolInput.HTML_FORMAT:
                JRHtmlExporter exporter = new JRHtmlExporter();
                exporter.setParameter(JRExporterParameter.JASPER_PRINT, reportPrint);
                exporter.setParameter(JRExporterParameter.OUTPUT_STREAM, outputStream);
                exporter.setParameter(JRHtmlExporterParameter.IS_USING_IMAGES_TO_ALIGN, Boolean.FALSE);
                exporter.exportReport();
                break;

            case ToolInput.PDF_FORMAT:
                JasperExportManager.exportReportToPdfStream(reportPrint, outputStream);
                break;

            case ToolInput.XLS_FORMAT:
                // report name must be less than 31 characters
                reportPrint.setName(StringUtils.truncate(reportPrint.getName(), ReportConstants.XLS_NAME_MAX_CHARS));
                JRXlsExporter xlsExporter = new JRXlsExporter();
                xlsExporter.setParameter(JRExporterParameter.JASPER_PRINT, reportPrint);
                xlsExporter.setParameter(JRExporterParameter.OUTPUT_STREAM, outputStream);
                xlsExporter.exportReport();
                break;
        }
    }

    /**
     * Retrieve a list of existing Documents for the student and document name.
     *
     * @param documentName
     * @param columnKeys
     * @param mapSizes
     *
     * @return Map
     */
    private Collection<com.follett.fsc.core.k12.beans.Document> getExistingDocumentsIEP(String personOid,
                                                                                        String documentName) {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(com.follett.fsc.core.k12.beans.Document.COL_PERSON_OID, personOid);
        criteria.addEqualTo(com.follett.fsc.core.k12.beans.Document.COL_NAME, documentName);
        QueryByCriteria query = new QueryByCriteria(com.follett.fsc.core.k12.beans.Document.class, criteria);

        return getBroker().getCollectionByQuery(query);
    }

    /**
     * Gets the iep year.
     *
     * @return String
     */
    private String getIepYear() {
        String year = null;

        String iprcDate = null;
        X2BaseBean formStorage = getFormStorage();
        if (formStorage instanceof IepData) {
            iprcDate = (String) ((IepData) formStorage).getFieldValueByAlias(ALIAS_IEP_IPRC_EFFECTIVE_DATE,
                    getDictionary());
        } else if (formStorage instanceof IepMeeting) {
            IepData iepData = ((IepMeeting) formStorage).getIepData();
            if (iepData != null) {
                iprcDate = (String) iepData.getFieldValueByAlias(ALIAS_IEP_IPRC_EFFECTIVE_DATE, getDictionary());
            }
        }
        // If the IPRC date is present, use that to determine the school year.
        if (!StringUtils.isEmpty(iprcDate)) {
            DateAsStringConverter converter = (DateAsStringConverter) ConverterFactory
                    .getConverterForClass(Converter.DATE_CONVERTER, getLocale(), true);
            PlainDate iprcPlainDate = (PlainDate) converter.parseSystemString(iprcDate);
            if (iprcPlainDate != null) {
                Criteria criteria = new Criteria();
                criteria.addLessOrEqualThan(SisDistrictSchoolYearContext.COL_START_DATE, iprcPlainDate);
                criteria.addGreaterOrEqualThan(SisDistrictSchoolYearContext.COL_END_DATE, iprcPlainDate);
                SisDistrictSchoolYearContext ctx =
                        getBroker().getBeanByQuery(new QueryByCriteria(SisDistrictSchoolYearContext.class, criteria));
                if (ctx != null) {
                    year = ctx.getContextId();
                }
            }
        }
        // If no IPRC date, use the selected school year context.
        if (year == null) {
            year = getCurrentContext().getContextId();
        }
        return year;
    }

    /**
     * Gets the most recent document.
     *
     * @param document Document
     * @param documents Collection<Document>
     * @return Document
     */
    private com.follett.fsc.core.k12.beans.Document getMostRecentDocumentIEP(Collection<com.follett.fsc.core.k12.beans.Document> documents) {
        com.follett.fsc.core.k12.beans.Document mostRecentDocument = null;
        com.follett.fsc.core.k12.beans.Document document = null;
        if (documents != null) {
            for (com.follett.fsc.core.k12.beans.Document doc : documents) {
                if (document == null) {
                    mostRecentDocument = doc;
                } else {
                    if (doc.getLastModifiedTime() > document.getLastModifiedTime()) {
                        mostRecentDocument = doc;
                    }
                }
            }
        }

        return mostRecentDocument;
    }

    /**
     * Gets the parent information.
     *
     * @param teamMember IepTeamMember
     * @return String
     */
    private String getParentInformation(IepTeamMember teamMember, DataDictionaryField guardianField) {
        if (teamMember.getPerson() != null) {
            String personOid = teamMember.getPerson().getOid();
            Criteria criteria = new Criteria();
            criteria.addEqualTo(StudentContact.COL_STUDENT_OID, teamMember.getStudentOid());
            criteria.addEqualTo(StudentContact.REL_CONTACT + ModelProperty.PATH_DELIMITER +
                    Contact.COL_PERSON_OID, personOid);
            if (guardianField != null) {
                criteria.addEqualTo(guardianField.getJavaName(), BooleanAsStringConverter.TRUE);
            }
            StudentContact studentContact =
                    (StudentContact) getBroker().getBeanByQuery(new QueryByCriteria(StudentContact.class, criteria));
            if (studentContact != null) {
                Person parent = studentContact.getPerson();
                return parent.getFirstName() + " " + parent.getLastName();
            }
        }
        return null;
    }

    /**
     * @param outputStream
     * @return
     * @throws FileNotFoundException
     */
    private OutputStream prepareSaveToDocuments(OutputStream outputStream) throws FileNotFoundException {
        m_saveToDocumentsResultFile = new File(
                getJob().getTempFolder() + File.separator + System.currentTimeMillis()
                        + Integer.toString(getJob().getJobId()) + "report.pdf");

        @SuppressWarnings("resource")
        FileOutputStream foStream = new FileOutputStream(m_saveToDocumentsResultFile);
        OutputStream oss[] = new OutputStream[2];
        oss[0] = outputStream;
        oss[1] = foStream;
        DuplicateOutputStream dos = new DuplicateOutputStream(oss);
        return dos;
    }

    /**
     * @param pdfOutputStream
     */
    private void saveToDocuments(OutputStream pdfOutputStream) {
        // Create a Student Document record with the PDF set as the file source.
        if (m_saveToDocumentsResultFile.isFile()) {
            Student student = null;
            Person person = null;
            School school = null;
            X2BaseBean owner = getFormStorage();
            if (owner instanceof IepData) {
                student = ((IepData) owner).getStudent();
                person = student.getPerson();
                school = student.getSchool();
            } else {
                owner = getFormOwner();
                if (owner instanceof IepData) {
                    student = ((IepData) owner).getStudent();
                    person = student.getPerson();
                    school = student.getSchool();
                }
            }
            if (student != null && person != null) {
                // Prepare some global variables so the method createDocument() will work.
                // Find or create the document record.
                com.follett.fsc.core.k12.beans.Document document =
                        createDocumentIEP(true, person, student, school, m_saveToDocumentsResultFile);

                getBroker().saveBeanForced(document);
            }
            m_saveToDocumentsResultFile.delete();
        }
    }

    /**
     * A duplicating output stream.
     * Anything written to the output stream will be written to
     * multiple underlying output streams.
     *
     * @author Follett Software Company
     * @copyright 2021
     */
    private static class DuplicateOutputStream extends FilterOutputStream {
        private OutputStream m_streams[];

        /**
         * Constructor.
         * Provides a list of output streams to be written to.
         *
         * @param streams
         */
        public DuplicateOutputStream(OutputStream streams[]) {
            super(streams[0]);
            m_streams = streams;
        }

        /**
         * Write to all underlying output streams
         *
         * @see java.io.FilterOutputStream#write(int)
         */
        @Override
        public void write(int i) throws IOException {
            for (OutputStream os : m_streams) {
                os.write(i);
            }
        }

        /**
         * Write to all underlying output streams
         *
         * @see java.io.FilterOutputStream#write(byte[])
         */
        @Override
        public void write(byte[] b) throws IOException {
            for (OutputStream os : m_streams) {
                os.write(b);
            }
        }

        /**
         * Write to all underlying output streams
         *
         * @see java.io.FilterOutputStream#write(byte[], int, int)
         */
        @Override
        public void write(byte[] b, int offset, int length) throws IOException {
            for (OutputStream os : m_streams) {
                os.write(b, offset, length);
            }
        }

        /**
         * Flush all underlying output streams
         *
         * @see java.io.FilterOutputStream#flush()
         */
        @Override
        public void flush() throws IOException {
            for (OutputStream os : m_streams) {
                os.flush();
            }
        }

        /**
         * Close all underlying output streams
         *
         * @see java.io.FilterOutputStream#close()
         */
        @Override
        public void close() throws IOException {
            for (OutputStream os : m_streams) {
                os.close();
            }
        }
    }
}
