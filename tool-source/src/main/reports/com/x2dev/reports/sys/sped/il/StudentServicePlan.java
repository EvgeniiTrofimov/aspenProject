/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2003 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.reports.sys.sped.il;

import static com.follett.fsc.core.k12.tools.reports.ReportConstants.FIELD_DATA_SOURCE;
import static com.follett.fsc.core.k12.tools.reports.ReportConstants.FIELD_FORMAT;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.FormDefinition;
import com.follett.fsc.core.k12.beans.FormInstance;
import com.follett.fsc.core.k12.beans.GenericFormChildData;
import com.follett.fsc.core.k12.beans.GenericFormData;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.beans.ToolSourceCode;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.ToolRunException;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.utils.ByteArrayClassLoader;
import com.x2dev.utils.ByteArrayCompiler;
import com.x2dev.utils.KeyValuePair;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.types.PlainDate;
import java.io.ByteArrayInputStream;
import java.lang.reflect.Method;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;
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
public class StudentServicePlan extends BaseFormReportJavaSource {

    private static final String ALIAS_NOTIF_CONF_CHLD_HOME_SCHOOL = "notif-conf-chld-home-school";
    private static final String ALIAS_NOTIF_CONF_CHLD_RECORD_TYPE = "notif-conf-chld-record-type";
    private static final String ALIAS_NOTIF_CONF_CHLD_SHCOOL_START = "notif-conf-chld-shcool-start";
    private static final String ALIAS_NOTIF_CONF_CHLD_CLROOM_TEACHER = "notif-conf-chld-clroom-teacher";
    private static final String PARAM_SCHOOL_YEAR = "iep-srvcplan-schoolyear";

    private static final String ALIAS_NOTIF_CONF_CHLD_SCHOOL_NAME = "notif-conf-chld-school-name";
    private static final String ALIAS_IEP_SRVCPLAN_CONF_DATE = "iep-srvcplan-conf-date";



    private static final String FORM_DEF_ID_SPED_IL_NOTIFACTION = "SPED-IL-NOTIFACTION";
    private static final String FIELD_PARAMETERS_MAP = "PARAMETERS_MAP";

    private static final String KEY_SCHOOL_TEACHER = "schoolTeacher";
    private static final String KEY_PRIMARY_NAME = "primaryName";
    private static final String KEY_RESIDENT_DISTRICT = "residentDistrict";
    private static final String KEY_RESIDENT_SCHOOL = "residentSchool";
    private static final String KEY_SECONDARY = "secondary";
    private static final String KEY_SECONDARY_NAMES = "secondaryNames";
    private static final String KEY_SERVING_SCHOOL = "servingSchool";


    private static final String METHOD_FILL_GRID = "fillGrid";
    private static final String METHOD_INIT_BEAN_REPORT = "initBeanReport";

    private static final String PREFIX_NOTIF_TITLE = "notif-title";
    private static final String PARAM_ADDITIONAL_DSBL = "additionalDsbl";
    private static final String PARAM_PARENTS = "parents";
    private static final String PARAM_PARTICIPANT_LIST = "participantList";
    private static final String PARAM_PRIMARY = "primary";
    private static final String PARAM_SECONDARY_DSBL = "secondaryDsbl";
    private static final String POINT = ".";

    private static final String RECORD_TYPE_PRIVATE_SCHOOL = "privateSchool";

    private static final String REPORT_SYS_SPED_IL_GOALOBJ = "SYS-SPED-IL-GOALOBJ";

    private static final String SPLIT_COMMA = ",";

    private static final String UI_FORMAT_MM_DD_YYYY = "MM/dd/yyyy";

    IlSpedHelper m_ilSpedHelper = new IlSpedHelper();
    private FormInstance m_formInstNotification = null;
    DataDictionary m_ddxNotification = null;
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
        IepData iepData = (IepData) getFormOwner();

        if (iepData != null) {
            m_ilSpedHelper.initializeHelper(getBroker(), getDictionary());
            m_ddxNotification = m_ilSpedHelper.getDictionaryByExtendedDictionaryId("SPED-IL-NOTIFICATION");
            fillSchoolInformation(iepData);
            fillParentsInformation(iepData);
            fillParticipantInformation();
            fillDisabilities(iepData);
            fillEducationServiceAndPlacement();
            fillGoalAndObjectives();
        }

        return new SimpleFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale());
    }

    /**
     * Gets the data source from report.
     *
     * @param report Report
     * @param parametrsMap Map<String,Object>
     * @return JR data source
     */
    protected JRDataSource getDataSourceFromReport(Report report, Map<String, Object> parametrsMap) {
        Object javaSource = null;
        try {
            javaSource = getJavaSourceFabric(report);
        } catch (ToolRunException e) {
            // Do nothing
        }

        JRDataSource dataSource = null;
        Method method = null;

        for (Class cls = javaSource.getClass(); method == null && cls != Object.class; cls = cls.getSuperclass()) {
            try {
                method = cls.getDeclaredMethod(METHOD_FILL_GRID,
                        new Class[] {Map.class, Locale.class});
            } catch (Exception e) {
                // Do nothing;
            }
        }
        if (method != null) {
            try {
                dataSource = (JRDataSource) method.invoke(javaSource, new Object[] {parametrsMap, getLocale()});
            } catch (Exception e) {
                // Do nothing;
            }
        }
        return dataSource;
    }

    /**
     * initialize report Java Source and return it.
     *
     * @param report Report
     * @return Object
     * @throws ToolRunException exception
     */
    protected Object getJavaSourceFabric(Report report) throws ToolRunException {

        Object toolObject = null;
        Object javaSource = null;
        ToolSourceCode sourceCode = report.getSourceCode();
        KeyValuePair<String, String> classNamePair =
                ByteArrayCompiler.extractClassName(sourceCode.getSourceCode().getBytes());
        String packageName = classNamePair.getKey();
        String simpleClassName = classNamePair.getValue();

        String className = simpleClassName;
        if (packageName != null) {
            className = packageName + POINT + simpleClassName;
        }
        byte[] compileCode = report.getSourceCode().getCompiledCode();
        ByteArrayClassLoader classLoader;
        try {
            classLoader = new ByteArrayClassLoader(Report.class.getClassLoader(),
                    compileCode,
                    null, null);
            Class toolObjectClass = classLoader.loadClass(className);
            toolObject = toolObjectClass.getDeclaredConstructor().newInstance();
        } catch (Throwable t) {
            throw new ToolRunException(t);
        }

        if (toolObject != null) {
            Method method = null;
            for (Class cls = toolObject.getClass(); method == null && cls != Object.class; cls = cls.getSuperclass()) {
                try {
                    method = cls.getDeclaredMethod(METHOD_INIT_BEAN_REPORT,
                            new Class[] {X2BaseBean.class, X2BaseBean.class, DataDictionary.class,
                                    Map.class, X2Broker.class});
                } catch (Exception e) {
                    // Do nothing;
                }
            }
            if (method != null) {
                try {
                    method.invoke(toolObject, new Object[] {getFormStorage(), getFormOwner(), getDictionary(),
                            getParameters(), getBroker()});
                } catch (Exception e) {
                    // Do nothing;
                }
            }
            javaSource = toolObject;
        }
        return javaSource;
    }

    /**
     * Gets the report by id.
     *
     * @param id String
     * @return Report
     */
    protected Report getReportById(String id) {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(Report.COL_ID, id);
        QueryByCriteria byCriteria = new QueryByCriteria(Report.class, criteria);
        Report report = (Report) getBroker().getBeanByQuery(byCriteria);
        return report;
    }

    /**
     * Fill disabilities.
     *
     * @param iepData IepData
     */
    private void fillDisabilities(IepData iepData) {

        Map<String, String> disabilies = m_ilSpedHelper.getDisabilities(iepData, false);

        addParameter(PARAM_PRIMARY, disabilies.get(KEY_PRIMARY_NAME));
        String secondaryDsbl = disabilies.get(KEY_SECONDARY_NAMES);
        String additionalDsbl = null;
        List<String> secondaryDsblList = new ArrayList<String>(Arrays.asList(secondaryDsbl.split(SPLIT_COMMA)));
        if (secondaryDsblList.size() > 0) {
            secondaryDsbl = secondaryDsblList.get(0);
            secondaryDsblList.remove(0);
            additionalDsbl = m_ilSpedHelper.listToString(secondaryDsblList, SPLIT_COMMA + IlSpedHelper.SPACE);

        }

        addParameter(PARAM_SECONDARY_DSBL, secondaryDsbl);
        addParameter(PARAM_ADDITIONAL_DSBL, additionalDsbl);
        addParameter(KEY_SECONDARY, disabilies.get(KEY_SECONDARY));
    }

    /**
     * Fill goal and objectives.
     */
    private void fillGoalAndObjectives() {
        Report report = getReportById(REPORT_SYS_SPED_IL_GOALOBJ);

        Report subreport = ReportUtils.getReport(report.getId(), getBroker());
        ByteArrayInputStream format = new ByteArrayInputStream(subreport.getCompiledFormat());
        Map<String, Object> parametrsMap = new HashMap<String, Object>();
        parametrsMap.putAll(getParameters());
        JRDataSource dataSource = getDataSourceFromReport(report, parametrsMap);
        if (dataSource != null) {
            addParameter(FIELD_DATA_SOURCE, dataSource);
            addParameter(FIELD_FORMAT, format);
            addParameter(FIELD_PARAMETERS_MAP, parametrsMap);
        }
    }


    /**
     * Fill education service and placement.
     */
    private void fillEducationServiceAndPlacement() {
        Report report = getReportById("SYS-SPED-IL-3454PQ");

        Report subreport = ReportUtils.getReport(report.getId(), getBroker());
        ByteArrayInputStream format = new ByteArrayInputStream(subreport.getCompiledFormat());
        Map<String, Object> parametrsMap = new HashMap<String, Object>();
        parametrsMap.putAll(getParameters());
        JRDataSource dataSource = getDataSourceFromReport(report, parametrsMap);
        if (dataSource != null) {
            addParameter(FIELD_DATA_SOURCE + 2, dataSource);
            addParameter(FIELD_FORMAT + 2, format);
            addParameter(FIELD_PARAMETERS_MAP + 2, parametrsMap);
        }
    }


    /**
     * Fill parents information.
     *
     * @param iepData IepData
     */
    private void fillParentsInformation(IepData iepData) {
        List<Map<String, Object>> parents = m_ilSpedHelper.getParentsInformation(iepData);
        addParameter(PARAM_PARENTS, parents);
    }

    /**
     * Fill participant information.
     */
    private void fillParticipantInformation() {
        List<String> participantList = new ArrayList<String>();

        FormInstance formInstNotification = getFormInstNotification();
        if (formInstNotification != null) {
            ExtendedDataDictionary extDDX = formInstNotification.getFormDefinition().getExtendedDataDictionary();
            DataDictionary ddx = DataDictionary.getDistrictDictionary(extDDX, getBroker().getPersistenceKey());
            X2BaseBean storageTable = formInstNotification.getStorageObject();
            String prefixAlias = PREFIX_NOTIF_TITLE;
            for (int i = 1; i < 11; i++) {
                String participant = (String) storageTable.getFieldValueByAlias(prefixAlias + i, ddx);
                if (!StringUtils.isEmpty(participant)) {
                    participantList.add(participant);
                }
            }
        }

        addParameter(PARAM_PARTICIPANT_LIST, participantList);
    }

    /**
     * Fill school information.
     *
     * @param iepData IepData
     */
    private void fillSchoolInformation(IepData iepData) {
        if (getFormStorage().getOid() != null) {
            PlainDate conferenceDate = m_ilSpedHelper.getPlainDateByBeenAlias(iepData, ALIAS_IEP_SRVCPLAN_CONF_DATE);

            DistrictSchoolYearContext context = null;
            GenericFormChildData gfcd = (GenericFormChildData) getBeanWithLastServingSchool();
            if (gfcd != null) {
                PlainDate privateSchDate = IlSpedHelper.getPlainDateByBeenAlias(gfcd,
                        ALIAS_NOTIF_CONF_CHLD_SHCOOL_START, m_ddxNotification);
                context = m_ilSpedHelper.getSchoolYearContextByDate(privateSchDate);

                addParameter(KEY_SERVING_SCHOOL,
                        gfcd.getFieldValueByAlias(ALIAS_NOTIF_CONF_CHLD_SCHOOL_NAME, m_ddxNotification));
                addParameter(KEY_SCHOOL_TEACHER,
                        gfcd.getFieldValueByAlias(ALIAS_NOTIF_CONF_CHLD_CLROOM_TEACHER, m_ddxNotification));
                String homeSchool =
                        (String) gfcd.getFieldValueByAlias(ALIAS_NOTIF_CONF_CHLD_HOME_SCHOOL, m_ddxNotification);
                addParameter(KEY_RESIDENT_SCHOOL, homeSchool);
                /*
                 * String homeSchoolJavaName =
                 * m_ilSpedHelper.translateAliasToJavaName(ALIAS_NOTIF_CONF_CHLD_HOME_SCHOOL,
                 * m_ddxNotification);
                 * if (!StringUtils.isEmpty(homeSchool))
                 * {
                 * String homeSchoolDistrict =
                 * m_ilSpedHelper.lookupReferenceCodeByBeanPath(GenericFormChildData.class,
                 * homeSchoolJavaName, homeSchool, ReferenceMapTypeCode.STATE.ordinal());
                 * addParameter(KEY_RESIDENT_DISTRICT, homeSchoolDistrict);
                 * }
                 */
            }
            if (context == null && conferenceDate != null) {
                context = m_ilSpedHelper.getSchoolYearContextByDate(conferenceDate);
            }
            if (context == null) {
                conferenceDate = new PlainDate(getFormInstance().getCreatedTime());
                context = m_ilSpedHelper.getSchoolYearContextByDate(conferenceDate);
            }

            addParameter(PARAM_SCHOOL_YEAR, context.getContextId());

            addParameter(KEY_RESIDENT_DISTRICT, iepData.getStudent().getOrganization1().getName());

        }


    }

    /**
     * Find form instance.
     *
     * @param owner X2BaseBean
     * @param formDefinition String
     * @return Collection
     */
    private Collection<FormInstance> findFormInstance(X2BaseBean owner, String formDefinition) {
        Criteria criteria = new X2Criteria();
        criteria.addEqualTo(FormInstance.COL_OWNER_OBJECT_OID, owner.getOid());
        criteria.addEqualTo(FormInstance.REL_FORM_DEFINITION + ModelProperty.PATH_DELIMITER +
                FormDefinition.COL_ID, formDefinition);
        Collection<FormInstance> instances =
                getBroker().getCollectionByQuery(new QueryByCriteria(FormInstance.class, criteria));
        if (instances == null) {
            instances = new ArrayList<FormInstance>();
        }
        return instances;

    }

    /**
     * Gets the bean with last serving school.
     *
     * @return X 2 base bean
     */
    private X2BaseBean getBeanWithLastServingSchool() {
        FormInstance formInstNotification = getFormInstNotification();
        Comparator<GenericFormChildData> comparator = new Comparator<GenericFormChildData>() {

            @Override
            public int compare(GenericFormChildData o1, GenericFormChildData o2) {
                int returnInt = 0;

                String date1 = m_ilSpedHelper.getDateByAlias(o1, ALIAS_NOTIF_CONF_CHLD_SHCOOL_START, m_ddxNotification);
                String date2 = m_ilSpedHelper.getDateByAlias(o2, ALIAS_NOTIF_CONF_CHLD_SHCOOL_START, m_ddxNotification);
                SimpleDateFormat dateFormatter = new SimpleDateFormat(UI_FORMAT_MM_DD_YYYY);
                PlainDate start1 = null;
                PlainDate start2 = null;
                try {
                    start1 = new PlainDate(dateFormatter.parse(date1));
                    start2 = new PlainDate(dateFormatter.parse(date2));
                } catch (ParseException e) {
                    // nothing to do
                }
                start1 = start1 == null ? new PlainDate(0) : start1;
                start2 = start2 == null ? new PlainDate(0) : start2;
                if (start1.getTime() == start2.getTime()) {
                    returnInt = o2.getOid().compareTo(o1.getOid());
                } else {
                    long difference = start2.getTime() - start1.getTime();
                    returnInt = difference == 0 ? 0 : difference < 0 ? -1 : +1;
                }



                return returnInt;
            }

        };
        Set<GenericFormChildData> serviceShcools = new TreeSet<GenericFormChildData>(comparator);
        if (formInstNotification != null) {
            GenericFormData gfd = (GenericFormData) formInstNotification.getStorageObject();
            for (GenericFormChildData gfcd : gfd.getGenericFormDataChildren()) {
                String recrodType =
                        (String) gfcd.getFieldValueByAlias(ALIAS_NOTIF_CONF_CHLD_RECORD_TYPE, m_ddxNotification);

                if (recrodType != null && recrodType.equals(RECORD_TYPE_PRIVATE_SCHOOL)) {
                    serviceShcools.add(gfcd);
                }
            }

        }
        return serviceShcools.size() > 0 ? serviceShcools.iterator().next() : null;
    }

    /**
     * Gets the form inst notification.
     *
     * @return Form instance
     */
    private FormInstance getFormInstNotification() {
        if (m_formInstNotification == null) {
            Collection<FormInstance> formInstances = findFormInstance(getFormOwner(), FORM_DEF_ID_SPED_IL_NOTIFACTION);
            if (formInstances.size() == 1) {
                m_formInstNotification = formInstances.iterator().next();
            }

        }
        return m_formInstNotification;
    }

    /*
     * private DistrictSchoolYearContext getSchoolYearContextByYear(String year)
     * {
     * DistrictSchoolYearContext context = null;
     * if(!StringUtils.isEmpty(year))
     * {
     * Criteria criteria = new X2Criteria();
     * criteria.addEqualTo(DistrictSchoolYearContext.COL_SCHOOL_YEAR, year);
     * context = (DistrictSchoolYearContext)getBroker().getBeanByQuery(new
     * QueryByCriteria(DistrictSchoolYearContext.class, criteria));
     * }
     * return context;
     *
     * }
     */



}
