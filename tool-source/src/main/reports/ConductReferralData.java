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
import com.follett.fsc.core.k12.beans.FormInstance;
import com.follett.fsc.core.k12.beans.GenericFormChildData;
import com.follett.fsc.core.k12.beans.GenericFormData;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.beans.WorkflowProgressForm;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.BeanCollectionDataSource;
import com.follett.fsc.core.k12.tools.reports.ReportConstants;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import com.follett.fsc.core.k12.web.workflow.WorkflowUtils;
import com.x2dev.sis.model.beans.SisStaff;
import java.util.Collection;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Java source for the conduct referral form report. This report displays information from two
 * forms: the referral form and the action form. Actions are displayed on a subreport.
 *
 * @author X2 Development Corporation
 */
public class ConductReferralData extends BaseFormReportJavaSource {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Report parameter containing the boolean true if the referral contained no actions; false
     * otherwise.
     */
    public static final String PARAM_NO_ACTIONS = "noActions";

    /**
     * Report parameter containing the owner Staff object.
     */
    public static final String PARAM_OWNER_STAFF = "ownerStaff";

    /**
     * Report parameter containing the referral Staff object.
     */
    public static final String PARAM_REFERRAL_STAFF = "referralStaff";

    /**
     * Report parameter containing the administrator remarks.
     */
    public static final String PARAM_REMARKS = "remarks";

    /**
     * Report parameter containing the student object.
     */
    public static final String PARAM_STUDENT = "student";

    private static final String ACTION_SUBREPORT_FORMAT_ID = "SYS-CND-005-ACT";

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        SimpleFormDataSource dataSource = null;
        FormInstance referralForm = getReferralForm();
        if (referralForm != null) {
            X2BaseBean referralFormStorage = referralForm.getStorageObject(getBroker());
            DataDictionary referralDictionary = DataDictionary.getDistrictDictionary(
                    referralForm.getFormDefinition().getExtendedDataDictionary(), getBroker().getPersistenceKey());

            DataDictionary actionDictionary = getDictionary();
            X2BaseBean actionFormStorage = getFormStorage();

            addParameter(PARAM_STUDENT, getFormOwner());

            String referralStaffOid =
                    (String) referralFormStorage.getFieldValueByAlias("referral-staff-oid", referralDictionary);
            String ownerStaffOid = (String) actionFormStorage.getFieldValueByAlias("owner-staff-oid", actionDictionary);

            SisStaff referralStaff = (SisStaff) getBroker().getBeanByOid(SisStaff.class, referralStaffOid);
            SisStaff ownerStaff = (SisStaff) getBroker().getBeanByOid(SisStaff.class, ownerStaffOid);

            addParameter(PARAM_REFERRAL_STAFF, referralStaff);
            addParameter(PARAM_OWNER_STAFF, ownerStaff);

            String adminRemarks =
                    (String) actionFormStorage.getFieldValueByAlias("administrator-remarks", actionDictionary);
            if (adminRemarks != null) {
                addParameter(PARAM_REMARKS, adminRemarks);
            }

            Collection<GenericFormChildData> children =
                    ((GenericFormData) actionFormStorage).getGenericFormDataChildren(getBroker());

            addParameter(PARAM_NO_ACTIONS, Boolean.valueOf(children.isEmpty()));

            if (children.isEmpty() || isBlank()) {
                GenericFormChildData blankAction =
                        X2BaseBean.newInstance(GenericFormChildData.class, getBroker().getPersistenceKey());
                children.add(blankAction);
            }

            /*
             * Add support for the team subreport
             */
            Report teamSubreport = ReportUtils.getReport(ACTION_SUBREPORT_FORMAT_ID, getBroker());

            addParameter(ReportConstants.FIELD_FORMAT, teamSubreport.getCompiledFormat());
            addParameter(ReportConstants.FIELD_DATA_SOURCE,
                    new BeanCollectionDataSource(children, getDictionary(), getLocale()));
            dataSource = new SimpleFormDataSource(referralFormStorage, getFormOwner(), referralDictionary, getLocale());
        }

        return dataSource;
    }

    /**
     * Returns the referral form.
     *
     * @return FormInstance
     */
    private FormInstance getReferralForm() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(WorkflowProgressForm.COL_FORM_INSTANCE_OID, getFormInstance().getOid());

        QueryByCriteria query = new QueryByCriteria(WorkflowProgressForm.class, criteria);

        WorkflowProgressForm progressForm = (WorkflowProgressForm) getBroker().getBeanByQuery(query);

        FormInstance form = null;
        if (progressForm != null) {
            form = WorkflowUtils.getFormInstance(progressForm.getWorkflowProgress().getWorkflow(),
                    "CND-REF",
                    getBroker());
        }
        return form;
    }
}
