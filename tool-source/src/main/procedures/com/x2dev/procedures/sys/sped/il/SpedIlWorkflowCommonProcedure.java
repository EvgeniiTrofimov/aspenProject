/* #PROCEDURE-ID [SYS-SPED-COMMON-W] */
/*
 *
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

package com.x2dev.procedures.sys.sped.il;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.*;
import com.follett.fsc.core.k12.beans.FormDefinition.CreationType;
import com.follett.fsc.core.k12.business.BusinessRules;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.ValidationConstants;
import com.follett.fsc.core.k12.business.ValidationError;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryRelationship;
import com.follett.fsc.core.k12.business.dictionary.ExtendedFieldAttributes;
import com.follett.fsc.core.k12.tools.procedures.WorkflowProcedure;
import com.follett.fsc.core.k12.web.ChildDetailSet;
import com.follett.fsc.core.k12.web.EmbeddedListDetailSet;
import com.follett.fsc.core.k12.web.GenericDetail;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.follett.fsc.core.k12.web.forms.FormDetail;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepDisability;
import com.x2dev.sis.model.beans.IepMeeting;
import com.x2dev.sis.model.beans.IepMeeting.TypeCode;
import com.x2dev.sis.model.beans.IepPerformanceLevel;
import com.x2dev.sis.model.beans.IepService;
import com.x2dev.sis.model.beans.IepTeamMember;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.business.sped.MeetingAttendanceManager;
import com.x2dev.sis.web.sped.MeetingDetail;
import com.x2dev.sis.web.sped.WorkflowMeetingDetail;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.DateAsStringConverter;
import com.x2dev.utils.converters.TimeAsStringConverter;
import java.lang.reflect.Method;
import java.lang.reflect.Type;
import java.sql.Date;
import java.sql.Time;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * class provide specific methods which used for IL SPED workflows procedures.
 *
 * @author X2 Development Corporation
 */
public class SpedIlWorkflowCommonProcedure {

    private static final String FMD_OID_IL_INDICATOR = "fmdIliNdicator";
    private static final String DDX_OID_IL_INDICATOR = "ddxIlIndicator";
    private static final String FORM_DEF_ID_SPED_IL_INDICATOR = "SPED-IL-INDICATOR";
    private static final String DDX_OID_3457C = "ddxIl3457C";
    private static final String FMD_OID_IL3457BC = "fmdIl3457BC";
    private static final String FMD_OID_IL3457C = "fmdIl3457C";
    private static final String FORM_DEFINITION_OID_IL3457H = "fmdIl3457H";
    private static final String TM_ROLE_SPECIAL_EDUCATOR = "Special Educator";
    private static final String TM_ROLE_GENERAL_EDUCATOR = "General Educator";
    private static final String ALIAS_NAME_AO_TITLE = "name-ao-title";
    private static final String ALIAS_SPEC_ED_TEACHER = "spec-ed-teacher";
    private static final String ALIAS_GEN_ED_TEACHER = "gen-ed-teacher";
    private static final String ALIAS_LEA_REPR = "lea-repr";
    private static final String TM_ROLE_IEP_CHAIR = "IEP Chair";
    private static final String TM_ROLE_LEA_REPRESENTATIVE = "LEA Representative";
    private static final String FORM_DEFINITION_OID_IL3457D = "fmdIl3457D";
    private static final String FORM_DEFINITION_OID_IL_MTG = "fmdIlMtg";

    /**
     * class which help align form definition when run convert workflow process<br>
     * convert process - convert workflow with one type(for example re-evaluation) to another
     * type(annual review).<br>
     * Different types contain different phase and outcome and convert process will save only those
     * phase and outcome which contain the same align id<br>
     * but appear situation when outcome need to be convert but target workflow outcome contain
     * different attached form <br>
     * this class provide default behavior for fix this difference
     *
     * @author Follett Software Company
     */
    public class AlignByFormDefinitionDefauldManager {

        private X2Broker m_brokerInner;

        /**
         * Instantiates a new align by form definition defauld manager.
         *
         * @param broker X2Broker
         */
        public AlignByFormDefinitionDefauldManager(X2Broker broker) {
            m_brokerInner = broker;
        }

        /**
         * verify fromDefinition after convert one workflow to another.<br>
         * workflow can contain outcome which need to be convert, but form definition can be
         * different<br>
         * between old and new outcomes<br>
         * default process try match WorkflowPhaseOutcomeForm and WorkflowProgressForm and if found
         * cases when
         * form definition is different, and if storage table the same - fix form definition in
         * forminstance
         *
         * @param progress WorkflowProgress
         */
        public void align(WorkflowProgress progress) {
            boolean needSave = false;
            List<WorkflowProgressForm> progressForms =
                    new ArrayList<WorkflowProgressForm>(progress.getWorkflowProgressForms());
            List<WorkflowPhaseOutcomeForm> outcomeForms =
                    new ArrayList<WorkflowPhaseOutcomeForm>(
                            progress.getWorkflowPhaseOutcome().getWorkflowPhaseOutcomeForms());
            deleteIfFormDefinitionMatch(progressForms, outcomeForms);

            // step2 move to new method
            Map<Pair<String, String>, List<WorkflowPhaseOutcomeForm>> outcomeFormMap = getOutcomeFormMap(outcomeForms);
            Map<Pair<String, String>, List<WorkflowProgressForm>> progressFormMap = getProgressFromMap(progressForms);
            // end step 2

            // step 3 move to new method
            // we can align only in case if owner and storage table the same
            for (Pair<String, String> key : outcomeFormMap.keySet()) {
                List<WorkflowProgressForm> progressForms2 = progressFormMap.get(key);
                if (progressForms2 != null) {
                    List<WorkflowPhaseOutcomeForm> outcomeForms2 = outcomeFormMap.get(key);
                    if (progressForms2.size() == 1 && outcomeForms2.size() == 1) {
                        progressFormMap.remove(key);
                        // method for one to one case
                        boolean result = oneToOneCase(outcomeForms2, progressForms2);
                        if (needSave == false) {
                            needSave = result;
                        }

                    } else {
                        boolean result = otherCase(outcomeForms2, progressForms2);
                        if (needSave == false) {
                            needSave = result;
                        }
                    }
                }
            }

            if (!progressFormMap.isEmpty()) {
                Collection<List<WorkflowProgressForm>> values = new ArrayList(progressFormMap.values());
                List<WorkflowProgressForm> prForms = new ArrayList<WorkflowProgressForm>();
                for (List<WorkflowProgressForm> current : values) {
                    prForms.addAll(current);
                }
                boolean result = otherProgressFormsDestiny(prForms);
                if (needSave == false) {
                    needSave = result;
                }
            }

            if (needSave) {
                getBroker().saveBeanForced(progress);
            }


        }

        /**
         * One to one case.
         *
         * @param outcomeForms List<WorkflowPhaseOutcomeForm>
         * @param progressForms List<WorkflowProgressForm>
         * @return true, if successful
         */
        protected boolean oneToOneCase(List<WorkflowPhaseOutcomeForm> outcomeForms,
                                       List<WorkflowProgressForm> progressForms) {
            WorkflowProgressForm progForm = progressForms.iterator().next();
            WorkflowPhaseOutcomeForm outcForm = outcomeForms.iterator().next();
            FormInstance formInstance = progForm.getFormInstance();
            formInstance.setFormDefinitionOid(outcForm.getFormDefinitionOid());
            if (formInstance.isDirty()) {
                getBroker().saveBeanForced(formInstance);
            }
            return true;
        }


        /**
         * Other case.
         *
         * @param outcomeForms List<WorkflowPhaseOutcomeForm>
         * @param progressForms List<WorkflowProgressForm>
         * @return true, if successful
         */
        protected boolean otherCase(List<WorkflowPhaseOutcomeForm> outcomeForms,
                                    List<WorkflowProgressForm> progressForms) {
            return false;
        }

        /**
         * Other progress forms destiny.
         *
         * @param prForms List<WorkflowProgressForm>
         * @return true, if successful
         */
        protected boolean otherProgressFormsDestiny(List<WorkflowProgressForm> prForms) {
            return false;
        }



        /**
         * Delete if form definition match.
         *
         * @param progressForms List<WorkflowProgressForm>
         * @param outcomeFormsDefinition List<WorkflowPhaseOutcomeForm>
         */
        private void deleteIfFormDefinitionMatch(List<WorkflowProgressForm> progressForms,
                                                 List<WorkflowPhaseOutcomeForm> outcomeFormsDefinition) {
            // outcome form has correct definition start checking
            Iterator<WorkflowPhaseOutcomeForm> outcomeFormI = outcomeFormsDefinition.iterator();
            while (outcomeFormI.hasNext()) {
                WorkflowPhaseOutcomeForm outcomeForm = outcomeFormI.next();
                String correctFromDefinition = outcomeForm.getFormDefinitionOid();
                Iterator<WorkflowProgressForm> progressFormI = progressForms.iterator();
                while (progressFormI.hasNext()) {
                    WorkflowProgressForm progressForm = progressFormI.next();
                    String unknownFromDefinition = progressForm.getFormInstance().getFormDefinitionOid();
                    if (correctFromDefinition.equals(unknownFromDefinition)) {
                        outcomeFormI.remove();
                        progressFormI.remove();
                        break;
                    }
                }
            }
        }

        /**
         * Gets the outcome form map.
         *
         * @param outcomeForms List<WorkflowPhaseOutcomeForm>
         * @return Map
         */
        private Map<Pair<String, String>, List<WorkflowPhaseOutcomeForm>> getOutcomeFormMap(List<WorkflowPhaseOutcomeForm> outcomeForms) {
            Map<Pair<String, String>, List<WorkflowPhaseOutcomeForm>> outcomeFormMap =
                    new HashMap<Pair<String, String>, List<WorkflowPhaseOutcomeForm>>();
            for (WorkflowPhaseOutcomeForm outcomeForm : outcomeForms) {
                FormDefinition formDef = outcomeForm.getFormDefinition();
                Pair<String, String> key = getKey(formDef);
                List<WorkflowPhaseOutcomeForm> outcomeForms2 = outcomeFormMap.get(key);
                if (outcomeForms2 == null) {
                    outcomeForms2 = new ArrayList<WorkflowPhaseOutcomeForm>();
                    outcomeFormMap.put(key, outcomeForms2);
                }
                outcomeForms2.add(outcomeForm);
            }
            return outcomeFormMap;
        }

        /**
         * Gets the progress from map.
         *
         * @param progressForms List<WorkflowProgressForm>
         * @return Map
         */
        private Map<Pair<String, String>, List<WorkflowProgressForm>> getProgressFromMap(List<WorkflowProgressForm> progressForms) {
            Map<Pair<String, String>, List<WorkflowProgressForm>> progressFormMap =
                    new HashMap<Pair<String, String>, List<WorkflowProgressForm>>();
            for (WorkflowProgressForm progressForm : progressForms) {
                FormDefinition formDef = progressForm.getFormInstance().getFormDefinition();
                Pair<String, String> key = getKey(formDef);
                List<WorkflowProgressForm> progressForms2 = progressFormMap.get(key);
                if (progressForms2 == null) {
                    progressForms2 = new ArrayList<WorkflowProgressForm>();
                    progressFormMap.put(key, progressForms2);
                }
                progressForms2.add(progressForm);
            }
            return progressFormMap;
        }

        /**
         * Gets the key.
         *
         * @param formDefinition FormDefinition
         * @return Pair
         */
        private Pair<String, String> getKey(FormDefinition formDefinition) {
            return Pair.of(formDefinition.getOwnerDataTableConfigOid(),
                    formDefinition.getStorageDataTableConfigOid());
        }

        /**
         * Gets the broker.
         *
         * @return X 2 broker
         */
        private X2Broker getBroker() {
            return m_brokerInner;
        }
    }

    /**
     * list of meeting types.
     *
     * @author Follett Software Company
     */
    public enum MeetingTypes {
        AMENDMENT("amendment"), ANNUAL_REVIEW("annual review"), INITIAL_DOMAIN("initial domain"), INITIAl_ELIGIBILITY(
                "initial eligibility"), INITIAL_IEP("initial iep"), RE_ANNUAL_REVIEW(
                        "re-eval annual review"), RE_EVALUATION_DOMAIN(
                                "re-evaluation domain"), RE_EVALUATION_ELIGIBILITY(
                                        "re-evaluation eligibility"), TRANSFER_IN("transfer-in");

        private String m_value = null;

        /**
         * Instantiates a new meeting types.
         *
         * @param value String
         */
        MeetingTypes(String value) {
            m_value = value;
        }

        /**
         * @see java.lang.Enum#toString()
         */
        @Override
        public String toString() {
            return m_value;
        }
    }

    /**
     * Transfer Container from 57D to Meeting form.
     *
     * @author Follett Software Company
     */
    class TransferContaner57DtoMeeting extends TransferToX2BaseBeanContainer {

        /**
         * Instantiates a new transfer contaner 57 dto meeting.
         *
         * @param broker X2Broker
         */
        public TransferContaner57DtoMeeting(X2Broker broker) {
            super(broker);
            List<String> list3457dFields = new ArrayList<String>(
                    Arrays.asList(ALIAS_DATE_OF_CONFERENCE, ALIAS_TIME_OF_CONF, ALIAS_LOCATION, ALIAS_OTHER_SCHOOL));
            List<String> listMeetingFields = new ArrayList<String>(Arrays.asList(IepMeeting.COL_DATE,
                    IepMeeting.COL_TIME, ALIAS_MTG_SCH_LOCATION, ALIAS_MTG_OTHER_SCHOOL));
            setFieldsFrom(list3457dFields);
            setFieldsTo(listMeetingFields);

        }

    }

    /**
     * class which contain transfer data for move them form 57G form to meeting.
     *
     * @author Follett Software Company
     */
    class TransferContaner57GtoMeeting extends TransferToX2BaseBeanContainer {

        /**
         * Instantiates a new transfer contaner 57 gto meeting.
         *
         * @param broker X2Broker
         */
        public TransferContaner57GtoMeeting(X2Broker broker) {
            super(broker);
            List<String> list3457gFields = new ArrayList<String>(Arrays.asList(ALIAS_CONTACT_DATE,
                    ALIAS_NOT_AMEND_TIME_OF_CONF, ALIAS_NOT_AMEND_LOCATION, ALIAS_NOT_AMEND_OTHER_SCHOOL));
            List<String> listMeetingFields = new ArrayList<String>(Arrays.asList(IepMeeting.COL_DATE,
                    IepMeeting.COL_TIME, ALIAS_MTG_SCH_LOCATION, ALIAS_MTG_OTHER_SCHOOL));
            setFieldsFrom(list3457gFields);
            setFieldsTo(listMeetingFields);
            // TODO Auto-generated constructor stub
        }
    }

    /**
     * class for create transfer container
     * TransferContainer implementation put in <code>getContainer()</code> method<br>
     * default behavior put only <code>TransferContaner57GtoMeeting</code>,
     * <code>TransferContaner57DtoMeeting</code>
     * or <code>TransferContanerXferToIep</code><br>
     * depend on WFD_OID and WFD_OID.
     *
     * @author Follett Software Company
     */
    public class TransferContainerFabric {
        private X2Broker m_sub_broker = null;
        private WorkflowProgress m_progress = null;
        private DataDictionary m_ddx = null;
        private TransferToX2BaseBeanContainer m_container = null;


        /**
         * Instantiates a new transfer container fabric.
         *
         * @param progress WorkflowProgress
         * @param ddx DataDictionary
         * @param broker X2Broker
         */
        public TransferContainerFabric(WorkflowProgress progress, DataDictionary ddx, X2Broker broker) {
            this.m_sub_broker = broker;
            this.m_progress = progress;
            this.m_ddx = ddx;
            // TODO Auto-generated constructor stub
        }

        /**
         * create TransferToX2BaseBeanContainer.
         *
         * @param beanFrom X2BaseBean
         * @param beanTo X2BaseBean
         * @param ddxFrom DataDictionary
         * @param ddxTo DataDictionary
         * @return TransferToX2BaseBeanContainer
         * @see TransferToX2BaseBeanContainer
         */
        public TransferToX2BaseBeanContainer createTransferContainer(X2BaseBean beanFrom,
                                                                     X2BaseBean beanTo,
                                                                     DataDictionary ddxFrom,
                                                                     DataDictionary ddxTo) {
            m_container = getContainer();
            m_container.setDataDictionaryFrom(ddxFrom);
            m_container.setDataDictoinaryTo(ddxTo);
            m_container.setFrom(beanFrom);
            m_container.setTo(beanTo);
            m_container.initializeMaps();
            return m_container;
        }

        /**
         * @see java.lang.Object#equals(java.lang.Object)
         */
        @Override
        /**
         *
         * @see java.lang.Object#equals(java.lang.Object)
         */
        public boolean equals(Object obj) {
            if (this == obj) {
                return true;
            }
            if (obj == null) {
                return false;
            }
            if (getClass() != obj.getClass()) {
                return false;
            }
            TransferContainerFabric other = (TransferContainerFabric) obj;
            if (!getOuterType().equals(other.getOuterType())) {
                return false;
            }
            if (m_container == null) {
                if (other.m_container != null) {
                    return false;
                }
            } else if (!m_container.equals(other.m_container)) {
                return false;
            }
            return true;
        }

        /**
         * return broker.
         *
         * @return X 2 broker
         */
        public X2Broker getBroker() {
            return m_sub_broker;
        }

        /**
         * @see java.lang.Object#toString()
         */
        @Override
        /**
         *
         * @see java.lang.Object#toString()
         */
        public String toString() {
            return "TransferContanerFabric [m_container=" + m_container + "]";
        }

        /**
         * @see java.lang.Object#hashCode()
         */
        @Override
        /**
         *
         * @see java.lang.Object#hashCode()
         */
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + getOuterType().hashCode();
            result = prime * result + ((m_container == null) ? 0 : m_container.hashCode());
            return result;
        }


        /**
         * return container.
         *
         * @return Transfer to X 2 base bean container
         */
        protected TransferToX2BaseBeanContainer getContainer() {
            TransferToX2BaseBeanContainer container = null;
            String wfdOid = m_progress.getWorkflow().getWorkflowDefinitionOid();
            int sequence = m_progress.getWorkflowPhase().getSequenceNumber();

            if (NUMBER_OF_PHASE_FIRST == sequence && WFD_OID_TRANSFER.equals(wfdOid)) {
                container = new TransferContanerXferToIep(getBroker());
            } else {
                String meetingName = getMeetingNameByPhaseOid(m_progress.getWorkflowPhaseOid(), m_ddx);
                if (meetingName.equals(MeetingTypes.AMENDMENT.toString())) {
                    container = new TransferContaner57GtoMeeting(getBroker());
                } else {
                    container = new TransferContaner57DtoMeeting(getBroker());
                }
            }
            return container;
        }


        /**
         * Gets the outer type.
         *
         * @return Sped il workflow common procedure
         */
        private SpedIlWorkflowCommonProcedure getOuterType() {
            return SpedIlWorkflowCommonProcedure.this;
        }
    }

    /**
     * container for transfer data from Special Education IEP Transfer Form to Iep Data.
     *
     * @author Follett Software Company
     */
    class TransferContanerXferToIep extends TransferToX2BaseBeanContainer {

        private static final String PRIORITY_99 = "99";
        private static final String REG_EXP_PRIM = "prim";

        private Map<ModelProperty, ModelProperty> m_mapRelPath = new HashMap<ModelProperty, ModelProperty>();
        private List<String> m_relFrom = new ArrayList<String>();
        private List<String> m_relTo = new ArrayList<String>();

        /**
         * Instantiates a new transfer contaner xfer to iep.
         *
         * @param broker X2Broker
         */
        public TransferContanerXferToIep(X2Broker broker) {
            super(broker);
            List<String> listXferFields = new ArrayList<String>(Arrays.asList(ALIAS_TRANSFER_IEP_START_DATE,
                    ALIAS_TRANSFER_IEP_END_DATE, ALIAS_MEETING_TYPE, ALIAS_TRANSFER_IEP_MEETING_DATE, ALIAS_STAFF_OID,
                    ALIAS_TRANSFER_IEP_AN_REVIEW_RECENT, ALIAS_TRANSFER_IEP_AN_REVIEW_NEXT,
                    ALIAS_TRANSFER_IEP_RE_EVAL_LAST, ALIAS_TRANSFER_IEP_RE_EVAL_NEXT, ALIAS_TRANSFER_IEP_REFERAL_DATE,
                    ALIAS_TRANSFER_IEP_IN_ELEG_DATE));
            List<String> listIepFields = new ArrayList<String>(Arrays.asList(IepData.COL_START_DATE,
                    IepData.COL_END_DATE, IepData.COL_MEETING_TYPE_CODE, IepData.COL_MEETING_DATE,
                    IepData.COL_STAFF_OID, IepData.COL_LAST_REVIEW_DATE, IepData.COL_NEXT_REVIEW_DATE,
                    IepData.COL_LAST_EVALUATION_DATE, IepData.COL_NEXT_EVALUATION_DATE, IepData.COL_REFERRAL_DATE,
                    IepData.COL_INITIAL_ELIGIBILITY_DATE));
            setFieldsFrom(listXferFields);
            setFieldsTo(listIepFields);

            List<String> listXferRel = new ArrayList<String>(Arrays.asList(ALIAS_TRANSFER_IEP_PRIM_DSBL,
                    ALIAS_TRANSFER_IEP_SEC_DSBL1, ALIAS_TRANSFER_IEP_SEC_DSBL2));
            List<String> listIepRel = new ArrayList<String>(Arrays.asList(IepDisability.COL_DISABILITY_CODE,
                    IepDisability.COL_DISABILITY_CODE, IepDisability.COL_DISABILITY_CODE));
            setRelFrom(listXferRel);
            setRelTo(listIepRel);

        }

        /**
         * Gets the map rel path.
         *
         * @return the mapRelPath
         */
        public Map<ModelProperty, ModelProperty> getMapRelPath() {
            return m_mapRelPath;
        }

        /**
         * Gets the rel from.
         *
         * @return the relFrom
         */
        public List<String> getRelFrom() {
            return m_relFrom;
        }

        /**
         * Gets the rel to.
         *
         * @return the relTo
         */
        public List<String> getRelTo() {
            return m_relTo;
        }

        /**
         * Sets the rel to.
         *
         * @param RelTo void
         */
        public void setRelTo(List<String> RelTo) {
            this.m_relTo = RelTo;
        }

        /**
         * Sets the rel from.
         *
         * @param relFrom the relFrom to set
         */
        public void setRelFrom(List<String> relFrom) {
            this.m_relFrom = relFrom;
        }

        /**
         * @see com.x2dev.procedures.sys.sped.il.SpedIlWorkflowCommonProcedure.TransferToX2BaseBeanContainer#initializeMaps()
         */
        @Override
        /**
         *
         * @see com.x2dev.procedures.sys.sped.il.SpedIlWorkflowCommonProcedure.TransferToX2BaseBeanContainer#initializeMaps()
         */
        protected void initializeMaps() {
            // TODO Auto-generated method stub
            super.initializeMaps();
            initializeFieldMap(getMapRelPath(), getRelFrom(), getRelTo(), getDataDictionaryFrom(),
                    getDataDictoinaryTo(), getFrom().getClass(), IepDisability.class);
        }


        /**
         * @see com.x2dev.procedures.sys.sped.il.SpedIlWorkflowCommonProcedure.TransferToX2BaseBeanContainer#copyRelation(boolean)
         */
        @Override
        /**
         *
         * @see com.x2dev.procedures.sys.sped.il.SpedIlWorkflowCommonProcedure.TransferToX2BaseBeanContainer#copyRelation(boolean)
         */
        protected List<ValidationError> copyRelation(boolean save) {
            IepData iepData = (IepData) getTo();
            List<String> existingDisabiliyCodes = new ArrayList<String>();
            for (IepDisability disability : iepData.getIepDisability()) {
                String dsblCode = disability.getDisabilityCode();
                existingDisabiliyCodes.add(dsblCode);
            }

            List<ValidationError> errors = super.copyRelation(save);
            Map<String, IepDisability> disabilities = new HashMap<String, IepDisability>();
            for (Entry<ModelProperty, ModelProperty> entry : getMapRelPath().entrySet()) {
                ModelProperty fieldPathFrom = entry.getKey();
                String fromAlias = fieldPathFrom.getField().getExtendedDataField().getAlias();
                Object fieldValue = getFrom().getFieldValueByBeanPath(fieldPathFrom.getBeanPath());
                ModelProperty fieldPathTo = entry.getValue();
                fieldValue = translateObject(fieldPathFrom, fieldPathTo, fieldValue);

                if (fieldValue != null && !fieldValue.toString().isEmpty()
                        && !existingDisabiliyCodes.contains(fieldValue)) {
                    IepDisability disability =
                            X2BaseBean.newInstance(IepDisability.class, getBroker().getPersistenceKey());
                    disability.setPrimaryIndicator(fromAlias.contains(REG_EXP_PRIM));
                    disability.setFieldValueByBeanPath(fieldPathTo.getBeanPath(), fieldValue);
                    disability.setStudentOid(iepData.getStudentOid());
                    disability.setFieldValueByAlias(ALIAS_DSB_PRIORITY, PRIORITY_99, getDataDictoinaryTo());
                    disabilities.put(fromAlias, disability);

                }
            }
            // we need save disabilities in order like alias was put
            for (String alias : getRelFrom()) {
                IepDisability disability = disabilities.get(alias);
                if (disability != null) {
                    iepData.addToIepDisability(disability);
                }
            }

            if (save) {
                errors.addAll(getBroker().saveBean(iepData));
            }
            return errors;
        }
    }

    /**
     * class for provide common way for transfer data form one bean to another.
     *
     * @author Follett Software Company
     */
    abstract class TransferToX2BaseBeanContainer {
        private DataDictionary m_dataDictoinaryTo = null;
        private DataDictionary m_dataDictionaryFrom = null;
        private List<String> m_fieldsFrom = new ArrayList<String>();
        private List<String> m_fieldsTo = new ArrayList<String>();
        private X2BaseBean m_from = null;
        private X2Broker m_local_broker = null;
        private Map<ModelProperty, ModelProperty> m_mapFieldPath = new HashMap<ModelProperty, ModelProperty>();
        private X2BaseBean m_to = null;

        /**
         * Instantiates a new transfer to X 2 base bean container.
         *
         * @param x2Broker X2Broker
         */
        public TransferToX2BaseBeanContainer(X2Broker x2Broker) {
            this.m_local_broker = x2Broker;
        }


        /**
         * Gets the broker.
         *
         * @return the m_broker
         */
        public X2Broker getBroker() {
            return m_local_broker;
        }


        /**
         * Gets the data dictionary from.
         *
         * @return the dataDictionaryFrom
         */
        public DataDictionary getDataDictionaryFrom() {
            return m_dataDictionaryFrom;
        }


        /**
         * Gets the data dictoinary to.
         *
         * @return the dataDictoinaryTo
         */
        public DataDictionary getDataDictoinaryTo() {
            return m_dataDictoinaryTo;
        }


        /**
         * data will copy from this bean.
         *
         * @return the from
         */
        public X2BaseBean getFrom() {
            return m_from;
        }


        /**
         * list of fields which will transfer form <code>m_form</code> bean.
         *
         * @return the fieldsFrom
         */
        public List<String> getFieldsFrom() {
            return m_fieldsFrom;
        }



        /**
         * list of fields where will put data to <code>m_to</code> bean form <code>m_form</code>
         * bean.
         *
         * @return the fieldsTo
         */
        public List<String> getFieldsTo() {
            return m_fieldsTo;
        }


        /**
         * map match fields form <code>m_form</code> bean and <code>m_to</code> bean.
         *
         * @return the mapFieldPath
         */
        public Map<ModelProperty, ModelProperty> getMapFieldPath() {
            return m_mapFieldPath;
        }


        /**
         * data will copy into this bean.
         *
         * @return the to
         */
        public X2BaseBean getTo() {
            return m_to;
        }


        /**
         * set DataDictionary for <code>m_form</code> bean.
         *
         * @param dataDictionaryFrom the dataDictionaryFrom to set
         */
        public void setDataDictionaryFrom(DataDictionary dataDictionaryFrom) {
            this.m_dataDictionaryFrom = dataDictionaryFrom;
        }


        /**
         * set DataDictionary for <code>m_to</code> bean.
         *
         * @param dataDictoinaryTo the dataDictoinaryTo to set
         */
        public void setDataDictoinaryTo(DataDictionary dataDictoinaryTo) {
            this.m_dataDictoinaryTo = dataDictoinaryTo;
        }


        /**
         * set bean where data will transfer from.
         *
         * @param from the from to set
         */
        public void setFrom(X2BaseBean from) {
            this.m_from = from;
        }


        /**
         * set fields which will transfer from.
         *
         * @param fieldsFrom the fieldsFrom to set
         */
        public void setFieldsFrom(List<String> fieldsFrom) {
            this.m_fieldsFrom = fieldsFrom;
        }


        /**
         * set fields which will transfer to.
         *
         * @param fieldsTo the fieldsTo to set
         */
        public void setFieldsTo(List<String> fieldsTo) {
            this.m_fieldsTo = fieldsTo;
        }


        /**
         * set bean where data will copy to .
         *
         * @param to the to to set
         */
        public void setTo(X2BaseBean to) {
            this.m_to = to;
        }

        /**
         * run process for transfer data form <code>m_form</code> bean to <code>m_to</code> bean.
         *
         * @param save boolean
         * @return List
         */
        public List<ValidationError> transferValues(boolean save) {
            List<ValidationError> errors = copyFields(save);
            errors.addAll(copyRelation(save));
            return errors;
        }


        /**
         * @see java.lang.Object#toString()
         */
        @Override
        /**
         *
         * @see java.lang.Object#toString()
         */
        public String toString() {
            return "TransferToX2BaseBeanContainer [m_dataDictoinaryTo=" + m_dataDictoinaryTo + ", m_dataDictionaryFrom="
                    +
                    m_dataDictionaryFrom + ", m_fieldsFrom=" + m_fieldsFrom + ", m_fieldsTo=" + m_fieldsTo +
                    ", m_mapFieldPath=" + m_mapFieldPath + ", m_to=" + m_to + ", m_from=" + m_from + "]";
        }


        /**
         * @see java.lang.Object#hashCode()
         */
        @Override
        /**
         *
         * @see java.lang.Object#hashCode()
         */
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + getOuterType().hashCode();
            result = prime * result + ((m_dataDictionaryFrom == null) ? 0 : m_dataDictionaryFrom.hashCode());
            result = prime * result + ((m_dataDictoinaryTo == null) ? 0 : m_dataDictoinaryTo.hashCode());
            result = prime * result + ((m_fieldsFrom == null) ? 0 : m_fieldsFrom.hashCode());
            result = prime * result + ((m_fieldsTo == null) ? 0 : m_fieldsTo.hashCode());
            result = prime * result + ((m_from == null) ? 0 : m_from.hashCode());
            result = prime * result + ((m_mapFieldPath == null) ? 0 : m_mapFieldPath.hashCode());
            result = prime * result + ((m_to == null) ? 0 : m_to.hashCode());
            return result;
        }


        /**
         * copy fields form <code>m_form</code> bean to <code>m_to</code> bean.
         *
         * @param save if true and <code>m_to</code> has changes - save it
         * @return List
         */
        protected List<ValidationError> copyFields(boolean save) {
            List<ValidationError> errors = new ArrayList<ValidationError>();
            if (m_from != null && m_to != null && m_mapFieldPath != null && !m_mapFieldPath.isEmpty()) {
                for (Entry<ModelProperty, ModelProperty> entry : m_mapFieldPath.entrySet()) {
                    ModelProperty fieldPathFrom = entry.getKey();
                    ModelProperty fieldPathTo = entry.getValue();
                    if (fieldPathFrom != null && fieldPathTo != null) {
                        Object fieldValue = m_from.getFieldValueByBeanPath(fieldPathFrom.getBeanPath());
                        fieldValue = translateObject(fieldPathFrom, fieldPathTo, fieldValue);
                        Object object = m_to.getFieldValueByBeanPath(fieldPathTo.getBeanPath());
                        if ((object == null || object.toString().isEmpty()) && fieldValue != null) {
                            m_to.setFieldValueByBeanPath(fieldPathTo.getBeanPath(), fieldValue);
                        }
                    }
                }
                if (save && m_to.isDirty()) {
                    errors = getBroker().saveBean(m_to);
                }
            }
            return errors;
        }


        /**
         * copy relaton form <code>m_form</code> bean to <code>m_to</code> bean.
         *
         * @param save if true and <code>m_to</code> has changes - save it
         * @return List
         */
        protected List<ValidationError> copyRelation(boolean save) {
            List<ValidationError> errors = new ArrayList<ValidationError>();
            return errors;
        }


        /**
         * create map based on <code>fieldsFrom</code> and <code>fieldsTo</code><br>
         * for each field create ModelProperty and put into map, key ModelProperty for
         * <code>from(fieldsFrom)</code> field, value ModelProperty for <code>to(fieldsTo)</code>
         * field<br>
         * field form and field to must have the same position in <code>fieldsFrom</code> and
         * <code>fieldsTo</code><br>
         * .
         *
         * @param mapFieldPath Map<ModelProperty,ModelProperty>
         * @param fieldsFrom List<String>
         * @param fieldsTo List<String>
         * @param ddxFrom DataDictionary
         * @param ddxTo DataDictionary
         * @param classFrom Class
         * @param classTo Class
         */
        protected void initializeFieldMap(Map<ModelProperty, ModelProperty> mapFieldPath,
                                          List<String> fieldsFrom,
                                          List<String> fieldsTo,
                                          DataDictionary ddxFrom,
                                          DataDictionary ddxTo,
                                          Class classFrom,
                                          Class classTo) {

            if (fieldsFrom != null && fieldsTo != null && fieldsFrom.size() == fieldsTo.size()
                    && mapFieldPath != null) {
                for (int i = 0; i < fieldsFrom.size(); i++) {
                    ModelProperty pathFrom = getPath(fieldsFrom.get(i), classFrom, ddxFrom);
                    ModelProperty pathTo = getPath(fieldsTo.get(i), classTo, ddxTo);
                    mapFieldPath.put(pathFrom, pathTo);
                }
            }
        }


        /**
         * create map based on <code>fieldsFrom</code> and <code>fieldsTo</code><br>
         * for each field create ModelProperty and put into map, key ModelProperty for
         * <code>from(fieldsFrom)</code> field, value ModelProperty for <code>to(fieldsTo)</code>
         * field<br>
         * field form and field to must have the same position in <code>fieldsFrom</code> and
         * <code>fieldsTo</code><br>
         * .
         *
         * @param mapFieldPath Map<ModelProperty,ModelProperty>
         * @param fieldsFrom List<String>
         * @param fieldsTo List<String>
         */
        protected void initializeFieldMap(Map<ModelProperty, ModelProperty> mapFieldPath,
                                          List<String> fieldsFrom,
                                          List<String> fieldsTo) {
            initializeFieldMap(mapFieldPath, fieldsFrom, fieldsTo, getDataDictionaryFrom(), getDataDictoinaryTo(),
                    getFrom().getClass(), getTo().getClass());
        }


        /**
         * method will create map using data form this container.
         *
         * @see {@link TransferToX2BaseBeanContainer#initializeFieldMap(Map, List, List)}
         */
        protected void initializeMaps() {
            initializeFieldMap(getMapFieldPath(), getFieldsFrom(), getFieldsTo());
        }


        /**
         * Gets the outer type.
         *
         * @return Sped il workflow common procedure
         */
        private SpedIlWorkflowCommonProcedure getOuterType() {
            return SpedIlWorkflowCommonProcedure.this;
        }

    }

    public static final String ALIAS_CONTACT_DATE = "contact-date";
    public static final String ALIAS_CH_IND_GOAL_SUB = "ch-ind-goal-sub";
    public static final String ALIAS_DATE_OF_CONFERENCE = "date-of-conference";
    public static final String ALIAS_DOMAIN_TYPES = "domain-types";
    public static final String ALIAS_DSB_PRIORITY = "dsb-priority";
    public static final String ALIAS_GFD_EMBEDDED_LIST_TYPE = "gfd-embedded-list-type";
    public static final String ALIAS_LOCATION = "location";
    public static final String ALIAS_MEETING_TYPE = "meeting-type";
    public static final String ALIAS_NOT_AMEND_LOCATION = "not-amend-location";
    public static final String ALIAS_NOT_AMEND_OTHER_SCHOOL = "not-amend-other-school";
    public static final String ALIAS_NOT_AMEND_TIME_OF_CONF = "not-amend-time-of-conf";
    public static final String ALIAS_MTG_OTHER_SCHOOL = "other-school";
    public static final String ALIAS_MTG_SCH_LOCATION = "mtg-sch-location";
    public static final String ALIAS_MTG_SYSTEM_TYPE = "mtg-system-type";
    public static final String ALIAS_MTG_TYPE = "mtg-type";
    public static final String ALIAS_OTHER_SCHOOL = "other-school";
    public static final String ALIAS_SPAPPR_CHLD_ACCOMOD_SUP = "spappr-chld-accomod-sup";
    public static final String ALIAS_STAFF_OID = "staff-oid";
    public static final String ALIAS_TIME_OF_CONF = "time-of-conf";
    public static final String ALIAS_TRANS_ASSESS_CATEGOTY = "transAssess-categoty";
    public static final String ALIAS_TRANSFER_IEP_AN_REVIEW_NEXT = "transfer-iep-an-review-next";
    public static final String ALIAS_TRANSFER_IEP_AN_REVIEW_RECENT = "transfer-iep-an-review-recent";
    public static final String ALIAS_TRANSFER_IEP_END_DATE = "transfer-iep-end-date";
    public static final String ALIAS_TRANSFER_IEP_IN_ELEG_DATE = "transfer-iep-in-eleg-date";
    public static final String ALIAS_TRANSFER_IEP_MEETING_DATE = "transfer-iep-meeting-date";
    public static final String ALIAS_TRANSFER_IEP_PRIM_DSBL = "transfer-iep-prim-dsbl";
    public static final String ALIAS_TRANSFER_IEP_RE_EVAL_LAST = "transfer-iep-re-eval-last";
    public static final String ALIAS_TRANSFER_IEP_RE_EVAL_NEXT = "transfer-iep-re-eval-next";
    public static final String ALIAS_TRANSFER_IEP_REFERAL_DATE = "transfer-iep-referal-date";
    public static final String ALIAS_TRANSFER_IEP_SEC_DSBL1 = "transfer-iep-sec-dsbl1";
    public static final String ALIAS_TRANSFER_IEP_SEC_DSBL2 = "transfer-iep-sec-dsbl2";
    public static final String ALIAS_TRANSFER_IEP_START_DATE = "transfer-iep-start-date";
    public static final String ALIAS_TRANSITION_AREA = "transition-area";

    public static final String COMMA = ",";
    public static final String EMBEDDED_ID_TRANS_ASSESS = "trans-assess";
    public static final String EMBEDDED_ID_SERVICES_TRANS = "services-trans";
    public static final String EMBEDDED_ID_IDENTIFICATION_ASSESSMENTS = "identificationAssessments";
    public static final String EMPTY = "";

    public static final String EXTENDED_DICTIOANRY_ID_SPED_IL_IEP = "SPED-IL-IEP";

    public static final String FMD_ID_TRANSFER = "SPED-XFER";
    public static final String FMD_ID_MEETING = "MTG";
    public static final String FMD_ID_IL_SUM_PERF = "SPED-IL-SUM-PERF";
    public static final String FMD_ID_IL_3454I = "SPED-IL-3454I";
    public static final String FMD_ID_IL_3454V = "SPED-IL-3454V";
    public static final String FMD_ID_IL_INDICATOR = FORM_DEF_ID_SPED_IL_INDICATOR;
    public static final String FMD_ID_3457D = "SPED-IL-3457D";
    public static final String FMD_ID_3457G = "SPED-IL-3457G";
    public static final String FMD_ID_IL3457BC = "SPED-IL-3457B/C";
    public static final String FMD_ID_IL3457C = "SPED-IL-3457C";
    public static final String FMD_ID_IL_SP_APPR = "SPED-IL-SP-APPR";

    public static final int NUMBER_OF_PHASE_ANY = -1;
    public static final int NUMBER_OF_PHASE_FIRST = 0;
    public static final int NUMBER_OF_PHASE_LAST = -2;

    public static final String RTB_OID_IL_DOMAIN_TYPES = "rtbILDmnTypes";
    public static final String RTB_ID_TRANSITION_ASSESSMENTS_TYPES = "rtbILTrAsTy";
    public static final String RTB_ID_IL_TR_AS_TY = RTB_ID_TRANSITION_ASSESSMENTS_TYPES;
    public static final String RTD_ID_IL_TR_SERV = "rtbILTrServ";

    public static final String SPACE = " ";
    public static final String STRATEGY_COUNT = "count";
    public static final String STRATEGY_FILTER_RTB = "filterRtb";
    public static final String STRATEGY_RTB = "rtb";

    public static final String WFD_OID_TRANSFER = "wfdIlSpedTrans";

    private static final String ENTER = "\n";
    private static final String MESSAGE_HAS_BEEN_ADDED = "has been added whith next values:";
    private static final String METHOD_PREFIX_ADD_TO = "addTo";
    private static final String METHOD_PREFIX_GET = "get";
    private static final String POINT = ".";
    private static final String STRING_VALUE = "Value";
    private static final String STRING_FIELD = "Field";

    private boolean m_needDeleteNotAppropriateRecords = false;
    private WorkflowProcedure m_workflowProcedure = null;
    private X2Broker m_broker = null;
    private Organization m_district = null;
    private Locale m_locale = null;
    private User m_user = null;
    private WorkflowDefinition m_workflowDefinition = null;
    private Map<String, DataDictionary> m_dictionaryMap = new HashMap<String, DataDictionary>();



    /**
     * Constructs a new SpedReferralProcedure.
     *
     * @param procedure WorkflowProcedure
     * @param definition WorkflowDefinition
     * @param district Organization
     * @param user User
     * @param broker X2Broker
     * @param locale Locale
     */
    public SpedIlWorkflowCommonProcedure(WorkflowProcedure procedure, WorkflowDefinition definition,
            Organization district,
            User user, X2Broker broker, Locale locale) {
        m_workflowProcedure = procedure;
        m_workflowDefinition = definition;
        m_broker = broker;
        m_user = user;
        m_district = district;
        m_locale = locale;

    }

    /**
     * Create new indicator13 Form Instance when Re-Evaluation Workflow is created and fill it by
     * data from
     * Form Instance indicator13 of Initial Workflow.
     *
     * @param owner IepData
     * @param student SisStudent
     */
    public void addInd13FormInstances(IepData owner, SisStudent student) {
        String activeOwner = student.getActiveIepOid();

        FormInstance oldFormInstance = getSingleFormInstanceByID(activeOwner, FORM_DEF_ID_SPED_IL_INDICATOR);

        if (oldFormInstance != null) {
            GenericFormData oldStorage = (GenericFormData) oldFormInstance.getStorageObject();
            GenericFormData newStorage = (GenericFormData) oldStorage.copyBean();
            newStorage.setExtendedDataDictionaryOid(DDX_OID_IL_INDICATOR);
            getBroker().saveBean(newStorage);

            Collection<GenericFormChildData> domains = oldStorage.getGenericFormDataChildren();
            for (GenericFormChildData domain : domains) {
                GenericFormChildData newDomain = (GenericFormChildData) domain.copyBean();
                newDomain.setGenericFormDataOid(newStorage.getOid());
                newStorage.addToGenericFormDataChildren(newDomain);
            }
            getBroker().saveBean(newStorage);


            FormInstance formInstance = X2BaseBean.newInstance(FormInstance.class, getBroker().getPersistenceKey());

            formInstance.setCreatedTime(System.currentTimeMillis());
            formInstance.setFormDefinitionOid(FMD_OID_IL_INDICATOR);
            formInstance.setOwnerView(owner.getStudent().getNameView());
            formInstance.setOwnerObjectOid(owner.getOid());
            formInstance.setStorageObjectOid(newStorage.getOid());

            getBroker().saveBean(formInstance);
        }

    }


    /**
     * Create new 3457C Form Instance when Re-Evaluation Workflow is created and fill it by data
     * from
     * Form Instance 3457B/C of Initial Workflow.
     *
     * @param owner IepData
     * @param student SisStudent
     */
    public void add3457CFormInstances(IepData owner, SisStudent student) {
        // Get active Owner Oid to determine old Form Instance.
        String activeOwner = student.getActiveIepOid();

        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(FormInstance.COL_OWNER_OBJECT_OID, activeOwner);
        criteria.addEqualTo(FormInstance.REL_FORM_DEFINITION + ModelProperty.PATH_DELIMITER + X2BaseBean.COL_OID,
                FMD_OID_IL3457BC);

        QueryByCriteria query = new QueryByCriteria(FormInstance.class, criteria);

        // Get old Form Instance and copy Generic Form Data Children (domains)
        // to new Form Instance.
        FormInstance oldFormInstance = (FormInstance) getBroker().getBeanByQuery(query);

        if (oldFormInstance != null) {
            // Create new Storage and fill it by domains.
            GenericFormData newStorage = X2BaseBean.newInstance(GenericFormData.class,
                    getBroker().getPersistenceKey());

            newStorage.setExtendedDataDictionaryOid(DDX_OID_3457C);
            getBroker().saveBean(newStorage);

            Collection<GenericFormChildData> domains =
                    ((GenericFormData) oldFormInstance.getStorageObject()).getGenericFormDataChildren();

            for (GenericFormChildData domain : domains) {
                GenericFormChildData newDomain = (GenericFormChildData) domain.copyBean();
                newDomain.setGenericFormDataOid(newStorage.getOid());
                newStorage.addToGenericFormDataChildren(newDomain);
            }
            getBroker().saveBean(newStorage);

            // Create new Form Instance and bind it with new Storage.
            FormInstance formInstance = X2BaseBean.newInstance(FormInstance.class,
                    getBroker().getPersistenceKey());

            formInstance.setCreatedTime(System.currentTimeMillis());
            formInstance.setFormDefinitionOid(FMD_OID_IL3457C);
            formInstance.setOwnerView(owner.getStudent().getNameView());
            formInstance.setOwnerObjectOid(owner.getOid());
            formInstance.setStorageObjectOid(newStorage.getOid());

            getBroker().saveBean(formInstance);
        }
    }

    /**
     * start using from b-5-5 version
     * method where you can align outcome between converted Workflow<br>
     * It used when outcomes which must be aligned contains different form definition.
     *
     * @param workflow Workflow
     * @param alignNames List<String>
     */
    public void alignDifFormDefinition(Workflow workflow, List<String> alignNames) {
        AlignByFormDefinitionDefauldManager manager = new AlignByFormDefinitionDefauldManager(getBroker());
        for (String align : alignNames) {
            WorkflowProgress progress = SpedIlWorkflowCommonProcedure.getProgressByAlign(workflow, align);
            if (progress != null) {
                manager.align(progress);
            }

        }
    }

    /**
     * *****************************************************
     * *****************************************************
     * *****************************************************
     * *****************************************************
     * *****************************************************
     * *****************************************************
     * *****************************************************
     * *****************************************************
     * *****************************************************
     * *****************************************************
     * *****************************************************.
     *
     * @param iepData IepData
     * @param meetings List<IepMeeting>
     */
    /**
     * start using from b-5-5 version
     * try find another then input meetings and delete them<br>
     * input meeting can be null, in this case will delete all meetings
     *
     * @param iepData
     * @param meetings
     */
    public void deleteOtherMeeting(IepData iepData, List<IepMeeting> meetings) {

        for (IepMeeting current : iepData.getIepMeeting()) {
            if (!meetings.contains(current)) {
                getBroker().deleteBean(current);
            }
        }

    }

    /**
     * prepare SPED-IL-3454H form.
     *
     * @param iepData IepData
     * @return StringBuilder
     */
    public StringBuilder fill3454H(IepData iepData) {

        Map<String, Object> mapfilterSecTrans = new HashMap<String, Object>();
        mapfilterSecTrans.put("iplType", "Transition Planning");
        mapfilterSecTrans.put("relIplStdOid", iepData.getStudentOid());
        return prepareForminstance(iepData, "SPED-IL-3454H", STRATEGY_RTB, null, "transAssess-categoty",
                mapfilterSecTrans);

    }

    /**
     * prepare 3454I form.
     *
     * @param iepData IepData
     * @return StringBuilder
     */
    public StringBuilder fill3454I(IepData iepData) {
        Map<String, Object> mapFilter44I = new HashMap<String, Object>();
        mapFilter44I.put("isvServiceMode", "Transition Services");
        mapFilter44I.put("relIsvStdOid", iepData.getStudentOid());
        return prepareForminstance(iepData, FMD_ID_IL_3454I, STRATEGY_RTB, null, ALIAS_TRANSITION_AREA, mapFilter44I);
    }

    /**
     * prepare SPED-IL-3454V form.
     *
     * @param iepData IepData
     * @return StringBuilder
     */
    public StringBuilder fill3454V(IepData iepData) {
        return prepareForminstance(iepData, FMD_ID_IL_3454V, STRATEGY_RTB, null, "aut-conf-ch-factors",
                new HashMap<String, Object>());
    }

    /**
     * prepare 3457bc form.
     *
     * @param iepData IepData
     * @return StringBuilder
     */
    public StringBuilder fill3457BC(IepData iepData) {
        return prepareForminstance(iepData, FMD_ID_IL3457BC, STRATEGY_RTB, null, ALIAS_DOMAIN_TYPES,
                new HashMap<String, Object>());
    }

    /**
     * prepare 3457c form.
     *
     * @param iepData IepData
     * @return StringBuilder
     */
    public StringBuilder fill3457C(IepData iepData) {
        return prepareForminstance(iepData, FMD_ID_IL3457C, STRATEGY_RTB, null, ALIAS_DOMAIN_TYPES,
                new HashMap<String, Object>());
    }

    /**
     * prepare Indicator form.
     *
     * @param iepData IepData
     * @return StringBuilder
     */
    public StringBuilder fillIndicator(IepData iepData) {
        Map<String, Object> mapFilterInd = new HashMap<String, Object>();
        mapFilterInd.put(ALIAS_GFD_EMBEDDED_LIST_TYPE, "GoalSubcategoryIndicator");
        StringBuilder message = prepareForminstance(iepData, FMD_ID_IL_INDICATOR, STRATEGY_RTB, null,
                ALIAS_CH_IND_GOAL_SUB, mapFilterInd);
        return message;
    }

    /**
     * prepare SPED-IL-SP-APPR form.
     *
     * @param iepData IepData
     * @return StringBuilder
     */
    public StringBuilder fillSpAppr(IepData iepData) {
        return prepareForminstance(iepData, FMD_ID_IL_SP_APPR, STRATEGY_COUNT, Integer.valueOf(7),
                ALIAS_SPAPPR_CHLD_ACCOMOD_SUP, new HashMap<String, Object>());
    }

    /**
     * prepare SPED-IL-SUM-PERF form.
     *
     * @param iepData IepData
     * @return StringBuilder
     */
    public StringBuilder fillSumPref(IepData iepData) {
        StringBuilder message = fill3454H(iepData);
        Map<String, Object> mapfilterSumPerf = new HashMap<String, Object>();
        mapfilterSumPerf = new HashMap<String, Object>();
        mapfilterSumPerf.put("iplType", "Recommendation");
        mapfilterSumPerf.put("relIplStdOid", iepData.getStudentOid());
        message.append(prepareForminstance(iepData, FMD_ID_IL_SUM_PERF, STRATEGY_RTB, null, "transAssess-recom-area",
                mapfilterSumPerf));
        mapfilterSumPerf = new HashMap<String, Object>();
        mapfilterSumPerf.put("relIplStdOid", iepData.getStudentOid());
        List<String> needCodesSumPerf = new ArrayList<String>();
        needCodesSumPerf.add("Academic Read & Math");
        needCodesSumPerf.add("Functional Perform");
        needCodesSumPerf.add("Independent Living");
        needCodesSumPerf.add("Communication Status");
        needCodesSumPerf.add("Vocational & Career");
        message.append(prepareForminstance(iepData, FMD_ID_IL_SUM_PERF, STRATEGY_FILTER_RTB, needCodesSumPerf,
                "iplType", mapfilterSumPerf));
        return message;
    }

    /**
     * return DataDictionary from FormInstance.
     *
     * @param formInstance FormInstance
     * @return Data dictionary
     */
    public DataDictionary getDataDictionaryByFormInstance(FormInstance formInstance) {
        ExtendedDataDictionary extendedDataDictionary = formInstance.getFormDefinition().getExtendedDataDictionary();
        return DataDictionary.getDistrictDictionary(extendedDataDictionary, getBroker().getPersistenceKey());
    }

    /**
     * return DataDictionary by extendedDataDictionaryID.
     *
     * @param extendedDataDictionaryID String
     * @return Data dictionary
     */
    public DataDictionary getDictionaryByExtendedDictionaryId(String extendedDataDictionaryID) {
        DataDictionary returnValue = m_dictionaryMap.get(extendedDataDictionaryID);
        if (returnValue == null) {
            Criteria criteria = new Criteria();
            criteria.addEqualTo(ExtendedDataDictionary.COL_ID, extendedDataDictionaryID);
            QueryByCriteria byCriteria = new QueryByCriteria(ExtendedDataDictionary.class, criteria);
            ExtendedDataDictionary extendedDataDictionary =
                    (ExtendedDataDictionary) getBroker().getBeanByQuery(byCriteria);
            returnValue = DataDictionary.getDistrictDictionary(extendedDataDictionary, getBroker().getPersistenceKey());
            m_dictionaryMap.put(extendedDataDictionaryID, returnValue);
        }
        return returnValue;
    }

    /**
     * Gets the organization.
     *
     * @return Organization
     */
    public Organization getOrganization() {
        return m_district;
    }

    /**
     * try to find out WorkflowProgress which contain input align.
     *
     * @param workflow Workflow
     * @param align String
     * @return Workflow progress
     */
    public static WorkflowProgress getProgressByAlign(Workflow workflow, String align) {
        WorkflowProgress returnProgress = null;
        for (WorkflowProgress progress : workflow.getWorkflowProgress()) {
            WorkflowPhaseOutcome outcome = progress.getWorkflowPhaseOutcome();
            String aligID = outcome.getAlignmentId();
            if (!StringUtils.isEmpty(aligID) && aligID.equals(align)) {
                returnProgress = progress;
                break;
            }
        }
        return returnProgress;
    }

    /**
     * Gets the reference codes by rtb oid.
     *
     * @param rtbOid String
     * @return Collection<ReferenceCode> by reference table oid
     */
    public Collection<ReferenceCode> getReferenceCodesByRtbOid(String rtbOid) {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, rtbOid);
        QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, criteria);
        return getBroker().getCollectionByQuery(query);
    }

    /**
     * try find ViewTemplate by context and return it name.
     *
     * @param context String
     * @return String
     */
    public String getTemplateNameByContext(String context) {
        String returnName = null;
        Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ViewTemplate.COL_CONTEXT, context);
        ViewTemplate viewTemplate =
                (ViewTemplate) getBroker().getBeanByQuery(new QueryByCriteria(ViewTemplate.class, criteria));
        if (viewTemplate != null) {
            returnName = viewTemplate.getName();
        }

        return returnName == null ? EMPTY : returnName;
    }

    /**
     * return TransferContainerFabric.
     *
     * @param progress WorkflowProgress
     * @param ddx DataDictionary
     * @return Transfer container fabric
     * @see
     */
    public TransferContainerFabric getTransferContainerFabric(WorkflowProgress progress, DataDictionary ddx) {
        TransferContainerFabric transferContainerFabric = new TransferContainerFabric(progress, ddx, getBroker());
        return transferContainerFabric;
    }

    /**
     * Gets the validation errors.
     *
     * @return ValidationError form current workflowProcedure
     */
    public List<ValidationError> getValidationErrors() {
        List<ValidationError> errors = new ArrayList<ValidationError>();
        if (m_workflowProcedure != null) {
            errors.addAll(m_workflowProcedure.getValidationErrors());
        }
        return errors;
    }

    /**
     * try find MeetingAttendanceManager form WorkflowMeetingDetail if current detail has all
     * fromdefinitionOids described on <code>trimmedFormDefinitionOids</code>.
     *
     * @param wMeetingDetail WorkflowMeetingDetail
     * @param trimmedFormDefinitionOids List<String>
     * @return Meeting attendance manager
     */
    public MeetingAttendanceManager getMAttManagerFromWMeetingDetail(WorkflowMeetingDetail wMeetingDetail,
                                                                     List<String> trimmedFormDefinitionOids) {
        MeetingAttendanceManager manager = null;
        List<String> formDefOids = new ArrayList<String>();
        for (FormDefinition formDefinition : wMeetingDetail.getFormDefinitions()) {
            formDefOids.add(formDefinition.getOid().trim());
        }
        if (formDefOids.containsAll(trimmedFormDefinitionOids)) {
            String formDefinitionOid = wMeetingDetail.getCurrentFormDefinitionOid();
            MeetingDetail meetingDetail = null;
            if (!formDefinitionOid.equals(FORM_DEFINITION_OID_IL_MTG + "      ")) {
                wMeetingDetail.setCurrentFormDefinitionOid(FORM_DEFINITION_OID_IL_MTG + "      ");
                meetingDetail = (MeetingDetail) wMeetingDetail.getCurrentFormDetail();
                wMeetingDetail.setCurrentFormDefinitionOid(formDefinitionOid);
            } else {
                meetingDetail = (MeetingDetail) wMeetingDetail.getCurrentFormDetail();

            }
            if (meetingDetail != null) {
                manager = meetingDetail.getMeetingManager();
            }

        }
        return manager;
    }

    /**
     * try find meeting form instance located on the same workflow progress like input
     * formInstance<br>
     * if found - create MeetingAttendanceManager and return it<br>
     * it is will work if <code>formInstance</code> is not new bean and it has
     * <code>WorkflowProgress</code>.
     *
     * @param formInstance FormInstance
     * @return Meeting attendance manager
     */
    public MeetingAttendanceManager getMAttManagerFromNeighborFormInstance(FormInstance formInstance) {
        MeetingAttendanceManager manager = null;
        IepData iepData = (IepData) formInstance.getOwner();

        Collection<WorkflowProgressForm> fiProgressForms = formInstance.getWorkflowProgressForms();
        if (fiProgressForms.size() == 1) {
            WorkflowProgressForm fiProgressForm = fiProgressForms.iterator().next();
            WorkflowProgress progress = fiProgressForm.getWorkflowProgress();
            WorkflowProgressForm progressMeetingForm = null;
            for (WorkflowProgressForm progressForm : progress.getWorkflowProgressForms()) {
                if (!fiProgressForm.equals(progressForm)) {
                    if (progressForm.getFormInstance().getFormDefinitionOid().trim()
                            .equals(FORM_DEFINITION_OID_IL_MTG)) {
                        progressMeetingForm = progressForm;
                        break;
                    }
                }
            }
            if (progressMeetingForm != null) {
                FormInstance meetingFormInstance = progressMeetingForm.getFormInstance();
                String storageCnfgOid = meetingFormInstance.getFormDefinition().getStorageDataTableConfigOid();
                if (!StringUtils.isEmpty(storageCnfgOid) && storageCnfgOid.trim().equals("tbdIEPMeeting")) {
                    String meetingOid = meetingFormInstance.getStorageObjectOid();
                    manager = new MeetingAttendanceManager(meetingOid, iepData, getBroker());
                }
            }
        }
        return manager;
    }

    /**
     * try find neighbor meeting for 3457h form, and find team member which excused from this
     * meeting and put members name into appropriate fields into <code>formStorage</code> .
     *
     * @param formInstance FormInstance
     * @param formStorage X2BaseBean
     * @param userData UserDataContainer
     * @param alias String
     * @param dataDictionary DataDictionary
     */
    public void initializeExcusalTemplate(FormInstance formInstance,
                                          X2BaseBean formStorage,
                                          UserDataContainer userData,
                                          String alias,
                                          DataDictionary dataDictionary) {
        List<IepTeamMember> teamMembers = new ArrayList<IepTeamMember>();
        MeetingAttendanceManager manager = null;
        if (userData.getCurrentDetail() != null && userData.getCurrentDetail() instanceof WorkflowMeetingDetail) {
            WorkflowMeetingDetail wMeetingDetail = (WorkflowMeetingDetail) userData.getCurrentDetail();
            List<String> trimmedFormDefinitionOids =
                    new ArrayList<String>(Arrays.asList(FORM_DEFINITION_OID_IL_MTG, FORM_DEFINITION_OID_IL3457H));
            manager = getMAttManagerFromWMeetingDetail(wMeetingDetail, trimmedFormDefinitionOids);

        } else if (userData.getCurrentDetail() != null && userData.getCurrentDetail() instanceof FormDetail &&
                formInstance != null && formInstance.getOid() != null) {
            manager = getMAttManagerFromNeighborFormInstance(formInstance);

        }

        if (manager != null) {
            for (IepTeamMember teamMember : manager.getTeamMembers()) {
                if (manager.getExcused(teamMember.getOid())) {
                    teamMembers.add(teamMember);
                }
            }
        }
        fillExucedTeamMember(formStorage, teamMembers, alias, dataDictionary);

    }

    /**
     * prepare ParentNotificationOfConference<br>
     *
     * try find neighbor meeting for 3457d form, and find team member which invited for this meeting
     * and put these members name into appropriate fields into <code>formStorage</code>.
     *
     * @param formInstance FormInstance
     * @param formStorage X2BaseBean
     * @param userData UserDataContainer
     * @param dataDictionary DataDictionary
     */
    public void initializeParentNotificationOfConference(FormInstance formInstance,
                                                         X2BaseBean formStorage,
                                                         UserDataContainer userData,
                                                         DataDictionary dataDictionary) {
        List<IepTeamMember> teamMembers = new ArrayList<IepTeamMember>();
        MeetingAttendanceManager manager = null;
        if (userData.getCurrentDetail() != null && userData.getCurrentDetail() instanceof WorkflowMeetingDetail) {
            WorkflowMeetingDetail wMeetingDetail = (WorkflowMeetingDetail) userData.getCurrentDetail();
            List<String> trimmedFormDefinitionOids =
                    new ArrayList<String>(Arrays.asList(FORM_DEFINITION_OID_IL_MTG, FORM_DEFINITION_OID_IL3457D));
            manager = getMAttManagerFromWMeetingDetail(wMeetingDetail, trimmedFormDefinitionOids);

        } else if (userData.getCurrentDetail() != null && userData.getCurrentDetail() instanceof FormDetail &&
                formInstance != null && formInstance.getOid() != null) {
            manager = getMAttManagerFromNeighborFormInstance(formInstance);

        }

        if (manager != null) {
            for (IepTeamMember teamMember : manager.getTeamMembers()) {
                if (manager.getInvited(teamMember.getOid())) {
                    teamMembers.add(teamMember);
                }
            }
        }
        fillParticipantInto3457dFormStorage(formStorage, teamMembers, dataDictionary);
    }


    /**
     * pre-populating embedded lists for form id="SPED-IL-3457B/C" name="Consent for evaluation
     * (34-57B/C)".
     *
     * @param formDetail FormDetail
     * @param userData UserDataContainer
     * @throws X2BaseException exception
     */
    public void populateConsentEvaluation(FormDetail formDetail, UserDataContainer userData) throws X2BaseException {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, RTB_OID_IL_DOMAIN_TYPES);
        QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, criteria);
        Collection<ReferenceCode> codes = getBroker().getCollectionByQuery(query);
        EmbeddedListDetailSet embeddedListDetailSet = null;
        for (String childDetailSetId : formDetail.getChildDetailSetIds()) {
            if (childDetailSetId.equals(EMBEDDED_ID_IDENTIFICATION_ASSESSMENTS)) {
                ChildDetailSet childDetailSet = formDetail.getChildDetailSet(childDetailSetId);
                if (childDetailSet instanceof EmbeddedListDetailSet) {
                    embeddedListDetailSet = (EmbeddedListDetailSet) childDetailSet;
                }
            }
        }
        if (embeddedListDetailSet != null) {
            for (ReferenceCode referenceCode : codes) {
                GenericFormChildData gfcd = X2BaseBean.newInstance(GenericFormChildData.class,
                        getBroker().getPersistenceKey());
                gfcd.setFieldValueByAlias(ALIAS_DOMAIN_TYPES, referenceCode.getCode(), formDetail.getDataDictionary());
                X2BaseBean bean = gfcd;
                embeddedListDetailSet.addChild(bean, userData, (ModelBroker) getBroker());

            }
        }
    }

    /**
     * prepopulate indicator 13 form.
     *
     * @param formDetail FormDetail
     * @param userData UserDataContainer
     * @throws X2BaseException exception
     */
    public void populateIndicator13(FormDetail formDetail, UserDataContainer userData) throws X2BaseException {
        Collection<ReferenceCode> codes = getReferenceCodesByRtbOid(RTB_ID_TRANSITION_ASSESSMENTS_TYPES);

        List embeddedListNames = new ArrayList(Arrays.asList("GoalSubcategoryIndicator", "EmploymentArea",
                "EducationArea", "TrainingArea", "IndependentLivivngArea"));
        Map<String, EmbeddedListDetailSet> embeddedListMap = new HashMap<String, EmbeddedListDetailSet>();
        for (String childDetailSetId : formDetail.getChildDetailSetIds()) {
            if (embeddedListNames.contains(childDetailSetId)) {
                ChildDetailSet childDetailSet = formDetail.getChildDetailSet(childDetailSetId);
                if (childDetailSet instanceof EmbeddedListDetailSet) {
                    embeddedListMap.put(childDetailSetId, (EmbeddedListDetailSet) childDetailSet);
                }
            }
        }
        EmbeddedListDetailSet embeddedListGoalSubcategory = embeddedListMap.get("GoalSubcategoryIndicator");
        if (embeddedListGoalSubcategory != null) {
            for (ReferenceCode referenceCode : codes) {
                GenericFormChildData gfcd = X2BaseBean.newInstance(GenericFormChildData.class,
                        getBroker().getPersistenceKey());
                gfcd.setFieldValueByAlias(ALIAS_CH_IND_GOAL_SUB, referenceCode.getCode(),
                        formDetail.getDataDictionary());
                X2BaseBean bean = gfcd;
                embeddedListGoalSubcategory.addChild(bean, userData, (ModelBroker) getBroker());
            }
        }

        for (Entry<String, EmbeddedListDetailSet> entry : embeddedListMap.entrySet()) {
            if (!entry.getKey().equals("GoalSubcategoryIndicator")) {
                addBeanToEmbeddedList(GenericFormChildData.class, entry.getValue(), userData, 3);
            }
        }

    }

    /**
     * Populate 3454H form.
     *
     * @param formDetail FormDetail
     * @param userData UserDataContainer
     * @throws X2BaseException exception
     */
    public void populateSecondaryTransition(FormDetail formDetail, UserDataContainer userData) throws X2BaseException {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, RTB_ID_IL_TR_AS_TY);
        QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, criteria);
        Collection<ReferenceCode> codes = getBroker().getCollectionByQuery(query);

        EmbeddedListDetailSet transAssessEmbeddedList = null;
        for (String childDetailSetId : formDetail.getChildDetailSetIds()) {
            if (childDetailSetId.equals(EMBEDDED_ID_TRANS_ASSESS)) {
                ChildDetailSet childDetailSet = formDetail.getChildDetailSet(childDetailSetId);
                if (childDetailSet instanceof EmbeddedListDetailSet) {
                    transAssessEmbeddedList = (EmbeddedListDetailSet) childDetailSet;
                }
            }

        }
        if (transAssessEmbeddedList != null) {
            List<String> existingCategory = new ArrayList<String>();
            for (GenericDetail genericDetail : transAssessEmbeddedList.getChildDetails()) {
                String category = (String) genericDetail.getValueByAlias(ALIAS_TRANS_ASSESS_CATEGOTY);
                existingCategory.add(category);
            }

            for (ReferenceCode referenceCode : codes) {
                if (!existingCategory.contains(referenceCode.getCode())) {
                    IepPerformanceLevel performanceLevel = new IepPerformanceLevel(getBroker().getPersistenceKey());
                    performanceLevel.setFieldValueByAlias(ALIAS_TRANS_ASSESS_CATEGOTY, referenceCode.getCode(),
                            formDetail.getDataDictionary());
                    X2BaseBean bean = performanceLevel;
                    transAssessEmbeddedList.addChild(bean, userData, (ModelBroker) getBroker());
                }
            }
        }


    }

    /**
     * populate 3454i form
     * Form need pre-populating embedded lists, but for now form hasn't any workflow.
     * This code was create for testing
     *
     * @param formDetail FormDetail
     * @param userData UserDataContainer
     * @throws X2BaseException exception
     */
    public void populateTransitionServices(FormDetail formDetail, UserDataContainer userData) throws X2BaseException {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, RTD_ID_IL_TR_SERV);
        QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, criteria);
        Collection<ReferenceCode> codes = getBroker().getCollectionByQuery(query);

        EmbeddedListDetailSet transServiceEmbeddedList = null;
        for (String childDetailSetId : formDetail.getChildDetailSetIds()) {
            if (childDetailSetId.equals(EMBEDDED_ID_SERVICES_TRANS)) {
                ChildDetailSet childDetailSet = formDetail.getChildDetailSet(childDetailSetId);
                if (childDetailSet instanceof EmbeddedListDetailSet) {
                    transServiceEmbeddedList = (EmbeddedListDetailSet) childDetailSet;
                }
            }

        }
        if (transServiceEmbeddedList != null) {
            for (ReferenceCode referenceCode : codes) {
                IepService iepService = new IepService(getBroker().getPersistenceKey());
                iepService.setFieldValueByAlias(ALIAS_TRANSITION_AREA, referenceCode.getCode(),
                        formDetail.getDataDictionary());
                X2BaseBean bean = iepService;
                transServiceEmbeddedList.addChild(bean, userData, (ModelBroker) getBroker());
            }
        }
    }

    /**
     * prepare form instance<br>
     * create data in embedded list using difference behavior descibed on <code>strategy</code>
     * param<br>
     * logic use isNeedDeleteNotAppropriateRecords method for determine - dose it need delete old
     * records.
     *
     * @param iepData IepData
     * @param fmdId String
     * @param strategy 1) "rtb" create child bean for each code in reference table.
     *        try get reference table oid from strategyValue param. If get strategyValue == null
     *        then try
     *        found reference table in alias param<br>
     *        2) "filterRtb" create child bean for each code in reference table + apply filter:
     *        use codes whitch describe on strategyValue param. strategyValue must be List
     *        &lt;String&gt;<br>
     *        <br>
     *        3) "count" <br>
     * @param strategyValue Object
     * @param field String
     * @param methodNameForAddChild String
     * @param methodNameForGetChild String
     * @param filterMap Map<String,Object>
     * @return StringBuilder
     */
    public StringBuilder prepareForminstance(IepData iepData,
                                             String fmdId,
                                             String strategy,
                                             Object strategyValue,
                                             String field,
                                             String methodNameForAddChild,
                                             String methodNameForGetChild,
                                             Map<String, Object> filterMap) {
        List<ValidationError> errors = new ArrayList<ValidationError>();
        StringBuilder message = new StringBuilder();
        FormDefinition formDefinition = getFormDefinitionById(fmdId);
        message.append("Form: " + formDefinition.getName());
        message.append(ENTER);
        FormInstance formInstance = getFormInstanceByOids(iepData.getOid(), formDefinition.getOid());
        X2BaseBean storageData = getFormStorageFromFormInstance(formInstance, formDefinition, iepData);
        DataDictionary dataDictionary = DataDictionary.getDistrictDictionary(formDefinition.getExtendedDataDictionary(),
                getBroker().getPersistenceKey());
        Collection existingRecord = getChildCollection(storageData, methodNameForGetChild, filterMap, dataDictionary);
        ModelProperty property = getPath(field, null, dataDictionary);
        Class x2ChildClass = getClassByProperty(property);

        if (strategy.equals(STRATEGY_RTB) || strategy.equals(STRATEGY_FILTER_RTB)) {
            String rtbOid = null;
            if (strategyValue != null && strategyValue instanceof String) {
                rtbOid = (String) strategyValue;

            }

            if (rtbOid == null || rtbOid.isEmpty()) {
                ExtendedFieldAttributes extField = property.getField().getExtendedDataField();
                if (extField != null) {
                    rtbOid = extField.getReferenceTableOid();
                } else {
                    rtbOid = property.getField().getReferenceTableOid();
                }
            }

            List<String> codes = getCodesByRtbOid(rtbOid);
            List<String> allCodes = getCodesByRtbOid(rtbOid);
            if (strategy.equals(STRATEGY_FILTER_RTB)) {
                List<String> rtbCodes = (List<String>) strategyValue;
                Iterator<String> iterator = codes.iterator();
                while (iterator.hasNext()) {
                    String code = iterator.next();
                    if (!rtbCodes.contains(code)) {
                        iterator.remove();
                    }
                }
            }

            if (!storageData.isNew() && !codes.isEmpty()) {
                if (isNeedDeleteNotAppropriateRecords()) {
                    deleteExistingRecords(existingRecord, allCodes, property);
                }
                deleteExistingCodes(existingRecord, codes, property);
            }
            message.append(createNewChildRecordForEachCode(storageData, x2ChildClass,
                    methodNameForAddChild, codes, property, filterMap, dataDictionary));
        } else if (strategy.equals(STRATEGY_COUNT)) {
            int countForAdd = ((Integer) strategyValue).intValue();
            existingRecord = getChildCollection(storageData, methodNameForGetChild, filterMap, dataDictionary);

            int existingChildData = existingRecord.size();
            for (int i = 0; i < (countForAdd - existingChildData); i++) {
                message.append(message = createNewChildRecord(storageData, x2ChildClass, methodNameForAddChild,
                        filterMap, dataDictionary));
            }
        }

        if (storageData.isNew() || storageData.isDirty()) {
            if (storageData instanceof GenericFormData) {
                GenericFormData formData = (GenericFormData) storageData;
                String extendedDdxOid = formData.getExtendedDataDictionaryOid();
                if (extendedDdxOid == null || extendedDdxOid.isEmpty()) {
                    extendedDdxOid = formDefinition.getExtendedDataDictionaryOid();
                }

            }
            errors.addAll(getBroker().saveBean(storageData));

        }
        if (formInstance.isNew()) {
            fillingNewFormInstance(formInstance, iepData, storageData, formDefinition.getOid());
            errors.addAll(getBroker().saveBean(formInstance));
        }
        if (!errors.isEmpty()) {
            message = new StringBuilder();
            message.append("Form: " + formDefinition.getName());
            message.append("get some errors when tryed save form:");
            message.append(ENTER);
            for (ValidationError error : errors) {
                message.append("Cause - " + error.getCause());
                message.append(ENTER);
                message.append("Details - " + Arrays.toString(error.getDetails()));
            }
        }
        return message;
    }



    /**
     * the same like prepareForminstance + try find method for getChild and addChild automatically.
     *
     * @param iepData IepData
     * @param fmdId String
     * @param strategy String
     * @param strategyValue Object
     * @param field String
     * @param filterMap Map<String,Object>
     * @return StringBuilder
     * @see #prepareForminstance(IepData, String, String, Object, String, String, String, Map)
     */
    public StringBuilder prepareForminstance(IepData iepData,
                                             String fmdId,
                                             String strategy,
                                             Object strategyValue,
                                             String field,
                                             Map<String, Object> filterMap) {
        FormDefinition formDefinition = getFormDefinitionById(fmdId);
        String methodNameForAddChild = getMethodNameForAddChild(formDefinition, field);
        String methodNameForGetChild = getMethodNameForGetChild(formDefinition, field);
        return prepareForminstance(iepData, fmdId, strategy, strategyValue, field, methodNameForAddChild,
                methodNameForGetChild, filterMap);
    }



    /**
     * Need call this method in case if in form definition Scale type is "many"<BR>
     * and user try Uncheck this phase<br>
     * After Uncheck phase and recreating it, application will insert new fromInstance with new
     * storageTable<br>
     * In the result you get unnecessary formInstance and storage bean(previous and new)<br>
     * <br>
     * Method try to delete existing formInstanse and storage table (expect IepData) on current
     * phaseOutcome <BR>
     * if meetingtype has value - try find all formInstance which use this table like storage and
     * delete it too <BR>
     * if meetingtype is null - will delete all expect FormInstance based on IepMeeting storage
     * table <BR>
     * (include formInstance on current phaseOutcome) <BR>
     * this method also will delete other formInstance related by the same storage table <BR>
     * .
     *
     * @param progress WorkflowProgress
     * @param meetingtype String
     */
    public void rollbackMeeitngType(WorkflowProgress progress, String meetingtype) {
        String ownerOid = progress.getWorkflow().getOwnerOid();
        List<FormInstance> existingInstances = new ArrayList<FormInstance>();
        List<String> existingInstanceOids = new ArrayList<String>();
        FormInstance meetingInstance = getFormInstanceByFmdId(FMD_ID_MEETING, progress);

        // find all available form Instance on the this phaseOutcome. If meetingtype is null we need
        // exclude meeting instance
        for (WorkflowPhaseOutcomeForm form : progress.getWorkflowPhaseOutcome().getWorkflowPhaseOutcomeForms()) {
            String formDefOid = form.getFormDefinitionOid();
            FormInstance formInstance = progress.getFormInstance(formDefOid);

            if (formInstance != null
                    && formInstance.getFormDefinition().getCreationTypeEnum().equals(CreationType.INSERT)) {
                // if type null - exclude meeting Instance
                if (!(meetingtype == null && formInstance.getFormDefinition().getId().equals(FMD_ID_MEETING))) {
                    existingInstanceOids.add(formInstance.getOid());
                    existingInstances.add(formInstance);

                }
            }
        }
        // end
        // find all iep Meeting with meeting type = meetingtype
        if (meetingInstance != null && meetingtype != null) {
            DataDictionary dictionary = getDataDictionaryByFormInstance(meetingInstance);
            String meetingType = translateAliasToJavaName(dictionary, ALIAS_MTG_TYPE);
            Criteria criteria = new X2Criteria();
            criteria.addEqualTo(IepMeeting.COL_IEP_DATA_OID, ownerOid);
            criteria.addEqualTo(meetingType, meetingtype);
            Criteria instaceCriteria = new X2Criteria();
            SubQuery subQuery = new SubQuery(IepMeeting.class, X2BaseBean.COL_OID, criteria);
            instaceCriteria.addIn(FormInstance.COL_STORAGE_OBJECT_OID, subQuery);
            instaceCriteria.addNotIn(X2BaseBean.COL_OID, existingInstanceOids);
            List<FormInstance> additionInstances = (List<FormInstance>) getBroker()
                    .getCollectionByQuery(new QueryByCriteria(FormInstance.class, instaceCriteria));
            for (FormInstance formInstance : additionInstances) {
                existingInstances.add(formInstance);
                existingInstanceOids.add(formInstance.getOid());
            }
        }
        // end

        // find all other related form instance
        List<String> storageOidsForFindRelated = new ArrayList<String>();
        for (FormInstance formInstance : existingInstances) {
            X2BaseBean storageObject = formInstance.getStorageObject();
            if (!(storageObject instanceof IepData)) {
                storageOidsForFindRelated.add(storageObject.getOid());
            }
        }

        if (!storageOidsForFindRelated.isEmpty()) {
            Criteria criteria = new X2Criteria();
            criteria.addNotIn(X2BaseBean.COL_OID, existingInstanceOids);
            criteria.addIn(FormInstance.COL_STORAGE_OBJECT_OID, storageOidsForFindRelated);
            existingInstances
                    .addAll(getBroker().getCollectionByQuery(new QueryByCriteria(FormInstance.class, criteria)));
        }
        // end

        // delete form instance and storage
        for (FormInstance forDelete : existingInstances) {
            X2BaseBean storageBean = forDelete.getStorageObject();
            if (!(storageBean instanceof IepData)) {
                getBroker().deleteBean(storageBean);
            }
            getBroker().deleteBean(forDelete);
        }

    }

    /**
     * After finish phase set meeting type to the field in IepMeeting bean
     * if Meeting instance not exist create it using data form SPED-IL-3457D form
     * if form SPED-IL-3457D not exist - nothing to do.
     *
     * @param progress WorkflowProgress
     * @param meetingType String
     */
    public void setMeeitngType(WorkflowProgress progress, String meetingType) {
        List<ValidationError> errors = new ArrayList<ValidationError>();
        FormInstance meetingFormInstance = getFormInstanceByFmdId(FMD_ID_MEETING, progress);
        meetingFormInstance =
                meetingFormInstance == null ? meetingFormInstance = createMeetingFormInstance() : meetingFormInstance;

        DataDictionary dataDictionaryIep = getDataDictionaryByFormInstance(meetingFormInstance);
        meetingType = StringUtils.isEmpty(meetingType)
                ? getMeetingNameByPhaseOid(progress.getWorkflowPhaseOid(), dataDictionaryIep)
                : meetingType;
        FormInstance formInstance = getFormInstanceFromWillCopy(meetingType, progress);
        if (formInstance != null) {
            IepData iepData = (IepData) formInstance.getOwner();
            IepMeeting beanTo = (IepMeeting) meetingFormInstance.getStorageObject();
            beanTo = beanTo == null ? createIepMeeting(iepData, dataDictionaryIep, progress, meetingType) : beanTo;
            DataDictionary ddxFrom = getDataDictionaryByFormInstance(
                    getFormInstanceByFmdId(formInstance.getFormDefinition().getId(), progress));
            TransferContainerFabric transferContainerFabric = getTransferContainerFabric(progress, dataDictionaryIep);
            X2BaseBean beanFrom = formInstance.getStorageObject();


            beanTo.setFieldValueByAlias(ALIAS_MTG_TYPE, meetingType, dataDictionaryIep);
            TransferToX2BaseBeanContainer container =
                    transferContainerFabric.createTransferContainer(beanFrom, beanTo, ddxFrom, dataDictionaryIep);
            errors.addAll(container.transferValues(true));

            if (meetingFormInstance.isNew()) {
                fillingNewFormInstance(meetingFormInstance, progress.getWorkflow().getOwner(), container.getTo(),
                        meetingFormInstance.getFormDefinitionOid());
                errors.addAll(getBroker().saveBean(meetingFormInstance));
                WorkflowProgressForm progressForm =
                        X2BaseBean.newInstance(WorkflowProgressForm.class, getBroker().getPersistenceKey());
                progressForm.setWorkflowProgressOid(progress.getOid());
                progressForm.setFormInstanceOid(meetingFormInstance.getOid());
                errors.addAll(getBroker().saveBean(progressForm));
            }
        }

        if (meetingFormInstance == null || meetingFormInstance.getOid() == null) {
            errors.add(new ValidationError(ValidationConstants.BUSINESS_RULE_VIOLATION,
                    Integer.valueOf(BusinessRules.REQUIRE_FIELD), "\"Meeting date\" and \"Time\" must be fiiling"));
        }
        addValidationErrors(errors);
    }

    /**
     * try find meeting attached into outcome with "align" and set into meeting "type" and
     * "meetingType".
     *
     * @param workflow Workflow
     * @param ddx DataDictionary
     * @param align String
     * @param type TypeCode
     * @param meetingType String
     * @return IepMeeting
     */
    public IepMeeting setMeetingTypeIntoMeeting(Workflow workflow,
                                                DataDictionary ddx,
                                                String align,
                                                TypeCode type,
                                                String meetingType) {
        IepMeeting iepMeeting = null;
        WorkflowProgress progress = getProgressByAlign(workflow, align);
        if (progress != null) {
            for (WorkflowProgressForm progressForm : progress.getWorkflowProgressForms()) {
                FormInstance formInstance = progressForm.getFormInstance();
                X2BaseBean bean = formInstance.getStorageObject();

                if (bean instanceof IepMeeting) {
                    iepMeeting = (IepMeeting) bean;
                    iepMeeting.setTypeCodeEnum(type);
                    bean.setFieldValueByAlias(ALIAS_MTG_TYPE, meetingType, ddx);
                    break;
                }
            }
        }
        return iepMeeting;
    }

    /**
     * provide behavior is need to delete not Appropriate records, used in prepareForminstance
     * method .
     *
     * @param needDeleteNotAppropriateRecords void
     */
    public void setNeedDeleteNotAppropriateRecords(boolean needDeleteNotAppropriateRecords) {
        m_needDeleteNotAppropriateRecords = needDeleteNotAppropriateRecords;
    }

    /**
     * start using from b-5-5 version
     * set type to meeting and save IepData.
     *
     * @param iepData IepData
     * @param type TypeCode
     */
    public void setTypeToIep(IepData iepData, TypeCode type) {
        iepData.setMeetingTypeCodeEnum(type);
        if (iepData.isDirty()) {
            getBroker().saveBeanForced(iepData);
        }
    }

    /**
     * Translates an alias into a Java bean path name. An initialization error will be logged
     * if the alias does not exist.
     *
     * @param dictionary DataDictionary
     * @param alias String
     * @return String
     */
    public String translateAliasToJavaName(DataDictionary dictionary, String alias) {
        String javaName = null;
        DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(alias);
        if (field != null) {
            javaName = field.getJavaName();
        }
        return javaName;
    }

    /**
     * Translate alias to property.
     *
     * @param alias String
     * @param dictionary DataDictionary
     * @return String
     */
    public String translateAliasToProperty(String alias, DataDictionary dictionary) {
        DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(alias);
        return field == null ? null : field.getId();
    }

    /**
     * Add validation error.
     *
     * @param error ValidationError
     */
    protected void addValidationError(ValidationError error) {
        if (m_workflowProcedure != null) {
            m_workflowProcedure.getValidationErrors().add(error);
        }
    }

    /**
     * Adds to the list of validation errors.
     *
     * @param errors List<ValidationError>
     */
    protected void addValidationErrors(List<ValidationError> errors) {
        if (m_workflowProcedure != null) {
            m_workflowProcedure.getValidationErrors().addAll(errors);
        }
    }

    /**
     * return User.
     *
     * @return User
     */
    protected User getUser() {
        return m_user;
    }

    /**
     * Gets the workflow definition.
     *
     * @return WorkflowDefinition
     */
    protected WorkflowDefinition getWorkflowDefinition() {
        return m_workflowDefinition;
    }

    /**
     * create IepMeeting and put needed relation and default fields (meeting type).
     *
     * @param iepData IepData
     * @param dataDictionaryIep DataDictionary
     * @param progress WorkflowProgress
     * @param meetingType String
     * @return IepMeeting
     */
    IepMeeting createIepMeeting(IepData iepData,
                                DataDictionary dataDictionaryIep,
                                WorkflowProgress progress,
                                String meetingType) {
        IepMeeting iepMeeting = X2BaseBean.newInstance(IepMeeting.class, getBroker().getPersistenceKey());
        iepMeeting.setStudentOid(iepData.getStudentOid());
        iepMeeting.setIepDataOid(iepData.getOid());
        iepMeeting.setFieldValueByAlias(ALIAS_MTG_TYPE, meetingType, dataDictionaryIep);
        TypeCode typeCode = getMeetingType(progress.getWorkflow().getWorkflowDefinition().getOid().trim());
        iepMeeting.setTypeCodeEnum(typeCode);
        return iepMeeting;
    }

    /**
     * find form instance in current phaseOutcome with form definition Id like in input param
     * If not find and OutcomeForm (with form definition Id like in input param) has instance id -
     * try found formInstance in other phaseOutcome with the same instance id.
     *
     * @param formDefinitionId String
     * @param workflowProgress WorkflowProgress
     * @return Form instance
     */
    FormInstance getFormInstanceByFmdId(String formDefinitionId, WorkflowProgress workflowProgress) {
        FormInstance formInstance = null;

        for (WorkflowProgressForm progressForm : workflowProgress.getWorkflowProgressForms()) {
            if (formDefinitionId.equals(progressForm.getFormInstance().getFormDefinition().getId())) {
                formInstance = progressForm.getFormInstance();
                break;
            }
        }

        if (formInstance == null) {
            WorkflowPhaseOutcome outcome = workflowProgress.getWorkflowPhaseOutcome();
            WorkflowPhaseOutcomeForm outcomeForm = null;
            for (WorkflowPhaseOutcomeForm wOutcomeForm : outcome.getWorkflowPhaseOutcomeForms()) {
                if (wOutcomeForm.getFormDefinition().getId().equals(formDefinitionId)) {
                    outcomeForm = wOutcomeForm;
                    break;
                }
            }
            if (outcomeForm != null) {
                X2Criteria instanceIdCriteria = new X2Criteria();
                instanceIdCriteria.addNotEqualTo(FormInstance.REL_WORKFLOW_PROGRESS_FORMS + POINT +
                        WorkflowProgress.REL_WORKFLOW_PHASE_OUTCOME + POINT +
                        WorkflowPhaseOutcome.REL_WORKFLOW_PHASE_OUTCOME_FORMS + POINT +
                        X2BaseBean.COL_OID, outcomeForm.getOid());
                instanceIdCriteria.addEqualTo(FormInstance.REL_WORKFLOW_PROGRESS_FORMS + POINT +
                        WorkflowProgressForm.REL_WORKFLOW_PROGRESS + POINT +
                        WorkflowProgress.COL_WORKFLOW_OID, workflowProgress.getWorkflowOid());
                if (outcomeForm.getInstanceId() != null && !outcomeForm.getInstanceId().isEmpty()) {
                    instanceIdCriteria.addEqualTo(FormInstance.REL_WORKFLOW_PROGRESS_FORMS + POINT +
                            WorkflowProgressForm.REL_WORKFLOW_PROGRESS + POINT +
                            WorkflowProgress.REL_WORKFLOW_PHASE_OUTCOME + POINT +
                            WorkflowPhaseOutcome.REL_WORKFLOW_PHASE_OUTCOME_FORMS + POINT +
                            WorkflowPhaseOutcomeForm.COL_INSTANCE_ID, outcomeForm.getInstanceId());
                } else {
                    instanceIdCriteria.addEmpty(FormInstance.REL_WORKFLOW_PROGRESS_FORMS + POINT +
                            WorkflowProgressForm.REL_WORKFLOW_PROGRESS + POINT +
                            WorkflowProgress.REL_WORKFLOW_PHASE_OUTCOME + POINT +
                            WorkflowPhaseOutcome.REL_WORKFLOW_PHASE_OUTCOME_FORMS + POINT +
                            WorkflowPhaseOutcomeForm.COL_INSTANCE_ID, getBroker().getPersistenceKey());
                }
                instanceIdCriteria.addEqualTo(FormInstance.REL_FORM_DEFINITION + POINT +
                        FormDefinition.COL_ID, formDefinitionId);
                QueryByCriteria query = new QueryByCriteria(FormInstance.class, instanceIdCriteria);

                formInstance = (FormInstance) getBroker().getBeanByQuery(query);

            }
        }

        return formInstance;
    }

    /**
     * try find meeting type belong to input phase oid.
     *
     * @param phaseOid String
     * @param dictionary DataDictionary
     * @return String
     */
    String getMeetingNameByPhaseOid(String phaseOid, DataDictionary dictionary) {
        String meetingName = null;
        if (dictionary != null) {
            DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(ALIAS_MTG_SYSTEM_TYPE);
            if (field != null) {
                ReferenceTable refTable = field.getReferenceTable();
                if (refTable != null) {
                    for (ReferenceCode refCode : refTable.getReferenceCodes()) {
                        if (refCode.getCode().equals(phaseOid.trim())) {
                            meetingName = refCode.getDescription();
                            break;
                        }
                    }
                }
            }
        }
        return meetingName;
    }

    /**
     * try find ModelProperty used path bean class and dataDictionary .
     *
     * @param path String
     * @param beanClass Class
     * @param dataDictionary if need find extended alias
     * @return Model property
     */
    ModelProperty getPath(String path, Class beanClass, DataDictionary dataDictionary) {
        // WebUtils.getProperty(bean, property)


        ModelProperty returnModel = null;
        DataDictionaryField field = dataDictionary.findDataDictionaryFieldByAlias(path);
        DataDictionaryRelationship ddrelation = null;
        if (field == null) {
            field = dataDictionary.findDataDictionaryField(path);
            ddrelation = dataDictionary.findDataDictionaryRelationship(path);

        }
        if (field != null) {
            returnModel = new ModelProperty(field, dataDictionary);

        }
        if (ddrelation != null) {
            String dPath = ddrelation.getSystemDataRelationship().getPrimaryDataIndex().getFieldList();
            returnModel = new ModelProperty(dPath, getBroker().getPersistenceKey());
        }

        if (returnModel == null && beanClass != null) {
            returnModel = new ModelProperty(beanClass, path, dataDictionary);
        }

        return returnModel;
    }

    /**
     * try cast object.
     *
     * @param from should describe current object
     * @param to should describe ModelProperty which will used for cast object to
     * @param value Object
     * @return Object
     */
    Object translateObject(ModelProperty from, ModelProperty to, Object value) {
        Object object = null;
        if (value != null && from != null && to != null) {
            String javaFromType = from.getField().getJavaType();
            String javaToType = to.getField().getJavaType();
            String stringClassName = String.class.getName();

            if (javaFromType.equals(stringClassName) && javaToType.equals(stringClassName)) {
                object = value;
            } else if (javaFromType.equals(stringClassName) && javaToType.equals(Time.class.getName())) {
                TimeAsStringConverter tConv = (TimeAsStringConverter) ConverterFactory
                        .getConverterForClass(Converter.TIME_CONVERTER, getLocale(), true);
                object = tConv.parseSystemString((String) value);
            } else if (javaFromType.equals(stringClassName) && javaToType.equals(Date.class.getName())) {
                DateAsStringConverter dConv = (DateAsStringConverter) ConverterFactory
                        .getConverterForClass(Converter.DATE_CONVERTER, getLocale(), true);
                object = dConv.parseSystemString((String) value);
            }
        }
        return object;
    }

    /**
     * add bean to embedded list.
     *
     * @param beanClass Class
     * @param listDetailSet EmbeddedListDetailSet
     * @param userData UserDataContainer
     * @param count int
     * @throws X2BaseException exception
     */
    private void addBeanToEmbeddedList(Class beanClass,
                                       EmbeddedListDetailSet listDetailSet,
                                       UserDataContainer userData,
                                       int count)
            throws X2BaseException {
        if (listDetailSet != null) {
            for (int i = 0; i < count; i++) {
                X2BaseBean baseBean = X2BaseBean.newInstance(beanClass, getBroker().getPersistenceKey());
                listDetailSet.addChild(baseBean, userData, (ModelBroker) getBroker());
            }
        }
    }

    /**
     * create Meeting FormInstance
     * and fill require fields.
     *
     * @return FormInstance
     */
    private FormInstance createMeetingFormInstance() {
        FormInstance meetingFormInstance = X2BaseBean.newInstance(FormInstance.class, getBroker().getPersistenceKey());
        String mtgFormDefOid = getFormDefinitionById(FMD_ID_MEETING).getOid();
        meetingFormInstance.setFormDefinitionOid(mtgFormDefOid);
        return meetingFormInstance;
    }

    /**
     * add <code>x2ChildClass</code> object into <code>parent</code><br>
     * also put values used <code>filterMap</code>.
     *
     * @param parent X2BaseBean
     * @param x2ChildClass Class
     * @param methodNameForAddChild String
     * @param filterMap Key - path(field or alias name) value - value for this path
     * @param dictionary DataDictionary
     * @return StringBuilder
     */
    private StringBuilder createNewChildRecord(X2BaseBean parent,
                                               Class x2ChildClass,
                                               String methodNameForAddChild,
                                               Map<String, Object> filterMap,
                                               DataDictionary dictionary) {
        StringBuilder message = new StringBuilder();
        try {
            Method method = getMethod(parent.getClass(), methodNameForAddChild, new Class[] {x2ChildClass});

            X2BaseBean baseBean = X2BaseBean.newInstance(x2ChildClass, getBroker().getPersistenceKey());
            StringBuilder partOfMessage = new StringBuilder();
            String beanName = baseBean.getClass().toString() + ENTER;
            for (Entry<String, Object> entry : filterMap.entrySet()) {
                ModelProperty property = getPath(entry.getKey(), null, dictionary);
                baseBean.setFieldValueByProperty(property, entry.getValue());
                partOfMessage.append(STRING_FIELD + entry.getKey() + SPACE + STRING_VALUE + entry.getValue());
                partOfMessage.append(ENTER);
            }

            if (method != null) {
                method.invoke(parent, new Object[] {baseBean});
                message.append(beanName + MESSAGE_HAS_BEEN_ADDED);
                message.append(partOfMessage);
                message.append(ENTER);
            }

        } catch (Exception e) {
            // Do nothing;
        }
        return message;
    }

    /**
     * add <code>x2ChildClass</code> object into <code>parent</code><br>
     * and put <code>codes</code> into <code>x2ChildClass</code> object by
     * <code>property.</code><br>
     * also put values used <code>filterMap</code>
     *
     * @param parent X2BaseBean
     * @param x2ChildClass Class
     * @param methodNameForAddChild String
     * @param codes List<String>
     * @param property ModelProperty
     * @param filterMap Key - path(field or alias name) value - value for this path
     * @param dictionary DataDictionary
     * @return StringBuilder
     */
    private StringBuilder createNewChildRecordForEachCode(X2BaseBean parent,
                                                          Class x2ChildClass,
                                                          String methodNameForAddChild,
                                                          List<String> codes,
                                                          ModelProperty property,
                                                          Map<String, Object> filterMap,
                                                          DataDictionary dictionary) {
        StringBuilder message = new StringBuilder();

        try {
            Method method = getMethod(parent.getClass(), methodNameForAddChild, new Class[] {x2ChildClass});

            StringBuilder partOfMessage = new StringBuilder();
            String beanName = x2ChildClass.toString() + ENTER;
            for (String code : codes) {
                X2BaseBean baseBean = X2BaseBean.newInstance(x2ChildClass, getBroker().getPersistenceKey());

                baseBean.setFieldValueByProperty(property, code);
                partOfMessage.append(STRING_FIELD + SPACE + property + SPACE + STRING_VALUE + code);
                partOfMessage.append(ENTER);

                for (Entry<String, Object> entry : filterMap.entrySet()) {
                    ModelProperty filterProperty = getPath(entry.getKey(), null, dictionary);
                    baseBean.setFieldValueByProperty(filterProperty, entry.getValue());
                    partOfMessage
                            .append(STRING_FIELD + SPACE + entry.getKey() + SPACE + STRING_VALUE + entry.getValue());
                    partOfMessage.append(ENTER);
                }

                if (method != null) {
                    method.invoke(parent, new Object[] {baseBean});
                    message.append(beanName);
                    message.append(ENTER);
                    message.append(MESSAGE_HAS_BEEN_ADDED);
                    message.append(ENTER);
                    message.append(partOfMessage);
                    message.append(ENTER);
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return message;

    }

    /**
     * try find first team member with <code>expectedRole</code> and delete this team member.
     *
     * @param teamMembers Collection<IepTeamMember>
     * @param expectedRole String
     * @return String
     */
    private String cutFirstConsiliencePersonName(Collection<IepTeamMember> teamMembers, String expectedRole) {
        String result = null;
        Iterator<IepTeamMember> iter = teamMembers.iterator();
        while (iter.hasNext()) {
            IepTeamMember teamMember = iter.next();
            String role = teamMember.getMemberRoleCode();
            if (role.equals(expectedRole)) {
                result = getNameAndTitle(teamMember);
                iter.remove();
                break;
            }
        }
        return result;
    }

    /**
     * try delete code from <code>codes</code> if this code exist in any record from
     * <code>existingRecords</code> on <code>property</code> field.
     *
     * @param existingRecords Collection
     * @param codes List<String>
     * @param property ModelProperty
     */
    private void deleteExistingCodes(Collection existingRecords, List<String> codes, ModelProperty property) {
        for (Object object : existingRecords) {
            if (object instanceof X2BaseBean) {
                X2BaseBean baseBean = (X2BaseBean) object;
                String code = (String) baseBean.getFieldValueByProperty(property);
                if (code != null && !code.isEmpty()) {
                    codes.remove(code);
                }
            }
        }

    }

    /**
     * try delete record from <code>existingRecords</code> if this recrod contain any code from
     * <code>codes</code> on <code>property</code> field.
     *
     * @param existingRecord Collection
     * @param codes List<String>
     * @param property ModelProperty
     */
    private void deleteExistingRecords(Collection existingRecord, List<String> codes, ModelProperty property) {
        Iterator iterator = existingRecord.iterator();

        while (iterator.hasNext()) {
            Object object = iterator.next();
            if (object instanceof X2BaseBean) {
                X2BaseBean baseBean = (X2BaseBean) object;
                String code = (String) baseBean.getFieldValueByProperty(property);
                if (code != null && !code.isEmpty()) {
                    if (!codes.contains(code)) {
                        getBroker().deleteBean(baseBean);
                        iterator.remove();
                    }
                }
            }

        }
    }



    /**
     * try fill input <code>teamMembers</code> into <code>formStorage</code> into appropriate
     * fields.
     *
     * @param formStorage X2BaseBean
     * @param teamMembers List<IepTeamMember>
     * @param alias String
     * @param dataDictionary DataDictionary
     */
    private void fillExucedTeamMember(X2BaseBean formStorage,
                                      List<IepTeamMember> teamMembers,
                                      String alias,
                                      DataDictionary dataDictionary) {
        for (int i = 0; i < teamMembers.size(); i++) {
            IepTeamMember teamMember = teamMembers.get(i);
            String memberName = getNameAndTitle(teamMember);
            formStorage.setFieldValueByAlias(alias + (i + 1), memberName, dataDictionary);
        }
    }

    /**
     * fill required values into <code>formInstance</code>.
     *
     * @param formInstance FormInstance
     * @param formOwner X2BaseBean
     * @param formStorage X2BaseBean
     * @param formDefOid String
     */
    private void fillingNewFormInstance(FormInstance formInstance,
                                        X2BaseBean formOwner,
                                        X2BaseBean formStorage,
                                        String formDefOid) {
        formInstance.setStorageObjectOid(formStorage.getOid());
        formInstance.setCreatedTime(System.currentTimeMillis());
        formInstance.setOwnerObjectOid(formOwner.getOid());
        if (formOwner instanceof IepData) {
            String nameView = ((IepData) formOwner).getStudent().getNameView();
            formInstance.setOwnerView(nameView);
        }
        formInstance.setFormDefinitionOid(formDefOid);
    }

    /**
     * try fill input <code>teamMembers</code> into <code>formStorage</code> into appropriate
     * fields.
     *
     * @param formStorage X2BaseBean
     * @param teamMembers List<IepTeamMember>
     * @param dataDictionary DataDictionary
     */
    private void fillParticipantInto3457dFormStorage(X2BaseBean formStorage,
                                                     List<IepTeamMember> teamMembers,
                                                     DataDictionary dataDictionary) {
        String leaRepr = cutFirstConsiliencePersonName(teamMembers, TM_ROLE_LEA_REPRESENTATIVE);
        if (leaRepr != null) {
            formStorage.setFieldValueByAlias(ALIAS_LEA_REPR, leaRepr, dataDictionary);
        } else {
            leaRepr = cutFirstConsiliencePersonName(teamMembers, TM_ROLE_IEP_CHAIR);
            if (leaRepr != null) {
                formStorage.setFieldValueByAlias(ALIAS_LEA_REPR, leaRepr, dataDictionary);
            }
        }
        String generalEducator = cutFirstConsiliencePersonName(teamMembers, TM_ROLE_GENERAL_EDUCATOR);
        formStorage.setFieldValueByAlias(ALIAS_GEN_ED_TEACHER, generalEducator, dataDictionary);
        String spetialEducator = cutFirstConsiliencePersonName(teamMembers, TM_ROLE_SPECIAL_EDUCATOR);
        formStorage.setFieldValueByAlias(ALIAS_SPEC_ED_TEACHER, spetialEducator, dataDictionary);
        for (int i = 0; i < teamMembers.size(); i++) {
            IepTeamMember teamMember = teamMembers.get(i);
            String memberName = getNameAndTitle(teamMember);
            formStorage.setFieldValueByAlias(ALIAS_NAME_AO_TITLE + (i + 1), memberName, dataDictionary);
        }
    }

    /**
     * Find mehod by prefix.
     *
     * @param parentClass Class
     * @param prefixName String
     * @param classArgs Class[]
     * @param returnType Class
     * @param genericReturnType String
     * @return Method
     */
    private Method findMehodByPrefix(Class parentClass,
                                     String prefixName,
                                     Class[] classArgs,
                                     Class returnType,
                                     String genericReturnType) {
        Method method = null;
        int paramLength = classArgs == null ? 0 : classArgs.length;
        exitlabel: for (Class cls = parentClass; cls != Object.class; cls = cls.getSuperclass()) {
            for (Method anyMethod : cls.getDeclaredMethods()) {
                Class[] currentParameterTypes = anyMethod.getParameterTypes();
                if (anyMethod.getName().startsWith(prefixName) && currentParameterTypes.length == paramLength) {
                    boolean theSameParameters = false;
                    if (paramLength > 0) {
                        theSameParameters = true;
                        for (int i = 0; i < paramLength; i++) {
                            if (!currentParameterTypes[i].equals(classArgs[i])) {
                                theSameParameters = false;
                                break;
                            }
                        }
                    } else {
                        theSameParameters = true;
                    }

                    if (theSameParameters) {
                        Class someClass = anyMethod.getReturnType();
                        if (returnType == null && (someClass == null || someClass.getName().equals("void"))) {
                            method = anyMethod;
                            break exitlabel;

                        } else if (someClass != null && returnType != null && someClass.equals(returnType)) {
                            if (genericReturnType != null) {
                                Type someType = anyMethod.getGenericReturnType();
                                if (someType.toString().contains(genericReturnType)) {
                                    method = anyMethod;
                                    break exitlabel;
                                }
                            } else {
                                method = anyMethod;
                                break exitlabel;
                            }
                        }
                    }
                }
            }

        }

        return method;
    }

    /**
     * Gets the broker.
     *
     * @return X2Broker
     */
    private X2Broker getBroker() {
        return m_broker;
    }

    /**
     * return filtered child beans.
     *
     * @param parent bean where will get child's
     * @param methodNameForGetChild method for call get child's
     * @param filterMap filter which will apply
     * @param dataDictionary DataDictionary
     * @return Collection
     */
    private Collection getChildCollection(X2BaseBean parent,
                                          String methodNameForGetChild,
                                          Map<String, Object> filterMap,
                                          DataDictionary dataDictionary) {
        Collection recordForReturn = null;
        try {
            Class[] args = {X2Broker.class};
            Method method = getMethod(parent.getClass(), methodNameForGetChild, args);
            if (method != null) {
                recordForReturn = (Collection) method.invoke(parent, new Object[] {getBroker()});
            } else {
                method = getMethod(parent.getClass(), methodNameForGetChild, null);
                if (method != null) {
                    recordForReturn = (Collection) method.invoke(parent);
                }
            }
        } catch (Exception e) {
            // nothing to do
        }
        if (recordForReturn == null) {
            recordForReturn = new ArrayList();
        } else {
            if (filterMap != null && !filterMap.isEmpty()) {
                Iterator iterator = recordForReturn.iterator();
                while (iterator.hasNext()) {
                    Object object = iterator.next();
                    if (object instanceof X2BaseBean) {
                        X2BaseBean bean = (X2BaseBean) object;
                        for (Entry<String, Object> entry : filterMap.entrySet()) {
                            Object needValue = entry.getValue();
                            ModelProperty property = getPath(entry.getKey(), null, dataDictionary);
                            if (property != null) {

                                Object fieldValue = bean.getFieldValueByProperty(property);
                                if (fieldValue == null || !needValue.equals(fieldValue)) {
                                    iterator.remove();
                                    break;
                                }
                            }
                        }
                    }
                }
            }
        }
        return recordForReturn;
    }

    /**
     * Gets the class by property.
     *
     * @param property ModelProperty
     * @return class which belong to <code>property</code> field
     */
    private Class getClassByProperty(ModelProperty property) {
        Class aliasClass = null;
        DataDictionaryField dataDictionaryField = property.getField();
        String aliasClassName = dataDictionaryField.getTable().getClassName();
        try {
            aliasClass = Class.forName(aliasClassName);

        } catch (Exception e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        return aliasClass;
    }

    /**
     * Gets the codes by rtb oid.
     *
     * @param refTableOid String
     * @return List codes for reference table found by <code>refTableOid</code>
     */
    private List<String> getCodesByRtbOid(String refTableOid) {
        Criteria refTableCriteria = new Criteria();
        refTableCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, refTableOid);
        QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, refTableCriteria);
        Collection<ReferenceCode> refCodes = getBroker().getCollectionByQuery(query);

        List<String> codes = new ArrayList<String>();
        for (ReferenceCode referenceCode : refCodes) {
            String code = referenceCode.getCode();
            codes.add(code);
        }
        return codes;
    }

    /**
     * find FormDefinition by <code>formDefId</code>.
     *
     * @param formDefId String
     * @return Form definition
     */
    private FormDefinition getFormDefinitionById(String formDefId) {
        Criteria criteria = new X2Criteria();
        criteria.addEqualTo(FormDefinition.COL_ID, formDefId);

        return (FormDefinition) getBroker().getBeanByQuery(new QueryByCriteria(FormDefinition.class, criteria));

    }

    /**
     * try find form instance with "Parent Notification" form (<code>FMD_ID_3457G</code> or
     * <code>FMD_ID_3457D</code>) which located on the same outcome like meeting with type
     * <code>meetingType</code><br>
     * .
     *
     * @param meetingType String
     * @param progress WorkflowProgress
     * @return Form instance
     */
    private FormInstance getFormInstanceFromWillCopy(String meetingType, WorkflowProgress progress) {
        FormInstance formInstance = null;
        if (meetingType != null) {
            String formDefinitionId = null;
            if (meetingType.equals(MeetingTypes.AMENDMENT.toString())) {
                formDefinitionId = FMD_ID_3457G;
            } else {
                formDefinitionId = FMD_ID_3457D;
            }
            formInstance = getFormInstanceByFmdId(formDefinitionId, progress);

        }
        return formInstance;
    }

    /**
     * try find FormInstance by <code>ownerOid</code> and <code>formDefOid</code><br>
     * if doesn't found - create new FormInstance.
     *
     * @param ownerOid - owner oid
     * @param formDefOid - form defintion oid
     * @return find FormInstance by criteria using input oids. <br>
     *         If formInstance not exist - create new
     */
    private FormInstance getFormInstanceByOids(String ownerOid, String formDefOid) {
        FormInstance returnFormInstance = null;
        Criteria criteria = new X2Criteria();
        criteria.addEqualTo(FormInstance.COL_OWNER_OBJECT_OID, ownerOid);
        criteria.addEqualTo(FormInstance.COL_FORM_DEFINITION_OID, formDefOid);
        returnFormInstance = (FormInstance) getBroker().getBeanByQuery(
                new QueryByCriteria(FormInstance.class, criteria));
        if (returnFormInstance == null) {
            returnFormInstance = X2BaseBean.newInstance(
                    FormInstance.class, getBroker().getPersistenceKey());
        }

        return returnFormInstance;
    }

    /**
     * try find <code>form storage object</code> used on <code>formInstance</code><br>
     * if doesn't found and CreationType is INSERT - create new storage object if<br>
     * also if storage table is IepData - return owner .
     *
     * @param formInstance FormInstance
     * @param formDefinition FormDefinition
     * @param iepData IepData
     * @return X 2 base bean
     */
    private X2BaseBean getFormStorageFromFormInstance(FormInstance formInstance,
                                                      FormDefinition formDefinition,
                                                      IepData iepData) {

        X2BaseBean returnFormStorage = null;
        if (formInstance != null && formInstance.getFormDefinition() != null) {
            returnFormStorage = formInstance.getStorageObject();
        }

        if (returnFormStorage == null) {
            String className = formDefinition.getStorageDataTableConfig().getDataTable().getClassName();
            Class x2BaseBeanClass = null;
            try {
                x2BaseBeanClass = Class.forName(className);
            } catch (ClassNotFoundException e) {
                e.printStackTrace();
            }
            if (!x2BaseBeanClass.equals(IepData.class)
                    && formDefinition.getCreationTypeEnum().equals(CreationType.INSERT)) {
                returnFormStorage = X2BaseBean.newInstance(x2BaseBeanClass, getBroker().getPersistenceKey());

            } else if (x2BaseBeanClass.equals(IepData.class)) {
                returnFormStorage = iepData;
            }
        }

        return returnFormStorage;
    }

    /**
     * Gets the locale.
     *
     * @return Locale
     */
    private Locale getLocale() {
        return m_locale;
    }

    /**
     * Gets the method.
     *
     * @param parentClass Class
     * @param methodName String
     * @param classArgs Class[]
     * @return Method
     */
    private Method getMethod(Class parentClass, String methodName, Class[] classArgs) {
        Method method = null;
        for (Class cls = parentClass; cls != Object.class; cls = cls.getSuperclass()) {

            try {
                method = cls.getDeclaredMethod(methodName, classArgs);
                break;
            } catch (NoSuchMethodException e1) {
                // TODO Auto-generated catch block
            } catch (NullPointerException e) {
                // TODO: handle exception
            }
        }
        return method;
    }

    /**
     * try find method name for add child into storage object .
     *
     * @param formDefinition FormDefinition
     * @param field String
     * @return String
     */
    private String getMethodNameForAddChild(FormDefinition formDefinition, String field) {
        DataTableConfig dataTableConfig = formDefinition.getStorageDataTableConfig();
        String storageclassName = dataTableConfig.getDataTable().getClassName();

        DataDictionary dataDictionary = DataDictionary.getDistrictDictionary(formDefinition.getExtendedDataDictionary(),
                getBroker().getPersistenceKey());
        ModelProperty property = getPath(field, null, dataDictionary);
        DataDictionaryField dataDictionaryField = property.getField();

        Class aliasClass = dataDictionaryField.getTable().getBeanClass();
        Method targetMethod = null;
        try {
            Class storageClass = Class.forName(storageclassName);
            targetMethod = findMehodByPrefix(storageClass, METHOD_PREFIX_ADD_TO, new Class[] {aliasClass}, null, null);
        } catch (Exception e) {
            // nothing to do
        }

        String methodName = targetMethod == null ? EMPTY : targetMethod.getName();
        return methodName;
    }

    /**
     * try find method name for get child from storage object.
     *
     * @param formDefinition FormDefinition
     * @param alias String
     * @return String
     */
    private String getMethodNameForGetChild(FormDefinition formDefinition, String alias) {
        String methodName = null;
        DataDictionary dataDictionary = DataDictionary.getDistrictDictionary(formDefinition.getExtendedDataDictionary(),
                getBroker().getPersistenceKey());
        ModelProperty property = getPath(alias, null, dataDictionary);
        DataDictionaryField dataDictionaryField = property.getField();
        String aliasClassFullName = dataDictionaryField.getTable().getClassName();
        String aliasClassPackage = dataDictionaryField.getTable().getBeanClass().getPackage().getName() + POINT;
        String aliasClassName = aliasClassFullName.replace(aliasClassPackage, EMPTY);
        String storageClassFullName = formDefinition.getStorageDataTableConfig().getDataTable().getClassName();
        methodName = METHOD_PREFIX_GET + aliasClassName;
        Class storageClass = null;
        try {
            storageClass = Class.forName(storageClassFullName);
        } catch (ClassNotFoundException e) {
            // TODO Auto-generated catch block
        }
        Method method = getMethod(storageClass, methodName, null);

        if (method == null) {
            method = findMehodByPrefix(storageClass, METHOD_PREFIX_GET, null, Collection.class, aliasClassFullName);
        }
        methodName = method == null ? EMPTY : method.getName();
        return methodName;
    }

    /**
     * return meeting type which used on current <code>worklowDefinitionOid</code>.
     *
     * @param worklowDefinitionOid String
     * @return Type code
     */
    private TypeCode getMeetingType(String worklowDefinitionOid) {
        TypeCode meetingType = TypeCode.OTHER;
        if ("wfdIlSpedAmend".equals(worklowDefinitionOid)) {
            meetingType = TypeCode.AMENDMENT;
        } else if ("wfdIlSpedRefer".equals(worklowDefinitionOid)) {
            meetingType = TypeCode.INITIAL;
        } else if ("wfdIlSpedReev".equals(worklowDefinitionOid)) {
            meetingType = TypeCode.REEVAL;
        } else if ("wfdIlSpedRenew".equals(worklowDefinitionOid)) {
            meetingType = TypeCode.REVIEW;
        }

        return meetingType;
    }


    /**
     * return name and title in one String.
     *
     * @param teamMember IepTeamMember
     * @return String
     */
    private String getNameAndTitle(IepTeamMember teamMember) {
        String nameTitle = null;
        StringBuilder memberNameBuilder = new StringBuilder();
        if (!StringUtils.isEmpty(teamMember.getNameView())) {
            memberNameBuilder.append(teamMember.getNameView());
            memberNameBuilder.append(COMMA + SPACE);
        }

        if (!StringUtils.isEmpty(teamMember.getMemberRoleCode())) {
            memberNameBuilder.append(teamMember.getMemberRoleCode());
        }
        nameTitle = memberNameBuilder.toString();
        return nameTitle == null ? EMPTY : nameTitle;


    }

    /**
     * Gets the single form instance by ID.
     *
     * @param ownerOid String
     * @param fromDefId String
     * @return Form instance
     */
    private FormInstance getSingleFormInstanceByID(String ownerOid, String fromDefId) {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(FormInstance.COL_OWNER_OBJECT_OID, ownerOid);
        criteria.addEqualTo(FormInstance.REL_FORM_DEFINITION + ModelProperty.PATH_DELIMITER + FormDefinition.COL_ID,
                fromDefId);

        QueryByCriteria query = new QueryByCriteria(FormInstance.class, criteria);

        // Get old Form Instance and copy Generic Form Data Children (domains)
        // to new Form Instance.
        return (FormInstance) getBroker().getBeanByQuery(query);
    }

    /**
     * provide behavior is need to delete not Appropriate records, used in prepareForminstance
     * method.
     *
     * @return true, if is need delete not appropriate records
     * @see #prepareForminstance(IepData, String, String, Object, String, Map)
     * @see #prepareForminstance(IepData, String, String, Object, String, String, String, Map)
     */
    private boolean isNeedDeleteNotAppropriateRecords() {
        return m_needDeleteNotAppropriateRecords;
    }

}
