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
package com.x2dev.procedures.sys.sped.on;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.*;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.PrivilegeSet;
import com.follett.fsc.core.k12.business.ValidationConstants;
import com.follett.fsc.core.k12.business.ValidationError;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.procedures.DynamicFormProcedure;
import com.follett.fsc.core.k12.web.ApplicationContext;
import com.follett.fsc.core.k12.web.ChildDetailSet;
import com.follett.fsc.core.k12.web.EmbeddedListDetailSet;
import com.follett.fsc.core.k12.web.GenericDetail;
import com.follett.fsc.core.k12.web.GenericDetailForm;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.follett.fsc.core.k12.web.WebUtils;
import com.follett.fsc.core.k12.web.forms.FormDetail;
import com.follett.fsc.core.k12.web.nav.NavigationValue;
import com.follett.fsc.core.k12.web.struts.UserEvent;
import com.follett.fsc.core.k12.web.template.ElementContainer;
import com.follett.fsc.core.k12.web.template.EmbeddedList;
import com.follett.fsc.core.k12.web.template.Section;
import com.follett.fsc.core.k12.web.template.Template;
import com.follett.fsc.core.k12.web.template.TemplateElement;
import com.follett.fsc.core.k12.web.template.Text;
import com.follett.fsc.core.k12.web.template.TextFormat;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.business.sped.MeetingAttendanceManager;
import com.x2dev.sis.model.business.sped.SpedUtils;
import com.x2dev.sis.web.sped.MeetingDetail;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.types.PlainDate;
import java.util.*;
import java.util.Map.Entry;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.struts.action.ActionErrors;

/**
 * The Class OnSpedIepTemplateProcedure.
 *
 * @author Follett Software Company
 * @copyright 2019
 */
public class OnSpedIepTemplateProcedure implements DynamicFormProcedure {
    private static final String ALIAS_COURSE_DELIVERY = "igl-course-delivery";
    private static final String ALIAS_COURSE_DESCRIPTION = "igl-course-desc";
    private static final String ALIAS_COURSE_STAFF = "igl-course-staff";
    private static final String ALIAS_COURSE_NO = "igl-course-num";
    private static final String ALIAS_COURSE_TERM = "igl-course-term";
    private static final String ALIAS_COURSE_GRADE_LEVEL = "igl-course-grade-level";
    private static final String ALIAS_MEETING_CATEGORY = "img-category";
    private static final String ALIAS_IPRC_DECISION = "iep-iprc-decision";
    private static final String ALIAS_IPRC_PLACEMENT = "iep-iprc-placement-decision";
    private static final String ALIAS_IPRC_PREV_DECISION = "iep-iprc-prv-decision";
    private static final String ALIAS_IPRC_PREV_PLACEMENT = "iep-iprc-prv-placement";
    private static final String ALIAS_IPRC_PREV_IDENTIFICATION = "iep-iprc-prv-identification";
    private static final String ALIAS_IPRC_MEMBER = "itm-iprc-member-indicator";
    private static final String ALIAS_IPRC_EFFECTIVE_DATE = "iep-iprc-effective-date";
    private static final String ALIAS_IPRC_INITIAL_DATE = "iep-date-iprc";
    private static final String ALIAS_IPRC_LAST_DATE = "iep-iprc-last-review-date";
    private static final String ALIAS_IPRC_NEXT_DATE = "iep-iprc-next-review-date";
    private static final String DEFAULT_COURSE_DELIVERY = "AC";
    private static final String DEFAULT_GOAL = "Enter goal for this class ...";
    private static final String FIELD_IEP_OID = "iepOID";
    private static final String FIELD_IGL_GOAL = "iglGoal";
    private static final String FIELD_IGL_IDENTIFIER = "iglIdentifier";
    private static final String FORM_MEETING_FORM_ID = "ON-SPED-MTG";
    private static final String FORM_MEETING_IPRC_INSTANCE = "IP";
    private static final String KEY_CURRENT_IDENTIFICATION = "current-identification";
    private static final String KEY_GOAL_TRANSITION_ID = "goal-transition-id";
    private static final String KEY_GOAL_COURSE_ID = "goal-course-id";
    private static final String KEY_GOAL_COURSE_POPULATE = "goal-course-populate";
    private static final String KEY_UPDATE_TEMPLATE = "updateTemplate";
    private static final String KEY_MEETING_TYPE = "meeting-type";
    private static final String MATCH_VALUE_SCHEDULE_TERM = "!!ScheduleTerm!!";
    private static final String STRING_EMPTY = "";
    private static final String STRING_CONSTANT = "constant";
    private static final String TEMPLATE_ID_GOALS = "iep-goals";

    private static final String KEY_EXCEPTIONAL = "label.template.form.on.sped.iprc.Exceptional";
    private static final String KEY_NON_EXCEPTIONAL = "label.template.form.on.sped.iprc.Non.Exceptional";
    private static final String KEY_NOT_IDENTIFIED = "label.template.form.on.sped.iprc.Not.Identified";
    private static final String KEY_SELECT_SCHEDULE_TERM = "label.template.iep.Select.Schedule.Term";
    private static final String KEY_POPULATE_SUBJECTS = "label.template.iep.Populate.Subjects";
    private static final String KEY_TEAM_SIZE_ERROR = "label.template.tab.mtg.error.teamcount";

    private ModelBroker m_broker;
    private DataDictionary m_dictionary;
    private Locale m_locale;
    private PrivilegeSet m_privilegeSet;
    private boolean m_meetingTypeInitializedOnce;

    /**
     * After save template.
     *
     * @param detail GenericDetail
     * @param userData UserDataContainer
     * @param broker ModelBroker
     * @return List
     * @see com.follett.fsc.core.k12.tools.procedures.DynamicFormProcedure#afterSaveTemplate(com.follett.fsc.core.k12.web.GenericDetail,
     *      com.follett.fsc.core.k12.web.UserDataContainer,
     *      com.follett.fsc.core.k12.business.ModelBroker)
     */
    @Override
    public List<ValidationError> afterSaveTemplate(GenericDetail detail,
                                                   UserDataContainer userData,
                                                   ModelBroker broker) {
        return null;
    }

    /**
     * Initialize template.
     *
     * @param template Template
     * @param applicationContext ApplicationContext
     * @param dictionary DataDictionary
     * @param privilegeSet PrivilegeSet
     * @param locale Locale
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.procedures.DynamicFormProcedure#initializeTemplate(com.follett.fsc.core.k12.web.template.Template,
     *      com.follett.fsc.core.k12.web.ApplicationContext,
     *      com.follett.fsc.core.k12.business.dictionary.DataDictionary,
     *      com.follett.fsc.core.k12.business.PrivilegeSet, java.util.Locale)
     */
    @Override
    public void initializeTemplate(Template template,
                                   ApplicationContext applicationContext,
                                   DataDictionary dictionary,
                                   PrivilegeSet privilegeSet,
                                   Locale locale)
            throws X2BaseException {
        m_dictionary = dictionary;
        m_privilegeSet = privilegeSet;
        m_locale = locale;
    }

    /**
     * Modify form.
     *
     * @param detail GenericDetail
     * @param key String
     * @param value String
     * @param userData UserDataContainer
     * @param template Template
     * @param errorsList List
     * @return Map
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.procedures.DynamicFormProcedure#modifyForm(com.follett.fsc.core.k12.web.GenericDetail,
     *      java.lang.String, java.lang.String, com.follett.fsc.core.k12.web.UserDataContainer,
     *      com.follett.fsc.core.k12.web.template.Template, java.util.List)
     */
    @Override
    public Map<String, Object> modifyForm(GenericDetail detail,
                                          String key,
                                          String value,
                                          UserDataContainer userData,
                                          Template template,
                                          List errorsList)
            throws X2BaseException {
        Map<String, Object> returnMap = new HashMap<String, Object>();
        Map<String, Object> prop = new HashMap<String, Object>();

        if (key.equals(KEY_UPDATE_TEMPLATE)) {
            if (adjustTemplate(detail, userData, template)) {
                returnMap.put(PROPERTY_DYNAMIC_FORM_REFRESH, Integer.valueOf(UserEvent.EVENT_REFRESH));
            }
        } else if (key.equals(KEY_GOAL_TRANSITION_ID) || key.equals(KEY_GOAL_COURSE_ID)) {
            if (StringUtils.isEmpty(value)) {
                prop.put(FIELD_IGL_IDENTIFIER, Integer.toString(getGoalIdentifier(detail, key)));
                returnMap.put(PROPERTY_DYNAMIC_FORM_PROPERTIES, prop);
            }
        } else if (key.equals(KEY_GOAL_COURSE_POPULATE)) {
            boolean refresh = populateCourse(detail, userData, value, errorsList);
            if (refresh) {
                returnMap.put(PROPERTY_DYNAMIC_FORM_REFRESH, Integer.valueOf(UserEvent.EVENT_REFRESH));
            }
        } else if (key.equals(ALIAS_COURSE_NO)) {
            String description = populateOtherCourse(detail, userData, value, errorsList);
            if (!StringUtils.isEmpty(description)) {
                prop.put(translateAliasToFieldId(ALIAS_COURSE_DESCRIPTION), description);
                returnMap.put(PROPERTY_DYNAMIC_FORM_PROPERTIES, prop);
            }
        } else if (key.equals(KEY_MEETING_TYPE)) {
            setMeetingType(detail, returnMap);
        } else if (key.equals(ALIAS_IPRC_LAST_DATE)) {
            String nextDate = getNextReviewDate(detail, value);
            if (!StringUtils.isEmpty(nextDate)) {
                prop.put(translateAliasToFieldId(ALIAS_IPRC_NEXT_DATE), nextDate);
                returnMap.put(PROPERTY_DYNAMIC_FORM_PROPERTIES, prop);
            }
        } else if (key.equals(ALIAS_IPRC_EFFECTIVE_DATE)) {
            String initialDate = getInitialDate(detail, value);
            if (!StringUtils.isEmpty(initialDate)) {
                prop.put(translateAliasToFieldId(ALIAS_IPRC_INITIAL_DATE), initialDate);
                returnMap.put(PROPERTY_DYNAMIC_FORM_PROPERTIES, prop);
            }
        } else if (key.equals(KEY_CURRENT_IDENTIFICATION)) {
            String[] previousStatus = getPreviousStatus(detail, userData, template, errorsList);
            if (previousStatus[0] != null) {
                prop.put(translateAliasToFieldId(ALIAS_IPRC_PREV_IDENTIFICATION), previousStatus[0]);
            }
            if (previousStatus[1] != null) {
                prop.put(translateAliasToFieldId(ALIAS_IPRC_PREV_PLACEMENT), previousStatus[1]);
            }
            if (previousStatus[2] != null) {
                prop.put(translateAliasToFieldId(ALIAS_IPRC_PREV_DECISION), previousStatus[2]);
            }
            if (prop.size() > 0) {
                returnMap.put(PROPERTY_DYNAMIC_FORM_PROPERTIES, prop);
            }
        }
        return returnMap;
    }

    /**
     * Validate template.
     *
     * @param form GenericDetailForm
     * @param detail GenericDetail
     * @param userData UserDataContainer
     * @param broker ModelBroker
     * @return List
     * @see com.follett.fsc.core.k12.tools.procedures.DynamicFormProcedure#validateTemplate(com.follett.fsc.core.k12.web.GenericDetailForm,
     *      com.follett.fsc.core.k12.web.GenericDetail,
     *      com.follett.fsc.core.k12.web.UserDataContainer,
     *      com.follett.fsc.core.k12.business.ModelBroker)
     */
    @Override
    public List<ValidationError> validateTemplate(GenericDetailForm form,
                                                  GenericDetail detail,
                                                  UserDataContainer userData,
                                                  ModelBroker broker) {
        List<ValidationError> errors = new ArrayList<ValidationError>();
        if (FORM_MEETING_IPRC_INSTANCE.equals(getMeetingInstance(detail))) {
            if (detail instanceof MeetingDetail) {
                MeetingDetail meetingDetail = (MeetingDetail) detail;
                MeetingAttendanceManager manager = meetingDetail.getMeetingManager();
                Collection<IepTeamMember> members = manager.getTeamMembers();
                if (members != null) {
                    int memberCount = 0;
                    for (IepTeamMember member : members) {
                        if (BooleanAsStringConverter.TRUE
                                .equals(member.getFieldValueByAlias(ALIAS_IPRC_MEMBER, m_dictionary))) {
                            memberCount++;
                        }
                    }
                    if (memberCount < 3) {
                        Template template = detail.getTemplates().get(detail.getCurrentTemplateIndex());
                        String errorMsg = findResource(template, KEY_TEAM_SIZE_ERROR,
                                "There must be at least 3 IPRC team members.");
                        errors.add(new ValidationError(ValidationConstants.CUSTOM_ERROR, null, errorMsg));
                    }
                }
            }
        }

        return errors;
    }

    /**
     * Adjust template.
     *
     * @param detail GenericDetail
     * @param userData UserDataContainer
     * @param template Template
     * @return true, if successful
     */
    private boolean adjustTemplate(GenericDetail detail, UserDataContainer userData, Template template) {
        boolean value = false;
        PlainDate today = new PlainDate();
        Section section = template.getFirstSection();
        if (section != null) {
            ElementContainer parent = findParent(section, MATCH_VALUE_SCHEDULE_TERM);
            if (parent != null) {
                String labelScheduleTerm = findResource(template, KEY_SELECT_SCHEDULE_TERM, "Select schedule term:");
                String labelPopulateSubjects = findResource(template, KEY_POPULATE_SUBJECTS, "Populate Subjects");

                StringBuilder itemText = new StringBuilder();
                itemText.append(labelScheduleTerm);
                itemText.append("&nbsp;&nbsp;&nbsp;&nbsp;");
                itemText.append("<select name=\"scheduleterm\" size=\"1\"  id=\"scheduleTerm\">");
                itemText.append("<option value=\"\"></option>");
                Map<String, List<StudentSchedule>> schedules =
                        getStudentSchedulesByTerm(userData, getStudent(userData));
                Map<String, ReferenceCode> termCodes = getTermCodesMap();
                Map<String, Collection<ScheduleTermDate>> termDatesMap = getTermDatesMap(userData);
                for (String termCode : schedules.keySet()) {
                    ReferenceCode term = termCodes.get(termCode);
                    Collection<ScheduleTermDate> termDates = termDatesMap.get(termCode);
                    if (term != null) {
                        boolean selected = false;
                        if (termDates != null) {
                            for (ScheduleTermDate date : termDates) {
                                if (!today.before(date.getStartDate()) &&
                                        !today.after(date.getEndDate())) {
                                    selected = true;
                                    break;
                                }
                            }
                        }

                        itemText.append("<option value=\"");
                        itemText.append(term.getCode());
                        itemText.append("\"");
                        if (selected) {
                            itemText.append(" selected ");
                        }
                        itemText.append(">");
                        itemText.append(term.getDescription());
                        itemText.append("</option>");
                    }
                }
                itemText.append("</select>");
                itemText.append("&nbsp;&nbsp;&nbsp;&nbsp;");
                itemText.append("<button type=\"button\" onclick =\"populateSchedule()\">");
                itemText.append(labelPopulateSubjects);
                itemText.append("</button>");
                TextFormat textFormat = new TextFormat("medium", true, false, false);
                Text text = new Text(itemText.toString(), null, m_dictionary, textFormat, m_locale);

                for (TemplateElement item : parent.getChildren()) {
                    if (item instanceof Text && MATCH_VALUE_SCHEDULE_TERM.equals(((Text) item).getText())) {
                        ((List<TemplateElement>) parent.getChildren()).remove(item);
                        ((List<TemplateElement>) parent.getChildren()).add(text);
                        value = true;
                        break;
                    }
                }
            }
        }
        return value;
    }

    /**
     * Creates the new embadded child.
     *
     * @param userData UserDataContainer
     * @param embeddedDetail EmbeddedListDetailSet
     * @param properties Map<String,Object>
     * @return X2BaseBean
     * @throws X2BaseException exception
     */
    private X2BaseBean createNewEmbaddedChild(UserDataContainer userData,
                                              EmbeddedListDetailSet embeddedDetail,
                                              Map<String, Object> properties)
            throws X2BaseException {
        X2BaseBean bean = X2BaseBean.newInstance(embeddedDetail.getChildClass(), getBroker().getPersistenceKey());
        Map<String, Object> allProperties = new HashMap();
        allProperties.putAll(properties);
        allProperties.putAll(initializeSystemPreference(userData, embeddedDetail.getEmbeddedList()));
        for (Entry<String, Object> entry : allProperties.entrySet()) {
            ModelProperty modelProperty = new ModelProperty(entry.getKey(), m_dictionary);
            DataDictionaryField field = modelProperty.getField();
            if (field != null) {
                ActionErrors errors = new ActionErrors();
                Object convertedValue =
                        GenericDetail.getTypedValue(field, entry.getValue(), Locale.getDefault(), errors);
                bean.setFieldValueByBeanPath(field.getJavaName(), convertedValue);
            }
        }
        return bean;
    }

    /**
     * Find parent.
     *
     * @param container ElementContainer
     * @param matchingValue String
     * @return ElementContainer
     */
    private ElementContainer findParent(ElementContainer container, String matchingValue) {
        ElementContainer parent = null;

        for (TemplateElement child : container.getChildren()) {
            if (child instanceof Text && matchingValue.equals(((Text) child).getText())) {
                parent = container;
            } else if (child instanceof ElementContainer) {
                parent = findParent((ElementContainer) child, matchingValue);
            }

            if (parent != null) {
                break;
            }
        }

        return parent;
    }

    /**
     * Gets the all property values.
     *
     * @param detail GenericDetail
     * @param property String
     * @return Sets the
     */
    private Set<String> getAllPropertyValues(GenericDetail detail, String property) {
        Set<String> values = new HashSet();
        String value = (String) detail.getValue(property); // getPropertyValues().get(property);
        if (!StringUtils.isEmpty(value)) {
            values.add(value);
        }
        for (String childSetId : detail.getChildDetailSetIds()) {
            ChildDetailSet childSet = detail.getChildDetailSet(childSetId);
            for (GenericDetail child : childSet.getChildDetails()) {
                values.addAll(getAllPropertyValues(child, property));
            }
        }
        return values;
    }

    /**
     * Gets the broker.
     *
     * @return Model broker
     */
    private ModelBroker getBroker() {
        if (m_broker == null) {
            m_broker = new ModelBroker(m_privilegeSet);
        }

        return m_broker;
    }

    /**
     * Gets the children ids.
     *
     * @param detailSet ChildDetailSet
     * @return Sets the
     */
    private Set<String> getChildrenIds(ChildDetailSet detailSet) {
        Set<String> set = new HashSet();
        for (GenericDetail child : detailSet.getChildDetails()) {
            set.add(child.getId());
        }
        return set;
    }

    /**
     * Gets the goal identifier.
     *
     * @param detail GenericDetail
     * @param key String
     * @return int
     */
    private int getGoalIdentifier(GenericDetail detail, String key) {
        Set<String> usedIdentifiers = getAllPropertyValues(detail, FIELD_IGL_IDENTIFIER);
        int max = key.equals(KEY_GOAL_COURSE_ID) ? 1000 : 0;
        for (String identifier : usedIdentifiers) {
            int intValue = 0;
            try {
                intValue = Integer.valueOf(identifier).intValue();
            } catch (Exception e) {
                // Ignore this string
            }
            if ((intValue < 1000 || key.equals(KEY_GOAL_COURSE_ID)) && intValue > max) {
                max = intValue;
            }
        }
        return max + 1;
    }

    /**
     * Gets the goal schedule properties.
     *
     * @param schedule StudentSchedule
     * @param id int
     * @return Map
     */
    private Map<String, Object> getGoalScheduleProperties(StudentSchedule schedule, int id) {
        Map<String, Object> properties = new HashMap<String, Object>();
        properties.put(FIELD_IGL_IDENTIFIER, Integer.toString(id));
        properties.put(translateAliasToFieldId(ALIAS_COURSE_NO), schedule.getSection().getCourseView());
        properties.put(translateAliasToFieldId(ALIAS_COURSE_TERM), schedule.getSection().getScheduleTerm().getCode());
        properties.put(translateAliasToFieldId(ALIAS_COURSE_DESCRIPTION), schedule.getSection().getDescription());
        properties.put(translateAliasToFieldId(ALIAS_COURSE_STAFF), schedule.getSection().getStaffView());
        properties.put(translateAliasToFieldId(ALIAS_COURSE_GRADE_LEVEL),
                schedule.getSection().getSchoolCourse().getGradeLevel());
        properties.put(translateAliasToFieldId(ALIAS_COURSE_DELIVERY), DEFAULT_COURSE_DELIVERY);
        properties.put(FIELD_IGL_GOAL, DEFAULT_GOAL);
        return properties;
    }

    /**
     * Gets the iep oid.
     *
     * @param detail GenericDetail
     * @return String
     */
    private String getIepOid(GenericDetail detail) {
        return (String) detail.getPropertyValues().get(FIELD_IEP_OID);
    }

    /**
     * Gets the keys.
     *
     * @param embeddedListDetailSet EmbeddedListDetailSet
     * @return Sets the
     */
    private Set<String> getKeys(EmbeddedListDetailSet embeddedListDetailSet) {
        Set<String> keys = new HashSet();
        for (GenericDetail detail : embeddedListDetailSet.getChildDetails()) {
            String key = (String) detail.getPropertyValues().get(translateAliasToFieldId(ALIAS_COURSE_NO));
            if (!StringUtils.isEmpty(key)) {
                keys.add(key);
            }
        }
        return keys;
    }

    /**
     * Lookup the current identification and placement from the previous (active) Iep/Iprc.
     *
     * @param detail
     * @param userData
     * @param errorsList
     *
     * @return String[]
     */
    private String[] getPreviousStatus(GenericDetail detail,
                                       UserDataContainer userData,
                                       Template template,
                                       List errorsList) {
        String[] status = new String[3];
        String identification = (String) detail.getValueByAlias(ALIAS_IPRC_PREV_IDENTIFICATION);
        String placement = (String) detail.getValueByAlias(ALIAS_IPRC_PREV_PLACEMENT);
        String decision = (String) detail.getValueByAlias(ALIAS_IPRC_DECISION);

        Organization district = userData.getOrganization();
        ExtendedDataDictionary iepDictionary = SpedUtils.getIepDictionary(district, getBroker());
        DataDictionary dictionary =
                DataDictionary.getDistrictDictionary(iepDictionary, getBroker().getPersistenceKey());
        String iepOid = detail.getOid();
        IepData iep = getBroker().getBeanByOid(IepData.class, iepOid);
        SisStudent student = iep.getStudent();
        IepData activeIep = student.getActiveIep();

        String labelNotIdentified = findResource(template, KEY_NOT_IDENTIFIED, "Not Identified");
        String labelExceptional = findResource(template, KEY_EXCEPTIONAL, "Exceptional");
        String labelNonExceptional = findResource(template, KEY_NON_EXCEPTIONAL, "Non Exceptional");

        decision = labelNotIdentified;
        if (activeIep != null) {
            decision = labelNonExceptional;
            // For IPRC, copy previous Identification (Disabilities) into the "Previous"
            // fields.
            Collection<IepDisability> disabilities = activeIep.getIepDisability(getBroker());
            for (IepDisability idb : disabilities) {
                identification = idb.getDisabilityCode();
                if (idb.getPrimaryIndicator()) {
                    break;
                }
            }
            if (!StringUtils.isEmpty(identification)) {
                DataDictionaryField field = dictionary.findDataDictionaryField(IepDisability.class.getName(),
                        IepDisability.COL_DISABILITY_CODE);
                if (field.hasReferenceTable()) {
                    X2Criteria criteria = new X2Criteria();
                    criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, field.getReferenceTableOid());
                    criteria.addEqualTo(ReferenceCode.COL_CODE, identification);
                    ReferenceCode rcd =
                            getBroker().getBeanByQuery(new QueryByCriteria(ReferenceCode.class, criteria));
                    if (rcd != null && !StringUtils.isEmpty(rcd.getDescription())) {
                        identification = rcd.getDescription();
                    }
                }
            }
            if (!StringUtils.isEmpty(identification)) {
                decision = labelExceptional;
            } else {
                identification = labelNotIdentified;
            }
            detail.setValueByAlias(ALIAS_IPRC_PREV_IDENTIFICATION, identification);
            detail.setValueByAlias(ALIAS_IPRC_PREV_DECISION, decision);

            // If identification is empty, populate it.
            // For IPRC, copy previous Placement into the "Previous" fields.
            if (activeIep != null) {
                placement = (String) activeIep.getFieldValueByAlias(ALIAS_IPRC_PLACEMENT, dictionary);
                if (StringUtils.isEmpty(placement)) {
                    detail.setValueByAlias(ALIAS_IPRC_PREV_PLACEMENT, placement);
                }
            }
        }
        // turn placement into description.
        if (!StringUtils.isEmpty(placement)) {
            DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(ALIAS_IPRC_PLACEMENT);
            ReferenceTable table = field.getReferenceTable();
            if (table != null) {
                Collection<ReferenceCode> codes = table.getReferenceCodes(getBroker());
                for (ReferenceCode code : codes) {
                    if (code.getCode().equals(placement)) {
                        placement = code.getDescription();
                        break;
                    }
                }
            }
        }
        status[0] = identification;
        status[1] = placement;
        status[2] = decision;
        return status;
    }

    /**
     * Lookup a resource key fromthe current template.
     * Apply a default if the value does not exist.
     *
     * @param template
     * @param key
     * @param defaultValue
     *
     * @return String
     */
    private String findResource(Template template, String key, String defaultValue) {
        String value = template.translateUserKey(key, m_locale);
        if (StringUtils.isEmpty(value) || key.equals(value)) {
            value = defaultValue;
        }
        return value;
    }

    /**
     * Gets the student.
     *
     * @param userData UserDataContainer
     * @return Sis student
     */
    private SisStudent getStudent(UserDataContainer userData) {
        SisStudent student = null;
        IepData iep = userData.getCurrentRecord(IepData.class);
        if (iep != null) {
            student = iep.getStudent();
        } else {
            student = userData.getCurrentRecord(SisStudent.class);
        }
        return student;
    }

    /**
     * Gets the student schedules by term.
     *
     * @param userData UserDataContainer
     * @param student SisStudent
     * @return Map
     */
    private Map<String, List<StudentSchedule>> getStudentSchedulesByTerm(UserDataContainer userData,
                                                                         SisStudent student) {
        X2Criteria criteria = new X2Criteria();
        // Master type Class
        criteria.addEqualTo(StudentSchedule.REL_SECTION + PATH_DELIMITER +
                MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                SchoolCourse.COL_MASTER_TYPE, SchoolCourse.MASTER_TYPE_CLASS);
        // From active Schedule for the selected year.
        criteria.addEqualTo(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.REL_ACTIVE_SCHOOL_SCHEDULE_CONTEXTS + PATH_DELIMITER +
                SchoolScheduleContext.COL_DISTRICT_CONTEXT_OID,
                userData.getCurrentContext().getOid());
        // For this student
        criteria.addEqualTo(StudentSchedule.COL_STUDENT_OID, student.getOid());
        QueryByCriteria query = new QueryByCriteria(StudentSchedule.class, criteria);
        query.addOrderByAscending(
                StudentSchedule.REL_SECTION + ModelProperty.PATH_DELIMITER + MasterSchedule.COL_DESCRIPTION);
        return getBroker().getGroupedCollectionByQuery(query, StudentSchedule.REL_SECTION + ModelProperty.PATH_DELIMITER
                + MasterSchedule.REL_SCHEDULE_TERM + ModelProperty.PATH_DELIMITER + ScheduleTerm.COL_CODE, 32);
    }

    /**
     * Gets the term codes map.
     *
     * @return Map
     */
    private Map<String, ReferenceCode> getTermCodesMap() {
        Map<String, ReferenceCode> map = Collections.EMPTY_MAP;
        DataDictionaryField field = m_dictionary.findDataDictionaryField(ScheduleTerm.class.getName(),
                ScheduleTerm.COL_CODE);
        if (field != null && field.hasReferenceTable()) {
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, field.getReferenceTableOid());
            criteria.addNotEqualTo(ReferenceCode.COL_DISABLED_INDICATOR, Boolean.TRUE);
            QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, criteria);
            query.addOrderByAscending(ReferenceCode.COL_DESCRIPTION);
            map = getBroker().getMapByQuery(query, ReferenceCode.COL_CODE, 32);
        }
        return map;
    }

    /**
     * Gets the term codes map.
     *
     * @return Map
     */
    private Map<String, Collection<ScheduleTermDate>> getTermDatesMap(UserDataContainer userData) {
        Map<String, Collection<ScheduleTermDate>> map = Collections.EMPTY_MAP;
        String contextOid = userData.getCurrentContextOid();
        School school = null;
        IepData iep = userData.getCurrentRecord(IepData.class);
        if (iep != null) {
            school = iep.getStudent().getSchool();
        }
        if (school != null) {
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(ScheduleTermDate.REL_SCHEDULE_TERM + ModelProperty.PATH_DELIMITER +
                    ScheduleTerm.REL_SCHEDULE + ModelProperty.PATH_DELIMITER +
                    Schedule.COL_SCHOOL_OID, school.getOid());
            criteria.addEqualTo(ScheduleTermDate.REL_SCHEDULE_TERM + ModelProperty.PATH_DELIMITER +
                    ScheduleTerm.REL_SCHEDULE + ModelProperty.PATH_DELIMITER +
                    Schedule.REL_ACTIVE_SCHOOL_SCHEDULE_CONTEXTS + PATH_DELIMITER +
                    SchoolScheduleContext.COL_DISTRICT_CONTEXT_OID, contextOid);
            QueryByCriteria query = new QueryByCriteria(ScheduleTermDate.class, criteria);
            query.addOrderByAscending(ScheduleTermDate.REL_SCHEDULE_TERM + ModelProperty.PATH_DELIMITER +
                    ScheduleTerm.COL_CODE);
            map = getBroker().getGroupedCollectionByQuery(query, ScheduleTermDate.REL_SCHEDULE_TERM +
                    ModelProperty.PATH_DELIMITER + ScheduleTerm.COL_CODE, 32);
        }
        return map;
    }

    /**
     * Initialize system preference.
     *
     * @param userData UserDataContainer
     * @param embList EmbeddedList
     * @return Map
     */
    private Map<String, Object> initializeSystemPreference(UserDataContainer userData, EmbeddedList embList) {
        Map<String, Object> systemValues = new HashMap<String, Object>();
        for (NavigationValue value : embList.getSystemValues()) {
            Object interpretedValue = null;
            try {
                interpretedValue = WebUtils.interpretValue(value, userData, Locale.getDefault());
            } catch (X2BaseException e) {
                // Ignore
            }
            if (interpretedValue != null) {
                ModelProperty modelProperty = value.getField();
                systemValues.put(modelProperty.getFieldId(), interpretedValue);
            }
        }
        return systemValues;
    }

    /**
     * Populate course goals with new courses using course number for key
     *
     * @param detail GenericDetail
     * @param userData UserDataContainer
     * @param value String
     * @param errorsList List
     * @return true, if successful
     * @throws X2BaseException exception
     */
    private boolean populateCourse(GenericDetail detail, UserDataContainer userData, String value, List errorsList)
            throws X2BaseException {
        boolean refresh = false;
        EmbeddedListDetailSet embeddedListDetailSet = null;
        for (String childDetailSetId : detail.getChildDetailSetIds()) {
            if (childDetailSetId.equals(TEMPLATE_ID_GOALS)) {
                ChildDetailSet childDetailSet = detail.getChildDetailSet(childDetailSetId);
                if (childDetailSet instanceof EmbeddedListDetailSet) {
                    embeddedListDetailSet = (EmbeddedListDetailSet) childDetailSet;
                }
            }
        }
        if (embeddedListDetailSet != null) {
            int firstId = getGoalIdentifier(detail, KEY_GOAL_COURSE_ID);
            List<StudentSchedule> schedules = null;
            if (StringUtils.isEmpty(value)) {
                schedules = new LinkedList();
                for (List<StudentSchedule> items : getStudentSchedulesByTerm(userData, getStudent(userData))
                        .values()) {
                    schedules.addAll(items);
                }
            } else {
                schedules = getStudentSchedulesByTerm(userData, getStudent(userData)).get(value);
            }
            if (schedules != null && !schedules.isEmpty()) {
                for (StudentSchedule schedule : schedules) {
                    if (!getKeys(embeddedListDetailSet).contains(schedule.getSection().getCourseView())) {
                        Map<String, Object> properties = getGoalScheduleProperties(schedule, firstId++);
                        X2BaseBean bean = createNewEmbaddedChild(userData, embeddedListDetailSet, properties);
                        if (bean != null) {
                            if (embeddedListDetailSet.getEmbeddedList().isIndependentSave()) {
                                errorsList.addAll(getBroker().saveBean(bean, m_dictionary));
                                if (errorsList.isEmpty()) {
                                    embeddedListDetailSet.addChild(bean, userData, getBroker());
                                    refresh = true;
                                }
                            } else {
                                embeddedListDetailSet.addChild(bean, userData, getBroker());
                                refresh = true;
                            }

                        }
                    }
                }
            }
        }
        return refresh;
    }

    /**
     * For an Other Courses reference code value, find the reference code description.
     *
     * @param detail
     * @param userData
     * @param value
     * @param errorsList
     *
     * @return String
     */
    private String populateOtherCourse(GenericDetail detail,
                                       UserDataContainer userData,
                                       String value,
                                       List errorsList) {
        String description = null;
        DataDictionaryField field = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_COURSE_NO);
        ReferenceTable refTable = field.getReferenceTable();
        Map<String, ReferenceCode> codesMap = refTable.getCodeMap();
        ReferenceCode code = codesMap.get(value);
        if (code != null) {
            description = code.getDescription();
        }

        return description;
    }


    /**
     * When the form is a meeting form , detect if this is an IPRC meeting or another meeting.
     * If IRPC, preset the IPRC indicator. If another, preset the IPE indicator.
     *
     * @param detail
     * @param returnMap
     */
    private void setMeetingType(GenericDetail detail, Map<String, Object> returnMap) {
        if (!m_meetingTypeInitializedOnce) {
            String meetingType = getMeetingInstance(detail);
            if (FORM_MEETING_IPRC_INSTANCE.equals(meetingType)) { // IPRC Meeting.
                String type = (String) detail.getValueByAlias(ALIAS_MEETING_CATEGORY);
                if (!"IPRC".equals(type)) {
                    detail.setValueByAlias(ALIAS_MEETING_CATEGORY, "IPRC");
                    returnMap.put(PROPERTY_DYNAMIC_FORM_REFRESH, Integer.valueOf(UserEvent.EVENT_REFRESH));
                }
            } else if (!StringUtils.isEmpty(meetingType)) { // Another type of meeting.
                String type = (String) detail.getValueByAlias(ALIAS_MEETING_CATEGORY);
                if (!"IEP".equals(type)) {
                    detail.setValueByAlias(ALIAS_MEETING_CATEGORY, "IEP");
                    returnMap.put(PROPERTY_DYNAMIC_FORM_REFRESH, Integer.valueOf(UserEvent.EVENT_REFRESH));
                }
            }
            m_meetingTypeInitializedOnce = true;
        }
    }

    /**
     * Check the form to see if it is an IEP meeting form, and which instance it is.
     * Instance "IP" is an IPRC meeting. Anything else is an IEP meeting.
     *
     * @param detail
     *
     * @return String meeting instance ID.
     */
    private String getMeetingInstance(GenericDetail detail) {
        String meetingInstance = null;
        if (detail instanceof FormDetail) {
            FormDetail formDetail = (FormDetail) detail;
            FormInstance formInstance = formDetail.getFormInstance();
            FormDefinition formDefinition = formInstance.getFormDefinition();
            String formId = formDefinition.getId();
            String instance = null;
            List<WorkflowProgressForm> progressForms = (List) formInstance.getWorkflowProgressForms(getBroker());
            WorkflowProgressForm progressForm = (progressForms.size() > 0) ? progressForms.get(0) : null;
            if (progressForm != null) {
                WorkflowProgress progress = progressForm.getWorkflowProgress();
                if (progress != null) {
                    WorkflowPhaseOutcome phaseOutcome = progress.getWorkflowPhaseOutcome();
                    if (phaseOutcome == null) {
                        // Would like to have the outcome here, but it is sometimes empty.
                        // Dig into the phase for any outcome/form with the correct form
                        // ID/INSTANCE.
                        WorkflowPhase phase = progress.getWorkflowPhase();
                        if (phase != null) {
                            List<WorkflowPhaseOutcome> outcomes = (List) phase.getWorkflowPhaseOutcomes(getBroker());
                            for (WorkflowPhaseOutcome outcome : outcomes) {
                                for (WorkflowPhaseOutcomeForm outcomeForm : outcome
                                        .getWorkflowPhaseOutcomeForms(getBroker())) {
                                    FormDefinition outcomeFormDefinition = outcomeForm.getFormDefinition();
                                    if (FORM_MEETING_FORM_ID.equals(outcomeFormDefinition.getId())) {
                                        phaseOutcome = outcome;
                                    }
                                }
                            }
                        }
                    }
                    // phase outcome found, get/verify the form instance ID.
                    if (phaseOutcome != null) {
                        List<WorkflowPhaseOutcomeForm> forms =
                                (List) phaseOutcome.getWorkflowPhaseOutcomeForms(getBroker());
                        for (WorkflowPhaseOutcomeForm wpoForm : forms) {
                            if (wpoForm.getFormDefinitionOid().equals(formInstance.getFormDefinitionOid())) {
                                instance = wpoForm.getInstanceId();
                                break;
                            }
                        }
                    }
                }
            }

            // If we are the correct form (ON-SPED-TEAM), return the instance ID (IP) for IPRC,
            // anything else for IEP.
            if (FORM_MEETING_FORM_ID.equals(formId)) {
                meetingInstance = instance;
            }
        }
        return meetingInstance;
    }

    /**
     * When entering the Effective date, if the Initial Date is empty,
     * also set that to the same value.
     *
     * @param detail
     * @param value
     * @return
     */
    private String getInitialDate(GenericDetail detail, String value) {
        if (!StringUtils.isEmpty(value)) {
            // Get initial IPRC date to see if it is empty.
            String initialDate = (String) detail.getValueByAlias(ALIAS_IPRC_INITIAL_DATE);
            if (StringUtils.isEmpty(initialDate)) {
                // Add to detail and screen as return value.
                detail.setValueByAlias(ALIAS_IPRC_INITIAL_DATE, value);
                return value;
            }
        }
        return null;
    }

    /**
     * When entering the Last IPRC Review Date, calculate and set the Next IPRC Review Date
     * as one year in the future.
     *
     * @param detail
     * @param value
     */
    private String getNextReviewDate(GenericDetail detail, String value) {
        if (!StringUtils.isEmpty(value)) {
            Converter converter = ConverterFactory.getConverterForClass(Converter.DATE_CONVERTER, detail.getLocale());
            PlainDate lastDate = (PlainDate) converter.stringToJava(value);
            if (lastDate != null) {
                // Convert last review to PlainDate, add one year, convert to String as next review
                // date.
                Calendar cal = Calendar.getInstance(detail.getLocale());
                cal.setTimeInMillis(lastDate.getTime());
                cal.add(Calendar.YEAR, 1);
                PlainDate nextDate = new PlainDate(cal.getTimeInMillis());
                String nextDateValue = converter.javaToString(nextDate);

                // Add to detail and screen as return value.
                detail.setValueByAlias(ALIAS_IPRC_NEXT_DATE, nextDateValue);
                return nextDateValue;
            }
        }
        return null;
    }

    /**
     * Translate alias to field id.
     *
     * @param alias String
     * @return String
     */
    private String translateAliasToFieldId(String alias) {
        DataDictionaryField field = m_dictionary.findDataDictionaryFieldByAlias(alias);
        return field.getId();
    }
}
