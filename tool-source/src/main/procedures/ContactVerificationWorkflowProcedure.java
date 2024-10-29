/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2010 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

import com.follett.fsc.core.k12.beans.*;
import com.follett.fsc.core.k12.beans.BeanManager.PersistenceKey;
import com.follett.fsc.core.k12.business.InvalidDictionaryIdException;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryRelationship;
import com.follett.fsc.core.k12.tools.procedures.WorkflowProcedure;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.follett.fsc.core.k12.web.WebUtils;
import com.follett.fsc.core.k12.web.Wizard;
import com.follett.fsc.core.k12.web.presentation.AddressParser;
import com.follett.fsc.core.k12.web.workflow.InitiationConstants;
import com.x2dev.sis.model.beans.SisAddress;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.IntegerConverter;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import org.apache.commons.beanutils.PropertyUtils;

/**
 * Procedure for managing the Contact Verification workflow.
 *
 * @author X2 Development Corporation
 */
public class ContactVerificationWorkflowProcedure extends WorkflowProcedure {
    /**
     *
     */
    private static final long serialVersionUID = 1L;
    /*
     * T30036272 - Contact Verification - Posting does not update the change made
     * --------------------------------------------------------------------------------------------
     * Collection that keeps track of whether a specific Address has already been saved. If it has
     * then it won't be saved again. This is used to prevent changes to shared addresses from being
     * overwritten.
     */
    private Collection<String> m_changedAddressOids;

    /**
     * Constructs a new ContactVerificationWorkflowProcedure.
     *
     * @param definition WorkflowDefinition
     * @param district Organization
     * @param user User
     * @param broker X2Broker
     * @param locale Locale
     */
    public ContactVerificationWorkflowProcedure(WorkflowDefinition definition, Organization district,
            User user, X2Broker broker, Locale locale) {
        super(definition, district, user, broker, locale);
    }

    /**
     * Execute method for the "post" method ID. This method performs the following:
     *
     * <ul>
     * <li>Updates any properties of the Student that have been modified by the parent or admin.
     * </li>
     * <li>Updates any properties of related Contacts that have been modified by the parent or
     * admin.</li>
     * <li>Adds new Contact(s) from the form to be associated to the Student.</li>
     * <li>Deletes Contact association(s) from the Student if they are not present in the form.</li>
     * </ul>
     *
     * @param progress the WorkflowProgress
     */
    public void executePost(WorkflowProgress progress) {
        FormInstance form = progress.getFormInstance(getBroker());
        GenericFormData formData = (GenericFormData) form.getStorageObject(getBroker());

        DataDictionary dictionary = DataDictionary.getDistrictDictionary(
                form.getFormDefinition().getExtendedDataDictionary(), getBroker().getPersistenceKey());

        Map<ModelProperty, String> contactProperties = new HashMap<ModelProperty, String>();
        Map<ModelProperty, String> studentProperties = new HashMap<ModelProperty, String>();
        populateProperties(contactProperties, studentProperties, dictionary);

        m_changedAddressOids = new ArrayList<String>();

        /*
         * Retrieve the student bean and update its properties based on the form (flag the student
         * as having updated contact information).
         */
        SisStudent student = (SisStudent) progress.getWorkflow().getOwner(getBroker());
        updateAndSaveBean(student, studentProperties, formData, dictionary);

        /*
         * Retrieve the contact beans and set the properties on child forms
         */
        Collection<GenericFormChildData> children = formData.getGenericFormDataChildren(getBroker());
        Collection<StudentContact> existingContacts = student.getContacts();
        Map<String, StudentContact> existingContactsByOid = new LinkedHashMap<String, StudentContact>();

        for (StudentContact sContact : existingContacts) {
            existingContactsByOid.put(sContact.getOid(), sContact);
        }

        for (GenericFormChildData childFormData : children) {
            String oid = (String) childFormData.getFieldValueByAlias("contact-oid", dictionary);

            StudentContact studentContactRelation;
            if (oid != null) {
                studentContactRelation = (StudentContact) getBroker().getBeanByOid(StudentContact.class, oid.trim());
                existingContactsByOid.remove(oid);

            } else {
                /*
                 * This is a new contact, create related beans before updating properties.
                 */
                PersistenceKey persistenceKey = getUser().getPersistenceKey();
                studentContactRelation = createContactAndrelatedBeans(student, persistenceKey);
                oid = studentContactRelation.getContactOid();
            }

            if (studentContactRelation != null) {
                updateAndSaveBean(studentContactRelation, contactProperties, childFormData, dictionary);
            }

        }
        /*
         * Delete any contacts that have been deleted by the user.
         */
        for (StudentContact s : existingContactsByOid.values()) {
            getBroker().deleteBean(s);
        }
    }

    /**
     * Creates a new {@link StudentContact}, {@link Contact}, {@link Person} and {@link Address} and
     * sets the related OID properties on those beans.
     *
     * <p>
     * <em>Side effects: All beans created by this method are persisted to the database.</em>
     * </p>
     *
     * <p>
     * These relations must be created for each new bean in the standard/example Contact
     * Verification workflow Form that ships with Aspen.
     * <strong>It is important to create these related beans even if the properties in the form are
     * unvalued.</strong>
     * </p>
     *
     * <p>
     * i.e. it is perfectly valid for a parent to leave an Address blank. We create the Address
     * associated to the Person and leave the Address fields unvalued.
     * Same holds for any properties that are not marked "required" in the Contact Verification
     * form.
     * </p>
     *
     * @param student The student to associate the new Contact to.
     * @param persistenceKey the current user's PersistenceKey.
     * @return the StudentContact relation.
     */
    private StudentContact createContactAndrelatedBeans(final SisStudent student, final PersistenceKey persistenceKey) {
        StudentContact studentContactRelation = X2BaseBean.newInstance(StudentContact.class, persistenceKey);
        Contact contact = X2BaseBean.newInstance(Contact.class, persistenceKey);
        Person person = X2BaseBean.newInstance(Person.class, persistenceKey);
        Address physicalAddress = X2BaseBean.newInstance(Address.class, persistenceKey);

        getBroker().saveBeanForced(physicalAddress);
        person.setPhysicalAddressOid(physicalAddress.getOid());

        getBroker().saveBeanForced(person);
        contact.setPersonOid(person.getOid());

        getBroker().saveBeanForced(contact);

        studentContactRelation.setStudentOid(student.getOid());
        studentContactRelation.setContactOid(contact.getOid());
        getBroker().saveBeanForced(studentContactRelation);

        return studentContactRelation;
    }

    /**
     * Initialize form.
     *
     * @param formInstance FormInstance
     * @param formStorage X2BaseBean
     * @param userData UserDataContainer
     * @throws X2BaseException exception
     * @see
     *      com.follett.fsc.core.k12.tools.procedures.WorkflowProcedure#initializeForm(com.follett.fsc.
     *      core.k12.beans.FormInstance, com.follett.fsc.core.k12.beans.X2BaseBean,
     *      com.follett.fsc.core.k12.web.UserDataContainer)
     */
    @Override
    public void initializeForm(FormInstance formInstance, X2BaseBean formStorage, UserDataContainer userData)
            throws X2BaseException {
        GenericFormData formData = (GenericFormData) formStorage;

        DataDictionary dictionary = DataDictionary.getDistrictDictionary(
                formInstance.getFormDefinition().getExtendedDataDictionary(), getBroker().getPersistenceKey());

        Map<ModelProperty, String> contactProperties = new HashMap<ModelProperty, String>();
        Map<ModelProperty, String> studentProperties = new HashMap<ModelProperty, String>();
        populateProperties(contactProperties, studentProperties, dictionary);

        /*
         * Retrieve the student bean and set the properties on the form
         */
        Wizard wizard = userData.getCurrentWizard();
        List<String> selectedParents = (List) wizard.getFirstStepValue(InitiationConstants.SELECTION_OIDS_KEY);
        SisStudent student = (SisStudent) getBroker().getBeanByOid(SisStudent.class, selectedParents.get(0));

        setFormFieldValues(formData, dictionary, studentProperties, student);

        /*
         * Retrieve the contact beans and set the properties on child forms
         */
        Collection<StudentContact> contacts = student.getContacts(getBroker());

        for (StudentContact contact : contacts) {
            GenericFormChildData childFormData =
                    X2BaseBean.newInstance(GenericFormChildData.class, getBroker().getPersistenceKey());
            childFormData.setFieldValueByAlias("contact-oid", contact.getOid(), dictionary);

            setFormFieldValues(childFormData, dictionary, contactProperties, contact);

            formData.addToGenericFormDataChildren(childFormData);
        }

        getBroker().saveBeanForced(formData);
    }

    /**
     * Sets the form field values.
     *
     * @param formData X2BaseBean
     * @param dictionary DataDictionary
     * @param aliasesByBeanProperty Map<ModelProperty,String>
     * @param bean X2BaseBean
     * @throws X2BaseException exception
     */
    private void setFormFieldValues(X2BaseBean formData,
                                    DataDictionary dictionary,
                                    Map<ModelProperty, String> aliasesByBeanProperty,
                                    X2BaseBean bean)
            throws X2BaseException {
        for (ModelProperty property : aliasesByBeanProperty.keySet()) {
            String alias = aliasesByBeanProperty.get(property);
            String fieldValue = "";
            try {
                fieldValue = ("D".equals(property.getField().getDatabaseType()))
                        ? ((PropertyUtils.getProperty(bean, property.getBeanPath()) != null)
                                ? PropertyUtils.getProperty(bean, property.getBeanPath()).toString()
                                : "")
                        : WebUtils.getPropertyAsString(bean, property, getLocale());
            } catch (InvalidDictionaryIdException e) {
                AppGlobals.getLog().warning("Unable to retrieve fieldValue from bean: " + e);
            } catch (IllegalAccessException e) {
                AppGlobals.getLog().warning("Unable to retrieve fieldValue from bean: " + e);
            } catch (InvocationTargetException e) {
                AppGlobals.getLog().warning("Unable to retrieve fieldValue from bean: " + e);
            } catch (NoSuchMethodException e) {
                AppGlobals.getLog().warning("Unable to retrieve fieldValue from bean: " + e);
            }
            formData.setFieldValueByAlias(alias, fieldValue, dictionary);
        }
    }

    /**
     * Converts the value for the given field alias to the appropriate type of the field.
     *
     * @param form either a Form or FormChild bean
     * @param alias String
     * @param field DataDictionaryField
     * @param dictionary DataDictionary
     * @return Object
     */
    private Object getValueFromForm(X2BaseBean form,
                                    String alias,
                                    DataDictionaryField field,
                                    DataDictionary dictionary) {
        Object value = form.getFieldValueByAlias(alias, dictionary);
        if (value != null) {
            String format =
                    (((String) value).matches("^[\\d]{4}-[\\d]{2}-[\\d]{2}$") && "D".equals(field.getDatabaseType()))
                            ? "yyyy-MM-dd"
                            : null;
            Converter converter =
                    ConverterFactory.getConverterForClass(field.getJavaType(), getLocale(), field.isString(), format);
            if (converter != null) {
                // IntegerConverter.stringToJava does not treat a string of zeroes "000000" "0" as a
                // Zero value.
                // All zeroes are stored as null in user-defined fields
                // user defined numeric fields are padded with zeroes
                //
                if (converter instanceof IntegerConverter) {
                    value = Integer.valueOf((String) value); // if value == null, set value to 0
                } else {
                    value = converter.stringToJava((String) value);
                }
            }
        }
        return value;
    }

    /**
     * Parses the description into two maps of model properties - one for the student and one for
     * the contacts. Each map is keyed on a source ModelProperty and points to an alias in the
     * extended dictionary.
     *
     * @param contactProperties Map<ModelProperty,String>
     * @param studentProperties Map<ModelProperty,String>
     * @param dictionary DataDictionary
     */
    private void populateProperties(Map<ModelProperty, String> contactProperties,
                                    Map<ModelProperty, String> studentProperties,
                                    DataDictionary dictionary) {
        String description = getWorkflowDefinition().getDescription();
        Collection<String> lines = StringUtils.convertDelimitedStringToList(description, '\n', true);
        for (String line : lines) {
            String[] components = line.split("\\|");
            ModelProperty property = new ModelProperty(components[0], dictionary);
            String alias = components[1];

            if (property.getRootDataTable().getObjectPrefix().equals(SisStudent.OBJECT_PREFIX)) {
                studentProperties.put(property, alias);
            } else {
                contactProperties.put(property, alias);
            }
        }
    }

    /**
     * Updates the root bean and any related records with values from the form.
     *
     * @param rootBean X2BaseBean
     * @param properties Map<ModelProperty,String>
     * @param form X2BaseBean
     * @param dictionary DataDictionary
     */
    private void updateAndSaveBean(X2BaseBean rootBean,
                                   Map<ModelProperty, String> properties,
                                   X2BaseBean form,
                                   DataDictionary dictionary) {
        Collection<X2BaseBean> beans = new HashSet<X2BaseBean>();
        for (ModelProperty property : properties.keySet()) {
            String alias = properties.get(property);

            X2BaseBean bean = rootBean;

            // HACK: This won't work for new beans!
            List<DataDictionaryRelationship> relationships = property.getRelationships();
            for (DataDictionaryRelationship relationship : relationships) {
                bean = (X2BaseBean) bean.getFieldValueByBeanPath(relationship.getRelatedJavaName());
            }

            if (bean != null && !m_changedAddressOids.contains(bean.getOid())) {
                beans.add(bean);

                bean.setFieldValueByBeanPath(property.getField().getJavaName(),
                        getValueFromForm(form, alias, property.getField(), dictionary));
            }
        }

        for (X2BaseBean bean : beans) {
            /*
             * Parse address lines 1 and 3 into their separate components
             */
            if (bean instanceof SisAddress) {
                SisAddress address = (SisAddress) bean;

                if (address.isAddressLine01Dirty()) {
                    if (AddressParser.parseLine01(getOrganization(), address.getAddressLine01(), address, true,
                            getBroker()) == AddressParser.NO_MATCH) {
                        address.setUserValidatedIndicator1(true);
                    }
                }

                if (address.isAddressLine02Dirty()) {
                    if (AddressParser.parseLine02(getOrganization(), address.getAddressLine02(), address, true,
                            getBroker()) == AddressParser.NO_MATCH) {
                        address.setUserValidatedIndicator2(true);
                    }
                }

                if (address.isAddressLine03Dirty()) {
                    if (AddressParser.parseLine03(getOrganization(), address.getAddressLine03(), address, true,
                            getBroker()) == AddressParser.NO_MATCH) {
                        address.setUserValidatedIndicator3(true);
                    }
                }

                if (address.isDirty()) {
                    m_changedAddressOids.add(address.getOid());
                }
            }

            getBroker().saveBeanForced(bean);
        }
    }
}
