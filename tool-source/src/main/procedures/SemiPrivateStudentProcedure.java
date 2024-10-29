import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.tools.procedures.DynamicFormProcedure;
import static com.follett.fsc.core.k12.business.BusinessRules.SHELTERED_INDICATOR_MISSING;
import static com.follett.fsc.core.k12.business.BusinessRules.SHELTERED_DETAIL_MISSING;
import static com.follett.fsc.core.k12.business.ValidationConstants.BUSINESS_RULE_VIOLATION;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Address;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.PersonAddress;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.StudentAlert;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.PrivilegeSet;
import com.follett.fsc.core.k12.business.ValidationError;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.web.ApplicationContext;
import com.follett.fsc.core.k12.web.GenericDetail;
import com.follett.fsc.core.k12.web.GenericDetailForm;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.follett.fsc.core.k12.web.address.AddressTypeLocalCode;
import com.follett.fsc.core.k12.web.template.Template;
import com.x2dev.utils.StringUtils;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.struts.util.MessageResources;

/**
 * Procedure for creating/removing Student alerts triggered by the change of the
 * SemiPrivateIndicator on the Student record.
 */
public class SemiPrivateStudentProcedure extends ProcedureJavaSource implements DynamicFormProcedure {
	private static final long serialVersionUID = 1L;
	private static final String NEW_LINE_DELIMITER = "\n";
	private static final String COMMA_DELIMITER = ",";
	public static final String ALL_ALR_SHELTERED_IND_ALIAS = "all-alr-ShelteredInd";
	public static final String ALL_ALR_SHELTERED_DETAIL_ALIAS = "all-alr-ShelteredDetail";

	private enum IndicatorChangeState {
		UNCHANGED, TURNED_ON, TURNED_OFF
	}

	private IndicatorChangeState semiPrivateIndicatorChange = IndicatorChangeState.UNCHANGED;

	/**
	 * Validate template list. Called first before save
	 *
	 * @param form     GenericDetailForm
	 * @param detail   GenericDetail
	 * @param userData UserDataContainer
	 * @param broker   ModelBroker
	 * @return List<ValidationError>
	 */
	@Override
	public List<ValidationError> validateTemplate(GenericDetailForm form, GenericDetail detail,
			UserDataContainer userData, ModelBroker broker) {

		// check if the required UDF's are defined, exit if not
		List<ValidationError> errors = new ArrayList<>(validateUDFs(broker));
		if (!errors.isEmpty()) {
			return errors;
		}

		String semiPrivateEntry = (String) detail.getPropertyValues().get("relStdPsnOid.psnSemiPrvInd");

		boolean oldSemiPrivateValue = false;
		if (!StringUtils.isEmpty(detail.getOid())) {
			oldSemiPrivateValue = getSemiPrivateIndFromDatabase(detail, broker);
		}

		// reset this to prevent multiple alerts from being created
		semiPrivateIndicatorChange = IndicatorChangeState.UNCHANGED;
		if ("true".equalsIgnoreCase(semiPrivateEntry)) {
			if (!oldSemiPrivateValue) {
				// indicator switched from false to true
				semiPrivateIndicatorChange = IndicatorChangeState.TURNED_ON;
			}
		} else {
			if (oldSemiPrivateValue) {
				// indicator switched from true to false
				semiPrivateIndicatorChange = IndicatorChangeState.TURNED_OFF;
			}
		}

		return errors;
	}

	/**
	 * After save template. This method triggers all of the updates.
	 *
	 * @param detail   GenericDetail
	 * @param userData UserDataContainer
	 * @param broker   ModelBroker
	 * @return List<ValidationError>
	 */
	@Override
	public List<ValidationError> afterSaveTemplate(GenericDetail detail, UserDataContainer userData,
			ModelBroker broker) {
		List<ValidationError> errors = new ArrayList<>();

		if (semiPrivateIndicatorChange == IndicatorChangeState.TURNED_ON) {
			errors.addAll(semiPrivateTurnedOn(detail, userData, broker));
		} else if (semiPrivateIndicatorChange == IndicatorChangeState.TURNED_OFF) {
			errors.addAll(semiPrivateTurnedOff(detail, broker));
		}
		// another reset, just in case
		semiPrivateIndicatorChange = IndicatorChangeState.UNCHANGED;

		return errors;
	}

	/**
	 * Semi private turned on. Creates the StudentAlert record and removes the
	 * applicable address data
	 *
	 * @param detail   GenericDetail
	 * @param userData UserDataContainer
	 * @param broker   ModelBroker
	 */
	List<ValidationError> semiPrivateTurnedOn(GenericDetail detail, UserDataContainer userData, ModelBroker broker) {
		List<ValidationError> errors = new ArrayList<>();

		// create StudentAlert record
		StudentAlert studentAlert = X2BaseBean.newInstance(StudentAlert.class, broker.getPersistenceKey());
		studentAlert.setStudentOid(detail.getOid());
		studentAlert.setFieldValueByAlias(ALL_ALR_SHELTERED_IND_ALIAS, "true");
		studentAlert.setIconFilename(StudentAlert.ICON_FILENAME_BR_LAWS);
		studentAlert.setAlertType(StudentAlert.AlertType.OTHER.ordinal());

		// set description
		String alertDescription = createAlertDescription(userData, broker);
		studentAlert.setAlertDescription(alertDescription);

		Student student = broker.getBeanByOid(Student.class, detail.getOid());
		Person person = broker.getBeanByOid(Person.class, student.getPersonOid());
		Collection<PersonAddress> personAddresses = person.getAddresses();

		StringBuffer originalAddresses = new StringBuffer();

		if (!personAddresses.isEmpty()) {
			// All (if any) PAD records for this student are deleted
			for (PersonAddress personAddress : personAddresses) {
				// Grab the associated address data to add to the alert before deleting the PAD
				// record.
				if (personAddress.getActiveIndicator()) {
					// Copy addresses to DField
					// For each Active PAD:
					// PAD_ADDRESS_TYPE
					// ADR_ADDRESS_LINE_01, ADR_ADDRESS_LINE_02, ADR_ADDRESS_LINE_03
					Address address = broker.getBeanByOid(Address.class, personAddress.getAddressOid());
					if (address != null) {
						originalAddresses.append(personAddress.getAddressType());
						originalAddresses.append(NEW_LINE_DELIMITER);
						originalAddresses.append(address.getAddressLine01());
						if (!StringUtils.isBlank(address.getAddressLine02())) {
							originalAddresses.append(COMMA_DELIMITER);
							originalAddresses.append(address.getAddressLine02());
						}
						if (!StringUtils.isBlank(address.getAddressLine03())) {
							originalAddresses.append(COMMA_DELIMITER);
							originalAddresses.append(address.getAddressLine03());
						}
						originalAddresses.append(NEW_LINE_DELIMITER);
						originalAddresses.append(NEW_LINE_DELIMITER);
					}
				}

				errors.addAll(broker.deleteBeanByOid(PersonAddress.class, personAddress.getOid()));
			}
		} else {
			// Just use the physical and mailing address off the Person, since there are no
			// PAD records
			Address physicalAddress = person.getPhysicalAddress();
			originalAddresses.append(AddressTypeLocalCode.getPhysicalCodeForLocalCode(broker));
			originalAddresses.append(NEW_LINE_DELIMITER);
			originalAddresses.append(physicalAddress.getAddressLine01());
			if (!StringUtils.isBlank(physicalAddress.getAddressLine02())) {
				originalAddresses.append(COMMA_DELIMITER);
				originalAddresses.append(physicalAddress.getAddressLine02());
			}
			if (!StringUtils.isBlank(physicalAddress.getAddressLine03())) {
				originalAddresses.append(COMMA_DELIMITER);
				originalAddresses.append(physicalAddress.getAddressLine03());
			}
			originalAddresses.append(NEW_LINE_DELIMITER);
			originalAddresses.append(NEW_LINE_DELIMITER);

			Address mailingAddress = person.getMailingAddress();
			originalAddresses.append(AddressTypeLocalCode.getPhysicalCodeForLocalCode(broker));
			originalAddresses.append(NEW_LINE_DELIMITER);
			originalAddresses.append(mailingAddress.getAddressLine01());
			if (!StringUtils.isBlank(mailingAddress.getAddressLine02())) {
				originalAddresses.append(COMMA_DELIMITER);
				originalAddresses.append(mailingAddress.getAddressLine02());
			}
			if (!StringUtils.isBlank(mailingAddress.getAddressLine03())) {
				originalAddresses.append(COMMA_DELIMITER);
				originalAddresses.append(mailingAddress.getAddressLine03());
			}
			originalAddresses.append(NEW_LINE_DELIMITER);
			originalAddresses.append(NEW_LINE_DELIMITER);
		}

		// Copy PSN_PHONE_01, PSN_PHONE_02, PSN_PHONE_03 to DField
		originalAddresses.append(person.getPhone01());
		if (!StringUtils.isBlank(person.getPhone02())) {
			originalAddresses.append(COMMA_DELIMITER);
			originalAddresses.append(person.getPhone02());
		}
		if (!StringUtils.isBlank(person.getPhone03())) {
			originalAddresses.append(COMMA_DELIMITER);
			originalAddresses.append(person.getPhone03());
		}
		originalAddresses.append(NEW_LINE_DELIMITER);
		studentAlert.setFieldValueByAlias(ALL_ALR_SHELTERED_DETAIL_ALIAS, originalAddresses.toString());

		errors.addAll(broker.saveBean(studentAlert));

		// Remove all other addresses and phone numbers for the student
		// The following field values for the student are deleted:
		// PSN_PHONE_01
		// PSN_PHONE_02
		// PSN_PHONE_03
		// PSN_ADR_OID_MAIL
		// PSN_ADR_OID_PHYSICAL
		person.setPhone01(null);
		person.setPhone02(null);
		person.setPhone03(null);
		person.setMailingAddressOid(null);
		person.setPhysicalAddressOid(null);

		errors.addAll(broker.saveBean(person));

		return errors;
	}

	/**
	 * Create alert description string.
	 *
	 * @param userData UserDataContainer
	 * @return String
	 */
	private String createAlertDescription(UserDataContainer userData, ModelBroker broker) {
		Organization rootOrg = userData.getRootOrganization();
		String addr01 = (String) rootOrg.getFieldValueByAlias("all-org-sheltered-address01");
		String addr02 = (String) rootOrg.getFieldValueByAlias("all-org-sheltered-address02");
		String addr03 = (String) rootOrg.getFieldValueByAlias("all-org-sheltered-address03");
		String phone01 = (String) rootOrg.getFieldValueByAlias("all-org-sheltered-phone01");

		MessageResources resources = LocalizationCache.getMessages(broker.getPersistenceKey(), getLocale());

		return String.join(NEW_LINE_DELIMITER, resources.getMessage(getLocale(), "sheltered.student.alert.detail"),
				addr01 == null ? "" : addr01, addr02 == null ? "" : addr02, addr03 == null ? "" : addr03,
				phone01 == null ? "" : phone01);
	}

	/**
	 * Semi private turned off.
	 *
	 * @param detail GenericDetail
	 * @param broker ModelBroker
	 */
	private List<ValidationError> semiPrivateTurnedOff(GenericDetail detail, ModelBroker broker) {
		List<ValidationError> errors = new ArrayList<>();

		DataDictionary dictionary = DataDictionary.getDistrictDictionary(broker.getPersistenceKey());
		// field was checked earlier, so we know it exists here
		DataDictionaryField shelteredIndicatorField = dictionary
				.findDataDictionaryFieldByAlias(ALL_ALR_SHELTERED_IND_ALIAS);

		X2Criteria criteria = new X2Criteria();
		criteria.addEqualTo(StudentAlert.COL_ALERT_TYPE, StudentAlert.AlertType.OTHER.ordinal());
		criteria.addEqualTo(StudentAlert.COL_STUDENT_OID, detail.getOid());
		criteria.addEqualTo(shelteredIndicatorField.getJavaName(), "true");

		QueryByCriteria query = new QueryByCriteria(StudentAlert.class, criteria);
		query.addOrderByAscending(StudentAlert.COL_SEQUENCE_NUMBER);

		StudentAlert alert = broker.getBeanByQuery(query);
		if (alert != null) {
			errors.addAll(broker.deleteBean(alert));
		}
		return errors;
	}

	/**
	 * Initialize template.
	 *
	 * @param template           Template
	 * @param applicationContext ApplicationContext
	 * @param dictionary         DataDictionary
	 * @param privilegeSet       PrivilegeSet
	 * @param locale             Locale
	 */
	@Override
	public void initializeTemplate(Template template, ApplicationContext applicationContext, DataDictionary dictionary,
			PrivilegeSet privilegeSet, Locale locale) {
		// Do nothing
	}

	/**
	 * Modify form map.
	 *
	 * @param detail   GenericDetail
	 * @param key      String
	 * @param value    String
	 * @param userData UserDataContainer
	 * @param template Template
	 * @param errors   List
	 * @return Map<String, Object>
	 */
	@SuppressWarnings("rawtypes")
	@Override
	public Map<String, Object> modifyForm(GenericDetail detail, String key, String value, UserDataContainer userData,
			Template template, List errors) {
		// Do nothing
		return null;
	}

	/**
	 * Gets semi private ind from database.
	 *
	 * @param detail GenericDetail
	 * @param broker ModelBroker
	 * @return boolean
	 */
	private boolean getSemiPrivateIndFromDatabase(GenericDetail detail, ModelBroker broker) {
		Student student = broker.getBeanByOid(Student.class, detail.getId());
		Person person = student.getPerson();
		return person.getSemiprivateIndicator();
	}

	/**
	 * Validate udfs list.
	 *
	 * @param broker ModelBroker
	 * @return List<ValidationError>
	 */
	private List<ValidationError> validateUDFs(ModelBroker broker) {
		List<ValidationError> errors = new ArrayList<>();

		DataDictionary dictionary = DataDictionary.getDistrictDictionary(broker.getPersistenceKey());
		DataDictionaryField shelteredIndicatorField = dictionary
				.findDataDictionaryFieldByAlias(ALL_ALR_SHELTERED_IND_ALIAS);
		DataDictionaryField shelteredDetailField = dictionary
				.findDataDictionaryFieldByAlias(ALL_ALR_SHELTERED_DETAIL_ALIAS);

		// leave with error if UDFs do not exist
		if (shelteredIndicatorField == null) {
			errors.add(new ValidationError(BUSINESS_RULE_VIOLATION, SHELTERED_INDICATOR_MISSING));
		}
		if (shelteredDetailField == null) {
			errors.add(new ValidationError(BUSINESS_RULE_VIOLATION, SHELTERED_DETAIL_MISSING));
		}

		return errors;
	}

	@Override
	protected void execute() throws Exception {
		// Intentionally left blank
	}
}