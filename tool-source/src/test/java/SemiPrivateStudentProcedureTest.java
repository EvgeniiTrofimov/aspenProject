/*
 *
 */
package com.follett.fsc.core.k12.tools.procedures;

import com.follett.fsc.core.k12.beans.Address;
import com.follett.fsc.core.k12.beans.BeanManager.PersistenceKey;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.PersonAddress;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.StudentAlert;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.ValidationError;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.web.GenericDetail;
import com.follett.fsc.core.k12.web.GenericDetailForm;
import com.follett.fsc.core.k12.web.UserDataContainer;

import java.util.List;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;

@PrepareForTest({DataDictionary.class, ModelBroker.class, X2BaseBean.class})
@RunWith(PowerMockRunner.class)
public class SemiPrivateStudentProcedureTest {
	@Mock
	private Address address;
	@Mock
	private DataDictionary dataDictionary;
	@Mock
	private DataDictionaryField dataDictionaryField;
	@Mock
	private GenericDetail detail;
	@Mock
	private GenericDetailForm form;
	@Mock
    private ModelBroker broker;
	@Mock
	private Organization organization;
	@Mock
	private PersistenceKey persistenceKey;
	@Mock
	private Person person;
	@Mock
	private PersonAddress personAddress;
	@Mock
	private Student student;
	@Mock
	private StudentAlert alert;
    @Mock
    private UserDataContainer userData;
    
    private SemiPrivateStudentProcedure procedure;
    
    @Before
    public void setUp() throws NoSuchMethodException, SecurityException {
        MockitoAnnotations.initMocks(this);
        PowerMockito.mockStatic(DataDictionary.class);
        PowerMockito.mockStatic(X2BaseBean.class);
        
        procedure = new SemiPrivateStudentProcedure();
    }

    @Test
    public void testCreateSpecialCodes() throws Exception {
    	person.setPhone01("555-123-4567");
    	person.addToAddresses(personAddress);
    	personAddress.setAddressType("ADDRESS_TYPE");
        address.setAddressLine01("Address line 1");
        address.setAddressLine03("Address line 3");
    	
    	Mockito.when(broker.getPersistenceKey()).thenReturn(persistenceKey);
    	Mockito.when(X2BaseBean.newInstance(StudentAlert.class, persistenceKey)).thenReturn(alert);
    	Mockito.when(userData.getRootOrganization()).thenReturn(organization);
        Mockito.when(broker.getBeanByOid(Mockito.eq(Student.class), Mockito.any())).thenReturn(student);
        Mockito.when(broker.getBeanByOid(Mockito.eq(Person.class), Mockito.any())).thenReturn(person);
        Mockito.when(broker.getBeanByOid(Mockito.eq(Address.class), Mockito.any())).thenReturn(address);
        
        List<ValidationError> errors = procedure.semiPrivateTurnedOn(detail, userData, broker);
        Assert.assertTrue(errors.isEmpty());
        Assert.assertNull(person.getPhone01());
        Assert.assertNull(person.getPhysicalAddressOid());
        Assert.assertEquals(0, person.getAddresses().size());
    }

    @Test
    public void testValidateTemplate_Errors() throws Exception {
    	Mockito.when(broker.getPersistenceKey()).thenReturn(persistenceKey);
        Mockito.when(DataDictionary.getDistrictDictionary(persistenceKey)).thenReturn(dataDictionary);
        Mockito.when(dataDictionary.findDataDictionaryFieldByAlias(SemiPrivateStudentProcedure.ALL_ALR_SHELTERED_IND_ALIAS)).thenReturn(null);
        Mockito.when(dataDictionary.findDataDictionaryFieldByAlias(SemiPrivateStudentProcedure.ALL_ALR_SHELTERED_DETAIL_ALIAS)).thenReturn(null);
        
        List<ValidationError> errors = procedure.validateTemplate(form, detail, userData, broker);
        Assert.assertEquals(2, errors.size());
    }
}
