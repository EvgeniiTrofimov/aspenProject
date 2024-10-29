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
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Address;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.PersonAddress;
import com.follett.fsc.core.k12.business.AddressManager;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.test.BeanCreator;
import com.follett.fsc.core.k12.test.BeanStorageHelper;
import com.follett.fsc.core.k12.test.X2BaseTest;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.follett.fsc.core.k12.web.address.AddressTypeLocalCode;
import com.x2dev.utils.X2RuntimeException;
import com.x2dev.utils.types.PlainDate;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.logging.Level;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

public class UpdateActivePersonAddressesProcedureTest extends X2BaseTest {
    private static String OTHER_CODE;
    private static String PHYSICAL_CODE;
    private static String MAILING_CODE;
    X2Broker broker;
    private UpdateActivePersonAddressesProcedure updateActivePersonAddressesProcedure;

    private Person m_personPAD;
    private PersonAddress firstAddress, secondAddress, thirdAddress, fourthAddress, fifthAddress, sixthAddress,
            seventhAddress, eighthAddress, ninthAddress;

    @Override
    @Before
    public void setUp() throws Exception {
        super.setUp();
        broker = getBroker();

        updateActivePersonAddressesProcedure = new UpdateActivePersonAddressesProcedure() {
            @Override
            protected X2Broker getBroker() {
                return broker;
            }
        };

        PHYSICAL_CODE = AddressTypeLocalCode.getPhysicalCodeForLocalCode(broker);
        MAILING_CODE = AddressTypeLocalCode.getMailingCodeForLocalCode(broker);
        OTHER_CODE = AddressTypeLocalCode.getOtherCodeForLocalCode(broker);

        m_personPAD = BeanStorageHelper.createPerson(this, broker, "PersonAddressTests", false, true, false);
    }


    @Test
    public void execute_HasMailingOnlyPhysicalActive() throws Exception {
        firstAddress = createPersonAddress(m_personPAD, false, PHYSICAL_CODE, "one", BeanCreator.dateOffset(-30),
                BeanCreator.dateOffset(10));
        secondAddress = createPersonAddress(m_personPAD, false, MAILING_CODE, "two", BeanCreator.dateOffset(-30),
                BeanCreator.dateOffset(-10));

        QueryByCriteria query = new BeanQuery(PersonAddress.class, new X2Criteria());
        int aCount = broker.getCount(query);
        Assert.assertEquals(2, aCount);

        updateActivePersonAddressesProcedure.execute();
        assertActiveIndicator(firstAddress.getOid(), true, AddressTypeLocalCode.PHYSICAL);
        assertActiveIndicator(secondAddress.getOid(), false, AddressTypeLocalCode.MAILING);
    }

    @Test
    public void execute_HasNoneActive() throws Exception {
        firstAddress = createPersonAddress(m_personPAD, false, PHYSICAL_CODE, "one", BeanCreator.dateOffset(-30),
                BeanCreator.dateOffset(-5));
        secondAddress = createPersonAddress(m_personPAD, false, MAILING_CODE, "two", BeanCreator.dateOffset(-30),
                BeanCreator.dateOffset(-5));
        thirdAddress =
                createPersonAddress(m_personPAD, false, OTHER_CODE, "three", BeanCreator.dateOffset(-30),
                        BeanCreator.dateOffset(-5));

        QueryByCriteria query = new BeanQuery(PersonAddress.class, new X2Criteria());
        int aCount = broker.getCount(query);
        Assert.assertEquals(3, aCount);

        updateActivePersonAddressesProcedure.execute();
        assertActiveIndicator(firstAddress.getOid(), false, AddressTypeLocalCode.PHYSICAL);
        assertActiveIndicator(secondAddress.getOid(), false, AddressTypeLocalCode.MAILING);
        assertActiveIndicator(thirdAddress.getOid(), false, AddressTypeLocalCode.OTHER);
    }


    @Test
    public void execute_HasPhysicalAndMailingOnlyOtherActive() throws Exception {
        firstAddress = createPersonAddress(m_personPAD, false, PHYSICAL_CODE, "one", BeanCreator.dateOffset(-30),
                BeanCreator.dateOffset(-5));
        secondAddress = createPersonAddress(m_personPAD, false, MAILING_CODE, "two", BeanCreator.dateOffset(-30),
                BeanCreator.dateOffset(-5));
        thirdAddress =
                createPersonAddress(m_personPAD, false, OTHER_CODE, "three", BeanCreator.dateOffset(-30), null);

        QueryByCriteria query = new BeanQuery(PersonAddress.class, new X2Criteria());
        int aCount = broker.getCount(query);
        Assert.assertEquals(3, aCount);

        updateActivePersonAddressesProcedure.execute();
        assertActiveIndicator(firstAddress.getOid(), false, AddressTypeLocalCode.PHYSICAL);
        assertActiveIndicator(secondAddress.getOid(), false, AddressTypeLocalCode.MAILING);
        assertActiveIndicator(thirdAddress.getOid(), true, AddressTypeLocalCode.OTHER);
    }

    @Test
    public void execute_HasPhysicalOnlyMailingActive() throws Exception {
        firstAddress = createPersonAddress(m_personPAD, true, PHYSICAL_CODE, "one", BeanCreator.dateOffset(-30),
                BeanCreator.dateOffset(-10));
        secondAddress = createPersonAddress(m_personPAD, true, MAILING_CODE, "two", BeanCreator.dateOffset(-30),
                BeanCreator.dateOffset(0));

        QueryByCriteria query = new BeanQuery(PersonAddress.class, new X2Criteria());
        int aCount = broker.getCount(query);
        Assert.assertEquals(2, aCount);

        updateActivePersonAddressesProcedure.execute();
        assertActiveIndicator(firstAddress.getOid(), false, AddressTypeLocalCode.PHYSICAL);
        assertActiveIndicator(secondAddress.getOid(), true, AddressTypeLocalCode.MAILING);
    }

    @Test
    public void execute_Mailing() throws Exception {
        // Test cases:
        // Multiple active addresses => one active, the rest inactive
        // Active future date that should be inactive
        // Active one with past end date
        // Active one with null end date (should remain active)
        // Active one with valid dates (should remain active)
        // Active one that ends today (should remain active)
        // Active one that starts today (should remain active)

        createPersonAddressesAsBatch(m_personPAD, false, MAILING_CODE);

        QueryByCriteria query = new BeanQuery(PersonAddress.class, new X2Criteria());
        int aCount = broker.getCount(query);
        Assert.assertEquals(9, aCount);

        updateActivePersonAddressesProcedure.execute();
        assertActiveIndicator(firstAddress.getOid(), false, AddressTypeLocalCode.MAILING);
        assertActiveIndicator(secondAddress.getOid(), false, AddressTypeLocalCode.MAILING);
        assertActiveIndicator(thirdAddress.getOid(), false, AddressTypeLocalCode.MAILING);
        assertActiveIndicator(fourthAddress.getOid(), false, AddressTypeLocalCode.MAILING);
        assertActiveIndicator(fifthAddress.getOid(), false, AddressTypeLocalCode.MAILING);
        assertActiveIndicator(sixthAddress.getOid(), true, AddressTypeLocalCode.MAILING);
        assertActiveIndicator(seventhAddress.getOid(), false, AddressTypeLocalCode.MAILING);
        assertActiveIndicator(eighthAddress.getOid(), false, AddressTypeLocalCode.MAILING);
        assertActiveIndicator(ninthAddress.getOid(), false, AddressTypeLocalCode.MAILING);
    }

    @Test
    public void execute_MultiplePeopleAndTypes_initialInactive() throws Exception {

        Person person2 = BeanStorageHelper.createPerson(this, broker, "PersonTwo", false, true, false);
        Person person3 = BeanStorageHelper.createPerson(this, broker, "PersonThree", false, true, false);

        firstAddress = createPersonAddress(person2, false, PHYSICAL_CODE, "one",
                BeanCreator.dateOffset(-30),
                BeanCreator.dateOffset(30));

        secondAddress = createPersonAddress(person2, false, OTHER_CODE, "two",
                BeanCreator.dateOffset(-30),
                BeanCreator.dateOffset(0));

        thirdAddress = createPersonAddress(m_personPAD, false, PHYSICAL_CODE, "three",
                BeanCreator.dateOffset(-30),
                null);

        fourthAddress = createPersonAddress(person3, false, MAILING_CODE, "four",
                BeanCreator.dateOffset(0),
                BeanCreator.dateOffset(30));

        fifthAddress = createPersonAddress(m_personPAD, false, OTHER_CODE, "five",
                BeanCreator.dateOffset(0),
                BeanCreator.dateOffset(0));

        sixthAddress = createPersonAddress(person3, false, PHYSICAL_CODE, "six",
                BeanCreator.dateOffset(0),
                null);

        seventhAddress = createPersonAddress(m_personPAD, false, MAILING_CODE, "seven",
                BeanCreator.dateOffset(30),
                BeanCreator.dateOffset(60));

        eighthAddress = createPersonAddress(person2, false, OTHER_CODE, "eight",
                BeanCreator.dateOffset(30),
                null);

        ninthAddress = createPersonAddress(person3, false, PHYSICAL_CODE, "nine",
                BeanCreator.dateOffset(-30),
                BeanCreator.dateOffset(-15));

        QueryByCriteria query = new BeanQuery(PersonAddress.class, new X2Criteria());
        int aCount = broker.getCount(query);
        Assert.assertEquals(9, aCount);

        updateActivePersonAddressesProcedure.execute();

        assertActiveIndicator(firstAddress.getOid(), true, AddressTypeLocalCode.PHYSICAL);
        assertActiveIndicator(secondAddress.getOid(), true, AddressTypeLocalCode.OTHER);
        assertActiveIndicator(thirdAddress.getOid(), true, AddressTypeLocalCode.PHYSICAL);
        assertActiveIndicator(fourthAddress.getOid(), true, AddressTypeLocalCode.MAILING);
        assertActiveIndicator(fifthAddress.getOid(), true, AddressTypeLocalCode.OTHER);
        assertActiveIndicator(sixthAddress.getOid(), true, AddressTypeLocalCode.PHYSICAL);
        assertActiveIndicator(seventhAddress.getOid(), false, AddressTypeLocalCode.MAILING);
        assertActiveIndicator(eighthAddress.getOid(), false, AddressTypeLocalCode.OTHER);
        assertActiveIndicator(ninthAddress.getOid(), false, AddressTypeLocalCode.PHYSICAL);
    }

    @Test
    public void execute_MultiplePeopleAndTypes_initialActive() throws Exception {

        Person person2 = BeanStorageHelper.createPerson(this, broker, "PersonTwo", false, true, false);
        Person person3 = BeanStorageHelper.createPerson(this, broker, "PersonThree", false, true,
                false);

        firstAddress = createPersonAddress(person2, true, PHYSICAL_CODE, "one",
                BeanCreator.dateOffset(-30),
                BeanCreator.dateOffset(30));

        secondAddress = createPersonAddress(person2, true, OTHER_CODE, "two",
                BeanCreator.dateOffset(-30),
                BeanCreator.dateOffset(0));

        thirdAddress = createPersonAddress(m_personPAD, true, PHYSICAL_CODE, "three",
                BeanCreator.dateOffset(-30),
                null);

        fourthAddress = createPersonAddress(person3, true, MAILING_CODE, "four",
                BeanCreator.dateOffset(0),
                BeanCreator.dateOffset(30));

        fifthAddress = createPersonAddress(m_personPAD, true, OTHER_CODE, "five",
                BeanCreator.dateOffset(0),
                BeanCreator.dateOffset(0));

        sixthAddress = createPersonAddress(person3, true, PHYSICAL_CODE, "six",
                BeanCreator.dateOffset(0),
                null);

        seventhAddress = createPersonAddress(m_personPAD, true, MAILING_CODE, "seven",
                BeanCreator.dateOffset(30),
                BeanCreator.dateOffset(60));

        eighthAddress = createPersonAddress(person2, true, OTHER_CODE, "eight",
                BeanCreator.dateOffset(30),
                null);

        ninthAddress = createPersonAddress(person3, true, PHYSICAL_CODE, "nine",
                BeanCreator.dateOffset(-30),
                BeanCreator.dateOffset(-15));

        QueryByCriteria query = new BeanQuery(PersonAddress.class, new X2Criteria());
        int aCount = broker.getCount(query);
        Assert.assertEquals(9, aCount);

        updateActivePersonAddressesProcedure.execute();

        // person2
        assertActiveIndicator(firstAddress.getOid(), true, AddressTypeLocalCode.PHYSICAL);
        assertActiveIndicator(eighthAddress.getOid(), false, AddressTypeLocalCode.OTHER);
        assertActiveIndicator(secondAddress.getOid(), true, AddressTypeLocalCode.OTHER);

        // m_personPAD
        assertActiveIndicator(thirdAddress.getOid(), true, AddressTypeLocalCode.PHYSICAL);
        assertActiveIndicator(fifthAddress.getOid(), true, AddressTypeLocalCode.OTHER);
        assertActiveIndicator(seventhAddress.getOid(), false, AddressTypeLocalCode.MAILING);

        // person3
        assertActiveIndicator(sixthAddress.getOid(), true, AddressTypeLocalCode.PHYSICAL);
        assertActiveIndicator(ninthAddress.getOid(), false, AddressTypeLocalCode.PHYSICAL);
        assertActiveIndicator(fourthAddress.getOid(), true, AddressTypeLocalCode.MAILING);
    }

    @Test
    public void execute_Other() throws Exception {
        // Test cases:
        // Multiple active addresses => one active, the rest inactive
        // Active future date that should be inactive
        // Active one with past end date
        // Active one with null end date (should remain active)
        // Active one with valid dates (should remain active)
        // Active one that ends today (should remain active)
        // Active one that starts today (should remain active)

        createPersonAddressesAsBatch(m_personPAD, false, OTHER_CODE);

        QueryByCriteria query = new BeanQuery(PersonAddress.class, new X2Criteria());
        int aCount = broker.getCount(query);
        Assert.assertEquals(9, aCount);

        updateActivePersonAddressesProcedure.execute();
        assertActiveIndicator(firstAddress.getOid(), true, AddressTypeLocalCode.OTHER);
        assertActiveIndicator(secondAddress.getOid(), true, AddressTypeLocalCode.OTHER);
        assertActiveIndicator(thirdAddress.getOid(), true, AddressTypeLocalCode.OTHER);
        assertActiveIndicator(fourthAddress.getOid(), true, AddressTypeLocalCode.OTHER);
        assertActiveIndicator(fifthAddress.getOid(), true, AddressTypeLocalCode.OTHER);
        assertActiveIndicator(sixthAddress.getOid(), true, AddressTypeLocalCode.OTHER);
        assertActiveIndicator(seventhAddress.getOid(), false, AddressTypeLocalCode.OTHER);
        assertActiveIndicator(eighthAddress.getOid(), false, AddressTypeLocalCode.OTHER);
        assertActiveIndicator(ninthAddress.getOid(), false, AddressTypeLocalCode.OTHER);
    }

    @Test
    public void execute_Physical() throws Exception {
        // Test cases:
        // Multiple active addresses => one active, the rest inactive
        // Active future date that should be inactive
        // Active one with past end date
        // Active one with null end date (should remain active)
        // Active one with valid dates (should remain active)
        // Active one that ends today (should remain active)
        // Active one that starts today (should remain active)

        createPersonAddressesAsBatch(m_personPAD, false, PHYSICAL_CODE);

        QueryByCriteria query = new BeanQuery(PersonAddress.class, new X2Criteria());
        int aCount = broker.getCount(query);
        Assert.assertEquals(9, aCount);

        updateActivePersonAddressesProcedure.execute();
        assertActiveIndicator(firstAddress.getOid(), false, AddressTypeLocalCode.PHYSICAL);
        assertActiveIndicator(secondAddress.getOid(), false, AddressTypeLocalCode.PHYSICAL);
        assertActiveIndicator(thirdAddress.getOid(), false, AddressTypeLocalCode.PHYSICAL);
        assertActiveIndicator(fourthAddress.getOid(), false, AddressTypeLocalCode.PHYSICAL);
        assertActiveIndicator(fifthAddress.getOid(), false, AddressTypeLocalCode.PHYSICAL);
        assertActiveIndicator(sixthAddress.getOid(), true, AddressTypeLocalCode.PHYSICAL);
        assertActiveIndicator(seventhAddress.getOid(), false, AddressTypeLocalCode.PHYSICAL);
        assertActiveIndicator(eighthAddress.getOid(), false, AddressTypeLocalCode.PHYSICAL);
        assertActiveIndicator(ninthAddress.getOid(), false, AddressTypeLocalCode.PHYSICAL);
    }

    private void assertActiveIndicator(String personAddressOid, boolean isActive, AddressTypeLocalCode addressType) {

        PersonAddress personAddress = broker.getBeanByOid(PersonAddress.class, personAddressOid);
        Assert.assertNotNull(personAddress);

        Person person = broker.getBeanByOid(Person.class, personAddress.getPersonOid());
        Assert.assertNotNull(person);

        Address address = broker.getBeanByOid(Address.class, personAddress.getAddressOid());
        Assert.assertNotNull(address);


        if (isActive) {
            Assert.assertTrue(personAddress.getActiveIndicator());
            // make sure the physical/mailing also set
            switch (addressType) {

                case MAILING:
                    Assert.assertEquals(address.getOid(), person.getMailingAddressOid());
                    break;

                case PHYSICAL:
                    Assert.assertEquals(address.getOid(), person.getPhysicalAddressOid());
                    break;

                case OTHER:
                    break;
            }
        } else {
            // make sure the physical/mailing oid not set to this address's oid
            PersonAddress activeAddress = null;

            switch (addressType) {

                case MAILING:
                    activeAddress =
                            AddressManager.getActivePersonAddressForType(person.getOid(), MAILING_CODE, broker);
                    if (activeAddress == null) {
                        Assert.assertNull(person.getMailingAddressOid());
                    } else {
                        Assert.assertNotEquals(personAddress.getAddressOid(), person.getMailingAddressOid());
                    }
                    break;

                case PHYSICAL:
                    activeAddress =
                            AddressManager.getActivePersonAddressForType(person.getOid(), PHYSICAL_CODE, broker);
                    if (activeAddress == null) {
                        Assert.assertNull(person.getPhysicalAddressOid());
                    } else {
                        Assert.assertNotEquals(personAddress.getAddressOid(), person.getPhysicalAddressOid());
                    }
                    break;

                case OTHER:
                    break;
            }

            Assert.assertFalse(personAddress.getActiveIndicator());

            if (!personAddress.getStartDate().after(updateActivePersonAddressesProcedure.today)) {
                personAddress.setEndDate(updateActivePersonAddressesProcedure.today);
            }
        }
    }

    private PersonAddress createPersonAddress(Person person,
                                              boolean activeIndicator,
                                              String addressType,
                                              String comment,
                                              PlainDate startDate,
                                              PlainDate endDate) {

        Address createAddress = BeanStorageHelper.createAddress(this, broker, null);
        createAddress.setCity(comment);
        broker.saveBean(createAddress);
        PersonAddress pad = BeanStorageHelper.createPersonAddress(this, broker, person,
                createAddress, activeIndicator, addressType,
                startDate, endDate, comment);

        pad.setActiveIndicator(activeIndicator);
        broker.saveBeanForced(pad, true, false);

        return pad;
    }

    private void createPersonAddressesAsBatch(Person person, boolean activeIndicator, String addressType) {
        firstAddress = createPersonAddress(person, activeIndicator, addressType, "one", BeanCreator.dateOffset(-30),
                BeanCreator.dateOffset(30));
        secondAddress = createPersonAddress(person, activeIndicator, addressType, "two", BeanCreator.dateOffset(-30),
                BeanCreator.dateOffset(0));
        thirdAddress =
                createPersonAddress(person, activeIndicator, addressType, "three", BeanCreator.dateOffset(-30), null);
        fourthAddress = createPersonAddress(person, activeIndicator, addressType, "four", BeanCreator.dateOffset(0),
                BeanCreator.dateOffset(30));
        fifthAddress = createPersonAddress(person, activeIndicator, addressType, "five", BeanCreator.dateOffset(0),
                BeanCreator.dateOffset(0));
        sixthAddress =
                createPersonAddress(person, activeIndicator, addressType, "six", BeanCreator.dateOffset(0), null);
        seventhAddress = createPersonAddress(person, activeIndicator, addressType, "seven", BeanCreator.dateOffset(30),
                BeanCreator.dateOffset(60));
        eighthAddress =
                createPersonAddress(person, activeIndicator, addressType, "eight", BeanCreator.dateOffset(30), null);
        ninthAddress = createPersonAddress(person, activeIndicator, addressType, "nine", BeanCreator.dateOffset(-30),
                BeanCreator.dateOffset(-15));
    }

    private void forceActiveOrInactive(String queryString) {

        Connection connection = broker.borrowConnection();
        try {
            Statement statement = null;
            ResultSet resultSet = null;
            try {
                statement = connection.createStatement(ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY);
                statement.executeUpdate(queryString);
            } catch (Exception e) {
                AppGlobals.getLog().log(Level.WARNING, e.getLocalizedMessage(), e);
            } finally {
                if (resultSet != null) {
                    resultSet.close();
                }
                if (statement != null) {
                    statement.close();
                }
            }
        } catch (SQLException sqle) {
            AppGlobals.getLog().log(Level.WARNING, sqle.getLocalizedMessage(), sqle);
            throw new X2RuntimeException(sqle);
        } finally {
            broker.returnConnection();
        }
    }

    private void validateAddressCreation() {
        QueryByCriteria query = new BeanQuery(PersonAddress.class, new X2Criteria());
        int aCount = broker.getCount(query);
        Assert.assertEquals(9, aCount);
    }
}
