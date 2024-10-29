/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2017 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.web.presentation.AddressParser;
import com.x2dev.sis.model.beans.SisAddress;
import org.apache.ojb.broker.query.QueryByCriteria;

/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2009 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

/**
 * This procedure iterates through every bean in the Address table and applies the AddressParser
 * field validation (if any) for the Addresses parent organization to each of the three address
 * lines. It also does some clean up, changing all street type abbreviations to the
 * non-abbreviated word.
 * <p>
 * If an address line is successfully parsed, then the UserValidationIndicator is set to false.
 * <p>
 * A side benefit to this procedure is that if grid codes are enabled, any modified beans will
 * have the Grid Code association automatically updated on save.
 *
 * @author X2 Development Corporation
 */
public class AddressValidationParser extends ProcedureJavaSource {

    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        logMessage("Address Validation Parser");

        X2Criteria criteria = new X2Criteria();
        QueryByCriteria query = new QueryByCriteria(SisAddress.class, criteria);
        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            /*
             * iterate through every bean in the Address table
             */
            while (iterator.hasNext()) {
                SisAddress address = (SisAddress) iterator.next();

                /*
                 * Get the addresses associated organization so that the proper expressions are
                 * used for parsing the data.
                 */
                Organization organization = OrganizationManager.getParentOrganization(address);

                /*
                 * Clean up the abbreviations
                 */
                cleanupAddressLine1(address);

                /*
                 * Parse each line, logging address lines that fail,
                 * remove user validation flag is successful
                 */
                if (AddressParser.NO_MATCH == AddressParser.parseLine01(organization, address.getAddressLine01(),
                        address, true, getBroker())) {
                    logMessage("Address line 1 not validated:  bean " + address.getOid() + " "
                            + address.getAddressLine01());
                } else {
                    address.setUserValidatedIndicator1(false);
                }
                if (AddressParser.NO_MATCH == AddressParser.parseLine02(organization, address.getAddressLine02(),
                        address, true, getBroker())) {
                    logMessage("Address line 2 not validated:  bean " + address.getOid() + " "
                            + address.getAddressLine02());
                } else {
                    address.setUserValidatedIndicator2(false);
                }
                if (AddressParser.NO_MATCH == AddressParser.parseLine03(organization, address.getAddressLine03(),
                        address, true, getBroker())) {
                    logMessage("Address line 3 not validated:  bean " + address.getOid() + " "
                            + address.getAddressLine03());
                } else {
                    address.setUserValidatedIndicator3(false);
                }

                /*
                 * Save the modified bean
                 */
                try {
                    getBroker().saveBeanForced(address);
                } catch (Exception e) {
                    logMessage("Exception: " + e.getCause() + " " + e.getMessage());
                    logMessage("Exception saving address line1:  bean " + address.getOid() + " "
                            + address.getAddressLine01());
                    logMessage("Exception saving address line2:  bean " + address.getOid() + " "
                            + address.getAddressLine02());
                    logMessage("Exception saving address line3:  bean " + address.getOid() + " "
                            + address.getAddressLine03());
                }
            }
        } finally {
            iterator.close();
        }
    }

    /**
     * Cleans up address line 1, Changing all street types to abbreviations sans .
     *
     * @param address SisAddress
     */
    private void cleanupAddressLine1(SisAddress address) {
        String addr = address.getAddressLine01();
        if (addr != null) {
            addr = addr.replaceAll("\\bStreet\\b", "St");
            addr = addr.replaceAll("\\bAvenue\\b", "Ave");
            addr = addr.replaceAll("\\bCourt\\b", "Ct");
            addr = addr.replaceAll("\\bBoulevard\\b", "Blvd");
            addr = addr.replaceAll("\\bTurnpike\\b", "Tpk");
            addr = addr.replaceAll("\\bLane\\b", "Ln");
            addr = addr.replaceAll("\\bTerrace\\b", "Terr");
            addr = addr.replaceAll("\\bDrive\\b", "Dr");
            addr = addr.replaceAll("\\bRoad\\b", "Rd");
            addr = addr.replaceAll("\\bCircle\\b", "Cir");

            addr = addr.replaceAll("St\\.", "St");
            addr = addr.replaceAll("Ave\\.", "Ave");
            addr = addr.replaceAll("Ct\\.", "Ct");
            addr = addr.replaceAll("Blvd\\.", "Blvd");
            addr = addr.replaceAll("Tpk\\.", "Tpk");
            addr = addr.replaceAll("Ln\\.", "Ln");
            addr = addr.replaceAll("Terr\\.", "Terr");
            addr = addr.replaceAll("Dr\\.", "Dr");
            addr = addr.replaceAll("Rd\\.", "Rd");
            addr = addr.replaceAll("Cir\\.", "Cir");


            address.setAddressLine01(addr);
        }
    }

}
