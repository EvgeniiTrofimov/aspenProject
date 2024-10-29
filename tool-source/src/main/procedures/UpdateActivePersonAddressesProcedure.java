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

import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.business.AddressManager;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.ValidationError;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.web.address.AddressTypeLocalCode;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * Procedure for processing the PERSON_TO_ADDRESS table and updating all addresses to be active/not
 * active based on start and end date.
 */
public class UpdateActivePersonAddressesProcedure extends ProcedureJavaSource {

    private final List<ValidationError> validationErrors = new ArrayList<ValidationError>();
    final PlainDate today = new PlainDate();
    final Set<String> peopleToSave = new HashSet<>();
    final Set<String> oidsActivated = new HashSet<>();
    final Set<String> oidsDeactivated = new HashSet<>();

    /**
     * Main execution for running update on PersonAddresses.
     *
     * @throws Exception exception
     */
    @Override
    protected void execute() throws Exception {
        ModelBroker modelBroker = null;
        if (getBroker() instanceof ModelBroker) {
            modelBroker = (ModelBroker) getBroker();
        } else {
            modelBroker = new ModelBroker(getPrivilegeSet());
        }

        peopleToSave.addAll(AddressManager.getPersonOidsToMarkActiveCriteria(modelBroker, today));
        peopleToSave.addAll(AddressManager.getPersonOidsToMarkInactiveCriteria(modelBroker, today));

        for (String oid : peopleToSave) {
            if (oid == null) {
                continue;
            }

            Person person = modelBroker.getBeanByOid(Person.class, oid);

            if (person == null) {
                continue;
            }

            // Activate the ones to be active by their date ranges
            oidsActivated.addAll(AddressManager.updatePersonAddressInactiveIndicators(modelBroker, oid, today));
            // Run the code to make sure only the right ones are active
            oidsDeactivated.addAll(AddressManager.updatePersonAddressActiveIndicators(modelBroker,
                    AddressTypeLocalCode.PHYSICAL, oid, today));
            oidsDeactivated.addAll(AddressManager.updatePersonAddressActiveIndicators(modelBroker,
                    AddressTypeLocalCode.MAILING, oid, today));
            oidsDeactivated.addAll(
                    AddressManager.updatePersonAddressActiveIndicators(modelBroker, AddressTypeLocalCode.OTHER, oid,
                            today));
            // properly set the right address records to be on the person bean
            AddressManager.updatePersonAddressRecord(modelBroker, person, true, true);

            // Save the dirty members because the super implementation will clear them.
            modelBroker.saveBeanForced(person, true, true);
        }

        if (!validationErrors.isEmpty()) {
            logMessage("Errors during UpdateActivePersonAddressesProcedure: " + validationErrors.toString());
        }

        Set<String> intersection = new HashSet<String>(oidsActivated);
        intersection.retainAll(oidsDeactivated);
        oidsActivated.removeAll(intersection);
        oidsDeactivated.removeAll(intersection);
        logMessage(String.format("%s address(es) were activated", oidsActivated.size()));
        logMessage(String.format("%s address(es) were deactivated", oidsDeactivated.size()));
    }

    /**
     * Adds to the list of validation errors.
     *
     * @param errors List<ValidationError>
     */
    private void addValidationErrors(List<ValidationError> errors) {
        validationErrors.addAll(errors);
    }
}
