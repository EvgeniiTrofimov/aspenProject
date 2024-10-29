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
package com.x2dev.procedures.statereporting.on;

import static com.follett.fsc.core.k12.business.AccountCreationManager.PLACEHOLDER_CONFIRM;
import static com.follett.fsc.core.k12.business.AccountCreationManager.PLACEHOLDER_UNREGISTER;
import static com.follett.fsc.core.k12.business.AccountCreationManager.PLACEHOLDER_USER;
import com.follett.fsc.core.k12.beans.BeanManager.PersistenceKey;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.User;
import com.follett.fsc.core.k12.business.AccountCreationManager;
import com.follett.fsc.core.k12.business.EmailManager;
import com.follett.fsc.core.k12.business.MessageProperties;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.WriteEmailManager;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.procedures.AccountCreationProcedure;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.utils.KeyValuePair;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.types.PlainDate;
import java.util.logging.Level;

/**
 * @author Follett Software Company
 * @copyright 2019
 */
public class RenfrewAccountCreationProcedure extends AccountCreationProcedure {
    @Override
    public String emailResponse(String userOid, boolean confirm, Organization org, X2Broker broker) {
        String message = "";

        User user = (User) broker.getBeanByOid(User.class, userOid);
        if (user != null) {
            if (user.getSelfCreatedIndicator()) {
                if (user.getEmailVerifiedDate() == null) {
                    if (confirm) {
                        user.setEmailVerifiedDate(new PlainDate(OrganizationManager.getTimeZone(org)));
                        user.setLoginStatus(User.LoginStatus.ENABLED.ordinal());
                        broker.saveBeanForced(user);

                        message =
                                "<img src=\"images/check.png\" style=\"padding-right: 3px;\"><font size=\"4\">Verification completed successfully!</font><p style=\"padding: 5px 25px 5px 25px\"><font size=\"2\">You can now log into Aspen using the email address and password provided during the request process.</font></p>";
                    } else {
                        if (deleteUser(user, broker)) {
                            message = getMessageAccountRemoved();
                        } else {
                            message = getErrorDeletingUser();

                            AppGlobals.getLog().log(Level.SEVERE, "error.accountCreation.deleteUser");
                        }
                    }
                } else {
                    message = getErrorAlreadyVerified();
                }
            } else {
                message = getErrorAccountNotSelfCreated();
            }
        } else {
            message = getErrorAccountNotFound();
        }

        return message;
    }

    @Override
    public KeyValuePair<Boolean, String> sendValidationEmail(Person person,
                                                             String confirmationLink,
                                                             String unregsiterLink,
                                                             Organization org,
                                                             PersistenceKey persistenceKey) {
        KeyValuePair<Boolean, String> result = new KeyValuePair<Boolean, String>(Boolean.FALSE, getErrorSendingEmail());
        String m_emailValidationBody =
                LocalizationCache.getCurrentMessages().getMessage("label.accountCreation.emailValidationBody",
                        new Object[] {PLACEHOLDER_USER, PLACEHOLDER_CONFIRM, PLACEHOLDER_UNREGISTER});
        String m_emailValidationSubject =
                LocalizationCache.getCurrentMessages().getMessage("label.accountCreation.emailValidationSubject");

        String email = person.getEmail01();

        confirmationLink = addDeploymentId(confirmationLink, persistenceKey.getDeploymentId(), "confirmation=");

        if (!StringUtils.isEmpty(email) && EmailManager.validateEmailAddress(email)) {
            WriteEmailManager emailManager = new WriteEmailManager(org);
            if (emailManager.connect()) {
                try {
                    String body = m_emailValidationBody;
                    body = body.replace(AccountCreationManager.PLACEHOLDER_USER,
                            person.getFirstName() + " " + person.getLastName())
                            .replace(AccountCreationManager.PLACEHOLDER_CONFIRM,
                                    addDeploymentId(confirmationLink, persistenceKey.getDeploymentId(),
                                            "confirmation="))
                            .replace(AccountCreationManager.PLACEHOLDER_UNREGISTER, addDeploymentId(unregsiterLink,
                                    persistenceKey.getDeploymentId(), "confirmation="));

                    MessageProperties message = new MessageProperties(email,
                            null,
                            emailManager.getUsername(),
                            m_emailValidationSubject,
                            body,
                            "text/html");

                    if (emailManager.sendMail(message)) {
                        result = new KeyValuePair<Boolean, String>(Boolean.TRUE, "true");
                    }
                } finally {
                    emailManager.disconnect();
                }
            }
        } else {
            result = new KeyValuePair<Boolean, String>(Boolean.FALSE, getErrorInvalidEmail());
        }

        return result;
    }

    private String addDeploymentId(String url, String deploymentId, String matchString) {
        return url.replace(matchString, "deploymentId=" + deploymentId + "&" + matchString);
    }

}
