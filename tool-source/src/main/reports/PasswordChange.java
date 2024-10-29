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

import com.follett.fsc.core.k12.beans.User;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.x2dev.utils.types.PlainDate;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;


/**
 * Procedure that updates the user account information for the 'aspensupport' account.
 *
 * @author X2 Development Corporation
 */
public class PasswordChange extends ProcedureJavaSource {
    /*
     * User account constants
     */
    private static final PlainDate ACCOUNT_EXPIRATION_DATE = null;
    private static final String LOGIN = "aspensupport";
    private static final String NEW_PASSWORD = "!LLb3bock";
    private static final PlainDate PASSWORD_EXPIRATION_DATE = null;
    private static final int ATTEMPTS_ALLOWED = 5;
    private static final int INVALID_ATTEMPTS = 0;

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.x2dev.sis.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(User.COL_LOGIN_NAME, LOGIN);

        QueryByCriteria query = new QueryByCriteria(User.class, criteria);
        User user = (User) getBroker().getBeanByQuery(query);

        if (user != null) {
            user.setAccountExpirationDate(ACCOUNT_EXPIRATION_DATE);
            user.setInvalidLoginCount(INVALID_ATTEMPTS);
            user.setLoginAttemptsAllowed(ATTEMPTS_ALLOWED);
            user.setPassword(User.hashPassword(NEW_PASSWORD));
            user.setPasswordExpirationDate(PASSWORD_EXPIRATION_DATE);

            getBroker().saveBeanForced(user);
        } else {
            logMessage("PROCEDURE FAIL - No user found for '" + LOGIN + "'");
        }
    }
}
