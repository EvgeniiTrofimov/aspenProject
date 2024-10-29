/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2006 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Role;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.beans.User;
import com.follett.fsc.core.k12.beans.User.AuthenticationType;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.LdapUtils;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisUser;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.types.PlainDate;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Hashtable;
import javax.naming.Context;
import javax.naming.NamingEnumeration;
import javax.naming.directory.Attribute;
import javax.naming.directory.Attributes;
import javax.naming.directory.DirContext;
import javax.naming.directory.SearchControls;
import javax.naming.directory.SearchResult;
import javax.naming.ldap.InitialLdapContext;
import javax.naming.ldap.LdapContext;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Java source for creating user accounts by querying from the Active Directory
 * <ol>
 * The following will be done.
 * <li>Create missing user account
 * <li>Disable invalid user account
 *
 * @author X2 Development Corporation
 */
public class UserAccountCreationThroughActiveDirectory extends ProcedureJavaSource {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    private static String[] LDAP_SEARCH_ATTRIBUTES = {"sAMAccountName", "sn", "givenName"};

    private Collection<String> m_roleGroups;
    private HashMap<String, SisUser> m_userByLogInMap;

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        /*
         * Prepare work
         */
        prepare();

        if (!m_roleGroups.isEmpty()) {
            String serverName = PreferenceManager.getPreferenceValue(getOrganization(),
                    SystemPreferenceDefinition.ACTIVE_DIRECTORY_SERVER);
            String domainName = PreferenceManager.getPreferenceValue(getOrganization(),
                    SystemPreferenceDefinition.ACTIVE_DIRECTORY_DOMAIN);
            String port = PreferenceManager.getPreferenceValue(getOrganization(),
                    SystemPreferenceDefinition.ACTIVE_DIRECTORY_PORT);
            String adminLogin = PreferenceManager.getPreferenceValue(getOrganization(),
                    SystemPreferenceDefinition.ACTIVE_DIRECTORY_USER_NAME);
            String adminPassword = PreferenceManager.getPreferenceValue(getOrganization(),
                    SystemPreferenceDefinition.ACTIVE_DIRECTORY_USER_PASSWORD);
            boolean isLdaps = "true".equals(PreferenceManager.getPreferenceValue(getOrganization(),
                    SystemPreferenceDefinition.ACTIVE_DIRECTORY_LDAPS_INDICATOR));

            Hashtable<String, String> ladpEnviorment = new Hashtable<String, String>();

            String principalName = adminLogin + "@" + domainName;
            String ldapURL = LdapUtils.getServerURL(serverName, domainName, port, isLdaps);
            String searchBase = LdapUtils.getSearchBase(domainName);

            ladpEnviorment.put(Context.INITIAL_CONTEXT_FACTORY, "com.sun.jndi.ldap.LdapCtxFactory");
            ladpEnviorment.put(Context.PROVIDER_URL, ldapURL);
            ladpEnviorment.put(Context.SECURITY_AUTHENTICATION, "simple");
            ladpEnviorment.put(Context.SECURITY_PRINCIPAL, principalName);
            ladpEnviorment.put(Context.SECURITY_CREDENTIALS, adminPassword);

            try {
                /*
                 * Bind to the LDAP server with the current user credential
                 */
                LdapContext context = new InitialLdapContext(ladpEnviorment, null);

                HashMap<String, String[]> userInfo = new HashMap<String, String[]>();
                /*
                 * Perform the search
                 */
                for (String roleGroup : m_roleGroups) {
                    String searchFilter = "(&(objectClass=group)(CN=" + roleGroup + "))";
                    performSearch(context, searchBase, searchFilter, userInfo);
                }

                logMessage("Total Users :" + userInfo.size());
                /*
                 * Create missing user accounts
                 */
                logMessage("The following user accounts are created");
                createUserAccounts(userInfo);

                /*
                 * Disable accounts that no longer in the active directory.
                 */
                logMessage("The following user accounts are disabled");
                disableUserAccounts(userInfo.keySet());

                context.close();
            } catch (Exception e) {
                e.printStackTrace();
                logMessage("Error..." + e.toString());
            }
        }
    }

    /**
     * Creates and updates user accounts.
     *
     * @param userInfo HashMap<String,String[]>
     */
    private void createUserAccounts(HashMap<String, String[]> userInfo) {
        for (String userId : userInfo.keySet()) {
            /*
             * If the user account does not exist, create new user account
             */
            if (!m_userByLogInMap.containsKey(userId)) {
                String[] personInfo = userInfo.get(userId);

                SisUser user = X2BaseBean.newInstance(SisUser.class, getBroker().getPersistenceKey());
                user.setOrganization1Oid(getOrganization().getOid());
                user.setLoginName(userId);

                SisPerson newPerson = X2BaseBean.newInstance(SisPerson.class, getBroker().getPersistenceKey());
                newPerson.setOrganization1Oid(getOrganization().getOid());

                newPerson.setLastName(personInfo[0]);
                newPerson.setFirstName(personInfo[1]);

                getBroker().saveBeanForced(newPerson);

                user.setPersonOid(newPerson.getOid());
                user.setNameView(newPerson.getNameView());
                user.setCreationDate(new PlainDate());

                getBroker().saveBeanForced(user);
                logMessage("User name " + newPerson.getNameView() + " with login ID " + userId);
            } else {
                SisUser user = m_userByLogInMap.get(userId);
                if (user != null && user.getLoginStatus() != User.LoginStatus.ENABLED.ordinal()) {
                    user.setLoginStatus(User.LoginStatus.ENABLED.ordinal());
                    getBroker().saveBeanForced(user);
                }
            }
        }
    }

    /**
     * Disable use accounts.
     *
     * @param existingUserIds Collection<String>
     */
    private void disableUserAccounts(Collection<String> existingUserIds) {
        for (String userId : m_userByLogInMap.keySet()) {
            /*
             * If the user account is not part of active directory, disable the use account.
             */
            if (!existingUserIds.contains(userId)) {
                SisUser user = m_userByLogInMap.get(userId);
                user.setLoginStatus(User.LoginStatus.DISABLED_AND_LOCKED.ordinal());

                getBroker().saveBeanForced(user);
                logMessage("User name " + user.getNameView() + " with login ID " + userId);
            }
        }
    }

    /**
     * Returns the object class for the passed DN name.
     *
     * @param context DirContext
     * @param searchBase String
     * @param dnName String
     * @return String
     */
    private String getObjectClass(DirContext context,
                                  String searchBase,
                                  String dnName) {
        String objectClass = "";

        try {
            String newSearchBase = "distinguishedName=" + dnName;

            SearchControls searchControl = new SearchControls();
            searchControl.setSearchScope(SearchControls.SUBTREE_SCOPE);

            String returnedAttributes[] = {"objectClass"};
            searchControl.setReturningAttributes(returnedAttributes);

            NamingEnumeration results = context.search(searchBase, newSearchBase, searchControl);

            while (results != null && results.hasMoreElements()) {
                SearchResult searchResult = (SearchResult) results.next();

                Attributes attributes = searchResult.getAttributes();

                NamingEnumeration attributeElements = attributes.getAll();

                while (attributeElements != null && attributeElements.hasMoreElements()) {
                    Attribute attribute = (Attribute) attributeElements.next();
                    objectClass = attribute.toString();
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
            logMessage("Error..." + e.toString());
        }

        return objectClass;
    }

    /**
     * Performs the search to the active directory with the passed search base and filter and
     * returns
     * the list of user included in the group.
     *
     * @param context LdapContext
     * @param searchBase String
     * @param searchFilter String
     * @param userInfo Key on user Id, value is the user's first and last name
     * @return userIds The list of users for the given search base.
     */
    private void performSearch(LdapContext context,
                               String searchBase,
                               String searchFilter,
                               HashMap<String, String[]> userInfo) {
        try {
            SearchControls constraints = new SearchControls();
            constraints.setSearchScope(SearchControls.SUBTREE_SCOPE);
            String returnedAttributes[] = {"member"};
            constraints.setReturningAttributes(returnedAttributes);

            NamingEnumeration results = context.search(searchBase, searchFilter, constraints);

            while (results != null && results.hasMoreElements()) {
                SearchResult result = (SearchResult) results.next();
                Attributes attrs = result.getAttributes();

                if (attrs != null) {
                    NamingEnumeration attributeElements = attrs.getAll();

                    while (attributeElements != null && attributeElements.hasMoreElements()) {
                        Attribute attribute = (Attribute) attributeElements.next();

                        NamingEnumeration elements = attribute.getAll();
                        while (elements != null && elements.hasMoreElements()) {
                            String dnName = (String) elements.next();
                            dnName = StringUtils.replaceAll(dnName, "\\", "\\\\\\\\");

                            String objectClass = getObjectClass(context, searchBase, dnName);
                            /*
                             * Determine if the object is a user or another group
                             */
                            if (objectClass.indexOf("user") != -1) {
                                String newSearchBase = "distinguishedName=" + dnName;
                                searchMemberAttributes(context, searchBase, newSearchBase, userInfo);
                            } else if (objectClass.indexOf("group") != -1) {
                                /*
                                 * Search the members of the group
                                 */
                                String newSearchBase = "memberOf=" + dnName;
                                searchMemberAttributes(context, searchBase, newSearchBase, userInfo);

                                /*
                                 * Recursively search the other group inside the group.
                                 */
                                String newSearchFilter = "(&(objectClass=group)(" + dnName + "))";
                                performSearch(context, searchBase, newSearchFilter, userInfo);

                            }
                        }
                    }
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
            logMessage("Error..." + e.toString());
        }
    }

    /**
     * Prepares for creating user accounts.
     */
    private void prepare() {
        m_userByLogInMap = new HashMap<String, SisUser>();
        m_roleGroups = new HashSet<String>();

        X2Criteria userCriteria = new X2Criteria();
        userCriteria.addEqualTo(SisUser.COL_AUTHENTICATION_TYPE, Integer.valueOf(AuthenticationType.AD.ordinal()));

        QueryByCriteria userQuery = new QueryByCriteria(SisUser.class, userCriteria);
        m_userByLogInMap =
                (HashMap<String, SisUser>) getBroker().getMapByQuery(userQuery, SisUser.COL_LOGIN_NAME, 1000);

        X2Criteria roleCriteria = new X2Criteria();
        roleCriteria.addNotEmpty(Role.COL_ACTIVE_DIRECTORY_GROUP_NAME, getBroker().getPersistenceKey());

        SubQuery roleSubQuery = new SubQuery(Role.class, Role.COL_ACTIVE_DIRECTORY_GROUP_NAME, roleCriteria);
        m_roleGroups = getBroker().getSubQueryCollectionByQuery(roleSubQuery);
    }

    /**
     * Performs the group members attributes.
     *
     * @param context LdapContext
     * @param searchBase String
     * @param searchFilter String
     * @param userInfo HashMap<String,String[]>
     */
    private void searchMemberAttributes(LdapContext context,
                                        String searchBase,
                                        String searchFilter,
                                        HashMap<String, String[]> userInfo) {
        try {
            // Perform the search
            SearchControls constraints = new SearchControls();
            constraints.setSearchScope(SearchControls.SUBTREE_SCOPE);

            HashMap<String, SearchResult> results = LdapUtils.performSearch(context, searchBase, searchFilter);

            for (String resultName : results.keySet()) {
                String dn = resultName + "," + searchBase;

                Attributes attrs = context.getAttributes(dn, LDAP_SEARCH_ATTRIBUTES);

                if (attrs != null) {
                    String[] user = new String[LDAP_SEARCH_ATTRIBUTES.length];
                    String attributeString = "";
                    for (int i = 0; i < LDAP_SEARCH_ATTRIBUTES.length; ++i) {
                        Attribute attr = attrs.get(LDAP_SEARCH_ATTRIBUTES[i]);
                        if (attr != null) {
                            user[i] = (String) attr.get(0);
                            attributeString += attr.toString() + " ";
                        }
                    }
                    logMessage(dn + "; " + attributeString);
                    userInfo.put(user[0], new String[] {user[1], user[2]});
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
            logMessage("Error..." + e.toString());
        }
    }
}
