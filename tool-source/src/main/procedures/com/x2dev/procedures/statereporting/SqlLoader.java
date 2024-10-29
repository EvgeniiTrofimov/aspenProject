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

package com.x2dev.procedures.statereporting;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Procedure;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryCache;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.jdom.Document;
import org.jdom.Element;
import org.jdom.JDOMException;
import org.jdom.input.SAXBuilder;

/**
 *
 * This class is intended primarily as a mechanism to deploy SQL updates to a client using the
 * ImportAndRunProcedure. The class can perform any number of INSERT, UPDATE and DELETE statements
 * that are defined in the input definition.
 *
 * This class also requires the use of a test sql statement that is used to confirm that the
 * statements
 * are only executed for databases with particular characteristics.
 *
 * There are additional parameters that can be set to control cache reinitialization and dictionary
 * rebuild.
 *
 * The procedure identifier in the input definition is used to find the SQL statements in use.
 *
 * The sample input definition is SqlLoaderInput.xml
 */
public class SqlLoader extends ProcedureJavaSource {
    private static final String CDATA_END = "CDATA-END";
    private static final String PROCEDURE_ID = "procedureId";
    private static final String REFRESH_CACHE = "refreshCache";
    private static final String RELOAD_DICTIONARY = "reloadDictionary";
    private static final String SQL_DOCUMENT = "SQLDocument";
    private static final String SQL_STATEMENT = "SQLStatement";

    private static final String SQL_TEST = "SQLTest";
    private static final String SQL_TEST_STATEMENT = "TestStatement";
    private static final String SQL_TEST_VALUE = "TestValue";

    /**
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        Pattern deletePattern = Pattern.compile("(?s)(?i)^\\s*(DELETE .*);");
        Pattern insertPattern = Pattern.compile("(?s)(?i)^\\s*(INSERT .*);");
        Pattern updatePattern = Pattern.compile("(?s)(?i)^\\s*(UPDATE .*);");
        Procedure proc = getProcedure();
        logMessage("In SQL Loader:" + proc);
        if (proc != null) {
            Connection connection = null;
            Statement statement = null;
            boolean autoCommit = false;
            boolean setAutoCommit = false;
            try {
                connection = getBroker().borrowConnection();
                autoCommit = connection.getAutoCommit();
                setAutoCommit = true;
                connection.setAutoCommit(false);
                statement = connection.createStatement();

                if (sqlTestOK(statement, proc.getFullInputDefinition())) {
                    Collection<String> sqlStatements = getSqlStatements(proc.getFullInputDefinition());
                    for (String sqlStatement : sqlStatements) {
                        String sql = null;
                        Matcher matcher = deletePattern.matcher(sqlStatement);
                        if (matcher.find()) {
                            sql = matcher.group(1);
                            int result = statement.executeUpdate(sql);
                            logMessage("Delete result: " + result);
                        } else {
                            matcher = insertPattern.matcher(sqlStatement);
                            if (matcher.find()) {
                                sql = matcher.group(1);
                                int result = statement.executeUpdate(sql);
                                logMessage("Insert result: " + result);
                            } else {
                                matcher = updatePattern.matcher(sqlStatement);
                                if (matcher.find()) {
                                    sql = matcher.group(1);
                                    int result = statement.executeUpdate(sql);
                                    logMessage("Update result: " + result);
                                } else {
                                    logMessage("Unprocessed SQLStatement ");
                                }
                            }
                        }
                    }

                    connection.commit();
                    if (((Boolean) getParameter(RELOAD_DICTIONARY)).booleanValue()) {
                        DataDictionaryCache.clearDictionaries(getUser().getPersistenceKey(), true);
                    }
                    if (((Boolean) getParameter(REFRESH_CACHE)).booleanValue()) {
                        try {
                            (new ModelBroker(getPrivilegeSet())).clearCache();
                        } catch (Exception e) {
                            // ignore any clear cache exception
                        }
                    }
                } else {
                    logMessage("SQLTest failed");
                }
            } catch (SQLException sqle) {
                connection.rollback();
                logMessage(sqle.toString());
            } finally {
                if (statement != null) {
                    try {
                        statement.close();
                    } catch (SQLException e) {
                        // ignore
                    }
                }

                if (connection != null) {
                    if (setAutoCommit) {
                        try {
                            connection.setAutoCommit(autoCommit);
                        } catch (SQLException e) {
                            // ignore
                        }
                    }
                }

                getBroker().returnConnection();
            }
        }
    }

    /**
     * Some difficulties were experienced running this on the report server.
     * Normal results were observed using application server.
     *
     * @param userData UserDataContainer
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException {
        this.runOnApplicationServer();
        super.saveState(userData);
    }

    /**
     * Get the procedure bean based on the id from the input definition.
     * This procedure's input definition is the source of the SQLDocument.
     *
     * @return Procedure
     */
    private Procedure getProcedure() {
        Procedure bean = null;
        String procId = (String) getParameter(PROCEDURE_ID);
        if (!StringUtils.isEmpty(procId)) {
            X2Criteria crit = new X2Criteria();
            crit.addEqualTo(Procedure.COL_ID, procId);
            bean = (Procedure) getBroker().getBeanByQuery(new QueryByCriteria(Procedure.class, crit));
        }
        return bean;
    }

    /**
     * Get the SQLDocument element.
     *
     * @param parent Element
     * @return Element
     */
    private Element getSqlDocument(Element parent) {
        Element document = null;
        if (parent.getChildren() != null && parent.getChildren().size() > 0) {
            Iterator<Element> children = parent.getChildren().iterator();
            while (children.hasNext()) {
                Element element = children.next();
                String name = element.getName();
                if (SQL_DOCUMENT.equals(name)) {
                    document = element;
                    break;
                }
            }
        }
        return document;
    }

    /**
     * Get a list of the SQLStatements in the input definition.
     *
     * @param input String
     * @return Collection
     * @throws JDOMException exception
     * @throws IOException Signals that an I/O exception has occurred.
     */
    private Collection<String> getSqlStatements(String input) throws JDOMException, IOException {
        List<String> statements = new LinkedList();
        SAXBuilder builder = new SAXBuilder();
        Document document = builder.build(new ByteArrayInputStream(input.getBytes()));
        Element root = document.getRootElement();
        Element sqlDocument = getSqlDocument(root);
        if (sqlDocument != null && sqlDocument.getChildren(SQL_STATEMENT) != null
                && !sqlDocument.getChildren(SQL_STATEMENT).isEmpty()) {
            Iterator<Element> children = sqlDocument.getChildren(SQL_STATEMENT).iterator();
            while (children.hasNext()) {
                Element element = children.next();
                statements.add(element.getText().replaceAll(CDATA_END, "]]>"));
            }
        }
        return statements;
    }

    /**
     * Verify that at lease one SQLTest is provided and that all are valid.
     *
     * @param statement Statement
     * @param input String
     * @return true, if successful
     * @throws SQLException exception
     * @throws JDOMException exception
     * @throws IOException Signals that an I/O exception has occurred.
     */
    private boolean sqlTestOK(Statement statement, String input) throws SQLException, JDOMException, IOException {
        boolean retValue = false;
        SAXBuilder builder = new SAXBuilder();
        Document document = builder.build(new ByteArrayInputStream(input.getBytes()));
        Element root = document.getRootElement();
        Element sqlDocument = getSqlDocument(root);
        if (sqlDocument != null && sqlDocument.getChildren(SQL_TEST) != null
                && !sqlDocument.getChildren(SQL_TEST).isEmpty()) {
            Iterator<Element> children = sqlDocument.getChildren(SQL_TEST).iterator();
            while (children.hasNext()) {
                Element element = children.next();
                String testValue = element.getChildText(SQL_TEST_VALUE);
                String testStatement = element.getChildText(SQL_TEST_STATEMENT);
                ResultSet results = statement.executeQuery(testStatement);
                if (results.next() && results.getObject(1) != null
                        && testValue.equals(results.getObject(1).toString())) {
                    retValue = true;
                } else {
                    logMessage("Failing Test Statement: " + testStatement);
                    retValue = false;
                    break;
                }
            }
        }
        return retValue;
    }
}
