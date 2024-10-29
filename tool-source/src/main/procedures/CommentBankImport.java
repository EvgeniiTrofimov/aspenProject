/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2002-2012 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
import com.follett.fsc.core.k12.beans.CommentBankCode;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryCache;
import com.follett.fsc.core.k12.tools.imports.XlsImportJavaSource;
import com.x2dev.sis.model.beans.SisCommentBankTable;
import com.x2dev.utils.StreamUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.util.LinkedList;
import java.util.List;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Imports comment bank codes from an Excel (.xls) file.
 * <p>
 * Codes in the spreadsheet must be in the columns: <br />
 * 
 * <pre>
 * A  | B       | C    | D          | E          | F
 * ID | PREVIEW | CODE | CATEGORY 1 | CATEGORY 2 | CATEGORY 3
 * </pre>
 * <dl>
 * <dt>ID</dt>
 * <dd>The identifier for the code, does not have to be unique.</dd>
 * <dt>PREVIEW</dt>
 * <dd>A sample preview of the code.<br />
 * Note: Preview exceeding 200 characters will be truncated to 197 characters + `...`</dd>
 * <dt>CODE</dt>
 * <dd>The code. Refer to the wiki page `Comment Banks` for more info.</dd>
 * <dt>CATEGORY 1</dt>
 * <dd>Category 1</dd>
 * <dt>CATEGORY 2</dt>
 * <dd>Category 2</dd>
 * <dt>CATEGORY 3</dt>
 * <dd>Category 3</dd>
 * </dl>
 *
 * @author Follett Software Company
 *
 */
public class CommentBankImport extends XlsImportJavaSource {

    private static final String PARAM_CBT_CATEGORY1 = "cbtCategory1";
    private static final String PARAM_CBT_CATEGORY2 = "cbtCategory2";
    private static final String PARAM_CBT_CATEGORY3 = "cbtCategory3";
    private static final String PARAM_CBT_NAME = "cbtName";

    /**
     * List of `codes to save`
     */
    private List<CommentBankCode> m_cbcsToSave;

    /**
     * The comment bank table where the codes will be saved to
     */
    private SisCommentBankTable m_commentBankTable;

    /**
     * Messages to be printed at the end of this script
     */
    private List<String> m_messages;

    /**
     * Does the comment bank already exists?
     */
    private boolean m_commentBankTableExists;

    /**
     * Print out what I've done.
     *
     * @throws X2BaseException exception
     */
    @Override
    protected void exportResults() throws X2BaseException {
        StringBuilder builder = new StringBuilder();

        builder.append("===============================================\n");
        if (!m_commentBankTableExists) {
            builder.append(" Created table: " + m_commentBankTable.getName() + "\n\n");
            builder.append(" Category 1 : " + m_commentBankTable.getCategory1() + "\n");
            builder.append(" Category 2 : " + m_commentBankTable.getCategory2() + "\n");
            builder.append(" Category 3 : " + m_commentBankTable.getCategory3() + "\n\n");
        } else {
            builder.append("Updated table: " + m_commentBankTable.getName() + "\n\n");
        }
        builder.append("Total codes added: " + m_cbcsToSave.size() + "\n");
        builder.append("===============================================\n\n");
        builder.append(StringUtils.convertCollectionToDelimitedString(m_messages, "\n"));

        ByteArrayInputStream inputStream = new ByteArrayInputStream(builder.toString().getBytes());
        try {
            StreamUtils.copyStream(inputStream, getResultHandler().getOutputStream());
        } catch (IOException e) {
            throw new X2BaseException();
        }
    }

    /**
     * I expect there to be 6 columns, in this order:
     * <p>
     * ID, PREVIEW, CODE, CATEGORY 1, CATEGORY 2, CATEGORY 3.
     *
     * @return int
     */
    @Override
    protected int getFieldCount() {
        return 6;
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        m_messages = new LinkedList<String>();
        m_cbcsToSave = new LinkedList<CommentBankCode>();

        /*
         * Check and see if the comment bank already exists with the name
         */
        String commentBankTableName = (String) getParameter(PARAM_CBT_NAME);
        Criteria cbtCriteria = new Criteria();
        cbtCriteria.addEqualTo(SisCommentBankTable.COL_NAME, commentBankTableName);
        QueryByCriteria cbtQuery = new QueryByCriteria(SisCommentBankTable.class, cbtCriteria);
        m_commentBankTable = (SisCommentBankTable) getBroker().getBeanByQuery(cbtQuery);

        /*
         * Otherwise, we'll create a new comment bank using that name and categories.
         */
        if (m_commentBankTable != null) {
            m_commentBankTableExists = true;
        } else {
            m_commentBankTableExists = false;

            String category1 = (String) getParameter(PARAM_CBT_CATEGORY1);
            String category2 = (String) getParameter(PARAM_CBT_CATEGORY2);
            String category3 = (String) getParameter(PARAM_CBT_CATEGORY3);

            m_commentBankTable = X2BaseBean.newInstance(SisCommentBankTable.class, getBroker().getPersistenceKey());
            m_commentBankTable.setName(commentBankTableName);
            m_commentBankTable.setCategory1(category1);
            m_commentBankTable.setCategory2(category2);
            m_commentBankTable.setCategory3(category3);

            getBroker().saveBeanForced(m_commentBankTable); // have to save now because to generate
                                                            // OID
        }
    }

    /**
     * @see com.follett.fsc.core.k12.tools.imports.XlsImportJavaSource#importData(java.io.File)
     */
    @Override
    protected void importData(File sourceFile) throws Exception {
        /*
         * The parent's `importData` method imports all the codes into `m_cbcsToSave`
         */
        super.importData(sourceFile);

        saveCbcs();
    }

    /**
     * Each record will create a new comment bank code for the table
     * <p>
     * Add it to a list of `codes to save`.
     *
     * @param record List<String>
     * @param lineNumber int
     * @throws Exception exception
     */
    @Override
    protected void importRecord(List<String> record, int lineNumber)
            throws Exception {
        String id = record.get(0);
        String preview = record.get(1);
        String code = record.get(2);
        String category1 = record.get(3);
        String category2 = record.get(4);
        String category3 = record.get(5);

        CommentBankCode cbc = X2BaseBean.newInstance(CommentBankCode.class, getBroker().getPersistenceKey());
        cbc.setId(id);
        cbc.setPreview(StringUtils.truncate(preview, 200));
        cbc.setCode(code);
        cbc.setCategory1(category1);
        cbc.setCategory2(category2);
        cbc.setCategory3(category3);
        cbc.setCommentBankTableOid(m_commentBankTable.getOid());

        m_cbcsToSave.add(cbc);
    }

    /**
     * Clear dictionaries.
     */
    @Override
    protected void releaseResources() {
        DataDictionaryCache.clearDictionaries(getBroker().getPersistenceKey(), true);
    }

    /**
     * Save the codes from `m_cbcsToSave`.
     */
    private void saveCbcs() {
        boolean exception = false;
        getBroker().beginTransaction();
        try {
            for (CommentBankCode cbc : m_cbcsToSave) {
                m_messages.add(String.format("Added code (ID: %s) to %s", cbc.getId(), m_commentBankTable.getName()));
                getBroker().saveBeanForced(cbc);
            }
        } catch (RuntimeException re) {
            exception = true;
        } finally {
            if (exception) {
                getBroker().rollbackTransaction();
            } else {
                getBroker().commitTransaction();
            }
        }
    }

}
