/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2002-2016 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
import com.follett.fsc.core.k12.beans.Document;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.FileAttachmentManager;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.ValidationError;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.imports.ImportJavaSource;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.FolderUtils;
import java.io.File;
import java.util.Collection;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.struts.util.MessageResources;

/**
 * Import document to students document.
 *
 * @author Follett Software Company
 */
public class ImportStudentDocument extends ImportJavaSource {
    public static final String DOCUMENT_TYPE = "other";
    public static final String QUERY_BY_PARAM = "queryBy";
    public static final String QUERY_STRING_PARAM = "queryString";
    private Collection<ValidationError> m_errors;

    /**
     * Import data.
     *
     * @param sourceFile File
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.imports.ImportJavaSource#importData(java.io.File)
     */
    @Override
    protected void importData(File sourceFile) throws Exception {
        m_errors = FileAttachmentManager.checkFileSize(getOrganization(), sourceFile.length());

        if (m_errors.isEmpty()) {
            String fileName = sourceFile.getName();
            String extension = FolderUtils.getFileExtension(fileName);

            Criteria criteria = new Criteria();
            criteria.addEqualTo(SisStudent.COL_SCHOOL_OID, getSchool().getOid());
            addUserCriteria(criteria,
                    (String) getParameter(QUERY_BY_PARAM),
                    (String) getParameter(QUERY_STRING_PARAM),
                    null,
                    null);
            QueryByCriteria query = createQueryByCriteria(SisStudent.class, criteria);
            QueryIterator students = getBroker().getIteratorByQuery(query);
            try {
                while (students.hasNext()) {
                    SisStudent student = (SisStudent) students.next();

                    Document document = X2BaseBean.newInstance(Document.class, getBroker().getPersistenceKey());
                    document.setPersonOid(student.getPersonOid());
                    document.setBinaryFile(sourceFile);
                    document.setName(fileName);
                    document.setFormatCode(extension.toUpperCase());
                    document.setTypeCode(DOCUMENT_TYPE);
                    document.setFilename(fileName);
                    getBroker().saveBean(document);
                    incrementInsertCount();
                }
            } finally {
                students.close();
            }
        }
    }

    /**
     * Gets the import statistics.
     *
     * @return String builder
     * @see com.follett.fsc.core.k12.tools.imports.ImportJavaSource#getImportStatistics()
     */
    @Override
    protected StringBuilder getImportStatistics() {
        StringBuilder buffer = super.getImportStatistics();
        if (m_errors != null && !m_errors.isEmpty()) {
            MessageResources resources = LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale());
            String maxFileSize =
                    PreferenceManager.getPreferenceValue(getOrganization(), SystemPreferenceDefinition.FILE_UPLOAD_MAX);
            buffer.append("\n");
            buffer.append(resources.getMessage(getLocale(), "message.import.results.exceed.limit", maxFileSize));
        }
        return buffer;
    }
}
