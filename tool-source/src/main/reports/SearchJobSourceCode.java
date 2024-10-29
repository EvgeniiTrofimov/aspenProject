
/*
 *
 */
import org.apache.ojb.broker.query.QueryByCriteria;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ImportExportDefinition;
import com.follett.fsc.core.k12.beans.JobEntry;
import com.follett.fsc.core.k12.beans.JobEntry.ToolTypeCode;
import com.follett.fsc.core.k12.beans.Procedure;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.beans.ToolSourceCode;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;

public class SearchJobSourceCode extends ProcedureJavaSource {

	@Override
	protected void execute() throws Exception {
		String searchString = (String) getParameter("queryString");
		Boolean activeOnly = (Boolean) getParameter("activeOnly");
		Boolean includeProcedures = (Boolean) getParameter("includeProcedures");
		Boolean includeImportsExports = (Boolean) getParameter("includeImportsExports");
		Boolean includeReports = (Boolean) getParameter("includeReports");

		X2Criteria toolSourceCriteria = new X2Criteria();
		toolSourceCriteria.addContains(ToolSourceCode.COL_SOURCE_CODE, searchString);
		SubQuery toolSourceSubQuery = new SubQuery(ToolSourceCode.class, X2BaseBean.COL_OID, toolSourceCriteria);

		X2Criteria procedureCriteria = new X2Criteria();
		procedureCriteria.addIn(Procedure.COL_SOURCE_CODE_OID, toolSourceSubQuery);
		SubQuery procedureSubQuery = new SubQuery(Procedure.class, X2BaseBean.COL_OID, procedureCriteria);

		X2Criteria importExportCriteria = new X2Criteria();
		importExportCriteria.addIn(ImportExportDefinition.COL_SOURCE_CODE_OID, toolSourceSubQuery);
		SubQuery importExportSubQuery = new SubQuery(Procedure.class, X2BaseBean.COL_OID, importExportCriteria);

		X2Criteria reportCriteria = new X2Criteria();
		reportCriteria.addIn(Report.COL_SOURCE_CODE_OID, toolSourceSubQuery);
		SubQuery reportSubQuery = new SubQuery(Procedure.class, X2BaseBean.COL_OID, reportCriteria);

		X2Criteria jobCriteria = null;
		if (includeProcedures) {
			jobCriteria = new X2Criteria();
			jobCriteria.addIn(JobEntry.COL_TOOL_OID, procedureSubQuery);
		}

		if (includeImportsExports) {
			if (jobCriteria == null) {
				jobCriteria = new X2Criteria();
				jobCriteria.addIn(JobEntry.COL_TOOL_OID, importExportSubQuery);
			} else {
				X2Criteria orCriteria = new X2Criteria();
				orCriteria.addIn(JobEntry.COL_TOOL_OID, importExportSubQuery);
				jobCriteria.addOrCriteria(orCriteria);
			}
		}

		if (includeReports) {
			if (jobCriteria == null) {
				jobCriteria = new X2Criteria();
				jobCriteria.addIn(JobEntry.COL_TOOL_OID, reportSubQuery);
			} else {
				X2Criteria orCriteria = new X2Criteria();
				orCriteria.addIn(JobEntry.COL_TOOL_OID, reportSubQuery);
				jobCriteria.addOrCriteria(orCriteria);
			}
		}
		if (jobCriteria != null) {
			if (activeOnly) {
				jobCriteria.addEqualTo(JobEntry.COL_STATUS, Boolean.TRUE);
			}
			QueryByCriteria query = new QueryByCriteria(JobEntry.class, jobCriteria);
			try (QueryIterator<JobEntry> iterator = getBroker().getIteratorByQuery(query)) {
				logMessage("Job Name,Tool Name,Active,Custom");
				while (iterator.hasNext()) {
					JobEntry jobEntry = iterator.next();
					if (jobEntry.getToolType() == ToolTypeCode.PROCEDURE.ordinal()) {
						Procedure procedure = (Procedure) getBroker().getBeanByOid(Procedure.class,
								jobEntry.getToolOid());
						logMessage(jobEntry.getName() + "," + procedure.getName() + "," + jobEntry.getStatus() + ","
								+ ((procedure.getCustomInputIndicator() || procedure.getCustomJavaSourceIndicator())
										? "true"
										: "false"));
					} else if (jobEntry.getToolType() == ToolTypeCode.REPORT.ordinal()) {
						Report report = (Report) getBroker().getBeanByOid(Report.class, jobEntry.getToolOid());
						logMessage(jobEntry.getName() + "," + report.getName() + "," + jobEntry.getStatus() + ","
								+ ((report.getCustomInputIndicator() || report.getCustomJavaSourceIndicator()
										|| report.getCustomFormatIndicator()) ? "true" : "false"));
					} else if (jobEntry.getToolType() == ToolTypeCode.IMPORT_EXPORT.ordinal()) {
						ImportExportDefinition importExport = (ImportExportDefinition) getBroker()
								.getBeanByOid(ImportExportDefinition.class, jobEntry.getToolOid());
						logMessage(jobEntry.getName() + "," + importExport.getName() + "," + jobEntry.getStatus() + ","
								+ ((importExport.getCustomInputIndicator()
										|| importExport.getCustomJavaSourceIndicator()) ? "true" : "false"));
					}
				}
			}
		}
	}
}
