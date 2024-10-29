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
import com.follett.fsc.core.k12.beans.DataRestore;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryTable;
import com.follett.fsc.core.k12.business.restore.DataRestoreManager;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.x2dev.utils.types.PlainDate;
import java.util.Calendar;

/**
 * Procedure to remove <code>DATA_RESTORE</code> records after a certain number of days.
 *
 * @author Follett Software Company
 */
public class DeleteDataRestoreRecords extends ProcedureJavaSource {
    /**
     * Name for the "days to keep" report input parameter. The value is an Integer.
     */
    public static final String DAYS_TO_KEEP_PARAM = "daysToKeep";

    private static final int MAX_DAYS = 180;

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        Calendar calendar = Calendar.getInstance(getTimeZone(), getLocale());
        calendar.add(Calendar.DATE, -getDayCount());

        PlainDate date = new PlainDate(calendar.getTimeInMillis());
        int count = DataRestoreManager.delete(date, getBroker());

        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryTable table = dictionary.findDataDictionaryTableById(DataRestore.DICTIONARY_ID);
        String tableName = table.getDisplayString(getBroker().getPersistenceKey(), getLocale());

        logMessage("Removed " + count + " " + tableName + " record(s).");
    }

    /**
     * Returns the number of days to keep the data restore records.
     *
     * @return int
     */
    private int getDayCount() {
        int dayCount = ((Integer) getParameter(DAYS_TO_KEEP_PARAM)).intValue();

        if (dayCount > MAX_DAYS) {
            dayCount = MAX_DAYS;
        }

        return dayCount;
    }
}
