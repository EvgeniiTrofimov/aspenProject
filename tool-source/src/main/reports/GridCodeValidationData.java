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

import com.follett.fsc.core.k12.beans.GridCode;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.ObjectUtils;
import java.util.ArrayList;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.QueryByCriteria;


/**
 * Search all grid codes for inconsistancies, overlaps, numeric gaps.
 *
 * @author X2 Development Corporation
 */
public class GridCodeValidationData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Name for the "check LASIDs" report parameter. The value is a Boolean.
     */
    public static final String CHECK_LOCAL_IDS_PARAM = "checkLocalId";

    /**
     * Name for the "check person IDs (SSNs)" report parameter. The value is a Boolean.
     */
    public static final String CHECK_PERSON_IDS_PARAM = "checkPersonId";

    /**
     * Name for the "check SASIDs" report parameter. The value is a Boolean.
     */
    public static final String CHECK_STATE_IDS_PARAM = "checkStateId";

    /**
     * Name of the report grid column that contains the student's address. The value is a String.
     */
    public static final String GRID_ADDRESS_VIEW = "addressView";

    /**
     * Name of the report grid column that contains the actual duplicated ID. The value is a String.
     */
    public static final String GRID_DUPLICATE_ID = "duplicateId";

    /**
     * Name of the report grid column that contains the type of duplicated ID. The value is an
     * Integer:
     * <ul>
     * <li>1 -> Local ID
     * <li>2 -> State ID
     * <li>3 -> Person ID
     * </ul>
     */
    public static final String GRID_ERR_TYPE = "errType";

    /**
     * Name of the report grid column that contains the student's status. The value is a String.
     */
    public static final String GRID_GAP = "enrollmentStatus";

    /**
     * Name of the report grid column that contains the student's local ID. The value is a String.
     */
    public static final String GRID_NUMERIC_OVERLAP = "overlap";

    /**
     * Name of the report grid column that contains the student's name. The value is a String.
     */
    public static final String GRID_ODD_EVEN_OVERLAP = "nameView";

    /**
     * Name of the report grid column that contains the student's OID. The value is a String.
     */
    public static final String GRID_ODD_EVEN_RANGE = "oid";

    /**
     * Name of the report grid column that contains the student's person's ID. The value is a
     * String.
     */
    public static final String GRID_ID1 = "gridCode1";
    public static final String GRID_ID2 = "gridCode2";
    public static final String GRID_ADDRESS_VIEW1 = "gridView1";
    public static final String GRID_ADDRESS_VIEW2 = "gridView2";

    /**
     * Name of the report grid column that contains the indicator for whether or not to display the
     * duplicated ID. This is used for records that represent a second, third, or more student for
     * a single ID. The value is a Boolean.
     */
    public static final String GRID_PRINT = "print";

    /**
     * Name of the report grid column that contains the student's school's name. The value is a
     * String.
     */
    public static final String GRID_SCHOOL = "school";

    /**
     * Name of the report grid column that contains the student's state ID. The value is a String.
     */
    public static final String GRID_STATE_ID = "stateId";

    /**
     * Name of the report grid column that contains the student's YOG. The value is an Integer.
     */
    public static final String GRID_YOG = "yog";

    /**
     * Error type flags
     */
    private static final int ERROR_TYPE_OVERLAP = 1;
    private static final int ERROR_TYPE_GAP = 2;
    private static final int ERROR_TYPE_ODD_EVEN_RANGE = 3;

    /**
     * Map to hold translated error messages for the report.
     */
    private String[] m_messages = new String[3];

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        // Assumption: < 5% of grid codes are in error
        QueryByCriteria gridCodeQuery = new QueryByCriteria(GridCode.class, null);
        int totalGridCodes = getBroker().getCount(gridCodeQuery);
        int estimatedGridCodes = (int) (totalGridCodes * 0.05);

        ReportDataGrid gridCodeGrid = new ReportDataGrid(estimatedGridCodes, 6 + 1);

        checkGridCodes(gridCodeGrid);

        gridCodeGrid.beforeTop();

        return gridCodeGrid;
    }

    /**
     * Appends the student information to the grid.
     *
     * @param grid ReportDataGrid
     * @param gridCode1 GridCode
     * @param gridCode2 GridCode
     * @param errType int
     */
    private void appendRecord(ReportDataGrid grid, GridCode gridCode1, GridCode gridCode2, int errType) {
        grid.append();

        /*
         * error type message
         */
        String message = m_messages[errType - 1];
        grid.set(GRID_ERR_TYPE, message);

        /*
         * grid code values
         */
        grid.set(GRID_ID1, gridCode1.getGridCode());
        grid.set(GRID_ADDRESS_VIEW1, gridCode1.getStreetView());
        grid.set(GRID_ID2, gridCode2.getGridCode());
        grid.set(GRID_ADDRESS_VIEW2, gridCode2.getStreetView());
    }

    /**
     * Group grid codes by street.
     * Then check groups for numeric range problems.
     *
     * @param grid ReportDataGrid
     */
    private void checkGridCodes(ReportDataGrid grid) {
        QueryByCriteria query = new QueryByCriteria(SisStudent.class, null);
        query.addOrderByAscending(GridCode.COL_STREET_NAME);
        query.addOrderByAscending(GridCode.COL_STREET_TYPE);
        query.addOrderByAscending(GridCode.COL_STREET_PRE_DIRECTION);
        query.addOrderByAscending(GridCode.COL_STREET_POST_DIRECTION);
        query.addOrderByAscending(GridCode.COL_STREET_SIDE);
        query.addOrderByAscending(GridCode.COL_FIRST_STREET_NUMBER);

        GridCode lastGridCode = null;
        ArrayList<GridCode> gridList = new ArrayList(5);

        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                GridCode gridCode = (GridCode) iterator.next();

                if (lastGridCode != null) {
                    if (!ObjectUtils.match(gridCode.getStreetName(), lastGridCode.getStreetName()) ||
                            !ObjectUtils.match(gridCode.getStreetType(), lastGridCode.getStreetType()) ||
                            !ObjectUtils.match(gridCode.getStreetPreDirection(), lastGridCode.getStreetPreDirection())
                            ||
                            !ObjectUtils.match(gridCode.getStreetPostDirection(),
                                    lastGridCode.getStreetPostDirection())) {
                        // New street, change collection.
                        if (!gridList.isEmpty()) {
                            validateList(grid, gridList);
                            gridList.clear();

                        }
                    }
                }
                gridList.add(gridCode);
                lastGridCode = gridCode;
            }
            if (!gridList.isEmpty()) {
                validateList(grid, gridList);
            }
        } finally {
            iterator.close();
        }
    }

    /**
     * Given a collection of grid codes that represent the same street (and variations of numbers),
     * check the numbers are complete and non overlapping.
     *
     * @param grid ReportDataGrid
     * @param gridList ArrayList<GridCode>
     */
    private void validateList(ReportDataGrid grid, ArrayList<GridCode> gridList) {
        // Track ranges covered by the grid elements.
        // element 0 : length used
        // element pair (1, 2) : start/end pair.
        if (gridList.size() > 1) {
            GridCode[] oddRange = new GridCode[gridList.size() + 1];
            GridCode[] evenRange = new GridCode[gridList.size() + 1];
            int oddLength = 0;
            int evenLength = 0;

            for (GridCode code : gridList) {
                int start = code.getFirstStreetNumber();
                if (code.getStreetSide() == GridCode.StreetSideCode.BOTH.ordinal() ||
                        code.getStreetSide() == GridCode.StreetSideCode.ODD.ordinal()) {
                    if (oddLength == 0) {
                        if (start > 1) {
                            appendRecord(grid, code, code, ERROR_TYPE_GAP);
                        }
                        oddRange[++oddLength] = code;
                    } else {
                        if (start < oddRange[oddLength].getLastStreetNumber() + 1) {
                            appendRecord(grid, oddRange[oddLength], code, ERROR_TYPE_OVERLAP);
                        } else if (start > oddRange[oddLength].getLastStreetNumber() + 1) {
                            appendRecord(grid, oddRange[oddLength], code, ERROR_TYPE_GAP);
                        }
                        oddRange[++oddLength] = code;
                    }
                }
                if (code.getStreetSide() == GridCode.StreetSideCode.BOTH.ordinal() ||
                        code.getStreetSide() == GridCode.StreetSideCode.EVEN.ordinal()) {
                    if (evenLength == 0) {
                        if (start > 1) {
                            appendRecord(grid, code, code, ERROR_TYPE_GAP);
                        }
                        evenRange[++evenLength] = code;
                    } else {
                        if (start < evenRange[evenLength].getLastStreetNumber() + 1) {
                            appendRecord(grid, evenRange[evenLength], code, ERROR_TYPE_OVERLAP);
                        } else if (start > evenRange[evenLength].getLastStreetNumber() + 1) {
                            appendRecord(grid, evenRange[evenLength], code, ERROR_TYPE_GAP);
                        }
                        evenRange[++evenLength] = code;
                    }
                }
            }
            if (oddLength == 0) {
                appendRecord(grid, evenRange[0], evenRange[0], ERROR_TYPE_ODD_EVEN_RANGE);
            } else if (evenLength == 0) {
                appendRecord(grid, oddRange[0], oddRange[0], ERROR_TYPE_ODD_EVEN_RANGE);
            } else if (Math.abs(oddRange[oddLength - 1].getLastStreetNumber()
                    - evenRange[evenLength - 1].getLastStreetNumber()) > 1) {
                appendRecord(grid, oddRange[oddLength - 1], evenRange[evenLength - 1], ERROR_TYPE_ODD_EVEN_RANGE);
            }
        }
    }
}

