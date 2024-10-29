/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2012 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.wa;

import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.utils.ZipUtils;
import java.io.File;
import java.io.FilenameFilter;
import java.text.DecimalFormat;
import java.text.SimpleDateFormat;

/**
 * Procedure to take 15 individual state report output files and package
 * them into a zip file with appropriate file names.
 *
 * @author X2 Development Corporation
 */
public class WAPackageReports extends ProcedureJavaSource {
    private static final String ALIAS_DISTRICT_ID = "DOE DISTRICT ID";
    private static final String FILE_SUFFIX = ".txt";
    private static final String FILE_ZIP_NAME = "CEDARS";
    private static final String FILE_ZIP_SUFFIX = ".zip";
    private static final String PARAM_FILE_PATH = "filePath";
    private static final String PARAM_ZIP_NAME = "zipName";
    private static final String[] SOURCE_FILES = {"Location",
            "DistrictStudent",
            "SchoolStudent",
            "CourseCatalog",
            "StudentSchedule",
            "Staff",
            "StaffSchedule",
            "StudentGradeHistory",
            "StudentPrograms",
            "BilingualPrograms",
            "SpecEdPrograms",
            "StudentEthnicity",
            "StudentRace",
            "StudentAbsence",
            "StudentDiscipline",
            "LAPStudentGrowth"};

    private static final String PARAM_NO_DELETE = "doNotDelete";
    private static final String PARAM_NO_DELETE_DEST = "doNotDeleteDest";



    /**
     * Custom file filter to identify files for the bundle.
     */
    private class CEDARSFilter implements FilenameFilter {
        /**
         * The file prefix to match for.
         */
        private String m_prefix;

        /**
         * Constructor.
         * <p>
         * Accepts the file prefix pattern to match.
         *
         * @param prefix String
         */
        protected CEDARSFilter(String prefix) {
            m_prefix = prefix;
        }

        /**
         * Accept.
         *
         * @param dir File
         * @param name String
         * @return true, if successful
         * @see java.io.FilenameFilter.accept(File, String)
         */
        @Override
        public boolean accept(File dir, String name) {
            return name.startsWith(m_prefix); // && name.endsWith(m_postfix + FILE_SUFFIX);
        }
    }

    /**
     * Main procedure for procedure.
     *
     * @throws Exception exception
     */
    @Override
    protected void execute() throws Exception {
        Boolean expandZipName = (Boolean) getParameter(PARAM_ZIP_NAME);
        Boolean noDeleteDest = (Boolean) getParameter(PARAM_NO_DELETE_DEST);


        SimpleDateFormat dateFormat = new SimpleDateFormat("yyyyMMdd");
        DecimalFormat intFormat = new DecimalFormat("0000");

        // Get components of new file names.
        String districtId = (String) getOrganization().getFieldValueByAlias(ALIAS_DISTRICT_ID);
        String reportDate = dateFormat.format(new java.util.Date());
        int year = getCurrentContext().getSchoolYear();
        String schoolYear = intFormat.format(year - 1) + intFormat.format(year);
        String prefix = districtId + "_0000_";
        String postfix = "_" + reportDate + "_" + schoolYear;
        FilenameFilter filter = new CEDARSFilter(prefix);

        // Get working directory.
        String exportFileDirectoryPath = getSaveFilePath();
        File exportFileDirectory = new File(exportFileDirectoryPath);

        // Clean out text files from previous runs.
        workDirClean(exportFileDirectory, filter);

        // Rename export files and include into the zip file.
        for (String fileRoot : SOURCE_FILES) {
            String sourceFileName = exportFileDirectoryPath + fileRoot + FILE_SUFFIX;
            String destinationFileName = exportFileDirectoryPath + prefix + fileRoot + postfix + FILE_SUFFIX;

            File sourceFile = new File(sourceFileName);
            File destFile = new File(destinationFileName);

            // If the destination file exists, delete it.
            if (destFile.exists() && !noDeleteDest.booleanValue()) {
                destFile.delete();
            }

            // If the source file exists, rename it to the destination file.
            if (sourceFile.exists()) {
                sourceFile.renameTo(destFile);
            }
        }

        // Write the zip file.
        String zipFileName = null;
        if (expandZipName.booleanValue()) {
            zipFileName = prefix + FILE_ZIP_NAME + postfix + FILE_ZIP_SUFFIX;
        } else {
            zipFileName = FILE_ZIP_NAME + FILE_ZIP_SUFFIX;
        }
        File zipFile = new File(exportFileDirectoryPath + zipFileName);
        if (zipFile.exists()) {
            zipFile.delete();
        }
        ZipUtils.zipFilesToDisk(exportFileDirectory, filter, zipFile);
    }

    /**
     * Determine the final file directory that contains the 15 export files to pack.
     * <p>
     * If useSecure mode in on in ApplicationConfig.properties:
     * <br>
     * Get the secure directory and append the path provided in parameter "filePath".
     * <p>
     * If useSecure mode in off in ApplicationConfig.properties:
     * <br>
     * Use the path provided in parameter "filePath" as the full path to the directory.
     *
     * @return String
     */
    private String getSaveFilePath() {
        File secureDir = AppGlobals.getSecureRootDirectory(getOrganization(), "x2sis");
        String fileSavePath = (String) getParameter(PARAM_FILE_PATH);

        String path = null;
        if (secureDir != null) {
            path = secureDir.getAbsolutePath();
            AppGlobals.getLog().severe("PATH 1: " + path);

            if (!path.endsWith("/") && !path.endsWith("\\")) {
                path += "/";
            }
            path += fileSavePath;
            if (!path.endsWith("/") && !path.endsWith("\\")) {
                path += "/";
            }
        } else {
            path = fileSavePath;
            if (!path.endsWith("/") && !path.endsWith("\\")) {
                path += "/";
            }
        }

        /*
         * This was edited to return the path entered by the user since the above code seems to only
         * be for the hosted
         * environment.
         */
        return fileSavePath;
    }

    /**
     * Clean out text files from previous version.
     * <br>
     * Use the working directory to search for files, and the file name filter to identify files
     * to delete.
     *
     * @param directory File
     * @param filter FilenameFilter
     */
    private void workDirClean(File directory, FilenameFilter filter) {
        Boolean noDelete = (Boolean) getParameter(PARAM_NO_DELETE);
        if (directory.exists() && directory.isDirectory() && !noDelete.booleanValue()) {
            File[] filesToDelete = directory.listFiles(filter);
            for (File file : filesToDelete) {
                file.delete();
            }
        }
    }
}
