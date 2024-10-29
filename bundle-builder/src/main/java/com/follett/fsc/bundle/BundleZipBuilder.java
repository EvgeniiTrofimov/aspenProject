/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2021 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.follett.fsc.bundle;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.Duration;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.stream.Stream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipException;
import java.util.zip.ZipOutputStream;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Logger;

/**
 * @author Follett Software Company
 * @copyright 2021
 */
public class BundleZipBuilder {
	final BundleProperties bundleProperties;
	final Set<String> packagePaths;
	final Set<String> commonSources;
	final File customBundleDefinitionLocation;
	final boolean debug = false;
	private final static Logger logger = Logger.getLogger(BundleZipBuilder.class);
	final String sysSharedDir = "shared";

	public enum PathType {
		FLAT, SHARED, FULL, BUNDLE_RESOURCES;
	}

	/**
	 * @param bundleProperties
	 * @param packagePaths
	 * @param commonSources
	 * @param customBundleDefinitionLocation
	 */
	public BundleZipBuilder(BundleProperties bundleProperties, Set<String> packagePaths, Set<String> commonSources,
			File customBundleDefinitionLocation) {
		super();
		this.bundleProperties = bundleProperties;
		this.commonSources = commonSources;
		this.packagePaths = packagePaths;
		this.customBundleDefinitionLocation = customBundleDefinitionLocation;

	}

	public void createZipFile() throws MissingFileException, BundleVerificationException {
		Map<File, PathType> fullPathZipFileList = createZipFileList();
		fullPathZipFileList.put(customBundleDefinitionLocation, PathType.FLAT);
		writeZipFile(fullPathZipFileList);
	}

	private void writeZipFile(Map<File, PathType> fullPathZipFileList) throws BundleVerificationException {
		String zipFile = Paths.get(bundleProperties.getTargetDir().toString(), bundleProperties.getZipFileName())
				.toString();

		Set<String> foldersCreated = new HashSet<String>();

		try (FileOutputStream fos = new FileOutputStream(zipFile); ZipOutputStream zos = new ZipOutputStream(fos);) {
			byte[] buffer = new byte[1024];
			for (Entry<File, PathType> srcFileEntry : fullPathZipFileList.entrySet()) {
				File srcFile = srcFileEntry.getKey();
				if (srcFile.isDirectory()) {
					logger.error(srcFile + " is a directory ******************** ");
				}
				FileInputStream fis = new FileInputStream(srcFile);

				if (srcFileEntry.getValue() != PathType.FLAT) {
					String folder = getFolder(srcFile.toPath(), srcFileEntry.getValue());
					createZipFolderTree(foldersCreated, Paths.get(folder), zos);
				}
				zos.putNextEntry(new ZipEntry(getFolderSpecificFilePath(srcFile.toPath(), srcFileEntry.getValue()).replace("\\","/")));
				int length;
				while ((length = fis.read(buffer)) > 0) {
					zos.write(buffer, 0, length);
				}
				zos.closeEntry();
				fis.close();
			}
		} catch (IOException ioe) {
			ioe.printStackTrace();
			logger.error("Error creating zip file: " + ioe);
		}
		if (bundleProperties.isStateFile()) {
			BundleVerifier.verify(Paths.get(zipFile), bundleProperties.getState(), bundleProperties.getOtherStates());
		}
	}

	/**
	 * Creates the tree structure in the zip file
	 *
	 * @param folderCreated
	 * @param fullFolderPath
	 * @param zos
	 * @throws IOException
	 */
	private void createZipFolderTree(Set<String> folderCreated, Path fullFolderPath, ZipOutputStream zos)
			throws IOException {
		if (fullFolderPath == null) {
			return;
		}
		String fullFolder = fullFolderPath.toString();
		if (!fullFolder.endsWith(File.separator)) {
			fullFolder = fullFolder.concat(File.separator);
		}
		if (folderCreated.contains(fullFolder)) {
			return;
		}
		if (fullFolder.equals(File.separator)) {
			return;
		}
		try {
			if (!StringUtils.isEmpty(fullFolder.toString())) {
				zos.putNextEntry(new ZipEntry(fullFolder.replace("\\","/")));
				folderCreated.add(fullFolder);
			}
			createZipFolderTree(folderCreated, fullFolderPath.getParent(), zos);
		} catch (ZipException e) {
			// ignore the duplicate entry on the folder
		}
		return;
	}

	/**
	 * Adds all the full paths to the map, so we can create the zip file. If there
	 * are files missing from the zip, this is the method to debugs
	 *
	 * @return
	 * @throws MissingFileException
	 */
	private Map<File, PathType> createZipFileList() throws MissingFileException {

		Map<File, PathType> fullPathZipFileList = new HashMap<File, PathType>();
		Path aspenProjectPath = bundleProperties.getStateSourcePath();
		Set<String> foundSources = new HashSet<String>();

		if (commonSources.isEmpty()) {
			return fullPathZipFileList;
		}

		logger.debug("BEGIN ZIP");

		logger.debug("packages: " + packagePaths);
		for (String packagePath : packagePaths) {
			logger.debug("  package: " + packagePath);
			Path fileSearchPath = getFileSearchPath(packagePath);
			if (fileSearchPath == null) {
				logger.debug("Skipping [" + fileSearchPath + "][" + packagePath + "] because it doesn't exist");
				continue;
			}

			Path pathWithoutPackage = Paths.get(fileSearchPath.toString().replace(packagePath, ""));

			if (!Files.exists(pathWithoutPackage)) {
				logger.debug("The pathWithoutThePackage does not exist: " + pathWithoutPackage);
			}
			logger.debug("  searchPath: " + fileSearchPath);
			logger.debug("  mainPath: " + pathWithoutPackage);

			List<Path> directoriesToSearch = findRecursiveSingleStateDirectories(pathWithoutPackage);

			for (String source : commonSources) {
				Path sourcePath = Paths.get(fileSearchPath.toString(), source);

				if (Files.exists(sourcePath)) {
					if (Files.isDirectory(sourcePath)) {
						logger.error("1. SourcePath is a directory: " + sourcePath + "  [" + source + "]");
					}

					if (sourcePath.toString().contains("bundle-resources")) {
						fullPathZipFileList.put(sourcePath.toFile(), PathType.BUNDLE_RESOURCES);
						logger.debug("    1.  file is BUNDLE_RESOURCES");
					}
					else if (sourcePath.getParent().endsWith(sysSharedDir)) {
						fullPathZipFileList.put(sourcePath.toFile(), PathType.SHARED);
						logger.debug("    1.  file is SHARED");
					} else {
						fullPathZipFileList.put(sourcePath.toFile(), PathType.FULL);
						logger.debug("    1.  file is FULL");
					}
					foundSources.add(source);
					logger.debug(" [" + sourcePath + "] found. " + source + " to evaluated");

					List<File> recursiveFiles = findRecursiveFiles(directoriesToSearch, source);
					for (File file : recursiveFiles) {
						if (file.isDirectory()) {
							logger.error("2. file is a directory: " + file);
						}
						
						if (file.getAbsolutePath().contains("bundle-resources")) {
							fullPathZipFileList.put(file, PathType.BUNDLE_RESOURCES);
							logger.debug("    2.  file is BUNDLE_RESOURCES");
						}
						else if (file.getParent().endsWith(sysSharedDir)) {
							fullPathZipFileList.put(file, PathType.SHARED);
							logger.debug("    2.  file is SHARED");
						} else {
							fullPathZipFileList.put(file, PathType.FULL);
							logger.debug("    2.  file is FULL");
						}
						
						logger.debug("    Added **/" + file.getAbsolutePath() + " to zip");
					}

				} else {
					logger.debug("Skipping [" + sourcePath + "] because it doesn't exist or was added already");
				}
			}
		}
		logger.debug("END ZIP");
		commonSources.removeAll(foundSources);

		if (commonSources.isEmpty()) {
			return fullPathZipFileList;
		}

		foundSources = new HashSet<String>();
		// look search paths only is sources left.
		Set<String> searchPaths = bundleProperties.getSearchPaths();
		logger.debug("BEGIN COPY");

		for (String searchPath : searchPaths) {
			logger.debug("  searchPath: " + searchPath);
			List<Path> directoriesToSearch = findRecursiveSingleStateDirectories(
					bundleProperties.getDirectoryPathsAsPath(aspenProjectPath, searchPath));

			for (String source : commonSources) {
				Path sourcePath = bundleProperties.getDirectoryPathsAsPath(aspenProjectPath, searchPath, source);

				if (Files.exists(sourcePath)) {
					logger.debug("    [" + sourcePath + "] found.  ");
					foundSources.add(source);
					List<File> recursiveFiles = findRecursiveFiles(directoriesToSearch, source);
					for (File file : recursiveFiles) {
						if (file.isDirectory()) {
							logger.error("3. file is a directory: " + file);
						}
						if (file.getAbsolutePath().contains("bundle-resources")) {
							fullPathZipFileList.put(file, PathType.BUNDLE_RESOURCES);
							logger.debug("    3.  file is BUNDLE_RESOURCES");
						} else if (file.getParent().endsWith(sysSharedDir)) {
							fullPathZipFileList.put(file, PathType.SHARED);
							logger.debug("    3.  file is SHARED");
						} else {
							fullPathZipFileList.put(file, PathType.FLAT);
							logger.debug("    3.  file is FLAT");
						}
						logger.debug("    Added **/" + file.getAbsolutePath() + " to copy");
					}
				} else {
					logger.debug("Skipping [" + sourcePath + "] because it doesn't exist");
				}
			}
		}
		logger.debug("END COPY");
		commonSources.removeAll(foundSources);

		if (!commonSources.isEmpty()) {
			throw new MissingFileException(bundleProperties.getState(), bundleProperties.getExportDirectory(),
					commonSources);
		}

		return fullPathZipFileList;
	}

	private void outputDuration(String message, long durationInMillis) {
		Duration duration = Duration.ofMillis(durationInMillis);
		logger.debug(message + " took "
				+ duration.toString().substring(2).replaceAll("(\\d[HMS])(?!$)", "$1 ").toLowerCase());
	}

	private List<File> findRecursiveFiles(List<Path> directories, String filename) {
		List<File> files = new ArrayList<File>();
		long start = System.currentTimeMillis();
		for (Path singleStatePath : directories) {
			for (File f : singleStatePath.toFile().listFiles()) {
				if (f.getName().equals(filename)) {
					files.add(f);
				}
			}
		}
		outputDuration("findRecursiveFiles", System.currentTimeMillis() - start);
		return files;
	}

	private List<Path> findRecursiveSingleStateDirectories(Path parentPath) {
		List<Path> directories = new ArrayList<Path>();
		long start = System.currentTimeMillis();

		try (Stream<Path> walkStream = Files.walk(parentPath)) {
			walkStream.filter(p -> p.toFile().isDirectory()).forEach(f -> {
				if (!BundleVerifier.entryContainsOtherState(f.toString(), bundleProperties.getOtherStates())) {
					directories.add(f);
				}
			});
		} catch (IOException e) {
			// Error while reading the directory
		}
		outputDuration("findRecursiveDirectories [" + parentPath + "]", System.currentTimeMillis() - start);
		logger.debug("We are searching: " + directories);
		return directories;
	}

	private String getFolderSpecificFilePath(Path fullPathFileName, PathType pathType) {
		switch (pathType) {
		case FLAT:
			return fullPathFileName.getFileName().toString();
		case SHARED:
			String fileName = fullPathFileName.getFileName().toString();
			return Paths.get("shared", fileName).toString();
		case BUNDLE_RESOURCES:
			String fileName2 = fullPathFileName.getFileName().toString();
			return Paths.get("bundle-resources", fileName2).toString();
		case FULL:
		default:
			// strip off the common pieces and return as a full path
			String relativeFileName = fullPathFileName.toString();
			if (relativeFileName.contains("procedures")) {
				relativeFileName = StringUtils.replaceOnce(relativeFileName, bundleProperties.getSharedProceduresDir().toString(),
						"");
			} else {
				relativeFileName = StringUtils.replaceOnce(relativeFileName, bundleProperties.getSharedReportsDir().toString(), "");
			}
			if (relativeFileName.startsWith(File.separator)) {
				relativeFileName = relativeFileName.substring(1);
			}
			return relativeFileName;
		}
	}

	private String getFolder(Path fullPathFileName, PathType pathType) {
		switch (pathType) {
		case FLAT:
			return "";
		case SHARED:
			return "shared/";
		case BUNDLE_RESOURCES:
			return "bundle-resources/";
		case FULL:
		default:
			// strip off the common pieces and return as a full path
			String relativeFileName = fullPathFileName.getParent().toString();
			if (relativeFileName.contains("procedures")) {
				relativeFileName = StringUtils.replaceOnce(relativeFileName, bundleProperties.getSharedProceduresDir().toString(),
						"");
			} else {
				relativeFileName = StringUtils.replaceOnce(relativeFileName, bundleProperties.getSharedReportsDir().toString(), "");
			}
			if (relativeFileName.startsWith(File.separator)) {
				relativeFileName = relativeFileName.substring(1);
			}
			if (!relativeFileName.endsWith(File.separator)) {
				relativeFileName = relativeFileName.concat(File.separator);
			}
			return relativeFileName;
		}
	}

	private Path getFileSearchPath(String packagePath) {
		Set<String> searchPaths = bundleProperties.getSearchPaths();

		for (String searchPath : searchPaths) {
			if (searchPath.endsWith(packagePath)) {
				Path fullPath = bundleProperties.getDirectoryPathsAsPath(bundleProperties.getStateSourcePath(),
						searchPath);
				if (Files.exists(fullPath)) {
					return fullPath;
				}
			}
		}
		return null;
	}
}
