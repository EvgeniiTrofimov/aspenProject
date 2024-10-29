/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2013 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.reports.destiny;

import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.ws.client.WebServiceException;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.Unmarshaller;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementWrapper;
import javax.xml.bind.annotation.XmlRootElement;
import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.methods.GetMethod;


/**
 * Report to find the Destiny school information using the remote services preferences of Server
 * name and
 * district context. The report should return all the schools with a site id and short name.
 *
 */
@SuppressWarnings("serial")
public class DestinySchoolReport extends ReportJavaSourceNet {

    // Report fields
    private static final String FIELD_SCHOOL_NAME = "schoolName";
    private static final String FIELD_SCHOOL_SHORT_NAME = "schoolShortName";
    private static final String FIELD_SITE_ID = "schoolSiteId";

    /**
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        ReportDataGrid grid = new ReportDataGrid();
        String server = PreferenceManager.getPreferenceValue(getOrganization(), "sys.destiny.server");
        String context1 = PreferenceManager.getPreferenceValue(getOrganization(), "sys.destiny.context");

        HttpClient client = new HttpClient();
        GetMethod get = new GetMethod("http://" + server + "/rest/district/site/list?contextName=" + context1);
        int status = client.executeMethod(get);
        if (status == 200) {
            String response = get.getResponseBodyAsString();
            StringReader reader = new StringReader(response);

            JAXBContext jaxbContext = JAXBContext.newInstance(SiteResults.class);
            Unmarshaller jaxbUnmarshaller = jaxbContext.createUnmarshaller();
            SiteResults siteResults = (SiteResults) jaxbUnmarshaller.unmarshal(reader);

            /*
             * Results will look like the following :
             * <siteResults>
             * <sites>
             * <siteRecord>
             * <asset>false</asset>
             * <library>true</library>
             * <media>false</media>
             * <siteID>100</siteID>
             * <siteName>Middle School</siteName>
             * <siteShortName>Middle</siteShortName>
             * <textbook>false</textbook>
             * </siteRecord>
             * <siteRecord>
             * <asset>false</asset>
             * <library>true</library>
             * <media>false</media>
             * <siteID>101</siteID>
             * <siteName>High School</siteName>
             * <siteShortName>High</siteShortName>
             * <textbook>false</textbook>
             * </siteRecord>
             * </sites>
             * </siteResults>
             */

            try {
                for (SiteRecord record : siteResults.getSiteList()) {
                    grid.append();
                    grid.set(FIELD_SITE_ID, String.valueOf(record.getSiteId()));
                    grid.set(FIELD_SCHOOL_NAME, record.getSiteName());
                    grid.set(FIELD_SCHOOL_SHORT_NAME, record.getSiteShortName());

                }
                grid.beforeTop();
                grid.sort(FIELD_SCHOOL_NAME, false);
            } catch (WebServiceException exception) {
                throw exception;
            }
        }
        return grid;
    }


    /**
     * The Class SiteResults.
     */
    @XmlRootElement(name = "siteResults")
    private static class SiteResults {

        private List<SiteRecord> siteList;

        /**
         * Instantiates a new site results.
         */
        @SuppressWarnings("unused")
        public SiteResults() {
            siteList = new ArrayList<SiteRecord>();
        }

        /**
         * Gets the site list.
         *
         * @return List
         */
        @XmlElementWrapper(name = "sites")
        @XmlElement(name = "siteRecord")
        public List<SiteRecord> getSiteList() {
            return siteList;
        }

        /**
         * Sets the site list.
         *
         * @param siteList void
         */
        @SuppressWarnings("unused")
        public void setSiteList(List<SiteRecord> siteList) {
            this.siteList = siteList;
        }
    }

    /**
     * Inner class to define a SiteRecord XML object.
     */
    private static class SiteRecord {
        private boolean asset;
        private boolean library;
        private boolean media;
        private int siteId;
        private String siteName;
        private String siteShortName;
        private boolean textbook;

        /**
         * Instantiates a new site record.
         */
        @SuppressWarnings(value = {"unused"})
        public SiteRecord() {

        }

        /**
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            return "SiteName : " + siteName;
        }

        /**
         * Checks if is asset.
         *
         * @return true, if is asset
         */
        @XmlElement(name = "asset")
        public boolean isAsset() {
            return asset;
        }

        /**
         * Checks if is library.
         *
         * @return true, if is library
         */
        @XmlElement(name = "library")
        public boolean isLibrary() {
            return library;
        }

        /**
         * Checks if is media.
         *
         * @return true, if is media
         */
        @XmlElement(name = "media")
        public boolean isMedia() {
            return media;
        }

        /**
         * Gets the site id.
         *
         * @return int
         */
        @XmlElement(name = "siteID")
        public int getSiteId() {
            return siteId;
        }

        /**
         * Gets the site name.
         *
         * @return String
         */
        @XmlElement(name = "siteName")
        public String getSiteName() {
            return siteName;
        }

        /**
         * Gets the site short name.
         *
         * @return String
         */
        @XmlElement(name = "siteShortName")
        public String getSiteShortName() {
            return siteShortName;
        }

        /**
         * Checks if is textbook.
         *
         * @return true, if is textbook
         */
        @XmlElement(name = "textbook")
        public boolean isTextbook() {
            return textbook;
        }

        /**
         * Sets the asset.
         *
         * @param asset void
         */
        @SuppressWarnings(value = {"unused"})
        public void setAsset(boolean asset) {
            this.asset = asset;
        }

        /**
         * Sets the library.
         *
         * @param library void
         */
        @SuppressWarnings(value = {"unused"})
        public void setLibrary(boolean library) {
            this.library = library;
        }

        /**
         * Sets the media.
         *
         * @param media void
         */
        @SuppressWarnings(value = {"unused"})
        public void setMedia(boolean media) {
            this.media = media;
        }

        /**
         * Sets the site id.
         *
         * @param siteId void
         */
        @SuppressWarnings(value = {"unused"})
        public void setSiteId(int siteId) {
            this.siteId = siteId;
        }

        /**
         * Sets the site name.
         *
         * @param siteName void
         */
        @SuppressWarnings("unused")
        public void setSiteName(String siteName) {
            this.siteName = siteName;
        }

        /**
         * Sets the site short name.
         *
         * @param siteShortName void
         */
        @SuppressWarnings(value = {"unused"})
        public void setSiteShortName(String siteShortName) {
            this.siteShortName = siteShortName;
        }

        /**
         * Sets the textbook.
         *
         * @param textbook void
         */
        @SuppressWarnings(value = {"unused"})
        public void setTextbook(boolean textbook) {
            this.textbook = textbook;
        }
    }
}
