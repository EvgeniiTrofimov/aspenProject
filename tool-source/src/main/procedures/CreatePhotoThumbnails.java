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
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.k12.beans.Photo;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.business.PhotoManager;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import org.apache.ojb.broker.query.Criteria;

/**
 * Procedure for generating thumbnails previews of photos uploaded into Aspen. Intended for versions
 * of Aspen prior to
 * 5.0 since they do not automatically create thumbnails.
 *
 * @author Follett Software Company
 */
public class CreatePhotoThumbnails extends ProcedureJavaSource {
    private int m_updatedPhotos;

    /**
     * Execute.
     *
     * @see
     *      com.follett.fsc.ermuniversalschema.database.dbupgrade.procedures.DbUpgradeProcedure#execute()
     */
    @Override
    public void execute() {
        Criteria criteria = new Criteria();
        criteria.addIsNull(Photo.COL_PHOTO_THUMBNAIL);
        criteria.addNotNull(Photo.COL_PHOTO);

        BeanQuery query = new BeanQuery(Photo.class, criteria);

        QueryIterator iterator = getBroker().getIteratorByQuery(query);

        while (iterator.hasNext()) {
            Photo photo = (Photo) iterator.next();

            byte[] thumbnailPhoto = PhotoManager.generateThumbnail(photo.getPhoto());

            photo.setPhotoThumbnail(thumbnailPhoto);

            getBroker().saveBeanForced(photo);
            m_updatedPhotos++;
        }

        logMessage("Updated " + m_updatedPhotos + " photos to have thumbnails.");
    }
}
