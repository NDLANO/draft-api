/*
 * Part of NDLA draft-api.
 * Copyright (C) 2018 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.service

import java.io.InputStream

import com.amazonaws.services.s3.model._
import com.typesafe.scalalogging.LazyLogging
import no.ndla.draftapi.DraftApiProperties.AttachmentStorageName
import no.ndla.draftapi.integration.AmazonClient

import scala.util.Try

trait FileStorageService {
  this: AmazonClient =>
  val fileStorage: FileStorageService

  class FileStorageService extends LazyLogging {

    def uploadFromStream(stream: InputStream, storageKey: String, contentType: String, size: Long): Try[String] = {
      val metadata = new ObjectMetadata()
      metadata.setContentType(contentType)
      metadata.setContentLength(size)

      Try(amazonClient.putObject(new PutObjectRequest(AttachmentStorageName, storageKey, stream, metadata))).map(_ =>
        storageKey)
    }

    def objectExists(storageKey: String): Boolean = {
      Try(amazonClient.doesObjectExist(AttachmentStorageName, storageKey)).getOrElse(false)
    }

    def deleteObject(storageKey: String): Try[_] = Try(amazonClient.deleteObject(AttachmentStorageName, storageKey))
  }

}
