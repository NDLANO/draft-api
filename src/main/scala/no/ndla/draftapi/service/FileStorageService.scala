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
import no.ndla.draftapi.model.domain

import scala.util.Try

trait FileStorageService {
  this: AmazonClient =>
  val fileStorage: FileStorageService

  case class Uploaded(path: String, contentLength: Long, contentType: String)

  class FileStorageService extends LazyLogging {
    private val resourceDirectory = "resources"

    def uploadResourceFromStream(stream: InputStream,
                                 storageKey: String,
                                 contentType: String,
                                 size: Long): Try[Uploaded] = {
      val metadata = new ObjectMetadata()
      metadata.setContentType(contentType)
      metadata.setContentLength(size)

      val uploadPath = s"$resourceDirectory/$storageKey"

      Try(
        amazonClient
          .putObject(new PutObjectRequest(AttachmentStorageName, uploadPath, stream, metadata))
      ).map(
        por =>
          Uploaded(
            uploadPath,
            por.getMetadata.getContentLength,
            por.getMetadata.getContentType,
        ))
    }

    def resourceExists(storageKey: String): Boolean =
      Try(amazonClient.doesObjectExist(AttachmentStorageName, s"$resourceDirectory/$storageKey")).getOrElse(false)

    def deleteResource(storageKey: String): Try[_] =
      Try(amazonClient.deleteObject(AttachmentStorageName, s"$resourceDirectory/$storageKey"))
  }

}
