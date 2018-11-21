/*
 * Part of NDLA draft-api.
 * Copyright (C) 2018 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.service
import com.amazonaws.AmazonServiceException
import no.ndla.draftapi.{TestEnvironment, UnitSuite}
import org.mockito.ArgumentMatchers._
import org.mockito.Mockito._

class FileStorageServiceTest extends UnitSuite with TestEnvironment {
  val Content = "content"
  val ContentType = "application/pdf"
  override lazy val fileStorage = new FileStorageService

  test("That objectExists returns true when file exists") {
    when(amazonClient.doesObjectExist(any[String], any[String])).thenReturn(true)
    fileStorage.objectExists("existingKey") should be(true)
  }

  test("That objectExists returns false when file does not exist") {
    when(amazonClient.doesObjectExist(any[String], any[String])).thenThrow(mock[AmazonServiceException])
    fileStorage.objectExists("nonExistingKey") should be(false)
  }
}
