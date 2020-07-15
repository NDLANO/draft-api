/*
 * Part of NDLA draft-api.
 * Copyright (C) 2020 NDLA
 *
 * See LICENSE
 */

package no.ndla.draftapi.controller

import no.ndla.draftapi.model.api.{UpdatedUserData}
import no.ndla.draftapi.{DraftSwagger, TestData, TestEnvironment, UnitSuite}
import no.ndla.draftapi.auth.UserInfo
import org.json4s.DefaultFormats
import org.scalatra.test.scalatest.ScalatraFunSuite

import scala.util.{Failure, Success}

class UserDataControllerTest extends UnitSuite with TestEnvironment with ScalatraFunSuite {
  implicit val formats: DefaultFormats.type = org.json4s.DefaultFormats
  implicit val swagger: DraftSwagger = new DraftSwagger

  lazy val controller = new UserDataController()
  addServlet(controller, "/test")

  test("GET / should return 200 if user has access roles and the user exists in database") {
    when(readService.getUserData(any[String])).thenReturn(Success(TestData.emptyApiUserData))
    when(user.getUser).thenReturn(TestData.userWithWriteAccess)

    get(s"/test/") {
      status should equal(200)
    }
  }

  test("GET / should return 403 if user has no access roles") {
    when(user.getUser).thenReturn(TestData.userWithNoRoles)

    get(s"/test/") {
      status should equal(403)
    }
  }
  // TODO: i dunno how to make it return 500 error type, fix before merge
  test("GET / should return 500 if there was error returning the data") {
    when(readService.getUserData(any[String])).thenReturn(Failure(???))
    when(user.getUser).thenReturn(TestData.userWithWriteAccess)

    get(s"/test/") {
      status should equal(500)
    }
  }

  test("PATCH / should return 200 if user has access roles and data has been updated correctly") {
    when(writeService.updateUserData(any[UpdatedUserData], any[UserInfo]))
      .thenReturn(Success(TestData.emptyApiUserData))
    when(user.getUser).thenReturn(TestData.userWithWriteAccess)

    patch(s"/test/") {
      status should equal(200)
    }
  }

  test("PATCH / should return 403 if user has no access roles") {
    when(user.getUser).thenReturn(TestData.userWithNoRoles)

    patch(s"/test/") {
      status should equal(403)
    }
  }
  // TODO: i dunno how to make it return 500 error type, fix before merge
  test("PATCH / should return 500 if there was error returning the data") {
    when(writeService.updateUserData(any[UpdatedUserData], any[UserInfo])).thenReturn(Failure(???))
    when(user.getUser).thenReturn(TestData.userWithNoRoles)

    patch(s"/test/") {
      status should equal(500)
    }
  }

}
