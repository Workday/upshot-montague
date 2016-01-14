/**
 * Code for Salesforce Rest API, typically used for OAuth and querying.
 */
package sfdc

// TODO: A lot of the Play imports break layering (we want this to standalone)
import play.api.libs.json._
import play.api.libs.ws._
import play.api.libs.concurrent.Execution.Implicits._
import scala.concurrent.{Future, Await}
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration
import _root_.util.Logging
import play.api.libs.ws.WS.WSRequestHolder
import java.net.URL
import play.api.http.{ContentTypeOf, Writeable}

case class ErrorResponse(message: String, errorCode: String)

object RestTimeout {
  import scala.concurrent.duration._

  val maxSfdcRestQueryTimeout = 15 minutes
}

/**
 * Authorization info (including access token) for SFDC. It could come from OAuth or SignedRequest
 * @param id id is actually the idUrl: "https://login.salesforce.com/id/<org id>/<user id>
 * @param issuedAt When the token was issued. According to the Salesforce docs:
 *                 "When the signature was created, represented as the number of seconds since the Unix epoch
 *                 (00:00:00 UTC on 1 January 1970)."
 * @param instanceUrl "Identifies the Salesforce instance to which API calls should be sent."
 * @param accessToken "Access token that acts as a session ID that the application uses for making requests.
 *                    This token should be protected as though it were user credentials."
 * @todo Perhaps rename this, since it contains an accessToken field. But I think this is the best concise name.
 */
abstract class AuthInfo {
  def id: String
  def issuedAt: Long
  def instanceUrl: String
  def accessToken: String

  /**
   * The Oauth response returns a field called "id" that looks something like:
   *  "https://login.salesforce.com/id/<org id>/<user id>
   * Parse it for the org id and user id
   */
  private[this] def parseIdUrl: (String, String) = {
    val idUrlParts = new URL(id).getPath.split("/")
    assert(idUrlParts(1) == "id")
    val sfdcOrganizationId = idUrlParts(2)
    val sfdcUserId = idUrlParts(3)
    assert(idUrlParts.length == 4)
    (sfdcOrganizationId, sfdcUserId)
  }

  lazy val (organizationId, userId) = parseIdUrl

  /**
   * Refresh token. None for AuthInfos that don't have refresh tokens (i.e. signed request tokens), and Some[String]
   * for AuthInfos that do (OauthTokens).
   */
  def refreshTokenOpt: Option[String]
}

/**
 * http://www.salesforce.com/us/developer/docs/api_rest/Content/intro_understanding_refresh_token_oauth.htm
 * @param refreshToken Used for refreshing (reauthenticating) access to OAuth services.
 * @todo refreshToken should be a String, not Option[String]. However, some users might have overwritten their OauthToken
 *       with a CanvasRequest, in which case the database is stale and we need to do a migration. (Look at the backup from
 *       20140502, or a day or so before, before people got access to canvas.)
 */
case class OauthInfo(id: String, issuedAt: Long, refreshToken: Option[String], instanceUrl: String, accessToken: String)
  extends AuthInfo {
  override def refreshTokenOpt = refreshToken
}

/**
 * Autorization information (access token) from a (canvas) Signed Request. Unfortunately, does not contain a refresh token.
 */
case class SignedRequestInfo(id: String, issuedAt: Long, instanceUrl: String, accessToken: String)
  extends AuthInfo {
  override def refreshTokenOpt = None
}

object RestOauthAuthApi {
  def getLoginHost(isSandbox: Boolean) = {
    if (!isSandbox) "login.salesforce.com" else "test.salesforce.com"
  }
}

class RestOauthAuthApi(oauthConsumerKey: String, oauthConsumerSecret: String, isSandbox: Boolean) {
  private[this] val loginApiUrl = "https://" + RestOauthAuthApi.getLoginHost(isSandbox) + "/services/oauth2/token"
  // TODO: redirectUri is duplicated in frontdoor.scala.html

  /* Retrieve the OAuth tokens, given the code from the OAuth authorization.
   * http://wiki.developerforce.com/page/Digging_Deeper_into_OAuth_2.0_on_Force.com
   */
  def oauthTokensJson(authorizationCode: String, oauthCallbackURL: String) = {
    val params = Map(
      "code" -> Seq(authorizationCode),
      "grant_type" -> Seq("authorization_code"),
      "client_id" -> Seq(oauthConsumerKey),
      "client_secret" -> Seq(oauthConsumerSecret),
      "redirect_uri" -> Seq(oauthCallbackURL)
    )
    val response = WS.url(loginApiUrl).post(params).map { response =>
    //logger.info("login raw response = " + response.body)
      response.json
    }
    response
  }

  def refreshTokenJson(refreshToken: String) = {
    val params = Map(
      "grant_type" -> Seq("refresh_token"),
      "refresh_token" -> Seq(refreshToken),
      "client_id" -> Seq(oauthConsumerKey),
      "client_secret" -> Seq(oauthConsumerSecret)
    )
    val response = WS.url(loginApiUrl).post(params).map(_.json)
    response
  }

  private[this] def handleErrorCode(json: JsValue) = {
    /**
     * Error responses seem to be in one of two formats: {errorCode, message} or {error, error_description}
     */
    val errorCodeOpt = (json \ "errorCode").asOpt[String]
    val errorOpt = (json \ "error").asOpt[String]
    errorCodeOpt map { errorCode =>
      Left(ErrorResponse((json \ "message").asOpt[String].getOrElse(""), errorCode))
    } getOrElse {
      errorOpt map { error =>
        Left(ErrorResponse((json \ "error_description").asOpt[String].getOrElse(""), error))
      } getOrElse {
        Right(json)
      }
    }
  }

  def oauthTokens(authorizationCode: String, oauthCallbackURL: String): Future[Either[ErrorResponse, OauthInfo]] = {
    oauthTokensJson(authorizationCode, oauthCallbackURL) map { json =>
      jsonToOauthResponse(json)
    }
  }

  def refreshToken(refreshToken: String): Future[Either[ErrorResponse, OauthInfo]] = {
    refreshTokenJson(refreshToken) map { json =>
      jsonToOauthResponse(json, Some(refreshToken))
    }
  }

  private[this] def jsonToOauthResponse(json: JsValue, refreshTokenOpt: Option[String] = None) = {
    handleErrorCode(json).right map { json =>
      val id = (json \ "id").as[String]
      val issuedAt = (json \ "issued_at").as[String].toLong
      val refreshToken =
        if (refreshTokenOpt.isDefined) { refreshTokenOpt.get }
        else { (json \ "refresh_token").as[String] }
      val instanceUrl = (json \ "instance_url").as[String]
      val signature = (json \ "signature").as[String]
      // TODO: Check the signature
      val accessToken = (json \ "access_token").as[String]
      val oauthInfo = OauthInfo(id, issuedAt, Some(refreshToken), instanceUrl, accessToken)
      oauthInfo
    }
  }

  /**
   * Example:
   * curl "https://login.salesforce.com/services/oauth2/token" -d "grant_type=password" -d "client_id=3MVG9A2kN3Bn17hvgl2BTy4y3zs8mT8R082MwIdpZgXpqbz2MVpLmX10GIULNxZja_ZP0OZ9NctN4x58_lXFi" -d "client_secret=2628136435652732755" --data-urlencode "username=analytic168+de@gmail.com" -d "password=7YX3slHUW0oIU2lJTTUXVBlidTpiSAA11X1B"
{"id":"https://login.salesforce.com/id/00Di0000000ehppEAA/005i0000001byUHAAY","issued_at":"1382326160558","instance_url":"https://na15.salesforce.com","signature":"rJQnEkOu6Wm9h5EJa+ikFYyDp7stfiVL23p1lRRh/xs=","access_token":"00Di0000000ehpp!ARYAQMVLQuhMPoWbu9FsSvJAQMHki8TJmLw_mUvnG3SROWI9DC_470xNKdhN2zgmNkcb7JA0NKUKx2QY_wsrYZ47uCrxAWwH"}
   */

  /* Login with username + password, and return JSON.
   * Use of username + password login is only for dev use.
   * In product, use OAuth. */
  def loginRaw(username: String, password: String) = {
    val params = Map(
      "grant_type" -> Seq("password"),
      "client_id" -> Seq(oauthConsumerKey),
      "client_secret" -> Seq(oauthConsumerSecret),
      "username" -> Seq(username),
      "password" -> Seq(password)
    )
    val response = WS.url(loginApiUrl).post(params).map { response =>
      //logger.info("login raw response = " + response.body)
      response.json
    }
    response
  }

  /* Login with username + password, and return a RestApi wrapper.
  * Use of username + password login is only for dev use.
  * In product, use OAuth. */
  def login(username: String, password: String, apiVersion: String) = {
    loginRaw(username, password).map { response =>
      val instanceUrl = (response \ "instance_url").as[String]
      val accessToken = (response \ "access_token").as[String]
      new RestApi(instanceUrl, accessToken, apiVersion)
    }
  }


}

class RestApi(instanceUrl: String, accessToken: String, apiVersionStr: String) extends RestApiImpl {
  override def apiVersion = apiVersionStr
  override def acquireInstanceUrl = instanceUrl
  override def acquireAccessToken = accessToken

  def isSandbox = {
    // Is this a sandbox instance?
    // http://www.michaelforce.org/recipeView?id=a0Ga000000Ekp65EAB
    val server = new URL(instanceUrl).getHost
    server.startsWith("cs")
  }
}

trait RestApiImpl {
  def apiVersion: String

  def acquireInstanceUrl: String

  def acquireAccessToken: String

  def query(query: String) = {
    authenticatedJsonGet("query", "q" -> query)
  }

  def insert(objectName: String, js: JsValue) = {
    authenticatedJsonPost("sobjects/" + objectName, js)
  }

  // See http://www.salesforce.com/us/developer/docs/api_analytics/index.htm
  // Gets recently run reports by the user
  def analyticsReports = {
    authenticatedJsonGet("analytics/reports")
  }

  def executeReport(reportId: String, includeDetails: Boolean = false) = {
    authenticatedJsonGet("analytics/reports/" + reportId + (if (includeDetails) "?includeDetails=true" else ""))
  }

  def authenticatedJsonGet(resource: String, params: (String, String)*) = {
    authenticatedApiRequest(resource, params: _*).get().map(_.json)
  }

  def authenticatedJsonPost[T](resource: String, body: T)(implicit wrt: Writeable[T], ct: ContentTypeOf[T]) = {
    authenticatedApiRequest(resource).post(body).map(_.json)
  }

  def queryRequest(query: String) = {
    authenticatedApiRequest("query", "q" -> query)
  }

  protected def authenticatedApiRequest(resource: String, params: (String, String)*): WSRequestHolder = {
    authenticatedApiRequest(resource).withQueryString(params: _*)
  }

  protected def authenticatedApiRequest(resource: String): WSRequestHolder = {
    val serviceUrl = s"$acquireInstanceUrl/services/data/v$apiVersion"
    authenticatedRequest(serviceUrl + "/" + resource)
  }

  def authenticatedRequest(url: String) = {
    val httpsUrl = 
      if (url.startsWith("http://")) { url.replaceFirst("http://", "https://") }
      else url
    WS.url(httpsUrl).withHeaders("Authorization" -> ("Bearer " + acquireAccessToken))
  }
}

import com.github.nscala_time.time.Imports._

sealed trait AutoRefreshState
case class AutoRefreshSuccess(oauthResponse: OauthInfo) extends AutoRefreshState
case object RefreshTokenExpired extends AutoRefreshState
/** this indicates an unexpected error while refreshing the user's token, anything other than the refresh token being expired */
case class AutoRefreshError(error: ErrorResponse) extends AutoRefreshState

/**
 * RestApi implementation that automatically refreshes the accessToken if a request returns with an expired session ID error.
 * The result of any auto-refresh is stored as a side effect.  The side effect can be inspected at the convenience of the
 * client code.
 */
class RestOauthApiWithAutoRefresh(oauthConsumerKey: String, oauthConsumerSecret: String, apiVersionStr: String,
                             instanceUrl: String, accessToken: String, refreshToken: String)
  extends RestApi(instanceUrl, accessToken, apiVersionStr) with Logging {
  private[this] val authApi = new RestOauthAuthApi(oauthConsumerKey, oauthConsumerSecret, super.isSandbox)

  var autoRefreshState: Option[AutoRefreshState] = None

  def latestOauthInfo = autoRefreshState.collect({ case AutoRefreshSuccess(oauthInfo) => oauthInfo })

  override def acquireInstanceUrl: String = this.latestOauthInfo.map(_.instanceUrl).getOrElse(instanceUrl)

  override def acquireAccessToken: String = this.latestOauthInfo.map(_.accessToken).getOrElse(accessToken)

  override def authenticatedJsonGet(resource: String, params: (String, String)*) = {
    super.authenticatedJsonGet(resource, params: _*) flatMap { json =>
      logger.info(json.toString)
      val errorCode = (json(0) \ "errorCode").asOpt[String]
      logger.info(errorCode.toString)
      errorCode match {
        case Some("INVALID_SESSION_ID") =>
          // Retry just once
          authApi.refreshToken(refreshToken) flatMap { result =>
            logger.info(result.toString)
            result match {
              case Right(oauthInfo) =>
                this.autoRefreshState = Some(AutoRefreshSuccess(oauthInfo))
                super.authenticatedJsonGet(resource, params: _*)
              case Left(ErrorResponse("expired access/refresh token", "invalid_grant")) =>
                this.autoRefreshState = Some(RefreshTokenExpired)
                Future(json)
              case Left(errorResponse) =>
                this.autoRefreshState = Some(AutoRefreshError(errorResponse))
                Future(json)
            }
          }
        case _ =>
          Future(json)
      }
    }
  }

  def hasNewOauthInfo = latestOauthInfo.isDefined

  def mapNewOauthInfo[T](fn: OauthInfo => T) = latestOauthInfo.map(fn)

  def mapRefreshState[T](fn: Option[AutoRefreshState] => T): T = fn(autoRefreshState)
}

/**
 * This class caches the connection. Thus, it only works when this is a global variable.
 * Useful primarily for the demo
 */
class RestApiWithAutoRelogin(consumerKey: String, consumerSecret: String, apiVersionStr: String, username: String, password: String, isSandbox: Boolean = false) extends RestApiImpl {
  val authApi = new RestOauthAuthApi(consumerKey, consumerSecret, isSandbox)

  private[this] var (accessToken, instanceUrl) = login()
  private[this] var lastLoginDate = DateTime.now

  override def apiVersion = apiVersionStr

  private[this] def login() = {
    Await.result(authApi.loginRaw(username, password)
      .map(r => ((r \ "access_token").as[String], (r \ "instance_url").as[String])),
      new FiniteDuration(1, TimeUnit.MINUTES))
  }

  private[this] def updateIfNecessary() {
    val durationSinceLastLogin = (lastLoginDate to DateTime.now).toDuration
    if (durationSinceLastLogin isLongerThan 1.hour) {
      val (newAccessToken, newInstanceUrl) = login()
      accessToken = newAccessToken
      instanceUrl = newInstanceUrl
      lastLoginDate = DateTime.now
    }
  }

  override def acquireInstanceUrl: String = {
    updateIfNecessary()
    instanceUrl
  }

  override def acquireAccessToken: String = {
    updateIfNecessary()
    accessToken
  }
}

object RestApiWithOrWithoutAutoRefresh {
  def create(consumerKey: String, consumerSecret: String, apiVersionStr: String,
             instanceUrl: String, accessToken: String, refreshToken: Option[String]): RestApi = {
    if (refreshToken.isDefined)
      new sfdc.RestOauthApiWithAutoRefresh(consumerKey, consumerSecret, apiVersionStr,
        instanceUrl, accessToken, refreshToken.get)
    else
      new sfdc.RestApi(instanceUrl, accessToken, apiVersionStr)
  }
}
