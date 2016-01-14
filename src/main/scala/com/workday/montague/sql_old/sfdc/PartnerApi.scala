/**
 * Code for Salesforce Partner API, typically used for extracting metadata.
 */
package sfdc

import com.sforce.soap.partner.{Connector, PartnerConnection}
import com.sforce.ws.ConnectorConfig

object PartnerApi {
  def partnerConnection(authInfo: AuthInfo, organizationId: String, apiVersion: String): PartnerConnection = {
    var sessionId = authInfo.accessToken
    val serverUrl = makeServerUrl(authInfo.instanceUrl, organizationId, apiVersion)
    partnerConnection(sessionId, serverUrl)
  }

// Create a connection to the Partner API.
  def partnerConnection(sessionId: String, serverUrl: String): PartnerConnection = {
    // Create new WSC Partner Connection
    // http://www.tgerm.com/2010/08/salesforce-wsc-apacheaxis-session.html
    val config = new ConnectorConfig
    config.setManualLogin(true)
    config.setServiceEndpoint(serverUrl)
    config.setSessionId(sessionId)
    Connector.newConnection(config)
}

  /**
   * The partner API requires a serverURL that looks something like:
   *  "https://na15.salesforce.com/services/Soap/u/29.0/00Di0000000ehpp"
   * TODO: This is brittle and may break if the serverUrl format changes.
   */
  private[this] def makeServerUrl(instanceUrl: String, organizationId: String, apiVersion: String): String = {
    val serverUrl = s"$instanceUrl/services/Soap/u/$apiVersion/$organizationId"
    serverUrl
  }
}
