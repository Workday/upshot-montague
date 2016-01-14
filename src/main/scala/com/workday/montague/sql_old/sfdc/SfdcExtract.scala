package sfdc

import play.api.Play.current
import play.api.libs.concurrent.Execution.Implicits._
import models._
import models.sfdc.SfdcDescribeMetadata
import com.sforce.soap.partner.PartnerConnection
import org.joda.time.DateTime
import com.mongodb.casbah.commons.MongoDBObject
import controllers.QueryApp
import org.bson.types.ObjectId
import play.api.libs.concurrent.Akka
import scala.collection.mutable
import scala.Some
import util.Logging

/**
 * Code for extracting various things from Sfdc, including metadata and org info.
 */
object SfdcExtract extends Logging {

  // Navigate this customer's metadata.
  // We scrape the tables (SObjects) specified.
  // TODO: Scrape *all* the metadata
  //    You might be able to, by calling 'describe' against REST API.
  //    The only problem is that JSON format might change without notice.
  private[this] def extractMetadata(authInfo: AuthInfo, apiVersion: String, user: User, metadataId: ObjectId): SfdcDescribeMetadata = {
    val conn = PartnerApi.partnerConnection(authInfo, user.sfdcUser.organizationId, apiVersion)
    extractMetadata(conn, user.id, metadataId)
  }

  def extractMetadata(conn: PartnerConnection, userId: ObjectId, metadataId: ObjectId): SfdcDescribeMetadata = {
    val userInfo = conn.getUserInfo
    val allSObjects = getAllDesiredSObjects(conn)
    val describeSObjects = extractSObjects(conn, allSObjects)

    // Find the createdAt for the existing metadata, or just use now
    val oldMetadataOpt = models.sfdc.SfdcDescribeMetadata.findOne(MongoDBObject("_id" -> metadataId))
    val createdAt = oldMetadataOpt.flatMap(_.createdAt).getOrElse(DateTime.now)

    val sfdcDescribeMetadata = models.sfdc.SfdcDescribeMetadata(id=metadataId, createdAt=Some(createdAt),
      updatedAt=Some(DateTime.now), createdByUserId=userId, sfdcOrganizationId=userInfo.getOrganizationId,
      sfdcUserId=userInfo.getUserId, sobjects=describeSObjects)
    sfdcDescribeMetadata
  }

  /**
   * Extract and save a User's Metadata.
   */
  def saveMetadata(authInfo: AuthInfo, apiVersion: String, user: User, metadataId: ObjectId) = {
    val metadata = extractMetadata(authInfo, apiVersion, user, metadataId)
    SfdcDescribeMetadata.save(metadata)
    metadata
  }

  /**
   * Extract the Organization information for this User.
   */
  private[this] def extractOrganization(user: User): (Organization, OrganizationKnownUsers) = {
    val displayLocation = user.bestDisplayLocation
    val restApi = user.restApi(displayLocation)

    def stringOrNone(v: Any): Option[String] = {
      v match {
        case null => None
        case _ => {
          val s = v.asInstanceOf[String].trim
          assert (s != "")
          Some(s)
        }
      }
    }

    // @todo This is blocking. Make it asynchronous?
    // @todo What happens if there are too many results? And we need to paginate?db
    val knownUsers = QueryApp.executeSoql(restApi, "select Id, FirstName, LastName, Name, Email from User") match {
      case Left(errorMessage) => {
        logger.error("Could not extract User table: " + errorMessage)
        Nil
      }
      case Right(rows) => {
        if (rows.isEmpty) logger.error("User table is empty! " + user.id.toString)
        rows.map(row => {
          val sfdcUserId = row("Id").asInstanceOf[String]
          val firstName = stringOrNone(row("FirstName"))
          val lastName = stringOrNone(row("LastName")).get
          val name = stringOrNone(row("Name")).get
          val email = stringOrNone(row("Email"))
          SfdcKnownUser(userId=sfdcUserId, firstName=firstName, lastName=lastName, name=name, email=email)
        })
      }
    }

    // @todo Don't duplicate so much code with the above. There must be a cleaner way to do this
    val sfdcOrganizationOpt = (QueryApp.executeSoql(restApi, "select Id, Division, FiscalYearStartMonth, Name, " +
      "OrganizationType, PrimaryContact, UsesStartDateAsFiscalYearName from Organization") match {
      case Left(errorMessage) => {
        logger.error("Could not extract Organization table: " + errorMessage)
        Nil
      }
      case Right(rows) => {
        if (rows.isEmpty) logger.error("Organization table is empty! " + user.id.toString)
        rows.map(row => {
          val organizationId = row("Id").asInstanceOf[String]
          val division = stringOrNone(row("Division"))
          val fiscalYearStartMonth = Option(row("FiscalYearStartMonth").asInstanceOf[scala.math.BigDecimal])
          val name = stringOrNone(row("Name")).get
          val organizationType = stringOrNone(row("OrganizationType"))
          val primaryContact = stringOrNone(row("PrimaryContact"))

          SfdcOrganization(organizationId=organizationId, division=division, fiscalYearStartMonth=fiscalYearStartMonth, name=name,
            organizationType=organizationType, primaryContact=primaryContact)
        })
      }
    }).toSeq

    assert(sfdcOrganizationOpt.length == 1)
    val sfdcOrganization = sfdcOrganizationOpt.head

    // Find the existing ids for the Organization and OrganizationKnownUsers, or create new ones
    // @todo It would be faster if we just pass these ids in, instead of doing a second lookup within this method
    // @note There is a potential race condition here (e.g. multiple users from one org sign up at once, so keep
    // this as close to the object saves as possible)
    val oldOrganization = Organization.findByUser(user)
    val organizationId = oldOrganization.map(_.id).getOrElse(new ObjectId)
    val organizationKnownUsersId = oldOrganization.map(_.organizationKnownUsers.id).getOrElse(new ObjectId)

    val organizationCreatedAt = oldOrganization.map(_.createdAt).getOrElse(DateTime.now)
    val organizationKnownUsersCreatedAt = oldOrganization.map(_.organizationKnownUsers.createdAt).getOrElse(DateTime.now)


    // @todo What to do if knownUsers is None?
    val organizationKnownUsers = OrganizationKnownUsers(id=organizationKnownUsersId,
      createdAt=organizationKnownUsersCreatedAt, updatedAt=DateTime.now, sfdcKnownUsers=knownUsers.toSeq, organizationId=organizationId)

    val sfdcOrganizationId = user.sfdcUser.organizationId
    // @todo Assert it's the same as if we query the organization table?

    val organization = models.Organization(id=organizationId, createdAt=organizationCreatedAt,
      updatedAt=DateTime.now, sfdcOrganization=sfdcOrganization)
    (organization, organizationKnownUsers)
  }

  /**
   * Extract and save user Organization.
   */
  def saveOrganizationAsync(user: User) = {
    import scala.concurrent.duration._
    Akka.system.scheduler.scheduleOnce(new FiniteDuration(0, SECONDS)) {
      val (organization, organizationKnownUsers) = extractOrganization(user)
      Organization.save(organization)
      OrganizationKnownUsers.save(organizationKnownUsers)
      // Note that user is constant, so this variable won't be updated, just the database
      User.setOrganizationId(user, organization.id)
      (organization, organizationKnownUsers)
    }
  }

  /**
   * This function determines which objects we extract metadata for.  We extract metadata for only a few objects
   * because it adds a noticeable and significant amount of time to try to extract metadata for many objects.
   *
   * @todo In the future we should make the metadata extraction asynchronous, in which case it won't matter how
   *       long it takes to extract the metadata, and we can extract everything, which is what we want.
   */
  private[this] def getAllDesiredSObjects(conn: PartnerConnection) = {
    val describeGlobal = conn.describeGlobal()
    /**
     * The isLayoutable check basically restricts us to a handful of top-level objects.  That's too restrictive.  For
     * now, any additional objects we may want we just explicitly add.
     *
     * Filter out custom objects because some people can have hundreds of custom objects, again causing a slowdown.
     */
    val allSObjects = describeGlobal.getSobjects.filter(sobject =>
      sobject.isLayoutable || Set("AccountTeamMember", "OpportunityContactRole", "ContactHistory", "OpportunityLineItem", "Product2", "PricebookEntry").contains(sobject.getName)
    ).map(_.getName)
    allSObjects
  }

  private[this] def extractSObjects(conn: PartnerConnection, SObjectsToScrape: Seq[String]) = {
    val recordTypeDeveloperNames = getRecordTypeDeveloperNames(conn)
    var describeSObjects = new mutable.ListBuffer[models.sfdc.DescribeSObject]
    // 99 otherwise you hit EXCEEDED_MAX_TYPES_LIMIT exception
    // https://www.salesforce.com/us/developer/docs/api/Content/sforce_api_calls_describesobjects.htm
    // http://www.salesforce.com/us/developer/docs/api/Content/sforce_api_calls_concepts_core_data_objects.htm
    val describeResults = SObjectsToScrape.grouped(99).flatMap(group => conn.describeSObjects(group.toArray))
    for(describeResult <- describeResults) {
      var metadataFields = new mutable.ListBuffer[models.sfdc.Field]
      val describeFields = describeResult.getFields
      val childRelationships = new mutable.ListBuffer[models.sfdc.ChildRelationship]
      for(describeField <- describeFields) {
        var metadataPicklistEntries = new mutable.ListBuffer[models.sfdc.PicklistEntry]
        val picklistEntries = describeField.getPicklistValues
        for(picklistEntry <- picklistEntries) {
          metadataPicklistEntries += models.sfdc.PicklistEntry(picklistEntry.getValue, Option(picklistEntry.getLabel), picklistEntry.isDefaultValue, picklistEntry.isActive)
        }
        metadataFields += models.sfdc.Field(describeField.getName, describeField.getType.name, describeField.getLabel, describeField.getReferenceTo.toList, Option(describeField.getRelationshipName), describeField.isNameField, metadataPicklistEntries.toList)
      }
      for(childRelationship <- describeResult.getChildRelationships) {
        if (childRelationship.getRelationshipName != null) {
          childRelationships += models.sfdc.ChildRelationship(childRelationship.getRelationshipName, childRelationship.getChildSObject, childRelationship.getField, childRelationship.getDeprecatedAndHidden)
        }
      }
      val recordTypes = describeResult.getRecordTypeInfos flatMap { recordTypeInfo =>
        val label = recordTypeInfo.getName
        val recordTypeId = recordTypeInfo.getRecordTypeId
        // These aren't useful now but they might be useful in the future:
        //val isAvailable = recordTypeInfo.isAvailable
        //val isDefault = recordTypeInfo.isDefaultRecordTypeMapping

        // Annoyingly, you can't get the DeveloperName from RecordTypeInfo, which is the one thing you really want
        // so we get it from our recordTypeDeveloperNames map
        recordTypeDeveloperNames.get(recordTypeId) map { developerName =>
          models.sfdc.RecordType(recordTypeId, developerName, label)
        }
      }
      describeSObjects += models.sfdc.DescribeSObject(describeResult.getName, describeResult.getLabel, describeResult.getLabelPlural, metadataFields.toSeq, childRelationships.toSeq, recordTypes)
    }
    describeSObjects.toSeq
  }

  /**
   * @todo probably we should rewrite this whole extraction to use the REST API
   *       then we can store raw JSON, the downside is that it requires one HTTP call per sobject
   *       though maybe the connection is persistent?
   * @return map from recordTypeId -> DeveloperName
   */
  private[this] def getRecordTypeDeveloperNames(conn: PartnerConnection): Map[String, String] = {
    try {
      val qr = conn.query("select id, developername, sobjecttype from recordtype")
      val list = for (record <- qr.getRecords) yield {
        (record.getId, record.getField("DeveloperName").asInstanceOf[String])
      }
      list.toMap
    } catch {
      case x: Exception =>
        logger.error("Error while fetching record types", x)
        Map()
    }
  }

  def getSfdcUserInfo(restApi: RestApi, idUrl: String) = {
    restApi.authenticatedRequest(idUrl).get().map { response =>
      try {
        val json = response.json
        SfdcUser(
          (json \ "organization_id").as[String], (json \ "user_id").as[String], (json \ "id").as[String],
          (json \ "username").as[String], (json \ "nick_name").as[String], (json \ "display_name").as[String],
          (json \ "email").as[String], (json \ "language").as[String], (json \ "locale").as[String],
          (json \ "timezone").asOpt[String], (json \ "utcOffset").as[Long]
        )
      } catch {
        case e: Exception =>
          logger.error("Error " + e.getMessage() + " for " + idUrl + " response deserializing JSON to SfdcUser. Response body: " + response.body.toString)
          throw e
      }
    }
  }
}