package sfdc

import metadata._
import org.joda.time.{DateTime, LocalDate}
import models.sfdc.SfdcDescribeMetadata
import util.Logging
import models.sfdc.DescribeSObject
import scala.Some
import metadata.SimpleContentInfo
import metadata.BooleanInfo
import models.sfdc.Field
import metadata.CategorialInfo
import metadata.ForeignKeyInfo
import util.Cast._

class SfdcMetadata(tables: Seq[SfdcTable]) extends MetadataImpl(tables)

class SfdcTable(
  name: String,
  fromExpr: String,
  displayLabel: String,
  val labelPlural: String,
  columns0: => List[Column],
  childRelationships0: => Seq[ChildRelationship] = Seq(),
  val recordTypes: Seq[models.sfdc.RecordType]
) extends Table(name, fromExpr, displayLabel, Seq(displayLabel, labelPlural), columns0, childRelationships0) {

  def getOwnerColumn = columns.find(SfdcMetadata.isOwnerColumn(_))
}

import scala.collection.mutable

class SfdcMetadataBuilder extends GenericMetadataBuilder[DescribeSObject, SfdcTableBuilder] {
  override val tableBuildersMap = new mutable.HashMap[String, SfdcTableBuilder]

  override def createTableBuilder(sobject: DescribeSObject) = {
    new SfdcTableBuilder(sobject.name, sobject.name, sobject.label, sobject.labelPlural, sobject.recordTypes)
  }

  override def build() = {
    new SfdcMetadata(tableBuildersMap.values.map(_.build()).toSeq)
  }
}

class SfdcTableBuilder(name: String, fromExpr: String, displayLabel: String, labelPlural: String,
                       recordTypes: Seq[models.sfdc.RecordType]) extends TableBuilder(name, fromExpr, displayLabel, Seq(displayLabel, labelPlural)) {
  override val table = new SfdcTable(name, fromExpr, displayLabel, labelPlural, { columns.toList }, { childRelationships.toSeq }, recordTypes)

  override def build() = {
    super.build()
    table
  }
}

/**
 * Methods for converting from SfdcDescribeMetadata to database Metadata
 */
object SfdcMetadata extends Logging {
  private[sfdc] def isOwnerColumn(col: Column) = {
    col.name == "OwnerId" && col.isForeignKey && col.get[ForeignKeyInfo].get.targetTable.name == "User"
  }

  def metadict(user: models.User): MetadictContext = {
    val md = metadata(user)
    metadict(md)
  }

  def metadict(md: Metadata) = {
    val dict1 = MetaDictGenerator.generateDict(md)
    val dict2 = md.asInstanceOfOpt[SfdcMetadata].map(SfdcMetadict.generateDict(_)).getOrElse(Map())
    new MetadictContext(md, dict1 ++ dict2, SfdcNQMetadata.generateNQMetadata(md))
  }

  def metadata(user: models.User): Metadata = {
    SfdcDescribeMetadata.findByUser(user).map(metadata(_, user.additionalSobjects)).getOrElse(Metadata.Empty)
  }

  /**
   * Extract database metadata from SalesforceDescribeMetadata.
   */
  def metadata(sfdcDescribeMetadata: SfdcDescribeMetadata, additionalSobjects: Set[String] = Set()): SfdcMetadata = {
    val mdb = new SfdcMetadataBuilder

    val sobjects = sfdcDescribeMetadata.sobjects

   // We first create all TableBuilders.
    // This is because when we addColumn, we might want to reference an existing table.
    for(sobject <- sobjects) {
      mdb.newTableBuilder(sobject)
    }

    for(sobject <- sobjects) {
      val tb = mdb.getTableBuilder(sobject.name)
      for(field <- sobject.fields
          if !shouldSkipField(sobject, field)) {
        // TODO: field.labelPlural is not used
        // TODO: Figure out which columns are isProminent
        // TODO: datatypeStr is unused
        // @todo refactor so that tb.addColumn() is invoked at the end
        // and the match just returns the typeSpecificInfo to use

        if ((sobject.name, field.name) == ("CampaignMember", "CreatedDate")) {
          /**
           * CampaignMember.CreatedDate is presented in Salesforce reports as a date-only field
           * "Member First Associated Date" and is never presented as anything else in the Salesforce UI.
           * So we replicate that here.
           */
          tb.addColumn(field.name, "Member First Associated Date", SimpleContentInfo[LocalDate])
        } else field.typeName match {
          case "id" => tb.addColumn(field.name, field.label, PrimaryKeyColumn)
          case "_boolean" =>
            val falseLabel =
              if (field.name == "IsWon") {
                "lost"
              } else if (field.name == "IsClosed") {
                "open"
              } else {
                "not " + field.label
              }
            tb.addColumn(field.name, field.label, BooleanInfo[Boolean] (true, false, Map(true -> field.label, false -> falseLabel), Map()))
          case "string" =>
            if (!field.isNameField) {
              tb.addColumn(field.name, field.label, SimpleContentInfo[String])
            } else {
              tb.addNameColumn(field.name, field.label, SimpleContentInfo[String])
            }
          case "reference" => field.referenceTo.length match {
            case 0 => logger.error(s"reference field ${field.name} must have referenceTo values")
            // Foreign key to the table named by referenceTo[0]
            case 1 => {
              val refTable = field.referenceTo(0)
              if (mdb.hasTable(refTable)) {
                // If your foreign key has a label like "Created By ID" then create an aka for "Created By"
                val aka = stripSuffixIgnoreCase(field.label, " id") // "User ID" => "User"
                tb.addColumn(field.name, ForeignKeyInfo(mdb(refTable), field.relationshipName), field.label, aka.toList: _*)
              } else {
                // @todo handle all tables
              }
            }
            case _ => // @todo handle polymorphic references
          }
          case "datetime" =>
            var label = field.label
            if ((sobject.name, field.name) == ("CampaignMember", "CreatedDate")) {
              label = "Member First Associated Date"
            }
            tb.addColumn(field.name, label, SimpleContentInfo[DateTime])
          case "picklist" =>
            if (!field.name.startsWith("ForecastCategory")) {
              tb.addColumn(field.name, field.label, new CategorialInfo[String](
                field.picklistEntries map {p => (p.value, p.labelForMetadata)} toMap
              ))
            }
          case "phone" => tb.addColumn(field.name, field.label, SimpleContentInfo[String])
          case "_double" => tb.addColumn(field.name, field.label, SimpleContentInfo[BigDecimal])
          case "textarea" => tb.addColumn(field.name, field.label, SimpleContentInfo[String])
          case "date" => {
            if (field.name == "ActivityDate") {
              val aka = List("Due Date")
              tb.addColumn(field.name, SimpleContentInfo[LocalDate], field.label, aka: _*)    // "due date" is a synonym of "due date only"
            } else {
              tb.addColumn(field.name, field.label, SimpleContentInfo[LocalDate])
            }
          }
          case "url" => tb.addColumn(field.name, field.label, SimpleContentInfo[String])
          case "email" => tb.addColumn(field.name, field.label, SimpleContentInfo[String])
          case "currency" => tb.addColumn(field.name, field.label, CurrencyColumn)
          case "_int" => tb.addColumn(field.name, field.label, SimpleContentInfo[Int])
          case "percent" => tb.addColumn(field.name, field.label, SimpleContentInfo[BigDecimal])
          case "multipicklist" => // @todo
          case "combobox" =>
            /*
             * From http://www.salesforce.com/us/developer/docs/api/Content/field_types.htm#i1435537
             * "A combobox is a picklist that also allows users to type a value that is not already specified
             * in the list. A combobox is defined as a string value."
             * @todo do something smart about the picklist values here e.g. treat them as adjective filters like
             * we do for other picklists
             */
            if (!field.isNameField) {
              tb.addColumn(field.name, field.label, SimpleContentInfo[String])
            } else {
              // This is for Task and Event
              tb.addNameColumn(field.name, field.label, SimpleContentInfo[String])
            }
          case "location" => //@todo
          case "base64" => // @todo
          case "encryptedstring" => //@todo
          case "anyType" =>
            /** Occurs for things like ContactHistory.OldValue / NewValue */
            tb.addColumn(field.name, field.label, SimpleContentInfo[String])
          case _ => logger.error(s"Unknown typeName ${field.typeName} (field = ${field.name})")
        }
      }

    }

    for(sobject <- sobjects) {
      val tb = mdb.getTableBuilder(sobject.name)
      for(childRelationship <- sobject.childRelationships) {
        if (mdb.hasTable(childRelationship.childSObjectName)) {
          val childTableBuilder = mdb.getTableBuilder(childRelationship.childSObjectName)
          childTableBuilder.getColumn(childRelationship.childFieldName) map { column =>
            tb.addChildRelationship(childRelationship.relationshipName, childTableBuilder.table, column)
          }
        }
      }
    }

    val metadata = mdb.build()
//    logger.debug(metadata.toString)
    metadata
  }


  private[this] def stripSuffixIgnoreCase(str: String, suffix: String) = {
    if (str.toLowerCase endsWith suffix.toLowerCase) {
      Some(str.substring(0, str.length - suffix.length))
    } else {
      None
    }
  }

  /**
   * Specifies fields that should be elided from our metadata for one reason or another
   */
  private[this] def shouldSkipField(sobject: DescribeSObject, field: Field): Boolean = {
    /**
     * TimeZoneSidKey and LocaleSidKey fields are being skipped because they have hundreds of picklist entries, each of
     * which gets mapped into our dictionary
     *
     * User.UserPermissions* and User.Preferences* fields are being skipped because there's about a hundred of them and each
     * one produces more than a few dict entries
     */
    (sobject.name, field.name) match {
      case ("User", "LocaleSidKey") | ("User", "TimeZoneSidKey") | ("User", "LanguageLocaleKey") => true
      case ("User", fieldName) if fieldName startsWith "UserPermissions" => true
      case ("User", fieldName) if fieldName startsWith "UserPreferences" => true
      case ("Task" | "Event", "RecurrenceTimeZoneSidKey") => true
      case _ => false
    }
  }
}
