package com.kismatka.mi

import scalaj.http.{HttpOptions, Http}
import scala.xml.{Node, XML}
import com.kismatka.mi.model.Article
import org.joda.time.DateTime
import scala.collection.mutable

object PubMedLoader {
  private val PubMedUrl = "http://www.ncbi.nlm.nih.gov/pubmed"
  private var cookie: String = _


  def load(query: String, numberOfRecords: Int) = {
    initCookie(query)
    val parts = for (start <- (1 to numberOfRecords by 200).par) yield {
      val part = getPartAsString(query, start)
      val content = "<PubmedArticles>" + cleanXml(part) + "</PubmedArticles>"
      ((XML.loadString(content) \\ "MedlineCitation") map processArticle).toIndexedSeq
    }
    parts.seq.flatten.take(numberOfRecords)
  }

  def initCookie(query: String) {
    val term = Http.urlEncode(query)
    val request = Http(s"http://www.ncbi.nlm.nih.gov/pubmed/?term=$term")
      .headers("User-Agent" -> "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/31.0.1650.63 Safari/537.36",
        "Host" -> "www.ncbi.nlm.nih.gov")
      .options(HttpOptions.connTimeout(10000), HttpOptions.readTimeout(10000))
    val responseHeaders = request.asCodeHeaders._2
    cookie = responseHeaders.get("Set-Cookie").map(_.map(_.split(';')(0)).mkString(";")).getOrElse(throw new Exception("Could not find cookie"))
  }

  def cleanXml(xml: String): String = {
    xml.replace("\n", "").replace("&gt;", ">").replace("&lt;", "<").replace("<pre>", "").replace("</pre>", "")
      .replace("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">", "")
      .replace("<?xml version=\"1.0\" encoding=\"utf-8\"?>", "")
  }

  def getPartAsString(query: String, start: Int) = {
    val term = Http.urlEncode(query)
    val page = start / 200 + 1
    val postdata = s"term=$term&EntrezSystem2.PEntrez.PubMed.Pubmed_PageController.PreviousPageName=results&EntrezSystem2.PEntrez.PubMed.Pubmed_Facets.FacetsUrlFrag=filters%3D&EntrezSystem2.PEntrez.PubMed.Pubmed_Facets.FacetSubmitted=false&EntrezSystem2.PEntrez.PubMed.Pubmed_Facets.BMFacets=&EntrezSystem2.PEntrez.PubMed.Pubmed_ResultsPanel.Pubmed_DisplayBar.sPresentation=xml&EntrezSystem2.PEntrez.PubMed.Pubmed_ResultsPanel.Pubmed_DisplayBar.sPageSize=200&EntrezSystem2.PEntrez.PubMed.Pubmed_ResultsPanel.Pubmed_DisplayBar.sSort=none&EntrezSystem2.PEntrez.PubMed.Pubmed_ResultsPanel.Pubmed_DisplayBar.FFormat=docsum&EntrezSystem2.PEntrez.PubMed.Pubmed_ResultsPanel.Pubmed_DisplayBar.FSort=&email_format=docsum&email_sort=&email_count=200&email_start=$start&email_address=&email_subj=$term+-+PubMed&email_add_text=&citman_count=200&citman_start=$start&EntrezSystem2.PEntrez.PubMed.Pubmed_ResultsPanel.Pubmed_DisplayBar.FileFormat=docsum&EntrezSystem2.PEntrez.PubMed.Pubmed_ResultsPanel.Pubmed_DisplayBar.LastPresentation=docsum&EntrezSystem2.PEntrez.PubMed.Pubmed_ResultsPanel.Pubmed_DisplayBar.Presentation=xml&EntrezSystem2.PEntrez.PubMed.Pubmed_ResultsPanel.Pubmed_DisplayBar.PageSize=200&EntrezSystem2.PEntrez.PubMed.Pubmed_ResultsPanel.Pubmed_DisplayBar.LastPageSize=200&EntrezSystem2.PEntrez.PubMed.Pubmed_ResultsPanel.Pubmed_DisplayBar.Sort=&EntrezSystem2.PEntrez.PubMed.Pubmed_ResultsPanel.Pubmed_DisplayBar.LastSort=&EntrezSystem2.PEntrez.PubMed.Pubmed_ResultsPanel.Pubmed_DisplayBar.FileSort=&EntrezSystem2.PEntrez.PubMed.Pubmed_ResultsPanel.Pubmed_DisplayBar.Format=text&EntrezSystem2.PEntrez.PubMed.Pubmed_ResultsPanel.Pubmed_DisplayBar.LastFormat=&CitationManagerStartIndex=1&CitationManagerCustomRange=false&EntrezSystem2.PEntrez.PubMed.Pubmed_ResultsPanel.Entrez_Pager.cPage=$page&EntrezSystem2.PEntrez.PubMed.Pubmed_ResultsPanel.Entrez_Pager.CurrPage=$page&EntrezSystem2.PEntrez.PubMed.Pubmed_ResultsPanel.Pubmed_ResultsController.ResultCount=17687&EntrezSystem2.PEntrez.PubMed.Pubmed_ResultsPanel.Pubmed_ResultsController.RunLastQuery=&EntrezSystem2.PEntrez.PubMed.Pubmed_ResultsPanel.Entrez_Pager.cPage=$page&EntrezSystem2.PEntrez.PubMed.Pubmed_ResultsPanel.Pubmed_DisplayBar.sPresentation2=docsum&EntrezSystem2.PEntrez.PubMed.Pubmed_ResultsPanel.Pubmed_DisplayBar.sPageSize2=200&EntrezSystem2.PEntrez.PubMed.Pubmed_ResultsPanel.Pubmed_DisplayBar.sSort2=none&EntrezSystem2.PEntrez.PubMed.Pubmed_ResultsPanel.Pubmed_DisplayBar.FFormat2=docsum&EntrezSystem2.PEntrez.PubMed.Pubmed_ResultsPanel.Pubmed_DisplayBar.FSort2=&email_format2=docsum&email_sort2=&email_count2=200&email_start2=$start&email_address2=&email_subj2=$term+-+PubMed&email_add_text2=&citman_count2=200&citman_start2=$start&CurrTimelineYear=&EntrezSystem2.PEntrez.PubMed.Pubmed_ResultsPanel.EmailTab.EmailReport=&EntrezSystem2.PEntrez.PubMed.Pubmed_ResultsPanel.EmailTab.EmailFormat=&EntrezSystem2.PEntrez.PubMed.Pubmed_ResultsPanel.EmailTab.EmailCount=&EntrezSystem2.PEntrez.PubMed.Pubmed_ResultsPanel.EmailTab.EmailStart=&EntrezSystem2.PEntrez.PubMed.Pubmed_ResultsPanel.EmailTab.EmailSort=&EntrezSystem2.PEntrez.PubMed.Pubmed_ResultsPanel.EmailTab.Email=&EntrezSystem2.PEntrez.PubMed.Pubmed_ResultsPanel.EmailTab.EmailSubject=&EntrezSystem2.PEntrez.PubMed.Pubmed_ResultsPanel.EmailTab.EmailText=&EntrezSystem2.PEntrez.PubMed.Pubmed_ResultsPanel.EmailTab.EmailQueryKey=&EntrezSystem2.PEntrez.PubMed.Pubmed_ResultsPanel.EmailTab.QueryDescription=&EntrezSystem2.PEntrez.PubMed.Pubmed_ResultsPanel.EmailTab.Key=&EntrezSystem2.PEntrez.PubMed.Pubmed_ResultsPanel.EmailTab.Answer=&EntrezSystem2.PEntrez.PubMed.Pubmed_ResultsPanel.TimelineAdPlaceHolder.CurrTimelineYear=&EntrezSystem2.PEntrez.DbConnector.Db=pubmed&EntrezSystem2.PEntrez.DbConnector.LastDb=pubmed&EntrezSystem2.PEntrez.DbConnector.Term=$term&EntrezSystem2.PEntrez.DbConnector.LastTabCmd=&EntrezSystem2.PEntrez.DbConnector.LastQueryKey=1&EntrezSystem2.PEntrez.DbConnector.IdsFromResult=&EntrezSystem2.PEntrez.DbConnector.LastIdsFromResult=&EntrezSystem2.PEntrez.DbConnector.LinkName=&EntrezSystem2.PEntrez.DbConnector.LinkReadableName=&EntrezSystem2.PEntrez.DbConnector.LinkSrcDb=&EntrezSystem2.PEntrez.DbConnector.Cmd=displaychanged&EntrezSystem2.PEntrez.DbConnector.TabCmd=&EntrezSystem2.PEntrez.DbConnector.QueryKey=&p%24a=EntrezSystem2.PEntrez.PubMed.Pubmed_ResultsPanel.Pubmed_DisplayBar.SetDisplay&p%24l=EntrezSystem2&p%24st=pubmed"
    Http.postData(PubMedUrl, postdata)
      .headers("User-Agent" -> "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/31.0.1650.63 Safari/537.36",
        "Origin" -> "http://www.ncbi.nlm.nih.gov",
        "Host" -> "www.ncbi.nlm.nih.gov",
        "Referer" -> "http://www.ncbi.nlm.nih.gov/pubmed",
        "Cookie" -> cookie
          //"prevsearch=; clicknext=link_name%3DEntrezSystem2.PEntrez.PubMed.Pubmed_ResultsPanel.Pubmed_DisplayBar.SetDisplay%26link_text%3DApply%26link_class%3Dbutton_apply%252Cncbipopper-close-button%26browserwidth%3D1440%26browserheight%3D738%26evt_coor_x%3D789%26evt_coor_y%3D285%26jseventms%3D5q6rw0%26iscontextmenu%3Dfalse%26jsevent%3Dclicknext%26ancestorId%3Ddisplay_settings_menu%2Cmaincontent%2CEntrezForm%26ancestorClassName%3Ddisp_settings%2CtabPopper%2Cui-helper-reset%2Cui-ncbipopper-wrapper%2Cui-ncbipopper-basic%2Cresults_settings%2Ccontent%2Ccol%2Cseven_col%26maxScroll_x%3D0%26maxScroll_y%3D0%26currScroll_x%3D0%26currScroll_y%3D0%26hasScrolled%3Dfalse%26ncbi_phid%3D396D29EF2B22B9A100000000001888F0; ncbi_prevPHID=396D29EF2B22B9A100000000001888F0; unloadnext=jsevent%3Dunloadnext%26ncbi_pingaction%3Dunload%26ncbi_timeonpage%3D6032%26ncbi_onloadTime%3D702%26jsperf_dns%3D0%26jsperf_connect%3D0%26jsperf_ttfb%3D817%26jsperf_basePage%3D396%26jsperf_frontEnd%3D335%26jsperf_navType%3D0%26jsperf_redirectCount%3D0%26maxScroll_x%3D0%26maxScroll_y%3D0%26currScroll_x%3D0%26currScroll_y%3D0%26hasScrolled%3Dfalse%26ncbi_phid%3D396D29EF2B22B9A100000000001888F0; ncbi_sid=CE8D4E2E28E92A01_0061SID; WebEnv=10us9kf7yvrzue_QvQggrtleNkuRvFb_kOGT25_1pnqyGtuIn8TC8uK_g_agBUAG3T--noNO70pYoP_-WPhvbDRNeljFA_H8AOXYwX%40CE8D4E2E28E92A01_0061SID"
      )
      .options(HttpOptions.connTimeout(10000), HttpOptions.readTimeout(10000)).asString
  }

  def processArticle(node: Node) = {
    val dateCreatedNode = (node \ "DateCreated")(0)
    val dateCompletedNode = if((node \ "DateCompleted" size) > 0) Some((node \ "DateCompleted")(0)) else None

    val articleNode: Node = (node \ "Article")(0)
    val authors = (articleNode \\ "Author") map processAuthor
    val journal = processJournal((articleNode \ "Journal")(0))
    val title = (node \\ "ArticleTitle")(0).text

    Article(
      title = title,
      dateCreated = processDate(dateCreatedNode),
      dateCompleted = dateCompletedNode map processDate,
      authors = authors,
      journal = journal,
      pubTypes = processPubTypes(node),
      keywords = processKeywords(node)
    )
  }

  def processPubTypes(node: Node): Seq[String] = {
    (node \\ "PublicationType").map(_.text)
  }

  def processKeywords(node: Node): Seq[String] = {
    node \\ "DescriptorName" map (_.text)
  }

  def processJournal(node: Node): String = (node \ "Title").text

  def processDate(node: Node) =
    new DateTime((node \ "Year" text) toInt, (node \ "Month" text) toInt, (node \ "Day" text) toInt, 0, 0)

  def processAuthor(node: Node): String = {
    var name = mutable.ListBuffer[String]()
    if ((node \ "CollectiveName").size > 0)
      name += (node \ "CollectiveName")(0).text
    if((node \ "ForeName").size > 0)
      name += (node \ "ForeName")(0).text
    if((node \ "LastName").size > 0)
      name += (node \ "LastName")(0).text
    name.mkString(" ")
  }
}
