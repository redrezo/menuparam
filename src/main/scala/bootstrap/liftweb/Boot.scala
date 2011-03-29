package bootstrap.liftweb

import _root_.net.liftweb.common._
import _root_.net.liftweb.util._
import _root_.net.liftweb.http._
import _root_.net.liftweb.sitemap._
import _root_.net.liftweb.sitemap.Loc._
import Helpers._

/**
  * A class that's instantiated early and run.  It allows the application
  * to modify lift's environment
  */
class Boot {
  def boot {
    // where to search snippet
    LiftRules.addToPackages("liftissue.menuparam")

    // Build SiteMap
    val entries = Menu(Loc("Home", List("index"), "Home")) :: Nil ::: SitemapHelper.menu
    LiftRules.setSiteMap(SiteMap(entries:_*))
  }
}

case class Page(id: String, feats: List[String])


import scala.xml.{Text,NodeSeq}

object SitemapHelper {
	def pageEncoder(p: Page) = p.id
	def pageParser(pid: String) = Full(Page(pid, "a"::"b"::"c"::Nil))
	val pageText = new LinkText[Page]( p => Text("(A) Page " + p.id) )
	
	val apage = Menu.param[Page]("pid", pageText, pageParser _, pageEncoder _) / "A" / * >> 
		Template( () => TemplateFinder.findAnyTemplate( "index" :: Nil ) openOr NodeSeq.Empty ) >>
		LocGroup( "page" )
	
	
	val afa = Menu.param[Page]("pid-fa", "(A) feature a", pageParser _, pageEncoder _) / "A" / * / "a" >> 
		Template( () => TemplateFinder.findAnyTemplate( "index" :: Nil ) openOr NodeSeq.Empty ) >>
		LocGroup( "page" ) >>
		CalcValue( () => apage.currentValue )
		
	val afb = Menu.param[Page]("pid-fb", "(A) feature b", pageParser _, pageEncoder _) / "A" / * / "b" >> 
		Template( () => TemplateFinder.findAnyTemplate( "index" :: Nil ) openOr NodeSeq.Empty ) >>
		LocGroup( "page" ) >>
		CalcValue( () => apage.currentValue )
		
	val afc = Menu.param[Page]("pid-fc", "(A) feature c", pageParser _, pageEncoder _) / "A" / * / "c" >> 
		Template( () => TemplateFinder.findAnyTemplate( "index" :: Nil ) openOr NodeSeq.Empty ) >>
		LocGroup( "page" ) >>
		CalcValue( () => apage.currentValue )
		
	val bfacalc: () => Box[Page] = () => {
		List( bpage, bfb, bfc ).map( m => m.currentValue ).flatten match {
			case c :: tail => Full(c)
			case _ => Empty
		}
	}
	
	val bfbcalcLoop: () => Box[Page] = () => {
		List( bpage, bfa, bfc ).map( m => m.currentValue ).flatten match {
			case c :: tail => Full(c)
			case _ => Empty
		}
	}
	
	val bfbcalc = () => bpage.currentValue
//	val bfbcalc = bfbcalcLoop
	
	val bpageText = new LinkText[Page]( p => Text("(B) Page " + p.id) )
	val bpage = Menu.param[Page]("bpid", bpageText, pageParser _, pageEncoder _) / "B" / * >> 
		Template( () => TemplateFinder.findAnyTemplate( "index" :: Nil ) openOr NodeSeq.Empty ) >>
	 	LocGroup( "page" )
	
	
	
	val bfa = Menu.param[Page]("bpid-fa", "(B) feature a", pageParser _, pageEncoder _) / "B" / * / "a" >> 
		Template( () => TemplateFinder.findAnyTemplate( "index" :: Nil ) openOr NodeSeq.Empty ) >>
		LocGroup( "page" ) >>
		CalcValue( bfacalc )
		
	val bfb = Menu.param[Page]("bpid-fb", "(B) feature b", pageParser _, pageEncoder _) / "B" / * / "b" >> 
		Template( () => TemplateFinder.findAnyTemplate( "index" :: Nil ) openOr NodeSeq.Empty ) >>
		LocGroup( "page" ) >>
		CalcValue( bfbcalc )
		
	val bfc = Menu.param[Page]("bpid-fc", "(B) feature c", pageParser _, pageEncoder _) / "B" / * / "c" >> 
		Template( () => TemplateFinder.findAnyTemplate( "index" :: Nil ) openOr NodeSeq.Empty ) >>
		LocGroup( "page" ) >>
		CalcValue( () => bpage.currentValue )
		
	lazy val menu = apage :: afa :: afb :: afc ::
					bpage :: bfa :: bfb :: bfc :: Nil
	
}
