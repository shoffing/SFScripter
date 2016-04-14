package helpers

import java.io.{File, PrintWriter}

import play.Play
import play.api.libs.json._

import scala.util.Random

object Markov {

  case class Node(label: String, var links: Map[String, Link], graph: Map[String, Node]) {
    // Select next linked node randomly by weight
    // (or selects random node in graph if it can't find one)
    def next: Node = {
      val rand = Random.nextFloat
      val weightSum = links.values.map(_.weight).sum

      var acc = 0f
      links.values.find { link =>
        acc += link.weight / weightSum.toFloat
        acc > rand
      } match {
        case Some(link) => link.node
        case None => graph.values.toList(Random.nextInt(graph.values.size))
      }
    }
  }
  case class Link(node: Node, weight: Int)

  // Use this to include symbols as nodes, making language flow more naturally
  def explode(str: String, pattern: Char => Boolean): List[String] = {
    val strSpan = str.span(pattern)
    if (strSpan._2.length > 1) {
      if (strSpan._1.nonEmpty)
        List(strSpan._1, strSpan._2.head.toString) ++ explode(strSpan._2.tail, pattern)
      else
        List(strSpan._2.head.toString) ++ explode(strSpan._2.tail, pattern)
    }
    else if (strSpan._1.nonEmpty && strSpan._2.nonEmpty) List(strSpan._1, strSpan._2)
    else if (strSpan._1.nonEmpty) List(strSpan._1)
    else if (strSpan._2.nonEmpty) List(strSpan._2)
    else Nil
  }

  //

  val wordCharacters = "'$0123456789abcdefghijklmnopqrstuvwxyz"

  val inputText = scala.io.Source.fromFile(Play.application.getFile("resources/corpus_seinfeld.txt"))
    .getLines.mkString("\n")
    .replaceAll("[^\\x00-\\x7F]", "") // remove unicode stuff

  val inputSeq = inputText.split(Array(' ', '\t')).flatMap(w => explode(w, c => wordCharacters.contains(c.toLower)))

  // Generate Markov Chain graph from input
  var graph = Map.empty[String, Node]
  for (wordPair <- inputSeq.sliding(2)) {
    if (!graph.contains(wordPair(0)))
      graph = graph + (wordPair(0) -> Node(wordPair(0), Map.empty[String, Link], graph))
    if (!graph.contains(wordPair(1)))
      graph = graph + (wordPair(1) -> Node(wordPair(1), Map.empty[String, Link], graph))

    if (graph(wordPair(0)).links.contains(wordPair(1)))
      graph(wordPair(0)).links = graph(wordPair(0)).links.updated(wordPair(1), Link(graph(wordPair(1)),
        graph(wordPair(0)).links(wordPair(1)).weight + 1))
    else
      graph(wordPair(0)).links = graph(wordPair(0)).links + (wordPair(1) -> Link(graph(wordPair(1)), 1))
  }


//  val linkCounts = graph.values.filter(_.label.forall(wordCharacters.contains(_))).map(node =>
//    node.label -> node.links.size
//  ).toList
//
//  linkCounts.sortBy(-_._2).foreach(n => println(s"${n._1} -> ${n._2}"))
//
//  val pw = new PrintWriter(new File("/Users/shoffing/seinfeld_node_link_counts.txt"))
//  linkCounts.foreach(n => pw.write(s"${n._1}, ${n._2}"))
//  pw.close()

//  // convert the graph to JSON
//  val graphJSON: JsValue = JsObject(Seq(
//    "nodes" -> JsArray(
//      graph.values.map(node =>
//        JsObject(Seq(
//          "label" -> JsString(node.label),
//          "links" -> JsArray(
//            node.links.values.map(link =>
//              JsObject(Seq(
//                "label" -> JsString(link.node.label),
//                "weight" -> JsNumber(link.weight / node.links.values.map(_.weight).sum.toDouble)
//              ))
//            ).toSeq
//          )
//        ))
//      ).toSeq
//    )
//  ))
//
//  // output the graph JSON to a text file
//  val pw = new PrintWriter(new File("/Users/shoffing/graph_hamlet.json"))
//  pw.write(Json.stringify(graphJSON))
//  pw.close()

  // Initialize the current node as a random node in the graph
  var curNode = graph.values.toList(Random.nextInt(graph.size))

  // Returns the current node's label as a string, then traverses to the next node
  def next: String = {
    var output = curNode.label
    val nextNode = curNode.next
    if (nextNode.label.forall(c => wordCharacters.contains(c.toLower))) output += " "
    curNode = nextNode
    output
  }

}

