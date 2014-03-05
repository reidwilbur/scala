package ch5

import scala.collection.immutable.{Stack, Queue}
import scala.annotation.tailrec

case class Graph(
  edge: IndexedSeq[List[Edge]],
  edgeCount: Int,
  degree: IndexedSeq[Int],
  directed: Boolean) {
  assert(edge.size == degree.size)

  val vertexCount = edge.size
}

case class Edge(vertex: Int, weight: Int)

object Graph {
  import collection.mutable.ArrayBuffer

  def apply(vertices: Int, edges: List[(Int,Int)], directed: Boolean = false): Graph = {
    val edgeBuffer  = ArrayBuffer.fill[List[Edge]](vertices)(List.empty)
    val degree = ArrayBuffer.fill[Int](vertices)(0)
    var edgeCount = 0

    edges.foreach{ (e) =>
      assert(e._1 < vertices)
      assert(e._2 < vertices)

      edgeBuffer(e._1) = Edge(e._2, 0) :: edgeBuffer(e._1)
      degree(e._1) = degree(e._1) + 1
      edgeCount += 1

      if (!directed) {
        edgeBuffer(e._2) = Edge(e._1, 0) :: edgeBuffer(e._2)
        degree(e._2) = degree(e._2) + 1
      }
    }

    Graph(edgeBuffer, edgeCount, degree, directed)
  }

  abstract class VertexState
  case object UnDiscovered extends VertexState
  case object Discovered extends VertexState
  case object Processed extends VertexState

  case class SearchResult(vertexState: IndexedSeq[VertexState], parent: IndexedSeq[Option[Int]])

  def bfs(g: Graph, root: Int)
         (vertexEarly: (Int)  => Unit = {(v) =>},
          vertexLate:  (Int)  => Unit = {(v) =>},
          processEdge: (Int, Edge) => Unit = {(r, e) =>}): SearchResult = {
    _bfs(
      g,
      root,
      ArrayBuffer.fill(g.vertexCount)(UnDiscovered),
      ArrayBuffer.fill(g.vertexCount)(None)
    )(vertexEarly, vertexLate, processEdge)
  }

  private def _bfs(g: Graph, root: Int, vertexState: ArrayBuffer[VertexState], parent: ArrayBuffer[Option[Int]])
         (vertexEarly: (Int) => Unit = {(v) =>},
          vertexLate:  (Int) => Unit = {(v) =>},
          processEdge: (Int, Edge) => Unit = {(r, e) =>}): SearchResult = {
    assert(root >= 0)
    assert(root < g.vertexCount)

    @tailrec
    def traverse(vertexQueue: Queue[Int]): Unit = {
      if (vertexQueue.nonEmpty) {
        val (v, rest) = vertexQueue.dequeue

        vertexEarly(v)
        vertexState(v) = Processed

        val nextQueue = g.edge(v).foldLeft(rest){ (q, e) =>
          if (vertexState(e.vertex) != Processed || g.directed) {
            processEdge(v, e)
          }
          if (vertexState(e.vertex) == UnDiscovered) {
            vertexState(e.vertex) = Discovered
            parent(e.vertex) = Some(v)
            q.enqueue(e.vertex)
          }
          else q
        }

        vertexLate(v)

        traverse(nextQueue)
      }
    }

    traverse(Queue[Int](root))

    SearchResult(vertexState, parent)
  }

  def dfs(g: Graph, root: Int)
         (vertexEarly: (Int) => Unit = {(v) =>},
          vertexLate:  (Int) => Unit = {(v) =>},
          processEdge: (Int, Edge) => Unit = {(v,e) =>}): SearchResult = {
    val vertexState = ArrayBuffer.fill[VertexState](g.vertexCount)(UnDiscovered)
    val parent = ArrayBuffer.fill[Option[Int]](g.vertexCount)(None)
    val entry = ArrayBuffer.fill[Int](g.vertexCount)(0)
    val exit = ArrayBuffer.fill[Int](g.vertexCount)(0)
    var time = 0

    @tailrec
    def traverse(vertexStack: List[(Int, List[Edge])] ): Unit = {
      vertexStack match {
        case Nil =>
        case (v, edges) :: rest =>
          vertexState(v) match {
            case UnDiscovered =>
              vertexState(v) = Discovered
              vertexEarly(v)

              entry(v) = time
              time += 1

              traverse((v, g.edge(v)) :: rest)

            case Discovered if (edges.isEmpty) =>
              vertexLate(v)

              vertexState(v) = Processed
              exit(v) = time
              time += 1

              traverse(rest)

            case Discovered =>
              val e = edges.head
              val nextStack = (v, edges.tail) :: rest
              if (vertexState(e.vertex) == UnDiscovered) {
                parent(e.vertex) = Some(v)
                processEdge(v, e)
                traverse((e.vertex, Nil) :: nextStack)
              }
              else if ((vertexState(e.vertex) != Processed && parent(v) != Some(e.vertex)) || g.directed) {
                processEdge(v, e)
                traverse(nextStack)
              }
              else {
                traverse(nextStack)
              }

            case Processed =>
              assert(false, "vertex "+v+" is in processed state and still in stack, should not be possible")
          }
      }
    }

    traverse(List((root, Nil)))

    SearchResult(vertexState, parent)
  }

  def connectedComponents(g: Graph): Map[Int, Set[Int]] = {
    val vertexState = ArrayBuffer.fill[VertexState](g.vertexCount)(UnDiscovered)
    val parent = ArrayBuffer.fill[Option[Int]](g.vertexCount)(None)

    var c = 0
    var conComps = Map.empty[Int, Set[Int]]

    def addVertexToComponent(v: Int): Unit = {
      conComps = conComps + (c -> (conComps.getOrElse(c, Set[Int]()) + v))
    }

    (0 until g.vertexCount).foreach{ i =>
      if (vertexState(i) == UnDiscovered) {
        _bfs(g, i, vertexState, parent)(vertexEarly = addVertexToComponent)
        c += 1
      }
    }
    conComps
  }

  def isBiPartite(g: Graph): Boolean = {
    sealed abstract class Color
    case object NoColor extends Color
    case object White extends Color
    case object Black extends Color

    def complement(c: Color): Color =
      c match {
        case NoColor => NoColor
        case Black => White
        case White => Black
      }

    val color = ArrayBuffer.fill[Color](g.vertexCount)(NoColor)
    val vertexState = ArrayBuffer.fill[VertexState](g.vertexCount)(UnDiscovered)
    val parent = ArrayBuffer.fill[Option[Int]](g.vertexCount)(None)

    var bipartite = true

    def checkColors(v: Int, e: Edge): Unit = {
      if (color(v) == color(e.vertex)) bipartite = false
      color(e.vertex) = complement(color(v))
    }

    (0 until g.vertexCount).foreach{ i =>
      if (vertexState(i) == UnDiscovered) {
        color(i) = White
      }
      _bfs(g, i, vertexState, parent)(processEdge = checkColors)
    }

    bipartite
  }
}
