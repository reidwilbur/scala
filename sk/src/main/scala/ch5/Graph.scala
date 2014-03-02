package ch5

import scala.collection.immutable.Queue

case class Graph(
  val edge: IndexedSeq[List[Edge]],
  val edgeCount: Int,
  val degree: IndexedSeq[Int],
  val directed: Boolean) {
  assert(edge.size == degree.size)

  val vertexCount = edge.size
}

case class Edge(vertex: Int, weight: Int)

object Graph {
  import collection.mutable.ArrayBuffer

  def apply(vertices: Int, edges: List[(Int,Int)], directed: Boolean = false): Graph = {
    val edgeBuffer  = ArrayBuffer.fill[List[Edge]](vertices)(List.empty)
    val degree = ArrayBuffer.fill[Int](vertices)(0)
    var edgeCount = 0;

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

  case class BfsResult(vertexState: IndexedSeq[VertexState], parent: IndexedSeq[Option[Int]])

  def bfs(g: Graph, root: Int)
         (vertexEarly: (Int)  => Unit = {(v) =>},
          vertexLate:  (Int)  => Unit = {(v) =>},
          processEdge: (Int, Edge) => Unit = {(r, e) =>}): BfsResult = {
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
          processEdge: (Int, Edge) => Unit = {(r, e) =>}): BfsResult = {
    assert(root >= 0)
    assert(root < g.vertexCount)

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
    BfsResult(vertexState, parent)
  }

  def connectedComponents(g: Graph): Map[Int, Set[Int]] = {
    val vertexState = ArrayBuffer.fill[VertexState](g.vertexCount)(UnDiscovered)
    val parent = ArrayBuffer.fill[Option[Int]](g.vertexCount)(None)

    var c = 0
    var conComps = Map.empty[Int, Set[Int]]
    (0 until g.vertexCount).foreach{ i =>
      if (vertexState(i) == UnDiscovered) {
        _bfs(
          g, i, vertexState, parent
        )(vertexEarly = {v: Int => conComps = conComps + (c -> conComps.getOrElse(c, Set[Int]()).+(v) ) })
        c += 1
      }
    }
    conComps
  }

  def isBiPartite(g: Graph): Boolean = {
    val vertexState = ArrayBuffer.fill[VertexState](g.vertexCount)(UnDiscovered)
    val parent = ArrayBuffer.fill[Option[Int]](g.vertexCount)(None)

    sealed abstract class Color
    case object NoColor extends Color
    case object White extends Color
    case object Black extends Color

    def complement(c: Color): Color = {
      c match {
        case NoColor => NoColor
        case Black => White
        case White => Black
      }
    }

    val color = ArrayBuffer.fill[Color](g.vertexCount)(NoColor)

    var bipartite = true;
    (0 until g.vertexCount).foreach{ i =>
      if (vertexState(i) == UnDiscovered) {
        color(i) = White
      }
      _bfs(g, i, vertexState, parent)(processEdge = {
        (v, e) =>
          if (color(v) == color(e.vertex)) bipartite = false
          color(e.vertex) = complement(color(v))
      })
    }
    bipartite
  }
}
