package ch5

import org.scalatest.{FunSpec}
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class GraphSuite extends FunSpec {
  describe("An undirected graph") {
    it("should contain all initialized edges") {
      //    1
      //   / \
      // 0   3 - 4
      //  \ /
      //   2
      val g = Graph(5, List((0,1), (0,2), (1,3), (2,3), (3,4)))

      assert(g.directed == false)
      assert(g.edge.size == 5)
      assert(g.degree.size == 5)

      var edges = List(Edge(2,0), Edge(1,0))
      assert(g.edge(0) == edges)
      assert(g.degree(0) == 2)

      edges = List(Edge(3,0), Edge(0,0))
      assert(g.edge(1) == edges)
      assert(g.degree(1) == 2)

      edges = List(Edge(3,0), Edge(0,0))
      assert(g.edge(2) == edges)
      assert(g.degree(1) == 2)

      edges = List(Edge(4,0), Edge(2,0), Edge(1,0))
      assert(g.edge(3) == edges)
      assert(g.degree(3) == 3)

      edges = List(Edge(3,0))
      assert(g.edge(4) == edges)
      assert(g.degree(4) == 1)
    }
  }

  describe("An directed graph") {
    it("should contain all initialized edges") {
      //    1
      //   / \
      // 0   3 - 4
      //  \ /
      //   2
      val g = Graph(5, List((0,1), (0,2), (1,3), (2,3), (3,4)), directed = true)

      assert(g.directed == true)
      assert(g.edge.size == 5)
      assert(g.degree.size == 5)

      var edges = List(Edge(2,0), Edge(1,0))
      assert(g.edge(0) == edges)
      assert(g.degree(0) == 2)

      edges = List(Edge(3,0))
      assert(g.edge(1) == edges)
      assert(g.degree(1) == 1)

      edges = List(Edge(3,0))
      assert(g.edge(2) == edges)
      assert(g.degree(1) == 1)

      edges = List(Edge(4,0))
      assert(g.edge(3) == edges)
      assert(g.degree(3) == 1)

      edges = List()
      assert(g.edge(4) == edges)
      assert(g.degree(4) == 0)
    }
  }

  describe("bfs on an undirected graph") {
    it("should visit parent nodes before leaf nodes") {
      //    1
      //   / \
      // 0   3 - 4
      //  \ /
      //   2
      val g = Graph(5, List((0,1), (0,2), (1,3), (2,3), (3,4)))

      var processedVerts = List.empty[Int]
      Graph.bfs(g, 0)((v: Int) => processedVerts = v :: processedVerts)
      assert(processedVerts == List(4,3,1,2,0))
    }

    it("should traverse all edges") {
      val g = Graph(5, List((0,1), (0,2), (1,3), (2,3), (3,4)))

      var edgeCount = 0;
      Graph.bfs(g, 0)(processEdge = {(v, e) => edgeCount += 1})
      assert(edgeCount == 5)
    }

    it("should return indexed seq of parent for each vertex") {
      val g = Graph(5, List((0,1), (0,2), (1,3), (2,3), (3,4)))

      val result = Graph.bfs(g, 0)()

      assert(result.parent == IndexedSeq[Option[Int]](None, Some(0), Some(0), Some(2), Some(3)))
    }
  }

  describe("connectedComponents on an undirected graph") {
    it("should return a map of the connected vertices with 1 connected component") {
      //    1
      //   / \
      // 0   3 - 4
      //  \ /
      //   2
      val g = Graph(5, List((0,1), (0,2), (1,3), (2,3), (3,4)))

      val result = Graph.connectedComponents(g)

      assert(result == Map((0 -> Set(0,1,2,3,4))))
    }

    it("should return a map of the connected vertices with multiple connected components") {
      //    1
      //   / \       5 - 6
      // 0   3 - 4
      //  \ /       7
      //   2
      val g = Graph(8, List((0,1), (0,2), (1,3), (2,3), (3,4), (5,6)))

      val result = Graph.connectedComponents(g)

      assert( result == Map( 0 -> Set(0,1,2,3,4), 1 -> Set(5,6), 2 -> Set(7) ) )
    }
  }

  describe("isBipartite") {
    it("should return true for bipartite graph") {
      //    1
      //   / \
      // 0   3 - 4
      //  \ /
      //   2
      val g = Graph(5, List((0,1), (0,2), (1,3), (2,3), (3,4)))

      assert(true == Graph.isBiPartite(g))
    }

    it("should return false for non bipartite graph") {
      //    1
      //   / \
      // 0 - 3 - 4
      //  \ /
      //   2
      val g = Graph(5, List((0,1), (0,2), (1,3), (2,3), (3,4), (0,3)))

      assert(false == Graph.isBiPartite(g))
    }
  }
}
