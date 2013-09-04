package ninetynineprobs {
  case class Logic(val v: Boolean) {
    def and(b: Logic): Logic = {
      (this, b) match {
        case (Logic(true), Logic(true)) => Logic(true)
        case _ => Logic(false)
      }
    }

    def or(b: Logic): Logic = {
      (this, b) match {
        case (Logic(true), _) => Logic(true)
        case (_, Logic(true)) => Logic(true)
        case _ => Logic(false)
      }
    }
  }

  object S99Logic {
  
    implicit def boolean2Logic(b: Boolean) = Logic(b)
    implicit def logic2Boolean(l: Logic) = l.v

    def and(a: Boolean, b: Boolean): Boolean =
      (a, b) match {
        case (true, true) => true
        case (_, _) => false
      }

    def or(a: Boolean, b: Boolean): Boolean =
      (a, b) match {
        case (true, _) => true
        case (_, true) => true
        case (_, _) => false
      }

    def not(a: Boolean): Boolean =
      a match {
        case true => false
        case false => true
      }

    def nand(a: Boolean, b: Boolean): Boolean =
      not(and(a,b))

    def nor(a: Boolean, b: Boolean): Boolean =
      not(or(a,b))

    def xor(a: Boolean, b: Boolean): Boolean =
      (a, b) match {
        case (true, false) => true
        case (false, true) => true
        case (_, _) => false
      }

    def equ(a: Boolean, b: Boolean): Boolean =
      not(xor(a,b))

    def impl(a: Boolean, b: Boolean): Boolean =
      or(not(a), b)

    def table2(f: (Boolean, Boolean) => Boolean): List[String] = {
      import collection.mutable.ListBuffer
      val lb = new ListBuffer[String]
      lb += "A     B     result"
      for(
        a <- List(false, true);
        b <- List(false, true)
      ) lb += ("%-5s %-5s %s".format(a, b, f(a,b)))
      lb.toList
    }

    def gray(n: Int): List[String] = 
      n match {
        case 0 => Nil
        case 1 => List("0", "1")
        case _ =>
          val l = gray(n-1)
          val r = l.reverse
          l.map { "0"+_ } ::: r.map { "1"+_ }
      }

    val grayMap = collection.mutable.Map[Int, List[String]](0 -> Nil, 1 -> List("0", "1"))
    def grayMemo(n: Int): List[String] = {
      if (!grayMap.contains(n)) {
        val l = grayMemo(n-1)
        val r = l.reverse
        val codes = l.map { "0"+_ } ::: r.map { "1"+_ }
        grayMap += (n -> codes)
      }

      grayMap(n)
    }

    case class HuffNode(
      val symbol: String, 
      val freq: Int, 
      val child1: HuffNode, 
      val child2: HuffNode) {
    }

    object HuffNodeOrdering extends Ordering[HuffNode] {
      def compare(a: HuffNode, b:HuffNode): Int = 
        if (a.freq < b.freq)
          1
        else if (a.freq > b.freq)
          -1
        else
          0
    }

    // this impl uses the scala mutable priority queue to implement the single
    // queue approach from here http://en.wikipedia.org/wiki/Huffman_coding#Basic_technique
    def huffmanMut(symFreqList: List[(String, Int)]): List[(String, String)] = {
      def walkTree(node: HuffNode, code: String): List[(String, String)] = {
        node match {
          case HuffNode(sym, _, _, _) if (sym != null) => 
            (sym, code) :: Nil
          case HuffNode(_, _, c1, c2) =>
            walkTree(c1, code+"0") ::: walkTree(c2, code+"1")
          case _ =>
            throw new RuntimeException("tree's effed up bro, got a wacky node: "+node)
        }
      }

      def buildTreeAndWalk: List[(String, String)] = {
        import collection.mutable.PriorityQueue

        val q = new PriorityQueue[HuffNode]()(HuffNodeOrdering)

        symFreqList.foreach { t => q += new HuffNode(t._1, t._2, null, null) }

        while(q.size != 1) {
          val node1 = q.dequeue()
          val node2 = q.dequeue()

          val par12 = HuffNode(null, node1.freq+node2.freq, node1, node2)

          q += par12
        }

        val head = q.dequeue()

        walkTree(head, "")
      }

      buildTreeAndWalk
    }

    // this impl uses 2 immutable scala queues to implement the double
    // queue approach from here http://en.wikipedia.org/wiki/Huffman_coding#Basic_technique
    def huffmanImm(symFreqList: List[(String, Int)]): List[(String, String)] = {
      def walkTree(node: HuffNode, code: String): List[(String, String)] = {
        node match {
          case HuffNode(sym, _, _, _) if (sym != null) => 
            (sym, code) :: Nil
          case HuffNode(_, _, c1, c2) =>
            walkTree(c1, code+"0") ::: walkTree(c2, code+"1")
          case _ =>
            throw new RuntimeException("tree's effed up bro, got a wacky node: "+node)
        }
      }

      import collection.immutable.Queue

      //this assumes that there at least 1 node in q2
      def getLowestNode(
        q1: Queue[HuffNode], 
        q2: Queue[HuffNode]): 
          (HuffNode, Queue[HuffNode], Queue[HuffNode]) = {

        (q1.headOption, q2.headOption) match {
          case (None, _) => 
            (q2.head, q1, q2.tail)
          case (_, None) => 
            (q1.head, q1.tail, q2)
          case (Some(n1), Some(n2)) if (n1.freq < n2.freq) =>
            (n1, q1.tail, q2)
          case _ =>
            (q2.head, q1, q2.tail)
        }
      }

      //this requires initial conditions of leafQ full of leaf nodes in ascending frequency order
      //resQ emtpy
      def buildTreeAndWalkIm(leafQ: Queue[HuffNode], resQ: Queue[HuffNode]): List[(String, String)] = {
        if (leafQ.size == 0 && resQ.size == 1)
          walkTree(resQ.head, "")
        else {
          val (n1, q1,  q2) = getLowestNode(leafQ, resQ)
          val (n2, nextLeafQ, nextResQ) = getLowestNode(q1, q2)

          val pn = HuffNode(null, n1.freq+n2.freq, n1, n2)

          buildTreeAndWalkIm(nextLeafQ, nextResQ.enqueue(pn))
        }
      }

      val leafNodesAscFreq = symFreqList.map{ pair => HuffNode(pair._1, pair._2, null, null) }.sortWith{ _.freq < _.freq}
      val leafQ: Queue[HuffNode] = Queue.empty.enqueue(leafNodesAscFreq)

      buildTreeAndWalkIm(leafQ, Queue.empty)
    }

  }
}
