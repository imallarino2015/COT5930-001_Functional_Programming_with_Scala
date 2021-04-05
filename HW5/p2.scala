import fpinscala.monoids.Monoid

object p2 {
  case class MinMax[A](min:Option[A], max:Option[A])

  def getMinMax[A](seq:IndexedSeq[A])(less: (A,A) => Boolean): MinMax[A] = {
    def min(a:Option[A],b:Option[A])=if (less(a.get, b.get)) a else b
    def max(a:Option[A],b:Option[A])=if (less(a.get, b.get)) b else a
    val m = new Monoid[MinMax[A]] {
      def op(mm1: MinMax[A], mm2: MinMax[A]) =
        (mm1, mm2) match {
          case (MinMax(None, None), MinMax(None, None)) =>
            MinMax(None, None)

          case (MinMax(min1, None), MinMax(None, None)) =>
            MinMax(min1, None)
          case (MinMax(None, max1), MinMax(None, None)) =>
            MinMax(None, max1)
          case (MinMax(None, None), MinMax(min2, None)) =>
            MinMax(min2, None)
          case (MinMax(None, None), MinMax(None, max2)) =>
            MinMax(None, max2)

          case (MinMax(None, None), MinMax(min2, max2)) =>
            MinMax(min2, max2)
          case (MinMax(None, max1), MinMax(None, max2)) =>
            MinMax(None, max(max1, max2))
          case (MinMax(None, max1), MinMax(min2, None)) =>
            MinMax(min2, max1)
          case (MinMax(min1, None), MinMax(None, max2)) =>
            MinMax(min1, max2)
          case (MinMax(min1, None), MinMax(min2, None)) =>
            MinMax(min(min1, min2), None)
          case (MinMax(min1, max1), MinMax(None, None)) =>
            MinMax(min1, max1)

          case (MinMax(None, max1), MinMax(min2, max2)) =>
            MinMax(min2, max(max1, max2))
          case (MinMax(min1, None), MinMax(min2, max2)) =>
            MinMax(min(min1, min2), max2)
          case (MinMax(min1, max1), MinMax(None, max2)) =>
            MinMax(min1, max(max2, max1))
          case (MinMax(min1, max1), MinMax(min2, None)) =>
            MinMax(min(min1, min2), max1)

          case (MinMax(min1, max1), MinMax(min2, max2)) =>
            MinMax(min(min1, min2), max(max1, max2))
        }
      val zero = MinMax(None, None)
    }
    Monoid.foldMapV(seq, m)(i => MinMax(Some(i), Some(i)))
  }

  def main(args: Array[String])={
    println(getMinMax(IndexedSeq(3,7,3,1,5,7,4,6,9))(_<_))
/*
  getMinMax(IndexedSeq())==MinMax(None,None)
  IndexedSeq().length==0
  m.zero==MinMax(None,None)
  MinMax(None, None)==MinMax(None,None)
*/
    println("Zero Law: "+(getMinMax(IndexedSeq[Int]())(_<_)==MinMax(None,None)))

/*
  getMinMax(getMinMax(a,b),c)==getMinMax(a,getMinMax(b,c))
  getMinMax(MinMax(min(a,b),max(a,b)),c)==getMinMax(a,MinMax(min(b,c),max(b,c)))
  MinMax(min(MinMax(min(a,b),max(a,b)),c),max(MinMax(min(a,b),max(a,b)),c))==
    MinMax(min(a,MinMax(min(b,c),max(b,c))),max(a,MinMax(min(b,c),max(b,c))))
  MinMax(min(a,MinMax.min),max(a,MinMax.max))==
    MinMax(min(MinMax.min,c),max(MinMax.max,c))
  MinMax(min(a,min(b,c)),max(a,max(b,c)))==MinMax(min(min(a,b),c),max(max(a,b),c))
  MinMax(min(a,b,c),max(a,b,c))== MinMax(min(a,b,c),max(a,b,c))
*/

    val tst=IndexedSeq[Int](1,3,2)
    val leftMM=getMinMax(IndexedSeq(tst(0),tst(1)))(_<_)
    val rightMM=getMinMax(IndexedSeq(tst(1),tst(2)))(_<_)
    println("Associative Property: "+(
      getMinMax(IndexedSeq[Int](leftMM.min.get,leftMM.max.get,tst(2)))(_<_)==
      getMinMax(IndexedSeq[Int](rightMM.min.get,rightMM.max.get,tst(0)))(_<_)
      ))
  }
}