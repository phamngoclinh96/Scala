package tree

import jdk.nashorn.internal.parser.TokenType

import util.control.Breaks._
object TDigestMain {
  def main(args:Array[String]): Unit ={
    var tdigest=new TDigest(100)
    tdigest.add(-1.0)
    tdigest.add(2.0)
    tdigest.add(3.0)
    tdigest.add(5.0)
    println(s"MEDIAN: ${tdigest.quantile(0.4)}")
    println(s"95-PERCENTILE: ${tdigest.quantile(0.95)}")
    //    breakable {while (true){
    //
    //        var x = 1
    //        if (x == 1) break
    //      }
    //    }


  }
}
