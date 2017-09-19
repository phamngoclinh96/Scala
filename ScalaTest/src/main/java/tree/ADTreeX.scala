package tree

/**
  * Created by linhpn on 19/09/2017.
  */
package tree
/**
  * Created by linhpn on 18/09/2017.
  */
import scala.collection.mutable
import scala.io.Source._
class Record{
  var record:Array[Array[Int]]=new Array[Array[Int]](0)
  var arity:Array[Int]=new Array[Int](0)
  var numberAttribute=0
  def this(num:Int){
    this()
    record = new Array[Array[Int]](0)
    numberAttribute = num
    arity =new Array[Int](num)
  }
  def this(array:Array[Array[Int]]){
    this()
    this.record=array
    numberAttribute = record(0).length
    arity = new Array[Int](record(0).length)
    record.foreach(r=>{
      for(i <- 0 until r.length){
        if(arity(i) < r(i)) arity(i) = r(i)
      }
    })
  }
  def getArity(i:Int):Int= arity(i)
  def getRow(row:Int):Array[Int]=record(row)
  def getValue(row:Int,col:Int):Int = record(row)(col)
  def numRecord:Int = record.length
  def add(row:Array[Int]): Unit ={
    for( i <- 0 until arity.length){
      if( arity(i) < row(i))
        arity(i) = row(i)
    }
    record = record.union(Array(row))
    numberAttribute = record(0).length
  }
  def numAttribute:Int = numberAttribute
  def Record:Array[Array[Int]]=record
  def print(array: Array[Int]): Unit ={
    array.foreach(s=> printf(s"${s} "))
    println()
  }
  def print(): Unit ={
    println(s"number Attribute : ${numberAttribute}")
    println(s"number record : ${numRecord}")
    printf(s"arity :")
    print(arity)
    println("----record-----")
    record.foreach(print)
    println("-----end------")
  }
}
class ADNodeX{
  var count =0
  var children:Array[VaryNodeX]=new Array[VaryNodeX](0)
  var start =0
  def this(i:Int,record:Record,rMin:Int){
    this()
    this.count = record.numRecord
    this.start = i
    var m =record.numAttribute
    this.children = new Array[VaryNodeX](m-start+1)
    for(j <- i+1 until m ){
      children(j-start) = new VaryNodeX(j,record,rMin)
    }
  }
  def getCount():Int= count
  def getChildren(i:Int):VaryNodeX=if(i-start<children.length) children(i-start) else  null
  def Query(attibute:Array[Int],value:Array[Int]):Int = makeContab(this,attibute,value,0)
  private def makeContab(adnode:ADNodeX,attibute:Array[Int],value:Array[Int],i:Int):Int={
    if(adnode==null) return  0
    if(attibute.length == i) return adnode.getCount()
    var varyN:VaryNodeX = adnode.getChildren(attibute(i))
    var mcv =varyN.mcv
    var n= varyN.arity
    if(value(i)> n) return 0
    var ct=new Array[Int](n+1)
    if(value(i)!= mcv) return makeContab(varyN.getChildren(value(i)),attibute,value,i+1)
    for(k <- 1 until n+1)
      if(k!=mcv) {
        var adn = varyN.getChildren(k)
        ct(k) = makeContab(adn,attibute,value,i+1)
      }else ct(k)=0
    ct(mcv) = makeContab(adnode,attibute,value,i+1) - ct.reduce(_+_)
    return  ct(value(i))
  }
}
class VaryNodeX{
  var mcv =0
  var children:Array[ADNodeX]=new Array[ADNodeX](0)
  var arity=0
  var count =0
  def this(attributeNum:Int,record:Record,rMin:Int){
    this()
    count = record.numRecord
    arity=record.getArity(attributeNum)
    var childRecord:Array[Record]=new Array[Record](record.getArity(attributeNum)+1)
    for(i <- 0 until childRecord.length) childRecord(i)=new Record(record.numAttribute)

    for(i <- 0 until record.numRecord){
      var v = record.getValue(i,attributeNum)
      if(v < childRecord.length)
        childRecord(v).add(record.getRow(i))
    }
    for(i <- 0 until childRecord.length)
      if(childRecord(i).numRecord > mcv) mcv=i
    children = new Array[ADNodeX](record.getArity(attributeNum)+1)
    if(attributeNum < record.numAttribute)
      for(i<- 1 until childRecord.length )
      {
        if(i!=mcv && childRecord(i).numRecord >rMin ) children(i) =new ADNodeX(attributeNum,childRecord(i),rMin)
        //if( childRecord(i).numRecord >rMin ) children(i) =new ADNodeX(attributeNum,childRecord(i),rMin)
      }
  }
  def getChildren(i:Int):ADNodeX={
    if(i>=children.length) return null
    children(i)
  }
  def getCount(i:Int):Int={
    if(i>= children.length) return  0
    if(i==mcv) return count - children.map(_.count).reduce(_+_)
    return children(i).count
  }
}
object ADTreeX {

  def main(arg:Array[String]): Unit ={
    var lines = fromFile("D:/scala/record.txt").getLines()
    var data:Array[Array[Int]] = lines.map(
      _.split(' ').map(_.toInt).toList.toArray
    ).toArray
    var record = new Record(data)
    var root = new ADNodeX(-1,record,rMin = 0)
    println(s"${root.Query(Array(0,1,2),Array(1,3,2))}")
  }
}


