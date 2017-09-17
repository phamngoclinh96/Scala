package tree

import util.control.Breaks._

class TDigest {
  private var _compression:Double =100
  private var _count:Double =0
  private var _centroids:AvlTree =new AvlTree()
  def this(compression:Double){
    this()
    _compression=compression
  }
  def size():Long = _count.toLong
  def add(x:Double): Unit ={
    add(x,1)
  }
  def add(x:Double,w:Int): Unit ={
    var start:Int=_centroids.floor(x)
    if(start == 0 ) start = _centroids.first()

    if(start == 0){
      assert(_centroids.size()==0)
      _centroids.add(x,w)
      _count +=w
    }else{
      var minDistance:Double = Double.MaxValue
      var lastNeighbor:Int = 0
      var neighbor = start
      breakable {
        while (start != 0) {
          var z: Double = math.abs(_centroids.value(neighbor) - x)
          if (z < minDistance) {
            start = neighbor
            minDistance = z
          } else {
            lastNeighbor = neighbor
            break
          }
          neighbor = _centroids.nextNode(neighbor)
        }
      }
      var closest=0
      var sum :Long = _centroids.ceilSum(start)
      var n:Double=0
      neighbor = start
      while (neighbor!=lastNeighbor){
        assert(minDistance==math.abs(_centroids.value(neighbor)-x))
        var q:Double = if(_count ==1) 0.5 else (sum +(_centroids.count(neighbor)-1/2.0))/(_count-10)
        var k:Double = 4*_count*q*(1-q)/_compression
        if(_centroids.count(neighbor)+w<=k){
          n+=1
          if(math.random<1/n) closest=neighbor
        }
        sum+=_centroids.count(neighbor)
        neighbor=_centroids.nextNode(neighbor)
      }
      if(closest==0) _centroids.add(x,w)
      else _centroids.update(closest,x,w)
      _count+=w
      if(_centroids.size()>20*_compression){
        println(s"Compress: ${_centroids.size()}")
        compress()
      }
    }
  }
  def interpolate(x:Double,a:Double,b:Double):Double= (x-a)/(b-a)
  def quantile(previousIndex:Double,index:Double,nextIndex:Double,previousMean:Double,nextMean:Double):Double={
    var delta= nextIndex-previousIndex
    var previousWeight =(nextIndex-index)/delta
    var nextWeight =(index-previousIndex)/delta
    previousMean*previousWeight + nextMean*nextWeight
  }
  def centroids():AvlTree = _centroids
  def merge(digest:TDigest): Unit ={
    var centroids:AvlTree = digest.centroids()
    var n = centroids.first()
    while (n!=0){
      add(centroids.value(n),centroids.count(n))
      n=centroids.nextNode(n)
    }
  }
  def compress(): Unit ={

  }
  def quantile(q:Double):Double={
    if(q<0 || q>1) 0
    if(_centroids.size()==0) 0
    else if(_centroids.size()==1) _centroids.value(_centroids.first())

    var index:Double = q*(_count-1)
    var previousMean = Double.NaN
    var previousIndex =0.0
    var next:Int =_centroids.floorSum(index.toLong)
    assert(next!=0)
    var total:Long = _centroids.ceilSum(next)
    var prev:Int =_centroids.prevNode(next)
    if(prev != 0){
      previousMean =_centroids.value(prev)
      previousIndex =total -(_centroids.count(prev)+1.0)/2
    }
    while (true){
      var nextIndex=total+(_centroids.count(next)-1.0)/2
      if(nextIndex >= index){
        if(previousMean == Double.NaN){
          assert(total==0)
          if(nextIndex ==previousIndex) _centroids.value(next)

          var next2:Int=_centroids.value(next).toInt
          var nextIndex2 =total +_centroids.count(next)+(_centroids.count(next2)-1.0)/2
          previousMean =(nextIndex2*_centroids.value(next)-nextIndex*_centroids.value(next2))/(nextIndex2-nextIndex)
        }
        return quantile(previousIndex,index,nextIndex,previousMean,_centroids.value(next))
      }else if(_centroids.value(next) == 0){
        var nextIndex2:Double = _count-1
        var nextMean2:Double = (_centroids.value(next)*(nextIndex2-previousIndex)-previousMean*(nextIndex2-nextIndex))/(nextIndex-previousIndex)
        return  quantile(nextIndex,index,nextIndex2,_centroids.value(next),nextMean2)
      }
      total+=_centroids.count(next)
      previousMean =_centroids.value(next)
      previousIndex=nextIndex
      next =_centroids.nextNode(next)
    }
    return 0.0
  }
}
