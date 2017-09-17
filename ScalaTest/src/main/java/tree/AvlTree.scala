package tree
import util.control.Breaks._

class AvlTree {
  private var _root=0
  private var _n = 0
  private var _max= 1000
  private var _parent=new Array[Int](_max)
  private var _left=new Array[Int](_max)
  private var _right=new Array[Int](_max)
  private var _depth=new Array[Int](_max)
  private var _count =new Array[Int](_max)
  private var _values=new Array[Double](_max)
  private var _aggregatedCount=new Array[Int](_max)

  //  def this(){
  //    this()
  //  }
  def compare(node:Int,x:Double): Int= if(value(node)<x) 1 else if(value(node) ==x) 0 else -1
  def compare(nodeA:Int,nodeB:Int):Int = compare(nodeA,value(nodeB))
  def root():Int=_root
  def size():Int=_n
  def parentNode(node:Int):Int=_parent(node)
  def leftNode(node:Int):Int =_left(node)
  def rightNode(node:Int):Int=_right(node)
  def depth(node:Int):Int=_depth(node)
  def count(node:Int):Int=_count(node)
  def aggregatedCount(node:Int):Int=_aggregatedCount(node)
  def value(node:Int):Double=_values(node)
  //Tree accessors
  def first(node:Int):Int={
    if(node == 0) 0
    var n = node
    var left =leftNode(n)
    while (left != 0){
      n = left
      left = leftNode(n)
    }
    n
  }
  def first():Int=first(_root)
  def last(node:Int):Int={
    var n=node
    breakable{while (true){
      var right=rightNode(n)
      if(right==0) break
      n=right
    }}
    n
  }
  def nextNode(node:Int):Int={
    var n=node
    var right =rightNode(n)
    if(right!=0) first(right)
    else {
      var parent =parentNode(n)
      while (parent!=0 && n == rightNode(parent)){
        n=parent
        parent=parentNode(parent)
      }
      parent
    }
  }
  def prevNode(node:Int):Int={
    var n=node
    var left =leftNode(node)
    if(left!= 0) last(left)
    else {
      var parent =parentNode(n)
      while (parent!=0 && n == leftNode(parent)){
        n=parent
        parent=parentNode(parent)
      }
      parent
    }
  }
  // Mutators
  def updateAggregates(node:Int): Unit ={

    _depth(node)=1 + math.max(depth(leftNode(node)),depth(rightNode(node)))
    _aggregatedCount(node) = count(node) +aggregatedCount(leftNode((node)))+aggregatedCount(rightNode(node))
  }
  def update(node:Int,x:Double,w:Int): Unit ={
    _values(node)+=w*(x-value(node))/count(node)
    _count(node)+=w
    var n=node
    while (n!=0){
      updateAggregates(n)
      n=parentNode(n)
    }
  }
  def merge(node:Int,x:Double,w:Int): Unit ={
    assert(value(node)==x)
    _count(node)+=w
    var n=node
    while(n!=0){
      updateAggregates(n)
      n=parentNode(n)
    }
  }
  def add(x:Double,w:Int):Boolean={
    if(_root==0){
      _n +=1
      _root =  _n
      _values(_root) = x
      _count(_root) =w
      _left(_root) = 0
      _right(_root)=0
      _parent(_root)=0

      updateAggregates(_root)
    }else{
      var node =_root
      var parent = 0
      var cmp =0
      do{
        cmp =compare(node,x)
        if(cmp<0){
          parent=node
          node =leftNode(node)
        }else if(cmp>0){
          parent=node
          node=rightNode(node)
        }else{
          merge(node,x,w)
          false
        }
      }while(node != 0)
      _n += 1
      node = _n
      _values(node)=x
      _count(node)=w
      _left(node)=0
      _right(node)=0
      _parent(node)=0
      if(cmp<0) _left(parent)=node
      else {
        assert(cmp>0)
        _right(parent)=node
      }

    }
    true
  }
  def find(x:Double):Int={
    var node =_root
    while (node != 0){
      var cmp =compare(node,x)
      if(cmp <0) node =leftNode(node)
      else if(cmp>0) node =rightNode(node)
      else  node
    }
    0
  }
  def floor(x:Double):Int={
    var f =0
    var node =_root
    while (node != 0){
      var cmp =compare(node,x)
      if(cmp <=0) node =leftNode(node)
      else {
        f = node
        node =rightNode(node)
      }
    }
    f
  }
  def floorSum(x:Long):Int={
    var sum = x
    var f=0
    var node = _root
    while(node != 0){
      var left =leftNode(node)
      var leftCount:Long = aggregatedCount(left)
      if(leftCount <= sum){
        f=node
        sum -= leftCount+count(node)
        node =rightNode(node)
      }else node =leftNode(node)
    }
    f
  }
  def ceilSum(node:Int):Long={
    var left =leftNode(node)
    var sum = aggregatedCount(left)
    var n =node
    var p = parentNode(node)
    while (p!= 0){
      if(n==rightNode(p)){
        var leftP=leftNode(p)
        sum += count(p)+aggregatedCount(leftP)
      }
      n=p
      p=parentNode(n)
    }
    sum
  }
  private def balanceFactor(node:Int):Int= depth(leftNode(node))-depth(rightNode(node))
  private def rebalance(node:Int): Unit ={
    var n =node
    breakable{ while (n!= 0) {
      var p = parentNode(n)
      updateAggregates(n)
      balanceFactor(n) match {
        case -2 => {
          var right = rightNode(n)
          if (balanceFactor(right) == 1) rotateRight(right)
          rotateLeft(n)
          break
        }
        case 2 => {
          var left = leftNode(n)
          if (balanceFactor(left) == -1) rotateLeft(left)
          rotateRight(n)
          break
        }
        case -1 => break
        case 1 => break
        case 0 => break
        case _ => assert(true == false)
      }
      n=p
    }

    }
  }
  private def rotateLeft(node:Int): Unit ={
    var r=rightNode(node)
    var lr =leftNode(r)
    _right(node)=lr
    if(lr!= 0) _parent(lr)=node

    var p =parentNode(node)
    _parent(r)=p
    if( p==0) _root=r
    else if(leftNode(p)==node) _left(p)=r
    else {
      assert(rightNode(p)==node)
      _right(p)=r
    }
    _left(r)=node
    _parent(node)=r
    updateAggregates(node)
    updateAggregates(parentNode(node))
  }
  private def rotateRight(node:Int): Unit ={
    var l=leftNode(node)
    var rl =rightNode(l)
    _left(node)=rl
    if(rl!=0) _parent(rl)=node
    var p =parentNode(node)
    _parent(l)=p
    if(p==0) _root=l
    else if(rightNode(p)==node) _right(p)=l
    else {
      assert(leftNode(p)==node)
      _left(p)=l
    }
    _right(l)=node
    _parent(node)=l
    updateAggregates(node)
    updateAggregates(parentNode(node))
  }
  def checkBalance(node:Int):Boolean={
    if(node == 0) depth(node)==0
    else (depth(node)==(1+math.max(depth(leftNode(node)),depth(rightNode(node)))) ) && checkBalance(leftNode(node)) && checkBalance(rightNode(node))
  }
  def checkAggregates():Boolean=checkBalance(_root)
  def checkIntegrity(node:Int):Boolean={
    if(node == 0) true
    else {
      var ok =true
      if(leftNode(node)!=0) {
        ok &= _values(node)>= _values(leftNode(node))
        ok &= checkIntegrity(rightNode(node))
      }
      if(rightNode(node)!=0) {
        ok &= _values(node)>= _values(rightNode(node))
        ok &= checkIntegrity(leftNode(node))
      }
      ok
    }
  }
  def checkIntegrity():Boolean=checkIntegrity(_root)
  def print(node:Int): Unit ={
    if(node != 0){
      println(s"Node : ${node}")
      println(s"Value : ${_values(node)}")
      print(leftNode(node))
      print(rightNode(node))
    }
  }
  def print():Unit=print(_root)
}

