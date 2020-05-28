import scala.collection.immutable.HashMap

class CompressedSTree (val subtrees:Map[String,CompressedSTree], leafNumber:Set[(Int,Int)] = Set()){
  def printLeaf()  = if(leafNumber.isEmpty) {""} else {" ["+leafNumber.toString()+"]"}
  def print(indent: Int) : String = {
    def subPrint(s:String ):String = if(subtrees(s).subtrees.isEmpty) {
        STree.nspaces(indent) + "-"+ s + subtrees(s).printLeaf + ".\n"
    } else {
        STree.nspaces(indent) + "-"+ s + subtrees(s).printLeaf + "|\n"+ subtrees(s).print(indent+s.length+1)
    }
    val subPrinted = subtrees.keys.map(subPrint)
    subPrinted.foldLeft(printLeaf)((a:String,b:String)=>a+b)
  }
}
class STree(val subtrees:Map[Char,STree], leafNumber: Set[(Int,Int)] = Set()) {
  def insert(s:String, id: Int, n: Int):STree = {
    if(s.length == 0) {
      new STree(subtrees,leafNumber++Set((id,n)))
    } else {
      val c = s(0)
      if (subtrees.keySet.contains(c)) {
        val st = subtrees(c)
        new STree(subtrees + (c -> st.insert(s.substring(1), id, n)))
      } else {
        new STree(subtrees + (s(0) -> STree.single(s.substring(1),id,  n)))
      }
    }
  }

  def print(indent: Int) : String = {
    def printLeaf = if(leafNumber.isEmpty) {""}else {STree.nspaces(indent)+"["+leafNumber.toString()+"]\n"}
    def subPrint(c:Char ):String = STree.nspaces(indent) + c + printLeaf+  "-\n"+ subtrees(c).print(indent+1)
    val subPrinted = subtrees.keys.map(subPrint)
    subPrinted.foldLeft(printLeaf)((a:String,b:String)=>a+b)
  }


  def compressedPrefixed():(String,CompressedSTree)  = {
    def submap(c:Char):(String,CompressedSTree) = {
          subtrees(c).compressedPrefixed match {
            case (path,cst) => (c + path) -> cst
          }
    }
    if(subtrees.isEmpty) {
       val m =new HashMap[String,CompressedSTree]()
      ("",new CompressedSTree(m,leafNumber))
    } else if(subtrees.keys.size == 1) {
      submap(subtrees.keys.head)
    } else {
        val m = subtrees.keys.toList.map(submap)
        ("",new CompressedSTree(m.toMap,leafNumber))
    }
  }

  def suffixTree(s:String,id:Int): STree = {
    (0 until s.length).map(n=>s.substring(n)).foldRight(this)((v:String,t:STree)=>t.insert(v,id,s.length-v.length))
  }

  def compressed(): CompressedSTree = {
    compressedPrefixed match {
      case (s, st) => if (s.equals("")) {
        st
      } else {
        new CompressedSTree(List(s -> st).toMap,leafNumber)
      }
    }
  }
}

object STree {
  def nspaces(n: Int): String  = " ".toString * n
  def empty(id:Int,number:Int): STree = {
    new STree(new HashMap[Char,STree](),Set((id,number)))
  }
  def single(s: String,id:Int,number: Int): STree = {
    if (s.isEmpty) {
      empty(id,number)
    } else {
      val suffix = s.substring(1);
      return new STree(List(s(0) -> single(suffix,id, number)).toMap)
    }
  }
  def suffixTree(s:String,id:Int): STree = {
    empty(id,0).suffixTree(s, id)
  }
}
