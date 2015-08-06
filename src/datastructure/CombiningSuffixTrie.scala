package datastructure

/**
 * @author xingyan
 */
class CombiningSuffixTrie {
	class TreeNode {
		val children: scala.collection.mutable.Map[Char, TreeNode] = scala.collection.mutable.Map.empty[Char, TreeNode];
		var nextLink: TreeNode = null;
		var traffic = 0;

		def display(name: String): String = {
			return "(%s,%d) => %s".format(name, this.traffic, this.children.keySet.toArray.sorted.mkString(","));
		}
	}

	val root: TreeNode = new TreeNode();
	var endNode = root;

	def buildString(str: String) {
		str.toCharArray().foreach(c => buildIndex(c));
		endNode = root;
	}
	
	def displaySuffix() {
		displaySuffix(root, "");
	}

	def displaySuffix(node: TreeNode, prefix: String) {
		println(node.display(prefix));
		for (c <- node.children.keySet.toArray.sorted)
			displaySuffix(node.children(c), prefix + c);
	}

	private def buildIndex(c: Char) {
		this.endNode = buildIndex(c, this.endNode);
		this.endNode.nextLink;
	}

	private def buildIndex(c: Char, curNode: TreeNode): TreeNode = {
		var workingNode: TreeNode = null;
		if (curNode.children.contains(c)) {
			workingNode = curNode.children(c);
		} else {
			workingNode = new TreeNode;
			curNode.children += ((c, workingNode));
			//			workingNode.string = curNode.string + c;
		}
		workingNode.traffic += 1;
		if (curNode.nextLink != null) {
			workingNode.nextLink = buildIndex(c, curNode.nextLink);
		} else {
			workingNode.nextLink = curNode; //root
		}
		return workingNode;
	}

	def toSubStringAppearFrequencyMap(): Map[String, Int] = {
		val result = scala.collection.mutable.Map.empty[String, Int];
		toSubStringAppearFrequencyMap(result, root, "");
		return result.toMap;
	}

	private def toSubStringAppearFrequencyMap(map: scala.collection.mutable.Map[String, Int], node: TreeNode, prefix: String) {
		if (!prefix.equals(""))
			map += ((prefix, node.traffic));
		for (c <- node.children.keySet.toArray.sorted)
			toSubStringAppearFrequencyMap(map, node.children(c), prefix + c);
	}
	
	
	def getIndexStringList(indexList:Array[Int]):Map[Int,String] = {
		val targetSet = indexList.toSet;
		val map = scala.collection.mutable.Map.empty[Int, String];
		travelIndex(0, root, "", (i:Int, str:String) => {
			if(targetSet.contains(i))
				map.put(i, str);
		});
		return map.toMap;
	}
	
	private def travelIndex(curIndex:Int, node: TreeNode, prefix: String, callback:(Int, String) => Unit ):Int ={
		var ci = curIndex;
		callback(ci, prefix);
		for (c <- node.children.keySet.toArray.sorted) {
			ci +=1;
			ci = travelIndex(ci, node.children(c), prefix + c, callback);
		}
		return ci;
	}
}

object CombineSuffixTrieObj {
	def main(args: Array[String]): Unit = {
		val ct = new CombiningSuffixTrie();
//		ct.buildString("aabbcc");
		ct.buildString("aab");
		ct.buildString("aac");
//		ct.displaySubString();
		println(ct.getIndexStringList((0 to 10).map(x => x).toArray))
	}
}