package datastructure

import scala.collection.mutable.Map;

/**
 * @author xingyan
 */
class SuffixTrie(str: String) {
	class TreeNode {
		val children: scala.collection.mutable.Map[Char, TreeNode] = scala.collection.mutable.Map.empty[Char, TreeNode];
		var nextLink: TreeNode = null;
		var traffic = 0;
		//		var string = "";
		//		override def toString(): String = {
		//			return "(%s,%d) => %s".format(this.string, this.traffic, this.children.keySet.toArray.sorted.mkString(","));
		//		}

		def display(name: String): String = {
			return "(%s,%d) => %s".format(name, this.traffic, this.children.keySet.toArray.sorted.mkString(","));
		}
	}

	val root: TreeNode = new TreeNode();
	var endNode = root;

	str.toCharArray().foreach(c => buildIndex(c));

	def display() {
		display(root, "");
	}

	def display(node: TreeNode, prefix: String) {
		println(node.display(prefix));
		for (c <- node.children.keySet.toArray.sorted)
			display(node.children(c), prefix + c);
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
		val result = Map.empty[String, Int];
		toSubStringAppearFrequencyMap(result, root, "");
		return result;
	}

	private def toSubStringAppearFrequencyMap(map: Map[String, Int], node: TreeNode, prefix: String) {
		if (!prefix.equals(""))
			map += ((prefix, node.traffic));
		for (c <- node.children.keySet.toArray.sorted)
			toSubStringAppearFrequencyMap(map, node.children(c), prefix + c);
	}
}

object SuffixTrie {
	def main(args: Array[String]): Unit = {
		println(scala.collection.mutable.Set.empty[Int].map(x => x).max);		
	}

	def main1(args: Array[String]): Unit = {
		//		var str = "aabababaabaaabaabbabbabbbabababbabaabaaaababbaabaaaabababaababaaababaaababaaababaabbaaabaaababbabababababaababaabababababaaaaabababaaaabbaabaaaabaaaaababaabababaaababaabbabbabaabbabbaababababaabaabaabababaaabbbabbabababababbaaababaaababaaabababaabbabababbababaabaaaababababababababaaabaababaaaabbbabaaabaaabababaaaabababababaabababaabababaabbbaaaabababaaabbabaabaababaababababaabababbaababbabaaabaabababaaaabaaabaababaabaabaabaaaaaaaaababaaabaaababbababbaababababaababaaaaaaabaabababaabbabababababbabaabababababababaababaababaabaabaabaaababababbbabaaaaabaaabababaaaabbaaaababaaababaaaababaababababaabaababaababaababaaababbaababaaaaabababaabababbbabababaaababaabaabaababaaabababaababaabababbabbaaaaababbbabaababbaababbabaababaaaabaaaabaabbabaababababababbabaabbabaababaabaabaaaaabaabaaabaaaababaaabaaabaabababaababaabaaababbaabaababaaabababaabaabaababaabaababababaaabaababaaababaababaaaabaababababababbbabaababaabaababaababbabaababbabababaabaaabababbababaaaaaabababaaababaababababbabaabbabaaaabababaaababaabaaabababababababaabaababaabbaaaabbaababababaabaaabaabaabababababaaababaaababaababaaababaaabbabaababaabababaabaabababababaabababbaaababababaaba";
		//		var str = "ababcababd";
		var str = "ababa"

		val t0 = compat.Platform.currentTime;

		val st: SuffixTrie = new SuffixTrie(str);
		st.display()
		//		val m1 = st.toSubStringAppearFrequencyMap();

		val t1 = compat.Platform.currentTime;
		println((t1 - t0) + "ms")
		val m2 = (1 to str.length()).map(len => (0 to str.length() - len).map(i => (str.substring(i, i + len), 1)) //
			.groupBy(_._1).map(x => (x._1, x._2.map(_._2).sum))).flatten.toMap;

		val t2 = compat.Platform.currentTime;
		println(m2);
		println((t2 - t1) + "ms")
		//		st.display();
	}
}