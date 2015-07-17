package datastructure

/**
 * @author xingyan
 */
class TrieTree {
	class Node {
		val children = scala.collection.mutable.Map.empty[Char, Node];
		var traffic = -1;
		def display(name: String): String = {
			return "(%s,%d) => %s".format(name, this.traffic, this.children.keySet.toArray.sorted.mkString(","));
		}
	}
	val root: Node = new Node();
	def addString(str: String) {
		var curNode = root;
		str.toCharArray().map { c =>
			if (!curNode.children.contains(c)) {
				val newNode = new Node();
				curNode.children += ((c, newNode));
			}
			curNode = curNode.children(c);
		};
		curNode.traffic = 0;
	}

	def display() {
		display(root, "");
	}

	def display(node: Node, prefix: String) {
		println(node.display(prefix));
		for (c <- node.children.keySet.toArray.sorted)
			display(node.children(c), prefix + c);
	}

	def getScannerPool(): MultiThreadScannerPool = {
		return new MultiThreadScannerPool();
	}

	class MultiThreadScannerPool() {
		class TrieTreeScanner {
			var curNode: Node = root;
			var count = 0;
			var stopped = false;
		}

		def calculate(str: String): Int = {
			(0 to str.length() - 1).par.filter(i => root.children.contains(str(i))).foldLeft(0) { (l, i) =>
				var scanner = new TrieTreeScanner();
				var index = i;
				while (scanner.stopped == false && index < str.length()) {
					if (scanner.curNode.children.contains(str(index))) {
						scanner.curNode = scanner.curNode.children(str(index));
						if (scanner.curNode.traffic == 0)
							scanner.count += 1;
					} else {
						scanner.stopped = true;
					}
					index += 1;
				}
				l + scanner.count;
			}
		}
	}

	@deprecated
	class TrieScannerPool {
		class TrieTreeScanner {
			var curNode: Node = root;
			var count = 0;
			var stopped = false;
		}
		val scannerList = scala.collection.mutable.ArrayBuffer[TrieTreeScanner]();
		var scannerNumberSave = 0;
		def addScanner(): TrieTreeScanner = {
			val r = new TrieTreeScanner();
			scannerList.append(r);
			return r;
		}

		def allScannerProcess(c: Char): TrieScannerPool = {
			var stoppedList = scannerList.filter(_.stopped);
			scannerNumberSave += stoppedList.map(_.count).sum;
			scannerList --= stoppedList;
			scannerList.foreach { scanner =>
				if (scanner.curNode.children.contains(c)) {
					scanner.curNode = scanner.curNode.children(c);
					if (scanner.curNode.traffic == 0)
						scanner.count += 1;
				} else {
					scanner.stopped = true;
				}
			}
			return this;
		}

		def getCurrentScannerSum(): Int = {
			return scannerList.map(_.count).sum + scannerNumberSave;
		}

		def resetScanners() {
			scannerList.clear();
			scannerNumberSave = 0;
		}
	}
}

object TrieTree {
	def main(args: Array[String]): Unit = {
		val tt = new TrieTree();
		tt.addString("she"); //1
		tt.addString("he"); //2
		tt.addString("sheild"); //10
		tt.addString("hell"); //8
		tt.display();

		val str = "shell";
		val pool = tt.getScannerPool();
		println(pool.calculate(str));
		//		str.toCharArray().foreach { c =>
		//			pool.addScanner();
		//			println(pool.allScannerProcess(c).getCurrentScannerSum(), c);
		//		};
	}
}