package datastructure

/**
 * @author xingyan
 */
object StringMatch {

	/**
	 * O(n)
	 */
	def buildKMPTable(patternStr: String): Array[Int] = {
		val result = new Array[Int](patternStr.length());
		result(0) = 0;
		var j = 0;
		for (i <- (1 to patternStr.length() - 1)) {
			while (j > 0 && patternStr(j) != patternStr(i))
				j = result(j - 1);
			if (patternStr(i) == patternStr(j))
				j += 1;
			result(i) = j;
		}
		return result;
	}

	/**
	 * buildSuffixLongestCommonPrefixLengthArray
	 * O(n)
	 * Z-Algorithm
	 */
	def buildSuffixLCPLengthArray(str: String): Array[Long] = {
		val result = new Array[Long](str.length());
		var start = 0;
		var interval = 0;
		(1 to str.length() - 1).foreach { i =>
			if (interval <= i - start) {
				interval = 0;
				while (interval + i < str.length() && str(interval) == str(i + interval))
					interval += 1;
				result(i) = interval;
				start = i;
			} else {
				var correspondingLocation = i - start;
				var corLength = result(correspondingLocation);
				if (corLength < interval - correspondingLocation) {
					result(i) = result(correspondingLocation);
				} else {
					interval = interval - correspondingLocation
					while (interval + i < str.length() && str(interval) == str(i + interval))
						interval += 1;
					result(i) = interval;
					start = i;
				}
			}
		}
		result(0) = str.length();
		return result;
	}
	
	def buildPLCArray(str:String) : Array[Int] = {
		val subStrings = (0 to str.length()-1).map(i => (i, str.substring(i))).sortBy(_._2);
		println(subStrings)
		val plcArray = new Array[Int](str.length());
		var curCommonLength = 0;
		(0 to str.length()-1).foreach{i => 
			if(i == 0)
				plcArray(i) = -1;
			else {
				var curSubString = subStrings(i)._2;
				var preSubString = subStrings(i-1)._2;
				if(curSubString(0) != preSubString(0))
					curCommonLength = 0;
				else {
					var compareLength = math.min(curSubString.length(), preSubString.length());
					while(i+curCommonLength < compareLength &&  
							curSubString(i+curCommonLength) == preSubString(i+curCommonLength))
						curCommonLength+=1;
				}
				plcArray(i) = curCommonLength;
			}
		}
		return plcArray;
	}
}

object StringMatchTest {
	def main(args: Array[String]): Unit = {
		println(StringMatch.buildPLCArray("banana").mkString(","));
	}
}