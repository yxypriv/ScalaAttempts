package datastructure

/**
 * @author xingyan
 */
class StringMatch {
	
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
	 * O(n)
	 */
	def substringPrefixLength(str: String): Array[Long] = {
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
}