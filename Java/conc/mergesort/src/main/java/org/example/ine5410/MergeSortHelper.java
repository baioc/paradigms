package org.example.ine5410;

import javax.annotation.Nonnull;
import java.util.ArrayList;
import java.util.List;

class MergeSortHelper {
	static @Nonnull <T extends Comparable<T>> ArrayList<T>
	merge(@Nonnull List<T> left, @Nonnull List<T> right) {
		ArrayList<T> out = new ArrayList<>();

		final int lSize = left.size();
		final int rSize = right.size();

		for (int l = 0, r = 0; l < lSize || r < rSize; ) {
			if (l >= lSize) {
				out.add(right.get(r++));
			} else if (r >= rSize) {
				out.add(left.get(l++));
			} else {
				final T lVal = left.get(l);
				final T rVal = right.get(r);
				final int diff = lVal.compareTo(rVal);
				if (diff <= 0) {
					out.add(lVal);
					++l;
				} else {
					out.add(rVal);
					++r;
				}
			}
		}

		return out;
	}
}
