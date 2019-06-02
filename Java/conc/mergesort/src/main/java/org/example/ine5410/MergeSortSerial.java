package org.example.ine5410;

import javax.annotation.Nonnull;
import java.util.ArrayList;
import java.util.List;

public class MergeSortSerial<T extends Comparable<T>> implements MergeSort<T> {
	@Nonnull
	@Override
	public ArrayList<T> sort(@Nonnull final List<T> list) {
		if (list.size() <= 1)
			return new ArrayList<>(list);

		final int mid = list.size() / 2;

		return MergeSortHelper.merge(
			sort(list.subList(0, mid)),
			sort(list.subList(mid, list.size()))
		);
	}
}
