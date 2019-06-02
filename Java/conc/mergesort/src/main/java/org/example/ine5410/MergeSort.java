package org.example.ine5410;

import javax.annotation.Nonnull;
import java.util.ArrayList;
import java.util.List;

public interface MergeSort<T extends Comparable<T>> {
	@Nonnull ArrayList<T> sort(@Nonnull final List<T> list);
}
