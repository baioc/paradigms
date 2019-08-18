/*
 * Copyright (c) 2019 Gabriel B. Sant'Anna <baiocchi.gabriel@gmail.com>
 *
 * @License Apache <https://gitlab.com/baioc/paradigms>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package br.ufsc.baioc;

import java.util.Collections;
import java.util.List;
import java.util.ArrayList;
import java.util.Random;


/**
 * Custom Arrays implementation with static utility functions.
 */
public class Arrays {

	// STATIC METHODS
	/**
	 * Very simple and stable sorting algorithm, better for an array that is already mostly sorted. Generally worse than selectionSort.
	 * <p>
	 * Worst case: O(n^2) comparisons AND swaps.
	 * <p>
	 * Best case: O(n) comparisons, O(1) swaps.
	 * <p>
	 * Average: O(n^2) comparisons AND swaps.
	 * <p>
	 * Space: O(1).
	 */
	public static <T extends Comparable<T>> void bubbleSort(T[] array) {
		boolean done = false; //early out

		// each iteration puts the highest element of the unsorted subarray on its final place at the sorted subarray
		for (int sorted = 0; sorted < array.length - 1 && !done; ++sorted) {
			done = true;

			// swap adjacent elements if the first is bigger than the second
			for (int i = 0; i < array.length - 1 - sorted; ++i) {
				if (array[i].compareTo(array[i + 1]) > 0) {
					done = false;
					swap(array, i, i + 1);
				}
			}

		}
	}

	/**
	 * Simple and stable in-place sorting algorithm, generally worse than insertionSort.
	 * <p>
	 * Worst case: O(n^2) comparisons, O(n) swaps.
	 * <p>
	 * Best case: O(n^2) comparisons, O(1) swaps.
	 * <p>
	 * Average: O(n^2) comparisons, O(n) swaps.
	 * <p>
	 * Space: O(1).
	 */
	public static <T extends Comparable<T>> void selectionSort(T[] array) {
		// each iteration puts the lowest element of the unsorted subarray on its final place at the sorted subarray
		for (int j = 0; j < array.length - 1; ++j) {
			int iMin = indexOfMinimum(array, j);
			if (iMin != j) {
				swap(array, j, iMin);
			}
		}
	}

	/**
	 * One of the fastest in-place sorting algorithms for small arrays, stable.
	 * <p>
	 * Worst case: O(n^2) comparisons, O(n) insertions.
	 * <p>
	 * Best case: O(n) comparisons, O(1) insertions.
	 * <p>
	 * Average: O(n^2) comparisons, O(n) insertions.
	 * <p>
	 * Space: O(1).
	 */
	public static <T extends Comparable<T>> void insertionSort(T[] array) {
		for (int i = 1; i < array.length; ++i) {
			insert(array, i - 1, array[i]);
		}
	}

	/**
	 * Fast sorting algorithm using recursion. Stable but not in-place.
	 * <p>
	 * Always runs on O(n*logn) time.
	 * <p>
	 * Space: O(n).
	 */
	public static <T extends Comparable<T>> void mergeSort(T[] array) {
		mergeSort(array, 0, array.length - 1);
	}

	/**
	 * The fastest (on average) sorting method available. Unstable.
	 * Should not be used on arrays already sorted.
	 * <p>
	 * Worst case: O(n^2).
	 * <p>
	 * Best case: O(n*logn).
	 * <p>
	 * Average: O(n*logn).
	 * <p>
	 * Space: O(n).
	 */
	public static <T extends Comparable<T>> void quickSort(T[] array) {
		quickSort(array, 0, array.length - 1);
	}


	/**
	 * Swaps elements at the specified indexes of given array.
	 * @throws ArrayIndexOutOfBoundsException
	 */
	public static <T> void swap(T[] array, int firstIndex, int secondIndex) {
		T temp = array[firstIndex];
		array[firstIndex] = array[secondIndex];
		array[secondIndex] = temp;
	}

	/**
	 * Reverses the order of a one-dimensional array.
	 */
	public static <T> void reverse(T[] array) {
		int mid = array.length / 2;
		for (int i = 0; i < mid; ++i) {
			swap(array, i, array.length - 1 - i);
		}
	}

	/**
	 * Finds the first occurence of the minimal element of the array on the interval [startIndex, endIndex)
	 *
	 * @param <T> - Comparable type.
	 *
	 * @param startIndex - Beggining of the subarray, inclusive.
	 * @param endIndex - Ending of the subarray, non-inclusive.
	 *
	 * @return the index of the minimal element on the subarray.
	 *
	 * @throws ArrayIndexOutOfBoundsException when specified start or end indexes are invalid.
	 */
	public static <T extends Comparable<T>> int indexOfMinimum(T[] array, int startIndex, int endIndex) {
		int iMin = startIndex;

		for (int i = iMin + 1; i < endIndex; ++i) {
			if (array[i].compareTo(array[iMin]) < 0) {
				iMin = i;
			}
		}

		return iMin;
	}

	/**
	 * Finds the first occurence of the minimal element on the subarray starting at a specific index to the end of the array.
	 *
	 * @param <T> - Comparable type.
	 *
	 * @param startIndex - Beggining of the subarray.
	 *
	 * @return the index of the minimal element on the subarray.
	 *
	 * @throws ArrayIndexOutOfBoundsException when specified index is invalid.
	 */
	public static <T extends Comparable<T>> int indexOfMinimum(T[] array, int startIndex) {
		return indexOfMinimum(array, startIndex, array.length);
	}

	/**
	 * Finds the first occurence of the minimal element on the array.
	 *
	 * @param <T> - Comparable type.
	 *
	 * @return the index of the minimal element on the array.
	 */
	public static <T extends Comparable<T>> int indexOfMinimum(T[] array) {
		return indexOfMinimum(array, 0, array.length);
	}


	// PRIVATE
	/**
	 * Inserts an element at its correct position on a previously sorted subarray going from zero to rightIndex.
	 *
	 * @param array The whole array.
	 * @param rightIndex Limits the sorted subarray to be iterated on.
	 * @param e The element to be inserted.
	 *
	 * @throws ArrayIndexOutOfBoundsException when rightIndex is bigger or equal than the array length.
	 */
	private static <T extends Comparable<T>> void insert(T[] array, int rightIndex, T e) {
		int i = rightIndex;

		for (; i >= 0 && array[i].compareTo(e) > 0; --i) {
			array[i + 1] = array[i];
		}

		array[i + 1] = e;
	}

	/**
	 * Actual recursive merge-sort algorithm.
	 *
	 * @throws ArrayIndexOutOfBoundsException when specified start or end indexes are invalid.
	 */
	private static <T extends Comparable<T>> void mergeSort(T[] array, int startIndex, int endIndex) {
		if (endIndex > startIndex) {
			int mid = (startIndex + endIndex) / 2;
			mergeSort(array, startIndex, mid);
			mergeSort(array, mid + 1, endIndex);
			merge(array, startIndex, mid, endIndex);
		}
	}

	/**
	 * Merges two sorted subarrays into a sorted whole.
	 * <p>
	 * Allocates space equal to the size of the two subarrays.
	 */
	private static <T extends Comparable<T>> void merge(T[] array, int startIndex, int midIndex, int endIndex) {
		T[] low = java.util.Arrays.copyOfRange(array, startIndex, midIndex + 1);
		T[] high = java.util.Arrays.copyOfRange(array, midIndex + 1, endIndex + 1);

		int i, j;
		int k = startIndex;

		// shallow copy
		for (i = 0; i < low.length; ++i, ++k) {
			low[i] = array[k];
		}
		for (j = 0; j < high.length; ++j, ++k) {
			high[j] = array[k];
		}

		i = 0;
		j = 0;
		k = startIndex;

		while (i < low.length && j < high.length) {
			if (low[i].compareTo(high[j]) <= 0) {
				array[k] = low[i];
				i++;
			} else {
				array[k] = high[j];
				j++;
			}
			k++;
		}

		// finish
		while (i < low.length) {
			array[k] = low[i];
			i++;
			k++;
		}
		while (j < high.length) {
			array[k] = high[j];
			j++;
			k++;
		}
	}

	/**
	 * Actual recursive quick-sort algorithm.
	 *
	 * @throws ArrayIndexOutOfBoundsException when specified start or end indexes are invalid.
	 */
	private static <T extends Comparable<T>> void quickSort(T[] array, int startIndex, int endIndex) {
		if (startIndex < endIndex) {
			int pivot = partition(array, startIndex, endIndex);
			quickSort(array, startIndex, pivot - 1);
			quickSort(array, pivot + 1, endIndex);
		}
	}

	/**
	 * Partitions a subarray around a pivot and returns its index. Takes linear time.
	 *
	 * @return pivot index/position on the array.
	 *
	 * @throws ArrayIndexOutOfBoundsException when specified start or end indexes are invalid.
	 */
	private static <T extends Comparable<T>> int partition(T[] array, int startIndex, int endIndex) {
		int pivot = startIndex;

		for (int i = startIndex; i < endIndex; ++i) {
			if (array[i].compareTo(array[endIndex]) <= 0) {	// chosen pivot is the rightmost element in the subarray
				swap(array, i, pivot);	// pivot here means the index of the leftmost element in group G
				++pivot;
			}
		}

		swap(array, endIndex, pivot);	// puts the pivot on the correct position

		return pivot;

		// partition(array, p, r)
		// Compare array[j] with array[r], for j = p, p+1,...r-1 maintaining that:
		// 	array[p..q-1] are values known to be <= to array[r]
		// 	array[q..j-1] are values known to be > array[r]
		// 	array[j..r-1] haven't been compared with array[r]
		// If array[j] > array[r], just increment j.
		// If array[j] <= array[r], swap array[j] with array[q], increment q and increment j.
		// Once all elements in array[p..r-1] have been compared with array[r],
		// 	swap array[r] with array[q]
		// 	return q.
	}


	// SPEED TEST
	/**
	 * Testing different sorting algorithms. Prints csv-formatted output in nanosecond scale.
	 *
	 * @param args -- respectively minimum and maximum array size, followed by increment in size.
	 * Defaults are used when any of those are not found.
	 */
	public static void main(String... args) {
		int min, max, stride;
		Random rand = new Random(System.currentTimeMillis());

		try {
			min = Integer.parseInt(args[0]);
			max = Integer.parseInt(args[1]);
			stride = Integer.parseInt(args[2]);
		} catch (ArrayIndexOutOfBoundsException | NumberFormatException e) {
			min = 0;
			max = 10000;
			stride = 50;
		}

		System.out.print("n,bubble,selection,insertion,merge,quick,default,stream");

		for (int n = min; n <= max; n += stride) {
			System.err.println(n);
			System.out.print("\n" + n);

			List<Integer> numbers = new ArrayList<>(n);
			for (int i = 0; i < n; ++i) {
				numbers.add(rand.nextInt());
			}

			{	// BUBBLE SORT
				Collections.shuffle(numbers);
				Integer[] array = numbers.toArray(new Integer[numbers.size()]);
				long start = System.nanoTime();
				bubbleSort(array);
				long end = System.nanoTime();
				System.out.print("," + (end - start));
			}

			{	// SELECTION SORT
				Collections.shuffle(numbers);
				Integer[] array = numbers.toArray(new Integer[numbers.size()]);
				long start = System.nanoTime();
				selectionSort(array);
				long end = System.nanoTime();
				System.out.print("," + (end - start));
			}

			{	// INSERTION SORT
				Collections.shuffle(numbers);
				Integer[] array = numbers.toArray(new Integer[numbers.size()]);
				long start = System.nanoTime();
				insertionSort(array);
				long end = System.nanoTime();
				System.out.print("," + (end - start));
			}

			{	// MERGE SORT
				Collections.shuffle(numbers);
				Integer[] array = numbers.toArray(new Integer[numbers.size()]);
				long start = System.nanoTime();
				mergeSort(array);
				long end = System.nanoTime();
				System.out.print("," + (end - start));
			}

			{	// QUICK SORT
				Collections.shuffle(numbers);
				Integer[] array = numbers.toArray(new Integer[numbers.size()]);
				long start = System.nanoTime();
				quickSort(array);
				long end = System.nanoTime();
				System.out.print("," + (end - start));
			}

			{	// DEFAULT java.util.Arrays' SORT
				Collections.shuffle(numbers);
				Integer[] array = numbers.toArray(new Integer[numbers.size()]);
				long start = System.nanoTime();
				java.util.Arrays.sort(array);
				long end = System.nanoTime();
				System.out.print("," + (end - start));
			}

			{	// DEFAULT "streamed" SORT
				Collections.shuffle(numbers);
				Integer[] array = numbers.toArray(new Integer[numbers.size()]);
				long start = System.nanoTime();
				array = java.util.Arrays.stream(array).sorted().toArray(Integer[]::new);
				long end = System.nanoTime();
				System.out.print("," + (end - start));
			}
		}

		System.out.println();
	}

}
