package org.example.ine5410;

import org.apache.commons.lang3.time.StopWatch;
import org.junit.Assert;
import org.junit.Test;

import javax.annotation.Nonnull;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class MergeSortTest {

	private void testSort(@Nonnull MergeSort<Integer> sorter) {
		int sizes[] = {0, 1, 2, 4, 8, 128};
		for (int i = 0; i < 4; i++) {
			for (int size : sizes) {
				String msg = String.format("i=%d, size=%d", i, size);
				List<Integer> data = new ArrayList<>();
				for (int j = 0; j < size; j++) data.add((int) (Math.random() * 1000000));

				ArrayList<Integer> expected = new ArrayList<>(data);
				Collections.sort(expected);

				ArrayList<Integer> actual = sorter.sort(data);
				if (data.size() > 100) {
					//avoid huuuuge logs
					//noinspection SimplifiableJUnitAssertion
					Assert.assertTrue(msg, expected.equals(actual));
				} else {
					Assert.assertEquals(msg, expected, actual);
				}
			}
		}
	}

	@Test
	public void testSortSerial() {
		testSort(new MergeSortSerial<Integer>());
	}
	@Test
	public void testSortThread() {
		testSort(new MergeSortThread<Integer>());
	}
	@Test
	public void testMergeSortExecutor() {
		testSort(new MergeSortExecutor<Integer>());
	}

	@Test
	public void testSpeed() {
		List<Integer> data = new ArrayList<>();
		for (int j = 0; j < 262144; j++)
			data.add((int) (Math.random() * 1000000));

		double executorAvg = 0, threadAvg = 0, serialAvg = 0;
		for (int i = 0; i < 7; i++) {
			StopWatch sw = StopWatch.createStarted();
			new MergeSortSerial<Integer>().sort(data);
			serialAvg += sw.getTime();

			sw = StopWatch.createStarted();
			new MergeSortExecutor<Integer>().sort(data);
			executorAvg += sw.getTime();

			sw = StopWatch.createStarted();
			new MergeSortThread<Integer>().sort(data);
			threadAvg += sw.getTime();

			Runtime.getRuntime().gc();
			try {
				Thread.sleep(100);
			} catch (InterruptedException ignored) {}
		}
		executorAvg /= 7;
		threadAvg /= 7;
		serialAvg /= 7;

		System.out.printf("executorAvg: %.3f, threadAvg: %.3f serialAvg: %.3f\n",
				executorAvg, threadAvg, serialAvg);
		Assert.assertTrue(executorAvg < 1.6*serialAvg);
		Assert.assertTrue(threadAvg < 1.6*serialAvg);
	}

}
