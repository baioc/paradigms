package org.example.ine5410;

import javax.annotation.Nonnull;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.*;

public class MergeSortExecutor<T extends Comparable<T>> implements MergeSort<T> {
	private final int MIN_FORK_SIZE = 8192;

	@Nonnull
	@Override
	public ArrayList<T> sort(@Nonnull final List<T> list) {
		// 1. Crie um Cached ExecutorService
		//  (Executors Single thread ou fixed thread pool) causarão starvation!
		/* ~~~~ O tipo do executor precisa ser Cached!!!! ~~~~ */
		ExecutorService executor = Executors.newCachedThreadPool();

		// 2. Submete uma tarefa incial ao executor
		// 3. Essa tarefa inicial vai se subdividir em novas tarefas enviadas para
		//  o mesmo executor
		ArrayList<T> merged = mergesort(executor, list);

		// 4. Desligue o executor ao sair!
		executor.shutdown();

		return merged;
	}

	@Nonnull
	private ArrayList<T> mergesort(ExecutorService exec, @Nonnull final List<T> list) {
		if (list.size() <= 1)
			return new ArrayList<>(list);

		final int mid = list.size() / 2;

		if (mid < MIN_FORK_SIZE) {
			return MergeSortHelper.merge(
				new MergeSortSerial<T>().sort(list.subList(0, mid)),
				new MergeSortSerial<T>().sort(list.subList(mid, list.size()))
			);

		} else {
			Future<ArrayList<T>> futureLeft = exec.submit(new Callable<ArrayList<T>>() {
				@Override
				public ArrayList<T> call() {
					return mergesort(exec, list.subList(0, mid));
				}
			});

			ArrayList<T> right = mergesort(exec, list.subList(mid, list.size()));

			ArrayList<T> left = null;
			try {
				left = futureLeft.get();
			} catch (InterruptedException | ExecutionException e) {
				e.printStackTrace();
			}
			return MergeSortHelper.merge(left, right);
		}
	}
}
