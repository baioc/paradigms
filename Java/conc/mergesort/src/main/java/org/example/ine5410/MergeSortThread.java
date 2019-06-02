package org.example.ine5410;

import javax.annotation.Nonnull;
import java.util.ArrayList;
import java.util.List;
import java.lang.Thread;

public class MergeSortThread<T extends Comparable<T>> implements MergeSort<T> {
	private final int MIN_FORK_SIZE = 8192;

	@Nonnull
	@Override
	public ArrayList<T> sort(@Nonnull final List<T> list) {
		if (list.size() <= 1)
			return new ArrayList<>(list);

		final int mid = list.size() / 2;

		if (mid < MIN_FORK_SIZE) {
			return MergeSortHelper.merge(
				new MergeSortSerial<T>().sort(list.subList(0, mid)),
				new MergeSortSerial<T>().sort(list.subList(mid, list.size()))
			);

		} else {
			// 1. Há duas sub-tarefas, execute-as em paralelo usando threads
			//  (Para pegar um retorno da thread filha faça ela escrever em um ArrayList)
			//  Dica: para obter o resultado produzido por uma thread, faça com que,
			//  no escopo do método que a criou, escreva seu resultado em um final ArrayList<ArrayList<T>>
			final ArrayList<ArrayList<T>> results = new ArrayList<>();
			results.add(null);

			Thread lo = new Thread(new Runnable() {
				@Override
				public void run() {
					results.set(0, sort(list.subList(0, mid)));
				}
			});

			lo.start();
			ArrayList<T> right = sort(list.subList(mid, list.size()));

			try {
				lo.join();
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
			return MergeSortHelper.merge(results.get(0), right);
		}
	}
}
