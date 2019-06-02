package org.example.ine5410;

import org.apache.commons.lang3.time.StopWatch;
import org.kohsuke.args4j.Argument;
import org.kohsuke.args4j.CmdLineException;
import org.kohsuke.args4j.CmdLineParser;
import org.kohsuke.args4j.Option;

import javax.annotation.Nonnull;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class Main  {
	enum Implementation {
		SERIAL {
			@Nonnull @Override MergeSort<Integer> getMergeSort() {
				return new MergeSortSerial<>();
			}
		}, THREADS {
			@Nonnull @Override MergeSort<Integer> getMergeSort() {
				return new MergeSortThread<>();
			}
		}, EXECUTOR {
			@Nonnull @Override MergeSort<Integer> getMergeSort() {
				return new MergeSortExecutor<>();
			}
		};
		@Nonnull abstract MergeSort<Integer> getMergeSort();
	};

	@Option(name = "--implementation", aliases = {"-i"},
	       usage = "Define a implementação de MergeSort a ser usada")
	private Implementation implementation = Implementation.SERIAL;

	@Option(name = "--help", aliases = {"-h"}, help = true)
	private boolean help = false;

	@Argument(metaVar = "SIZE", required = true,
	          usage = "Size fo the randomly generated array to sort")
	private int size;

	public static void main(String[] args) throws Exception {
		Main app = new Main();
		CmdLineParser parser = new CmdLineParser(app);
		try {
			parser.parseArgument(args);
		} catch (CmdLineException e) {
			showHelp(parser, System.err);
			throw e;
		}
		if (app.help) showHelp(parser, System.out);
		else app.run();
	}

	private static void showHelp(CmdLineParser parser, PrintStream out) {
		out.print("Uso: java -jar JARFILE ");
		parser.printSingleLineUsage(System.err);
		System.err.println();
		parser.printUsage(System.err);
	}

	private void run() {
		MergeSort<Integer> ms = implementation.getMergeSort();

		List<Integer> data = new ArrayList<>();
		for (int i = 0; i < size; i++)
			data.add((int) (Math.random() * 1000000));

		ArrayList<Integer> expected = new ArrayList<>(data);
		Collections.sort(expected);

		double avg = 0;
		boolean correct = true;
		for (int i = 0; i < 10; i++) {
			StopWatch sw = StopWatch.createStarted();
			ArrayList<Integer> actual = ms.sort(data);
			correct &= expected.equals(actual);
			avg += sw.getTime() / 10.0;

			Runtime.getRuntime().gc();
			try {
				Thread.sleep(100);
			} catch (InterruptedException ignored) {}
		}

		System.out.printf("Ordenou %d números %s em %s ms\n", size,
		                  correct ? "corretamente" : "INCORRETAMENTE", avg);
	}
}
