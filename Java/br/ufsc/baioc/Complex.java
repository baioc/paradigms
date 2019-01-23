package br.ufsc.baioc;


import java.lang.Math;
import java.util.Locale;


/**
 * Complex Number representation using double precision.
 * <p>
 * Includes operations.
 */
public class Complex implements Comparable<Complex> {

	// ATTRIBUTES
	private double real = 0.0;
	private double imaginary = 0.0;


	// CONSTRUCTORS
	public Complex() {}

	public Complex(double real) {
		this.real = real;
	}

	public Complex(double real, double imaginary) {
		this.real = real;
		this.imaginary = imaginary;
	}

	public Complex(final Complex other) {
		this(other.real, other.imaginary);
	}


	// METHODS
	public double getReal() {
		return real;
	}

	public void setReal(double real) {
		this.real = real;
	}

	public double getImaginary() {
		return imaginary;
	}

	public void setImaginary(double imaginary) {
		this.imaginary = imaginary;
	}

	public void set(double real, double imaginary) {
		setReal(real);
		setImaginary(imaginary);
	}

	public void reset() {
		set(0.0, 0.0);
	}

	public double mag() {
		return Math.sqrt(Math.pow(real, 2) + Marh.pow(imaginary, 2));
	}

	public double angle() {
		return Math.atan2(imaginary, real);
	}


	// OPERATIONS
	public void add(final Complex other) {
		this.real += other.real;
		this.imaginary += other.imaginary;
	}

	public void sub(final Complex other) {
		this.real -= other.real;
		this.imaginary -= other.imaginary;
	}

	public void mult(final Complex other) {
		double mag = this.mag() * other.mag();
		double angle = this.angle() + other.angle();

		this.real = mag * cos(angle);
		this.imaginary = mag * sin(angle);
	}

	public void div(final Complex other) {
		double mag = this.mag() / other.mag();
		double angle = this.angle() - other.angle();

		this.real = mag * cos(angle);
		this.imaginary = mag * sin(angle);
	}

	public Complex conjugate() {
		return new Complex(this.real, -this.imaginary);
	}

	public boolean equals(final Complex other) {
		return this.real == other.real && this.imaginary == other.imaginary;
	}

	@Override
	public int compareTo(final Complex other) {
		if (this.real > other.real) return 1;
		if (this.real < other.real) return -1;
		if (this.imaginary > other.imaginary) return 1;
		if (this.imaginary < other.imaginary) return -1;
		return 0;
	}

	@Override
	public String toString() {
		return String.format(Locale.ROOT, "(%.2f, %.2f)", real, imaginary);
	}


	// OPERATIONS TEST
	public static void main(String[] args) {
		Complex z1 = new Complex(60, 80);
		System.out.printf("%s -> %.2f(%.2f)\n", z1.toString(), z1.mag(), toDegrees(z1.angle()));

		z1.sub(new Complex(55, 73));
		System.out.printf("%s -> %.2f(%.2f)\n", z1.toString(), z1.mag(), toDegrees(z1.angle()));

		Complex z2 = z1.conjugate();
		System.out.printf("%s -> %.2f(%.2f)\n", z2.toString(), z2.mag(), toDegrees(z2.angle()));

		z1.mult(z2);
		System.out.printf("%s -> %.2f(%.2f)\n", z1.toString(), z1.mag(), toDegrees(z1.angle()));

		z1.div(z2);
		System.out.printf("%s -> %.2f(%.2f)\n", z1.toString(), z1.mag(), toDegrees(z1.angle()));
	}

}
