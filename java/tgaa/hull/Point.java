package tgaa.hull;

import tgaa.hull.Point;

/*
 * Class that implements a point in the Euclidean space.
 * 
 */
public class Point implements Comparable<Point> {
	private double x;
	private double y;
	private Point comparatorPoint = null;

	public Point(double x, double y) {
		this.x = x;
		this.y = y;
	}

	public static boolean isCounterclockwise(Point a, Point b, Point c) {
		// determines if the angle formed by a -> b -> c is counterclockwise
		double signed_area_doubled = (b.x - a.x) * (c.y - a.y) - (b.y - a.y)
				* (c.x - a.x);
		return (signed_area_doubled > 0);
	}

	public static void printPoints(Point[] points) {
		for (Point p : points) {
			System.out.println(p);
		}
	}

	protected void setComparatorPoint(Point p) {
		this.comparatorPoint = p;
	}

	public double getPolarRadius() {
		return Math.sqrt(x * x + y * y);
	}

	// gets the polar angle between this point and origin of the plane
	public double getPolarAngle() {
		// java library requires swapped arguments
		double arctan = Math.atan2(y, x);
		return (arctan >= 0) ? arctan : (Math.PI * 2 - arctan);
	}

	// gets the polar angle between this point and the point given as
	// argument with the argument as origin
	public double getPolarAngle(Point p) {
		double x_n = p.x - x;
		double y_n = p.y - y;
		return new Point(x_n, y_n).getPolarAngle();
	}

	public double getEuclideanDistance(Point p) {
		return Math.sqrt(Math.pow(p.x - x, 2) + Math.pow(p.y - y, 2));
	}

	@Override
	public boolean equals(Object o) {
		if (o instanceof Point) {
			Point p = (Point) o;
			return p.x == x && p.y == y;
		} else {
			return false;
		}
	}

	@Override
	public int compareTo(Point p) {
		// compares two points in the plane according to the polar angle they
		// form with the comparator point of this object
		// if a comparator point is not provided, the default is the origin
		// of the plane
		if (comparatorPoint == null)
			comparatorPoint = new Point(0, 0);
		Double angle1 = comparatorPoint.getPolarAngle(this);
		Double angle2 = comparatorPoint.getPolarAngle(p);
		return angle1.compareTo(angle2);
	}

	@Override
	public String toString() {
		return "(" + x + ", " + y + ")";
	}

	public double getX() {
		return x;
	}

	public double getY() {
		return y;
	}

}
