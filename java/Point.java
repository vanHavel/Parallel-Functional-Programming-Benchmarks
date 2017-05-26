public class Point {
  public double x;
  public double y;

  public double squaredDistanceTo(Point p) {
    return Math.pow(x - p.x, 2) + Math.pow(y - p.y, 2);
  }
}
