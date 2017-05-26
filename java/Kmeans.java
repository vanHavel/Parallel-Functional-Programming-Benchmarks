import java.util.Arrays;
import java.util.stream.DoubleStream;
import java.util.stream.IntStream;
import java.util.LinkedList;

public class Kmeans {
  public static int[] assign(LinkedList<Point> means, LinkedList<Point> data) {
    return data.stream().mapToInt(p -> closest(means, p))
                              .toArray();
  }

  public static int[] passign(LinkedList<Point> means, LinkedList<Point> data) {
    return data.parallelStream().mapToInt(p -> closest(means, p))
                                .toArray();
  }

  public static int closest(LinkedList<Point> means, Point p) {
    DoubleStream distanceStream = means.stream().mapToDouble(q -> p.squaredDistanceTo(q));
    double[] distances = distanceStream.toArray();
    double minDistance = Arrays.stream(distances).min().getAsDouble();
    return IntStream.range(0, distances.length).filter(i -> distances[i] == minDistance)
                                               .findFirst()
                                               .getAsInt();
  }

}
