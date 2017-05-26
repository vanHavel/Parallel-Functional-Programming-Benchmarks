import java.util.LinkedList;
import java.util.stream.Stream;
import java.util.stream.Collectors;
import java.util.List;

public class Mergesort {

  public static <T extends Comparable<? super T>> LinkedList<T> sort(LinkedList<T> xs) {
    List<T> res = xs.stream().sorted().collect(Collectors.toList());
    return new LinkedList<T>(res);
  }

  public static <T extends Comparable<T>> LinkedList<T> sortParallel(LinkedList<T> xs){
    List<T> res = xs.parallelStream().sorted().collect(Collectors.toList());
    return new LinkedList<T>(res);
  }


}
