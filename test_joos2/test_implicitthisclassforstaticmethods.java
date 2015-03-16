
public class test_implicitthisclassforstaticmethods {
  public static int m1() {
    return 42;
  }
  public int m2() {
    return m1();
  }
}

