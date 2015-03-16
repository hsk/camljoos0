
public class test_nonstaticaccessofstaticmethod {
  public static int m1() {
    return 42;
  }
  public int m2() {
    test_nonstaticaccessofstaticmethod a = new test_nonstaticaccessofstaticmethod();
    return a.m1();
  }
}

