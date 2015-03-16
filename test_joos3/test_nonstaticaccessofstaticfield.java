
public class test_nonstaticaccessofstaticfield {
  public static int x;
  public int m() {
    test_nonstaticaccessofstaticfield a = new test_nonstaticaccessofstaticfield();
    return a.x;
  }
}

