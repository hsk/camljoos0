
public class test_closestmatchmethodoverloading {
  public int m1(Object x, Object y) {
    return 42;
  }
  public int m1(Object x, test_closestmatchmethodoverloading y) {
    return 87;
  }
  public int m2() {
    return this.m1(new test_closestmatchmethodoverloading(), new test_closestmatchmethodoverloading());
  }
}

