
public class test_nonthisfieldaccess {
  public test_nonthisfieldaccess() {}
  public int x;
  public void m() {
    new test_nonthisfieldaccess().x = 42;
  }
}

