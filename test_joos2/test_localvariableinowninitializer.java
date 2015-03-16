
public class test_localvariableinowninitializer {
  public test_localvariableinowninitializer() {}
  public void m() {
    int x  = (x = 1) + x;
  }
}

