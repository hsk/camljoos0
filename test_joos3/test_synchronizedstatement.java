
public class test_synchronizedstatement {
  public int x;
  public void m() {
    synchronized(this) {
      x = x-1;
    }
  }
}


