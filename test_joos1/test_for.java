
public class test_for {
  public test_for() {}
  public int m(int x) {
    int y = 0;
    for (int i=x; i>0; i=i-1) y=y+1;
    // or
    for ( ; ; ) {
    	return y;
    }
    // but not:
    // for (int i=x, j=x; i>0; i=i-1, j=j+1) y=y+j;
  }
}


