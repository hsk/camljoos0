
public class test_formult {
  public test_formult() {}
  public int m(int x) {
    int y = 0;
    for (int i=x, j=x; i>0; i=i-1, j=j+1) y=y+j;
    return y;
  }
}

