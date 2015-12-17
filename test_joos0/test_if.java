
public class test_if {
  public test_fieldinitializers() {
    System.out.print(this.x);
  }
  public static void main(String[] argv) {
    new test_fieldinitializers();
  }

  public int m(int x) {
    int y = 0;
    if (x==0) y=42;
    else y=87;
    return y;
  }
}

