
public class test_while {
  public test_while() {}
  public test_fieldinitializers() {
    System.out.print(this.x);
  }
  public static void main(String[] argv) {
    new test_fieldinitializers();
  }
  public int m(int x) {
    while (x>0) x=x-1;
    return x;
  }
}


