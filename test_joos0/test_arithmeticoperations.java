
public class test_arithmeticoperations {
  public test_arithmeticoperations() {
    System.out.print(this.m(10));
  }
  public static void main(String[] argv) {
    new test_arithmeticoperations();
  }
  public int m(int x) {
    return -2*x+87%x-(x/7);
  }
}

