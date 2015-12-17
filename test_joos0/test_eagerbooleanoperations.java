
public class test_eagerbooleanoperations {
  public test_eagerbooleanoperations() {
    System.out.print(this.m(true));
  }
  public static void main(String[] argv) {
    new test_eagerbooleanoperations();
  }
  public boolean m(boolean x) {
    return (x & true) | !x;
  }
}

