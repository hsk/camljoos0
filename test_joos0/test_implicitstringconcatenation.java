
public class test_implicitstringconcatenation {
  public test_implicitstringconcatenation() {}
  public test_fieldinitializers() {
    System.out.print(this.x);
  }
  public static void main(String[] argv) {
    new test_fieldinitializers();
  }
  public String m(int x) {
    return "foo" + x + true + null;
  }
}

