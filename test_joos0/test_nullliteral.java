
public class test_nullliteral {
  public test_nullliteral() {}
  public test_fieldinitializers() {
    System.out.print(this.x);
  }
  public static void main(String[] argv) {
    new test_fieldinitializers();
  }
  protected test_nullliteral x = null;
}

