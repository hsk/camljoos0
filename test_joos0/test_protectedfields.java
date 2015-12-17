
public class test_protectedfields {
  public test_protectedfields() {}
  public test_fieldinitializers() {
    System.out.print(this.x);
  }
  public static void main(String[] argv) {
    new test_fieldinitializers();
  }
  protected int x;
}

