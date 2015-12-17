
public class test_comparisonoperations {
  public test_comparisonoperations() {
    System.out.print(this.m(10));
  }
  public static void main(String[] argv) {
    new test_comparisonoperations();
  }
  public boolean m(int x) {
    return (x<87) && (x>42) && (x<=86) && (x>=43) && (x==51) && (x!=52) ;
  }
}

