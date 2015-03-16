
public class test_closestmatchconstructoroverloading {
  public test_closestmatchconstructoroverloading() {}
  public test_closestmatchconstructoroverloading(Object x, Object y) {}
  public test_closestmatchconstructoroverloading(Object x, test_closestmatchconstructoroverloading y) {}
  public void m() {
    new test_closestmatchconstructoroverloading(new test_closestmatchconstructoroverloading(), new test_closestmatchconstructoroverloading());
  }
}

