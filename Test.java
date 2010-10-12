package GottaTest;

import gnu.trove.TObjectIntHashMap;



public class Test {
  private static Object root = null;

  public static void main (String[] args) {

    TObjectIntHashMap test = new TObjectIntHashMap();

    int what = test.get( root );

    System.out.println( what );

  }

}

