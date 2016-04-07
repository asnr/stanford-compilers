class Main inherits A {
  main() : Object {
    {
        out_string("\nExpecting 999:\n");
        out_int(b);
        out_string("\n\n");
    }
  };
};

class A inherits IO {
  a : Bool;
  b : Int <- 999;
};
