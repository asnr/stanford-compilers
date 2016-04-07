class Main inherits IO {
  a : Int <- b + 999;
  b : Int;
  (* If ordering of d and c is swapped, dispatch to void error *)
  d : C <- (new C);
  c : C <- d.init(1, true);  
  main() : Object {
    {
      out_string("\nExpecting 0:\n");
      out_int(b);
      out_string("\n\n");
    }
  };
};

class C {
    a : Int;
    b : Bool;
    init(x : Int, y : Bool) : SELF_TYPE {
           {
        a <- x;
        b <- y;
        self;
           }
    };
};
