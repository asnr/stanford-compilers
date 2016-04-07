
(*  Example cool program testing as many aspects of the code generator
    as possible.
 *)

class C1 inherits C {
    c : Int;
    init(x: Int, y: Bool) : SELF_TYPE {
      {
        a <- x;
        b <- y;
        c <- 999;
        self;
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

Class Main {
    main():C {
      (new C).init(1,true)
    };
};
