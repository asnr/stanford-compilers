class C1 inherits Int {
    c : Int;
};

class C {
    a : Int;
    b : Bool;
    init(x : Int, y : Bool) : C {
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
