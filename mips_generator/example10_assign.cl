class Main {
    a : A <- new A;
    main() : Object {
        let io : IO, b : Int <- 12 in {
            io <- new IO;
            a <- new A;
            io.out_string("\nExpecting 1\n");
            io.out_int(a.init(1) + (b <- ~1));
            io.out_string("\n\n");
        }
    };
};

class A {
    z : Int;
    init(a : Int) : Int {
        {
            z <- a;
            z + 1;
        }
    };
};
