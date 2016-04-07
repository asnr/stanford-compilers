class Main inherits IO {
    aa : A <- new A;
    bb : B <- new B;
    cc : C <- new C;
    dd : D <- new D;
    ee : E <- new E;
    z : A;  (* void *)
    main() : Object {
        {
            out_string("\nexpecting no matching in case statement error\n");
            case aa of
                b : B => out_string("error\n");
            esac;
        }

    };
};

class A {
    name() : String { "Dispatched to A, " }; 
};

class B inherits A {
    name() : String { "Dispatched to B, " }; 
};


class C inherits A {
    name() : String { "Dispatched to C, " }; 
};


class D inherits C {
    name() : String { "Dispatched to D, " }; 
};

class E {
    name() : String { "Dispatched to E, " }; 
};
