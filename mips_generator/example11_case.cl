class Main inherits IO {
    aa : A <- new A;
    bb : B <- new B;
    cc : C <- new C;
    dd : D <- new D;
    ee : E <- new E;
    z : A;  (* void *)
    main() : Object {
        {
            out_string("\n");

            case aa of
                o : Object => out_string("Dispatched to Object, correct\n");
                b : B => out_string("error\n");
                e : E => out_string("error\n");
            esac;

            case aa of
                a : A => out_string(a@A.name().concat("correct\n"));
                b : B => out_string(b@B.name().concat("error\n"));
                e : E => out_string(e@E.name().concat("error\n"));
            esac;

            case bb of
                a : A => out_string(a@A.name().concat("error\n"));
                b : B => out_string(b@B.name().concat("correct\n"));
                e : E => out_string(e@E.name().concat("error\n"));
            esac;

            case dd of
                a : A => out_string(a@A.name().concat("error\n"));
                c : C => out_string(c@C.name().concat("correct\n"));
                e : E => out_string(e@E.name().concat("error\n"));
            esac;

            out_string("expecting match on void in case statement error\n");
            case z of
                o : Object => out_string("error\n");
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