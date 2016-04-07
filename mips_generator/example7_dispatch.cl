class Main {
  z : Int <- 777;
  main() : Object {
    {
        (let a : IO <- (new IO), b : B <- (new B), c : A in
        {
            a.out_string("Expecting 1 then 2 then dispatch to void error\n");
            a.out_int(b@A.foo());
            a.out_string("\n");
            a.out_int(b.foo());
            a.out_string("\n");
            c.foo();
        });
    }
  };
};

class A {
  z : Int <- 1;
  foo() : Int {
    z
  };
};

class B inherits A {
  y : Int <- 2;
  foo() : Int {
    y
  };
};