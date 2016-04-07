class Main {
  z : Int <- 777;
  main() : Object {
    {
        (let a : IO <- (new IO) in a.out_string("\nhi you\n"));
        (let a : String <- "how you\n" in            
            (let b : IO <- (new IO), c : String <- "\ndoin?\n\n" in
                {
                    b.out_string(a);
                    b.out_int(z);
                    b.out_string(c);
                }
            )
        );
    }
  };
};

class A {
  z : Int <- ~1;
  foo() : Int {
    z - 1
  };
};