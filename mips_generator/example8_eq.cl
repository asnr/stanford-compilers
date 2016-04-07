class Main {
  io : IO <- (new IO);
  x : Object <- 1;
  a : A <- (new A);
  main() : Object {
    {
      if x = a.foo() then  
        io.out_string("\n(Probably) correct: checking equality by value for dynamic types\n\n")
      else
        io.out_string("\n(Probably) incorrect: do not check equality by value because static types\n\n")
      fi;
    }
  };
};

class A {
  a : Int <- 2;
  foo() : Object {
    a - 1
  };
};