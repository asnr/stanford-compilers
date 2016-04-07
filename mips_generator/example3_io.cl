class Main {
  msg : String <- "hi you\n";
  main() : Object {
    (let a : IO <- (new IO) in
        {
            a.out_string(msg);
        }
    )
  };
};
