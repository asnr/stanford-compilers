class Main inherits IO {
    main() : Object {
        {
            out_string(let a : String <- "\nExpecting 2, 1:\n" in a);
            out_int(1 + (let a : Int <- 1 in a));
            out_string(", ");
            out_int(1 * (let a : Int <- 1 in a));
            out_string("\n\n");
        }
    };
};