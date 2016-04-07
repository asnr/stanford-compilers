class Main inherits IO {
    main() : Object {
        let t : Bool <- true,
            f : Bool <- false,
            t_obj1 : Bool <- t,
            t_obj2 : Bool <- true in {

            out_string("\n");
            out_string(t.type_name().concat("\n"));
            out_string(t_obj1.type_name().concat("\n"));
            out_string(t_obj2.type_name().concat("\n\n"));

        }
    };
};