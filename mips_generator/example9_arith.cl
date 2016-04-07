class Main inherits IO {

    a : A;
    b : A <- new A;

    main() : Object {
        {
            out_string("\n");
            out_string("3 = ");
            out_int(3);
            out_string("\n3 + 2 = ");
            out_int(3 + 2);
            out_string("\n3 - 2 = ");
            out_int(3 - 2);
            out_string("\n3 * 2 = ");
            out_int(3 * 2);
            out_string("\n3 / 2 = ");
            out_int(3 / 2);
            out_string("\n~1 = ");
            out_int(~1);
            out_string("\nif 1 = 1 then ... = ");
            out_string(if 1 = 1 then "correct" else "error" fi);
            out_string("\nif 0 = 1 then ... = ");
            out_string(if 0 = 1 then "error" else "correct" fi);
            out_string("\nif 0 < 1 then ... = ");
            out_string(if 0 < 1 then "correct" else "error" fi);
            out_string("\nif 1 < 1 then ... = ");
            out_string(if 1 < 1 then "error" else "correct" fi);
            out_string("\nif 10 < 4 then ... = ");
            out_string(if 10 < 4 then "error" else "correct" fi);
            out_string("\nif 10 < 2*2 then ... = ");
            out_string(if 10 < 2*2 then "error" else "correct" fi);
            out_string("\nif 1 <= (100 - 99) then ... = ");
            out_string(if 1 <= (100 - 99) then "correct" else "error" fi);
            out_string("\nif 2 <= 1 then ... = ");
            out_string(if 2 <= 1 then "error" else "correct" fi);
            out_string("\nif not true then \"error\" else \"correct\" fi = ");
            out_string(if not true then "error" else "correct" fi);
            out_string("\nif isvoid a then \"correct\" else \"error\" fi = ");
            out_string(if isvoid a then "correct" else "error" fi);
            out_string("\nif isvoid b then \"error\" else \"correct\" fi = ");
            out_string(if isvoid b then "error" else "correct" fi);
            out_string("\n\n");
        }
    };
};

class A {
    z : Int;
};